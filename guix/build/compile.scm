;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2014, 2016-2020, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix build compile)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (system base target)
  #:use-module (system base compile)
  #:use-module (system base message)
  #:use-module (guix modules)
  #:use-module (guix build utils)
  #:use-module (language tree-il optimize)
  #:use-module (language cps optimize)
  #:export (compile-files))

;;; Commentary:
;;;
;;; Support code to compile Guile code as efficiently as possible (with 2.2).
;;;
;;; Code:

(define (clear-keyword-arguments keywords args)
  "Set to #f the value associated with each of the KEYWORDS in ARGS."
  (let loop ((args   args)
             (result '()))
    (match args
      (()
       (reverse result))
      (((? keyword? kw) arg . rest)
       (loop rest
             (if (memq kw keywords)
                 (cons* #f kw result)
                 (cons* arg kw result))))
      ((head . tail)
       (loop tail (cons head result))))))

(define optimizations-for-level
  (or (and=> (false-if-exception
              (resolve-interface '(system base optimize)))
             (lambda (iface)
               (module-ref iface 'optimizations-for-level))) ;Guile 3.0
      (let ()                                                ;Guile 2.2
        (define %default-optimizations
          ;; Default optimization options (equivalent to -O2 on Guile 2.2).
          (append (tree-il-default-optimization-options)
                  (cps-default-optimization-options)))

        (define %lightweight-optimizations
          ;; Lightweight optimizations (like -O0, but with partial evaluation).
          (let loop ((opts %default-optimizations)
                     (result '()))
            (match opts
              (() (reverse result))
              ((#:partial-eval? _ rest ...)
               (loop rest `(#t #:partial-eval? ,@result)))
              ((kw _ rest ...)
               (loop rest `(#f ,kw ,@result))))))

        (lambda (level)
          ;; In the upcoming Guile 3.0.8, .go files include code of their
          ;; inlinable exports and free variables are resolved at compile time
          ;; (both are enabled at -O1) to permit cross-module inlining
          ;; (enabled at -O2).  Unfortunately, this currently leads to
          ;; non-reproducible and more expensive builds, so we turn it off
          ;; here:
          ;; <https://wingolog.org/archives/2021/05/13/cross-module-inlining-in-guile>.
          (clear-keyword-arguments '(#:inlinable-exports? #:resolve-free-vars?
                                     #:cross-module-inlining?)
                                   (if (<= level 1)
                                       %lightweight-optimizations
                                       %default-optimizations))))))

(define (supported-warning-type? type)
  "Return true if TYPE, a symbol, denotes a supported warning type."
  (find (lambda (warning-type)
          (eq? type (warning-type-name warning-type)))
        %warning-types))

(define %warnings
  ;; FIXME: 'format' is missing because it reports "non-literal format
  ;; strings" due to the fact that we use 'G_' instead of '_'.  We'll need
  ;; help from Guile to solve this.
  (let ((optional (lambda (type)
                    (if (supported-warning-type? type)
                        (list type)
                        '()))))
    `(unbound-variable arity-mismatch
      macro-use-before-definition                         ;new in 2.2
      ,@(optional 'shadowed-toplevel))))                  ;new in 2.2.5

(define (optimization-options file)
  "Return the default set of optimizations options for FILE."
  (define (strip-option option lst)
    (let loop ((lst lst)
               (result '()))
      (match lst
        (()
         (reverse result))
        ((kw value rest ...)
         (if (eq? kw option)
             (append (reverse result) rest)
             (loop rest (cons* value kw result)))))))

  (define (override-option option value lst)
    `(,option ,value ,@(strip-option option lst)))

  (cond ((or (string-contains file "gnu/packages/")
             (string-contains file "gnu/tests/"))
         ;; Use '-O1' to have partial evaluation and primitive inlining so we
         ;; can honor the "macro writer's bill of rights".
         (optimizations-for-level 1))
        ((string-contains file "gnu/services/")
         ;; '-O2 -Ono-letrectify' compiles about ~20% faster than '-O2' for
         ;; large files like gnu/services/mail.scm.
         (override-option #:letrectify? #f
                          (optimizations-for-level 2)))
        (else
         (optimizations-for-level 3))))

(define (scm->go file)
  "Strip the \".scm\" suffix from FILE, and append \".go\"."
  (string-append (string-drop-right file 4) ".go"))

(define (relative-file directory file)
  "Return FILE relative to DIRECTORY, if possible."
  (if (string-prefix? (string-append directory "/") file)
      (string-drop file (+ 1 (string-length directory)))
      file))

(define* (load-files directory files
                     #:key
                     (report-load (const #f))
                     (debug-port (%make-void-port "w")))
  "Load FILES, a list of relative file names, from DIRECTORY."
  (define total
    (length files))

  (let loop ((files files)
             (completed 0))
    (match files
      (()
       (unless (zero? total)
         (report-load #f total completed))
       *unspecified*)
      ((file files ...)
       (let ((file (relative-file directory file)))
         (report-load file total completed)
         (format debug-port "~%loading '~a'...~%" file)

         (resolve-interface (file-name->module-name file))

         (loop files (+ 1 completed)))))))

(define-syntax-rule (with-augmented-search-path path item body ...)
  "Within the dynamic extent of BODY, augment PATH by adding ITEM to the
front."
  (let ((initial-value path))
    (dynamic-wind
      (lambda ()
        (set! path (cons item path)))
      (lambda ()
        body ...)
      (lambda ()
        (set! path initial-value)))))

(define (call/exit-on-exception file thunk)
  "Evaluate THUNK and exit right away if an exception is thrown.  Report FILE
as the file that was being compiled when the exception was thrown."
  (catch #t
    thunk
    (const #f)
    (lambda (key . args)
      (false-if-exception
       ;; Duplicate stderr to avoid thread-safety issues.
       (let* ((port  (duplicate-port (current-error-port) "w0"))
              (stack (make-stack #t))
              (depth (stack-length stack))
              (frame (and (> depth 1) (stack-ref stack 1))))
         (newline port)
         (format port "error: failed to compile '~a':~%~%" file)
         (false-if-exception (display-backtrace stack port))
         (print-exception port frame key args)))

      ;; Don't go any further.
      (primitive-exit 1))))

(define-syntax-rule (exit-on-exception file exp ...)
  "Evaluate EXP and exit if an exception is thrown.  Report FILE as the faulty
file when an exception is thrown."
  (call/exit-on-exception file (lambda () exp ...)))

(define* (compile-files source-directory build-directory files
                        #:key
                        (host %host-type)
                        (workers (current-processor-count))
                        (optimization-options optimization-options)
                        (warning-options `(#:warnings ,%warnings))
                        (report-load (const #f))
                        (report-compilation (const #f))
                        (debug-port (%make-void-port "w")))
  "Compile FILES, a list of source files taken from SOURCE-DIRECTORY, to
BUILD-DIRECTORY, using up to WORKERS parallel workers.  The resulting object
files are for HOST, a GNU triplet such as \"x86_64-linux-gnu\"."
  (define progress-lock (make-mutex))
  (define total (length files))
  (define progress 0)

  (define (build file)
    (with-mutex progress-lock
      (report-compilation file total progress)
      (set! progress (+ 1 progress)))

    ;; Exit as soon as something goes wrong.
    (exit-on-exception
     file
     (let ((relative (relative-file source-directory file)))
       (compile-file file
                     #:output-file (string-append build-directory "/"
                                                  (scm->go relative))
                     #:opts (append warning-options
                                    (optimization-options relative))))))

  (with-augmented-search-path %load-path source-directory
    (with-augmented-search-path %load-compiled-path build-directory
      (with-fluids ((*current-warning-prefix* ""))
        ;; Make sure the compiler's modules are loaded before 'with-target'
        ;; (since 'with-target' influences the .go loader), and before
        ;; starting to compile files in parallel.
        (compile #f)

        (with-target host
          (lambda ()
            ;; FIXME: To work around <https://bugs.gnu.org/15602>, we first
            ;; load all of FILES.
            (load-files source-directory files
                        #:report-load report-load
                        #:debug-port debug-port)

            ;; XXX: Don't use too many workers to work around the insane
            ;; memory requirements of the compiler in Guile 2.2.2:
            ;; <https://lists.gnu.org/archive/html/guile-devel/2017-05/msg00033.html>.
            (n-par-for-each (min workers 8) build files)

            (unless (zero? total)
              (report-compilation #f total total))))))))

(eval-when (eval load)
  (when (and (string=? "2" (major-version))
             (or (string=? "0" (minor-version))
                 (and (string=? (minor-version) "2")
                      (< (string->number (micro-version)) 4))))
    ;; Work around <https://bugs.gnu.org/31878> on Guile < 2.2.4.
    ;; Serialize 'try-module-autoload' calls.
    (set! (@ (guile) try-module-autoload)
      (let ((mutex (make-mutex 'recursive))
            (real  (@ (guile) try-module-autoload)))
        (lambda* (module #:optional version)
          (with-mutex mutex
            (real module version)))))))

;;; Local Variables:
;;; eval: (put 'with-augmented-search-path 'scheme-indent-function 2)
;;; eval: (put 'with-target 'scheme-indent-function 1)
;;; End:
