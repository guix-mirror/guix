;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix build gnu-build-system)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            gnu-build))

;; Commentary:
;;
;; Standard build procedure for packages using the GNU Build System or
;; something compatible ("./configure && make && make install").  This is the
;; builder-side code.
;;
;; Code:

(define (first-subdirectory dir)
  "Return the path of the first sub-directory of DIR."
  (file-system-fold (lambda (path stat result)
                      (string=? path dir))
                    (lambda (path stat result) result) ; leaf
                    (lambda (path stat result) result) ; down
                    (lambda (path stat result) result) ; up
                    (lambda (path stat result)         ; skip
                      (or result path))
                    (lambda (path stat errno result)   ; error
                      (error "first-subdirectory" (strerror errno)))
                    #f
                    dir))

(define* (set-paths #:key inputs (path-exclusions '())
                    #:allow-other-keys)
  (let ((inputs (map cdr inputs)))
    (set-path-environment-variable "PATH" '("bin")
                                   (remove (cute member <>
                                                 (or (assoc-ref path-exclusions
                                                                "PATH")
                                                     '()))
                                           inputs))
    (set-path-environment-variable "CPATH" '("include")
                                   (remove (cute member <>
                                                 (or (assoc-ref path-exclusions
                                                                "CPATH")
                                                     '()))
                                           inputs))
    (set-path-environment-variable "LIBRARY_PATH" '("lib" "lib64")
                                   (remove (cute member <>
                                                 (or (assoc-ref path-exclusions
                                                                "LIBRARY_PATH")
                                                     '()))
                                           inputs))

    ;; FIXME: Eventually move this to the `search-paths' field of the
    ;; `pkg-config' package.
    (set-path-environment-variable "PKG_CONFIG_PATH"
                                   '("lib/pkgconfig" "lib64/pkgconfig")
                                   (remove (cute member <>
                                                 (or (assoc-ref path-exclusions
                                                                "PKG_CONFIG_PATH")
                                                     '()))
                                           inputs))

    ;; Dump the environment variables as a shell script, for handy debugging.
    (system "export > environment-variables")))

(define* (unpack #:key source #:allow-other-keys)
  (and (zero? (system* "tar" "xvf" source))
       (chdir (first-subdirectory "."))))

(define* (patch #:key (patches '()) (patch-flags '("--batch" "-p1"))
                #:allow-other-keys)
  (every (lambda (p)
           (format #t "applying patch `~a'~%" p)
           (zero? (apply system* "patch"
                         (append patch-flags (list "--input" p)))))
         patches))

(define* (configure #:key outputs (configure-flags '()) out-of-source?
                    #:allow-other-keys)
  (let* ((prefix     (assoc-ref outputs "out"))
         (libdir     (assoc-ref outputs "lib"))
         (includedir (assoc-ref outputs "include"))
         (flags      `(,(string-append "--prefix=" prefix)
                       "--enable-fast-install"    ; when using Libtool

                       ;; Produce multiple outputs when specific output names
                       ;; are recognized.
                       ,@(if libdir
                              (list (string-append "--libdir=" libdir "/lib"))
                              '())
                       ,@(if includedir
                             (list (string-append "--includedir="
                                                  includedir "/include"))
                             '())
                       ,@configure-flags))
         (abs-srcdir (getcwd))
         (srcdir     (if out-of-source?
                         (string-append "../" (basename abs-srcdir))
                         ".")))
    (format #t "source directory: ~s (relative from build: ~s)~%"
            abs-srcdir srcdir)
    (if out-of-source?
        (begin
          (mkdir "../build")
          (chdir "../build")))
    (format #t "build directory: ~s~%" (getcwd))
    (format #t "configure flags: ~s~%" flags)

    ;; Call `configure' with a relative path.  Otherwise, GCC's build system
    ;; (for instance) records absolute source file names, which typically
    ;; contain the hash part of the `.drv' file, leading to a reference leak.
    (zero? (apply system*
                  (string-append srcdir "/configure")
                  flags))))

(define* (build #:key (make-flags '()) (parallel-build? #t)
                #:allow-other-keys)
  (zero? (apply system* "make"
                `(,@(if parallel-build?
                        `("-j" ,(getenv "NIX_BUILD_CORES"))
                        '())
                  ,@make-flags))))

(define* (check #:key (make-flags '()) (tests? #t) (test-target "check")
                (parallel-tests? #t)
                #:allow-other-keys)
  (if tests?
      (zero? (apply system* "make" test-target
                    `(,@(if parallel-tests?
                            `("-j" ,(getenv "NIX_BUILD_CORES"))
                            '())
                      ,@make-flags)))
      (begin
        (format #t "test suite not run~%")
        #t)))

(define* (install #:key (make-flags '()) #:allow-other-keys)
  (zero? (apply system* "make" "install" make-flags)))

(define* (patch-shebangs #:key outputs (patch-shebangs? #t)
                         #:allow-other-keys)
  (define (list-of-files dir)
    (map (cut string-append dir "/" <>)
         (or (scandir dir (lambda (f)
                            (let ((s (stat (string-append dir "/" f))))
                              (eq? 'regular (stat:type s)))))
             '())))

  (define bindirs
    (append-map (match-lambda
                 ((_ . dir)
                  (list (string-append dir "/bin")
                        (string-append dir "/sbin"))))
                outputs))

  (when patch-shebangs?
    (let ((path (append bindirs
                        (search-path-as-string->list (getenv "PATH")))))
      (for-each (lambda (dir)
                  (let ((files (list-of-files dir)))
                    (for-each (cut patch-shebang <> path) files)))
                bindirs)))
  #t)

(define* (strip #:key outputs (strip-binaries? #t)
                (strip-flags '("--strip-debug"))
                (strip-directories '("lib" "lib64" "libexec"
                                     "bin" "sbin"))
                #:allow-other-keys)
  (define (strip-dir dir)
    (format #t "stripping binaries in ~s with flags ~s~%"
            dir strip-flags)
    (file-system-fold (const #t)
                      (lambda (path stat result)  ; leaf
                        (zero? (apply system* "strip"
                                      (append strip-flags (list path)))))
                      (const #t)                  ; down
                      (const #t)                  ; up
                      (const #t)                  ; skip
                      (lambda (path stat errno result)
                        (format (current-error-port)
                                "strip: failed to access `~a': ~a~%"
                                path (strerror errno))
                        #f)
                      #t
                      dir))

  (or (not strip-binaries?)
      (every strip-dir
             (append-map (match-lambda
                          ((_ . dir)
                           (filter-map (lambda (d)
                                         (let ((sub (string-append dir "/" d)))
                                           (and (directory-exists? sub) sub)))
                                       strip-directories)))
                         outputs))))

(define %standard-phases
  ;; Standard build phases, as a list of symbol/procedure pairs.
  (let-syntax ((phases (syntax-rules ()
                         ((_ p ...) `((p . ,p) ...)))))
    (phases set-paths unpack patch configure build check install
            patch-shebangs strip)))


(define* (gnu-build #:key (source #f) (outputs #f) (inputs #f)
                    (phases %standard-phases)
                    #:allow-other-keys
                    #:rest args)
  "Build from SOURCE to OUTPUTS, using INPUTS, and by running all of PHASES
in order.  Return #t if all the PHASES succeeded, #f otherwise."
  (setvbuf (current-output-port) _IOLBF)

  ;; The trick is to #:allow-other-keys everywhere, so that each procedure in
  ;; PHASES can pick the keyword arguments it's interested in.
  (every (match-lambda
          ((name . proc)
           (format #t "starting phase `~a'~%" name)
           (apply proc args)))
         phases))
