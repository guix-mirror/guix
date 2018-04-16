;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (build-self)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (guix ui)
  #:use-module (guix config)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:export (build))

;;; Commentary:
;;;
;;; When loaded, this module returns a monadic procedure of at least one
;;; argument: the source tree to build.  It returns a derivation that
;;; builds it.
;;;
;;; This file uses modules provided by the already-installed Guix.  Those
;;; modules may be arbitrarily old compared to the version we want to
;;; build.  Because of that, it must rely on the smallest set of features
;;; that are likely to be provided by the (guix) and (gnu) modules, and by
;;; Guile itself, forever and ever.
;;;
;;; Code:


;;;
;;; Generating (guix config).
;;;
;;; This is copied from (guix self) because we cannot assume (guix self) is
;;; available at this point.
;;;

(define %dependency-variables
  ;; (guix config) variables corresponding to dependencies.
  '(%libgcrypt %libz %xz %gzip %bzip2 %nix-instantiate))

(define %persona-variables
  ;; (guix config) variables that define Guix's persona.
  '(%guix-package-name
    %guix-version
    %guix-bug-report-address
    %guix-home-page-url))

(define %config-variables
  ;; (guix config) variables corresponding to Guix configuration (storedir,
  ;; localstatedir, etc.)
  (sort (filter pair?
                (module-map (lambda (name var)
                              (and (not (memq name %dependency-variables))
                                   (not (memq name %persona-variables))
                                   (cons name (variable-ref var))))
                            (resolve-interface '(guix config))))
        (lambda (name+value1 name+value2)
          (string<? (symbol->string (car name+value1))
                    (symbol->string (car name+value2))))))

(define* (make-config.scm #:key libgcrypt zlib gzip xz bzip2
                          (package-name "GNU Guix")
                          (package-version "0")
                          (bug-report-address "bug-guix@gnu.org")
                          (home-page-url "https://gnu.org/s/guix"))

  ;; Hack so that Geiser is not confused.
  (define defmod 'define-module)

  (scheme-file "config.scm"
               #~(begin
                   (#$defmod (guix config)
                     #:export (%guix-package-name
                               %guix-version
                               %guix-bug-report-address
                               %guix-home-page-url
                               %libgcrypt
                               %libz
                               %gzip
                               %bzip2
                               %xz
                               %nix-instantiate))

                   ;; XXX: Work around <http://bugs.gnu.org/15602>.
                   (eval-when (expand load eval)
                     #$@(map (match-lambda
                               ((name . value)
                                #~(define-public #$name #$value)))
                             %config-variables)

                     (define %guix-package-name #$package-name)
                     (define %guix-version #$package-version)
                     (define %guix-bug-report-address #$bug-report-address)
                     (define %guix-home-page-url #$home-page-url)

                     (define %gzip
                       #+(and gzip (file-append gzip "/bin/gzip")))
                     (define %bzip2
                       #+(and bzip2 (file-append bzip2 "/bin/bzip2")))
                     (define %xz
                       #+(and xz (file-append xz "/bin/xz")))

                     (define %libgcrypt
                       #+(and libgcrypt
                              (file-append libgcrypt "/lib/libgcrypt")))
                     (define %libz
                       #+(and zlib
                              (file-append zlib "/lib/libz")))

                     (define %nix-instantiate     ;for (guix import snix)
                       "nix-instantiate")))))


;;;
;;; 'gexp->script'.
;;;
;;; This is our own variant of 'gexp->script' with an extra #:module-path
;;; parameter, which was unavailable in (guix gexp) until commit
;;; 1ae16033f34cebe802023922436883867010850f (March 2018.)
;;;

(define (load-path-expression modules path)
  "Return as a monadic value a gexp that sets '%load-path' and
'%load-compiled-path' to point to MODULES, a list of module names.  MODULES
are searched for in PATH."
  (mlet %store-monad ((modules  (imported-modules modules
                                                  #:module-path path))
                      (compiled (compiled-modules modules
                                                  #:module-path path)))
    (return (gexp (eval-when (expand load eval)
                    (set! %load-path
                      (cons (ungexp modules) %load-path))
                    (set! %load-compiled-path
                      (cons (ungexp compiled)
                            %load-compiled-path)))))))

(define* (gexp->script name exp
                       #:key (guile (default-guile))
                       (module-path %load-path))
  "Return an executable script NAME that runs EXP using GUILE, with EXP's
imported modules in its search path."
  (mlet %store-monad ((set-load-path
                       (load-path-expression (gexp-modules exp)
                                             module-path)))
    (gexp->derivation name
                      (gexp
                       (call-with-output-file (ungexp output)
                         (lambda (port)
                           ;; Note: that makes a long shebang.  When the store
                           ;; is /gnu/store, that fits within the 128-byte
                           ;; limit imposed by Linux, but that may go beyond
                           ;; when running tests.
                           (format port
                                   "#!~a/bin/guile --no-auto-compile~%!#~%"
                                   (ungexp guile))

                           (write '(ungexp set-load-path) port)
                           (write '(ungexp exp) port)
                           (chmod port #o555))))
                      #:module-path module-path)))


(define (date-version-string)
  "Return the current date and hour in UTC timezone, for use as a poor
person's version identifier."
  ;; XXX: Replace with a Git commit id.
  (date->string (current-date 0) "~Y~m~d.~H"))

(define* (build-program source version
                        #:optional (guile-version (effective-version)))
  "Return a program that computes the derivation to build Guix from SOURCE."
  (define select?
    ;; Select every module but (guix config) and non-Guix modules.
    (match-lambda
      (('guix 'config) #f)
      (('guix _ ...)   #t)
      (('gnu _ ...)    #t)
      (_               #f)))

  (with-imported-modules `(((guix config)
                            => ,(make-config.scm
                                 #:libgcrypt
                                 (specification->package "libgcrypt")))
                           ,@(source-module-closure `((guix store)
                                                      (guix self)
                                                      (guix derivations)
                                                      (gnu packages bootstrap))
                                                    (list source)
                                                    #:select? select?))
    (gexp->script "compute-guix-derivation"
                  #~(begin
                      (use-modules (ice-9 match))

                      (eval-when (expand load eval)
                        ;; Don't augment '%load-path'.
                        (unsetenv "GUIX_PACKAGE_PATH")

                        ;; (gnu packages …) modules are going to be looked up
                        ;; under SOURCE.  (guix config) is looked up in FRONT.
                        (match %load-path
                          ((#$source _ ...)
                           #t)                    ;already done
                          ((front _ ...)
                           (set! %load-path (list #$source front))))

                        ;; Only load our own modules or those of Guile.
                        (match %load-compiled-path
                          ((front _ ... sys1 sys2)
                           (set! %load-compiled-path
                             (list front sys1 sys2)))))

                      (use-modules (guix store)
                                   (guix self)
                                   (guix derivations)
                                   (srfi srfi-1))

                      (define (spin system)
                        (define spin
                          (circular-list "-" "\\" "|" "/" "-" "\\" "|" "/"))

                        (format (current-error-port)
                                "Computing Guix derivation for '~a'...  "
                                system)
                        (let loop ((spin spin))
                          (display (string-append "\b" (car spin))
                                   (current-error-port))
                          (force-output (current-error-port))
                          (sleep 1)
                          (loop (cdr spin))))

                      (match (command-line)
                        ((_ _ system)
                         (with-store store
                           (call-with-new-thread
                            (lambda ()
                              (spin system)))

                           (display
                            (derivation-file-name
                             (run-with-store store
                               (guix-derivation #$source #$version
                                                #$guile-version)
                               #:system system)))))))
                  #:module-path (list source))))

;; The procedure below is our return value.
(define* (build source
                #:key verbose? (version (date-version-string)) system
                (guile-version (match ((@ (guile) version))
                                 ("2.2.2" "2.2.2")
                                 (_       (effective-version))))
                #:allow-other-keys
                #:rest rest)
  "Return a derivation that unpacks SOURCE into STORE and compiles Scheme
files."
  ;; Build the build program and then use it as a trampoline to build from
  ;; SOURCE.
  (mlet %store-monad ((build  (build-program source version guile-version))
                      (system (if system (return system) (current-system))))
    (mbegin %store-monad
      (show-what-to-build* (list build))
      (built-derivations (list build))
      (let* ((pipe   (begin
                       (setenv "GUILE_WARN_DEPRECATED" "no") ;be quiet and drive
                       (open-pipe* OPEN_READ
                                   (derivation->output-path build)
                                   source system)))
             (str    (get-string-all pipe))
             (status (close-pipe pipe)))
        (match str
          ((? eof-object?)
           (error "build program failed" (list build status)))
          ((? derivation-path? drv)
           (mbegin %store-monad
             (return (newline (current-output-port)))
             ((store-lift add-temp-root) drv)
             (return (read-derivation-from-file drv))))
          ((? string? str)
           (error "invalid build result" (list build str))))))))

;; This file is loaded by 'guix pull'; return it the build procedure.
build

;; Local Variables:
;; eval: (put 'with-load-path 'scheme-indent-function 1)
;; End:

;;; build-self.scm ends here
