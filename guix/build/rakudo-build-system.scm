;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (guix build rakudo-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            rakudo-build))

;; Commentary:
;;
;; Builder-side code of the standard Rakudo package build procedure.
;;
;; Code:

(define* (check #:key tests? inputs with-prove6? #:allow-other-keys)
  (if (and tests? (assoc-ref inputs "perl6-tap-harness"))
  ;(if (and tests? with-prove6?)
      (invoke "prove6" "-I=lib" "t/")
      (format #t "test suite not run~%"))
  #t)

(define* (install #:key inputs outputs with-zef? #:allow-other-keys)
  "Install a given Perl6 package."
  (let* ((out   (assoc-ref outputs "out"))
         (perl6 (string-append out "/share/perl6")))
    (if (assoc-ref inputs "perl6-zef")
    ;(if with-zef?
        (begin
          (let ((zef (string-append (assoc-ref inputs "perl6-zef")
                                    "/bin/zef")))
            (setenv "HOME" (getcwd))
            (mkdir-p perl6)
            (invoke zef "install" "--verbose" "."
                    ;; Don't install any of the following:
                    "--/depends" "--/build-depends" "--/test-depends"
                    (string-append "--install-to=" perl6))
            (delete-file (string-append perl6 "/repo.lock")))
          #t)
        (begin
          (let ((inst (string-append (assoc-ref inputs "rakudo")
                                     "/share/perl6/tools/install-dist.p6")))
            (setenv "RAKUDO_RERESOLVE_DEPENDENCIES" "0")
            (setenv "RAKUDO_MODULE_DEBUG" "1") ; be verbose while building
            (invoke inst (string-append "--to=" perl6) "--for=site"))))))

(define* (install-libs #:key outputs #:allow-other-keys)
  (let ((out  (assoc-ref outputs "out"))
        (lock "lib/.precomp/.lock"))
    (when (file-exists? lock)
      (delete-file "lib/.precomp/.lock"))
    (copy-recursively "lib" (string-append out "/share/perl6/lib"))
    #t))

(define* (install-bins #:key outputs #:allow-other-keys)
  (let ((out  (assoc-ref outputs "out")))
    (when (file-exists? "bin")
      (for-each (lambda (file)
                  (install-file file (string-append out "/bin"))
                  (chmod (string-append out "/" file) #o555))
                (find-files "bin" ".*")))
    (when (file-exists? "sbin")
      (for-each (lambda (file)
                  (install-file file (string-append out "/sbin"))
                  (chmod (string-append out "/" file) #o555))
                (find-files "sbin" ".*")))
    #t))

(define* (install-resources #:key outputs #:allow-other-keys)
  (let ((out  (assoc-ref outputs "out")))
    (when (file-exists? "resources")
      (copy-recursively "resources"
                        (string-append out "/share/perl6/resources")))
  #t))

(define* (wrap #:key inputs outputs #:allow-other-keys)
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

  (let* ((out  (assoc-ref outputs "out"))
         (var `("PERL6LIB" "," prefix
                ,(cons (string-append out "/share/perl6/lib,"
                                      out "/share/perl6/site/lib,"
                                      out "/share/perl6/vendor/lib")
                       (search-path-as-string->list
                        (or (getenv "PERL6LIB") "") #\,)))))
    (for-each (lambda (dir)
                (let ((files (list-of-files dir)))
                  (for-each (cut wrap-program <> var)
                            files)))
              bindirs)
    #t))

(define %standard-phases
  ;; No need for 'bootstrap, 'configure or 'build.
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'build)
    (replace 'check check)
    (replace 'install install)
    (add-before 'install 'install-lib-dir install-libs)
    (add-after 'install-lib-dir 'install-resources install-resources)
    (add-after 'install-resources 'install-binaries install-bins)
    ;; needs to be after 'install-binaries and all 'install phases
    (add-after 'install 'wrap wrap)))

(define* (rakudo-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given Perl6 package, applying all of PHASES in order."
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))

;;; rakudo-build-system.scm ends here
