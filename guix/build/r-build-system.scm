;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix build r-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)
  #:export (%standard-phases
            r-build))

;; Commentary:
;;
;; Builder-side code of the standard build procedure for R packages.
;;
;; Code:

(define (invoke-r command params)
  (apply invoke "R" "CMD" command params))

(define (pipe-to-r command params)
  (let ((port (apply open-pipe* OPEN_WRITE "R" params)))
    (display command port)
    (let ((code (status:exit-val (close-pipe port))))
      (unless (zero? code)
        (raise (condition ((@@ (guix build utils) &invoke-error)
                           (program "R")
                           (arguments (cons command params))
                           (exit-status (status:exit-val code))
                           (term-signal (status:term-sig code))
                           (stop-signal (status:stop-sig code)))))))))

(define (generate-site-path inputs)
  (string-join (map (match-lambda
                      ((_ . path)
                       (string-append path "/site-library")))
                    ;; Restrict to inputs beginning with "r-".
                    (filter (match-lambda
                              ((name . _)
                               (string-prefix? "r-" name)))
                            inputs))
               ":"))

(define* (check #:key test-target inputs outputs tests? #:allow-other-keys)
  "Run the test suite of a given R package."
  (let* ((libdir    (string-append (assoc-ref outputs "out") "/site-library/"))

         ;; R package names are case-sensitive and cannot be derived from the
         ;; Guix package name.  The exact package name is required as an
         ;; argument to ‘tools::testInstalledPackage’, which runs the tests
         ;; for a package given its name and the path to the “library” (a
         ;; location for a collection of R packages) containing it.

         ;; Since there can only be one R package in any collection (=
         ;; “library”), the name of the only directory in the collection path
         ;; is the original name of the R package.
         (pkg-name  (car (scandir libdir (negate (cut member <> '("." ".."))))))
         (testdir   (string-append libdir pkg-name "/" test-target))
         (site-path (string-append libdir ":" (generate-site-path inputs))))
    (when (and tests? (file-exists? testdir))
      (setenv "R_LIBS_SITE" site-path)
      (pipe-to-r (string-append "tools::testInstalledPackage(\"" pkg-name "\", "
                                "lib.loc = \"" libdir "\")")
                 '("--no-save" "--slave")))
    #t))

(define* (install #:key outputs inputs (configure-flags '())
                  #:allow-other-keys)
  "Install a given R package."
  (let* ((out          (assoc-ref outputs "out"))
         (site-library (string-append out "/site-library/"))
         (params       (append configure-flags
                               (list "--install-tests"
                                     (string-append "--library=" site-library)
                                     "--built-timestamp=1970-01-01"
                                     ".")))
         (site-path    (string-append site-library ":"
                                      (generate-site-path inputs))))
    ;; If dependencies cannot be found at install time, R will refuse to
    ;; install the package.
    (setenv "R_LIBS_SITE" site-path)
    ;; Some R packages contain a configure script for which the CONFIG_SHELL
    ;; variable should be set.
    (setenv "CONFIG_SHELL" (which "bash"))
    (mkdir-p site-library)
    (invoke-r "INSTALL" params)))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'build)
    (delete 'check) ; tests must be run after installation
    (replace 'install install)
    (add-after 'install 'check check)))

(define* (r-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given R package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; r-build-system.scm ends here
