;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (guix build android-ndk-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            android-ndk-build))

;; Commentary:
;;
;; Builder-side code of the Android NDK build system.
;;
;; Code:

(define* (configure #:key inputs outputs #:allow-other-keys)
  (let ((library-directories (filter-map (match-lambda
                                          ((name . path)
                                           (if (eq? 'directory (stat:type (stat path)))
                                               path
                                               #f)))
                                         inputs)))
    (setenv "CC" "gcc")
    (setenv "CXX" "g++")
    (setenv "CPPFLAGS"
     (string-join
      (map (cut string-append "-I " <> "/include") library-directories)
      " "))
    (setenv "LDFLAGS"
     (string-append "-L . "
                    (string-join
                     (map (lambda (x)
                            (string-append "-L " x "/lib" " -Wl,-rpath=" x "/lib"))
                          library-directories)
                     " ")))
    #t))

(define* (install #:key inputs outputs (make-flags '()) #:allow-other-keys)
  (let ((out (assoc-ref outputs "out")))
    (apply invoke "make" "install"
           (string-append "prefix=" out)
           make-flags)
    #t))

(define* (check #:key target inputs outputs (tests? (not target)) (make-flags '()) #:allow-other-keys)
  (if tests?
      (begin
        (apply invoke "make" "check" make-flags)
        (when (and (file-exists? "tests") tests?)
          (with-directory-excursion "tests"
            (apply invoke "make" "check" make-flags))))
      (format #t "test suite not run~%"))
  #t)

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (replace 'configure configure)
    (replace 'install install)
    (replace 'check check)))

(define* (android-ndk-build #:key inputs (phases %standard-phases)
                            #:allow-other-keys #:rest args)
  "Build the given Android NDK package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
