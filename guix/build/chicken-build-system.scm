;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 raingloom <raingloom@riseup.net>
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

(define-module (guix build chicken-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build union)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:export (%standard-phases
            chicken-build))

;; CHICKEN_EGG_CACHE is where sources are fetched and binaries are built
;; CHICKEN_INSTALL_REPOSITORY is where dependencies are looked up
;; its first component is also where new eggs are installed.

;; TODO: deduplicate with go-build-system.scm ?
;; TODO: the binary version should be defined in one of the relevant modules
;; instead of being hardcoded everywhere. Tried to do that but got undefined
;; variable errors.

(define (chicken-package? name)
  (string-prefix? "chicken-" name))

(define* (setup-chicken-environment #:key inputs outputs #:allow-other-keys)
  (setenv "CHICKEN_INSTALL_REPOSITORY"
          (string-concatenate
           ;; see TODO item about binary version above
           (append (list (assoc-ref outputs "out") "/var/lib/chicken/11/")
                   (let ((oldenv (getenv "CHICKEN_INSTALL_REPOSITORY")))
                     (if oldenv
                         (list  ":" oldenv)
                         '())))))
  (setenv "CHICKEN_EGG_CACHE" (getcwd))
  #t)

;; This is copied from go-build-system.scm so it could probably be simplified.
;; I used it because the source of the egg needs to be unpacked into a directory
;; that is named after the egg and I knew that the go build system does that.
(define* (unpack #:key source egg-name unpack-path #:allow-other-keys)
  "Relative to $CHICKEN_EGG_CACHE, unpack SOURCE in UNPACK-PATH, or EGG-NAME
when UNPACK-PATH is unset.  If the SOURCE archive has a single top level
directory, it is stripped so that the sources appear directly under UNPACK-PATH.
When SOURCE is a directory, copy its content into UNPACK-PATH instead of
unpacking."
  (define (unpack-maybe-strip source dest)
    (let* ((scratch-dir (string-append (or (getenv "TMPDIR") "/tmp")
                                       "/scratch-dir"))
           (out (mkdir-p scratch-dir)))
      (with-directory-excursion scratch-dir
        (if (string-suffix? ".zip" source)
            (invoke "unzip" source)
            (invoke "tar" "-xvf" source))
        (let ((top-level-files (remove (lambda (x)
                                         (member x '("." "..")))
                                       (scandir "."))))
          (match top-level-files
            ((top-level-file)
             (when (file-is-directory? top-level-file)
               (copy-recursively top-level-file dest #:keep-mtime? #t)))
            (_
             (copy-recursively "." dest #:keep-mtime? #t)))))
      (delete-file-recursively scratch-dir)))

  (when (string-null? egg-name)
    (display "WARNING: The egg name is unset.\n"))
  (when (string-null? unpack-path)
    (set! unpack-path egg-name))
  (let ((dest (string-append (getenv "CHICKEN_EGG_CACHE") "/" unpack-path)))
    (mkdir-p dest)
    (if (file-is-directory? source)
        (copy-recursively source dest #:keep-mtime? #t)
        (unpack-maybe-strip source dest)))
  #t)

(define* (build #:key egg-name #:allow-other-keys)
  "Build the Chicken egg named by EGG-NAME"
  (invoke "chicken-install" "-cached" "-no-install" egg-name))

(define* (install #:key egg-name #:allow-other-keys)
  "Install the already built egg named by EGG-NAME"
  (invoke "chicken-install" "-cached" egg-name))

(define* (check #:key egg-name tests? #:allow-other-keys)
  "Build and run tests for the Chicken egg EGG-NAME"
  ;; there is no "-test-only" option, but we've already run install
  ;; so this just runs tests.
  ;; i think it's a fair assumption that phases won't be reordered.
  (setenv "CHICKEN_REPOSITORY_PATH"
          (string-append (getenv "CHICKEN_INSTALL_REPOSITORY")
                         ":"
                         (getenv "CHICKEN_REPOSITORY_PATH")))
  (when tests?
    (invoke "chicken-install" "-cached" "-test" "-no-install" egg-name)))

;; It doesn't look like Chicken generates any unnecessary references.
;; So we don't have to remove them either. Nice.

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (replace 'unpack unpack)
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'patch-generated-file-shebangs)
    (add-before 'unpack 'setup-chicken-environment setup-chicken-environment)
    (replace 'build build)
    (delete 'check)
    (replace 'install install)
    (add-after 'install 'check check)))

(define* (chicken-build #:key inputs (phases %standard-phases)
                        #:allow-other-keys #:rest args)
  "Build the given Chicken package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
