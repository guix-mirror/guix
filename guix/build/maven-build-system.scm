;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Julien Lepiller <julien@lepiller.eu>
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

(define-module (guix build maven-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (guix build maven pom)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            maven-build))

;; Commentary:
;;
;; Builder-side code of the standard maven build procedure.
;;
;; Code:

(define* (set-home #:key outputs inputs #:allow-other-keys)
  (let ((home (string-append (getcwd) "/build-home")))
    (setenv "HOME" home))
  (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
  #t)

(define* (configure #:key inputs #:allow-other-keys)
  (let* ((m2-files (map
                     (lambda (input)
                       (match input
                         ((name . dir)
                          (let ((m2-dir (string-append dir "/lib/m2")))
                            (if (file-exists? m2-dir) m2-dir #f)))))
                     inputs))
         (m2-files (filter (lambda (a) a) m2-files)))
    (for-each
      (lambda (m2-dir)
        (for-each
          (lambda (file)
            (let ((dir (string-append (getenv "HOME") "/.m2/repository/"
                                      (dirname file))))
              (mkdir-p dir)
              (symlink (string-append m2-dir "/" file)
                       (string-append dir "/" (basename file)))))
          (with-directory-excursion m2-dir
            (find-files "." ".*.(jar|pom)$"))))
      m2-files))
  (invoke "mvn" "-v")
  #t)

(define (fix-pom pom-file inputs local-packages excludes)
  (chmod pom-file #o644)
  (format #t "fixing ~a~%" pom-file)
  (fix-pom-dependencies pom-file (map cdr inputs)
                        #:with-plugins? #t #:with-build-dependencies? #t
                        #:with-modules? #t
                        #:local-packages local-packages
                        #:excludes excludes))

(define* (fix-pom-files #:key inputs local-packages exclude #:allow-other-keys)
  (let ((local-packages (pom-local-packages "pom.xml" #:local-packages local-packages)))
    (format (current-error-port) "Fix pom files with local packages: ~a~%" local-packages)
    (for-each
      (lambda (pom)
        (when (file-exists? pom)
          (fix-pom pom inputs local-packages exclude)))
      (pom-and-submodules "pom.xml")))
  #t)

(define* (build #:key outputs #:allow-other-keys)
  "Build the given package."
  (invoke "mvn" "package"
          ;; offline mode: don't download dependencies
          "-o"
          ;, set directory where dependencies are installed
          (string-append "-Duser.home=" (getenv "HOME")))
  #t)

(define* (check #:key tests? #:allow-other-keys)
  "Check the given package."
  (when tests?
    (invoke "mvn" "test"
            (string-append "-Duser.home=" (getenv "HOME"))
            "-e"))
  #t)

(define* (install #:key outputs #:allow-other-keys)
  "Install the given package."
  (let* ((out (assoc-ref outputs "out"))
         (java (string-append out "/lib/m2")))
    (invoke "mvn" "install" "-o" "-e"
            "-DskipTests"
            (string-append "-Duser.home=" (getenv "HOME")))
    ;; Go through the repository to find files that can be installed
    (with-directory-excursion (string-append (getenv "HOME") "/.m2/repository")
      (let ((installable
              (filter (lambda (file)
                        (not (eq? 'symlink (stat:type (lstat file)))))
                      (find-files "." "."))))
        (mkdir-p java)
        (for-each
          (lambda (file)
            (mkdir-p (string-append java "/" (dirname file)))
            (copy-file file (string-append java "/" file)))
          installable)))
    ;; Remove some files that are not required and introduce timestamps
    (for-each delete-file (find-files out "maven-metadata-local.xml"))
    (for-each delete-file (find-files out "_remote.repositories")))
  #t)

(define %standard-phases
  ;; Everything is as with the GNU Build System except for the `configure'
  ;; , `build', `check' and `install' phases.
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (add-before 'configure 'set-home set-home)
    (replace 'configure configure)
    (add-after 'configure 'fix-pom-files fix-pom-files)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))

(define* (maven-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; maven-build-system.scm ends here
