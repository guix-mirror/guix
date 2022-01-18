;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
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

(define-module (guix build clojure-build-system)
  #:use-module ((guix build ant-build-system)
                #:select ((%standard-phases . %standard-phases@ant)
                          ant-build))
  #:use-module (guix build clojure-utils)
  #:use-module (guix build java-utils)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            clojure-build))

;; Commentary:
;;
;; Builder-side code of the standard build procedure for Clojure packages.
;;
;; Code:

(define* (compile-java #:key
                       java-source-dirs java-compile-dir
                       #:allow-other-keys)
  "Compile java sources for use in clojure-build-system."
  (let ((java-files (append-map (lambda (dir)
                                  (find-files dir "\\.java$"))
                                java-source-dirs)))
    (mkdir-p java-compile-dir)
    (when (not (null? java-files))
      (apply invoke
             "javac"
             "-verbose"
             "-d" java-compile-dir
             java-files))))

(define* (build #:key
                source-dirs java-source-dirs
                compile-dir java-compile-dir
                jar-names main-class omit-source?
                aot-include aot-exclude
                #:allow-other-keys)
  "Standard 'build' phase for clojure-build-system."
  (let* ((libs (append-map find-clojure-libs source-dirs))
         (libs* (include-list\exclude-list aot-include
                                           aot-exclude
                                           #:all-list libs)))
    (mkdir-p compile-dir)
    (eval-with-clojure `(run! compile ',libs*)
                       (cons*  compile-dir
                               java-compile-dir
                               source-dirs))
    (let ((source-dir-files-alist (map (lambda (dir)
                                         (cons dir (find-files* dir)))
                                       (append source-dirs
                                           java-source-dirs)))
          ;; workaround transitive compilation in Clojure
          (classes (filter (lambda (class)
                             (any (cut compiled-from? class <>)
                                  libs*))
                           (find-files* compile-dir))))
      (for-each (cut create-jar <> (cons* (cons compile-dir classes)
                                          (cons java-compile-dir
                                                (find-files* java-compile-dir))
                                          (if omit-source?
                                              '()
                                              source-dir-files-alist))
                     #:main-class main-class)
                jar-names)
      #t)))

(define* (check #:key
                test-dirs
                jar-names
                tests? test-include test-exclude
                #:allow-other-keys)
  "Standard 'check' phase for clojure-build-system.  Note that TEST-EXCLUDE has
priority over TEST-INCLUDE."
  (if tests?
      (let* ((libs (append-map find-clojure-libs test-dirs))
             (libs* (include-list\exclude-list test-include
                                               test-exclude
                                               #:all-list libs)))
        (for-each (lambda (jar)
                    (eval-with-clojure `(do (apply require
                                                   '(clojure.test ,@libs*))
                                            (if (clojure.test/successful?
                                                 (apply clojure.test/run-tests
                                                        ',libs*))
                                                (System/exit 0)
                                                (System/exit 1)))
                                       (cons jar test-dirs)))
                  jar-names)))
  #t)

(define-with-docs install
  "Standard 'install' phase for clojure-build-system."
  (install-jars "./"))

(define-with-docs %standard-phases
  "Standard build phases for clojure-build-system."
  (modify-phases %standard-phases@ant
    (add-before 'build 'compile-java compile-java)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'install-license-files 'install-doc install-doc)))

(define* (clojure-build #:key
                        inputs
                        (phases %standard-phases)
                        #:allow-other-keys
                        #:rest args)
  "Build the given Clojure package, applying all of PHASES in order."
  (apply ant-build
         #:inputs inputs
         #:phases phases
         args))

;;; clojure-build-system.scm ends here
