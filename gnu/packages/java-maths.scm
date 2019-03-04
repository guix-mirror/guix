;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages java-maths)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ruby))

(define-public java-jblas
  (package
    (name "java-jblas")
    (version "1.2.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mikiobraun/jblas.git")
                    (commit (string-append "jblas-" version))))
              (sha256
               (base32
                "0afh48hq8i8li5z11x415c8slwsfrlib0w1xjfbg186mximqvv3g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are none
       #:configure-flags
       (list (string-append "--libpath="
                            (assoc-ref %build-inputs "openblas")
                            "/lib")
             "--build-type=openblas")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-writable
           (lambda _
             (map make-file-writable
                  (find-files "." ".*"))
             #t))
         (add-before 'build 'setenv-JAVA_HOME
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
             #t))
         (add-after 'build 'build-jar
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "M2_HOME" (assoc-ref outputs "out"))
             (invoke "ant" "jar" (string-append "-Dversion=" ,version))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/share/java")))
               (mkdir-p target)
               (install-file (string-append "jblas-" ,version ".jar") target))
             #t)))))
    (inputs
     `(("openblas" ,openblas)))
    (native-inputs
     `(("ant" ,ant)
       ("ruby" ,ruby)                   ; for configure script
       ("gfortran" ,gfortran)
       ("jdk" ,icedtea "jdk")))
    (home-page "http://jblas.org")
    (synopsis "Linear algebra for Java")
    (description
     "jblas is a fast linear algebra library for Java.  jblas is based on BLAS
and LAPACK, the de-facto industry standard for matrix computations, and uses
state-of-the-art implementations for all its computational routines, making
jBLAS very fast.")
    (license license:bsd-3)))
