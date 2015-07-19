;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

(define-module (gnu packages markdown)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages python)
  #:use-module (gnu packages web))

(define-public hoedown
  (package
    (name "hoedown")
    (version "3.0.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/hoedown/hoedown/archive/"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "0mmmkfayqgh6k39kbi3pq68mg03x35aiygy3zypxzvwx9y8b53ky"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'fix-makefile
                              (lambda* (#:key outputs #:allow-other-keys)
                                (setenv "CC" "gcc")
                                (substitute* '("Makefile")
                                  (("/usr/local")
                                   (assoc-ref outputs "out")))
                                #t))
                  (delete 'configure)) ; no configure script
       #:test-target "test"))
    (native-inputs
     `(("python" ,python-2)
       ("tidy" ,tidy)))
    (synopsis "Markdown processing library")
    (description "Hoedown is a standards compliant, fast, secure markdown
processing library written in C.")
    (home-page "https://github.com/hoedown/hoedown")
    (license expat)))
