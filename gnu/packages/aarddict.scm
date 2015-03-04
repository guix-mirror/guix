;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages aarddict)
  #:use-module ((guix licenses) #:select (gpl3))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt))

(define-public aarddict
  (package
    (name "aarddict")
    (version "0.9.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/aarddict/desktop/archive/"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "12h7m0z7nd7rg8avpi9syd265k0rhh4vbdh464nq0jzdg8m9p28c"))))
    (build-system python-build-system)
    (inputs
     `(("python2-pyicu" ,python2-pyicu)
       ("python2-pyqt-4" ,python2-pyqt-4)
       ("python2-setuptools" ,python2-setuptools)
       ("python2-simplejson" ,python2-simplejson)
       ("python2-sip" ,python2-sip)))
    (arguments
     `(#:python ,python-2 ; incompatible with Python 3
       #:phases
        (alist-cons-before
         'build 'configure
         ;; Force data into the output instead of the python package.
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (substitute* "setup.py"
               (("sys.prefix") (string-append "'" out "'")))))
         %standard-phases)))
    (home-page "http://aarddict.org/index.html")
    (synopsis
     "Dictionary program and offline Wikipedia reader")
    (description
     "Aard Dictionary is a free, fast, easy to use word lookup program that
looks up words fast even with huge dictionaries like English Wikipedia;
looks up words in multiple dictionaries in multiple languages without
switching;
works great as offline Wikipedia reader;
is keyboard navigation friendly;
has efficient, highly compressed dictionary data storage format with
ability to verify data integrity built-in.")
    (license gpl3)))
