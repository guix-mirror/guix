;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages pumpio)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web))

(define-public pumpa
  (package
    (name "pumpa")
    (version "0.9.2")
    (source (origin
              (method git-fetch) ; no source tarballs
              (uri (git-reference
                    (url "git://pumpa.branchable.com/")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "09www29s4ldvd6apr73w7r4nmq93rcl2d182fylwgfcnncbvpy8s"))
              (file-name (string-append name "-" version "-checkout"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-replace
                 'configure
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   ;; Fix dependency tests.
                   (substitute* "pumpa.pro"
                     (("/usr/include/tidy\\.h")
                      (string-append (assoc-ref inputs "tidy")
                                     "/include/tidy.h"))
                     (("/usr/include/aspell.h")
                      (string-append (assoc-ref inputs "aspell")
                                     "/include/aspell.h")))
                   ;; Run qmake with proper installation prefix.
                   (let ((prefix (string-append "PREFIX="
                                                (assoc-ref outputs "out"))))
                     (zero? (system* "qmake" prefix))))
                 %standard-phases)))
    (inputs
     `(("aspell" ,aspell)
       ("qt" ,qt)
       ("qjson" ,qjson)
       ("tidy" ,tidy)))
    (synopsis "Qt-based pump.io client")
    (description "Pumpa is a simple pump.io client written in C++ and Qt.")
    (home-page "https://pumpa.branchable.com/")
    (license gpl3+)))
