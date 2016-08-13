;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages benchmark)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux))

(define-public fio
  (package
    (name "fio")
    (version "2.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                       "http://brick.kernel.dk/snaps/"
                       "fio-" version ".tar.bz2"))
              (sha256
               (base32
                "10bn9jzks8mam5vz6falhr9pmd9wx248kh53vqp1bazbfxs3qgz0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; No tests.
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The configure script doesn't understand some of the
             ;; GNU options, so we can't use #:configure-flags.
             (let ((out (assoc-ref outputs "out")))
               (zero? (system* "./configure"
                               (string-append "--prefix=" out)))))))))
    (inputs
     `(("libaio" ,libaio)
       ("zlib" ,zlib)))
    (home-page "https://github.com/axboe/fio")
    (synopsis "Flexible I/O tester")
    (description
     "fio is a tool that will spawn a number of threads or processes doing a
particular type of I/O action as specified by the user.  The typical use of fio
is to write a job file matching the I/O load one wants to simulate.")
    ;; The software is distributed under the GPL2, but a handful of components
    ;; are covered by other licenses.
    (license (list license:gpl2 license:gpl2+ license:bsd-2
                   license:public-domain))))
