;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
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

(define-module (gnu packages nim)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public nim
  (package
    (name "nim")
    (version "0.16.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://nim-lang.org/download/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "0rsibhkc5n548bn9yyb9ycrdgaph5kq84sfxc9gabjs7pqirh6cy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests.
       #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'patch-installer
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "install.sh"
                   (("1/nim") "1"))
                 #t)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (zero? (system* "./install.sh" out))))))))
    (home-page "http://nim-lang.org")
    (synopsis "Statically-typed, imperative programming language")
    (description "Nim (formerly known as Nimrod) is a statically-typed,
imperative programming language that tries to give the programmer ultimate power
without compromises on runtime efficiency.  This means it focuses on compile-time
mechanisms in all their various forms.")
    (license license:expat)))
