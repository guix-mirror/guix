;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages serveez)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages guile))

(define-public serveez
  (package
    (name "serveez")
    (version "0.3.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/serveez/serveez-"
                          version ".tar.lz"))
      (sha256
       (base32
        "0capm8i6fyjvh5rflxkfd07llfaa0kk4i3y836qpld9vzy49q6ky"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-libserveez-install")
       #:phases
       (modify-phases %standard-phases
         (add-before 'patch-source-shebangs 'patch-test-source
           (lambda _
             (substitute*
                 (find-files "test" "^t[0-9]{3}$")
               (("/bin/sh") (which "sh")))
             #t)))))
    (native-inputs
     (list lzip))
    (inputs
     (list guile-2.2))
    (home-page "https://www.gnu.org/software/serveez/")
    (synopsis "Framework for implementing IP-based servers")
    (description
     "GNU Serveez is a server framework providing the routines necessary to
easily implement IP-based servers in your application.  It
demonstrates aspects of network programming in a portable manner,
making it convenient for both simplifying the process of adding a
server to your application or for learning about how network services
work.  Several example servers are provided already, such as an HTTP
server and an IRC server.")
    (license gpl3+)))
