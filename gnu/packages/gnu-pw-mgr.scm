;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages gnu-pw-mgr)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages which))

(define-public gnu-pw-mgr
  (package
    (name "gnu-pw-mgr")
    (version "1.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnu-pw-mgr/gnu-pw-mgr-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0sn9gzngqkrv74iwxzn5ldqx3w73w9paldcdh8rsv9yvgarv2bm4"))))
    (build-system gnu-build-system)
    (inputs `(("which" ,which)))
    (home-page "http://www.gnu.org/software/gnu-pw-mgr/")
    (synopsis "Retrieve login credentials without recording passwords")
    (description
     "This program is designed to make it easy to reconstruct difficult
passwords when they are needed while limiting the risk of attack.  The
user of this program inputs a self-defined transformation of a web
site URL and obtains the password and user name hint for that web
site.")
    (license gpl3+)))
