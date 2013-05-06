;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages help2man)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl))

(define-public help2man
  (package
    (name "help2man")
    (version "1.41.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/help2man/help2man-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1as014d8wxy7529fr8n2ihy61c2p5jzywzirkqggvz8lwg6r933a"))))
    (build-system gnu-build-system)
    (arguments `(;; There's no `check' target.
                 #:tests? #f))
    (inputs
     `(("perl" ,perl)
       ;; TODO: Add these optional dependencies.
       ;; ("perl-LocaleGettext" ,perl-LocaleGettext)
       ;; ("gettext" ,gettext)
       ))
    (home-page "http://www.gnu.org/software/help2man/")
    (synopsis "Automatically generate man pages from program --help")
    (description
     "help2man produces simple manual pages from the ‘--help’ and
‘--version’ output of other commands.")
    (license gpl3+)))
