;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages mastodon)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages time)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

(define-public toot
  (package
    (name "toot")
    (version "0.27.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "toot" version))
        (sha256
         (base32 "1mfbqmgna7046d134pc5qx1vyfd60vwcn0xr9lxzlmc5rjdbmz8x"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "py.test"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-requests" ,python-requests)
       ("python-urwid" ,python-urwid)
       ("python-wcwidth" ,python-wcwidth)))
    (home-page "https://github.com/ihabunek/toot/")
    (synopsis "Mastodon CLI client")
    (description "Interact with Mastodon social network from the command line.
Features include:
@itemize
@item Posting, replying, deleting statuses
@item Support for media uploads, spoiler text, sensitive content
@item Search by account or hash tag
@item Following, muting and blocking accounts
@item Simple switching between authenticated in Mastodon accounts
@end itemize")
    (license license:gpl3)))

(define-public python-mastodon-py
  (package
    (name "python-mastodon-py")
    (version "1.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Mastodon.py" version))
        (sha256
         (base32
          "1vikvkzcij2gd730cssigxi38vlmzqmwdy58r3y2cwsxifnxpz9a"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-blurhash" ,python-blurhash)
       ("python-dateutil" ,python-dateutil)
       ("python-decorator" ,python-decorator)
       ("python-magic" ,python-magic)
       ("python-pytz" ,python-pytz)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-blurhash" ,python-blurhash)
       ("python-cryptography" ,python-cryptography)
       ("python-http-ece" ,python-http-ece)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-mock" ,python-pytest-mock)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-pytest-vcr" ,python-pytest-vcr)
       ("python-requests-mock" ,python-requests-mock)
       ("python-vcrpy" ,python-vcrpy)))
    (home-page "https://github.com/halcy/Mastodon.py")
    (synopsis "Python wrapper for the Mastodon API")
    (description
     "This package provides a python wrapper for the Mastodon API.")
    (license license:expat)))
