;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Tanguy Le Carrour <tanguy@bioneland.org>
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

(define-module (gnu packages dav)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml))

(define-public radicale
  (package
    (name "radicale")
    (version "3.0.6")
    (source
     (origin
       ;; There are no tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Kozea/Radicale")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xlsvrmx6jhi71j6j8z9sli5vwxasivzjyqf8zq8r0l5p7350clf"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-pytest-flake8
           python-pytest-isort
           python-pytest-runner
           python-waitress))
    (propagated-inputs
     (list python-dateutil python-defusedxml python-passlib
           python-vobject))
    (synopsis "Basic CalDAV and CardDAV server")
    (description "Radicale is a CalDAV and CardDAV server for UNIX-like
platforms.  Calendars and address books are available for both local and remote
access, possibly limited through authentication policies.  They can be viewed
and edited by calendar and contact clients on mobile phones or computers.

Radicale intentionally does not fully comply with the CalDAV and CardDAV RFCs.
Instead, it supports the CalDAV and CardDAV implementations of popular
clients.")
    (home-page "https://radicale.org/")
    (license gpl3+)))

(define-public xandikos
  (package
    (name "xandikos")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xandikos" version))
       (sha256
        (base32 "13ikmcja9p42azb5ccqj2bw98zybna6zlflj10hqy0kvbib70l94"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-aiohttp
           python-defusedxml
           python-dulwich
           python-icalendar
           python-jinja2
           python-multidict))
    (home-page "https://www.xandikos.org/")
    (synopsis "Lightweight CalDAV/CardDAV server")
    (description
     "Xandikos is a lightweight yet complete CardDAV/CalDAV server that backs
onto a Git repository.

Features:

@itemize
@item Easy to set up
@item Share calendars (events, todo items, journal entries) via CalDAV and
contacts (vCard) via CardDAV
@item Automatically keep history and back up changes in Git
@item Supports synchronization extensions for CalDAV/CardDAV for quick and
efficient syncing
@item Automatically keep history and back up
@item Works with all tested CalDAV and CardDAV clients
@end itemize")
    (license gpl3+)))

(define-public vdirsyncer
  (package
    (name "vdirsyncer")
    ;; When updating, check whether python-click-5 can be removed entirely.
    (version "0.18.0")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri name version))
             (sha256
              (base32
               "00f2bw1a2jbbd1sbci0swnd67kylr341aa9rpbxkajbp3zakxg17"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; The test suite is very flakey.
       #:phases (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key inputs outputs tests? #:allow-other-keys)
            (add-installed-pythonpath inputs outputs)
            (setenv "DETERMINISTIC_TESTS" "true")
            (setenv "DAV_SERVER" "radicale")
            (setenv "REMOTESTORAGE_SERVER" "skip")
            (if tests?
                (invoke "make" "test")
                #t)))
        (add-after 'unpack 'patch-version-call
          (lambda _
            (substitute* "docs/conf.py"
              (("^release.*") (string-append "release = '" ,version "'\n")))
            #t))
        (add-after 'install 'manpage
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (invoke "make" "--directory=docs/" "man")
            (install-file
              "docs/_build/man/vdirsyncer.1"
              (string-append
                (assoc-ref outputs "out")
                "/share/man/man1"))
            #t)))))
    (native-inputs
     (list python-setuptools-scm
           python-sphinx
           ;; Required for testing
           python-hypothesis
           python-pytest
           python-pytest-localserver
           python-pytest-subtesthack
           python-urllib3
           python-wsgi-intercept
           radicale))
    (inputs
     (list ;; XXX https://github.com/mitsuhiko/click/issues/200
           python-click-5))
    (propagated-inputs
     (list python-atomicwrites python-click-log python-click-threading
           python-requests-toolbelt))
    (synopsis "Synchronize calendars and contacts")
    (description "Vdirsyncer synchronizes your calendars and addressbooks
between two storage locations.  The most popular purpose is to
synchronize a CalDAV or CardDAV server with a local folder or file.  The
local data can then be accessed via a variety of programs, none of which
have to know or worry about syncing to a server.")
    (home-page "https://github.com/pimutils/vdirsyncer")
    (license bsd-3)))
