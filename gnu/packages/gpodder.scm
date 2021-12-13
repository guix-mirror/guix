;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
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

(define-module (gnu packages gpodder)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xml))

(define-public gpodder
  (package
    (name "gpodder")
    (version "3.10.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gpodder/gpodder")
             (commit version)))
       (sha256
        (base32 "0n73jm5ypsj962gpr0dk10lqh83giqsczm63wchyhmrkyf1wgga1"))
       (file-name (git-file-name name version))
       (patches (search-patches "gpodder-disable-updater.patch"))))
    (build-system python-build-system)
    (native-inputs
     (list intltool
           python-coverage
           python-minimock
           python-pytest
           python-pytest-cov
           python-pytest-httpserver
           which))
    (inputs
     (list bash-minimal
           gtk+
           python-pygobject
           python-pycairo
           python-requests
           python-dbus
           python-html5lib
           python-mutagen
           python-mygpoclient
           python-podcastparser
           youtube-dl
           xdg-utils))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Avoid needing xdg-utils as a propagated input.
         (add-after 'unpack 'patch-xdg-open
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xdg-utils (assoc-ref inputs "xdg-utils")))
               (substitute* "src/gpodder/util.py"
                 (("xdg-open") (string-append xdg-utils "/bin/xdg-open")))
               #t)))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "make" "unittest"))))
         ;; 'msgmerge' introduces non-determinism by resetting the
         ;; POT-Creation-Date in .po files.
         (add-before 'install 'do-not-run-msgmerge
           (lambda _
             (substitute* "makefile"
               (("msgmerge") "true"))
             #t))
         (add-before 'install 'make-po-files-writable
           (lambda _
             (for-each
               (lambda (f)
                 (chmod f #o664))
               (find-files "po"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "PREFIX" (assoc-ref outputs "out"))
             (invoke "make" "install")))
         (add-after 'install 'wrap-gpodder
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/gpodder")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))
               #t))))))
    (home-page "https://gpodder.github.io")
    (synopsis "Simple podcast client")
    (description "gPodder is a podcatcher, i.e. an application that allows
podcast feeds (RSS, Atom, Youtube, Soundcloud, Vimeo and XSPF) to be
subscribed to, checks for new episodes and allows the podcast to be saved
locally for later listening.")
    (license license:gpl3+)))

(define-public libmygpo-qt
  (package
    (name "libmygpo-qt")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://stefan.derkits.at/files/"
                                  "libmygpo-qt/libmygpo-qt." version ".tar.gz"))
              (sha256
               (base32
                "1kg18qrq2rsswgzhl65r3mlyx7kpqg4wwnbp4yiv6svvmadmlxl2"))
              (patches (search-patches "libmygpo-qt-fix-qt-5.11.patch"
                                       "libmygpo-qt-missing-qt5-modules.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     `(("qt" ,qtbase-5)))
    (arguments
     `(#:configure-flags '("-DMYGPO_BUILD_TESTS=ON")
       ;; TODO: Enable tests when https://github.com/gpodder/gpodder/issues/446
       ;; is fixed.
       #:tests? #f))
    (home-page "https://gpodder.github.io")
    (synopsis "Qt/C++ library wrapping the gpodder web service")
    (description "@code{libmygpo-qt} is a Qt/C++ library wrapping the
@url{https://gpodder.net} APIs.  It allows applications to discover, manage
and track podcasts.")
    (license license:lgpl2.1+)))

(define-public python-mygpoclient
  (package
    (name "python-mygpoclient")
    (version "1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mygpoclient" version))
       (sha256
        (base32
         "1fi5x6k1mngr0iviw2s4n1f3y2x7pwqy5ivkcrjdprzvwr37f0mh"))))
    (build-system python-build-system)
    (native-inputs
     (list python-coverage python-minimock python-nose))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "make" "test"))))))
    (home-page "https://mygpoclient.readthedocs.io")
    (synopsis "Python library for the gPodder web service")
    (description "@code{mygpoclient} provides an easy and structured way to
access the @url{https://gpodder.net} web services.  In addition to
subscription list synchronization and storage, the API supports uploading and
downloading episode status changes.")
    (license license:gpl3+)))

(define-public python-podcastparser
  (package
    (name "python-podcastparser")
    (version "0.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "podcastparser" version))
       (sha256
        (base32 "0m24r2qhck0win44xfhxajbppkss4ha6am0042s0xyq3408883m3"))))
    (native-inputs
     (list python-pytest))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke "pytest"))))))
    (build-system python-build-system)
    (home-page "http://gpodder.org/podcastparser")
    (synopsis "Simplified and fast RSS parser Python library")
    (description "@code{podcastparser} is a library for the gPodder project to
provide an easy and reliable way of parsing RSS and Atom-based podcast feeds
in Python.")
    (license license:isc)))

(define-public castget
  (package
    (name "castget")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mlj/castget")
             (commit (string-append "rel_" (string-replace-substring
                                            version "." "_")))))
       (sha256
        (base32 "1129x64rw587q3sdpa3lrgs0gni5f0siwbvmfz8ya4zkbhgi2ik7"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config ronn-ng))
    (inputs (list curl glib id3lib libxml2))
    (synopsis "Command line podcast downloader")
    (description
     "castget is a simple, command-line based RSS enclosure downloader.  It is
primarily intended for automatic, unattended downloading of podcasts.  It uses
libcurl for the download process.")
    (license license:lgpl2.1+)
    (home-page "https://castget.johndal.com")))
