;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages syndication)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public newsboat
  (package
    (name "newsboat")
    (version "2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://newsboat.org/releases/" version
                           "/newsboat-" version ".tar.xz"))
       (sha256
        (base32
         "0pik1d98ydzqi6055vdbkjg5krwifbk2hy2f5jp5p1wcy2s16dn7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ;; For building documentation.
       ("asciidoc" ,asciidoc)))
    (inputs
     `(("curl" ,curl)
       ("json-c" ,json-c)
       ("libxml2" ,libxml2)
       ("ncurses" ,ncurses)
       ("stfl" ,stfl)
       ("sqlite" ,sqlite)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'build 'build-documentation
           (lambda _
             (invoke "make" "doc"))))
       #:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:test-target "test"))
    (native-search-paths
     ;; Newsboat respects CURL_CA_BUNDLE.
     (package-native-search-paths curl))
    (home-page "https://newsboat.org/")
    (synopsis "Text-mode RSS and Atom feed reader with podcast support")
    (description "Newsboat is a feed reader for @dfn{RSS} and @dfn{Atom}, XML
formats widely used to transmit, publish, and syndicate news or blog articles.
It's designed for use on text terminals, and to have a coherent and easy-to-use
interface that might look familiar to @command{mutt} or @command{slrn} users.

Newsboat supports OPML import/exports, HTML rendering, podcasts (with
@command{podboat}), off-line reading, searching and storing articles to your
file system, and many more features.

It started life as a fork of the currently unmaintained Newsbeuter.")
    (license (list license:gpl2+        ; filter/*
                   license:expat))))    ; everything else

(define-public newsbeuter
  ;; Newsbeuter is unmaintained with multiple CVEs, and was forked as Newsboat.
  (deprecated-package "newsbeuter" newsboat))

(define-public rtv
  (package
    (name "rtv")
    (version "1.26.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rtv" version))
        (sha256
         (base32
          "1aamkli1mlq2vxixlva790y0l0cbvbkz07lknajin0841sdq0411"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-environment-variables
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "HOME" (getcwd))
             (setenv "TERM" "linux")
             (setenv "TERMINFO" (string-append (assoc-ref inputs "ncurses")
                                               "/share/terminfo"))
             #t)))
       #:tests? #f)) ; tests fail: _curses.error: nocbreak() returned ERR
    (propagated-inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-decorator" ,python-decorator)
       ("python-kitchen" ,python-kitchen)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (native-inputs
     `(("ncurses" ,ncurses)
       ("python-coveralls" ,python-coveralls)
       ("python-coverage" ,python-coverage)
       ("python-mock" ,python-mock)
       ("python-pylint" ,python-pylint)
       ("python-pytest" ,python-pytest)
       ("python-vcrpy" ,python-vcrpy)))
    (home-page "https://github.com/michael-lazar/rtv")
    (synopsis "Terminal viewer for Reddit (Reddit Terminal Viewer)")
    (description
     "RTV provides a text-based interface to view and interact with Reddit.")
    (license (list license:expat
                   license:gpl3+)))) ; rtv/packages/praw
