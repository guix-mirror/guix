;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web))

(define-public newsbeuter
  (package
    (name "newsbeuter")
    (version "2.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://newsbeuter.org/downloads/newsbeuter-"
                            version ".tar.gz"))
        (patches (search-patches "newsbeuter-CVE-2017-12904.patch"
                                 "newsbeuter-CVE-2017-14500.patch"))
        (sha256
         (base32
          "1j1x0hgwxz11dckk81ncalgylj5y5fgw5bcmp9qb5hq9kc0vza3l"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* "config.sh"
               ;; try to remove this at the next release
               (("ncursesw5") "ncursesw6"))
             #t)))
       #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:test-target "test"))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("ruby" ,ruby))) ; for tests
    (inputs
     `(("curl" ,curl)
       ("json-c" ,json-c-0.12)      ; check whether json-c-0.12 can be removed
       ("ncurses" ,ncurses)
       ("stfl" ,stfl)
       ("sqlite" ,sqlite)
       ("libxml2" ,libxml2)))
    (home-page "https://newsbeuter.org/")
    (synopsis "Text mode rss feed reader with podcast support")
    (description "Newsbeuter is an innovative RSS feed reader for the text
console.  It supports OPML import/exports, HTML rendering, podcast (podbeuter),
offline reading, searching and storing articles to your filesystem, and many
more features.  Its user interface is coherent, easy to use, and might look
common to users of @command{mutt} and @command{slrn}.")
    (license (list license:gpl2+        ; filter/*
                   license:expat))))    ; everything else

(define-public newsboat
  (package
    (name "newsboat")
    (version "2.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://newsboat.org/releases/" version
                           "/newsboat-" version ".tar.xz"))
       (sha256
        (base32
         "1x4nxx1kvmrcm8jy73dvg56h4z15y3sach2vr13cw8rsbi7v99px"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ;; For building documentation.
       ("asciidoc" ,asciidoc)))
    (inputs
     `(("curl" ,curl)
       ("json-c" ,json-c-0.12)      ; check whether json-c-0.12 can be removed
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
             (zero? (system* "make" "doc")))))
       #:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:test-target "test"))
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
