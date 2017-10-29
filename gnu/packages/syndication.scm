;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages web)
  #:use-module (srfi srfi-1))

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
       ("json-c" ,json-c)
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
    (license (list license:gpl2+ ; filter/*
                   license:x11))))

(define-public newsboat
  (package
    (inherit newsbeuter)
    (name "newsboat")
    (version "2.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://newsboat.org/releases/" version
                            "/newsboat-" version ".tar.xz"))
        (sha256
         (base32
          "1xgqkhpjbq916g9hkaxs0s2fz8bg103pzjx75ziq5ba688l9imj4"))))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure) ; no configure
                  (add-after 'build 'build-documentation
                    (lambda _ (zero? (system* "make" "doc")))))
       #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out"))
                          ;; see https://github.com/newsboat/newsboat/issues/43
                          "WARNFLAGS=-Wno-sign-compare")
       #:test-target "test"))
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ,@(alist-delete "ruby" (package-native-inputs newsbeuter))))
    (home-page "https://newsboat.org/")
    (description "Newsboat is a fork of Newsbeuter, an RSS/Atom feed reader for
the text console.  It supports OPML import/exports, HTML rendering, podcast
(podboat), offline reading, searching and storing articles to your filesystem,
and many more features.  Its user interface is coherent, easy to use, and might
look common to users of @command{mutt} and @command{slrn}.")))
