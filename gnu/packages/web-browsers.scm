;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Kei Kebreau <kei@openmailbox.org>
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

(define-module (gnu packages web-browsers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages image)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public dillo
  (package
    (name "dillo")
    (version "3.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.dillo.org/download/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "12ql8n1lypv3k5zqgwjxlw1md90ixz3ag6j1gghfnhjq3inf26yv"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags '("--enable-ssl" "--enable-ipv6")))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("fltk" ,fltk)
              ("fontconfig" ,fontconfig)
              ("libjpeg" ,libjpeg)
              ("libpng" ,libpng)
              ("libxcursor" ,libxcursor)
              ("libxft" ,libxft)
              ("libxi" ,libxi)
              ("libxinerama" ,libxinerama)
              ("openssl" ,openssl)
              ("perl" ,perl)
              ("zlib" ,zlib)))
    (synopsis "Very small and fast graphical web browser")
    (description "Dillo is a minimalistic web browser particularly intended for
older or slower computers and embedded systems.")
    (home-page "http://www.dillo.org")
    (license license:gpl3+)))

(define-public netsurf
  (package
    (name "netsurf")
    (version "3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.netsurf-browser.org/netsurf/"
                           "releases/source/netsurf-" version "-src.tar.gz"))
       (sha256
        (base32
         "174sjx0566agckwmlj4w2cip5qbxdiafyhlp185a1qprxx84pbjr"))
       (patches (search-patches "netsurf-system-utf8proc.patch"
                                "netsurf-y2038-tests.patch"
                                "netsurf-longer-test-timeout.patch"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("netsurf-buildsystem" ,netsurf-buildsystem)
       ("nsgenbind" ,nsgenbind)
       ("libidn" ,libidn)               ;only for tests
       ("check" ,check)
       ("perl" ,perl)
       ("perl-html-parser" ,perl-html-parser)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("gtk+" ,gtk+-2)
       ("openssl" ,openssl)
       ("utf8proc" ,utf8proc)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg)
       ("libcss" ,libcss)
       ("libdom" ,libdom)
       ("libnsbmp" ,libnsbmp)
       ("libnsgif" ,libnsgif)
       ("libnspsl" ,libnspsl)
       ("libnsutils" ,libnsutils)
       ("libsvgtiny" ,libsvgtiny)
       ("miscfiles" ,miscfiles)))
    (arguments
     `(#:make-flags `("CC=gcc" "BUILD_CC=gcc"
                      ,(string-append "PREFIX=" %output)
                      ,(string-append "NSSHARED="
                                      (assoc-ref %build-inputs
                                                 "netsurf-buildsystem")
                                      "/share/netsurf-buildsystem"))
       #:test-target "test"
       #:modules ((ice-9 rdelim)
                  (ice-9 match)
                  (srfi srfi-1)
                  (sxml simple)
                  ,@%glib-or-gtk-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'build 'adjust-welcome
           (lambda _
             ;; First, fix some unended tags and simple substitutions
             (substitute* "frontends/gtk/res/welcome.html"
               (("<(img|input)([^>]*)>" _ tag contents)
                (string-append "<" tag contents " />"))
               (("Licence") "License") ;prefer GNU spelling
               ((" open source") ", free software")
               (("web&nbsp;site") "website")
               ;; Prefer privacy-respecting default search engine
               (("www.google.co.uk") "www.duckduckgo.com/html")
               (("Google Search") "DuckDuckGo Search")
               (("name=\"btnG\"") ""))
             ;; Remove default links so it doesn't seem we're endorsing them
             (with-atomic-file-replacement "frontends/gtk/res/welcome.html"
               (lambda (in out)
                 ;; Leave the DOCTYPE header as is
                 (display (read-line in 'concat) out)
                 (sxml->xml
                  (let rec ((sxml (xml->sxml in)))
                    ;; We'd like to use sxml-match here, but it can't
                    ;; match against generic tag symbols...
                    (match sxml
                      (`(div (@ (class "links")) . ,rest)
                       '())
                      ((x ...)
                       (map rec x))
                      (x x)))
                  out)))
             #t))
         (add-before 'check 'patch-check
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("test/bloom.c" "test/hashtable.c")
               (("/usr/share/dict/words")
                (string-append (assoc-ref inputs "miscfiles") "/share/web2")))
             #t))
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (desktop (string-append out "/share/applications/"
                                            "netsurf.desktop")))
               (mkdir-p (dirname desktop))
               (copy-file "frontends/gtk/res/netsurf-gtk.desktop"
                          desktop)
               (substitute* desktop
                 (("netsurf-gtk") (string-append out "/bin/netsurf"))
                 (("netsurf.png") (string-append out "/share/netsurf/"
                                                 "netsurf.xpm")))
               (install-file "Docs/netsurf-gtk.1"
                             (string-append out "/share/man/man1/"))
               #t))))))
    (home-page "http://www.netsurf-browser.org")
    (synopsis "Web browser")
    (description
     "NetSurf is a lightweight web browser that has its own layout and
rendering engine entirely written from scratch.  It is small and capable of
handling many of the web standards in use today.")
    (license license:gpl2+)))
