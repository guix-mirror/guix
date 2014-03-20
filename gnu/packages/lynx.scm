;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages lynx)
  #:use-module ((guix licenses) #:select (gpl2))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages compression))

(define-public lynx
  (package
    (name "lynx")
    (version "2.8.8rel.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://lynx.isc.org/lynx"
                    (substring version 0 (string-index version char-set:letter))
                    "/lynx" version ".tar.bz2"))
              (sha256
               (base32 "1rxysl08acqll5b87368f04kckl8sggy1qhnq59gsxyny1ffg039"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("perl" ,perl)))
    (inputs `(("ncurses" ,ncurses)
              ("libidn" ,libidn)
              ("gnutls" ,gnutls)
              ("libgcrypt" ,libgcrypt)
              ("unzip" ,unzip)
              ("zlib" ,zlib)
              ("gzip" ,gzip)
              ("bzip2" ,bzip2)))
    (arguments
     `(#:configure-flags '("--with-pkg-config"
                           "--with-screen=ncurses"
                           "--with-zlib"
                           "--with-bzlib"
                           "--with-gnutls"
                           ;; "--with-socks5"    ; XXX TODO
                           "--enable-widec"
                           "--enable-ascii-ctypes"
                           "--enable-local-docs"
                           "--enable-htmlized-cfg"
                           "--enable-gzip-help"
                           "--enable-nls"
                           "--enable-ipv6")
       #:tests? #f  ; no check target
       #:phases (alist-replace
                 'install
                 (lambda* (#:key (make-flags '()) #:allow-other-keys)
                   (zero? (apply system* "make" "install-full" make-flags)))
                 %standard-phases)))
    (synopsis "Text Web Browser")
    (description
     "Lynx is a fully-featured World Wide Web (WWW) client for users running
cursor-addressable, character-cell display devices.  It will display Hypertext
Markup Language (HTML) documents containing links to files on the local
system, as well as files on remote systems running http, gopher, ftp, wais,
nntp, finger, or cso/ph/qi servers.  Lynx can be used to access information on
the WWW, or to build information systems intended primarily for local
access.")
    (home-page "http://lynx.isc.org/")
    (license gpl2)))

;;; lynx.scm ends here
