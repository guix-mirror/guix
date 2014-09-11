;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Kevin Lemonnier <lemonnierk@ulrar.net>
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

;; TODO: Add ruby

(define-module (gnu packages weechat)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages file)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:select (gpl3)))

(define-public weechat
  (package
    (name "weechat")
    (version "1.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://weechat.org/files/src/weechat-"
                                  version ".tar.gz"))
             (sha256
              (base32 "1z17wyrl5fp697qp44srpmzk79w37f5hm1r0krffbmga6sbzdj3x"))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,autoconf)
                     ("pkg-config" ,pkg-config)
                     ("file" ,file)
                     ("autogen" ,autogen)
                     ("automake" ,automake)))
    (inputs `(("ncurses" ,ncurses)
              ("diffutils" ,diffutils)
              ("gettext" ,gnu-gettext)
              ("libtool" ,libtool "bin")
              ("libtool" ,libtool "out")
              ("libgcrypt" ,libgcrypt "out")
              ("zlib" ,zlib)
              ("aspell" ,aspell)
              ("curl" ,curl)
              ("gnutls" ,gnutls)
              ("guile" ,guile-2.0)
              ("openssl" ,openssl)
              ("cyrus-sasl" ,cyrus-sasl)
              ("lua" ,lua-5.1)
              ("python" ,python-2)
              ("perl" ,perl)
              ("tcl" ,tcl)))
    (arguments `(#:configure-flags (list
                                    (string-append
                                     "--with-tclconfig="
                                     (assoc-ref %build-inputs "tcl") "/lib"))
                 #:phases (alist-cons-after
                           'autogen 'fix-file
                           (lambda _
                             (substitute* "configure"
                               (("/usr/bin/file") (which "file"))))
                           (alist-cons-before
                            'configure 'autogen
                            (lambda _
                              (zero? (system* "./autogen.sh")))
                            %standard-phases))))
    (synopsis "Extensible chat client")
    (description "WeeChat (Wee Enhanced Environment for Chat) is an
Internet Relay Chat client, which is designed to be light and fast.
The client uses a curses frontend, and there are remote interfaces
for Web, Qt, Android and Emacs.  In WeeChat everything can be done
with a keyboard, though it also supports mouse.  It is customizable
and extensible with plugins and scripts.")
    (home-page "http://www.weechat.org/")
    (license gpl3)))
