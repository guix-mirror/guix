;;; GNU Guix --- Functional package management for GNU
;;; Copyright  2014 John Darrington <jmd@gnu.org>
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

(define-module (gnu packages links)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages image)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public links
  (package
    (name "links")
    (version "2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://links.twibright.com/download/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32 "15h07498z52jfdahzgvkphg1f7qvxnpbyfn2xmsls0d2dwwdll3r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; The tarball uses a very old version of autconf. It doesn't understand
                   ;; extra flags like `--enable-fast-install', so we need to
                   ;; invoke it with just what it understand.
                   (let ((out (assoc-ref outputs "out")))
                     ;; 'configure' doesn't understand '--host'.
                     ,@(if (%current-target-system)
                           `((setenv "CHOST" ,(%current-target-system)))
                           '())
                     (setenv "CONFIG_SHELL" (which "bash"))
                     (zero?
                      (system* "./configure"
                               (string-append "--prefix=" out)
                               "--enable-graphics"
                               ))))
                 %standard-phases)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("zlib" ,zlib)
              ("openssl" ,openssl)
              ("libjpeg" ,libjpeg)
              ("libtiff" ,libtiff)
              ("libpng" ,libpng)
              ("libxt" ,libxt)))
    (synopsis "Text and graphics mode web browser")
    (description "Links is a graphics and text mode web browser, with many
features including, tables, builtin image display, bookmarks, SSL and more.")
    (home-page "http://links.twibright.com")
    ;;   The distribution contains a copy of GPLv2
    ;;   However, the copyright notices simply say: 
    ;; "This file is a part of the Links program, released under GPL."
    ;; Therefore, under the provisions of Section 9, we can choose
    ;; any version ever published by the FSF
    ;;   One file (https.c) contains an exception permitting 
    ;; linking of the program with openssl
    (license license:gpl1+)))  



