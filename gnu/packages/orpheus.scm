;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages orpheus)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages which)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml))

(define-public orpheus
  (package
    (name "orpheus")
    (version "1.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://thekonst.net/download/orpheus-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1xbgxq8fybwhm51nw9hvvrgi873qzkc2qvmy15d2m2hw2yqa99hq"))
      (patches (list (search-patch "orpheus-cast-errors-and-includes.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)
       ("libvorbis" ,libvorbis)
       ("vorbis-tools" ,vorbis-tools)
       ("mpg321" ,mpg321)
       ;; TODO: add ghttp
       ("libxml2" ,libxml2)
       ("which" ,which)))
    (arguments
     `(#:phases
       (alist-replace
        'configure
        (lambda* (#:key outputs #:allow-other-keys)
          ;; This old `configure' script does not support variables passed as
          ;; arguments.
          (let ((out (assoc-ref outputs "out")))
            (setenv "CONFIG_SHELL" (which "bash"))
            (setenv "SHELL" (which "bash"))
            (setenv "LIBS" "-logg")     ;doesn't declare its use of libogg
            (zero?
             (system* "./configure" (string-append "--prefix=" out)))))
        (alist-cons-after
         'configure 'configure-players
         (lambda* (#:key inputs #:allow-other-keys)
           ;; To avoid propagating the mpg321 and vorbis-tools inputs, we can
           ;; make the orpheus application execute the needed players from the
           ;; store.
           (let ((ogg123 (string-append (assoc-ref inputs "vorbis-tools")
                                        "/bin/ogg123"))
                 (mpg321 (string-append (assoc-ref inputs "mpg321")
                                        "/bin/mpg321"))
                 (which  (string-append (assoc-ref inputs "which")
                                        "/bin/which")))
             (substitute* "src/orpheusconf.cc"
               (("ogg123") ogg123)
               (("which")  which)
               (("mpg321") mpg321))))
         (alist-cons-before
          'build 'patch-shells
          (lambda _
            (substitute* '("src/mp3track.cc"
                           "src/streamtrack.cc"
                           "src/oggtrack.cc")
              (("/bin/sh") (which "bash"))))
          %standard-phases)))))
    (home-page "http://thekonst.net/en/orpheus")
    (synopsis "Text-mode audio player")
    (description
     "Orpheus is a light-weight text mode menu- and window-driven audio player
application for CDs, internet stream broadcasts, and files in MP3 and Vorbis
OGG format.")
    (license gpl2+)))
