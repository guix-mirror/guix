;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Kei Kebreau <kei@openmailbox.org>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages image)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

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

(define-public links
  (package
    (name "links")
    (version "2.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://links.twibright.com/download/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1f24y83wa1vzzjq5kp857gjqdpnmf8pb29yw7fam0m8wxxw0c3gp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The tarball uses a very old version of autconf. It doesn't
             ;; understand extra flags like `--enable-fast-install', so
             ;; we need to invoke it with just what it understands.
             (let ((out (assoc-ref outputs "out")))
               ;; 'configure' doesn't understand '--host'.
               ,@(if (%current-target-system)
                     `((setenv "CHOST" ,(%current-target-system)))
                     '())
               (setenv "CONFIG_SHELL" (which "bash"))
               (zero?
                (system* "./configure"
                         (string-append "--prefix=" out)
                         "--enable-graphics"))))))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("zlib" ,zlib)
              ("openssl" ,openssl)
              ("libjpeg" ,libjpeg)
              ("libtiff" ,libtiff)
              ("libevent" ,libevent)
              ("libpng" ,libpng)
              ("libxt" ,libxt)))
    (synopsis "Text and graphics mode web browser")
    (description "Links is a graphics and text mode web browser, with many
features including, tables, builtin image display, bookmarks, SSL and more.")
    (home-page "http://links.twibright.com")
    ;; The distribution contains a copy of GPLv2
    ;; However, the copyright notices simply say:
    ;; "This file is a part of the Links program, released under GPL."
    ;; Therefore, under the provisions of Section 9, we can choose
    ;; any version ever published by the FSF.
    ;; One file (https.c) contains an exception permitting
    ;; linking of the program with openssl.
    (license license:gpl1+)))

(define-public qutebrowser
  (package
    (name "qutebrowser")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/The-Compiler/"
                           "qutebrowser/releases/download/v" version "/"
                           "qutebrowser-" version ".tar.gz"))
       (sha256
        (base32
         "05qryn56w2pbqhir4pl99idx7apx2xqw9f8wmbrhj59b1xgr3x2p"))))
    (build-system python-build-system)
    (native-inputs
     `(("asciidoc" ,asciidoc)))
    (inputs
     `(("python-colorama" ,python-colorama)
       ("python-cssutils" ,python-cssutils)
       ("python-jinja2" ,python-jinja2)
       ("python-markupsafe" ,python-markupsafe)
       ("python-pygments" ,python-pygments)
       ("python-pypeg2" ,python-pypeg2)
       ("python-pyyaml" ,python-pyyaml)
       ("python-pyqt" ,python-pyqt)
       ("qtwebkit" ,qtwebkit)))
    (arguments
     `(#:tests? #f                      ;no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (app (string-append out "/share/applications"))
                    (hicolor (string-append out "/share/icons/hicolor")))
               (system* "a2x" "-f" "manpage" "doc/qutebrowser.1.asciidoc")
               (install-file "doc/qutebrowser.1"
                             (string-append out "/share/man/man1"))

               (for-each
                (lambda (i)
                  (let ((src  (format #f "icons/qutebrowser-~dx~d.png" i i))
                        (dest (format #f "~a/~dx~d/apps/qutebrowser.png"
                                      hicolor i i)))
                    (mkdir-p (dirname dest))
                    (copy-file src dest)))
                '(16 24 32 48 64 128 256 512))
               (install-file "icons/qutebrowser.svg"
                             (string-append hicolor "/scalable/apps"))
               
               (substitute* "qutebrowser.desktop"
                 (("Exec=qutebrowser")
                  (string-append "Exec=" out "/bin/qutebrowser")))
               (install-file "qutebrowser.desktop" app)
               #t))))))
    (home-page "https://qutebrowser.org/")
    (synopsis "Minimal, keyboard-focused, vim-like web browser")
    (description "qutebrowser is a keyboard-focused browser with a minimal
GUI.  It is based on PyQt5 and QtWebKit.")
    (license license:gpl3+)))
