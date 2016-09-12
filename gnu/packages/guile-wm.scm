;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Alex ter Weele <alex.ter.weele@gmail.com>
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

(define-module (gnu packages guile-wm)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public guile-xcb
  (package
    (name "guile-xcb")
    (version "1.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.markwitmer.com/dist/guile-xcb-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "04dvbqdrrs67490gn4gkq9zk8mqy3mkls2818ha4p0ckhh0pm149"))))
    (build-system gnu-build-system)
    (arguments '(;; Parallel builds fail.
                 #:parallel-build? #f

                 #:configure-flags (list (string-append
                                          "--with-guile-site-dir="
                                          (assoc-ref %outputs "out")
                                          "/share/guile/site/2.0")
                                         (string-append
                                          "--with-guile-site-ccache-dir="
                                          (assoc-ref %outputs "out")
                                          "/share/guile/site/2.0"))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("guile" ,guile-2.0)
              ("xcb" ,xcb-proto)))
    (home-page "http://www.markwitmer.com/guile-xcb/guile-xcb.html")
    (synopsis "XCB bindings for Guile")
    (description
     "Guile-XCB implements the XCB protocol and provides all the tools
necessary to write X client code in Guile Scheme without any external
dependencies.")
    (license gpl3+)))

(define-public guile-wm
  (package
    (name "guile-wm")
    (version "1.0")
    (synopsis "X11 window manager toolkit in Scheme")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.markwitmer.com/dist/guile-wm-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1l9qcz236jxvryndimjy62cf8zxf8i3f8vg3zpqqjhw15j9mdk3r"))))
    (build-system gnu-build-system)
    (arguments
     `( ;; The '.scm' files go to $(datadir), so set that to the
       ;; standard value.
       #:configure-flags (list (string-append "--datadir="
                                              (assoc-ref %outputs "out")
                                              "/share/guile/site/2.0"))
       #:phases (alist-cons-before
                 'configure 'set-go-directory
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Install .go files to $out/share/guile/site/2.0.
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* "module/Makefile.in"
                       (("^wmdir = .*$")
                        (string-append "wmdir = " out
                                       "/share/guile/site/2.0\n")))))
                 (alist-cons-after
                  'install 'set-load-path
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    ;; Put Guile-XCB's and Guile-WM's modules in the
                    ;; search path of PROG.
                    (let* ((out  (assoc-ref outputs "out"))
                           (prog (string-append out "/bin/guile-wm"))
                           (mods (string-append
                                  out "/share/guile/site/2.0"))
                           (xcb  (string-append
                                  (assoc-ref inputs "guile-xcb")
                                  "/share/guile/site/2.0")))
                      (wrap-program
                          prog
                        `("GUILE_LOAD_PATH" ":" prefix (,mods ,xcb))
                        `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                          (,mods ,xcb)))))
                  (alist-cons-after
                   'install 'install-xsession
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; add a .desktop file to xsessions
                     (let ((xsessions (string-append
                                       %output "/share/xsessions")))
                       (mkdir-p xsessions)
                       (call-with-output-file (string-append
                                               xsessions "/guile-wm.desktop")
                         (lambda (port)
                           (format port
                                   "[Desktop Entry]~@
                                    Name=~a~@
                                    Comment=~a~@
                                    Exec=~a/bin/guile-wm~@
                                    Type=Application~%"
                            ,name ,synopsis %output)))))
                   %standard-phases)))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("guile" ,guile-2.0)
              ("guile-xcb" ,guile-xcb)))
    (home-page "http://www.markwitmer.com/guile-xcb/guile-wm.html")
    (description
     "Guile-WM is a simple window manager that's completely customizable—you
have total control of what it does by choosing which modules to include.
Included with it are a few modules that provide basic TinyWM-like window
management, some window record-keeping, multi-monitor support, and emacs-like
keymaps and minibuffer.  At this point, it's just enough to get you started.")
    (license gpl3+)))
