;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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
    (version "1.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.markwitmer.com/dist/guile-xcb-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "009qrw46ay74z3mw8gz7jqvn90z9ilyhy00801w5vpyias02730y"))))
    (build-system gnu-build-system)
    (arguments '(;; Parallel builds fail.
                 #:parallel-build? #f

                 ;; The '.scm' files go to $(datadir), so set that to the
                 ;; standard value.
                 #:configure-flags (list (string-append
                                          "--datadir="
                                          (assoc-ref %outputs "out")
                                          "/share/guile/site/2.0"))
                 #:phases (alist-cons-before
                           'configure 'set-go-directory
                           (lambda* (#:key outputs #:allow-other-keys)
                             ;; The makefile sets the .go directory to Guile's
                             ;; own .go site directory, which is read-only.
                             ;; Change it to point to $out/share/guile/site/2.0.
                             (let ((out (assoc-ref outputs "out")))
                               (substitute* "Makefile.in"
                                 (("^godir = .*$")
                                  (string-append "godir = " out
                                                 "/share/guile/site/2.0\n")))))
                           %standard-phases)))
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
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.markwitmer.com/dist/guile-wm-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0vv6avpkl6lgrhy2a16z470fqjhvzi4r93qwl87xw9v5dvldf08p"))))
    (build-system gnu-build-system)
    (arguments '(;; The '.scm' files go to $(datadir), so set that to the
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
                            %standard-phases))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("guile" ,guile-2.0)
              ("guile-xcb" ,guile-xcb)))
    (home-page "http://www.markwitmer.com/guile-xcb/guile-wm.html")
    (synopsis "X11 window manager toolkit in Scheme")
    (description
     "Guile-WM is a simple window manager that's completely customizable—you
have total control of what it does by choosing which modules to include.
Included with it are a few modules that provide basic TinyWM-like window
management, some window record-keeping, multi-monitor support, and emacs-like
keymaps and minibuffer. At this point, it's just enough to get you started.")
    (license gpl3+)))
