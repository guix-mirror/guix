;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Alex ter Weele <alex.ter.weele@gmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 ng0 <ng0@n0.is>
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
  #:use-module (gnu packages texinfo)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public guile-xcb
  (let ((commit "db7d5a393cc37a56f66541b3f33938b40c6f35b3")
        (revision "1"))
    (package
      (name "guile-xcb")
      (version (git-version "1.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mwitmer/guile-xcb")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "16w4vgzbmnwih4bgfn8rw85ryfvzhc6hyly6bic9sd7hhc82rcnd"))))
      (build-system gnu-build-system)
      (arguments '(;; Parallel builds fail.
                   #:parallel-build? #f
                   #:configure-flags (list (string-append
                                            "--with-guile-site-dir="
                                            (assoc-ref %outputs "out")
                                            "/share/guile/site/2.2")
                                           (string-append
                                            "--with-guile-site-ccache-dir="
                                            (assoc-ref %outputs "out")
                                            "/lib/guile/2.2/site-ccache"))))
      (native-inputs `(("pkg-config" ,pkg-config)
                       ("texinfo" ,texinfo)))
      (inputs `(("guile" ,guile-2.2)
                ("xcb" ,xcb-proto)))
      (home-page "https://github.com/mwitmer/guile-xcb")
      (synopsis "XCB bindings for Guile")
      (description
       "Guile-XCB implements the XCB protocol and provides all the tools
necessary to write X client code in Guile Scheme without any external
dependencies.")
      (license gpl3+))))

(define-public guile-wm
  (let ((commit "f3c7b3be719f425ffb87265d34855a73366351be")
        (revision "1"))
    (package
      (name "guile-wm")
      (version (git-version "1.0" revision commit))
      (synopsis "X11 window manager toolkit in Scheme")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mwitmer/guile-wm")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "086dijnpl5dpglf70d6f9sizyakr313y7blpdjrmbi687j1x3qcl"))))
      (build-system gnu-build-system)
      (arguments
       `( ;; The '.scm' files go to $(datadir), so set that to the
         ;; standard value.
         #:configure-flags (list (string-append "--datadir="
                                                (assoc-ref %outputs "out")
                                                "/share/guile/site/2.2"))
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'set-go-directory
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Install .go files to $out/share/guile/site/2.2.
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "module/Makefile.in"
                   (("^wmdir = .*$")
                    (string-append "wmdir = " out
                                   "/share/guile/site/2.2\n"))))
               #t))
           (add-after 'install 'set-load-path
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Put Guile-XCB's and Guile-WM's modules in the
               ;; search path of PROG.
               (let* ((out  (assoc-ref outputs "out"))
                      (prog (string-append out "/bin/guile-wm"))
                      (mods (string-append
                             out "/share/guile/site/2.2"))
                      (xcb  (string-append
                             (assoc-ref inputs "guile-xcb")
                             "/share/guile/site/2.2")))
                 (wrap-program
                     prog
                   `("GUILE_LOAD_PATH" ":" prefix (,mods ,xcb))
                   `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                     (,mods ,xcb))))
               #t))
           (add-after 'install 'install-xsession
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
                             ,name ,synopsis %output))))
               #t)))))
      (native-inputs `(("pkg-config" ,pkg-config)
                       ("texinfo" ,texinfo)))
      (inputs `(("guile" ,guile-2.2)
                ("guile-xcb" ,guile-xcb)))
      (home-page "https://github.com/mwitmer/guile-wm/releases")
      (description
       "Guile-WM is a simple window manager that's completely customizable—you
have total control of what it does by choosing which modules to include.
Included with it are a few modules that provide basic TinyWM-like window
management, some window record-keeping, multi-monitor support, and emacs-like
keymaps and minibuffer.  At this point, it's just enough to get you started.")
      (license gpl3+))))
