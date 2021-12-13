;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2019 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages sawfish)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xorg))

(define-public librep
  (package
    (name "librep")
    (version "0.92.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.tuxfamily.org/librep/"
                                  "librep_" version ".tar.xz"))
              (sha256
               (base32
                "1bmcjl1x1rdh514q9z3hzyjmjmwwwkziipjpjsl301bwmiwrd8a8"))))
    (build-system gnu-build-system)
    (native-inputs
     (list texinfo pkg-config autoconf automake libtool))
    (inputs
     (list gdbm gmp libffi readline))
    (native-search-paths
     (list (search-path-specification
            (variable "REP_DL_LOAD_PATH")
            (files '("lib/rep")))))
    (home-page "https://sawfish.fandom.com/wiki/Librep")
    (synopsis "Lisp system for sawfish")
    (description
     "Librep is a dialect of Lisp, designed to be used both as an extension
language for applications and as a general purpose programming language.  It
was originally written to be mostly-compatible with Emacs Lisp, but has
subsequently diverged markedly.  Its aim is to combine the best features of
Scheme and Common Lisp and provide an environment that is comfortable for
implementing both small and large scale systems.")
    (license gpl2+)))

(define-public rep-gtk
  (package
    (name "rep-gtk")
    (version "0.90.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.tuxfamily.org/librep/"
                                  name "/" name "_" version ".tar.xz"))
              (sha256
               (base32
                "0hgkkywm8zczir3lqr727bn7ybgg71x9cwj1av8fykkr8pdpard9"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Makefile.in"
                    (("installdir=\\$\\(repexecdir\\)")
                     ;; Install libraries for librep to $out/lib/rep.
                     "installdir=$(libdir)/rep"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'remove-autogen
           (lambda _
             ;; Remove autogen.sh so that the bootstrap phase can run
             ;; autoreconf.
             (delete-file "autogen.sh")
             #t)))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (propagated-inputs
     ;; required by rep-gtk.pc.
     (list gtk+-2 librep))
    (home-page "https://sawfish.fandom.com/wiki/Rep-GTK")
    (synopsis "GTK+ binding for librep")
    (description
     "Rep-GTK is a GTK+ (and GLib, GDK) binding to the librep, and one of the
backend of Sawfish.")
    (license gpl2+)))

(define-public sawfish
  (package
    (name "sawfish")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.tuxfamily.org/sawfish/"
                                  name "_" version ".tar.xz"))
              (sha256
               (base32
                "1z7awzgw8d15aw17kpbj460pcxq8l2rhkaxk47w7yg9qrmg0xja4"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Makedefs.in"
                    (("/bin/sh") "@SHELL@")
                    (("REP_DL_LOAD_PATH=")
                     ;; To find rep-gtk when building sawfish.
                     "REP_DL_LOAD_PATH=$(REP_DL_LOAD_PATH):"))
                  (substitute* "src/Makefile.in"
                    ;; Install libraries for librep to $out/lib/rep.
                    (("\\$\\(repexecdir\\)") "$(libdir)/rep"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-exec-rep
           (lambda _
             (substitute* '("lisp/sawfish/cfg/main.jl.in"
                            "scripts/sawfish-about.jl.in"
                            "scripts/sawfish-client.jl"
                            "scripts/sawfish-menu.jl")
               (("exec rep") (string-append "exec " (which "rep"))))
             #t))
         (add-after 'install 'wrap-scripts
           ;; Wrap scripts with REP_DL_LOAD_PATH for finding rep-gtk
           ;; and sawfish.client.
           (lambda* (#:key outputs #:allow-other-keys)
             (define (wrap-script script)
               (let ((out (assoc-ref outputs "out")))
                 (wrap-program (string-append out script)
                   `("REP_DL_LOAD_PATH" =
                     ,(list (getenv "REP_DL_LOAD_PATH")
                            (string-append out "/lib/rep"))))))
             (for-each wrap-script
                       (list "/bin/sawfish-about"
                             "/bin/sawfish-client"
                             "/bin/sawfish-config"
                             "/lib/sawfish/sawfish-menu"))
             #t)))))
    (native-inputs
     `(("gettext"     ,gettext-minimal)
       ("makeinfo"    ,texinfo)
       ("pkg-config"  ,pkg-config)
       ("which"       ,which)))
    (inputs
     (list libsm
           libxft
           libxinerama
           libxrandr
           libxtst
           rep-gtk))
    (home-page "https://sawfish.tuxfamily.org")
    (synopsis "Configurable window manager")
    (description
     "Sawfish is an extensible window manager using a Lisp-based scripting
language.  Its policy is very minimal compared to most window managers.  Its aim
is simply to manage windows in the most flexible and attractive manner possible.
All high-level WM functions are implemented in Lisp for future extensibility or
redefinition.")
    (license gpl2+)))
