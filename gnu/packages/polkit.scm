;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Huang Ying <huang.ying.caritas@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages polkit)
  #:use-module ((guix licenses) #:select (lgpl2.0+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix memoization)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml)
  #:export (polkit))

(define-public polkit-mozjs
  (package
    (name "polkit")
    (version "0.120")
    (replacement polkit-mozjs/fixed)
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://www.freedesktop.org/software/polkit/releases/"
                   name "-" version ".tar.gz"))
             (sha256
              (base32
               "00zfg9b9ivkcj2jcf5b92cpvvyljz8cmfwj86lkvy5rihnd5jypf"))
             (patches (search-patches "polkit-configure-elogind.patch"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (use-modules (guix build utils))
                 ;; Disable broken test.
                 (substitute* "test/Makefile.in"
                   (("SUBDIRS = mocklibc . polkit polkitbackend")
                    "SUBDIRS = mocklibc . polkit"))
                 ;; Disable a test that requires Python, D-Bus and a few
                 ;; libraries and fails with "ERROR: timed out waiting for bus
                 ;; process to terminate".
                 (substitute* "test/polkitbackend/Makefile.am"
                   (("TEST_PROGS \\+= polkitbackendjsauthoritytest-wrapper.py")
                    ""))
                 ;; Guix System's polkit
                 ;; service stores actions under /etc/polkit-1/actions.
                 (substitute* "src/polkitbackend/polkitbackendinteractiveauthority.c"
                   (("PACKAGE_DATA_DIR \"/polkit-1/actions\"")
                    "PACKAGE_SYSCONF_DIR \"/polkit-1/actions\""))
                 ;; Set the setuid helper's real location.
                 (substitute* "src/polkitagent/polkitagentsession.c"
                   (("PACKAGE_PREFIX \"/lib/polkit-1/polkit-agent-helper-1\"")
                    "\"/run/setuid-programs/polkit-agent-helper-1\""))))))
    (build-system gnu-build-system)
    (inputs
     (list expat linux-pam elogind mozjs-78 nspr))
    (propagated-inputs
     (list glib)) ; required by polkit-gobject-1.pc
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin") ; for glib-mkenums
       ("intltool" ,intltool)
       ("gobject-introspection" ,gobject-introspection)
       ("libxslt" ,libxslt) ; for man page generation
       ("docbook-xsl" ,docbook-xsl))) ; for man page generation
    (arguments
     `(#:configure-flags '("--sysconfdir=/etc"
                           "--enable-man-pages"
                           ;; Prevent ‘configure: error: cannot check for
                           ;; file existence when cross compiling’.
                           ,@(if (%current-target-system)
                                 '("--with-os-type=unknown")
                                 '()))
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'fix-introspection-install-dir
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* (find-files "." "Makefile.in")
                (("@INTROSPECTION_GIRDIR@")
                 (string-append out "/share/gir-1.0/"))
                (("@INTROSPECTION_TYPELIBDIR@")
                 (string-append out "/lib/girepository-1.0/"))))))
         (add-after 'unpack 'fix-manpage-generation
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xsldoc (string-append (assoc-ref inputs "docbook-xsl")
                                          "/xml/xsl/docbook-xsl-"
                                          ,(package-version docbook-xsl))))
               (substitute* '("docs/man/Makefile.am" "docs/man/Makefile.in")
                 (("http://docbook.sourceforge.net/release/xsl/current")
                  xsldoc)))))
         (replace
          'install
          (lambda* (#:key outputs (make-flags '()) #:allow-other-keys)
            ;; Override sysconfdir during "make install", to avoid attempting
            ;; to install in /etc, and to instead install the skeletons in the
            ;; output directory.
            (let ((out (assoc-ref outputs "out")))
             (apply invoke "make" "install"
                           (string-append "sysconfdir=" out "/etc")
                           (string-append "polkit_actiondir="
                                          out "/share/polkit-1/actions")
                           make-flags)))))))
    (home-page "https://www.freedesktop.org/wiki/Software/polkit/")
    (synopsis "Authorization API for privilege management")
    (description "Polkit is an application-level toolkit for defining and
handling the policy that allows unprivileged processes to speak to
privileged processes.  It is a framework for centralizing the decision
making process with respect to granting access to privileged operations
for unprivileged applications.")
    (license lgpl2.0+)))

(define-public polkit-mozjs/fixed
  (package
    (inherit polkit-mozjs)
    (version "0.121")
    (source (origin
              (inherit (package-source polkit-mozjs))
              (patches (cons (search-patch "polkit-CVE-2021-4034.patch")
                             (origin-patches
                              (package-source polkit-mozjs))))))))

;;; Variant of polkit built with Duktape, a lighter JavaScript engine compared
;;; to mozjs.
(define-public polkit-duktape
  (let ((base polkit-mozjs))
    (package/inherit base
      (name "polkit-duktape")
      (source
       (origin
         (inherit (package-source base))
         (patches
          (append
              (search-patches "polkit-use-duktape.patch")
              (origin-patches (package-source base))))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags)
          `(cons "--with-duktape" ,flags))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'force-gnu-build-system-bootstrap
               (lambda _
                 (delete-file "configure")))))))
      (native-inputs
       (append `(("autoconf" ,autoconf)
                 ("automake" ,automake)
                 ("libtool" ,libtool)
                 ("pkg-config" ,pkg-config))
           (package-native-inputs base)))
      (inputs (alist-replace "mozjs" `(,duktape)
                             (package-inputs base))))))

(define polkit-for-system
  (mlambda (system)
    "Return a polkit package that can be built for SYSTEM; that is, either the
regular polkit that requires mozjs or its duktape variant."
    (if (string-prefix? "x86_64" system)
        polkit-mozjs
        polkit-duktape)))

;;; Define a top level polkit variable that can be built on any of the
;;; supported platforms.  This is to work around the fact that our
;;; mrustc-bootstrapped rust toolchain currently only supports the x86_64
;;; architecture.
(define-syntax polkit
  (identifier-syntax (polkit-for-system
                      (or (%current-target-system) (%current-system)))))

(define-public polkit-qt
  (package
    (name "polkit-qt")
    (version "1-0.112.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kde//stable/apps/KDE4.x/admin/"
                   "polkit-qt-" version ".tar.bz2"))
             (sha256
              (base32
               "1ip78x20hjqvm08kxhp6gb8hf6k5n6sxyx6kk2yvvq53djzh7yv7"))))
    (build-system cmake-build-system)
    (inputs
     (list polkit))
    (propagated-inputs
     (list qtbase-5))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:configure-flags (list (string-append "-DCMAKE_INSTALL_RPATH="
                                              (assoc-ref %outputs "out")
                                              "/lib:"
                                              (assoc-ref %outputs "out")
                                              "/lib64"))
       #:tests? #f)) ; there is a test subdirectory, but no test target
    (home-page "https://api.kde.org/kdesupport-api/polkit-qt-1-apidocs/")
    (synopsis "Qt frontend to the polkit library")
    (description "Polkit-qt is a library that lets developers use the
PolicyKit API through a Qt-styled API.  It is mainly a wrapper around
QAction and QAbstractButton that lets you integrate those two component
easily with PolicyKit.")
    (license lgpl2.0+)))

(define-public polkit-gnome
  (package
    (name "polkit-gnome")
    (version "0.105")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/"
                                  name "/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0sckmcbxyj6sbrnfc5p5lnw27ccghsid6v6wxq09mgxqcd4lk10p"))))
    (build-system gnu-build-system)
    (inputs (list gtk+ polkit))
    (native-inputs (list intltool pkg-config))
    (synopsis "Legacy polkit authentication agent for GNOME")
    (description "PolicyKit-gnome provides a D-Bus session bus service
that is used to bring up authentication dialogs used for obtaining
privileges.")
    (home-page "https://www.freedesktop.org/wiki/Software/polkit/")
    (license lgpl2.0+)))
