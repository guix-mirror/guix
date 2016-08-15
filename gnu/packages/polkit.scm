;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml))

(define-public polkit
  (package
    (name "polkit")
    (version "0.113")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://www.freedesktop.org/software/polkit/releases/"
                   name "-" version ".tar.gz"))
             (sha256
              (base32
               "109w86kfqrgz83g9ivggplmgc77rz8kx8646izvm2jb57h4rbh71"))
             (patches (search-patches "polkit-drop-test.patch"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (use-modules (guix build utils))
                 (substitute* "configure"
                   ;; Replace libsystemd-login with libelogind.
                   (("libsystemd-login") "libelogind")
                   ;; Skip the sanity check that the current system runs
                   ;; systemd.
                   (("test ! -d /sys/fs/cgroup/systemd/") "false"))
                 (substitute* "src/polkit/polkitunixsession-systemd.c"
                   (("systemd") "elogind"))
                 (substitute* "src/polkitbackend/polkitbackendsessionmonitor-systemd.c"
                   (("systemd") "elogind"))
                 (substitute* "src/polkitbackend/polkitbackendjsauthority.c"
                   (("systemd") "elogind"))

                 ;; GuixSD's polkit service stores actions under
                 ;; /etc/polkit-1/actions.
                 (substitute* "src/polkitbackend/polkitbackendinteractiveauthority.c"
                   (("PACKAGE_DATA_DIR \"/polkit-1/actions\"")
                    "PACKAGE_SYSCONF_DIR \"/polkit-1/actions\""))

                 ;; Set the setuid helper's real location.
                 (substitute* "src/polkitagent/polkitagentsession.c"
                   (("PACKAGE_PREFIX \"/lib/polkit-1/polkit-agent-helper-1\"")
                    "\"/run/setuid-programs/polkit-agent-helper-1\""))))))
    (build-system gnu-build-system)
    (inputs
     `(("expat" ,expat)
       ("linux-pam" ,linux-pam)
       ("elogind" ,elogind)
       ("mozjs" ,mozjs)
       ("nspr" ,nspr)))
    (propagated-inputs
     `(("glib" ,glib))) ; required by polkit-gobject-1.pc
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin") ; for glib-mkenums
       ("intltool" ,intltool)
       ("gobject-introspection" ,gobject-introspection)))
    (arguments
     `(#:configure-flags '("--sysconfdir=/etc"
                           "--enable-man-pages")
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
         (replace
          'install
          (lambda* (#:key outputs (make-flags '()) #:allow-other-keys)
            ;; Override sysconfdir during "make install", to avoid attempting
            ;; to install in /etc, and to instead install the skeletons in the
            ;; output directory.
            (let ((out (assoc-ref outputs "out")))
             (zero? (apply system*
                           "make" "install"
                           (string-append "sysconfdir=" out "/etc")
                           (string-append "polkit_actiondir="
                                          out "/share/polkit-1/actions")
                           make-flags))))))))
    (home-page "http://www.freedesktop.org/wiki/Software/polkit/")
    (synopsis "Authorization API for privilege management")
    (description "Polkit is an application-level toolkit for defining and
handling the policy that allows unprivileged processes to speak to
privileged processes.  It is a framework for centralizing the decision
making process with respect to granting access to privileged operations
for unprivileged applications.")
    (license lgpl2.0+)))

(define-public polkit-qt
  (package
    (name "polkit-qt")
    (version "1-0.112.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://download.kde.org/stable/apps/KDE4.x/admin/"
                   name "-" version ".tar.bz2"))
             (sha256
              (base32
               "1ip78x20hjqvm08kxhp6gb8hf6k5n6sxyx6kk2yvvq53djzh7yv7"))))
    (build-system cmake-build-system)
    (inputs
     `(("polkit" ,polkit)))
    (propagated-inputs
     `(("qtbase" ,qtbase)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags (list (string-append "-DCMAKE_INSTALL_RPATH="
                                              (assoc-ref %outputs "out")
                                              "/lib:"
                                              (assoc-ref %outputs "out")
                                              "/lib64"))
       #:tests? #f)) ; there is a test subdirectory, but no test target
    (home-page "http://api.kde.org/kdesupport-api/polkit-qt-1-apidocs/")
    (synopsis "Qt frontend to the polkit library")
    (description "Polkit-qt is a library that lets developers use the
PolicyKit API through a Qt-styled API.  It is mainly a wrapper around
QAction and QAbstractButton that lets you integrate those two component
easily with PolicyKit.")
    (license lgpl2.0+)))
