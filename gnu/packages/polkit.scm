;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
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
  #:use-module ((guix licenses) #:select (lgpl2.0+ mpl2.0))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml))

(define-public mozjs
  (package
    (name "mozjs")
    (version "17.0.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/js/"
                   name version ".tar.gz"))
             (sha256
              (base32
               "1fig2wf4f10v43mqx67y68z6h77sy900d1w0pz9qarrqx57rc7ij"))))
    (build-system gnu-build-system)
    (native-inputs
      `(("perl", perl)
        ("python" ,python-2)))
    (arguments
      `(#:phases
          (alist-cons-before
           'configure 'chdir
           (lambda _
             (chdir "js/src"))
           (alist-replace
            'configure
            ;; configure fails if it is followed by SHELL and CONFIG_SHELL
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (setenv "SHELL" (which "sh"))
                (setenv "CONFIG_SHELL" (which "sh"))
                (zero? (system*
                        "./configure" (string-append "--prefix=" out)))))
            %standard-phases))))
    (home-page
     "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey")
    (synopsis "Mozilla javascript engine")
    (description "SpiderMonkey is Mozilla's JavaScript engine written
in C/C++.")
    (license mpl2.0))) ; and others for some files

(define-public nspr
  (package
    (name "nspr")
    (version "4.10.7")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                   version "/src/nspr-" version ".tar.gz"))
             (sha256
              (base32
               "0f1ri51yzjikigf6z31g03cdv6sgi9gw2c3vvv39psk3m37zb6iq"))))
    (build-system gnu-build-system)
    (native-inputs
      `(("perl", perl)))
    (arguments
      `(#:tests? #f ; no check target
        #:configure-flags
        `("--enable-64bit")
        #:phases
          (alist-cons-before
           'configure 'chdir
           (lambda _
             (chdir "nspr"))
            %standard-phases)))
    (home-page
     "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSPR")
    (synopsis "Netscape API for system level and libc-like functions")
    (description "Netscape Portable Runtime (NSPR) provides a
platform-neutral API for system level and libc-like functions.  It is used
in the Mozilla clients.")
    (license mpl2.0)))

(define-public polkit
  (package
    (name "polkit")
    (version "0.112")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.freedesktop.org/software/polkit/releases/"
                   name "-" version ".tar.gz"))
             (sha256
              (base32
               "1xkary7yirdcjdva950nqyhmsz48qhrdsr78zciahj27p8yg95fn"))
             (patches (list (search-patch "polkit-drop-test.patch")))))
    (build-system gnu-build-system)
    (inputs
      `(("expat" ,expat)
        ("glib" ,glib)
        ("glib:bin" ,glib "bin") ; for glib-mkenums
        ("intltool" ,intltool)
        ("linux-pam" ,linux-pam)
        ("mozjs" ,mozjs)
        ("nspr" ,nspr)))
    (native-inputs
      `(("pkg-config", pkg-config)))
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
      `(("glib" ,glib)
        ("polkit" ,polkit)))
    (propagated-inputs
      `(("qt" ,qt-4))) ; according to the pkg-config files
    (native-inputs
      `(("pkg-config", pkg-config)))
    (arguments
      `(#:tests? #f)) ; there is a test subdirectory, but no test target
    (home-page "http://api.kde.org/kdesupport-api/polkit-qt-1-apidocs/")
    (synopsis "Qt frontend to the polkit library")
    (description "Polkit-qt is a library that lets developers use the
PolicyKit API through a Qt-styled API.  It is mainly a wrapper around
QAction and QAbstractButton that lets you integrate those two component
easily with PolicyKit.")
    (license lgpl2.0+)))
