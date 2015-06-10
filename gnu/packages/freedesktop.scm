;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Andy Wingo <wingo@pobox.com>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages freedesktop)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages glib)                ;intltool
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public xdg-utils
  (package
    (name "xdg-utils")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
          (uri (string-append
                 "http://portland.freedesktop.org/download/xdg-utils-"
                 version ".tgz"))
          (sha256
            (base32
             "1b019d3r1379b60p33d6z44kx589xjgga62ijz9vha95dg8vgbi1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no check target
    (home-page "http://portland.freedesktop.org/")
    (synopsis "Freedesktop.org scripts for desktop integration")
    (description "The xdg-utils package is a set of simple scripts that
provide basic desktop integration functions in the framework of the
freedesktop.org project.")
    (license license:expat)))

(define-public libinput
  (package
    (name "libinput")
    (version "0.14.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://freedesktop.org/software/libinput/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0r0v5jqbnwgndq6ns3ss3kv1438ny302m7bg1najcl1dpqp21v9b"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libudev" ,eudev))) ; required by libinput.pc
    (inputs
     `(("libevdev" ,libevdev)
       ("mtdev" ,mtdev)))
    (home-page "http://www.freedesktop.org/wiki/Software/libinput/")
    (synopsis "Input devices handling library")
    (description
     "Libinput is a library to handle input devices for display servers and
other applications that need to directly deal with input devices.")
    (license license:x11)))

(define-public elogind
  (let ((commit "14405a9"))
    (package
      (name "elogind")
      (version (string-append "219." commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "http://git.elephly.net/software/elogind.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1wz5lxj95qg64x2q5hf4zcb35hpxlw3wfswx6sb2srvsg50y3y72"))
                (file-name (string-append name "-checkout-" commit))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (use-modules (guix build utils))
                    (substitute* "Makefile.am"
                      ;; Avoid validation against DTD because the DTDs for
                      ;; both doctype 4.2 and 4.5 are needed.
                      (("XSLTPROC_FLAGS = ") "XSLTPROC_FLAGS = --novalid"))))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags
         (list
          ;; pam_elogind fails because of bus-error.c hackery
          "--disable-pam"
          (string-append "--with-rootprefix=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'autogen
                      (lambda _
                        (and (zero? (system* "intltoolize" "--force" "--automake"))
                             (zero? (system* "autoreconf" "-vif"))))))))
      (native-inputs
       `(("intltool" ,intltool)
         ("gettext" ,gnu-gettext)
         ("docbook-xsl" ,docbook-xsl)
         ("docbook-xml" ,docbook-xml)
         ("xsltproc" ,libxslt)
         ("libxml2" ,libxml2)                     ;for XML_CATALOG_FILES
         ("pkg-config", pkg-config)
         ("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("gperf" ,gperf)))
      (inputs
       `(("linux-pam" ,linux-pam)
         ("linux-libre-headers" ,linux-libre-headers)
         ("libcap" ,libcap)
         ("dbus" ,dbus)
         ("eudev" ,eudev)))
      (home-page "https://github.com/andywingo/elogind")
      (synopsis "User, seat, and session management service")
      (description "Elogind is the systemd project's \"logind\" service,
extracted out as a separate project.  Elogind integrates with PAM to provide
the org.freedesktop.login1 interface over the system bus, allowing other parts
of a the system to know what users are logged in, and where.")
      (license license:lgpl2.1+))))

(define-public python-pyxdg
  (package
    (name "python-pyxdg")
    (version "0.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pyxdg/pyxdg-"
             version ".tar.gz"))
       (sha256
        (base32
         "179767h8m634ydlm4v8lnz01ba42gckfp684id764zaip7h87s41"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (alist-replace
        'check
        (lambda* (#:key inputs #:allow-other-keys)
          (setenv "XDG_DATA_DIRS"
                  (string-append (assoc-ref inputs "shared-mime-info")
                                 "/share/"))
          (substitute* "test/test-icon.py"
            (("/usr/share/icons/hicolor/index.theme")
             (string-append (assoc-ref inputs "hicolor-icon-theme")
                            "/share/icons/hicolor/index.theme")))

          ;; One test fails with:
          ;; AssertionError: 'x-apple-ios-png' != 'png'
          (substitute* "test/test-mime.py"
            (("self.check_mimetype\\(imgpng, 'image', 'png'\\)") "#"))
          (zero? (system* "nosetests" "-v")))
        %standard-phases)))
    (native-inputs
     `(("shared-mime-info" ,shared-mime-info) ;for tests
       ("hicolor-icon-theme" ,hicolor-icon-theme) ;for tests
       ("python-nose" ,python-nose)
       ("python-setuptools" ,python-setuptools)))
    (home-page "http://freedesktop.org/wiki/Software/pyxdg")
    (synopsis "Implementations of freedesktop.org standards in Python")
    (description
     "PyXDG is a collection of implementations of freedesktop.org standards in
Python")
    (license license:lgpl2.0)))

(define-public python2-pyxdg
  (package-with-python2 python-pyxdg))
