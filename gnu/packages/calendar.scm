;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016 Stefan Reichoer <stefan@xsteve.at>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages calendar)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages dav)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-26))

(define-public libical
  (package
    (name "libical")
    (version "3.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/libical/libical/releases/download/v"
                    version "/libical-" version ".tar.gz"))
              (sha256
               (base32
                "0ifisnh42cw5z53hp9p52l3ggc7k877zlqk0n06gdhrk0bhidckj"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; test suite appears broken
       #:configure-flags '("-DSHARED_ONLY=true")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; FIXME: This should be patched to use TZDIR so we can drop
             ;; the tzdata dependency.
             (let ((tzdata (assoc-ref inputs "tzdata")))
               (substitute* "src/libical/icaltz-util.c"
                 (("\\\"/usr/share/zoneinfo\\\",")
                  (string-append "\"" tzdata "/share/zoneinfo\""))
                 (("\\\"/usr/lib/zoneinfo\\\",") "")
                 (("\\\"/etc/zoneinfo\\\",") "")
                 (("\\\"/usr/share/lib/zoneinfo\\\"") "")))
             #t)))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("libxml2" ,libxml2)
       ("tzdata" ,tzdata)))
    (propagated-inputs
     ;; In Requires.private of libical.pc.
     `(("icu4c" ,icu4c)))
    (home-page "https://libical.github.io/libical/")
    (synopsis "iCalendar protocols and data formats implementation")
    (description
     "Libical is an implementation of the iCalendar protocols and protocol
data units.")
    ;; Can be used with either license.  See COPYING.
    (license (list license:lgpl2.1 license:mpl2.0))))

(define-public khal
  (package
    (name "khal")
    (version "0.10.1")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "khal" version))
             (sha256
              (base32
               "1r8bkgjwkh7i8ygvsv51h1cnax50sb183vafg66x5snxf3dgjl6l"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
        ;; Building the manpage requires khal to be installed.
        (add-after 'install 'manpage
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; Make installed package available for running the tests
            (add-installed-pythonpath inputs outputs)
            (invoke "make" "--directory=doc/" "man")
            (install-file
             "doc/build/man/khal.1"
             (string-append (assoc-ref outputs "out") "/share/man/man1"))
            #t))
        (replace 'check
          (lambda* (#:key inputs #:allow-other-keys)
            ;; The tests require us to choose a timezone.
            (setenv "TZ"
                    (string-append (assoc-ref inputs "tzdata")
                                   "/share/zoneinfo/Zulu"))
            (invoke "py.test" "tests"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ;; Required for tests
       ("python-freezegun" ,python-freezegun)
       ("tzdata" ,tzdata-for-tests)
       ("vdirsyncer" ,vdirsyncer)
       ;; Required to build manpage
       ("python-sphinxcontrib-newsfeed" ,python-sphinxcontrib-newsfeed)
       ("python-sphinx" ,python-sphinx)))
    (inputs
     `(("sqlite" ,sqlite)))
    (propagated-inputs
     `(("python-configobj" ,python-configobj)
       ("python-dateutil" ,python-dateutil)
       ("python-icalendar" ,python-icalendar)
       ("python-tzlocal" ,python-tzlocal)
       ("python-urwid" ,python-urwid)
       ("python-pyxdg" ,python-pyxdg)))
    (synopsis "Console calendar program")
    (description "Khal is a standards based console calendar program,
able to synchronize with CalDAV servers through vdirsyncer.")
    (home-page "http://lostpackets.de/khal/")
    (license license:expat)))

(define-public remind
  (package
    (name "remind")
    (version "3.1.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dianne.skoll.ca/projects/remind/download/"
                           "remind-"
                           (string-join (map (cut string-pad <> 2 #\0)
                                             (string-split version #\.))
                                        ".")
                           ".tar.gz"))
       (sha256
        (base32 "14yavwqmimba8rdpwx3wlav9sfb0v5rcd1iyzqrs08wx07a9pdzf"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f))                    ; no "check" target
    (home-page "https://dianne.skoll.ca/projects/remind/")
    (synopsis "Sophisticated calendar and alarm program")
    (description
     "Remind allows you to remind yourself of upcoming events and appointments.
Each reminder or alarm can consist of a message sent to standard output, or a
program to be executed.  It also features: sophisticated date calculation,
moon phases, sunrise/sunset, Hebrew calendar, alarms, PostScript output and
proper handling of holidays.")
    (license license:gpl2)))

(define-public libhdate
  (package
    (name "libhdate")
    (version "1.6.02")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/libhdate/libhdate/libhdate-"
                            version "/" name "-" version ".tar.bz2"))
        (sha256
         (base32
          "0qkpq412p78znw8gckwcx3l0wcss9s0dgw1pvjb1ih2pxf6hm4rw"))))
    (build-system gnu-build-system)
    (home-page "http://libhdate.sourceforge.net/")
    (synopsis "Library to use Hebrew dates")
    (description "LibHdate is a small library for the Hebrew calendar and times
of day, written in C, and including bindings for C++, pascal, perl, php, python,
and ruby.  It includes two illustrative command-line programs, @code{hcal} and
@code{hdate}, and some snippets and scripts written in the binding languages.")
    (license license:gpl3+)))
