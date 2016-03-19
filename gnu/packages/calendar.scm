;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016 Leo Famulari <leo@famulari.name>
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
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dav)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python))

(define-public libical
  (package
    (name "libical")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/libical/libical/releases/download/v"
                    version "/libical-" version ".tar.gz"))
              (sha256
               (base32
                "14lmjj63zyx88rf1z71l0v9ms4c2vpdhmixksjjxgywp5p2f7708"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; test suite appears broken
    (native-inputs
     `(("perl" ,perl)))
    (inputs
     `(("icu4c" ,icu4c)))
    (home-page "https://libical.github.io/libical/")
    (synopsis "iCalendar protocols and data formats implementation")
    (description
     "Libical is an implementation of the iCalendar protocols and protocol
data units.")
    (license lgpl2.1)))

(define-public khal
  (package
    (name "khal")
    (version "0.7.0")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "khal" version))
             (sha256
              (base32
               "00llxj7cv31mjsx0j6zxmyi9s1q20yvfkn025xcy8cv1ylfwic66"))
             (modules '((guix build utils)))
             ;; Patch broken path in 'doc' Makefile.
             ;; Patch sent upstream: https://github.com/geier/khal/pull/307
             (snippet
               '(substitute* "doc/source/Makefile"
                 (("../../../khal/khal/settings/khal.spec")
                  "../../khal/settings/khal.spec" )))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
        (add-after 'unpack 'disable-tests
          (lambda _
            ;; Bug reported for test_only_update_old_event:
            ;; https://github.com/geier/khal/issues/309
            (substitute* "tests/khalendar_test.py"
                         (("test_only_update_old_event")
                           "disabled_only_update_old_event"))

            ;; Bug reported for test_dt_two_tz:
            ;; https://github.com/pimutils/khal/issues/382
            (substitute* "tests/event_test.py"
                         (("test_dt_two_tz")
                           "disabled_dt_two_tz"))
            ;; Another timezone / DST issue:
            ;; https://github.com/pimutils/khal/issues/146
            (substitute* "tests/event_test.py"
                         (("test_raw_dt")
                           "disabled_raw_dt"))))

        ;; Building the manpage requires khal to be installed.
        (add-after 'install 'manpage
          (lambda* (#:key outputs #:allow-other-keys)
            (setenv "PYTHONPATH"
                    (string-append
                      (getenv "PYTHONPATH") ":" (assoc-ref outputs "out")))
            (zero? (system* "make" "--directory=doc/" "man"))
            (install-file
              "doc/build/man/khal.1"
              (string-append (assoc-ref outputs "out") "/share/man/man1"))))

        ;; The tests require us to choose a timezone.
        (replace 'check
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "TZ"
                    (string-append (assoc-ref inputs "tzdata")
                                   "/share/zoneinfo/Zulu"))
            (zero? (system* "py.test" "tests")))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ;; Required for tests
       ("tzdata" ,tzdata)
       ;; Required to build manpage
       ("python-sphinxcontrib-newsfeed" ,python-sphinxcontrib-newsfeed)
       ("python-sphinx" ,python-sphinx)))
    (inputs
     `(("sqlite" ,sqlite)))
    (propagated-inputs
     `(("python-configobj" ,python-configobj)
       ("python-dateutil-2" ,python-dateutil-2)
       ("python-icalendar" ,python-icalendar)
       ("python-tzlocal" ,python-tzlocal)
       ("python-urwid" ,python-urwid)
       ("python-pyxdg" ,python-pyxdg)
       ("vdirsyncer" ,vdirsyncer)))
    (synopsis "Console calendar program")
    (description "Khal is a standards based console calendar program,
able to synchronize with CalDAV servers through vdirsyncer.")
    (home-page "http://lostpackets.de/khal/")
    (license expat)))
