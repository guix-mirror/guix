;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016, 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016, 2021 Stefan Reichoer <stefan@xsteve.at>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com
;;; Copyright © 2020 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Peng Mei Yu <pengmeiyu@riseup.net>
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
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages dav)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-26))

(define-public date
  ;; We make the same choice as the Arch package maintainer by choosing a
  ;; recent commit to fix some bugs.
  ;; https://github.com/Alexays/Waybar/issues/565
  (let ((commit "9a0ee2542848ab8625984fc8cdbfb9b5414c0082"))
    (package
      (name "date")
      (version (string-append "2.4.1-" (string-take commit 8)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/HowardHinnant/date")
               (commit "9a0ee2542848ab8625984fc8cdbfb9b5414c0082")))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0yxsn0hj22n61bjywysxqgfv7hj5xvsl6isma95fl8xrimpny083"))
         (patches
          ;; Install pkg-config files
          ;; https://github.com/HowardHinnant/date/pull/538
          (search-patches "date-output-pkg-config-files.patch"))))
      (inputs `(("tzdata" ,tzdata)))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags (list "-DUSE_SYSTEM_TZ_DB=ON"
                                 "-DBUILD_SHARED_LIBS=ON"
                                 "-DBUILD_TZ_LIB=ON"
                                 "-DENABLE_DATE_TESTING=ON")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-bin-bash
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "compile_fail.sh"
                 (("/bin/bash") (which "bash")))
               #t))
           (add-after 'unpack 'patch-zoneinfo-path
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/tz.cpp"
                 (("/usr/share/zoneinfo")
                  (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo")))
               #t))
           (replace 'check
             (lambda _
               ;; Disable test that requires checking timezone that
               ;; isn't set in the build environment.
               (substitute* "CTestTestfile.cmake"
                 (("add_test.tz_test_pass_zoned_time_deduction_test.*") "")
                 (("set_tests_properties.tz_test_pass_zoned_time_deduction_test.*") ""))
               (invoke "make" "testit"))))))
      (synopsis "Date and time library for C++11 and C++14")
      (description "Date is a header only C++ library that extends the chrono
date algorithms library for calendar dates and durations.  It also provides
the <tz.h> library for handling time zones and leap seconds.")
      (home-page "https://howardhinnant.github.io/date/date.html")
      (license license:expat))))

(define-public libical
  (package
    (name "libical")
    (version "3.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/libical/libical/releases/download/v"
                    version "/libical-" version ".tar.gz"))
              (sha256
               (base32
                "0vr8s7hn8204lyc4ys5bs3j5qss4lmc9ffly2m1a59avyz5cmzh9"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; test suite appears broken
       #:parallel-build? #f             ;may cause GIR generation failure
       #:configure-flags '("-DSHARED_ONLY=true"
                           ;; required by evolution-data-server
                           "-DGOBJECT_INTROSPECTION=true"
                           "-DICAL_GLIB_VAPI=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-reference
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "doc/reference/libical-glib/libical-glib-docs.sgml.in"
               (("http://www.oasis-open.org/docbook/xml/4.3/")
                (string-append (assoc-ref inputs "docbook-xml")
                               "/xml/dtd/docbook/")))
             #t))
         (add-before 'configure 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; TODO: libical 3.1.0 supports using TZDIR instead of a hard-coded
             ;; zoneinfo database.  When that is released we can drop
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
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
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
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "khal" version))
              (sha256
               (base32
                "11qhrga44knlnp88py9p547d4nr5kn041d2nszwa3dqw7mf22ks9"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; The test suite is unreliable. See <https://bugs.gnu.org/44197>
       #:phases (modify-phases %standard-phases
        ;; Building the manpage requires khal to be installed.
        (add-after 'install 'manpage
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; Make installed package available for running the tests
            (add-installed-pythonpath inputs outputs)
            (invoke "make" "--directory=doc/" "man")
            (install-file
             "doc/build/man/khal.1"
             (string-append (assoc-ref outputs "out") "/share/man/man1"))
            #t)))))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)
       ;; Required to build manpage
       ("python-sphinxcontrib-newsfeed" ,python-sphinxcontrib-newsfeed)
       ("python-sphinx" ,python-sphinx)))
    (inputs
     `(("sqlite" ,sqlite)
       ("python-configobj" ,python-configobj)
       ("python-dateutil" ,python-dateutil)
       ("python-icalendar" ,python-icalendar)
       ("python-tzlocal" ,python-tzlocal)
       ("python-urwid" ,python-urwid)
       ("python-pytz" ,python-pytz)
       ("python-setproctitle" ,python-setproctitle)
       ("python-atomicwrites" ,python-atomicwrites)
       ("python-click" ,python-click)
       ("python-click-log" ,python-click-log)
       ("python-pyxdg" ,python-pyxdg)))
    (synopsis "Console calendar program")
    (description "Khal is a standards based console calendar program,
able to synchronize with CalDAV servers through vdirsyncer.")
    (home-page "https://lostpackets.de/khal/")
    (license license:expat)))

(define-public remind
  (package
    (name "remind")
    (version "3.3.5")
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
        (base32 "1hbfsq6444abkiws28xqy0k9cwzgzfi1hwfmd1rgm4yydgc1gvb1"))))
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

(define-public confclerk
  (package
    (name "confclerk")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.toastfreeware.priv.at/tarballs/"
                            "confclerk/confclerk-" version ".tar.gz"))
        (sha256
         (base32
          "10rhg44px4nvbkd3p341cmp2ds43jn8r4rvgladda9v8zmsgr2b3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Install directory is currently hard-coded.
               (substitute* "src/app/app.pro"
                 (("PREFIX = /usr/bin")
                  (string-append "PREFIX =" out "/bin")))
               (invoke "qmake"))))
         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share")))
               (install-file "data/confclerk.1"
                             (string-append share "/man/man1"))
               (install-file "data/confclerk.desktop"
                             (string-append share "/applications"))
               (install-file "data/confclerk.svg"
                             (string-append share "/icons/hicolor/scalable/apps"))
               #t))))
       #:tests? #f)) ; no tests
    (native-inputs
     `(("perl" ,perl))) ; pod2man
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://www.toastfreeware.priv.at/confclerk")
    (synopsis "Offline conference schedule application")
    (description
     "ConfClerk is an application written in Qt, which makes conference schedules
available offline.  It displays the conference schedule from various views,
support searches on various items (speaker, speech topic, location, etc.) and
enables you to select favorite events and create your own schedule.

At the moment ConfClerk is able to import schedules in XML format created by
the PentaBarf conference management system (or frab) used by e.g. FOSDEM,
DebConf, FrOSCon, Grazer LinuxTage, and the CCC congresses.

ConfClerk is targeted at mobile devices but works on any system running Qt.")
    (license (list license:gpl2+
                   license:lgpl3)))) ; or cc-by3.0 for src/icons/*

(define-public ccal
  (package
    (name "ccal")
    (version "2.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ccal.chinesebay.com/ccal/ccal-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15nza1d1lvk3dp0wcl53wsd32yhbgyzznha092mh5kh5z74vsk1x"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("/usr/local/bin")
                  (string-append out "/bin"))
                 (("/usr/local/man")
                  (string-append out "/share/man"))))
             #t))
         (add-after 'install 'install-manuals
           (lambda _
             (invoke "make" "install-man"))))
       ;; no tests
       #:tests? #f))
    (home-page "http://ccal.chinesebay.com/ccal/ccal.htm")
    (synopsis "Command line program for Chinese calendar")
    (description "@command{ccal} is a command line program which writes a
Gregorian calendar together with Chinese calendar to standard output.  Its
usage is similar to the @command{cal} program.  In addition to console output,
it can also generate Encapsulated Postscript and HTML table outputs for use in
do-it-yourself calendars and web pages.  It supports both simplified and
traditional Chinese characters.")
    ;; Both licenses are in use in various source files.  Note that
    ;; COPYING.LESSER specifies LGPL 3.0, but all source files say
    ;; 'Lesser GPL version 2 or later'.
    (license (list license:gpl2+ license:lgpl2.1+))))
