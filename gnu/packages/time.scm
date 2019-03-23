;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015, 2017 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019 Kyle Meyer <kyle@kyleam.com>
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

(define-module (gnu packages time)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public time
  (package
    (name "time")
    (version "1.9")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/time/time-"
                          version ".tar.gz"))
      (sha256
       (base32
        "07jj7cz6lc13iqrpgn81ivqh8rkm73p4rnivwgrrshk23v4g1b7v"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/time/")
    (synopsis "Run a command, then display its resource usage")
    (description
     "Time is a command that displays information about the resources that a
program uses.  The display output of the program can be customized or saved
to a file.")
    (license gpl3+)))

(define-public python-pytzdata
  (package
    (name "python-pytzdata")
    (version "2017.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytzdata" version))
       (sha256
        (base32
         "1wi3jh39zsa9iiyyhynhj7w5b2p9wdyd0ppavpsrmf3wxvr7cwz8"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/sdispater/pytzdata")
    (synopsis "Timezone database for Python")
    (description
     "This library provides a timezone database for Python.")
    (license expat)))

(define-public python2-tzdata
  (package-with-python2 python-pytzdata))

(define-public python-pytz
  (package
    (name "python-pytz")
    (version "2018.9")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "pytz" version))
      (sha256
       (base32
        "0k1pqnq3c4gvcspjxf3fschdjn71fk89i4wz801rxqh7f145xw6m"))))
    (build-system python-build-system)
    (home-page "http://pythonhosted.org/pytz")
    (synopsis "Python timezone library")
    (description "This library brings the Olson tz database into Python.  It
allows accurate and cross platform timezone calculations using Python 2.4 or
higher.  It also solves the issue of ambiguous times at the end of daylight
saving time.  Almost all of the Olson timezones are supported.")
    (license expat)))

(define-public python2-pytz
  (package-with-python2 python-pytz))

(define-public python-pendulum
  (package
    (name "python-pendulum")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pendulum" version))
       (sha256
        (base32
         "1fj36yxi2f4lzchzd8ny1qjl67dbypnk0gn8qwad2w78579m8m8z"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-pytzdata" ,python-pytzdata)
       ("python-tzlocal" ,python-tzlocal)))
    (home-page "https://github.com/sdispater/pendulum")
    (synopsis "Alternate API for Python datetimes")
    (description "Pendulum is a drop-in replacement for the standard
@{datetime} class, providing an alternative API.  As it inherits from the
standard @code{datetime} all @code{datetime} instances can be replaced by
Pendulum instances.")
    (license expat)))

(define-public python2-pendulum
  (package-with-python2 python-pendulum))

(define-public python-dateutil
  (package
    (name "python-dateutil")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-dateutil" version))
       (sha256
        (base32
         "17nsfhy4xdz1khrfxa61vd7pmvd5z0wa3zb6v4gb4kfnykv0b668"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      ;; Delete tests that depend on "freezegun" to avoid a
                      ;; circular dependency.
                      (delete-file "dateutil/test/test_utils.py")
                      (delete-file "dateutil/test/test_rrule.py")

                      ;; XXX: Fails to get timezone from /etc/localtime.
                      (delete-file "dateutil/test/test_tz.py")

                      (invoke "pytest" "-vv"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://dateutil.readthedocs.io/en/stable/")
    (synopsis "Extensions to the standard datetime module")
    (description
     "The dateutil module provides powerful extensions to the standard
datetime module, available in Python 2.3+.")
    ;; The license was changed from the three-clause BSD license to a dual
    ;; Apache 2.0/BSD-3 variant at 2017-12-01.  Some code is only available as
    ;; BSD-3 still; but all new code is dual licensed (the user can choose).
    (license (list bsd-3 asl2.0))))

(define-public python2-dateutil
  (package-with-python2 python-dateutil))

(define-public python-parsedatetime
  (package
    (name "python-parsedatetime")
    (version "2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "parsedatetime" version))
       (sha256
        (base32
         "0jxqkjks7z9dn222cqgvskp4wr6d92aglinxq7pd2w4mzdc7r09x"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-pyicu" ,python-pyicu)
       ("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (propagated-inputs
     `(("python-future" ,python-future)))
    (home-page "https://github.com/bear/parsedatetime/")
    (synopsis "Parse human-readable date/time text")
    (description
     "Parse human-readable date/time text.")
    (license asl2.0)))

(define-public python2-parsedatetime
  (package-with-python2 python-parsedatetime))

(define-public python-tzlocal
  (package
    (name "python-tzlocal")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tzlocal" version))
       (sha256
        (base32
         "0kiciwiqx0bv0fbc913idxibc4ygg4cb7f8rcpd9ij2shi4bigjf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-symlink-test
           ;; see: https://github.com/regebro/tzlocal/issues/53
           (lambda _
             (delete-file "tzlocal/test_data/symlink_localtime/etc/localtime")
             (symlink "../usr/share/zoneinfo/Africa/Harare"
                      "tzlocal/test_data/symlink_localtime/etc/localtime")
             #t)))))
    (propagated-inputs
     `(("python-pytz" ,python-pytz)))
    (native-inputs
     `(("python-mock" ,python-mock)))
    (home-page "https://github.com/regebro/tzlocal")
    (synopsis "Local timezone information for Python")
    (description
     "Tzlocal returns a tzinfo object with the local timezone information.
This module attempts to fix a glaring hole in pytz, that there is no way to
get the local timezone information, unless you know the zoneinfo name, and
under several distributions that's hard or impossible to figure out.")
    (license expat)))

(define-public python-isodate
  (package
    (name "python-isodate")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "isodate" version))
       (sha256
        (base32
         "1n7jkz68kk5pwni540pr5zdh99bf6ywydk1p5pdrqisrawylldif"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-six" ,python-six)))
    (home-page "https://github.com/gweis/isodate/")
    (synopsis "Python date parser and formatter")
    (description
     "Python-isodate is a python module for parsing and formatting
ISO 8601 dates, time and duration.")
    (license bsd-3)))

(define-public python2-isodate
  (package-with-python2 python-isodate))

(define-public python-iso8601
  (package
    (name "python-iso8601")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "iso8601" version))
       (sha256
        (base32
         "10nyvvnrhw2w3p09v1ica4lgj6f4g9j3kkfx17qmraiq3w7b5i29"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://bitbucket.org/micktwomey/pyiso8601")
    (synopsis "Module to parse ISO 8601 dates")
    (description
     "This module parses the most common forms of ISO 8601 date strings (e.g.
@code{2007-01-14T20:34:22+00:00}) into @code{datetime} objects.")
    (license expat)))

(define-public python2-iso8601
  (package-with-python2 python-iso8601))

(define-public python-monotonic
  (package
    (name "python-monotonic")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "monotonic" version))
       (sha256
        (base32
         "1c6z46yb600klbfhqadyl7vq0jdjdxkm72k43ra3iw3d0xakv593"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ; no tests
    (home-page "https://github.com/atdt/monotonic")
    (synopsis "Implementation of time.monotonic() for Python 2 & < 3.3")
    (description
     "This module provides a @code{monotonic()} function which returns the
value (in fractional seconds) of a clock which never goes backwards.")
    (license asl2.0)))

(define-public python2-monotonic
  (package-with-python2 python-monotonic))

(define-public python-pyrfc3339
  (package
    (name "python-pyrfc3339")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyRFC3339" version))
       (sha256
        (base32
         "06jv7ar7lpvvk0dixzwdr3wgm0g1lipxs429s2z7knwwa7hwpf41"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytz" ,python-pytz)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/kurtraschke/pyRFC3339")
    (synopsis "Python timestamp library")
    (description "Python library for generating and parsing RFC 3339-compliant
timestamps.")
    (license expat)))

(define-public python2-pyrfc3339
  (package-with-python2 python-pyrfc3339))

(define-public python-arrow
  (package
    (name "python-arrow")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "arrow" version))
              (sha256
               (base32
                "08n7q2l69hlainds1byd4lxhwrq7zsw7s640zkqc3bs5jkq0cnc0"))))
    (build-system python-build-system)
    (native-inputs
     `(;; For testing
       ("python-chai" ,python-chai)
       ("python-simplejson" ,python-simplejson)))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)))
    (home-page "https://github.com/crsmithdev/arrow/")
    (synopsis "Dates and times for Python")
    (description
     "Arrow is a Python library to creating, manipulating, formatting and
converting dates, times, and timestamps.  It implements and updates the
datetime type.")
    (license asl2.0)))

(define-public python2-arrow
  (package-with-python2 python-arrow))

(define-public python-aniso8601
  (package
    (name "python-aniso8601")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aniso8601" version))
       (sha256
        (base32
         "1waj54iv3n3lw1fapbz8a93yjgrybgpc86wif5baxdh1arpj9df3"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)))
    (home-page "https://bitbucket.org/nielsenb/aniso8601")
    (synopsis "Python library for parsing ISO 8601 strings")
    (description
     "This package contains a library for parsing ISO 8601 datetime strings.")
    (license bsd-3)))

(define-public python2-aniso8601
  (package-with-python2 python-aniso8601))

(define-public datefudge
  (package
    (name "datefudge")
    (version "1.22")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://salsa.debian.org/debian/datefudge.git")
                    (commit (string-append "debian/" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fmd05r00wx4zc90lbi804jl7xwdl11jq2a1kp5lqimk3yyvfw4c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags (list "CC=gcc"
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile
           (lambda _
             (substitute* "Makefile"
               ((" -o root -g root") "")
               (("VERSION := \\$\\(shell dpkg-parsechangelog .*")
                (string-append "VERSION = " ,version)))
             #t))
         (delete 'configure))))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "https://salsa.debian.org/debian/datefudge")
    (synopsis "Pretend the system date is different")
    (description
     "Utility that fakes the system time by pre-loading a small library that
modifies the @code{time}, @code{gettimeofday} and @code{clock_gettime} system
calls.")
    (license gpl2)))
