;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python))

(define-public time
  (package
    (name "time")
    (version "1.8")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/time/time-"
                          version ".tar.gz"))
      (sha256
       (base32
        "06rfg8dn0q2r8pdq8i6brrs6rqrsgvkwbbl4kfx3a6lnal0m8bwa"))))
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
    (version "2017.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytzdata" version))
       (sha256
        (base32
         "1c1az8spm2d3km6qhjy69y4dlj71p6984l48mizr83nh4f0ipld4"))))
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
