;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (distro packages time)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public time
  (package
    (name "time")
    (version "1.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/time/time-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0va9063fcn7xykv658v2s9gilj2fq4rcdxx2mn2mmy1v4ndafzp3"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/time/")
    (synopsis
     "GNU Time, a tool that runs programs and summarizes the system
resources they use")
    (description
     "The 'time' command runs another program, then displays information
about the resources used by that program, collected by the system while
the program was running.  You can select which information is reported
and the format in which it is shown, or have 'time' save the information
in a file instead of displaying it on the screen.

The resources that 'time' can report on fall into the general categories
of time, memory, and I/O and IPC calls.  Some systems do not provide
much information about program resource use; 'time' reports unavailable
information as zero values.
")
    (license gpl2+)))