;;; GNU Guix --- Functional package management for GNU
;;; Copyright 2014  John Darrington <jmd@gnu.org>
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

(define-module (gnu packages ntp)
  #:use-module (gnu packages)
  #:use-module (gnu packages which)
  #:use-module (gnu packages linux)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1))

(define-public ntp
  (package
   (name "ntp")
   (version "4.2.6p5")
   (source (origin
	    (method url-fetch)
	    (uri (string-append 
                  "http://www.eecis.udel.edu/~ntp/ntp_spool/ntp4/ntp-"
                  (string-join (take (string-split version #\.) 2) ".")
                  "/ntp-" version ".tar.gz"))
	    (sha256
	     (base32
	      "077r69a41hasl8zf5c44km7cqgfhrkaj6a4jnr75j7nkz5qq7ayn"))))
   (native-inputs `(("which" ,which)))
   (inputs
    ;; Build with POSIX capabilities support on GNU/Linux.  This allows 'ntpd'
    ;; to run as non-root (when invoked with '-u'.)
    (if (string-suffix? "-linux"
                        (or (%current-target-system) (%current-system)))
        `(("libcap" ,libcap))
        '()))
   (build-system gnu-build-system)
   (synopsis "Real time clock synchonization system")
   (description "NTP is a system designed to synchronize the clocks of
computers over a network.")
   (license (x11-style
             "http://www.eecis.udel.edu/~mills/ntp/html/copyright.html"
             "A non-copyleft free licence from the University of Delaware"))
   (home-page "http://www.ntp.org")))
