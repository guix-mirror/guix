;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages scsi)
  #:use-module ((guix licenses)
                #:select (gpl2+ bsd-2 bsd-3))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public sg3-utils
  (package
    (name "sg3-utils")
    (version "1.46")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://sg.danny.cz/sg/p/sg3_utils-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "185rlxppnsmi6q7garfhglmw31gji2ff24xg2yjk3klk1fqnihjr"))))
    (build-system gnu-build-system)
    (home-page "http://sg.danny.cz/sg/sg3_utils.html")
    (synopsis "SCSI device utilities")
    (description
     "sg3-utils is a collection of utilities for devices that use the Small
Computer System Interface (@dfn{SCSI}) command set.  It includes utilities to
read data from, write data to, control, modify, and query the state of SCSI
devices.

For example, this package provides command-line tools to:
@itemize
@item copy data based on @code{dd} syntax and semantics (called @command{sg_dd},
@command{sgp_dd}, and @command{sgm_dd})
@item check @code{INQUIRY} data and @code{VPD pages} (@command{sg_inq})
@item check mode and log pages (@command{sginfo}, @command{sg_modes}, and
@command{sg_logs})
@item spin up and down disks (@command{sg_start})
@item do self-tests (@code{sg_senddiag})
@item parse sense data (@code{sg_decode_sense})
@item and perform various other functions.
@end itemize

In addition, this package includes a library, called libsgutils, which can be
used in C and C++ programs to interact with SCSI devices.")
    ;; See README: "All utilities and libraries have either a "2 clause" BSD
    ;;   license or are "GPL-2ed". [...] That BSD license was updated from the
    ;;   "3 clause" to the newer "2 clause" version on 20180119. To save space
    ;;   various source code files refer to a file called "BSD_LICENSE" [...]."
    ;; Some files (like sg_compare_and_write.c) retain their 3-clause headers!
    (license (list gpl2+ bsd-2 bsd-3))))
