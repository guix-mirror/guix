;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Claes Wallin <claes.wallin@greatsinodevelopment.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages pv)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public pv
  (package
    (name "pv")
    (version "1.6.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.ivarch.com/programs/sources/pv-"
                           version ".tar.bz2"))
       (sha256
        (base32 "00y6zla8h653sn4axgqz7rr0x79vfwl62a7gn6lzn607zwg9acg8"))))
    (build-system gnu-build-system)
    (home-page "https://www.ivarch.com/programs/pv.shtml")
    (synopsis "Pipeline progress indicator")
    (description
     "@acronym{pv, Pipe Viewer} is a terminal tool for monitoring the progress
of data through a pipeline.  It can be inserted into any normal pipeline
between two processes.  It gives a visual indication of how quickly data is
passing through, how much has been transferred and how near to completion it is
(with a progress bar), how long it has taken, and an estimate of the remaining
time before completion.")
    (license artistic2.0)))
