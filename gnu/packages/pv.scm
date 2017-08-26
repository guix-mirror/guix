;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Claes Wallin <claes.wallin@greatsinodevelopment.com>
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
    (version "1.6.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.ivarch.com/programs/sources/pv-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "1wbk14xh9rfypiwyy68ssl8dliyji30ly70qki1y2xx3ywszk3k0"))))
    (build-system gnu-build-system)
    (home-page "https://www.ivarch.com/programs/pv.shtml")
    (synopsis "Pipeline progress indicator")
    (description
     "pv (Pipe Viewer) is a terminal-based tool for monitoring the progress
of data through a pipeline.  It can be inserted into any normal pipeline
between two processes to give a visual indication of how quickly data is
passing through, how long it has taken, how near to completion it is, and an
estimate of how long it will be until completion.")
    (license artistic2.0)))
