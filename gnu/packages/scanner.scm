;;; GNU Guix --- Functional package management for GNU
;;; Copyright 2014 John Darrington <jmd@gnu.org>
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

(define-module (gnu packages scanner)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses)
                #:prefix licence:))

(define-public sane-backends
  (package
    (name "sane-backends")
    (version "1.0.24")
    (source (origin
             (method url-fetch)
             (uri (string-append 
                   "https://alioth.debian.org/frs/download.php/file/3958/"
                   name "-" version ".tar.gz"))
             (sha256
              (base32
               "0ba68m6bzni54axjk15i51rya7hfsdliwvqyan5msl7iaid0iir7"))))
    (build-system gnu-build-system)
    (arguments
      `(#:tests? #f)) 
    ;; It would seem that tests are not maintained - fails with
    ;; the following:
    ;;
    ;; < This page was last updated on Wed Jul 31 07:52:48 2013
    ;; <  by sane-desc 3.5 from sane-backends 1.0.24git
    ;; ---
    ;; > This page was last updated on Sun Oct 19 15:41:39 2014
    ;; >  by sane-desc 3.5 from sane-backends 1.0.24
    ;; **** File generated for html-backends-split mode is different from reference
    ;; Makefile:501: recipe for target 'check.local' failed
    (home-page "http://www.sane-project.org")
    (synopsis "Raster image scanner library and drivers")
    (description "SANE stands for \"Scanner Access Now Easy\" and is an API
proving access to any raster image scanner hardware (flatbed scanner,
hand-held scanner, video- and still-cameras, frame-grabbers, etc.).  The
package contains the library and drivers.")
    (license licence:gpl2+))) ; plus linking exception

