;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build profiles)
  #:use-module (guix build union)
  #:use-module (ice-9 pretty-print)
  #:export (build-profile))

;;; Commentary:
;;;
;;; Build a user profile (essentially the union of all the installed packages)
;;; with its associated meta-data.
;;;
;;; Code:

(define* (build-profile output inputs
                        #:key manifest)
  "Build a user profile from INPUTS in directory OUTPUT.  Write MANIFEST, an
sexp, to OUTPUT/manifest."
  (union-build output inputs
               #:log-port (%make-void-port "w"))
  (call-with-output-file (string-append output "/manifest")
    (lambda (p)
      (pretty-print manifest p))))

;;; profile.scm ends here
