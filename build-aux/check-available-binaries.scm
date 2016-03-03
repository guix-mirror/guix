;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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

;;;
;;; Check whether important binaries are available at hydra.gnu.org.
;;;

(use-modules (guix store)
             (guix grafts)
             (guix packages)
             (guix derivations)
             (gnu packages emacs)
             (gnu packages make-bootstrap)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 format))

(with-store store
  (parameterize ((%graft? #f))
    (let* ((native (append-map (lambda (system)
                                 (map (cut package-derivation store <> system)
                                      (list %bootstrap-tarballs emacs)))
                               %hydra-supported-systems))
           (cross  (map (cut package-cross-derivation store
                             %bootstrap-tarballs <>)
                        '("mips64el-linux-gnuabi64")))
           (total  (append native cross)))

      (set-build-options store #:use-substitutes? #t)
      (let* ((total     (map derivation->output-path total))
             (available (substitutable-paths store total))
             (missing   (lset-difference string=? total available)))
        (if (null? missing)
            (format (current-error-port) "~a packages found substitutable~%"
                    (length total))
            (format (current-error-port)
                    "~a packages are not substitutable:~%~{  ~a~%~}~%"
                    (length missing) missing))
        (exit (null? missing))))))
