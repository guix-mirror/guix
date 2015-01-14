;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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
             (guix packages)
             (guix derivations)
             (gnu packages emacs)
             (gnu packages make-bootstrap)
             (srfi srfi-1)
             (srfi srfi-26))

(with-store store
  (let* ((native (append-map (lambda (system)
                               (map (cut package-derivation store <> system)
                                    (list %bootstrap-tarballs emacs)))
                             %supported-systems))
         (cross  (map (cut package-cross-derivation store
                           %bootstrap-tarballs <>)
                      '("mips64el-linux-gnuabi64")))
         (total  (append native cross)))
    (define (warn item system)
      (format (current-error-port) "~a (~a) is not substitutable~%"
              item system)
      #f)

    (set-build-options store #:use-substitutes? #t)
    (let* ((substitutable? (substitution-oracle store total))
           (result         (every (lambda (drv)
                                    (let ((out (derivation->output-path drv)))
                                      (or (substitutable? out)
                                          (warn out (derivation-system drv)))))
                                  total)))
      (when result
        (format (current-error-port) "~a packages found substitutable~%"
                (length total)))
      (exit result))))
