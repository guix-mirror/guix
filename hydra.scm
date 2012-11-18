;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

;;;
;;; This file defines build jobs for the Hydra continuation integration
;;; tool.
;;;

(use-modules (guix store)
             (guix packages)
             (distro)
             (distro packages guile)
             (ice-9 match))

(define (package->alist store package system)
  "Convert PACKAGE to an alist suitable for Hydra."
  `((derivation . ,(package-derivation store package system))
    (description . ,(package-synopsis package))
    (long-description . ,(package-description package))
    (license . ,(package-license package))
    (maintainers . ("gnu-system-discuss@gnu.org"))))

(define (package-job store job-name package system)
  "Return a job called JOB-NAME that builds PACKAGE on SYSTEM."
  `(,job-name . ,(package->alist store package system)))

(define (hydra-jobs store arguments)
  "Return Hydra jobs."
  (define system
    (assoc-ref arguments "system"))

  (map (match-lambda
        ((job-name (? package? package))
         (package-job store job-name package system))
        ((job-name (? string? name))
         (package-job store job-name
                      (car (find-packages-by-name name))
                      system)))
       `((hello "hello")
         (gmp "gmp")
         (guile_2_0 ,guile-2.0)
         (guile_1_8 ,guile-1.8))))
