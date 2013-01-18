;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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
;;; This file defines build jobs for the Hydra continuation integration
;;; tool.
;;;

(use-modules (guix store)
             (guix packages)
             ((guix utils) #:select (%current-system))
             (gnu packages)
             (gnu packages base)
             (gnu packages guile)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match))

;; XXX: Debugging hack: since `hydra-eval-guile-jobs' redirects the output
;; port to the bit bucket, let us write to the error port instead.
(setvbuf (current-error-port) _IOLBF)
(set-current-output-port (current-error-port))

(define (package->alist store package system)
  "Convert PACKAGE to an alist suitable for Hydra."
  `((derivation . ,(package-derivation store package system))
    (description . ,(package-synopsis package))
    (long-description . ,(package-description package))
    (license . ,(package-license package))
    (home-page . ,(package-home-page package))
    (maintainers . ("bug-guix@gnu.org"))))

(define (package-job store job-name package system)
  "Return a job called JOB-NAME that builds PACKAGE on SYSTEM."
  `(,job-name . ,(cut package->alist store package system)))

(define (hydra-jobs store arguments)
  "Return Hydra jobs."
  (define system
    (or (assoc-ref arguments system)
        (%current-system)))

  ;; Return one job for each package, except bootstrap packages.
  (let ((base-packages (delete-duplicates
                        (append-map (match-lambda
                                     ((_ package _ ...)
                                      (match (package-transitive-inputs
                                              package)
                                        (((_ inputs _ ...) ...)
                                         inputs))))
                                    %final-inputs))))
    (fold-packages (lambda (package result)
                     (if (member package base-packages)
                         result
                         (let ((name (string->symbol
                                      (package-full-name package))))
                           (cons (package-job store name package
                                              system)
                                 result))))
                   '())))
