;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

;; Attempt to use our very own Guix modules.
(eval-when (compile load eval)

  ;; Ignore any available .go, and force recompilation.  This is because our
  ;; checkout in the store has mtime set to the epoch, and thus .go files look
  ;; newer, even though they may not correspond.
  (set! %fresh-auto-compile #t)

  (and=> (assoc-ref (current-source-location) 'filename)
         (lambda (file)
           (let ((dir (dirname file)))
             (format (current-error-port) "prepending ~s to the load path~%"
                     dir)
             (set! %load-path (cons dir %load-path))))))

(use-modules (guix store)
             (guix packages)
             ((guix utils) #:select (%current-system))
             (gnu packages)
             (gnu packages base)
             (gnu packages gawk)
             (gnu packages guile)
             (gnu packages gettext)
             (gnu packages multiprecision)
             (gnu packages make-bootstrap)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match))

;; XXX: Debugging hack: since `hydra-eval-guile-jobs' redirects the output
;; port to the bit bucket, let us write to the error port instead.
(setvbuf (current-error-port) _IOLBF)
(set-current-output-port (current-error-port))

(define* (package->alist store package system
                         #:optional (package-derivation package-derivation))
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

(define (package-cross-job store job-name package target system)
  "Return a job called TARGET.JOB-NAME that cross-builds PACKAGE for TARGET on
SYSTEM."
  `(,(symbol-append (string->symbol target) (string->symbol ".") job-name) .
    ,(cute package->alist store package system
           (cut package-cross-derivation <> <> target <>))))

(define %packages-to-cross-build
  (list gmp mpfr mpc coreutils findutils diffutils patch sed grep
        gawk gettext hello guile-2.0
        ;; %bootstrap-binaries-tarball
        ;; %binutils-bootstrap-tarball
        ;; %glibc-bootstrap-tarball
        ;; %gcc-bootstrap-tarball
        ;; %guile-bootstrap-tarball
        ))

(define %cross-targets
  '("mips64el-linux-gnu"))

(define (hydra-jobs store arguments)
  "Return Hydra jobs."
  (define system
    (or (assoc-ref arguments system)
        (%current-system)))

  (define job-name
    (compose string->symbol package-full-name))

  (define cross-jobs
    (append-map (lambda (target)
                  (map (lambda (package)
                         (package-cross-job store (job-name package)
                                            package target system))
                       %packages-to-cross-build))
                %cross-targets))

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
                         (cons (package-job store (job-name package)
                                            package system)
                               result)))
                   cross-jobs)))
