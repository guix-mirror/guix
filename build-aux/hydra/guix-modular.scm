;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2020 Ludovic Courtès <ludo@gnu.org>
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
;;; This file defines a continuous integration job to build the same modular
;;; Guix as 'guix pull', which is defined in (guix self).
;;;

(use-modules (guix store)
             (guix config)
             (guix utils)
             ((guix packages) #:select (%hydra-supported-systems))
             (guix derivations)
             (guix monads)
             ((guix licenses) #:prefix license:)
             (srfi srfi-1)
             (ice-9 match))

;; XXX: Debugging hack: since `hydra-eval-guile-jobs' redirects the output
;; port to the bit bucket, let us write to the error port instead.
(setvbuf (current-error-port) 'line)
(set-current-output-port (current-error-port))

(define* (build-job store source version system)
  "Return a Hydra job a list building the modular Guix derivation from SOURCE
for SYSTEM.  Use VERSION as the version identifier."
  (lambda ()
    (define build
      (primitive-load (string-append source "/build-aux/build-self.scm")))

    `((derivation . ,(derivation-file-name
                      (run-with-store store
                        (build source #:version version #:system system
                               #:pull-version 1
                               #:guile-version "2.2")))) ;the latest 2.2.x
      (description . "Modular Guix")
      (long-description
       . "This is the modular Guix package as produced by 'guix pull'.")
      (license . ,license:gpl3+)
      (home-page . ,%guix-home-page-url)
      (maintainers . (,%guix-bug-report-address)))))

(define (hydra-jobs store arguments)
  "Return Hydra jobs."
  (define systems
    (match (assoc-ref arguments 'systems)
      (#f              %hydra-supported-systems)
      ((lst ...)       lst)
      ((? string? str) (call-with-input-string str read))))

  (define guix-checkout
    (or (assq-ref arguments 'guix)                ;Hydra on hydra
        (assq-ref arguments 'guix-modular)))      ;Cuirass on berlin

  (define version
    (or (assq-ref guix-checkout 'revision)
        "0.unknown"))

  (let ((file (assq-ref guix-checkout 'file-name)))
    (format (current-error-port) "using checkout ~s (~s; arguments: ~s)~%"
            guix-checkout file arguments)

    (map (lambda (system)
           (let ((name (string->symbol
                        (string-append "guix." system))))
             `(,name
               . ,(build-job store file version system))))
         systems)))
