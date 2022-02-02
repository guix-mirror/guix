;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 David Thompson <davet@gnu.org>
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
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

(define-module (gnu machine)
  #:use-module (gnu system)
  #:use-module (guix derivations)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module ((guix diagnostics) #:select (source-properties->location))
  #:use-module (srfi srfi-35)
  #:export (environment-type
            environment-type?
            environment-type-name
            environment-type-description
            environment-type-location

            machine
            machine?

            machine-operating-system
            machine-environment
            machine-configuration
            machine-display-name

            deploy-machine
            roll-back-machine
            machine-remote-eval

            &deploy-error
            deploy-error?
            deploy-error-should-roll-back
            deploy-error-captured-args))

;;; Commentary:
;;;
;;; This module provides the types used to declare individual machines in a
;;; heterogeneous Guix deployment. The interface allows users to specify system
;;; configurations and the means by which resources should be provisioned on a
;;; per-host basis.
;;;
;;; Code:


;;;
;;; Declarations for resources that can be provisioned.
;;;

(define-record-type* <environment-type> environment-type
  make-environment-type
  environment-type?

  ;; Interface to the environment type's deployment code. Each procedure
  ;; should take the same arguments as the top-level procedure of this file
  ;; that shares the same name. For example, 'machine-remote-eval' should be
  ;; of the form '(machine-remote-eval machine exp)'.
  (machine-remote-eval environment-type-machine-remote-eval) ; procedure
  (deploy-machine      environment-type-deploy-machine)      ; procedure
  (roll-back-machine   environment-type-roll-back-machine)   ; procedure

  ;; Metadata.
  (name        environment-type-name)       ; symbol
  (description environment-type-description ; string
               (default #f))
  (location    environment-type-location    ; <location>
               (default (and=> (current-source-location)
                               source-properties->location))
               (innate)))


;;;
;;; Declarations for machines in a deployment.
;;;

(define-record-type* <machine> machine make-machine
  machine?
  (operating-system %machine-operating-system); <operating-system>
  (environment      machine-environment)      ; symbol
  (configuration    machine-configuration     ; configuration object
                    (default #f)))            ; specific to environment

(define (machine-operating-system machine)
  "Return the operating system of MACHINE."
  (operating-system-with-provenance
   (%machine-operating-system machine)))

(define (machine-display-name machine)
  "Return the host-name identifying MACHINE."
  (operating-system-host-name (machine-operating-system machine)))

(define (machine-remote-eval machine exp)
  "Evaluate EXP, a gexp, on MACHINE. Ensure that all the elements EXP refers to
are built and deployed to MACHINE beforehand."
  (let ((environment (machine-environment machine)))
    ((environment-type-machine-remote-eval environment) machine exp)))

(define (deploy-machine machine)
  "Monadic procedure transferring the new system's OS closure to the remote
MACHINE, activating it on MACHINE and switching MACHINE to the new generation."
  (let ((environment (machine-environment machine)))
    ((environment-type-deploy-machine environment) machine)))

(define (roll-back-machine machine)
  "Monadic procedure rolling back to the previous system generation on
MACHINE. Return the number of the generation that was current before switching
and the new generation number."
  (let ((environment (machine-environment machine)))
    ((environment-type-roll-back-machine environment) machine)))


;;;
;;; Error types.
;;;

(define-condition-type &deploy-error &error
  deploy-error?
  (should-roll-back deploy-error-should-roll-back)
  (captured-args deploy-error-captured-args))
