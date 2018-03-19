;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu services mcron)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:autoload   (gnu packages guile) (mcron)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (mcron-configuration
            mcron-configuration?
            mcron-configuration-mcron
            mcron-configuration-jobs

            mcron-service-type
            mcron-service))

;;; Commentary:
;;;
;;; This module implements a service that to run instances of GNU mcron, a
;;; periodic job execution daemon.  Example of a service:
;;
;;  (service mcron-service-type
;;           (mcron-configuration
;;            (jobs (list #~(job next-second-from
;;                               (lambda ()
;;                                 (call-with-output-file "/dev/console"
;;                                   (lambda (port)
;;                                     (display "hello!\n" port)))))))))
;;;
;;; Code:

(define-record-type* <mcron-configuration> mcron-configuration
  make-mcron-configuration
  mcron-configuration?
  (mcron             mcron-configuration-mcron    ;package
                     (default mcron))
  (jobs              mcron-configuration-jobs     ;list of <mcron-job>
                     (default '())))

(define (job-file job)
  (scheme-file "mcron-job" job))

(define mcron-shepherd-services
  (match-lambda
    (($ <mcron-configuration> mcron ())           ;nothing to do!
     '())
    (($ <mcron-configuration> mcron jobs)
     (list (shepherd-service
            (provision '(mcron))
            (requirement '(user-processes))
            (modules `((srfi srfi-1)
                       (srfi srfi-26)
                       ,@%default-modules))
            (start #~(make-forkexec-constructor
                      (list (string-append #$mcron "/bin/mcron")
                            #$@(map job-file jobs))

                      ;; Disable auto-compilation of the job files and set a
                      ;; sane value for 'PATH'.
                      #:environment-variables
                      (cons* "GUILE_AUTO_COMPILE=0"
                             "PATH=/run/current-system/profile/bin"
                             (remove (cut string-prefix? "PATH=" <>)
                                     (environ)))))
            (stop #~(make-kill-destructor)))))))

(define mcron-service-type
  (service-type (name 'mcron)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          mcron-shepherd-services)
                       (service-extension profile-service-type
                                          (compose list
                                                   mcron-configuration-mcron))))
                (compose concatenate)
                (extend (lambda (config jobs)
                          (mcron-configuration
                           (inherit config)
                           (jobs (append (mcron-configuration-jobs config)
                                         jobs)))))
                (default-value (mcron-configuration)))) ;empty job list

(define* (mcron-service jobs #:optional (mcron mcron))
  "Return an mcron service running @var{mcron} that schedules @var{jobs}, a
list of gexps denoting mcron job specifications.

This is a shorthand for:
@example
  (service mcron-service-type
           (mcron-configuration (mcron mcron) (jobs jobs)))
@end example
"
  (service mcron-service-type
           (mcron-configuration (mcron mcron) (jobs jobs))))

;;; mcron.scm ends here
