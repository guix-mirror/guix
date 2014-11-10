;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu services dmd)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (gnu services)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (dmd-configuration-file))

;;; Commentary:
;;;
;;; Instantiating system services as a dmd configuration file.
;;;
;;; Code:

(define (dmd-configuration-file services)
  "Return the dmd configuration file for SERVICES."
  (define modules
    ;; Extra modules visible to dmd.conf.
    '((guix build syscalls)
      (gnu build file-systems)
      (guix build utils)))

  (mlet %store-monad ((modules  (imported-modules modules))
                      (compiled (compiled-modules modules)))
    (define config
      #~(begin
          (eval-when (expand load eval)
            (set! %load-path (cons #$modules %load-path))
            (set! %load-compiled-path
                  (cons #$compiled %load-compiled-path)))

          (use-modules (ice-9 ftw)
                       (guix build syscalls)
                       (guix build utils)
                       ((gnu build file-systems)
                        #:select (check-file-system canonicalize-device-spec)))

          (register-services
           #$@(map (lambda (service)
                     #~(make <service>
                         #:docstring '#$(service-documentation service)
                         #:provides '#$(service-provision service)
                         #:requires '#$(service-requirement service)
                         #:respawn? '#$(service-respawn? service)
                         #:start #$(service-start service)
                         #:stop #$(service-stop service)))
                   services))

          ;; guix-daemon 0.6 aborts if 'PATH' is undefined, so work around it.
          (setenv "PATH" "/run/current-system/profile/bin")

          (format #t "starting services...~%")
          (for-each start
                    '#$(append-map service-provision
                                   (filter service-auto-start? services)))))

    (gexp->file "dmd.conf" config)))

;;; dmd.scm ends here
