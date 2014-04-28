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

(define (dmd-configuration-file services etc)
  "Return the dmd configuration file for SERVICES, that initializes /etc from
ETC (the derivation that builds the /etc directory) on startup."
  (define config
    #~(begin
        (use-modules (ice-9 ftw))

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

        ;; /etc is a mixture of static and dynamic settings.  Here is where we
        ;; initialize it from the static part.
        (format #t "populating /etc from ~a...~%" #$etc)
        (let ((rm-f (lambda (f)
                      (false-if-exception (delete-file f)))))
          (rm-f "/etc/static")
          (symlink #$etc "/etc/static")
          (for-each (lambda (file)
                      ;; TODO: Handle 'shadow' specially so that changed
                      ;; password aren't lost.
                      (let ((target (string-append "/etc/" file))
                            (source (string-append "/etc/static/" file)))
                        (rm-f target)
                        (symlink source target)))
                    (scandir #$etc
                             (lambda (file)
                               (not (member file '("." ".."))))))

          ;; Prevent ETC from being GC'd.
          (rm-f "/var/guix/gcroots/etc-directory")
          (symlink #$etc "/var/guix/gcroots/etc-directory"))

        ;; guix-daemon 0.6 aborts if 'PATH' is undefined, so work around it.
        (setenv "PATH" "/run/current-system/bin")

        (format #t "starting services...~%")
        (for-each start '#$(append-map service-provision services))))

  (gexp->file "dmd.conf" config))

;;; dmd.scm ends here
