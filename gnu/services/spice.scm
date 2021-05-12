;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
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

(define-module (gnu services spice)
  #:use-module (gnu packages spice)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (spice-vdagent-configuration
            spice-vdagent-configuration?
            spice-vdagent-service-type
            spice-vdagent-service))

(define-record-type* <spice-vdagent-configuration>
  spice-vdagent-configuration make-spice-vdagent-configuration
  spice-vdagent-configuration?
  (spice-vdagent spice-vdagent-configuration-spice-vdagent
                 (default spice-vdagent)))

(define (spice-vdagent-shepherd-service config)
  "Return a <shepherd-service> for spice-vdagentd with CONFIG."
  (define spice-vdagent (spice-vdagent-configuration-spice-vdagent config))

  (define spice-vdagentd-command
    (list
     (file-append spice-vdagent "/sbin/spice-vdagentd")
     "-x"))

  (list
   (shepherd-service
    (documentation "Spice vdagentd service")
    (requirement '(dbus-system))
    (provision '(spice-vdagentd))
    (start #~(lambda args
               ;; spice-vdagentd supports being activated upon the client
               ;; connecting to its socket; when not using such feature, the
               ;; socket should not exist before vdagentd creates it itself.
               (mkdir-p "/run/spice-vdagentd")
               (false-if-exception
                (delete-file "/run/spice-vdagentd/spice-vdagent-sock"))
               (fork+exec-command '#$spice-vdagentd-command)))
    (stop #~(make-kill-destructor)))))

(define spice-vdagent-profile
  (compose list spice-vdagent-configuration-spice-vdagent))

(define spice-vdagent-service-type
  (service-type
   (name 'spice-vdagent)
   (default-value (spice-vdagent-configuration))
   (extensions
    (list (service-extension shepherd-root-service-type
                             spice-vdagent-shepherd-service)
          (service-extension profile-service-type
                             spice-vdagent-profile)))))

(define* (spice-vdagent-service
          #:optional (config (spice-vdagent-configuration)))
  "Start the @command{vdagentd} and @command{vdagent} daemons
from @var{spice-vdagent} to enable guest window resizing and
clipboard sharing."
  (service spice-vdagent-service-type config))
