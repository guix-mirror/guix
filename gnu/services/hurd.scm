;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu services hurd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages hurd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (hurd-console-configuration
            hurd-console-service-type
            hurd-getty-configuration
            hurd-getty-service-type))

;;; Commentary:
;;;
;;; This module implements services for the Hurd.
;;;
;;; Code:

;;;
;;; The Hurd VGA console service.
;;;

(define-record-type* <hurd-console-configuration>
  hurd-console-configuration make-hurd-console-configuration
  hurd-console-configuration?
  (hurd   hurd-console-configuration-hurd ;file-like
          (default hurd)))

(define (hurd-console-shepherd-service config)
  "Return a <shepherd-service> for a Hurd VGA console with CONFIG."

  (define console-command
    #~(list
       (string-append #$(hurd-console-configuration-hurd config) "/bin/console")
       "-c" "/dev/vcs"
       "-d" "vga"
       "-d" "pc_kbd"
       "-d" "generic_speaker"))

  (list (shepherd-service
         (documentation "Run the Hurd’s VGA console client.")
         (provision '(console))
         (requirement '(user-processes))
         (start #~(make-forkexec-constructor #$console-command))
         (stop #~(make-kill-destructor)))))

(define hurd-console-service-type
  (service-type
   (name 'console)
   (description "Run the Hurd console client.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             hurd-console-shepherd-service)))
   (default-value (hurd-console-configuration))))


;;;
;;; The Hurd getty service.
;;;

(define-record-type* <hurd-getty-configuration>
  hurd-getty-configuration make-hurd-getty-configuration
  hurd-getty-configuration?
  (hurd       hurd-getty-configuration-hurd  ;file-like
              (default hurd))
  (tty        hurd-getty-configuration-tty)  ;string
  (baud-rate  hurd-getty-configuration-baud-rate
              (default 38400)))              ;integer

(define (hurd-getty-shepherd-service config)
  "Return a <shepherd-service> for a Hurd getty with CONFIG."

  (let ((hurd      (hurd-getty-configuration-hurd config))
        (tty       (hurd-getty-configuration-tty config))
        (baud-rate (hurd-getty-configuration-baud-rate config)))

    (define getty-command
      #~(list
         (string-append #$hurd "/libexec/getty")
         #$(number->string baud-rate)
         #$tty))

    (list
     (shepherd-service
      (documentation "Run getty on a tty.")
      (provision (list (string->symbol (string-append "term-" tty))))
      (requirement '(user-processes console))
      (start #~(make-forkexec-constructor #$getty-command))
      (stop  #~(make-kill-destructor))))))

(define hurd-getty-service-type
  (service-type
   (name 'getty)
   (extensions (list (service-extension shepherd-root-service-type
                                        hurd-getty-shepherd-service)))
   (description
    "Provide console login using the Hurd @command{getty} program.")))

;;; hurd.scm ends here
