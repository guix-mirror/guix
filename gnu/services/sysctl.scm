;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Sou Bunnbu <iyzsong@member.fsf.org>
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

(define-module (gnu services sysctl)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (sysctl-configuration
            sysctl-configuration-sysctl
            sysctl-configuration-settings
            sysctl-service-type
            %default-sysctl-settings))


;;;
;;; System Control Service.
;;;

(define %default-sysctl-settings
  ;; Default kernel parameters enabled with sysctl.
  '(("fs.protected_hardlinks" . "1")
    ("fs.protected_symlinks" . "1")))

(define-record-type* <sysctl-configuration>
  sysctl-configuration make-sysctl-configuration
  sysctl-configuration?
  (sysctl   sysctl-configuration-sysctl    ; path of the 'sysctl' command
            (default (file-append procps "/sbin/sysctl")))
  (settings sysctl-configuration-settings  ; alist of string pairs
            (default %default-sysctl-settings)))

(define (sysctl-configuration-settings->sysctl.conf settings)
  "Return a file for @command{sysctl} to set kernel parameters as specified by
@var{settings}."
  (apply mixed-text-file "sysctl.conf"
         (append-map (match-lambda
                       ((key . value)
                        (list key "=" value "\n")))
                     settings)))

(define sysctl-shepherd-service
  (match-lambda
    (($ <sysctl-configuration> sysctl settings)
     (let ((sysctl.conf
            (sysctl-configuration-settings->sysctl.conf settings)))
       (shepherd-service
        (documentation "Configure kernel parameters at boot.")
        (provision '(sysctl))
        (start #~(lambda _
                   (zero? (system* #$sysctl "--load" #$sysctl.conf))))
        (one-shot? #t))))))

(define sysctl-service-type
  (service-type
   (name 'sysctl)
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list sysctl-shepherd-service))))
   (compose concatenate)
   (extend (lambda (config settings)
             (sysctl-configuration
              (inherit config)
              (settings (append (sysctl-configuration-settings config)
                                settings)))))
   (default-value (sysctl-configuration))))
