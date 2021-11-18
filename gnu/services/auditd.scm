;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Robin Green <greenrd@greenrd.org>
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

(define-module (gnu services auditd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (auditd-configuration
            auditd-service-type
            %default-auditd-configuration-directory))

(define auditd.conf
  (plain-file "auditd.conf" "log_file = /var/log/audit.log\nlog_format = \
ENRICHED\nfreq = 1\nspace_left = 5%\nspace_left_action = \
syslog\nadmin_space_left_action = ignore\ndisk_full_action = \
ignore\ndisk_error_action = syslog\n"))

(define %default-auditd-configuration-directory
  (computed-file "auditd"
                 #~(begin
                     (mkdir #$output)
                     (copy-file #$auditd.conf
                                (string-append #$output "/auditd.conf")))))

(define-record-type* <auditd-configuration>
  auditd-configuration make-auditd-configuration
  auditd-configuration?
  (audit                   auditd-configuration-audit                          ; file-like
                           (default audit))
  (configuration-directory auditd-configuration-configuration-directory))      ; file-like

(define (auditd-shepherd-service config)
  (let* ((audit (auditd-configuration-audit config))
         (configuration-directory (auditd-configuration-configuration-directory config)))
    (list (shepherd-service
           (documentation "Auditd allows you to audit file system accesses and process execution.")
           (provision '(auditd))
           (start #~(make-forkexec-constructor
                     (list (string-append #$audit "/sbin/auditd") "-c" #$configuration-directory)
                     #:pid-file "/var/run/auditd.pid"))
           (stop #~(make-kill-destructor))))))

(define auditd-service-type
  (service-type (name 'auditd)
                (description "Allows auditing file system accesses and process execution.")
                (extensions
                 (list
                  (service-extension shepherd-root-service-type
                                     auditd-shepherd-service)))
                (default-value
                  (auditd-configuration
                   (configuration-directory %default-auditd-configuration-directory)))))
