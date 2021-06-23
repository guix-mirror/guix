;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
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

(define-module (gnu services security-token)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages security-token)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (pcscd-configuration
            pcscd-configuration?
            pcscd-configuration-pcsc-lite
            pcscd-configuration-usb-drivers
            pcscd-service-type))

;;;
;;; PC/SC Smart Card Daemon
;;;

(define-record-type* <pcscd-configuration>
  pcscd-configuration make-pcscd-configuration pcscd-configuration?
  (pcsc-lite pcscd-configuration-pcsc-lite
             (default pcsc-lite))
  (usb-drivers pcscd-configuration-usb-drivers
               (default (list ccid))))

(define pcscd-shepherd-service
  (match-lambda
    (($ <pcscd-configuration> pcsc-lite)
     (with-imported-modules (source-module-closure
                             '((gnu build shepherd)))
       (shepherd-service
        (documentation "PC/SC Smart Card Daemon")
        (provision '(pcscd))
        (requirement '(syslogd))
        (modules '((gnu build shepherd)))
        (start #~(lambda _
                   (let ((socket "/run/pcscd/pcscd.comm"))
                     (when (file-exists? socket)
                       (delete-file socket)))
                   (invoke #$(file-append pcsc-lite "/sbin/pcscd"))
                   (call-with-input-file "/run/pcscd/pcscd.pid" read)))
        (stop #~(make-kill-destructor)))))))

(define pcscd-activation
  (match-lambda
    (($ <pcscd-configuration> pcsc-lite usb-drivers)
     (with-imported-modules (source-module-closure
                             '((guix build utils)))
       #~(begin
           (use-modules (guix build utils))
           ;; XXX: We can't use (guix utils) because it requires a
           ;; dynamically-linked Guile, hence the duplicate switch-symlinks.
           (define (switch-symlinks link target)
             (let ((pivot (string-append link ".new")))
               (symlink target pivot)
               (rename-file pivot link)))
           (mkdir-p "/var/lib")
           (switch-symlinks "/var/lib/pcsc"
                            #$(directory-union
                               "pcsc"
                               (map (cut file-append <> "/pcsc")
                                    usb-drivers))))))))

(define pcscd-service-type
  (service-type
   (name 'pcscd)
   (description
    "Run @command{pcscd}, the PC/SC smart card daemon.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list pcscd-shepherd-service))
          (service-extension activation-service-type
                             pcscd-activation)))
   (default-value (pcscd-configuration))))
