;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (gnu services pam-mount)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu system pam)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (pam-mount-configuration
            pam-mount-configuration?
            pam-mount-service-type))

(define %pam-mount-default-configuration
  `((debug (@ (enable "0")))
    (mntoptions (@ (allow ,(string-join
                            '("nosuid" "nodev" "loop"
                              "encryption" "fsck" "nonempty"
                              "allow_root" "allow_other")
                            ","))))
    (mntoptions (@ (require "nosuid,nodev")))
    (logout (@ (wait "0")
               (hup "0")
               (term "no")
               (kill "no")))
    (mkmountpoint (@ (enable "1")
                     (remove "true")))))

(define (make-pam-mount-configuration-file config)
  (computed-file
   "pam_mount.conf.xml"
   #~(begin
       (use-modules (sxml simple))
       (call-with-output-file #$output
         (lambda (port)
           (sxml->xml
            '(*TOP*
              (*PI* xml "version='1.0' encoding='utf-8'")
              (pam_mount
               #$@(pam-mount-configuration-rules config)
               (pmvarrun
                #$(file-append pam-mount
                               "/sbin/pmvarrun -u '%(USER)' -o '%(OPERATION)'"))
               (cryptmount
                #$(file-append pam-mount
                               (string-append
                                "/sbin/mount.crypt"
                                " '%(if %(CIPHER),-ocipher=%(CIPHER))'"
                                " '%(if %(FSKEYCIPHER),"
                                "-ofsk_cipher=%(FSKEYCIPHER))'"
                                " '%(if %(FSKEYHASH),-ofsk_hash=%(FSKEYHASH))'"
                                " '%(if %(FSKEYPATH),-okeyfile=%(FSKEYPATH))'"
                                " '%(if %(OPTIONS),-o%(OPTIONS))'"
                                " '%(VOLUME)' '%(MNTPT)'")))
               (cryptumount
                #$(file-append pam-mount "/sbin/umount.crypt '%(MNTPT)'"))))
            port))))))

(define-record-type* <pam-mount-configuration>
  pam-mount-configuration
  make-pam-mount-configuration
  pam-mount-configuration?
  (rules pam-mount-configuration-rules
         (default %pam-mount-default-configuration)))

(define (pam-mount-etc-service config)
  `(("security/pam_mount.conf.xml"
     ,(make-pam-mount-configuration-file config))))

(define (pam-mount-pam-service config)
  (define optional-pam-mount
    (pam-entry
     (control "optional")
     (module #~(string-append #$pam-mount "/lib/security/pam_mount.so"))))
  (list (lambda (pam)
          (if (member (pam-service-name pam)
                      '("login" "su" "slim" "gdm-password" "sddm"))
              (pam-service
               (inherit pam)
               (auth (append (pam-service-auth pam)
                             (list optional-pam-mount)))
               (session (append (pam-service-session pam)
                                (list optional-pam-mount))))
              pam))))

(define pam-mount-service-type
  (service-type
   (name 'pam-mount)
   (extensions (list (service-extension etc-service-type
                                        pam-mount-etc-service)
                     (service-extension pam-root-service-type
                                        pam-mount-pam-service)))
   (default-value (pam-mount-configuration))
   (description "Activate PAM-Mount support.  It allows mounting volumes for
specific users when they log in.")))
