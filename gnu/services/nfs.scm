;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
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

(define-module (gnu services nfs)
  #:use-module (gnu)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages linux)
  #:use-module (guix)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (gnu build file-systems)
  #:export (rpcbind-service-type
            rpcbind-configuration
            rpcbind-configuration?

            pipefs-service-type
            pipefs-configuration
            pipefs-configuration?

            idmap-service-type
            idmap-configuration
            idmap-configuration?

            gss-service-type
            gss-configuration
            gss-configuration?))


(define default-pipefs-directory "/var/lib/nfs/rpc_pipefs")



(define-record-type* <rpcbind-configuration>
  rpcbind-configuration make-rpcbind-configuration
  rpcbind-configuration?
  (rpcbind             rpcbind-configuration-rpcbind
                       (default rpcbind))
  (warm-start?         rpcbind-configuration-warm-start?
                       (default #t)))

(define rpcbind-service-type
  (shepherd-service-type
   'rpcbind
   (lambda (config)
     (define nfs-utils
       (rpcbind-configuration-rpcbind config))

     (define rpcbind-command
       #~(list (string-append #$nfs-utils "/bin/rpcbind") "-f"
               #$@(if (rpcbind-configuration-warm-start? config) '("-w") '())))

     (shepherd-service
      (documentation "Start the RPC bind daemon.")
      (requirement '(networking))
      (provision '(rpcbind-daemon))

      (start #~(make-forkexec-constructor #$rpcbind-command))
      (stop #~(make-kill-destructor))))))



(define-record-type* <pipefs-configuration>
  pipefs-configuration make-pipefs-configuration
  pipefs-configuration?
  (mount-point           pipefs-configuration-mount-point
                         (default default-pipefs-directory)))

(define pipefs-service-type
  (shepherd-service-type
   'pipefs
   (lambda (config)
     (define pipefs-directory (pipefs-configuration-mount-point config))

     (shepherd-service
      (documentation "Mount the pipefs pseudo file system.")
      (provision '(rpc-pipefs))

      (start #~(lambda ()
                 (mkdir-p #$pipefs-directory)
                 (mount "rpc_pipefs" #$pipefs-directory "rpc_pipefs")
                 (member #$pipefs-directory (mount-points))))

      (stop #~(lambda (pid . args)
                (umount #$pipefs-directory MNT_DETACH)
                (not (member #$pipefs-directory (mount-points)))))))))



(define-record-type* <gss-configuration>
  gss-configuration make-gss-configuration
  gss-configuration?
  (pipefs-directory            gss-configuration-pipefs-directory
                         (default default-pipefs-directory))
  (nfs-utils             gss-configuration-gss
                         (default nfs-utils)))

(define gss-service-type
  (shepherd-service-type
   'gss
   (lambda (config)
     (define nfs-utils
       (gss-configuration-gss config))

     (define pipefs-directory
       (gss-configuration-pipefs-directory config))

     (define gss-command
       #~(list (string-append #$nfs-utils "/sbin/rpc.gssd") "-f"
               "-p" #$pipefs-directory))

     (shepherd-service
      (documentation "Start the RPC GSS daemon.")
      (requirement '(rpcbind-daemon rpc-pipefs))
      (provision '(gss-daemon))

      (start #~(make-forkexec-constructor #$gss-command))
      (stop #~(make-kill-destructor))))))



(define-record-type* <idmap-configuration>
  idmap-configuration make-idmap-configuration
  idmap-configuration?
  (pipefs-directory            idmap-configuration-pipefs-directory
                         (default default-pipefs-directory))
  (domain                idmap-configuration-domain
                           (default #f))
  (nfs-utils             idmap-configuration-idmap
                         (default nfs-utils)))

(define idmap-service-type
  (shepherd-service-type
   'idmap
   (lambda (config)

     (define nfs-utils
       (idmap-configuration-idmap config))

     (define pipefs-directory
       (idmap-configuration-pipefs-directory config))

     (define domain (idmap-configuration-domain config))

     (define (idmap-config-file config)
       (plain-file "idmapd.conf"
                   (string-append
                    "\n[General]\n"
                    (if domain
                        (format #f "Domain = ~a\n" domain))
                    "\n[Mapping]\n"
                    "Nobody-User = nobody\n"
                    "Nobody-Group = nogroup\n")))

     (define idmap-command
       #~(list (string-append #$nfs-utils "/sbin/rpc.idmapd") "-f"
               "-p" #$pipefs-directory
               "-c" #$(idmap-config-file config)))

     (shepherd-service
       (documentation "Start the RPC IDMAP daemon.")
       (requirement '(rpcbind-daemon rpc-pipefs))
       (provision '(idmap-daemon))
       (start #~(make-forkexec-constructor #$idmap-command))
       (stop #~(make-kill-destructor))))))

