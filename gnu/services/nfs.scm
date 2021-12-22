;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (gnu packages nfs)
  #:use-module (guix)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
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
            gss-configuration?

            nfs-service-type
            nfs-configuration
            nfs-configuration?))


(define default-pipefs-directory "/var/lib/nfs/rpc_pipefs")



(define-record-type* <rpcbind-configuration>
  rpcbind-configuration make-rpcbind-configuration
  rpcbind-configuration?
  (rpcbind             rpcbind-configuration-rpcbind
                       (default rpcbind))
  (warm-start?         rpcbind-configuration-warm-start?
                       (default #t)))

(define rpcbind-service-type
  (let ((proc
         (lambda (config)
           (define rpcbind
             (rpcbind-configuration-rpcbind config))

           (define rpcbind-command
             #~(list (string-append #$rpcbind "/sbin/rpcbind") "-f"
                     #$@(if (rpcbind-configuration-warm-start? config) '("-w") '())))

           (shepherd-service
            (documentation "Start the RPC bind daemon.")
            (requirement '(networking))
            (provision '(rpcbind-daemon))

            (start #~(make-forkexec-constructor #$rpcbind-command))
            (stop #~(make-kill-destructor))))))
    (service-type
     (name 'rpcbind)
     (extensions
      (list (service-extension shepherd-root-service-type
                               (compose list proc))))
     ;; We use the extensions feature to allow other services to automatically
     ;; configure and start this service.  Only one value can be provided.  We
     ;; override it with the value returned by the extending service.
     (compose identity)
     (extend (lambda (config values)
               (match values
                 ((first . rest) first)
                 (_ config))))
     (default-value (rpcbind-configuration)))))



(define-record-type* <pipefs-configuration>
  pipefs-configuration make-pipefs-configuration
  pipefs-configuration?
  (mount-point           pipefs-configuration-mount-point
                         (default default-pipefs-directory)))

(define pipefs-service-type
  (let ((proc
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
    (service-type
     (name 'pipefs)
     (extensions
      (list (service-extension shepherd-root-service-type
                               (compose list proc))))
     ;; We use the extensions feature to allow other services to automatically
     ;; configure and start this service.  Only one value can be provided.  We
     ;; override it with the value returned by the extending service.
     (compose identity)
     (extend (lambda (config values) (first values)))
     (default-value (pipefs-configuration)))))



(define-record-type* <gss-configuration>
  gss-configuration make-gss-configuration
  gss-configuration?
  (pipefs-directory      gss-configuration-pipefs-directory
                         (default default-pipefs-directory))
  (nfs-utils             gss-configuration-gss
                         (default nfs-utils)))

(define gss-service-type
  (let ((proc
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
    (service-type
     (name 'gss)
     (extensions
      (list (service-extension shepherd-root-service-type
                               (compose list proc))))
     ;; We use the extensions feature to allow other services to automatically
     ;; configure and start this service.  Only one value can be provided.  We
     ;; override it with the value returned by the extending service.
     (compose identity)
     (extend (lambda (config values)
               (match values
                 ((first . rest) first)
                 (_ config))))
     (default-value (gss-configuration)))))



(define-record-type* <idmap-configuration>
  idmap-configuration make-idmap-configuration
  idmap-configuration?
  (pipefs-directory      idmap-configuration-pipefs-directory
                         (default default-pipefs-directory))
  (domain                idmap-configuration-domain
                         (default #f))
  (nfs-utils             idmap-configuration-nfs-utils
                         (default nfs-utils))
  (verbosity             idmap-configuration-verbosity
                         (default 0)))

(define idmap-service-type
  (let ((proc
         (lambda (config)

           (define nfs-utils
             (idmap-configuration-nfs-utils config))

           (define pipefs-directory
             (idmap-configuration-pipefs-directory config))

           (define domain (idmap-configuration-domain config))

           (define (idmap-config-file config)
             (plain-file "idmapd.conf"
                         (string-append
                          "\n[General]\n"
                          "Verbosity = "
                          (number->string
                           (idmap-configuration-verbosity config))
                          "\n"
                          (if domain
                              (format #f "Domain = ~a\n" domain)
                              "")
                          "\n[Mapping]\n"
                          "Nobody-User = nobody\n"
                          "Nobody-Group = nogroup\n")))

           (define idmap-command
             #~(list (string-append #$nfs-utils "/sbin/rpc.idmapd") "-f"
                     "-p" #$pipefs-directory
                     ;; TODO: this is deprecated
                     "-c" #$(idmap-config-file config)))

           (shepherd-service
            (documentation "Start the RPC IDMAP daemon.")
            (requirement '(rpcbind-daemon rpc-pipefs))
            (provision '(idmap-daemon))
            (start #~(make-forkexec-constructor #$idmap-command))
            (stop #~(make-kill-destructor))))))
    (service-type
     (name 'idmap)
     (extensions
      (list (service-extension shepherd-root-service-type
                               (compose list proc))))
     ;; We use the extensions feature to allow other services to automatically
     ;; configure and start this service.  Only one value can be provided.  We
     ;; override it with the value returned by the extending service.
     (compose identity)
     (extend (lambda (config values) (first values)))
     (default-value (idmap-configuration)))))

(define-record-type* <nfs-configuration>
  nfs-configuration make-nfs-configuration
  nfs-configuration?
  (nfs-utils           nfs-configuration-nfs-utils
                       (default nfs-utils))
  (nfs-versions        nfs-configuration-nfs-versions
                       (default '("4.2" "4.1" "4.0")))
  (exports             nfs-configuration-exports
                       (default '()))
  (rpcmountd-port      nfs-configuration-rpcmountd-port
                       (default #f))
  (rpcstatd-port       nfs-configuration-rpcstatd-port
                       (default #f))
  (rpcbind             nfs-configuration-rpcbind
                       (default rpcbind))
  (idmap-domain        nfs-configuration-idmap-domain
                       (default "localdomain"))
  (nfsd-port           nfs-configuration-nfsd-port
                       (default 2049))
  (nfsd-threads        nfs-configuration-nfsd-threads
                       (default 8))
  (nfsd-tcp?           nfs-configuration-nfsd-tcp?
                       (default #t))
  (nfsd-udp?           nfs-configuration-nfsd-udp?
                       (default #f))
  (pipefs-directory    nfs-configuration-pipefs-directory
                       (default default-pipefs-directory))
  ;; List of modules to debug; any of nfsd, nfs, rpc, idmap, statd, or mountd.
  (debug               nfs-configuration-debug
                       (default '())))

(define (nfs-shepherd-services config)
  "Return a list of <shepherd-service> for the NFS daemons with CONFIG."
  (match-record config <nfs-configuration>
    (nfs-utils nfs-versions exports
               rpcmountd-port rpcstatd-port nfsd-port nfsd-threads
               nfsd-tcp? nfsd-udp?
               pipefs-directory debug)
    (list (shepherd-service
           (documentation "Mount the nfsd pseudo file system.")
           (provision '(/proc/fs/nfsd))
           (start #~(lambda ()
                      (mount "nfsd" "/proc/fs/nfsd" "nfsd")
                      (member "/proc/fs/nfsd" (mount-points))))

           (stop #~(lambda (pid . args)
                     (umount "/proc/fs/nfsd" MNT_DETACH)
                     (not (member "/proc/fs/nfsd" (mount-points))))))
          (shepherd-service
           (documentation "Run the NFS statd daemon.")
           (provision '(rpc.statd))
           (requirement '(/proc/fs/nfsd rpcbind-daemon))
           (start
            #~(make-forkexec-constructor
               (list #$(file-append nfs-utils "/sbin/rpc.statd")
                     ;; TODO: notification support may require a little more
                     ;; configuration work.
                     "--no-notify"
                     #$@(if (member 'statd debug)
                            '("--no-syslog") ; verbose logging to stderr
                            '())
                     "--foreground"
                     #$@(if rpcstatd-port
                            #~("--port" #$(number->string rpcstatd-port))
                            '()))
               #:pid-file "/var/run/rpc.statd.pid"))
           (stop #~(make-kill-destructor)))
          (shepherd-service
           (documentation "Run the NFS mountd daemon.")
           (provision '(rpc.mountd))
           (requirement '(/proc/fs/nfsd rpc.statd))
           (start
            #~(make-forkexec-constructor
               (list #$(file-append nfs-utils "/sbin/rpc.mountd")
                     "--foreground"
                     #$@(if (member 'mountd debug)
                            '("--debug" "all")
                            '())
                     #$@(if rpcmountd-port
                            #~("--port" #$(number->string rpcmountd-port))
                            '()))))
           (stop #~(make-kill-destructor)))
          (shepherd-service
           (documentation "Run the NFS daemon.")
           (provision '(rpc.nfsd))
           (requirement '(/proc/fs/nfsd rpc.statd networking))
           (start
            #~(lambda _
                (zero? (apply system* #$(file-append nfs-utils "/sbin/rpc.nfsd")
                              (list
                               #$@(if (member 'nfsd debug)
                                      '("--debug")
                                      '())
                               "--port" #$(number->string nfsd-port)
                               #$@(map (lambda (version)
                                         (string-append "--nfs-version=" version))
                                       nfs-versions)
                               #$(number->string nfsd-threads)
                               #$(if nfsd-tcp?
                                     "--tcp"
                                     "--no-tcp")
                               #$(if nfsd-udp?
                                     "--udp"
                                     "--no-udp"))))))
           (stop
            #~(lambda _
                (zero?
                 (system* #$(file-append nfs-utils "/sbin/rpc.nfsd") "0")))))
          (shepherd-service
           (documentation "Run the NFS mountd daemon and refresh exports.")
           (provision '(nfs))
           (requirement '(/proc/fs/nfsd rpc.nfsd rpc.mountd rpc.statd rpcbind-daemon))
           (start
            #~(lambda _
                (let ((rpcdebug #$(file-append nfs-utils "/sbin/rpcdebug")))
                  (cond
                   ((member 'nfsd '#$debug)
                    (system* rpcdebug "-m" "nfsd" "-s" "all"))
                   ((member 'nfs '#$debug)
                    (system* rpcdebug "-m" "nfs" "-s" "all"))
                   ((member 'rpc '#$debug)
                    (system* rpcdebug "-m" "rpc" "-s" "all"))))
                (zero? (system*
                        #$(file-append nfs-utils "/sbin/exportfs")
                        "-r"            ; re-export
                        "-a"            ; everthing
                        "-v"            ; be verbose
                        "-d" "all"      ; debug
                        ))))
           (stop
            #~(lambda _
                (let ((rpcdebug #$(file-append nfs-utils "/sbin/rpcdebug")))
                  (cond
                   ((member 'nfsd '#$debug)
                    (system* rpcdebug "-m" "nfsd" "-c" "all"))
                   ((member 'nfs '#$debug)
                    (system* rpcdebug "-m" "nfs" "-c" "all"))
                   ((member 'rpc '#$debug)
                    (system* rpcdebug "-m" "rpc" "-c" "all"))))
                #t))
           (respawn? #f)))))

(define %nfs-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        ;; directory containing monitor list
        (mkdir-p "/var/lib/nfs/sm")
        ;; Needed for client recovery tracking
        (mkdir-p "/var/lib/nfs/v4recovery")
        (let ((user (getpw "nobody")))
          (chown "/var/lib/nfs"
                 (passwd:uid user)
                 (passwd:gid user))
          (chown "/var/lib/nfs/v4recovery"
                 (passwd:uid user)
                 (passwd:gid user)))
        #t)))

(define nfs-service-type
  (service-type
   (name 'nfs)
   (extensions
    (list
     (service-extension shepherd-root-service-type nfs-shepherd-services)
     (service-extension activation-service-type (const %nfs-activation))
     (service-extension etc-service-type
                        (lambda (config)
                          `(("exports"
                             ,(plain-file "exports"
                                          (string-join
                                           (map string-join
                                                (nfs-configuration-exports config))
                                           "\n"))))))
     ;; The NFS service depends on these other services.  They are extended so
     ;; that users don't need to configure them manually.
     (service-extension idmap-service-type
                        (lambda (config)
                          (idmap-configuration
                           (domain (nfs-configuration-idmap-domain config))
                           (verbosity
                            (if (member 'idmap (nfs-configuration-debug config))
                                10 0))
                           (pipefs-directory (nfs-configuration-pipefs-directory config))
                           (nfs-utils (nfs-configuration-nfs-utils config)))))
     (service-extension pipefs-service-type
                        (lambda (config)
                          (pipefs-configuration
                           (mount-point (nfs-configuration-pipefs-directory config)))))
     (service-extension rpcbind-service-type
                        (lambda (config)
                          (rpcbind-configuration
                           (rpcbind (nfs-configuration-rpcbind config)))))))
   (description
    "Run all NFS daemons and refresh the list of exported file systems.")))
