;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu tests nfs)
  #:use-module (gnu tests)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services nfs)
  #:use-module (gnu services networking)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages nfs)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:export (%test-nfs
            %test-nfs-server
            %test-nfs-root-fs))

(define %base-os
  (operating-system
    (host-name "olitupmok")
    (timezone "Europe/Berlin")
    (locale "en_US.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/sdX")))
    (file-systems %base-file-systems)
    (users %base-user-accounts)
    (packages (cons*
               rpcbind
               %base-packages))
    (services (cons*
               (service rpcbind-service-type)
               (service dhcp-client-service-type)
               %base-services))))

(define (run-nfs-test name socket)
  "Run a test of an OS running RPC-SERVICE, which should create SOCKET."
  (define os
    (marionette-operating-system
     %base-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (define (wait-for-socket file)
            ;; Wait until SOCKET  exists in the guest
            (marionette-eval
             `(let loop ((i 10))
                (cond ((and (file-exists? ,file)
                            (eq? 'socket (stat:type (stat ,file))))
                       #t)
                      ((> i 0)
                       (sleep 1)
                       (loop (- i 1)))
                      (else
                       (error "Socket didn't show up: " ,file))))
             marionette))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "rpc-daemon")

          ;; Wait for the rpcbind daemon to be up and running.
          (test-assert "RPC service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))

                ;; Ensure 'rpcinfo' can be found below.
                (setenv "PATH" "/run/current-system/profile/bin")

                (start-service 'rpcbind-daemon))
             marionette))

          ;; Check the socket file and that the service is still running.
          (test-assert "RPC socket exists"
            (and
             (wait-for-socket #$socket)
             (marionette-eval
              '(begin
                 (use-modules (gnu services herd)
                              (srfi srfi-1))

                 (live-service-running
                  (find (lambda (live)
                          (memq 'rpcbind-daemon
                                (live-service-provision live)))
                        (current-services))))
              marionette)))

          (test-assert "Probe RPC daemon"
            (marionette-eval
             '(zero? (system* "rpcinfo" "-p"))
             marionette))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation name test))

(define %test-nfs
  (system-test
   (name "nfs")
   (description "Test some things related to NFS.")
   (value (run-nfs-test name "/var/run/rpcbind.sock"))))


(define %nfs-os
  (let ((os (simple-operating-system
             (simple-service 'create-target-directory activation-service-type
                             #~(begin
                                 (mkdir "/remote")
                                 (chmod "/remote" #o777)
                                 #t))
             (service dhcp-client-service-type)
             (service nfs-service-type
                      (nfs-configuration
                       (debug '(nfs nfsd mountd))
                       (exports '(("/export"
                                   ;; crossmnt = This is the pseudo root.
                                   ;; fsid=0 = root file system of the export
                                   "*(ro,insecure,no_subtree_check,crossmnt,fsid=0)"))))))))
    (operating-system
      (inherit os)
      (host-name "nfs-server")
      ;; We need to use a tmpfs here, because the test system's root file
      ;; system cannot be re-exported via NFS.
      (file-systems (cons
                     (file-system
                       (device "none")
                       (mount-point "/export")
                       (type "tmpfs")
                       (create-mount-point? #t))
                     %base-file-systems))
      (services
       ;; Enable debugging output.
       (modify-services (operating-system-user-services os)
         (syslog-service-type config
                              =>
                              (syslog-configuration
                               (inherit config)
                               (config-file
                                (plain-file
                                 "syslog.conf"
                                 "*.* /dev/console\n")))))))))

(define (run-nfs-server-test)
  "Run a test of an OS running a service of NFS-SERVICE-TYPE."
  (define os
    (marionette-operating-system
     %nfs-os
     #:requirements '(nscd)
     #:imported-modules '((gnu services herd)
                          (guix combinators))))
  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "nfs-daemon")
          (marionette-eval
           '(begin
              (current-output-port
               (open-file "/dev/console" "w0"))
              (chmod "/export" #o777)
              (with-output-to-file "/export/hello"
                (lambda () (display "hello world")))
              (chmod "/export/hello" #o777))
           marionette)

          (test-assert "nscd PID file is created"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'nscd))
             marionette))

          (test-assert "nscd is listening on its socket"
            (wait-for-unix-socket "/var/run/nscd/socket"
                                  marionette))

          (test-assert "network is up"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'networking))
             marionette))

          ;; Wait for the NFS services to be up and running.
          (test-assert "nfs services are running"
            (and (marionette-eval
                  '(begin
                     (use-modules (gnu services herd))
                     (start-service 'nfs))
                  marionette)
                 (wait-for-file "/var/run/rpc.statd.pid" marionette)))

          (test-assert "nfs share is advertised"
            (marionette-eval
             '(zero? (system* (string-append #$nfs-utils "/sbin/showmount")
                              "-e" "nfs-server"))
             marionette))

          (test-assert "nfs share mounted"
            (marionette-eval
             '(begin
                (and (zero? (system* (string-append #$nfs-utils "/sbin/mount.nfs4")
                                     "nfs-server:/" "/remote" "-v"))
                     (file-exists? "/remote/hello")))
             marionette))
          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "nfs-server-test" test))

(define %test-nfs-server
  (system-test
   (name "nfs-server")
   (description "Test that an NFS server can be started and exported
directories can be mounted.")
   (value (run-nfs-server-test))))


(define (run-nfs-root-fs-test)
  "Run a test of an OS mounting its root file system via NFS."
  (define nfs-root-server-os
    (marionette-operating-system
     (operating-system
       (inherit %nfs-os)
       (services
         (modify-services (operating-system-user-services %nfs-os)
           (nfs-service-type config =>
            (nfs-configuration
             (debug '(nfs nfsd mountd))
             ;;; Note: Adding the following line causes Guix to hang.
             ;(rpcmountd-port 20001)
             ;;; Note: Adding the following line causes Guix to hang.
             ;(rpcstatd-port 20002) ; FIXME: Set broadcast port AND listening port.
             (nfsd-port 2049)
             (nfs-versions '("4.2"))
             (exports '(("/export"
                         "*(rw,insecure,no_subtree_check,crossmnt,fsid=root,no_root_squash,insecure,async)"))))))))
     #:requirements '(nscd)
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define nfs-root-client-os
    (marionette-operating-system
     (operating-system
       (inherit (simple-operating-system (service dhcp-client-service-type)))
       (kernel-arguments '("ip=dhcp"))
       (file-systems (cons
                      (file-system
                        (type "nfs")
                        (mount-point "/")
                        (device ":/export")
                        (options "addr=127.0.0.1,vers=4.2"))
                     %base-file-systems)))
     #:requirements '(nscd)
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "start-nfs-boot-test")

          ;;; Start up NFS server host.

          (mkdir "/tmp/server")
          (define server-marionette
            (make-marionette (list #$(virtual-machine
                                      nfs-root-server-os
                                      ;(operating-system nfs-root-server-os)
                                      ;(port-forwardings '( ; (111 . 111)
                                      ;                    (2049 . 2049)
                                      ;                    (20001 . 20001)
                                      ;                    (20002 . 20002)))
))
                             #:socket-directory "/tmp/server"))

          (marionette-eval
           '(begin
              (use-modules (gnu services herd))
              (current-output-port
               (open-file "/dev/console" "w0"))
              ;; FIXME: Instead statfs "/" and "/export" and wait until they
              ;; are different file systems.  But Guile doesn't seem to have
              ;; statfs.
              (sleep 5)
              (chmod "/export" #o777)
              (symlink "/gnu" "/export/gnu")
              (start-service 'nscd)
              (start-service 'networking)
              (start-service 'nfs))
           server-marionette)

          ;;; Wait for the NFS services to be up and running.

          (test-assert "nfs services are running"
           (wait-for-file "/var/run/rpc.statd.pid" server-marionette))

          (test-assert "NFS port is ready"
            (wait-for-tcp-port 2049 server-marionette))

          (test-assert "NFS statd port is ready"
            (wait-for-tcp-port 20002 server-marionette))

          (test-assert "NFS mountd port is ready"
            (wait-for-tcp-port 20001 server-marionette))

          ;;; FIXME: (test-assert "NFS portmapper port is ready"
          ;;; FIXME:  (wait-for-tcp-port 111 server-marionette))

          ;;; Start up NFS client host.

          (define client-marionette
            (make-marionette (list #$(virtual-machine
                                      nfs-root-client-os
                                      ;(port-forwardings '((111 . 111)
                                      ;                    (2049 . 2049)
                                      ;                    (20001 . 20001)
                                      ;                    (20002 . 20002)))
                                                          ))))

          (marionette-eval
           '(begin
              (use-modules (gnu services herd))
              (use-modules (rnrs io ports))

              (current-output-port
               (open-file "/dev/console" "w0"))
              (let ((content (call-with-input-file "/proc/mounts" get-string-all)))
                (call-with-output-file "/mounts.new"
                  (lambda (port)
                    (display content port))))
              (chmod "/mounts.new" #o777)
              (rename-file "/mounts.new" "/mounts"))
           client-marionette)

          (test-assert "nfs-root-client booted")

          ;;; Check whether NFS client host communicated with NFS server host.

          (test-assert "nfs client deposited file"
           (wait-for-file "/export/mounts" server-marionette))
          (marionette-eval
           '(begin
              (current-output-port
               (open-file "/dev/console" "w0"))
              (call-with-input-file "/export/mounts" display))
           server-marionette)

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "nfs-root-fs-test" test))

(define %test-nfs-root-fs
  (system-test
   (name "nfs-root-fs")
   (description "Test that an NFS server can be started and the exported
directory can be used as root file system.")
   (value (run-nfs-root-fs-test))))
