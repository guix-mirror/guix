;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
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

(define-module (gnu tests ganeti)
  #:use-module (gnu)
  #:use-module (gnu tests)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services ganeti)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages virtualization)
  #:use-module (guix gexp)
  #:use-module (ice-9 format)
  #:export (%test-ganeti-kvm %test-ganeti-lxc))

(define %ganeti-os
  (operating-system
    (host-name "gnt1")
    (timezone "Etc/UTC")
    (locale "en_US.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets '("/dev/vda"))))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (firmware '())

    ;; The hosts file must contain a nonlocal IP for host-name.
    ;; In addition, the cluster name must resolve to an IP address that
    ;; is not currently provisioned.
    (hosts-file (plain-file "hosts" (format #f "
127.0.0.1       localhost
::1             localhost
10.0.2.15       gnt1.example.com gnt1
192.168.254.254 ganeti.example.com
")))

    (packages (append (list ganeti-instance-debootstrap ganeti-instance-guix)
                      %base-packages))
    (services
     (append (list (service static-networking-service-type
                            (list %qemu-static-networking))
                   (service openssh-service-type
                            (openssh-configuration
                             (permit-root-login 'prohibit-password)))

                   (service ganeti-service-type
                            (ganeti-configuration
                             (file-storage-paths '("/srv/ganeti/file-storage"))
                             (rapi-configuration
                              (ganeti-rapi-configuration
                               ;; Disable TLS so we can test the RAPI without
                               ;; pulling in GnuTLS.
                               (ssl? #f)))
                             (os %default-ganeti-os))))
             %base-services))))

(define* (run-ganeti-test hypervisor #:key
                          (master-netdev "eth0")
                          (hvparams '())
                          (extra-packages '())
                          (rapi-port 5080))
  "Run tests in %GANETI-OS."
  (define os
    (marionette-operating-system
     (operating-system
       (inherit %ganeti-os)
       (packages (append extra-packages
                         (operating-system-packages %ganeti-os))))
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define %forwarded-rapi-port 5080)

  (define vm
    (virtual-machine
     (operating-system os)
     ;; Some of the daemons are fairly memory-hungry.
     (memory-size 512)
     ;; Forward HTTP ports so we can access them from the "outside".
     (port-forwardings `((,%forwarded-rapi-port . ,rapi-port)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (web uri) (web client) (web response)
                       (ice-9 iconv)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "ganeti")

          ;; Ganeti uses the Shepherd to start/stop daemons, so make sure
          ;; it is ready before we begin.  It takes a while because all
          ;; Ganeti daemons fail to start initially.
          (test-assert "shepherd is ready"
            (wait-for-unix-socket "/var/run/shepherd/socket" marionette))

          (test-eq "gnt-cluster init"
            0
            (marionette-eval
             '(begin
                (setenv
                 "PATH"
                 ;; Init needs to run 'ssh-keygen', 'ip', etc.
                 "/run/current-system/profile/sbin:/run/current-system/profile/bin")
                (system* #$(file-append ganeti "/sbin/gnt-cluster") "init"
                         (string-append "--master-netdev=" #$master-netdev)
                         ;; TODO: Enable more disk backends.
                         "--enabled-disk-templates=file"
                         (string-append "--enabled-hypervisors="
                                        #$hypervisor)
                         (string-append "--hypervisor-parameters="
                                        #$hypervisor ":"
                                        (string-join '#$hvparams "\n"))
                         ;; Set the default NIC mode to 'routed' to avoid having to
                         ;; configure a full bridge to placate 'gnt-cluster verify'.
                         "--nic-parameters=mode=routed,link=eth0"
                         "ganeti.example.com"))
             marionette))

          ;; Disable the watcher while doing daemon tests to prevent interference.
          (test-eq "watcher pause"
            0
            (marionette-eval
             '(begin
                (system* #$(file-append ganeti "/sbin/gnt-cluster")
                         "watcher" "pause" "1h"))
             marionette))

          (test-assert "force-start wconfd"
            ;; Check that the 'force-start' Shepherd action works, used in a
            ;; master-failover scenario.
            (marionette-eval
             '(begin
                (setenv "PATH" "/run/current-system/profile/bin")
                (invoke "herd" "stop" "ganeti-wconfd")
                (invoke "herd" "disable" "ganeti-wconfd")
                (invoke "herd" "force-start" "ganeti-wconfd"))
             marionette))

          ;; Verify that the cluster is healthy.
          (test-eq "gnt-cluster verify 1"
            0
            (marionette-eval
             '(begin
                (system* #$(file-append ganeti "/sbin/gnt-cluster") "verify"))
             marionette))

          ;; Try stopping and starting daemons with daemon-util like
          ;; 'gnt-node add', 'gnt-cluster init', etc.
          (test-eq "daemon-util stop-all"
            0
            (marionette-eval
             '(begin
                (system* #$(file-append ganeti "/lib/ganeti/daemon-util")
                         "stop-all"))
             marionette))

          (test-eq "daemon-util start-all"
            0
            (marionette-eval
             '(begin
                (system* #$(file-append ganeti "/lib/ganeti/daemon-util")
                         "start-all"))
             marionette))

          ;; Check that the cluster is still healthy after the daemon restarts.
          (test-eq "gnt-cluster verify 2"
            0
            (marionette-eval
             '(begin
                (system* #$(file-append ganeti "/sbin/gnt-cluster") "verify"))
             marionette))

          (test-eq "watcher continue"
            0
            (marionette-eval
             '(begin
                (system* #$(file-append ganeti "/sbin/gnt-cluster")
                         "watcher" "continue"))
             marionette))

          ;; Try accessing the RAPI.
          (test-equal "http-get RAPI version"
            '(200 "2\n")
            (let-values
                (((response text)
                  (http-get #$(simple-format
                               #f "http://localhost:~A/version"
                               %forwarded-rapi-port)
                            #:decode-body? #f)))
              (list (response-code response)
                    ;; The API response lacks a content-type, so
                    ;; (http-client) won't decode it for us.
                    (bytevector->string text "UTF-8"))))

          (test-equal "gnt-os list"
            "debootstrap+default\nguix+default\n"
            (marionette-eval
             '(begin
                (use-modules (ice-9 popen))
                (let* ((port (open-pipe*
                              OPEN_READ
                              #$(file-append ganeti "/sbin/gnt-os")
                              "list" "--no-headers"))
                       (output (get-string-all port)))
                  (close-pipe port)
                  output))
             marionette))

          (test-eq "gnt-cluster destroy"
            0
            (marionette-eval
             '(begin
                (system* #$(file-append ganeti "/sbin/gnt-cluster")
                         "destroy" "--yes-do-it"))
             marionette))

          (test-end))))

  (gexp->derivation (string-append "ganeti-" hypervisor "-test") test))

(define %test-ganeti-kvm
  (system-test
   (name "ganeti-kvm")
   (description "Provision a Ganeti cluster using the KVM hypervisor.")
   (value (run-ganeti-test "kvm"
                           ;; Set kernel_path to an empty string to prevent
                           ;; 'gnt-cluster verify' from testing for its presence.
                           #:hvparams '("kernel_path=")
                           #:extra-packages (list qemu)))))

(define %test-ganeti-lxc
  (system-test
   (name "ganeti-lxc")
   (description "Provision a Ganeti cluster using LXC as the hypervisor.")
   (value (run-ganeti-test "lxc"
                           #:extra-packages (list lxc)))))
