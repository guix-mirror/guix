;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu tests networking)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu services shepherd)
  #:use-module (ice-9 match)
  #:export (%test-inetd %test-openvswitch %test-dhcpd %test-tor %test-iptables))

(define %inetd-os
  ;; Operating system with 2 inetd services.
  (simple-operating-system
   (service dhcp-client-service-type)
   (service inetd-service-type
            (inetd-configuration
             (entries (list
                       (inetd-entry
                        (name "echo")
                        (socket-type 'stream)
                        (protocol "tcp")
                        (wait? #f)
                        (user "root"))
                       (inetd-entry
                        (name "dict")
                        (socket-type 'stream)
                        (protocol "tcp")
                        (wait? #f)
                        (user "root")
                        (program (file-append bash
                                              "/bin/bash"))
                        (arguments
                         (list "bash" (plain-file "my-dict.sh" "\
while read line
do
    if [[ $line =~ ^DEFINE\\ (.*)$ ]]
    then
        case ${BASH_REMATCH[1]} in
            Guix)
                echo GNU Guix is a package management tool for the GNU system.
                ;;
            G-expression)
                echo Like an S-expression but with a G.
                ;;
            *)
                echo NO DEFINITION FOUND
                ;;
        esac
    else
        echo ERROR
    fi
done" ))))))))))

(define* (run-inetd-test)
  "Run tests in %INETD-OS, where the inetd service provides an echo service on
port 7, and a dict service on port 2628."
  (define os
    (marionette-operating-system %inetd-os))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((8007 . 7)
                         (8628 . 2628)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (ice-9 rdelim)
                       (srfi srfi-64)
                       (gnu build marionette))
          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "inetd")

          ;; Make sure the PID file is created.
          (test-assert "PID file"
            (marionette-eval
             '(file-exists? "/var/run/inetd.pid")
             marionette))

          ;; Test the echo service.
          (test-equal "echo response"
            "Hello, Guix!"
            (let ((echo (socket PF_INET SOCK_STREAM 0))
                  (addr (make-socket-address AF_INET INADDR_LOOPBACK 8007)))
              (connect echo addr)
              (display "Hello, Guix!\n" echo)
              (let ((response (read-line echo)))
                (close echo)
                response)))

          ;; Test the dict service
          (test-equal "dict response"
            "GNU Guix is a package management tool for the GNU system."
            (let ((dict (socket PF_INET SOCK_STREAM 0))
                  (addr (make-socket-address AF_INET INADDR_LOOPBACK 8628)))
              (connect dict addr)
              (display "DEFINE Guix\n" dict)
              (let ((response (read-line dict)))
                (close dict)
                response)))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "inetd-test" test))

(define %test-inetd
  (system-test
   (name "inetd")
   (description "Connect to a host with an INETD server.")
   (value (run-inetd-test))))


;;;
;;; Open vSwitch
;;;

(define setup-openvswitch
  #~(let ((ovs-vsctl (lambda (str)
                       (zero? (apply system*
                                     #$(file-append openvswitch "/bin/ovs-vsctl")
                                     (string-tokenize str)))))
          (add-native-port (lambda (if)
                             (string-append "--may-exist add-port br0 " if
                                            " vlan_mode=native-untagged"
                                            " -- set Interface " if
                                            " type=internal"))))
      (and (ovs-vsctl "--may-exist add-br br0")
           ;; Connect eth0 as an "untagged" port (no VLANs).
           (ovs-vsctl "--may-exist add-port br0 eth0 vlan_mode=native-untagged")
           (ovs-vsctl (add-native-port "ovs0")))))

(define openvswitch-configuration-service
  (simple-service 'openvswitch-configuration shepherd-root-service-type
                  (list (shepherd-service
                         (provision '(openvswitch-configuration))
                         (requirement '(vswitchd))
                         (start #~(lambda ()
                                    #$setup-openvswitch))
                         (respawn? #f)))))

(define %openvswitch-os
  (simple-operating-system
   (static-networking-service "ovs0" "10.1.1.1"
                              #:netmask "255.255.255.252"
                              #:requirement '(openvswitch-configuration))
   (service openvswitch-service-type)
   openvswitch-configuration-service))

(define (run-openvswitch-test)
  (define os
    (marionette-operating-system %openvswitch-os
                                 #:imported-modules '((gnu services herd))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (ice-9 popen)
                       (ice-9 rdelim)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "openvswitch")

          ;; Make sure the bridge is created.
          (test-assert "br0 exists"
            (marionette-eval
             '(zero? (system* "ovs-vsctl" "br-exists" "br0"))
             marionette))

          ;; Make sure eth0 is connected to the bridge.
          (test-equal "eth0 is connected to br0"
            "br0"
            (marionette-eval
             '(begin
                (use-modules (ice-9 popen) (ice-9 rdelim))
                (let* ((port (open-pipe*
                              OPEN_READ
                              (string-append #$openvswitch "/bin/ovs-vsctl")
                              "port-to-br" "eth0"))
                       (output (read-line port)))
                  (close-pipe port)
                  output))
             marionette))

          ;; Make sure the virtual interface got a static IP.
          (test-assert "networking has started on ovs0"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd)
                             (srfi srfi-1))
                (live-service-running
                 (find (lambda (live)
                         (memq 'networking-ovs0
                               (live-service-provision live)))
                       (current-services))))
             marionette))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "openvswitch-test" test))

(define %test-openvswitch
  (system-test
   (name "openvswitch")
   (description "Test a running OpenvSwitch configuration.")
   (value (run-openvswitch-test))))


;;;
;;; DHCP Daemon
;;;

(define minimal-dhcpd-v4-config-file
  (plain-file "dhcpd.conf"
              "\
default-lease-time 600;
max-lease-time 7200;

subnet 192.168.1.0 netmask 255.255.255.0 {
 range 192.168.1.100 192.168.1.200;
 option routers 192.168.1.1;
 option domain-name-servers 192.168.1.2, 192.168.1.3;
 option domain-name \"dummy.domain.name.abc123xyz\";
}
"))

(define dhcpd-v4-configuration
  (dhcpd-configuration
   (config-file minimal-dhcpd-v4-config-file)
   (version "4")
   (interfaces '("eth0"))))

(define %dhcpd-os
  (simple-operating-system
   (static-networking-service "eth0" "192.168.1.4"
                              #:netmask "255.255.255.0"
                              #:gateway "192.168.1.1"
                              #:name-servers '("192.168.1.2" "192.168.1.3"))
   (service dhcpd-service-type dhcpd-v4-configuration)))

(define (run-dhcpd-test)
  (define os
    (marionette-operating-system %dhcpd-os
                                 #:imported-modules '((gnu services herd))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (ice-9 popen)
                       (ice-9 rdelim)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "dhcpd")

          (test-assert "pid file exists"
            (marionette-eval
             '(file-exists?
               #$(dhcpd-configuration-pid-file dhcpd-v4-configuration))
             marionette))

          (test-assert "lease file exists"
            (marionette-eval
             '(file-exists?
               #$(dhcpd-configuration-lease-file dhcpd-v4-configuration))
             marionette))

          (test-assert "run directory exists"
            (marionette-eval
             '(file-exists?
               #$(dhcpd-configuration-run-directory dhcpd-v4-configuration))
             marionette))

          (test-assert "dhcpd is alive"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd)
                             (srfi srfi-1))
                (live-service-running
                 (find (lambda (live)
                         (memq 'dhcpv4-daemon
                               (live-service-provision live)))
                       (current-services))))
             marionette))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "dhcpd-test" test))

(define %test-dhcpd
  (system-test
   (name "dhcpd")
   (description "Test a running DHCP daemon configuration.")
   (value (run-dhcpd-test))))


;;;
;;; Services related to Tor
;;;

(define %tor-os
  (simple-operating-system
   (service tor-service-type)))

(define %tor-os/unix-socks-socket
  (simple-operating-system
   (service tor-service-type
            (tor-configuration
             (socks-socket-type 'unix)))))

(define (run-tor-test)
  (define os
    (marionette-operating-system %tor-os
                                 #:imported-modules '((gnu services herd))
                                 #:requirements '(tor)))

  (define os/unix-socks-socket
    (marionette-operating-system %tor-os/unix-socks-socket
                                 #:imported-modules '((gnu services herd))
                                 #:requirements '(tor)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (ice-9 popen)
                       (ice-9 rdelim)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (define (tor-is-alive? marionette)
            (marionette-eval
             '(begin
                (use-modules (gnu services herd)
                             (srfi srfi-1))
                (live-service-running
                 (find (lambda (live)
                         (memq 'tor
                               (live-service-provision live)))
                       (current-services))))
             marionette))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "tor")

          ;; Test the usual Tor service.

          (test-assert "tor is alive"
            (tor-is-alive? marionette))

          (test-assert "tor is listening"
            (let ((default-port 9050))
              (wait-for-tcp-port default-port marionette)))

          ;; Don't run two VMs at once.
          (marionette-control "quit" marionette)

          ;; Test the Tor service using a SOCKS socket.

          (let* ((socket-directory "/tmp/more-sockets")
                 (_ (mkdir socket-directory))
                 (marionette/unix-socks-socket
                  (make-marionette
                   (list #$(virtual-machine os/unix-socks-socket))
                   ;; We can't use the same socket directory as the first
                   ;; marionette.
                   #:socket-directory socket-directory)))
            (test-assert "tor is alive, even when using a SOCKS socket"
              (tor-is-alive? marionette/unix-socks-socket))

            (test-assert "tor is listening, even when using a SOCKS socket"
              (wait-for-unix-socket "/var/run/tor/socks-sock"
                                    marionette/unix-socks-socket)))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "tor-test" test))

(define %test-tor
  (system-test
   (name "tor")
   (description "Test a running Tor daemon configuration.")
   (value (run-tor-test))))

(define* (run-iptables-test)
  "Run tests of 'iptables-service-type'."
  (define iptables-rules
    "*filter
:INPUT ACCEPT
:FORWARD ACCEPT
:OUTPUT ACCEPT
-A INPUT -p tcp -m tcp --dport 7 -j REJECT --reject-with icmp-port-unreachable
COMMIT
")

  (define ip6tables-rules
    "*filter
:INPUT ACCEPT
:FORWARD ACCEPT
:OUTPUT ACCEPT
-A INPUT -p tcp -m tcp --dport 7 -j REJECT --reject-with icmp6-port-unreachable
COMMIT
")

  (define inetd-echo-port 7)

  (define os
    (marionette-operating-system
     (simple-operating-system
      (service dhcp-client-service-type)
      (service inetd-service-type
               (inetd-configuration
                (entries (list
                          (inetd-entry
                           (name "echo")
                           (socket-type 'stream)
                           (protocol "tcp")
                           (wait? #f)
                           (user "root"))))))
      (service iptables-service-type
               (iptables-configuration
                (ipv4-rules (plain-file "iptables.rules" iptables-rules))
                (ipv6-rules (plain-file "ip6tables.rules" ip6tables-rules)))))
     #:imported-modules '((gnu services herd))
     #:requirements '(inetd iptables)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))
          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (define (dump-iptables iptables-save marionette)
            (marionette-eval
             `(begin
                (use-modules (ice-9 popen)
                             (ice-9 rdelim)
                             (ice-9 regex))
                (call-with-output-string
                  (lambda (out)
                    (call-with-port
                     (open-pipe* OPEN_READ ,iptables-save)
                     (lambda (in)
                       (let loop ((line (read-line in)))
                         ;; iptables-save does not output rules in the exact
                         ;; same format we loaded using iptables-restore. It
                         ;; adds comments, packet counters, etc. We remove
                         ;; these additions.
                         (unless (eof-object? line)
                           (cond
                            ;; Remove comments
                            ((string-match "^#" line) #t)
                            ;; Remove packet counters
                            ((string-match "^:([A-Z]*) ([A-Z]*) .*" line)
                             => (lambda (match-record)
                                  (format out ":~a ~a~%"
                                          (match:substring match-record 1)
                                          (match:substring match-record 2))))
                            ;; Pass other lines without modification
                            (else (display line out)
                                  (newline out)))
                           (loop (read-line in)))))))))
             marionette))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "iptables")

          (test-equal "iptables-save dumps the same rules that were loaded"
            (dump-iptables #$(file-append iptables "/sbin/iptables-save")
                           marionette)
            #$iptables-rules)

          (test-equal "ip6tables-save dumps the same rules that were loaded"
            (dump-iptables #$(file-append iptables "/sbin/ip6tables-save")
                           marionette)
            #$ip6tables-rules)

          (test-error "iptables firewall blocks access to inetd echo service"
                      'misc-error
                      (wait-for-tcp-port inetd-echo-port marionette #:timeout 5))

          ;; TODO: This test freezes up at the login prompt without any
          ;; relevant messages on the console. Perhaps it is waiting for some
          ;; timeout. Find and fix this issue.
          ;; (test-assert "inetd echo service is accessible after iptables firewall is stopped"
          ;;   (begin
          ;;     (marionette-eval
          ;;      '(begin
          ;;         (use-modules (gnu services herd))
          ;;         (stop-service 'iptables))
          ;;      marionette)
          ;;     (wait-for-tcp-port inetd-echo-port marionette #:timeout 5)))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "iptables" test))

(define %test-iptables
  (system-test
   (name "iptables")
   (description "Test a running iptables daemon.")
   (value (run-iptables-test))))
