;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

(define-module (test-syscalls)
  #:use-module (guix utils)
  #:use-module (guix build syscalls)
  #:use-module (gnu build linux-container)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

;; Test the (guix build syscalls) module, although there's not much that can
;; actually be tested without being root.

(test-begin "syscalls")

(test-equal "mount, ENOENT"
  ENOENT
  (catch 'system-error
    (lambda ()
      (mount "/dev/null" "/does-not-exist" "ext2")
      #f)
    (compose system-error-errno list)))

(test-assert "umount, ENOENT/EPERM"
  (catch 'system-error
    (lambda ()
      (umount "/does-not-exist")
      #f)
    (lambda args
      ;; Both return values have been encountered in the wild.
      (memv (system-error-errno args) (list EPERM ENOENT)))))

(test-assert "mount-points"
  ;; Reportedly "/" is not always listed as a mount point, so check a few
  ;; others (see <http://bugs.gnu.org/20261>.)
  (any (cute member <> (mount-points))
       '("/" "/proc" "/sys" "/dev")))

(test-assert "swapon, ENOENT/EPERM"
  (catch 'system-error
    (lambda ()
      (swapon "/does-not-exist")
      #f)
    (lambda args
      (memv (system-error-errno args) (list EPERM ENOENT)))))

(test-assert "swapoff, ENOENT/EINVAL/EPERM"
  (catch 'system-error
    (lambda ()
      (swapoff "/does-not-exist")
      #f)
    (lambda args
      (memv (system-error-errno args) (list EPERM EINVAL ENOENT)))))

(test-assert "mkdtemp!"
  (let* ((tmp (or (getenv "TMPDIR") "/tmp"))
         (dir (mkdtemp! (string-append tmp "/guix-test-XXXXXX"))))
    (and (file-exists? dir)
         (begin
           (rmdir dir)
           #t))))

(define (user-namespace pid)
  (string-append "/proc/" (number->string pid) "/ns/user"))

(define perform-container-tests?
  (and (user-namespace-supported?)
       (unprivileged-user-namespace-supported?)))

(unless perform-container-tests?
  (test-skip 1))
(test-assert "clone"
  (match (clone (logior CLONE_NEWUSER SIGCHLD))
    (0 (primitive-exit 42))
    (pid
     ;; Check if user namespaces are different.
     (and (not (equal? (readlink (user-namespace pid))
                       (readlink (user-namespace (getpid)))))
          (match (waitpid pid)
            ((_ . status)
             (= 42 (status:exit-val status))))))))

(unless perform-container-tests?
  (test-skip 1))
(test-assert "setns"
  (match (clone (logior CLONE_NEWUSER SIGCHLD))
    (0 (primitive-exit 0))
    (clone-pid
     (match (pipe)
       ((in . out)
        (match (primitive-fork)
          (0
           (close in)
           ;; Join the user namespace.
           (call-with-input-file (user-namespace clone-pid)
             (lambda (port)
               (setns (port->fdes port) 0)))
           (write 'done out)
           (close out)
           (primitive-exit 0))
          (fork-pid
           (close out)
           ;; Wait for the child process to join the namespace.
           (read in)
           (let ((result (and (equal? (readlink (user-namespace clone-pid))
                                      (readlink (user-namespace fork-pid))))))
             ;; Clean up.
             (waitpid clone-pid)
             (waitpid fork-pid)
             result))))))))

(unless perform-container-tests?
  (test-skip 1))
(test-assert "pivot-root"
  (match (pipe)
    ((in . out)
     (match (clone (logior CLONE_NEWUSER CLONE_NEWNS SIGCHLD))
       (0
        (close in)
        (call-with-temporary-directory
         (lambda (root)
           (let ((put-old (string-append root "/real-root")))
             (mount "none" root "tmpfs")
             (mkdir put-old)
             (call-with-output-file (string-append root "/test")
               (lambda (port)
                 (display "testing\n" port)))
             (pivot-root root put-old)
             ;; The test file should now be located inside the root directory.
             (write (file-exists? "/test") out)
             (close out))))
        (primitive-exit 0))
       (pid
        (close out)
        (let ((result (read in)))
          (close in)
          (and (zero? (match (waitpid pid)
                        ((_ . status)
                         (status:exit-val status))))
               (eq? #t result))))))))

(test-assert "all-network-interface-names"
  (match (all-network-interface-names)
    (((? string? names) ..1)
     (member "lo" names))))

(test-assert "network-interface-names"
  (match (network-interface-names)
    (((? string? names) ..1)
     (lset<= string=? names (all-network-interface-names)))))

(test-assert "network-interface-flags"
  (let* ((sock  (socket AF_INET SOCK_STREAM 0))
         (flags (network-interface-flags sock "lo")))
    (close-port sock)
    (and (not (zero? (logand flags IFF_LOOPBACK)))
         (not (zero? (logand flags IFF_UP))))))

(test-equal "loopback-network-interface?"
  ENODEV
  (and (loopback-network-interface? "lo")
       (catch 'system-error
         (lambda ()
           (loopback-network-interface? "nonexistent")
           #f)
         (lambda args
           (system-error-errno args)))))

(test-skip (if (zero? (getuid)) 1 0))
(test-assert "set-network-interface-flags"
  (let ((sock (socket AF_INET SOCK_STREAM 0)))
    (catch 'system-error
      (lambda ()
        (set-network-interface-flags sock "lo" IFF_UP))
      (lambda args
        (close-port sock)
        ;; We get EPERM with Linux 3.18ish and EACCES with 2.6.32.
        (memv (system-error-errno args) (list EPERM EACCES))))))

(test-equal "network-interface-address lo"
  (make-socket-address AF_INET (inet-pton AF_INET "127.0.0.1") 0)
  (let* ((sock (socket AF_INET SOCK_STREAM 0))
         (addr (network-interface-address sock "lo")))
    (close-port sock)
    addr))

(test-skip (if (zero? (getuid)) 1 0))
(test-assert "set-network-interface-address"
  (let ((sock (socket AF_INET SOCK_STREAM 0)))
    (catch 'system-error
      (lambda ()
        (set-network-interface-address sock "nonexistent"
                                       (make-socket-address
                                        AF_INET
                                        (inet-pton AF_INET "127.12.14.15")
                                        0)))
      (lambda args
        (close-port sock)
        ;; We get EPERM with Linux 3.18ish and EACCES with 2.6.32.
        (memv (system-error-errno args) (list EPERM EACCES))))))

(test-equal "network-interfaces returns one or more interfaces"
  '(#t #t #t)
  (match (network-interfaces)
    ((interfaces ..1)
     (list (every interface? interfaces)
           (every string? (map interface-name interfaces))
           (every (lambda (sockaddr)
                    ;; Sometimes interfaces have no associated address.
                    (or (vector? sockaddr)
                        (not sockaddr)))
                  (map interface-address interfaces))))))

(test-equal "network-interfaces returns \"lo\""
  (list #t (make-socket-address AF_INET (inet-pton AF_INET "127.0.0.1") 0))
  (match (filter (lambda (interface)
                   (string=? "lo" (interface-name interface)))
                 (network-interfaces))
    ((loopbacks ..1)
     (list (every (lambda (lo)
                    (not (zero? (logand IFF_LOOPBACK (interface-flags lo)))))
                  loopbacks)
           (match (find (lambda (lo)
                          (= AF_INET (sockaddr:fam (interface-address lo))))
                        loopbacks)
             (#f #f)
             (lo (interface-address lo)))))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))
