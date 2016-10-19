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

(define temp-file
  (string-append "t-utils-" (number->string (getpid))))


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

(test-equal "statfs, ENOENT"
  ENOENT
  (catch 'system-error
    (lambda ()
      (statfs "/does-not-exist"))
    (compose system-error-errno list)))

(test-assert "statfs"
  (let ((fs (statfs "/")))
    (and (file-system? fs)
         (> (file-system-block-size fs) 0)
         (>= (file-system-blocks-available fs) 0)
         (>= (file-system-blocks-free fs)
             (file-system-blocks-available fs)))))

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
(test-equal "pivot-root"
  #t
  (match (pipe)
    ((in . out)
     (match (clone (logior CLONE_NEWUSER CLONE_NEWNS SIGCHLD))
       (0
        (dynamic-wind
          (const #t)
          (lambda ()
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
                 (close out)))))
          (lambda ()
            (primitive-exit 0))))
       (pid
        (close out)
        (let ((result (read in)))
          (close in)
          (and (zero? (match (waitpid pid)
                        ((_ . status)
                         (status:exit-val status))))
               (eq? #t result))))))))

(false-if-exception (delete-file temp-file))
(test-equal "fcntl-flock wait"
  42                                              ; the child's exit status
  (let ((file (open-file temp-file "w0b")))
    ;; Acquire an exclusive lock.
    (fcntl-flock file 'write-lock)
    (match (primitive-fork)
      (0
       (dynamic-wind
         (const #t)
         (lambda ()
           ;; Reopen FILE read-only so we can have a read lock.
           (let ((file (open-file temp-file "r0b")))
             ;; Wait until we can acquire the lock.
             (fcntl-flock file 'read-lock)
             (primitive-exit (read file)))
           (primitive-exit 1))
         (lambda ()
           (primitive-exit 2))))
      (pid
       ;; Write garbage and wait.
       (display "hello, world!"  file)
       (force-output file)
       (sleep 1)

       ;; Write the real answer.
       (seek file 0 SEEK_SET)
       (truncate-file file 0)
       (write 42 file)
       (force-output file)

       ;; Unlock, which should let the child continue.
       (fcntl-flock file 'unlock)

       (match (waitpid pid)
         ((_  . status)
          (let ((result (status:exit-val status)))
            (close-port file)
            result)))))))

(test-equal "fcntl-flock non-blocking"
  EAGAIN                                          ; the child's exit status
  (match (pipe)
    ((input . output)
     (match (primitive-fork)
       (0
        (dynamic-wind
          (const #t)
          (lambda ()
            (close-port output)

            ;; Wait for the green light.
            (read-char input)

            ;; Open FILE read-only so we can have a read lock.
            (let ((file (open-file temp-file "w0")))
              (catch 'flock-error
                (lambda ()
                  ;; This attempt should throw EAGAIN.
                  (fcntl-flock file 'write-lock #:wait? #f))
                (lambda (key errno)
                  (primitive-exit (pk 'errno errno)))))
            (primitive-exit -1))
          (lambda ()
            (primitive-exit -2))))
       (pid
        (close-port input)
        (let ((file (open-file temp-file "w0")))
          ;; Acquire an exclusive lock.
          (fcntl-flock file 'write-lock)

          ;; Tell the child to continue.
          (write 'green-light output)
          (force-output output)

          (match (waitpid pid)
            ((_  . status)
             (let ((result (status:exit-val status)))
               (fcntl-flock file 'unlock)
               (close-port file)
               result)))))))))

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

(test-equal "tcgetattr ENOTTY"
  ENOTTY
  (catch 'system-error
    (lambda ()
      (call-with-input-file "/dev/null"
        (lambda (port)
          (tcgetattr (fileno port)))))
    (compose system-error-errno list)))

(test-skip (if (and (file-exists? "/proc/self/fd/0")
                    (string-prefix? "/dev/pts/" (readlink "/proc/self/fd/0")))
               0
               2))

(test-assert "tcgetattr"
  (let ((termios (tcgetattr 0)))
    (and (termios? termios)
         (> (termios-input-speed termios) 0)
         (> (termios-output-speed termios) 0))))

(test-assert "tcsetattr"
  (let ((first (tcgetattr 0)))
    (tcsetattr 0 (tcsetattr-action TCSANOW) first)
    (equal? first (tcgetattr 0))))

(test-assert "terminal-window-size ENOTTY"
  (call-with-input-file "/dev/null"
    (lambda (port)
      (catch 'system-error
        (lambda ()
          (terminal-window-size port))
        (lambda args
          ;; Accept EINVAL, which some old Linux versions might return.
          (memv (system-error-errno args)
                (list ENOTTY EINVAL)))))))

(test-assert "terminal-columns"
  (> (terminal-columns) 0))

(test-assert "terminal-columns non-file port"
  (> (terminal-columns (open-input-string "Join us now, share the software!"))
     0))

(test-end)

(false-if-exception (delete-file temp-file))
