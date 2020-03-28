;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu installer final)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer user)
  #:use-module (gnu services herd)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (gnu build accounts)
  #:use-module ((gnu system shadow) #:prefix sys:)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:export (install-system))

(define %seed
  (seed->random-state
   (logxor (getpid) (car (gettimeofday)))))

(define (integer->alphanumeric-char n)
  "Map N, an integer in the [0..62] range, to an alphanumeric character."
  (cond ((< n 10)
         (integer->char (+ (char->integer #\0) n)))
        ((< n 36)
         (integer->char (+ (char->integer #\A) (- n 10))))
        ((< n 62)
         (integer->char (+ (char->integer #\a) (- n 36))))
        (else
         (error "integer out of bounds" n))))

(define (random-string len)
  "Compute a random string of size LEN where each character is alphanumeric."
  (let loop ((chars '())
             (len len))
    (if (zero? len)
        (list->string chars)
        (let ((n (random 62 %seed)))
          (loop (cons (integer->alphanumeric-char n) chars)
                (- len 1))))))

(define (create-user-database users root)
  "Create /etc/passwd, /etc/shadow, and /etc/group under ROOT for the given
USERS."
  (define etc
    (string-append root "/etc"))

  (define (salt)
    ;; "$6" gives us a SHA512 password hash; the random string must be taken
    ;; from the './0-9A-Za-z' alphabet (info "(libc) Passphrase Storage").
    (string-append "$6$" (random-string 10)))

  (define users*
    (map (lambda (user)
           (define root?
             (string=? "root" (user-name user)))

           (sys:user-account (name (user-name user))
                             (comment (user-real-name user))
                             (group "users")
                             (uid (if root? 0 #f))
                             (home-directory
                              (user-home-directory user))
                             (password (crypt (user-password user)
                                              (salt)))

                             ;; We need a string here, not a file-like, hence
                             ;; this choice.
                             (shell
                              "/run/current-system/profile/bin/bash")))
         users))

  (define-values (group password shadow)
    (user+group-databases users* sys:%base-groups
                          #:current-passwd '()
                          #:current-groups '()
                          #:current-shadow '()))

  (mkdir-p etc)
  (write-group group (string-append etc "/group"))
  (write-passwd password (string-append etc "/passwd"))
  (write-shadow shadow (string-append etc "/shadow")))

(define* (kill-cow-users cow-path #:key (spare '("udevd")))
  "Kill all processes that have references to the given COW-PATH in their
'maps' file.  The process whose names are in SPARE list are spared."
  (define %not-nul
    (char-set-complement (char-set #\nul)))

  (let ((pids
         (filter-map (lambda (pid)
                       (false-if-exception
                        (call-with-input-file
                            (string-append "/proc/" pid "/maps")
                          (lambda (port)
                            (and (string-contains (get-string-all port)
                                                  cow-path)
                                 (string->number pid))))))
                     (scandir "/proc" string->number))))
    (for-each (lambda (pid)
                ;; cmdline does not always exist.
                (false-if-exception
                 (call-with-input-file
                     (string-append "/proc/" (number->string pid) "/cmdline")
                   (lambda (port)
                     (match (string-tokenize (read-string port) %not-nul)
                       ((argv0 _ ...)
                        (unless (member (pk (basename argv0)) spare)
                          (syslog "Killing process ~a~%" pid)
                          (kill pid SIGKILL)))
                       (_ #f))))))
              pids)))

(define (umount-cow-store)
  "Remove the store overlay and the bind-mount on /tmp created by the
cow-store service.  This procedure is very fragile and a better approach would
be much appreciated."

  ;; Remove when integrated in (gnu services herd).
  (define (restart-service name)
    (with-shepherd-action name ('restart) result
      result))

  (catch #t
    (lambda ()
      (let ((tmp-dir "/remove"))
        (mkdir-p tmp-dir)
        (mount (%store-directory) tmp-dir "" MS_MOVE)

        ;; The guix-daemon has possibly opened files from the cow-store,
        ;; restart it.
        (restart-service 'guix-daemon)

        ;; Kill all processes started while the cow-store was active (logins
        ;; on other TTYs for instance).
        (kill-cow-users tmp-dir)

        ;; Try to umount the store overlay. Some process such as udevd
        ;; workers might still be active, so do some retries.
        (let loop ((try 5))
          (sleep 1)
          (let ((umounted? (false-if-exception (umount tmp-dir))))
            (if (and (not umounted?) (> try 0))
                (loop (- try 1))
                (if umounted?
                    (syslog "Umounted ~a successfully.~%" tmp-dir)
                    (syslog "Failed to umount ~a.~%" tmp-dir)))))

        (umount "/tmp")))
    (lambda args
      (syslog "~a~%" args))))

(define* (install-system locale #:key (users '()))
  "Create /etc/shadow and /etc/passwd on the installation target for USERS.
Start COW-STORE service on target directory and launch guix install command in
a subshell.  LOCALE must be the locale name under which that command will run,
or #f.  Return #t on success and #f on failure."
  (let* ((options         (catch 'system-error
                            (lambda ()
                              ;; If this file exists, it can provide
                              ;; additional command-line options.
                              (call-with-input-file
                                  "/tmp/installer-system-init-options"
                                read))
                            (const '())))
         (install-command (append (list "guix" "system" "init"
                                        "--fallback")
                                  options
                                  (list (%installer-configuration-file)
                                        (%installer-target-dir)))))
    (mkdir-p (%installer-target-dir))

    ;; We want to initialize user passwords but we don't want to store them in
    ;; the config file since the password hashes would end up world-readable
    ;; in the store.  Thus, create /etc/shadow & co. here such that, on the
    ;; first boot, the activation snippet that creates accounts will reuse the
    ;; passwords that we've put in there.
    (create-user-database users (%installer-target-dir))

    (dynamic-wind
      (lambda ()
        (start-service 'cow-store (list (%installer-target-dir))))
      (lambda ()
        (run-command install-command #:locale locale))
      (lambda ()
        (stop-service 'cow-store)
        ;; Remove the store overlay created at cow-store service start.
        ;; Failing to do that will result in further umount calls to fail
        ;; because the target device is seen as busy. See:
        ;; https://lists.gnu.org/archive/html/guix-devel/2018-12/msg00161.html.
        (umount-cow-store)
        #f))))
