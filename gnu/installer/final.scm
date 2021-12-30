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
  #:use-module (gnu build install)
  #:use-module (gnu build linux-container)
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

(define (call-with-mnt-container thunk)
  "This is a variant of call-with-container. Run THUNK in a new container
process, within a separate MNT namespace. The container is not jailed so that
it can interact with the rest of the system."
  (let ((pid (run-container "/" '() '(mnt) 1 thunk)))
    ;; Catch SIGINT and kill the container process.
    (sigaction SIGINT
      (lambda (signum)
        (false-if-exception
         (kill pid SIGKILL))))

    (match (waitpid pid)
      ((_ . status) status))))

(define (install-locale locale)
  "Install the given LOCALE or the en_US.utf8 locale as a fallback."
  (let ((supported? (false-if-exception
                     (setlocale LC_ALL locale))))
    (if supported?
        (begin
          (syslog "install supported locale ~a~%." locale)
          (setenv "LC_ALL" locale))
        (begin
          ;; If the selected locale is not supported, install a default UTF-8
          ;; locale. This is required to copy some files with UTF-8
          ;; characters, in the nss-certs package notably. Set LANGUAGE
          ;; anyways, to have translated messages if possible.
          (syslog "~a locale is not supported, installating en_US.utf8 \
locale instead.~%" locale)
          (setlocale LC_ALL "en_US.utf8")
          (setenv "LC_ALL" "en_US.utf8")
          (setenv "LANGUAGE"
                  (string-take locale
                               (or (string-index locale #\_)
                                   (string-length locale))))))))

(define* (install-system locale #:key (users '()))
  "Create /etc/shadow and /etc/passwd on the installation target for USERS.
Start COW-STORE service on target directory and launch guix install command in
a subshell.  LOCALE must be the locale name under which that command will run,
or #f.  Return #t on success and #f on failure."
  (define backing-directory
    ;; Sub-directory used as the backing store for copy-on-write.
    "/tmp/guix-inst")

  (define (assert-exit x)
    (primitive-exit (if x 0 1)))

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
                                        (%installer-target-dir))))
         (database-dir    "/var/guix/db")
         (database-file   (string-append database-dir "/db.sqlite"))
         (saved-database  (string-append database-dir "/db.save"))
         (ret             #f))
    (mkdir-p (%installer-target-dir))

    ;; We want to initialize user passwords but we don't want to store them in
    ;; the config file since the password hashes would end up world-readable
    ;; in the store.  Thus, create /etc/shadow & co. here such that, on the
    ;; first boot, the activation snippet that creates accounts will reuse the
    ;; passwords that we've put in there.
    (create-user-database users (%installer-target-dir))

    ;; When the store overlay is mounted, other processes such as kmscon, udev
    ;; and guix-daemon may open files from the store, preventing the
    ;; underlying install support from being umounted. See:
    ;; https://lists.gnu.org/archive/html/guix-devel/2018-12/msg00161.html.
    ;;
    ;; To avoid this situation, mount the store overlay inside a container,
    ;; and run the installation from within that container.
    (zero?
     (call-with-mnt-container
       (lambda ()
         (dynamic-wind
           (lambda ()
             ;; Install the locale before mounting the cow-store, otherwise
             ;; the loaded cow-store locale files will prevent umounting.
             (install-locale locale)

             ;; Save the database, so that it can be restored once the
             ;; cow-store is umounted.
             (copy-file database-file saved-database)
             (mount-cow-store (%installer-target-dir) backing-directory))
           (lambda ()
             ;; We need to drag the guix-daemon to the container MNT
             ;; namespace, so that it can operate on the cow-store.
             (stop-service 'guix-daemon)
             (start-service 'guix-daemon (list (number->string (getpid))))

             (setvbuf (current-output-port) 'none)
             (setvbuf (current-error-port) 'none)

             ;; If there are any connected clients, assume that we are running
             ;; installation tests. In that case, dump the standard and error
             ;; outputs to syslog.
             (set! ret
                   (if (not (null? (current-clients)))
                       (with-output-to-file "/dev/console"
                         (lambda ()
                           (with-error-to-file "/dev/console"
                             (lambda ()
                               (run-command install-command)))))
                       (run-command install-command))))
           (lambda ()
             ;; Restart guix-daemon so that it does no keep the MNT namespace
             ;; alive.
             (restart-service 'guix-daemon)
             (copy-file saved-database database-file)

             ;; Finally umount the cow-store and exit the container.
             (unmount-cow-store (%installer-target-dir) backing-directory)
             (assert-exit ret))))))))
