;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu build hurd-boot)
  #:use-module (system repl error-handling)
  #:autoload   (system repl repl) (start-repl)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (guix build utils)
  #:use-module ((guix build syscalls)
                #:hide (file-system-type))
  #:export (make-hurd-device-nodes
            boot-hurd-system))

;;; Commentary:
;;;
;;; Utility procedures useful to boot a Hurd system.
;;;
;;; Code:

;; XXX FIXME c&p from linux-boot.scm
(define (find-long-option option arguments)
  "Find OPTION among ARGUMENTS, where OPTION is something like \"--load\".
Return the value associated with OPTION, or #f on failure."
  (let ((opt (string-append option "=")))
    (and=> (find (cut string-prefix? opt <>)
                 arguments)
           (lambda (arg)
             (substring arg (+ 1 (string-index arg #\=)))))))

;; XXX FIXME c&p from guix/utils.scm
(define (readlink* file)
  "Call 'readlink' until the result is not a symlink."
  (define %max-symlink-depth 50)

  (let loop ((file  file)
             (depth 0))
    (define (absolute target)
      (if (absolute-file-name? target)
          target
          (string-append (dirname file) "/" target)))

    (if (>= depth %max-symlink-depth)
        file
        (call-with-values
            (lambda ()
              (catch 'system-error
                (lambda ()
                  (values #t (readlink file)))
                (lambda args
                  (let ((errno (system-error-errno args)))
                    (if (or (= errno EINVAL))
                        (values #f file)
                        (apply throw args))))))
          (lambda (success? target)
            (if success?
                (loop (absolute target) (+ depth 1))
                file))))))

(define* (make-hurd-device-nodes #:optional (root "/"))
  "Make some of the nodes needed on GNU/Hurd."
  (define (scope dir)
    (string-append root (if (string-suffix? "/" root) "" "/") dir))

  (mkdir (scope "dev"))
  (for-each (lambda (file)
              (call-with-output-file (scope file)
                (lambda (port)
                  (display file port)   ;avoid hard-linking
                  (chmod port #o666))))
            '("dev/null"
              "dev/zero"
              "dev/full"
              "dev/random"
              "dev/urandom"))
  ;; Don't create /dev/console, /dev/vcs, etc.: they are created by
  ;; console-run on first boot.

  (mkdir (scope "servers"))
  (for-each (lambda (file)
              (call-with-output-file (scope (string-append "servers/" file))
                (lambda (port)
                  (display file port)   ;avoid hard-linking
                  (chmod port #o444))))
            '("startup"
              "exec"
              "proc"
              "password"
              "default-pager"
              "crash-dump-core"
              "kill"
              "suspend"))

  (mkdir (scope "servers/socket"))
  ;; Don't create /servers/socket/1 & co: runsystem does that on first boot.

  ;; TODO: Set the 'gnu.translator' extended attribute for passive translator
  ;; settings?
  )


(define* (boot-hurd-system #:key (on-error 'debug))
  "This procedure is meant to be called from an early RC script.

Install the relevant passive translators on the first boot.  Then, run system
activation by using the kernel command-line options '--system' and '--load';
starting the Shepherd.

XXX TODO: see linux-boot.scm:boot-system.
XXX TODO: add proper file-system checking, mounting
XXX TODO: move bits to (new?) (hurd?) (activation?) services
XXX TODO: use settrans/setxattr instead of MAKEDEV

"
  (define translators
    '(("/servers/crash-dump-core" ("/hurd/crash" "--dump-core"))
      ("/servers/crash-kill" ("/hurd/crash" "--kill"))
      ("/servers/crash-suspend" ("/hurd/crash" "--suspend"))
      ("/servers/password" ("/hurd/password"))
      ("/servers/socket/1" ("/hurd/pflocal"))
      ("/servers/socket/2" ("/hurd/pfinet" "--interface" "eth0"
                            "--address" "10.0.2.15" ;the default QEMU guest IP
                            "--netmask" "255.255.255.0"
                            "--gateway" "10.0.2.2"
                            "--ipv6" "/servers/socket/16"))))

  (display "Welcome, this is GNU's early boot Guile.\n")
  (display "Use '--repl' for an initrd REPL.\n\n")

  (call-with-error-handling
   (lambda ()

     (define (translated? node)
       ;; Return true if a translator is installed on NODE.
       (with-output-to-port (%make-void-port "w")
         (lambda ()
           (with-error-to-port (%make-void-port "w")
             (lambda ()
               (zero? (system* "showtrans" "--silent" node)))))))

     (let* ((args    (command-line))
            (system  (find-long-option "--system" args))
            (to-load (find-long-option "--load" args)))

       (format #t "Creating essential servers...\n")
       (setenv "PATH" (string-append system "/profile/bin"
                                     ":" system "/profile/sbin"))
       (for-each (match-lambda
                   ((node command)
                    (unless (translated? node)
                      (mkdir-p (dirname node))
                      (apply invoke "settrans" "--create" node command))))
                 translators)

       (format #t "Creating essential device nodes...\n")
       (with-directory-excursion "/dev"
         (invoke "MAKEDEV" "--devdir=/dev" "std")
         (invoke "MAKEDEV" "--devdir=/dev" "vcs")
         (invoke "MAKEDEV" "--devdir=/dev" "tty1""tty2" "tty3" "tty4" "tty5" "tty6")
         (invoke "MAKEDEV" "--devdir=/dev" "ptyp0" "ptyp1" "ptyp2")
         (invoke "MAKEDEV" "--devdir=/dev" "console"))

       (false-if-exception (delete-file "/hurd"))
       (let ((hurd/hurd (readlink* (string-append system "/profile/hurd"))))
         (symlink hurd/hurd "/hurd"))

       (format #t "Starting pager...\n")
       (unless (zero? (system* "/hurd/mach-defpager"))
         (format #t "FAILED...Good luck!\n"))

       (cond ((member "--repl" args)
              (format #t "Starting repl...\n")
              (start-repl))
             (to-load
              (format #t "loading '~a'...\n" to-load)
              (primitive-load to-load)
              (format (current-error-port)
                      "boot program '~a' terminated, rebooting~%"
                      to-load)
              (sleep 2)
              (reboot))
             (else
              (display "no boot file passed via '--load'\n")
              (display "entering a warm and cozy REPL\n")
              (start-repl)))))
   #:on-error on-error))

;;; hurd-boot.scm ends here
