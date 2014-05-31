;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)                ; 'user-account', etc.
  #:use-module (gnu system linux)                 ; 'pam-service', etc.
  #:use-module (gnu packages admin)
  #:use-module ((gnu packages base)
                #:select (glibc-final))
  #:use-module (gnu packages package-management)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:export (root-file-system-service
            file-system-service
            user-processes-service
            host-name-service
            mingetty-service
            nscd-service
            syslog-service
            guix-service
            %base-services))

;;; Commentary:
;;;
;;; Base system services---i.e., services that 99% of the users will want to
;;; use.
;;;
;;; Code:

(define (root-file-system-service)
  "Return a service whose sole purpose is to re-mount read-only the root file
system upon shutdown (aka. cleanly \"umounting\" root.)

This service must be the root of the service dependency graph so that its
'stop' action is invoked when dmd is the only process left."
  (with-monad %store-monad
    (return
     (service
      (documentation "Take care of the root file system.")
      (provision '(root-file-system))
      (start #~(const #t))
      (stop #~(lambda _
                ;; Return #f if successfully stopped.
                (sync)

                (call-with-blocked-asyncs
                 (lambda ()
                   (let ((null (%make-void-port "w")))
                     ;; Close 'dmd.log'.
                     (display "closing log\n")
                     ;; XXX: Ideally we'd use 'stop-logging', but that one
                     ;; doesn't actually close the port as of dmd 0.1.
                     (close-port (@@ (dmd comm) log-output-port))
                     (set! (@@ (dmd comm) log-output-port) null)

                     ;; Redirect the default output ports..
                     (set-current-output-port null)
                     (set-current-error-port null)

                     ;; Close /dev/console.
                     (for-each close-fdes '(0 1 2))

                     ;; At this point, there are no open files left, so the
                     ;; root file system can be re-mounted read-only.
                     (mount #f "/" #f
                            (logior MS_REMOUNT MS_RDONLY)
                            #:update-mtab? #f)

                     #f)))))
      (respawn? #f)))))

(define* (file-system-service device target type
                              #:key (check? #t) options)
  "Return a service that mounts DEVICE on TARGET as a file system TYPE with
OPTIONS.  When CHECK? is true, check the file system before mounting it."
  (with-monad %store-monad
    (return
     (service
      (provision (list (symbol-append 'file-system- (string->symbol target))))
      (requirement '(root-file-system))
      (documentation "Check, mount, and unmount the given file system.")
      (start #~(lambda args
                 #$(if check?
                       #~(check-file-system #$device #$type)
                       #~#t)
                 (mount #$device #$target #$type 0 #$options)
                 #t))
      (stop #~(lambda args
                ;; Normally there are no processes left at this point, so
                ;; TARGET can be safely unmounted.
                (umount #$target)
                #f))))))

(define %do-not-kill-file
  ;; Name of the file listing PIDs of processes that must survive when halting
  ;; the system.  Typical example is user-space file systems.
  "/etc/dmd/do-not-kill")

(define* (user-processes-service requirements #:key (grace-delay 2))
  "Return the service that is responsible for terminating all the processes so
that the root file system can be re-mounted read-only, just before
rebooting/halting.  Processes still running GRACE-DELAY seconds after SIGTERM
has been sent are terminated with SIGKILL.

The returned service will depend on 'root-file-system' and on all the services
listed in REQUIREMENTS.

All the services that spawn processes must depend on this one so that they are
stopped before 'kill' is called."
  (with-monad %store-monad
    (return (service
             (documentation "When stopped, terminate all user processes.")
             (provision '(user-processes))
             (requirement (cons 'root-file-system requirements))
             (start #~(const #t))
             (stop #~(lambda _
                       (define (kill-except omit signal)
                         ;; Kill all the processes with SIGNAL except those
                         ;; listed in OMIT and the current process.
                         (let ((omit (cons (getpid) omit)))
                           (for-each (lambda (pid)
                                       (unless (memv pid omit)
                                         (false-if-exception
                                          (kill pid signal))))
                                     (processes))))

                       (define omitted-pids
                         ;; List of PIDs that must not be killed.
                         (if (file-exists? #$%do-not-kill-file)
                             (map string->number
                                  (call-with-input-file #$%do-not-kill-file
                                    (compose string-tokenize
                                             (@ (ice-9 rdelim) read-string))))
                             '()))

                       ;; When this happens, all the processes have been
                       ;; killed, including 'deco', so DMD-OUTPUT-PORT and
                       ;; thus CURRENT-OUTPUT-PORT are dangling.
                       (call-with-output-file "/dev/console"
                         (lambda (port)
                           (display "sending all processes the TERM signal\n"
                                    port)))

                       (if (null? omitted-pids)
                           (begin
                             ;; Easy: terminate all of them.
                             (kill -1 SIGTERM)
                             (sleep #$grace-delay)
                             (kill -1 SIGKILL))
                           (begin
                             ;; Kill them all except OMITTED-PIDS.  XXX: We
                             ;; would like to (kill -1 SIGSTOP) to get a fixed
                             ;; list of processes, like 'killall5' does, but
                             ;; that seems unreliable.
                             (kill-except omitted-pids SIGTERM)
                             (sleep #$grace-delay)
                             (kill-except omitted-pids SIGKILL)
                             (delete-file #$%do-not-kill-file)))

                       (display "all processes have been terminated\n")
                       #f))
             (respawn? #f)))))

(define (host-name-service name)
  "Return a service that sets the host name to NAME."
  (with-monad %store-monad
    (return (service
             (documentation "Initialize the machine's host name.")
             (provision '(host-name))
             (start #~(lambda _
                        (sethostname #$name)))
             (respawn? #f)))))

(define* (mingetty-service tty
                           #:key
                           (motd (text-file "motd" "Welcome.\n"))
                           auto-login
                           login-program
                           login-pause?
                           (allow-empty-passwords? #t))
  "Return a service to run mingetty on @var{tty}.

When @var{allow-empty-passwords?} is true, allow empty log-in password.  When
@var{auto-login} is true, it must be a user name under which to log-in
automatically.  @var{login-pause?} can be set to @code{#t} in conjunction with
@var{auto-login}, in which case the user will have to press a key before the
login shell is launched.

When true, @var{login-program} is a gexp or a monadic gexp denoting the name
of the log-in program (the default is the @code{login} program from the Shadow
tool suite.)

@var{motd} is a monadic value containing a text file to use as
the \"message of the day\"."
  (mlet %store-monad ((motd motd)
                      (login-program (cond ((gexp? login-program)
                                            (return login-program))
                                           ((not login-program)
                                            (return #f))
                                           (else
                                            login-program))))
    (return
     (service
      (documentation (string-append "Run mingetty on " tty "."))
      (provision (list (symbol-append 'term- (string->symbol tty))))

      ;; Since the login prompt shows the host name, wait for the 'host-name'
      ;; service to be done.
      (requirement '(user-processes host-name))

      (start  #~(make-forkexec-constructor
                 (string-append #$mingetty "/sbin/mingetty")
                 "--noclear" #$tty
                 #$@(if auto-login
                        #~("--autologin" #$auto-login)
                        #~())
                 #$@(if login-program
                        #~("--loginprog" #$login-program)
                        #~())
                 #$@(if login-pause?
                        #~("--loginpause")
                        #~())))
      (stop   #~(make-kill-destructor))

      (pam-services
       ;; Let 'login' be known to PAM.  All the mingetty services will have
       ;; that PAM service, but that's fine because they're all identical and
       ;; duplicates are removed.
       (list (unix-pam-service "login"
                               #:allow-empty-passwords? allow-empty-passwords?
                               #:motd motd)))))))

(define* (nscd-service #:key (glibc glibc-final))
  "Return a service that runs libc's name service cache daemon (nscd)."
  (with-monad %store-monad
    (return (service
             (documentation "Run libc's name service cache daemon (nscd).")
             (provision '(nscd))
             (requirement '(user-processes))

             (activate #~(begin
                           (use-modules (guix build utils))
                           (mkdir-p "/var/run/nscd")))

             (start
              #~(make-forkexec-constructor (string-append #$glibc "/sbin/nscd")
                                           "-f" "/dev/null"
                                           "--foreground"))
             (stop #~(make-kill-destructor))

             (respawn? #f)))))

(define (syslog-service)
  "Return a service that runs 'syslogd' with reasonable default settings."

  ;; Snippet adapted from the GNU inetutils manual.
  (define contents "
     # Log all kernel messages, authentication messages of
     # level notice or higher and anything of level err or
     # higher to the console.
     # Don't log private authentication messages!
     *.err;kern.*;auth.notice;authpriv.none  /dev/console

     # Log anything (except mail) of level info or higher.
     # Don't log private authentication messages!
     *.info;mail.none;authpriv.none          /var/log/messages

     # Same, in a different place.
     *.info;mail.none;authpriv.none          /dev/tty12

     # The authpriv file has restricted access.
     authpriv.*                              /var/log/secure

     # Log all the mail messages in one place.
     mail.*                                  /var/log/maillog
")

  (mlet %store-monad
      ((syslog.conf (text-file "syslog.conf" contents)))
    (return
     (service
      (documentation "Run the syslog daemon (syslogd).")
      (provision '(syslogd))
      (requirement '(user-processes))
      (start
       #~(make-forkexec-constructor (string-append #$inetutils
                                                   "/libexec/syslogd")
                                    "--no-detach"
                                    "--rcfile" #$syslog.conf))
      (stop #~(make-kill-destructor))))))

(define* (guix-build-accounts count #:key
                              (group "guixbuild")
                              (first-uid 30001)
                              (shadow shadow))
  "Return a list of COUNT user accounts for Guix build users, with UIDs
starting at FIRST-UID, and under GID."
  (with-monad %store-monad
    (return (unfold (cut > <> count)
                    (lambda (n)
                      (user-account
                       (name (format #f "guixbuilder~2,'0d" n))
                       (uid (+ first-uid n -1))
                       (group group)
                       (comment (format #f "Guix Build User ~2d" n))
                       (home-directory "/var/empty")
                       (shell #~(string-append #$shadow "/sbin/nologin"))))
                    1+
                    1))))

(define* (guix-service #:key (guix guix) (builder-group "guixbuild")
                       (build-accounts 10))
  "Return a service that runs the build daemon from GUIX, and has
BUILD-ACCOUNTS user accounts available under BUILD-USER-GID."
  (mlet %store-monad ((accounts (guix-build-accounts build-accounts
                                                     #:group builder-group)))
    (return (service
             (provision '(guix-daemon))
             (requirement '(user-processes))
             (start
              #~(make-forkexec-constructor (string-append #$guix
                                                          "/bin/guix-daemon")
                                           "--build-users-group"
                                           #$builder-group))
             (stop #~(make-kill-destructor))
             (user-accounts accounts)
             (user-groups (list (user-group
                                 (name builder-group)
                                 (members (map user-account-name
                                               user-accounts)))))))))

(define %base-services
  ;; Convenience variable holding the basic services.
  (let ((motd (text-file "motd" "
This is the GNU operating system, welcome!\n\n")))
    (list (mingetty-service "tty1" #:motd motd)
          (mingetty-service "tty2" #:motd motd)
          (mingetty-service "tty3" #:motd motd)
          (mingetty-service "tty4" #:motd motd)
          (mingetty-service "tty5" #:motd motd)
          (mingetty-service "tty6" #:motd motd)
          (syslog-service)
          (guix-service)
          (nscd-service))))

;;; base.scm ends here
