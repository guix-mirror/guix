;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 pinoaffe <pinoaffe@airmail.cc>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu services ssh)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (lsh-configuration
            lsh-configuration?
            lsh-service
            lsh-service-type

            openssh-configuration
            openssh-configuration?
            openssh-service-type

            dropbear-configuration
            dropbear-configuration?
            dropbear-service-type
            dropbear-service

            autossh-configuration
            autossh-configuration?
            autossh-service-type

            webssh-configuration
            webssh-configuration?
            webssh-service-type
            %webssh-configuration-nginx))

;;; Commentary:
;;;
;;; This module implements secure shell (SSH) services.
;;;
;;; Code:

(define-record-type* <lsh-configuration>
  lsh-configuration make-lsh-configuration
  lsh-configuration?
  (lsh lsh-configuration-lsh
       (default lsh))
  (daemonic? lsh-configuration-daemonic?)
  (host-key lsh-configuration-host-key)
  (interfaces lsh-configuration-interfaces)
  (port-number lsh-configuration-port-number)
  (allow-empty-passwords? lsh-configuration-allow-empty-passwords?)
  (root-login? lsh-configuration-root-login?)
  (syslog-output? lsh-configuration-syslog-output?)
  (pid-file? lsh-configuration-pid-file?)
  (pid-file lsh-configuration-pid-file)
  (x11-forwarding? lsh-configuration-x11-forwarding?)
  (tcp/ip-forwarding? lsh-configuration-tcp/ip-forwarding?)
  (password-authentication? lsh-configuration-password-authentication?)
  (public-key-authentication? lsh-configuration-public-key-authentication?)
  (initialize? lsh-configuration-initialize?))

(define %yarrow-seed
  "/var/spool/lsh/yarrow-seed-file")

(define (lsh-initialization lsh host-key)
  "Return the gexp to initialize the LSH service for HOST-KEY."
  #~(begin
      (unless (file-exists? #$%yarrow-seed)
        (system* (string-append #$lsh "/bin/lsh-make-seed")
                 "--sloppy" "-o" #$%yarrow-seed))

      (unless (file-exists? #$host-key)
        (mkdir-p (dirname #$host-key))
        (format #t "creating SSH host key '~a'...~%" #$host-key)

        ;; FIXME: We're just doing a simple pipeline, but 'system' cannot be
        ;; used yet because /bin/sh might be dangling; factorize this somehow.
        (let* ((in+out (pipe))
               (keygen (primitive-fork)))
          (case keygen
            ((0)
             (close-port (car in+out))
             (close-fdes 1)
             (dup2 (fileno (cdr in+out)) 1)
             (execl (string-append #$lsh "/bin/lsh-keygen")
                    "lsh-keygen" "--server"))
            (else
             (let ((write-key (primitive-fork)))
               (case write-key
                 ((0)
                  (close-port (cdr in+out))
                  (close-fdes 0)
                  (dup2 (fileno (car in+out)) 0)
                  (execl (string-append #$lsh "/bin/lsh-writekey")
                         "lsh-writekey" "--server" "-o" #$host-key))
                 (else
                  (close-port (car in+out))
                  (close-port (cdr in+out))
                  (waitpid keygen)
                  (waitpid write-key))))))))))

(define (lsh-activation config)
  "Return the activation gexp for CONFIG."
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/spool/lsh")
      #$(if (lsh-configuration-initialize? config)
            (lsh-initialization (lsh-configuration-lsh config)
                                (lsh-configuration-host-key config))
            #t)))

(define (lsh-shepherd-service config)
  "Return a <shepherd-service> for lsh with CONFIG."
  (define lsh (lsh-configuration-lsh config))
  (define pid-file (lsh-configuration-pid-file config))
  (define pid-file? (lsh-configuration-pid-file? config))
  (define daemonic? (lsh-configuration-daemonic? config))
  (define interfaces (lsh-configuration-interfaces config))

  (define lsh-command
    (append
     (cons (file-append lsh "/sbin/lshd")
           (if daemonic?
               (let ((syslog (if (lsh-configuration-syslog-output? config)
                                 '()
                                 (list "--no-syslog"))))
                 (cons "--daemonic"
                       (if pid-file?
                           (cons #~(string-append "--pid-file=" #$pid-file)
                                 syslog)
                           (cons "--no-pid-file" syslog))))
               (if pid-file?
                   (list #~(string-append "--pid-file=" #$pid-file))
                   '())))
     (cons* #~(string-append "--host-key="
                             #$(lsh-configuration-host-key config))
            #~(string-append "--password-helper=" #$lsh "/sbin/lsh-pam-checkpw")
            #~(string-append "--subsystems=sftp=" #$lsh "/sbin/sftp-server")
            "-p" (number->string (lsh-configuration-port-number config))
            (if (lsh-configuration-password-authentication? config)
                "--password" "--no-password")
            (if (lsh-configuration-public-key-authentication? config)
                "--publickey" "--no-publickey")
            (if (lsh-configuration-root-login? config)
                "--root-login" "--no-root-login")
            (if (lsh-configuration-x11-forwarding? config)
                "--x11-forward" "--no-x11-forward")
            (if (lsh-configuration-tcp/ip-forwarding? config)
                "--tcpip-forward" "--no-tcpip-forward")
            (if (null? interfaces)
                '()
                (map (cut string-append "--interface=" <>)
                     interfaces)))))

  (define requires
    (if (and daemonic? (lsh-configuration-syslog-output? config))
        '(networking syslogd)
        '(networking)))

  (list (shepherd-service
         (documentation "GNU lsh SSH server")
         (provision '(ssh-daemon ssh sshd))
         (requirement requires)
         (start #~(make-forkexec-constructor (list #$@lsh-command)))
         (stop  #~(make-kill-destructor)))))

(define (lsh-pam-services config)
  "Return a list of <pam-services> for lshd with CONFIG."
  (list (unix-pam-service
         "lshd"
         #:login-uid? #t
         #:allow-empty-passwords?
         (lsh-configuration-allow-empty-passwords? config))))

(define lsh-service-type
  (service-type (name 'lsh)
                (description
                 "Run the GNU@tie{}lsh secure shell (SSH) daemon,
@command{lshd}.")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          lsh-shepherd-service)
                       (service-extension pam-root-service-type
                                          lsh-pam-services)
                       (service-extension activation-service-type
                                          lsh-activation)))))

(define* (lsh-service #:key
                      (lsh lsh)
                      (daemonic? #t)
                      (host-key "/etc/lsh/host-key")
                      (interfaces '())
                      (port-number 22)
                      (allow-empty-passwords? #f)
                      (root-login? #f)
                      (syslog-output? #t)
                      (pid-file? #f)
                      (pid-file "/var/run/lshd.pid")
                      (x11-forwarding? #t)
                      (tcp/ip-forwarding? #t)
                      (password-authentication? #t)
                      (public-key-authentication? #t)
                      (initialize? #t))
  "Run the @command{lshd} program from @var{lsh} to listen on port @var{port-number}.
@var{host-key} must designate a file containing the host key, and readable
only by root.

When @var{daemonic?} is true, @command{lshd} will detach from the
controlling terminal and log its output to syslogd, unless one sets
@var{syslog-output?} to false.  Obviously, it also makes lsh-service
depend on existence of syslogd service.  When @var{pid-file?} is true,
@command{lshd} writes its PID to the file called @var{pid-file}.

When @var{initialize?} is true, automatically create the seed and host key
upon service activation if they do not exist yet.  This may take long and
require interaction.

When @var{initialize?} is false, it is up to the user to initialize the
randomness generator (@pxref{lsh-make-seed,,, lsh, LSH Manual}), and to create
a key pair with the private key stored in file @var{host-key} (@pxref{lshd
basics,,, lsh, LSH Manual}).

When @var{interfaces} is empty, lshd listens for connections on all the
network interfaces; otherwise, @var{interfaces} must be a list of host names
or addresses.

@var{allow-empty-passwords?} specifies whether to accept log-ins with empty
passwords, and @var{root-login?} specifies whether to accept log-ins as
root.

The other options should be self-descriptive."
  (service lsh-service-type
           (lsh-configuration (lsh lsh) (daemonic? daemonic?)
                              (host-key host-key) (interfaces interfaces)
                              (port-number port-number)
                              (allow-empty-passwords? allow-empty-passwords?)
                              (root-login? root-login?)
                              (syslog-output? syslog-output?)
                              (pid-file? pid-file?) (pid-file pid-file)
                              (x11-forwarding? x11-forwarding?)
                              (tcp/ip-forwarding? tcp/ip-forwarding?)
                              (password-authentication?
                               password-authentication?)
                              (public-key-authentication?
                               public-key-authentication?)
                              (initialize? initialize?))))


;;;
;;; OpenSSH.
;;;

(define-record-type* <openssh-configuration>
  openssh-configuration make-openssh-configuration
  openssh-configuration?
  ;; file-like object
  (openssh               openssh-configuration-openssh
                         (default openssh))
  ;; string
  (pid-file              openssh-configuration-pid-file
                         (default "/var/run/sshd.pid"))
  ;; integer
  (port-number           openssh-configuration-port-number
                         (default 22))
  ;; Boolean | 'prohibit-password
  (permit-root-login     openssh-configuration-permit-root-login
                         (default #f))
  ;; Boolean
  (allow-empty-passwords? openssh-configuration-allow-empty-passwords?
                          (default #f))
  ;; Boolean
  (password-authentication? openssh-configuration-password-authentication?
                            (default #t))
  ;; Boolean
  (public-key-authentication? openssh-configuration-public-key-authentication?
                              (default #t))
  ;; Boolean
  (x11-forwarding?       openssh-configuration-x11-forwarding?
                         (default #f))

  ;; Boolean
  (allow-agent-forwarding? openssh-configuration-allow-agent-forwarding?
                           (default #t))

  ;; Boolean
  (allow-tcp-forwarding? openssh-configuration-allow-tcp-forwarding?
                         (default #t))

  ;; Boolean
  (gateway-ports? openssh-configuration-gateway-ports?
                         (default #f))

  ;; Boolean
  (challenge-response-authentication? openssh-challenge-response-authentication?
                                      (default #f))
  ;; Boolean
  (use-pam?              openssh-configuration-use-pam?
                         (default #t))
  ;; Boolean
  (print-last-log?       openssh-configuration-print-last-log?
                         (default #t))
  ;; list of two-element lists
  (subsystems            openssh-configuration-subsystems
                         (default '(("sftp" "internal-sftp"))))

  ;; list of strings
  (accepted-environment  openssh-configuration-accepted-environment
                         (default '()))

  ;; symbol
  (log-level             openssh-configuration-log-level
                         (default 'info))

  ;; String
  ;; This is an "escape hatch" to provide configuration that isn't yet
  ;; supported by this configuration record.
  (extra-content         openssh-configuration-extra-content
                         (default ""))

  ;; list of user-name/file-like tuples
  (authorized-keys       openssh-authorized-keys
                         (default '()))

  ;; Boolean
  ;; XXX: This should really be handled in an orthogonal way, for instance as
  ;; proposed in <https://bugs.gnu.org/27155>.  Keep it internal/undocumented
  ;; for now.
  (%auto-start?          openssh-auto-start?
                         (default #t)))

(define %openssh-accounts
  (list (user-group (name "sshd") (system? #t))
        (user-account
          (name "sshd")
          (group "sshd")
          (system? #t)
          (comment "sshd privilege separation user")
          (home-directory "/var/run/sshd")
          (shell (file-append shadow "/sbin/nologin")))))

(define (openssh-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (define (touch file-name)
          (call-with-output-file file-name (const #t)))

        ;; Make sure /etc/ssh can be read by the 'sshd' user.
        (mkdir-p "/etc/ssh")
        (chmod "/etc/ssh" #o755)
        (mkdir-p (dirname #$(openssh-configuration-pid-file config)))

        ;; 'sshd' complains if the authorized-key directory and its parents
        ;; are group-writable, which rules out /gnu/store.  Thus we copy the
        ;; authorized-key directory to /etc.
        (catch 'system-error
          (lambda ()
            (delete-file-recursively "/etc/authorized_keys.d"))
          (lambda args
            (unless (= ENOENT (system-error-errno args))
              (apply throw args))))
        (copy-recursively #$(authorized-key-directory
                             (openssh-authorized-keys config))
                          "/etc/ssh/authorized_keys.d")

        (chmod "/etc/ssh/authorized_keys.d" #o555)

        (let ((lastlog "/var/log/lastlog"))
          (when #$(openssh-configuration-print-last-log? config)
            (unless (file-exists? lastlog)
              (touch lastlog))))

        ;; Generate missing host keys.
        (system* (string-append #$(openssh-configuration-openssh config)
                                "/bin/ssh-keygen") "-A"))))

(define (authorized-key-directory keys)
  "Return a directory containing the authorized keys specified in KEYS, a list
of user-name/file-like tuples."
  (define build
    (with-imported-modules (source-module-closure '((guix build utils)))
      #~(begin
          (use-modules (ice-9 match) (srfi srfi-26)
                       (guix build utils))

          (mkdir #$output)
          (for-each (match-lambda
                      ((user keys ...)
                       (let ((file (string-append #$output "/" user)))
                         (call-with-output-file file
                           (lambda (port)
                             (for-each (lambda (key)
                                         (call-with-input-file key
                                           (cut dump-port <> port)))
                                       keys))))))
                    '#$keys))))

  (computed-file "openssh-authorized-keys" build))

(define (openssh-config-file config)
  "Return the sshd configuration file corresponding to CONFIG."
  (computed-file
   "sshd_config"
   #~(begin
       (use-modules (ice-9 match))
       (call-with-output-file #$output
         (lambda (port)
           (display "# Generated by 'openssh-service'.\n" port)
           (format port "Port ~a\n"
                   #$(number->string
                      (openssh-configuration-port-number config)))
           (format port "PermitRootLogin ~a\n"
                   #$(match (openssh-configuration-permit-root-login config)
                       (#t "yes")
                       (#f "no")
                       ('without-password (warn-about-deprecation
                                           'without-password #f
                                           #:replacement 'prohibit-password)
                                          "prohibit-password")
                       ('prohibit-password "prohibit-password")))
           (format port "PermitEmptyPasswords ~a\n"
                   #$(if (openssh-configuration-allow-empty-passwords? config)
                         "yes" "no"))
           (format port "PasswordAuthentication ~a\n"
                   #$(if (openssh-configuration-password-authentication? config)
                         "yes" "no"))
           (format port "PubkeyAuthentication ~a\n"
                   #$(if (openssh-configuration-public-key-authentication?
                          config)
                         "yes" "no"))
           (format port "X11Forwarding ~a\n"
                   #$(if (openssh-configuration-x11-forwarding? config)
                         "yes" "no"))
           (format port "AllowAgentForwarding ~a\n"
                   #$(if (openssh-configuration-allow-agent-forwarding? config)
                         "yes" "no"))
           (format port "AllowTcpForwarding ~a\n"
                   #$(if (openssh-configuration-allow-tcp-forwarding? config)
                         "yes" "no"))
           (format port "GatewayPorts ~a\n"
                   #$(if (openssh-configuration-gateway-ports? config)
                         "yes" "no"))
           (format port "PidFile ~a\n"
                   #$(openssh-configuration-pid-file config))
           (format port "ChallengeResponseAuthentication ~a\n"
                   #$(if (openssh-challenge-response-authentication? config)
                         "yes" "no"))
           (format port "UsePAM ~a\n"
                   #$(if (openssh-configuration-use-pam? config)
                         "yes" "no"))
           (format port "PrintLastLog ~a\n"
                   #$(if (openssh-configuration-print-last-log? config)
                         "yes" "no"))
           (format port "LogLevel ~a\n"
                   #$(string-upcase
                      (symbol->string
                       (openssh-configuration-log-level config))))

           ;; Add '/etc/authorized_keys.d/%u', which we populate.
           (format port "AuthorizedKeysFile \
 .ssh/authorized_keys .ssh/authorized_keys2 /etc/ssh/authorized_keys.d/%u\n")

           (for-each (lambda (s) (format port "AcceptEnv ~a\n" s))
                     '#$(openssh-configuration-accepted-environment config))

           (for-each
            (match-lambda
              ((name command) (format port "Subsystem\t~a\t~a\n" name command)))
            '#$(openssh-configuration-subsystems config))

           (format port "~a\n"
                   #$(openssh-configuration-extra-content config))
           #t)))))

(define (openssh-shepherd-service config)
  "Return a <shepherd-service> for openssh with CONFIG."

  (define pid-file
    (openssh-configuration-pid-file config))

  (define openssh-command
    #~(list (string-append #$(openssh-configuration-openssh config) "/sbin/sshd")
            "-D" "-f" #$(openssh-config-file config)))

  (list (shepherd-service
         (documentation "OpenSSH server.")
         (requirement '(syslogd loopback))
         (provision '(ssh-daemon ssh sshd))
         (start #~(make-forkexec-constructor #$openssh-command
                                             #:pid-file #$pid-file))
         (stop #~(make-kill-destructor))
         (auto-start? (openssh-auto-start? config)))))

(define (openssh-pam-services config)
  "Return a list of <pam-services> for sshd with CONFIG."
  (list (unix-pam-service
         "sshd"
         #:login-uid? #t
         #:allow-empty-passwords?
         (openssh-configuration-allow-empty-passwords? config))))

(define (extend-openssh-authorized-keys config keys)
  "Extend CONFIG with the extra authorized keys listed in KEYS."
  (openssh-configuration
   (inherit config)
   (authorized-keys
    (match (openssh-authorized-keys config)
      (((users _ ...) ...)
       ;; Build a user/key-list mapping.
       (let ((user-keys (alist->vhash (openssh-authorized-keys config))))
         ;; Coalesce the key lists associated with each user.
         (map (lambda (user)
                `(,user
                  ,@(concatenate (vhash-fold* cons '() user user-keys))))
              users)))))))

(define openssh-service-type
  (service-type (name 'openssh)
                (description
                 "Run the OpenSSH secure shell (SSH) server, @command{sshd}.")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          openssh-shepherd-service)
                       (service-extension pam-root-service-type
                                          openssh-pam-services)
                       (service-extension activation-service-type
                                          openssh-activation)
                       (service-extension account-service-type
                                          (const %openssh-accounts))

                       ;; Install OpenSSH in the system profile.  That way,
                       ;; 'scp' is found when someone tries to copy to or from
                       ;; this machine.
                       (service-extension profile-service-type
                                          (lambda (config)
                                            (list (openssh-configuration-openssh
                                                   config))))))
                (compose concatenate)
                (extend extend-openssh-authorized-keys)
                (default-value (openssh-configuration))))


;;;
;;; Dropbear.
;;;

(define-record-type* <dropbear-configuration>
  dropbear-configuration make-dropbear-configuration
  dropbear-configuration?
  (dropbear               dropbear-configuration-dropbear
                          (default dropbear))
  (port-number            dropbear-configuration-port-number
                          (default 22))
  (syslog-output?         dropbear-configuration-syslog-output?
                          (default #t))
  (pid-file               dropbear-configuration-pid-file
                          (default "/var/run/dropbear.pid"))
  (root-login?            dropbear-configuration-root-login?
                          (default #f))
  (allow-empty-passwords? dropbear-configuration-allow-empty-passwords?
                          (default #f))
  (password-authentication? dropbear-configuration-password-authentication?
                            (default #t)))

(define (dropbear-activation config)
  "Return the activation gexp for CONFIG."
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/etc/dropbear")))

(define (dropbear-shepherd-service config)
  "Return a <shepherd-service> for dropbear with CONFIG."
  (define dropbear
    (dropbear-configuration-dropbear config))

  (define pid-file
    (dropbear-configuration-pid-file config))

  (define dropbear-command
    #~(list (string-append #$dropbear "/sbin/dropbear")

            ;; '-R' allows host keys to be automatically generated upon first
            ;; connection, at a time when /dev/urandom is more likely securely
            ;; seeded.
            "-F" "-R"

            "-p" #$(number->string (dropbear-configuration-port-number config))
            "-P" #$pid-file
            #$@(if (dropbear-configuration-syslog-output? config) '() '("-E"))
            #$@(if (dropbear-configuration-root-login? config) '() '("-w"))
            #$@(if (dropbear-configuration-password-authentication? config)
                   '()
                   '("-s" "-g"))
            #$@(if (dropbear-configuration-allow-empty-passwords? config)
                   '("-B")
                   '())))

  (define requires
    (if (dropbear-configuration-syslog-output? config)
        '(networking syslogd) '(networking)))

  (list (shepherd-service
         (documentation "Dropbear SSH server.")
         (requirement requires)
         (provision '(ssh-daemon ssh sshd))
         (start #~(make-forkexec-constructor #$dropbear-command
                                             #:pid-file #$pid-file))
         (stop #~(make-kill-destructor)))))

(define dropbear-service-type
  (service-type (name 'dropbear)
                (description
                 "Run the Dropbear secure shell (SSH) server.")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          dropbear-shepherd-service)
                       (service-extension activation-service-type
                                          dropbear-activation)))
                (default-value (dropbear-configuration))))

(define* (dropbear-service #:optional (config (dropbear-configuration)))
  "Run the @uref{https://matt.ucc.asn.au/dropbear/dropbear.html,Dropbear SSH
daemon} with the given @var{config}, a @code{<dropbear-configuration>}
object."
  (service dropbear-service-type config))


;;;
;;; AutoSSH.
;;;


(define-record-type* <autossh-configuration>
  autossh-configuration make-autossh-configuration
  autossh-configuration?
  (user            autossh-configuration-user
                   (default "autossh"))
  (poll            autossh-configuration-poll
                   (default 600))
  (first-poll      autossh-configuration-first-poll
                   (default #f))
  (gate-time       autossh-configuration-gate-time
                   (default 30))
  (log-level       autossh-configuration-log-level
                   (default 1))
  (max-start       autossh-configuration-max-start
                   (default #f))
  (message         autossh-configuration-message
                   (default ""))
  (port            autossh-configuration-port
                   (default "0"))
  (ssh-options     autossh-configuration-ssh-options
                   (default '())))

(define (autossh-file-name config file)
  "Return a path in /var/run/autossh/ that is writable
   by @code{user} from @code{config}."
  (string-append "/var/run/autossh/"
                 (autossh-configuration-user config)
                 "/" file))

(define (autossh-shepherd-service config)
  (shepherd-service
   (documentation "Automatically set up ssh connections (and keep them alive).")
   (provision '(autossh))
   (start #~(make-forkexec-constructor
             (list #$(file-append autossh "/bin/autossh")
                   #$@(autossh-configuration-ssh-options config))
             #:user #$(autossh-configuration-user config)
             #:group (passwd:gid (getpw #$(autossh-configuration-user config)))
             #:pid-file #$(autossh-file-name config "pid")
             #:log-file #$(autossh-file-name config "log")
             #:environment-variables
             '(#$(string-append "AUTOSSH_PIDFILE="
                                (autossh-file-name config "pid"))
               #$(string-append "AUTOSSH_LOGFILE="
                                (autossh-file-name config "log"))
               #$(string-append "AUTOSSH_POLL="
                                (number->string
                                 (autossh-configuration-poll config)))
               #$(string-append "AUTOSSH_FIRST_POLL="
                                (number->string
                                 (or
                                  (autossh-configuration-first-poll config)
                                  (autossh-configuration-poll config))))
               #$(string-append "AUTOSSH_GATETIME="
                                (number->string
                                 (autossh-configuration-gate-time config)))
               #$(string-append "AUTOSSH_LOGLEVEL="
                                (number->string
                                 (autossh-configuration-log-level config)))
               #$(string-append "AUTOSSH_MAXSTART="
                                (number->string
                                 (or (autossh-configuration-max-start config)
                                     -1)))
               #$(string-append "AUTOSSH_MESSAGE="
                                (autossh-configuration-message config))
               #$(string-append "AUTOSSH_PORT="
                                (autossh-configuration-port config)))))
   (stop #~(make-kill-destructor))))

(define (autossh-service-activation config)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (define %user
          (getpw #$(autossh-configuration-user config)))
        (let* ((directory #$(autossh-file-name config ""))
               (log (string-append directory "/log")))
          (mkdir-p directory)
          (chown directory (passwd:uid %user) (passwd:gid %user))
          (call-with-output-file log (const #t))
          (chown log (passwd:uid %user) (passwd:gid %user))))))

(define autossh-service-type
  (service-type
   (name 'autossh)
   (description "Automatically set up ssh connections (and keep them alive).")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list autossh-shepherd-service))
          (service-extension activation-service-type
                             autossh-service-activation)))
   (default-value (autossh-configuration))))


;;;
;;; WebSSH
;;;

(define-record-type* <webssh-configuration>
  webssh-configuration make-webssh-configuration
  webssh-configuration?
  (package     webssh-configuration-package     ;file-like
               (default webssh))
  (user-name   webssh-configuration-user-name   ;string
               (default "webssh"))
  (group-name  webssh-configuration-group-name  ;string
               (default "webssh"))
  (policy      webssh-configuration-policy      ;symbol
               (default #f))
  (known-hosts webssh-configuration-known-hosts ;list of strings
               (default #f))
  (port        webssh-configuration-port        ;number
               (default #f))
  (address     webssh-configuration-address     ;string
               (default #f))
  (log-file    webssh-configuration-log-file    ;string
               (default "/var/log/webssh.log"))
  (log-level   webssh-configuration-log-level   ;symbol
               (default #f)))

(define %webssh-configuration-nginx
  (nginx-server-configuration
   (listen '("80"))
   (locations
    (list (nginx-location-configuration
           (uri "/")
           (body '("proxy_pass http://127.0.0.1:8888;"
                   "proxy_http_version 1.1;"
                   "proxy_read_timeout 300;"
                   "proxy_set_header Upgrade $http_upgrade;"
                   "proxy_set_header Connection \"upgrade\";"
                   "proxy_set_header Host $http_host;"
                   "proxy_set_header X-Real-IP $remote_addr;"
                   "proxy_set_header X-Real-PORT $remote_port;")))))))

(define webssh-account
  ;; Return the user accounts and user groups for CONFIG.
  (match-lambda
    (($ <webssh-configuration> _ user-name group-name _ _ _ _ _ _)
     (list (user-group
            (name group-name))
           (user-account
            (name user-name)
            (group group-name)
            (comment "webssh privilege separation user")
            (home-directory (string-append "/var/run/" user-name))
            (shell #~(string-append #$shadow "/sbin/nologin")))))))

(define webssh-activation
  ;; Return the activation GEXP for CONFIG.
  (match-lambda
    (($ <webssh-configuration> _ user-name group-name policy known-hosts _ _
                               log-file _)
     (with-imported-modules '((guix build utils))
       #~(begin
           (let* ((home-dir (string-append "/var/run/" #$user-name))
                  (ssh-dir (string-append home-dir "/.ssh"))
                  (known-hosts-file (string-append ssh-dir "/known_hosts")))
             (call-with-output-file #$log-file (const #t))
             (mkdir-p ssh-dir)
             (case '#$policy
               ((reject)
                (if '#$known-hosts
                    (call-with-output-file known-hosts-file
                      (lambda (port)
                        (for-each (lambda (host) (display host port) (newline port))
                                  '#$known-hosts)))
                    (display-hint (G_ "webssh: reject policy requires `known-hosts'.")))))
             (for-each (lambda (file)
                         (chown file
                                (passwd:uid (getpw #$user-name))
                                (group:gid (getpw #$group-name))))
                       (list #$log-file ssh-dir known-hosts-file))
             (chmod ssh-dir #o700)))))))

(define webssh-shepherd-service
  (match-lambda
    (($ <webssh-configuration> package user-name group-name policy _ port
                               address log-file log-level)
     (list (shepherd-service
            (provision '(webssh))
            (documentation "Run webssh daemon.")
            (start #~(make-forkexec-constructor
                      `(,(string-append #$webssh "/bin/wssh")
                        ,(string-append "--log-file-prefix=" #$log-file)
                        ,@(case '#$log-level
                            ((debug) '("--logging=debug"))
                            (else '()))
                        ,@(case '#$policy
                            ((reject) '("--policy=reject"))
                            (else '()))
                        ,@(if #$port
                              (list (string-append "--port=" (number->string #$port)))
                              '())
                        ,@(if #$address
                              (list (string-append "--address=" #$address))
                              '()))
                      #:user #$user-name
                      #:group #$group-name))
            (stop #~(make-kill-destructor)))))))

(define webssh-service-type
  (service-type
   (name 'webssh)
   (extensions
    (list (service-extension shepherd-root-service-type
                             webssh-shepherd-service)
          (service-extension account-service-type
                             webssh-account)
          (service-extension activation-service-type
                             webssh-activation)))
   (default-value (webssh-configuration))
   (description
    "Run the webssh.")))

;;; ssh.scm ends here
