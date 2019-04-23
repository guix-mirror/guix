;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
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

(define-module (gnu system shadow)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:use-module (guix sets)
  #:use-module (guix ui)
  #:use-module (gnu system accounts)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module ((gnu system file-systems)
                #:select (%tty-gid))
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile-wm)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)

  ;; Re-export these bindings for backward compatibility.
  #:re-export (user-account
               user-account?
               user-account-name
               user-account-password
               user-account-uid
               user-account-group
               user-account-supplementary-groups
               user-account-comment
               user-account-home-directory
               user-account-create-home-directory?
               user-account-shell
               user-account-system?

               user-group
               user-group?
               user-group-name
               user-group-password
               user-group-id
               user-group-system?)

  #:export (default-skeletons
            skeleton-directory
            %base-groups
            %base-user-accounts

            account-service-type
            account-service))

;;; Commentary:
;;;
;;; Utilities for configuring the Shadow tool suite ('login', 'passwd', etc.)
;;;
;;; Code:

;; Change the default shell used by new <user-account> records.
(default-shell (file-append bash "/bin/bash"))

(define %base-groups
  ;; Default set of groups.
  (let-syntax ((system-group (syntax-rules ()
                               ((_ args ...)
                                (user-group (system? #t) args ...)))))
    (list (system-group (name "root") (id 0))
          (system-group (name "wheel"))           ; root-like users
          (system-group (name "users"))           ; normal users
          (system-group (name "nogroup"))         ; for daemons etc.

          ;; The following groups are conventionally used by things like udev to
          ;; control access to hardware devices.
          (system-group (name "tty") (id %tty-gid))
          (system-group (name "dialout"))
          (system-group (name "kmem"))
          (system-group (name "input"))           ; input devices, from udev
          (system-group (name "video"))
          (system-group (name "audio"))
          (system-group (name "netdev"))          ; used in avahi-dbus.conf
          (system-group (name "lp"))
          (system-group (name "disk"))
          (system-group (name "floppy"))
          (system-group (name "cdrom"))
          (system-group (name "tape"))
          (system-group (name "kvm")))))          ; for /dev/kvm

(define %base-user-accounts
  ;; List of standard user accounts.  Note that "root" is a special case, so
  ;; it's not listed here.
  (list (user-account
         (name "nobody")
         (uid 65534)
         (group "nogroup")
         (shell (file-append shadow "/sbin/nologin"))
         (home-directory "/nonexistent")
         (create-home-directory? #f)
         (system? #t))))

(define (default-skeletons)
  "Return the default skeleton files for /etc/skel.  These files are copied by
'useradd' in the home directory of newly created user accounts."
  (define copy-guile-wm
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (copy-file (car (find-files #+guile-wm "wm-init-sample.scm"))
                     #$output))))

  (let ((profile (plain-file "bash_profile" "\
# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi\n"))
        (bashrc  (plain-file "bashrc" "\
# Bash initialization for interactive non-login shells and
# for remote shells (info \"(bash) Bash Startup Files\").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in \"ssh host command\"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n \"$SSH_CLIENT\" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

# Source the system-wide file.
source /etc/bashrc

# Adjust the prompt depending on whether we're in 'guix environment'.
if [ -n \"$GUIX_ENVIRONMENT\" ]
then
    PS1='\\u@\\h \\w [env]\\$ '
else
    PS1='\\u@\\h \\w\\$ '
fi
alias ls='ls -p --color=auto'
alias ll='ls -l'
alias grep='grep --color=auto'\n"))
        (zprofile    (plain-file "zprofile" "\
# Honor system-wide environment variables
source /etc/profile\n"))
        (guile-wm  (computed-file "guile-wm" copy-guile-wm))
        (xdefaults (plain-file "Xdefaults" "\
XTerm*utf8: always
XTerm*metaSendsEscape: true\n"))
        (gdbinit   (plain-file "gdbinit" "\
# Tell GDB where to look for separate debugging files.
set debug-file-directory ~/.guix-profile/lib/debug

# Authorize extensions found in the store, such as the
# pretty-printers of libstdc++.
set auto-load safe-path /gnu/store/*/lib\n")))
    `((".bash_profile" ,profile)
      (".bashrc" ,bashrc)
      ;; Zsh sources ~/.zprofile before ~/.zshrc, and it sources ~/.zlogin
      ;; after ~/.zshrc.  To avoid interfering with any customizations a user
      ;; may have made in their ~/.zshrc, put this in .zprofile, not .zlogin.
      (".zprofile" ,zprofile)
      (".nanorc" ,(plain-file "nanorc" "\
# Include all the syntax highlighting modules.
include /run/current-system/profile/share/nano/*.nanorc\n"))
      (".Xdefaults" ,xdefaults)
      (".guile" ,(plain-file "dot-guile"
                             "(cond ((false-if-exception (resolve-interface '(ice-9 readline)))
       =>
       (lambda (module)
         ;; Enable completion and input history at the REPL.
         ((module-ref module 'activate-readline))))
      (else
       (display \"Consider installing the 'guile-readline' package for
convenient interactive line editing and input history.\\n\\n\")))

      (unless (getenv \"INSIDE_EMACS\")
        (cond ((false-if-exception (resolve-interface '(ice-9 colorized)))
               =>
               (lambda (module)
                 ;; Enable completion and input history at the REPL.
                 ((module-ref module 'activate-colorized))))
              (else
               (display \"Consider installing the 'guile-colorized' package
for a colorful Guile experience.\\n\\n\"))))\n"))
      (".guile-wm" ,guile-wm)
      (".gdbinit" ,gdbinit))))

(define (skeleton-directory skeletons)
  "Return a directory containing SKELETONS, a list of name/derivation tuples."
  (computed-file "skel"
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (ice-9 match)
                                    (guix build utils))

                       (mkdir #$output)
                       (chdir #$output)

                       ;; Note: copy the skeletons instead of symlinking
                       ;; them like 'file-union' does, because 'useradd'
                       ;; would just copy the symlinks as is.
                       (for-each (match-lambda
                                   ((target source)
                                    (copy-recursively source target)))
                                 '#$skeletons)
                       #t))))

(define (assert-valid-users/groups users groups)
  "Raise an error if USERS refer to groups not listed in GROUPS."
  (let ((groups (list->set (map user-group-name groups))))
    (define (validate-supplementary-group user group)
      (unless (set-contains? groups group)
        (raise (condition
                (&message
                 (message
                  (format #f (G_ "supplementary group '~a' \
of user '~a' is undeclared")
                          group
                          (user-account-name user))))))))

    (for-each (lambda (user)
                (unless (set-contains? groups (user-account-group user))
                  (raise (condition
                          (&message
                           (message
                            (format #f (G_ "primary group '~a' \
of user '~a' is undeclared")
                                    (user-account-group user)
                                    (user-account-name user)))))))

                (for-each (cut validate-supplementary-group user <>)
                          (user-account-supplementary-groups user)))
              users)))


;;;
;;; Service.
;;;

(define (user-group->gexp group)
  "Turn GROUP, a <user-group> object, into a list-valued gexp suitable for
'active-groups'."
  #~(list #$(user-group-name group)
          #$(user-group-password group)
          #$(user-group-id group)
          #$(user-group-system? group)))

(define (user-account->gexp account)
  "Turn ACCOUNT, a <user-account> object, into a list-valued gexp suitable for
'activate-users'."
  #~`(#$(user-account-name account)
      #$(user-account-uid account)
      #$(user-account-group account)
      #$(user-account-supplementary-groups account)
      #$(user-account-comment account)
      #$(user-account-home-directory account)
      #$(user-account-create-home-directory? account)
      ,#$(user-account-shell account)             ; this one is a gexp
      #$(user-account-password account)
      #$(user-account-system? account)))

(define (account-activation accounts+groups)
  "Return a gexp that activates ACCOUNTS+GROUPS, a list of <user-account> and
<user-group> objects.  Raise an error if a user account refers to a undefined
group."
  (define accounts
    (filter user-account? accounts+groups))

  (define user-specs
    (map user-account->gexp accounts))

  (define groups
    (filter user-group? accounts+groups))

  (define group-specs
    (map user-group->gexp groups))

  (assert-valid-users/groups accounts groups)

  ;; Add users and user groups.
  (with-imported-modules (source-module-closure '((gnu system accounts)))
    #~(begin
        (use-modules (gnu system accounts))

        (activate-users+groups (map sexp->user-account (list #$@user-specs))
                               (map sexp->user-group (list #$@group-specs))))))

(define (account-shepherd-service accounts+groups)
  "Return a Shepherd service that creates the home directories for the user
accounts among ACCOUNTS+GROUPS."
  (define accounts
    (filter user-account? accounts+groups))

  ;; Create home directories only once 'file-systems' is up.  This makes sure
  ;; they are created in the right place if /home lives on a separate
  ;; partition.
  ;;
  ;; XXX: We arrange for this service to stop right after it's done its job so
  ;; that 'guix system reconfigure' knows that it can reload it fearlessly
  ;; (and thus create new home directories).  The cost of this hack is that
  ;; there's a small window during which first-time logins could happen before
  ;; the home directory has been created.
  (list (shepherd-service
         (requirement '(file-systems))
         (provision '(user-homes))
         (one-shot? #t)
         (modules '((gnu build activation)
                    (gnu system accounts)))
         (start (with-imported-modules (source-module-closure
                                        '((gnu build activation)
                                          (gnu system accounts)))
                  #~(lambda ()
                      (activate-user-home
                       (map sexp->user-account
                            (list #$@(map user-account->gexp accounts))))
                      #t)))                       ;success
         (documentation "Create user home directories."))))

(define (shells-file shells)
  "Return a file-like object that builds a shell list for use as /etc/shells
based on SHELLS.  /etc/shells is used by xterm, polkit, and other programs."
  (computed-file "shells"
                 #~(begin
                     (use-modules (srfi srfi-1))

                     (define shells
                       (delete-duplicates (list #$@shells)))

                     (call-with-output-file #$output
                       (lambda (port)
                         (display "\
/bin/sh
/run/current-system/profile/bin/sh
/run/current-system/profile/bin/bash\n" port)
                         (for-each (lambda (shell)
                                     (display shell port)
                                     (newline port))
                                   shells))))))
(define (etc-files arguments)
  "Filter out among ARGUMENTS things corresponding to skeletons, and return
the /etc/skel directory for those."
  (let ((skels (filter pair? arguments))
        (users (filter user-account? arguments)))
    `(("skel" ,(skeleton-directory skels))
      ("shells" ,(shells-file (map user-account-shell users))))))

(define account-service-type
  (service-type (name 'account)

                ;; Concatenate <user-account>, <user-group>, and skeleton
                ;; lists.
                (compose concatenate)
                (extend append)

                (extensions
                 (list (service-extension activation-service-type
                                          account-activation)
                       (service-extension shepherd-root-service-type
                                          account-shepherd-service)
                       (service-extension etc-service-type
                                          etc-files)))))

(define (account-service accounts+groups skeletons)
  "Return a <service> that takes care of user accounts and user groups, with
ACCOUNTS+GROUPS as its initial list of accounts and groups."
  (service account-service-type
           (append skeletons accounts+groups)))

;;; shadow.scm ends here
