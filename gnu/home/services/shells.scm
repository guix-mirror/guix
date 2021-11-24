;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
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

(define-module (gnu home services shells)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services utils)
  #:use-module (gnu home services)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages bash)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)

  #:export (home-shell-profile-service-type
            home-shell-profile-configuration

            home-bash-service-type
            home-bash-configuration
            home-bash-extension

            home-zsh-service-type
            home-zsh-configuration
            home-zsh-extension

            home-fish-service-type
            home-fish-configuration
            home-fish-extension))

;;; Commentary:
;;;
;;; This module contains shell related services like Zsh.
;;;
;;; Code:


;;;
;;; Shell profile.
;;;

(define path? string?)
(define (serialize-path field-name val) val)

(define-configuration home-shell-profile-configuration
  (profile
   (text-config '())
   "\
@code{home-shell-profile} is instantiated automatically by
@code{home-environment}, DO NOT create this service manually, it can
only be extended.

@code{profile} is a list of file-like objects, which will go to
@file{~/.profile}.  By default @file{~/.profile} contains the
initialization code, which have to be evaluated by login shell to make
home-environment's profile available to the user, but other commands
can be added to the file if it is really necessary.

In most cases shell's configuration files are preferred places for
user's customizations.  Extend home-shell-profile service only if you
really know what you do."))

(define (add-shell-profile-file config)
  `(("profile"
     ,(mixed-text-file
       "shell-profile"
       "\
HOME_ENVIRONMENT=$HOME/.guix-home
. $HOME_ENVIRONMENT/setup-environment
$HOME_ENVIRONMENT/on-first-login\n"
       (serialize-configuration
        config
        (filter-configuration-fields
         home-shell-profile-configuration-fields '(profile)))))))

(define (add-profile-extensions config extensions)
  (home-shell-profile-configuration
   (inherit config)
   (profile
    (append (home-shell-profile-configuration-profile config)
            extensions))))

(define home-shell-profile-service-type
  (service-type (name 'home-shell-profile)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        add-shell-profile-file)))
                (compose concatenate)
                (extend add-profile-extensions)
                (default-value (home-shell-profile-configuration))
                (description "Create @file{~/.profile}, which is used
for environment initialization of POSIX compliant login shells.  This
service type can be extended with a list of file-like objects.")))

(define (serialize-boolean field-name val) "")
(define (serialize-posix-env-vars field-name val)
  #~(string-append
     #$@(map
         (match-lambda
           ((key . #f)
            "")
           ((key . #t)
            #~(string-append "export " #$key "\n"))
           ((key . value)
            #~(string-append "export " #$key "=" #$value "\n")))
         val)))


;;;
;;; Zsh.
;;;

(define-configuration home-zsh-configuration
  (package
    (package zsh)
    "The Zsh package to use.")
  (xdg-flavor?
   (boolean #t)
   "Place all the configs to @file{$XDG_CONFIG_HOME/zsh}.  Makes
@file{~/.zshenv} to set @env{ZDOTDIR} to @file{$XDG_CONFIG_HOME/zsh}.
Shell startup process will continue with
@file{$XDG_CONFIG_HOME/zsh/.zshenv}.")
  (environment-variables
   (alist '())
   "Association list of environment variables to set for the Zsh session."
   serialize-posix-env-vars)
  (zshenv
   (text-config '())
   "List of file-like objects, which will be added to @file{.zshenv}.
Used for setting user's shell environment variables.  Must not contain
commands assuming the presence of tty or producing output.  Will be
read always.  Will be read before any other file in @env{ZDOTDIR}.")
  (zprofile
   (text-config '())
   "List of file-like objects, which will be added to @file{.zprofile}.
Used for executing user's commands at start of login shell (In most
cases the shell started on tty just after login).  Will be read before
@file{.zlogin}.")
  (zshrc
   (text-config '())
   "List of file-like objects, which will be added to @file{.zshrc}.
Used for executing user's commands at start of interactive shell (The
shell for interactive usage started by typing @code{zsh} or by
terminal app or any other program).")
  (zlogin
   (text-config '())
   "List of file-like objects, which will be added to @file{.zlogin}.
Used for executing user's commands at the end of starting process of
login shell.")
  (zlogout
   (text-config '())
   "List of file-like objects, which will be added to @file{.zlogout}.
Used for executing user's commands at the exit of login shell.  It
won't be read in some cases (if the shell terminates by exec'ing
another process for example)."))

(define (add-zsh-configuration config)
  (let* ((xdg-flavor? (home-zsh-configuration-xdg-flavor? config)))

    (define prefix-file
      (cut string-append
        (if xdg-flavor?
            "config/zsh/."
            "") <>))

    (define (filter-fields field)
      (filter-configuration-fields home-zsh-configuration-fields
                                   (list field)))

    (define (serialize-field field)
      (serialize-configuration
       config
       (filter-fields field)))

    (define (file-if-not-empty field)
      (let ((file-name (symbol->string field))
            (field-obj (car (filter-fields field))))
        (if (not (null? ((configuration-field-getter field-obj) config)))
            `(,(prefix-file file-name)
              ,(mixed-text-file
                file-name
                (serialize-field field)))
            '())))

    (filter
     (compose not null?)
     `(,(if xdg-flavor?
            `("zshenv"
              ,(mixed-text-file
                "auxiliary-zshenv"
                (if xdg-flavor?
                    "source ${XDG_CONFIG_HOME:-$HOME/.config}/zsh/.zshenv\n"
                    "")))
            '())
       (,(prefix-file "zshenv")
        ,(mixed-text-file
          "zshenv"
          (if xdg-flavor?
              "export ZDOTDIR=${XDG_CONFIG_HOME:-$HOME/.config}/zsh\n"
              "")
          (serialize-field 'zshenv)
          (serialize-field 'environment-variables)))
       (,(prefix-file "zprofile")
        ,(mixed-text-file
          "zprofile"
          "\
# Setups system and user profiles and related variables
source /etc/profile
# Setups home environment profile
source ~/.profile

# It's only necessary if zsh is a login shell, otherwise profiles will
# be already sourced by bash
"
          (serialize-field 'zprofile)))

       ,@(list (file-if-not-empty 'zshrc)
               (file-if-not-empty 'zlogin)
               (file-if-not-empty 'zlogout))))))

(define (add-zsh-packages config)
  (list (home-zsh-configuration-package config)))

(define-configuration/no-serialization home-zsh-extension
  (environment-variables
   (alist '())
   "Association list of environment variables to set.")
  (zshrc
   (text-config '())
   "List of file-like objects.")
  (zshenv
   (text-config '())
   "List of file-like objects.")
  (zprofile
   (text-config '())
   "List of file-like objects.")
  (zlogin
   (text-config '())
   "List of file-like objects.")
  (zlogout
   (text-config '())
   "List of file-like objects."))

(define (home-zsh-extensions original-config extension-configs)
  (home-zsh-configuration
   (inherit original-config)
   (environment-variables
    (append (home-zsh-configuration-environment-variables original-config)
            (append-map
             home-zsh-extension-environment-variables extension-configs)))
   (zshrc
    (append (home-zsh-configuration-zshrc original-config)
            (append-map
             home-zsh-extension-zshrc extension-configs)))
   (zshenv
    (append (home-zsh-configuration-zshenv original-config)
            (append-map
             home-zsh-extension-zshenv extension-configs)))
   (zprofile
    (append (home-zsh-configuration-zprofile original-config)
            (append-map
             home-zsh-extension-zprofile extension-configs)))
   (zlogin
    (append (home-zsh-configuration-zlogin original-config)
            (append-map
             home-zsh-extension-zlogin extension-configs)))
   (zlogout
    (append (home-zsh-configuration-zlogout original-config)
            (append-map
             home-zsh-extension-zlogout extension-configs)))))

(define home-zsh-service-type
  (service-type (name 'home-zsh)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        add-zsh-configuration)
                       (service-extension
                        home-profile-service-type
                        add-zsh-packages)))
                (compose identity)
                (extend home-zsh-extensions)
                (default-value (home-zsh-configuration))
                (description "Install and configure Zsh.")))


;;;
;;; Bash.
;;;

(define (bash-serialize-aliases field-name val)
  #~(string-append
     #$@(map
         (match-lambda
           ((key . #f)
            "")
           ((key . #t)
            #~(string-append "alias " #$key "\n"))
           ((key . value)
            #~(string-append "alias " #$key "=\"" #$value "\"\n")))
         val)))

(define-configuration home-bash-configuration
  (package
   (package bash)
   "The Bash package to use.")
  (guix-defaults?
   (boolean #t)
   "Add sane defaults like reading @file{/etc/bashrc} and coloring the output of
@command{ls} to the end of the @file{.bashrc} file.")
  (environment-variables
   (alist '())
   "Association list of environment variables to set for the Bash session.  The
rules for the @code{home-environment-variables-service-type} apply
here (@pxref{Essential Home Services}).  The contents of this field will be
added after the contents of the @code{bash-profile} field."
   serialize-posix-env-vars)
  (aliases
   (alist '())
   "Association list of aliases to set for the Bash session.  The aliases will be
defined after the contents of the @code{bashrc} field has been put in the
@file{.bashrc} file.  The alias will automatically be quoted, so something line
this:

@lisp
'((\"ls\" . \"ls -alF\"))
@end lisp

turns into

@example
alias ls=\"ls -alF\"
@end example"
   bash-serialize-aliases)
  (bash-profile
   (text-config '())
   "List of file-like objects, which will be added to @file{.bash_profile}.
Used for executing user's commands at start of login shell (In most
cases the shell started on tty just after login).  @file{.bash_login}
won't be ever read, because @file{.bash_profile} always present.")
  (bashrc
   (text-config '())
   "List of file-like objects, which will be added to @file{.bashrc}.
Used for executing user's commands at start of interactive shell (The
shell for interactive usage started by typing @code{bash} or by
terminal app or any other program).")
  (bash-logout
   (text-config '())
   "List of file-like objects, which will be added to @file{.bash_logout}.
Used for executing user's commands at the exit of login shell.  It
won't be read in some cases (if the shell terminates by exec'ing
another process for example)."))

;; TODO: Use value from (gnu system shadow)
(define guix-bashrc
  "\
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
if [[ -e /etc/bashrc ]]; then
    source /etc/bashrc
fi

# Adjust the prompt depending on whether we're in 'guix environment'.
if [ -n \"$GUIX_ENVIRONMENT\" ]
then
    PS1='\\u@\\h \\w [env]\\$ '
else
    PS1='\\u@\\h \\w\\$ '
fi
alias ls='ls -p --color=auto'
alias ll='ls -l'
alias grep='grep --color=auto'\n")

(define (add-bash-configuration config)
  (define (filter-fields field)
    (filter-configuration-fields home-bash-configuration-fields
                                 (list field)))

  (define (serialize-field field)
    (serialize-configuration
     config
     (filter-fields field)))

  (define* (file-if-not-empty field #:optional (extra-content #f))
    (let ((file-name (symbol->string field))
          (field-obj (car (filter-fields field))))
      (if (or extra-content
              (not (null? ((configuration-field-getter field-obj) config))))
          `(,(object->snake-case-string file-name)
            ,(apply mixed-text-file
                    (object->snake-case-string file-name)
                    (append (or extra-content '())
                        (list (serialize-field field)))))
          '())))

  (filter
   (compose not null?)
   `(("bash_profile"
      ,(mixed-text-file
        "bash_profile"
        "\
# Setups system and user profiles and related variables
# /etc/profile will be sourced by bash automatically
# Setups home environment profile
if [ -f ~/.profile ]; then source ~/.profile; fi

# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then source ~/.bashrc; fi
"
        (serialize-field 'bash-profile)
        (serialize-field 'environment-variables)))

     ,@(list (file-if-not-empty
              'bashrc
              (if (home-bash-configuration-guix-defaults? config)
                  (list (serialize-field 'aliases) guix-bashrc)
                  (list (serialize-field 'alises))))
             (file-if-not-empty 'bash-logout)))))

(define (add-bash-packages config)
  (list (home-bash-configuration-package config)))

(define-configuration/no-serialization home-bash-extension
  (environment-variables
   (alist '())
   "Additional environment variables to set.  These will be combined with the
environment variables from other extensions and the base service to form one
coherent block of environment variables.")
  (aliases
   (alist '())
   "Additional aliases to set.  These will be combined with the aliases from
other extensions and the base service.")
  (bash-profile
   (text-config '())
   "Additional text blocks to add to @file{.bash_profile}, which will be combined
with text blocks from other extensions and the base service.")
  (bashrc
   (text-config '())
   "Additional text blocks to add to @file{.bashrc}, which will be combined
with text blocks from other extensions and the base service.")
  (bash-logout
   (text-config '())
   "Additional text blocks to add to @file{.bash_logout}, which will be combined
with text blocks from other extensions and the base service."))

(define (home-bash-extensions original-config extension-configs)
  (match original-config
    (($ <home-bash-configuration> _ _ _ environment-variables aliases
                                  bash-profile bashrc bash-logout)
     (home-bash-configuration
      (inherit original-config)
      (environment-variables
       (append environment-variables
               (append-map
                home-bash-extension-environment-variables extension-configs)))
      (aliases
       (append aliases
               (append-map
                home-bash-extension-aliases extension-configs)))
      (bash-profile
       (append bash-profile
               (append-map
                home-bash-extension-bash-profile extension-configs)))
      (bashrc
       (append bashrc
               (append-map
                home-bash-extension-bashrc extension-configs)))
      (bash-logout
       (append bash-logout
               (append-map
                home-bash-extension-bash-logout extension-configs)))))))

(define home-bash-service-type
  (service-type (name 'home-bash)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        add-bash-configuration)
                       (service-extension
                        home-profile-service-type
                        add-bash-packages)))
                (compose identity)
                (extend home-bash-extensions)
                (default-value (home-bash-configuration))
                (description "Install and configure GNU Bash.")))


;;;
;;; Fish.
;;;

(define (serialize-fish-aliases field-name val)
  #~(string-append
     #$@(map (match-lambda
               ((key . value)
                #~(string-append "alias " #$key " \"" #$value "\"\n"))
               (_ ""))
             val)))

(define (serialize-fish-abbreviations field-name val)
  #~(string-append
     #$@(map (match-lambda
               ((key . value)
                #~(string-append "abbr --add " #$key " " #$value "\n"))
               (_ ""))
             val)))

(define (serialize-fish-env-vars field-name val)
  #~(string-append
     #$@(map (match-lambda
               ((key . #f)
                "")
               ((key . #t)
                #~(string-append "set " #$key "\n"))
               ((key . value)
                #~(string-append "set " #$key " "  #$value "\n")))
             val)))

(define-configuration home-fish-configuration
  (package
    (package fish)
    "The Fish package to use.")
  (config
   (text-config '())
   "List of file-like objects, which will be added to
@file{$XDG_CONFIG_HOME/fish/config.fish}.")
  (environment-variables
   (alist '())
   "Association list of environment variables to set in Fish."
   serialize-fish-env-vars)
  (aliases
   (alist '())
   "Association list of aliases for Fish, both the key and the value
should be a string.  An alias is just a simple function that wraps a
command, If you want something more akin to @dfn{aliases} in POSIX
shells, see the @code{abbreviations} field."
   serialize-fish-aliases)
  (abbreviations
   (alist '())
   "Association list of abbreviations for Fish.  These are words that,
when typed in the shell, will automatically expand to the full text."
   serialize-fish-abbreviations))

(define (fish-files-service config)
  `(("config/fish/config.fish"
     ,(mixed-text-file
       "fish-config.fish"
       #~(string-append "\
# if we haven't sourced the login config, do it
status --is-login; and not set -q __fish_login_config_sourced
and begin

  set --prepend fish_function_path "
                        #$fish-foreign-env
                        "/share/fish/functions
  fenv source $HOME/.profile
  set -e fish_function_path[1]

  set -g __fish_login_config_sourced 1

end\n\n")
       (serialize-configuration
        config
        home-fish-configuration-fields)))))

(define (fish-profile-service config)
  (list (home-fish-configuration-package config)))

(define-configuration/no-serialization home-fish-extension
  (config
   (text-config '())
   "List of file-like objects for extending the Fish initialization file.")
  (environment-variables
   (alist '())
   "Association list of environment variables to set.")
  (aliases
   (alist '())
   "Association list of Fish aliases.")
  (abbreviations
   (alist '())
   "Association list of Fish abbreviations."))

(define (home-fish-extensions original-config extension-configs)
  (home-fish-configuration
   (inherit original-config)
   (config
    (append (home-fish-configuration-config original-config)
            (append-map
             home-fish-extension-config extension-configs)))
   (environment-variables
    (append (home-fish-configuration-environment-variables original-config)
            (append-map
             home-fish-extension-environment-variables extension-configs)))
   (aliases
    (append (home-fish-configuration-aliases original-config)
            (append-map
             home-fish-extension-aliases extension-configs)))
   (abbreviations
    (append (home-fish-configuration-abbreviations original-config)
            (append-map
             home-fish-extension-abbreviations extension-configs)))))

;; TODO: Support for generating completion files
;; TODO: Support for installing plugins
(define home-fish-service-type
  (service-type (name 'home-fish)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        fish-files-service)
                       (service-extension
                        home-profile-service-type
                        fish-profile-service)))
                (compose identity)
                (extend home-fish-extensions)
                (default-value (home-fish-configuration))
                (description "\
Install and configure Fish, the friendly interactive shell.")))


(define (generate-home-shell-profile-documentation)
  (generate-documentation
   `((home-shell-profile-configuration
      ,home-shell-profile-configuration-fields))
   'home-shell-profile-configuration))

(define (generate-home-bash-documentation)
  (string-append
   (generate-documentation
    `((home-bash-configuration
       ,home-bash-configuration-fields))
    'home-bash-configuration)
   "\n\n"
   (generate-documentation
    `((home-bash-extension
       ,home-bash-extension-fields))
    'home-bash-extension)))

(define (generate-home-zsh-documentation)
  (generate-documentation
   `((home-zsh-configuration
      ,home-zsh-configuration-fields))
   'home-zsh-configuration))

(define (generate-home-fish-documentation)
  (string-append
   (generate-documentation
    `((home-fish-configuration
       ,home-fish-configuration-fields))
    'home-fish-configuration)
   "\n\n"
   (generate-documentation
    `((home-fish-extension
       ,home-fish-extension-fields))
    'home-fish-extension)))
