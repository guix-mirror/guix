;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 ng0 <ng0@n0.is>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
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

(define-module (gnu services version-control)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages admin)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (git-daemon-service
            git-daemon-service-type
            git-daemon-configuration
            git-daemon-configuration?

            git-http-configuration
            git-http-configuration?
            git-http-nginx-location-configuration

            <gitolite-configuration>
            gitolite-configuration
            gitolite-configuration-package
            gitolite-configuration-user
            gitolite-configuration-rc-file
            gitolite-configuration-admin-pubkey

            <gitolite-rc-file>
            gitolite-rc-file
            gitolite-rc-file-umask
            gitolite-rc-file-git-config-keys
            gitolite-rc-file-roles
            gitolite-rc-file-enable

            gitolite-service-type))

;;; Commentary:
;;;
;;; Version Control related services.
;;;
;;; Code:


;;;
;;; Git daemon.
;;;

(define-record-type* <git-daemon-configuration>
  git-daemon-configuration
  make-git-daemon-configuration
  git-daemon-configuration?
  (package          git-daemon-configuration-package        ;package
                    (default git))
  (export-all?      git-daemon-configuration-export-all     ;boolean
                    (default #f))
  (base-path        git-daemon-configuration-base-path      ;string | #f
                    (default "/srv/git"))
  (user-path        git-daemon-configuration-user-path      ;string | #f
                    (default #f))
  (listen           git-daemon-configuration-listen         ;list of string
                    (default '()))
  (port             git-daemon-configuration-port           ;number | #f
                    (default #f))
  (whitelist        git-daemon-configuration-whitelist      ;list of string
                    (default '()))
  (extra-options    git-daemon-configuration-extra-options  ;list of string
                    (default '())))

(define git-daemon-shepherd-service
  (match-lambda
    (($ <git-daemon-configuration>
        package export-all? base-path user-path
        listen port whitelist extra-options)
     (let* ((git     (file-append package "/bin/git"))
            (command `(,git
                       "daemon" "--syslog" "--reuseaddr"
                       ,@(if export-all?
                             '("--export-all")
                             '())
                       ,@(if base-path
                             `(,(string-append "--base-path=" base-path))
                             '())
                       ,@(if user-path
                             `(,(string-append "--user-path=" user-path))
                             '())
                       ,@(map (cut string-append "--listen=" <>) listen)
                       ,@(if port
                             `(,(string-append
                                 "--port=" (number->string port)))
                             '())
                       ,@extra-options
                       ,@whitelist)))
       (list (shepherd-service
              (documentation "Run the git-daemon.")
              (requirement '(networking))
              (provision '(git-daemon))
              (start #~(make-forkexec-constructor '#$command
                                                  #:user "git-daemon"
                                                  #:group "git-daemon"))
              (stop #~(make-kill-destructor))))))))

(define %git-daemon-accounts
  ;; User account and group for git-daemon.
  (list (user-group
         (name "git-daemon")
         (system? #t))
        (user-account
         (name "git-daemon")
         (system? #t)
         (group "git-daemon")
         (comment "Git daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (git-daemon-activation config)
  "Return the activation gexp for git-daemon using CONFIG."
  (let ((base-path (git-daemon-configuration-base-path config)))
    #~(begin
        (use-modules (guix build utils))
        ;; Create the 'base-path' directory when it's not '#f'.
        (and=> #$base-path mkdir-p))))

(define git-daemon-service-type
  (service-type
   (name 'git-daemon)
   (extensions
    (list (service-extension shepherd-root-service-type
                             git-daemon-shepherd-service)
          (service-extension account-service-type
                             (const %git-daemon-accounts))
          (service-extension activation-service-type
                             git-daemon-activation)))
   (description
    "Expose Git respositories over the insecure @code{git://} TCP-based
protocol.")
   (default-value (git-daemon-configuration))))

(define* (git-daemon-service #:key (config (git-daemon-configuration)))
  "Return a service that runs @command{git daemon}, a simple TCP server to
expose repositories over the Git protocol for annoymous access.

The optional @var{config} argument should be a
@code{<git-daemon-configuration>} object, by default it allows read-only
access to exported repositories under @file{/srv/git}."
  (service git-daemon-service-type config))


;;;
;;; HTTP access.  Add the result of calling
;;; git-http-nginx-location-configuration to an nginx-server-configuration's
;;; "locations" field.
;;;

(define-record-type* <git-http-configuration>
  git-http-configuration
  make-git-http-configuration
  git-http-configuration?
  (package          git-http-configuration-package        ;package
                    (default git))
  (git-root         git-http-configuration-git-root       ;string
                    (default "/srv/git"))
  (export-all?      git-http-configuration-export-all?    ;boolean
                    (default #f))
  (uri-path         git-http-configuration-uri-path       ;string
                    (default "/git/"))
  (fcgiwrap-socket  git-http-configuration-fcgiwrap-socket ;string
                    (default "127.0.0.1:9000")))

(define* (git-http-nginx-location-configuration #:optional
                                                (config
                                                 (git-http-configuration)))
  (match config
    (($ <git-http-configuration> package git-root export-all?
                                 uri-path fcgiwrap-socket)
     (nginx-location-configuration
      (uri (string-append "~ /" (string-trim-both uri-path #\/) "(/.*)"))
      (body
       (list
        (list "fastcgi_pass " fcgiwrap-socket ";")
        (list "fastcgi_param SCRIPT_FILENAME "
              package "/libexec/git-core/git-http-backend"
              ";")
        "fastcgi_param QUERY_STRING $query_string;"
        "fastcgi_param REQUEST_METHOD $request_method;"
        "fastcgi_param CONTENT_TYPE $content_type;"
        "fastcgi_param CONTENT_LENGTH $content_length;"
        (if export-all?
            "fastcgi_param GIT_HTTP_EXPORT_ALL \"\";"
            "")
        (list "fastcgi_param GIT_PROJECT_ROOT " git-root ";")
        "fastcgi_param PATH_INFO $1;"))))))


;;;
;;; Gitolite
;;;

(define-record-type* <gitolite-rc-file>
  gitolite-rc-file make-gitolite-rc-file
  gitolite-rc-file?
  (umask           gitolite-rc-file-umask
                   (default #o0077))
  (git-config-keys gitolite-rc-file-git-config-keys
                   (default ""))
  (roles           gitolite-rc-file-roles
                   (default '(("READERS" . 1)
                              ("WRITERS" . 1))))
  (enable          gitolite-rc-file-enable
                   (default '("help"
                              "desc"
                              "info"
                              "perms"
                              "writable"
                              "ssh-authkeys"
                              "git-config"
                              "daemon"
                              "gitweb"))))

(define-gexp-compiler (gitolite-rc-file-compiler
                       (file <gitolite-rc-file>) system target)
  (match file
    (($ <gitolite-rc-file> umask git-config-keys roles enable)
     (apply text-file* "gitolite.rc"
      `("%RC = (\n"
        "    UMASK => " ,(format #f "~4,'0o" umask) ",\n"
        "    GIT_CONFIG_KEYS => '" ,git-config-keys "',\n"
        "    ROLES => {\n"
        ,@(map (match-lambda
                 ((role . value)
                  (simple-format #f "        ~A => ~A,\n" role value)))
               roles)
        "    },\n"
        "\n"
        "    ENABLE => [\n"
        ,@(map (lambda (value)
                 (simple-format #f "        '~A',\n" value))
               enable)
        "    ],\n"
        ");\n"
        "\n"
        "1;\n")))))

(define-record-type* <gitolite-configuration>
  gitolite-configuration make-gitolite-configuration
  gitolite-configuration?
  (package        gitolite-configuration-package
                  (default gitolite))
  (user           gitolite-configuration-user
                  (default "git"))
  (group          gitolite-configuration-group
                  (default "git"))
  (home-directory gitolite-configuration-home-directory
                  (default "/var/lib/gitolite"))
  (rc-file        gitolite-configuration-rc-file
                  (default (gitolite-rc-file)))
  (admin-pubkey   gitolite-configuration-admin-pubkey))

(define gitolite-accounts
  (match-lambda
    (($ <gitolite-configuration> package user group home-directory
                                 rc-file admin-pubkey)
     ;; User group and account to run Gitolite.
     (list (user-group (name user) (system? #t))
           (user-account
            (name user)
            (group group)
            (system? #t)
            (comment "Gitolite user")
            (home-directory home-directory))))))

(define gitolite-activation
  (match-lambda
    (($ <gitolite-configuration> package user group home
                                 rc-file admin-pubkey)
     #~(begin
         (use-modules (ice-9 match)
                      (guix build utils))

         (let* ((user-info (getpwnam #$user))
                (admin-pubkey #$admin-pubkey)
                (pubkey-file (string-append
                              #$home "/"
                              (basename
                               (strip-store-file-name admin-pubkey)))))

           (simple-format #t "guix: gitolite: installing ~A\n" #$rc-file)
           (copy-file #$rc-file #$(string-append home "/.gitolite.rc"))

           ;; The key must be writable, so copy it from the store
           (copy-file admin-pubkey pubkey-file)

           (chmod pubkey-file #o500)
           (chown pubkey-file
                  (passwd:uid user-info)
                  (passwd:gid user-info))

           ;; Set the git configuration, to avoid gitolite trying to use
           ;; the hostname command, as the network might not be up yet
           (with-output-to-file #$(string-append home "/.gitconfig")
             (lambda ()
               (display "[user]
        name = GNU Guix
        email = guix@localhost
")))
           ;; Run Gitolite setup, as this updates the hooks and include the
           ;; admin pubkey if specified. The admin pubkey is required for
           ;; initial setup, and will replace the previous key if run after
           ;; initial setup
           (match (primitive-fork)
             (0
              ;; Exit with a non-zero status code if an exception is thrown.
              (dynamic-wind
                (const #t)
                (lambda ()
                  (setenv "HOME" (passwd:dir user-info))
                  (setenv "USER" #$user)
                  (setgid (passwd:gid user-info))
                  (setuid (passwd:uid user-info))
                  (primitive-exit
                   (system* #$(file-append package "/bin/gitolite")
                            "setup"
                            "-m" "gitolite setup by GNU Guix"
                            "-pk" pubkey-file)))
                (lambda ()
                  (primitive-exit 1))))
             (pid (waitpid pid)))

           (when (file-exists? pubkey-file)
             (delete-file pubkey-file)))))))

(define gitolite-service-type
  (service-type
   (name 'gitolite)
   (extensions
    (list (service-extension activation-service-type
                             gitolite-activation)
          (service-extension account-service-type
                             gitolite-accounts)
          (service-extension profile-service-type
                             ;; The Gitolite package in Guix uses
                             ;; gitolite-shell in the authorized_keys file, so
                             ;; gitolite-shell needs to be on the PATH for
                             ;; gitolite to work.
                             (lambda (config)
                               (list
                                (gitolite-configuration-package config))))))
   (description
    "Setup @command{gitolite}, a Git hosting tool providing access over SSH..
By default, the @code{git} user is used, but this is configurable.
Additionally, Gitolite can integrate with with tools like gitweb or cgit to
provide a web interface to view selected repositories.")))
