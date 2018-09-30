;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
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
  #:use-module (ice-9 match)
  #:export (git-daemon-service
            git-daemon-service-type
            git-daemon-configuration
            git-daemon-configuration?

            git-http-configuration
            git-http-configuration?
            git-http-nginx-location-configuration))

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
