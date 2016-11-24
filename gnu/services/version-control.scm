;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@member.fsf.org>
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
  #:use-module (gnu system shadow)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages admin)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (git-daemon-service
            git-daemon-service-type
            git-daemon-configuration
            git-daemon-configuration?))

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
                             git-daemon-activation)))))

(define* (git-daemon-service #:key (config (git-daemon-configuration)))
  "Return a service that runs @command{git daemon}, a simple TCP server to
expose repositories over the Git protocol for annoymous access.

The optional @var{config} argument should be a
@code{<git-daemon-configuration>} object, by default it allows read-only
access to exported repositories under @file{/srv/git}."
  (service git-daemon-service-type config))
