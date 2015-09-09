;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu services databases)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:export (postgresql-service))

;;; Commentary:
;;;
;;; Database services.
;;;
;;; Code:

(define %default-postgres-hba
  (plain-file "pg_hba.conf"
              "
local	all	all			trust
host	all	all	127.0.0.1/32 	trust
host	all	all	::1/128 	trust"))

(define %default-postgres-ident
  (plain-file "pg_ident.conf"
             "# MAPNAME       SYSTEM-USERNAME         PG-USERNAME"))

(define %default-postgres-config
  (mixed-text-file "postgresql.conf"
                   "hba_file = '" %default-postgres-hba "'\n"
                   "ident_file = '" %default-postgres-ident "\n"))

(define* (postgresql-service #:key (postgresql postgresql)
                             (config-file %default-postgres-config)
                             (data-directory "/var/lib/postgresql/data"))
  "Return a service that runs @var{postgresql}, the PostgreSQL database server.

The PostgreSQL daemon loads its runtime configuration from @var{config-file}
and stores the database cluster in @var{data-directory}."
  ;; Wrapper script that switches to the 'postgres' user before launching
  ;; daemon.
  (define start-script
    (program-file "start-postgres"
                  #~(let ((user (getpwnam "postgres"))
                          (postgres (string-append #$postgresql
                                                   "/bin/postgres")))
                      (setgid (passwd:gid user))
                      (setuid (passwd:uid user))
                      (system* postgres
                               (string-append "--config-file=" #$config-file)
                               "-D" #$data-directory))))

  (define activate
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 match))

        (let ((user (getpwnam "postgres"))
              (initdb (string-append #$postgresql "/bin/initdb")))
          ;; Create db state directory.
          (mkdir-p #$data-directory)
          (chown #$data-directory (passwd:uid user) (passwd:gid user))

          ;; Drop privileges and init state directory in a new
          ;; process.  Wait for it to finish before proceeding.
          (match (primitive-fork)
            (0
             ;; Exit with a non-zero status code if an exception is thrown.
             (dynamic-wind
               (const #t)
               (lambda ()
                 (setgid (passwd:gid user))
                 (setuid (passwd:uid user))
                 (primitive-exit (system* initdb "-D" #$data-directory)))
               (lambda ()
                 (primitive-exit 1))))
            (pid (waitpid pid))))))

  (service
   (provision '(postgres))
   (documentation "Run the PostgreSQL daemon.")
   (requirement '(user-processes loopback))
   (start #~(make-forkexec-constructor #$start-script))
   (stop #~(make-kill-destructor))
   (activate activate)
   (user-groups (list (user-group
                       (name "postgres")
                       (system? #t))))
   (user-accounts (list (user-account
                         (name "postgres")
                         (group "postgres")
                         (system? #t)
                         (comment "PostgreSQL server user")
                         (home-directory "/var/empty")
                         (shell
                          #~(string-append #$shadow "/sbin/nologin")))))))
