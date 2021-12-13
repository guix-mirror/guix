;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2017, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
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

(define-module (gnu tests version-control)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services version-control)
  #:use-module (gnu services cgit)
  #:use-module (gnu services ssh)
  #:use-module (gnu services web)
  #:use-module (gnu services networking)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages ssh)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:export (%test-cgit
            %test-git-http
            %test-gitolite
            %test-gitile))

(define README-contents
  "Hello!  This is what goes inside the 'README' file.")

(define %make-git-repository
  ;; Create Git repository in /srv/git/test.
  (with-imported-modules (source-module-closure
                          '((guix build utils)))
    #~(begin
        (use-modules (guix build utils))

        (let ((git (string-append #$git "/bin/git")))
          (mkdir-p "/tmp/test-repo")
          (with-directory-excursion "/tmp/test-repo"
            (call-with-output-file "/tmp/test-repo/README"
              (lambda (port)
                (display #$README-contents port)))
            (invoke git "config" "--global" "user.email" "charlie@example.org")
            (invoke git "config" "--global" "user.name" "A U Thor")
            (invoke git "init")
            (invoke git "add" ".")
            (invoke git "commit" "-m" "That's a commit."))

          (mkdir-p "/srv/git")
          (rename-file "/tmp/test-repo/.git" "/srv/git/test")
          (with-output-to-file "/srv/git/test/git-daemon-export-ok"
            (lambda _
              (display "")))))))

(define %test-repository-service
  ;; Service that creates /srv/git/test.
  (simple-service 'make-git-repository activation-service-type
                  %make-git-repository))

(define %cgit-configuration-nginx
  (list
   (nginx-server-configuration
    (root cgit)
    (locations
     (list
      (nginx-location-configuration
       (uri "@cgit")
       (body '("fastcgi_param SCRIPT_FILENAME $document_root/lib/cgit/cgit.cgi;"
               "fastcgi_param PATH_INFO $uri;"
               "fastcgi_param QUERY_STRING $args;"
               "fastcgi_param HTTP_HOST $server_name;"
               "fastcgi_pass 127.0.0.1:9000;")))))
    (try-files (list "$uri" "@cgit"))
    (listen '("19418"))
    (ssl-certificate #f)
    (ssl-certificate-key #f))))

(define %cgit-os
  ;; Operating system under test.
  (let ((base-os
         (simple-operating-system
          (service dhcp-client-service-type)
          (service cgit-service-type
                   (cgit-configuration
                    (nginx %cgit-configuration-nginx)))
          %test-repository-service)))
    (operating-system
      (inherit base-os)
      (packages (cons* git
                       (operating-system-packages base-os))))))

(define* (run-cgit-test #:optional (http-port 19418))
  "Run tests in %CGIT-OS, which has nginx running and listening on
HTTP-PORT."
  (define os
    (marionette-operating-system
     %cgit-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((8080 . ,http-port)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (web uri)
                       (web client)
                       (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "cgit")

          ;; XXX: Shepherd reads the config file *before* binding its control
          ;; socket, so /var/run/shepherd/socket might not exist yet when the
          ;; 'marionette' service is started.
          (test-assert "shepherd socket ready"
            (marionette-eval
             `(begin
                (use-modules (gnu services herd))
                (let loop ((i 10))
                  (cond ((file-exists? (%shepherd-socket-file))
                         #t)
                        ((> i 0)
                         (sleep 1)
                         (loop (- i 1)))
                        (else
                         'failure))))
             marionette))

          ;; Wait for nginx to be up and running.
          (test-assert "nginx running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'nginx))
             marionette))

          ;; Wait for fcgiwrap to be up and running.
          (test-assert "fcgiwrap running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'fcgiwrap))
             marionette))

          ;; Make sure the PID file is created.
          (test-assert "PID file"
            (marionette-eval
             '(file-exists? "/var/run/nginx/pid")
             marionette))

          ;; Make sure the configuration file is created.
          (test-assert "configuration file"
            (marionette-eval
             '(file-exists? "/etc/cgitrc")
             marionette))

          ;; Make sure Git test repository is created.
          (test-assert "Git test repository"
            (marionette-eval
             '(file-exists? "/srv/git/test")
             marionette))

          ;; Make sure we can access pages that correspond to our repository.
          (letrec-syntax ((test-url
                           (syntax-rules ()
                             ((_ path code)
                              (test-equal (string-append "GET " path)
                                code
                                (let-values (((response body)
                                              (http-get (string-append
                                                         "http://localhost:8080"
                                                         path))))
                                  (response-code response))))
                             ((_ path)
                              (test-url path 200)))))
            (test-url "/")
            (test-url "/test")
            (test-url "/test/log")
            (test-url "/test/tree")
            (test-url "/test/tree/README")
            (test-url "/test/does-not-exist" 404)
            (test-url "/test/tree/does-not-exist" 404)
            (test-url "/does-not-exist" 404))

          (test-end))))

  (gexp->derivation "cgit-test" test))

(define %test-cgit
  (system-test
   (name "cgit")
   (description "Connect to a running Cgit server.")
   (value (run-cgit-test))))


;;;
;;; Git server.
;;;

(define %git-nginx-configuration
  (nginx-configuration
   (server-blocks
    (list
     (nginx-server-configuration
      (listen '("19418"))
      (ssl-certificate #f)
      (ssl-certificate-key #f)
      (locations
       (list (git-http-nginx-location-configuration
              (git-http-configuration (export-all? #t)
                                      (uri-path "/git"))))))))))

(define %git-http-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service fcgiwrap-service-type)
   (service nginx-service-type %git-nginx-configuration)
   %test-repository-service))

(define* (run-git-http-test #:optional (http-port 19418))
  (define os
    (marionette-operating-system
     %git-http-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((8080 . ,http-port)))))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build utils))
      #~(begin
          (use-modules (srfi srfi-64)
                       (rnrs io ports)
                       (gnu build marionette)
                       (guix build utils))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "git-http")

          ;; Wait for nginx to be up and running.
          (test-assert "nginx running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'nginx))
             marionette))

          ;; Make sure Git test repository is created.
          (test-assert "Git test repository"
            (marionette-eval
             '(file-exists? "/srv/git/test")
             marionette))

          (test-assert "fcgiwrap listens"
            ;; Wait for fcgiwrap to be ready before cloning.
            (wait-for-tcp-port 9000 marionette))

          ;; Make sure we can clone the repo from the host.
          (test-equal "clone"
            '#$README-contents
            (begin
              (invoke #$(file-append git "/bin/git") "clone" "-v"
                      "http://localhost:8080/git/test" "/tmp/clone")
              (call-with-input-file "/tmp/clone/README"
                get-string-all)))

          (test-end))))

  (gexp->derivation "git-http" test))

(define %test-git-http
  (system-test
   (name "git-http")
   (description "Connect to a running Git HTTP server.")
   (value (run-git-http-test))))


;;;
;;; Gitolite.
;;;

(define %gitolite-test-admin-keypair
  (computed-file
   "gitolite-test-admin-keypair"
   (with-imported-modules (source-module-closure
                           '((guix build utils)))
     #~(begin
         (use-modules (ice-9 match) (srfi srfi-26)
                      (guix build utils))

         (mkdir #$output)
         (invoke #$(file-append openssh "/bin/ssh-keygen")
                 "-f" (string-append #$output "/test-admin")
                 "-t" "rsa"
                 "-q"
                 "-N" "")))))

(define %gitolite-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service openssh-service-type)
   (service gitolite-service-type
            (gitolite-configuration
             (admin-pubkey
              (file-append %gitolite-test-admin-keypair "/test-admin.pub"))))))

(define (run-gitolite-test)
  (define os
    (marionette-operating-system
     %gitolite-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((2222 . 22)))))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build utils))
      #~(begin
          (use-modules (srfi srfi-64)
                       (rnrs io ports)
                       (gnu build marionette)
                       (guix build utils))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "gitolite")

          ;; Wait for sshd to be up and running.
          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'ssh-daemon))
             marionette))

          (display #$%gitolite-test-admin-keypair)

          (setenv "GIT_SSH_VARIANT" "ssh")
          (setenv "GIT_SSH_COMMAND"
                  (string-join
                   '(#$(file-append openssh "/bin/ssh")
                     "-i" #$(file-append %gitolite-test-admin-keypair
                                         "/test-admin")
                     "-o" "UserKnownHostsFile=/dev/null"
                     "-o" "StrictHostKeyChecking=no")))

          (test-assert "cloning the admin repository"
            (invoke #$(file-append git "/bin/git")
                    "clone" "-v"
                    "ssh://git@localhost:2222/gitolite-admin"
                    "/tmp/clone"))

          (test-assert "admin key exists"
            (file-exists? "/tmp/clone/keydir/test-admin.pub"))

          (with-directory-excursion "/tmp/clone"
            (invoke #$(file-append git "/bin/git")
                    "-c" "user.name=Guix" "-c" "user.email=guix"
                    "commit"
                    "-m" "Test commit"
                    "--allow-empty")

            (test-assert "pushing, and the associated hooks"
              (invoke #$(file-append git "/bin/git") "push")))

          (test-end))))

  (gexp->derivation "gitolite" test))

(define %test-gitolite
  (system-test
   (name "gitolite")
   (description "Clone the Gitolite admin repository.")
   (value (run-gitolite-test))))

;;;
;;; Gitile.
;;;

(define %gitile-configuration-nginx
  (nginx-server-configuration
   (root "/does/not/exists")
   (try-files (list "$uri" "=404"))
   (listen '("19418"))
   (ssl-certificate #f)
   (ssl-certificate-key #f)))

(define %gitile-os
  ;; Operating system under test.
  (simple-operating-system
   (service dhcp-client-service-type)
   (simple-service 'srv-git activation-service-type
                   #~(mkdir-p "/srv/git"))
   (service gitile-service-type
            (gitile-configuration
             (base-git-url "http://localhost")
             (repositories "/srv/git")
             (nginx %gitile-configuration-nginx)))
   %test-repository-service))

(define* (run-gitile-test #:optional (http-port 19418))
  "Run tests in %GITOLITE-OS, which has nginx running and listening on
HTTP-PORT."
  (define os
    (marionette-operating-system
     %gitile-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((8081 . ,http-port)))
     (memory-size 1024)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (web uri)
                       (web client)
                       (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "gitile")

          ;; XXX: Shepherd reads the config file *before* binding its control
          ;; socket, so /var/run/shepherd/socket might not exist yet when the
          ;; 'marionette' service is started.
          (test-assert "shepherd socket ready"
            (marionette-eval
             `(begin
                (use-modules (gnu services herd))
                (let loop ((i 10))
                  (cond ((file-exists? (%shepherd-socket-file))
                         #t)
                        ((> i 0)
                         (sleep 1)
                         (loop (- i 1)))
                        (else
                         'failure))))
             marionette))

          ;; Wait for nginx to be up and running.
          (test-assert "nginx running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'nginx))
             marionette))

          ;; Make sure the PID file is created.
          (test-assert "PID file"
            (marionette-eval
             '(file-exists? "/var/run/nginx/pid")
             marionette))

          ;; Make sure Git test repository is created.
          (test-assert "Git test repository"
            (marionette-eval
             '(file-exists? "/srv/git/test")
             marionette))

          (sleep 2)

          ;; Make sure we can access pages that correspond to our repository.
          (letrec-syntax ((test-url
                           (syntax-rules ()
                             ((_ path code)
                              (test-equal (string-append "GET " path)
                                code
                                (let-values (((response body)
                                              (http-get (string-append
                                                         "http://localhost:8081"
                                                         path))))
                                  (response-code response))))
                             ((_ path)
                              (test-url path 200)))))
            (test-url "/")
            (test-url "/css/gitile.css")
            (test-url "/test")
            (test-url "/test/commits")
            (test-url "/test/tree" 404)
            (test-url "/test/tree/-")
            (test-url "/test/tree/-/README")
            (test-url "/test/does-not-exist" 404)
            (test-url "/test/tree/-/does-not-exist" 404)
            (test-url "/does-not-exist" 404))

          (test-end))))

  (gexp->derivation "gitile-test" test))

(define %test-gitile
  (system-test
   (name "gitile")
   (description "Connect to a running Gitile server.")
   (value (run-gitile-test))))
