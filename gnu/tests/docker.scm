;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu tests docker)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services docker)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages bootstrap) ; %bootstrap-guile
  #:use-module (gnu packages docker)
  #:use-module (gnu packages guile)
  #:use-module (guix gexp)
  #:use-module (guix grafts)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix scripts pack)
  #:use-module (guix store)
  #:use-module (guix tests)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (%test-docker
            %test-docker-system))

(define %docker-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (dbus-service)
   (polkit-service)
   (service elogind-service-type)
   (service docker-service-type)))

(define (run-docker-test docker-tarball)
  "Load DOCKER-TARBALL as Docker image and run it in a Docker container,
inside %DOCKER-OS."
  (define os
    (marionette-operating-system
     %docker-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 700)
     (disk-image-size (* 1500 (expt 2 20)))
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "docker")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'dockerd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-eq "fetch version"
            0
            (marionette-eval
             `(begin
                (system* ,(string-append #$docker-cli "/bin/docker")
                         "version"))
             marionette))

          (test-equal "Load docker image and run it"
            "hello world"
            (marionette-eval
             `(begin
                (define slurp
                  (lambda args
                    (let* ((port (apply open-pipe* OPEN_READ args))
                           (output (read-line port))
                           (status (close-pipe port)))
                      output)))
                (let* ((raw-line (slurp ,(string-append #$docker-cli
                                                        "/bin/docker")
                                                        "load" "-i"
                                                        ,#$docker-tarball))
                       (repository&tag (string-drop raw-line
                                                    (string-length
                                                     "Loaded image: ")))
                       (response (slurp
                                  ,(string-append #$docker-cli "/bin/docker")
                                  "run" "--entrypoint" "bin/Guile"
                                  repository&tag
                                  "/aa.scm")))
                  response))
             marionette))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "docker-test" test))

(define (build-tarball&run-docker-test)
  (mlet* %store-monad
      ((_ (set-grafting #f))
       (guile (set-guile-for-build (default-guile)))
       (guest-script-package ->
        (package
          (name "guest-script")
          (version "0")
          (source #f)
          (build-system trivial-build-system)
          (arguments `(#:guile ,%bootstrap-guile
                       #:builder
                       (let ((out (assoc-ref %outputs "out")))
                         (mkdir out)
                         (call-with-output-file (string-append out "/a.scm")
                           (lambda (port)
                             (display "(display \"hello world\n\")" port)))
                         #t)))
          (synopsis "Display hello world using Guile")
          (description "This package displays the text \"hello world\" on the
standard output device and then enters a new line.")
          (home-page #f)
          (license license:public-domain)))
       (profile (profile-derivation (packages->manifest
                                     (list %bootstrap-guile
                                           guest-script-package))
                                    #:hooks '()
                                    #:locales? #f))
       (tarball (docker-image "docker-pack" profile
                              #:symlinks '(("/bin/Guile" -> "bin/guile")
                                           ("aa.scm" -> "a.scm"))
                              #:localstatedir? #t)))
    (run-docker-test tarball)))

(define %test-docker
  (system-test
   (name "docker")
   (description "Test Docker container of Guix.")
   (value (build-tarball&run-docker-test))))


(define (run-docker-system-test tarball)
  "Load DOCKER-TARBALL as Docker image and run it in a Docker container,
inside %DOCKER-OS."
  (define os
    (marionette-operating-system
     %docker-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     ;; FIXME: Because we're using the volatile-root setup where the root file
     ;; system is a tmpfs overlaid over a small root file system, 'docker
     ;; load' must be able to store the whole image into memory, hence the
     ;; huge memory requirements.  We should avoid the volatile-root setup
     ;; instead.
     (memory-size 3000)
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build utils))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (guix build utils))

          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "docker")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'dockerd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-assert "load system image and run it"
            (marionette-eval
             `(begin
                (define (slurp command . args)
                  ;; Return the output from COMMAND.
                  (let* ((port (apply open-pipe* OPEN_READ command args))
                         (output (read-line port))
                         (status (close-pipe port)))
                    output))

                (define (docker-cli command . args)
                  ;; Run the given Docker COMMAND.
                  (apply invoke #$(file-append docker-cli "/bin/docker")
                         command args))

                (define (wait-for-container-file container file)
                  ;; Wait for FILE to show up in CONTAINER.
                  (docker-cli "exec" container
                              #$(file-append guile-2.2 "/bin/guile")
                              "-c"
                              (object->string
                               `(let loop ((n 15))
                                  (when (zero? n)
                                    (error "file didn't show up" ,file))
                                  (unless (file-exists? ,file)
                                    (sleep 1)
                                    (loop (- n 1)))))))

                (let* ((line (slurp #$(file-append docker-cli "/bin/docker")
                                    "load" "-i" #$tarball))
                       (repository&tag (string-drop line
                                                    (string-length
                                                     "Loaded image: ")))
                       (container (slurp
                                   #$(file-append docker-cli "/bin/docker")
                                   "create" repository&tag)))
                  (docker-cli "start" container)

                  ;; Wait for shepherd to be ready.
                  (wait-for-container-file container
                                           "/var/run/shepherd/socket")

                  (docker-cli "exec" container
                              "/run/current-system/profile/bin/herd"
                              "status")
                  (slurp #$(file-append docker-cli "/bin/docker")
                         "exec" container
                         "/run/current-system/profile/bin/herd"
                         "status" "guix-daemon")))
             marionette))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "docker-system-test" test))

(define %test-docker-system
  (system-test
   (name "docker-system")
   (description "Run a system image as produced by @command{guix system
docker-image} inside Docker.")
   (value (with-monad %store-monad
            (>>= (system-docker-image (simple-operating-system))
                 run-docker-system-test)))))
