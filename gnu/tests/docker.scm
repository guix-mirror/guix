;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
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
  #:use-module (guix gexp)
  #:use-module (guix grafts)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix scripts pack)
  #:use-module (guix store)
  #:use-module (guix tests)
  #:use-module (guix build-system trivial)
  #:export (%test-docker))

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
        (dummy-package "guest-script"
                       (build-system trivial-build-system)
                       (arguments
                        `(#:guile ,%bootstrap-guile
                          #:builder
                          (let ((out (assoc-ref %outputs "out")))
                            (mkdir out)
                            (call-with-output-file (string-append out "/a.scm")
                              (lambda (port)
                                (display "(display \"hello world\n\")" port)))
                            #t)))))
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
