;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu tests singularity)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu system shadow)
  #:use-module (gnu services)
  #:use-module (gnu services docker)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)               ;singularity
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix grafts)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix scripts pack)
  #:export (%test-singularity))

(define %singularity-os
  (simple-operating-system
   (service singularity-service-type)
   (simple-service 'guest-account
                   account-service-type
                   (list (user-account (name "guest") (uid 1000) (group "guest"))
                         (user-group (name "guest") (id 1000))))))

(define (run-singularity-test image)
  "Load IMAGE, a Squashfs image, as a Singularity image and run it inside
%SINGULARITY-OS."
  (define os
    (marionette-operating-system %singularity-os))

  (define singularity-exec
    #~(begin
        (use-modules (ice-9 popen) (rnrs io ports))

        (let* ((pipe (open-pipe* OPEN_READ
                                 #$(file-append singularity
                                                "/bin/singularity")
                                 "exec" #$image "/bin/guile"
                                 "-c" "(display \"hello, world\")"))
               (str  (get-string-all pipe))
               (status (close-pipe pipe)))
          (and (zero? status)
               (string=? str "hello, world")))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (test-runner-current (system-test-runner #$output))
          (test-begin "singularity")

          (test-assert "singularity exec /bin/guile (as root)"
            (marionette-eval '#$singularity-exec
                             marionette))

          (test-equal "singularity exec /bin/guile (unprivileged)"
            0
            (marionette-eval
             `(begin
                (use-modules (ice-9 match))

                (match (primitive-fork)
                  (0
                   (dynamic-wind
                     (const #f)
                     (lambda ()
                       (setgid 1000)
                       (setuid 1000)
                       (execl #$(program-file "singularity-exec-test"
                                              #~(exit #$singularity-exec))
                              "test"))
                     (lambda ()
                       (primitive-exit 127))))
                  (pid
                   (cdr (waitpid pid)))))
             marionette))

          (test-equal "singularity run"           ;test the entry point
            42
            (marionette-eval
             `(status:exit-val
               (system* #$(file-append singularity "/bin/singularity")
                        "run" #$image "-c" "(exit 42)"))
             marionette))

          ;; FIXME: Singularity 2.x doesn't directly honor
          ;; /.singularity.d/env/*.sh.  Instead, you have to load those files
          ;; manually, which we don't do.  Remove 'test-skip' call once we've
          ;; switch to Singularity 3.x.
          (test-skip 1)
          (test-equal "singularity run, with environment"
            0
            (marionette-eval
             ;; Check whether GUILE_LOAD_PATH is properly set, allowing us to
             ;; find the (json) module.
             `(status:exit-val
               (system* #$(file-append singularity "/bin/singularity")
                        "--debug" "run" #$image "-c" "(use-modules (json))"))
             marionette))

          (test-end))))

  (gexp->derivation "singularity-test" test))

(define (build-tarball&run-singularity-test)
  (mlet* %store-monad
      ((_        (set-grafting #f))
       (guile    (set-guile-for-build (default-guile)))
       ;; 'singularity exec' insists on having /bin/sh in the image.
       (profile  (profile-derivation (packages->manifest
                                      (list bash-minimal
                                            guile-2.2 guile-json-3))
                                     #:hooks '()
                                     #:locales? #f))
       (tarball  (squashfs-image "singularity-pack" profile
                                 #:entry-point "bin/guile"
                                 #:symlinks '(("/bin" -> "bin")))))
    (run-singularity-test tarball)))

(define %test-singularity
  (system-test
   (name "singularity")
   (description "Test Singularity container of Guix.")
   (value (build-tarball&run-singularity-test))))
