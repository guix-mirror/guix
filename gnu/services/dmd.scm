;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu services dmd)
  #:use-module (guix ui)
  #:use-module (guix sets)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix derivations)                 ;imported-modules, etc.
  #:use-module (gnu services)
  #:use-module (gnu packages admin)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (dmd-root-service-type
            %dmd-root-service
            dmd-service-type

            dmd-service
            dmd-service?
            dmd-service-documentation
            dmd-service-provision
            dmd-service-requirement
            dmd-service-respawn?
            dmd-service-start
            dmd-service-stop
            dmd-service-auto-start?
            dmd-service-modules
            dmd-service-imported-modules

            %default-imported-modules
            %default-modules

            dmd-service-back-edges))

;;; Commentary:
;;;
;;; Instantiating system services as a dmd configuration file.
;;;
;;; Code:


(define (dmd-boot-gexp services)
  (mlet %store-monad ((dmd-conf (dmd-configuration-file services)))
    (return #~(begin
                ;; Keep track of the booted system.
                (false-if-exception (delete-file "/run/booted-system"))
                (symlink (readlink "/run/current-system")
                         "/run/booted-system")

                ;; Close any remaining open file descriptors to be on the safe
                ;; side.  This must be the very last thing we do, because
                ;; Guile has internal FDs such as 'sleep_pipe' that need to be
                ;; alive.
                (let loop ((fd 3))
                  (when (< fd 1024)
                    (false-if-exception (close-fdes fd))
                    (loop (+ 1 fd))))

                ;; Start dmd.
                (execl (string-append #$dmd "/bin/dmd")
                       "dmd" "--config" #$dmd-conf)))))

(define dmd-root-service-type
  (service-type
   (name 'dmd-root)
   ;; Extending the root dmd service (aka. PID 1) happens by concatenating the
   ;; list of services provided by the extensions.
   (compose concatenate)
   (extend append)
   (extensions (list (service-extension boot-service-type dmd-boot-gexp)))))

(define %dmd-root-service
  ;; The root dmd service, aka. PID 1.  Its parameter is a list of
  ;; <dmd-service> objects.
  (service dmd-root-service-type '()))

(define-syntax-rule (dmd-service-type service-name proc)
  "Return a <service-type> denoting a simple dmd service--i.e., the type for a
service that extends DMD-ROOT-SERVICE-TYPE and nothing else."
  (service-type
   (name service-name)
   (extensions
    (list (service-extension dmd-root-service-type
                             (compose list proc))))))

(define %default-imported-modules
  ;; Default set of modules imported for a service's consumption.
  '((guix build utils)
    (guix build syscalls)))

(define %default-modules
  ;; Default set of modules visible in a service's file.
  `((dmd service)
    (oop goops)
    (guix build utils)
    (guix build syscalls)))

(define-record-type* <dmd-service>
  dmd-service make-dmd-service
  dmd-service?
  (documentation dmd-service-documentation        ;string
                 (default "[No documentation.]"))
  (provision     dmd-service-provision)           ;list of symbols
  (requirement   dmd-service-requirement          ;list of symbols
                 (default '()))
  (respawn?      dmd-service-respawn?             ;Boolean
                 (default #t))
  (start         dmd-service-start)               ;g-expression (procedure)
  (stop          dmd-service-stop                 ;g-expression (procedure)
                 (default #~(const #f)))
  (auto-start?   dmd-service-auto-start?          ;Boolean
                 (default #t))
  (modules       dmd-service-modules              ;list of module names
                 (default %default-modules))
  (imported-modules dmd-service-imported-modules  ;list of module names
                    (default %default-imported-modules)))


(define (assert-valid-graph services)
  "Raise an error if SERVICES does not define a valid dmd service graph, for
instance if a service requires a nonexistent service, or if more than one
service uses a given name.

These are constraints that dmd's 'register-service' verifies but we'd better
verify them here statically than wait until PID 1 halts with an assertion
failure."
  (define provisions
    ;; The set of provisions (symbols).  Bail out if a symbol is given more
    ;; than once.
    (fold (lambda (service set)
            (define (assert-unique symbol)
              (when (set-contains? set symbol)
                (raise (condition
                        (&message
                         (message
                          (format #f (_ "service '~a' provided more than once")
                                  symbol)))))))

            (for-each assert-unique (dmd-service-provision service))
            (fold set-insert set (dmd-service-provision service)))
          (setq 'dmd)
          services))

  (define (assert-satisfied-requirements service)
    ;; Bail out if the requirements of SERVICE aren't satisfied.
    (for-each (lambda (requirement)
                (unless (set-contains? provisions requirement)
                  (raise (condition
                          (&message
                           (message
                            (format #f (_ "service '~a' requires '~a', \
which is undefined")
                                    (match (dmd-service-provision service)
                                      ((head . _) head)
                                      (_          service))
                                    requirement)))))))
              (dmd-service-requirement service)))

  (for-each assert-satisfied-requirements services))

(define (dmd-service-file-name service)
  "Return the file name where the initialization code for SERVICE is to be
stored."
  (let ((provisions (string-join (map symbol->string
                                      (dmd-service-provision service)))))
    (string-append "dmd-"
                   (string-map (match-lambda
                                 (#\/ #\-)
                                 (chr chr))
                               provisions)
                   ".scm")))

(define (dmd-service-file service)
  "Return a file defining SERVICE."
  (gexp->file (dmd-service-file-name service)
              #~(begin
                  (use-modules #$@(dmd-service-modules service))

                  (make <service>
                    #:docstring '#$(dmd-service-documentation service)
                    #:provides '#$(dmd-service-provision service)
                    #:requires '#$(dmd-service-requirement service)
                    #:respawn? '#$(dmd-service-respawn? service)
                    #:start #$(dmd-service-start service)
                    #:stop #$(dmd-service-stop service)))))

(define (dmd-configuration-file services)
  "Return the dmd configuration file for SERVICES."
  (define modules
    (delete-duplicates
     (append-map dmd-service-imported-modules services)))

  (assert-valid-graph services)

  (mlet %store-monad ((modules  (imported-modules modules))
                      (compiled (compiled-modules modules))
                      (files    (mapm %store-monad dmd-service-file services)))
    (define config
      #~(begin
          (eval-when (expand load eval)
            (set! %load-path (cons #$modules %load-path))
            (set! %load-compiled-path
              (cons #$compiled %load-compiled-path)))

          (use-modules (system repl error-handling))

          ;; Arrange to spawn a REPL if loading one of FILES fails.  This is
          ;; better than a kernel panic.
          (call-with-error-handling
            (lambda ()
              (apply register-services (map primitive-load '#$files))))

          ;; guix-daemon 0.6 aborts if 'PATH' is undefined, so work around it.
          (setenv "PATH" "/run/current-system/profile/bin")

          (format #t "starting services...~%")
          (for-each start
                    '#$(append-map dmd-service-provision
                                   (filter dmd-service-auto-start?
                                           services)))))

    (gexp->file "dmd.conf" config)))

(define (dmd-service-back-edges services)
  "Return a procedure that, when given a <dmd-service> from SERVICES, returns
the list of <dmd-service> that depend on it."
  (define provision->service
    (let ((services (fold (lambda (service result)
                            (fold (cut vhash-consq <> service <>)
                                  result
                                  (dmd-service-provision service)))
                          vlist-null
                          services)))
      (lambda (name)
        (match (vhash-assq name services)
          ((_ . service) service)
          (#f            #f)))))

  (define edges
    (fold (lambda (service edges)
            (fold (lambda (requirement edges)
                    (vhash-consq (provision->service requirement) service
                                 edges))
                  edges
                  (dmd-service-requirement service)))
          vlist-null
          services))

  (lambda (service)
    (vhash-foldq* cons '() service edges)))

;;; dmd.scm ends here
