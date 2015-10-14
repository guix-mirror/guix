;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix records)
  #:use-module (guix sets)
  #:use-module (guix ui)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:export (service-extension
            service-extension?

            service-type
            service-type?
            service-type-name
            service-type-extensions
            service-type-compose
            service-type-extend

            service
            service?
            service-kind
            service-parameters

            service-back-edges
            fold-services

            service-error?
            missing-target-service-error?
            missing-target-service-error-service
            missing-target-service-error-target-type
            ambiguous-target-service-error?
            ambiguous-target-service-error-service
            ambiguous-target-service-error-target-type

            boot-service-type
            activation-service-type
            activation-service->script
            etc-service-type
            etc-directory
            setuid-program-service-type
            firmware-service-type

            %boot-service
            %activation-service
            etc-service

            file-union))                      ;XXX: for lack of a better place

;;; Comment:
;;;
;;; This module defines a broad notion of "service types" and "services."
;;;
;;; A service type describe how its instances extend instances of other
;;; service types.  For instance, some services extend the instance of
;;; ACCOUNT-SERVICE-TYPE by providing it with accounts and groups to create;
;;; others extend DMD-ROOT-SERVICE-TYPE by passing it instances of
;;; <dmd-service>.
;;;
;;; When applicable, the service type defines how it can itself be extended,
;;; by providing one procedure to compose extensions, and one procedure to
;;; extend itself.
;;;
;;; A notable service type is BOOT-SERVICE-TYPE, which has a single instance,
;;; %BOOT-SERVICE.  %BOOT-SERVICE constitutes the root of the service DAG.  It
;;; produces the boot script that the initrd loads.
;;;
;;; The 'fold-services' procedure can be passed a list of procedures, which it
;;; "folds" by propagating extensions down the graph; it returns the root
;;; service after the applying all its extensions.
;;;
;;; Code:

(define-record-type <service-extension>
  (service-extension target compute)
  service-extension?
  (target  service-extension-target)              ;<service-type>
  (compute service-extension-compute))            ;params -> params

(define-record-type* <service-type> service-type make-service-type
  service-type?
  (name       service-type-name)                  ;symbol (for debugging)

  ;; Things extended by services of this type.
  (extensions service-type-extensions)            ;list of <service-extensions>

  ;; Given a list of extensions, "compose" them.
  (compose    service-type-compose                ;list of Any -> Any
              (default #f))

  ;; Extend the services' own parameters with the extension composition.
  (extend     service-type-extend                 ;list of Any -> parameters
              (default #f)))

(define (write-service-type type port)
  (format port "#<service-type ~a ~a>"
          (service-type-name type)
          (number->string (object-address type) 16)))

(set-record-type-printer! <service-type> write-service-type)

;; Services of a given type.
(define-record-type <service>
  (service type parameters)
  service?
  (type       service-kind)
  (parameters service-parameters))




;;;
;;; Core services.
;;;

(define (compute-boot-script mexps)
  (mlet %store-monad ((gexps (sequence %store-monad mexps)))
    (gexp->file "boot"
                #~(begin
                    (use-modules (guix build utils))

                    ;; Clean out /tmp and /var/run.
                    ;;
                    ;; XXX This needs to happen before service activations, so
                    ;; it has to be here, but this also implicitly assumes
                    ;; that /tmp and /var/run are on the root partition.
                    (false-if-exception (delete-file-recursively "/tmp"))
                    (false-if-exception (delete-file-recursively "/var/run"))
                    (false-if-exception (mkdir "/tmp"))
                    (false-if-exception (chmod "/tmp" #o1777))
                    (false-if-exception (mkdir "/var/run"))
                    (false-if-exception (chmod "/var/run" #o755))

                    ;; Activate the system and spawn dmd.
                    #$@gexps))))

(define (second-argument a b) b)

(define boot-service-type
  ;; The service of this type is extended by being passed gexps as monadic
  ;; values.  It aggregates them in a single script, as a monadic value, which
  ;; becomes its 'parameters'.  It is the only service that extends nothing.
  (service-type (name 'boot)
                (extensions '())
                (compose compute-boot-script)
                (extend second-argument)))

(define %boot-service
  ;; This is the ultimate service, the root of the service DAG.
  (service boot-service-type #t))

(define* (file-union name files)                  ;FIXME: Factorize.
  "Return a <computed-file> that builds a directory containing all of FILES.
Each item in FILES must be a list where the first element is the file name to
use in the new directory, and the second element is a gexp denoting the target
file."
  (computed-file name
                 #~(begin
                     (mkdir #$output)
                     (chdir #$output)
                     #$@(map (match-lambda
                               ((target source)
                                #~(symlink #$source #$target)))
                             files))))

(define (directory-union name things)
  "Return a directory that is the union of THINGS."
  (match things
    ((one)
     ;; Only one thing; return it.
     one)
    (_
     (computed-file name
                    #~(begin
                        (use-modules (guix build union))
                        (union-build #$output '#$things))
                    #:modules '((guix build union))))))

(define (modprobe-wrapper)
  "Return a wrapper for the 'modprobe' command that knows where modules live.

This wrapper is typically invoked by the Linux kernel ('call_modprobe', in
kernel/kmod.c), a situation where the 'LINUX_MODULE_DIRECTORY' environment
variable is not set---hence the need for this wrapper."
  (let ((modprobe "/run/current-system/profile/bin/modprobe"))
    (gexp->script "modprobe"
                  #~(begin
                      (setenv "LINUX_MODULE_DIRECTORY"
                              "/run/booted-system/kernel/lib/modules")
                      (apply execl #$modprobe
                             (cons #$modprobe (cdr (command-line))))))))

(define* (activation-service->script service)
  "Return as a monadic value the activation script for SERVICE, a service of
ACTIVATION-SCRIPT-TYPE."
  (activation-script (service-parameters service)))

(define (activation-script gexps)
  "Return the system's activation script, which evaluates GEXPS."
  (define %modules
    '((gnu build activation)
      (gnu build linux-boot)
      (gnu build linux-modules)
      (gnu build file-systems)
      (guix build utils)
      (guix build syscalls)
      (guix elf)))

  (define (service-activations)
    ;; Return the activation scripts for SERVICES.
    (mapm %store-monad
          (cut gexp->file "activate-service" <>)
          gexps))

  (mlet* %store-monad ((actions  (service-activations))
                       (modules  (imported-modules %modules))
                       (compiled (compiled-modules %modules))
                       (modprobe (modprobe-wrapper)))
    (gexp->file "activate"
                #~(begin
                    (eval-when (expand load eval)
                      ;; Make sure 'use-modules' below succeeds.
                      (set! %load-path (cons #$modules %load-path))
                      (set! %load-compiled-path
                        (cons #$compiled %load-compiled-path)))

                    (use-modules (gnu build activation))

                    ;; Make sure /bin/sh is valid and current.
                    (activate-/bin/sh
                     (string-append #$(canonical-package bash) "/bin/sh"))

                    ;; Tell the kernel to use our 'modprobe' command.
                    (activate-modprobe #$modprobe)

                    ;; Let users debug their own processes!
                    (activate-ptrace-attach)

                    ;; Run the services' activation snippets.
                    ;; TODO: Use 'load-compiled'.
                    (for-each primitive-load '#$actions)

                    ;; Set up /run/current-system.
                    (activate-current-system)))))

(define (gexps->activation-gexp gexps)
  "Return a gexp that runs the activation script containing GEXPS."
  (mlet %store-monad ((script (activation-script gexps)))
    (return #~(primitive-load #$script))))

(define activation-service-type
  (service-type (name 'activate)
                (extensions
                 (list (service-extension boot-service-type
                                          gexps->activation-gexp)))
                (compose append)
                (extend second-argument)))

(define %activation-service
  ;; The activation service produces the activation script from the gexps it
  ;; receives.
  (service activation-service-type #t))

(define (etc-directory service)
  "Return the directory for SERVICE, a service of type ETC-SERVICE-TYPE."
  (files->etc-directory (service-parameters service)))

(define (files->etc-directory files)
  (file-union "etc" files))

(define etc-service-type
  (service-type (name 'etc)
                (extensions
                 (list
                  (service-extension activation-service-type
                                     (lambda (files)
                                       (let ((etc
                                              (files->etc-directory files)))
                                         #~(activate-etc #$etc))))))
                (compose concatenate)
                (extend append)))

(define (etc-service files)
  "Return a new service of ETC-SERVICE-TYPE that populates /etc with FILES.
FILES must be a list of name/file-like object pairs."
  (service etc-service-type files))

(define setuid-program-service-type
  (service-type (name 'setuid-program)
                (extensions
                 (list (service-extension activation-service-type
                                          (lambda (programs)
                                            #~(activate-setuid-programs
                                               (list #$@programs))))))
                (compose concatenate)
                (extend append)))

(define (firmware->activation-gexp firmware)
  "Return a gexp to make the packages listed in FIRMWARE loadable by the
kernel."
  (let ((directory (directory-union "firmware" firmware)))
    ;; Tell the kernel where firmware is.
    #~(activate-firmware (string-append #$directory "/lib/firmware"))))

(define firmware-service-type
  ;; The service that collects firmware.
  (service-type (name 'firmware)
                (extensions
                 (list (service-extension activation-service-type
                                          firmware->activation-gexp)))
                (compose concatenate)
                (extend append)))


;;;
;;; Service folding.
;;;

(define-condition-type &service-error &error
  service-error?)

(define-condition-type &missing-target-service-error &service-error
  missing-target-service-error?
  (service      missing-target-service-error-service)
  (target-type  missing-target-service-error-target-type))

(define-condition-type &ambiguous-target-service-error &service-error
  ambiguous-target-service-error?
  (service      ambiguous-target-service-error-service)
  (target-type  ambiguous-target-service-error-target-type))

(define (service-back-edges services)
  "Return a procedure that, when passed a <service>, returns the list of
<service> objects that depend on it."
  (define (add-edges service edges)
    (define (add-edge extension edges)
      (let ((target-type (service-extension-target extension)))
        (match (filter (lambda (service)
                         (eq? (service-kind service) target-type))
                       services)
          ((target)
           (vhash-consq target service edges))
          (()
           (raise
            (condition (&missing-target-service-error
                        (service service)
                        (target-type target-type))
                       (&message
                        (message
                         (format #f (_ "no target of type '~a' for service ~s")
                                 (service-type-name target-type)
                                 service))))))
          (x
           (raise
            (condition (&ambiguous-target-service-error
                        (service service)
                        (target-type target-type))
                       (&message
                        (message
                         (format #f
                                 (_ "more than one target service of type '~a'")
                                 (service-type-name target-type))))))))))

    (fold add-edge edges (service-type-extensions (service-kind service))))

  (let ((edges (fold add-edges vlist-null services)))
    (lambda (node)
      (reverse (vhash-foldq* cons '() node edges)))))

(define* (fold-services services #:key (target-type boot-service-type))
  "Fold SERVICES by propagating their extensions down to the root of type
TARGET-TYPE; return the root service adjusted accordingly."
  (define dependents
    (service-back-edges services))

  (define (matching-extension target)
    (let ((target (service-kind target)))
      (match-lambda
        (($ <service-extension> type)
         (eq? type target)))))

  (define (apply-extension target)
    (lambda (service)
      (match (find (matching-extension target)
                   (service-type-extensions (service-kind service)))
        (($ <service-extension> _ compute)
         (compute (service-parameters service))))))

  (match (filter (lambda (service)
                   (eq? (service-kind service) target-type))
                 services)
    ((sink)
     (let loop ((sink sink))
       (let* ((dependents (map loop (dependents sink)))
              (extensions (map (apply-extension sink) dependents))
              (extend     (service-type-extend (service-kind sink)))
              (compose    (service-type-compose (service-kind sink)))
              (params     (service-parameters sink)))
         ;; We distinguish COMPOSE and EXTEND because PARAMS typically has a
         ;; different type than the elements of EXTENSIONS.
         (if extend
             (service (service-kind sink)
                      (extend params (compose extensions)))
             sink))))
    (()
     (raise
      (condition (&missing-target-service-error
                  (service #f)
                  (target-type target-type))
                 (&message
                  (message (format #f (_ "service of type '~a' not found")
                                   (service-type-name target-type)))))))
    (x
     (raise
      (condition (&ambiguous-target-service-error
                  (service #f)
                  (target-type target-type))
                 (&message
                  (message
                   (format #f
                           (_ "more than one target service of type '~a'")
                           (service-type-name target-type)))))))))

;;; services.scm ends here.
