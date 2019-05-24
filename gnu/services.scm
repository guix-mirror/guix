;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
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
  #:use-module (guix profiles)
  #:use-module (guix discovery)
  #:use-module (guix combinators)
  #:use-module (guix sets)
  #:use-module (guix ui)
  #:use-module ((guix utils) #:select (source-properties->location))
  #:use-module (guix modules)
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
            service-extension-target
            service-extension-compute

            service-type
            service-type?
            service-type-name
            service-type-extensions
            service-type-compose
            service-type-extend
            service-type-default-value
            service-type-description
            service-type-location

            %service-type-path
            fold-service-types
            lookup-service-types

            service
            service?
            service-kind
            service-value
            service-parameters                    ;deprecated

            simple-service
            modify-services
            service-back-edges
            instantiate-missing-services
            fold-services

            service-error?
            missing-value-service-error?
            missing-value-service-error-type
            missing-value-service-error-location
            missing-target-service-error?
            missing-target-service-error-service
            missing-target-service-error-target-type
            ambiguous-target-service-error?
            ambiguous-target-service-error-service
            ambiguous-target-service-error-target-type

            system-service-type
            boot-service-type
            cleanup-service-type
            activation-service-type
            activation-service->script
            %linux-bare-metal-service
            special-files-service-type
            extra-special-file
            etc-service-type
            etc-directory
            setuid-program-service-type
            profile-service-type
            firmware-service-type
            gc-root-service-type

            %boot-service
            %activation-service
            etc-service))

;;; Comment:
;;;
;;; This module defines a broad notion of "service types" and "services."
;;;
;;; A service type describe how its instances extend instances of other
;;; service types.  For instance, some services extend the instance of
;;; ACCOUNT-SERVICE-TYPE by providing it with accounts and groups to create;
;;; others extend SHEPHERD-ROOT-SERVICE-TYPE by passing it instances of
;;; <shepherd-service>.
;;;
;;; When applicable, the service type defines how it can itself be extended,
;;; by providing one procedure to compose extensions, and one procedure to
;;; extend itself.
;;;
;;; A notable service type is SYSTEM-SERVICE-TYPE, which has a single
;;; instance, which is the root of the service DAG.  Its value is the
;;; derivation that produces the 'system' directory as returned by
;;; 'operating-system-derivation'.
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

(define &no-default-value
  ;; Value used to denote service types that have no associated default value.
  '(no default value))

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
              (default #f))

  ;; Optional default value for instances of this type.
  (default-value service-type-default-value       ;Any
                 (default &no-default-value))

  ;; Meta-data.
  (description  service-type-description          ;string
                (default #f))
  (location     service-type-location             ;<location>
                (default (and=> (current-source-location)
                                source-properties->location))
                (innate)))

(define (write-service-type type port)
  (format port "#<service-type ~a ~a>"
          (service-type-name type)
          (number->string (object-address type) 16)))

(set-record-type-printer! <service-type> write-service-type)

(define %distro-root-directory
  ;; Absolute file name of the module hierarchy.
  (dirname (search-path %load-path "guix.scm")))

(define %service-type-path
  ;; Search path for service types.
  (make-parameter `((,%distro-root-directory . "gnu/services")
                    (,%distro-root-directory . "gnu/system"))))

(define (all-service-modules)
  "Return the default set of service modules."
  (cons (resolve-interface '(gnu services))
        (all-modules (%service-type-path)
                     #:warn warn-about-load-error)))

(define* (fold-service-types proc seed
                             #:optional
                             (modules (all-service-modules)))
  "For each service type exported by one of MODULES, call (PROC RESULT).  SEED
is used as the initial value of RESULT."
  (fold-module-public-variables (lambda (object result)
                                  (if (service-type? object)
                                      (proc object result)
                                      result))
                                seed
                                modules))

(define lookup-service-types
  (let ((table
         (delay (fold-service-types (lambda (type result)
                                      (vhash-consq (service-type-name type)
                                                   type result))
                                    vlist-null))))
    (lambda (name)
      "Return the list of services with the given NAME (a symbol)."
      (vhash-foldq* cons '() name (force table)))))

;; Services of a given type.
(define-record-type <service>
  (make-service type value)
  service?
  (type       service-kind)
  (value      service-value))

(define-syntax service
  (syntax-rules ()
    "Return a service instance of TYPE.  The service value is VALUE or, if
omitted, TYPE's default value."
    ((_ type value)
     (make-service type value))
    ((_ type)
     (%service-with-default-value (current-source-location)
                                  type))))

(define (%service-with-default-value location type)
  "Return a instance of service type TYPE with its default value, if any.  If
TYPE does not have a default value, an error is raised."
  ;; TODO: Currently this is a run-time error but with a little bit macrology
  ;; we could turn it into an expansion-time error.
  (let ((default (service-type-default-value type)))
    (if (eq? default &no-default-value)
        (let ((location (source-properties->location location)))
          (raise
           (condition
            (&missing-value-service-error (type type) (location location))
            (&message
             (message (format #f (G_ "~a: no value specified \
for service of type '~a'")
                              (location->string location)
                              (service-type-name type)))))))
        (service type default))))

(define-condition-type &service-error &error
  service-error?)

(define-condition-type &missing-value-service-error &service-error
  missing-value-service-error?
  (type     missing-value-service-error-type)
  (location missing-value-service-error-location))



;;;
;;; Helpers.
;;;

(define service-parameters
  ;; Deprecated alias.
  service-value)

(define (simple-service name target value)
  "Return a service that extends TARGET with VALUE.  This works by creating a
singleton service type NAME, of which the returned service is an instance."
  (let* ((extension (service-extension target identity))
         (type      (service-type (name name)
                                  (extensions (list extension)))))
    (service type value)))

(define-syntax %modify-service
  (syntax-rules (=>)
    ((_ service)
     service)
    ((_ svc (kind param => exp ...) clauses ...)
     (if (eq? (service-kind svc) kind)
         (let ((param (service-value svc)))
           (service (service-kind svc)
                    (begin exp ...)))
         (%modify-service svc clauses ...)))))

(define-syntax modify-services
  (syntax-rules ()
    "Modify the services listed in SERVICES according to CLAUSES and return
the resulting list of services.  Each clause must have the form:

  (TYPE VARIABLE => BODY)

where TYPE is a service type, such as 'guix-service-type', and VARIABLE is an
identifier that is bound within BODY to the value of the service of that
TYPE.  Consider this example:

  (modify-services %base-services
    (guix-service-type config =>
                       (guix-configuration
                        (inherit config)
                        (use-substitutes? #f)
                        (extra-options '(\"--gc-keep-derivations\"))))
    (mingetty-service-type config =>
                           (mingetty-configuration
                            (inherit config)
                            (motd (plain-file \"motd\" \"Hi there!\")))))

It changes the configuration of the GUIX-SERVICE-TYPE instance, and that of
all the MINGETTY-SERVICE-TYPE instances.

This is a shorthand for (map (lambda (svc) ...) %base-services)."
    ((_ services clauses ...)
     (map (lambda (service)
            (%modify-service service clauses ...))
          services))))


;;;
;;; Core services.
;;;

(define (system-derivation mentries mextensions)
  "Return as a monadic value the derivation of the 'system' directory
containing the given entries."
  (mlet %store-monad ((entries    mentries)
                      (extensions (sequence %store-monad mextensions)))
    (lower-object
     (file-union "system"
                 (append entries (concatenate extensions))))))

(define system-service-type
  ;; This is the ultimate service type, the root of the service DAG.  The
  ;; service of this type is extended by monadic name/item pairs.  These items
  ;; end up in the "system directory" as returned by
  ;; 'operating-system-derivation'.
  (service-type (name 'system)
                (extensions '())
                (compose identity)
                (extend system-derivation)
                (description
                 "Build the operating system top-level directory, which in
turn refers to everything the operating system needs: its kernel, initrd,
system profile, boot script, and so on.")))

(define (compute-boot-script _ gexps)
  ;; Reverse GEXPS so that extensions appear in the boot script in the right
  ;; order.  That is, user extensions would come first, and extensions added
  ;; by 'essential-services' (e.g., running shepherd) are guaranteed to come
  ;; last.
  (gexp->file "boot"
              ;; Clean up and activate the system, then spawn shepherd.
              #~(begin #$@(reverse gexps))))

(define (boot-script-entry mboot)
  "Return, as a monadic value, an entry for the boot script in the system
directory."
  (mlet %store-monad ((boot mboot))
    (return `(("boot" ,boot)))))

(define boot-service-type
  ;; The service of this type is extended by being passed gexps.  It
  ;; aggregates them in a single script, as a monadic value, which becomes its
  ;; value.
  (service-type (name 'boot)
                (extensions
                 (list (service-extension system-service-type
                                          boot-script-entry)))
                (compose identity)
                (extend compute-boot-script)
                (description
                 "Produce the operating system's boot script, which is spawned
by the initrd once the root file system is mounted.")))

(define %boot-service
  ;; The service that produces the boot script.
  (service boot-service-type #t))

(define (cleanup-gexp _)
  "Return a gexp to clean up /tmp and similar places upon boot."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        ;; Clean out /tmp and /var/run.
        ;;
        ;; XXX This needs to happen before service activations, so it
        ;; has to be here, but this also implicitly assumes that /tmp
        ;; and /var/run are on the root partition.
        (letrec-syntax ((fail-safe (syntax-rules ()
                                     ((_ exp rest ...)
                                      (begin
                                        (catch 'system-error
                                          (lambda () exp)
                                          (const #f))
                                        (fail-safe rest ...)))
                                     ((_)
                                      #t))))
          ;; Ignore I/O errors so the system can boot.
          (fail-safe
           ;; Remove stale Shadow lock files as they would lead to
           ;; failures of 'useradd' & co.
           (delete-file "/etc/group.lock")
           (delete-file "/etc/passwd.lock")
           (delete-file "/etc/.pwd.lock")         ;from 'lckpwdf'

           ;; Force file names to be decoded as UTF-8.  See
           ;; <https://bugs.gnu.org/26353>.
           (setenv "GUIX_LOCPATH"
                   #+(file-append glibc-utf8-locales "/lib/locale"))
           (setlocale LC_CTYPE "en_US.utf8")
           (delete-file-recursively "/tmp")
           (delete-file-recursively "/var/run")

           (mkdir "/tmp")
           (chmod "/tmp" #o1777)
           (mkdir "/var/run")
           (chmod "/var/run" #o755)
           (delete-file-recursively "/run/udev/watch.old"))))))

(define cleanup-service-type
  ;; Service that cleans things up in /tmp and similar.
  (service-type (name 'cleanup)
                (extensions
                 (list (service-extension boot-service-type
                                          cleanup-gexp)))
                (description
                 "Delete files from @file{/tmp}, @file{/var/run}, and other
temporary locations at boot time.")))

(define* (activation-service->script service)
  "Return as a monadic value the activation script for SERVICE, a service of
ACTIVATION-SCRIPT-TYPE."
  (activation-script (service-value service)))

(define (activation-script gexps)
  "Return the system's activation script, which evaluates GEXPS."
  (define actions
    (map (cut scheme-file "activate-service" <>) gexps))

  (scheme-file "activate"
               (with-imported-modules (source-module-closure
                                       '((gnu build activation)
                                         (guix build utils)))
                 #~(begin
                     (use-modules (gnu build activation)
                                  (guix build utils))

                     ;; Make sure the user accounting database exists.  If it
                     ;; does not exist, 'setutxent' does not create it and
                     ;; thus there is no accounting at all.
                     (close-port (open-file "/var/run/utmpx" "a0"))

                     ;; Same for 'wtmp', which is populated by mingetty et
                     ;; al.
                     (mkdir-p "/var/log")
                     (close-port (open-file "/var/log/wtmp" "a0"))

                     ;; Set up /run/current-system.  Among other things this
                     ;; sets up locales, which the activation snippets
                     ;; executed below may expect.
                     (activate-current-system)

                     ;; Run the services' activation snippets.
                     ;; TODO: Use 'load-compiled'.
                     (for-each primitive-load '#$actions)))))

(define (gexps->activation-gexp gexps)
  "Return a gexp that runs the activation script containing GEXPS."
  #~(primitive-load #$(activation-script gexps)))

(define (second-argument a b) b)

(define activation-service-type
  (service-type (name 'activate)
                (extensions
                 (list (service-extension boot-service-type
                                          gexps->activation-gexp)))
                (compose identity)
                (extend second-argument)
                (description
                 "Run @dfn{activation} code at boot time and upon
@command{guix system reconfigure} completion.")))

(define %activation-service
  ;; The activation service produces the activation script from the gexps it
  ;; receives.
  (service activation-service-type #t))

(define %modprobe-wrapper
  ;; Wrapper for the 'modprobe' command that knows where modules live.
  ;;
  ;; This wrapper is typically invoked by the Linux kernel ('call_modprobe',
  ;; in kernel/kmod.c), a situation where the 'LINUX_MODULE_DIRECTORY'
  ;; environment variable is not set---hence the need for this wrapper.
  (let ((modprobe "/run/current-system/profile/bin/modprobe"))
    (program-file "modprobe"
                  #~(begin
                      (setenv "LINUX_MODULE_DIRECTORY"
                              "/run/booted-system/kernel/lib/modules")
                      (apply execl #$modprobe
                             (cons #$modprobe (cdr (command-line))))))))

(define %linux-kernel-activation
  ;; Activation of the Linux kernel running on the bare metal (as opposed to
  ;; running in a container.)
  #~(begin
      ;; Tell the kernel to use our 'modprobe' command.
      (activate-modprobe #$%modprobe-wrapper)

      ;; Let users debug their own processes!
      (activate-ptrace-attach)))

(define %linux-bare-metal-service
  ;; The service that does things that are needed on the "bare metal", but not
  ;; necessary or impossible in a container.
  (simple-service 'linux-bare-metal
                  activation-service-type
                  %linux-kernel-activation))


(define special-files-service-type
  ;; Service to install "special files" such as /bin/sh and /usr/bin/env.
  (service-type
   (name 'special-files)
   (extensions
    (list (service-extension activation-service-type
                             (lambda (files)
                               #~(activate-special-files '#$files)))))
   (compose concatenate)
   (extend append)
   (description
    "Add special files to the root file system---e.g.,
@file{/usr/bin/env}.")))

(define (extra-special-file file target)
  "Use TARGET as the \"special file\" FILE.  For example, TARGET might be
  (file-append coreutils \"/bin/env\")
and FILE could be \"/usr/bin/env\"."
  (simple-service (string->symbol (string-append "special-file-" file))
                  special-files-service-type
                  `((,file ,target))))

(define (etc-directory service)
  "Return the directory for SERVICE, a service of type ETC-SERVICE-TYPE."
  (files->etc-directory (service-value service)))

(define (files->etc-directory files)
  (file-union "etc" files))

(define (etc-entry files)
  "Return an entry for the /etc directory consisting of FILES in the system
directory."
  (with-monad %store-monad
    (return `(("etc" ,(files->etc-directory files))))))

(define etc-service-type
  (service-type (name 'etc)
                (extensions
                 (list
                  (service-extension activation-service-type
                                     (lambda (files)
                                       (let ((etc
                                              (files->etc-directory files)))
                                         #~(activate-etc #$etc))))
                  (service-extension system-service-type etc-entry)))
                (compose concatenate)
                (extend append)
                (description "Populate the @file{/etc} directory.")))

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
                (extend append)
                (description
                 "Populate @file{/run/setuid-programs} with the specified
executables, making them setuid-root.")))

(define (packages->profile-entry packages)
  "Return a system entry for the profile containing PACKAGES."
  (mlet %store-monad ((profile (profile-derivation
                                (packages->manifest
                                 (delete-duplicates packages eq?)))))
    (return `(("profile" ,profile)))))

(define profile-service-type
  ;; The service that populates the system's profile---i.e.,
  ;; /run/current-system/profile.  It is extended by package lists.
  (service-type (name 'profile)
                (extensions
                 (list (service-extension system-service-type
                                          packages->profile-entry)))
                (compose concatenate)
                (extend append)
                (description
                 "This is the @dfn{system profile}, available as
@file{/run/current-system/profile}.  It contains packages that the sysadmin
wants to be globally available to all the system users.")))

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
                (extend append)
                (description
                 "Make ``firmware'' files loadable by the operating system
kernel.  Firmware may then be uploaded to some of the machine's devices, such
as Wifi cards.")))

(define (gc-roots->system-entry roots)
  "Return an entry in the system's output containing symlinks to ROOTS."
  (mlet %store-monad ((entry (gexp->derivation
                              "gc-roots"
                              #~(let ((roots '#$roots))
                                  (mkdir #$output)
                                  (chdir #$output)
                                  (for-each symlink
                                            roots
                                            (map number->string
                                                 (iota (length roots))))))))
    (return (if (null? roots)
                '()
                `(("gc-roots" ,entry))))))

(define gc-root-service-type
  ;; A service to associate extra garbage-collector roots to the system.  This
  ;; is a simple hack that guarantees that the system retains references to
  ;; the given list of roots.  Roots must be "lowerable" objects like
  ;; packages, or derivations.
  (service-type (name 'gc-roots)
                (extensions
                 (list (service-extension system-service-type
                                          gc-roots->system-entry)))
                (compose concatenate)
                (extend append)
                (description
                 "Register garbage-collector roots---i.e., store items that
will not be reclaimed by the garbage collector.")
                (default-value '())))


;;;
;;; Service folding.
;;;

(define-condition-type &missing-target-service-error &service-error
  missing-target-service-error?
  (service      missing-target-service-error-service)
  (target-type  missing-target-service-error-target-type))

(define-condition-type &ambiguous-target-service-error &service-error
  ambiguous-target-service-error?
  (service      ambiguous-target-service-error-service)
  (target-type  ambiguous-target-service-error-target-type))

(define (missing-target-error service target-type)
  (raise
   (condition (&missing-target-service-error
               (service service)
               (target-type target-type))
              (&message
               (message
                (format #f (G_ "no target of type '~a' for service '~a'")
                        (service-type-name target-type)
                        (service-type-name
                         (service-kind service))))))))

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
           (missing-target-error service target-type))
          (x
           (raise
            (condition (&ambiguous-target-service-error
                        (service service)
                        (target-type target-type))
                       (&message
                        (message
                         (format #f
                                 (G_ "more than one target service of type '~a'")
                                 (service-type-name target-type))))))))))

    (fold add-edge edges (service-type-extensions (service-kind service))))

  (let ((edges (fold add-edges vlist-null services)))
    (lambda (node)
      (reverse (vhash-foldq* cons '() node edges)))))

(define (instantiate-missing-services services)
  "Return SERVICES, a list, augmented with any services targeted by extensions
and missing from SERVICES.  Only service types with a default value can be
instantiated; other missing services lead to a
'&missing-target-service-error'."
  (define (adjust-service-list svc result instances)
    (fold2 (lambda (extension result instances)
             (define target-type
               (service-extension-target extension))

             (match (vhash-assq target-type instances)
               (#f
                (let ((default (service-type-default-value target-type)))
                  (if (eq? &no-default-value default)
                      (missing-target-error svc target-type)
                      (let ((new (service target-type)))
                        (values (cons new result)
                                (vhash-consq target-type new instances))))))
               (_
                (values result instances))))
           result
           instances
           (service-type-extensions (service-kind svc))))

  (let loop ((services services))
    (define instances
      (fold (lambda (service result)
              (vhash-consq (service-kind service) service
                           result))
            vlist-null services))

    (define adjusted
      (fold2 adjust-service-list
             services instances
             services))

    ;; If we instantiated services, they might in turn depend on missing
    ;; services.  Loop until we've reached fixed point.
    (if (= (length adjusted) (vlist-length instances))
        adjusted
        (loop adjusted))))

(define* (fold-services services
                        #:key (target-type system-service-type))
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
         (compute (service-value service))))))

  (match (filter (lambda (service)
                   (eq? (service-kind service) target-type))
                 services)
    ((sink)
     (let loop ((sink sink))
       (let* ((dependents (map loop (dependents sink)))
              (extensions (map (apply-extension sink) dependents))
              (extend     (service-type-extend (service-kind sink)))
              (compose    (service-type-compose (service-kind sink)))
              (params     (service-value sink)))
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
                  (message (format #f (G_ "service of type '~a' not found")
                                   (service-type-name target-type)))))))
    (x
     (raise
      (condition (&ambiguous-target-service-error
                  (service #f)
                  (target-type target-type))
                 (&message
                  (message
                   (format #f
                           (G_ "more than one target service of type '~a'")
                           (service-type-name target-type)))))))))

;;; services.scm ends here.
