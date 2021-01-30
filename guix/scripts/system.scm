;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017, 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
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

(define-module (guix scripts system)
  #:use-module (guix config)
  #:use-module (guix ui)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix store)
  #:autoload   (guix base16) (bytevector->base16-string)
  #:autoload   (guix store database)
               (sqlite-register store-database-file call-with-database)
  #:autoload   (guix build store-copy) (copy-store-item)
  #:use-module (guix describe)
  #:use-module (guix grafts)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix profiles)
  #:use-module (guix scripts)
  #:use-module (guix channels)
  #:use-module (guix scripts build)
  #:autoload   (guix scripts package) (delete-generations
                                       delete-matching-generations)
  #:autoload   (guix scripts pull) (channel-commit-hyperlink)
  #:autoload   (guix graph) (export-graph node-type
                             graph-backend-name %graph-backends)
  #:use-module (guix scripts graph)
  #:use-module (guix scripts system reconfigure)
  #:use-module (guix build utils)
  #:use-module (guix progress)
  #:use-module ((guix build syscalls) #:select (terminal-columns))
  #:use-module (gnu build install)
  #:autoload   (gnu build file-systems)
                 (find-partition-by-label find-partition-by-uuid)
  #:autoload   (gnu build linux-modules)
                 (device-module-aliases matching-modules)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu image)
  #:use-module (gnu system)
  #:use-module (gnu bootloader)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system linux-container)
  #:use-module (gnu system uuid)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services herd)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:export (guix-system
            read-operating-system))


;;;
;;; Operating system declaration.
;;;

(define %user-module
  ;; Module in which the machine description file is loaded.
  (make-user-module '((gnu system)
                      (gnu services)
                      (gnu system shadow))))

(define (read-operating-system file)
  "Read the operating-system declaration from FILE and return it."
  (load* file %user-module))


;;;
;;; Installation.
;;;

(define-syntax-rule (save-load-path-excursion body ...)
  "Save the current values of '%load-path' and '%load-compiled-path', run
BODY..., and restore them."
  (let ((path %load-path)
        (cpath %load-compiled-path))
    (dynamic-wind
      (const #t)
      (lambda ()
        body ...)
      (lambda ()
        (set! %load-path path)
        (set! %load-compiled-path cpath)))))

(define-syntax-rule (save-environment-excursion body ...)
  "Save the current environment variables, run BODY..., and restore them."
  (let ((env (environ)))
    (dynamic-wind
      (const #t)
      (lambda ()
        body ...)
      (lambda ()
        (environ env)))))

(define topologically-sorted*
  (store-lift topologically-sorted))


(define* (copy-item item info target db
                    #:key (log-port (current-error-port)))
  "Copy ITEM to the store under root directory TARGET and populate DB with the
given INFO, a <path-info> record."
  (let ((dest (string-append target item)))
    (format log-port "copying '~a'...~%" item)

    ;; Remove DEST if it exists to make sure that (1) we do not fail badly
    ;; while trying to overwrite it (see <http://bugs.gnu.org/20722>), and
    ;; (2) we end up with the right contents.
    (when (false-if-exception (lstat dest))
      (for-each make-file-writable
                (find-files dest (lambda (file stat)
                                   (eq? 'directory (stat:type stat)))
                            #:directories? #t))
      (delete-file-recursively dest))

    (copy-store-item item target
                     #:deduplicate? #t)

    (sqlite-register db
                     #:path item
                     #:references (path-info-references info)
                     #:deriver (path-info-deriver info)
                     #:hash (string-append
                             "sha256:"
                             (bytevector->base16-string (path-info-hash info)))
                     #:nar-size (path-info-nar-size info))))

(define* (copy-closure item target
                       #:key (log-port (current-error-port)))
  "Copy ITEM and all its dependencies to the store under root directory
TARGET, and register them."
  (mlet* %store-monad ((to-copy (topologically-sorted* (list item)))
                       (info    (mapm %store-monad query-path-info* to-copy))
                       (size -> (reduce + 0 (map path-info-nar-size info))))
    (define progress-bar
      (progress-reporter/bar (length to-copy)
                             (format #f (G_ "copying to '~a'...")
                                     target)))

    (define state
      (string-append target "/var/guix"))

    (check-available-space size target)

    ;; Explicitly use "TARGET/var/guix" as the state directory to avoid
    ;; reproducing the user's current settings; see
    ;; <http://bugs.gnu.org/18049>.
    (call-with-database (store-database-file #:prefix target
                                             #:state-directory state)
      (lambda (db)
        (call-with-progress-reporter progress-bar
          (lambda (report)
            (let ((void (%make-void-port "w")))
              (for-each (lambda (item info)
                          (copy-item item info target db #:log-port void)
                          (report))
                        to-copy info))))))

    (return *unspecified*)))

(define* (install os-drv target
                  #:key (log-port (current-output-port))
                  install-bootloader? bootloader bootcfg)
  "Copy the closure of BOOTCFG, which includes the output of OS-DRV, to
directory TARGET.  TARGET must be an absolute directory name since that's what
'register-path' expects.

When INSTALL-BOOTLOADER? is true, install bootloader using BOOTCFG."
  (define (maybe-copy to-copy)
    (with-monad %store-monad
      (if (string=? target "/")
          (begin
            (warning (G_ "initializing the current root file system~%"))
            (return #t))
          (begin
            ;; Make sure the target store exists.
            (mkdir-p (string-append target (%store-prefix)))

            ;; Copy items to the new store.
            (copy-closure to-copy target #:log-port log-port)))))

  ;; Make sure TARGET is root-owned when running as root, but still allow
  ;; non-root uses (useful for testing.)  See
  ;; <http://lists.gnu.org/archive/html/guix-devel/2015-05/msg00452.html>.
  (if (zero? (geteuid))
      (chown target 0 0)
      (warning (G_ "not running as 'root', so \
the ownership of '~a' may be incorrect!~%")
               target))

  ;; If a previous installation was attempted, make sure we start anew; in
  ;; particular, we don't want to keep a store database that might not
  ;; correspond to what we're actually putting in the store.
  (let ((state (string-append target "/var/guix")))
    (when (file-exists? state)
      (delete-file-recursively state)))

  (chmod target #o755)
  (let ((os-dir   (derivation->output-path os-drv))
        (format   (lift format %store-monad))
        (populate (lift2 populate-root-file-system %store-monad)))

    (mlet %store-monad ((bootcfg (lower-object bootcfg)))
      (mbegin %store-monad
        ;; Copy the closure of BOOTCFG, which includes OS-DIR,
        ;; eventual background image and so on.
        (maybe-copy (derivation->output-path bootcfg))

        ;; Create a bunch of additional files.
        (format log-port "populating '~a'...~%" target)
        (populate os-dir target)

        (mwhen install-bootloader?
          (install-bootloader local-eval bootloader bootcfg
                              #:target target)
          (return
           (info (G_ "bootloader successfully installed on '~a'~%")
                 (bootloader-configuration-target bootloader))))))))


;;;
;;; Reconfiguration.
;;;

(define %system-profile
  ;; The system profile.
  (string-append %state-directory "/profiles/system"))

(define-syntax-rule (with-shepherd-error-handling mbody ...)
  "Catch and report Shepherd errors that arise when binding MBODY, a monadic
expression in %STORE-MONAD."
  (lambda (store)
    (catch 'system-error
      (lambda ()
        (guard (c ((shepherd-error? c)
                   (values (report-shepherd-error c) store)))
          (values (run-with-store store (mbegin %store-monad mbody ...))
                  store)))
      (lambda (key proc format-string format-args errno . rest)
        (warning (G_ "while talking to shepherd: ~a~%")
                 (apply format #f format-string format-args))
        (values #f store)))))

(define (report-shepherd-error error)
  "Report ERROR, a '&shepherd-error' error condition object."
  (when error
    (cond ((service-not-found-error? error)
           (warning (G_ "service '~a' could not be found~%")
                    (service-not-found-error-service error)))
          ((action-not-found-error? error)
           (warning (G_ "service '~a' does not have an action '~a'~%")
                    (action-not-found-error-service error)
                    (action-not-found-error-action error)))
          ((action-exception-error? error)
           (warning (G_ "exception caught while executing '~a' \
on service '~a':~%")
                    (action-exception-error-action error)
                    (action-exception-error-service error))
           (print-exception (current-error-port) #f
                            (action-exception-error-key error)
                            (action-exception-error-arguments error)))
          ((unknown-shepherd-error? error)
           (warning (G_ "something went wrong: ~s~%")
                    (unknown-shepherd-error-sexp error)))
          ((shepherd-error? error)
           (warning (G_ "shepherd error~%"))))

    ;; Don't leave users out in the cold and explain what that means and what
    ;; they can do.
    (warning (G_ "some services could not be upgraded~%"))
    (display-hint (G_ "To allow changes to all the system services to take
effect, you will need to reboot."))))

(define-syntax-rule (unless-file-not-found exp)
  (catch 'system-error
    (lambda ()
      exp)
    (lambda args
      (if (= ENOENT (system-error-errno args))
          #f
          (apply throw args)))))

(define (seconds->string seconds)
  "Return a string representing the date for SECONDS."
  (let ((time (make-time time-utc 0 seconds)))
    (date->string (time-utc->date time)
                  "~Y-~m-~d ~H:~M")))

(define* (profile-boot-parameters #:optional (profile %system-profile)
                                  (numbers
                                   (reverse (generation-numbers profile))))
  "Return a list of 'boot-parameters' for the generations of PROFILE specified
by NUMBERS, which is a list of generation numbers. The list is ordered from
the most recent to the oldest profiles."
  (define (system->boot-parameters system number time)
    (unless-file-not-found
     (let* ((params           (read-boot-parameters-file system))
            (label            (boot-parameters-label params)))
       (boot-parameters
         (inherit params)
         (label (string-append label " (#"
                               (number->string number) ", "
                               (seconds->string time) ")"))))))
  (let* ((systems (map (cut generation-file-name profile <>)
                       numbers))
         (times   (map (lambda (system)
                         (unless-file-not-found
                          (stat:mtime (lstat system))))
                       systems)))
    (filter-map system->boot-parameters systems numbers times)))


;;;
;;; Roll-back.
;;;
(define (roll-back-system store)
  "Roll back the system profile to its previous generation.  STORE is an open
connection to the store."
  (switch-to-system-generation store "-1"))


;;;
;;; Switch generations.
;;;
(define (switch-to-system-generation store spec)
  "Switch the system profile to the generation specified by SPEC, and
re-install bootloader with a configuration file that uses the specified system
generation as its default entry.  STORE is an open connection to the store."
  (let* ((number (relative-generation-spec->number %system-profile spec))
         (generation (generation-file-name %system-profile number))
         (activate (string-append generation "/activate")))
    (if number
        (begin
          (reinstall-bootloader store number)
          (switch-to-generation* %system-profile number)
          (unless-file-not-found (primitive-load activate)))
        (leave (G_ "cannot switch to system generation '~a'~%") spec))))

(define* (system-bootloader-name #:optional (system %system-profile))
  "Return the bootloader name stored in SYSTEM's \"parameters\" file."
  (let ((params (unless-file-not-found
                 (read-boot-parameters-file system))))
    (boot-parameters-bootloader-name params)))

(define (reinstall-bootloader store number)
  "Re-install bootloader for existing system profile generation NUMBER.
STORE is an open connection to the store."
  (let* ((generation (generation-file-name %system-profile number))
         ;; Detect the bootloader used in %system-profile.
         (bootloader (lookup-bootloader-by-name (system-bootloader-name)))

         ;; Use the detected bootloader with default configuration.
         ;; It will be enough to allow the system to boot.
         (bootloader-config (bootloader-configuration
                             (bootloader bootloader)))

         ;; Make the specified system generation the default entry.
         (params (first (profile-boot-parameters %system-profile
                                                 (list number))))
         (locale (boot-parameters-locale params))
         (store-crypto-devices (boot-parameters-store-crypto-devices params))
         (store-directory-prefix
          (boot-parameters-store-directory-prefix params))
         (old-generations
          (delv number (reverse (generation-numbers %system-profile))))
         (old-params (profile-boot-parameters
                       %system-profile old-generations))
         (entries (cons (boot-parameters->menu-entry params)
                        (boot-parameters-bootloader-menu-entries params)))
         (old-entries (map boot-parameters->menu-entry old-params)))
    (run-with-store store
      (mlet* %store-monad
          ((bootcfg (lower-object
                     ((bootloader-configuration-file-generator bootloader)
                      bootloader-config entries
                      #:locale locale
                      #:store-crypto-devices store-crypto-devices
                      #:store-directory-prefix store-directory-prefix
                      #:old-entries old-entries)))
           (drvs -> (list bootcfg)))
        (mbegin %store-monad
          (built-derivations drvs)
          ;; Only install bootloader configuration file.
          (install-bootloader local-eval bootloader-config bootcfg
                              #:run-installer? #f))))))


;;;
;;; Graphs.
;;;

(define (service-node-label service)
  "Return a label to represent SERVICE."
  (let ((type  (service-kind service))
        (value (service-value service)))
    (string-append (symbol->string (service-type-name type))
                   (cond ((or (number? value) (symbol? value))
                          (string-append " " (object->string value)))
                         ((string? value)
                          (string-append " " value))
                         ((file-system? value)
                          (string-append " " (file-system-mount-point value)))
                         (else
                          "")))))

(define (service-node-type services)
  "Return a node type for SERVICES.  Since <service> instances are not
self-contained (they express dependencies on service types, not on services),
we have to create the 'edges' procedure dynamically as a function of the full
list of services."
  (node-type
   (name "service")
   (description "the DAG of services")
   (identifier (lift1 object-address %store-monad))
   (label service-node-label)
   (edges (lift1 (service-back-edges services) %store-monad))))

(define (shepherd-service-node-label service)
  "Return a label for a node representing a <shepherd-service>."
  (string-join (map symbol->string (shepherd-service-provision service))))

(define (shepherd-service-node-type services)
  "Return a node type for SERVICES, a list of <shepherd-service>."
  (node-type
   (name "shepherd-service")
   (description "the dependency graph of shepherd services")
   (identifier (lift1 shepherd-service-node-label %store-monad))
   (label shepherd-service-node-label)
   (edges (lift1 (shepherd-service-back-edges services) %store-monad))))


;;;
;;; Generations.
;;;

(define* (display-system-generation number
                                    #:optional (profile %system-profile))
  "Display a summary of system generation NUMBER in a human-readable format."
  (define (display-channel channel)
    (format #t     "    ~a:~%" (channel-name channel))
    (format #t (G_ "      repository URL: ~a~%") (channel-url channel))
    (when (channel-branch channel)
      (format #t (G_ "      branch: ~a~%") (channel-branch channel)))
    (format #t (G_ "      commit: ~a~%")
            (if (supports-hyperlinks?)
                (channel-commit-hyperlink channel)
                (channel-commit channel))))

  (unless (zero? number)
    (let* ((generation  (generation-file-name profile number))
           (params      (read-boot-parameters-file generation))
           (label       (boot-parameters-label params))
           (bootloader-name (boot-parameters-bootloader-name params))
           (root        (boot-parameters-root-device params))
           (root-device (if (bytevector? root)
                            (uuid->string root)
                            root))
           (kernel      (boot-parameters-kernel params))
           (multiboot-modules (boot-parameters-multiboot-modules params)))
      (define-values (channels config-file)
        (system-provenance generation))

      (display-generation profile number)
      (format #t (G_ "  file name: ~a~%") generation)
      (format #t (G_ "  canonical file name: ~a~%") (readlink* generation))
      ;; TRANSLATORS: Please preserve the two-space indentation.
      (format #t (G_ "  label: ~a~%") label)
      (format #t (G_ "  bootloader: ~a~%") bootloader-name)

      ;; TRANSLATORS: The '~[', '~;', and '~]' sequences in this string must
      ;; be preserved.  They denote conditionals, such that the result will
      ;; look like:
      ;;   root device: UUID: 12345-678
      ;; or:
      ;;   root device: label: "my-root"
      ;; or just:
      ;;   root device: /dev/sda3
      (format #t (G_ "  root device: ~[UUID: ~a~;label: ~s~;~a~]~%")
              (cond ((uuid? root-device) 0)
                    ((file-system-label? root-device) 1)
                    (else 2))
              (file-system-device->string root-device))

      (format #t (G_ "  kernel: ~a~%") kernel)

      (match multiboot-modules
        (() #f)
        (((modules . _) ...)
         (format #t (G_ "  multiboot: ~a~%")
                 (string-join modules "\n    "))))

      (unless (null? channels)
        ;; TRANSLATORS: Here "channel" is the same terminology as used in
        ;; "guix describe" and "guix pull --channels".
        (format #t (G_ "  channels:~%"))
        (for-each display-channel channels))
      (when config-file
        (format #t (G_ "  configuration file: ~a~%")
                (if (supports-hyperlinks?)
                    (file-hyperlink config-file)
                    config-file))))))

(define* (list-generations pattern #:optional (profile %system-profile))
  "Display in a human-readable format all the system generations matching
PATTERN, a string.  When PATTERN is #f, display all the system generations."
  (cond ((not (file-exists? profile))             ; XXX: race condition
         (raise (condition (&profile-not-found-error
                            (profile profile)))))
        ((not pattern)
         (for-each display-system-generation (profile-generations profile)))
        ((matching-generations pattern profile)
         =>
         (lambda (numbers)
           (if (null-list? numbers)
               (exit 1)
               (leave-on-EPIPE
                (for-each display-system-generation numbers)))))))


;;;
;;; File system declaration checks.
;;;

(define (check-file-system-availability file-systems)
  "Check whether the UUIDs or partition labels that FILE-SYSTEMS refer to, if
any, are available.  Raise an error if they're not."
  (define relevant
    (filter (lambda (fs)
              (and (file-system-mount? fs)
                   (not (member (file-system-type fs)
                                %pseudo-file-system-types))
                   ;; Don't try to validate network file systems.
                   (not (string-prefix? "nfs" (file-system-type fs)))
                   (not (memq 'bind-mount (file-system-flags fs)))))
            file-systems))

  (define labeled
    (filter (lambda (fs)
              (file-system-label? (file-system-device fs)))
            relevant))

  (define literal
    (filter (lambda (fs)
              (string? (file-system-device fs)))
            relevant))

  (define uuid
    (filter (lambda (fs)
              (uuid? (file-system-device fs)))
            relevant))

  (define fail? #f)

  (define (file-system-location* fs)
    (and=> (file-system-location fs)
           source-properties->location))

  (let-syntax ((error (syntax-rules ()
                        ((_ args ...)
                         (begin
                           (set! fail? #t)
                           (report-error args ...))))))
    (for-each (lambda (fs)
                (catch 'system-error
                  (lambda ()
                    (stat (file-system-device fs)))
                  (lambda args
                    (let ((errno  (system-error-errno args))
                          (device (file-system-device fs)))
                      (error (file-system-location* fs)
                             (G_ "device '~a' not found: ~a~%")
                             device (strerror errno))
                      (unless (string-prefix? "/" device)
                        (display-hint (format #f (G_ "If '~a' is a file system
label, write @code{(file-system-label ~s)} in your @code{device} field.")
                                              device device)))))))
              literal)
    (for-each (lambda (fs)
                (let ((label (file-system-label->string
                              (file-system-device fs))))
                  (unless (find-partition-by-label label)
                    (error (file-system-location* fs)
                           (G_ "file system with label '~a' not found~%")
                           label))))
              labeled)
    (for-each (lambda (fs)
                (unless (find-partition-by-uuid (file-system-device fs))
                  (error (file-system-location* fs)
                         (G_ "file system with UUID '~a' not found~%")
                         (uuid->string (file-system-device fs)))))
              uuid)

    (when fail?
      ;; Better be safe than sorry.
      (exit 1))))

(define (check-mapped-devices os)
  "Check that each of MAPPED-DEVICES is valid according to the 'check'
procedure of its type."
  (define boot-mapped-devices
    (operating-system-boot-mapped-devices os))

  (define (needed-for-boot? md)
    (memq md boot-mapped-devices))

  (define initrd-modules
    (operating-system-initrd-modules os))

  (for-each (lambda (md)
              (let ((check (mapped-device-kind-check
                            (mapped-device-type md))))
                ;; We expect CHECK to raise an exception with a detailed
                ;; '&message' if something goes wrong.
                (check md
                       #:needed-for-boot? (needed-for-boot? md)
                       #:initrd-modules initrd-modules)))
            (operating-system-mapped-devices os)))

(define (check-initrd-modules os)
  "Check that modules needed by 'needed-for-boot' file systems in OS are
available in the initrd.  Note that mapped devices are responsible for
checking this by themselves in their 'check' procedure."
  (define (file-system-/dev fs)
    (let ((device (file-system-device fs)))
      (match device
        ((? string?)
         device)
        ((? uuid?)
         (find-partition-by-uuid device))
        ((? file-system-label?)
         (find-partition-by-label (file-system-label->string device))))))

  (define file-systems
    (filter file-system-needed-for-boot?
            (operating-system-file-systems os)))

  (for-each (lambda (fs)
              (check-device-initrd-modules (file-system-/dev fs)
                                           (operating-system-initrd-modules os)
                                           (source-properties->location
                                            (file-system-location fs))))
            file-systems))


;;;
;;; Action.
;;;

(define* (system-derivation-for-action image action
                                       #:key
                                       full-boot?
                                       container-shared-network?
                                       mappings)
  "Return as a monadic value the derivation for IMAGE according to ACTION."
  (mlet %store-monad ((target (current-target-system))
                      (os -> (image-operating-system image))
                      (image-size -> (image-size image)))
    (case action
      ((build init reconfigure)
       (operating-system-derivation os))
      ((container)
       (container-script
        os
        #:mappings mappings
        #:shared-network? container-shared-network?))
      ((vm)
       (system-qemu-image/shared-store-script os
                                              #:full-boot? full-boot?
                                              #:disk-image-size
                                              (if full-boot?
                                                  image-size
                                                  (* 70 (expt 2 20)))
                                              #:mappings mappings))
      ((image disk-image vm-image)
       (when (eq? action 'disk-image)
         (warning (G_ "'disk-image' is deprecated: use 'image' instead~%")))
       (when (eq? action 'vm-image)
         (warning (G_ "'vm-image' is deprecated: use 'image' instead~%")))
       (lower-object (system-image image)))
      ((docker-image)
       (system-docker-image os
                            #:shared-network? container-shared-network?)))))

(define (maybe-suggest-running-guix-pull)
  "Suggest running 'guix pull' if this has never been done before."
  ;; Check whether we're running a 'guix pull'-provided 'guix' command.  When
  ;; 'current-profile' returns #f, we may be running the globally-installed
  ;; 'guix' and thus run the risk of deploying an older 'guix'.  See
  ;; <https://lists.gnu.org/archive/html/guix-devel/2014-08/msg00057.html>
  (unless (or (current-profile) (getenv "GUIX_UNINSTALLED"))
    (warning (G_ "Consider running 'guix pull' before 'reconfigure'.~%"))
    (warning (G_ "Failing to do that may downgrade your system!~%"))))

(define (bootloader-installer-script installer
                                     bootloader device target)
  "Return a file calling INSTALLER gexp with given BOOTLOADER, DEVICE
and TARGET arguments."
  (scheme-file "bootloader-installer"
               (with-imported-modules '((gnu build bootloader)
                                        (guix build utils))
                 #~(begin
                     (use-modules (gnu build bootloader)
                                  (guix build utils)
                                  (ice-9 binary-ports)
                                  (srfi srfi-34)
                                  (srfi srfi-35))

                     (guard (c ((message-condition? c) ;XXX: i18n
                                (format (current-error-port) "error: ~a~%"
                                        (condition-message c))
                                (exit 1)))
                       (#$installer #$bootloader #$device #$target)
                       (info (G_ "bootloader successfully installed on '~a'~%")
                             #$device))))))

(define (local-eval exp)
  "Evaluate EXP, a G-Expression, in-place."
  (mlet* %store-monad ((lowered (lower-gexp exp))
                       (_ (built-derivations (lowered-gexp-inputs lowered))))
    (save-load-path-excursion
     (set! %load-path (lowered-gexp-load-path lowered))
     (set! %load-compiled-path (lowered-gexp-load-compiled-path lowered))
     (return (primitive-eval (lowered-gexp-sexp lowered))))))

(define* (perform-action action image
                         #:key
                         (validate-reconfigure ensure-forward-reconfigure)
                         save-provenance?
                         skip-safety-checks?
                         install-bootloader?
                         dry-run? derivations-only?
                         use-substitutes? bootloader-target target
                         full-boot?
                         container-shared-network?
                         (mappings '())
                         (gc-root #f))
  "Perform ACTION for IMAGE.  INSTALL-BOOTLOADER? specifies whether to install
bootloader; BOOTLOADER-TAGET is the target for the bootloader; TARGET is the
target root directory.

FULL-BOOT? is used for the 'vm' action; it determines whether to
boot directly to the kernel or to the bootloader.  CONTAINER-SHARED-NETWORK?
determines if the container will use a separate network namespace.

When DERIVATIONS-ONLY? is true, print the derivation file name(s) without
building anything.

When GC-ROOT is a path, also make that path an indirect root of the build
output when building a system derivation, such as a disk image.

When SKIP-SAFETY-CHECKS? is true, skip the file system and initrd module
static checks."
  (define println
    (cut format #t "~a~%" <>))

  (define menu-entries
    (if (eq? 'init action)
        '()
        (map boot-parameters->menu-entry (profile-boot-parameters))))

  (define os
    (image-operating-system image))

  (define bootloader
    (operating-system-bootloader os))

  (define bootcfg
    (and (memq action '(init reconfigure))
         (operating-system-bootcfg os menu-entries)))

  (when (eq? action 'reconfigure)
    (maybe-suggest-running-guix-pull)
    (check-forward-update validate-reconfigure))

  ;; Check whether the declared file systems exist.  This is better than
  ;; instantiating a broken configuration.  Assume that we can only check if
  ;; running as root.
  (when (and (not skip-safety-checks?)
             (memq action '(init reconfigure)))
    (check-mapped-devices os)
    (when (zero? (getuid))
      (check-file-system-availability (operating-system-file-systems os))
      (check-initrd-modules os)))

  (mlet* %store-monad
      ((sys       (system-derivation-for-action image action
                                                #:full-boot? full-boot?
                                                #:container-shared-network? container-shared-network?
                                                #:mappings mappings))

       ;; For 'init' and 'reconfigure', always build BOOTCFG, even if
       ;; --no-bootloader is passed, because we then use it as a GC root.
       ;; See <http://bugs.gnu.org/21068>.
       (drvs      (mapm/accumulate-builds lower-object
                                          (if (memq action '(init reconfigure))
                                              (list sys bootcfg)
                                              (list sys))))
       (%         (if derivations-only?
                      (return (for-each (compose println derivation-file-name)
                                        drvs))
                      (built-derivations drvs))))

    (if (or dry-run? derivations-only?)
        (return #f)
        (begin
          (for-each (compose println derivation->output-path)
                    drvs)

          (case action
            ((reconfigure)
             (newline)
             (format #t (G_ "activating system...~%"))
             (mbegin %store-monad
               (switch-to-system local-eval os)
               (mwhen install-bootloader?
                 (install-bootloader local-eval bootloader bootcfg
                                     #:target (or target "/"))
                 (return
                  (info (G_ "bootloader successfully installed on '~a'~%")
                        (bootloader-configuration-target bootloader))))
               (with-shepherd-error-handling
                 (upgrade-shepherd-services local-eval os)
                 (return (format #t (G_ "\
To complete the upgrade, run 'herd restart SERVICE' to stop,
upgrade, and restart each service that was not automatically restarted.\n")))
                 (return (format #t (G_ "\
Run 'herd status' to view the list of services on your system.\n"))))))
            ((init)
             (newline)
             (format #t (G_ "initializing operating system under '~a'...~%")
                     target)
             (install sys (canonicalize-path target)
                      #:install-bootloader? install-bootloader?
                      #:bootloader bootloader
                      #:bootcfg bootcfg))
            (else
             ;; All we had to do was to build SYS and maybe register an
             ;; indirect GC root.
             (let ((output (derivation->output-path sys)))
               (mbegin %store-monad
                 (mwhen gc-root
                   (register-root* (list output) gc-root))
                 (return output)))))))))

(define (lookup-backend name)                     ;TODO: factorize
  "Return the graph backend called NAME.  Raise an error if it is not found."
  (or (find (lambda (backend)
              (string=? (graph-backend-name backend) name))
            %graph-backends)
      (leave (G_ "~a: unknown backend~%") name)))

(define* (export-extension-graph os port
                                 #:key (backend (lookup-backend "graphviz")))
  "Export the service extension graph of OS to PORT using BACKEND."
  (let* ((services (operating-system-services os))
         (system   (find (lambda (service)
                           (eq? (service-kind service) system-service-type))
                         services)))
    (export-graph (list system) (current-output-port)
                  #:backend backend
                  #:node-type (service-node-type services)
                  #:reverse-edges? #t)))

(define* (export-shepherd-graph os port
                                #:key (backend (lookup-backend "graphviz")))
  "Export the graph of shepherd services of OS to PORT using BACKEND."
  (let* ((services  (operating-system-services os))
         (pid1      (fold-services services
                                   #:target-type shepherd-root-service-type))
         ;; Get the list of <shepherd-service>.
         (shepherds (shepherd-configuration-services (service-value pid1)))
         (sinks     (filter (lambda (service)
                              (null? (shepherd-service-requirement service)))
                            shepherds)))
    (export-graph sinks (current-output-port)
                  #:backend backend
                  #:node-type (shepherd-service-node-type shepherds)
                  #:reverse-edges? #t)))


;;;
;;; Images.
;;;

(define (list-image-types)
  "Print the available image types."
  (display (G_ "The available image types are:\n"))
  (newline)
  (format #t "~{   - ~a ~%~}" (map image-type-name (force %image-types))))


;;;
;;; Options.
;;;

(define (show-help)
  (display (G_ "Usage: guix system [OPTION ...] ACTION [ARG ...] [FILE]
Build the operating system declared in FILE according to ACTION.
Some ACTIONS support additional ARGS.\n"))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   search           search for existing service types\n"))
  (display (G_ "\
   reconfigure      switch to a new operating system configuration\n"))
  (display (G_ "\
   roll-back        switch to the previous operating system configuration\n"))
  (display (G_ "\
   describe         describe the current system\n"))
  (display (G_ "\
   list-generations list the system generations\n"))
  (display (G_ "\
   switch-generation switch to an existing operating system configuration\n"))
  (display (G_ "\
   delete-generations delete old system generations\n"))
  (display (G_ "\
   build            build the operating system without installing anything\n"))
  (display (G_ "\
   container        build a container that shares the host's store\n"))
  (display (G_ "\
   vm               build a virtual machine image that shares the host's store\n"))
  (display (G_ "\
   image            build a Guix System image\n"))
  (display (G_ "\
   docker-image     build a Docker image\n"))
  (display (G_ "\
   init             initialize a root file system to run GNU\n"))
  (display (G_ "\
   extension-graph  emit the service extension graph in Dot format\n"))
  (display (G_ "\
   shepherd-graph   emit the graph of shepherd services in Dot format\n"))

  (show-build-options-help)
  (display (G_ "
  -d, --derivation       return the derivation of the given system"))
  (display (G_ "
  -e, --expression=EXPR  consider the operating-system EXPR evaluates to
                         instead of reading FILE, when applicable"))
  (display (G_ "
      --allow-downgrades for 'reconfigure', allow downgrades to earlier
                         channel revisions"))
  (display (G_ "
      --on-error=STRATEGY
                         apply STRATEGY (one of nothing-special, backtrace,
                         or debug) when an error occurs while reading FILE"))
  (display (G_ "
      --list-image-types list available image types"))
  (display (G_ "
  -t, --image-type=TYPE  for 'image', produce an image of TYPE"))
  (display (G_ "
      --image-size=SIZE  for 'image', produce an image of SIZE"))
  (display (G_ "
      --no-bootloader    for 'init', do not install a bootloader"))
  (display (G_ "
      --volatile         for 'image', make the root file system volatile"))
  (display (G_ "
      --label=LABEL      for 'image', label disk image with LABEL"))
  (display (G_ "
      --save-provenance  save provenance information"))
  (display (G_ "
      --share=SPEC       for 'vm' and 'container', share host file system with
                         read/write access according to SPEC"))
  (display (G_ "
      --expose=SPEC      for 'vm' and 'container', expose host file system
                         directory as read-only according to SPEC"))
  (display (G_ "
  -N, --network          for 'container', allow containers to access the network"))
  (display (G_ "
  -r, --root=FILE        for 'vm', 'image', 'container' and 'build',
                         make FILE a symlink to the result, and
                         register it as a garbage collector root"))
  (display (G_ "
      --full-boot        for 'vm', make a full boot sequence"))
  (display (G_ "
      --skip-checks      skip file system and initrd module safety checks"))
  (display (G_ "
      --target=TRIPLET   cross-build for TRIPLET--e.g., \"armel-linux-gnu\""))
  (display (G_ "
  -v, --verbosity=LEVEL  use the given verbosity LEVEL"))
  (newline)
  (display (G_ "
      --graph-backend=BACKEND
                         use BACKEND for 'extension-graphs' and 'shepherd-graph'"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specifications of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix system")))
         (option '(#\e "expression") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'expression arg result)))
         (option '(#\d "derivation") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'derivations-only? #t result)))
         (option '("allow-downgrades") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'validate-reconfigure
                               warn-about-backward-reconfigure
                               result)))
         (option '("on-error") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'on-error (string->symbol arg)
                               result)))
         (option '(#\t "image-type") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'image-type (string->symbol arg)
                               result)))
         (option '("list-image-types") #f #f
                 (lambda (opt name arg result)
                   (list-image-types)
                   (exit 0)))
         (option '("image-size") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'image-size (size->number arg)
                               result)))
         (option '(#\N "network") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'container-shared-network? #t result)))
         (option '("no-bootloader" "no-grub") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'install-bootloader? #f result)))
         (option '("volatile") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'volatile-root? #t result)))
         (option '("label") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'label arg result)))
         (option '("full-boot") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'full-boot? #t result)))
         (option '("save-provenance") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'save-provenance? #t result)))
         (option '("skip-checks") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'skip-safety-checks? #t result)))

         (option '("share") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'file-system-mapping
                               (specification->file-system-mapping arg #t)
                               result)))
         (option '("expose") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'file-system-mapping
                               (specification->file-system-mapping arg #f)
                               result)))

         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t result)))
         (option '(#\v "verbosity") #t #f
                 (lambda (opt name arg result)
                   (let ((level (string->number* arg)))
                     (alist-cons 'verbosity level
                                 (alist-delete 'verbosity result)))))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg
                               (alist-delete 'system result eq?))))
         (option '("target") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'target arg
                               (alist-delete 'target result eq?))))
         (option '(#\r "root") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'gc-root arg result)))
         (option '("graph-backend") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'graph-backend arg result)))
         %standard-build-options))

(define %default-options
  ;; Alist of default option values.
  `((system . ,(%current-system))
    (target . #f)
    (substitutes? . #t)
    (offload? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (graft? . #t)
    (debug . 0)
    (verbosity . #f)                              ;default
    (validate-reconfigure . ,ensure-forward-reconfigure)
    (image-type . efi-raw)
    (image-size . guess)
    (install-bootloader? . #t)
    (label . #f)
    (volatile-root? . #f)
    (graph-backend . "graphviz")))

(define (verbosity-level opts)
  "Return the verbosity level based on OPTS, the alist of parsed options."
  (or (assoc-ref opts 'verbosity)
      (if (eq? (assoc-ref opts 'action) 'build)
          2 1)))


;;;
;;; Entry point.
;;;

(define (process-action action args opts)
  "Process ACTION, a sub-command, with the arguments are listed in ARGS.
ACTION must be one of the sub-commands that takes an operating system
declaration as an argument (a file name.)  OPTS is the raw alist of options
resulting from command-line parsing."
  (define (ensure-operating-system-or-image file-or-exp obj)
    (unless (or (operating-system? obj) (image? obj))
      (leave (G_ "'~a' does not return an operating system or an image~%")
             file-or-exp))
    obj)

  (define save-provenance?
    (or (assoc-ref opts 'save-provenance?)
        (memq action '(init reconfigure))))

  (let* ((file        (match args
                        (() #f)
                        ((x . _) x)))
         (expr        (assoc-ref opts 'expression))
         (system      (assoc-ref opts 'system))
         (target      (assoc-ref opts 'target))
         (transform   (lambda (obj)
                        (if (and save-provenance? (operating-system? obj))
                            (operating-system-with-provenance obj file)
                            obj)))
         (obj          (transform
                        (ensure-operating-system-or-image
                         (or file expr)
                         (cond
                          ((and expr file)
                           (leave
                            (G_ "both file and expression cannot be specified~%")))
                          (expr
                           (read/eval expr))
                          (file
                           (load* file %user-module
                                  #:on-error (assoc-ref opts 'on-error)))
                          (else
                           (leave (G_ "no configuration specified~%")))))))
         (dry?        (assoc-ref opts 'dry-run?))
         (bootloader? (assoc-ref opts 'install-bootloader?))
         (label       (assoc-ref opts 'label))
         (image-type  (lookup-image-type-by-name
                       (assoc-ref opts 'image-type)))
         (image       (let* ((image-type (if (eq? action 'vm-image)
                                            qcow2-image-type
                                            image-type))
                            (image-size (assoc-ref opts 'image-size))
                            (volatile?  (assoc-ref opts 'volatile-root?))
                            (base-image (if (operating-system? obj)
                                            (os->image obj
                                                       #:type image-type)
                                            obj))
                            (base-target (image-target base-image)))
                        (image
                         (inherit (if label
                                      (image-with-label base-image label)
                                      base-image))
                         (target (or base-target target))
                         (size image-size)
                         (volatile-root? volatile?))))
         (os          (image-operating-system image))
         (target-file (match args
                        ((first second) second)
                        (_ #f)))
         (bootloader-target
                      (and bootloader?
                           (bootloader-configuration-target
                            (operating-system-bootloader os)))))

    (define (graph-backend)
      (lookup-backend (assoc-ref opts 'graph-backend)))

    (with-store store
      (set-build-options-from-command-line store opts)

      (with-build-handler (build-notifier #:use-substitutes?
                                          (assoc-ref opts 'substitutes?)
                                          #:verbosity
                                          (verbosity-level opts)
                                          #:dry-run?
                                          (assoc-ref opts 'dry-run?))
        (run-with-store store
          (mbegin %store-monad
            (set-guile-for-build (default-guile))
            (case action
              ((extension-graph)
               (export-extension-graph os (current-output-port)
                                       #:backend (graph-backend)))
              ((shepherd-graph)
               (export-shepherd-graph os (current-output-port)
                                      #:backend (graph-backend)))
              (else
               (unless (memq action '(build init))
                 (warn-about-old-distro #:suggested-command
                                        "guix system reconfigure"))

               (perform-action action image
                               #:dry-run? dry?
                               #:derivations-only? (assoc-ref opts
                                                              'derivations-only?)
                               #:use-substitutes? (assoc-ref opts 'substitutes?)
                               #:skip-safety-checks?
                               (assoc-ref opts 'skip-safety-checks?)
                               #:validate-reconfigure
                               (assoc-ref opts 'validate-reconfigure)
                               #:full-boot? (assoc-ref opts 'full-boot?)
                               #:container-shared-network?
                               (assoc-ref opts 'container-shared-network?)
                               #:mappings (filter-map (match-lambda
                                                        (('file-system-mapping . m)
                                                         m)
                                                        (_ #f))
                                                      opts)
                               #:install-bootloader? bootloader?
                               #:target target-file
                               #:bootloader-target bootloader-target
                               #:gc-root (assoc-ref opts 'gc-root)))))
          #:target target
          #:system system)))
    (warn-about-disk-space)))

(define (resolve-subcommand name)
  (let ((module (resolve-interface
                 `(guix scripts system ,(string->symbol name))))
        (proc (string->symbol (string-append "guix-system-" name))))
    (module-ref module proc)))

(define (process-command command args opts)
  "Process COMMAND, one of the 'guix system' sub-commands.  ARGS is its
argument list and OPTS is the option alist."
  (define-syntax-rule (with-store* store exp ...)
    (with-store store
      (set-build-options-from-command-line store opts)
      exp ...))

  (case command
    ;; The following commands do not need to use the store, and they do not need
    ;; an operating system configuration file.
    ((list-generations)
     (let ((pattern (match args
                      (() #f)
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (list-generations pattern)))
    ((describe)
     (match (generation-number %system-profile)
       (0
        (error (G_ "no system generation, nothing to describe~%")))
       (generation
        (display-system-generation generation))))
    ((search)
     (apply (resolve-subcommand "search") args))
    ;; The following commands need to use the store, but they do not need an
    ;; operating system configuration file.
    ((delete-generations)
     (let ((pattern (match args
                      (() #f)
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store* store
         (delete-matching-generations store %system-profile pattern)
         (reinstall-bootloader store (generation-number %system-profile)))))
    ((switch-generation)
     (let ((pattern (match args
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store* store
         (switch-to-system-generation store pattern))))
    ((roll-back)
     (let ((pattern (match args
                      (() "")
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store* store
         (roll-back-system store))))
    ;; The following commands need to use the store, and they also
    ;; need an operating system configuration file.
    (else (process-action command args opts))))

(define-command (guix-system . args)
  (synopsis "build and deploy full operating systems")

  (define (parse-sub-command arg result)
    ;; Parse sub-command ARG and augment RESULT accordingly.
    (if (assoc-ref result 'action)
        (alist-cons 'argument arg result)
        (let ((action (string->symbol arg)))
          (case action
            ((build container vm vm-image image disk-image reconfigure init
              extension-graph shepherd-graph
              list-generations describe
              delete-generations roll-back
              switch-generation search docker-image)
             (alist-cons 'action action result))
            (else (leave (G_ "~a: unknown action~%") action))))))

  (define (match-pair car)
    ;; Return a procedure that matches a pair with CAR.
    (match-lambda
      ((head . tail)
       (and (eq? car head) tail))
      (_ #f)))

  (define (option-arguments opts)
    ;; Extract the plain arguments from OPTS.
    (let* ((args   (reverse (filter-map (match-pair 'argument) opts)))
           (count  (length args))
           (action (assoc-ref opts 'action))
           (expr   (assoc-ref opts 'expression)))
      (define (fail)
        (leave (G_ "wrong number of arguments for action '~a'~%")
               action))

      (unless action
        (format (current-error-port)
                (G_ "guix system: missing command name~%"))
        (format (current-error-port)
                (G_ "Try 'guix system --help' for more information.~%"))
        (exit 1))

      (case action
        ((build container vm vm-image image disk-image docker-image
                reconfigure)
         (unless (or (= count 1)
                     (and expr (= count 0)))
           (fail)))
        ((init)
         (unless (= count 2)
           (fail))))
      args))

  (with-error-handling
    (let* ((opts     (parse-command-line args %options
                                         (list %default-options)
                                         #:argument-handler
                                         parse-sub-command))
           (args     (option-arguments opts))
           (command  (assoc-ref opts 'action)))
      (parameterize ((%graft? (assoc-ref opts 'graft?)))
        (with-status-verbosity (verbosity-level opts)
          (process-command command args opts))))))

;;; Local Variables:
;;; eval: (put 'with-store* 'scheme-indent-function 1)
;;; End:

;;; system.scm ends here
