;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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
  #:use-module (guix store)
  #:use-module (guix grafts)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix profiles)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module (guix graph)
  #:use-module (guix scripts graph)
  #:use-module (guix build utils)
  #:use-module (gnu build install)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system linux-container)
  #:use-module (gnu system vm)
  #:use-module (gnu system grub)
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


(define* (copy-item item target
                    #:key (log-port (current-error-port)))
  "Copy ITEM to the store under root directory TARGET and register it."
  (mlet* %store-monad ((refs (references* item)))
    (let ((dest  (string-append target item))
          (state (string-append target "/var/guix")))
      (format log-port "copying '~a'...~%" item)

      ;; Remove DEST if it exists to make sure that (1) we do not fail badly
      ;; while trying to overwrite it (see <http://bugs.gnu.org/20722>), and
      ;; (2) we end up with the right contents.
      (when (file-exists? dest)
        (delete-file-recursively dest))

      (copy-recursively item dest
                        #:log (%make-void-port "w"))

      ;; Register ITEM; as a side-effect, it resets timestamps, etc.
      ;; Explicitly use "TARGET/var/guix" as the state directory, to avoid
      ;; reproducing the user's current settings; see
      ;; <http://bugs.gnu.org/18049>.
      (unless (register-path item
                             #:prefix target
                             #:state-directory state
                             #:references refs)
        (leave (G_ "failed to register '~a' under '~a'~%")
               item target))

      (return #t))))

(define* (copy-closure item target
                       #:key (log-port (current-error-port)))
  "Copy ITEM and all its dependencies to the store under root directory
TARGET, and register them."
  (mlet* %store-monad ((refs    (references* item))
                       (to-copy (topologically-sorted*
                                 (delete-duplicates (cons item refs)
                                                    string=?))))
    (sequence %store-monad
              (map (cut copy-item <> target #:log-port log-port)
                   to-copy))))

(define (install-grub* grub.cfg device target)
  "This is a variant of 'install-grub' with error handling, lifted in
%STORE-MONAD"
  (let* ((gc-root      (string-append target %gc-roots-directory
                                      "/grub.cfg"))
         (temp-gc-root (string-append gc-root ".new"))
         (delete-file  (lift1 delete-file %store-monad))
         (make-symlink (lift2 switch-symlinks %store-monad))
         (rename       (lift2 rename-file %store-monad)))
    (mbegin %store-monad
      ;; Prepare the symlink to GRUB.CFG to make sure that it's a GC root when
      ;; 'install-grub' completes (being a bit paranoid.)
      (make-symlink temp-gc-root grub.cfg)

      (munless (false-if-exception (install-grub grub.cfg device target))
        (delete-file temp-gc-root)
        (leave (G_ "failed to install GRUB on device '~a'~%") device))

      ;; Register GRUB.CFG as a GC root so that its dependencies (background
      ;; image, font, etc.) are not reclaimed.
      (rename temp-gc-root gc-root))))

(define* (install os-drv target
                  #:key (log-port (current-output-port))
                  grub? grub.cfg device)
  "Copy the closure of GRUB.CFG, which includes the output of OS-DRV, to
directory TARGET.  TARGET must be an absolute directory name since that's what
'guix-register' expects.

When GRUB? is true, install GRUB on DEVICE, using GRUB.CFG."
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

  (chmod target #o755)
  (let ((os-dir   (derivation->output-path os-drv))
        (format   (lift format %store-monad))
        (populate (lift2 populate-root-file-system %store-monad)))

    (mbegin %store-monad
      ;; Copy the closure of GRUB.CFG, which includes OS-DIR, GRUB's
      ;; background image and so on.
      (maybe-copy grub.cfg)

      ;; Create a bunch of additional files.
      (format log-port "populating '~a'...~%" target)
      (populate os-dir target)

      (mwhen grub?
        (install-grub* grub.cfg device target)))))


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
          (values (run-with-store store (begin mbody ...))
                  store)))
      (lambda (key proc format-string format-args errno . rest)
        (warning (G_ "while talking to shepherd: ~a~%")
                 (apply format #f format-string format-args))
        (values #f store)))))

(define (report-shepherd-error error)
  "Report ERROR, a '&shepherd-error' error condition object."
  (cond ((service-not-found-error? error)
         (report-error (G_ "service '~a' could not be found~%")
                       (service-not-found-error-service error)))
        ((action-not-found-error? error)
         (report-error (G_ "service '~a' does not have an action '~a'~%")
                       (action-not-found-error-service error)
                       (action-not-found-error-action error)))
        ((action-exception-error? error)
         (report-error (G_ "exception caught while executing '~a' \
on service '~a':~%")
                       (action-exception-error-action error)
                       (action-exception-error-service error))
         (print-exception (current-error-port) #f
                          (action-exception-error-key error)
                          (action-exception-error-arguments error)))
        ((unknown-shepherd-error? error)
         (report-error (G_ "something went wrong: ~s~%")
                       (unknown-shepherd-error-sexp error)))
        ((shepherd-error? error)
         (report-error (G_ "shepherd error~%")))
        ((not error)                              ;not an error
         #t)))

(define (call-with-service-upgrade-info new-services mproc)
  "Call MPROC, a monadic procedure in %STORE-MONAD, passing it the list of
names of services to load (upgrade), and the list of names of services to
unload."
  (match (current-services)
    ((services ...)
     (let-values (((to-unload to-load)
                   (shepherd-service-upgrade services new-services)))
       (mproc to-load
              (map (compose first live-service-provision)
                   to-unload))))
    (#f
     (with-monad %store-monad
       (warning (G_ "failed to obtain list of shepherd services~%"))
       (return #f)))))

(define (upgrade-shepherd-services os)
  "Upgrade the Shepherd (PID 1) by unloading obsolete services and loading new
services specified in OS and not currently running.

This is currently very conservative in that it does not stop or unload any
running service.  Unloading or stopping the wrong service ('udev', say) could
bring the system down."
  (define new-services
    (service-value
     (fold-services (operating-system-services os)
                    #:target-type shepherd-root-service-type)))

  ;; Arrange to simply emit a warning if the service upgrade fails.
  (with-shepherd-error-handling
   (call-with-service-upgrade-info new-services
     (lambda (to-load to-unload)
        (for-each (lambda (unload)
                    (info (G_ "unloading service '~a'...~%") unload)
                    (unload-service unload))
                  to-unload)

        (with-monad %store-monad
          (munless (null? to-load)
            (let ((to-load-names  (map shepherd-service-canonical-name to-load))
                  (to-start       (filter shepherd-service-auto-start? to-load)))
              (info (G_ "loading new services:~{ ~a~}...~%") to-load-names)
              (mlet %store-monad ((files (mapm %store-monad shepherd-service-file
                                               to-load)))
                ;; Here we assume that FILES are exactly those that were computed
                ;; as part of the derivation that built OS, which is normally the
                ;; case.
                (load-services (map derivation->output-path files))

                (for-each start-service
                          (map shepherd-service-canonical-name to-start))
                (return #t)))))))))

(define* (switch-to-system os
                           #:optional (profile %system-profile))
  "Make a new generation of PROFILE pointing to the directory of OS, switch to
it atomically, and then run OS's activation script."
  (mlet* %store-monad ((drv    (operating-system-derivation os))
                       (script (operating-system-activation-script os)))
    (let* ((system     (derivation->output-path drv))
           (number     (+ 1 (generation-number profile)))
           (generation (generation-file-name profile number)))
      (switch-symlinks generation system)
      (switch-symlinks profile generation)

      (format #t (G_ "activating system...~%"))

      ;; The activation script may change $PATH, among others, so protect
      ;; against that.
      (save-environment-excursion
       ;; Tell 'activate-current-system' what the new system is.
       (setenv "GUIX_NEW_SYSTEM" system)

       ;; The activation script may modify '%load-path' & co., so protect
       ;; against that.  This is necessary to ensure that
       ;; 'upgrade-shepherd-services' gets to see the right modules when it
       ;; computes derivations with 'gexp->derivation'.
       (save-load-path-excursion
        (primitive-load (derivation->output-path script))))

      ;; Finally, try to update system services.
      (upgrade-shepherd-services os))))

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
                                  (numbers (generation-numbers profile)))
  "Return a list of 'menu-entry' for the generations of PROFILE specified by
NUMBERS, which is a list of generation numbers."
  (define (system->boot-parameters system number time)
    (unless-file-not-found
     (let* ((params           (read-boot-parameters-file system)))
       params)))
  (let* ((systems (map (cut generation-file-name profile <>)
                       numbers))
         (times   (map (lambda (system)
                         (unless-file-not-found
                          (stat:mtime (lstat system))))
                       systems)))
    (filter-map system->boot-parameters systems numbers times)))

(define* (profile-grub-entries #:optional (profile %system-profile)
                                  (numbers (generation-numbers profile)))
  "Return a list of 'menu-entry' for the generations of PROFILE specified by
NUMBERS, which is a list of generation numbers."
  (define (system->grub-entry system number time)
    (unless-file-not-found
     (let* ((params           (read-boot-parameters-file system))
            (label            (boot-parameters-label params))
            (root             (boot-parameters-root-device params))
            (root-device      (if (bytevector? root)
                                  (uuid->string root)
                                  root))
            (kernel           (boot-parameters-kernel params))
            (kernel-arguments (boot-parameters-kernel-arguments params))
            (initrd           (boot-parameters-initrd params)))
       (menu-entry
        (label (string-append label " (#"
                              (number->string number) ", "
                              (seconds->string time) ")"))
        (device (boot-parameters-store-device params))
        (device-mount-point (boot-parameters-store-mount-point params))
        (linux kernel)
        (linux-arguments
         (cons* (string-append "--root=" root-device)
                (string-append "--system=" system)
                (string-append "--load=" system "/boot")
                kernel-arguments))
        (initrd initrd)))))

  (let* ((systems (map (cut generation-file-name profile <>)
                       numbers))
         (times   (map (lambda (system)
                         (unless-file-not-found
                          (stat:mtime (lstat system))))
                       systems)))
    (filter-map system->grub-entry systems numbers times)))


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
re-install grub with a grub configuration file that uses the specified system
generation as its default entry.  STORE is an open connection to the store."
  (let ((number (relative-generation-spec->number %system-profile spec)))
    (if number
        (begin
          (reinstall-grub store number)
          (switch-to-generation* %system-profile number))
        (leave (G_ "cannot switch to system generation '~a'~%") spec))))

(define (reinstall-grub store number)
  "Re-install grub for existing system profile generation NUMBER.  STORE is an
open connection to the store."
  (let* ((generation (generation-file-name %system-profile number))
         (params (unless-file-not-found
                  (read-boot-parameters-file generation)))
         (root-device (boot-parameters-root-device params))
         ;; We don't currently keep track of past menu entries' details.  The
         ;; default values will allow the system to boot, even if they differ
         ;; from the actual past values for this generation's entry.
         (grub-config (grub-configuration (device root-device)))
         ;; Make the specified system generation the default entry.
         (entries (profile-grub-entries %system-profile (list number)))
         (old-generations (delv number (generation-numbers %system-profile)))
         (old-entries (profile-grub-entries %system-profile old-generations))
         (grub.cfg (run-with-store store
                     (grub-configuration-file grub-config
                                              entries
                                              #:old-entries old-entries))))
    (show-what-to-build store (list grub.cfg))
    (build-derivations store (list grub.cfg))
    ;; This is basically the same as install-grub*, but for now we avoid
    ;; re-installing the GRUB boot loader itself onto a device, mainly because
    ;; we don't in general have access to the same version of the GRUB package
    ;; which was used when installing this other system generation.
    (let* ((grub.cfg-path (derivation->output-path grub.cfg))
           (gc-root (string-append %gc-roots-directory "/grub.cfg"))
           (temp-gc-root (string-append gc-root ".new")))
      (switch-symlinks temp-gc-root grub.cfg-path)
      (unless (false-if-exception (install-grub-config grub.cfg-path "/"))
        (delete-file temp-gc-root)
        (leave (G_ "failed to re-install GRUB configuration file: '~a'~%")
               grub.cfg-path))
      (rename-file temp-gc-root gc-root))))


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
  (unless (zero? number)
    (let* ((generation  (generation-file-name profile number))
           (params      (read-boot-parameters-file generation))
           (label       (boot-parameters-label params))
           (root        (boot-parameters-root-device params))
           (root-device (if (bytevector? root)
                            (uuid->string root)
                            root))
           (kernel      (boot-parameters-kernel params)))
      (display-generation profile number)
      (format #t (G_ "  file name: ~a~%") generation)
      (format #t (G_ "  canonical file name: ~a~%") (readlink* generation))
      ;; TRANSLATORS: Please preserve the two-space indentation.
      (format #t (G_ "  label: ~a~%") label)
      (format #t (G_ "  root device: ~a~%") root-device)
      (format #t (G_ "  kernel: ~a~%") kernel))))

(define* (list-generations pattern #:optional (profile %system-profile))
  "Display in a human-readable format all the system generations matching
PATTERN, a string.  When PATTERN is #f, display all the system generations."
  (cond ((not (file-exists? profile))             ; XXX: race condition
         (raise (condition (&profile-not-found-error
                            (profile profile)))))
        ((string-null? pattern)
         (for-each display-system-generation (profile-generations profile)))
        ((matching-generations pattern profile)
         =>
         (lambda (numbers)
           (if (null-list? numbers)
               (exit 1)
               (leave-on-EPIPE
                (for-each display-system-generation numbers)))))
        (else
         (leave (G_ "invalid syntax: ~a~%") pattern))))


;;;
;;; Action.
;;;

(define* (system-derivation-for-action os action
                                       #:key image-size full-boot? mappings)
  "Return as a monadic value the derivation for OS according to ACTION."
  (case action
    ((build init reconfigure)
     (operating-system-derivation os))
    ((container)
     (container-script os #:mappings mappings))
    ((vm-image)
     (system-qemu-image os #:disk-image-size image-size))
    ((vm)
     (system-qemu-image/shared-store-script os
                                            #:full-boot? full-boot?
                                            #:disk-image-size
                                            (if full-boot?
                                                image-size
                                                (* 30 (expt 2 20)))
                                            #:mappings mappings))
    ((disk-image)
     (system-disk-image os #:disk-image-size image-size))))

(define (maybe-suggest-running-guix-pull)
  "Suggest running 'guix pull' if this has never been done before."
  ;; The reason for this is that the 'guix' binding that we see here comes
  ;; from either ~/.config/latest or, if it's missing, from the
  ;; globally-installed Guix, which is necessarily older.  See
  ;; <http://lists.gnu.org/archive/html/guix-devel/2014-08/msg00057.html> for
  ;; a discussion.
  (define latest
    (string-append (config-directory) "/latest"))

  (unless (file-exists? latest)
    (warning (G_ "~a not found: 'guix pull' was never run~%") latest)
    (warning (G_ "Consider running 'guix pull' before 'reconfigure'.~%"))
    (warning (G_ "Failing to do that may downgrade your system!~%"))))

(define* (perform-action action os
                         #:key bootloader? dry-run? derivations-only?
                         use-substitutes? device target
                         image-size full-boot?
                         (mappings '())
                         (gc-root #f))
  "Perform ACTION for OS.  GRUB? specifies whether to install GRUB; DEVICE is
the target devices for GRUB; TARGET is the target root directory; IMAGE-SIZE
is the size of the image to be built, for the 'vm-image' and 'disk-image'
actions.  FULL-BOOT? is used for the 'vm' action; it determines whether to
boot directly to the kernel or to the bootloader.

When DERIVATIONS-ONLY? is true, print the derivation file name(s) without
building anything.

When GC-ROOT is a path, also make that path an indirect root of the build
output when building a system derivation, such as a disk image."
  (define println
    (cut format #t "~a~%" <>))

  (when (eq? action 'reconfigure)
    (maybe-suggest-running-guix-pull))

  (mlet* %store-monad
      ((sys       (system-derivation-for-action os action
                                                #:image-size image-size
                                                #:full-boot? full-boot?
                                                #:mappings mappings))
       (grub      (package->derivation (grub-configuration-grub
                                        (operating-system-bootloader os))))
       (grub.cfg  (if (eq? 'container action)
                      (return #f)
                      (operating-system-bootcfg os
                                                (if (eq? 'init action)
                                                    '()
                                                    (profile-grub-entries)))))

       ;; For 'init' and 'reconfigure', always build GRUB.CFG, even if
       ;; --no-grub is passed, because GRUB.CFG because we then use it as a GC
       ;; root.  See <http://bugs.gnu.org/21068>.
       (drvs   -> (if (memq action '(init reconfigure))
                      (if bootloader?
                          (list sys grub.cfg grub)
                          (list sys grub.cfg))
                      (list sys)))
       (%         (if derivations-only?
                      (return (for-each (compose println derivation-file-name)
                                        drvs))
                      (maybe-build drvs #:dry-run? dry-run?
                                   #:use-substitutes? use-substitutes?))))

    (if (or dry-run? derivations-only?)
        (return #f)
        (begin
          (for-each (compose println derivation->output-path)
                    drvs)

          ;; Make sure GRUB is accessible.
          (when bootloader?
            (let ((prefix (derivation->output-path grub)))
              (setenv "PATH"
                      (string-append  prefix "/bin:" prefix "/sbin:"
                                      (getenv "PATH")))))

          (case action
            ((reconfigure)
             (mbegin %store-monad
               (switch-to-system os)
               (mwhen bootloader?
                 (install-grub* (derivation->output-path grub.cfg)
                                device "/"))))
            ((init)
             (newline)
             (format #t (G_ "initializing operating system under '~a'...~%")
                     target)
             (install sys (canonicalize-path target)
                      #:grub? bootloader?
                      #:grub.cfg (derivation->output-path grub.cfg)
                      #:device device))
            (else
             ;; All we had to do was to build SYS and maybe register an
             ;; indirect GC root.
             (let ((output (derivation->output-path sys)))
               (mbegin %store-monad
                 (mwhen gc-root
                   (register-root* (list output) gc-root))
                 (return output)))))))))

(define (export-extension-graph os port)
  "Export the service extension graph of OS to PORT."
  (let* ((services (operating-system-services os))
         (system   (find (lambda (service)
                           (eq? (service-kind service) system-service-type))
                         services)))
    (export-graph (list system) (current-output-port)
                  #:node-type (service-node-type services)
                  #:reverse-edges? #t)))

(define (export-shepherd-graph os port)
  "Export the graph of shepherd services of OS to PORT."
  (let* ((services  (operating-system-services os))
         (pid1      (fold-services services
                                   #:target-type shepherd-root-service-type))
         (shepherds (service-value pid1))         ;list of <shepherd-service>
         (sinks     (filter (lambda (service)
                              (null? (shepherd-service-requirement service)))
                            shepherds)))
    (export-graph sinks (current-output-port)
                  #:node-type (shepherd-service-node-type shepherds)
                  #:reverse-edges? #t)))


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
   reconfigure      switch to a new operating system configuration\n"))
  (display (G_ "\
   roll-back        switch to the previous operating system configuration\n"))
  (display (G_ "\
   switch-generation switch to an existing operating system configuration\n"))
  (display (G_ "\
   list-generations list the system generations\n"))
  (display (G_ "\
   build            build the operating system without installing anything\n"))
  (display (G_ "\
   container        build a container that shares the host's store\n"))
  (display (G_ "\
   vm               build a virtual machine image that shares the host's store\n"))
  (display (G_ "\
   vm-image         build a freestanding virtual machine image\n"))
  (display (G_ "\
   disk-image       build a disk image, suitable for a USB stick\n"))
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
      --on-error=STRATEGY
                         apply STRATEGY when an error occurs while reading FILE"))
  (display (G_ "
      --image-size=SIZE  for 'vm-image', produce an image of SIZE"))
  (display (G_ "
      --no-bootloader    for 'init', do not install a bootloader"))
  (display (G_ "
      --share=SPEC       for 'vm', share host file system according to SPEC"))
  (display (G_ "
  -r, --root=FILE        for 'vm', 'vm-image', 'disk-image', 'container',
                         and 'build', make FILE a symlink to the result, and
                         register it as a garbage collector root"))
  (display (G_ "
      --expose=SPEC      for 'vm', expose host file system according to SPEC"))
  (display (G_ "
      --full-boot        for 'vm', make a full boot sequence"))
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
         (option '(#\d "derivation") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'derivations-only? #t result)))
         (option '("on-error") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'on-error (string->symbol arg)
                               result)))
         (option '("image-size") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'image-size (size->number arg)
                               result)))
         (option '("no-bootloader" "no-grub") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'install-bootloader? #f result)))
         (option '("full-boot") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'full-boot? #t result)))

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
                   (alist-cons 'dry-run? #t (alist-cons 'graft? #f result))))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg
                               (alist-delete 'system result eq?))))
         (option '(#\r "root") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'gc-root arg result)))
         %standard-build-options))

(define %default-options
  ;; Alist of default option values.
  `((system . ,(%current-system))
    (substitutes? . #t)
    (graft? . #t)
    (build-hook? . #t)
    (max-silent-time . 3600)
    (verbosity . 0)
    (image-size . ,(* 900 (expt 2 20)))
    (install-bootloader? . #t)))


;;;
;;; Entry point.
;;;

(define (process-action action args opts)
  "Process ACTION, a sub-command, with the arguments are listed in ARGS.
ACTION must be one of the sub-commands that takes an operating system
declaration as an argument (a file name.)  OPTS is the raw alist of options
resulting from command-line parsing."
  (let* ((file        (match args
                        (() #f)
                        ((x . _) x)))
         (system      (assoc-ref opts 'system))
         (os          (if file
                          (load* file %user-module
                                 #:on-error (assoc-ref opts 'on-error))
                          (leave (G_ "no configuration file specified~%"))))

         (dry?        (assoc-ref opts 'dry-run?))
         (bootloader? (assoc-ref opts 'install-bootloader?))
         (target      (match args
                        ((first second) second)
                        (_ #f)))
         (device      (and bootloader?
                           (grub-configuration-device
                            (operating-system-bootloader os)))))

    (with-store store
      (set-build-options-from-command-line store opts)

      (run-with-store store
        (mbegin %store-monad
          (set-guile-for-build (default-guile))
          (case action
            ((extension-graph)
             (export-extension-graph os (current-output-port)))
            ((shepherd-graph)
             (export-shepherd-graph os (current-output-port)))
            (else
             (perform-action action os
                             #:dry-run? dry?
                             #:derivations-only? (assoc-ref opts
                                                            'derivations-only?)
                             #:use-substitutes? (assoc-ref opts 'substitutes?)
                             #:image-size (assoc-ref opts 'image-size)
                             #:full-boot? (assoc-ref opts 'full-boot?)
                             #:mappings (filter-map (match-lambda
                                                      (('file-system-mapping . m)
                                                       m)
                                                      (_ #f))
                                                    opts)
                             #:bootloader? bootloader?
                             #:target target #:device device
                             #:gc-root (assoc-ref opts 'gc-root)))))
        #:system system))))

(define (process-command command args opts)
  "Process COMMAND, one of the 'guix system' sub-commands.  ARGS is its
argument list and OPTS is the option alist."
  (case command
    ;; The following commands do not need to use the store, and they do not need
    ;; an operating system configuration file.
    ((list-generations)
     (let ((pattern (match args
                      (() "")
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (list-generations pattern)))
    ;; The following commands need to use the store, but they do not need an
    ;; operating system configuration file.
    ((switch-generation)
     (let ((pattern (match args
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store store
         (set-build-options-from-command-line store opts)
         (switch-to-system-generation store pattern))))
    ((roll-back)
     (let ((pattern (match args
                      (() "")
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store store
         (set-build-options-from-command-line store opts)
         (roll-back-system store))))
    ;; The following commands need to use the store, and they also
    ;; need an operating system configuration file.
    (else (process-action command args opts))))

(define (guix-system . args)
  (define (parse-sub-command arg result)
    ;; Parse sub-command ARG and augment RESULT accordingly.
    (if (assoc-ref result 'action)
        (alist-cons 'argument arg result)
        (let ((action (string->symbol arg)))
          (case action
            ((build container vm vm-image disk-image reconfigure init
              extension-graph shepherd-graph list-generations roll-back
              switch-generation)
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
           (action (assoc-ref opts 'action)))
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
        ((build container vm vm-image disk-image reconfigure)
         (unless (= count 1)
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
        (process-command command args opts)))))

;;; Local Variables:
;;; eval: (put 'call-with-service-upgrade-info 'scheme-indent-function 1)
;;; End:

;;; system.scm ends here
