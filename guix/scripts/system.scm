;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix scripts build)
  #:use-module (guix build utils)
  #:use-module (gnu build install)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu system grub)
  #:use-module (gnu packages grub)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (guix-system
            read-operating-system))


;;;
;;; Operating system declaration.
;;;

(define %user-module
  ;; Module in which the machine description file is loaded.
  (let ((module (make-fresh-user-module)))
    (for-each (lambda (iface)
                (module-use! module (resolve-interface iface)))
              '((gnu system)
                (gnu services)
                (gnu system shadow)))
    module))

(define (read-operating-system file)
  "Read the operating-system declaration from FILE and return it."
  ;; TODO: Factorize.
  (catch #t
    (lambda ()
      ;; Avoid ABI incompatibility with the <operating-system> record.
      (set! %fresh-auto-compile #t)

      (save-module-excursion
       (lambda ()
         (set-current-module %user-module)
         (primitive-load file))))
    (lambda args
      (match args
        (('system-error . _)
         (let ((err (system-error-errno args)))
           (leave (_ "failed to open operating system file '~a': ~a~%")
                  file (strerror err))))
        (('syntax-error proc message properties form . rest)
         (let ((loc (source-properties->location properties)))
           (leave (_ "~a: ~a~%")
                  (location->string loc) message)))
        (_
         (leave (_ "failed to load operating system file '~a': ~s~%")
                file args))))))


;;;
;;; Installation.
;;;

;; TODO: Factorize.
(define references*
  (store-lift references))
(define topologically-sorted*
  (store-lift topologically-sorted))
(define show-what-to-build*
  (store-lift show-what-to-build))


(define* (copy-item item target
                    #:key (log-port (current-error-port)))
  "Copy ITEM to the store under root directory TARGET and register it."
  (mlet* %store-monad ((refs (references* item)))
    (let ((dest  (string-append target item))
          (state (string-append target "/var/guix")))
      (format log-port "copying '~a'...~%" item)
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
        (leave (_ "failed to register '~a' under '~a'~%")
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

(define* (install os-drv target
                  #:key (log-port (current-output-port))
                  grub? grub.cfg device)
  "Copy the output of OS-DRV and its dependencies to directory TARGET.  TARGET
must be an absolute directory name since that's what 'guix-register' expects.

When GRUB? is true, install GRUB on DEVICE, using GRUB.CFG."
  (define (maybe-copy to-copy)
    (with-monad %store-monad
      (if (string=? target "/")
          (begin
            (warning (_ "initializing the current root file system~%"))
            (return #t))
          (begin
            ;; Make sure the target store exists.
            (mkdir-p (string-append target (%store-prefix)))

            ;; Copy items to the new store.
            (copy-closure to-copy target #:log-port log-port)))))

  (mlet* %store-monad ((os-dir -> (derivation->output-path os-drv))
                       (%         (maybe-copy os-dir)))

    ;; Create a bunch of additional files.
    (format log-port "populating '~a'...~%" target)
    (populate-root-file-system os-dir target)

    (when grub?
      (unless (false-if-exception (install-grub grub.cfg device target))
        (leave (_ "failed to install GRUB on device '~a'~%") device)))

    (return #t)))


;;;
;;; Reconfiguration.
;;;

(define %system-profile
  ;; The system profile.
  (string-append %state-directory "/profiles/system"))

(define-syntax-rule (save-environment-excursion body ...)
  "Save the current environment variables, run BODY..., and restore them."
  (let ((env (environ)))
    (dynamic-wind
      (const #t)
      (lambda ()
        body ...)
      (lambda ()
        (environ env)))))

(define* (switch-to-system os
                           #:optional (profile %system-profile))
  "Make a new generation of PROFILE pointing to the directory of OS, switch to
it atomically, and then run OS's activation script."
  (mlet* %store-monad ((drv    (operating-system-derivation os))
                       (script (operating-system-activation-script os)))
    (let* ((system     (derivation->output-path drv))
           (number     (+ 1 (generation-number profile)))
           (generation (generation-file-name profile number)))
      (symlink system generation)
      (switch-symlinks profile generation)

      (format #t (_ "activating system...~%"))

      ;; The activation script may change $PATH, among others, so protect
      ;; against that.
      (return (save-environment-excursion
               ;; Tell 'activate-current-system' what the new system is.
               (setenv "GUIX_NEW_SYSTEM" system)

               (primitive-load (derivation->output-path script))))

      ;; TODO: Run 'deco reload ...'.
      )))

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

(define* (previous-grub-entries #:optional (profile %system-profile))
  "Return a list of 'menu-entry' for the generations of PROFILE."
  (define (system->grub-entry system number time)
    (unless-file-not-found
     (call-with-input-file (string-append system "/parameters")
       (lambda (port)
         (match (read port)
           (('boot-parameters ('version 0)
                              ('label label) ('root-device root)
                              ('kernel linux)
                              _ ...)
            (menu-entry
             (label (string-append label " (#"
                                   (number->string number) ", "
                                   (seconds->string time) ")"))
             (linux linux)
             (linux-arguments
              (list (string-append "--root=" root)
                    #~(string-append "--system=" #$system)
                    #~(string-append "--load=" #$system "/boot")))
             (initrd #~(string-append #$system "/initrd"))))
           (_                                     ;unsupported format
            (warning (_ "unrecognized boot parameters for '~a'~%")
                     system)
            #f))))))

  (let* ((numbers (generation-numbers profile))
         (systems (map (cut generation-file-name profile <>)
                       numbers))
         (times   (map (lambda (system)
                         (unless-file-not-found
                          (stat:mtime (lstat system))))
                       systems)))
    (filter-map system->grub-entry systems numbers times)))


;;;
;;; Action.
;;;

(define* (system-derivation-for-action os action
                                       #:key image-size full-boot? mappings)
  "Return as a monadic value the derivation for OS according to ACTION."
  (case action
    ((build init reconfigure)
     (operating-system-derivation os))
    ((vm-image)
     (system-qemu-image os #:disk-image-size image-size))
    ((vm)
     (system-qemu-image/shared-store-script os
                                            #:full-boot? full-boot?
                                            #:disk-image-size image-size
                                            #:mappings mappings))
    ((disk-image)
     (system-disk-image os #:disk-image-size image-size))))

(define (grub.cfg os)
  "Return the GRUB configuration file for OS."
  (operating-system-grub.cfg os (previous-grub-entries)))

(define* (maybe-build drvs
                      #:key dry-run? use-substitutes?)
  "Show what will/would be built, and actually build DRVS, unless DRY-RUN? is
true."
  (with-monad %store-monad
    (>>= (show-what-to-build* drvs
                              #:dry-run? dry-run?
                              #:use-substitutes? use-substitutes?)
         (lambda (_)
           (if dry-run?
               (return #f)
               (built-derivations drvs))))))

(define* (perform-action action os
                         #:key grub? dry-run?
                         use-substitutes? device target
                         image-size full-boot?
                         (mappings '()))
  "Perform ACTION for OS.  GRUB? specifies whether to install GRUB; DEVICE is
the target devices for GRUB; TARGET is the target root directory; IMAGE-SIZE
is the size of the image to be built, for the 'vm-image' and 'disk-image'
actions.  FULL-BOOT? is used for the 'vm' action; it determines whether to
boot directly to the kernel or to the bootloader."
  (mlet* %store-monad
      ((sys       (system-derivation-for-action os action
                                                #:image-size image-size
                                                #:full-boot? full-boot?
                                                #:mappings mappings))
       (grub      (package->derivation grub))
       (grub.cfg  (grub.cfg os))
       (drvs   -> (if (and grub? (memq action '(init reconfigure)))
                      (list sys grub grub.cfg)
                      (list sys)))
       (%         (maybe-build drvs #:dry-run? dry-run?
                               #:use-substitutes? use-substitutes?)))

    (if dry-run?
        (return #f)
        (begin
          (for-each (cut format #t "~a~%" <>)
                    (map derivation->output-path drvs))

          ;; Make sure GRUB is accessible.
          (when grub?
            (let ((prefix (derivation->output-path grub)))
              (setenv "PATH"
                      (string-append  prefix "/bin:" prefix "/sbin:"
                                      (getenv "PATH")))))

          (case action
            ((reconfigure)
             (mlet %store-monad ((% (switch-to-system os)))
               (when grub?
                 (unless (false-if-exception
                          (install-grub (derivation->output-path grub.cfg)
                                        device "/"))
                   (leave (_ "failed to install GRUB on device '~a'~%")
                          device)))
               (return #t)))
            ((init)
             (newline)
             (format #t (_ "initializing operating system under '~a'...~%")
                     target)
             (install sys (canonicalize-path target)
                      #:grub? grub?
                      #:grub.cfg (derivation->output-path grub.cfg)
                      #:device device))
            (else
             ;; All we had to do was to build SYS.
             (return (derivation->output-path sys))))))))


;;;
;;; Options.
;;;

(define (show-help)
  (display (_ "Usage: guix system [OPTION] ACTION FILE
Build the operating system declared in FILE according to ACTION.\n"))
  (newline)
  (display (_ "The valid values for ACTION are:\n"))
  (display (_ "\
  - 'reconfigure', switch to a new operating system configuration\n"))
  (display (_ "\
  - 'build', build the operating system without installing anything\n"))
  (display (_ "\
  - 'vm', build a virtual machine image that shares the host's store\n"))
  (display (_ "\
  - 'vm-image', build a freestanding virtual machine image\n"))
  (display (_ "\
  - 'disk-image', build a disk image, suitable for a USB stick\n"))
  (display (_ "\
  - 'init', initialize a root file system to run GNU.\n"))

  (show-build-options-help)
  (display (_ "
      --image-size=SIZE  for 'vm-image', produce an image of SIZE"))
  (display (_ "
      --no-grub          for 'init', do not install GRUB"))
  (display (_ "
      --share=SPEC       for 'vm', share host file system according to SPEC"))
  (display (_ "
      --expose=SPEC      for 'vm', expose host file system according to SPEC"))
  (display (_ "
      --full-boot        for 'vm', make a full boot sequence"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define (specification->file-system-mapping spec writable?)
  "Read the SPEC and return the corresponding <file-system-mapping>."
  (let ((index (string-index spec #\=)))
    (if index
        (file-system-mapping
         (source (substring spec 0 index))
         (target (substring spec (+ 1 index)))
         (writable? writable?))
        (file-system-mapping
         (source spec)
         (target spec)
         (writable? writable?)))))

(define %options
  ;; Specifications of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix system")))
         (option '("image-size") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'image-size (size->number arg)
                               result)))
         (option '("no-grub") #f #f
                 (lambda (opt name arg result)
                   (alist-delete 'install-grub? result)))
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
                   (alist-cons 'dry-run? #t result)))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg
                               (alist-delete 'system result eq?))))
         %standard-build-options))

(define %default-options
  ;; Alist of default option values.
  `((system . ,(%current-system))
    (substitutes? . #t)
    (build-hook? . #t)
    (max-silent-time . 3600)
    (verbosity . 0)
    (image-size . ,(* 900 (expt 2 20)))
    (install-grub? . #t)))


;;;
;;; Entry point.
;;;

(define (guix-system . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* (append args (environment-build-options))
                %options
                (lambda (opt name arg result)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (if (assoc-ref result 'action)
                      (alist-cons 'argument arg result)
                      (let ((action (string->symbol arg)))
                        (case action
                          ((build vm vm-image disk-image reconfigure init)
                           (alist-cons 'action action result))
                          (else (leave (_ "~a: unknown action~%")
                                       action))))))
                %default-options))

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
        (leave (_ "wrong number of arguments for action '~a'~%")
               action))

      (case action
        ((build vm vm-image disk-image reconfigure)
         (unless (= count 1)
           (fail)))
        ((init)
         (unless (= count 2)
           (fail))))
      args))

  (with-error-handling
    (let* ((opts     (parse-options))
           (args     (option-arguments opts))
           (file     (first args))
           (action   (assoc-ref opts 'action))
           (system   (assoc-ref opts 'system))
           (os       (if file
                         (read-operating-system file)
                         (leave (_ "no configuration file specified~%"))))

           (dry?     (assoc-ref opts 'dry-run?))
           (grub?    (assoc-ref opts 'install-grub?))
           (target   (match args
                       ((first second) second)
                       (_ #f)))
           (device   (and grub?
                          (grub-configuration-device
                           (operating-system-bootloader os))))

           (store    (open-connection)))
      (set-build-options-from-command-line store opts)

      (run-with-store store
        (perform-action action os
                        #:dry-run? dry?
                        #:use-substitutes? (assoc-ref opts 'substitutes?)
                        #:image-size (assoc-ref opts 'image-size)
                        #:full-boot? (assoc-ref opts 'full-boot?)
                        #:mappings (filter-map (match-lambda
                                                (('file-system-mapping . m)
                                                 m)
                                                (_ #f))
                                               opts)
                        #:grub? grub?
                        #:target target #:device device)
        #:system system))))

;;; system.scm ends here
