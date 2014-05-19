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
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix scripts build)
  #:use-module (guix build utils)
  #:use-module (guix build install)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu system grub)
  #:use-module (gnu packages grub)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (guix-system
            read-operating-system))

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
        (_
         (leave (_ "failed to load machine file '~a': ~s~%")
                file args))))))

(define* (copy-closure store item target
                       #:key (log-port (current-error-port)))
  "Copy ITEM to the store under root directory TARGET and register it."
  (let ((dest (string-append target item))
        (refs (references store item)))
    (format log-port "copying '~a'...~%" item)
    (copy-recursively item dest
                      #:log (%make-void-port "w"))

    ;; Register ITEM; as a side-effect, it resets timestamps, etc.
    (unless (register-path item
                           #:prefix target
                           #:references refs)
      (leave (_ "failed to register '~a' under '~a'~%")
             item target))))

(define* (install store os-dir target
                  #:key (log-port (current-output-port))
                  grub? grub.cfg device)
  "Copy OS-DIR and its dependencies to directory TARGET.  TARGET must be an
absolute directory name since that's what 'guix-register' expects.

When GRUB? is true, install GRUB on DEVICE, using GRUB.CFG."
  (define to-copy
    (let ((lst (delete-duplicates (cons os-dir (references store os-dir))
                                  string=?)))
      (topologically-sorted store lst)))

  (if (string=? target "/")
      (warning (_ "initializing the current root file system~%"))
      ;; Copy items to the new store.
      (for-each (cut copy-closure store <> target #:log-port log-port)
                to-copy))

  ;; Create a bunch of additional files.
  (format log-port "populating '~a'...~%" target)
  (populate-root-file-system target)

  (when grub?
    (unless (install-grub grub.cfg device target)
      (leave (_ "failed to install GRUB on device '~a'~%") device))))


;;;
;;; Options.
;;;

(define (show-help)
  (display (_ "Usage: guix system [OPTION] ACTION FILE
Build the operating system declared in FILE according to ACTION.\n"))
  (newline)
  (display (_ "The valid values for ACTION are:\n"))
  (display (_ "\
  - 'build', build the operating system without installing anything\n"))
  (display (_ "\
  - 'vm', build a virtual machine image that shares the host's store\n"))
  (display (_ "\
  - 'vm-image', build a freestanding virtual machine image\n"))
  (display (_ "\
  - 'init', initialize a root file system to run GNU.\n"))

  (show-build-options-help)
  (display (_ "
      --image-size=SIZE  for 'vm-image', produce an image of SIZE"))
  (display (_ "
      --no-grub          for 'init', do not install GRUB"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
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
         (option '("image-size") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'image-size (size->number arg)
                               result)))
         (option '("no-grub") #f #f
                 (lambda (opt name arg result)
                   (alist-delete 'install-grub? result)))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t result)))
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
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (if (assoc-ref result 'action)
                      (alist-cons 'argument arg result)
                      (let ((action (string->symbol arg)))
                        (case action
                          ((build vm vm-image init)
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
        ((build vm vm-image)
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
           (os       (if file
                         (read-operating-system file)
                         (leave (_ "no configuration file specified~%"))))
           (mdrv     (case action
                       ((build init)
                        (operating-system-derivation os))
                       ((vm-image)
                        (let ((size (assoc-ref opts 'image-size)))
                          (system-qemu-image os
                                             #:disk-image-size size)))
                       ((vm)
                        (system-qemu-image/shared-store-script os))))
           (store    (open-connection))
           (dry?     (assoc-ref opts 'dry-run?))
           (drv      (run-with-store store mdrv))
           (grub?    (assoc-ref opts 'install-grub?))
           (grub.cfg (run-with-store store
                       (operating-system-grub.cfg os)))
           (grub     (package-derivation store grub))
           (drv-lst  (if grub?
                         (list drv grub grub.cfg)
                         (list drv))))
      (set-build-options-from-command-line store opts)
      (show-what-to-build store drv-lst
                          #:dry-run? dry?
                          #:use-substitutes? (assoc-ref opts 'substitutes?))

      (unless dry?
        (build-derivations store drv-lst)
        (display (derivation->output-path drv))
        (newline)

        (when (eq? action 'init)
          (let* ((target (second args))
                 (device (grub-configuration-device
                          (operating-system-bootloader os))))
            (format #t (_ "initializing operating system under '~a'...~%")
                    target)

            (when grub
              (let ((prefix (derivation->output-path grub)))
                (setenv "PATH"
                        (string-append  prefix "/bin:" prefix "/sbin:"
                                        (getenv "PATH")))))

            (install store (derivation->output-path drv)
                     (canonicalize-path target)
                     #:grub? grub?
                     #:grub.cfg (derivation->output-path grub.cfg)
                     #:device device)))))))
