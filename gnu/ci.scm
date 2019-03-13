;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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

(define-module (gnu ci)
  #:use-module (guix config)
  #:use-module (guix store)
  #:use-module (guix grafts)
  #:use-module (guix profiles)
  #:use-module (guix packages)
  #:use-module (guix channels)
  #:use-module (guix derivations)
  #:use-module (guix build-system)
  #:use-module (guix monads)
  #:use-module (guix ui)
  #:use-module ((guix licenses)
                #:select (gpl3+ license? license-name))
  #:use-module ((guix utils) #:select (%current-system))
  #:use-module ((guix scripts system) #:select (read-operating-system))
  #:use-module ((guix scripts pack)
                #:select (lookup-compressor self-contained-tarball))
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages make-bootstrap)
  #:use-module (gnu packages package-management)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu system install)
  #:use-module (gnu tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (hydra-jobs))

;;; Commentary:
;;;
;;; This file defines build jobs for the Hydra and Cuirass continuation
;;; integration tools.
;;;
;;; Code:

(define* (package->alist store package system
                         #:optional (package-derivation package-derivation))
  "Convert PACKAGE to an alist suitable for Hydra."
  (parameterize ((%graft? #f))
    `((derivation . ,(derivation-file-name
                      (package-derivation store package system
                                          #:graft? #f)))
      (description . ,(package-synopsis package))
      (long-description . ,(package-description package))

      ;; XXX: Hydra ignores licenses that are not a <license> structure or a
      ;; list thereof.
      (license . ,(let loop ((license (package-license package)))
                    (match license
                      ((? license?)
                       (license-name license))
                      ((lst ...)
                       (map loop license)))))

      (home-page . ,(package-home-page package))
      (maintainers . ("bug-guix@gnu.org"))
      (max-silent-time . ,(or (assoc-ref (package-properties package)
                                         'max-silent-time)
                              3600))              ;1 hour by default
      (timeout . ,(or (assoc-ref (package-properties package) 'timeout)
                      72000)))))                  ;20 hours by default

(define (package-job store job-name package system)
  "Return a job called JOB-NAME that builds PACKAGE on SYSTEM."
  (let ((job-name (symbol-append job-name (string->symbol ".")
                                 (string->symbol system))))
    `(,job-name . ,(cut package->alist store package system))))

(define (package-cross-job store job-name package target system)
  "Return a job called TARGET.JOB-NAME that cross-builds PACKAGE for TARGET on
SYSTEM."
  `(,(symbol-append (string->symbol target) (string->symbol ".") job-name
                    (string->symbol ".") (string->symbol system)) .
    ,(cute package->alist store package system
           (lambda* (store package system #:key graft?)
             (package-cross-derivation store package target system
                                       #:graft? graft?)))))

(define %core-packages
  ;; Note: Don't put the '-final' package variants because (1) that's
  ;; implicit, and (2) they cannot be cross-built (due to the explicit input
  ;; chain.)
  (list gcc-4.8 gcc-4.9 gcc-5 glibc binutils
        gmp mpfr mpc coreutils findutils diffutils patch sed grep
        gawk gnu-gettext hello guile-2.0 guile-2.2 zlib gzip xz
        %bootstrap-binaries-tarball
        %binutils-bootstrap-tarball
        (%glibc-bootstrap-tarball)
        %gcc-bootstrap-tarball
        %guile-bootstrap-tarball
        %bootstrap-tarballs))

(define %packages-to-cross-build
  %core-packages)

(define %cross-targets
  '("mips64el-linux-gnu"
    "mips64el-linux-gnuabi64"
    "arm-linux-gnueabihf"
    "aarch64-linux-gnu"
    "powerpc-linux-gnu"
    "i586-pc-gnu"                                 ;aka. GNU/Hurd
    "i686-w64-mingw32"))

(define %guixsd-supported-systems
  '("x86_64-linux" "i686-linux" "armhf-linux"))

(define %u-boot-systems
  '("armhf-linux"))

(define (qemu-jobs store system)
  "Return a list of jobs that build QEMU images for SYSTEM."
  (define (->alist drv)
    `((derivation . ,(derivation-file-name drv))
      (description . "Stand-alone QEMU image of the GNU system")
      (long-description . "This is a demo stand-alone QEMU image of the GNU
system.")
      (license . ,(license-name gpl3+))
      (max-silent-time . 600)
      (timeout . 3600)
      (home-page . ,%guix-home-page-url)
      (maintainers . ("bug-guix@gnu.org"))))

  (define (->job name drv)
    (let ((name (symbol-append name (string->symbol ".")
                               (string->symbol system))))
      `(,name . ,(lambda ()
                   (parameterize ((%graft? #f))
                     (->alist drv))))))

  (define MiB
    (expt 2 20))

  (if (member system %guixsd-supported-systems)
      (if (member system %u-boot-systems)
          (list (->job 'flash-image
                       (run-with-store store
                         (mbegin %store-monad
                           (set-guile-for-build (default-guile))
                           (system-disk-image
                            (operating-system (inherit installation-os)
                             (bootloader (bootloader-configuration
                                          (bootloader u-boot-bootloader)
                                          (target #f))))
                            #:disk-image-size
                            (* 1500 MiB))))))
          (list (->job 'usb-image
                       (run-with-store store
                         (mbegin %store-monad
                           (set-guile-for-build (default-guile))
                           (system-disk-image installation-os
                                              #:disk-image-size
                                              (* 1500 MiB)))))
                (->job 'iso9660-image
                       (run-with-store store
                         (mbegin %store-monad
                           (set-guile-for-build (default-guile))
                           (system-disk-image installation-os
                                              #:file-system-type
                                              "iso9660"))))))
      '()))

(define channel-build-system
  ;; Build system used to "convert" a channel instance to a package.
  (let* ((build (lambda* (store name inputs
                                #:key instance #:allow-other-keys)
                  (run-with-store store
                    (channel-instances->derivation (list instance)))))
         (lower (lambda* (name #:key system instance #:allow-other-keys)
                  (bag
                    (name name)
                    (system system)
                    (build build)
                    (arguments `(#:instance ,instance))))))
    (build-system (name 'channel)
                  (description "Turn a channel instance into a package.")
                  (lower lower))))

(define (channel-instance->package instance)
  "Return a package for the given channel INSTANCE."
  (package
    (inherit guix)
    (version (or (string-take (channel-instance-commit instance) 7)
                 (string-append (package-version guix) "+")))
    (build-system channel-build-system)
    (arguments `(#:instance ,instance))
    (inputs '())
    (native-inputs '())
    (propagated-inputs '())))

(define* (system-test-jobs store system
                           #:key source commit)
  "Return a list of jobs for the system tests."
  (define instance
    (checkout->channel-instance source #:commit commit))

  (define (test->thunk test)
    (lambda ()
      (define drv
        (run-with-store store
          (mbegin %store-monad
            (set-current-system system)
            (set-grafting #f)
            (set-guile-for-build (default-guile))
            (system-test-value test))))

      `((derivation . ,(derivation-file-name drv))
        (description . ,(format #f "Guix '~a' system test"
                                (system-test-name test)))
        (long-description . ,(system-test-description test))
        (license . ,(license-name gpl3+))
        (max-silent-time . 600)
        (timeout . 3600)
        (home-page . ,%guix-home-page-url)
        (maintainers . ("bug-guix@gnu.org")))))

  (define (->job test)
    (let ((name (string->symbol
                 (string-append "test." (system-test-name test)
                                "." system))))
      (cons name (test->thunk test))))

  (if (and (member system %guixsd-supported-systems)

           ;; XXX: Our build farm has too few ARMv7 machines and they are very
           ;; slow, so skip system tests there.
           (not (string=? system "armhf-linux")))
      ;; Override the value of 'current-guix' used by system tests.  Using a
      ;; channel instance makes tests that rely on 'current-guix' less
      ;; expensive.  It also makes sure we get a valid Guix package when this
      ;; code is not running from a checkout.
      (parameterize ((current-guix-package
                      (channel-instance->package instance)))
        (map ->job (all-system-tests)))
      '()))

(define (tarball-jobs store system)
  "Return Hydra jobs to build the self-contained Guix binary tarball."
  (define (->alist drv)
    `((derivation . ,(derivation-file-name drv))
      (description . "Stand-alone binary Guix tarball")
      (long-description . "This is a tarball containing binaries of Guix and
all its dependencies, and ready to be installed on \"foreign\" distributions.")
      (license . ,(license-name gpl3+))
      (home-page . ,%guix-home-page-url)
      (maintainers . ("bug-guix@gnu.org"))))

  (define (->job name drv)
    (let ((name (symbol-append name (string->symbol ".")
                               (string->symbol system))))
      `(,name . ,(lambda ()
                   (parameterize ((%graft? #f))
                     (->alist drv))))))

  ;; XXX: Add a job for the stable Guix?
  (list (->job 'binary-tarball
               (run-with-store store
                 (mbegin %store-monad
                   (set-guile-for-build (default-guile))
                   (>>= (profile-derivation (packages->manifest (list guix)))
                        (lambda (profile)
                          (self-contained-tarball "guix-binary" profile
                                                  #:localstatedir? #t
                                                  #:compressor
                                                  (lookup-compressor "xz")))))
                 #:system system))))

(define job-name
  ;; Return the name of a package's job.
  (compose string->symbol
           (cut package-full-name <> "-")))

(define package->job
  (let ((base-packages
         (delete-duplicates
          (append-map (match-lambda
                       ((_ package _ ...)
                        (match (package-transitive-inputs package)
                          (((_ inputs _ ...) ...)
                           inputs))))
                      (%final-inputs)))))
    (lambda (store package system)
      "Return a job for PACKAGE on SYSTEM, or #f if this combination is not
valid."
      (cond ((member package base-packages)
             (package-job store (symbol-append 'base. (job-name package))
                          package system))
            ((supported-package? package system)
             (let ((drv (package-derivation store package system
                                            #:graft? #f)))
               (and (substitutable-derivation? drv)
                    (package-job store (job-name package)
                                 package system))))
            (else
             #f)))))

(define (all-packages)
  "Return the list of packages to build."
  (define (adjust package result)
    (cond ((package-replacement package)
           (cons* package                         ;build both
                  (package-replacement package)
                  result))
          ((package-superseded package)
           result)                                ;don't build it
          (else
           (cons package result))))

  (fold-packages adjust
                 (fold adjust '()                 ;include base packages
                       (match (%final-inputs)
                         (((labels packages _ ...) ...)
                          packages)))
                 #:select? (const #t)))           ;include hidden packages

(define (arguments->manifests arguments)
  "Return the list of manifests extracted from ARGUMENTS."
  (map (match-lambda
         ((input-name . relative-path)
          (let* ((checkout (assq-ref arguments (string->symbol input-name)))
                 (base (assq-ref checkout 'file-name)))
            (in-vicinity base relative-path))))
       (assq-ref arguments 'manifests)))

(define (manifests->packages store manifests)
  "Return the list of packages found in MANIFESTS."
  (define (load-manifest manifest)
    (save-module-excursion
     (lambda ()
       (set-current-module (make-user-module '((guix profiles) (gnu))))
       (primitive-load manifest))))

  (delete-duplicates!
   (map manifest-entry-item
        (append-map (compose manifest-entries
                             load-manifest)
                    manifests))))


;;;
;;; Hydra entry point.
;;;

(define (hydra-jobs store arguments)
  "Return Hydra jobs."
  (define subset
    (match (assoc-ref arguments 'subset)
      ("core" 'core)                              ; only build core packages
      ("hello" 'hello)                            ; only build hello
      (((? string?) (? string?) ...) 'list)       ; only build selected list of packages
      ("manifests" 'manifests)                    ; only build packages in the list of manifests
      (_ 'all)))                                  ; build everything

  (define systems
    (match (assoc-ref arguments 'systems)
      (#f              %hydra-supported-systems)
      ((lst ...)       lst)
      ((? string? str) (call-with-input-string str read))))

  (define checkout
    ;; Extract metadata about the 'guix' checkout.  Its key in ARGUMENTS may
    ;; vary, so pick up the first one that's neither 'subset' nor 'systems'.
    (any (match-lambda
           ((key . value)
            (and (not (memq key '(systems subset)))
                 value)))
         arguments))

  (define commit
    (assq-ref checkout 'revision))

  (define source
    (assq-ref checkout 'file-name))

  (define (cross-jobs system)
    (define (from-32-to-64? target)
      ;; Return true if SYSTEM is 32-bit and TARGET is 64-bit.  This hack
      ;; prevents known-to-fail cross-builds from i686-linux or armhf-linux to
      ;; mips64el-linux-gnuabi64.
      (and (or (string-prefix? "i686-" system)
               (string-prefix? "i586-" system)
               (string-prefix? "armhf-" system))
           (string-contains target "64")))    ;x86_64, mips64el, aarch64, etc.

    (define (same? target)
      ;; Return true if SYSTEM and TARGET are the same thing.  This is so we
      ;; don't try to cross-compile to 'mips64el-linux-gnu' from
      ;; 'mips64el-linux'.
      (or (string-contains target system)
          (and (string-prefix? "armhf" system)    ;armhf-linux
               (string-prefix? "arm" target))))   ;arm-linux-gnueabihf

    (define (pointless? target)
      ;; Return #t if it makes no sense to cross-build to TARGET from SYSTEM.
      (match system
        ((or "x86_64-linux" "i686-linux")
         (if (string-contains target "mingw")
             (not (string=? "x86_64-linux" system))
             #f))
        (_
         ;; Don't try to cross-compile from non-Intel platforms: this isn't
         ;; very useful and these are often brittle configurations.
         #t)))

    (define (either proc1 proc2 proc3)
      (lambda (x)
        (or (proc1 x) (proc2 x) (proc3 x))))

    (append-map (lambda (target)
                  (map (lambda (package)
                         (package-cross-job store (job-name package)
                                            package target system))
                       %packages-to-cross-build))
                (remove (either from-32-to-64? same? pointless?)
                        %cross-targets)))

  ;; Turn off grafts.  Grafting is meant to happen on the user's machines.
  (parameterize ((%graft? #f))
    ;; Return one job for each package, except bootstrap packages.
    (append-map (lambda (system)
                  (format (current-error-port)
                          "evaluating for '~a' (heap size: ~a MiB)...~%"
                          system
                          (round
                           (/ (assoc-ref (gc-stats) 'heap-size)
                              (expt 2. 20))))
                  (invalidate-derivation-caches!)
                  (case subset
                    ((all)
                     ;; Build everything, including replacements.
                     (let ((all (all-packages))
                           (job (lambda (package)
                                  (package->job store package
                                                system))))
                       (append (filter-map job all)
                               (qemu-jobs store system)
                               (system-test-jobs store system
                                                 #:source source
                                                 #:commit commit)
                               (tarball-jobs store system)
                               (cross-jobs system))))
                    ((core)
                     ;; Build core packages only.
                     (append (map (lambda (package)
                                    (package-job store (job-name package)
                                                 package system))
                                  %core-packages)
                             (cross-jobs system)))
                    ((hello)
                     ;; Build hello package only.
                     (if (string=? system (%current-system))
                         (let ((hello (specification->package "hello")))
                           (list (package-job store (job-name hello) hello system)))
                         '()))
                    ((list)
                     ;; Build selected list of packages only.
                     (if (string=? system (%current-system))
                         (let* ((names (assoc-ref arguments 'subset))
                                (packages (map specification->package names)))
                           (map (lambda (package)
                                    (package-job store (job-name package)
                                                 package system))
                                  packages))
                         '()))
                    ((manifests)
                     ;; Build packages in the list of manifests.
                     (let* ((manifests (arguments->manifests arguments))
                            (packages (manifests->packages store manifests)))
                       (map (lambda (package)
                              (package-job store (job-name package)
                                           package system))
                            packages)))
                    (else
                     (error "unknown subset" subset))))
                systems)))
