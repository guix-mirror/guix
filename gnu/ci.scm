;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018, 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020, 2021 Mathieu Othacehe <othacehe@gnu.org>
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
  #:use-module (guix channels)
  #:use-module (guix config)
  #:use-module (guix describe)
  #:use-module (guix store)
  #:use-module (guix grafts)
  #:use-module (guix profiles)
  #:use-module (guix packages)
  #:autoload   (guix transformations) (tunable-package? tuned-package)
  #:use-module (guix channels)
  #:use-module (guix config)
  #:use-module (guix derivations)
  #:use-module (guix build-system)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix ui)
  #:use-module ((guix licenses)
                #:select (gpl3+ license? license-name))
  #:use-module ((guix utils) #:select (%current-system))
  #:use-module ((guix scripts system) #:select (read-operating-system))
  #:use-module ((guix scripts pack)
                #:select (lookup-compressor self-contained-tarball))
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
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
  #:use-module (gnu system image)
  #:use-module (gnu system vm)
  #:use-module (gnu system install)
  #:use-module (gnu system images hurd)
  #:use-module (gnu system images novena)
  #:use-module (gnu system images pine64)
  #:use-module (gnu system images pinebook-pro)
  #:use-module (gnu tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (derivation->job
            image->job

            %core-packages
            %cross-targets
            channel-source->package

            arguments->systems
            cuirass-jobs))

;;; Commentary:
;;;
;;; This file defines build jobs for Cuirass.
;;;
;;; Code:

(define* (derivation->job name drv
                          #:key
                          (max-silent-time 3600)
                          (timeout (* 5 3600)))
  "Return a Cuirass job called NAME and describing DRV.

MAX-SILENT-TIME and TIMEOUT are build options passed to the daemon when
building the derivation."
  `((#:job-name . ,name)
    (#:derivation . ,(derivation-file-name drv))
    (#:inputs . ,(map (compose derivation-file-name
                               derivation-input-derivation)
                      (derivation-inputs drv)))
    (#:outputs . ,(filter-map
                   (lambda (res)
                     (match res
                       ((name . path)
                        `(,name . ,path))))
                   (derivation->output-paths drv)))
    (#:nix-name . ,(derivation-name drv))
    (#:system . ,(derivation-system drv))
    (#:max-silent-time . ,max-silent-time)
    (#:timeout . ,timeout)))

(define* (package-job store job-name package system
                      #:key cross? target (suffix ""))
  "Return a job called JOB-NAME that builds PACKAGE on SYSTEM."
  (let ((job-name (string-append job-name "." system suffix)))
    (parameterize ((%graft? #f))
      (let* ((drv (if cross?
                      (package-cross-derivation store package target system
                                                #:graft? #f)
                      (package-derivation store package system
                                          #:graft? #f)))
             (max-silent-time (or (assoc-ref (package-properties package)
                                             'max-silent-time)
                                  3600))
             (timeout (or (assoc-ref (package-properties package)
                                     'timeout)
                          72000)))
        (derivation->job job-name drv
                         #:max-silent-time max-silent-time
                         #:timeout timeout)))))

(define (package-cross-job store job-name package target system)
  "Return a job called TARGET.JOB-NAME that cross-builds PACKAGE for TARGET on
SYSTEM."
  (let ((name (string-append target "." job-name)))
    (package-job store name package system
                 #:cross? #t
                 #:target target)))

(define %core-packages
  ;; Note: Don't put the '-final' package variants because (1) that's
  ;; implicit, and (2) they cannot be cross-built (due to the explicit input
  ;; chain.)
  (list gcc-8 gcc-9 gcc-10 gcc-11 glibc binutils
        gmp mpfr mpc coreutils findutils diffutils patch sed grep
        gawk gnu-gettext hello guile-2.2 guile-3.0 zlib gzip xz guix
        %bootstrap-binaries-tarball
        %binutils-bootstrap-tarball
        (%glibc-bootstrap-tarball)
        %gcc-bootstrap-tarball
        %guile-bootstrap-tarball
        %bootstrap-tarballs))

(define (commencement-packages system)
  "Return the list of bootstrap packages from the commencement module for
SYSTEM."
  ;; Only include packages supported on SYSTEM.  For example, the Mes
  ;; bootstrap graph is currently not supported on ARM so it should be
  ;; excluded.
  (filter (lambda (obj)
            (and (package? obj)
                 (supported-package? obj system)))
          (module-map (lambda (sym var)
                        (variable-ref var))
                      (resolve-module '(gnu packages commencement)))))

(define (packages-to-cross-build target)
  "Return the list of packages to cross-build for TARGET."
  ;; Don't cross-build the bootstrap tarballs for MinGW.
  (if (string-contains target "mingw")
      (drop-right %core-packages 6)
      %core-packages))

(define %cross-targets
  '("mips64el-linux-gnu"
    "arm-linux-gnueabihf"
    "aarch64-linux-gnu"
    "powerpc-linux-gnu"
    "powerpc64le-linux-gnu"
    "riscv64-linux-gnu"
    "i586-pc-gnu"                                 ;aka. GNU/Hurd
    "i686-w64-mingw32"
    "x86_64-w64-mingw32"))

(define (cross-jobs store system)
  "Return a list of cross-compilation jobs for SYSTEM."
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
                     (packages-to-cross-build target)))
              (remove (either from-32-to-64? same? pointless?)
                      %cross-targets)))

(define* (guix-jobs store systems #:key source commit)
  "Return a list of jobs for Guix itself."
  (define build
    (primitive-load (string-append source "/build-aux/build-self.scm")))

  (map
   (lambda (system)
     (let ((name (string->symbol
                  (string-append "guix." system)))
           (drv (run-with-store store
                  (build source #:version commit #:system system
                         #:pull-version 1
                         #:guile-version "2.2"))))
       (derivation->job name drv)))
   systems))

;; Architectures that are able to build or cross-build Guix System images.
;; This does not mean that other architectures are not supported, only that
;; they are often not fast enough to support Guix System images building.
(define %guix-system-supported-systems
  '("x86_64-linux" "i686-linux"))

(define %guix-system-images
  (list hurd-barebones-qcow2-image
        pine64-barebones-raw-image
        pinebook-pro-barebones-raw-image
        novena-barebones-raw-image))

(define (hours hours)
  (* 3600 hours))

(define* (image->job store image
                     #:key name system)
  "Return the job for IMAGE on SYSTEM.  If NAME is passed, use it as job name,
otherwise use the IMAGE name."
  (let* ((image-name (or name
                         (symbol->string (image-name image))))
         (name (string-append image-name "." system))
         (drv (run-with-store store
                (mbegin %store-monad
                  (set-guile-for-build (default-guile))
                  (lower-object (system-image image))))))
    (parameterize ((%graft? #f))
      (derivation->job name drv))))

(define (image-jobs store system)
  "Return a list of jobs that build images for SYSTEM."
  (define MiB
    (expt 2 20))

  (if (member system %guix-system-supported-systems)
      `(,(image->job store
                     (image
                      (inherit efi-disk-image)
                      (operating-system installation-os))
                     #:name "usb-image"
                     #:system system)
        ,(image->job
          store
          (image
           (inherit (image-with-label
                     iso9660-image
                     (string-append "GUIX_" system "_"
                                    (if (> (string-length %guix-version) 7)
                                        (substring %guix-version 0 7)
                                        %guix-version))))
           (operating-system installation-os))
          #:name "iso9660-image"
          #:system system)
        ;; Only cross-compile Guix System images from x86_64-linux for now.
        ,@(if (string=? system "x86_64-linux")
              (map (cut image->job store <>
                        #:system system)
                   %guix-system-images)
              '()))
      '()))

(define channel-build-system
  ;; Build system used to "convert" a channel instance to a package.
  (let* ((build (lambda* (name inputs
                               #:key source commit system
                               #:allow-other-keys)
                  (mlet* %store-monad ((source (if (string? source)
                                                   (return source)
                                                   (lower-object source)))
                                       (instance
                                        -> (checkout->channel-instance
                                            source #:commit commit)))
                    (channel-instances->derivation (list instance)))))
         (lower (lambda* (name #:key system source commit
                               #:allow-other-keys)
                  (bag
                    (name name)
                    (system system)
                    (build build)
                    (arguments `(#:source ,source
                                 #:commit ,commit))))))
    (build-system (name 'channel)
                  (description "Turn a channel instance into a package.")
                  (lower lower))))

(define* (channel-source->package source #:key commit)
  "Return a package for the given channel SOURCE, a lowerable object."
  (package
    (inherit guix)
    (version (string-append (package-version guix) "+"))
    (build-system channel-build-system)
    (arguments `(#:source ,source
                 #:commit ,commit))
    (inputs '())
    (native-inputs '())
    (propagated-inputs '())))

(define* (system-test-jobs store system
                           #:key source commit)
  "Return a list of jobs for the system tests."
  (define (->job test)
    (let ((name (string-append "test." (system-test-name test)
                               "." system))
          (drv (run-with-store store
                 (mbegin %store-monad
                   (set-current-system system)
                   (set-grafting #f)
                   (set-guile-for-build (default-guile))
                   (system-test-value test)))))

      (derivation->job name drv)))

  (if (member system %guix-system-supported-systems)
      ;; Override the value of 'current-guix' used by system tests.  Using a
      ;; channel instance makes tests that rely on 'current-guix' less
      ;; expensive.  It also makes sure we get a valid Guix package when this
      ;; code is not running from a checkout.
      (parameterize ((current-guix-package
                      (channel-source->package source #:commit commit)))
        (map ->job (all-system-tests)))
      '()))

(define (tarball-jobs store system)
  "Return jobs to build the self-contained Guix binary tarball."
  (define (->job name drv)
    (let ((name (string-append name "." system)))
      (parameterize ((%graft? #f))
        (derivation->job name drv))))

  ;; XXX: Add a job for the stable Guix?
  (list
   (->job "binary-tarball"
          (run-with-store store
            (mbegin %store-monad
              (set-guile-for-build (default-guile))
              (>>= (profile-derivation (packages->manifest (list guix)))
                   (lambda (profile)
                     (self-contained-tarball "guix-binary" profile
                                             #:profile-name "current-guix"
                                             #:localstatedir? #t
                                             #:compressor
                                             (lookup-compressor "xz")))))
            #:system system))))

(define job-name
  ;; Return the name of a package's job.
  package-name)

(define package->job
  (let ((base-packages
         (delete-duplicates
          (append-map (match-lambda
                        ((_ package _ ...)
                         (match (package-transitive-inputs package)
                           (((_ inputs _ ...) ...)
                            inputs))))
                      (%final-inputs)))))
    (lambda* (store package system #:key (suffix ""))
      "Return a job for PACKAGE on SYSTEM, or #f if this combination is not
valid.  Append SUFFIX to the job name."
      (cond ((member package base-packages)
             (package-job store (string-append "base." (job-name package))
                          package system #:suffix suffix))
            ((supported-package? package system)
             (let ((drv (package-derivation store package system
                                            #:graft? #f)))
               (and (substitutable-derivation? drv)
                    (package-job store (job-name package)
                                 package system #:suffix suffix))))
            (else
             #f)))))

(define %x86-64-micro-architectures
  ;; Micro-architectures for which we build tuned variants.
  '("westmere" "ivybridge" "haswell" "skylake" "skylake-avx512"))

(define (tuned-package-jobs store package system)
  "Return a list of jobs for PACKAGE tuned for SYSTEM's micro-architectures."
  (filter-map (lambda (micro-architecture)
                (define suffix
                  (string-append "." micro-architecture))

                (package->job store
                              (tuned-package package micro-architecture)
                              system
                              #:suffix suffix))
              (match system
                ("x86_64-linux" %x86-64-micro-architectures)
                (_ '()))))

(define (all-packages)
  "Return the list of packages to build."
  (define (adjust package result)
    (cond ((package-replacement package)
           ;; XXX: If PACKAGE and its replacement have the same name/version,
           ;; then both Cuirass jobs will have the same name, which
           ;; effectively means that the second one will be ignored.  Thus,
           ;; return the replacement first.
           (cons* (package-replacement package)   ;build both
                  package
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

(define (arguments->manifests arguments channels)
  "Return the list of manifests extracted from ARGUMENTS."
  (map (lambda (manifest)
         (any (lambda (checkout)
                (let ((path (in-vicinity checkout manifest)))
                  (and (file-exists? path)
                       path)))
              (map channel-url channels)))
       arguments))

(define (manifests->jobs store manifests)
  "Return the list of jobs for the entries in MANIFESTS, a list of file
names."
  (define (load-manifest manifest)
    (save-module-excursion
     (lambda ()
       (set-current-module (make-user-module '((guix profiles) (gnu))))
       (primitive-load manifest))))

  (define (manifest-entry-job-name entry)
    (string-append (manifest-entry-name entry) "-"
                   (manifest-entry-version entry)))

  (define (manifest-entry->job entry)
    (let* ((obj (manifest-entry-item entry))
           (drv (parameterize ((%graft? #f))
                  (run-with-store store
                    (lower-object obj))))
           (max-silent-time (or (and (package? obj)
                                     (assoc-ref (package-properties obj)
                                                'max-silent-time))
                                3600))
           (timeout (or (and (package? obj)
                             (assoc-ref (package-properties obj) 'timeout))
                        (* 5 3600))))
      (derivation->job (manifest-entry-job-name entry) drv
                       #:max-silent-time max-silent-time
                       #:timeout timeout)))

  (map manifest-entry->job
       (delete-duplicates
        (append-map (compose manifest-entries load-manifest)
                    manifests)
        manifest-entry=?)))

(define (arguments->systems arguments)
  "Return the systems list from ARGUMENTS."
  (match (assoc-ref arguments 'systems)
    (#f              %cuirass-supported-systems)
    ((lst ...)       lst)
    ((? string? str) (call-with-input-string str read))))


;;;
;;; Cuirass entry point.
;;;

(define (cuirass-jobs store arguments)
  "Register Cuirass jobs."
  (define subset
    (assoc-ref arguments 'subset))

  (define systems
    (arguments->systems arguments))

  (define channels
    (let ((channels (assq-ref arguments 'channels)))
      (map sexp->channel channels)))

  (define guix
    (find guix-channel? channels))

  (define commit
    (channel-commit guix))

  (define source
    (channel-url guix))

  ;; Turn off grafts.  Grafting is meant to happen on the user's machines.
  (parameterize ((%graft? #f))
    ;; Return one job for each package, except bootstrap packages.
    (append-map
     (lambda (system)
       (format (current-error-port)
               "evaluating for '~a' (heap size: ~a MiB)...~%"
               system
               (round
                (/ (assoc-ref (gc-stats) 'heap-size)
                   (expt 2. 20))))
       (invalidate-derivation-caches!)
       (match subset
         ('all
          ;; Build everything, including replacements.
          (let ((all (all-packages))
                (jobs (lambda (package)
                        (match (package->job store package system)
                          (#f '())
                          (main-job
                           (cons main-job
                                 (if (tunable-package? package)
                                     (tuned-package-jobs store package system)
                                     '())))))))
            (append
             (append-map jobs all)
             (cross-jobs store system))))
         ('core
          ;; Build core packages only.
          (append
           (map (lambda (package)
                  (package-job store (job-name package)
                               package system))
                (append (commencement-packages system) %core-packages))
           (cross-jobs store system)))
         ('guix
          ;; Build Guix modules only.
          (guix-jobs store systems
                     #:source source
                     #:commit commit))
         ('hello
          ;; Build hello package only.
          (let ((hello (specification->package "hello")))
            (list (package-job store (job-name hello)
                               hello system))))
         ('images
          ;; Build Guix System images only.
          (image-jobs store system))
         ('system-tests
          ;; Build Guix System tests only.
          (system-test-jobs store system
                            #:source source
                            #:commit commit))
         ('tarball
          ;; Build Guix tarball only.
          (tarball-jobs store system))
         (('custom . modules)
          ;; Build custom modules jobs only.
          (append-map
           (lambda (module)
             (let ((proc (module-ref
                          (resolve-interface module)
                          'cuirass-jobs)))
               (proc store arguments)))
           modules))
         (('channels . channels)
          ;; Build only the packages from CHANNELS.
          (let ((all (all-packages)))
            (filter-map
             (lambda (package)
               (any (lambda (channel)
                      (and (member (channel-name channel) channels)
                           (package->job store package system)))
                    (package-channels package)))
             all)))
         (('packages . rest)
          ;; Build selected list of packages only.
          (let ((packages (map specification->package rest)))
            (map (lambda (package)
                   (package-job store (job-name package)
                                package system))
                 packages)))
         (('manifests . rest)
          ;; Build packages in the list of manifests.
          (let ((manifests (arguments->manifests rest channels)))
            (manifests->jobs store manifests)))
         (else
          (error "unknown subset" subset))))
     systems)))
