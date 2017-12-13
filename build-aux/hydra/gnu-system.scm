;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
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

;;;
;;; This file defines build jobs for the Hydra continuation integration
;;; tool.
;;;

(use-modules (system base compile))

(eval-when (compile load eval)

  ;; Pre-load the compiler so we don't end up auto-compiling it.
  (compile #t)

  ;; Use our very own Guix modules.
  (set! %fresh-auto-compile #t)

  (and=> (assoc-ref (current-source-location) 'filename)
         (lambda (file)
           (let ((dir (string-append (dirname file) "/../..")))
             (format (current-error-port) "prepending ~s to the load path~%"
                     dir)
             (set! %load-path (cons dir %load-path))))))

(use-modules (guix config)
             (guix store)
             (guix grafts)
             (guix profiles)
             (guix packages)
             (guix derivations)
             (guix monads)
             ((guix licenses) #:select (gpl3+))
             ((guix utils) #:select (%current-system))
             ((guix scripts system) #:select (read-operating-system))
             ((guix scripts pack)
              #:select (lookup-compressor self-contained-tarball))
             (gnu packages)
             (gnu packages gcc)
             (gnu packages base)
             (gnu packages gawk)
             (gnu packages guile)
             (gnu packages gettext)
             (gnu packages compression)
             (gnu packages multiprecision)
             (gnu packages make-bootstrap)
             (gnu packages package-management)
             (gnu system)
             (gnu system vm)
             (gnu system install)
             (gnu tests)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match))

;; XXX: Debugging hack: since `hydra-eval-guile-jobs' redirects the output
;; port to the bit bucket, let us write to the error port instead.
(setvbuf (current-error-port) _IOLBF)
(set-current-output-port (current-error-port))

(define* (package->alist store package system
                         #:optional (package-derivation package-derivation))
  "Convert PACKAGE to an alist suitable for Hydra."
  (parameterize ((%graft? #f))
    `((derivation . ,(derivation-file-name
                      (package-derivation store package system
                                          #:graft? #f)))
      (description . ,(package-synopsis package))
      (long-description . ,(package-description package))
      (license . ,(package-license package))
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
  '("x86_64-linux" "i686-linux"))

(define (qemu-jobs store system)
  "Return a list of jobs that build QEMU images for SYSTEM."
  (define (->alist drv)
    `((derivation . ,(derivation-file-name drv))
      (description . "Stand-alone QEMU image of the GNU system")
      (long-description . "This is a demo stand-alone QEMU image of the GNU
system.")
      (license . ,gpl3+)
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
      (list (->job 'usb-image
                   (run-with-store store
                     (mbegin %store-monad
                       (set-guile-for-build (default-guile))
                       (system-disk-image installation-os
                                          #:disk-image-size
                                          (* 1024 MiB)))))
            (->job 'iso9660-image
                   (run-with-store store
                     (mbegin %store-monad
                       (set-guile-for-build (default-guile))
                       (system-disk-image installation-os
                                          #:file-system-type
                                          "iso9660")))))
      '()))

(define (system-test-jobs store system)
  "Return a list of jobs for the system tests."
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
        (description . ,(format #f "GuixSD '~a' system test"
                                (system-test-name test)))
        (long-description . ,(system-test-description test))
        (license . ,gpl3+)
        (home-page . ,%guix-home-page-url)
        (maintainers . ("bug-guix@gnu.org")))))

  (define (->job test)
    (let ((name (string->symbol
                 (string-append "test." (system-test-name test)
                                "." system))))
      (cons name (test->thunk test))))

  (if (member system %guixsd-supported-systems)
      (map ->job (all-system-tests))
      '()))

(define (tarball-jobs store system)
  "Return Hydra jobs to build the self-contained Guix binary tarball."
  (define (->alist drv)
    `((derivation . ,(derivation-file-name drv))
      (description . "Stand-alone binary Guix tarball")
      (long-description . "This is a tarball containing binaries of Guix and
all its dependencies, and ready to be installed on non-GuixSD distributions.")
      (license . ,gpl3+)
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
  (compose string->symbol package-full-name))

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
      (_ 'all)))                                  ; build everything

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
      (and (string-contains target "mingw")
           (not (string=? "x86_64-linux" system))))

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
                               (system-test-jobs store system)
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
                    (else
                     (error "unknown subset" subset))))
                %hydra-supported-systems)))
