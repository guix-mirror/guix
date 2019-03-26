;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (build-self)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (guix ui)
  #:use-module (guix config)
  #:use-module (guix modules)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:export (build))

;;; Commentary:
;;;
;;; When loaded, this module returns a monadic procedure of at least one
;;; argument: the source tree to build.  It returns a derivation that
;;; builds it.
;;;
;;; This file uses modules provided by the already-installed Guix.  Those
;;; modules may be arbitrarily old compared to the version we want to
;;; build.  Because of that, it must rely on the smallest set of features
;;; that are likely to be provided by the (guix) and (gnu) modules, and by
;;; Guile itself, forever and ever.
;;;
;;; Code:


;;;
;;; Generating (guix config).
;;;
;;; This is copied from (guix self) because we cannot assume (guix self) is
;;; available at this point.
;;;

(define %dependency-variables
  ;; (guix config) variables corresponding to dependencies.
  '(%libgcrypt %libz %xz %gzip %bzip2))

(define %persona-variables
  ;; (guix config) variables that define Guix's persona.
  '(%guix-package-name
    %guix-version
    %guix-bug-report-address
    %guix-home-page-url))

(define %config-variables
  ;; (guix config) variables corresponding to Guix configuration.
  (letrec-syntax ((variables (syntax-rules ()
                               ((_)
                                '())
                               ((_ variable rest ...)
                                (cons `(variable . ,variable)
                                      (variables rest ...))))))
    (variables %localstatedir %storedir %sysconfdir %system)))

(define* (make-config.scm #:key zlib gzip xz bzip2
                          (package-name "GNU Guix")
                          (package-version "0")
                          (bug-report-address "bug-guix@gnu.org")
                          (home-page-url "https://gnu.org/s/guix"))

  ;; Hack so that Geiser is not confused.
  (define defmod 'define-module)

  (scheme-file "config.scm"
               #~(begin
                   (#$defmod (guix config)
                     #:export (%guix-package-name
                               %guix-version
                               %guix-bug-report-address
                               %guix-home-page-url
                               %store-directory
                               %state-directory
                               %store-database-directory
                               %config-directory
                               %libz
                               %gzip
                               %bzip2
                               %xz))

                   ;; XXX: Work around <http://bugs.gnu.org/15602>.
                   (eval-when (expand load eval)
                     #$@(map (match-lambda
                               ((name . value)
                                #~(define-public #$name #$value)))
                             %config-variables)

                     (define %store-directory
                       (or (and=> (getenv "NIX_STORE_DIR") canonicalize-path)
                           %storedir))

                     (define %state-directory
                       ;; This must match `NIX_STATE_DIR' as defined in
                       ;; `nix/local.mk'.
                       (or (getenv "GUIX_STATE_DIRECTORY")
                           (string-append %localstatedir "/guix")))

                     (define %store-database-directory
                       (or (getenv "GUIX_DATABASE_DIRECTORY")
                           (string-append %state-directory "/db")))

                     (define %config-directory
                       ;; This must match `GUIX_CONFIGURATION_DIRECTORY' as
                       ;; defined in `nix/local.mk'.
                       (or (getenv "GUIX_CONFIGURATION_DIRECTORY")
                           (string-append %sysconfdir "/guix")))

                     (define %guix-package-name #$package-name)
                     (define %guix-version #$package-version)
                     (define %guix-bug-report-address #$bug-report-address)
                     (define %guix-home-page-url #$home-page-url)

                     (define %gzip
                       #+(and gzip (file-append gzip "/bin/gzip")))
                     (define %bzip2
                       #+(and bzip2 (file-append bzip2 "/bin/bzip2")))
                     (define %xz
                       #+(and xz (file-append xz "/bin/xz")))

                     (define %libz
                       #+(and zlib
                              (file-append zlib "/lib/libz")))))))


;;;
;;; 'gexp->script'.
;;;
;;; This is our own variant of 'gexp->script' with an extra #:module-path
;;; parameter, which was unavailable in (guix gexp) until commit
;;; 1ae16033f34cebe802023922436883867010850f (March 2018.)
;;;

(define (load-path-expression modules path)
  "Return as a monadic value a gexp that sets '%load-path' and
'%load-compiled-path' to point to MODULES, a list of module names.  MODULES
are searched for in PATH."
  (mlet %store-monad ((modules  (imported-modules modules
                                                  #:module-path path))
                      (compiled (compiled-modules modules
                                                  #:module-path path)))
    (return (gexp (eval-when (expand load eval)
                    (set! %load-path
                      (cons (ungexp modules) %load-path))
                    (set! %load-compiled-path
                      (cons (ungexp compiled)
                            %load-compiled-path)))))))

(define* (gexp->script name exp
                       #:key (guile (default-guile))
                       (module-path %load-path))
  "Return an executable script NAME that runs EXP using GUILE, with EXP's
imported modules in its search path."
  (mlet %store-monad ((set-load-path
                       (load-path-expression (gexp-modules exp)
                                             module-path)))
    (gexp->derivation name
                      (gexp
                       (call-with-output-file (ungexp output)
                         (lambda (port)
                           ;; Note: that makes a long shebang.  When the store
                           ;; is /gnu/store, that fits within the 128-byte
                           ;; limit imposed by Linux, but that may go beyond
                           ;; when running tests.
                           (format port
                                   "#!~a/bin/guile --no-auto-compile~%!#~%"
                                   (ungexp guile))

                           (write '(ungexp set-load-path) port)
                           (write '(ungexp exp) port)
                           (chmod port #o555))))
                      #:module-path module-path)))


(define (date-version-string)
  "Return the current date and hour in UTC timezone, for use as a poor
person's version identifier."
  ;; XXX: Replace with a Git commit id.
  (date->string (current-date 0) "~Y~m~d.~H"))

(define guile-gcrypt
  ;; The host Guix may or may not have 'guile-gcrypt', which was introduced in
  ;; August 2018.  If it has it, it's at least version 0.1.0, which is good
  ;; enough.  If it doesn't, specify our own package because the target Guix
  ;; requires it.
  (match (find-best-packages-by-name "guile-gcrypt" #f)
    (()
     (package
       (name "guile-gcrypt")
       (version "0.1.0")
       (home-page "https://notabug.org/cwebber/guile-gcrypt")
       (source (origin
                 (method url-fetch)
                 (uri (string-append home-page "/archive/v" version ".tar.gz"))
                 (sha256
                  (base32
                   "1gir7ifknbmbvjlql5j6wzk7bkb5lnmq80q59ngz43hhpclrk5k3"))
                 (file-name (string-append name "-" version ".tar.gz"))))
       (build-system gnu-build-system)
       (arguments
        ;; The 'bootstrap' phase appeared in 'core-updates', which was merged
        ;; into 'master' ca. June 2018.
        '(#:phases (modify-phases %standard-phases
                     (delete 'bootstrap)
                     (add-before 'configure 'bootstrap
                       (lambda _
                         (unless (zero? (system* "autoreconf" "-vfi"))
                           (error "autoreconf failed"))
                         #t)))))
       (native-inputs
        `(("pkg-config" ,(specification->package "pkg-config"))
          ("autoconf" ,(specification->package "autoconf"))
          ("automake" ,(specification->package "automake"))
          ("texinfo" ,(specification->package "texinfo"))))
       (inputs
        `(("guile" ,(specification->package "guile"))
          ("libgcrypt" ,(specification->package "libgcrypt"))))
       (synopsis "Cryptography library for Guile using Libgcrypt")
       (description
        "Guile-Gcrypt provides a Guile 2.x interface to a subset of the
GNU Libgcrypt crytographic library.  It provides modules for cryptographic
hash functions, message authentication codes (MAC), public-key cryptography,
strong randomness, and more.  It is implemented using the foreign function
interface (FFI) of Guile.")
       (license #f)))                             ;license:gpl3+
    ((package . _)
     package)))

(define* (build-program source version
                        #:optional (guile-version (effective-version))
                        #:key (pull-version 0))
  "Return a program that computes the derivation to build Guix from SOURCE."
  (define select?
    ;; Select every module but (guix config) and non-Guix modules.
    (match-lambda
      (('guix 'config) #f)
      (('guix _ ...)   #t)
      (('gnu _ ...)    #t)
      (_               #f)))

  (define fake-gcrypt-hash
    ;; Fake (gcrypt hash) module; see below.
    (scheme-file "hash.scm"
                 #~(define-module (gcrypt hash)
                     #:export (sha1 sha256))))

  (define fake-git
    (scheme-file "git.scm" #~(define-module (git))))

  (with-imported-modules `(((guix config)
                            => ,(make-config.scm))

                           ;; To avoid relying on 'with-extensions', which was
                           ;; introduced in 0.15.0, provide a fake (gcrypt
                           ;; hash) just so that we can build modules, and
                           ;; adjust %LOAD-PATH later on.
                           ((gcrypt hash) => ,fake-gcrypt-hash)

                           ;; (guix git-download) depends on (git) but only
                           ;; for peripheral functionality.  Provide a dummy
                           ;; (git) to placate it.
                           ((git) => ,fake-git)

                           ,@(source-module-closure `((guix store)
                                                      (guix self)
                                                      (guix derivations)
                                                      (gnu packages bootstrap))
                                                    (list source)
                                                    #:select? select?))
    (gexp->script "compute-guix-derivation"
                  #~(begin
                      (use-modules (ice-9 match))

                      (eval-when (expand load eval)
                        ;; (gnu packages …) modules are going to be looked up
                        ;; under SOURCE.  (guix config) is looked up in FRONT.
                        (match (command-line)
                          ((_ source _ ...)
                           (match %load-path
                             ((front _ ...)
                              (unless (string=? front source) ;already done?
                                (set! %load-path
                                  (list source
                                        (string-append #$guile-gcrypt
                                                       "/share/guile/site/"
                                                       (effective-version))
                                        front)))))))

                        ;; Only load Guile-Gcrypt, our own modules, or those
                        ;; of Guile.
                        (set! %load-compiled-path
                          (cons (string-append #$guile-gcrypt "/lib/guile/"
                                               (effective-version)
                                               "/site-ccache")
                                %load-compiled-path))

                        ;; Disable position recording to save time and space
                        ;; when loading the package modules.
                        (read-disable 'positions))

                      (use-modules (guix store)
                                   (guix self)
                                   (guix derivations)
                                   (srfi srfi-1))

                      (define (spin system)
                        (define spin
                          (circular-list "-" "\\" "|" "/" "-" "\\" "|" "/"))

                        (format (current-error-port)
                                "Computing Guix derivation for '~a'...  "
                                system)
                        (when (isatty? (current-error-port))
                          (let loop ((spin spin))
                            (display (string-append "\b" (car spin))
                                     (current-error-port))
                            (force-output (current-error-port))
                            (sleep 1)
                            (loop (cdr spin)))))

                      (match (command-line)
                        ((_ source system version protocol-version)
                         ;; The current input port normally wraps a file
                         ;; descriptor connected to the daemon, or it is
                         ;; connected to /dev/null.  In the former case, reuse
                         ;; the connection such that we inherit build options
                         ;; such as substitute URLs and so on; in the latter
                         ;; case, attempt to open a new connection.
                         (let* ((proto (string->number protocol-version))
                                (store (if (integer? proto)
                                           (port->connection (duplicate-port
                                                              (current-input-port)
                                                              "w+0")
                                                             #:version proto)
                                           (open-connection))))
                           (call-with-new-thread
                            (lambda ()
                              (spin system)))

                           (display
                            (and=>
                             (run-with-store store
                               (guix-derivation source version
                                                #$guile-version
                                                #:pull-version
                                                #$pull-version)
                               #:system system)
                             derivation-file-name))))))
                  #:module-path (list source))))

(define (call-with-clean-environment thunk)
  (let ((env (environ)))
    (dynamic-wind
      (lambda ()
        (environ '()))
      thunk
      (lambda ()
        (environ env)))))

(define-syntax-rule (with-clean-environment exp ...)
  "Evaluate EXP in a context where zero environment variables are defined."
  (call-with-clean-environment (lambda () exp ...)))

;; The procedure below is our return value.
(define* (build source
                #:key verbose? (version (date-version-string)) system
                (pull-version 0)

                ;; For the standalone Guix, default to Guile 2.2.  For old
                ;; versions of 'guix pull' (pre-0.15.0), we have to use the
                ;; same Guile as the current one.
                (guile-version (if (> pull-version 0)
                                   "2.2"
                                   (effective-version)))

                #:allow-other-keys
                #:rest rest)
  "Return a derivation that unpacks SOURCE into STORE and compiles Scheme
files."
  ;; Build the build program and then use it as a trampoline to build from
  ;; SOURCE.
  (mlet %store-monad ((build  (build-program source version guile-version
                                             #:pull-version pull-version))
                      (system (if system (return system) (current-system)))
                      (port   ((store-lift nix-server-socket)))
                      (major  ((store-lift nix-server-major-version)))
                      (minor  ((store-lift nix-server-minor-version))))
    (mbegin %store-monad
      (show-what-to-build* (list build))
      (built-derivations (list build))

      ;; Use the port beneath the current store as the stdin of BUILD.  This
      ;; way, we know 'open-pipe*' will not close it on 'exec'.  If PORT is
      ;; not a file port (e.g., it's an SSH channel), then the subprocess's
      ;; stdin will actually be /dev/null.
      (let* ((pipe   (with-input-from-port port
                       (lambda ()
                         ;; Make sure BUILD is not influenced by
                         ;; $GUILE_LOAD_PATH & co.
                         (with-clean-environment
                          (setenv "GUILE_WARN_DEPRECATED" "no") ;be quiet and drive
                          (open-pipe* OPEN_READ
                                      (derivation->output-path build)
                                      source system version
                                      (if (file-port? port)
                                          (number->string
                                           (logior major minor))
                                          "none"))))))
             (str    (get-string-all pipe))
             (status (close-pipe pipe)))
        (match str
          ((? eof-object?)
           (error "build program failed" (list build status)))
          ((? derivation-path? drv)
           (mbegin %store-monad
             (return (newline (current-error-port)))
             ((store-lift add-temp-root) drv)
             (return (read-derivation-from-file drv))))
          ("#f"
           ;; Unsupported PULL-VERSION.
           (return #f))
          ((? string? str)
           (raise (condition
                   (&message
                    (message (format #f "You found a bug: the program '~a'
failed to compute the derivation for Guix (version: ~s; system: ~s;
host version: ~s; pull-version: ~s).
Please report it by email to <~a>.~%"
                                     (derivation->output-path build)
                                     version system %guix-version pull-version
                                     %guix-bug-report-address)))))))))))

;; This file is loaded by 'guix pull'; return it the build procedure.
build

;; Local Variables:
;; eval: (put 'with-load-path 'scheme-indent-function 1)
;; End:

;;; build-self.scm ends here
