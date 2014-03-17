;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts archive)
  #:use-module (guix config)
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix monads)
  #:use-module (guix ui)
  #:use-module (guix pki)
  #:use-module (guix pk-crypto)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (guix scripts build)
  #:use-module (guix scripts package)
  #:use-module (rnrs io ports)
  #:export (guix-archive))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((system . ,(%current-system))
    (substitutes? . #t)
    (max-silent-time . 3600)
    (verbosity . 0)))

(define (show-help)
  (display (_ "Usage: guix archive [OPTION]... PACKAGE...
Export/import one or more packages from/to the store.\n"))
  (display (_ "
      --export           export the specified files/packages to stdout"))
  (display (_ "
      --import           import from the archive passed on stdin"))
  (display (_ "
      --missing          print the files from stdin that are missing"))
  (newline)
  (display (_ "
      --generate-key[=PARAMETERS]
                         generate a key pair with the given parameters"))
  (display (_ "
      --authorize        authorize imports signed by the public key on stdin"))
  (newline)
  (display (_ "
  -e, --expression=EXPR  build the package or derivation EXPR evaluates to"))
  (display (_ "
  -S, --source           build the packages' source derivations"))
  (display (_ "
  -s, --system=SYSTEM    attempt to build for SYSTEM--e.g., \"i686-linux\""))
  (display (_ "
      --target=TRIPLET   cross-build for TRIPLET--e.g., \"armel-linux-gnu\""))

  (newline)
  (show-build-options-help)

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
                   (show-version-and-exit "guix build")))

         (option '("export") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'export #t result)))
         (option '("import") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'import #t result)))
         (option '("missing") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'missing #t result)))
         (option '("generate-key") #f #t
                 (lambda (opt name arg result)
                   (catch 'gcry-error
                     (lambda ()
                       (let ((params
                              (string->canonical-sexp
                               (or arg "(genkey (rsa (nbits 4:4096)))"))))
                         (alist-cons 'generate-key params result)))
                     (lambda (key err)
                       (leave (_ "invalid key generation parameters: ~a: ~a~%")
                              (error-source err)
                              (error-string err))))))
         (option '("authorize") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'authorize #t result)))

         (option '(#\S "source") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'source? #t result)))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg
                               (alist-delete 'system result eq?))))
         (option '("target") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'target arg
                               (alist-delete 'target result eq?))))
         (option '(#\e "expression") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'expression arg result)))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t result)))
         (option '(#\r "root") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'gc-root arg result)))

         %standard-build-options))

(define (derivation-from-expression store str package-derivation
                                    system source?)
  "Read/eval STR and return the corresponding derivation path for SYSTEM.
When SOURCE? is true and STR evaluates to a package, return the derivation of
the package source; otherwise, use PACKAGE-DERIVATION to compute the
derivation of a package."
  (match (read/eval str)
    ((? package? p)
     (if source?
         (let ((source (package-source p)))
           (if source
               (package-source-derivation store source)
               (leave (_ "package `~a' has no source~%")
                      (package-name p))))
         (package-derivation store p system)))
    ((? procedure? proc)
     (run-with-store store (proc) #:system system))))

(define (options->derivations+files store opts)
  "Given OPTS, the result of 'args-fold', return a list of derivations to
build and a list of store files to transfer."
  (define package->derivation
    (match (assoc-ref opts 'target)
      (#f package-derivation)
      (triplet
       (cut package-cross-derivation <> <> triplet <>))))

  (define src? (assoc-ref opts 'source?))
  (define sys  (assoc-ref opts 'system))

  (fold2 (lambda (arg derivations files)
           (match arg
             (('expression . str)
              (let ((drv (derivation-from-expression store str
                                                     package->derivation
                                                     sys src?)))
                (values (cons drv derivations)
                        (cons (derivation->output-path drv) files))))
             (('argument . (? store-path? file))
              (values derivations (cons file files)))
             (('argument . (? string? spec))
              (let-values (((p output)
                            (specification->package+output spec)))
                (if src?
                    (let* ((s   (package-source p))
                           (drv (package-source-derivation store s)))
                      (values (cons drv derivations)
                              (cons (derivation->output-path drv)
                                    files)))
                    (let ((drv (package->derivation store p sys)))
                      (values (cons drv derivations)
                              (cons (derivation->output-path drv output)
                                    files))))))
             (_
              (values derivations files))))
         '()
         '()
         opts))


;;;
;;; Entry point.
;;;

(define (export-from-store store opts)
  "Export the packages or derivations specified in OPTS from STORE.  Write the
resulting archive to the standard output port."
  (let-values (((drv files)
                (options->derivations+files store opts)))
    (set-build-options-from-command-line store opts)
    (show-what-to-build store drv
                        #:use-substitutes? (assoc-ref opts 'substitutes?)
                        #:dry-run? (assoc-ref opts 'dry-run?))

    (if (or (assoc-ref opts 'dry-run?)
            (build-derivations store drv))
        (export-paths store files (current-output-port))
        (leave (_ "unable to export the given packages~%")))))

(define (generate-key-pair parameters)
  "Generate a key pair with PARAMETERS, a canonical sexp, and store it in the
right place."
  (when (or (file-exists? %public-key-file)
            (file-exists? %private-key-file))
    (leave (_ "key pair exists under '~a'; remove it first~%")
           (dirname %public-key-file)))

  (format (current-error-port)
          (_ "Please wait while gathering entropy to generate the key pair;
this may take time...~%"))

  (let* ((pair   (catch 'gcry-error
                   (lambda ()
                     (generate-key parameters))
                   (lambda (key err)
                     (leave (_ "key generation failed: ~a: ~a~%")
                            (error-source err)
                            (error-string err)))))
         (public (find-sexp-token pair 'public-key))
         (secret (find-sexp-token pair 'private-key)))
    ;; Create the following files as #o400.
    (umask #o266)

    (mkdir-p (dirname %public-key-file))
    (with-atomic-file-output %public-key-file
      (lambda (port)
        (display (canonical-sexp->string public) port)))
    (with-atomic-file-output %private-key-file
      (lambda (port)
        (display (canonical-sexp->string secret) port)))

    ;; Make the public key readable by everyone.
    (chmod %public-key-file #o444)))

(define (authorize-key)
  "Authorize imports signed by the public key passed as an advanced sexp on
the input port."
  (define (read-key)
    (catch 'gcry-error
      (lambda ()
        (string->canonical-sexp (get-string-all (current-input-port))))
      (lambda (key err)
        (leave (_ "failed to read public key: ~a: ~a~%")
               (error-source err) (error-string err)))))

  (let ((key (read-key))
        (acl (current-acl)))
    (unless (eq? 'public-key (canonical-sexp-nth-data key 0))
      (leave (_ "s-expression does not denote a public key~%")))

    ;; Add KEY to the ACL and write that.
    (let ((acl (public-keys->acl (cons key (acl->public-keys acl)))))
      (with-atomic-file-output %acl-file
        (lambda (port)
          (display (canonical-sexp->string acl) port))))))

(define (guix-archive . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (alist-cons 'argument arg result))
                %default-options))

  (define (lines port)
    ;; Return lines read from PORT.
    (let loop ((line   (read-line port))
               (result '()))
      (if (eof-object? line)
          (reverse result)
          (loop (read-line port)
                (cons line result)))))

  (with-error-handling
    ;; Ask for absolute file names so that .drv file names passed from the
    ;; user to 'read-derivation' are absolute when it returns.
    (with-fluids ((%file-port-name-canonicalization 'absolute))
      (let ((opts (parse-options)))
        (cond ((assoc-ref opts 'generate-key)
               =>
               generate-key-pair)
              ((assoc-ref opts 'authorize)
               (authorize-key))
              (else
               (let ((store (open-connection)))
                 (cond ((assoc-ref opts 'export)
                        (export-from-store store opts))
                       ((assoc-ref opts 'import)
                        (import-paths store (current-input-port)))
                       ((assoc-ref opts 'missing)
                        (let* ((files   (lines (current-input-port)))
                               (missing (remove (cut valid-path? store <>)
                                                files)))
                          (format #t "~{~a~%~}" missing)))
                       (else
                        (leave
                         (_ "either '--export' or '--import' \
must be specified~%")))))))))))
