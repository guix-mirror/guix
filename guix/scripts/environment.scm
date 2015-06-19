;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts environment)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix monads)
  #:use-module ((guix gexp) #:select (lower-inputs))
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module (gnu build linux-container)
  #:use-module (gnu system linux-container)
  #:use-module (gnu system file-systems)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-98)
  #:export (guix-environment))

(define (evaluate-input-search-paths inputs search-paths)
  "Evaluate SEARCH-PATHS, a list of search-path specifications, for the
directories corresponding to INPUTS, a list of (DERIVATION) or (DERIVATION
OUTPUT) tuples."
  (let ((directories (map (match-lambda
                            (((? derivation? drv))
                             (derivation->output-path drv))
                            (((? derivation? drv) output)
                             (derivation->output-path drv output))
                            (((? string? item))
                             item))
                          inputs)))
    (evaluate-search-paths search-paths directories)))

;; Protect some env vars from purification.  Borrowed from nix-shell.
(define %precious-variables
  '("HOME" "USER" "LOGNAME" "DISPLAY" "TERM" "TZ" "PAGER"))

(define %default-shell
  (or (getenv "SHELL") "/bin/sh"))

(define %network-configuration-files
  '("/etc/resolv.conf"
    "/etc/nsswitch.conf"
    "/etc/services"
    "/etc/hosts"))

(define (purify-environment)
  "Unset almost all environment variables.  A small number of variables such
as 'HOME' and 'USER' are left untouched."
  (for-each unsetenv
            (remove (cut member <> %precious-variables)
                    (match (get-environment-variables)
                      (((names . _) ...)
                       names)))))

(define (create-environment inputs paths pure?)
  "Set the environment variables specified by PATHS for all the packages
within INPUTS.  When PURE? is #t, unset the variables in the current
environment.  Otherwise, augment existing enviroment variables with additional
search paths."
  (when pure? (purify-environment))
  (for-each (match-lambda
              ((($ <search-path-specification> variable _ separator) . value)
               (let ((current (getenv variable)))
                 (setenv variable
                         (if (and current (not pure?))
                             (string-append value separator current)
                             value)))))
            (evaluate-input-search-paths inputs paths))

  ;; Give users a way to know that they're in 'guix environment', so they can
  ;; adjust 'PS1' accordingly, for instance.
  (setenv "GUIX_ENVIRONMENT" "t"))

(define (show-search-paths inputs search-paths pure?)
  "Display SEARCH-PATHS applied to the packages specified by INPUTS, a list of
 (DERIVATION) or (DERIVATION OUTPUT) tuples.  When PURE? is #t, do not augment
existing environment variables with additional search paths."
  (for-each (match-lambda
              ((search-path . value)
               (display
                (search-path-definition search-path value
                                        #:kind (if pure? 'exact 'prefix)))
               (newline)))
            (evaluate-input-search-paths inputs search-paths)))

(define (package+propagated-inputs package output)
  "Return the union of PACKAGE's OUTPUT and its transitive propagated inputs."
  `((,(package-name package) ,package ,output)
    ,@(package-transitive-propagated-inputs package)))

(define (show-help)
  (display (_ "Usage: guix environment [OPTION]... PACKAGE... [-- COMMAND...]
Build an environment that includes the dependencies of PACKAGE and execute
COMMAND or an interactive shell in that environment.\n"))
  (display (_ "
  -e, --expression=EXPR  create environment for the package that EXPR
                         evaluates to"))
  (display (_ "
  -l, --load=FILE        create environment for the package that the code within
                         FILE evaluates to"))
  (display (_ "
      --ad-hoc           include all specified packages in the environment instead
                         of only their inputs"))
  (display (_ "
      --pure             unset existing environment variables"))
  (display (_ "
      --search-paths     display needed environment variable definitions"))
  (display (_ "
  -s, --system=SYSTEM    attempt to build for SYSTEM--e.g., \"i686-linux\""))
  (display (_ "
  -C, --container        run command within an isolated container"))
  (display (_ "
  -N, --network          allow containers to access the network"))
  (display (_ "
      --share=SPEC       for containers, share writable host file system
                         according to SPEC"))
  (display (_ "
      --expose=SPEC      for containers, expose read-only host file system
                         according to SPEC"))
  (display (_ "
      --bootstrap        use bootstrap binaries to build the environment"))
  (newline)
  (show-build-options-help)
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %default-options
  ;; Default to opening a new shell.
  `((exec . (,%default-shell))
    (system . ,(%current-system))
    (substitutes? . #t)
    (max-silent-time . 3600)
    (verbosity . 0)))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix environment")))
         (option '("pure") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'pure #t result)))
         (option '(#\E "exec") #t #f ; deprecated
                 (lambda (opt name arg result)
                   (alist-cons 'exec (list %default-shell "-c" arg) result)))
         (option '("search-paths") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'search-paths #t result)))
         (option '(#\l "load") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'load arg result)))
         (option '(#\e "expression") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'expression arg result)))
         (option '("ad-hoc") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'ad-hoc? #t result)))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t result)))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg
                               (alist-delete 'system result eq?))))
         (option '(#\C "container") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'container? #t result)))
         (option '(#\N "network") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'network? #t result)))
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
         (option '("bootstrap") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'bootstrap? #t result)))
         %standard-build-options))

(define (pick-all alist key)
  "Return a list of values in ALIST associated with KEY."
  (define same-key? (cut eq? key <>))

  (fold (lambda (pair memo)
          (match pair
            (((? same-key? k) . v)
             (cons v memo))
            (_ memo)))
        '() alist))

(define (options/resolve-packages opts)
  "Return OPTS with package specification strings replaced by actual
packages."
  (append-map (match-lambda
                (('package . (? string? spec))
                 (let-values (((package output)
                               (specification->package+output spec)))
                   `((package ,package ,output))))
                (('expression . str)
                 ;; Add all the outputs of the package STR evaluates to.
                 (match (read/eval str)
                   ((? package? package)
                    (map (lambda (output)
                           `(package ,package ,output))
                         (package-outputs package)))))
                (('load . file)
                 ;; Add all the outputs of the package defined in FILE.
                 (let ((package (load* file (make-user-module '()))))
                   (map (lambda (output)
                          `(package ,package ,output))
                        (package-outputs package))))
                (opt (list opt)))
              opts))

(define (build-inputs inputs opts)
  "Build the derivations in INPUTS, a list of (DERIVATION) or (DERIVATION
OUTPUT) tuples, using the build options in OPTS."
  (let ((substitutes? (assoc-ref opts 'substitutes?))
        (dry-run?     (assoc-ref opts 'dry-run?)))
    (match inputs
      (((derivations _ ...) ...)
       (mbegin %store-monad
         (show-what-to-build* derivations
                              #:use-substitutes? substitutes?
                              #:dry-run? dry-run?)
         (if dry-run?
             (return #f)
             (mbegin %store-monad
               (set-build-options-from-command-line* opts)
               (built-derivations derivations)
               (return derivations))))))))

(define requisites* (store-lift requisites))

(define (inputs->requisites inputs)
  "Convert INPUTS, a list of input tuples or store path strings, into a set of
requisite store items i.e. the union closure of all the inputs."
  (define (input->requisites input)
    (requisites*
     (match input
       ((drv output)
        (derivation->output-path drv output))
       ((drv)
        (derivation->output-path drv))
       ((? direct-store-path? path)
        path))))

  (mlet %store-monad ((reqs (sequence %store-monad
                                      (map input->requisites inputs))))
    (return (delete-duplicates (concatenate reqs)))))

(define exit/status (compose exit status:exit-val))
(define primitive-exit/status (compose primitive-exit status:exit-val))

(define (launch-environment command inputs paths pure?)
  "Run COMMAND in a new environment containing INPUTS, using the native search
paths defined by the list PATHS.  When PURE?, pre-existing environment
variables are cleared before setting the new ones."
  (create-environment inputs paths pure?)
  (apply system* command))

(define* (launch-environment/container #:key command bash user-mappings
                                       inputs paths network?)
  "Run COMMAND within a Linux container.  The environment features INPUTS, a
list of derivations to be shared from the host system.  Environment variables
are set according to PATHS, a list of native search paths.  The global shell
is BASH, a file name for a GNU Bash binary in the store.  When NETWORK?,
access to the host system network is permitted.  USER-MAPPINGS, a list of file
system mappings, contains the user-specified host file systems to mount inside
the container."
  (mlet %store-monad ((reqs (inputs->requisites
                             (cons (direct-store-path bash) inputs))))
    (return
     (let* ((cwd (getcwd))
            ;; Bind-mount all requisite store items, user-specified mappings,
            ;; /bin/sh, the current working directory, and possibly networking
            ;; configuration files within the container.
            (mappings
             (append user-mappings
                     ;; Current working directory.
                     (list (file-system-mapping
                            (source cwd)
                            (target cwd)
                            (writable? #t)))
                     ;; When in Rome, do as Nix build.cc does: Automagically
                     ;; map common network configuration files.
                     (if network?
                         (filter-map (lambda (file)
                                       (and (file-exists? file)
                                            (file-system-mapping
                                             (source file)
                                             (target file)
                                             (writable? #f))))
                                     %network-configuration-files)
                         '())
                     ;; Mappings for the union closure of all inputs.
                     (map (lambda (dir)
                            (file-system-mapping
                             (source dir)
                             (target dir)
                             (writable? #f)))
                          reqs)))
            (file-systems (append %container-file-systems
                                  (map mapping->file-system mappings))))
       (exit/status
        (call-with-container (map file-system->spec file-systems)
          (lambda ()
            ;; Setup global shell.
            (mkdir-p "/bin")
            (symlink bash "/bin/sh")

            ;; Setup directory for temporary files.
            (mkdir-p "/tmp")
            (for-each (lambda (var)
                        (setenv var "/tmp"))
                      ;; The same variables as in Nix's 'build.cc'.
                      '("TMPDIR" "TEMPDIR" "TMP" "TEMP"))

            ;; From Nix build.cc:
            ;;
            ;; Set HOME to a non-existing path to prevent certain
            ;; programs from using /etc/passwd (or NIS, or whatever)
            ;; to locate the home directory (for example, wget looks
            ;; for ~/.wgetrc).  I.e., these tools use /etc/passwd if
            ;; HOME is not set, but they will just assume that the
            ;; settings file they are looking for does not exist if
            ;; HOME is set but points to some non-existing path.
            (setenv "HOME" "/homeless-shelter")

            ;; For convenience, start in the user's current working
            ;; directory rather than the root directory.
            (chdir cwd)

            (primitive-exit/status
             ;; A container's environment is already purified, so no need to
             ;; request it be purified again.
             (launch-environment command inputs paths #f)))
          #:namespaces (if network?
                           (delq 'net %namespaces) ; share host network
                           %namespaces)))))))

(define (environment-bash container? bootstrap? system)
  "Return a monadic value in the store monad for the version of GNU Bash
needed in the environment for SYSTEM, if any.  If CONTAINER? is #f, return #f.
If CONTAINER? and BOOTSTRAP?, return the store path for the bootstrap Bash.
Otherwise, return the derivation for the Bash package."
  (with-monad %store-monad
    (cond
     ((and container? (not bootstrap?))
      (package->derivation bash))
     ;; Use the bootstrap Bash instead.
     ((and container? bootstrap?)
      (interned-file
       (search-bootstrap-binary "bash" system)))
     (else
      (return #f)))))

(define (parse-args args)
  "Parse the list of command line arguments ARGS."
  (define (handle-argument arg result)
    (alist-cons 'package arg result))

  ;; The '--' token is used to separate the command to run from the rest of
  ;; the operands.
  (let-values (((args command) (split args "--")))
    (let ((opts (parse-command-line args %options (list %default-options)
                                    #:argument-handler handle-argument)))
      (if (null? command)
          opts
          (alist-cons 'exec command opts)))))

;; Entry point.
(define (guix-environment . args)
  (with-error-handling
    (let* ((opts       (parse-args args))
           (pure?      (assoc-ref opts 'pure))
           (container? (assoc-ref opts 'container?))
           (network?   (assoc-ref opts 'network?))
           (ad-hoc?    (assoc-ref opts 'ad-hoc?))
           (bootstrap? (assoc-ref opts 'bootstrap?))
           (system     (assoc-ref opts 'system))
           (command    (assoc-ref opts 'exec))
           (packages   (pick-all (options/resolve-packages opts) 'package))
           (mappings   (pick-all opts 'file-system-mapping))
           (inputs     (if ad-hoc?
                           (append-map (match-lambda
                                        ((package output)
                                         (package+propagated-inputs package
                                                                    output)))
                                       packages)
                           (append-map (compose bag-transitive-inputs
                                                package->bag
                                                first)
                                       packages)))
           (paths      (delete-duplicates
                        (cons $PATH
                              (append-map (match-lambda
                                           ((label (? package? p) _ ...)
                                            (package-native-search-paths p))
                                           (_
                                            '()))
                                          inputs))
                        eq?)))
      (with-store store
        (run-with-store store
          (mlet* %store-monad ((inputs (lower-inputs
                                        (map (match-lambda
                                              ((label item)
                                               (list item))
                                              ((label item output)
                                               (list item output)))
                                             inputs)
                                        #:system system))
                               ;; Containers need a Bourne shell at /bin/sh.
                               (bash (environment-bash container?
                                                       bootstrap?
                                                       system)))
            (mbegin %store-monad
              ;; First build the inputs.  This is necessary even for
              ;; --search-paths.  Additionally, we might need to build bash
              ;; for a container.
              (build-inputs (if (derivation? bash)
                                `((,bash "out") ,@inputs)
                                inputs)
                            opts)
              (cond
               ((assoc-ref opts 'dry-run?)
                (return #t))
               ((assoc-ref opts 'search-paths)
                (show-search-paths inputs paths pure?)
                (return #t))
               (container?
                (let ((bash-binary
                       (if bootstrap?
                           bash
                           (string-append (derivation->output-path bash)
                                          "/bin/sh"))))
                  (launch-environment/container #:command command
                                                #:bash bash-binary
                                                #:user-mappings mappings
                                                #:inputs inputs
                                                #:paths paths
                                                #:network? network?)))
               (else
                (return
                 (exit/status
                  (launch-environment command inputs paths pure?))))))))))))
