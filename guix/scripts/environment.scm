;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2018 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Mike Gerwitz <mtg@gnu.org>
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
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix grafts)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix search-paths)
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
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages bootstrap) #:select (%bootstrap-guile))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-98)
  #:export (guix-environment))

;; Protect some env vars from purification.  Borrowed from nix-shell.
(define %precious-variables
  '("HOME" "USER" "LOGNAME" "DISPLAY" "TERM" "TZ" "PAGER"))

(define %default-shell
  (or (getenv "SHELL") "/bin/sh"))

(define (purify-environment white-list)
  "Unset all environment variables except those that match the regexps in
WHITE-LIST and those listed in %PRECIOUS-VARIABLES.  A small number of
variables such as 'HOME' and 'USER' are left untouched."
  (for-each unsetenv
            (remove (lambda (variable)
                      (or (member variable %precious-variables)
                          (find (cut regexp-exec <> variable)
                                white-list)))
                    (match (get-environment-variables)
                      (((names . _) ...)
                       names)))))

(define* (create-environment profile manifest
                             #:key pure? (white-list '()))
  "Set the environment variables specified by MANIFEST for PROFILE.  When
PURE?  is #t, unset the variables in the current environment except those that
match the regexps in WHITE-LIST.  Otherwise, augment existing environment
variables with additional search paths."
  (when pure?
    (purify-environment white-list))
  (for-each (match-lambda
              ((($ <search-path-specification> variable _ separator) . value)
               (let ((current (getenv variable)))
                 (setenv variable
                         (if (and current (not pure?))
                             (if separator
                                 (string-append value separator current)
                                 value)
                             value)))))
            (profile-search-paths profile manifest))

  ;; Give users a way to know that they're in 'guix environment', so they can
  ;; adjust 'PS1' accordingly, for instance.  Set it to PROFILE so users can
  ;; conveniently access its contents.
  (setenv "GUIX_ENVIRONMENT" profile))

(define* (show-search-paths profile manifest #:key pure?)
  "Display the search paths of MANIFEST applied to PROFILE.  When PURE? is #t,
do not augment existing environment variables with additional search paths."
  (for-each (match-lambda
              ((search-path . value)
               (display
                (search-path-definition search-path value
                                        #:kind (if pure? 'exact 'prefix)))
               (newline)))
            (profile-search-paths profile manifest)))

(define (input->manifest-entry input)
  "Return a manifest entry for INPUT, or #f if INPUT does not correspond to a
package."
  (match input
    ((_ (? package? package))
     (package->manifest-entry package))
    ((_ (? package? package) output)
     (package->manifest-entry package output))
    (_
     #f)))

(define (package-environment-inputs package)
  "Return a list of manifest entries corresponding to the transitive input
packages for PACKAGE."
  ;; Remove non-package inputs such as origin records.
  (filter-map input->manifest-entry
              (bag-transitive-inputs (package->bag package))))

(define (show-help)
  (display (G_ "Usage: guix environment [OPTION]... PACKAGE... [-- COMMAND...]
Build an environment that includes the dependencies of PACKAGE and execute
COMMAND or an interactive shell in that environment.\n"))
  (display (G_ "
  -e, --expression=EXPR  create environment for the package that EXPR
                         evaluates to"))
  (display (G_ "
  -l, --load=FILE        create environment for the package that the code within
                         FILE evaluates to"))
  (display (G_ "
  -m, --manifest=FILE    create environment with the manifest from FILE"))
  (display (G_ "
      --ad-hoc           include all specified packages in the environment instead
                         of only their inputs"))
  (display (G_ "
      --pure             unset existing environment variables"))
  (display (G_ "
  -E, --preserve=REGEXP  preserve environment variables that match REGEXP"))
  (display (G_ "
      --search-paths     display needed environment variable definitions"))
  (display (G_ "
  -s, --system=SYSTEM    attempt to build for SYSTEM--e.g., \"i686-linux\""))
  (display (G_ "
  -r, --root=FILE        make FILE a symlink to the result, and register it
                         as a garbage collector root"))
  (display (G_ "
  -C, --container        run command within an isolated container"))
  (display (G_ "
  -N, --network          allow containers to access the network"))
  (display (G_ "
  -P, --link-profile     link environment profile to ~/.guix-profile within
                         an isolated container"))
  (display (G_ "
  -u, --user=USER        instead of copying the name and home of the current
                         user into an isolated container, use the name USER
                         with home directory /home/USER"))
  (display (G_ "
      --share=SPEC       for containers, share writable host file system
                         according to SPEC"))
  (display (G_ "
      --expose=SPEC      for containers, expose read-only host file system
                         according to SPEC"))
  (display (G_ "
  -v, --verbosity=LEVEL  use the given verbosity LEVEL"))
  (display (G_ "
      --bootstrap        use bootstrap binaries to build the environment"))
  (newline)
  (show-build-options-help)
  (newline)
  (show-transformation-options-help)
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %default-options
  `((system . ,(%current-system))
    (substitutes? . #t)
    (build-hook? . #t)
    (graft? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (debug . 0)
    (verbosity . 2)))

(define (tag-package-arg opts arg)
  "Return a two-element list with the form (TAG ARG) that tags ARG with either
'ad-hoc' in OPTS has the 'ad-hoc?' key set to #t, or 'inputs' otherwise."
  ;; Normally, the transitive inputs to a package are added to an environment,
  ;; but the ad-hoc? flag changes the meaning of a package argument such that
  ;; the package itself is added to the environment instead.
  (if (assoc-ref opts 'ad-hoc?)
      `(ad-hoc-package ,arg)
      `(package ,arg)))

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
         (option '(#\E "preserve") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'inherit-regexp
                               (make-regexp* arg)
                               result)))
         (option '("inherit") #t #f               ;deprecated
                 (lambda (opt name arg result)
                   (warning (G_ "'--inherit' is deprecated, \
use '--preserve' instead~%"))
                   (alist-cons 'inherit-regexp
                               (make-regexp* arg)
                               result)))
         (option '("search-paths") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'search-paths #t result)))
         (option '(#\l "load") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'load
                               (tag-package-arg result arg)
                               result)))
         (option '(#\e "expression") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'expression
                               (tag-package-arg result arg)
                               result)))
         (option '(#\m "manifest") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'manifest
                               arg
                               result)))
         (option '("ad-hoc") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'ad-hoc? #t result)))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t (alist-cons 'graft? #f result))))
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
         (option '(#\P "link-profile") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'link-profile? #t result)))
         (option '(#\u "user") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'user arg
                               (alist-delete 'user result eq?))))
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
         (option '(#\r "root") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'gc-root arg result)))
         (option '(#\v "verbosity") #t #f
                 (lambda (opt name arg result)
                   (let ((level (string->number* arg)))
                     (alist-cons 'verbosity level
                                 (alist-delete 'verbosity result)))))
         (option '("bootstrap") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'bootstrap? #t result)))

         (append %transformation-options
                 %standard-build-options)))

(define (pick-all alist key)
  "Return a list of values in ALIST associated with KEY."
  (define same-key? (cut eq? key <>))

  (fold (lambda (pair memo)
          (match pair
            (((? same-key? k) . v)
             (cons v memo))
            (_ memo)))
        '() alist))

(define (options/resolve-packages store opts)
  "Return OPTS with package specification strings replaced by manifest entries
for the corresponding packages."
  (define (manifest-entry=? e1 e2)
    (and (eq? (manifest-entry-item e1) (manifest-entry-item e2))
         (string=? (manifest-entry-output e1)
                   (manifest-entry-output e2))))

  (define transform
    (cut (options->transformation opts) store <>))

  (define* (package->manifest-entry* package #:optional (output "out"))
    (package->manifest-entry (transform package) output))

  (define (packages->outputs packages mode)
    (match packages
      ((? package? package)
       (if (eq? mode 'ad-hoc-package)
           (list (package->manifest-entry* package))
           (package-environment-inputs package)))
      (((? package? package) (? string? output))
       (if (eq? mode 'ad-hoc-package)
           (list (package->manifest-entry* package output))
           (package-environment-inputs package)))
      ((lst ...)
       (append-map (cut packages->outputs <> mode) lst))))

  (manifest
   (delete-duplicates
    (append-map (match-lambda
                  (('package 'ad-hoc-package (? string? spec))
                   (let-values (((package output)
                                 (specification->package+output spec)))
                     (list (package->manifest-entry* package output))))
                  (('package 'package (? string? spec))
                   (package-environment-inputs
                    (specification->package+output spec)))
                  (('expression mode str)
                   ;; Add all the outputs of the package STR evaluates to.
                   (packages->outputs (read/eval str) mode))
                  (('load mode file)
                   ;; Add all the outputs of the package defined in FILE.
                   (let ((module (make-user-module '())))
                     (packages->outputs (load* file module) mode)))
                  (('manifest . file)
                   (let ((module (make-user-module '((guix profiles) (gnu)))))
                     (manifest-entries (load* file module))))
                  (_ '()))
                opts)
    manifest-entry=?)))

(define* (build-environment derivations opts)
  "Build the DERIVATIONS required by the environment using the build options
in OPTS."
  (let ((substitutes? (assoc-ref opts 'substitutes?))
        (dry-run?     (assoc-ref opts 'dry-run?)))
    (mbegin %store-monad
      (show-what-to-build* derivations
                           #:use-substitutes? substitutes?
                           #:dry-run? dry-run?)
      (if dry-run?
          (return #f)
          (built-derivations derivations)))))

(define (manifest->derivation manifest system bootstrap?)
  "Return the derivation for a profile of MANIFEST.
BOOTSTRAP? specifies whether to use the bootstrap Guile to build the profile."
  (profile-derivation manifest
                      #:system system

                      ;; Packages can have conflicting inputs, or explicit
                      ;; inputs that conflict with implicit inputs (e.g., gcc,
                      ;; gzip, etc.).  Thus, do not error out when we
                      ;; encounter collision.
                      #:allow-collisions? #t

                      #:hooks (if bootstrap?
                                  '()
                                  %default-profile-hooks)
                      #:locales? (not bootstrap?)))

(define requisites* (store-lift requisites))

(define (inputs->requisites inputs)
  "Convert INPUTS, a list of input tuples or store path strings, into a set of
requisite store items i.e. the union closure of all the inputs."
  (define (input->requisites input)
    (requisites*
     (match input
       ((drv output)
        (list (derivation->output-path drv output)))
       ((drv)
        (list (derivation->output-path drv)))
       ((? direct-store-path? path)
        (list path)))))

  (mlet %store-monad ((reqs (mapm %store-monad
                                  input->requisites inputs)))
    (return (delete-duplicates (concatenate reqs)))))

(define (status->exit-code status)
  "Compute the exit code made from STATUS, a value as returned by 'waitpid',
and suitable for 'exit'."
  ;; See <bits/waitstatus.h>.
  (or (status:exit-val status)
      (logior #x80 (status:term-sig status))))

(define exit/status (compose exit status->exit-code))
(define primitive-exit/status (compose primitive-exit status->exit-code))

(define* (launch-environment command profile manifest
                             #:key pure? (white-list '()))
  "Run COMMAND in a new environment containing INPUTS, using the native search
paths defined by the list PATHS.  When PURE?, pre-existing environment
variables are cleared before setting the new ones, except those matching the
regexps in WHITE-LIST."
  ;; Properly handle SIGINT, so pressing C-c in an interactive terminal
  ;; application works.
  (sigaction SIGINT SIG_DFL)
  (create-environment profile manifest
                      #:pure? pure? #:white-list white-list)
  (match command
    ((program . args)
     (apply execlp program program args))))

(define* (launch-environment/fork command profile manifest
                                  #:key pure? (white-list '()))
  "Run COMMAND in a new process with an environment containing PROFILE, with
the search paths specified by MANIFEST.  When PURE?, pre-existing environment
variables are cleared before setting the new ones, except those matching the
regexps in WHITE-LIST."
  (match (primitive-fork)
    (0 (launch-environment command profile manifest
                           #:pure? pure?
                           #:white-list white-list))
    (pid (match (waitpid pid)
           ((_ . status) status)))))

(define* (launch-environment/container #:key command bash user user-mappings
                                       profile manifest link-profile? network?)
  "Run COMMAND within a container that features the software in PROFILE.
Environment variables are set according to the search paths of MANIFEST.
The global shell is BASH, a file name for a GNU Bash binary in the
store.  When NETWORK?, access to the host system network is permitted.
USER-MAPPINGS, a list of file system mappings, contains the user-specified
host file systems to mount inside the container.  If USER is not #f, each
target of USER-MAPPINGS will be re-written relative to '/home/USER', and USER
will be used for the passwd entry.  LINK-PROFILE? creates a symbolic link from
~/.guix-profile to the environment profile."
  (mlet %store-monad ((reqs (inputs->requisites
                             (list (direct-store-path bash) profile))))
    (return
     (let* ((cwd      (getcwd))
            (home     (getenv "HOME"))
            (passwd   (mock-passwd (getpwuid (getuid))
                                   user
                                   bash))
            (home-dir (passwd:dir passwd))
            ;; Bind-mount all requisite store items, user-specified mappings,
            ;; /bin/sh, the current working directory, and possibly networking
            ;; configuration files within the container.
            (mappings
             (override-user-mappings
              user home
              (append user-mappings
                      ;; Current working directory.
                      (list (file-system-mapping
                             (source cwd)
                             (target cwd)
                             (writable? #t)))
                      ;; When in Rome, do as Nix build.cc does: Automagically
                      ;; map common network configuration files.
                      (if network?
                          %network-file-mappings
                          '())
                      ;; Mappings for the union closure of all inputs.
                      (map (lambda (dir)
                             (file-system-mapping
                              (source dir)
                              (target dir)
                              (writable? #f)))
                           reqs))))
            (file-systems (append %container-file-systems
                                  (map file-system-mapping->bind-mount
                                       mappings))))
       (exit/status
        (call-with-container file-systems
          (lambda ()
            ;; Setup global shell.
            (mkdir-p "/bin")
            (symlink bash "/bin/sh")

            ;; Set a reasonable default PS1.
            (setenv "PS1" "\\u@\\h \\w [env]\\$ ")

            ;; Setup directory for temporary files.
            (mkdir-p "/tmp")
            (for-each (lambda (var)
                        (setenv var "/tmp"))
                      ;; The same variables as in Nix's 'build.cc'.
                      '("TMPDIR" "TEMPDIR" "TMP" "TEMP"))

            ;; Create a dummy home directory.
            (mkdir-p home-dir)
            (setenv "HOME" home-dir)

            ;; If requested, link $GUIX_ENVIRONMENT to $HOME/.guix-profile;
            ;; this allows programs expecting that path to continue working as
            ;; expected within a container.
            (when link-profile? (link-environment profile home-dir))

            ;; Create a dummy /etc/passwd to satisfy applications that demand
            ;; to read it, such as 'git clone' over SSH, a valid use-case when
            ;; sharing the host's network namespace.
            (mkdir-p "/etc")
            (call-with-output-file "/etc/passwd"
              (lambda (port)
                (display (string-join (list (passwd:name passwd)
                                            "x" ; but there is no shadow
                                            "0" "0" ; user is now root
                                            (passwd:gecos passwd)
                                            (passwd:dir passwd)
                                            bash)
                                      ":")
                         port)
                (newline port)))

            ;; For convenience, start in the user's current working
            ;; directory rather than the root directory.
            (chdir (override-user-dir user home cwd))

            (primitive-exit/status
             ;; A container's environment is already purified, so no need to
             ;; request it be purified again.
             (launch-environment command profile manifest #:pure? #f)))
          #:namespaces (if network?
                           (delq 'net %namespaces) ; share host network
                           %namespaces)))))))

(define (mock-passwd passwd user-override shell)
  "Generate mock information for '/etc/passwd'.  If USER-OVERRIDE is not '#f',
it is expected to be a string representing the mock username; it will produce
a user of that name, with a home directory of '/home/USER-OVERRIDE', and no
GECOS field.  If USER-OVERRIDE is '#f', data will be inherited from PASSWD.
In either case, the shadow password and UID/GID are cleared, since the user
runs as root within the container.  SHELL will always be used in place of the
shell in PASSWD.

The resulting vector is suitable for use with Guile's POSIX user procedures.

See passwd(5) for more information each of the fields."
  (if user-override
      (vector
       user-override
        "x" "0" "0"  ;; no shadow, user is now root
        ""           ;; no personal information
        (user-override-home user-override)
        shell)
      (vector
       (passwd:name passwd)
        "x" "0" "0"  ;; no shadow, user is now root
        (passwd:gecos passwd)
        (passwd:dir passwd)
        shell)))

(define (user-override-home user)
  "Return home directory for override user USER."
  (string-append "/home/" user))

(define (override-user-mappings user home mappings)
  "If a username USER is provided, rewrite each HOME prefix in file system
mappings MAPPINGS to a home directory determined by 'override-user-dir';
otherwise, return MAPPINGS."
  (if (not user)
      mappings
      (map (lambda (mapping)
             (let ((target (file-system-mapping-target mapping)))
               (if (string-prefix? home target)
                   (file-system-mapping
                    (source    (file-system-mapping-source mapping))
                    (target    (override-user-dir user home target))
                    (writable? (file-system-mapping-writable? mapping)))
                   mapping)))
           mappings)))

(define (override-user-dir user home dir)
  "If username USER is provided, overwrite string prefix HOME in DIR with a
directory determined by 'user-override-home'; otherwise, return DIR."
  (if (and user (string-prefix? home dir))
      (string-append (user-override-home user)
                     (substring dir (string-length home)))
      dir))

(define (link-environment profile home-dir)
  "Create a symbolic link from HOME-DIR/.guix-profile to PROFILE."
  (let ((profile-dir (string-append home-dir "/.guix-profile")))
    (catch 'system-error
      (lambda ()
        (symlink profile profile-dir))
      (lambda args
        (if (= EEXIST (system-error-errno args))
            (leave (G_ "cannot link profile: '~a' already exists within container~%")
                   profile-dir)
            (apply throw args))))))

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
    (alist-cons 'package (tag-package-arg result arg) result))

  ;; The '--' token is used to separate the command to run from the rest of
  ;; the operands.
  (let-values (((args command) (break (cut string=? "--" <>) args)))
    (let ((opts (parse-command-line args %options (list %default-options)
                                    #:argument-handler handle-argument)))
      (match command
        (() opts)
        (("--") opts)
        (("--" command ...) (alist-cons 'exec command opts))))))

(define (assert-container-features)
  "Check if containers can be created and exit with an informative error
message if any test fails."
  (unless (user-namespace-supported?)
    (report-error (G_ "cannot create container: user namespaces unavailable\n"))
    (leave (G_ "is your kernel version < 3.10?\n")))

  (unless (unprivileged-user-namespace-supported?)
    (report-error (G_ "cannot create container: unprivileged user cannot create user namespaces\n"))
    (leave (G_ "please set /proc/sys/kernel/unprivileged_userns_clone to \"1\"\n")))

  (unless (setgroups-supported?)
    (report-error (G_ "cannot create container: /proc/self/setgroups does not exist\n"))
    (leave (G_ "is your kernel version < 3.19?\n"))))

(define (register-gc-root target root)
  "Make ROOT an indirect root to TARGET.  This is procedure is idempotent."
  (let* ((root (if (string-prefix? "/" root)
                   root
                   (string-append (canonicalize-path (dirname root))
                                  "/" root))))
    (catch 'system-error
      (lambda ()
        (symlink target root)
        ((store-lift add-indirect-root) root))
      (lambda args
        (if (and (= EEXIST (system-error-errno args))
                 (equal? (false-if-exception (readlink root)) target))
            (with-monad %store-monad
              (return #t))
            (apply throw args))))))


;;;
;;; Entry point.
;;;

(define (guix-environment . args)
  (with-error-handling
    (let* ((opts       (parse-args args))
           (pure?      (assoc-ref opts 'pure))
           (container? (assoc-ref opts 'container?))
           (link-prof? (assoc-ref opts 'link-profile?))
           (network?   (assoc-ref opts 'network?))
           (user       (assoc-ref opts 'user))
           (bootstrap? (assoc-ref opts 'bootstrap?))
           (system     (assoc-ref opts 'system))
           (command    (or (assoc-ref opts 'exec)
                           ;; Spawn a shell if the user didn't specify
                           ;; anything in particular.
                           (if container?
                               ;; The user's shell is likely not available
                               ;; within the container.
                               '("/bin/sh")
                               (list %default-shell))))
           (mappings   (pick-all opts 'file-system-mapping))
           (white-list (pick-all opts 'inherit-regexp)))

      (when container? (assert-container-features))

      (when (and (not container?) link-prof?)
        (leave (G_ "'--link-profile' cannot be used without '--container'~%")))
      (when (and (not container?) user)
        (leave (G_ "'--user' cannot be used without '--container'~%")))

      (with-store store
        (with-status-verbosity (assoc-ref opts 'verbosity)
          (define manifest
            (options/resolve-packages store opts))

          (set-build-options-from-command-line store opts)

          ;; Use the bootstrap Guile when requested.
          (parameterize ((%graft? (assoc-ref opts 'graft?))
                         (%guile-for-build
                          (package-derivation
                           store
                           (if bootstrap?
                               %bootstrap-guile
                               (canonical-package guile-2.2)))))
            (run-with-store store
              ;; Containers need a Bourne shell at /bin/sh.
              (mlet* %store-monad ((bash       (environment-bash container?
                                                                 bootstrap?
                                                                 system))
                                   (prof-drv   (manifest->derivation
                                                manifest system bootstrap?))
                                   (profile -> (derivation->output-path prof-drv))
                                   (gc-root -> (assoc-ref opts 'gc-root)))

                ;; First build the inputs.  This is necessary even for
                ;; --search-paths.  Additionally, we might need to build bash for
                ;; a container.
                (mbegin %store-monad
                  (build-environment (if (derivation? bash)
                                         (list prof-drv bash)
                                         (list prof-drv))
                                     opts)
                  (mwhen gc-root
                    (register-gc-root profile gc-root))

                  (cond
                   ((assoc-ref opts 'dry-run?)
                    (return #t))
                   ((assoc-ref opts 'search-paths)
                    (show-search-paths profile manifest #:pure? pure?)
                    (return #t))
                   (container?
                    (let ((bash-binary
                           (if bootstrap?
                               bash
                               (string-append (derivation->output-path bash)
                                              "/bin/sh"))))
                      (launch-environment/container #:command command
                                                    #:bash bash-binary
                                                    #:user user
                                                    #:user-mappings mappings
                                                    #:profile profile
                                                    #:manifest manifest
                                                    #:link-profile? link-prof?
                                                    #:network? network?)))
                   (else
                    (return
                     (exit/status
                      (launch-environment/fork command profile manifest
                                               #:white-list white-list
                                               #:pure? pure?))))))))))))))
