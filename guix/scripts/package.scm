;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Mark H Weaver <mhw@netris.org>
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

(define-module (guix scripts package)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix utils)
  #:use-module (guix config)
  #:use-module (guix scripts build)
  #:use-module ((guix build utils) #:select (directory-exists? mkdir-p))
  #:use-module ((guix ftp-client) #:select (ftp-open))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (gnu packages)
  #:use-module ((gnu packages base) #:select (guile-final))
  #:use-module ((gnu packages bootstrap) #:select (%bootstrap-guile))
  #:use-module (guix gnu-maintenance)
  #:export (specification->package+output
            guix-package))

(define %store
  (make-parameter #f))


;;;
;;; Profiles.
;;;

(define %user-profile-directory
  (and=> (getenv "HOME")
         (cut string-append <> "/.guix-profile")))

(define %profile-directory
  (string-append %state-directory "/profiles/"
                 (or (and=> (getenv "USER")
                            (cut string-append "per-user/" <>))
                     "default")))

(define %current-profile
  ;; Call it `guix-profile', not `profile', to allow Guix profiles to
  ;; coexist with Nix profiles.
  (string-append %profile-directory "/guix-profile"))

(define (link-to-empty-profile generation)
  "Link GENERATION, a string, to the empty profile."
  (let* ((drv  (profile-derivation (%store) (manifest '())))
         (prof (derivation->output-path drv "out")))
    (when (not (build-derivations (%store) (list drv)))
          (leave (_ "failed to build the empty profile~%")))

    (switch-symlinks generation prof)))

(define (switch-to-previous-generation profile)
  "Atomically switch PROFILE to the previous generation."
  (let* ((number              (generation-number profile))
         (previous-number     (previous-generation-number profile number))
         (previous-generation (generation-file-name profile previous-number)))
    (format #t (_ "switching from generation ~a to ~a~%")
            number previous-number)
    (switch-symlinks profile previous-generation)))

(define (roll-back profile)
  "Roll back to the previous generation of PROFILE."
  (let* ((number              (generation-number profile))
         (previous-number     (previous-generation-number profile number))
         (previous-generation (generation-file-name profile previous-number))
         (manifest            (string-append previous-generation "/manifest")))
    (cond ((not (file-exists? profile))                 ; invalid profile
           (leave (_ "profile '~a' does not exist~%")
                  profile))
          ((zero? number)                               ; empty profile
           (format (current-error-port)
                   (_ "nothing to do: already at the empty profile~%")))
          ((or (zero? previous-number)                  ; going to emptiness
               (not (file-exists? previous-generation)))
           (link-to-empty-profile previous-generation)
           (switch-to-previous-generation profile))
          (else
           (switch-to-previous-generation profile)))))  ; anything else

(define* (matching-generations str #:optional (profile %current-profile)
                               #:key (duration-relation <=))
  "Return the list of available generations matching a pattern in STR.  See
'string->generations' and 'string->duration' for the list of valid patterns.
When STR is a duration pattern, return all the generations whose ctime has
DURATION-RELATION with the current time."
  (define (valid-generations lst)
    (define (valid-generation? n)
      (any (cut = n <>) (generation-numbers profile)))

    (fold-right (lambda (x acc)
                  (if (valid-generation? x)
                      (cons x acc)
                      acc))
                '()
                lst))

  (define (filter-generations generations)
    (match generations
      (() '())
      (('>= n)
       (drop-while (cut > n <>)
                   (generation-numbers profile)))
      (('<= n)
       (valid-generations (iota n 1)))
      ((lst ..1)
       (valid-generations lst))
      (_ #f)))

  (define (filter-by-duration duration)
    (define (time-at-midnight time)
      ;; Return TIME at midnight by setting nanoseconds, seconds, minutes, and
      ;; hours to zeros.
      (let ((d (time-utc->date time)))
         (date->time-utc
          (make-date 0 0 0 0
                     (date-day d) (date-month d)
                     (date-year d) (date-zone-offset d)))))

    (define generation-ctime-alist
      (map (lambda (number)
             (cons number
                   (time-second
                    (time-at-midnight
                     (generation-time profile number)))))
           (generation-numbers profile)))

    (match duration
      (#f #f)
      (res
       (let ((s (time-second
                 (subtract-duration (time-at-midnight (current-time))
                                    duration))))
         (delete #f (map (lambda (x)
                           (and (duration-relation s (cdr x))
                                (first x)))
                         generation-ctime-alist))))))

  (cond ((string->generations str)
         =>
         filter-generations)
        ((string->duration str)
         =>
         filter-by-duration)
        (else #f)))

(define (show-what-to-remove/install remove install dry-run?)
  "Given the manifest entries listed in REMOVE and INSTALL, display the
packages that will/would be installed and removed."
  ;; TODO: Report upgrades more clearly.
  (match remove
    ((($ <manifest-entry> name version output path _) ..1)
     (let ((len    (length name))
           (remove (map (cut format #f "   ~a-~a\t~a\t~a" <> <> <> <>)
                        name version output path)))
       (if dry-run?
           (format (current-error-port)
                   (N_ "The following package would be removed:~%~{~a~%~}~%"
                       "The following packages would be removed:~%~{~a~%~}~%"
                       len)
                   remove)
           (format (current-error-port)
                   (N_ "The following package will be removed:~%~{~a~%~}~%"
                       "The following packages will be removed:~%~{~a~%~}~%"
                       len)
                   remove))))
    (_ #f))
  (match install
    ((($ <manifest-entry> name version output path _) ..1)
     (let ((len     (length name))
           (install (map (cut format #f "   ~a-~a\t~a\t~a" <> <> <> <>)
                         name version output path)))
       (if dry-run?
           (format (current-error-port)
                   (N_ "The following package would be installed:~%~{~a~%~}~%"
                       "The following packages would be installed:~%~{~a~%~}~%"
                       len)
                   install)
           (format (current-error-port)
                   (N_ "The following package will be installed:~%~{~a~%~}~%"
                       "The following packages will be installed:~%~{~a~%~}~%"
                       len)
                   install))))
    (_ #f)))


;;;
;;; Package specifications.
;;;

(define (find-packages-by-description rx)
  "Return the list of packages whose name, synopsis, or description matches
RX."
  (define (same-location? p1 p2)
    ;; Compare locations of two packages.
    (equal? (package-location p1) (package-location p2)))

  (delete-duplicates
   (sort
    (fold-packages (lambda (package result)
                     (define matches?
                       (cut regexp-exec rx <>))

                     (if (or (matches? (package-name package))
                             (and=> (package-synopsis package)
                                    (compose matches? P_))
                             (and=> (package-description package)
                                    (compose matches? P_)))
                         (cons package result)
                         result))
                   '())
    (lambda (p1 p2)
      (string<? (package-name p1)
                (package-name p2))))
   same-location?))

(define (input->name+path input)
  "Convert the name/package/sub-drv tuple INPUT to a name/store-path tuple."
  (let loop ((input input))
    (match input
      ((name (? package? package))
       (loop `(,name ,package "out")))
      ((name (? package? package) sub-drv)
       `(,name ,(package-output (%store) package sub-drv)))
      (_
       input))))

(define %sigint-prompt
  ;; The prompt to jump to upon SIGINT.
  (make-prompt-tag "interruptible"))

(define (call-with-sigint-handler thunk handler)
  "Call THUNK and return its value.  Upon SIGINT, call HANDLER with the signal
number in the context of the continuation of the call to this function, and
return its return value."
  (call-with-prompt %sigint-prompt
                    (lambda ()
                      (sigaction SIGINT
                        (lambda (signum)
                          (sigaction SIGINT SIG_DFL)
                          (abort-to-prompt %sigint-prompt signum)))
                      (dynamic-wind
                        (const #t)
                        thunk
                        (cut sigaction SIGINT SIG_DFL)))
                    (lambda (k signum)
                      (handler signum))))

(define-syntax-rule (waiting exp fmt rest ...)
  "Display the given message while EXP is being evaluated."
  (let* ((message (format #f fmt rest ...))
         (blank   (make-string (string-length message) #\space)))
    (display message (current-error-port))
    (force-output (current-error-port))
    (call-with-sigint-handler
     (lambda ()
       (dynamic-wind
         (const #f)
         (lambda () exp)
         (lambda ()
           ;; Clear the line.
           (display #\cr (current-error-port))
           (display blank (current-error-port))
           (display #\cr (current-error-port))
           (force-output (current-error-port)))))
     (lambda (signum)
       (format (current-error-port) "  interrupted by signal ~a~%" SIGINT)
       #f))))

(define-syntax-rule (leave-on-EPIPE exp ...)
  "Run EXP... in a context when EPIPE errors are caught and lead to 'exit'
with successful exit code.  This is useful when writing to the standard output
may lead to EPIPE, because the standard output is piped through 'head' or
similar."
  (catch 'system-error
    (lambda ()
      exp ...)
    (lambda args
      ;; We really have to exit this brutally, otherwise Guile eventually
      ;; attempts to flush all the ports, leading to an uncaught EPIPE down
      ;; the path.
      (if (= EPIPE (system-error-errno args))
          (primitive-_exit 0)
          (apply throw args)))))

(define* (specification->package+output spec #:optional (output "out"))
  "Return the package and output specified by SPEC, or #f and #f; SPEC may
optionally contain a version number and an output name, as in these examples:

  guile
  guile-2.0.9
  guile:debug
  guile-2.0.9:debug

If SPEC does not specify a version number, return the preferred newest
version; if SPEC does not specify an output, return OUTPUT."
  (define (ensure-output p sub-drv)
    (if (member sub-drv (package-outputs p))
        sub-drv
        (leave (_ "package `~a' lacks output `~a'~%")
               (package-full-name p)
               sub-drv)))

  (let-values (((name version sub-drv)
                (package-specification->name+version+output spec output)))
    (match (find-best-packages-by-name name version)
      ((p)
       (values p (ensure-output p sub-drv)))
      ((p p* ...)
       (warning (_ "ambiguous package specification `~a'~%")
                spec)
       (warning (_ "choosing ~a from ~a~%")
                (package-full-name p)
                (location->string (package-location p)))
       (values p (ensure-output p sub-drv)))
      (()
       (leave (_ "~a: package not found~%") spec)))))

(define (upgradeable? name current-version current-path)
  "Return #t if there's a version of package NAME newer than CURRENT-VERSION,
or if the newest available version is equal to CURRENT-VERSION but would have
an output path different than CURRENT-PATH."
  (match (vhash-assoc name (find-newest-available-packages))
    ((_ candidate-version pkg . rest)
     (case (version-compare candidate-version current-version)
       ((>) #t)
       ((<) #f)
       ((=) (let ((candidate-path (derivation->output-path
                                   (package-derivation (%store) pkg))))
              (not (string=? current-path candidate-path))))))
    (#f #f)))

(define ftp-open*
  ;; Memoizing version of `ftp-open'.  The goal is to avoid initiating a new
  ;; FTP connection for each package, esp. since most of them are to the same
  ;; server.  This has a noticeable impact when doing "guix upgrade -u".
  (memoize ftp-open))

(define (check-package-freshness package)
  "Check whether PACKAGE has a newer version available upstream, and report
it."
  ;; TODO: Automatically inject the upstream version when desired.

  (catch #t
    (lambda ()
      (when (false-if-exception (gnu-package? package))
        (let ((name      (package-name package))
              (full-name (package-full-name package)))
          (match (waiting (latest-release name
                                          #:ftp-open ftp-open*
                                          #:ftp-close (const #f))
                          (_ "looking for the latest release of GNU ~a...") name)
            ((latest-version . _)
             (when (version>? latest-version full-name)
               (format (current-error-port)
                       (_ "~a: note: using ~a \
but ~a is available upstream~%")
                       (location->string (package-location package))
                       full-name latest-version)))
            (_ #t)))))
    (lambda (key . args)
      ;; Silently ignore networking errors rather than preventing
      ;; installation.
      (case key
        ((getaddrinfo-error ftp-error) #f)
        (else (apply throw key args))))))


;;;
;;; Search paths.
;;;

(define* (search-path-environment-variables entries profile
                                            #:optional (getenv getenv))
  "Return environment variable definitions that may be needed for the use of
ENTRIES, a list of manifest entries, in PROFILE.  Use GETENV to determine the
current settings and report only settings not already effective."

  ;; Prefer ~/.guix-profile to the real profile directory name.
  (let ((profile (if (and %user-profile-directory
                          (false-if-exception
                           (string=? (readlink %user-profile-directory)
                                     profile)))
                     %user-profile-directory
                     profile)))

    ;; The search path info is not stored in the manifest.  Thus, we infer the
    ;; search paths from same-named packages found in the distro.

    (define manifest-entry->package
      (match-lambda
       (($ <manifest-entry> name version)
        ;; Use 'find-best-packages-by-name' and not 'find-packages-by-name';
        ;; the former traverses the module tree only once and then allows for
        ;; efficient access via a vhash.
        (match (or (find-best-packages-by-name name version)
                   (find-best-packages-by-name name #f))
          ((p _ ...) p)
          (_ #f)))))

    (define search-path-definition
      (match-lambda
       (($ <search-path-specification> variable directories separator)
        (let ((values      (or (and=> (getenv variable)
                                      (cut string-tokenize* <> separator))
                               '()))
              (directories (filter file-exists?
                                   (map (cut string-append profile
                                             "/" <>)
                                        directories))))
          (if (every (cut member <> values) directories)
              #f
              (format #f "export ~a=\"~a\""
                      variable
                      (string-join directories separator)))))))

    (let* ((packages     (filter-map manifest-entry->package entries))
           (search-paths (delete-duplicates
                          (append-map package-native-search-paths
                                      packages))))
      (filter-map search-path-definition search-paths))))

(define (display-search-paths entries profile)
  "Display the search path environment variables that may need to be set for
ENTRIES, a list of manifest entries, in the context of PROFILE."
  (let ((settings (search-path-environment-variables entries profile)))
    (unless (null? settings)
      (format #t (_ "The following environment variable definitions may be needed:~%"))
      (format #t "~{   ~a~%~}" settings))))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((profile . ,%current-profile)
    (max-silent-time . 3600)
    (verbosity . 0)
    (substitutes? . #t)))

(define (show-help)
  (display (_ "Usage: guix package [OPTION]... PACKAGES...
Install, remove, or upgrade PACKAGES in a single transaction.\n"))
  (display (_ "
  -i, --install=PACKAGE  install PACKAGE"))
  (display (_ "
  -e, --install-from-expression=EXP
                         install the package EXP evaluates to"))
  (display (_ "
  -r, --remove=PACKAGE   remove PACKAGE"))
  (display (_ "
  -u, --upgrade[=REGEXP] upgrade all the installed packages matching REGEXP"))
  (display (_ "
      --roll-back        roll back to the previous generation"))
  (display (_ "
      --search-paths     display needed environment variable definitions"))
  (display (_ "
  -l, --list-generations[=PATTERN]
                         list generations matching PATTERN"))
  (display (_ "
  -d, --delete-generations[=PATTERN]
                         delete generations matching PATTERN"))
  (display (_ "
  -p, --profile=PROFILE  use PROFILE instead of the user's default profile"))
  (newline)
  (display (_ "
      --bootstrap        use the bootstrap Guile to build the profile"))
  (display (_ "
      --verbose          produce verbose output"))
  (newline)
  (display (_ "
  -s, --search=REGEXP    search in synopsis and description using REGEXP"))
  (display (_ "
  -I, --list-installed[=REGEXP]
                         list installed packages matching REGEXP"))
  (display (_ "
  -A, --list-available[=REGEXP]
                         list available packages matching REGEXP"))
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
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix package")))

         (option '(#\i "install") #f #t
                 (lambda (opt name arg result arg-handler)
                   (let arg-handler ((arg arg) (result result))
                     (values (if arg
                                 (alist-cons 'install arg result)
                                 result)
                             arg-handler))))
         (option '(#\e "install-from-expression") #t #f
                 (lambda (opt name arg result arg-handler)
                   (values (alist-cons 'install (read/eval-package-expression arg)
                                       result)
                           #f)))
         (option '(#\r "remove") #f #t
                 (lambda (opt name arg result arg-handler)
                   (let arg-handler ((arg arg) (result result))
                     (values (if arg
                                 (alist-cons 'remove arg result)
                                 result)
                             arg-handler))))
         (option '(#\u "upgrade") #f #t
                 (lambda (opt name arg result arg-handler)
                   (let arg-handler ((arg arg) (result result))
                     (values (alist-cons 'upgrade arg
                                         ;; Delete any prior "upgrade all"
                                         ;; command, or else "--upgrade gcc"
                                         ;; would upgrade everything.
                                         (delete '(upgrade . #f) result))
                             arg-handler))))
         (option '("roll-back") #f #f
                 (lambda (opt name arg result arg-handler)
                   (values (alist-cons 'roll-back? #t result)
                           #f)))
         (option '(#\l "list-generations") #f #t
                 (lambda (opt name arg result arg-handler)
                   (values (cons `(query list-generations ,(or arg ""))
                                 result)
                           #f)))
         (option '(#\d "delete-generations") #f #t
                 (lambda (opt name arg result arg-handler)
                   (values (alist-cons 'delete-generations (or arg "")
                                       result)
                           #f)))
         (option '("search-paths") #f #f
                 (lambda (opt name arg result arg-handler)
                   (values (cons `(query search-paths) result)
                           #f)))
         (option '(#\p "profile") #t #f
                 (lambda (opt name arg result arg-handler)
                   (values (alist-cons 'profile arg
                                       (alist-delete 'profile result))
                           #f)))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result arg-handler)
                   (values (alist-cons 'dry-run? #t result)
                           #f)))
         (option '("bootstrap") #f #f
                 (lambda (opt name arg result arg-handler)
                   (values (alist-cons 'bootstrap? #t result)
                           #f)))
         (option '("verbose") #f #f
                 (lambda (opt name arg result arg-handler)
                   (values (alist-cons 'verbose? #t result)
                           #f)))
         (option '(#\s "search") #t #f
                 (lambda (opt name arg result arg-handler)
                   (values (cons `(query search ,(or arg ""))
                                 result)
                           #f)))
         (option '(#\I "list-installed") #f #t
                 (lambda (opt name arg result arg-handler)
                   (values (cons `(query list-installed ,(or arg ""))
                                 result)
                           #f)))
         (option '(#\A "list-available") #f #t
                 (lambda (opt name arg result arg-handler)
                   (values (cons `(query list-available ,(or arg ""))
                                 result)
                           #f)))

         %standard-build-options))

(define (options->installable opts manifest)
  "Given MANIFEST, the current manifest, and OPTS, the result of 'args-fold',
return the new list of manifest entries."
  (define (deduplicate deps)
    ;; Remove duplicate entries from DEPS, a list of propagated inputs, where
    ;; each input is a name/path tuple.
    (define (same? d1 d2)
      (match d1
        ((_ p1)
         (match d2
           ((_ p2) (eq? p1 p2))
           (_      #f)))
        ((_ p1 out1)
         (match d2
           ((_ p2 out2)
            (and (string=? out1 out2)
                 (eq? p1 p2)))
           (_ #f)))))

    (delete-duplicates deps same?))

  (define (package->manifest-entry p output)
    ;; Return a manifest entry for the OUTPUT of package P.
    (check-package-freshness p)
    ;; When given a package via `-e', install the first of its
    ;; outputs (XXX).
    (let* ((output (or output (car (package-outputs p))))
           (path   (package-output (%store) p output))
           (deps   (deduplicate (package-transitive-propagated-inputs p))))
      (manifest-entry
       (name (package-name p))
       (version (package-version p))
       (output output)
       (path path)
       (dependencies (map input->name+path deps))
       (inputs (cons (list (package-name p) p output)
                     deps)))))

  (define upgrade-regexps
    (filter-map (match-lambda
                 (('upgrade . regexp)
                  (make-regexp (or regexp "")))
                 (_ #f))
                opts))

  (define packages-to-upgrade
    (match upgrade-regexps
      (()
       '())
      ((_ ...)
       (let ((newest (find-newest-available-packages)))
         (filter-map (match-lambda
                      (($ <manifest-entry> name version output path _)
                       (and (any (cut regexp-exec <> name)
                                 upgrade-regexps)
                            (upgradeable? name version path)
                            (let ((output (or output "out")))
                              (call-with-values
                                  (lambda ()
                                    (specification->package+output name output))
                                list))))
                      (_ #f))
                     (manifest-entries manifest))))))

  (define to-upgrade
    (map (match-lambda
          ((package output)
           (package->manifest-entry package output)))
         packages-to-upgrade))

  (define packages-to-install
    (filter-map (match-lambda
                 (('install . (? package? p))
                  (list p "out"))
                 (('install . (? string? spec))
                  (and (not (store-path? spec))
                       (let-values (((package output)
                                     (specification->package+output spec)))
                         (and package (list package output)))))
                 (_ #f))
                opts))

  (define to-install
    (append (map (match-lambda
                  ((package output)
                   (package->manifest-entry package output)))
                 packages-to-install)
            (filter-map (match-lambda
                         (('install . (? package?))
                          #f)
                         (('install . (? store-path? path))
                          (let-values (((name version)
                                        (package-name->name+version
                                         (store-path-package-name path))))
                            (manifest-entry
                             (name name)
                             (version version)
                             (output #f)
                             (path path))))
                         (_ #f))
                        opts)))

  (append to-upgrade to-install))

(define (options->removable options manifest)
  "Given options, return the list of manifest patterns of packages to be
removed from MANIFEST."
  (filter-map (match-lambda
               (('remove . spec)
                (call-with-values
                    (lambda ()
                      (package-specification->name+version+output spec))
                  (lambda (name version output)
                    (manifest-pattern
                      (name name)
                      (version version)
                      (output output)))))
               (_ #f))
              options))

(define (maybe-register-gc-root store profile)
  "Register PROFILE as a GC root, unless it doesn't need it."
  (unless (string=? profile %current-profile)
    (add-indirect-root store (canonicalize-path profile))))


;;;
;;; Entry point.
;;;

(define (guix-package . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result arg-handler)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result arg-handler)
                  (if arg-handler
                      (arg-handler arg result)
                      (leave (_ "~A: extraneous argument~%") arg)))
                %default-options
                #f))

  (define (guile-missing?)
    ;; Return #t if %GUILE-FOR-BUILD is not available yet.
    (let ((out (derivation->output-path (%guile-for-build))))
      (not (valid-path? (%store) out))))

  (define (ensure-default-profile)
    ;; Ensure the default profile symlink and directory exist and are
    ;; writable.

    (define (rtfm)
      (format (current-error-port)
              (_ "Try \"info '(guix) Invoking guix package'\" for \
more information.~%"))
      (exit 1))

    ;; Create ~/.guix-profile if it doesn't exist yet.
    (when (and %user-profile-directory
               %current-profile
               (not (false-if-exception
                     (lstat %user-profile-directory))))
      (symlink %current-profile %user-profile-directory))

    (let ((s (stat %profile-directory #f)))
      ;; Attempt to create /…/profiles/per-user/$USER if needed.
      (unless (and s (eq? 'directory (stat:type s)))
        (catch 'system-error
          (lambda ()
            (mkdir-p %profile-directory))
          (lambda args
            ;; Often, we cannot create %PROFILE-DIRECTORY because its
            ;; parent directory is root-owned and we're running
            ;; unprivileged.
            (format (current-error-port)
                    (_ "error: while creating directory `~a': ~a~%")
                    %profile-directory
                    (strerror (system-error-errno args)))
            (format (current-error-port)
                    (_ "Please create the `~a' directory, with you as the owner.~%")
                    %profile-directory)
            (rtfm))))

      ;; Bail out if it's not owned by the user.
      (unless (or (not s) (= (stat:uid s) (getuid)))
        (format (current-error-port)
                (_ "error: directory `~a' is not owned by you~%")
                %profile-directory)
        (format (current-error-port)
                (_ "Please change the owner of `~a' to user ~s.~%")
                %profile-directory (or (getenv "USER") (getuid)))
        (rtfm))))

  (define (process-actions opts)
    ;; Process any install/remove/upgrade action from OPTS.

    (define dry-run? (assoc-ref opts 'dry-run?))
    (define verbose? (assoc-ref opts 'verbose?))
    (define profile  (assoc-ref opts 'profile))

    (define (same-package? entry name output)
      (match entry
        (($ <manifest-entry> entry-name _ entry-output _ ...)
         (and (equal? name entry-name)
              (equal? output entry-output)))))

    (define current-generation-number
      (generation-number profile))

    (define (display-and-delete number)
      (let ((generation (generation-file-name profile number)))
        (unless (zero? number)
          (format #t (_ "deleting ~a~%") generation)
          (delete-file generation))))

    (define (delete-generation number)
      (let* ((previous-number (previous-generation-number profile number))
             (previous-generation
              (generation-file-name profile previous-number)))
        (cond ((zero? number))  ; do not delete generation 0
              ((and (= number current-generation-number)
                    (not (file-exists? previous-generation)))
               (link-to-empty-profile previous-generation)
               (switch-to-previous-generation profile)
               (display-and-delete number))
              ((= number current-generation-number)
               (roll-back profile)
               (display-and-delete number))
              (else
               (display-and-delete number)))))

    ;; First roll back if asked to.
    (cond ((and (assoc-ref opts 'roll-back?) (not dry-run?))
           (begin
             (roll-back profile)
             (process-actions (alist-delete 'roll-back? opts))))
          ((and (assoc-ref opts 'delete-generations)
                (not dry-run?))
           (filter-map
            (match-lambda
             (('delete-generations . pattern)
              (cond ((not (file-exists? profile)) ; XXX: race condition
                     (leave (_ "profile '~a' does not exist~%")
                            profile))
                    ((string-null? pattern)
                     (let ((numbers (generation-numbers profile)))
                       (if (equal? numbers '(0))
                           (exit 0)
                           (for-each display-and-delete
                                     (delete current-generation-number
                                             numbers)))))
                    ;; Do not delete the zeroth generation.
                    ((equal? 0 (string->number pattern))
                     (exit 0))

                    ;; If PATTERN is a duration, match generations that are
                    ;; older than the specified duration.
                    ((matching-generations pattern profile
                                           #:duration-relation >)
                     =>
                     (lambda (numbers)
                       (if (null-list? numbers)
                           (exit 1)
                           (for-each delete-generation numbers))))
                    (else
                     (leave (_ "invalid syntax: ~a~%")
                            pattern)))

              (process-actions
               (alist-delete 'delete-generations opts)))
             (_ #f))
            opts))
          (else
           (let* ((manifest (profile-manifest profile))
                  (install  (options->installable opts manifest))
                  (remove   (options->removable opts manifest))
                  (entries
                   (append install
                           (fold (lambda (package result)
                                   (match package
                                     (($ <manifest-entry> name _ out _ ...)
                                      (filter (negate
                                               (cut same-package? <>
                                                    name out))
                                              result))))
                                 (manifest-entries
                                  (manifest-remove manifest remove))
                                 install)))
                  (new      (make-manifest entries)))

             (when (equal? profile %current-profile)
               (ensure-default-profile))

             (if (manifest=? new manifest)
                 (format (current-error-port) (_ "nothing to be done~%"))
                 (let ((prof-drv (profile-derivation (%store) new))
                       (remove   (manifest-matching-entries manifest remove)))
                   (show-what-to-remove/install remove install dry-run?)
                   (show-what-to-build (%store) (list prof-drv)
                                       #:use-substitutes?
                                       (assoc-ref opts 'substitutes?)
                                       #:dry-run? dry-run?)

                   (or dry-run?
                       (let* ((prof   (derivation->output-path prof-drv))
                              (number (generation-number profile))

                              ;; Always use NUMBER + 1 for the new profile,
                              ;; possibly overwriting a "previous future
                              ;; generation".
                              (name   (generation-file-name profile
                                                            (+ 1 number))))
                         (and (build-derivations (%store) (list prof-drv))
                              (let ((count (length entries)))
                                (switch-symlinks name prof)
                                (switch-symlinks profile name)
                                (maybe-register-gc-root (%store) profile)
                                (format #t (N_ "~a package in profile~%"
                                               "~a packages in profile~%"
                                               count)
                                        count)
                                (display-search-paths entries
                                                      profile)))))))))))

  (define (process-query opts)
    ;; Process any query specified by OPTS.  Return #t when a query was
    ;; actually processed, #f otherwise.
    (let ((profile  (assoc-ref opts 'profile)))
      (match (assoc-ref opts 'query)
        (('list-generations pattern)
         (define (list-generation number)
           (unless (zero? number)
             (let ((header (format #f (_ "Generation ~a\t~a") number
                                   (date->string
                                    (time-utc->date
                                     (generation-time profile number))
                                    "~b ~d ~Y ~T")))
                   (current (generation-number profile)))
               (if (= number current)
                   (format #t (_ "~a\t(current)~%") header)
                   (format #t "~a~%" header)))
             (for-each (match-lambda
                        (($ <manifest-entry> name version output location _)
                         (format #t "  ~a\t~a\t~a\t~a~%"
                                 name version output location)))

                       ;; Show most recently installed packages last.
                       (reverse
                        (manifest-entries
                         (profile-manifest
                          (generation-file-name profile number)))))
             (newline)))

         (cond ((not (file-exists? profile)) ; XXX: race condition
                (leave (_ "profile '~a' does not exist~%")
                       profile))
               ((string-null? pattern)
                (let ((numbers (generation-numbers profile)))
                  (leave-on-EPIPE
                   (if (equal? numbers '(0))
                       (exit 0)
                       (for-each list-generation numbers)))))
               ((matching-generations pattern profile)
                =>
                (lambda (numbers)
                  (if (null-list? numbers)
                      (exit 1)
                      (leave-on-EPIPE
                       (for-each list-generation numbers)))))
               (else
                (leave (_ "invalid syntax: ~a~%")
                       pattern)))
         #t)

        (('list-installed regexp)
         (let* ((regexp    (and regexp (make-regexp regexp)))
                (manifest  (profile-manifest profile))
                (installed (manifest-entries manifest)))
           (leave-on-EPIPE
            (for-each (match-lambda
                       (($ <manifest-entry> name version output path _)
                        (when (or (not regexp)
                                  (regexp-exec regexp name))
                          (format #t "~a\t~a\t~a\t~a~%"
                                  name (or version "?") output path))))

                      ;; Show most recently installed packages last.
                      (reverse installed)))
           #t))

        (('list-available regexp)
         (let* ((regexp    (and regexp (make-regexp regexp)))
                (available (fold-packages
                            (lambda (p r)
                              (let ((n (package-name p)))
                                (if regexp
                                    (if (regexp-exec regexp n)
                                        (cons p r)
                                        r)
                                    (cons p r))))
                            '())))
           (leave-on-EPIPE
            (for-each (lambda (p)
                        (format #t "~a\t~a\t~a\t~a~%"
                                (package-name p)
                                (package-version p)
                                (string-join (package-outputs p) ",")
                                (location->string (package-location p))))
                      (sort available
                            (lambda (p1 p2)
                              (string<? (package-name p1)
                                        (package-name p2))))))
           #t))

        (('search regexp)
         (let ((regexp (make-regexp regexp regexp/icase)))
           (leave-on-EPIPE
            (for-each (cute package->recutils <> (current-output-port))
                      (find-packages-by-description regexp)))
           #t))

        (('search-paths)
         (let* ((manifest (profile-manifest profile))
                (entries  (manifest-entries manifest))
                (packages (map manifest-entry-name entries))
                (settings (search-path-environment-variables entries profile
                                                             (const #f))))
           (format #t "~{~a~%~}" settings)
           #t))

        (_ #f))))

  (let ((opts (parse-options)))
    (or (process-query opts)
        (with-error-handling
          (parameterize ((%store (open-connection)))
            (set-build-options-from-command-line (%store) opts)

            (parameterize ((%guile-for-build
                            (package-derivation (%store)
                                                (if (assoc-ref opts 'bootstrap?)
                                                    %bootstrap-guile
                                                    guile-final))))
              (process-actions opts)))))))
