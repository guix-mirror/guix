;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix utils)
  #:use-module (guix config)
  #:use-module ((guix build utils) #:select (directory-exists? mkdir-p))
  #:use-module ((guix ftp-client) #:select (ftp-open))
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:use-module (gnu packages)
  #:use-module ((gnu packages base) #:select (guile-final))
  #:use-module ((gnu packages bootstrap) #:select (%bootstrap-guile))
  #:use-module (guix gnu-maintenance)
  #:export (guix-package))

(define %store
  (make-parameter #f))


;;;
;;; User environment.
;;;

(define %user-environment-directory
  (and=> (getenv "HOME")
         (cut string-append <> "/.guix-profile")))

(define %profile-directory
  (string-append (or (getenv "NIX_STATE_DIR") %state-directory) "/profiles/"
                 (or (and=> (getenv "USER")
                            (cut string-append "per-user/" <>))
                     "default")))

(define %current-profile
  ;; Call it `guix-profile', not `profile', to allow Guix profiles to
  ;; coexist with Nix profiles.
  (string-append %profile-directory "/guix-profile"))

(define (profile-manifest profile)
  "Return the PROFILE's manifest."
  (let ((manifest (string-append profile "/manifest")))
    (if (file-exists? manifest)
        (call-with-input-file manifest read)
        '(manifest (version 1) (packages ())))))

(define (manifest-packages manifest)
  "Return the packages listed in MANIFEST."
  (match manifest
    (('manifest ('version 0)
                ('packages ((name version output path) ...)))
     (zip name version output path
          (make-list (length name) '())))

    ;; Version 1 adds a list of propagated inputs to the
    ;; name/version/output/path tuples.
    (('manifest ('version 1)
                ('packages (packages ...)))
     packages)

    (_
     (error "unsupported manifest format" manifest))))

(define (profile-regexp profile)
  "Return a regular expression that matches PROFILE's name and number."
  (make-regexp (string-append "^" (regexp-quote (basename profile))
                              "-([0-9]+)")))

(define (generation-numbers profile)
  "Return the sorted list of generation numbers of PROFILE, or '(0) if no
former profiles were found."
  (define* (scandir name #:optional (select? (const #t))
                    (entry<? (@ (ice-9 i18n) string-locale<?)))
    ;; XXX: Bug-fix version introduced in Guile v2.0.6-62-g139ce19.
    (define (enter? dir stat result)
      (and stat (string=? dir name)))

    (define (visit basename result)
      (if (select? basename)
          (cons basename result)
          result))

    (define (leaf name stat result)
      (and result
           (visit (basename name) result)))

    (define (down name stat result)
      (visit "." '()))

    (define (up name stat result)
      (visit ".." result))

    (define (skip name stat result)
      ;; All the sub-directories are skipped.
      (visit (basename name) result))

    (define (error name* stat errno result)
      (if (string=? name name*)             ; top-level NAME is unreadable
          result
          (visit (basename name*) result)))

    (and=> (file-system-fold enter? leaf down up skip error #f name lstat)
           (lambda (files)
             (sort files entry<?))))

  (match (scandir (dirname profile)
                  (cute regexp-exec (profile-regexp profile) <>))
    (#f                                         ; no profile directory
     '(0))
    (()                                         ; no profiles
     '(0))
    ((profiles ...)                             ; former profiles around
     (sort (map (compose string->number
                         (cut match:substring <> 1)
                         (cute regexp-exec (profile-regexp profile) <>))
                profiles)
           <))))

(define (previous-generation-number profile number)
  "Return the number of the generation before generation NUMBER of
PROFILE, or 0 if none exists.  It could be NUMBER - 1, but it's not the
case when generations have been deleted (there are \"holes\")."
  (fold (lambda (candidate highest)
          (if (and (< candidate number) (> candidate highest))
              candidate
              highest))
        0
        (generation-numbers profile)))

(define (profile-derivation store packages)
  "Return a derivation that builds a profile (a user environment) with
all of PACKAGES, a list of name/version/output/path/deps tuples."
  (define packages*
    ;; Turn any package object in PACKAGES into its output path.
    (map (match-lambda
          ((name version output path (deps ...))
           `(,name ,version ,output ,path
                   ,(map input->name+path deps))))
         packages))

  (define builder
    `(begin
       (use-modules (ice-9 pretty-print)
                    (guix build union))

       (setvbuf (current-output-port) _IOLBF)
       (setvbuf (current-error-port) _IOLBF)

       (let ((output (assoc-ref %outputs "out"))
             (inputs (map cdr %build-inputs)))
         (format #t "building user environment `~a' with ~a packages...~%"
                 output (length inputs))
         (union-build output inputs)
         (call-with-output-file (string-append output "/manifest")
           (lambda (p)
             (pretty-print '(manifest (version 1)
                                      (packages ,packages*))
                           p))))))

  (define ensure-valid-input
    ;; If a package object appears in the given input, turn it into a
    ;; derivation path.
    (match-lambda
     ((name (? package? p) sub-drv ...)
      `(,name ,(package-derivation (%store) p) ,@sub-drv))
     (input
      input)))

  (build-expression->derivation store "user-environment"
                                (%current-system)
                                builder
                                (append-map (match-lambda
                                             ((name version output path deps)
                                              `((,name ,path)
                                                ,@(map ensure-valid-input
                                                       deps))))
                                            packages)
                                #:modules '((guix build union))))

(define (generation-number profile)
  "Return PROFILE's number or 0.  An absolute file name must be used."
  (or (and=> (false-if-exception (regexp-exec (profile-regexp profile)
                                              (basename (readlink profile))))
             (compose string->number (cut match:substring <> 1)))
      0))

(define (roll-back profile)
  "Roll back to the previous generation of PROFILE."
  (let* ((number              (generation-number profile))
         (previous-number     (previous-generation-number profile number))
         (previous-generation (format #f "~a-~a-link"
                                      profile previous-number))
         (manifest            (string-append previous-generation "/manifest")))

    (define (switch-link)
      ;; Atomically switch PROFILE to the previous generation.
      (format #t (_ "switching from generation ~a to ~a~%")
              number previous-number)
      (switch-symlinks profile previous-generation))

    (cond ((not (file-exists? profile))           ; invalid profile
           (leave (_ "profile `~a' does not exist~%")
                  profile))
          ((zero? number)                         ; empty profile
           (format (current-error-port)
                   (_ "nothing to do: already at the empty profile~%")))
          ((or (zero? previous-number)            ; going to emptiness
               (not (file-exists? previous-generation)))
           (let* ((drv  (profile-derivation (%store) '()))
                  (prof (derivation->output-path drv "out")))
             (when (not (build-derivations (%store) (list drv)))
               (leave (_ "failed to build the empty profile~%")))

             (switch-symlinks previous-generation prof)
             (switch-link)))
          (else (switch-link)))))                 ; anything else

(define (generation-time profile number)
  "Return the creation time of a generation in the UTC format."
  (make-time time-utc 0
             (stat:ctime (stat (format #f "~a-~a-link" profile number)))))

(define* (matching-generations str #:optional (profile %current-profile))
  "Return the list of available generations matching a pattern in STR.  See
'string->generations' and 'string->duration' for the list of valid patterns."
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
                           (and (<= s (cdr x))
                                (first x)))
                         generation-ctime-alist))))))

  (cond ((string->generations str)
         =>
         filter-generations)
        ((string->duration str)
         =>
         filter-by-duration)
        (else #f)))

(define (find-packages-by-description rx)
  "Search in SYNOPSIS and DESCRIPTION using RX.  Return a list of
matching packages."
  (define (same-location? p1 p2)
    ;; Compare locations of two packages.
    (equal? (package-location p1) (package-location p2)))

  (delete-duplicates
   (sort
    (fold-packages (lambda (package result)
                     (define matches?
                       (cut regexp-exec rx <>))

                     (if (or (and=> (package-synopsis package)
                                    (compose matches? gettext))
                             (and=> (package-description package)
                                    (compose matches? gettext)))
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

(define* (search-path-environment-variables packages profile
                                            #:optional (getenv getenv))
  "Return environment variable definitions that may be needed for the use of
PACKAGES in PROFILE.  Use GETENV to determine the current settings and report
only settings not already effective."

  ;; Prefer ~/.guix-profile to the real profile directory name.
  (let ((profile (if (and %user-environment-directory
                          (false-if-exception
                           (string=? (readlink %user-environment-directory)
                                     profile)))
                     %user-environment-directory
                     profile)))

    ;; The search path info is not stored in the manifest.  Thus, we infer the
    ;; search paths from same-named packages found in the distro.

    (define package-in-manifest->package
      (match-lambda
       ((name version _ ...)
        (match (append (find-packages-by-name name version)
                       (find-packages-by-name name))
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

    (let* ((packages     (filter-map package-in-manifest->package packages))
           (search-paths (delete-duplicates
                          (append-map package-native-search-paths
                                      packages))))
      (filter-map search-path-definition search-paths))))

(define (display-search-paths packages profile)
  "Display the search path environment variables that may need to be set for
PACKAGES, in the context of PROFILE."
  (let ((settings (search-path-environment-variables packages profile)))
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
  (newline)
  (display (_ "
  -p, --profile=PROFILE  use PROFILE instead of the user's default profile"))
  (display (_ "
  -n, --dry-run          show what would be done without actually doing it"))
  (display (_ "
      --fallback         fall back to building when the substituter fails"))
  (display (_ "
      --no-substitutes   build instead of resorting to pre-built substitutes"))
  (display (_ "
      --max-silent-time=SECONDS
                         mark the build as failed after SECONDS of silence"))
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
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix package")))

        (option '(#\i "install") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'install arg result)))
        (option '(#\e "install-from-expression") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'install (read/eval-package-expression arg)
                              result)))
        (option '(#\r "remove") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'remove arg result)))
        (option '(#\u "upgrade") #f #t
                (lambda (opt name arg result)
                  (alist-cons 'upgrade arg result)))
        (option '("roll-back") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'roll-back? #t result)))
        (option '(#\l "list-generations") #f #t
                (lambda (opt name arg result)
                  (cons `(query list-generations ,(or arg ""))
                        result)))
        (option '("search-paths") #f #f
                (lambda (opt name arg result)
                  (cons `(query search-paths) result)))
        (option '(#\p "profile") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'profile arg
                              (alist-delete 'profile result))))
        (option '(#\n "dry-run") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'dry-run? #t result)))
        (option '("fallback") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'fallback? #t
                              (alist-delete 'fallback? result))))
        (option '("no-substitutes") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'substitutes? #f
                              (alist-delete 'substitutes? result))))
        (option '("max-silent-time") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'max-silent-time (string->number* arg)
                              result)))
        (option '("bootstrap") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'bootstrap? #t result)))
        (option '("verbose") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'verbose? #t result)))
        (option '(#\s "search") #t #f
                (lambda (opt name arg result)
                  (cons `(query search ,(or arg ""))
                        result)))
        (option '(#\I "list-installed") #f #t
                (lambda (opt name arg result)
                  (cons `(query list-installed ,(or arg ""))
                        result)))
        (option '(#\A "list-available") #f #t
                (lambda (opt name arg result)
                  (cons `(query list-available ,(or arg ""))
                        result)))))


;;;
;;; Entry point.
;;;

(define (guix-package . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (leave (_ "~A: extraneous argument~%") arg))
                %default-options))

  (define (guile-missing?)
    ;; Return #t if %GUILE-FOR-BUILD is not available yet.
    (let ((out (derivation->output-path (%guile-for-build))))
      (not (valid-path? (%store) out))))

  (define newest-available-packages
    (memoize find-newest-available-packages))

  (define (find-best-packages-by-name name version)
    (if version
        (find-packages-by-name name version)
        (match (vhash-assoc name (newest-available-packages))
          ((_ version pkgs ...) pkgs)
          (#f '()))))

  (define* (find-package name #:optional (output "out"))
    ;; Find the package NAME; NAME may contain a version number and a
    ;; sub-derivation name.  If the version number is not present,
    ;; return the preferred newest version.  If the sub-derivation name is not
    ;; present, use OUTPUT.
    (define request name)

    (define (ensure-output p sub-drv)
      (if (member sub-drv (package-outputs p))
          p
          (leave (_ "package `~a' lacks output `~a'~%")
                 (package-full-name p)
                 sub-drv)))

    (let*-values (((name sub-drv)
                   (match (string-rindex name #\:)
                     (#f    (values name output))
                     (colon (values (substring name 0 colon)
                                    (substring name (+ 1 colon))))))
                  ((name version)
                   (package-name->name+version name)))
      (match (find-best-packages-by-name name version)
        ((p)
         (list name (package-version p) sub-drv (ensure-output p sub-drv)
               (package-transitive-propagated-inputs p)))
        ((p p* ...)
         (warning (_ "ambiguous package specification `~a'~%")
                  request)
         (warning (_ "choosing ~a from ~a~%")
                  (package-full-name p)
                  (location->string (package-location p)))
         (list name (package-version p) sub-drv (ensure-output p sub-drv)
               (package-transitive-propagated-inputs p)))
        (()
         (leave (_ "~a: package not found~%") request)))))

  (define (upgradeable? name current-version current-path)
    ;; Return #t if there's a version of package NAME newer than
    ;; CURRENT-VERSION, or if the newest available version is equal to
    ;; CURRENT-VERSION but would have an output path different than
    ;; CURRENT-PATH.
    (match (vhash-assoc name (newest-available-packages))
      ((_ candidate-version pkg . rest)
       (case (version-compare candidate-version current-version)
         ((>) #t)
         ((<) #f)
         ((=) (let ((candidate-path (derivation->output-path
                                     (package-derivation (%store) pkg))))
                (not (string=? current-path candidate-path))))))
      (#f #f)))

  (define (ensure-default-profile)
    ;; Ensure the default profile symlink and directory exist and are
    ;; writable.

    (define (rtfm)
      (format (current-error-port)
              (_ "Try \"info '(guix) Invoking guix package'\" for \
more information.~%"))
      (exit 1))

    ;; Create ~/.guix-profile if it doesn't exist yet.
    (when (and %user-environment-directory
               %current-profile
               (not (false-if-exception
                     (lstat %user-environment-directory))))
      (symlink %current-profile %user-environment-directory))

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

    (define (canonicalize-deps deps)
      ;; Remove duplicate entries from DEPS, a list of propagated inputs,
      ;; where each input is a name/path tuple.
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

    (define (same-package? tuple name out)
      (match tuple
        ((tuple-name _ tuple-output _ ...)
         (and (equal? name tuple-name)
              (equal? out tuple-output)))))

    (define (package->tuple p)
      ;; Convert package P to a tuple.
      ;; When given a package via `-e', install the first of its
      ;; outputs (XXX).
      (let* ((out  (car (package-outputs p)))
             (path (package-output (%store) p out))
             (deps (package-transitive-propagated-inputs p)))
        `(,(package-name p)
          ,(package-version p)
          ,out
          ,p
          ,(canonicalize-deps deps))))

    (define (show-what-to-remove/install remove install dry-run?)
      ;; Tell the user what's going to happen in high-level terms.
      ;; TODO: Report upgrades more clearly.
      (match remove
        (((name version _ path _) ..1)
         (let ((len    (length name))
               (remove (map (cut format #f "  ~a-~a\t~a" <> <> <>)
                            name version path)))
           (if dry-run?
               (format (current-error-port)
                       (N_ "The following package would be removed:~% ~{~a~%~}~%"
                           "The following packages would be removed:~% ~{~a~%~}~%"
                           len)
                       remove)
               (format (current-error-port)
                       (N_ "The following package will be removed:~% ~{~a~%~}~%"
                           "The following packages will be removed:~% ~{~a~%~}~%"
                           len)
                       remove))))
        (_ #f))
      (match install
        (((name version output path _) ..1)
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

    ;; First roll back if asked to.
    (if (and (assoc-ref opts 'roll-back?) (not dry-run?))
        (begin
          (roll-back profile)
          (process-actions (alist-delete 'roll-back? opts)))
        (let* ((installed (manifest-packages (profile-manifest profile)))
               (upgrade-regexps (filter-map (match-lambda
                                             (('upgrade . regexp)
                                              (make-regexp (or regexp "")))
                                             (_ #f))
                                            opts))
               (upgrade  (if (null? upgrade-regexps)
                             '()
                             (let ((newest (find-newest-available-packages)))
                               (filter-map (match-lambda
                                            ((name version output path _)
                                             (and (any (cut regexp-exec <> name)
                                                       upgrade-regexps)
                                                  (upgradeable? name version path)
                                                  (find-package name
                                                                (or output "out"))))
                                            (_ #f))
                                           installed))))
               (install  (append
                          upgrade
                          (filter-map (match-lambda
                                       (('install . (? package? p))
                                        (package->tuple p))
                                       (('install . (? store-path?))
                                        #f)
                                       (('install . package)
                                        (find-package package))
                                       (_ #f))
                                      opts)))
               (drv      (filter-map (match-lambda
                                      ((name version sub-drv
                                             (? package? package)
                                             (deps ...))
                                       (check-package-freshness package)
                                       (package-derivation (%store) package))
                                      (_ #f))
                                     install))
               (install* (append
                          (filter-map (match-lambda
                                       (('install . (? package? p))
                                        #f)
                                       (('install . (? store-path? path))
                                        (let-values (((name version)
                                                      (package-name->name+version
                                                       (store-path-package-name
                                                        path))))
                                          `(,name ,version #f ,path ())))
                                       (_ #f))
                                      opts)
                          (map (lambda (tuple drv)
                                 (match tuple
                                   ((name version sub-drv _ (deps ...))
                                    (let ((output-path
                                           (derivation->output-path
                                            drv sub-drv)))
                                      `(,name ,version ,sub-drv ,output-path
                                              ,(canonicalize-deps deps))))))
                               install drv)))
               (remove   (filter-map (match-lambda
                                      (('remove . package)
                                       package)
                                      (_ #f))
                                     opts))
               (remove*  (filter-map (cut assoc <> installed) remove))
               (packages (append install*
                                 (fold (lambda (package result)
                                         (match package
                                           ((name _ out _ ...)
                                            (filter (negate
                                                     (cut same-package? <>
                                                          name out))
                                                    result))))
                                       (fold alist-delete installed remove)
                                       install*))))

          (when (equal? profile %current-profile)
            (ensure-default-profile))

          (show-what-to-remove/install remove* install* dry-run?)
          (show-what-to-build (%store) drv
                              #:use-substitutes? (assoc-ref opts 'substitutes?)
                              #:dry-run? dry-run?)

          (or dry-run?
              (and (build-derivations (%store) drv)
                   (let* ((prof-drv (profile-derivation (%store) packages))
                          (prof     (derivation->output-path prof-drv))
                          (old-drv  (profile-derivation
                                     (%store) (manifest-packages
                                               (profile-manifest profile))))
                          (old-prof (derivation->output-path old-drv))
                          (number   (generation-number profile))

                          ;; Always use NUMBER + 1 for the new profile,
                          ;; possibly overwriting a "previous future
                          ;; generation".
                          (name     (format #f "~a-~a-link"
                                            profile (+ 1 number))))
                     (if (string=? old-prof prof)
                         (when (or (pair? install) (pair? remove))
                           (format (current-error-port)
                                   (_ "nothing to be done~%")))
                         (and (parameterize ((current-build-output-port
                                              ;; Output something when Guile
                                              ;; needs to be built.
                                              (if (or verbose? (guile-missing?))
                                                  (current-error-port)
                                                  (%make-void-port "w"))))
                                (build-derivations (%store) (list prof-drv)))
                              (let ((count (length packages)))
                                (switch-symlinks name prof)
                                (switch-symlinks profile name)
                                (format #t (N_ "~a package in profile~%"
                                               "~a packages in profile~%"
                                               count)
                                        count)
                                (display-search-paths packages
                                                      profile))))))))))

  (define (process-query opts)
    ;; Process any query specified by OPTS.  Return #t when a query was
    ;; actually processed, #f otherwise.
    (let ((profile  (assoc-ref opts 'profile)))
      (match (assoc-ref opts 'query)
        (('list-generations pattern)
         (define (list-generation number)
           (unless (zero? number)
             (format #t (_ "Generation ~a\t~a~%") number
                     (date->string
                      (time-utc->date
                       (generation-time profile number))
                      "~b ~d ~Y ~T"))
             (for-each (match-lambda
                        ((name version output location _)
                         (format #t "  ~a\t~a\t~a\t~a~%"
                                 name version output location)))

                       ;; Show most recently installed packages last.
                       (reverse
                        (manifest-packages
                         (profile-manifest
                          (format #f "~a-~a-link" profile number)))))
             (newline)))

         (cond ((not (file-exists? profile)) ; XXX: race condition
                (leave (_ "profile '~a' does not exist~%")
                       profile))
               ((string-null? pattern)
                (let ((numbers (generation-numbers profile)))
                  (if (equal? numbers '(0))
                      (exit 1)
                      (for-each list-generation numbers))))
               ((matching-generations pattern profile)
                =>
                (lambda (numbers)
                  (if (null-list? numbers)
                      (exit 1)
                      (for-each list-generation numbers))))
               (else
                (leave (_ "invalid syntax: ~a~%")
                       pattern)))
         #t)

        (('list-installed regexp)
         (let* ((regexp    (and regexp (make-regexp regexp)))
                (manifest  (profile-manifest profile))
                (installed (manifest-packages manifest)))
           (for-each (match-lambda
                      ((name version output path _)
                       (when (or (not regexp)
                                 (regexp-exec regexp name))
                         (format #t "~a\t~a\t~a\t~a~%"
                                 name (or version "?") output path))))

                     ;; Show most recently installed packages last.
                     (reverse installed))
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
           (for-each (lambda (p)
                       (format #t "~a\t~a\t~a\t~a~%"
                               (package-name p)
                               (package-version p)
                               (string-join (package-outputs p) ",")
                               (location->string (package-location p))))
                     (sort available
                           (lambda (p1 p2)
                             (string<? (package-name p1)
                                       (package-name p2)))))
           #t))

        (('search regexp)
         (let ((regexp (make-regexp regexp regexp/icase)))
           (for-each (cute package->recutils <> (current-output-port))
                     (find-packages-by-description regexp))
           #t))

        (('search-paths)
         (let* ((manifest (profile-manifest profile))
                (packages (manifest-packages manifest))
                (settings (search-path-environment-variables packages
                                                             profile
                                                             (const #f))))
           (format #t "~{~a~%~}" settings)
           #t))

        (_ #f))))

  (let ((opts (parse-options)))
    (or (process-query opts)
        (with-error-handling
          (parameterize ((%store (open-connection)))
            (set-build-options (%store)
                               #:fallback? (assoc-ref opts 'fallback?)
                               #:use-substitutes?
                               (assoc-ref opts 'substitutes?)
                               #:max-silent-time
                               (assoc-ref opts 'max-silent-time))

            (parameterize ((%guile-for-build
                            (package-derivation (%store)
                                                (if (assoc-ref opts 'bootstrap?)
                                                    %bootstrap-guile
                                                    guile-final))))
              (process-actions opts)))))))
