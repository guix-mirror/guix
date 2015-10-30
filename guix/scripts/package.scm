;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
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
  #:use-module (guix search-paths)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix config)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module ((guix build utils)
                #:select (directory-exists? mkdir-p search-path-as-list))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages bootstrap) #:select (%bootstrap-guile))
  #:export (delete-generations
            display-search-paths
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
                 (or (and=> (or (getenv "USER")
                                (getenv "LOGNAME"))
                            (cut string-append "per-user/" <>))
                     "default")))

(define %current-profile
  ;; Call it `guix-profile', not `profile', to allow Guix profiles to
  ;; coexist with Nix profiles.
  (string-append %profile-directory "/guix-profile"))

(define (canonicalize-profile profile)
  "If PROFILE is %USER-PROFILE-DIRECTORY, return %CURRENT-PROFILE.  Otherwise
return PROFILE unchanged.  The goal is to treat '-p ~/.guix-profile' as if
'-p' was omitted."                           ; see <http://bugs.gnu.org/17939>
  (if (and %user-profile-directory
           (string=? (canonicalize-path (dirname profile))
                     (dirname %user-profile-directory))
           (string=? (basename profile) (basename %user-profile-directory)))
      %current-profile
      profile))

(define (user-friendly-profile profile)
  "Return either ~/.guix-profile if that's what PROFILE refers to, directly or
indirectly, or PROFILE."
  (if (and %user-profile-directory
           (false-if-exception
            (string=? (readlink %user-profile-directory) profile)))
      %user-profile-directory
      profile))

(define (delete-generations store profile generations)
  "Delete GENERATIONS from PROFILE.
GENERATIONS is a list of generation numbers."
  (for-each (cut delete-generation* store profile <>)
            generations))

(define (delete-matching-generations store profile pattern)
  "Delete from PROFILE all the generations matching PATTERN.  PATTERN must be
a string denoting a set of generations: the empty list means \"all generations
but the current one\", a number designates a generation, and other patterns
denote ranges as interpreted by 'matching-derivations'."
  (let ((current (generation-number profile)))
    (cond ((not (file-exists? profile))            ; XXX: race condition
           (raise (condition (&profile-not-found-error
                              (profile profile)))))
          ((string-null? pattern)
           (delete-generations (%store) profile
                               (delv current (profile-generations profile))))
          ;; Do not delete the zeroth generation.
          ((equal? 0 (string->number pattern))
           #t)

          ;; If PATTERN is a duration, match generations that are
          ;; older than the specified duration.
          ((matching-generations pattern profile
                                 #:duration-relation >)
           =>
           (lambda (numbers)
             (when (memv current numbers)
               (warning (_ "not removing generation ~a, which is current~%")
                        current))

             ;; Make sure we don't inadvertently remove the current
             ;; generation.
             (let ((numbers (delv current numbers)))
               (when (null-list? numbers)
                 (leave (_ "no matching generation~%")))
               (delete-generations (%store) profile numbers))))
          (else
           (leave (_ "invalid syntax: ~a~%") pattern)))))


;;;
;;; Package specifications.
;;;

(define (find-packages-by-description rx)
  "Return the list of packages whose name, synopsis, or description matches
RX."
  (define version<? (negate version>=?))

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
     (case (string-compare (package-name p1) (package-name p2)
                           (const '<) (const '=) (const '>))
       ((=)  (version<? (package-version p1) (package-version p2)))
       ((<)  #t)
       (else #f)))))

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


;;;
;;; Search paths.
;;;

(define* (search-path-environment-variables entries profile
                                            #:optional (getenv getenv)
                                            #:key (kind 'exact))
  "Return environment variable definitions that may be needed for the use of
ENTRIES, a list of manifest entries, in PROFILE.  Use GETENV to determine the
current settings and report only settings not already effective.  KIND
must be one of 'exact, 'prefix, or 'suffix, depending on the kind of search
path definition to be returned."
  (let ((search-paths (delete-duplicates
                       (cons $PATH
                             (append-map manifest-entry-search-paths
                                         entries)))))
    (filter-map (match-lambda
                  ((spec . value)
                   (let ((variable (search-path-specification-variable spec))
                         (sep      (search-path-specification-separator spec)))
                     (environment-variable-definition variable value
                                                      #:separator sep
                                                      #:kind kind))))
                (evaluate-search-paths search-paths (list profile)
                                       getenv))))

(define* (display-search-paths entries profile
                               #:key (kind 'exact))
  "Display the search path environment variables that may need to be set for
ENTRIES, a list of manifest entries, in the context of PROFILE."
  (let* ((profile  (user-friendly-profile profile))
         (settings (search-path-environment-variables entries profile
                                                      #:kind kind)))
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
  (display (_ "Usage: guix package [OPTION]...
Install, remove, or upgrade packages in a single transaction.\n"))
  (display (_ "
  -i, --install PACKAGE ...
                         install PACKAGEs"))
  (display (_ "
  -e, --install-from-expression=EXP
                         install the package EXP evaluates to"))
  (display (_ "
  -f, --install-from-file=FILE
                         install the package that the code within FILE
                         evaluates to"))
  (display (_ "
  -r, --remove PACKAGE ...
                         remove PACKAGEs"))
  (display (_ "
  -u, --upgrade[=REGEXP] upgrade all the installed packages matching REGEXP"))
  (display (_ "
  -m, --manifest=FILE    create a new profile generation with the manifest
                         from FILE"))
  (display (_ "
      --do-not-upgrade[=REGEXP] do not upgrade any packages matching REGEXP"))
  (display (_ "
      --roll-back        roll back to the previous generation"))
  (display (_ "
      --search-paths[=KIND]
                         display needed environment variable definitions"))
  (display (_ "
  -l, --list-generations[=PATTERN]
                         list generations matching PATTERN"))
  (display (_ "
  -d, --delete-generations[=PATTERN]
                         delete generations matching PATTERN"))
  (display (_ "
  -S, --switch-generation=PATTERN
                         switch to a generation matching PATTERN"))
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
  (display (_ "
      --show=PACKAGE     show details about PACKAGE"))
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
         (option '(#\f "install-from-file") #t #f
                 (lambda (opt name arg result arg-handler)
                   (values (alist-cons 'install
                                       (load* arg (make-user-module '()))
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
         (option '("do-not-upgrade") #f #t
                 (lambda (opt name arg result arg-handler)
                   (let arg-handler ((arg arg) (result result))
                     (values (if arg
                                 (alist-cons 'do-not-upgrade arg result)
                                 result)
                             arg-handler))))
         (option '("roll-back") #f #f
                 (lambda (opt name arg result arg-handler)
                   (values (alist-cons 'roll-back? #t result)
                           #f)))
         (option '(#\m "manifest") #t #f
                 (lambda (opt name arg result arg-handler)
                   (values (alist-cons 'manifest arg result)
                           arg-handler)))
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
         (option '(#\S "switch-generation") #t #f
                 (lambda (opt name arg result arg-handler)
                   (values (alist-cons 'switch-generation arg result)
                           #f)))
         (option '("search-paths") #f #t
                 (lambda (opt name arg result arg-handler)
                   (let ((kind (match arg
                                 ((or "exact" "prefix" "suffix")
                                  (string->symbol arg))
                                 (#f
                                  'exact)
                                 (x
                                  (leave (_ "~a: unsupported \
kind of search path~%")
                                         x)))))
                     (values (cons `(query search-paths ,kind)
                                   result)
                             #f))))
         (option '(#\p "profile") #t #f
                 (lambda (opt name arg result arg-handler)
                   (values (alist-cons 'profile (canonicalize-profile arg)
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
         (option '("show") #t #t
                 (lambda (opt name arg result arg-handler)
                   (values (cons `(query show ,arg)
                                 result)
                           #f)))

         %standard-build-options))

(define (options->installable opts manifest)
  "Given MANIFEST, the current manifest, and OPTS, the result of 'args-fold',
return the new list of manifest entries."
  (define (package->manifest-entry* package output)
    (check-package-freshness package)
    ;; When given a package via `-e', install the first of its
    ;; outputs (XXX).
    (package->manifest-entry package output))

  (define upgrade-regexps
    (filter-map (match-lambda
                 (('upgrade . regexp)
                  (make-regexp* (or regexp "")))
                 (_ #f))
                opts))

  (define do-not-upgrade-regexps
    (filter-map (match-lambda
                 (('do-not-upgrade . regexp)
                  (make-regexp* regexp))
                 (_ #f))
                opts))

  (define packages-to-upgrade
    (match upgrade-regexps
      (()
       '())
      ((_ ...)
       (filter-map (match-lambda
                    (($ <manifest-entry> name version output path _)
                     (and (any (cut regexp-exec <> name)
                               upgrade-regexps)
                          (not (any (cut regexp-exec <> name)
                                    do-not-upgrade-regexps))
                          (upgradeable? name version path)
                          (let ((output (or output "out")))
                            (call-with-values
                                (lambda ()
                                  (specification->package+output name output))
                              list))))
                    (_ #f))
                   (manifest-entries manifest)))))

  (define to-upgrade
    (map (match-lambda
          ((package output)
           (package->manifest-entry* package output)))
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
                   (package->manifest-entry* package output)))
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
                             (item path))))
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

(define (register-gc-root store profile)
  "Register PROFILE, a profile generation symlink, as a GC root, unless it
doesn't need it."
  (define absolute
    ;; We must pass the daemon an absolute file name for PROFILE.  However, we
    ;; cannot use (canonicalize-path profile) because that would return us the
    ;; target of PROFILE in the store; using a store item as an indirect root
    ;; would mean that said store item will always remain live, which is not
    ;; what we want here.
    (if (string-prefix? "/" profile)
        profile
        (string-append (getcwd) "/" profile)))

  (add-indirect-root store absolute))


;;;
;;; Entry point.
;;;

(define (guix-package . args)
  (define (handle-argument arg result arg-handler)
    ;; Process non-option argument ARG by calling back ARG-HANDLER.
    (if arg-handler
        (arg-handler arg result)
        (leave (_ "~A: extraneous argument~%") arg)))

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
                %profile-directory (or (getenv "USER")
                                       (getenv "LOGNAME")
                                       (getuid)))
        (rtfm))))

  (define (process-actions opts)
    ;; Process any install/remove/upgrade action from OPTS.

    (define dry-run? (assoc-ref opts 'dry-run?))
    (define profile  (assoc-ref opts 'profile))

    (define (build-and-use-profile manifest)
      (let* ((bootstrap?  (assoc-ref opts 'bootstrap?)))

        (when (equal? profile %current-profile)
          (ensure-default-profile))

        (let* ((prof-drv (run-with-store (%store)
                           (profile-derivation
                            manifest
                            #:hooks (if bootstrap?
                                        '()
                                        %default-profile-hooks))))
               (prof     (derivation->output-path prof-drv)))
          (show-what-to-build (%store) (list prof-drv)
                              #:use-substitutes?
                              (assoc-ref opts 'substitutes?)
                              #:dry-run? dry-run?)

          (cond
           (dry-run? #t)
           ((and (file-exists? profile)
                 (and=> (readlink* profile) (cut string=? prof <>)))
            (format (current-error-port) (_ "nothing to be done~%")))
           (else
            (let* ((number (generation-number profile))

                   ;; Always use NUMBER + 1 for the new profile,
                   ;; possibly overwriting a "previous future
                   ;; generation".
                   (name   (generation-file-name profile
                                                 (+ 1 number))))
              (and (build-derivations (%store) (list prof-drv))
                   (let* ((entries (manifest-entries manifest))
                          (count   (length entries)))
                     (switch-symlinks name prof)
                     (switch-symlinks profile name)
                     (unless (string=? profile %current-profile)
                       (register-gc-root (%store) name))
                     (format #t (N_ "~a package in profile~%"
                                    "~a packages in profile~%"
                                    count)
                             count)
                     (display-search-paths entries profile)))))))))

    ;; First roll back if asked to.
    (cond ((and (assoc-ref opts 'roll-back?)
                (not dry-run?))
           (roll-back* (%store) profile)
           (process-actions (alist-delete 'roll-back? opts)))
          ((and (assoc-ref opts 'switch-generation)
                (not dry-run?))
           (for-each
            (match-lambda
              (('switch-generation . pattern)
               (let* ((number (string->number pattern))
                      (number (and number
                                   (case (string-ref pattern 0)
                                     ((#\+ #\-)
                                      (relative-generation profile number))
                                     (else number)))))
                 (if number
                     (switch-to-generation* profile number)
                     (leave (_ "cannot switch to generation '~a'~%")
                            pattern)))
               (process-actions (alist-delete 'switch-generation opts)))
              (_ #f))
            opts))
          ((and (assoc-ref opts 'delete-generations)
                (not dry-run?))
           (for-each
            (match-lambda
             (('delete-generations . pattern)
              (delete-matching-generations (%store) profile pattern)

              (process-actions
               (alist-delete 'delete-generations opts)))
             (_ #f))
            opts))
          ((assoc-ref opts 'manifest)
           (let* ((file-name   (assoc-ref opts 'manifest))
                  (user-module (make-user-module '((guix profiles)
                                                   (gnu))))
                  (manifest    (load* file-name user-module)))
             (if (assoc-ref opts 'dry-run?)
                 (format #t (_ "would install new manifest from '~a' with ~d entries~%")
                         file-name (length (manifest-entries manifest)))
                 (format #t (_ "installing new manifest from '~a' with ~d entries~%")
                         file-name (length (manifest-entries manifest))))
             (build-and-use-profile manifest)))
          (else
           (let* ((manifest    (profile-manifest profile))
                  (install     (options->installable opts manifest))
                  (remove      (options->removable opts manifest))
                  (transaction (manifest-transaction (install install)
                                                     (remove remove)))
                  (new         (manifest-perform-transaction
                                manifest transaction)))

             (unless (and (null? install) (null? remove))
               (show-manifest-transaction (%store) manifest transaction
                                          #:dry-run? dry-run?)
               (build-and-use-profile new))))))

  (define (process-query opts)
    ;; Process any query specified by OPTS.  Return #t when a query was
    ;; actually processed, #f otherwise.
    (let ((profile  (assoc-ref opts 'profile)))
      (match (assoc-ref opts 'query)
        (('list-generations pattern)
         (define (list-generation number)
           (unless (zero? number)
             (display-generation profile number)
             (display-profile-content profile number)
             (newline)))

         (cond ((not (file-exists? profile)) ; XXX: race condition
                (raise (condition (&profile-not-found-error
                                   (profile profile)))))
               ((string-null? pattern)
                (for-each list-generation (profile-generations profile)))
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
         (let* ((regexp    (and regexp (make-regexp* regexp)))
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
         (let* ((regexp    (and regexp (make-regexp* regexp)))
                (available (fold-packages
                            (lambda (p r)
                              (let ((n (package-name p)))
                                (if (supported-package? p)
                                    (if regexp
                                        (if (regexp-exec regexp n)
                                            (cons p r)
                                            r)
                                        (cons p r))
                                    r)))
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
         (let ((regexp (make-regexp* regexp regexp/icase)))
           (leave-on-EPIPE
            (for-each (cute package->recutils <> (current-output-port))
                      (find-packages-by-description regexp)))
           #t))

        (('show requested-name)
         (let-values (((name version)
                       (package-name->name+version requested-name)))
           (leave-on-EPIPE
            (for-each (cute package->recutils <> (current-output-port))
                      (find-packages-by-name name version)))
           #t))

        (('search-paths kind)
         (let* ((manifest (profile-manifest profile))
                (entries  (manifest-entries manifest))
                (profile  (user-friendly-profile profile))
                (settings (search-path-environment-variables entries profile
                                                             (const #f)
                                                             #:kind kind)))
           (format #t "~{~a~%~}" settings)
           #t))

        (_ #f))))

  (let ((opts (parse-command-line args %options (list %default-options #f)
                                  #:argument-handler handle-argument)))
    (with-error-handling
      (or (process-query opts)
          (parameterize ((%store (open-connection)))
            (set-build-options-from-command-line (%store) opts)

            (parameterize ((%guile-for-build
                            (package-derivation
                             (%store)
                             (if (assoc-ref opts 'bootstrap?)
                                 %bootstrap-guile
                                 (canonical-package guile-2.0)))))
              (process-actions opts)))))))
