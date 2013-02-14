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
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:use-module (gnu packages)
  #:use-module ((gnu packages base) #:select (guile-final))
  #:use-module ((gnu packages bootstrap) #:select (%bootstrap-guile))
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

(define (profile-numbers profile)
  "Return the list of generation numbers of PROFILE, or '(0) if no
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
     (map (compose string->number
                   (cut match:substring <> 1)
                   (cute regexp-exec (profile-regexp profile) <>))
          profiles))))

(define (previous-profile-number profile number)
  "Return the number of the generation before generation NUMBER of
PROFILE, or 0 if none exists.  It could be NUMBER - 1, but it's not the
case when generations have been deleted (there are \"holes\")."
  (fold (lambda (candidate highest)
          (if (and (< candidate number) (> candidate highest))
              candidate
              highest))
        0
        (profile-numbers profile)))

(define (profile-derivation store packages)
  "Return a derivation that builds a profile (a user environment) with
all of PACKAGES, a list of name/version/output/path/deps tuples."
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
                                      (packages ,packages))
                           p))))))

  (build-expression->derivation store "user-environment"
                                (%current-system)
                                builder
                                (append-map (match-lambda
                                             ((name version output path deps)
                                              `((,name ,path)
                                                ,@deps)))
                                            packages)
                                #:modules '((guix build union))))

(define (profile-number profile)
  "Return PROFILE's number or 0.  An absolute file name must be used."
  (or (and=> (false-if-exception (regexp-exec (profile-regexp profile)
                                              (basename (readlink profile))))
             (compose string->number (cut match:substring <> 1)))
      0))

(define (switch-symlinks link target)
  "Atomically switch LINK, a symbolic link, to point to TARGET.  Works
both when LINK already exists and when it does not."
  (let ((pivot (string-append link ".new")))
    (symlink target pivot)
    (rename-file pivot link)))

(define (roll-back profile)
  "Roll back to the previous generation of PROFILE."
  (let* ((number           (profile-number profile))
         (previous-number  (previous-profile-number profile number))
         (previous-profile (format #f "~a-~a-link"
                                   profile previous-number))
         (manifest         (string-append previous-profile "/manifest")))

    (define (switch-link)
      ;; Atomically switch PROFILE to the previous profile.
      (format #t (_ "switching from generation ~a to ~a~%")
              number previous-number)
      (switch-symlinks profile previous-profile))

    (cond ((not (file-exists? profile))           ; invalid profile
           (format (current-error-port)
                   (_ "error: profile `~a' does not exist~%")
                   profile))
          ((zero? number)                         ; empty profile
           (format (current-error-port)
                   (_ "nothing to do: already at the empty profile~%")))
          ((or (zero? previous-number)            ; going to emptiness
               (not (file-exists? previous-profile)))
           (let*-values (((drv-path drv)
                          (profile-derivation (%store) '()))
                         ((prof)
                          (derivation-output-path
                           (assoc-ref (derivation-outputs drv) "out"))))
             (when (not (build-derivations (%store) (list drv-path)))
               (leave (_ "failed to build the empty profile~%")))

             (switch-symlinks previous-profile prof)
             (switch-link)))
          (else (switch-link)))))                 ; anything else

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
      ((name package)
       (loop `(,name ,package "out")))
      ((name package sub-drv)
       (let*-values (((_ drv)
                      (package-derivation (%store) package))
                     ((out)
                      (derivation-output-path
                       (assoc-ref (derivation-outputs drv) sub-drv))))
         `(,name ,out))))))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((profile . ,%current-profile)))

(define (show-help)
  (display (_ "Usage: guix package [OPTION]... PACKAGES...
Install, remove, or upgrade PACKAGES in a single transaction.\n"))
  (display (_ "
  -i, --install=PACKAGE  install PACKAGE"))
  (display (_ "
  -r, --remove=PACKAGE   remove PACKAGE"))
  (display (_ "
  -u, --upgrade=REGEXP   upgrade all the installed packages matching REGEXP"))
  (display (_ "
      --roll-back        roll back to the previous generation"))
  (newline)
  (display (_ "
  -p, --profile=PROFILE  use PROFILE instead of the user's default profile"))
  (display (_ "
  -n, --dry-run          show what would be done without actually doing it"))
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
                  (show-version-and-exit "guix-package")))

        (option '(#\i "install") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'install arg result)))
        (option '(#\r "remove") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'remove arg result)))
        (option '(#\u "upgrade") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'upgrade arg result)))
        (option '("roll-back") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'roll-back? #t result)))
        (option '(#\p "profile") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'profile arg
                              (alist-delete 'profile result))))
        (option '(#\n "dry-run") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'dry-run? #t result)))
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
    (args-fold args %options
               (lambda (opt name arg result)
                 (leave (_ "~A: unrecognized option~%") name))
               (lambda (arg result)
                 (leave (_ "~A: extraneous argument~%") arg))
               %default-options))

  (define (guile-missing?)
    ;; Return #t if %GUILE-FOR-BUILD is not available yet.
    (let ((out (derivation-path->output-path (%guile-for-build))))
      (not (valid-path? (%store) out))))

  (define (show-what-to-build drv dry-run?)
    ;; Show what will/would be built in realizing the derivations listed
    ;; in DRV.
    (let* ((req  (append-map (lambda (drv-path)
                               (let ((d (call-with-input-file drv-path
                                          read-derivation)))
                                 (derivation-prerequisites-to-build
                                  (%store) d)))
                             drv))
           (req* (delete-duplicates
                  (append (remove (compose (cute valid-path? (%store) <>)
                                           derivation-path->output-path)
                                  drv)
                          (map derivation-input-path req)))))
      (if dry-run?
          (format (current-error-port)
                  (N_ "~:[the following derivation would be built:~%~{   ~a~%~}~;~]"
                      "~:[the following derivations would be built:~%~{    ~a~%~}~;~]"
                      (length req*))
                  (null? req*) req*)
          (format (current-error-port)
                  (N_ "~:[the following derivation will be built:~%~{   ~a~%~}~;~]"
                      "~:[the following derivations will be built:~%~{    ~a~%~}~;~]"
                      (length req*))
                  (null? req*) req*))))

  (define newest-available-packages
    (memoize find-newest-available-packages))

  (define (find-best-packages-by-name name version)
    (if version
        (find-packages-by-name name version)
        (match (vhash-assoc name (newest-available-packages))
          ((_ version pkgs ...) pkgs)
          (#f '()))))

  (define (find-package name)
    ;; Find the package NAME; NAME may contain a version number and a
    ;; sub-derivation name.  If the version number is not present,
    ;; return the preferred newest version.
    (define request name)

    (define (ensure-output p sub-drv)
      (if (member sub-drv (package-outputs p))
          p
          (leave (_ "~a: error: package `~a' lacks output `~a'~%")
                 (location->string (package-location p))
                 (package-full-name p)
                 sub-drv)))

    (let*-values (((name sub-drv)
                   (match (string-rindex name #\:)
                     (#f    (values name "out"))
                     (colon (values (substring name 0 colon)
                                    (substring name (+ 1 colon))))))
                  ((name version)
                   (package-name->name+version name)))
      (match (find-best-packages-by-name name version)
        ((p)
         (list name (package-version p) sub-drv (ensure-output p sub-drv)
               (package-transitive-propagated-inputs p)))
        ((p p* ...)
         (format (current-error-port)
                 (_ "warning: ambiguous package specification `~a'~%")
                 request)
         (format (current-error-port)
                 (_ "warning: choosing ~a from ~a~%")
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
         ((=) (let ((candidate-path (derivation-path->output-path
                                     (package-derivation (%store) pkg))))
                (not (string=? current-path candidate-path))))))
      (#f #f)))

  (define (ensure-default-profile)
    ;; Ensure the default profile symlink and directory exist.

    ;; Create ~/.guix-profile if it doesn't exist yet.
    (when (and %user-environment-directory
               %current-profile
               (not (false-if-exception
                     (lstat %user-environment-directory))))
      (symlink %current-profile %user-environment-directory))

    ;; Attempt to create /…/profiles/per-user/$USER if needed.
    (unless (directory-exists? %profile-directory)
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
          (exit 1)))))

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
          ((_ path1)
           (match d2
             ((_ path2)
              (string=? path1 path2))))))

      (delete-duplicates (map input->name+path deps) same?))

    ;; First roll back if asked to.
    (if (and (assoc-ref opts 'roll-back?) (not dry-run?))
        (begin
          (roll-back profile)
          (process-actions (alist-delete 'roll-back? opts)))
        (let* ((installed (manifest-packages (profile-manifest profile)))
               (upgrade-regexps (filter-map (match-lambda
                                             (('upgrade . regexp)
                                              (make-regexp regexp))
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
                                                  (find-package name)))
                                            (_ #f))
                                           installed))))
               (install  (append
                          upgrade
                          (filter-map (match-lambda
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
                                       (package-derivation (%store) package))
                                      (_ #f))
                                     install))
               (install* (append
                          (filter-map (match-lambda
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
                                           (derivation-path->output-path
                                            drv sub-drv)))
                                      `(,name ,version ,sub-drv ,output-path
                                              ,(canonicalize-deps deps))))))
                               install drv)))
               (remove   (filter-map (match-lambda
                                      (('remove . package)
                                       package)
                                      (_ #f))
                                     opts))
               (packages (append install*
                                 (fold (lambda (package result)
                                         (match package
                                           ((name _ ...)
                                            (alist-delete name result))))
                                       (fold alist-delete installed remove)
                                       install*))))

          (when (equal? profile %current-profile)
            (ensure-default-profile))

          (show-what-to-build drv dry-run?)

          (or dry-run?
              (and (build-derivations (%store) drv)
                   (let* ((prof-drv (profile-derivation (%store) packages))
                          (prof     (derivation-path->output-path prof-drv))
                          (old-drv  (profile-derivation
                                     (%store) (manifest-packages
                                               (profile-manifest profile))))
                          (old-prof (derivation-path->output-path old-drv))
                          (number   (profile-number profile))

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
                              (begin
                                (switch-symlinks name prof)
                                (switch-symlinks profile name))))))))))

  (define (process-query opts)
    ;; Process any query specified by OPTS.  Return #t when a query was
    ;; actually processed, #f otherwise.
    (let ((profile  (assoc-ref opts 'profile)))
      (match (assoc-ref opts 'query)
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
                     installed)
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
        (_ #f))))

  (install-locale)
  (textdomain "guix")
  (setvbuf (current-output-port) _IOLBF)
  (setvbuf (current-error-port) _IOLBF)

  (let ((opts (parse-options)))
    (or (process-query opts)
        (parameterize ((%store (open-connection)))
          (with-error-handling
            (parameterize ((%guile-for-build
                            (package-derivation (%store)
                                                (if (assoc-ref opts 'bootstrap?)
                                                    %bootstrap-guile
                                                    guile-final))))
              (process-actions opts)))))))
