;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
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

(define-module (guix scripts pull)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix scripts)
  #:use-module (guix store)
  #:use-module (guix config)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix profiles)
  #:use-module (guix gexp)
  #:use-module (guix grafts)
  #:use-module (guix memoization)
  #:use-module (guix monads)
  #:use-module (guix channels)
  #:autoload   (guix inferior) (open-inferior)
  #:use-module (guix scripts build)
  #:autoload   (guix build utils) (which)
  #:use-module (guix git)
  #:use-module (git)
  #:use-module (gnu packages)
  #:use-module ((guix scripts package) #:select (build-and-use-profile))
  #:use-module ((gnu packages base) #:select (canonical-package))
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages bootstrap)
                #:select (%bootstrap-guile))
  #:use-module ((gnu packages certs) #:select (le-certs))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (display-profile-content
            guix-pull))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((system . ,(%current-system))
    (substitutes? . #t)
    (build-hook? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (graft? . #t)
    (debug . 0)
    (verbosity . 1)))

(define (show-help)
  (display (G_ "Usage: guix pull [OPTION]...
Download and deploy the latest version of Guix.\n"))
  (display (G_ "
      --verbose          produce verbose output"))
  (display (G_ "
  -C, --channels=FILE    deploy the channels defined in FILE"))
  (display (G_ "
      --url=URL          download from the Git repository at URL"))
  (display (G_ "
      --commit=COMMIT    download the specified COMMIT"))
  (display (G_ "
      --branch=BRANCH    download the tip of the specified BRANCH"))
  (display (G_ "
  -N, --news             display news compared to the previous generation"))
  (display (G_ "
  -l, --list-generations[=PATTERN]
                         list generations matching PATTERN"))
  (display (G_ "
  -p, --profile=PROFILE  use PROFILE instead of ~/.config/guix/current"))
  (display (G_ "
  -v, --verbosity=LEVEL  use the given verbosity LEVEL"))
  (display (G_ "
  -s, --system=SYSTEM    attempt to build for SYSTEM--e.g., \"i686-linux\""))
  (display (G_ "
      --bootstrap        use the bootstrap Guile to build the new Guix"))
  (newline)
  (show-build-options-help)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specifications of the command-line options.
  (cons* (option '("verbose") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'verbose? #t result)))
         (option '(#\C "channels") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'channel-file arg result)))
         (option '(#\l "list-generations") #f #t
                 (lambda (opt name arg result)
                   (cons `(query list-generations ,(or arg ""))
                         result)))
         (option '(#\N "news") #f #f
                 (lambda (opt name arg result)
                   (cons '(query display-news) result)))
         (option '("url") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'repository-url arg
                               (alist-delete 'repository-url result))))
         (option '("commit") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'ref `(commit . ,arg) result)))
         (option '("branch") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'ref `(branch . ,arg) result)))
         (option '(#\p "profile") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'profile (canonicalize-profile arg)
                               result)))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg
                               (alist-delete 'system result eq?))))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t (alist-cons 'graft? #f result))))
         (option '(#\v "verbosity") #t #f
                 (lambda (opt name arg result)
                   (let ((level (string->number* arg)))
                     (alist-cons 'verbosity level
                                 (alist-delete 'verbosity result)))))
         (option '("bootstrap") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'bootstrap? #t result)))

         (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix pull")))

         %standard-build-options))

(define what-to-build
  (store-lift show-what-to-build))
(define indirect-root-added
  (store-lift add-indirect-root))

(define* (display-profile-news profile #:key concise?
                               current-is-newer?)
  "Display what's up in PROFILE--new packages, and all that.  If
CURRENT-IS-NEWER? is true, assume that the current process represents the
newest generation of PROFILE.x"
  (match (memv (generation-number profile)
               (reverse (profile-generations profile)))
    ((current previous _ ...)
     (let ((these (fold-available-packages
                   (lambda* (name version result
                                  #:key supported? deprecated?
                                  #:allow-other-keys)
                     (if (and supported? (not deprecated?))
                         (alist-cons name version result)
                         result))
                   '()))
           (those (profile-package-alist
                   (generation-file-name profile
                                         (if current-is-newer?
                                             previous
                                             current)))))
       (let ((old (if current-is-newer? those these))
             (new (if current-is-newer? these those)))
         (display-new/upgraded-packages old new
                                        #:concise? concise?
                                        #:heading
                                        (G_ "New in this revision:\n")))))
    (_ #t)))

(define* (build-and-install instances profile
                            #:key verbose? dry-run?)
  "Build the tool from SOURCE, and install it in PROFILE.  When DRY-RUN? is
true, display what would be built without actually building it."
  (define update-profile
    (store-lift build-and-use-profile))

  (mlet %store-monad ((manifest (channel-instances->manifest instances)))
    (mbegin %store-monad
      (update-profile profile manifest
                      #:hooks %channel-profile-hooks
                      #:dry-run? dry-run?)
      (munless dry-run?
        (return (newline))
        (return (display-profile-news profile #:concise? #t))
        (match (which "guix")
          (#f (return #f))
          (str
           (let ((new (map (cut string-append <> "/bin/guix")
                           (list (user-friendly-profile profile)
                                 profile))))
             (unless (member str new)
               (display-hint (format #f (G_ "After setting @code{PATH}, run
@command{hash guix} to make sure your shell refers to @file{~a}.")
                                     (first new))))
             (return #f))))))))

(define (honor-lets-encrypt-certificates! store)
  "Tell Guile-Git to use the Let's Encrypt certificates."
  (let* ((drv   (package-derivation store le-certs))
         (certs (string-append (derivation->output-path drv)
                               "/etc/ssl/certs")))
    (build-derivations store (list drv))
    (set-tls-certificate-locations! certs)))

(define (honor-x509-certificates store)
  "Use the right X.509 certificates for Git checkouts over HTTPS."
  (unless (honor-system-x509-certificates!)
    (honor-lets-encrypt-certificates! store)))

(define (report-git-error error)
  "Report the given Guile-Git error."
  ;; Prior to Guile-Git commit b6b2760c2fd6dfaa5c0fedb43eeaff06166b3134,
  ;; errors would be represented by integers.
  (match error
    ((? integer? error)                           ;old Guile-Git
     (leave (G_ "Git error ~a~%") error))
    ((? git-error? error)                         ;new Guile-Git
     (leave (G_ "Git error: ~a~%") (git-error-message error)))))

(define-syntax-rule (with-git-error-handling body ...)
  (catch 'git-error
    (lambda ()
      body ...)
    (lambda (key err)
      (report-git-error err))))


;;;
;;; Profile.
;;;

(define %current-profile
  ;; The "real" profile under /var/guix.
  (string-append %profile-directory "/current-guix"))

(define %user-profile-directory
  ;; The user-friendly name of %CURRENT-PROFILE.
  (string-append (config-directory #:ensure? #f) "/current"))

(define (migrate-generations profile directory)
  "Migrate the generations of PROFILE to DIRECTORY."
  (format (current-error-port)
          (G_ "Migrating profile generations to '~a'...~%")
          %profile-directory)
  (let ((current (generation-number profile)))
    (for-each (lambda (generation)
                (let ((source (generation-file-name profile generation))
                      (target (string-append directory "/current-guix-"
                                             (number->string generation)
                                             "-link")))
                  ;; Note: Don't use 'rename-file' as SOURCE and TARGET might
                  ;; live on different file systems.
                  (symlink (readlink source) target)
                  (delete-file source)))
              (profile-generations profile))
    (symlink (string-append "current-guix-"
                            (number->string current) "-link")
             (string-append directory "/current-guix"))))

(define (ensure-default-profile)
  (ensure-profile-directory)

  ;; In 0.15.0+ we'd create ~/.config/guix/current-[0-9]*-link symlinks.  Move
  ;; them to %PROFILE-DIRECTORY.
  (unless (string=? %profile-directory
                    (dirname (canonicalize-profile %user-profile-directory)))
    (migrate-generations %user-profile-directory %profile-directory))

  ;; Make sure ~/.config/guix/current points to /var/guix/profiles/….
  (let ((link %user-profile-directory))
    (unless (equal? (false-if-exception (readlink link))
                    %current-profile)
      (catch 'system-error
        (lambda ()
          (false-if-exception (delete-file link))
          (symlink %current-profile link))
        (lambda args
          (leave (G_ "while creating symlink '~a': ~a~%")
                 link (strerror (system-error-errno args))))))))


;;;
;;; Queries.
;;;

(define (display-profile-content profile number)
  "Display the packages in PROFILE, generation NUMBER, in a human-readable
way and displaying details about the channel's source code."
  (display-generation profile number)
  (for-each (lambda (entry)
              (format #t "  ~a ~a~%"
                      (manifest-entry-name entry)
                      (manifest-entry-version entry))
              (match (assq 'source (manifest-entry-properties entry))
                (('source ('repository ('version 0)
                                       ('url url)
                                       ('branch branch)
                                       ('commit commit)
                                       _ ...))
                 (format #t (G_ "    repository URL: ~a~%") url)
                 (when branch
                   (format #t (G_ "    branch: ~a~%") branch))
                 (format #t (G_ "    commit: ~a~%") commit))
                (_ #f)))

            ;; Show most recently installed packages last.
            (reverse
             (manifest-entries
              (profile-manifest (if (zero? number)
                                    profile
                                    (generation-file-name profile number)))))))

(define (indented-string str indent)
  "Return STR with each newline preceded by IDENT spaces."
  (define indent-string
    (make-list indent #\space))

  (list->string
   (string-fold-right (lambda (chr result)
                        (if (eqv? chr #\newline)
                            (cons chr (append indent-string result))
                            (cons chr result)))
                      '()
                      str)))

(define profile-package-alist
  (mlambda (profile)
    "Return a name/version alist representing the packages in PROFILE."
    (let* ((inferior (open-inferior profile))
           (packages (inferior-available-packages inferior)))
      (close-inferior inferior)
      packages)))

(define (new/upgraded-packages alist1 alist2)
  "Compare ALIST1 and ALIST2, both of which are lists of package name/version
pairs, and return two values: the list of packages new in ALIST2, and the list
of packages upgraded in ALIST2."
  (let* ((old      (fold (match-lambda*
                           (((name . version) table)
                            (match (vhash-assoc name table)
                              (#f
                               (vhash-cons name version table))
                              ((_ . previous-version)
                               (if (version>? version previous-version)
                                   (vhash-cons name version table)
                                   table)))))
                         vlist-null
                         alist1))
         (new      (remove (match-lambda
                             ((name . _)
                              (vhash-assoc name old)))
                           alist2))
         (upgraded (filter-map (match-lambda
                                 ((name . new-version)
                                  (match (vhash-assoc name old)
                                    (#f #f)
                                    ((_ . old-version)
                                     (and (version>? new-version old-version)
                                          (string-append name "@"
                                                         new-version))))))
                               alist2)))
    (values new upgraded)))

(define* (ellipsis #:optional (port (current-output-port)))
  "Return HORIZONTAL ELLIPSIS three dots if PORT's encoding cannot represent
it."
  (match (port-encoding port)
    ("UTF-8" "…")
    (_       "...")))

(define* (display-new/upgraded-packages alist1 alist2
                                        #:key (heading "") concise?)
  "Given the two package name/version alists ALIST1 and ALIST2, display the
list of new and upgraded packages going from ALIST1 to ALIST2.  When ALIST1
and ALIST2 differ, display HEADING upfront.  When CONCISE? is true, do not
display long package lists that would fill the user's screen."
  (define (pretty str column)
    (indented-string (fill-paragraph str (- (%text-width) 4)
                                     column)
                     4))

  (define concise/max-item-count
    ;; Maximum number of items to display when CONCISE? is true.
    12)

  (define list->enumeration
    (if concise?
        (lambda* (lst #:optional (max concise/max-item-count))
          (if (> (length lst) max)
              (string-append (string-join (take lst max) ", ")
                             ", " (ellipsis))
              (string-join lst ", ")))
        (cut string-join <> ", ")))

  (let-values (((new upgraded) (new/upgraded-packages alist1 alist2)))
    (define new-count (length new))
    (define upgraded-count (length upgraded))

    (unless (and (null? new) (null? upgraded))
      (display heading))

    (match new-count
      (0 #t)
      (count
       (format #t (N_ "  ~h new package: ~a~%"
                      "  ~h new packages: ~a~%" count)
               count
               (pretty (list->enumeration (sort (map first new) string<?))
                       30))))
    (match upgraded-count
      (0 #t)
      (count
       (format #t (N_ "  ~h package upgraded: ~a~%"
                      "  ~h packages upgraded: ~a~%" count)
               count
               (pretty (list->enumeration (sort upgraded string<?))
                       35))))

    (when (and concise?
               (or (> new-count concise/max-item-count)
                   (> upgraded-count concise/max-item-count)))
      (display-hint (G_ "Run @command{guix pull --news} to view the complete
list of package changes.")))))

(define (display-profile-content-diff profile gen1 gen2)
  "Display the changes in PROFILE GEN2 compared to generation GEN1."
  (define (package-alist generation)
    (profile-package-alist (generation-file-name profile generation)))

  (display-profile-content profile gen2)
  (display-new/upgraded-packages (package-alist gen1)
                                 (package-alist gen2)))

(define (process-query opts profile)
  "Process any query on PROFILE specified by OPTS."
  (match (assoc-ref opts 'query)
    (('list-generations pattern)
     (define (list-generations profile numbers)
       (match numbers
         ((first rest ...)
          (display-profile-content profile first)
          (let loop ((numbers numbers))
            (match numbers
              ((first second rest ...)
               (display-profile-content-diff profile
                                             first second)
               (loop (cons second rest)))
              ((_) #t)
              (()  #t))))))

     (leave-on-EPIPE
      (cond ((not (file-exists? profile))         ; XXX: race condition
             (raise (condition (&profile-not-found-error
                                (profile profile)))))
            ((string-null? pattern)
             (list-generations profile (profile-generations profile)))
            ((matching-generations pattern profile)
             =>
             (match-lambda
               (()
                (exit 1))
               ((numbers ...)
                (list-generations profile numbers)))))))
    (('display-news)
     ;; Display profile news, with the understanding that this process
     ;; represents the newest generation.
     (display-profile-news profile
                           #:current-is-newer? #t))))

(define (channel-list opts)
  "Return the list of channels to use.  If OPTS specify a channel file,
channels are read from there; otherwise, if ~/.config/guix/channels.scm
exists, read it; otherwise %DEFAULT-CHANNELS is used.  Apply channel
transformations specified in OPTS (resulting from '--url', '--commit', or
'--branch'), if any."
  (define file
    (assoc-ref opts 'channel-file))

  (define default-file
    (string-append (config-directory) "/channels.scm"))

  (define (load-channels file)
    (let ((result (load* file (make-user-module '((guix channels))))))
      (if (and (list? result) (every channel? result))
          result
          (leave (G_ "'~a' did not return a list of channels~%") file))))

  (define channels
    (cond (file
           (load-channels file))
          ((file-exists? default-file)
           (load-channels default-file))
          (else
           %default-channels)))

  (define (environment-variable)
    (match (getenv "GUIX_PULL_URL")
      (#f #f)
      (url
       (warning (G_ "The 'GUIX_PULL_URL' environment variable is deprecated.
Use '~/.config/guix/channels.scm' instead."))
       url)))

  (let ((ref (assoc-ref opts 'ref))
        (url (or (assoc-ref opts 'repository-url)
                 (environment-variable))))
    (if (or ref url)
        (match (find guix-channel? channels)
          ((? channel? guix)
           ;; Apply '--url', '--commit', and '--branch' to the 'guix' channel.
           (let ((url (or url (channel-url guix))))
             (cons (match ref
                     (('commit . commit)
                      (channel (inherit guix)
                               (url url) (commit commit) (branch #f)))
                     (('branch . branch)
                      (channel (inherit guix)
                               (url url) (commit #f) (branch branch)))
                     (#f
                      (channel (inherit guix) (url url))))
                   (remove guix-channel? channels))))
          (#f                           ;no 'guix' channel, failure will ensue
           channels))
        channels)))


(define (guix-pull . args)
  (with-error-handling
    (with-git-error-handling
     (let* ((opts     (parse-command-line args %options
                                          (list %default-options)))
            (cache    (string-append (cache-directory) "/pull"))
            (channels (channel-list opts))
            (profile  (or (assoc-ref opts 'profile) %current-profile)))
       (ensure-default-profile)
       (cond ((assoc-ref opts 'query)
              (process-query opts profile))
             (else
              (with-store store
                (with-status-verbosity (assoc-ref opts 'verbosity)
                  (parameterize ((%current-system (assoc-ref opts 'system))
                                 (%graft? (assoc-ref opts 'graft?))
                                 (%repository-cache-directory cache))
                    (set-build-options-from-command-line store opts)
                    (honor-x509-certificates store)

                    (let ((instances (latest-channel-instances store channels)))
                      (format (current-error-port)
                              (N_ "Building from this channel:~%"
                                  "Building from these channels:~%"
                                  (length instances)))
                      (for-each (lambda (instance)
                                  (let ((channel
                                         (channel-instance-channel instance)))
                                    (format (current-error-port)
                                            "  ~10a~a\t~a~%"
                                            (channel-name channel)
                                            (channel-url channel)
                                            (string-take
                                             (channel-instance-commit instance)
                                             7))))
                                instances)
                      (parameterize ((%guile-for-build
                                      (package-derivation
                                       store
                                       (if (assoc-ref opts 'bootstrap?)
                                           %bootstrap-guile
                                           (canonical-package guile-2.2)))))
                        (run-with-store store
                          (build-and-install instances profile
                                             #:dry-run?
                                             (assoc-ref opts 'dry-run?)
                                             #:verbose?
                                             (assoc-ref opts 'verbose?))))))))))))))

;;; pull.scm ends here
