;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module ((guix ui) #:hide (display-profile-content))
  #:use-module (guix colors)
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
  #:autoload   (guix inferior) (open-inferior
                                inferior-available-packages
                                close-inferior)
  #:use-module (guix scripts build)
  #:use-module (guix scripts describe)
  #:autoload   (guix build utils) (which mkdir-p)
  #:use-module ((guix build syscalls)
                #:select (with-file-lock/no-wait))
  #:use-module (guix git)
  #:use-module (git)
  #:use-module (gnu packages)
  #:use-module ((guix scripts package) #:select (build-and-use-profile
                                                 delete-matching-generations))
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
  #:use-module (ice-9 format)
  #:re-export (display-profile-content
               channel-commit-hyperlink)
  #:export (channel-list
            guix-pull))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((system . ,(%current-system))
    (substitutes? . #t)
    (offload? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (graft? . #t)
    (debug . 0)
    (verbosity . 1)
    (authenticate-channels? . #t)
    (validate-pull . ,ensure-forward-channel-update)))

(define (show-help)
  (display (G_ "Usage: guix pull [OPTION]...
Download and deploy the latest version of Guix.\n"))
  (display (G_ "
  -C, --channels=FILE    deploy the channels defined in FILE"))
  (display (G_ "
      --url=URL          download \"guix\" channel from the Git repository at URL"))
  (display (G_ "
      --commit=COMMIT    download the specified \"guix\" channel COMMIT"))
  (display (G_ "
      --branch=BRANCH    download the tip of the specified \"guix\" channel BRANCH"))
  (display (G_ "
      --allow-downgrades allow downgrades to earlier channel revisions"))
  (display (G_ "
      --disable-authentication
                         disable channel authentication"))
  (display (G_ "
  -N, --news             display news compared to the previous generation"))
  (display (G_ "
  -l, --list-generations[=PATTERN]
                         list generations matching PATTERN"))
  (display (G_ "
      --roll-back        roll back to the previous generation"))
  (display (G_ "
  -d, --delete-generations[=PATTERN]
                         delete generations matching PATTERN"))
  (display (G_ "
  -S, --switch-generation=PATTERN
                         switch to a generation matching PATTERN"))
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
  (cons* (option '(#\C "channels") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'channel-file arg result)))
         (option '(#\l "list-generations") #f #t
                 (lambda (opt name arg result)
                   (cons `(query list-generations ,arg)
                         result)))
         (option '("roll-back") #f #f
                 (lambda (opt name arg result)
                   (cons '(generation roll-back)
                         result)))
         (option '(#\S "switch-generation") #t #f
                 (lambda (opt name arg result)
                   (cons `(generation switch ,arg)
                         result)))
         (option '(#\d "delete-generations") #f #t
                 (lambda (opt name arg result)
                   (cons `(generation delete ,arg)
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
         (option '("allow-downgrades") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'validate-pull warn-about-backward-updates
                               result)))
         (option '("disable-authentication") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'authenticate-channels? #f result)))
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
                   (alist-cons 'dry-run? #t result)))
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

(define (warn-about-backward-updates channel start commit relation)
  "Warn about non-forward updates of CHANNEL from START to COMMIT, without
aborting."
  (match relation
    ((or 'ancestor 'self)
     #t)
    ('descendant
     (warning (G_ "rolling back channel '~a' from ~a to ~a~%")
              (channel-name channel) start commit))
    ('unrelated
     (warning (G_ "moving channel '~a' from ~a to unrelated commit ~a~%")
              (channel-name channel) start commit))))

(define* (display-profile-news profile #:key concise?
                               current-is-newer?)
  "Display what's up in PROFILE--new packages, and all that.  If
CURRENT-IS-NEWER? is true, assume that the current process represents the
newest generation of PROFILE.  Return true when there's more info to display."
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
    (_ #f)))

(define (display-channel channel)
  "Display information about CHANNEL."
  (format (current-error-port)
          ;; TRANSLATORS: This describes a "channel"; the first placeholder is
          ;; the channel name (e.g., "guix") and the second placeholder is its
          ;; URL.
          (G_ "    ~a at ~a~%")
          (channel-name channel)
          (channel-url channel)))

(define (channel=? channel1 channel2)
  "Return true if CHANNEL1 and CHANNEL2 are the same for all practical
purposes."
  ;; Assume that the URL matters less than the name.
  (eq? (channel-name channel1) (channel-name channel2)))

(define (display-news-entry-title entry language port)
  "Display the title of ENTRY, a news entry, to PORT."
  (define title
    (channel-news-entry-title entry))

  (let ((title (or (assoc-ref title language)
                   (assoc-ref title (%default-message-language))
                   "")))
    (format port "  ~a~%"
            (highlight
             (string-trim-right
              (catch 'parser-error
                (lambda ()
                  (texi->plain-text title))

                ;; When Texinfo markup is invalid, display it as-is.
                (const title)))))))

(define (display-news-entry entry channel language port)
  "Display ENTRY, a <channel-news-entry> from CHANNEL, in LANGUAGE, a language
code, to PORT."
  (define body
    (channel-news-entry-body entry))

  (define commit
    (channel-news-entry-commit entry))

  (display-news-entry-title entry language port)
  (format port (dim (G_ "    commit ~a~%"))
          (if (supports-hyperlinks?)
              (channel-commit-hyperlink channel commit)
              commit))
  (newline port)
  (let ((body (or (assoc-ref body language)
                  (assoc-ref body (%default-message-language))
                  "")))
    (format port "~a~%"
            (indented-string
             (parameterize ((%text-width (- (%text-width) 4)))
               (string-trim-right
                (catch 'parser-error
                  (lambda ()
                    (texi->plain-text body))
                  (lambda _
                    ;; When Texinfo markup is invalid, display it as-is.
                    (fill-paragraph body (%text-width))))))
             4))))

(define* (display-channel-specific-news new old
                                        #:key (port (current-output-port))
                                        concise?)
  "Display channel news applicable the commits between OLD and NEW, where OLD
and NEW are <channel> records with a proper 'commit' field.  When CONCISE? is
true, display nothing but the news titles.  Return true if there are more news
to display."
  (let ((channel new)
        (old     (channel-commit old))
        (new     (channel-commit new)))
    (when (and old new)
      (let ((language (current-message-language)))
        (match (channel-news-for-commit channel new old)
          (()                                     ;no news is good news
           #f)
          ((entries ...)
           (newline port)
           (format port (G_ "News for channel '~a'~%")
                   (channel-name channel))
           (for-each (if concise?
                         (cut display-news-entry-title <> language port)
                         (cut display-news-entry <> channel language port))
                     entries)
           (newline port)
           #t))))))

(define* (display-channel-news profile
                               #:optional
                               (previous
                                (and=> (relative-generation profile -1)
                                       (cut generation-file-name profile <>))))
  "Display news about the channels of PROFILE compared to PREVIOUS."
  (when previous
    (let ((old-channels (profile-channels previous))
          (new-channels (profile-channels profile)))
      (and (pair? old-channels) (pair? new-channels)
           (begin
             (match (lset-difference channel=? new-channels old-channels)
               (()
                #t)
               (new
                (let ((count (length new)))
                  (format (current-error-port)
                          (N_ "  ~a new channel:~%"
                              "  ~a new channels:~%" count)
                          count)
                  (for-each display-channel new))))
             (match (lset-difference channel=? old-channels new-channels)
               (()
                #t)
               (removed
                (let ((count (length removed)))
                  (format (current-error-port)
                          (N_ "  ~a channel removed:~%"
                              "  ~a channels removed:~%" count)
                          count)
                  (for-each display-channel removed))))

             ;; Display channel-specific news for those channels that were
             ;; here before and are still around afterwards.
             (for-each (match-lambda
                         ((new old)
                          (display-channel-specific-news new old)))
                       (filter-map (lambda (new)
                                     (define old
                                       (find (cut channel=? new <>)
                                             old-channels))

                                     (and old (list new old)))
                                   new-channels)))))))

(define* (display-channel-news-headlines profile)
  "Display the titles of news about the channels of PROFILE compared to its
previous generation.  Return true if there are news to display."
  (define previous
    (and=> (relative-generation profile -1)
           (cut generation-file-name profile <>)))

  (and previous
    (let ((old-channels (profile-channels previous))
          (new-channels (profile-channels profile)))
      ;; Find the channels present in both PROFILE and PREVIOUS, and print
      ;; their news.
      (and (pair? old-channels) (pair? new-channels)
           (let ((channels (filter-map (lambda (new)
                                         (define old
                                           (find (cut channel=? new <>)
                                                 old-channels))

                                         (and old (list new old)))
                                       new-channels)))
             (define more?
               (map (match-lambda
                      ((new old)
                       (display-channel-specific-news new old
                                                      #:concise? #t)))
                    channels))

             (any ->bool more?))))))

(define (display-news profile)
  ;; Display profile news, with the understanding that this process represents
  ;; the newest generation.
  (display-profile-news profile
                        #:current-is-newer? #t)

  (display-channel-news profile))

(define* (build-and-install instances profile)
  "Build the tool from SOURCE, and install it in PROFILE.  When DRY-RUN? is
true, display what would be built without actually building it."
  (define update-profile
    (store-lift build-and-use-profile))

  (define guix-command
    ;; The 'guix' command before we've built the new profile.
    (which "guix"))

  (mlet %store-monad ((manifest (channel-instances->manifest instances)))
    (mbegin %store-monad
      (update-profile profile manifest
                      #:hooks %channel-profile-hooks)

      (return
       (let ((more? (list (display-profile-news profile #:concise? #t)
                          (display-channel-news-headlines profile))))
         (newline)
         (when (any ->bool more?)
           (display-hint
            (G_ "Run @command{guix pull --news} to read all the news.")))))
      (if guix-command
          (let ((new (map (cut string-append <> "/bin/guix")
                          (list (user-friendly-profile profile)
                                profile))))
            ;; Is the 'guix' command previously in $PATH the same as the new
            ;; one?  If the answer is "no", then suggest 'hash guix'.
            (unless (member guix-command new)
              (display-hint (format #f (G_ "After setting @code{PATH}, run
@command{hash guix} to make sure your shell refers to @file{~a}.")
                                    (first new))))
            (return #f))
          (return #f)))))

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
  ;;
  ;; XXX: Ubuntu's 'sudo' preserves $HOME by default, and thus the second
  ;; condition below is always false when one runs "sudo guix pull".  As a
  ;; workaround, skip this code when $SUDO_USER is set.  See
  ;; <https://bugs.gnu.org/36785>.
  (unless (or (getenv "SUDO_USER")
              (not (file-exists? %user-profile-directory))
              (string=? %profile-directory
                        (dirname
                         (canonicalize-profile %user-profile-directory))))
    (migrate-generations %user-profile-directory %profile-directory))

  ;; Make sure ~/.config/guix/current points to /var/guix/profiles/….
  (let ((link %user-profile-directory))
    (unless (equal? (false-if-exception (readlink link))
                    %current-profile)
      (catch 'system-error
        (lambda ()
          (false-if-exception (delete-file link))
          (mkdir-p (dirname link))
          (symlink %current-profile link))
        (lambda args
          (leave (G_ "while creating symlink '~a': ~a~%")
                 link (strerror (system-error-errno args))))))))


;;;
;;; Queries.
;;;

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
display long package lists that would fill the user's screen.

Return true when there is more package info to display."
  (define (pretty str column)
    (indented-string (fill-paragraph str (- (%text-width) 4)
                                     column)
                     4 #:initial-indent? #f))

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

    (and concise?
         (or (> new-count concise/max-item-count)
             (> upgraded-count concise/max-item-count)))))

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
               (display-channel-news (generation-file-name profile second)
                                     (generation-file-name profile first))
               (loop (cons second rest)))
              ((_) #t)
              (()  #t))))))

     (leave-on-EPIPE
      (cond ((not (file-exists? profile))         ; XXX: race condition
             (raise (condition (&profile-not-found-error
                                (profile profile)))))
            ((not pattern)
             (list-generations profile (profile-generations profile)))
            ((matching-generations pattern profile)
             =>
             (match-lambda
               (()
                (exit 1))
               ((numbers ...)
                (list-generations profile numbers)))))))
    (('display-news)
     (display-news profile))))

(define (process-generation-change opts profile)
  "Process a request to change the current generation (roll-back, switch, delete)."
  (unless (assoc-ref opts 'dry-run?)
    (match (assoc-ref opts 'generation)
      (('roll-back)
       (with-store store
         (roll-back* store profile)))
      (('switch pattern)
       (let ((number (relative-generation-spec->number profile pattern)))
         (if number
             (switch-to-generation* profile number)
             (leave (G_ "cannot switch to generation '~a'~%") pattern))))
      (('delete pattern)
       (with-store store
         (delete-matching-generations store profile pattern))))))

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

  (define global-file
    (string-append %sysconfdir "/guix/channels.scm"))

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
          ((file-exists? global-file)
           (load-channels global-file))
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


(define-command (guix-pull . args)
  (synopsis "pull the latest revision of Guix")

  (define (no-arguments arg _‌)
    (leave (G_ "~A: extraneous argument~%") arg))

  (with-error-handling
    (with-git-error-handling
     (let* ((opts         (parse-command-line args %options
                                              (list %default-options)
                                              #:argument-handler no-arguments))
            (substitutes? (assoc-ref opts 'substitutes?))
            (dry-run?     (assoc-ref opts 'dry-run?))
            (profile      (or (assoc-ref opts 'profile) %current-profile))
            (current-channels (profile-channels profile))
            (validate-pull    (assoc-ref opts 'validate-pull))
            (authenticate?    (assoc-ref opts 'authenticate-channels?)))
       (cond
        ((assoc-ref opts 'query)
         (process-query opts profile))
        ((assoc-ref opts 'generation)
         (process-generation-change opts profile))
        (else
         (with-store store
           (with-status-verbosity (assoc-ref opts 'verbosity)
             (parameterize ((%current-system (assoc-ref opts 'system))
                            (%graft? (assoc-ref opts 'graft?)))
               (with-build-handler (build-notifier #:use-substitutes?
                                                   substitutes?
                                                   #:verbosity
                                                   (assoc-ref opts 'verbosity)
                                                   #:dry-run? dry-run?)
                 (set-build-options-from-command-line store opts)
                 (ensure-default-profile)
                 (honor-x509-certificates store)

                 (let* ((channels (channel-list opts))
                        (instances
                         (latest-channel-instances store channels
                                                   #:current-channels
                                                   current-channels
                                                   #:validate-pull
                                                   validate-pull
                                                   #:authenticate?
                                                   authenticate?)))
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
                                        (default-guile)))))
                     (with-profile-lock profile
                       (run-with-store store
                         (build-and-install instances profile)))))))))))))))

;;; pull.scm ends here
