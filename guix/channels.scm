;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
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

(define-module (guix channels)
  #:use-module (git)
  #:use-module (guix git)
  #:use-module (guix git-authenticate)
  #:use-module ((guix openpgp)
                #:select (openpgp-public-key-fingerprint
                          openpgp-format-fingerprint))
  #:use-module (guix base16)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix discovery)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix packages)
  #:use-module (guix progress)
  #:use-module (guix derivations)
  #:use-module (guix combinators)
  #:use-module (guix diagnostics)
  #:use-module (guix sets)
  #:use-module (guix store)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:autoload   (guix describe) (current-channels) ;XXX: circular dep
  #:autoload   (guix self) (whole-package make-config.scm)
  #:autoload   (guix inferior) (gexp->derivation-in-inferior) ;FIXME: circular dep
  #:autoload   (guix quirks) (%quirks %patches applicable-patch? apply-patch)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module ((ice-9 rdelim) #:select (read-string))
  #:use-module ((rnrs bytevectors) #:select (bytevector=?))
  #:export (channel
            channel?
            channel-name
            channel-url
            channel-branch
            channel-commit
            channel-introduction
            channel-location

            channel-introduction?
            make-channel-introduction
            channel-introduction-first-signed-commit
            channel-introduction-first-commit-signer

            openpgp-fingerprint->bytevector
            openpgp-fingerprint

            %default-guix-channel
            %default-channels
            guix-channel?

            channel-instance?
            channel-instance-channel
            channel-instance-commit
            channel-instance-checkout

            authenticate-channel
            latest-channel-instances
            checkout->channel-instance
            latest-channel-derivation
            channel-instance->sexp
            channel-instances->manifest
            %channel-profile-hooks
            channel-instances->derivation
            ensure-forward-channel-update

            profile-channels
            manifest-entry-channel
            sexp->channel
            channel->code

            channel-news-entry?
            channel-news-entry-commit
            channel-news-entry-tag
            channel-news-entry-title
            channel-news-entry-body

            channel-news-for-commit))

;;; Commentary:
;;;
;;; This module implements "channels."  A channel is usually a source of
;;; package definitions.  There's a special channel, the 'guix' channel, that
;;; provides all of Guix, including its commands and its documentation.
;;; User-defined channels are expected to typically provide a bunch of .scm
;;; files meant to be added to the '%package-search-path'.
;;;
;;; This module provides tools to fetch and update channels from a Git
;;; repository and to build them.
;;;
;;; Code:

(define-record-type* <channel> channel make-channel
  channel?
  (name      channel-name)
  (url       channel-url)
  (branch    channel-branch (default "master"))
  (commit    channel-commit (default #f))
  (introduction channel-introduction (default #f))
  (location  channel-location
             (default (current-source-location)) (innate)))

;; Channel introductions.  A "channel introduction" provides a commit/signer
;; pair that specifies the first commit of the authentication process as well
;; as its signer's fingerprint.  Introductions are used to bootstrap trust in
;; a channel.
(define-record-type <channel-introduction>
  (%make-channel-introduction first-signed-commit first-commit-signer)
  channel-introduction?
  (first-signed-commit  channel-introduction-first-signed-commit)  ;hex string
  (first-commit-signer  channel-introduction-first-commit-signer)) ;bytevector

(define (make-channel-introduction commit signer)
  "Return a new channel introduction: COMMIT is the introductory where
authentication starts, and SIGNER is the OpenPGP fingerprint (a bytevector) of
the signer of that commit."
  (%make-channel-introduction commit signer))

(define (openpgp-fingerprint->bytevector str)
  "Convert STR, an OpenPGP fingerprint (hexadecimal string with whitespace),
to the corresponding bytevector."
  (base16-string->bytevector
   (string-downcase (string-filter char-set:hex-digit str))))

(define-syntax openpgp-fingerprint
  (lambda (s)
    "Convert STR, an OpenPGP fingerprint (hexadecimal string with whitespace),
to the corresponding bytevector."
    (syntax-case s ()
      ((_ str)
       (string? (syntax->datum #'str))
       (openpgp-fingerprint->bytevector (syntax->datum #'str)))
      ((_ str)
       #'(openpgp-fingerprint->bytevector str)))))

(define %guix-channel-introduction
  ;; Introduction of the official 'guix channel.  The chosen commit is the
  ;; first one that introduces '.guix-authorizations' on the 'staging'
  ;; branch that was eventually merged in 'master'.  Any branch starting
  ;; before that commit cannot be merged or it will be rejected by 'guix pull'
  ;; & co.
  (make-channel-introduction
   "9edb3f66fd807b096b48283debdcddccfea34bad"     ;2020-05-26
   (openpgp-fingerprint                           ;mbakke
    "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))

(define %default-channel-url
  ;; URL of the default 'guix' channel.
  "https://git.savannah.gnu.org/git/guix.git")

(define %default-guix-channel
  (channel
   (name 'guix)
   (branch "master")
   (url %default-channel-url)
   (introduction %guix-channel-introduction)))

(define %default-channels
  ;; Default list of channels.
  (list %default-guix-channel))

(define (guix-channel? channel)
  "Return true if CHANNEL is the 'guix' channel."
  (eq? 'guix (channel-name channel)))

(define (ensure-default-introduction chan)
  "If CHAN represents the \"official\" 'guix' channel and lacks an
introduction, add it."
  (if (and (guix-channel? chan)
           (not (channel-introduction chan))
           (string=? (channel-url chan) %default-channel-url))
      (channel (inherit chan)
               (introduction %guix-channel-introduction))
      chan))

(define-record-type <channel-instance>
  (channel-instance channel commit checkout)
  channel-instance?
  (channel   channel-instance-channel)
  (commit    channel-instance-commit)
  (checkout  channel-instance-checkout))

(define-record-type <channel-metadata>
  (channel-metadata directory dependencies news-file keyring-reference url)
  channel-metadata?
  (directory     channel-metadata-directory)      ;string with leading slash
  (dependencies  channel-metadata-dependencies)   ;list of <channel>
  (news-file     channel-metadata-news-file)      ;string | #f
  (keyring-reference channel-metadata-keyring-reference) ;string
  (url           channel-metadata-url))           ;string | #f

(define %default-keyring-reference
  ;; Default value of the 'keyring-reference' field.
  "keyring")

(define (channel-reference channel)
  "Return the \"reference\" for CHANNEL, an sexp suitable for
'latest-repository-commit'."
  (match (channel-commit channel)
    (#f      `(branch . ,(channel-branch channel)))
    (commit  `(commit . ,(channel-commit channel)))))

(define sexp->channel-introduction
  (match-lambda
    (('channel-introduction ('version 0)
                            ('commit commit) ('signer signer)
                            _ ...)
     (make-channel-introduction commit (openpgp-fingerprint signer)))
    (x #f)))

(define (read-channel-metadata port)
  "Read from PORT channel metadata in the format expected for the
'.guix-channel' file.  Return a <channel-metadata> record, or raise an error
if valid metadata could not be read from PORT."
  (match (read port)
    (('channel ('version 0) properties ...)
     (let ((directory    (and=> (assoc-ref properties 'directory) first))
           (dependencies (or (assoc-ref properties 'dependencies) '()))
           (news-file    (and=> (assoc-ref properties 'news-file) first))
           (url          (and=> (assoc-ref properties 'url) first))
           (keyring-reference
            (or (and=> (assoc-ref properties 'keyring-reference) first)
                %default-keyring-reference)))
       (channel-metadata
        (cond ((not directory) "/")               ;directory
              ((string-prefix? "/" directory) directory)
              (else (string-append "/" directory)))
        (map (lambda (item)                       ;dependencies
               (let ((get (lambda* (key #:optional default)
                            (or (and=> (assoc-ref item key) first) default))))
                 (and-let* ((name (get 'name))
                            (url (get 'url))
                            (branch (get 'branch "master")))
                   (channel
                    (name name)
                    (branch branch)
                    (url url)
                    (commit (get 'commit))
                    (introduction (and=> (get 'introduction)
                                         sexp->channel-introduction))))))
             dependencies)
        news-file
        keyring-reference
        url)))
    ((and ('channel ('version version) _ ...) sexp)
     (raise (condition
             (&message (message "unsupported '.guix-channel' version"))
             (&error-location
              (location (source-properties->location
                         (source-properties sexp)))))))
    (sexp
     (raise (condition
             (&message (message "invalid '.guix-channel' file"))
             (&error-location
              (location (source-properties->location
                         (source-properties sexp)))))))))

(define (read-channel-metadata-from-source source)
  "Return a channel-metadata record read from channel's SOURCE/.guix-channel
description file, or return the default channel-metadata record if that file
doesn't exist."
  (catch 'system-error
    (lambda ()
      (call-with-input-file (string-append source "/.guix-channel")
        read-channel-metadata))
    (lambda args
      (if (= ENOENT (system-error-errno args))
          (channel-metadata "/" '() #f %default-keyring-reference #f)
          (apply throw args)))))

(define (channel-instance-metadata instance)
  "Return a channel-metadata record read from the channel INSTANCE's
description file or its default value."
  (read-channel-metadata-from-source (channel-instance-checkout instance)))

(define (channel-instance-dependencies instance)
  "Return the list of channels that are declared as dependencies for the given
channel INSTANCE."
  (channel-metadata-dependencies (channel-instance-metadata instance)))

(define (apply-patches checkout commit patches)
  "Apply the matching PATCHES to CHECKOUT, modifying files in place.  The
result is unspecified."
  (let loop ((patches patches))
    (match patches
      (() #t)
      ((patch rest ...)
       (when (applicable-patch? patch checkout commit)
         (apply-patch patch checkout))
       (loop rest)))))

(define commit-short-id
  (compose (cut string-take <> 7) oid->string commit-id))

(define* (authenticate-channel channel checkout commit
                               #:key (keyring-reference-prefix "origin/"))
  "Authenticate the given COMMIT of CHANNEL, available at CHECKOUT, a
directory containing a CHANNEL checkout.  Raise an error if authentication
fails."
  (define intro
    (channel-introduction channel))

  (define cache-key
    (string-append "channels/" (symbol->string (channel-name channel))))

  (define keyring-reference
    (channel-metadata-keyring-reference
     (read-channel-metadata-from-source checkout)))

  (define (make-reporter start-commit end-commit commits)
    (format (current-error-port)
            (G_ "Authenticating channel '~a', commits ~a to ~a (~h new \
commits)...~%")
            (channel-name channel)
            (commit-short-id start-commit)
            (commit-short-id end-commit)
            (length commits))

    (progress-reporter/bar (length commits)))

  (define authentic-commits
    ;; Consider the currently-used commit of CHANNEL as authentic so
    ;; authentication can skip it and all its closure.
    (match (find (lambda (candidate)
                   (eq? (channel-name candidate) (channel-name channel)))
                 (current-channels))
      (#f '())
      (channel
       (if (channel-commit channel)
           (list (channel-commit channel))
           '()))))

  ;; XXX: Too bad we need to re-open CHECKOUT.
  (with-repository checkout repository
    (authenticate-repository repository
                             (string->oid
                              (channel-introduction-first-signed-commit intro))
                             (channel-introduction-first-commit-signer intro)
                             #:end (string->oid commit)
                             #:keyring-reference
                             (string-append keyring-reference-prefix
                                            keyring-reference)
                             #:authentic-commits authentic-commits
                             #:make-reporter make-reporter
                             #:cache-key cache-key)))

(define* (latest-channel-instance store channel
                                  #:key (patches %patches)
                                  starting-commit
                                  (authenticate? #f)
                                  (validate-pull
                                   ensure-forward-channel-update))
  "Return the latest channel instance for CHANNEL.  When STARTING-COMMIT is
true, call VALIDATE-PULL with CHANNEL, STARTING-COMMIT, the target commit, and
their relation.  When AUTHENTICATE? is false, CHANNEL is not authenticated."
  (define (dot-git? file stat)
    (and (string=? (basename file) ".git")
         (eq? 'directory (stat:type stat))))

  (let-values (((channel)
                (ensure-default-introduction channel))
               ((checkout commit relation)
                (update-cached-checkout (channel-url channel)
                                        #:ref (channel-reference channel)
                                        #:starting-commit starting-commit)))
    (when relation
      (validate-pull channel starting-commit commit relation))

    (if authenticate?
        (if (channel-introduction channel)
            (authenticate-channel channel checkout commit)
            ;; TODO: Warn for all the channels once the authentication interface
            ;; is public.
            (when (guix-channel? channel)
              (raise (make-compound-condition
                      (formatted-message (G_ "channel '~a' lacks an \
introduction and cannot be authenticated~%")
                                         (channel-name channel))
                      (condition
                       (&fix-hint
                        (hint (G_ "Add the missing introduction to your
channels file to address the issue.  Alternatively, you can pass
@option{--disable-authentication}, at the risk of running unauthenticated and
thus potentially malicious code."))))))))
        (warning (G_ "channel authentication disabled~%")))

    (when (guix-channel? channel)
      ;; Apply the relevant subset of PATCHES directly in CHECKOUT.  This is
      ;; safe to do because 'switch-to-ref' eventually does a hard reset.
      (apply-patches checkout commit patches))

    (let* ((name     (url+commit->name (channel-url channel) commit))
           (checkout (add-to-store store name #t "sha256" checkout
                                   #:select? (negate dot-git?))))
      (channel-instance channel commit checkout))))

(define (ensure-forward-channel-update channel start commit relation)
  "Raise an error if RELATION is not 'ancestor, meaning that START is not an
ancestor of COMMIT, unless CHANNEL specifies a commit.

This procedure implements a channel update policy meant to be used as a
#:validate-pull argument."
  (match relation
    ('ancestor #t)
    ('self #t)
    (_
     (raise (make-compound-condition
             (condition
              (&message (message
                         (format #f (G_ "\
aborting update of channel '~a' to commit ~a, which is not a descendant of ~a")
                                 (channel-name channel)
                                 commit start))))

             ;; If the user asked for a specific commit, they might want
             ;; that to happen nevertheless, so tell them about the
             ;; relevant 'guix pull' option.
             (if (channel-commit channel)
                 (condition
                  (&fix-hint
                   (hint (G_ "Use @option{--allow-downgrades} to force
this downgrade."))))
                 (condition
                  (&fix-hint
                   (hint (G_ "This could indicate that the channel has
been tampered with and is trying to force a roll-back, preventing you from
getting the latest updates.  If you think this is not the case, explicitly
allow non-forward updates."))))))))))

(define (channel-instance-primary-url instance)
  "Return the primary URL advertised for INSTANCE, or #f if there is no such
information."
  (channel-metadata-url (channel-instance-metadata instance)))

(define* (latest-channel-instances store channels
                                   #:key
                                   (current-channels '())
                                   (authenticate? #t)
                                   (validate-pull
                                    ensure-forward-channel-update))
  "Return a list of channel instances corresponding to the latest checkouts of
CHANNELS and the channels on which they depend.

When AUTHENTICATE? is true, authenticate the subset of CHANNELS that has a
\"channel introduction\".

CURRENT-CHANNELS is the list of currently used channels.  It is compared
against the newly-fetched instances of CHANNELS, and VALIDATE-PULL is called
for each channel update and can choose to emit warnings or raise an error,
depending on the policy it implements."
  ;; Only process channels that are unique, or that are more specific than a
  ;; previous channel specification.
  (define (ignore? channel others)
    (member channel others
            (lambda (a b)
              (and (eq? (channel-name a) (channel-name b))
                   (or (channel-commit b)
                       (not (or (channel-commit a)
                                (channel-commit b))))))))

  (define (current-commit name)
    ;; Return the current commit for channel NAME.
    (any (lambda (channel)
           (and (eq? (channel-name channel) name)
                (channel-commit channel)))
         current-channels))

  (let loop ((channels channels)
             (previous-channels '()))
    ;; Accumulate a list of instances.  A list of processed channels is also
    ;; accumulated to decide on duplicate channel specifications.
    (define-values (resulting-channels instances)
      (fold2 (lambda (channel previous-channels instances)
               (if (ignore? channel previous-channels)
                   (values previous-channels instances)
                   (begin
                     (format (current-error-port)
                             (G_ "Updating channel '~a' from Git repository at '~a'...~%")
                             (channel-name channel)
                             (channel-url channel))
                     (let* ((current (current-commit (channel-name channel)))
                            (instance
                             (latest-channel-instance store channel
                                                      #:authenticate?
                                                      authenticate?
                                                      #:validate-pull
                                                      validate-pull
                                                      #:starting-commit
                                                      current)))
                       (when authenticate?
                         ;; CHANNEL is authenticated so we can trust the
                         ;; primary URL advertised in its metadata and warn
                         ;; about possibly stale mirrors.
                         (let ((primary-url (channel-instance-primary-url
                                             instance)))
                           (unless (or (not primary-url)
                                       (channel-commit channel)
                                       (string=? primary-url (channel-url channel)))
                             (warning (G_ "pulled channel '~a' from a mirror \
of ~a, which might be stale~%")
                                      (channel-name channel)
                                      primary-url))))

                       (let-values (((new-instances new-channels)
                                     (loop (channel-instance-dependencies instance)
                                           previous-channels)))
                         (values (append (cons channel new-channels)
                                         previous-channels)
                                 (append (cons instance new-instances)
                                         instances)))))))
             previous-channels
             '()                                  ;instances
             channels))

    (let ((instance-name (compose channel-name channel-instance-channel)))
      ;; Remove all earlier channel specifications if they are followed by a
      ;; more specific one.
      (values (delete-duplicates instances
                                 (lambda (a b)
                                   (eq? (instance-name a) (instance-name b))))
              resulting-channels))))

(define* (checkout->channel-instance checkout
                                     #:key commit
                                     (url checkout) (name 'guix))
  "Return a channel instance for CHECKOUT, which is assumed to be a checkout
of COMMIT at URL.  Use NAME as the channel name."
  (let* ((commit  (or commit (make-string 40 #\0)))
         (channel (channel (name name)
                           (commit commit)
                           (url url))))
    (channel-instance channel commit checkout)))

(define %self-build-file
  ;; The file containing code to build Guix.  This serves the same purpose as
  ;; a makefile, and, similarly, is intended to always keep this name.
  "build-aux/build-self.scm")

(define %pull-version
  ;; This is the version of the 'guix pull' protocol.  It specifies what's
  ;; expected from %SELF-BUILD-FILE.  The initial version ("0") was when we'd
  ;; place a set of compiled Guile modules in ~/.config/guix/latest.
  1)

(define (standard-module-derivation name source core dependencies)
  "Return a derivation that builds with CORE, a Guix instance, the Scheme
modules in SOURCE and that depend on DEPENDENCIES, a list of lowerable
objects.  The assumption is that SOURCE contains package modules to be added
to '%package-module-path'."

  (let* ((metadata (read-channel-metadata-from-source source))
         (directory (channel-metadata-directory metadata)))

    (define build
      ;; This is code that we'll run in CORE, a Guix instance, with its own
      ;; modules and so on.  That way, we make sure these modules are built for
      ;; the right Guile version, with the right dependencies, and that they get
      ;; to see the right (gnu packages …) modules.
      (with-extensions dependencies
        #~(begin
            (use-modules (guix build compile)
                         (guix build utils)
                         (srfi srfi-26))

            (define go
              (string-append #$output "/lib/guile/" (effective-version)
                             "/site-ccache"))
            (define scm
              (string-append #$output "/share/guile/site/"
                             (effective-version)))

            (define optimizations-for-level
              ;; Guile 3.0 provides this procedure but Guile 2.2 didn't.
              ;; Since this code may be executed by either version, we can't
              ;; rely on its availability.
              (or (and=> (false-if-exception
                          (resolve-interface '(system base optimize)))
                         (lambda (iface)
                           (module-ref iface 'optimizations-for-level)))
                  (const '())))

            (define -O1
              ;; Optimize for package module compilation speed.
              (optimizations-for-level 1))

            (let* ((subdir #$directory)
                   (source (string-append #$source subdir)))
              (compile-files source go (find-files source "\\.scm$")
                             #:optimization-options (const -O1))
              (mkdir-p (dirname scm))
              (symlink (string-append #$source subdir) scm))

            scm)))

    (gexp->derivation-in-inferior name build core)))

(define* (guile-for-source source #:optional (quirks %quirks))
  "Return the Guile package to use when building SOURCE or #f if the default
'%guile-for-build' should be good enough."
  (let loop ((quirks quirks))
    (match quirks
      (()
       #f)
      (((predicate . guile) rest ...)
       (if (predicate source) (guile) (loop rest))))))

(define (call-with-guile guile thunk)
  (lambda (store)
    (values (parameterize ((%guile-for-build
                            (if guile
                                (package-derivation store guile)
                                (%guile-for-build))))
              (run-with-store store (thunk)))
            store)))

(define-syntax-rule (with-guile guile exp ...)
  "Set GUILE as the '%guile-for-build' parameter for the dynamic extent of
EXP, a series of monadic expressions."
  (call-with-guile guile (lambda ()
                           (mbegin %store-monad exp ...))))

(define (with-trivial-build-handler mvalue)
  "Run MVALUE, a monadic value, with a \"trivial\" build handler installed
that unconditionally resumes the continuation."
  (lambda (store)
    (with-build-handler (lambda (continue . _)
                          (continue #t))
      (values (run-with-store store mvalue)
              store))))

(define* (build-from-source instance
                            #:key core verbose? (dependencies '()) system)
  "Return a derivation to build Guix from INSTANCE, using the self-build
script contained therein.  When CORE is true, build package modules under
SOURCE using CORE, an instance of Guix.  By default, build for the current
system, or SYSTEM if specified."
  (define name
    (symbol->string
     (channel-name (channel-instance-channel instance))))
  (define source
    (channel-instance-checkout instance))
  (define commit
    (channel-instance-commit instance))

  ;; Running the self-build script makes it easier to update the build
  ;; procedure: the self-build script of the Guix-to-be-installed contains the
  ;; right dependencies, build procedure, etc., which the Guix-in-use may not
  ;; know.
  (define script
    (string-append source "/" %self-build-file))

  (if (file-exists? script)
      (let ((build (save-module-excursion
                    (lambda ()
                      ;; Disable deprecation warnings; it's OK for SCRIPT to
                      ;; use deprecated APIs and the user doesn't have to know
                      ;; about it.
                      (parameterize ((guix-warning-port
                                      (%make-void-port "w")))
                        (primitive-load script)))))
            (guile (guile-for-source source)))
        ;; BUILD must be a monadic procedure of at least one argument: the
        ;; source tree.
        ;;
        ;; Note: BUILD can return #f if it does not support %PULL-VERSION.  In
        ;; the future we'll fall back to a previous version of the protocol
        ;; when that happens.
        (with-guile guile
          ;; BUILD is usually quite costly.  Install a "trivial" build handler
          ;; so we don't bounce an outer build-accumulator handler that could
          ;; cause us to redo half of the BUILD computation several times just
          ;; to realize it gives the same result.
          (with-trivial-build-handler
           (build source
                  #:verbose? verbose? #:version commit
                  #:system system
                  #:channel-metadata (channel-instance->sexp instance)
                  #:pull-version %pull-version))))

      ;; Build a set of modules that extend Guix using the standard method.
      (standard-module-derivation name source core dependencies)))

(define* (build-channel-instance instance system
                                 #:optional core (dependencies '()))
  "Return, as a monadic value, the derivation for INSTANCE, a channel
instance, for SYSTEM.  DEPENDENCIES is a list of extensions providing Guile
modules that INSTANCE depends on."
  (build-from-source instance
                     #:core core
                     #:dependencies dependencies
                     #:system system))

(define (resolve-dependencies instances)
  "Return a procedure that, given one of the elements of INSTANCES, returns
list of instances it depends on."
  (define channel-instance-name
    (compose channel-name channel-instance-channel))

  (define table                                   ;map a name to an instance
    (fold (lambda (instance table)
            (vhash-consq (channel-instance-name instance)
                         instance table))
          vlist-null
          instances))

  (define edges
    (fold (lambda (instance edges)
            (fold (lambda (channel edges)
                    (let ((name (channel-name channel)))
                      (match (vhash-assq name table)
                        ((_ . target)
                         (vhash-consq instance target edges)))))
                  edges
                  (channel-instance-dependencies instance)))
          vlist-null
          instances))

  (lambda (instance)
    (vhash-foldq* cons '() instance edges)))

(define* (channel-instance-derivations instances #:key system)
  "Return the list of derivations to build INSTANCES, in the same order as
INSTANCES.  Build for the current system by default, or SYSTEM if specified."
  (define core-instance
    ;; The 'guix' channel is treated specially: it's an implicit dependency of
    ;; all the other channels.
    (find (lambda (instance)
            (guix-channel? (channel-instance-channel instance)))
          instances))

  (define edges
    (resolve-dependencies instances))

  (define (instance->derivation instance)
    (mlet %store-monad ((system (if system (return system) (current-system))))
      (mcached (if (eq? instance core-instance)
                   (build-channel-instance instance system)
                   (mlet %store-monad ((core (instance->derivation core-instance))
                                       (deps (mapm %store-monad instance->derivation
                                                   (edges instance))))
                     (build-channel-instance instance system core deps)))
               instance
               system)))

  (unless core-instance
    (let ((loc (and=> (any (compose channel-location channel-instance-channel)
                           instances)
                      source-properties->location)))
      (raise (apply make-compound-condition
                    (condition
                     (&message (message "'guix' channel is lacking")))
                    (condition
                     (&fix-hint (hint (G_ "Make sure your list of channels
contains one channel named @code{guix} providing the core of Guix."))))
                    (if loc
                        (list (condition (&error-location (location loc))))
                        '())))))

  (mapm %store-monad instance->derivation instances))

(define (whole-package-for-legacy name modules)
  "Return a full-blown Guix package for MODULES, a derivation that builds Guix
modules in the old ~/.config/guix/latest style."
  (define packages
    (resolve-interface '(gnu packages guile)))

  (define modules+compiled
    ;; Since MODULES contains both .scm and .go files at its root, re-bundle
    ;; it so that it has share/guile/site and lib/guile, which is what
    ;; 'whole-package' expects.
    (computed-file (derivation-name modules)
                   (with-imported-modules '((guix build utils))
                     #~(begin
                         (use-modules (guix build utils))

                         (define version
                           (effective-version))
                         (define share
                           (string-append #$output "/share/guile/site"))
                         (define lib
                           (string-append #$output "/lib/guile/" version))

                         (mkdir-p share) (mkdir-p lib)
                         (symlink #$modules (string-append share "/" version))
                         (symlink #$modules (string-append lib "/site-ccache"))))))

  (letrec-syntax ((list (syntax-rules (->)
                          ((_)
                           '())
                          ((_ (module -> variable) rest ...)
                           (cons (module-ref (resolve-interface
                                              '(gnu packages module))
                                             'variable)
                                 (list rest ...)))
                          ((_ variable rest ...)
                           (cons (module-ref packages 'variable)
                                 (list rest ...))))))
    (whole-package name modules+compiled

                   ;; In the "old style", %SELF-BUILD-FILE would simply return a
                   ;; derivation that builds modules.  We have to infer what the
                   ;; dependencies of these modules were.
                   (list guile-json-3 guile-git guile-bytestructures
                         (ssh -> guile-ssh) (tls -> gnutls))
                   #:guile (default-guile))))

(define (old-style-guix? drv)
  "Return true if DRV corresponds to a ~/.config/guix/latest style of
derivation."
  ;; Here we rely on a gross historical fact: that derivations produced by the
  ;; "old style" (before commit 8a0d9bc8a3f153159d9e239a151c0fa98f1e12d8,
  ;; dated May 30, 2018) did not depend on "guix-command.drv".
  (not (find (lambda (input)
               (string=? "guix-command"
                         (derivation-name
                          (derivation-input-derivation input))))
             (derivation-inputs drv))))

(define (channel-instance->sexp instance)
  "Return an sexp representation of INSTANCE, a channel instance."
  (let* ((commit  (channel-instance-commit instance))
         (channel (channel-instance-channel instance))
         (intro   (channel-introduction channel)))
    `(repository
      (version 0)
      (url ,(channel-url channel))
      (branch ,(channel-branch channel))
      (commit ,commit)
      (name ,(channel-name channel))
      ,@(if intro
            `((introduction
               (channel-introduction
                (version 0)
                (commit
                 ,(channel-introduction-first-signed-commit
                   intro))
                (signer
                 ,(openpgp-format-fingerprint
                   (channel-introduction-first-commit-signer
                    intro))))))
            '()))))

(define* (channel-instances->manifest instances #:key system)
  "Return a profile manifest with entries for all of INSTANCES, a list of
channel instances.  By default, build for the current system, or SYSTEM if
specified."
  (define (instance->entry instance drv)
    (let ((commit  (channel-instance-commit instance))
          (channel (channel-instance-channel instance)))
      (manifest-entry
        (name (symbol->string (channel-name channel)))
        (version (string-take commit 7))
        (item (if (guix-channel? channel)
                  (if (old-style-guix? drv)
                      (whole-package-for-legacy (string-append name "-" version)
                                                drv)
                      drv)
                  drv))
        (properties
         `((source ,(channel-instance->sexp instance)))))))

  (mlet* %store-monad ((derivations (channel-instance-derivations instances
                                                                  #:system system))
                       (entries ->  (map instance->entry instances derivations)))
    (return (manifest entries))))

(define (package-cache-file manifest)
  "Build a package cache file for the instance in MANIFEST.  This is meant to
be used as a profile hook."
  (let ((profile (profile (content manifest) (hooks '()))))
    (define build
      #~(begin
          (use-modules (gnu packages))

          (if (defined? 'generate-package-cache)
              (begin
                ;; Delegate package cache generation to the inferior.
                (format (current-error-port)
                        "Generating package cache for '~a'...~%"
                        #$profile)
                (generate-package-cache #$output))
              (mkdir #$output))))

    (gexp->derivation-in-inferior "guix-package-cache" build
                                  profile

                                  ;; If the Guix in PROFILE is too old and
                                  ;; lacks 'guix repl', don't build the cache
                                  ;; instead of failing.
                                  #:silent-failure? #t

                                  #:properties '((type . profile-hook)
                                                 (hook . package-cache))
                                  #:local-build? #t)))

(define %channel-profile-hooks
  ;; The default channel profile hooks.
  (cons package-cache-file %default-profile-hooks))

(define (channel-instances->derivation instances)
  "Return the derivation of the profile containing INSTANCES, a list of
channel instances."
  (mlet %store-monad ((manifest (channel-instances->manifest instances)))
    (profile-derivation manifest
                        #:hooks %channel-profile-hooks)))

(define latest-channel-instances*
  (store-lift latest-channel-instances))

(define* (latest-channel-derivation #:optional (channels %default-channels)
                                    #:key
                                    (current-channels '())
                                    (validate-pull
                                     ensure-forward-channel-update))
  "Return as a monadic value the derivation that builds the profile for the
latest instances of CHANNELS.  CURRENT-CHANNELS and VALIDATE-PULL are passed
to 'latest-channel-instances'."
  (mlet %store-monad ((instances
                       (latest-channel-instances* channels
                                                  #:current-channels
                                                  current-channels
                                                  #:validate-pull
                                                  validate-pull)))
    (channel-instances->derivation instances)))

(define* (sexp->channel sexp #:optional (name 'channel))
  "Read SEXP, a provenance sexp as created by 'channel-instance->sexp'; use
NAME as the channel name if SEXP does not specify it.  Return #f if the sexp
does not have the expected structure."
  (match sexp
    (('repository ('version 0)
                  ('url url)
                  ('branch branch)
                  ('commit commit)
                  rest ...)
     ;; Historically channel sexps did not include the channel name.  It's OK
     ;; for channels created by 'channel-instances->manifest' because the
     ;; entry name is the channel name, but it was missing for entries created
     ;; by 'manifest-entry-with-provenance'.
     (channel (name (match (assq 'name rest)
                      (#f name)
                      (('name name) name)))
              (url url)
              (branch branch)
              (commit commit)
              (introduction
               (match (assq 'introduction rest)
                 (#f #f)
                 (('introduction intro)
                  (sexp->channel-introduction intro))))))

    (_ #f)))

(define (manifest-entry-channel entry)
  "Return the channel ENTRY corresponds to, or #f if that information is
missing or unreadable.  ENTRY must be an entry created by
'channel-instances->manifest', with the 'source' property."
  (let ((name (string->symbol (manifest-entry-name entry))))
    (match (assq-ref (manifest-entry-properties entry) 'source)
      ((sexp)
       (sexp->channel sexp name))
      (_
       ;; No channel information for this manifest entry.
       ;; XXX: Pre-0.15.0 Guix did not provide that information,
       ;; but there's not much we can do in that case.
       #f))))

(define (profile-channels profile)
  "Return the list of channels corresponding to entries in PROFILE.  If
PROFILE is not a profile created by 'guix pull', return the empty list."
  (filter-map manifest-entry-channel
              ;; Show most recently installed packages last.
              (reverse
               (manifest-entries (profile-manifest profile)))))

(define* (channel->code channel #:key (include-introduction? #t))
  "Return code (an sexp) to build CHANNEL.  When INCLUDE-INTRODUCTION? is
true, include its introduction, if any."
  (let ((intro (and include-introduction?
                    (channel-introduction channel))))
    `(channel
      (name ',(channel-name channel))
      (url ,(channel-url channel))
      (branch ,(channel-branch channel))
      (commit ,(channel-commit channel))
      ,@(if intro
            `((introduction (make-channel-introduction
                             ,(channel-introduction-first-signed-commit intro)
                             (openpgp-fingerprint
                              ,(openpgp-format-fingerprint
                                (channel-introduction-first-commit-signer
                                 intro))))))
            '()))))


;;;
;;; News.
;;;

;; Channel news.
(define-record-type <channel-news>
  (channel-news entries)
  channel-news?
  (entries channel-news-entries))                 ;list of <channel-news-entry>

;; News entry, associated with a specific commit of the channel.
(define-record-type <channel-news-entry>
  (channel-news-entry commit tag title body)
  channel-news-entry?
  (commit  channel-news-entry-commit)             ;hex string | #f
  (tag     channel-news-entry-tag)                ;#f | string
  (title   channel-news-entry-title)              ;list of language tag/string pairs
  (body    channel-news-entry-body))              ;list of language tag/string pairs

(define (sexp->channel-news-entry entry)
  "Return the <channel-news-entry> record corresponding to ENTRY, an sexp."
  (define (pair language message)
    (cons (symbol->string language) message))

  (match entry
    (('entry ((and (or 'commit 'tag) type) commit-or-tag)
             ('title ((? symbol? title-tags) (? string? titles)) ...)
             ('body ((? symbol? body-tags) (? string? bodies)) ...)
             _ ...)
     (channel-news-entry (and (eq? type 'commit) commit-or-tag)
                         (and (eq? type 'tag) commit-or-tag)
                         (map pair title-tags titles)
                         (map pair body-tags bodies)))
    (_
     (raise (condition
             (&message (message "invalid channel news entry"))
             (&error-location
              (location (source-properties->location
                         (source-properties entry)))))))))

(define (read-channel-news port)
  "Read a channel news feed from PORT and return it as a <channel-news>
record."
  (match (false-if-exception (read port))
    (('channel-news ('version 0) entries ...)
     (channel-news (map sexp->channel-news-entry entries)))
    (('channel-news ('version version) _ ...)
     ;; This is an unsupported version from the future.  There's nothing wrong
     ;; with that (the user may simply need to upgrade the 'guix' channel to
     ;; be able to read it), so silently ignore it.
     (channel-news '()))
    (#f
     (raise (condition
             (&message (message "syntactically invalid channel news file")))))
    (sexp
     (raise (condition
             (&message (message "invalid channel news file"))
             (&error-location
              (location (source-properties->location
                         (source-properties sexp)))))))))

(define (resolve-channel-news-entry-tag repository entry)
  "If ENTRY has its 'commit' field set, return ENTRY.  Otherwise, lookup
ENTRY's 'tag' in REPOSITORY and return ENTRY with its 'commit' field set to
the field its 'tag' refers to.  A 'git-error' exception is raised if the tag
cannot be found."
  (if (channel-news-entry-commit entry)
      entry
      (let* ((tag       (channel-news-entry-tag entry))
             (reference (reference-lookup repository
                                          (string-append "refs/tags/" tag)))
             (target    (reference-target reference))
             (oid       (let ((obj (object-lookup repository target)))
                          (if (= OBJ-TAG (object-type obj)) ;annotated tag?
                              (tag-target-id (tag-lookup repository target))
                              target))))
        (channel-news-entry (oid->string oid) tag
                            (channel-news-entry-title entry)
                            (channel-news-entry-body entry)))))

(define* (channel-news-for-commit channel new #:optional old)
  "Return a list of <channel-news-entry> for CHANNEL between commits OLD and
NEW.  When OLD is omitted or is #f, return all the news entries of CHANNEL."
  (catch 'git-error
    (lambda ()
      (let* ((checkout  (update-cached-checkout (channel-url channel)
                                                #:ref `(commit . ,new)))
             (metadata  (read-channel-metadata-from-source checkout))
             (news-file (channel-metadata-news-file metadata))
             (news-file (and news-file
                             (string-append checkout "/" news-file))))
        (if (and news-file (file-exists? news-file))
            (with-repository checkout repository
              (let* ((news    (call-with-input-file news-file
                                read-channel-news))
                     (entries (map (lambda (entry)
                                     (resolve-channel-news-entry-tag repository
                                                                     entry))
                                   (channel-news-entries news))))
                (if old
                    (let* ((new     (commit-lookup repository (string->oid new)))
                           (old     (commit-lookup repository (string->oid old)))
                           (commits (list->set
                                     (map (compose oid->string commit-id)
                                          (commit-difference new old)))))
                      (filter (lambda (entry)
                                (set-contains? commits
                                               (channel-news-entry-commit entry)))
                              entries))
                    entries)))
            '())))
    (lambda (key error . rest)
      ;; If commit NEW or commit OLD cannot be found, then something must be
      ;; wrong (for example, the history of CHANNEL was rewritten and these
      ;; commits no longer exist upstream), so quietly return the empty list.
      (if (= GIT_ENOTFOUND (git-error-code error))
          '()
          (apply throw key error rest)))))

;;; Local Variables:
;;; eval: (put 'with-guile 'scheme-indent-function 1)
;;; End:
