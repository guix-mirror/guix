;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix git)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix discovery)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix derivations)
  #:use-module (guix combinators)
  #:use-module (guix deprecation)
  #:use-module (guix store)
  #:use-module (guix i18n)
  #:use-module ((guix utils)
                #:select (source-properties->location
                          &error-location))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:autoload   (guix self) (whole-package make-config.scm)
  #:autoload   (guix inferior) (gexp->derivation-in-inferior) ;FIXME: circular dep
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (channel
            channel?
            channel-name
            channel-url
            channel-branch
            channel-commit
            channel-location

            %default-channels
            guix-channel?

            channel-instance?
            channel-instance-channel
            channel-instance-commit
            channel-instance-checkout

            latest-channel-instances
            checkout->channel-instance
            latest-channel-derivation
            channel-instances->manifest
            %channel-profile-hooks
            channel-instances->derivation))

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
  (location  channel-location
             (default (current-source-location)) (innate)))

(define %default-channels
  ;; Default list of channels.
  (list (channel
         (name 'guix)
         (branch "master")
         (url "https://git.savannah.gnu.org/git/guix.git"))))

(define (guix-channel? channel)
  "Return true if CHANNEL is the 'guix' channel."
  (eq? 'guix (channel-name channel)))

(define-record-type <channel-instance>
  (channel-instance channel commit checkout)
  channel-instance?
  (channel   channel-instance-channel)
  (commit    channel-instance-commit)
  (checkout  channel-instance-checkout))

(define-record-type <channel-metadata>
  (channel-metadata version dependencies)
  channel-metadata?
  (version       channel-metadata-version)
  (dependencies  channel-metadata-dependencies))

(define (channel-reference channel)
  "Return the \"reference\" for CHANNEL, an sexp suitable for
'latest-repository-commit'."
  (match (channel-commit channel)
    (#f      `(branch . ,(channel-branch channel)))
    (commit  `(commit . ,(channel-commit channel)))))

(define (read-channel-metadata instance)
  "Return a channel-metadata record read from the channel INSTANCE's
description file, or return #F if the channel instance does not include the
file."
  (let* ((source (channel-instance-checkout instance))
         (meta-file (string-append source "/.guix-channel")))
    (and (file-exists? meta-file)
         (and-let* ((raw (call-with-input-file meta-file read))
                    (version (and=> (assoc-ref raw 'version) first))
                    (dependencies (or (assoc-ref raw 'dependencies) '())))
           (channel-metadata
            version
            (map (lambda (item)
                   (let ((get (lambda* (key #:optional default)
                                (or (and=> (assoc-ref item key) first) default))))
                     (and-let* ((name (get 'name))
                                (url (get 'url))
                                (branch (get 'branch "master")))
                       (channel
                        (name name)
                        (branch branch)
                        (url url)
                        (commit (get 'commit))))))
                 dependencies))))))

(define (channel-instance-dependencies instance)
  "Return the list of channels that are declared as dependencies for the given
channel INSTANCE."
  (match (read-channel-metadata instance)
    (#f '())
    (($ <channel-metadata> version dependencies)
     dependencies)))

(define* (latest-channel-instances store channels #:optional (previous-channels '()))
  "Return a list of channel instances corresponding to the latest checkouts of
CHANNELS and the channels on which they depend.  PREVIOUS-CHANNELS is a list
of previously processed channels."
  ;; Only process channels that are unique, or that are more specific than a
  ;; previous channel specification.
  (define (ignore? channel others)
    (member channel others
            (lambda (a b)
              (and (eq? (channel-name a) (channel-name b))
                   (or (channel-commit b)
                       (not (or (channel-commit a)
                                (channel-commit b))))))))

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
                   (let-values (((checkout commit)
                                 (latest-repository-commit store (channel-url channel)
                                                           #:ref (channel-reference
                                                                  channel))))
                     (let ((instance (channel-instance channel commit checkout)))
                       (let-values (((new-instances new-channels)
                                     (latest-channel-instances
                                      store
                                      (channel-instance-dependencies instance)
                                      previous-channels)))
                         (values (append (cons channel new-channels)
                                         previous-channels)
                                 (append (cons instance new-instances)
                                         instances))))))))
           previous-channels
           '()                                    ;instances
           channels))

  (let ((instance-name (compose channel-name channel-instance-channel)))
    ;; Remove all earlier channel specifications if they are followed by a
    ;; more specific one.
    (values (delete-duplicates instances
                               (lambda (a b)
                                 (eq? (instance-name a) (instance-name b))))
            resulting-channels)))

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
  ;; FIXME: We should load, say SOURCE/.guix-channel.scm, which would allow
  ;; channel publishers to specify things such as the sub-directory where .scm
  ;; files live, files to exclude from the channel, preferred substitute URLs,
  ;; etc.

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

          (compile-files #$source go
                         (find-files #$source "\\.scm$"))
          (mkdir-p (dirname scm))
          (symlink #$source scm)
          scm)))

  (gexp->derivation-in-inferior name build core))

(define* (build-from-source name source
                            #:key core verbose? commit
                            (dependencies '()))
  "Return a derivation to build Guix from SOURCE, using the self-build script
contained therein; use COMMIT as the version string.  When CORE is true, build
package modules under SOURCE using CORE, an instance of Guix."
  ;; Running the self-build script makes it easier to update the build
  ;; procedure: the self-build script of the Guix-to-be-installed contains the
  ;; right dependencies, build procedure, etc., which the Guix-in-use may not
  ;; be know.
  (define script
    (string-append source "/" %self-build-file))

  (if (file-exists? script)
      (let ((build (save-module-excursion
                    (lambda ()
                      ;; Disable deprecation warnings; it's OK for SCRIPT to
                      ;; use deprecated APIs and the user doesn't have to know
                      ;; about it.
                      (parameterize ((deprecation-warning-port
                                      (%make-void-port "w")))
                        (primitive-load script))))))
        ;; BUILD must be a monadic procedure of at least one argument: the
        ;; source tree.
        ;;
        ;; Note: BUILD can return #f if it does not support %PULL-VERSION.  In
        ;; the future we'll fall back to a previous version of the protocol
        ;; when that happens.
        (build source #:verbose? verbose? #:version commit
               #:pull-version %pull-version))

      ;; Build a set of modules that extend Guix using the standard method.
      (standard-module-derivation name source core dependencies)))

(define* (build-channel-instance instance
                                 #:optional core (dependencies '()))
  "Return, as a monadic value, the derivation for INSTANCE, a channel
instance.  DEPENDENCIES is a list of extensions providing Guile modules that
INSTANCE depends on."
  (build-from-source (symbol->string
                      (channel-name (channel-instance-channel instance)))
                     (channel-instance-checkout instance)
                     #:commit (channel-instance-commit instance)
                     #:core core
                     #:dependencies dependencies))

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

(define (channel-instance-derivations instances)
  "Return the list of derivations to build INSTANCES, in the same order as
INSTANCES."
  (define core-instance
    ;; The 'guix' channel is treated specially: it's an implicit dependency of
    ;; all the other channels.
    (find (lambda (instance)
            (guix-channel? (channel-instance-channel instance)))
          instances))

  (define edges
    (resolve-dependencies instances))

  (define (instance->derivation instance)
    (mcached (if (eq? instance core-instance)
                 (build-channel-instance instance)
                 (mlet %store-monad ((core (instance->derivation core-instance))
                                     (deps (mapm %store-monad instance->derivation
                                                 (edges instance))))
                   (build-channel-instance instance core deps)))
             instance))

  (unless core-instance
    (let ((loc (and=> (any (compose channel-location channel-instance-channel)
                           instances)
                      source-properties->location)))
      (raise (apply make-compound-condition
                    (condition
                     (&message (message "'guix' channel is lacking")))
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
                   (list guile-json guile-git guile-bytestructures
                         (ssh -> guile-ssh) (tls -> gnutls)))))

(define (old-style-guix? drv)
  "Return true if DRV corresponds to a ~/.config/guix/latest style of
derivation."
  ;; Here we rely on a gross historical fact: that derivations produced by the
  ;; "old style" (before commit 8a0d9bc8a3f153159d9e239a151c0fa98f1e12d8,
  ;; dated May 30, 2018) did not depend on "guix-command.drv".
  (not (find (lambda (input)
               (string-suffix? "-guix-command.drv"
                               (derivation-input-path input)))
             (derivation-inputs drv))))

(define (channel-instances->manifest instances)
  "Return a profile manifest with entries for all of INSTANCES, a list of
channel instances."
  (define instance->entry
    (match-lambda
      ((instance drv)
       (let ((commit  (channel-instance-commit instance))
             (channel (channel-instance-channel instance)))
         (with-monad %store-monad
           (return (manifest-entry
                     (name (symbol->string (channel-name channel)))
                     (version (string-take commit 7))
                     (item (if (guix-channel? channel)
                               (if (old-style-guix? drv)
                                   (whole-package-for-legacy
                                    (string-append name "-" version)
                                    drv)
                                   drv)
                               drv))
                     (properties
                      `((source (repository
                                 (version 0)
                                 (url ,(channel-url channel))
                                 (branch ,(channel-branch channel))
                                 (commit ,commit))))))))))))

  (mlet* %store-monad ((derivations (channel-instance-derivations instances))
                       (entries     (mapm %store-monad instance->entry
                                          (zip instances derivations))))
    (return (manifest entries))))

(define (package-cache-file manifest)
  "Build a package cache file for the instance in MANIFEST.  This is meant to
be used as a profile hook."
  (mlet %store-monad ((profile (profile-derivation manifest
                                                   #:hooks '())))

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

(define* (latest-channel-derivation #:optional (channels %default-channels))
  "Return as a monadic value the derivation that builds the profile for the
latest instances of CHANNELS."
  (mlet %store-monad ((instances (latest-channel-instances* channels)))
    (channel-instances->derivation instances)))
