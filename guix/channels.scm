;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix discovery)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:autoload   (guix self) (whole-package)
  #:use-module (ice-9 match)
  #:export (channel
            channel?
            channel-name
            channel-url
            channel-branch
            channel-commit
            channel-location

            %default-channels

            channel-instance?
            channel-instance-channel
            channel-instance-commit
            channel-instance-checkout

            latest-channel-instances
            checkout->channel-instance
            latest-channel-derivation
            channel-instances->manifest
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
;; TODO: Add a way to express dependencies among channels.

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

(define (channel-reference channel)
  "Return the \"reference\" for CHANNEL, an sexp suitable for
'latest-repository-commit'."
  (match (channel-commit channel)
    (#f      `(branch . ,(channel-branch channel)))
    (commit  `(commit . ,(channel-commit channel)))))

(define (latest-channel-instances store channels)
  "Return a list of channel instances corresponding to the latest checkouts of
CHANNELS."
  (map (lambda (channel)
         (format (current-error-port)
                 (G_ "Updating channel '~a' from Git repository at '~a'...~%")
                 (channel-name channel)
                 (channel-url channel))
         (let-values (((checkout commit)
                       (latest-repository-commit store (channel-url channel)
                                                 #:ref (channel-reference
                                                        channel))))
           (channel-instance channel commit checkout)))
       channels))

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

(define (standard-module-derivation name source dependencies)
  "Return a derivation that builds the Scheme modules in SOURCE and that
depend on DEPENDENCIES, a list of lowerable objects.  The assumption is that
SOURCE contains package modules to be added to '%package-module-path'."
  (define modules
    (scheme-modules* source))

  ;; FIXME: We should load, say SOURCE/.guix-channel.scm, which would allow
  ;; channel publishers to specify things such as the sub-directory where .scm
  ;; files live, files to exclude from the channel, preferred substitute URLs,
  ;; etc.
  (mlet* %store-monad ((compiled
                        (compiled-modules modules
                                          #:name name
                                          #:module-path (list source)
                                          #:extensions dependencies)))

    (gexp->derivation name
                      (with-extensions dependencies
                        (with-imported-modules '((guix build utils))
                          #~(begin
                              (use-modules (guix build utils))

                              (let ((go  (string-append #$output "/lib/guile/"
                                                        (effective-version)
                                                        "/site-ccache"))
                                    (scm (string-append #$output
                                                        "/share/guile/site/"
                                                        (effective-version))))
                                (mkdir-p (dirname go))
                                (symlink #$compiled go)
                                (mkdir-p (dirname scm))
                                (symlink #$source scm))))))))

(define* (build-from-source name source
                            #:key verbose? commit
                            (dependencies '()))
  "Return a derivation to build Guix from SOURCE, using the self-build script
contained therein.  Use COMMIT as the version string."
  ;; Running the self-build script makes it easier to update the build
  ;; procedure: the self-build script of the Guix-to-be-installed contains the
  ;; right dependencies, build procedure, etc., which the Guix-in-use may not
  ;; be know.
  (define script
    (string-append source "/" %self-build-file))

  (if (file-exists? script)
      (let ((build (save-module-excursion
                    (lambda ()
                      (primitive-load script)))))
        ;; BUILD must be a monadic procedure of at least one argument: the
        ;; source tree.
        ;;
        ;; Note: BUILD can return #f if it does not support %PULL-VERSION.  In
        ;; the future we'll fall back to a previous version of the protocol
        ;; when that happens.
        (build source #:verbose? verbose? #:version commit
               #:pull-version %pull-version))

      ;; Build a set of modules that extend Guix using the standard method.
      (standard-module-derivation name source dependencies)))

(define* (build-channel-instance instance #:optional (dependencies '()))
  "Return, as a monadic value, the derivation for INSTANCE, a channel
instance.  DEPENDENCIES is a list of extensions providing Guile modules that
INSTANCE depends on."
  (build-from-source (symbol->string
                      (channel-name (channel-instance-channel instance)))
                     (channel-instance-checkout instance)
                     #:commit (channel-instance-commit instance)
                     #:dependencies dependencies))

(define (channel-instance-derivations instances)
  "Return the list of derivations to build INSTANCES, in the same order as
INSTANCES."
  (define core-instance
    ;; The 'guix' channel is treated specially: it's an implicit dependency of
    ;; all the other channels.
    (find (lambda (instance)
            (guix-channel? (channel-instance-channel instance)))
          instances))

  (define dependencies
    ;; Dependencies of CORE-INSTANCE.
    ;; FIXME: It would be best not to hard-wire this information here and
    ;; instead query it to CORE-INSTANCE.
    (list (module-ref (resolve-interface '(gnu packages gnupg))
                      'guile-gcrypt)
          (module-ref (resolve-interface '(gnu packages guile))
                      'guile-git)
          (module-ref (resolve-interface '(gnu packages guile))
                      'guile-bytestructures)))

  (mlet %store-monad ((core (build-channel-instance core-instance)))
    (mapm %store-monad
          (lambda (instance)
            (if (eq? instance core-instance)
                (return core)
                (build-channel-instance instance
                                        (cons core dependencies))))
          instances)))

(define (whole-package-for-legacy name modules)
  "Return a full-blown Guix package for MODULES, a derivation that builds Guix
modules in the old ~/.config/guix/latest style."
  (define packages
    (resolve-interface '(gnu packages guile)))

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
    (whole-package name modules

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

(define (channel-instances->derivation instances)
  "Return the derivation of the profile containing INSTANCES, a list of
channel instances."
  (mlet %store-monad ((manifest (channel-instances->manifest instances)))
    (profile-derivation manifest)))

(define latest-channel-instances*
  (store-lift latest-channel-instances))

(define* (latest-channel-derivation #:optional (channels %default-channels))
  "Return as a monadic value the derivation that builds the profile for the
latest instances of CHANNELS."
  (mlet %store-monad ((instances (latest-channel-instances* channels)))
    (channel-instances->derivation instances)))
