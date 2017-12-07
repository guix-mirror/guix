;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2017 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix scripts)
  #:use-module (guix store)
  #:use-module (guix config)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix grafts)
  #:use-module (guix monads)
  #:use-module (guix scripts build)
  #:use-module ((guix build utils)
                #:select (with-directory-excursion delete-file-recursively))
  #:use-module ((guix build download)
                #:select (%x509-certificate-directory))
  #:use-module (gnu packages base)
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages bootstrap)
                #:select (%bootstrap-guile))
  #:use-module ((gnu packages certs) #:select (le-certs))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (guix-pull))

(module-autoload! (resolve-module '(guix scripts pull))
                  '(git) '(git-error? set-tls-certificate-locations!)
                  '(guix git) '(latest-repository-commit))

(define (ensure-guile-git!)
  ;; Previously Guile-Git was not a prerequisite.  Thus, someone running 'guix
  ;; pull' on an old installation may be lacking Guile-Git.  To address this,
  ;; we autoload things that depend on Guile-Git and check in the entry point
  ;; whether Guile-Git is available.
  ;;
  ;; TODO: Remove this hack when Guile-Git is widespread or enforced.

  (unless (false-if-exception (resolve-interface '(git)))
    (leave (G_ "Guile-Git is missing but it is now required by 'guix pull'.
Install it by running:

  guix package -i ~a
  export GUILE_LOAD_PATH=$HOME/.guix-profile/share/guile/site/~a:$GUILE_LOAD_PATH
  export GUILE_LOAD_COMPILED_PATH=$HOME/.guix-profile/lib/guile/~a/site-ccache:$GUILE_LOAD_COMPILED_PATH
\n")
           (match (effective-version)
             ("2.0" "guile2.0-git")
             (_     "guile-git"))
           (effective-version)
           (effective-version)))

  ;; XXX: For unclear reasons this is needed for
  ;; 'set-tls-certificate-locations!'.
  (module-use! (resolve-module '(guix scripts pull))
               (resolve-interface '(git))))

(define %repository-url
  (or (getenv "GUIX_PULL_URL") "https://git.savannah.gnu.org/git/guix.git"))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((repository-url . ,%repository-url)
    (ref . (branch . "origin/master"))
    (system . ,(%current-system))
    (substitutes? . #t)
    (build-hook? . #t)
    (graft? . #t)
    (verbosity . 0)))

(define (show-help)
  (display (G_ "Usage: guix pull [OPTION]...
Download and deploy the latest version of Guix.\n"))
  (display (G_ "
      --verbose          produce verbose output"))
  (display (G_ "
      --url=URL          download from the Git repository at URL"))
  (display (G_ "
      --commit=COMMIT    download the specified COMMIT"))
  (display (G_ "
      --branch=BRANCH    download the tip of the specified BRANCH"))
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
         (option '("url") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'repository-url arg
                               (alist-delete 'repository-url result))))
         (option '("commit") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'ref `(commit . ,arg) result)))
         (option '("branch") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'ref `(branch . ,(string-append "origin/" arg))
                               result)))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t (alist-cons 'graft? #f result))))
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

(define %self-build-file
  ;; The file containing code to build Guix.  This serves the same purpose as
  ;; a makefile, and, similarly, is intended to always keep this name.
  "build-aux/build-self.scm")

(define* (build-from-source source
                            #:key verbose? commit)
  "Return a derivation to build Guix from SOURCE, using the self-build script
contained therein.  Use COMMIT as the version string."
  ;; Running the self-build script makes it easier to update the build
  ;; procedure: the self-build script of the Guix-to-be-installed contains the
  ;; right dependencies, build procedure, etc., which the Guix-in-use may not
  ;; be know.
  (let* ((script (string-append source "/" %self-build-file))
         (build  (primitive-load script)))
    ;; BUILD must be a monadic procedure of at least one argument: the source
    ;; tree.
    (build source #:verbose? verbose? #:version commit)))

(define* (build-and-install source config-dir
                            #:key verbose? commit)
  "Build the tool from SOURCE, and install it in CONFIG-DIR."
  (mlet* %store-monad ((source        (build-from-source source
                                                         #:commit commit
                                                         #:verbose? verbose?))
                       (source-dir -> (derivation->output-path source))
                       (to-do?        (what-to-build (list source)))
                       (built?        (built-derivations (list source))))
    ;; Always update the 'latest' symlink, regardless of whether SOURCE was
    ;; already built or not.
    (if built?
        (mlet* %store-monad
            ((latest -> (string-append config-dir "/latest"))
             (done      (indirect-root-added latest)))
          (if (and (file-exists? latest)
                   (string=? (readlink latest) source-dir))
              (begin
                (display (G_ "Guix already up to date\n"))
                (return #t))
              (begin
                (switch-symlinks latest source-dir)
                (format #t
                        (G_ "updated ~a successfully deployed under `~a'~%")
                        %guix-package-name latest)
                (return #t))))
        (leave (G_ "failed to update Guix, check the build log~%")))))

(define (honor-lets-encrypt-certificates! store)
  "Tell Guile-Git to use the Let's Encrypt certificates."
  (let* ((drv   (package-derivation store le-certs))
         (certs (string-append (derivation->output-path drv)
                               "/etc/ssl/certs")))
    (build-derivations store (list drv))

    ;; In the past Guile-Git would not provide this procedure.
    (if (module-defined? (resolve-interface '(git))
                         'set-tls-certificate-locations!)
        (set-tls-certificate-locations! certs)
        (begin
          ;; In this case we end up using whichever certificates OpenSSL
          ;; chooses to use: $SSL_CERT_FILE, $SSL_CERT_DIR, or /etc/ssl/certs.
          (warning (G_ "cannot enforce use of the Let's Encrypt \
certificates~%"))
          (warning (G_ "please upgrade Guile-Git~%"))))))

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


(define (guix-pull . args)
  (define (use-le-certs? url)
    (string-prefix? "https://git.savannah.gnu.org/" url))

  (with-error-handling
    (with-git-error-handling
     (let* ((opts  (parse-command-line args %options
                                       (list %default-options)))
            (url   (assoc-ref opts 'repository-url))
            (ref   (assoc-ref opts 'ref))
            (cache (string-append (cache-directory) "/pull")))
       (ensure-guile-git!)

       (unless (assoc-ref opts 'dry-run?)         ;XXX: not very useful
         (with-store store
           (parameterize ((%graft? (assoc-ref opts 'graft?)))
             (set-build-options-from-command-line store opts)

             ;; For reproducibility, always refer to the LE certificates when we
             ;; know we're talking to Savannah.
             (when (use-le-certs? url)
               (honor-lets-encrypt-certificates! store))

             (format (current-error-port)
                     (G_ "Updating from Git repository at '~a'...~%")
                     url)

             (let-values (((checkout commit)
                           (latest-repository-commit store url
                                                     #:ref ref
                                                     #:cache-directory cache)))

               (format (current-error-port)
                       (G_ "Building from Git commit ~a...~%")
                       commit)
               (parameterize ((%guile-for-build
                               (package-derivation
                                store
                                (if (assoc-ref opts 'bootstrap?)
                                    %bootstrap-guile
                                    (canonical-package guile-2.2)))))
                 (run-with-store store
                   (build-and-install checkout (config-directory)
                                      #:commit commit
                                      #:verbose?
                                      (assoc-ref opts 'verbose?))))))))))))

;;; pull.scm ends here
