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
  #:use-module (guix download)
  #:use-module (guix gexp)
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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:export (guix-pull))

(define %snapshot-url
  ;; "http://hydra.gnu.org/job/guix/master/tarball/latest/download"
  "https://git.savannah.gnu.org/cgit/guix.git/snapshot/master.tar.gz"
  )

(define-syntax-rule (with-environment-variable variable value body ...)
  (let ((original (getenv variable)))
    (dynamic-wind
      (lambda ()
        (setenv variable value))
      (lambda ()
        body ...)
      (lambda ()
        (setenv variable original)))))

(define-syntax-rule (with-PATH value body ...)
  (with-environment-variable "PATH" value body ...))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((tarball-url . ,%snapshot-url)
    (system . ,(%current-system))
    (substitutes? . #t)
    (graft? . #t)
    (max-silent-time . 3600)
    (verbosity . 0)))

(define (show-help)
  (display (G_ "Usage: guix pull [OPTION]...
Download and deploy the latest version of Guix.\n"))
  (display (G_ "
      --verbose          produce verbose output"))
  (display (G_ "
      --url=URL          download the Guix tarball from URL"))
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
                   (alist-cons 'tarball-url arg
                               (alist-delete 'tarball-url result))))
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

(define (temporary-directory)
  "Make a temporary directory and return its name."
  (let ((name (tmpnam)))
    (mkdir name)
    (chmod name #o700)
    name))

(define (first-directory directory)
  "Return a the name of the first file found under DIRECTORY."
  (match (scandir directory
                  (lambda (name)
                    (and (not (member name '("." "..")))
                         (file-is-directory? name))))
    ((directory)
     directory)
    (x
     (raise (condition
             (&message
              (message "tarball did not produce a single source directory")))))))

(define (interned-then-deleted directory name)
  "Add DIRECTORY to the store under NAME, and delete it.  Return the resulting
store file name."
  (mlet %store-monad ((result (interned-file directory name
                                             #:recursive? #t)))
    (delete-file-recursively directory)
    (return result)))

(define (unpack tarball)
  "Return the name of the directory where TARBALL has been unpacked."
  (mlet* %store-monad ((format -> (lift format %store-monad))
                       (tar  (package->derivation tar))
                       (gzip (package->derivation gzip)))
    (mbegin %store-monad
      (what-to-build (list tar gzip))
      (built-derivations (list tar gzip))
      (format #t (G_ "unpacking '~a'...~%") tarball)

      (let ((source (temporary-directory)))
        (with-directory-excursion source
          (with-PATH (string-append (derivation->output-path gzip) "/bin")
            (unless (zero? (system* (string-append (derivation->output-path tar)
                                                   "/bin/tar")
                                    "xf" tarball))
              (raise (condition
                      (&message (message "failed to unpack source code"))))))

          (interned-then-deleted (string-append source "/"
                                                (first-directory source))
                                 "guix-source"))))))

(define %self-build-file
  ;; The file containing code to build Guix.  This serves the same purpose as
  ;; a makefile, and, similarly, is intended to always keep this name.
  "build-aux/build-self.scm")

(define* (build-from-source tarball #:key verbose?)
  "Return a derivation to build Guix from TARBALL, using the self-build script
contained therein."
  ;; Running the self-build script makes it easier to update the build
  ;; procedure: the self-build script of the Guix-to-be-installed contains the
  ;; right dependencies, build procedure, etc., which the Guix-in-use may not
  ;; be know.
  (mlet* %store-monad ((source (unpack tarball))
                       (script -> (string-append source "/"
                                                 %self-build-file))
                       (build -> (primitive-load script)))
    ;; BUILD must be a monadic procedure of at least one argument: the source
    ;; tree.
    (build source #:verbose? verbose?)))

(define* (build-and-install tarball config-dir
                            #:key verbose?)
  "Build the tool from TARBALL, and install it in CONFIG-DIR."
  (mlet* %store-monad ((source        (build-from-source tarball
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


(define (guix-pull . args)
  (define (use-le-certs? url)
    (string-prefix? "https://git.savannah.gnu.org/" url))

  (define (fetch-tarball store url)
    (download-to-store store url "guix-latest.tar.gz"))

  (with-error-handling
    (let* ((opts  (parse-command-line args %options
                                      (list %default-options)))
           (url   (assoc-ref opts 'tarball-url)))
      (unless (assoc-ref opts 'dry-run?)          ;XXX: not very useful
        (with-store store
          (set-build-options-from-command-line store opts)
          (let ((tarball
                 (if (use-le-certs? url)
                     (let* ((drv (package-derivation store le-certs))
                            (certs (string-append (derivation->output-path drv)
                                                  "/etc/ssl/certs")))
                       (build-derivations store (list drv))
                       (parameterize ((%x509-certificate-directory certs))
                         (fetch-tarball store url)))
                     (fetch-tarball store url))))
            (unless tarball
              (leave (G_ "failed to download up-to-date source, exiting\n")))
            (parameterize ((%guile-for-build
                            (package-derivation store
                                                (if (assoc-ref opts 'bootstrap?)
                                                    %bootstrap-guile
                                                    (canonical-package guile-2.0)))))
              (run-with-store store
                (build-and-install tarball (config-directory)
                                   #:verbose? (assoc-ref opts 'verbose?))))))))))

;; Local Variables:
;; eval: (put 'with-PATH 'scheme-indent-function 1)
;; eval: (put 'with-temporary-directory 'scheme-indent-function 1)
;; End:

;;; pull.scm ends here
