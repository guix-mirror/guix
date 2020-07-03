;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016, 2017, 2018, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (guix import elpa)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module ((guix download) #:select (download-to-store))
  #:use-module (guix import utils)
  #:use-module (guix http-client)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix upstream)
  #:use-module (guix packages)
  #:use-module ((guix utils) #:select (call-with-temporary-output-file))
  #:export (elpa->guix-package
            %elpa-updater
            elpa-recursive-import))

(define (elpa-dependencies->names deps)
  "Convert DEPS, a list of symbol/version pairs à la ELPA, to a list of
package names as strings"
  (match deps
    (((names _ ...) ...)
     (map symbol->string names))))

(define emacs-standard-library?
  (let ((libs '("emacs" "cl-lib")))
    (lambda (lib)
      "Return true if LIB is part of Emacs itself.  The check is not
exhaustive and only attempts to recognize a subset of packages which in the
past were distributed separately from Emacs."
      (member lib libs))))

(define (filter-dependencies names)
  "Remove the package names included with Emacs from the list of
NAMES (strings)."
  (remove emacs-standard-library? names))

(define (elpa-name->package-name name)
  "Given the NAME of an Emacs package, return the corresponding Guix name."
  (let ((package-name-prefix "emacs-"))
    (if (string-prefix? package-name-prefix name)
        (string-downcase name)
        (string-append package-name-prefix (string-downcase name)))))

(define* (elpa-url #:optional (repo 'gnu))
  "Retrieve the URL of REPO."
  (let ((elpa-archives
         '((gnu . "https://elpa.gnu.org/packages")
           (gnu/http . "http://elpa.gnu.org/packages") ;for testing
           (melpa-stable . "https://stable.melpa.org/packages")
           (melpa . "https://melpa.org/packages"))))
    (assq-ref elpa-archives repo)))

(define* (elpa-fetch-archive #:optional (repo 'gnu))
  "Retrieve the archive with the list of packages available from REPO."
  (let ((url (and=> (elpa-url repo)
                    (cut string-append <> "/archive-contents"))))
    (if url
        ;; Use a relatively small TTL for the archive itself.
        (let* ((port (http-fetch/cached (string->uri url)
                                        #:ttl (* 6 3600)))
               (data (read port)))
          (close-port port)
          data)
        (leave (G_ "~A: currently not supported~%") repo))))

(define* (call-with-downloaded-file url proc #:optional (error-thunk #f))
  "Fetch URL, store the content in a temporary file and call PROC with that
file.  Returns the value returned by PROC.  On error call ERROR-THUNK and
return its value or leave if it's false."
  (catch #t
    (lambda ()
      (proc (http-fetch/cached (string->uri url))))
    (lambda (key . args)
      (if error-thunk
          (error-thunk)
          (leave (G_ "~A: download failed~%") url)))))

(define (is-elpa-package? name elpa-pkg-spec)
  "Return true if the string NAME corresponds to the name of the package
defined by ELPA-PKG-SPEC, a package specification as in an archive
'archive-contents' file."
  (eq? (first elpa-pkg-spec) (string->symbol name)))

(define* (elpa-package-info name #:optional (repo 'gnu))
  "Extract the information about the package NAME from the package archieve of
REPO."
  (let* ((archive (elpa-fetch-archive repo))
         (pkgs (match archive ((version pkg-spec ...) pkg-spec)))
         (info (filter (cut is-elpa-package? name <>) pkgs)))
    (if (pair? info) (first info) #f)))

;; Object to store information about an ELPA package.
(define-record-type <elpa-package>
  (make-elpa-package name version inputs synopsis kind home-page description
                     source-url)
  elpa-package?
  (name elpa-package-name)
  (version elpa-package-version)
  (inputs elpa-package-inputs)
  (synopsis elpa-package-synopsis)
  (kind elpa-package-kind)
  (home-page elpa-package-home-page)
  (description elpa-package-description)
  (source-url elpa-package-source-url))

(set-record-type-printer! <elpa-package>
                          (lambda (package port)
                            (format port "#<elpa-package ~a@~a>"
                                      (elpa-package-name package)
                                      (elpa-package-version package))))

(define (elpa-version->string elpa-version)
  "Convert the package version as used in Emacs package files into a string."
  (if (pair? elpa-version)
      (let-values (((ms rest) (match elpa-version
                                ((ms . rest)
                                 (values ms rest)))))
        (fold (lambda (n s) (string-append s "." (number->string n)))
              (number->string ms) rest))
      #f))

(define (package-home-page alist)
  "Extract the package home-page from ALIST."
  (or (assq-ref alist ':url) "unspecified"))

(define (ensure-list alist)
  "If ALIST is the symbol 'nil return the empty list.  Otherwise, return ALIST."
  (if (eq? alist 'nil)
      '()
      alist))

(define (package-source-url kind name version repo)
  "Return the source URL of the package described the the strings NAME and
VERSION at REPO.  KIND is either the symbol 'single or 'tar."
  (case kind
    ((single) (full-url repo name ".el" version))
    ((tar) (full-url repo name ".tar" version))
    (else
     #f)))

(define* (full-url repo name suffix #:optional (version #f))
  "Return the full URL of the package NAME at REPO and the SUFFIX.  Maybe
include VERSION."
  (if version
      (string-append (elpa-url repo) "/" name "-" version suffix)
      (string-append (elpa-url repo) "/" name suffix)))

(define (fetch-package-description kind name repo)
  "Fetch the description of package NAME of type KIND from REPO."
  (let ((url (full-url repo name "-readme.txt"))
        (error-thunk (lambda () "No description available.")))
    (call-with-downloaded-file url read-string error-thunk)))

(define* (fetch-elpa-package name #:optional (repo 'gnu))
  "Fetch package NAME from REPO."
  (let ((pkg (elpa-package-info name repo)))
    (match pkg
      ((name version reqs synopsis kind . rest)
       (let* ((name (symbol->string name))
             (ver (elpa-version->string version))
             (url (package-source-url kind name ver repo)))
         (make-elpa-package name ver
                            (ensure-list reqs) synopsis kind
                            (package-home-page (match rest
                                                 (() #f)
                                                 ((one) one)))
                            (fetch-package-description kind name repo)
                            url)))
      (_ #f))))

(define* (elpa-package->sexp pkg #:optional license)
  "Return the `package' S-expression for the Emacs package PKG, a record of
type '<elpa-package>'."

  (define name (elpa-package-name pkg))

  (define version (elpa-package-version pkg))

  (define source-url (elpa-package-source-url pkg))

  (define dependencies-names
    (filter-dependencies (elpa-dependencies->names
                          (elpa-package-inputs pkg))))

  (define dependencies
    (map (lambda (n)
           (let ((new-n (elpa-name->package-name n)))
             (list new-n (list 'unquote (string->symbol new-n)))))
         dependencies-names))

  (define (maybe-inputs input-type inputs)
    (match inputs
      (()
       '())
      ((inputs ...)
       (list (list input-type
                   (list 'quasiquote inputs))))))

  (let ((tarball (with-store store
                   (download-to-store store source-url))))
    (values
     `(package
        (name ,(elpa-name->package-name name))
        (version ,version)
        (source (origin
                  (method url-fetch)
                  (uri (string-append ,@(factorize-uri source-url version)))
                  (sha256
                   (base32
                    ,(if tarball
                         (bytevector->nix-base32-string (file-sha256 tarball))
                         "failed to download package")))))
        (build-system emacs-build-system)
        ,@(maybe-inputs 'propagated-inputs dependencies)
        (home-page ,(elpa-package-home-page pkg))
        (synopsis ,(elpa-package-synopsis pkg))
        (description ,(elpa-package-description pkg))
        (license ,license))
     dependencies-names)))

(define* (elpa->guix-package name #:optional (repo 'gnu))
  "Fetch the package NAME from REPO and produce a Guix package S-expression."
  (match (fetch-elpa-package name repo)
    (#f #f)
    (package
      ;; ELPA is known to contain only GPLv3+ code.  Other repos may contain
      ;; code under other license but there's no license metadata.
      (let ((license (and (memq repo '(gnu gnu/http)) 'license:gpl3+)))
        (elpa-package->sexp package license)))))


;;;
;;; Updates.
;;;

(define (latest-release package)
  "Return an <upstream-release> for the latest release of PACKAGE."
  (define name
    (if (string-prefix? "emacs-" (package-name package))
        (string-drop (package-name package) 6)
        (package-name package)))

  (let* ((repo    'gnu)
         (info    (elpa-package-info name repo))
         (version (match info
                    ((name raw-version . _)
                     (elpa-version->string raw-version))))
         (url     (match info
                    ((_ raw-version reqs synopsis kind . rest)
                     (package-source-url kind name version repo)))))
    (upstream-source
     (package (package-name package))
     (version version)
     (urls (list url))
     (signature-urls (list (string-append url ".sig"))))))

(define package-from-gnu.org?
  (url-predicate (lambda (url)
                   (let ((uri (string->uri url)))
                     (and uri
                          (string=? (uri-host uri) "elpa.gnu.org"))))))

(define %elpa-updater
  ;; The ELPA updater.  We restrict it to packages hosted on elpa.gnu.org
  ;; because for other repositories, we typically grab the source elsewhere.
  (upstream-updater
   (name 'elpa)
   (description "Updater for ELPA packages")
   (pred package-from-gnu.org?)
   (latest latest-release)))

(define elpa-guix-name (cut guix-name "emacs-" <>))

(define* (elpa-recursive-import package-name #:optional (repo 'gnu))
  (recursive-import package-name repo
                    #:repo->guix-package elpa->guix-package
                    #:guix-name elpa-guix-name))

;;; elpa.scm ends here
