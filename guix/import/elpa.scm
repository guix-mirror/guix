;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module ((guix download) #:select (download-to-store))
  #:use-module (guix import utils)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module ((guix utils) #:select (call-with-temporary-output-file
                                       memoize))
  #:export (elpa->guix-package))

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
  (filter emacs-standard-library? names))

(define (elpa-name->package-name name)
  "Given the NAME of an Emacs package, return the corresponding Guix name."
  (let ((package-name-prefix "emacs-"))
    (if (string-prefix? package-name-prefix name)
        (string-downcase name)
        (string-append package-name-prefix (string-downcase name)))))

(define* (elpa-url #:optional (repo 'gnu))
  "Retrun the URL of REPO."
  (let ((elpa-archives
         '((gnu . "http://elpa.gnu.org/packages")
           (melpa-stable . "http://stable.melpa.org/packages")
           (melpa . "http://melpa.org/packages"))))
    (assq-ref elpa-archives repo)))

(define* (elpa-fetch-archive #:optional (repo 'gnu))
  "Retrive the archive with the list of packages available from REPO."
  (let ((url (and=> (elpa-url repo)
                    (cut string-append <> "/archive-contents"))))
    (if url
        (call-with-downloaded-file url read)
        (leave (_ "~A: currently not supported~%") repo))))

(define (call-with-downloaded-file url proc)
  "Fetch URL, store the content in a temporary file and call PROC with that
file.  Returns the value returned by PROC."
  (call-with-temporary-output-file
   (lambda (temp port)
     (or (and (url-fetch url temp)
              (call-with-input-file temp proc))
         (error "download failed" url)))))

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
                            (format port "#<elpa-package ~a-~a>"
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
  (let ((url (full-url repo name "-readme.txt")))
    (call-with-downloaded-file url read-string)))

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
                            (package-home-page (first rest))
                            (fetch-package-description kind name repo)
                            url)))
      (_ #f))))

(define* (elpa-package->sexp pkg)
  "Return the `package' S-expression for the Emacs package PKG, a record of
type '<elpa-package>'."

  (define name (elpa-package-name pkg))

  (define version (elpa-package-version pkg))

  (define source-url (elpa-package-source-url pkg))

  (define dependencies
    (let* ((deps (elpa-package-inputs pkg))
           (names (filter-dependencies (elpa-dependencies->names deps))))
      (map (lambda (n)
             (let ((new-n (elpa-name->package-name n)))
               (list new-n (list 'unquote (string->symbol new-n)))))
           names)))

  (define (maybe-inputs input-type inputs)
    (match inputs
      (()
       '())
      ((inputs ...)
       (list (list input-type
                   (list 'quasiquote inputs))))))

  (let ((tarball (with-store store
                   (download-to-store store source-url))))
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
       ,@(maybe-inputs 'inputs dependencies)
       (home-page ,(elpa-package-home-page pkg))
       (synopsis ,(elpa-package-synopsis pkg))
       (description ,(elpa-package-description pkg))
       (license license:gpl3+))))

(define* (elpa->guix-package name #:optional (repo 'gnu))
  "Fetch the package NAME from REPO and produce a Guix package S-expression."
  (let ((pkg (fetch-elpa-package name repo)))
    (and=> pkg elpa-package->sexp)))

;;; elpa.scm ends here
