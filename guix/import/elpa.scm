;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016, 2017, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
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
  #:use-module (ice-9 regex)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module ((guix download) #:select (download-to-store))
  #:use-module (guix import utils)
  #:use-module (guix http-client)
  #:use-module (guix git)
  #:use-module (guix hash)
  #:use-module ((guix serialization) #:select (write-file))
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (guix base32)
  #:use-module (guix upstream)
  #:use-module (guix packages)
  #:use-module (guix memoization)
  #:use-module ((guix utils) #:select (call-with-temporary-output-file))
  #:export (elpa->guix-package
            guix-package->elpa-name
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
           (nongnu . "https://elpa.nongnu.org/nongnu")
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

(define* (download-git-repository url ref)
  "Fetch the given REF from the Git repository at URL."
  (with-store store
    (latest-repository-commit store url #:ref ref)))

(define (package-name->melpa-recipe package-name)
  "Fetch the MELPA recipe for PACKAGE-NAME, represented as an alist from
keywords to values."
  (define recipe-url
    (string-append "https://raw.githubusercontent.com/melpa/melpa/master/recipes/"
                   package-name))

  (define (data->recipe data)
    (match data
      (() '())
      ((key value . tail)
       (cons (cons key value) (data->recipe tail)))))

  (let* ((port (http-fetch/cached (string->uri recipe-url)
                                  #:ttl (* 6 3600)))
         (data (read port)))
    (close-port port)
    (data->recipe (cons ':name data))))

(define (git-repository->origin recipe url)
  "Fetch origin details from the Git repository at URL for the provided MELPA
RECIPE."
  (define ref
    (cond
     ((assoc-ref recipe #:branch)
      => (lambda (branch) (cons 'branch branch)))
     ((assoc-ref recipe #:commit)
      => (lambda (commit) (cons 'commit commit)))
     (else
      '())))

  (let-values (((directory commit) (download-git-repository url ref)))
    `(origin
       (method git-fetch)
       (uri (git-reference
             (url ,url)
             (commit ,commit)))
       (sha256
        (base32
         ,(bytevector->nix-base32-string
           (file-hash* directory #:recursive? #true)))))))

(define* (melpa-recipe->origin recipe)
  "Fetch origin details from the MELPA recipe and associated repository for
the package named PACKAGE-NAME."
  (define (github-repo->url repo)
    (string-append "https://github.com/" repo ".git"))
  (define (gitlab-repo->url repo)
    (string-append "https://gitlab.com/" repo ".git"))

  (match (assq-ref recipe ':fetcher)
    ('github (git-repository->origin recipe (github-repo->url (assq-ref recipe ':repo))))
    ('gitlab (git-repository->origin recipe (gitlab-repo->url (assq-ref recipe ':repo))))
    ('git    (git-repository->origin recipe (assq-ref recipe ':url)))
    (#f #f)   ; if we're not using melpa then this stops us printing a warning
    (_ (warning (G_ "Unsupported MELPA fetcher: ~a, falling back to unstable MELPA source.~%")
                (assq-ref recipe ':fetcher))
       #f)))

(define default-files-spec
  ;; This contains more than just the things contained in %default-include and
  ;; %default-exclude, presumably because this includes source files (*.in,
  ;; *.texi, etc.) which have already been processed for releases.
  ;;
  ;; Taken from:
  ;; https://github.com/melpa/melpa/blob/e8dc709d0ab2b4a68c59315f42858bcb86095f11/package-build/package-build.el#L580-L585
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el")))

(define* (melpa-recipe->maybe-arguments melpa-recipe)
  "Extract arguments for the build system from MELPA-RECIPE."
  (define (glob->regexp glob)
    (string-append
     "^"
     (regexp-substitute/global #f "\\*\\*?" glob
                               'pre
                               (lambda (m)
                                 (if (string= (match:substring m 0) "**")
                                     ".*"
                                     "[^/]+"))
                               'post)
     "$"))

  (let ((files (assq-ref melpa-recipe ':files)))
    (if files
        (let* ((with-default (apply append (map (lambda (entry)
                                                  (if (eq? ':defaults entry)
                                                      default-files-spec
                                                      (list entry)))
                                                files)))
               (inclusions (remove pair? with-default))
               (exclusions (apply append (map (match-lambda
                                                ((':exclude . values)
                                                 values)
                                                (_ '()))
                                              with-default))))
          `((arguments '(#:include ',(map glob->regexp inclusions)
                         #:exclude ',(map glob->regexp exclusions)))))
        '())))

(define* (elpa-package->sexp pkg #:optional license repo)
  "Return the `package' S-expression for the Emacs package PKG, a record of
type '<elpa-package>'."

  (define melpa-recipe
    ;; XXX: Call 'identity' to work around a Guile 3.0.[5-7] compiler bug:
    ;; <https://bugs.gnu.org/48368>.
    (and (eq? (identity repo) 'melpa)
         (package-name->melpa-recipe (elpa-package-name pkg))))

  (define name (elpa-package-name pkg))

  (define version (elpa-package-version pkg))

  (define source-url (elpa-package-source-url pkg))

  (define dependencies-names
    (filter-dependencies (elpa-dependencies->names
                          (elpa-package-inputs pkg))))

  (define dependencies
    (map (compose string->symbol elpa-name->package-name)
         dependencies-names))

  (define (maybe-inputs input-type inputs)
    (match inputs
      (()
       '())
      ((inputs ...)
       (list (list input-type `(list ,@inputs))))))

  (define melpa-source
    (melpa-recipe->origin melpa-recipe))

  (values
   `(package
      (name ,(elpa-name->package-name name))
      (version ,version)
      (source ,(or melpa-source
                   (let ((tarball (with-store store
                                    (download-to-store store source-url))))
                     `(origin
                        (method url-fetch)
                        (uri (string-append ,@(factorize-uri source-url version)))
                        (sha256
                         (base32
                          ,(if tarball
                               (bytevector->nix-base32-string
                                (file-hash* tarball #:recursive? #false))
                               "failed to download package")))))))
      (build-system emacs-build-system)
      ,@(maybe-inputs 'propagated-inputs dependencies)
      ,@(if melpa-source
            (melpa-recipe->maybe-arguments melpa-recipe)
            '())
      (home-page ,(elpa-package-home-page pkg))
      (synopsis ,(elpa-package-synopsis pkg))
      (description ,(beautify-description (elpa-package-description pkg)))
      (license ,license))
   dependencies-names))

(define* (elpa->guix-package name #:key (repo 'gnu) version)
  "Fetch the package NAME from REPO and produce a Guix package S-expression."
  (match (fetch-elpa-package name repo)
    (#false
     (raise (condition
             (&message
              (message (format #false
                               "couldn't find meta-data for ELPA package `~a'."
                               name))))))
    (package
      ;; ELPA is known to contain only GPLv3+ code.  Other repos may contain
      ;; code under other license but there's no license metadata.
      (let ((license (and (memq repo '(gnu gnu/http)) 'license:gpl3+)))
        (elpa-package->sexp package license repo)))))


;;;
;;; Updates.
;;;

(define (guix-package->elpa-name package)
  "Given a Guix package, PACKAGE, return the upstream name on ELPA."
  (or (and=> (package-properties package)
             (cut assq-ref <> 'upstream-name))
      (if (string-prefix? "emacs-" (package-name package))
          (string-drop (package-name package) 6)
          (package-name package))))

(define (latest-release package)
  "Return an <upstream-release> for the latest release of PACKAGE."
  (define name (guix-package->elpa-name package))
  (define repo (elpa-repository package))

  (match (elpa-package-info name repo)
    (#f
     ;; No info, perhaps because PACKAGE is not truly an ELPA package.
     #f)
    (info
     (let* ((version (match info
                       ((name raw-version . _)
                        (elpa-version->string raw-version))))
            (url     (match info
                       ((_ raw-version reqs synopsis kind . rest)
                        (package-source-url kind name version repo)))))
       (upstream-source
        (package (package-name package))
        (version version)
        (urls (list url))
        (signature-urls (list (string-append url ".sig"))))))))

(define elpa-repository
  (memoize
   (url-predicate (lambda (url)
                    (let ((uri (string->uri url)))
                      (and uri
                           (cond
                            ((string=? (uri-host uri) "elpa.gnu.org")
                             'gnu)
                            ((string=? (uri-host uri) "elpa.nongnu.org")
                             'nongnu)
                            (else #f))))))))

(define (package-from-elpa-repository? package)
  (member (elpa-repository package) '(gnu nongnu)))

(define %elpa-updater
  ;; The ELPA updater.  We restrict it to packages hosted on elpa.gnu.org
  ;; because for other repositories, we typically grab the source elsewhere.
  (upstream-updater
   (name 'elpa)
   (description "Updater for ELPA packages")
   (pred package-from-elpa-repository?)
   (latest latest-release)))

(define elpa-guix-name (cut guix-name "emacs-" <>))

(define* (elpa-recursive-import package-name #:optional (repo 'gnu))
  (recursive-import package-name
                    #:repo repo
                    #:repo->guix-package elpa->guix-package
                    #:guix-name elpa-guix-name))

;;; elpa.scm ends here
