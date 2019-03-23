;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 ng0 <ng0@n0.is>
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

(define-module (guix import hackage)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-1)
  #:use-module ((guix download) #:select (download-to-store url-fetch))
  #:use-module ((guix utils) #:select (package-name->name+version
                                       canonical-newline-port))
  #:use-module (guix http-client)
  #:use-module ((guix import utils) #:select (factorize-uri recursive-import))
  #:use-module (guix import cabal)
  #:use-module (guix store)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix memoization)
  #:use-module (guix upstream)
  #:use-module (guix packages)
  #:use-module ((guix utils) #:select (call-with-temporary-output-file))
  #:export (hackage->guix-package
            hackage-recursive-import
            %hackage-updater

            guix-package->hackage-name
            hackage-name->package-name
            hackage-fetch
            hackage-source-url
            hackage-cabal-url
            hackage-package?))

(define ghc-standard-libraries
  ;; List of libraries distributed with ghc (7.10.2). We include GHC itself as
  ;; some packages list it.
  '("array"
    "base"
    "bin-package-db"
    "binary"
    "bytestring"
    "cabal" ;; in the output of `ghc-pkg list` Cabal is uppercased, but
            ;; hackage-name->package-name takes this into account.
    "containers"
    "deepseq"
    "directory"
    "filepath"
    "ghc"
    "ghc-prim"
    "haskeline"
    "hoopl"
    "hpc"
    "integer-gmp"
    "pretty"
    "process"
    "rts"
    "template-haskell"
    "terminfo"
    "time"
    "transformers"
    "unix"
    "win32"
    "xhtml"))

(define package-name-prefix "ghc-")

(define (hackage-source-url name version)
  "Given a Hackage package NAME and VERSION, return a url to the source
tarball."
  (string-append "https://hackage.haskell.org/package/" name
                 "/" name "-" version ".tar.gz"))

(define* (hackage-cabal-url name #:optional version)
  "Given a Hackage package NAME and VERSION, return a url to the corresponding
.cabal file on Hackage.  If VERSION is #f or missing, the url for the latest
version is returned."
  (if version
      (string-append "https://hackage.haskell.org/package/"
                     name "-" version "/" name ".cabal")
      (string-append "https://hackage.haskell.org/package/"
                     name "/" name ".cabal")))

(define (hackage-name->package-name name)
  "Given the NAME of a Cabal package, return the corresponding Guix name."
  (if (string-prefix? package-name-prefix name)
      (string-downcase name)
      (string-append package-name-prefix (string-downcase name))))

(define guix-package->hackage-name
  (let ((uri-rx (make-regexp "https?://hackage.haskell.org/package/([^/]+)/.*"))
        (name-rx (make-regexp "(.*)-[0-9\\.]+")))
    (lambda (package)
      "Given a Guix package name, return the corresponding Hackage name."
      (let* ((source-url (and=> (package-source package) origin-uri))
             (name (match:substring (regexp-exec uri-rx source-url) 1)))
        (match (regexp-exec name-rx name)
          (#f name)
          (m (match:substring m 1)))))))

(define (hackage-fetch name-version)
  "Return the Cabal file for the package NAME-VERSION, or #f on failure.  If
the version part is omitted from the package name, then return the latest
version."
  (guard (c ((and (http-get-error? c)
                  (= 404 (http-get-error-code c)))
             #f))                       ;"expected" if package is unknown
    (let-values (((name version) (package-name->name+version name-version)))
      (let* ((url (hackage-cabal-url name version))
             (port (http-fetch url))
             (result (read-cabal (canonical-newline-port port))))
        (close-port port)
        result))))

(define string->license
  ;; List of valid values from
  ;; https://www.haskell.org
  ;; /cabal/release/cabal-latest/doc/API/Cabal/Distribution-License.html.
  (match-lambda
   ("GPL-2" 'gpl2)
   ("GPL-3" 'gpl3)
   ("GPL" "'gpl??")
   ("AGPL-3" 'agpl3)
   ("AGPL" "'agpl??")
   ("LGPL-2.1" 'lgpl2.1)
   ("LGPL-3" 'lgpl3)
   ("LGPL" "'lgpl??")
   ("BSD2" 'bsd-2)
   ("BSD3" 'bsd-3)
   ("MIT" 'expat)
   ("ISC" 'isc)
   ("MPL" 'mpl2.0)
   ("Apache-2.0" 'asl2.0)
   ((x) (string->license x))
   ((lst ...) `(list ,@(map string->license lst)))
   (_ #f)))


(define (cabal-dependencies->names cabal)
  "Return the list of dependencies names from the CABAL package object,
not including test suite dependencies or custom-setup dependencies."
  (let* ((lib (cabal-package-library cabal))
         (lib-deps (if (pair? lib)
                       (map cabal-dependency-name
                            (append-map cabal-library-dependencies lib))
                       '()))
         (exe (cabal-package-executables cabal))
         (exe-deps (if (pair? exe)
                       (map cabal-dependency-name
                            (append-map cabal-executable-dependencies exe))
                       '())))
    (delete-duplicates (append lib-deps exe-deps))))

(define (cabal-test-dependencies->names cabal)
  "Return the list of test suite dependencies from the CABAL package
object."
  (let* ((ts (cabal-package-test-suites cabal))
         (ts-deps (if (pair? ts)
                      (map cabal-dependency-name
                           (append-map cabal-test-suite-dependencies ts))
                      '())))
    ts-deps))

(define (cabal-custom-setup-dependencies->names cabal)
  "Return the list of custom-setup dependencies from the CABAL package
object."
  (let* ((custom-setup-dependencies (or (and=> (cabal-package-custom-setup cabal)
                                               cabal-custom-setup-dependencies)
                                        '())))
    (map cabal-dependency-name custom-setup-dependencies)))

(define (filter-dependencies dependencies own-name)
  "Filter the dependencies included with the GHC compiler from DEPENDENCIES, a
list with the names of dependencies.  OWN-NAME is the name of the Cabal
package being processed and is used to filter references to itself."
  (filter (lambda (d) (not (member (string-downcase d)
                                   (cons own-name ghc-standard-libraries))))
          dependencies))

(define* (hackage-module->sexp cabal #:key (include-test-dependencies? #t))
  "Return the `package' S-expression for a Cabal package.  CABAL is the
representation of a Cabal file as produced by 'read-cabal'."

  (define name
    (cabal-package-name cabal))

  (define version
    (cabal-package-version cabal))
  
  (define source-url
    (hackage-source-url name version))

  (define hackage-dependencies
    ((compose (cut filter-dependencies <>
                   (cabal-package-name cabal))
              (cut cabal-dependencies->names <>))
     cabal))

  (define hackage-native-dependencies
    (lset-difference
     equal?
     ((compose (cut filter-dependencies <>
                    (cabal-package-name cabal))
               ;; FIXME: Check include-test-dependencies?
               (lambda (cabal)
                 (append (if include-test-dependencies?
                             (cabal-test-dependencies->names cabal)
                             '())
                         (cabal-custom-setup-dependencies->names cabal))))
      cabal)
     hackage-dependencies))

  (define dependencies
    (map (lambda (name)
           (list name (list 'unquote (string->symbol name))))
         (map hackage-name->package-name
              hackage-dependencies)))

  (define native-dependencies
    (map (lambda (name)
           (list name (list 'unquote (string->symbol name))))
         (map hackage-name->package-name
              hackage-native-dependencies)))
  
  (define (maybe-inputs input-type inputs)
    (match inputs
      (()
       '())
      ((inputs ...)
       (list (list input-type
                   (list 'quasiquote inputs))))))
  
  (define (maybe-arguments)
    (if (not include-test-dependencies?)
        '((arguments `(#:tests? #f)))
        '()))

  (let ((tarball (with-store store
                   (download-to-store store source-url))))
    (values
     `(package
        (name ,(hackage-name->package-name name))
        (version ,version)
        (source (origin
                  (method url-fetch)
                  (uri (string-append ,@(factorize-uri source-url version)))
                  (sha256
                   (base32
                    ,(if tarball
                         (bytevector->nix-base32-string (file-sha256 tarball))
                         "failed to download tar archive")))))
        (build-system haskell-build-system)
        ,@(maybe-inputs 'inputs dependencies)
        ,@(maybe-inputs 'native-inputs native-dependencies)
        ,@(maybe-arguments)
        (home-page ,(cabal-package-home-page cabal))
        (synopsis ,(cabal-package-synopsis cabal))
        (description ,(cabal-package-description cabal))
        (license ,(string->license (cabal-package-license cabal))))
     (append hackage-dependencies hackage-native-dependencies))))

(define hackage->guix-package
  (memoize
   (lambda* (package-name #:key
                          (include-test-dependencies? #t)
                          (port #f)
                          (cabal-environment '()))
     "Fetch the Cabal file for PACKAGE-NAME from hackage.haskell.org, or, if the
called with keyword parameter PORT, from PORT.  Return the `package'
S-expression corresponding to that package, or #f on failure.
CABAL-ENVIRONMENT is an alist defining the environment in which the Cabal
conditionals are evaluated.  The accepted keys are: \"os\", \"arch\", \"impl\"
and the name of a flag.  The value associated with a flag has to be either the
symbol 'true' or 'false'.  The value associated with other keys has to conform
to the Cabal file format definition.  The default value associated with the
keys \"os\", \"arch\" and \"impl\" is \"linux\", \"x86_64\" and \"ghc\"
respectively."
     (let ((cabal-meta (if port
                           (read-cabal (canonical-newline-port port))
                           (hackage-fetch package-name))))
       (and=> cabal-meta (compose (cut hackage-module->sexp <>
                                       #:include-test-dependencies?
                                       include-test-dependencies?)
                                  (cut eval-cabal <> cabal-environment)))))))

(define* (hackage-recursive-import package-name . args)
  (recursive-import package-name #f
                    #:repo->guix-package (lambda (name repo)
                                           (apply hackage->guix-package (cons name args)))
                    #:guix-name hackage-name->package-name))

(define (hackage-package? package)
  "Return #t if PACKAGE is a Haskell package from Hackage."

  (define haskell-url?
    (let ((hackage-rx (make-regexp "https?://hackage.haskell.org")))
      (lambda (url)
        (regexp-exec hackage-rx url))))

  (let ((source-url (and=> (package-source package) origin-uri))
        (fetch-method (and=> (package-source package) origin-method)))
    (and (eq? fetch-method url-fetch)
         (match source-url
           ((? string?)
            (haskell-url? source-url))
           ((source-url ...)
            (any haskell-url? source-url))))))

(define (latest-release package)
  "Return an <upstream-source> for the latest release of PACKAGE."
  (let* ((hackage-name (guix-package->hackage-name package))
         (cabal-meta (hackage-fetch hackage-name)))
    (match cabal-meta
      (#f
       (format (current-error-port)
               "warning: failed to parse ~a~%"
               (hackage-cabal-url hackage-name))
       #f)
      ((_ *** ("version" (version)))
       (let ((url (hackage-source-url hackage-name version)))
         (upstream-source
          (package (package-name package))
          (version version)
          (urls (list url))))))))

(define %hackage-updater
  (upstream-updater
   (name 'hackage)
   (description "Updater for Hackage packages")
   (pred hackage-package?)
   (latest latest-release)))

;;; cabal.scm ends here
