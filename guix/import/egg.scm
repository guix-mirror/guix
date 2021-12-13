;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (guix import egg)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-71)
  #:use-module (gcrypt hash)
  #:use-module (guix git)
  #:use-module (guix i18n)
  #:use-module (guix base32)
  #:use-module (guix diagnostics)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix build-system)
  #:use-module (guix build-system chicken)
  #:use-module (guix store)
  #:use-module ((guix download) #:select (download-to-store url-fetch))
  #:use-module (guix import utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (egg->guix-package
            egg-recursive-import
            %egg-updater

            ;; For tests.
            guix-package->egg-name))

;;; Commentary:
;;;
;;; (guix import egg) provides package importer for CHICKEN eggs.  See the
;;; official specification format for eggs
;;; <https://wiki.call-cc.org/man/5/Egg%20specification%20format>.
;;;
;;; The following happens under the hood:
;;;
;;; * <git://code.call-cc.org/eggs-5-all> is a Git repository that contains
;;;   all versions of all CHICKEN eggs.  We look clone this repository and, by
;;;   default, retrieve the latest version number, and the PACKAGE.egg file,
;;;   which contains a list of lists containing metadata about the egg.
;;;
;;; * All the eggs are stored as tarballs at
;;;   <https://code.call-cc.org/egg-tarballs/5>, so we grab the tarball for
;;;   the egg from there.
;;;
;;; * The rest of the package fields will be parsed from the PACKAGE.egg file.
;;;
;;; Todos:
;;;
;;; * Support for CHICKEN 4?
;;;
;;; * Some packages will specify a specific version of a depencency in the
;;;   PACKAGE.egg file, how should we handle this?
;;;
;;; Code:


;;;
;;; Egg metadata fetcher and helper functions.
;;;

(define package-name-prefix "chicken-")

(define %eggs-url
  (make-parameter "https://code.call-cc.org/egg-tarballs/5"))

(define %eggs-home-page
  (make-parameter "https://wiki.call-cc.org/egg"))

(define (egg-source-url name version)
  "Return the URL to the source tarball for version VERSION of the CHICKEN egg
NAME."
  `(egg-uri ,name version))

(define (egg-name->guix-name name)
  "Return the package name for CHICKEN egg NAME."
  (string-append package-name-prefix name))

(define (eggs-repository)
  "Update or fetch the latest version of the eggs repository and return the path
to the repository."
  (let* ((url "git://code.call-cc.org/eggs-5-all")
         (directory commit _ (update-cached-checkout url)))
    directory))

(define (egg-directory name)
  "Return the directory containing the source code for the egg NAME."
  (let ((eggs-directory (eggs-repository)))
    (string-append eggs-directory "/" name)))

(define (find-latest-version name)
  "Get the latest version of the egg NAME."
  (let ((directory (scandir (egg-directory name))))
    (if directory
        (last directory)
        #f)))

(define* (egg-metadata name #:key (version #f) (file #f))
  "Return the package metadata file for the egg NAME at version VERSION, or if
FILE is specified, return the package metadata in FILE."
  (call-with-input-file (or file
                            (string-append (egg-directory name) "/"
                                           (or version
                                               (find-latest-version name))
                                           "/" name ".egg"))
    read))

(define (guix-name->egg-name name)
  "Return the CHICKEN egg name corresponding to the Guix package NAME."
  (if (string-prefix? package-name-prefix name)
      (string-drop name (string-length package-name-prefix))
      name))

(define (guix-package->egg-name package)
  "Return the CHICKEN egg name of the Guix CHICKEN PACKAGE."
  (or (assq-ref (package-properties package) 'upstream-name)
      (guix-name->egg-name (package-name package))))

(define (egg-package? package)
  "Check if PACKAGE is an CHICKEN egg package."
  (and (eq? (package-build-system package) chicken-build-system)
       (string-prefix? package-name-prefix (package-name package))))

(define string->license
  ;; Doesn't seem to use a specific format.
  ;; <https://wiki.call-cc.org/eggs-licensing>
  (match-lambda
   ("GPL-2" 'license:gpl2)
   ("GPL-2+" 'license:gpl2+)
   ("GPL-3" 'license:gpl3)
   ("GPL-3+" 'license:gpl3+)
   ("GPL" 'license:gpl?)
   ("AGPL-3" 'license:agpl3)
   ("AGPL" 'license:agpl?)
   ("LGPL-2.0" 'license:lgpl2.0)
   ("LGPL-2.0+" 'license:lgpl2.0+)
   ("LGPL-2.1" 'license:lgpl2.1)
   ("LGPL-2.1+" 'license:lgpl2.1+)
   ("LGPL-3" 'license:lgpl3)
   ("LGPL-3" 'license:lgpl3+)
   ("LGPL" 'license:lgpl?)
   ("BSD-1-Clause" 'license:bsd-1)
   ("BSD-2-Clause" 'license:bsd-2)
   ("BSD-3-Clause" 'license:bsd-3)
   ("BSD" 'license:bsd?)
   ("MIT" 'license:expat)
   ("ISC" 'license:isc)
   ("Artistic-2" 'license:artistic2.0)
   ("Apache-2.0" 'license:asl2.0)
   ("Public Domain" 'license:public-domain)
   ((x) (string->license x))
   ((lst ...) `(list ,@(map string->license lst)))
   (_ #f)))


;;;
;;; Egg importer.
;;;

(define* (egg->guix-package name version #:key (file #f) (source #f))
  "Import a CHICKEN egg called NAME from either the given .egg FILE, or from the
latest NAME metadata downloaded from the official repository if FILE is #f.
Return a <package> record or #f on failure.  If VERSION is specified, import
the particular version from the egg repository.

SOURCE is a ``file-like'' object containing the source code corresponding to
the egg.  If SOURCE is not specified, the latest tarball for egg NAME will be
downloaded.

Specifying the SOURCE argument is mainly useful for developing a CHICKEN egg
locally.  Note that if FILE and SOURCE are specified, recursive import will
not work."
  (define egg-content (if file
                          (egg-metadata name #:file file)
                          (egg-metadata name #:version version)))
  (if (not egg-content)
      (values #f '())                    ; egg doesn't exist
      (let* ((version* (or (assoc-ref egg-content 'version)
                           (find-latest-version name)))
             (version (if (list? version*) (first version*) version*))
             (source-url (if source #f (egg-source-url name version)))
             (tarball (if source
                          #f
                          (with-store store
                            (download-to-store
                             store (egg-uri name version))))))

        (define egg-home-page
          (string-append (%eggs-home-page) "/" name))

        (define egg-synopsis
          (match (assoc-ref egg-content 'synopsis)
            ((synopsis) synopsis)
            (_ #f)))

        (define egg-licenses
          (let ((licenses*
                 (match (assoc-ref egg-content 'license)
                   ((license)
                    (map string->license (string-split license #\/)))
                   (#f
                    '()))))
            (match licenses*
              ((license) license)
              ((license1 license2 ...) `(list ,@licenses*)))))

        (define (maybe-symbol->string sym)
          (if (symbol? sym) (symbol->string sym) sym))

        (define (prettify-system-dependency name)
          ;; System dependencies sometimes have spaces and/or upper case
          ;; letters in them.
          ;;
          ;; There will probably still be some weird edge cases.
          (string-map (lambda (char)
                        (case char
                          ((#\space) #\-)
                          (else char)))
                      (maybe-symbol->string name)))

        (define* (egg-parse-dependency name #:key (system? #f))
          (define extract-name
            (match-lambda
              ((name version) name)
              (name name)))

          (define (prettify-name name)
            (if system?
                (prettify-system-dependency name)
                (maybe-symbol->string name)))
          
          (let ((name (prettify-name (extract-name name))))
            ;; Dependencies are sometimes specified as symbols and sometimes
            ;; as strings
            (string->symbol (string-append
                             (if system? "" package-name-prefix)
                             name))))

        (define egg-propagated-inputs
          (let ((dependencies (assoc-ref egg-content 'dependencies)))
            (if (list? dependencies)
                (map egg-parse-dependency
                     dependencies)
                '())))

        ;; TODO: Or should these be propagated?
        (define egg-inputs
          (let ((dependencies (assoc-ref egg-content 'foreign-dependencies)))
            (if (list? dependencies)
                (map (lambda (name)
                       (egg-parse-dependency name #:system? #t))
                     dependencies)
                '())))

        (define egg-native-inputs
          (let* ((test-dependencies (or (assoc-ref egg-content
                                                   'test-dependencies)
                                        '()))
                 (build-dependencies (or (assoc-ref egg-content
                                                    'build-dependencies)
                                         '()))
                 (test+build-dependencies (append test-dependencies
                                                  build-dependencies)))
            (match test+build-dependencies
              ((_ _ ...) (map egg-parse-dependency
                              test+build-dependencies))
              (() '()))))

        ;; Copied from (guix import hackage).
        (define (maybe-inputs input-type inputs)
          (match inputs
            (()
             '())
            ((inputs ...)
             (list (list input-type
                         `(list ,@inputs))))))

        (values
         `(package
            (name ,(egg-name->guix-name name))
            (version ,version)
            (source
             ,(if source
                  source
                  `(origin
                     (method url-fetch)
                     (uri ,source-url)
                     (sha256
                      (base32 ,(if tarball
                                   (bytevector->nix-base32-string
                                    (file-sha256 tarball))
                                   "failed to download tar archive"))))))
            (build-system chicken-build-system)
            (arguments ,(list 'quasiquote (list #:egg-name name)))
            ,@(maybe-inputs 'native-inputs egg-native-inputs)
            ,@(maybe-inputs 'inputs egg-inputs)
            ,@(maybe-inputs 'propagated-inputs egg-propagated-inputs)
            (home-page ,egg-home-page)
            (synopsis ,egg-synopsis)
            (description #f)
            (license ,egg-licenses))
         (filter (lambda (name)
                   (not (member name '("srfi-4"))))
                 (map (compose guix-name->egg-name symbol->string)
                      (append egg-propagated-inputs
                              egg-native-inputs)))))))

(define egg->guix-package/m                   ;memoized variant
  (memoize egg->guix-package))

(define* (egg-recursive-import package-name #:optional version)
  (recursive-import package-name
                    #:version version
                    #:repo->guix-package (lambda* (name #:key version repo)
                                           (egg->guix-package/m name version))
                    #:guix-name egg-name->guix-name))


;;;
;;; Updater.
;;;

(define (latest-release package)
  "Return an @code{<upstream-source>} for the latest release of PACKAGE."
  (let* ((egg-name (guix-package->egg-name package))
         (version (find-latest-version egg-name))
         (source-url (egg-source-url egg-name version)))
    (upstream-source
     (package (package-name package))
     (version version)
     (urls (list source-url)))))

(define %egg-updater
  (upstream-updater
   (name 'egg)
   (description "Updater for CHICKEN egg packages")
   (pred egg-package?)
   (latest latest-release)))

;;; egg.scm ends here
