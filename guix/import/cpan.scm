;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (guix import cpan)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:use-module (guix hash)
  #:use-module (guix store)
  #:use-module (guix base32)
  #:use-module ((guix download) #:select (download-to-store))
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:export (cpan->guix-package))

;;; Commentary:
;;;
;;; Generate a package declaration template for the latest version of a CPAN
;;; module, using meta-data from metacpan.org.
;;;
;;; Code:

(define string->license
  (match-lambda
   ;; List of valid values from https://metacpan.org/pod/CPAN::Meta::Spec.
   ;; Some licenses are excluded based on their absense from (guix licenses).
   ("agpl_3" 'agpl3)
   ;; apache_1_1
   ("apache_2_0" 'asl2.0)
   ;; artistic_1_0
   ;; artistic_2_0
   ("bsd" 'bsd-3)
   ("freebsd" 'bsd-2)
   ;; gfdl_1_2
   ("gfdl_1_3" 'fdl1.3+)
   ("gpl_1" 'gpl1)
   ("gpl_2" 'gpl2)
   ("gpl_3" 'gpl3)
   ("lgpl_2_1" 'lgpl2.1)
   ("lgpl_3_0" 'lgpl3)
   ("mit" 'x11)
   ;; mozilla_1_0
   ("mozilla_1_1" 'mpl1.1)
   ("openssl" 'openssl)
   ("perl_5" 'gpl1+)                    ;and Artistic 1
   ("qpl_1_0" 'qpl)
   ;; ssleay
   ;; sun
   ("zlib" 'zlib)
   ((x) (string->license x))
   ((lst ...) `(list ,@(map string->license lst)))
   (_ #f)))

(define (module->name module)
  "Transform a 'module' name into a 'release' name"
  (regexp-substitute/global #f "::" module 'pre "-" 'post))

(define (cpan-fetch module)
  "Return an alist representation of the CPAN metadata for the perl module MODULE,
or #f on failure.  MODULE should be e.g. \"Test::Script\""
  ;; This API always returns the latest release of the module.
  (json-fetch (string-append "http://api.metacpan.org/release/"
                             ;; XXX: The 'release' api requires the "release"
                             ;; name of the package.  This substitution seems
                             ;; reasonably consistent across packages.
                             (module->name module))))

(define (cpan-home name)
  (string-append "http://search.cpan.org/dist/" name))

(define (cpan-module->sexp meta)
  "Return the `package' s-expression for a CPAN module from the metadata in
META."
  (define name
    (assoc-ref meta "distribution"))

  (define (guix-name name)
    (if (string-prefix? "perl-" name)
        (string-downcase name)
        (string-append "perl-" (string-downcase name))))

  (define version
    (assoc-ref meta "version"))

  (define (convert-inputs phases)
    ;; Convert phase dependencies into a list of name/variable pairs.
    (match (flatten
            (map (lambda (ph)
                   (filter-map (lambda (t)
                                 (assoc-ref* meta "metadata" "prereqs" ph t))
                               '("requires" "recommends" "suggests")))
                 phases))
      (#f
       '())
      ((inputs ...)
       (delete-duplicates
        ;; Listed dependencies may include core modules.  Filter those out.
        (filter-map (match-lambda
                     ((or (module . "0") ("perl" . _))
                      ;; TODO: A stronger test might to run MODULE through
                      ;; `corelist' from our perl package.  This current test
                      ;; seems to be only a loose convention.
                      #f)
                     ((module . _)
                      (let ((name (guix-name (module->name module))))
                        (list name
                              (list 'unquote (string->symbol name))))))
                    inputs)))))

  (define (maybe-inputs guix-name inputs)
    (match inputs
      (()
       '())
      ((inputs ...)
       (list (list guix-name
                   (list 'quasiquote inputs))))))

  (define source-url
    (assoc-ref meta "download_url"))

  (let ((tarball (with-store store
                   (download-to-store store source-url))))
    `(package
       (name ,(guix-name name))
       (version ,version)
       (source (origin
                 (method url-fetch)
                 (uri (string-append ,@(factorize-uri source-url version)))
                 (sha256
                  (base32
                   ,(bytevector->nix-base32-string (file-sha256 tarball))))))
       (build-system perl-build-system)
       ,@(maybe-inputs 'native-inputs
                       ;; "runtime" and "test" may also be needed here.  See
                       ;; https://metacpan.org/pod/CPAN::Meta::Spec#Phases,
                       ;; which says they are required during building.  We
                       ;; have not yet had a need for cross-compiled perl
                       ;; modules, however, so we leave them out.
                       (convert-inputs '("configure" "build")))
       ,@(maybe-inputs 'inputs
                       (convert-inputs '("runtime")))
       (home-page ,(string-append "http://search.cpan.org/dist/" name))
       (synopsis ,(assoc-ref meta "abstract"))
       (description fill-in-yourself!)
       (license ,(string->license (assoc-ref meta "license"))))))

(define (cpan->guix-package module-name)
  "Fetch the metadata for PACKAGE-NAME from metacpan.org, and return the
`package' s-expression corresponding to that package, or #f on failure."
  (let ((module-meta (cpan-fetch module-name)))
    (and=> module-meta cpan-module->sexp)))
