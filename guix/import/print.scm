;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix import print)
  #:use-module (guix base32)
  #:use-module (guix utils)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (gnu packages)
  #:use-module (srfi srfi-1)
  #:use-module (guix import utils)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:export (package->code))

;; FIXME: the quasiquoted arguments field may contain embedded package
;; objects, e.g. in #:disallowed-references; they will just be printed with
;; their usual #<package ...> representation, not as variable names.
(define (package->code package)
  "Return an S-expression representing the source code that produces PACKAGE
when evaluated."
  ;; The module in which the package PKG is defined
  (define (package-module-name pkg)
    (map string->symbol
         (string-split (string-drop-right
                        (location-file (package-location pkg)) 4)
                       #\/)))

  ;; Return the first candidate variable name that is bound to VAL.
  (define (variable-name val mod)
    (match (let/ec return
             (module-for-each (lambda (sym var)
                                (if (eq? val (variable-ref var))
                                    (return sym)
                                    #f))
                              (resolve-interface mod)))
      ((? symbol? sym) sym)
      (_ #f)))

  ;; Print either license variable name or the code for a license object
  (define (license->code lic)
    (let ((var (variable-name lic '(guix licenses))))
      (if var
          (symbol-append 'license: var)
          `(license
            (name ,(license-name lic))
            (uri ,(license-uri lic))
            (comment ,(license-comment lic))))))

  (define (search-path-specification->code spec)
    `(search-path-specification
      (variable ,(search-path-specification-variable spec))
      (files (list ,@(search-path-specification-files spec)))
      (separator ,(search-path-specification-separator spec))
      (file-type (quote ,(search-path-specification-file-type spec)))
      (file-pattern ,(search-path-specification-file-pattern spec))))

  (define (source->code source version)
    (let ((uri       (origin-uri source))
          (method    (origin-method source))
          (hash      (origin-hash source))
          (file-name (origin-file-name source))
          (patches   (origin-patches source)))
      `(origin
         ;; Since 'procedure-name' returns the procedure name within the
         ;; module where it's defined, not its public name.  Thus, try hard to
         ;; find its public name and use 'procedure-name' as a last resort.
         (method ,(or (any (lambda (module)
                             (variable-name method module))
                           '((guix download)
                             (guix git-download)
                             (guix hg-download)
                             (guix svn-download)))
                      (procedure-name method)))
         (uri (string-append ,@(match (factorize-uri uri version)
                                 ((? string? uri) (list uri))
                                 (factorized factorized))))
         ,(if (equal? (content-hash-algorithm hash) 'sha256)
              `(sha256 (base32 ,(bytevector->nix-base32-string
                                 (content-hash-value hash))))
              `(hash (content-hash ,(bytevector->nix-base32-string
                                     (content-hash-value hash))
                                   ,(content-hash-algorithm hash))))
         ;; FIXME: in order to be able to throw away the directory prefix,
         ;; we just assume that the patch files can be found with
         ;; "search-patches".
         ,@(if (null? patches) '()
               `((patches (search-patches ,@(map basename patches))))))))

  (define (package-lists->code lsts)
    (list 'quasiquote
          (map (match-lambda
                 ((? symbol? s)
                  (list (symbol->string s) (list 'unquote s)))
                 ((label pkg . out)
                  (let ((mod (package-module-name pkg)))
                    (cons* label
                           ;; FIXME: using '@ certainly isn't pretty, but it
                           ;; avoids having to import the individual package
                           ;; modules.
                           (list 'unquote
                                 (list '@ mod (variable-name pkg mod)))
                           out))))
               lsts)))

  (let ((name                (package-name package))
        (version             (package-version package))
        (source              (package-source package))
        (build-system        (package-build-system package))
        (arguments           (package-arguments package))
        (inputs              (package-inputs package))
        (propagated-inputs   (package-propagated-inputs package))
        (native-inputs       (package-native-inputs package))
        (outputs             (package-outputs package))
        (native-search-paths (package-native-search-paths package))
        (search-paths        (package-search-paths package))
        (replacement         (package-replacement package))
        (synopsis            (package-synopsis package))
        (description         (package-description package))
        (license             (package-license package))
        (home-page           (package-home-page package))
        (supported-systems   (package-supported-systems package))
        (properties          (package-properties package)))
    `(define-public ,(string->symbol name)
       (package
         (name ,name)
         (version ,version)
         (source ,(source->code source version))
         ,@(match properties
             (() '())
             (_  `((properties ,properties))))
         ,@(if replacement
               `((replacement ,replacement))
               '())
         (build-system (@ (guix build-system ,(build-system-name build-system))
                          ,(symbol-append (build-system-name build-system)
                                          '-build-system)))
         ,@(match arguments
             (() '())
             (args `((arguments ,(list 'quasiquote args)))))
         ,@(match outputs
             (("out") '())
             (outs `((outputs (list ,@outs)))))
         ,@(match native-inputs
             (() '())
             (pkgs `((native-inputs ,(package-lists->code pkgs)))))
         ,@(match inputs
             (() '())
             (pkgs `((inputs ,(package-lists->code pkgs)))))
         ,@(match propagated-inputs
             (() '())
             (pkgs `((propagated-inputs ,(package-lists->code pkgs)))))
         ,@(if (lset= string=? supported-systems %supported-systems)
               '()
               `((supported-systems (list ,@supported-systems))))
         ,@(match (map search-path-specification->code native-search-paths)
             (() '())
             (paths `((native-search-paths (list ,@paths)))))
         ,@(match (map search-path-specification->code search-paths)
             (() '())
             (paths `((search-paths (list ,@paths)))))
         (home-page ,home-page)
         (synopsis ,synopsis)
         (description ,description)
         (license ,(if (list? license)
                       `(list ,@(map license->code license))
                       (license->code license)))))))
