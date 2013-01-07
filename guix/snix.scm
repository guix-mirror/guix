;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2010, 2011, 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix snix)
  #:use-module (sxml ssax)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (guix utils)
  #:use-module (guix base32)
  #:use-module (guix config)
  #:export (open-nixpkgs
            xml->snix
            nixpkgs->guix-package))

;;; Commentary:
;;;
;;; Converting Nix code to s-expressions, and then to Guix `package'
;;; declarations, using the XML output of `nix-instantiate'.
;;;
;;; Code:


;;;
;;; SNix.
;;;

;; Nix object types visible in the XML output of `nix-instantiate' and
;; mapping to S-expressions (we map to sexps, not records, so that we
;; can do pattern matching):
;;
;;   at               (at varpat attrspat)
;;   attr             (attribute loc name value)
;;   attrs            (attribute-set attributes)
;;   attrspat         (attribute-set-pattern patterns)
;;   bool             #f|#t
;;   derivation       (derivation drv-path out-path attributes)
;;   ellipsis         '...
;;   expr             (snix loc body ...)
;;   function         (function loc at|attrspat|varpat)
;;   int              int
;;   list             list
;;   null             'null
;;   path             string
;;   string           string
;;   unevaluated      'unevaluated
;;   varpat           (varpat name)
;;
;; Initially ATTRIBUTES in `derivation' and `attribute-set' was a promise;
;; however, handling `repeated' nodes makes it impossible to do anything
;; lazily because the whole SXML tree has to be traversed to maintain the
;; list of known derivations.

(define (xml-element->snix elem attributes body derivations)
  "Return an SNix element corresponding to XML element ELEM."

  (define (loc)
    (location (assq-ref attributes 'path)
              (assq-ref attributes 'line)
              (assq-ref attributes 'column)))

  (case elem
    ((at)
     (values `(at ,(car body) ,(cadr body)) derivations))
    ((attr)
     (let ((name (assq-ref attributes 'name)))
       (cond ((null? body)
              (values `(attribute-pattern ,name) derivations))
             ((and (pair? body) (null? (cdr body)))
              (values `(attribute ,(loc) ,name ,(car body))
                      derivations))
             (else
              (error "invalid attribute body" name (loc) body)))))
    ((attrs)
     (values `(attribute-set ,(reverse body)) derivations))
    ((attrspat)
     (values `(attribute-set-pattern ,body) derivations))
    ((bool)
     (values (string-ci=? "true" (assq-ref attributes 'value))
             derivations))
    ((derivation)
     (let ((drv-path (assq-ref attributes 'drvPath))
           (out-path (assq-ref attributes 'outPath)))
       (if (equal? body '(repeated))
           (let ((body (vhash-assoc drv-path derivations)))
             (if (pair? body)
                 (values `(derivation ,drv-path ,out-path ,(cdr body))
                         derivations)

                 ;; DRV-PATH hasn't been encountered yet but may be later
                 ;; (see <http://article.gmane.org/gmane.linux.distributions.nixos/5946>.)
                 ;; Return an `unresolved' node.
                 (values `(unresolved
                           ,(lambda (derivations)
                              (let ((body (vhash-assoc drv-path derivations)))
                                (if (pair? body)
                                    `(derivation ,drv-path ,out-path
                                                 ,(cdr body))
                                    (error "no previous occurrence of derivation"
                                           drv-path)))))
                         derivations)))
           (values `(derivation ,drv-path ,out-path ,body)
                   (vhash-cons drv-path body derivations)))))
    ((ellipsis)
     (values '... derivations))
    ((expr)
     (values `(snix ,(loc) ,@body) derivations))
    ((function)
     (values `(function ,(loc) ,body) derivations))
    ((int)
     (values (string->number (assq-ref attributes 'value))
             derivations))
    ((list)
     (values body derivations))
    ((null)
     (values 'null derivations))
    ((path)
     (values (assq-ref attributes 'value) derivations))
    ((repeated)
     (values 'repeated derivations))
    ((string)
     (values (assq-ref attributes 'value) derivations))
    ((unevaluated)
     (values 'unevaluated derivations))
    ((varpat)
     (values `(varpat ,(assq-ref attributes 'name)) derivations))
    (else (error "unhandled Nix XML element" elem))))

(define (resolve snix derivations)
  "Return a new SNix tree where `unresolved' nodes from SNIX have been
replaced by the result of their application to DERIVATIONS, a vhash."
  (let loop ((node snix)
             (seen vlist-null))
    (if (vhash-assq node seen)
        (values node seen)
        (match node
          (('unresolved proc)
           (let ((n (proc derivations)))
             (values n seen)))
          ((tag body ...)
           (let ((body+seen (fold (lambda (n body+seen)
                                    (call-with-values
                                        (lambda ()
                                          (loop n (cdr body+seen)))
                                      (lambda (n* seen)
                                        (cons (cons n* (car body+seen))
                                              (vhash-consq n #t seen)))))
                                  (cons '() (vhash-consq node #t seen))
                                  body)))
             (values (cons tag (reverse (car body+seen)))
                     (vhash-consq node #t (cdr body+seen)))))
          (anything
           (values anything seen))))))

(define xml->snix
  (let ((parse
         (ssax:make-parser NEW-LEVEL-SEED
                           (lambda (elem-gi attributes namespaces expected-content
                                    seed)
                             (cons '() (cdr seed)))

                           FINISH-ELEMENT
                           (lambda (elem-gi attributes namespaces parent-seed
                                            seed)
                             (let ((snix        (car seed))
                                   (derivations (cdr seed)))
                               (let-values (((snix derivations)
                                             (xml-element->snix elem-gi
                                                                attributes
                                                                snix
                                                                derivations)))
                                 (cons (cons snix (car parent-seed))
                                       derivations))))

                           CHAR-DATA-HANDLER
                           (lambda (string1 string2 seed)
                             ;; Discard inter-node strings, which are blanks.
                             seed))))
    (lambda (port)
      "Return the SNix represention of TREE, an SXML tree as returned by
parsing the XML output of `nix-instantiate' on Nixpkgs."
      (match (parse port (cons '() vlist-null))
        (((snix) . derivations)
         (resolve snix derivations))))))

(define (attribute-value attribute)
  "Return the value of ATTRIBUTE."
  (match attribute
    (('attribute _ _ value) value)))

(define (derivation-source derivation)
  "Return the \"src\" attribute of DERIVATION or #f if not found."
  (match derivation
    (('derivation _ _ (attributes ...))
     (find-attribute-by-name "src" attributes))))

(define (derivation-output-path derivation)
  "Return the output path of DERIVATION."
  (match derivation
    (('derivation _ out-path _)
     out-path)
    (_ #f)))

(define (source-output-path src)
  "Return the output path of SRC, the \"src\" attribute of a derivation."
  (derivation-output-path (attribute-value src)))

(define (source-urls src)
  "Return the URLs of SRC, the \"src\" attribute of a derivation."
  (match src
    (('attribute _ _ ('derivation _ _ (attributes ...)))
     (match (find-attribute-by-name "urls" attributes)
       (('attribute _ _ value)
        value)))
    (_ #f)))

(define (source-sha256 src)
  "Return the sha256 of SRC, the \"src\" attribute of a derivation, as a
bytevector."
  (match src
    (('attribute _ _ ('derivation _ _ (attributes ...)))
     (match (find-attribute-by-name "outputHash" attributes)
       (('attribute _ _ value)
        (match value
          ((= string-length 52)
           (nix-base32-string->bytevector value))
          ((= string-length 64)
           (base16-string->bytevector value))
          (_
           (error "unsupported hash format" value))))))
    (_ #f)))

(define (derivation-source-output-path derivation)
  "Return the output path of the \"src\" attribute of DERIVATION or #f
if DERIVATION lacks an \"src\" attribute."
  (and=> (derivation-source derivation) source-output-path))

(define* (open-nixpkgs nixpkgs #:optional attribute)
  "Return an input pipe to the XML representation of Nixpkgs.  When
ATTRIBUTE is true, only that attribute is considered."
  (with-fluids ((%default-port-encoding "UTF-8"))
    (let ((cross-system (format #f "{
  config = \"i686-guix-linux-gnu\";
  libc = \"glibc\";
  arch = \"guix\";
  withTLS = true;
  float = \"hard\";
  openssl.system = \"linux-generic32\";
  platform = (import ~a/pkgs/top-level/platforms.nix).sheevaplug;
}" nixpkgs)))
      (apply open-pipe* OPEN_READ
             %nix-instantiate "--strict" "--eval-only" "--xml"

             ;; Pass a dummy `crossSystem' argument so that `buildInputs' and
             ;; `buildNativeInputs' are not coalesced.
             ;; XXX: This is hacky and has other problems.
             ;"--arg" "crossSystem" cross-system

             `(,@(if attribute
                     `("-A" ,attribute)
                     '())
               ,nixpkgs)))))

(define (pipe-failed? pipe)
  "Close pipe and return its status if it failed."
  (let ((status (close-pipe pipe)))
    (if (or (status:term-sig status)
            (not (= (status:exit-val status) 0)))
        status
        #f)))

(define (find-attribute-by-name name attributes)
  "Return attribute NAME in ATTRIBUTES, an attribute set or list of SNix
attributes, or #f if NAME cannot be found."
  (find (lambda (a)
          (match a
            (('attribute _ (? (cut string=? <> name)) _)
             a)
            (_ #f)))
        (match attributes
          (('attribute-set (attributes ...))
           attributes)
          (_
           attributes))))

(define (package-source-output-path package)
  "Return the output path of the \"src\" derivation of PACKAGE."
  (derivation-source-output-path (attribute-value package)))


;;;
;;; Conversion of "Nix expressions" to "Guix expressions".
;;;

(define (factorize-uri uri version)
  "Factorize URI, a package tarball URI as a string, such that any occurrences
of the string VERSION is replaced by the symbol 'version."
  (let ((version-rx (make-regexp (regexp-quote version))))
    (match (regexp-exec version-rx uri)
      (#f
       uri)
      (_
       (let ((indices (fold-matches version-rx uri
                                    '((0))
                                    (lambda (m result)
                                      (match result
                                        (((start) rest ...)
                                         `((,(match:end m))
                                           (,start . ,(match:start m))
                                           ,@rest)))))))
         (fold (lambda (index result)
                 (match index
                   ((start)
                    (cons (substring uri start)
                          result))
                   ((start . end)
                    (cons* (substring uri start end)
                           'version
                           result))))
               '()
               indices))))))

(define (snix-derivation->guix-package derivation)
  "Return the `package' s-expression corresponding to SNix DERIVATION, a
Nixpkgs `stdenv.mkDerivation'-style derivation, and the original source
location of DERIVATION."
  (match derivation
    (('derivation _ _ (attributes ...))
     (let*-values (((full-name loc)
                    (match (find-attribute-by-name "name" attributes)
                      (('attribute loc _ value)
                       (values value loc))
                      (_
                       (values #f #f))))
                   ((name version)
                    (package-name->name+version full-name)))
       (define (convert-inputs type)
         ;; Convert the derivation's input from a list of SNix derivations to
         ;; a list of name/variable pairs.
         (match (and=> (find-attribute-by-name type attributes)
                       attribute-value)
           (#f
            '())
           ((inputs ...)
            ;; Inputs can be either derivations or the null value.
            (filter-map (match-lambda
                         (('derivation _ _ (attributes ...))
                          (let* ((full-name
                                  (attribute-value
                                   (find-attribute-by-name "name" attributes)))
                                 (name (package-name->name+version full-name)))
                            (list name
                                  (list 'unquote (string->symbol name)))))
                         ('null #f))
                        inputs))))

       (define (maybe-inputs guix-name inputs)
         (match inputs
           (()
            '())
           ((inputs ...)
            (list (list guix-name
                        (list 'quasiquote inputs))))))

       (define (pretty-uri uri version)
         (match (factorize-uri uri version)
           ((items ...)
            `(string-append ,@items))
           (x x)))

       (let* ((source  (find-attribute-by-name "src" attributes))
              (urls    (source-urls source))
              (sha256  (source-sha256 source))
              (meta    (and=> (find-attribute-by-name "meta" attributes)
                              attribute-value)))
         (values
          `(package
             (name ,name)
             (version ,version)
             (source (origin
                      (method url-fetch)
                      (uri ,(pretty-uri (car urls) version))
                      (sha256
                       (base32
                        ,(bytevector->nix-base32-string sha256)))))
             (build-system gnu-build-system)

             ;; When doing a native Nixpkgs build, `buildInputs' is empty and
             ;; everything is in `buildNativeInputs'.  So we can't distinguish
             ;; between both, here.
             ,@(maybe-inputs 'inputs
                             (convert-inputs "buildNativeInputs"))
             ,@(maybe-inputs 'propagated-inputs
                             (convert-inputs "propagatedBuildNativeInputs"))

             (home-page ,(and=> (find-attribute-by-name "homepage" meta)
                                attribute-value))
             (synopsis ,(and=> (find-attribute-by-name "description" meta)
                               attribute-value))
             (description
              ,(and=> (find-attribute-by-name "longDescription" meta)
                      attribute-value))
             (license ,(and=> (find-attribute-by-name "license" meta)
                              attribute-value)))
          loc))))))

(define (nixpkgs->guix-package nixpkgs attribute)
  "Evaluate ATTRIBUTE in NIXPKGS, the file name of a Nixpkgs checkout,
and return the `package' s-expression corresponding to that package."
  (let ((port (open-nixpkgs nixpkgs attribute)))
    (match (xml->snix port)
      (('snix loc (and drv ('derivation _ ...)))
       (and (not (pipe-failed? port))
            (snix-derivation->guix-package drv)))
      (_
       (not (pipe-failed? port))))))

;;; snix.scm ends here
