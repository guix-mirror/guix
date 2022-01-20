;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix import texlive)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (gcrypt hash)
  #:use-module (guix derivations)
  #:use-module (guix memoization)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix base32)
  #:use-module (guix serialization)
  #:use-module (guix svn-download)
  #:use-module (guix import utils)
  #:use-module (guix utils)
  #:use-module (guix upstream)
  #:use-module (guix packages)
  #:use-module (guix build-system texlive)
  #:export (files-differ?
            texlive->guix-package
            texlive-recursive-import))

;;; Commentary:
;;;
;;; Generate a package declaration template for corresponding package in the
;;; Tex Live Package Database (tlpdb).  We fetch all sources from different
;;; locations in the SVN repository of the Texlive project.
;;;
;;; Code:

(define string->license
  (match-lambda
    ("artistic2" 'gpl3+)
    ("gpl" 'gpl3+)
    ("gpl1" 'gpl1)
    ("gpl1+" 'gpl1+)
    ("gpl2" 'gpl2)
    ("gpl2+" 'gpl2+)
    ("gpl3" 'gpl3)
    ("gpl3+" 'gpl3+)
    ("lgpl2.1" 'lgpl2.1)
    ("lgpl3" 'lgpl3)
    ("knuth" 'knuth)
    ("pd" 'public-domain)
    ("bsd2" 'bsd-2)
    ("bsd3" 'bsd-3)
    ("bsd4" 'bsd-4)
    ("opl" 'opl1.0+)
    ("ofl" 'silofl1.1)

    ("lpplgpl" `(list lppl gpl1+))
    ("lppl" 'lppl)
    ("lppl1" 'lppl1.0+) ; usually means "or later"
    ("lppl1.2" 'lppl1.2+) ; usually means "or later"
    ("lppl1.3" 'lppl1.3+) ; usually means "or later"
    ("lppl1.3a" 'lppl1.3a)
    ("lppl1.3b" 'lppl1.3b)
    ("lppl1.3c" 'lppl1.3c)
    ("cc-by-2" 'cc-by-2.0)
    ("cc-by-3" 'cc-by-3.0)
    ("cc-by-sa-2" 'cc-by-sa2.0)
    ("cc-by-sa-3" 'cc-by-sa3.0)
    ("mit" 'expat)
    ("fdl" 'fdl1.3+)
    ("gfl" 'gfl1.0)

    ;; These are known non-free licenses
    ("noinfo" 'unknown)
    ("nosell" 'non-free)
    ("shareware" 'non-free)
    ("nosource" 'non-free)
    ("nocommercial" 'non-free)
    ("cc-by-nc-nd-1" 'non-free)
    ("cc-by-nc-nd-2" 'non-free)
    ("cc-by-nc-nd-2.5" 'non-free)
    ("cc-by-nc-nd-3" 'non-free)
    ("cc-by-nc-nd-4" 'non-free)
    ((x) (string->license x))
    ((lst ...) `(list ,@(map string->license lst)))
    (x `(error unknown-license ,x))))

(define (guix-name name)
  "Return a Guix package name for a given Texlive package NAME."
  (string-append "texlive-"
                 (string-map (match-lambda
                               (#\_ #\-)
                               (#\. #\-)
                               (chr (char-downcase chr)))
                             name)))

(define (tlpdb-file)
  (define texlive-bin
    ;; Resolve this variable lazily so that (gnu packages ...) does not end up
    ;; in the closure of this module.
    (module-ref (resolve-interface '(gnu packages tex))
                'texlive-bin))

  (with-store store
    (run-with-store store
      (mlet* %store-monad
          ((drv (lower-object texlive-bin))
           (built (built-derivations (list drv))))
        (match (derivation->output-paths drv)
          (((names . items) ...)
           (return (string-append (first items)
                                  "/share/tlpkg/texlive.tlpdb"))))))))

(define tlpdb
  (memoize
   (lambda ()
     (let ((file (tlpdb-file))
           (fields
            '((name     . string)
              (shortdesc . string)
              (longdesc . string)
              (catalogue-license . string)
              (catalogue-ctan . string)
              (srcfiles . list)
              (runfiles . list)
              (docfiles . list)
              (depend   . simple-list)))
           (record
            (lambda* (key value alist #:optional (type 'string))
              (let ((new
                     (or (and=> (assoc-ref alist key)
                                (lambda (existing)
                                  (cond
                                   ((eq? type 'string)
                                    (string-append existing " " value))
                                   ((or (eq? type 'list) (eq? type 'simple-list))
                                    (cons value existing)))))
                         (cond
                          ((eq? type 'string)
                           value)
                          ((or (eq? type 'list) (eq? type 'simple-list))
                           (list value))))))
                (acons key new (alist-delete key alist))))))
       (call-with-input-file file
         (lambda (port)
           (let loop ((all (list))
                      (current (list))
                      (last-property #false))
             (let ((line (read-line port)))
               (cond
                ((eof-object? line) all)

                ;; End of record.
                ((string-null? line)
                 (loop (cons (cons (assoc-ref current 'name) current)
                             all)
                       (list) #false))

                ;; Continuation of a list
                ((and (zero? (string-index line #\space)) last-property)
                 ;; Erase optional second part of list values like
                 ;; "details=Readme" for files
                 (let ((plain-value (first
                                     (string-split
                                      (string-trim-both line) #\space))))
                   (loop all (record last-property
                                     plain-value
                                     current
                                     'list)
                         last-property)))
                (else
                 (or (and-let* ((space (string-index line #\space))
                                (key   (string->symbol (string-take line space)))
                                (value (string-drop line (1+ space)))
                                (field-type (assoc-ref fields key)))
                       ;; Erase second part of list keys like "size=29"
                       (cond
                        ((eq? field-type 'list)
                         (loop all current key))
                        (else
                         (loop all (record key value current field-type) key))))
                     (loop all current #false))))))))))))

(define* (files-differ? directory package-name
                        #:key
                        (package-database tlpdb)
                        (type #false)
                        (direction 'missing))
  "Return a list of files in DIRECTORY that differ from the expected installed
files for PACKAGE-NAME according to the PACKAGE-DATABASE.  By default all
files considered, but this can be restricted by setting TYPE to 'runfiles,
'docfiles, or 'srcfiles.  The names of files that are missing from DIRECTORY
are returned; by setting DIRECTION to anything other than 'missing, the names
of those files are returned that are unexpectedly installed."
  (define (strip-directory-prefix file-name)
    (string-drop file-name (1+ (string-length directory))))
  (let* ((data (or (assoc-ref (package-database) package-name)
                   (error (format #false
                                  "~a is not a valid package name in the TeX Live package database."
                                  package-name))))
         (files (if type
                    (or (assoc-ref data type) (list))
                    (append (or (assoc-ref data 'runfiles) (list))
                            (or (assoc-ref data 'docfiles) (list))
                            (or (assoc-ref data 'srcfiles) (list)))))
         (existing (file-system-fold
                    (const #true)                             ;enter?
                    (lambda (path stat result) (cons path result)) ;leaf
                    (lambda (path stat result) result)             ;down
                    (lambda (path stat result) result)             ;up
                    (lambda (path stat result) result)             ;skip
                    (lambda (path stat errno result) result)       ;error
                    (list)
                    directory)))
    (if (eq? direction 'missing)
        (lset-difference string=?
                         files (map strip-directory-prefix existing))
        ;; List files that are installed but should not be.
        (lset-difference string=?
                         (map strip-directory-prefix existing) files))))

(define (files->directories files)
  (define name->parts (cut string-split <> #\/))
  (map (cut string-join <> "/" 'suffix)
       (delete-duplicates (map (lambda (file)
                                 (drop-right (name->parts file) 1))
                               (sort files string<))
                          ;; Remove sub-directories, i.e. more specific
                          ;; entries with the same prefix.
                          (lambda (x y) (every equal? x y)))))

(define (tlpdb->package name package-database)
  (and-let* ((data (assoc-ref package-database name))
             (dirs (files->directories
                    (map (lambda (dir)
                           (string-drop dir (string-length "texmf-dist/")))
                         (append (or (assoc-ref data 'docfiles) (list))
                                 (or (assoc-ref data 'runfiles) (list))
                                 (or (assoc-ref data 'srcfiles) (list))))))
             (name (guix-name name))
             (version (number->string %texlive-revision))
             (ref (svn-multi-reference
                   (url (string-append "svn://www.tug.org/texlive/tags/"
                                       %texlive-tag "/Master/texmf-dist"))
                   (locations dirs)
                   (revision %texlive-revision)))
             (source (with-store store
                       (download-multi-svn-to-store
                        store ref (string-append name "-svn-multi-checkout")))))
    (values
     `(package
        (inherit (simple-texlive-package
                  ,name
                  (list ,@dirs)
                  (base32
                   ,(bytevector->nix-base32-string
                     (let-values (((port get-hash) (open-sha256-port)))
                       (write-file source port)
                       (force-output port)
                       (get-hash))))
                  ,@(if (assoc-ref data 'srcfiles) '() '(#:trivial? #true))))
        ,@(or (and=> (assoc-ref data 'depend)
                     (lambda (inputs)
                       `((propagated-inputs
                          (list ,@(map (lambda (tex-name)
                                         (let ((name (guix-name tex-name)))
                                           (string->symbol name)))
                                       inputs))))))
              '())
        ,@(or (and=> (assoc-ref data 'catalogue-ctan)
                     (lambda (url)
                       `((home-page ,(string-append "https://ctan.org" url)))))
              '((home-page "https://www.tug.org/texlive/")))
        (synopsis ,(assoc-ref data 'shortdesc))
        (description ,(beautify-description
                       (assoc-ref data 'longdesc)))
        (license ,(string->license
                   (assoc-ref data 'catalogue-license))))
     (or (assoc-ref data 'depend) (list)))))

(define texlive->guix-package
  (memoize
   (lambda* (name #:key repo version (package-database tlpdb))
     "Find the metadata for NAME in the tlpdb and return the `package'
s-expression corresponding to that package, or #f on failure."
     (tlpdb->package name (package-database)))))

(define (texlive-recursive-import name)
  (recursive-import name
                    #:repo->guix-package texlive->guix-package
                    #:guix-name guix-name))

;;; texlive.scm ends here
