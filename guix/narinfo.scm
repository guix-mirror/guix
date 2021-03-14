;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2018 Kyle Meyer <kyle@kyleam.com>
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

(define-module (guix narinfo)
  #:use-module (guix pki)
  #:use-module (guix i18n)
  #:use-module (guix base32)
  #:use-module (guix base64)
  #:use-module (guix records)
  #:use-module (guix diagnostics)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt pk-crypto)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (web uri)
  #:export (narinfo-signature->canonical-sexp

            narinfo?
            narinfo-path
            narinfo-uris
            narinfo-uri-base
            narinfo-compressions
            narinfo-file-hashes
            narinfo-file-sizes
            narinfo-hash
            narinfo-size
            narinfo-references
            narinfo-deriver
            narinfo-system
            narinfo-signature
            narinfo-contents

            narinfo-hash-algorithm+value

            narinfo-hash->sha256
            narinfo-best-uri

            valid-narinfo?

            read-narinfo
            write-narinfo

            string->narinfo
            narinfo->string

            equivalent-narinfo?))

(define-record-type <narinfo>
  (%make-narinfo path uri-base uris compressions file-sizes file-hashes
                 nar-hash nar-size references deriver system
                 signature contents)
  narinfo?
  (path         narinfo-path)
  (uri-base     narinfo-uri-base)        ;URI of the cache it originates from
  (uris         narinfo-uris)            ;list of strings
  (compressions narinfo-compressions)    ;list of strings
  (file-sizes   narinfo-file-sizes)      ;list of (integers | #f)
  (file-hashes  narinfo-file-hashes)
  (nar-hash     narinfo-hash)
  (nar-size     narinfo-size)
  (references   narinfo-references)
  (deriver      narinfo-deriver)
  (system       narinfo-system)
  (signature    narinfo-signature)      ; canonical sexp
  ;; The original contents of a narinfo file.  This field is needed because we
  ;; want to preserve the exact textual representation for verification purposes.
  ;; See <https://lists.gnu.org/archive/html/guix-devel/2014-02/msg00340.html>
  ;; for more information.
  (contents     narinfo-contents))

(define (narinfo-hash-algorithm+value narinfo)
  "Return two values: the hash algorithm used by NARINFO and its value as a
bytevector."
  (match (string-tokenize (narinfo-hash narinfo)
                          (char-set-complement (char-set #\:)))
    ((algorithm base32)
     (values (lookup-hash-algorithm (string->symbol algorithm))
             (nix-base32-string->bytevector base32)))
    (_
     (raise (formatted-message
             (G_ "invalid narinfo hash: ~s") (narinfo-hash narinfo))))))

(define (narinfo-hash->sha256 hash)
  "If the string HASH denotes a sha256 hash, return it as a bytevector.
Otherwise return #f."
  (and (string-prefix? "sha256:" hash)
       (nix-base32-string->bytevector (string-drop hash 7))))

(define (narinfo-signature->canonical-sexp str)
  "Return the value of a narinfo's 'Signature' field as a canonical sexp."
  (match (string-split str #\;)
    ((version host-name sig)
     (let ((maybe-number (string->number version)))
       (cond ((not (number? maybe-number))
              (leave (G_ "signature version must be a number: ~s~%")
                     version))
             ;; Currently, there are no other versions.
             ((not (= 1 maybe-number))
              (leave (G_ "unsupported signature version: ~a~%")
                     maybe-number))
             (else
              (let ((signature (utf8->string (base64-decode sig))))
                (catch 'gcry-error
                  (lambda ()
                    (string->canonical-sexp signature))
                  (lambda (key proc err)
                    (leave (G_ "signature is not a valid \
s-expression: ~s~%")
                           signature))))))))
    (x
     (leave (G_ "invalid format of the signature field: ~a~%") x))))

(define (narinfo-maker str cache-url)
  "Return a narinfo constructor for narinfos originating from CACHE-URL.  STR
must contain the original contents of a narinfo file."
  (lambda (path urls compressions file-hashes file-sizes
                nar-hash nar-size references deriver system
                signature)
    "Return a new <narinfo> object."
    (define len (length urls))
    (%make-narinfo path cache-url
                   ;; Handle the case where URL is a relative URL.
                   (map (lambda (url)
                          (or (string->uri url)
                              (string->uri
                               (string-append cache-url "/" url))))
                        urls)
                   compressions
                   (match file-sizes
                     (()        (make-list len #f))
                     ((lst ...) (map string->number lst)))
                   (match file-hashes
                     (()        (make-list len #f))
                     ((lst ...) (map string->number lst)))
                   nar-hash
                   (and=> nar-size string->number)
                   (string-tokenize references)
                   (match deriver
                     ((or #f "") #f)
                     (_ deriver))
                   system
                   (false-if-exception
                    (and=> signature narinfo-signature->canonical-sexp))
                   str)))

(define fields->alist
  ;; The narinfo format is really just like recutils.
  recutils->alist)

(define* (read-narinfo port #:optional url
                       #:key size)
  "Read a narinfo from PORT.  If URL is true, it must be a string used to
build full URIs from relative URIs found while reading PORT.  When SIZE is
true, read at most SIZE bytes from PORT; otherwise, read as much as possible.

No authentication and authorization checks are performed here!"
  (let ((str (utf8->string (if size
                               (get-bytevector-n port size)
                               (get-bytevector-all port)))))
    (alist->record (call-with-input-string str fields->alist)
                   (narinfo-maker str url)
                   '("StorePath" "URL" "Compression"
                     "FileHash" "FileSize" "NarHash" "NarSize"
                     "References" "Deriver" "System"
                     "Signature")
                   '("URL" "Compression" "FileSize" "FileHash"))))

(define (narinfo-sha256 narinfo)
  "Return the sha256 hash of NARINFO as a bytevector, or #f if NARINFO lacks a
'Signature' field."
  (define %mandatory-fields
    ;; List of fields that must be signed.  If they are not signed, the
    ;; narinfo is considered unsigned.
    '("StorePath" "NarHash" "References"))

  (let ((contents (narinfo-contents narinfo)))
    (match (string-contains contents "Signature:")
      (#f #f)
      (index
       (let* ((above-signature (string-take contents index))
              (signed-fields (match (call-with-input-string above-signature
                                      fields->alist)
                               (((fields . values) ...) fields))))
         (and (every (cut member <> signed-fields) %mandatory-fields)
              (sha256 (string->utf8 above-signature))))))))

(define* (valid-narinfo? narinfo #:optional (acl (current-acl))
                         #:key verbose?)
  "Return #t if NARINFO's signature is not valid."
  (let ((hash      (narinfo-sha256 narinfo))
        (signature (narinfo-signature narinfo))
        (uri       (uri->string (first (narinfo-uris narinfo)))))
    (and hash signature
         (signature-case (signature hash acl)
           (valid-signature #t)
           (invalid-signature
            (when verbose?
              (format (current-error-port)
                      "invalid signature for substitute at '~a'~%"
                      uri))
            #f)
           (hash-mismatch
            (when verbose?
              (format (current-error-port)
                      "hash mismatch for substitute at '~a'~%"
                      uri))
            #f)
           (unauthorized-key
            (when verbose?
              (format (current-error-port)
                      "substitute at '~a' is signed by an \
unauthorized party~%"
                      uri))
            #f)
           (corrupt-signature
            (when verbose?
              (format (current-error-port)
                      "corrupt signature for substitute at '~a'~%"
                      uri))
            #f)))))

(define (write-narinfo narinfo port)
  "Write NARINFO to PORT."
  (put-bytevector port (string->utf8 (narinfo-contents narinfo))))

(define (narinfo->string narinfo)
  "Return the external representation of NARINFO."
  (call-with-output-string (cut write-narinfo narinfo <>)))

(define (string->narinfo str cache-uri)
  "Return the narinfo represented by STR.  Assume CACHE-URI as the base URI of
the cache STR originates form."
  (call-with-input-string str (cut read-narinfo <> cache-uri)))

(define (equivalent-narinfo? narinfo1 narinfo2)
  "Return true if NARINFO1 and NARINFO2 are equivalent--i.e., if they describe
the same store item.  This ignores unnecessary metadata such as the Nar URL."
  (and (string=? (narinfo-hash narinfo1)
                 (narinfo-hash narinfo2))

       ;; The following is not needed if all we want is to download a valid
       ;; nar, but it's necessary if we want valid narinfo.
       (string=? (narinfo-path narinfo1)
                 (narinfo-path narinfo2))
       (equal? (narinfo-references narinfo1)
               (narinfo-references narinfo2))

       (= (narinfo-size narinfo1)
          (narinfo-size narinfo2))))

(define %compression-methods
  ;; Known compression methods and a thunk to determine whether they're
  ;; supported.  See 'decompressed-port' in (guix utils).
  `(("gzip"  . ,(const #t))
    ("lzip"  . ,(const #t))
    ("zstd"  . ,(lambda ()
                  (resolve-module '(zstd) #t #f #:ensure #f)))
    ("xz"    . ,(const #t))
    ("bzip2" . ,(const #t))
    ("none"  . ,(const #t))))

(define (supported-compression? compression)
  "Return true if COMPRESSION, a string, denotes a supported compression
method."
  (match (assoc-ref %compression-methods compression)
    (#f         #f)
    (supported? (supported?))))

(define (compresses-better? compression1 compression2)
  "Return true if COMPRESSION1 generally compresses better than COMPRESSION2;
this is a rough approximation."
  (match compression1
    ("none" #f)
    ("gzip" (string=? compression2 "none"))
    ("lzip" #t)
    (_      (or (string=? compression2 "none")
                (string=? compression2 "gzip")))))

(define (decompresses-faster? compression1 compression2)
  "Return true if COMPRESSION1 generally has a higher decompression throughput
than COMPRESSION2."
  (match compression1
    ("none" #t)
    ("zstd" #t)
    ("gzip" (string=? compression2 "lzip"))
    (_      #f)))

(define* (narinfo-best-uri narinfo #:key fast-decompression?)
  "Select the \"best\" URI to download NARINFO's nar, and return three values:
the URI, its compression method (a string), and the compressed file size.
When FAST-DECOMPRESSION? is true, prefer substitutes with faster
decompression (typically zstd) rather than substitutes with a higher
compression ratio (typically lzip)."
  (define choices
    (filter (match-lambda
              ((uri compression file-size)
               (supported-compression? compression)))
            (zip (narinfo-uris narinfo)
                 (narinfo-compressions narinfo)
                 (narinfo-file-sizes narinfo))))

  (define (file-size<? c1 c2)
    (match c1
      ((uri1 compression1 (? integer? file-size1))
       (match c2
         ((uri2 compression2 (? integer? file-size2))
          (< file-size1 file-size2))
         (_ #t)))
      ((uri compression1 #f)
       (match c2
         ((uri2 compression2 _)
          (compresses-better? compression1 compression2))))
      (_ #f)))                                    ;we can't tell

  (define (speed<? c1 c2)
    (match c1
      ((uri1 compression1 . _)
       (match c2
         ((uri2 compression2 . _)
          (decompresses-faster? compression2 compression1))))))

  (match (sort choices (if fast-decompression? (negate speed<?) file-size<?))
    (((uri compression file-size) _ ...)
     (values uri compression file-size))))
