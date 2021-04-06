;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2017, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-substitute)
  #:use-module (guix scripts substitute)
  #:use-module (guix narinfo)
  #:use-module (guix base64)
  #:use-module (gcrypt hash)
  #:use-module (guix serialization)
  #:use-module (gcrypt pk-crypto)
  #:use-module (guix pki)
  #:use-module (guix config)
  #:use-module (guix base32)
  #:use-module ((guix store) #:select (%store-prefix))
  #:use-module ((guix ui) #:select (guix-warning-port))
  #:use-module ((guix utils)
                #:select (call-with-temporary-directory
                          call-with-compressed-output-port))
  #:use-module ((guix build utils)
                #:select (mkdir-p delete-file-recursively dump-port))
  #:use-module (guix tests http)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (web uri)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module ((srfi srfi-64) #:hide (test-error)))

(define-syntax-rule (test-quit name error-rx exp)
  "Emit a test that passes when EXP throws to 'quit' with value 1, and when
it writes to GUIX-WARNING-PORT a messages that matches ERROR-RX."
  (test-equal name
    '(1 #t)
    (let ((error-output (open-output-string)))
      (parameterize ((current-error-port error-output)
                     (guix-warning-port error-output))
        (catch 'quit
          (lambda ()
            exp
            #f)
          (lambda (key value)
            (list value
                  (let ((message (get-output-string error-output)))
                    (->bool (string-match error-rx message))))))))))

(define (request-substitution item destination)
  "Run 'guix substitute --substitute' to fetch ITEM to DESTINATION."
  (parameterize ((guix-warning-port (current-error-port)))
    (with-input-from-string (string-append "substitute " item " "
                                           destination "\n")
      (lambda ()
        (guix-substitute "--substitute")))))

(define %public-key
  ;; This key is known to be in the ACL by default.
  (call-with-input-file (string-append %config-directory "/signing-key.pub")
    (compose string->canonical-sexp get-string-all)))

(define %private-key
  (call-with-input-file (string-append %config-directory "/signing-key.sec")
    (compose string->canonical-sexp get-string-all)))

(define* (signature-body bv #:key (public-key %public-key))
  "Return the signature of BV as the base64-encoded body of a narinfo's
'Signature' field."
  (base64-encode
   (string->utf8
    (canonical-sexp->string
     (signature-sexp (bytevector->hash-data (sha256 bv)
                                            #:key-type 'rsa)
                     %private-key
                     public-key)))))

(define %wrong-public-key
  (string->canonical-sexp "(public-key
 (rsa
  (n #00E05873AC2B168760343145918E954EE9AB73C026355693B192E01EE835261AA689E9EF46642E895BCD65C648524059FC450E4BA77A68F4C52D0E39EF0CC9359709AB6AAB153B63782201871325B0FDA19CB401CD99FD0C31A91CA9000AA90A77E82B89E036FB63BC1D3961207469B3B12468977148D376F8012BB12A4B11A8F1#)
  (e #010001#)
  )
 )"))

(define* (signature-field bv-or-str
                          #:key (version "1") (public-key %public-key))
  "Return the 'Signature' field value of bytevector/string BV-OR-STR, using
PUBLIC-KEY as the signature's principal, and using VERSION as the signature
version identifier.."
  (string-append version ";example.gnu.org;"
                 (signature-body (if (string? bv-or-str)
                                     (string->utf8 bv-or-str)
                                     bv-or-str)
                                 #:public-key public-key)))



(test-begin "substitute")

(test-quit "not a number"
    "signature version"
  (narinfo-signature->canonical-sexp
   (signature-field "foo" #:version "not a number")))

(test-quit "wrong version number"
    "unsupported.*version"
  (narinfo-signature->canonical-sexp
   (signature-field "foo" #:version "2")))

(test-assert "valid narinfo-signature->canonical-sexp"
  (canonical-sexp? (narinfo-signature->canonical-sexp (signature-field "foo"))))



(define %main-substitute-directory
  ;; The place where 'call-with-narinfo' stores its data by default.
  (uri-path (string->uri (getenv "GUIX_BINARY_SUBSTITUTE_URL"))))

(define %alternate-substitute-directory
  ;; Another place.
  (string-append (dirname %main-substitute-directory)
                 "/substituter-alt-data"))

(define %narinfo
  ;; Skeleton of the narinfo used below.
  (string-append "StorePath: " (%store-prefix)
                 "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo
URL: example.nar
Compression: none
NarHash: sha256:" (bytevector->nix-base32-string
                   (sha256 (string->utf8 "Substitutable data."))) "
NarSize: 42
References: bar baz
Deriver: " (%store-prefix) "/foo.drv
System: mips64el-linux\n"))

(define* (call-with-narinfo narinfo thunk
                            #:optional
                            (narinfo-directory %main-substitute-directory))
  "Call THUNK in a context where the directory at URL is populated with
a file for NARINFO."
  (mkdir-p narinfo-directory)
  (let ((cache-directory (string-append (getenv "XDG_CACHE_HOME")
                                        "/guix/substitute/")))
    (dynamic-wind
      (lambda ()
        (when (file-exists? cache-directory)
          (delete-file-recursively cache-directory))
        (call-with-output-file (string-append narinfo-directory
                                              "/nix-cache-info")
          (lambda (port)
            (format port "StoreDir: ~a\nWantMassQuery: 0\n"
                    (%store-prefix))))
        (call-with-output-file (string-append narinfo-directory "/"
                                              "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                              ".narinfo")
          (cut display narinfo <>))

        ;; Prepare the nar.
        (call-with-output-file
            (string-append narinfo-directory "/example.out")
          (cut display "Substitutable data." <>))
        (call-with-output-file
            (string-append narinfo-directory "/example.nar")
          (cute write-file
                (string-append narinfo-directory "/example.out") <>))

        (%allow-unauthenticated-substitutes? #f))
      thunk
      (lambda ()
        (when (file-exists? cache-directory)
          (delete-file-recursively cache-directory))))))

(define-syntax-rule (with-narinfo narinfo body ...)
  (call-with-narinfo narinfo (lambda () body ...)))

(define-syntax-rule (with-narinfo* narinfo directory body ...)
  (call-with-narinfo narinfo (lambda () body ...) directory))

;; Transmit these options to 'guix substitute'.
(substitute-urls (list (getenv "GUIX_BINARY_SUBSTITUTE_URL")))

;; Never use file descriptor 4, unlike what happens when invoked by the
;; daemon.
(%reply-file-descriptor #f)


(test-equal "query narinfo without signature"
  ""                                              ; not substitutable

  (with-narinfo %narinfo
    (string-trim-both
     (with-output-to-string
       (lambda ()
         (with-input-from-string (string-append "have " (%store-prefix)
                                                "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
           (lambda ()
             (guix-substitute "--query"))))))))

(test-equal "query narinfo with invalid hash"
  ;; The hash in the signature differs from the hash of %NARINFO.
  ""

  (with-narinfo (string-append %narinfo "Signature: "
                               (signature-field "different body")
                               "\n")
    (string-trim-both
     (with-output-to-string
       (lambda ()
         (with-input-from-string (string-append "have " (%store-prefix)
                                                "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
           (lambda ()
             (guix-substitute "--query"))))))))

(test-equal "query narinfo with signature over nothing"
  ;; The signature is computed over the empty string, not over the important
  ;; parts, so the narinfo must be ignored.
  ""

  (with-narinfo (string-append "Signature: " (signature-field "") "\n"
                                %narinfo "\n")
    (string-trim-both
     (with-output-to-string
       (lambda ()
         (with-input-from-string (string-append "have " (%store-prefix)
                                                "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
           (lambda ()
             (guix-substitute "--query"))))))))

(test-equal "query narinfo with signature over irrelevant bits"
  ;; The signature is valid but it does not cover the
  ;; StorePath/NarHash/References tuple and is thus irrelevant; the narinfo
  ;; must be ignored.
  ""

  (let ((prefix (string-append "StorePath: " (%store-prefix)
                               "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo
URL: example.nar
Compression: none\n")))
    (with-narinfo (string-append prefix
                                 "Signature: " (signature-field prefix) "
NarHash: sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
NarSize: 42
References: bar baz
Deriver: " (%store-prefix) "/foo.drv
System: mips64el-linux\n")
      (string-trim-both
       (with-output-to-string
         (lambda ()
           (with-input-from-string (string-append "have " (%store-prefix)
                                                  "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
             (lambda ()
               (guix-substitute "--query")))))))))

(test-equal "query narinfo signed with authorized key"
  (string-append (%store-prefix) "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")

  (with-narinfo (string-append %narinfo "Signature: "
                               (signature-field %narinfo)
                               "\n")
    (string-trim-both
     (with-output-to-string
       (lambda ()
         (with-input-from-string (string-append "have " (%store-prefix)
                                                "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
           (lambda ()
             (guix-substitute "--query"))))))))

(test-equal "query narinfo signed with unauthorized key"
  ""                                              ; not substitutable

  (with-narinfo (string-append %narinfo "Signature: "
                               (signature-field
                                %narinfo
                                #:public-key %wrong-public-key)
                               "\n")
    (string-trim-both
     (with-output-to-string
       (lambda ()
         (with-input-from-string (string-append "have " (%store-prefix)
                                                "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
           (lambda ()
             (guix-substitute "--query"))))))))

(test-quit "substitute, no signature"
    "no valid substitute"
  (with-narinfo %narinfo
    (with-input-from-string (string-append "substitute "
                                           (%store-prefix)
                                           "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo"
                                           " foo\n")
      (lambda ()
        (guix-substitute "--substitute")))))

(test-quit "substitute, invalid narinfo hash"
    "no valid substitute"
  ;; The hash in the signature differs from the hash of %NARINFO.
  (with-narinfo (string-append %narinfo "Signature: "
                               (signature-field "different body")
                               "\n")
    (with-input-from-string (string-append "substitute "
                                           (%store-prefix)
                                           "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo"
                                           " foo\n")
      (lambda ()
        (guix-substitute "--substitute")))))

(test-equal "substitute, invalid hash"
  (string-append "hash-mismatch sha256 "
                 (bytevector->nix-base32-string (sha256 #vu8())) " "
                 (let-values (((port get-hash)
                               (open-hash-port (hash-algorithm sha256)))
                              ((content)
                               "Substitutable data."))
                   (write-file-tree "foo" port
                                    #:file-type+size
                                    (lambda _
                                      (values 'regular
                                              (string-length content)))
                                    #:file-port
                                    (lambda _
                                      (open-input-string content)))
                   (close-port port)
                   (bytevector->nix-base32-string (get-hash)))
                 "\n")

  ;; Arrange so the actual data hash does not match the 'NarHash' field in the
  ;; narinfo.
  (with-output-to-string
    (lambda ()
      (let ((narinfo (string-append "StorePath: " (%store-prefix)
                                    "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-wrong-hash
URL: example.nar
Compression: none
NarHash: sha256:" (bytevector->nix-base32-string (sha256 #vu8())) "
NarSize: 42
References: 
Deriver: " (%store-prefix) "/foo.drv
System: mips64el-linux\n")))
        (with-narinfo (string-append narinfo "Signature: "
                                     (signature-field narinfo) "\n")
          (call-with-temporary-directory
           (lambda (directory)
             (with-input-from-string (string-append
                                      "substitute " (%store-prefix)
                                      "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-wrong-hash "
                                      directory "/wrong-hash\n")
               (lambda ()
                 (guix-substitute "--substitute"))))))))))

(test-quit "substitute, unauthorized key"
    "no valid substitute"
  (with-narinfo (string-append %narinfo "Signature: "
                               (signature-field
                                %narinfo
                                #:public-key %wrong-public-key)
                               "\n")
    (with-input-from-string (string-append "substitute "
                                           (%store-prefix)
                                           "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo"
                                           " foo\n")
      (lambda ()
        (guix-substitute "--substitute")))))

(test-equal "substitute, authorized key"
  '("Substitutable data." 1 #o444)
  (with-narinfo (string-append %narinfo "Signature: "
                               (signature-field %narinfo))
    (dynamic-wind
      (const #t)
      (lambda ()
        (request-substitution (string-append (%store-prefix)
                                             "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
                              "substitute-retrieved")
        (list (call-with-input-file "substitute-retrieved" get-string-all)
              (stat:mtime (lstat "substitute-retrieved"))
              (stat:perms (lstat "substitute-retrieved"))))
      (lambda ()
        (false-if-exception (delete-file "substitute-retrieved"))))))

(test-equal "substitute, unauthorized narinfo comes first"
  "Substitutable data."
  (with-narinfo*
      (string-append %narinfo "Signature: "
                     (signature-field
                      %narinfo
                      #:public-key %wrong-public-key))
      %alternate-substitute-directory

    (with-narinfo* (string-append %narinfo "Signature: "
                                  (signature-field %narinfo))
        %main-substitute-directory

      (dynamic-wind
        (const #t)
        (lambda ()
          ;; Remove this file so that the substitute can only be retrieved
          ;; from %ALTERNATE-SUBSTITUTE-DIRECTORY.
          (delete-file (string-append %main-substitute-directory
                                      "/example.nar"))

          (parameterize ((substitute-urls
                          (map (cut string-append "file://" <>)
                               (list %alternate-substitute-directory
                                     %main-substitute-directory))))
            (request-substitution (string-append (%store-prefix)
                                                 "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
                                  "substitute-retrieved"))
          (call-with-input-file "substitute-retrieved" get-string-all))
        (lambda ()
          (false-if-exception (delete-file "substitute-retrieved")))))))

(test-equal "substitute, unsigned narinfo comes first"
  "Substitutable data."
  (with-narinfo* %narinfo                         ;not signed!
      %alternate-substitute-directory

    (with-narinfo* (string-append %narinfo "Signature: "
                                  (signature-field %narinfo))
        %main-substitute-directory

      (dynamic-wind
        (const #t)
        (lambda ()
          ;; Remove this file so that the substitute can only be retrieved
          ;; from %ALTERNATE-SUBSTITUTE-DIRECTORY.
          (delete-file (string-append %main-substitute-directory
                                      "/example.nar"))

          (parameterize ((substitute-urls
                          (map (cut string-append "file://" <>)
                               (list %alternate-substitute-directory
                                     %main-substitute-directory))))
            (request-substitution (string-append (%store-prefix)
                                                 "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
                                  "substitute-retrieved"))
          (call-with-input-file "substitute-retrieved" get-string-all))
        (lambda ()
          (false-if-exception (delete-file "substitute-retrieved")))))))

(test-equal "substitute, first narinfo is unsigned and has wrong hash"
  "Substitutable data."
  (with-narinfo* (regexp-substitute #f
                                    (string-match "NarHash: [[:graph:]]+"
                                                  %narinfo)
                                    'pre
                                    "NarHash: sha256:"
                                    (bytevector->nix-base32-string
                                     (make-bytevector 32))
                                    'post)
      %alternate-substitute-directory

    (with-narinfo* (string-append %narinfo "Signature: "
                                  (signature-field %narinfo))
        %main-substitute-directory

      (dynamic-wind
        (const #t)
        (lambda ()
          ;; This time remove the file so that the substitute can only be
          ;; retrieved from %MAIN-SUBSTITUTE-DIRECTORY.
          (delete-file (string-append %alternate-substitute-directory
                                      "/example.nar"))

          (parameterize ((substitute-urls
                          (map (cut string-append "file://" <>)
                               (list %alternate-substitute-directory
                                     %main-substitute-directory))))
            (request-substitution (string-append (%store-prefix)
                                                 "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
                                  "substitute-retrieved"))
          (call-with-input-file "substitute-retrieved" get-string-all))
        (lambda ()
          (false-if-exception (delete-file "substitute-retrieved")))))))

(test-equal "substitute, first narinfo is unsigned and has wrong refs"
  "Substitutable data."
  (with-narinfo* (regexp-substitute #f
                                    (string-match "References: ([^\n]+)\n"
                                                  %narinfo)
                                    'pre "References: " 1
                                    " wrong set of references\n"
                                    'post)
      %alternate-substitute-directory

    (with-narinfo* (string-append %narinfo "Signature: "
                                  (signature-field %narinfo))
        %main-substitute-directory

      (dynamic-wind
        (const #t)
        (lambda ()
          ;; This time remove the file so that the substitute can only be
          ;; retrieved from %MAIN-SUBSTITUTE-DIRECTORY.
          (delete-file (string-append %alternate-substitute-directory
                                      "/example.nar"))

          (parameterize ((substitute-urls
                          (map (cut string-append "file://" <>)
                               (list %alternate-substitute-directory
                                     %main-substitute-directory))))
            (request-substitution (string-append (%store-prefix)
                                                 "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
                                  "substitute-retrieved"))
          (call-with-input-file "substitute-retrieved" get-string-all))
        (lambda ()
          (false-if-exception (delete-file "substitute-retrieved")))))))

(test-quit "substitute, two invalid narinfos"
    "no valid substitute"
  (with-narinfo* %narinfo                         ;not signed
      %alternate-substitute-directory

    (with-narinfo* (string-append %narinfo "Signature: " ;unauthorized
                                  (signature-field
                                   %narinfo
                                   #:public-key %wrong-public-key))
        %main-substitute-directory

      (with-input-from-string (string-append "substitute "
                                             (%store-prefix)
                                             "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo"
                                             " substitute-retrieved\n")
        (lambda ()
          (guix-substitute "--substitute"))))))

(test-equal "substitute, narinfo with several URLs"
  "Substitutable data."
  (let ((narinfo (string-append "StorePath: " (%store-prefix)
                                "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo
URL: example.nar.gz
Compression: gzip
URL: example.nar.lz
Compression: lzip
URL: example.nar
Compression: none
NarHash: sha256:" (bytevector->nix-base32-string
                   (sha256 (string->utf8 "Substitutable data."))) "
NarSize: 42
References: bar baz
Deriver: " (%store-prefix) "/foo.drv
System: mips64el-linux\n")))
    (with-narinfo (string-append narinfo "Signature: "
                                 (signature-field narinfo))
      (dynamic-wind
        (const #t)
        (lambda ()
          (define (compress input output compression)
            (call-with-output-file output
              (lambda (port)
                (call-with-compressed-output-port compression port
                  (lambda (port)
                    (call-with-input-file input
                      (lambda (input)
                        (dump-port input port))))))))

          (let ((nar (string-append %main-substitute-directory
                                    "/example.nar")))
            (compress nar (string-append nar ".gz") 'gzip)
            (compress nar (string-append nar ".lz") 'lzip))

          (parameterize ((substitute-urls
                          (list (string-append "file://"
                                               %main-substitute-directory))))
            (request-substitution (string-append (%store-prefix)
                                                 "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo")
                                  "substitute-retrieved"))
          (call-with-input-file "substitute-retrieved" get-string-all))
        (lambda ()
          (false-if-exception (delete-file "substitute-retrieved")))))))

(test-end "substitute")

;;; Local Variables:
;;; eval: (put 'with-narinfo 'scheme-indent-function 1)
;;; eval: (put 'with-narinfo* 'scheme-indent-function 2)
;;; eval: (put 'test-quit 'scheme-indent-function 2)
;;; End:
