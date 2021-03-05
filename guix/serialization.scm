;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix serialization)
  #:use-module (guix combinators)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 binary-ports)
  #:use-module ((ice-9 rdelim) #:prefix rdelim:)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (system foreign)
  #:export (write-int read-int
            write-long-long read-long-long
            write-padding
            write-bytevector write-string
            read-string read-latin1-string read-maybe-utf8-string
            write-string-list read-string-list
            write-string-pairs read-string-pairs
            write-store-path read-store-path
            write-store-path-list read-store-path-list
            (dump . dump-port*)

            &nar-error
            nar-error?
            nar-error-port
            nar-error-file

            &nar-read-error
            nar-read-error?
            nar-read-error-token

            write-file
            write-file-tree
            fold-archive
            restore-file
            dump-file))

;;; Comment:
;;;
;;; Serialization procedures used by the RPCs and the Nar format.  This module
;;; is for internal consumption.
;;;
;;; Code:

;; Similar to serialize.cc in Nix.

(define-condition-type &nar-error &error      ; XXX: inherit from &store-error ?
  nar-error?
  (file  nar-error-file)                       ; file we were restoring, or #f
  (port  nar-error-port))                      ; port from which we read

(define currently-restored-file
  ;; Name of the file being restored.  Used internally for error reporting.
  (make-parameter #f))


(define (get-bytevector-n* port count)
  (let ((bv (get-bytevector-n port count)))
    (when (or (eof-object? bv)
              (< (bytevector-length bv) count))
      (raise (condition (&nar-error
                         (file (currently-restored-file))
                         (port port)))))
    bv))

(define (sub-bytevector bv len)
  "Return a bytevector that aliases the first LEN bytes of BV."
  (define max (bytevector-length bv))
  (cond ((= len max) bv)
        ((< len max)
         ;; Yes, this is safe because the result of each conversion procedure
         ;; has its life cycle synchronized with that of its argument.
         (pointer->bytevector (bytevector->pointer bv) len))
        (else
         (error "sub-bytevector called to get a super bytevector"))))

(define (write-int n p)
  (let ((b (make-bytevector 8 0)))
    (bytevector-u32-set! b 0 n (endianness little))
    (put-bytevector p b)))

(define (read-int p)
  (let ((b (get-bytevector-n* p 8)))
    (bytevector-u32-ref b 0 (endianness little))))

(define (write-long-long n p)
  (let ((b (make-bytevector 8 0)))
    (bytevector-u64-set! b 0 n (endianness little))
    (put-bytevector p b)))

(define (read-long-long p)
  (let ((b (get-bytevector-n* p 8)))
    (bytevector-u64-ref b 0 (endianness little))))

(define write-padding
  (let ((zero (make-bytevector 8 0)))
    (lambda (n p)
      (let ((m (modulo n 8)))
        (or (zero? m)
            (put-bytevector p zero 0 (- 8 m)))))))

(define* (write-bytevector s p
                           #:optional (l (bytevector-length s)))
  (let* ((m (modulo l 8))
         (b (make-bytevector (+ 8 l (if (zero? m) 0 (- 8 m))))))
    (bytevector-u32-set! b 0 l (endianness little))
    (bytevector-copy! s 0 b 8 l)
    (put-bytevector p b)))

(define (write-string s p)
  (write-bytevector (string->utf8 s) p))

(define (read-byte-string p)
  (let* ((len (read-int p))
         (m   (modulo len 8))
         (pad (if (zero? m) 0 (- 8 m)))
         (bv  (get-bytevector-n* p (+ len pad))))
    (sub-bytevector bv len)))

(define (read-string p)
  (utf8->string (read-byte-string p)))

(define (read-latin1-string p)
  "Read an ISO-8859-1 string from P."
  ;; Note: do not use 'get-string-n' to work around Guile bug
  ;; <http://bugs.gnu.org/19621>.  See <http://bugs.gnu.org/19610> for
  ;; a discussion.
  (let ((bv (read-byte-string p)))
    ;; XXX: Rewrite using (ice-9 iconv).
    (list->string (map integer->char (bytevector->u8-list bv)))))

(define (read-maybe-utf8-string p)
  "Read a serialized string from port P.  Attempt to decode it as UTF-8 and
substitute invalid byte sequences with question marks.  This is a
\"permissive\" UTF-8 decoder."
  ;; XXX: We rely on the port's decoding mechanism to do permissive decoding
  ;; and substitute invalid byte sequences with question marks, but this is
  ;; not very efficient.  Eventually Guile may provide a lightweight
  ;; permissive UTF-8 decoder.
  (let* ((bv   (read-byte-string p))
         (port (open-bytevector-input-port bv)))
    (set-port-encoding! port "UTF-8")
    (set-port-conversion-strategy! port 'substitute)
    (rdelim:read-string port)))

(define (write-string-list l p)
  (write-int (length l) p)
  (for-each (cut write-string <> p) l))

(define (read-string-list p)
  (let ((len (read-int p)))
    (unfold (cut >= <> len)
            (lambda (i)
              (read-string p))
            1+
            0)))

(define (write-string-pairs l p)
  (write-int (length l) p)
  (for-each (match-lambda
             ((first . second)
              (write-string first p)
              (write-string second p)))
            l))

(define (read-string-pairs p)
  (let ((len (read-int p)))
    (unfold (cut >= <> len)
            (lambda (i)
              (cons (read-string p) (read-string p)))
            1+
            0)))

(define (write-store-path f p)
  (write-string f p))                             ; TODO: assert path

(define (read-store-path p)
  (read-string p))                                ; TODO: assert path

(define write-store-path-list write-string-list)
(define read-store-path-list read-string-list)

(define-syntax write-literal-strings
  (lambda (s)
    "Write the given literal strings to PORT in an optimized fashion, without
any run-time allocations or computations."
    (define (padding len)
      (let ((m (modulo len 8)))
        (if (zero? m)
            0
            (- 8 m))))

    (syntax-case s ()
      ((_ port strings ...)
       (let* ((bytes (map string->utf8 (syntax->datum #'(strings ...))))
              (len   (fold (lambda (bv size)
                             (+ size 8 (bytevector-length bv)
                                     (padding (bytevector-length bv))))
                           0
                           bytes))
              (bv    (make-bytevector len))
              (zeros (make-bytevector 8 0)))
         (fold (lambda (str offset)
                 (let ((len (bytevector-length str)))
                   (bytevector-u32-set! bv offset len (endianness little))
                   (bytevector-copy! str 0 bv (+ 8 offset) len)
                   (bytevector-copy! zeros 0 bv (+ 8 offset len)
                                     (padding len))
                   (+ offset 8 len (padding len))))
               0
               bytes)
         #`(put-bytevector port #,bv))))))


(define-condition-type &nar-read-error &nar-error
  nar-read-error?
  (token nar-read-error-token))                 ; faulty token, or #f


(define (dump in out size)
  "Copy SIZE bytes from IN to OUT."
  (define buf-size 65536)
  (define buf (make-bytevector buf-size))

  (let loop ((left size))
    (if (<= left 0)
        0
        (let ((read (get-bytevector-n! in buf 0 (min left buf-size))))
          (if (eof-object? read)
              left
              (begin
                (put-bytevector out buf 0 read)
                (loop (- left read))))))))

(define (write-contents-from-port input output size)
  "Write SIZE bytes from port INPUT to port OUTPUT."
  (write-string "contents" output)
  (write-long-long size output)
  ;; Use 'sendfile' when both OUTPUT and INPUT are file ports.
  (if (and (file-port? output) (file-port? input))
      (sendfile output input size 0)
      (dump input output size))
  (write-padding size output))

(define (read-file-type port)
  "Read the file type tag from PORT, and return either 'regular or
'executable."
  (match (read-string port)
    ("contents"
     'regular)
    ("executable"
     (match (list (read-string port) (read-string port))
       (("" "contents") 'executable)
       (x (raise
           (condition (&message
                       (message "unexpected executable file marker"))
                      (&nar-read-error (port port)
                                       (file #f)
                                       (token x)))))))
    (x
     (raise
      (condition (&message (message "unsupported nar file type"))
                 (&nar-read-error (port port) (file #f) (token x)))))))

(define %archive-version-1
  ;; Magic cookie for Nix archives.
  "nix-archive-1")

(define* (write-file file port
                     #:key (select? (const #t)))
  "Write the contents of FILE to PORT in Nar format, recursing into
sub-directories of FILE as needed.  For each directory entry, call (SELECT?
FILE STAT), where FILE is the entry's absolute file name and STAT is the
result of 'lstat'; exclude entries for which SELECT? does not return true."
  (write-file-tree file port
                   #:file-type+size
                   (lambda (file)
                     (let* ((stat (lstat file))
                            (size (stat:size stat)))
                       (case (stat:type stat)
                         ((directory)
                          (values 'directory size))
                         ((regular)
                          (values (if (zero? (logand (stat:mode stat)
                                                     #o100))
                                      'regular
                                      'executable)
                                  size))
                         (else
                          (values (stat:type stat) size))))) ;bah!
                   #:file-port (cut open-file <> "r0b")
                   #:symlink-target readlink

                   #:directory-entries
                   (lambda (directory)
                     ;; 'scandir' defaults to 'string-locale<?' to sort files,
                     ;; but this happens to be case-insensitive (at least in
                     ;; 'en_US' locale on libc 2.18.)  Conversely, we want
                     ;; files to be sorted in a case-sensitive fashion.
                     (define basenames
                       (scandir directory (negate (cut member <> '("." "..")))
                                string<?))

                     (filter-map (lambda (base)
                                   (let ((file (string-append directory
                                                              "/" base)))
                                     (and (select? file (lstat file))
                                          base)))
                                 basenames))

                   ;; The 'scandir' call above gives us filtered and sorted
                   ;; entries, so no post-processing is needed.
                   #:postprocess-entries identity))

(define (filter/sort-directory-entries lst)
  "Remove dot and dot-dot entries from LST, and sort it in lexicographical
order."
  (delete-duplicates
   (sort (remove (cute member <> '("." "..")) lst)
         string<?)
   string=?))

(define* (write-file-tree file port
                          #:key
                          file-type+size
                          file-port
                          symlink-target
                          directory-entries
                          (postprocess-entries filter/sort-directory-entries))
  "Write the contents of FILE to PORT in Nar format, recursing into
sub-directories of FILE as needed.

This procedure does not make any file-system I/O calls.  Instead, it calls the
user-provided FILE-TYPE+SIZE, FILE-PORT, SYMLINK-TARGET, and DIRECTORY-ENTRIES
procedures, which roughly correspond to 'lstat', 'readlink', and 'scandir'.
POSTPROCESS-ENTRIES ensures that directory entries are valid; leave it as-is
unless you know that DIRECTORY-ENTRIES provide filtered and sorted entries, in
which case you can use 'identity'."
  (define p port)

  (write-string %archive-version-1 p)

  (let dump ((f file))
    (define-values (type size)
      (file-type+size f))

    (write-literal-strings p "(")
    (case type
      ((regular executable)
       (write-literal-strings p "type" "regular")
       (when (eq? 'executable type)
         (write-literal-strings p "executable" ""))
       (let ((input (file-port f)))
         (dynamic-wind
           (const #t)
           (lambda ()
             (write-contents-from-port input p size))
           (lambda ()
             (close-port input)))))
      ((directory)
       (write-literal-strings p "type" "directory")
       (let ((entries (postprocess-entries (directory-entries f))))
         (for-each (lambda (e)
                     (let* ((f (string-append f "/" e)))
                       (write-literal-strings p "entry" "(" "name")
                       (write-string e p)
                       (write-literal-strings p "node")
                       (dump f)
                       (write-literal-strings p ")")))
                   entries)))
      ((symlink)
       (write-literal-strings p "type" "symlink" "target")
       (write-string (symlink-target f) p))
      (else
       (raise (condition (&message (message "unsupported file type"))
                         (&nar-error (file f) (port port))))))
    (write-literal-strings p ")")))

(define port-conversion-strategy
  (fluid->parameter %default-port-conversion-strategy))

(define (fold-archive proc seed port file)
  "Read a file (possibly a directory structure) in Nar format from PORT.  Call
PROC on each file or directory read from PORT using:

  (PROC FILE TYPE CONTENTS RESULT)

using SEED as the first RESULT.  TYPE is a symbol like 'regular, and CONTENTS
depends on TYPE."
  (parameterize ((currently-restored-file file)

                 ;; Error out if we can convert file names to the current
                 ;; locale.  (XXX: We'd prefer UTF-8 encoding for file names
                 ;; regardless of the locale, but that's what Guile gives us
                 ;; so far.)
                 (port-conversion-strategy 'error))
    (let ((signature (read-string port)))
      (unless (equal? signature %archive-version-1)
        (raise
         (condition (&message (message "invalid nar signature"))
                    (&nar-read-error (port port)
                                     (token signature)
                                     (file #f))))))

    (let read ((file file)
               (result seed))
      (define (read-eof-marker)
        (match (read-string port)
          (")" #t)
          (x (raise
              (condition
               (&message (message "invalid nar end-of-file marker"))
               (&nar-read-error (port port) (file file) (token x)))))))

      (currently-restored-file file)

      (match (list (read-string port) (read-string port) (read-string port))
        (("(" "type" "regular")
         (let* ((type   (read-file-type port))
                (size   (read-long-long port))

                ;; The caller must read exactly SIZE bytes from PORT.
                (result (proc file type `(,port . ,size) result)))
           (let ((m (modulo size 8)))
             (unless (zero? m)
               (get-bytevector-n* port (- 8 m))))
           (read-eof-marker)
           result))
        (("(" "type" "symlink")
         (match (list (read-string port) (read-string port))
           (("target" target)
            (let ((result (proc file 'symlink target result)))
              (read-eof-marker)
              result))
           (x (raise
               (condition
                (&message (message "invalid symlink tokens"))
                (&nar-read-error (port port) (file file) (token x)))))))
        (("(" "type" "directory")
         (let ((dir file))
           (let loop ((prefix (read-string port))
                      (result (proc file 'directory #f result)))
             (match prefix
               ("entry"
                (match (list (read-string port)
                             (read-string port) (read-string port)
                             (read-string port))
                  (("(" "name" file "node")
                   (let ((result (read (string-append dir "/" file) result)))
                     (match (read-string port)
                       (")" #f)
                       (x
                        (raise
                         (condition
                          (&message
                           (message "unexpected directory entry termination"))
                          (&nar-read-error (port port)
                                           (file file)
                                           (token x))))))
                     (loop (read-string port) result)))))
               (")"                               ;done with DIR
                (proc file 'directory-complete #f result))
               (x
                (raise
                 (condition
                  (&message (message "unexpected directory inter-entry marker"))
                  (&nar-read-error (port port) (file file) (token x)))))))))
        (x
         (raise
          (condition
           (&message (message "unsupported nar entry type"))
           (&nar-read-error (port port) (file file) (token x)))))))))

(define (dump-file file input size type)
  "Dump SIZE bytes from INPUT to FILE.

This procedure is suitable for use as the #:dump-file argument to
'restore-file'."
  (call-with-output-file file
    (lambda (output)
      (dump input output size))))

(define* (restore-file port file
                       #:key (dump-file dump-file))
  "Read a file (possibly a directory structure) in Nar format from PORT.
Restore it as FILE with canonical permissions and timestamps.  To write a
regular or executable file, call:

  (DUMP-FILE FILE INPUT SIZE TYPE)

The default is to dump SIZE bytes from INPUT to FILE, but callers can provide
a custom procedure, for instance to deduplicate FILE on the fly."
  (fold-archive (lambda (file type content result)
                  (match type
                    ('directory
                     (mkdir file))
                    ('directory-complete
                     (chmod file #o555)
                     (utime file 1 1 0 0))
                    ('symlink
                     (symlink content file)
                     (utime file 1 1 0 0 AT_SYMLINK_NOFOLLOW))
                    ((or 'regular 'executable)
                     (match content
                       ((input . size)
                        (dump-file file input size type)
                        (chmod file (if (eq? type 'executable)
                                        #o555
                                        #o444))
                        (utime file 1 1 0 0))))))
                #t
                port
                file))

;;; Local Variables:
;;; eval: (put 'call-with-binary-input-file 'scheme-indent-function 1)
;;; End:

;;; serialization.scm ends here
