;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2021 Mark H Weaver <mhw@netris.org>
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

(define-module (guix build graft)
  #:use-module (guix build utils)
  #:use-module (guix build debug-link)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)   ; list library
  #:use-module (srfi srfi-26)  ; cut and cute
  #:export (replace-store-references
            rewrite-directory
            graft))

;;; Commentary:
;;;
;;; This module supports "grafts".  Grafting a directory means rewriting it,
;;; with references to some specific items replaced by references to other
;;; store items---the grafts.
;;;
;;; This method is used to provide fast security updates as only the leaves of
;;; the dependency graph need to be grafted, even when the security updates
;;; affect a core component such as Bash or libc.  It is based on the idea of
;;; 'replace-dependency' implemented by Shea Levy in Nixpkgs.
;;;
;;; Code:

(define-syntax-rule (define-inline name val)
  (define-syntax name (identifier-syntax val)))

(define-inline hash-length 32)

(define nix-base32-char?
  (cute char-set-contains?
        ;; ASCII digits and lower case letters except e o t u
        (string->char-set "0123456789abcdfghijklmnpqrsvwxyz")
        <>))

(define (nix-base32-char-or-nul? c)
  "Return true if C is a nix-base32 character or NUL, otherwise return false."
  (or (nix-base32-char? c)
      (char=? c #\nul)))

(define (possible-utf16-hash? buffer i w)
  "Return true if (I - W) is large enough to hold a UTF-16 encoded
nix-base32 hash and if BUFFER contains NULs in all positions where NULs
are to be expected in a UTF-16 encoded hash+dash pattern whose dash is
found at position I.  Otherwise, return false."
  (and (<= (* 2 hash-length) (- i w))
       (let loop ((j (+ 1 (- i (* 2 hash-length)))))
         (or (>= j i)
             (and (zero? (bytevector-u8-ref buffer j))
                  (loop (+ j 2)))))))

(define (possible-utf32-hash? buffer i w)
  "Return true if (I - W) is large enough to hold a UTF-32 encoded
nix-base32 hash and if BUFFER contains NULs in all positions where NULs
are to be expected in a UTF-32 encoded hash+dash pattern whose dash is
found at position I.  Otherwise, return false."
  (and (<= (* 4 hash-length) (- i w))
       (let loop ((j (+ 1 (- i (* 4 hash-length)))))
         (or (>= j i)
             (and (zero? (bytevector-u8-ref buffer j))
                  (zero? (bytevector-u8-ref buffer (+ j 1)))
                  (zero? (bytevector-u8-ref buffer (+ j 2)))
                  (loop (+ j 4)))))))

(define (insert-nuls char-size bv)
  "Given a bytevector BV, return a bytevector containing the same bytes but
with (CHAR-SIZE - 1) NULs inserted between every two adjacent bytes from BV.
For example, (insert-nuls 4 #u8(1 2 3)) => #u8(1 0 0 0 2 0 0 0 3)."
  (if (= char-size 1)
      bv
      (let* ((len (bytevector-length bv))
             (bv* (make-bytevector (+ 1 (* char-size
                                           (- len 1)))
                                   0)))
        (let loop ((i 0))
          (when (< i len)
            (bytevector-u8-set! bv* (* i char-size)
                                (bytevector-u8-ref bv i))
            (loop (+ i 1))))
        bv*)))

(define* (replace-store-references input output replacement-table
                                   #:optional (store (%store-directory)))
  "Read data from INPUT, replacing store references according to
REPLACEMENT-TABLE, and writing the result to OUTPUT.  REPLACEMENT-TABLE is a
vhash that maps strings (original hashes) to bytevectors (replacement strings
comprising the replacement hash, a dash, and a string).

Note: We use string keys to work around the fact that guile-2.0 hashes all
bytevectors to the same value."

  (define (lookup-replacement s)
    (match (vhash-assoc s replacement-table)
      ((origin . replacement)
       replacement)
      (#f #f)))

  (define (optimize-u8-predicate pred)
    (cute vector-ref
          (list->vector (map pred (iota 256)))
          <>))

  (define nix-base32-byte-or-nul?
    (optimize-u8-predicate
     (compose nix-base32-char-or-nul?
              integer->char)))

  (define (dash? byte) (= byte 45))

  (define request-size (expt 2 20))  ; 1 MiB

  ;; We scan the file for the following 33-byte pattern: 32 bytes of
  ;; nix-base32 characters followed by a dash.  When we find such a pattern
  ;; whose hash is in REPLACEMENT-TABLE, we perform the required rewrite and
  ;; continue scanning.
  ;;
  ;; To support UTF-16 and UTF-32 store references, the 33 bytes comprising
  ;; this hash+dash pattern may optionally be interspersed by extra NUL bytes.
  ;; This simple approach works because the characters we are looking for are
  ;; restricted to ASCII.  UTF-16 hashes are interspersed with single NUL
  ;; bytes ("\0"), and UTF-32 hashes are interspersed with triplets of NULs
  ;; ("\0\0\0").  Note that we require NULs to be present only *between* the
  ;; other bytes, and not at either end, in order to be insensitive to byte
  ;; order.
  ;;
  ;; To accommodate large files, we do not read the entire file at once, but
  ;; instead work on buffers of up to REQUEST-SIZE bytes.  To ensure that
  ;; every hash+dash pattern appears in its entirety in at least one buffer,
  ;; adjacent buffers must overlap by one byte less than the maximum size of a
  ;; hash+dash pattern.  We accomplish this by "ungetting" a suffix of each
  ;; buffer before reading the next buffer, unless we know that we've reached
  ;; the end-of-file.
  (let ((buffer (make-bytevector request-size)))
    (define-syntax-rule (byte-at i)
      (bytevector-u8-ref buffer i))
    (let outer-loop ()
      (match (get-bytevector-n! input buffer 0 request-size)
        ((? eof-object?) 'done)
        (end
         (define (scan-from i w)
           ;; Scan the buffer for dashes that might be preceded by nix hashes,
           ;; where I is the minimum position where such a dash might be
           ;; found, and W is the number of bytes in the buffer that have been
           ;; written so far.  We assume that I - W >= HASH-LENGTH.
           ;;
           ;; The key optimization here is that whenever we find a byte at
           ;; position I that cannot occur within a nix hash (because it's
           ;; neither a nix-base32 character nor NUL), we can infer that the
           ;; earliest position where the next hash could start is at I + 1,
           ;; and therefore the earliest position for the following dash is
           ;; (+ I 1 HASH-LENGTH), which is I + 33.
           ;;
           ;; Since nix-base32-or-nul characters comprise only about 1/8 of
           ;; the 256 possible byte values, and exclude some of the most
           ;; common letters in English text (e t o u), we can advance 33
           ;; positions much of the time.
           (if (< i end)
               (let ((byte (byte-at i)))
                 (cond ((dash? byte)
                        (found-dash i w))
                       ((nix-base32-byte-or-nul? byte)
                        (scan-from (+ i 1) w))
                       (else
                        (not-part-of-hash i w))))
               (finish-buffer i w)))

         (define (not-part-of-hash i w)
           ;; Position I is known to not be within a nix hash that we must
           ;; rewrite.  Therefore, the earliest position where the next hash
           ;; might start is I + 1, and therefore the earliest position of
           ;; the following dash is (+ I 1 HASH-LENGTH).
           (scan-from (+ i 1 hash-length) w))

         (define (found-dash i w)
           ;; We know that there is a dash '-' at position I, and that
           ;; I - W >= HASH-LENGTH.  The immediately preceding bytes *might*
           ;; contain a nix-base32 hash, but that is not yet known.  Here,
           ;; we rule out all but one possible encoding (ASCII, UTF-16,
           ;; UTF-32) by counting how many NULs precede the dash.
           (cond ((not (zero? (byte-at (- i 1))))
                  ;; The dash is *not* preceded by a NUL, therefore it
                  ;; cannot possibly be a UTF-16 or UTF-32 hash.  Proceed
                  ;; to check for an ASCII hash.
                  (found-possible-hash 1 i w))

                 ((not (zero? (byte-at (- i 2))))
                  ;; The dash is preceded by exactly one NUL, therefore it
                  ;; cannot be an ASCII or UTF-32 hash.  Proceed to check
                  ;; for a UTF-16 hash.
                  (if (possible-utf16-hash? buffer i w)
                      (found-possible-hash 2 i w)
                      (not-part-of-hash i w)))

                 (else
                  ;; The dash is preceded by at least two NULs, therefore
                  ;; it cannot be an ASCII or UTF-16 hash.  Proceed to
                  ;; check for a UTF-32 hash.
                  (if (possible-utf32-hash? buffer i w)
                      (found-possible-hash 4 i w)
                      (not-part-of-hash i w)))))

         (define (found-possible-hash char-size i w)
           ;; We know that there is a dash '-' at position I, that
           ;; I - W >= CHAR-SIZE * HASH-LENGTH, and that the only
           ;; possible encoding for the preceding hash is as indicated by
           ;; CHAR-SIZE.  Here we check to see if the given hash is in
           ;; REPLACEMENT-TABLE, and if so, we perform the required
           ;; rewrite.
           (let* ((hash (string-tabulate
                         (lambda (j)
                           (integer->char
                            (byte-at (- i (* char-size
                                             (- hash-length j))))))
                         hash-length))
                  (replacement* (lookup-replacement hash))
                  (replacement (and replacement*
                                    (insert-nuls char-size replacement*))))
             (cond
              ((not replacement)
               (not-part-of-hash i w))
              (else
               ;; We've found a hash that needs to be replaced.
               ;; First, write out all bytes preceding the hash
               ;; that have not yet been written.
               (put-bytevector output buffer w
                               (- i (* char-size hash-length) w))
               ;; Now write the replacement string.
               (put-bytevector output replacement)
               ;; Now compute the new values of W and I and continue.
               (let ((w (+ (- i (* char-size hash-length))
                           (bytevector-length replacement))))
                 (scan-from (+ w hash-length) w))))))

         (define (finish-buffer i w)
           ;; We have finished scanning the buffer.  Now we determine how many
           ;; bytes have not yet been written, and how many bytes to "unget".
           ;; If END is less than REQUEST-SIZE then we read less than we asked
           ;; for, which indicates that we are at EOF, so we needn't unget
           ;; anything.  Otherwise, we unget up to (* 4 HASH-LENGTH) bytes.
           ;; However, we must be careful not to unget bytes that have already
           ;; been written, because that would cause them to be written again
           ;; from the next buffer.  In practice, this case occurs when a
           ;; replacement is made near or beyond the end of the buffer.  When
           ;; REPLACEMENT went beyond END, we consume the extra bytes from
           ;; INPUT.
           (if (> w end)
               (get-bytevector-n! input buffer 0 (- w end))
               (let* ((unwritten  (- end w))
                      (unget-size (if (= end request-size)
                                      (min (* 4 hash-length)
                                           unwritten)
                                      0))
                      (write-size (- unwritten unget-size)))
                 (put-bytevector output buffer w write-size)
                 (unget-bytevector input buffer (+ w write-size)
                                   unget-size)))
           (outer-loop))

         (scan-from hash-length 0))))))

(define (rename-matching-files directory mapping)
  "Apply MAPPING to the names of all the files in DIRECTORY, where MAPPING is
a list of store file name pairs."
  (let* ((mapping (map (match-lambda
                        ((source . target)
                         (cons (basename source) (basename target))))
                       mapping))
         (matches (find-files directory
                              (lambda (file stat)
                                (assoc-ref mapping (basename file)))
                              #:directories? #t)))

    ;; XXX: This is not quite correct: if MAPPING contains "foo", and
    ;; DIRECTORY contains "bar/foo/foo", we first rename "bar/foo" and then
    ;; "bar/foo/foo" no longer exists so we fail.  Oh well, surely that's good
    ;; enough!
    (for-each (lambda (file)
                (let ((target (assoc-ref mapping (basename file))))
                  (rename-file file
                               (string-append (dirname file) "/" target))))
              matches)))

(define (exit-on-exception proc)
  "Return a procedure that wraps PROC so that 'primitive-exit' is called when
an exception is caught."
  (lambda (arg)
    (catch #t
      (lambda ()
        (proc arg))
      (lambda (key . args)
        ;; Since ports are not thread-safe as of Guile 2.0, reopen stderr.
        (let ((port (fdopen 2 "w0")))
          (print-exception port #f key args)
          (primitive-exit 1))))))

;; We need this as long as we support Guile < 2.0.13.
(define* (mkdir-p* dir #:optional (mode #o755))
  "This is a variant of 'mkdir-p' that works around
<http://bugs.gnu.org/24659> by passing MODE explicitly in each 'mkdir' call."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (mkdir path mode)
             (loop tail path))
           (lambda args
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))

(define* (rewrite-directory directory output mapping
                            #:optional (store (%store-directory)))
  "Copy DIRECTORY to OUTPUT, replacing strings according to MAPPING, a list of
file name pairs."

  (define hash-mapping
    ;; List of hash/replacement pairs, where the hash is a nix-base32 string
    ;; and the replacement is a string that includes the replacement's name,
    ;; like "r837zajjc1q8z9hph4b6860a9c05blyy-openssl-1.0.2j".
    (let* ((prefix (string-append store "/"))
           (start  (string-length prefix))
           (end    (+ start hash-length)))
      (define (valid-hash? h)
        (every nix-base32-char? (string->list h)))
      (define (hash+rest s)
        (and (< end (string-length s))
             (let ((hash (substring s start end))
                   (all  (substring s start)))
               (and (string-prefix? prefix s)
                    (valid-hash? hash)
                    (eqv? #\- (string-ref s end))
                    (list hash all)))))

      (map (match-lambda
             (((= hash+rest (origin-hash origin-string))
               .
               (= hash+rest (replacement-hash replacement-string)))
              (unless (= (string-length origin-string)
                         (string-length replacement-string))
                (error "replacement length differs from the original length"
                       origin-string replacement-string))
              (cons origin-hash (string->utf8 replacement-string)))
             ((origin . replacement)
              (error "invalid replacement" origin replacement)))
           mapping)))

  (define replacement-table
    (alist->vhash hash-mapping))

  (define prefix-len
    (string-length directory))

  (define (destination file)
    (string-append output (string-drop file prefix-len)))

  (define (rewrite-leaf file)
    (let ((stat (lstat file))
          (dest (destination file)))
      (mkdir-p* (dirname dest))
      (case (stat:type stat)
        ((symlink)
         (let ((target (readlink file)))
           (symlink (call-with-output-string
                      (lambda (output)
                        (replace-store-references (open-input-string target)
                                                  output replacement-table
                                                  store)))
                    dest)))
        ((regular)
         (call-with-input-file file
           (lambda (input)
             (call-with-output-file dest
               (lambda (output)
                 (replace-store-references input output replacement-table
                                           store)
                 (chmod output (stat:perms stat)))))))
        ((directory)
         (mkdir-p* dest))
        (else
         (error "unsupported file type" stat)))))

  ;; Use 'exit-on-exception' to force an exit upon I/O errors, given that
  ;; 'n-par-for-each' silently swallows exceptions.
  ;; See <http://bugs.gnu.org/23581>.
  (n-par-for-each (parallel-job-count)
                  (exit-on-exception rewrite-leaf)
                  (find-files directory (const #t)
                              #:directories? #t))
  (rename-matching-files output mapping))

(define %graft-hooks
  ;; Default list of hooks run after grafting.
  (list graft-debug-links))

(define* (graft old-outputs new-outputs mapping
                #:key (log-port (current-output-port))
                (hooks %graft-hooks))
  "Apply the grafts described by MAPPING on OLD-OUTPUTS, leading to
NEW-OUTPUTS.  MAPPING must be a list of file name pairs; OLD-OUTPUTS and
NEW-OUTPUTS are lists of output name/file name pairs."
  (for-each (lambda (input output)
              (format log-port "grafting '~a' -> '~a'...~%" input output)
              (force-output)
              (rewrite-directory input output mapping))
            (match old-outputs
              (((names . files) ...)
               files))
            (match new-outputs
              (((names . files) ...)
               files)))
  (for-each (lambda (hook)
              (hook old-outputs new-outputs mapping
                    #:log-port log-port))
            hooks))

;;; graft.scm ends here
