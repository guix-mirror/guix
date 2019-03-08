;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (guix lzlib)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (guix config)
  #:export (lzlib-available?
            make-lzip-input-port
            make-lzip-output-port
            call-with-lzip-input-port
            call-with-lzip-output-port
            %default-member-length-limit
            %default-compression-level))

;;; Commentary:
;;;
;;; Bindings to the lzlib / liblz API.  Some convenience functions are also
;;; provided (see the export).
;;;
;;; While the bindings are complete, the convenience functions only support
;;; single member archives.  To decompress single member archives, we loop
;;; until lz-decompress-read returns 0.  This is simpler.  To support multiple
;;; members properly, we need (among others) to call lz-decompress-finish and
;;; loop over lz-decompress-read until lz-decompress-finished? returns #t.
;;; Otherwise a multi-member archive starting with an empty member would only
;;; decompress the empty member and stop there, resulting in truncated output.

;;; Code:

(define %lzlib
  ;; File name of lzlib's shared library.  When updating via 'guix pull',
  ;; '%liblz' might be undefined so protect against it.
  (delay (dynamic-link (if (defined? '%liblz)
                           %liblz
                           "liblz"))))

(define (lzlib-available?)
  "Return true if lzlib is available, #f otherwise."
  (false-if-exception (force %lzlib)))

(define (lzlib-procedure ret name parameters)
  "Return a procedure corresponding to C function NAME in liblz, or #f if
either lzlib or the function could not be found."
  (match (false-if-exception (dynamic-func name (force %lzlib)))
    ((? pointer? ptr)
     (pointer->procedure ret ptr parameters))
    (#f
     #f)))

(define-wrapped-pointer-type <lz-decoder>
  ;; Scheme counterpart of the 'LZ_Decoder' opaque type.
  lz-decoder?
  pointer->lz-decoder
  lz-decoder->pointer
  (lambda (obj port)
    (format port "#<lz-decoder ~a>"
            (number->string (object-address obj) 16))))

(define-wrapped-pointer-type <lz-encoder>
  ;; Scheme counterpart of the 'LZ_Encoder' opaque type.
  lz-encoder?
  pointer->lz-encoder
  lz-encoder->pointer
  (lambda (obj port)
    (format port "#<lz-encoder ~a>"
            (number->string (object-address obj) 16))))

;; From lzlib.h
(define %error-number-ok 0)
(define %error-number-bad-argument 1)
(define %error-number-mem-error 2)
(define %error-number-sequence-error 3)
(define %error-number-header-error 4)
(define %error-number-unexpected-eof 5)
(define %error-number-data-error 6)
(define %error-number-library-error 7)


;; Compression bindings.

(define lz-compress-open
  (let ((proc (lzlib-procedure '* "LZ_compress_open" (list int int uint64)))
        ;; member-size is an "unsigned long long", and the C standard guarantees
        ;; a minimum range of 0..2^64-1.
        (unlimited-size (- (expt 2 64) 1)))
    (lambda* (dictionary-size match-length-limit #:optional (member-size unlimited-size))
      "Initialize the internal stream state for compression and returns a
pointer that can only be used as the encoder argument for the other
lz-compress functions, or a null pointer if the encoder could not be
allocated.

See the manual: (lzlib) Compression functions."
      (let ((encoder-ptr (proc dictionary-size match-length-limit member-size)))
        (if (not (= (lz-compress-error encoder-ptr) -1))
            (pointer->lz-encoder encoder-ptr)
            (throw 'lzlib-error 'lz-compress-open))))))

(define lz-compress-close
  (let ((proc (lzlib-procedure int "LZ_compress_close" '(*))))
    (lambda (encoder)
      "Close encoder.  ENCODER can no longer be used as an argument to any
lz-compress function. "
      (let ((ret (proc (lz-encoder->pointer encoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-compress-close ret)
            ret)))))

(define lz-compress-finish
  (let ((proc (lzlib-procedure int "LZ_compress_finish" '(*))))
    (lambda (encoder)
      "Tell that all the data for this member have already been written (with
the `lz-compress-write' function).  It is safe to call `lz-compress-finish' as
many times as needed.  After all the produced compressed data have been read
with `lz-compress-read' and `lz-compress-member-finished?' returns #t, a new
member can be started with 'lz-compress-restart-member'."
      (let ((ret (proc (lz-encoder->pointer encoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-compress-finish (lz-compress-error encoder))
            ret)))))

(define lz-compress-restart-member
  (let ((proc (lzlib-procedure int "LZ_compress_restart_member" (list '* uint64))))
    (lambda (encoder member-size)
      "Start a new member in a multimember data stream.
Call this function only after `lz-compress-member-finished?' indicates that the
current member has been fully read (with the `lz-compress-read' function)."
      (let ((ret (proc (lz-encoder->pointer encoder) member-size)))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-compress-restart-member
                   (lz-compress-error encoder))
            ret)))))

(define lz-compress-sync-flush
  (let ((proc (lzlib-procedure int "LZ_compress_sync_flush" (list '*))))
    (lambda (encoder)
      "Make available to `lz-compress-read' all the data already written with
the `LZ-compress-write' function.  First call `lz-compress-sync-flush'.  Then
call 'lz-compress-read' until it returns 0.

Repeated use of `LZ-compress-sync-flush' may degrade compression ratio,
so use it only when needed. "
      (let ((ret (proc (lz-encoder->pointer encoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-compress-sync-flush
                   (lz-compress-error encoder))
            ret)))))

(define lz-compress-read
  (let ((proc (lzlib-procedure int "LZ_compress_read" (list '* '* int))))
    (lambda* (encoder lzfile-bv #:optional (start 0) (count (bytevector-length lzfile-bv)))
      "Read up to COUNT bytes from the encoder stream, storing the results in LZFILE-BV.
Return the number of uncompressed bytes written, a strictly positive integer."
      (let ((ret (proc (lz-encoder->pointer encoder)
                       (bytevector->pointer lzfile-bv start)
                       count)))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-compress-read (lz-compress-error encoder))
            ret)))))

(define lz-compress-write
  (let ((proc (lzlib-procedure int "LZ_compress_write" (list '* '* int))))
    (lambda* (encoder bv #:optional (start 0) (count (bytevector-length bv)))
      "Write up to COUNT bytes from BV to the encoder stream.  Return the
number of uncompressed bytes written, a strictly positive integer."
      (let ((ret (proc (lz-encoder->pointer encoder)
                       (bytevector->pointer bv start)
                       count)))
        (if (< ret 0)
            (throw 'lzlib-error 'lz-compress-write (lz-compress-error encoder))
            ret)))))

(define lz-compress-write-size
  (let ((proc (lzlib-procedure int "LZ_compress_write_size" '(*))))
    (lambda (encoder)
      "The maximum number of bytes that can be immediately written through the
`lz-compress-write' function.

It is guaranteed that an immediate call to `lz-compress-write' will accept a
SIZE up to the returned number of bytes. "
      (let ((ret (proc (lz-encoder->pointer encoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-compress-write-size (lz-compress-error encoder))
            ret)))))

(define lz-compress-error
  (let ((proc (lzlib-procedure int "LZ_compress_errno" '(*))))
    (lambda (encoder)
      "ENCODER can be a Scheme object or a pointer."
      (let* ((error-number (proc (if (lz-encoder? encoder)
                                     (lz-encoder->pointer encoder)
                                     encoder))))
        error-number))))

(define lz-compress-finished?
  (let ((proc (lzlib-procedure int "LZ_compress_finished" '(*))))
    (lambda (encoder)
      "Return #t if all the data have been read and `lz-compress-close' can
be safely called. Otherwise return #f."
      (let ((ret (proc (lz-encoder->pointer encoder))))
        (match ret
          (1 #t)
          (0 #f)
          (_ (throw 'lzlib-error 'lz-compress-finished? (lz-compress-error encoder))))))))

(define lz-compress-member-finished?
  (let ((proc (lzlib-procedure int "LZ_compress_member_finished" '(*))))
    (lambda (encoder)
      "Return #t if the current member, in a multimember data stream, has
been fully read and 'lz-compress-restart-member' can be safely called.
Otherwise return #f."
      (let ((ret (proc (lz-encoder->pointer encoder))))
        (match ret
          (1 #t)
          (0 #f)
          (_ (throw 'lzlib-error 'lz-compress-member-finished? (lz-compress-error encoder))))))))

(define lz-compress-data-position
  (let ((proc (lzlib-procedure uint64 "LZ_compress_data_position" '(*))))
    (lambda (encoder)
      "Return the number of input bytes already compressed in the current
member."
      (let ((ret (proc (lz-encoder->pointer encoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-compress-data-position
                   (lz-compress-error encoder))
            ret)))))

(define lz-compress-member-position
  (let ((proc (lzlib-procedure uint64 "LZ_compress_member_position" '(*))))
    (lambda (encoder)
      "Return the number of compressed bytes already produced, but perhaps
not yet read, in the current member."
      (let ((ret (proc (lz-encoder->pointer encoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-compress-member-position
                   (lz-compress-error encoder))
            ret)))))

(define lz-compress-total-in-size
  (let ((proc (lzlib-procedure uint64 "LZ_compress_total_in_size" '(*))))
    (lambda (encoder)
      "Return the total number of input bytes already compressed."
      (let ((ret (proc (lz-encoder->pointer encoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-compress-total-in-size
                   (lz-compress-error encoder))
            ret)))))

(define lz-compress-total-out-size
  (let ((proc (lzlib-procedure uint64 "LZ_compress_total_out_size" '(*))))
    (lambda (encoder)
      "Return the total number of compressed bytes already produced, but
perhaps not yet read."
      (let ((ret (proc (lz-encoder->pointer encoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-compress-total-out-size
                   (lz-compress-error encoder))
            ret)))))


;; Decompression bindings.

(define lz-decompress-open
  (let ((proc (lzlib-procedure '* "LZ_decompress_open" '())))
    (lambda ()
      "Initializes the internal stream state for decompression and returns a
pointer that can only be used as the decoder argument for the other
lz-decompress functions, or a null pointer if the decoder could not be
allocated.

See the manual: (lzlib) Decompression functions."
      (let ((decoder-ptr (proc)))
        (if (not (= (lz-decompress-error decoder-ptr) -1))
            (pointer->lz-decoder decoder-ptr)
            (throw 'lzlib-error 'lz-decompress-open))))))

(define lz-decompress-close
  (let ((proc (lzlib-procedure int "LZ_decompress_close" '(*))))
    (lambda (decoder)
      "Close decoder.  DECODER can no longer be used as an argument to any
lz-decompress function. "
      (let ((ret (proc (lz-decoder->pointer decoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-decompress-close ret)
            ret)))))

(define lz-decompress-finish
  (let ((proc (lzlib-procedure int "LZ_decompress_finish" '(*))))
    (lambda (decoder)
      "Tell that all the data for this stream have already been written (with
the `lz-decompress-write' function).  It is safe to call
`lz-decompress-finish' as many times as needed."
      (let ((ret (proc (lz-decoder->pointer decoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-decompress-finish (lz-decompress-error decoder))
            ret)))))

(define lz-decompress-reset
  (let ((proc (lzlib-procedure int "LZ_decompress_reset" '(*))))
    (lambda (decoder)
      "Reset the internal state of DECODER as it was just after opening it
with the `lz-decompress-open' function.  Data stored in the internal buffers
is discarded.  Position counters are set to 0."
      (let ((ret (proc (lz-decoder->pointer decoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-decompress-reset
                   (lz-decompress-error decoder))
            ret)))))

(define lz-decompress-sync-to-member
  (let ((proc (lzlib-procedure int "LZ_decompress_sync_to_member" '(*))))
    (lambda (decoder)
      "Reset the error state of DECODER and enters a search state that lasts
until a new member header (or the end of the stream) is found.  After a
successful call to `lz-decompress-sync-to-member', data written with
`lz-decompress-write' will be consumed and 'lz-decompress-read' will return 0
until a header is found.

This function is useful to discard any data preceding the first member, or to
discard the rest of the current member, for example in case of a data
error.  If the decoder is already at the beginning of a member, this function
does nothing."
      (let ((ret (proc (lz-decoder->pointer decoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-decompress-sync-to-member
                   (lz-decompress-error decoder))
            ret)))))

(define lz-decompress-read
  (let ((proc (lzlib-procedure int "LZ_decompress_read" (list '* '* int))))
    (lambda* (decoder file-bv #:optional (start 0) (count (bytevector-length file-bv)))
      "Read up to COUNT bytes from the decoder stream, storing the results in FILE-BV.
Return the number of uncompressed bytes written, a non-negative positive integer."
      (let ((ret (proc (lz-decoder->pointer decoder)
                       (bytevector->pointer file-bv start)
                       count)))
        (if (< ret 0)
            (throw 'lzlib-error 'lz-decompress-read (lz-decompress-error decoder))
            ret)))))

(define lz-decompress-write
  (let ((proc (lzlib-procedure int "LZ_decompress_write" (list '* '* int))))
    (lambda* (decoder bv #:optional (start 0) (count (bytevector-length bv)))
      "Write up to COUNT bytes from BV to the decoder stream.  Return the
number of uncompressed bytes written, a non-negative integer."
      (let ((ret (proc (lz-decoder->pointer decoder)
                       (bytevector->pointer bv start)
                       count)))
        (if (< ret 0)
            (throw 'lzlib-error 'lz-decompress-write (lz-decompress-error decoder))
            ret)))))

(define lz-decompress-write-size
  (let ((proc (lzlib-procedure int "LZ_decompress_write_size" '(*))))
    (lambda (decoder)
      "Return the maximum number of bytes that can be immediately written
through the `lz-decompress-write' function.

It is guaranteed that an immediate call to `lz-decompress-write' will accept a
SIZE up to the returned number of bytes. "
      (let ((ret (proc (lz-decoder->pointer decoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-decompress-write-size (lz-decompress-error decoder))
            ret)))))

(define lz-decompress-error
  (let ((proc (lzlib-procedure int "LZ_decompress_errno" '(*))))
    (lambda (decoder)
      "DECODER can be a Scheme object or a pointer."
      (let* ((error-number (proc (if (lz-decoder? decoder)
                                     (lz-decoder->pointer decoder)
                                     decoder))))
        error-number))))

(define lz-decompress-finished?
  (let ((proc (lzlib-procedure int "LZ_decompress_finished" '(*))))
    (lambda (decoder)
      "Return #t if all the data have been read and `lz-decompress-close' can
be safely called.  Otherwise return #f."
      (let ((ret (proc (lz-decoder->pointer decoder))))
        (match ret
          (1 #t)
          (0 #f)
          (_ (throw 'lzlib-error 'lz-decompress-finished? (lz-decompress-error decoder))))))))

(define lz-decompress-member-finished?
  (let ((proc (lzlib-procedure int "LZ_decompress_member_finished" '(*))))
    (lambda (decoder)
      "Return #t if the current member, in a multimember data stream, has
been fully read and `lz-decompress-restart-member' can be safely called.
Otherwise return #f."
      (let ((ret (proc (lz-decoder->pointer decoder))))
        (match ret
          (1 #t)
          (0 #f)
          (_ (throw 'lzlib-error 'lz-decompress-member-finished? (lz-decompress-error decoder))))))))

(define lz-decompress-member-version
  (let ((proc (lzlib-procedure int "LZ_decompress_member_version" '(*))))
    (lambda (decoder)
      (let ((ret (proc (lz-decoder->pointer decoder))))
        "Return the version of current member from member header."
        (if (= ret -1)
            (throw 'lzlib-error 'lz-decompress-data-position
                   (lz-decompress-error decoder))
            ret)))))

(define lz-decompress-dictionary-size
  (let ((proc (lzlib-procedure int "LZ_decompress_dictionary_size" '(*))))
    (lambda (decoder)
      (let ((ret (proc (lz-decoder->pointer decoder))))
        "Return the dictionary size of current member from member header."
        (if (= ret -1)
            (throw 'lzlib-error 'lz-decompress-member-position
                   (lz-decompress-error decoder))
            ret)))))

(define lz-decompress-data-crc
  (let ((proc (lzlib-procedure unsigned-int "LZ_decompress_data_crc" '(*))))
    (lambda (decoder)
      (let ((ret (proc (lz-decoder->pointer decoder))))
        "Return the 32 bit Cyclic Redundancy Check of the data decompressed
from the current member.  The returned value is valid only when
`lz-decompress-member-finished' returns #t. "
        (if (= ret -1)
            (throw 'lzlib-error 'lz-decompress-member-position
                   (lz-decompress-error decoder))
            ret)))))

(define lz-decompress-data-position
  (let ((proc (lzlib-procedure uint64 "LZ_decompress_data_position" '(*))))
    (lambda (decoder)
      "Return the number of decompressed bytes already produced, but perhaps
not yet read, in the current member."
      (let ((ret (proc (lz-decoder->pointer decoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-decompress-data-position
                   (lz-decompress-error decoder))
            ret)))))

(define lz-decompress-member-position
  (let ((proc (lzlib-procedure uint64 "LZ_decompress_member_position" '(*))))
    (lambda (decoder)
      "Return the number of input bytes already decompressed in the current
member."
      (let ((ret (proc (lz-decoder->pointer decoder))))
        (if (= ret -1)
            (throw 'lzlib-error 'lz-decompress-member-position
                   (lz-decompress-error decoder))
            ret)))))

(define lz-decompress-total-in-size
  (let ((proc (lzlib-procedure uint64 "LZ_decompress_total_in_size" '(*))))
    (lambda (decoder)
      (let ((ret (proc (lz-decoder->pointer decoder))))
        "Return the total number of input bytes already compressed."
        (if (= ret -1)
            (throw 'lzlib-error 'lz-decompress-total-in-size
                   (lz-decompress-error decoder))
            ret)))))

(define lz-decompress-total-out-size
  (let ((proc (lzlib-procedure uint64 "LZ_decompress_total_out_size" '(*))))
    (lambda (decoder)
      (let ((ret (proc (lz-decoder->pointer decoder))))
        "Return the total number of compressed bytes already produced, but
perhaps not yet read."
        (if (= ret -1)
            (throw 'lzlib-error 'lz-decompress-total-out-size
                   (lz-decompress-error decoder))
            ret)))))


;; High level functions.
(define %lz-decompress-input-buffer-size (* 64 1024))

(define* (lzread! decoder file-port bv
                  #:optional (start 0) (count (bytevector-length bv)))
  "Read up to COUNT bytes from FILE-PORT into BV at offset START.  Return the
number of uncompressed bytes actually read; it is zero if COUNT is zero or if
the end-of-stream has been reached."
  ;; WARNING: Because we don't alternate between lz-reads and lz-writes, we can't
  ;; process more than %lz-decompress-input-buffer-size from the file-port.
  (when (> count %lz-decompress-input-buffer-size)
    (set! count %lz-decompress-input-buffer-size))
  (let* ((written 0)
         (read 0)
         (file-bv (get-bytevector-n file-port count)))
    (unless (eof-object? file-bv)
      (begin
        (while (and (< 0 (lz-decompress-write-size decoder))
                    (< written (bytevector-length file-bv)))
          (set! written (+ written
                           (lz-decompress-write decoder file-bv written
                                                (- (bytevector-length file-bv) written)))))))
    (let loop ((rd 0))
      (if (< start (bytevector-length bv))
          (begin
            (set! rd (lz-decompress-read decoder bv start (- (bytevector-length bv) start)))
            (set! start (+ start rd))
            (set! read (+ read rd)))
          (set! rd 0))
      (unless (= rd 0)
        (loop rd)))
    read))

(define* (lzwrite encoder bv lz-port
                  #:optional (start 0) (count (bytevector-length bv)))
  "Write up to COUNT bytes from BV at offset START into LZ-PORT.  Return
the number of uncompressed bytes written, a non-negative integer."
  (let ((written 0)
        (read 0))
    (while (and (< 0 (lz-compress-write-size encoder))
                (< written count))
      (set! written (+ written
                       (lz-compress-write encoder bv (+ start written) (- count written)))))
    (when (= written 0)
      (lz-compress-finish encoder))
    (let ((lz-bv (make-bytevector written)))
      (let loop ((rd 0))
        (set! rd (lz-compress-read encoder lz-bv 0 (bytevector-length lz-bv)))
        (put-bytevector lz-port lz-bv 0 rd)
        (set! read (+ read rd))
        (unless (= rd 0)
          (loop rd))))
    ;; `written' is the total byte count of uncompressed data.
    written))


;;;
;;; Port interface.
;;;

;; Alist of (levels (dictionary-size match-length-limit)).  0 is the fastest.
;; See bbexample.c in lzlib's source.
(define %compression-levels
  `((0 (65535 16))
    (1 (,(bitwise-arithmetic-shift-left 1 20) 5))
    (2 (,(bitwise-arithmetic-shift-left 3 19) 6))
    (3 (,(bitwise-arithmetic-shift-left 1 21) 8))
    (4 (,(bitwise-arithmetic-shift-left 3 20) 12))
    (5 (,(bitwise-arithmetic-shift-left 1 22) 20))
    (6 (,(bitwise-arithmetic-shift-left 1 23) 36))
    (7 (,(bitwise-arithmetic-shift-left 1 24) 68))
    (8 (,(bitwise-arithmetic-shift-left 3 23) 132))
    (9 (,(bitwise-arithmetic-shift-left 1 25) 273))))

(define %default-compression-level
  6)

(define* (make-lzip-input-port port)
  "Return an input port that decompresses data read from PORT, a file port.
PORT is automatically closed when the resulting port is closed."
  (define decoder (lz-decompress-open))

  (define (read! bv start count)
    (lzread! decoder port bv start count))

  (make-custom-binary-input-port "lzip-input" read! #f #f
                                 (lambda ()
                                   (lz-decompress-close decoder)
                                   (close-port port))))

(define* (make-lzip-output-port port
                                #:key
                                (level %default-compression-level))
  "Return an output port that compresses data at the given LEVEL, using PORT,
a file port, as its sink.  PORT is automatically closed when the resulting
port is closed."
  (define encoder (apply lz-compress-open
                         (car (assoc-ref %compression-levels level))))

  (define (write! bv start count)
    (lzwrite encoder bv port start count))

  (make-custom-binary-output-port "lzip-output" write! #f #f
                                  (lambda ()
                                    (lz-compress-finish encoder)
                                    ;; "lz-read" the trailing metadata added by `lz-compress-finish'.
                                    (let ((lz-bv (make-bytevector (* 64 1024))))
                                      (let loop ((rd 0))
                                        (set! rd (lz-compress-read encoder lz-bv 0 (bytevector-length lz-bv)))
                                        (put-bytevector port lz-bv 0 rd)
                                        (unless (= rd 0)
                                          (loop rd))))
                                    (lz-compress-close encoder)
                                    (close-port port))))

(define* (call-with-lzip-input-port port proc)
  "Call PROC with a port that wraps PORT and decompresses data read from it.
PORT is closed upon completion."
  (let ((lzip (make-lzip-input-port port)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc lzip))
      (lambda ()
        (close-port lzip)))))

(define* (call-with-lzip-output-port port proc
                                     #:key
                                     (level %default-compression-level))
  "Call PROC with an output port that wraps PORT and compresses data.  PORT is
close upon completion."
  (let ((lzip (make-lzip-output-port port
                                     #:level level)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc lzip))
      (lambda ()
        (close-port lzip)))))

;;; lzlib.scm ends here
