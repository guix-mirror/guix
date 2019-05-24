;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu system uuid)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:export (uuid
            uuid?
            uuid-type
            uuid-bytevector
            uuid=?

            bytevector->uuid

            uuid->string
            dce-uuid->string
            string->uuid
            string->dce-uuid
            string->iso9660-uuid
            string->ext2-uuid
            string->ext3-uuid
            string->ext4-uuid
            string->btrfs-uuid
            string->fat-uuid
            iso9660-uuid->string

            ;; XXX: For lack of a better place.
            sub-bytevector
            latin1->string))


;;;
;;; Tools that lack a better place.
;;;

(define (sub-bytevector bv start size)
  "Return a copy of the SIZE bytes of BV starting from offset START."
  (let ((result (make-bytevector size)))
    (bytevector-copy! bv start result 0 size)
    result))

(define (latin1->string bv terminator)
  "Return a string of BV, a latin1 bytevector, or #f.  TERMINATOR is a predicate
that takes a number and returns #t when a termination character is found."
    (let ((bytes (take-while (negate terminator) (bytevector->u8-list bv))))
      (if (null? bytes)
          #f
          (list->string (map integer->char bytes)))))


;;;
;;; DCE UUIDs.
;;;

(define-syntax %network-byte-order
  (identifier-syntax (endianness big)))

(define (dce-uuid->string uuid)
  "Convert UUID, a 16-byte bytevector, to its string representation, something
like \"6b700d61-5550-48a1-874c-a3d86998990e\"."
  ;; See <https://tools.ietf.org/html/rfc4122>.
  (let ((time-low  (bytevector-uint-ref uuid 0 %network-byte-order 4))
        (time-mid  (bytevector-uint-ref uuid 4 %network-byte-order 2))
        (time-hi   (bytevector-uint-ref uuid 6 %network-byte-order 2))
        (clock-seq (bytevector-uint-ref uuid 8 %network-byte-order 2))
        (node      (bytevector-uint-ref uuid 10 %network-byte-order 6)))
    (format #f "~8,'0x-~4,'0x-~4,'0x-~4,'0x-~12,'0x"
            time-low time-mid time-hi clock-seq node)))

(define %uuid-rx
  ;; The regexp of a UUID.
  (make-regexp "^([[:xdigit:]]{8})-([[:xdigit:]]{4})-([[:xdigit:]]{4})-([[:xdigit:]]{4})-([[:xdigit:]]{12})$"))

(define (string->dce-uuid str)
  "Parse STR as a DCE UUID (see <https://tools.ietf.org/html/rfc4122>) and
return its contents as a 16-byte bytevector.  Return #f if STR is not a valid
UUID representation."
  (and=> (regexp-exec %uuid-rx str)
         (lambda (match)
           (letrec-syntax ((hex->number
                            (syntax-rules ()
                              ((_ index)
                               (string->number (match:substring match index)
                                               16))))
                           (put!
                            (syntax-rules ()
                              ((_ bv index (number len) rest ...)
                               (begin
                                 (bytevector-uint-set! bv index number
                                                       (endianness big) len)
                                 (put! bv (+ index len) rest ...)))
                              ((_ bv index)
                               bv))))
             (let ((time-low  (hex->number 1))
                   (time-mid  (hex->number 2))
                   (time-hi   (hex->number 3))
                   (clock-seq (hex->number 4))
                   (node      (hex->number 5))
                   (uuid      (make-bytevector 16)))
               (put! uuid 0
                     (time-low 4) (time-mid 2) (time-hi 2)
                     (clock-seq 2) (node 6)))))))


;;;
;;; ISO-9660.
;;;

;; <http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-119.pdf>.

(define %iso9660-uuid-rx
  ;;                   Y                m                d                H                M                S                ss
  (make-regexp "^([[:digit:]]{4})-([[:digit:]]{2})-([[:digit:]]{2})-([[:digit:]]{2})-([[:digit:]]{2})-([[:digit:]]{2})-([[:digit:]]{2})$"))
(define (string->iso9660-uuid str)
  "Parse STR as a ISO9660 UUID (which is really a timestamp - see /dev/disk/by-uuid).
Return its contents as a 16-byte bytevector.  Return #f if STR is not a valid
ISO9660 UUID representation."
  (and=> (regexp-exec %iso9660-uuid-rx str)
         (lambda (match)
           (letrec-syntax ((match-numerals
                            (syntax-rules ()
                              ((_ index (name rest ...) body)
                               (let ((name (match:substring match index)))
                                 (match-numerals (+ 1 index) (rest ...) body)))
                              ((_ index () body)
                               body))))
            (match-numerals 1 (year month day hour minute second hundredths)
              (string->utf8 (string-append year month day
                                           hour minute second hundredths)))))))
(define (iso9660-uuid->string uuid)
  "Given an UUID bytevector, return its timestamp string."
  (define (digits->string bytes)
    (latin1->string bytes (lambda (c) #f)))
  (let* ((year (sub-bytevector uuid 0 4))
         (month (sub-bytevector uuid 4 2))
         (day (sub-bytevector uuid 6 2))
         (hour (sub-bytevector uuid 8 2))
         (minute (sub-bytevector uuid 10 2))
         (second (sub-bytevector uuid 12 2))
         (hundredths (sub-bytevector uuid 14 2))
         (parts (list year month day hour minute second hundredths)))
    (string-append (string-join (map digits->string parts) "-"))))


;;;
;;; FAT32/FAT16.
;;;

(define-syntax %fat-endianness
  ;; Endianness of FAT32/FAT16 file systems.
  (identifier-syntax (endianness little)))

(define (fat-uuid->string uuid)
  "Convert FAT32/FAT16 UUID, a 4-byte bytevector, to its string representation."
  (let ((high  (bytevector-uint-ref uuid 0 %fat-endianness 2))
        (low (bytevector-uint-ref uuid 2 %fat-endianness 2)))
    (format #f "~:@(~4,'0x-~4,'0x~)" low high)))

(define %fat-uuid-rx
  (make-regexp "^([[:xdigit:]]{4})-([[:xdigit:]]{4})$"))

(define (string->fat-uuid str)
  "Parse STR, which is in FAT32/FAT16 format, and return a bytevector or #f."
  (match (regexp-exec %fat-uuid-rx str)
    (#f
     #f)
    (rx-match
     (uint-list->bytevector (list (string->number
                                   (match:substring rx-match 2) 16)
                                  (string->number
                                   (match:substring rx-match 1) 16))
                            %fat-endianness
                            2))))


;;;
;;; Generic interface.
;;;

(define string->ext2-uuid string->dce-uuid)
(define string->ext3-uuid string->dce-uuid)
(define string->ext4-uuid string->dce-uuid)
(define string->btrfs-uuid string->dce-uuid)

(define-syntax vhashq
  (syntax-rules (=>)
    ((_)
     vlist-null)
    ((_ (key others ... => value) rest ...)
     (vhash-consq key value
                  (vhashq (others ... => value) rest ...)))
    ((_ (=> value) rest ...)
     (vhashq rest ...))))

(define %uuid-parsers
  (vhashq
   ('dce 'ext2 'ext3 'ext4 'btrfs 'luks => string->dce-uuid)
   ('fat32 'fat16 'fat => string->fat-uuid)
   ('iso9660 => string->iso9660-uuid)))

(define %uuid-printers
  (vhashq
   ('dce 'ext2 'ext3 'ext4 'btrfs 'luks => dce-uuid->string)
   ('iso9660 => iso9660-uuid->string)
   ('fat32 'fat16 'fat => fat-uuid->string)))

(define* (string->uuid str #:optional (type 'dce))
  "Parse STR as a UUID of the given TYPE.  On success, return the
corresponding bytevector; otherwise return #f."
  (match (vhash-assq type %uuid-parsers)
    (#f #f)
    ((_ . (? procedure? parse)) (parse str))))

;; High-level UUID representation that carries its type with it.
;;
;; This is necessary to serialize bytevectors with the right printer in some
;; circumstances.  For instance, GRUB "search --fs-uuid" command compares the
;; string representation of UUIDs, not the raw bytes; thus, when emitting a
;; GRUB 'search' command, we need to produce the right string representation
;; (see <https://debbugs.gnu.org/cgi/bugreport.cgi?msg=52;att=0;bug=27735>).
(define-record-type <uuid>
  (make-uuid type bv)
  uuid?
  (type  uuid-type)                               ;'dce | 'iso9660 | ...
  (bv    uuid-bytevector))

(define* (bytevector->uuid bv #:optional (type 'dce))
  "Return a UUID object make of BV and TYPE."
  (make-uuid type bv))

(define-syntax uuid
  (lambda (s)
    "Return the UUID object corresponding to the given UUID representation or
#f if the string could not be parsed."
    (syntax-case s (quote)
      ((_ str (quote type))
       (and (string? (syntax->datum #'str))
            (identifier? #'type))
       ;; A literal string: do the conversion at expansion time.
       (let ((bv (string->uuid (syntax->datum #'str)
                               (syntax->datum #'type))))
         (unless bv
           (syntax-violation 'uuid "invalid UUID" s))
         #`(make-uuid 'type #,(datum->syntax s bv))))
      ((_ str)
       (string? (syntax->datum #'str))
       #'(uuid str 'dce))
      ((_ str)
       #'(let ((bv (string->uuid str 'dce)))
           (and bv (make-uuid 'dce bv))))
      ((_ str type)
       #'(let ((bv (string->uuid str type)))
           (and bv (make-uuid type bv)))))))

(define uuid->string
  ;; Convert the given bytevector or UUID object, to the corresponding UUID
  ;; string representation.
  (match-lambda*
    (((? bytevector? bv))
     (uuid->string bv 'dce))
    (((? bytevector? bv) type)
     (match (vhash-assq type %uuid-printers)
       (#f #f)
       ((_ . (? procedure? unparse)) (unparse bv))))
    (((? uuid? uuid))
     (uuid->string (uuid-bytevector uuid) (uuid-type uuid)))))

(define uuid=?
  ;; Return true if A is equal to B, comparing only the actual bits.
  (match-lambda*
    (((? bytevector? a) (? bytevector? b))
     (bytevector=? a b))
    (((? uuid? a) (? bytevector? b))
     (bytevector=? (uuid-bytevector a) b))
    (((? uuid? a) (? uuid? b))
     (bytevector=? (uuid-bytevector a) (uuid-bytevector b)))
    ((a b)
     (uuid=? b a))))
