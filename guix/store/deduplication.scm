;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Caleb Ristvedt <caleb.ristvedt@cune.org>
;;; Copyright © 2018-2021 Ludovic Courtès <ludo@gnu.org>
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

;;; This houses stuff we do to files when they arrive at the store - resetting
;;; timestamps, deduplicating, etc.

(define-module (guix store deduplication)
  #:use-module (gcrypt hash)
  #:use-module ((guix build utils) #:hide (dump-port))
  #:use-module (guix build syscalls)
  #:use-module (guix base32)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (guix serialization)
  #:export (nar-sha256
            deduplicate
            dump-file/deduplicate
            copy-file/deduplicate))

;; TODO: Remove once 'dump-port' in (guix build utils) has an optional 'len'
;; parameter.
(define* (dump-port in out
                    #:optional len
                    #:key (buffer-size 16384))
  "Read LEN bytes from IN (or as much as possible if LEN is #f) and write it
to OUT, using chunks of BUFFER-SIZE bytes."
  (define buffer
    (make-bytevector buffer-size))

  (let loop ((total 0)
             (bytes (get-bytevector-n! in buffer 0
                                       (if len
                                           (min len buffer-size)
                                           buffer-size))))
    (or (eof-object? bytes)
        (and len (= total len))
        (let ((total (+ total bytes)))
          (put-bytevector out buffer 0 bytes)
          (loop total
                (get-bytevector-n! in buffer 0
                                   (if len
                                       (min (- len total) buffer-size)
                                       buffer-size)))))))

(define (nar-sha256 file)
  "Gives the sha256 hash of a file and the size of the file in nar form."
  (let-values (((port get-hash) (open-sha256-port)))
    (write-file file port)
    (force-output port)
    (let ((hash (get-hash))
          (size (port-position port)))
      (close-port port)
      (values hash size))))

(define (tempname-in directory)
  "Gives an unused temporary name under DIRECTORY. Not guaranteed to still be
unused by the time you create anything with that name, but a good shot."
  (let ((const-part (string-append directory "/.tmp-link-"
                                   (number->string (getpid)))))
    (let try ((guess-part
               (number->string (random most-positive-fixnum) 16)))
      (if (file-exists? (string-append const-part "-" guess-part))
          (try (number->string (random most-positive-fixnum) 16))
          (string-append const-part "-" guess-part)))))

(define* (get-temp-link target #:optional (link-prefix (dirname target)))
  "Like mkstemp!, but instead of creating a new file and giving you the name,
it creates a new hardlink to TARGET and gives you the name. Since
cross-file-system hardlinks don't work, the temp link must be created on the
same file system - where in that file system it is can be controlled by
LINK-PREFIX."
  (let try ((tempname (tempname-in link-prefix)))
    (catch 'system-error
      (lambda ()
        (link target tempname)
        tempname)
      (lambda args
        (if (= (system-error-errno args) EEXIST)
            (try (tempname-in link-prefix))
            (apply throw args))))))

(define (call-with-writable-file file store thunk)
  (if (string=? file store)
      (thunk)                       ;don't meddle with the store's permissions
      (let ((stat (lstat file)))
        (dynamic-wind
          (lambda ()
            (make-file-writable file))
          thunk
          (lambda ()
            (set-file-time file stat)
            (chmod file (stat:mode stat)))))))

(define-syntax-rule (with-writable-file file store exp ...)
  "Make FILE writable for the dynamic extent of EXP..., except if FILE is the
store."
  (call-with-writable-file file store (lambda () exp ...)))

;; There are 3 main kinds of errors we can get from hardlinking: "Too many
;; things link to this" (EMLINK), "this link already exists" (EEXIST), and
;; "can't fit more stuff in this directory" (ENOSPC).

(define* (replace-with-link target to-replace
                            #:key (swap-directory (dirname target))
                            (store (%store-directory)))
  "Atomically replace the file TO-REPLACE with a link to TARGET.  Use
SWAP-DIRECTORY as the directory to store temporary hard links.  Upon ENOSPC
and EMLINK, TO-REPLACE is left unchanged.

Note: TARGET, TO-REPLACE, and SWAP-DIRECTORY must be on the same file system."
  (define temp-link
    (catch 'system-error
      (lambda ()
        (get-temp-link target swap-directory))
      (lambda args
        ;; We get ENOSPC when we can't fit an additional entry in
        ;; SWAP-DIRECTORY.  If it's EMLINK, then TARGET has reached its
        ;; maximum number of links.
        (if (memv (system-error-errno args) `(,ENOSPC ,EMLINK))
            #f
            (apply throw args)))))

  ;; If we couldn't create TEMP-LINK, that's OK: just don't do the
  ;; replacement, which means TO-REPLACE won't be deduplicated.
  (when temp-link
    (with-writable-file (dirname to-replace) store
      (catch 'system-error
        (lambda ()
          (rename-file temp-link to-replace))
        (lambda args
          (delete-file temp-link)
          (unless (= EMLINK (system-error-errno args))
            (apply throw args)))))))

(define %deduplication-minimum-size
  ;; Size below which files are not deduplicated.  This avoids adding too many
  ;; entries to '.links', which would slow down 'removeUnusedLinks' while
  ;; saving little space.  Keep in sync with optimize-store.cc.
  8192)

(define* (deduplicate path hash #:key (store (%store-directory)))
  "Check if a store item with sha256 hash HASH already exists.  If so,
replace PATH with a hardlink to the already-existing one.  If not, register
PATH so that future duplicates can hardlink to it.  PATH is assumed to be
under STORE."
  ;; Lightweight promises.
  (define-syntax-rule (delay exp)
    (let ((value #f))
      (lambda ()
        (unless value
          (set! value exp))
        value)))
  (define-syntax-rule (force promise)
    (promise))

  (define links-directory
    (string-append store "/.links"))

  (let loop ((path path)
             (type (stat:type (lstat path)))
             (hash hash))
    (if (eq? 'directory type)
        ;; Can't hardlink directories, so hardlink their atoms.
        (for-each (match-lambda
                    ((file . properties)
                     (unless (member file '("." ".."))
                       (let* ((file (string-append path "/" file))
                              (st   (delay (lstat file)))
                              (type (match (assoc-ref properties 'type)
                                      ((or 'unknown #f)
                                       (stat:type (force st)))
                                      (type type))))
                         (when (or (eq? 'directory type)
                                   (and (eq? 'regular type)
                                        (>= (stat:size (force st))
                                            %deduplication-minimum-size)))
                           (loop file type
                                 (and (not (eq? 'directory type))
                                      (nar-sha256 file))))))))
                  (scandir* path))
        (let ((link-file (string-append links-directory "/"
                                        (bytevector->nix-base32-string hash))))
          (if (file-exists? link-file)
              (replace-with-link link-file path
                                 #:swap-directory links-directory
                                 #:store store)
              (catch 'system-error
                (lambda ()
                  (link path link-file))
                (lambda args
                  (let ((errno (system-error-errno args)))
                    (cond ((= errno EEXIST)
                           ;; Someone else put an entry for PATH in
                           ;; LINKS-DIRECTORY before we could.  Let's use it.
                           (replace-with-link path link-file
                                              #:swap-directory
                                              links-directory
                                              #:store store))
                          ((= errno ENOENT)
                           ;; This most likely means that LINKS-DIRECTORY does
                           ;; not exist.  Attempt to create it and try again.
                           (mkdir-p links-directory)
                           (loop path type hash))
                          ((= errno ENOSPC)
                           ;; There's not enough room in the directory index for
                           ;; more entries in .links, but that's fine: we can
                           ;; just stop.
                           #f)
                          ((= errno EMLINK)
                           ;; PATH has reached the maximum number of links, but
                           ;; that's OK: we just can't deduplicate it more.
                           #f)
                          (else (apply throw args)))))))))))

(define (tee input len output)
  "Return a port that reads up to LEN bytes from INPUT and writes them to
OUTPUT as it goes."
  (define bytes-read 0)

  (define (fail)
    ;; Reached EOF before we had read LEN bytes from INPUT.
    (raise (condition
            (&nar-error (port input)
                        (file (port-filename output))))))

  (define (read! bv start count)
    ;; Read at most LEN bytes in total.
    (let ((count (min count (- len bytes-read))))
      (let loop ((ret (get-bytevector-n! input bv start count)))
        (cond ((eof-object? ret)
               (if (= bytes-read len)
                   0                              ; EOF
                   (fail)))
              ((and (zero? ret) (> count 0))
               ;; Do not return zero since zero means EOF, so try again.
               (loop (get-bytevector-n! input bv start count)))
              (else
               (put-bytevector output bv start ret)
               (set! bytes-read (+ bytes-read ret))
               ret)))))

  (make-custom-binary-input-port "tee input port" read! #f #f #f))

(define* (dump-file/deduplicate file input size type
                                #:key (store (%store-directory)))
  "Write SIZE bytes read from INPUT to FILE.  TYPE is a symbol, either
'regular or 'executable.

This procedure is suitable as a #:dump-file argument to 'restore-file'.  When
used that way, it deduplicates files on the fly as they are restored, thereby
removing the need for a deduplication pass that would re-read all the files
down the road."
  (define (dump-and-compute-hash)
    (call-with-output-file file
      (lambda (output)
        (let-values (((hash-port get-hash)
                      (open-hash-port (hash-algorithm sha256))))
          (write-file-tree file hash-port
                           #:file-type+size (lambda (_) (values type size))
                           #:file-port
                           (const (tee input size output)))
          (close-port hash-port)
          (get-hash)))))

  (if (>= size %deduplication-minimum-size)
      (deduplicate file (dump-and-compute-hash) #:store store)
      (call-with-output-file file
        (lambda (output)
          (dump-port input output size)))))

(define* (copy-file/deduplicate source target
                                #:key (store (%store-directory)))
  "Like 'copy-file', but additionally deduplicate TARGET in STORE."
  (call-with-input-file source
    (lambda (input)
      (let ((stat (stat input)))
        (dump-file/deduplicate target input (stat:size stat)
                               (if (zero? (logand (stat:mode stat)
                                                  #o100))
                                   'regular
                                   'executable)
                               #:store store)))))
