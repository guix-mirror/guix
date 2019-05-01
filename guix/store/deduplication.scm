;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Caleb Ristvedt <caleb.ristvedt@cune.org>
;;; Copyright © 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix build utils)
  #:use-module (guix base16)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 ftw)
  #:use-module (guix serialization)
  #:export (nar-sha256
            deduplicate))

;; XXX: This port is used as a workaround on Guile <= 2.2.4 where
;; 'port-position' throws to 'out-of-range' when the offset is great than or
;; equal to 2^32: <https://bugs.gnu.org/32161>.
(define (counting-wrapper-port output-port)
  "Return two values: an output port that wraps OUTPUT-PORT, and a thunk to
retrieve the number of bytes written to OUTPUT-PORT."
  (let ((byte-count 0))
    (values (make-custom-binary-output-port "counting-wrapper"
                                            (lambda (bytes offset count)
                                              (put-bytevector output-port bytes
                                                              offset count)
                                              (set! byte-count
                                                (+ byte-count count))
                                              count)
                                            (lambda ()
                                              byte-count)
                                            #f
                                            (lambda ()
                                              (close-port output-port)))
            (lambda ()
              byte-count))))

(define (nar-sha256 file)
  "Gives the sha256 hash of a file and the size of the file in nar form."
  (let*-values (((port get-hash) (open-sha256-port))
                ((wrapper get-size) (counting-wrapper-port port)))
    (write-file file wrapper)
    (force-output wrapper)
    (force-output port)
    (let ((hash (get-hash))
          (size (get-size)))
      (close-port wrapper)
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

;; There are 3 main kinds of errors we can get from hardlinking: "Too many
;; things link to this" (EMLINK), "this link already exists" (EEXIST), and
;; "can't fit more stuff in this directory" (ENOSPC).

(define* (replace-with-link target to-replace
                            #:key (swap-directory (dirname target)))
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
    (let* ((parent (dirname to-replace))
           (stat   (stat parent)))
      (make-file-writable parent)
      (catch 'system-error
        (lambda ()
          (rename-file temp-link to-replace))
        (lambda args
          (delete-file temp-link)
          (unless (= EMLINK (system-error-errno args))
            (apply throw args))))

      ;; Restore PARENT's mtime and permissions.
      (set-file-time parent stat)
      (chmod parent (stat:mode stat)))))

(define* (deduplicate path hash #:key (store %store-directory))
  "Check if a store item with sha256 hash HASH already exists.  If so,
replace PATH with a hardlink to the already-existing one.  If not, register
PATH so that future duplicates can hardlink to it.  PATH is assumed to be
under STORE."
  (let* ((links-directory (string-append store "/.links"))
         (link-file       (string-append links-directory "/"
                                         (bytevector->base16-string hash))))
    (mkdir-p links-directory)
    (if (eq? 'directory (stat:type (lstat path)))
        ;; Can't hardlink directories, so hardlink their atoms.
        (for-each (lambda (file)
                    (unless (or (member file '("." ".."))
                                (and (string=? path store)
                                     (string=? file ".links")))
                      (let ((file (string-append path "/" file)))
                        (deduplicate file (nar-sha256 file)
                                     #:store store))))
                  (scandir path))
        (if (file-exists? link-file)
            (replace-with-link link-file path
                               #:swap-directory links-directory)
            (catch 'system-error
              (lambda ()
                (link path link-file))
              (lambda args
                (let ((errno (system-error-errno args)))
                  (cond ((= errno EEXIST)
                         ;; Someone else put an entry for PATH in
                         ;; LINKS-DIRECTORY before we could.  Let's use it.
                         (replace-with-link path link-file
                                            #:swap-directory links-directory))
                        ((= errno ENOSPC)
                         ;; There's not enough room in the directory index for
                         ;; more entries in .links, but that's fine: we can
                         ;; just stop.
                         #f)
                        ((= errno EMLINK)
                         ;; PATH has reached the maximum number of links, but
                         ;; that's OK: we just can't deduplicate it more.
                         #f)
                        (else (apply throw args))))))))))
