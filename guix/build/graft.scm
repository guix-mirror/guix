;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:export (replace-store-references
            rewrite-directory))

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

(define* (replace-store-references input output mapping
                                   #:optional (store (%store-directory)))
  "Read data from INPUT, replacing store references according to MAPPING, and
writing the result to OUTPUT."
  (define pattern
    (let ((nix-base32-chars
           '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
             #\a #\b #\c #\d #\f #\g #\h #\i #\j #\k #\l #\m #\n
             #\p #\q #\r #\s #\v #\w #\x #\y #\z)))
      `(,@(map char-set (string->list store))
        ,(char-set #\/)
        ,@(make-list 32 (list->char-set nix-base32-chars))
        ,(char-set #\-))))

  ;; We cannot use `regexp-exec' here because it cannot deal with strings
  ;; containing NUL characters, hence 'fold-port-matches'.
  (with-fluids ((%default-port-encoding #f))
    (when (file-port? input)
      (setvbuf input _IOFBF 65536))
    (when (file-port? output)
      (setvbuf output _IOFBF 65536))

    (let* ((len     (+ 34 (string-length store)))
           (mapping (map (match-lambda
                          ((origin . replacement)
                           (unless (string=? (string-drop origin len)
                                             (string-drop replacement len))
                             (error "invalid replacement" origin replacement))
                           (cons (string-take origin len)
                                 (string-take replacement len))))
                         mapping)))
     (fold-port-matches (lambda (string result)
                          (match (assoc-ref mapping string)
                            (#f
                             (put-bytevector output (string->utf8 string)))
                            ((= string->utf8 replacement)
                             (put-bytevector output replacement)))
                          #t)
                        #f
                        pattern
                        input
                        (lambda (char result)     ;unmatched
                          (put-u8 output (char->integer char))
                          result)))))

(define* (rewrite-directory directory output mapping
                            #:optional (store (%store-directory)))
  "Copy DIRECTORY to OUTPUT, replacing strings according to MAPPING, a list of
file name pairs."
  (define prefix-len
    (string-length directory))

  (define (destination file)
    (string-append output (string-drop file prefix-len)))

  (define (rewrite-leaf file stat result)
    (case (stat:type stat)
      ((symlink)
       (let ((target (readlink file)))
         (symlink (call-with-output-string
                   (lambda (output)
                     (replace-store-references (open-input-string target)
                                               output mapping
                                               store)))
                  (destination file))))
      ((regular)
       (with-fluids ((%default-port-encoding #f))
         (call-with-input-file file
           (lambda (input)
             (call-with-output-file (destination file)
               (lambda (output)
                 (replace-store-references input output mapping
                                           store)
                 (chmod output (stat:perms stat))))))))
      (else
       (error "unsupported file type" stat))))

  (file-system-fold (const #t)
                    rewrite-leaf
                    (lambda (directory stat result) ;down
                      (mkdir (destination directory)))
                    (const #t)                      ;up
                    (const #f)                      ;skip
                    (lambda (file stat errno result) ;error
                      (error "read error" file stat errno))
                    #f
                    directory
                    lstat))

;;; graft.scm ends here
