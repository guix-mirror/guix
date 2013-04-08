;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-nar)
  #:use-module (guix nar)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 ftw))

;; Test the (guix nar) module.

(define (rm-rf dir)
  (file-system-fold (const #t)                    ; enter?
                    (lambda (file stat result)    ; leaf
                      (delete-file file))
                    (const #t)                    ; down
                    (lambda (dir stat result)     ; up
                      (rmdir dir))
                    (const #t)                    ; skip
                    (const #t)                    ; error
                    #t
                    dir
                    lstat))


(test-begin "nar")

(test-assert "write-file + restore-file"
  (let* ((input  (string-append (dirname (search-path %load-path "guix.scm"))
                                "/guix"))
         (output (string-append (dirname input)
                                "/test-nar-"
                                (number->string (getpid))))
         (nar    (string-append output ".nar")))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (call-with-output-file nar
          (cut write-file input <>))
        (call-with-input-file nar
          (cut restore-file <> output))
        (let* ((strip   (cute string-drop <> (string-length input)))
               (sibling (compose (cut string-append output <>) strip))
               (file=?  (lambda (a b)
                          (and (eq? (stat:type (lstat a)) (stat:type (lstat b)))
                               (case (stat:type (lstat a))
                                 ((regular)
                                  (equal?
                                   (call-with-input-file a get-bytevector-all)
                                   (call-with-input-file b get-bytevector-all)))
                                 ((symlink)
                                  (string=? (readlink a) (readlink b)))
                                 (else
                                  (error "what?" (lstat a))))))))
          (file-system-fold (const #t)
                            (lambda (name stat result) ; leaf
                              (and result
                                   (file=? name (sibling name))))
                            (lambda (name stat result) ; down
                              result)
                            (lambda (name stat result) ; up
                              result)
                            (const #f)                 ; skip
                            (lambda (name stat errno result)
                              (pk 'error name stat errno)
                              #f)
                            (> (stat:nlink (stat output)) 2)
                            input
                            lstat)))
      (lambda ()
        (false-if-exception (delete-file nar))
        (false-if-exception (rm-rf output))
        ))))

(test-end "nar")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
