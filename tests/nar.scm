;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix tests)
  #:use-module (guix nar)
  #:use-module (guix serialization)
  #:use-module (guix store)
  #:use-module ((gcrypt hash)
                #:select (open-sha256-port open-sha256-input-port))
  #:use-module ((guix packages)
                #:select (base32))
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module ((ice-9 control) #:select (let/ec))
  #:use-module (ice-9 match))

;; Test the (guix nar) module.


;;;
;;; File system testing tools, initially contributed to Guile, then libchop.
;;;

(define (random-file-size)
  (define %average (* 1024 512))                  ; 512 KiB
  (define %stddev  (* 1024 64))                   ; 64 KiB
  (inexact->exact
   (max 0 (round (+ %average (* %stddev (random:normal)))))))

(define (make-file-tree dir tree)
  "Make file system TREE at DIR."
  (let loop ((dir  dir)
             (tree tree))
    (define (scope file)
      (string-append dir "/" file))

    (match tree
      (('directory name (body ...))
       (mkdir (scope name))
       (for-each (cute loop (scope name) <>) body))
      (('directory name (? integer? mode) (body ...))
       (mkdir (scope name))
       (for-each (cute loop (scope name) <>) body)
       (chmod (scope name) mode))
      ((file)
       (populate-file (scope file) (random-file-size)))
      ((file (? integer? mode))
       (populate-file (scope file) (random-file-size))
       (chmod (scope file) mode))
      ((from '-> to)
       (symlink to (scope from))))))

(define (delete-file-tree dir tree)
  "Delete file TREE from DIR."
  (let loop ((dir  dir)
             (tree tree))
    (define (scope file)
      (string-append dir "/" file))

    (match tree
      (('directory name (body ...))
       (for-each (cute loop (scope name) <>) body)
       (rmdir (scope name)))
      (('directory name (? integer? mode) (body ...))
       (chmod (scope name) #o755)          ; make sure it can be entered
       (for-each (cute loop (scope name) <>) body)
       (rmdir (scope name)))
      ((from '-> _)
       (delete-file (scope from)))
      ((file _ ...)
       (delete-file (scope file))))))

(define-syntax-rule (with-file-tree dir tree body ...)
  (dynamic-wind
    (lambda ()
      (make-file-tree dir 'tree))
    (lambda ()
      body ...)
    (lambda ()
      (delete-file-tree dir 'tree))))

(define (file-tree-equal? input output)
  "Return #t if the file trees at INPUT and OUTPUT are equal."
  (define strip
    (cute string-drop <> (string-length input)))
  (define sibling
    (compose (cut string-append output <>) strip))

  (file-system-fold (const #t)
                    (lambda (name stat result)    ; leaf
                      (and result
                           (file=? name (sibling name))))
                    (lambda (name stat result)    ; down
                      result)
                    (lambda (name stat result)    ; up
                      result)
                    (const #f)                    ; skip
                    (lambda (name stat errno result)
                      (pk 'error name stat errno)
                      #f)
                    #t                            ; result
                    input
                    lstat))

(define (populate-file file size)
  (call-with-output-file file
    (lambda (p)
      (put-bytevector p (random-bytevector size)))))

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

(define %test-dir
  ;; An output directory under $top_builddir.
  (string-append (dirname (search-path %load-path "pre-inst-env"))
                 "/test-nar-" (number->string (getpid))))


(test-begin "nar")

(test-assert "write-file-tree + restore-file"
  (let* ((file1  (search-path %load-path "guix.scm"))
         (file2  (search-path %load-path "guix/base32.scm"))
         (file3  "#!/bin/something")
         (output (string-append %test-dir "/output")))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (define-values (port get-bytevector)
          (open-bytevector-output-port))
        (write-file-tree "root" port
                         #:file-type+size
                         (match-lambda
                           ("root"
                            (values 'directory 0))
                           ("root/foo"
                            (values 'regular (stat:size (stat file1))))
                           ("root/lnk"
                            (values 'symlink 0))
                           ("root/dir"
                            (values 'directory 0))
                           ("root/dir/bar"
                            (values 'regular (stat:size (stat file2))))
                           ("root/dir/exe"
                            (values 'executable (string-length file3))))
                         #:file-port
                         (match-lambda
                           ("root/foo" (open-input-file file1))
                           ("root/dir/bar" (open-input-file file2))
                           ("root/dir/exe" (open-input-string file3)))
                         #:symlink-target
                         (match-lambda
                           ("root/lnk" "foo"))
                         #:directory-entries
                         (match-lambda
                           ("root" '("foo" "dir" "lnk"))
                           ("root/dir" '("bar" "exe"))))
        (close-port port)

        (rm-rf %test-dir)
        (mkdir %test-dir)
        (restore-file (open-bytevector-input-port (get-bytevector))
                      output)
        (and (file=? (string-append output "/foo") file1)
             (string=? (readlink (string-append output "/lnk"))
                       "foo")
             (file=? (string-append output "/dir/bar") file2)
             (string=? (call-with-input-file (string-append output "/dir/exe")
                         get-string-all)
                       file3)
             (> (logand (stat:mode (lstat (string-append output "/dir/exe")))
                        #o100)
                0)
             (equal? '("." ".." "bar" "exe")
                     (scandir (string-append output "/dir")))
             (equal? '("." ".." "dir" "foo" "lnk")
                     (scandir output))))
      (lambda ()
        (false-if-exception (rm-rf %test-dir))))))

(test-assert "write-file supports non-file output ports"
  (let ((input  (string-append (dirname (search-path %load-path "guix.scm"))
                               "/guix"))
        (output (%make-void-port "w")))
    (write-file input output)
    #t))

(test-equal "write-file puts file in C locale collation order"
  (base32 "0sfn5r63k88w9ls4hivnvscg82bqg8a0w7955l6xlk4g96jnb2z3")
  (let ((input (string-append %test-dir ".input")))
    (dynamic-wind
      (lambda ()
        (define (touch file)
          (call-with-output-file (string-append input "/" file)
            (const #t)))

        (mkdir input)
        (touch "B")
        (touch "Z")
        (touch "a")
        (symlink "B" (string-append input "/z")))
      (lambda ()
        (let-values (((port get-hash) (open-sha256-port)))
          (write-file input port)
          (close-port port)
          (get-hash)))
      (lambda ()
        (rm-rf input)))))

(test-equal "restore-file with incomplete input"
  (string-append %test-dir "/foo")
  (let ((port (open-bytevector-input-port #vu8(1 2 3))))
    (guard (c ((nar-error? c)
               (and (eq? port (nar-error-port c))
                    (nar-error-file c))))
      (restore-file port (string-append %test-dir "/foo"))
      #f)))

(test-assert "write-file + restore-file"
  (let* ((input  (string-append (dirname (search-path %load-path "guix.scm"))
                                "/guix"))
         (output %test-dir)
         (nar    (string-append output ".nar")))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (call-with-output-file nar
          (cut write-file input <>))
        (call-with-input-file nar
          (cut restore-file <> output))
        (file-tree-equal? input output))
      (lambda ()
        (false-if-exception (delete-file nar))
        (false-if-exception (rm-rf output))))))

(test-assert "write-file + restore-file with symlinks"
  (let ((input (string-append %test-dir ".input")))
    (mkdir input)
    (dynamic-wind
      (const #t)
      (lambda ()
        (with-file-tree input
            (directory "root"
                       (("reg") ("exe" #o777) ("sym" -> "reg")))
          (let* ((output %test-dir)
                 (nar    (string-append output ".nar")))
            (dynamic-wind
              (lambda () #t)
              (lambda ()
                (call-with-output-file nar
                  (cut write-file input <>))
                (call-with-input-file nar
                  (cut restore-file <> output))
                (file-tree-equal? input output))
              (lambda ()
                (false-if-exception (delete-file nar))
                (false-if-exception (rm-rf output)))))))
      (lambda ()
        (rmdir input)))))

(test-assert "write-file #:select? + restore-file"
  (let ((input (string-append %test-dir ".input")))
    (mkdir input)
    (dynamic-wind
      (const #t)
      (lambda ()
        (with-file-tree input
            (directory "root"
                       ((directory "a" (("x") ("y") ("z")))
                        ("b") ("c") ("d" -> "b")))
          (let* ((output %test-dir)
                 (nar    (string-append output ".nar")))
            (dynamic-wind
              (lambda () #t)
              (lambda ()
                (call-with-output-file nar
                  (lambda (port)
                    (write-file input port
                                #:select?
                                (lambda (file stat)
                                  (and (not (string=? (basename file)
                                                      "a"))
                                       (not (eq? (stat:type stat)
                                                 'symlink)))))))
                (call-with-input-file nar
                  (cut restore-file <> output))

                ;; Make sure "a" and "d" have been filtered out.
                (and (not (file-exists? (string-append output "/root/a")))
                     (file=? (string-append output "/root/b")
                             (string-append input "/root/b"))
                     (file=? (string-append output "/root/c")
                             (string-append input "/root/c"))
                     (not (file-exists? (string-append output "/root/d")))))
              (lambda ()
                (false-if-exception (delete-file nar))
                (false-if-exception (rm-rf output)))))))
      (lambda ()
        (rmdir input)))))

;; 'restore-file-set' depends on 'open-sha256-input-port', which in turn
;; relies on a Guile 2.0.10+ feature.
(test-skip (if (false-if-exception
                (open-sha256-input-port (%make-void-port "r")))
               0
               3))

(test-assert "restore-file-set (signed, valid)"
  (with-store store
    (let* ((texts (unfold (cut >= <> 10)
                          (lambda _ (random-text))
                          1+
                          0))
           (files (map (cut add-text-to-store store "text" <>) texts))
           (dump  (call-with-bytevector-output-port
                   (cut export-paths store files <>))))
      (delete-paths store files)
      (and (every (negate file-exists?) files)
           (let* ((source   (open-bytevector-input-port dump))
                  (imported (restore-file-set source)))
             (and (equal? imported files)
                  (every (lambda (file)
                           (and (file-exists? file)
                                (valid-path? store file)))
                         files)
                  (equal? texts
                          (map (lambda (file)
                                 (call-with-input-file file
                                   get-string-all))
                               files))))))))

(test-assert "restore-file-set (missing signature)"
  (let/ec return
    (with-store store
      (let* ((file  (add-text-to-store store "foo" (random-text)))
             (dump  (call-with-bytevector-output-port
                     (cute export-paths store (list file) <>
                           #:sign? #f))))
        (delete-paths store (list file))
        (and (not (file-exists? file))
             (let ((source (open-bytevector-input-port dump)))
               (guard (c ((nar-signature-error? c)
                          (let ((message (condition-message c))
                                (port    (nar-error-port c)))
                            (return
                             (and (string-match "lacks.*signature" message)
                                  (string=? file (nar-error-file c))
                                  (eq? source port))))))
                 (restore-file-set source))
               #f))))))

(test-assert "restore-file-set (corrupt)"
  (let/ec return
    (with-store store
      (let* ((file  (add-text-to-store store "foo"
                                       (random-text)))
             (dump  (call-with-bytevector-output-port
                     (cute export-paths store (list file) <>))))
        (delete-paths store (list file))

        ;; Flip a byte in the file contents.
        (let* ((index 120)
               (byte  (bytevector-u8-ref dump index)))
          (bytevector-u8-set! dump index (logxor #xff byte)))

        (and (not (file-exists? file))
             (let ((source (open-bytevector-input-port dump)))
               (guard (c ((nar-invalid-hash-error? c)
                          (let ((message (condition-message c))
                                (port    (nar-error-port c)))
                            (return
                             (and (string-contains message "hash")
                                  (string=? file (nar-error-file c))
                                  (eq? source port))))))
                 (restore-file-set source))
               #f))))))

(test-end "nar")

;;; Local Variables:
;;; eval: (put 'with-file-tree 'scheme-indent-function 2)
;;; End:
