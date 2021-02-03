;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-store-database)
  #:use-module (guix tests)
  #:use-module (guix store)
  #:use-module (guix store database)
  #:use-module (guix build store-copy)
  #:use-module ((guix utils) #:select (call-with-temporary-output-file))
  #:use-module ((guix build utils)
                #:select (mkdir-p delete-file-recursively))
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

;; Test the (guix store database) module.

(define %store
  (open-connection-for-tests))


(test-begin "store-database")

(test-assert "register-items"
  (let ((file (string-append (%store-prefix) "/" (make-string 32 #\f)
                             "-fake")))
    (when (valid-path? %store file)
      (delete-paths %store (list file)))
    (false-if-exception (delete-file file))

    (let ((ref (add-text-to-store %store "ref-of-fake" (random-text)))
          (drv (string-append file ".drv")))
      (call-with-output-file file
        (cut display "This is a fake store item.\n" <>))
      (reset-timestamps file)
      (with-database (store-database-file) db
        (register-items db (list (store-info file drv (list ref)))))

      (and (valid-path? %store file)
           (equal? (references %store file) (list ref))
           (null? (valid-derivers %store file))
           (null? (referrers %store file))
           (list (stat:mtime (lstat file))
                 (stat:mtime (lstat ref)))))))

(test-equal "register-items, directory"
  '(1 1 1)
  (let ((file (string-append (%store-prefix) "/" (make-string 32 #\f)
                             "-fake-directory")))
    (when (valid-path? %store file)
      (delete-paths %store (list file)))
    (false-if-exception (delete-file-recursively file))

    (let ((drv (string-append file ".drv")))
      (mkdir-p (string-append file "/a"))
      (call-with-output-file (string-append file "/a/b")
        (const #t))
      (reset-timestamps file)
      (with-database (store-database-file) db
        (register-items db (list (store-info file drv '()))))

      (and (valid-path? %store file)
           (null? (references %store file))
           (null? (valid-derivers %store file))
           (null? (referrers %store file))
           (list (stat:mtime (lstat file))
                 (stat:mtime (lstat (string-append file "/a")))
                 (stat:mtime (lstat (string-append file "/a/b"))))))))

(test-equal "new database"
  (list 1 2)
  (call-with-temporary-output-file
   (lambda (db-file port)
     (delete-file db-file)
     (with-database db-file db
       (sqlite-register db
                        #:path "/gnu/foo"
                        #:references '()
                        #:deriver "/gnu/foo.drv"
                        #:hash (string-append "sha256:" (make-string 64 #\e))
                        #:nar-size 1234)
       (sqlite-register db
                        #:path "/gnu/bar"
                        #:references '("/gnu/foo")
                        #:deriver "/gnu/bar.drv"
                        #:hash (string-append "sha256:" (make-string 64 #\a))
                        #:nar-size 4321)
       (let ((path-id (@@ (guix store database) path-id)))
         (list (path-id db "/gnu/foo")
               (path-id db "/gnu/bar")))))))

(test-assert "sqlite-register with unregistered references"
  ;; Make sure we get a "NOT NULL constraint failed: Refs.reference" error
  ;; when we try to add references that are not registered yet.  Better safe
  ;; than sorry.
  (call-with-temporary-output-file
   (lambda (db-file port)
     (delete-file db-file)
     (catch 'sqlite-error
       (lambda ()
         (with-database db-file db
           (sqlite-register db #:path "/gnu/foo"
                            #:references '("/gnu/bar")
                            #:deriver "/gnu/foo.drv"
                            #:hash (string-append "sha256:" (make-string 64 #\e))
                            #:nar-size 1234))
         #f)
       (lambda args
         (pk 'welcome-exception! args)
         #t)))))

(test-equal "sqlite-register with incorrect size"
  'out-of-range
  (call-with-temporary-output-file
   (lambda (db-file port)
     (delete-file db-file)
     (catch #t
       (lambda ()
         (with-database db-file db
           (sqlite-register db #:path "/gnu/foo"
                            #:references '("/gnu/bar")
                            #:deriver "/gnu/foo.drv"
                            #:hash (string-append "sha256:" (make-string 64 #\e))
                            #:nar-size -1234))
         #f)
       (lambda (key . _)
         key)))))

(test-end "store-database")
