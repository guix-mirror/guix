;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages mysql)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module ((guix licenses) #:select (gpl2))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public mysql
  (package
    (name "mysql")
    (version "5.1.73")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://dev.mysql.com/get/Downloads/MySQL-5.1/mysql-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1dfwi4ck0vq6sdci6gz0031s7zz5lc3pddqlgm0292s00l9y5sq5"))))
    (build-system gnu-build-system)
    (inputs
     `(("procps" ,procps)
       ("openssl" ,openssl)
       ("perl" ,perl)
       ("zlib" ,zlib)
       ("ncurses" ,ncurses)))
    (arguments
     '(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw))                    ; for "rm -rf"
       #:phases (alist-cons-after
                 'install 'clean-up
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Remove the 112 MiB of tests that get installed.
                   (let ((out (assoc-ref outputs "out")))
                     (define (rm-rf dir)
                       (file-system-fold (const #t) ; enter?
                                         (lambda (file stat result) ; leaf
                                           (delete-file file))
                                         (const #t)               ; down
                                         (lambda (dir stat result) ; up
                                           (rmdir dir))
                                         (const #t)
                                         (lambda (file stat errno result)
                                           (format (current-error-port)
                                                   "error: ~a: ~a~%"
                                                   file (strerror errno)))
                                         #t
                                         (string-append out "/" dir)))
                     (rm-rf "mysql-test")
                     (rm-rf "sql-bench")

                     ;; Compress the 14 MiB Info file.
                     (zero?
                      (system* "gzip" "--best"
                               (string-append out "/share/info/mysql.info")))))
                 %standard-phases)))
    (home-page "http://www.mysql.com/")
    (synopsis "A fast, easy to use, and popular database")
    (description
     "MySQL is a fast, reliable, and easy to use relational database
management system that supports the standardized Structured Query
Language.")
    (license gpl2)))
