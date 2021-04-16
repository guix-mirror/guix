;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-ipfs)
  #:use-module (guix ipfs)
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module (guix tests)
  #:use-module (web uri)
  #:use-module (srfi srfi-64))

;; Test the (guix ipfs) module.

(define (ipfs-gateway-running?)
  "Return true if the IPFS gateway is running at %IPFS-BASE-URL."
  (let* ((uri    (string->uri (%ipfs-base-url)))
         (socket (socket AF_INET SOCK_STREAM 0)))
    (define connected?
      (catch 'system-error
        (lambda ()
          (format (current-error-port)
                  "probing IPFS gateway at localhost:~a...~%"
                  (uri-port uri))
          (connect socket AF_INET INADDR_LOOPBACK (uri-port uri))
          #t)
        (const #f)))

    (close-port socket)
    connected?))

(unless (ipfs-gateway-running?)
  (test-skip 1))

(test-assert "add-file-tree + restore-file-tree"
  (call-with-temporary-directory
   (lambda (directory)
     (let* ((source  (dirname (search-path %load-path "guix/base32.scm")))
            (target  (string-append directory "/r"))
            (content (pk 'content (add-file-tree source))))
       (restore-file-tree (content-name content) target)
       (file=? source target)))))
