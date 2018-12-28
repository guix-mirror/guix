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

(define-module (guix ipfs)
  #:use-module (json)
  #:use-module (guix base64)
  #:use-module ((guix build utils) #:select (dump-port))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:export (%ipfs-base-url
            add-data
            add-file

            content?
            content-name
            content-hash
            content-size

            add-empty-directory
            add-to-directory
            read-contents
            publish-name))

;;; Commentary:
;;;
;;; This module implements bindings for the HTTP interface of the IPFS
;;; gateway, documented here: <https://docs.ipfs.io/reference/api/http/>.  It
;;; allows you to add and retrieve files over IPFS, and a few other things.
;;;
;;; Code:

(define %ipfs-base-url
  ;; URL of the IPFS gateway.
  (make-parameter "http://localhost:5001"))

(define* (call url decode #:optional (method http-post)
               #:key body (false-if-404? #t) (headers '()))
  "Invoke the endpoint at URL using METHOD.  Decode the resulting JSON body
using DECODE, a one-argument procedure that takes an input port; when DECODE
is false, return the input port.  When FALSE-IF-404? is true, return #f upon
404 responses."
  (let*-values (((response port)
                 (method url #:streaming? #t
                         #:body body

                         ;; Always pass "Connection: close".
                         #:keep-alive? #f
                         #:headers `((connection close)
                                     ,@headers))))
    (cond ((= 200 (response-code response))
           (if decode
               (let ((result (decode port)))
                 (close-port port)
                 result)
               port))
          ((and false-if-404?
                (= 404 (response-code response)))
           (close-port port)
           #f)
          (else
           (close-port port)
           (throw 'ipfs-error url response)))))

;; Result of a file addition.
(define-json-mapping <content> make-content content?
  json->content
  (name   content-name "Name")
  (hash   content-hash "Hash")
  (bytes  content-bytes "Bytes")
  (size   content-size "Size" string->number))

;; Result of a 'patch/add-link' operation.
(define-json-mapping <directory> make-directory directory?
  json->directory
  (hash   directory-hash "Hash")
  (links  directory-links "Links" json->links))

;; A "link".
(define-json-mapping <link> make-link link?
  json->link
  (name   link-name "Name")
  (hash   link-hash "Hash")
  (size   link-size "Size" string->number))

;; A "binding", also known as a "name".
(define-json-mapping <binding> make-binding binding?
  json->binding
  (name   binding-name "Name")
  (value  binding-value "Value"))

(define (json->links json)
  (match json
    (#f    '())
    (links (map json->link links))))

(define %multipart-boundary
  ;; XXX: We might want to find a more reliable boundary.
  (string-append (make-string 24 #\-) "2698127afd7425a6"))

(define (bytevector->form-data bv port)
  "Write to PORT a 'multipart/form-data' representation of BV."
  (display (string-append "--" %multipart-boundary "\r\n"
                          "Content-Disposition: form-data\r\n"
                          "Content-Type: application/octet-stream\r\n\r\n")
           port)
  (put-bytevector port bv)
  (display (string-append "\r\n--" %multipart-boundary "--\r\n")
           port))

(define* (add-data data #:key (name "file.txt") recursive?)
  "Add DATA, a bytevector, to IPFS.  Return a content object representing it."
  (call (string-append (%ipfs-base-url)
                       "/api/v0/add?arg=" (uri-encode name)
                       "&recursive="
                       (if recursive? "true" "false"))
        json->content
        #:headers
        `((content-type
           . (multipart/form-data
              (boundary . ,%multipart-boundary))))
        #:body
        (call-with-bytevector-output-port
         (lambda (port)
           (bytevector->form-data data port)))))

(define (not-dot? entry)
  (not (member entry '("." ".."))))

(define* (add-file file #:key (name (basename file)))
  "Add FILE under NAME to the IPFS and return a content object for it."
  (add-data (match (call-with-input-file file get-bytevector-all)
              ((? eof-object?) #vu8())
              (bv bv))
            #:name name))

(define* (add-empty-directory #:key (name "directory"))
  "Return a content object for an empty directory."
  (add-data #vu8() #:recursive? #t #:name name))

(define* (add-to-directory directory file name)
  "Add FILE to DIRECTORY under NAME, and return the resulting directory.
DIRECTORY and FILE must be hashes identifying objects in the IPFS store."
  (call (string-append (%ipfs-base-url)
                       "/api/v0/object/patch/add-link?arg="
                       (uri-encode directory)
                       "&arg=" (uri-encode name) "&arg=" (uri-encode file)
                       "&create=true")
        json->directory))

(define* (read-contents object #:key offset length)
  "Return an input port to read the content of OBJECT from."
  (call (string-append (%ipfs-base-url)
                       "/api/v0/cat?arg=" object)
        #f))

(define* (publish-name object)
  "Publish OBJECT under the current peer ID."
  (call (string-append (%ipfs-base-url)
                       "/api/v0/name/publish?arg=" object)
        json->binding))
