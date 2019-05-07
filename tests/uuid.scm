;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-uuid)
  #:use-module (gnu system uuid)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors))

(test-begin "uuid")

(test-equal "uuid->string"
  "c5307e6b-d1ba-499d-89c5-cb0b143577c4"
  (uuid->string
   #vu8(197 48 126 107 209 186 73 157 137 197 203 11 20 53 119 196)))

(test-equal "string->uuid"
  '(16 "4dab5feb-d176-45de-b287-9b0a6e4c01cb")
  (let ((uuid (string->uuid "4dab5feb-d176-45de-b287-9b0a6e4c01cb")))
    (list (bytevector-length uuid) (uuid->string uuid))))

(test-assert "uuid"
  (let ((str "4dab5feb-d176-45de-b287-9b0a6e4c01cb"))
    (bytevector=? (uuid-bytevector
                   (uuid "4dab5feb-d176-45de-b287-9b0a6e4c01cb"))
                  (string->uuid "4dab5feb-d176-45de-b287-9b0a6e4c01cb"))))

(test-assert "uuid, syntax error"
  (catch 'syntax-error
    (lambda ()
      (eval '(uuid "foobar") (current-module))
      #f)
    (lambda (key proc message location form . args)
      (and (eq? proc 'uuid)
           (string-contains message "invalid UUID")
           (equal? form '(uuid "foobar" 'dce))))))

(test-equal "uuid, ISO-9660, format preserved"
  "1970-01-01-17-14-42-99"
  (uuid->string (uuid "1970-01-01-17-14-42-99" 'iso9660)))

(test-equal "uuid, FAT32, format preserved"
  "1234-ABCD"
  (uuid->string (uuid "1234-abcd" 'fat32)))

(test-equal "uuid, FAT32, leading zeros preserved"
  "00CA-050E"                                    ;<https://bugs.gnu.org/35582>
  (uuid->string (uuid "00CA-050E" 'fat32)))

(test-assert "uuid, dynamic value"
  (let* ((good "4dab5feb-d176-45de-b287-9b0a6e4c01cb")
         (bad  (string-drop good 3)))
    (and (uuid? (uuid good))
         (string=? good (uuid->string (uuid good)))
         (not (uuid bad)))))

(test-assert "uuid=?"
  (and (uuid=? (uuid-bytevector (uuid "1234-abcd" 'fat32))
               (uuid "1234-abcd" 'fat32))
       (uuid=? (uuid "1234-abcd" 'fat32)
               (uuid "1234-abcd" 'fat))))

(test-end)
