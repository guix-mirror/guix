;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
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

(define-module (guix build cargo-utils)
  #:use-module (guix build utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (generate-checksums))

;; Commentary:
;;
;; Stand alone utilities for building Rust crates or the compiler itself,
;; without depending on the entire cargo build-system itself.
;;
;; Code:

(define (file-sha256 file-name)
  "Calculate the hexdigest of the sha256 checksum of FILE-NAME and return it."
  (let ((port (open-pipe* OPEN_READ
                          "sha256sum"
                          "--"
                          file-name)))
    (let ((result (read-delimited " " port)))
      (close-pipe port)
      result)))

(define (generate-checksums dir-name)
  "Given DIR-NAME, a store directory, checksum all the files in it one
by one and put the result into the file \".cargo-checksum.json\" in
the same directory."
  (let* ((file-names (find-files dir-name "."))
         (dir-prefix-name (string-append dir-name "/"))
         (dir-prefix-name-len (string-length dir-prefix-name))
         (checksums-file-name (string-append dir-name "/.cargo-checksum.json")))
    (call-with-output-file checksums-file-name
      (lambda (port)
        (display "{\"files\":{" port)
        (let ((sep ""))
          (for-each (lambda (file-name)
            (let ((file-relative-name (string-drop file-name dir-prefix-name-len)))
                  (display sep port)
                  (set! sep ",")
                  (write file-relative-name port)
                  (display ":" port)
                  (write (file-sha256 file-name) port))) file-names))
        ;; NB: cargo requires the "package" field in order to check if the Cargo.lock
        ;; file needs to be regenerated when the value changes. However, it doesn't
        ;; appear to care what the value is to begin with...
        (display "},\"package\":" port)
        (write (file-sha256 "/dev/null") port)
        (display "}" port)))))
