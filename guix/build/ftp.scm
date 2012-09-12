;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix build ftp)
  #:use-module (guix ftp-client)
  #:use-module (guix build utils)
  #:use-module (web uri)
  #:export (ftp-fetch))

;;; Commentary:
;;;
;;; Fetch data such as tarballs over FTP (builder-side code).
;;;
;;; Code:

(define (ftp-fetch url file)
  "Fetch data from URL and write it to FILE.  Return FILE on success."

  (setvbuf (current-output-port) _IOLBF)
  (format #t "starting FTP download of `~a' from `~a'...~%" file url)
  (let* ((uri  (string->uri url))
         (conn (ftp-open (uri-host uri)))
         (in   (ftp-retr conn (basename (uri-path uri))
                         (dirname (uri-path uri)))))
    (call-with-output-file file
      (lambda (out)
        ;; TODO: Show a progress bar.
        (dump-port in out)))

    (ftp-close conn))
  file)

;;; ftp.scm ends here
