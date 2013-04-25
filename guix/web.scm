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

(define-module (guix web)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:export (http-fetch))

;;; Commentary:
;;;
;;; Web client portable among Guile versions.
;;;
;;; Code:

(define* (http-fetch uri #:key (text? #f))
  "Return an input port containing the data at URI, and the expected number of
bytes available or #f.  If TEXT? is true, the data at URI is considered to be
textual.  Follow any HTTP redirection."
  (let loop ((uri uri))
    (let*-values (((resp data)
                   ;; Try hard to use the API du jour to get an input port.
                   ;; On Guile 2.0.5 and before, we can only get a string or
                   ;; bytevector, and not an input port.  Work around that.
                   (if (version>? "2.0.7" (version))
                       (if (defined? 'http-get*)
                           (http-get* uri #:decode-body? text?) ; 2.0.7
                           (http-get uri #:decode-body? text?)) ; 2.0.5-
                       (http-get uri #:streaming? #t)))         ; 2.0.9+
                  ((code)
                   (response-code resp)))
      (case code
        ((200)
         (let ((len (response-content-length resp)))
           (cond ((not data)
                  (begin
                    ;; XXX: Guile 2.0.5 and earlier did not support chunked
                    ;; transfer encoding, which is required for instance when
                    ;; fetching %PACKAGE-LIST-URL (see
                    ;; <http://lists.gnu.org/archive/html/guile-devel/2011-09/msg00089.html>).
                    ;; Since users may still be using these versions, warn them
                    ;; and bail out.
                    (warning (_ "using Guile ~a, ~a ~s encoding~%")
                             (version)
                             "which does not support HTTP"
                             (response-transfer-encoding resp))
                    (leave (_ "download failed; use a newer Guile~%")
                           uri resp)))
                 ((string? data)                   ; `http-get' from 2.0.5-
                  (values (open-input-string data) len))
                 ((bytevector? data)               ; likewise
                  (values (open-bytevector-input-port data) len))
                 (else                             ; input port
                  (values data len)))))
        ((301                                      ; moved permanently
          302)                                     ; found (redirection)
         (let ((uri (response-location resp)))
           (format #t "following redirection to `~a'...~%"
                   (uri->string uri))
           (loop uri)))
        (else
         (error "download failed" uri code
                (response-reason-phrase resp)))))))

;;; web.scm ends here
