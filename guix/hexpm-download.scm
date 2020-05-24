;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix hexpm-download)
  #:use-module (ice-9 match)
  #:use-module (guix extracting-download)
  #:use-module (guix packages) ;; for %current-system
  #:use-module (srfi srfi-26)
  #:export (hexpm-fetch

            %hexpm-repo-url
            hexpm-url
            hexpm-url?
            hexpm-uri))

;;;
;;; An <origin> method that fetches a package from the hex.pm repository,
;;; unwrapping the actual content from the download tarball.
;;;

;; URL and paths from
;; https://github.com/hexpm/specifications/blob/master/endpoints.md
(define %hexpm-repo-url
  (make-parameter "https://repo.hex.pm"))
(define hexpm-url
  (string-append (%hexpm-repo-url) "/tarballs/"))
(define hexpm-url?
  (cut string-prefix? hexpm-url <>))

(define (hexpm-uri name version)
  "Return a URI string for the package hosted at hex.pm corresponding to NAME
and VERSION."
  (string-append hexpm-url name "-" version ".tar"))

(define* (hexpm-fetch url hash-algo hash
                    #:optional name
                    #:key
                    (filename-to-extract "contents.tar.gz")
                    (system (%current-system))
                    (guile (default-guile)))
  "Return a fixed-output derivation that fetches URL and extracts
\"contents.tar.gz\".  The output is expected to have hash HASH of type
HASH-ALGO (a symbol).  By default, the file name is the base name of URL;
optionally, NAME can specify a different file name.  By default, the file name
is the base name of URL with \".gz\" appended; optionally, NAME can specify a
different file name."
  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))

  (http-fetch/extract url "contents.tar.gz" hash-algo hash
                      ;; urls typically end with .tar, but contents is .tar.gz
                      (or name (string-append file-name ".gz"))
                      #:system system #:guile guile))
