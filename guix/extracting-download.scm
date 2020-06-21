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

(define-module (guix extracting-download)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module ((guix build download) #:prefix build:)
  #:use-module ((guix build utils) #:hide (delete))
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages) ;; for %current-system
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (srfi srfi-26)
  #:export (http-fetch/extract
            download-to-store/extract))

;;;
;;; Produce fixed-output derivations with data extracted from n archive
;;; fetched over HTTP or FTP.
;;;
;;; This is meant to be used for package repositories where the actual source
;;; archive is packed into another archive, eventually carrying meta-data.
;;; Using this derivation saves both storing the outer archive and extracting
;;; the actual one at build time.  The hash is calculated on the actual
;;; archive to ease validating the stored file.
;;;

(define* (http-fetch/extract url filename-to-extract hash-algo hash
                    #:optional name
                    #:key (system (%current-system)) (guile (default-guile)))
  "Return a fixed-output derivation that fetches an archive at URL, and
extracts FILE_TO_EXTRACT from the archive.  The FILE_TO_EXTRACT is expected to
have hash HASH of type HASH-ALGO (a symbol).  By default, the file name is the
base name of URL; optionally, NAME can specify a different file name."
  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))

  (define guile-zlib
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-zlib))

  (define guile-json
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-4))

  (define gnutls
    (module-ref (resolve-interface '(gnu packages tls)) 'gnutls))

  (define inputs
    `(("tar" ,(module-ref (resolve-interface '(gnu packages base))
                          'tar))))

  (define config.scm
    (scheme-file "config.scm"
                 #~(begin
                     (define-module (guix config)
                       #:export (%system))

                     (define %system
                       #$(%current-system)))))

  (define modules
    (cons `((guix config) => ,config.scm)
          (delete '(guix config)
                  (source-module-closure '((guix build download)
                                           (guix build utils)
                                           (guix utils)
                                           (web uri))))))

  (define build
    (with-imported-modules modules
      (with-extensions (list guile-json gnutls ;for (guix swh)
                             guile-zlib)
        #~(begin
            (use-modules (guix build download)
                         (guix build utils)
                         (guix utils)
                         (web uri)
                         (ice-9 match)
                         (ice-9 popen))
            ;; The code below expects tar to be in $PATH.
            (set-path-environment-variable "PATH" '("bin")
                                           (match '#+inputs
                                             (((names dirs outputs ...) ...)
                                              dirs)))

            (setvbuf (current-output-port) 'line)
            (setvbuf (current-error-port) 'line)

            (call-with-temporary-directory
             (lambda (directory)
               ;; TODO: Support different archive types, based on content-type
               ;; or archive name extention.
               (let* ((file-to-extract (getenv "extract filename"))
                      (port (http-fetch (string->uri (getenv "download url"))
                                        #:verify-certificate? #f))
                      (tar (open-pipe* OPEN_WRITE "tar" "-C" directory
                                       "-xf" "-" file-to-extract)))
                 (dump-port port tar)
                 (close-port port)
                 (let ((status (close-pipe tar)))
                   (unless (zero? status)
                     (error "tar extraction failure" status)))
                 (copy-file (string-append directory "/"
                                           (getenv "extract filename"))
                            #$output))))))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name file-name) build

                      ;; Use environment variables and a fixed script name so
                      ;; there's only one script in store for all the
                      ;; downloads.
                      #:script-name "extract-download"
                      #:env-vars
                      `(("download url" . ,url)
                        ("extract filename" . ,filename-to-extract))
                      #:leaked-env-vars '("http_proxy" "https_proxy"
                                          "LC_ALL" "LC_MESSAGES" "LANG"
                                          "COLUMNS")
                      #:system system
                      #:local-build? #t           ; don't offload download
                      #:hash-algo hash-algo
                      #:hash hash
                      #:guile-for-build guile)))


(define* (download-to-store/extract store url filename-to-extract
                                    #:optional (name (basename url))
                                    #:key (log (current-error-port))
                                    (verify-certificate? #t))
  "Download an archive from URL, and extracts FILE_TO_EXTRACT from the archive
to STORE, either under NAME or URL's basename if omitted.  Write progress
reports to LOG.  VERIFY-CERTIFICATE? determines whether or not to validate
HTTPS server certificates."
  (call-with-temporary-output-file
   (lambda (temp port)
     (let ((result
            (parameterize ((current-output-port log))
              (build:url-fetch url temp
                               ;;#:mirrors %mirrors
                               #:verify-certificate?
                               verify-certificate?))))
       (close port)
       (and result
            (call-with-temporary-output-file
             (lambda (contents port)
               (let ((tar (open-pipe* OPEN_READ
                                      "tar"  ;"--auto-compress"
                                      "-xf" temp "--to-stdout" filename-to-extract)))
                 (dump-port tar port)
                 (close-port port)
                 (let ((status (close-pipe tar)))
                   (unless (zero? status)
                     (error "tar extraction failure" status)))
                 (add-to-store store name #f "sha256" contents)))))))))
