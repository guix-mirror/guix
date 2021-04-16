;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (guix scripts discover)
  #:use-module (guix avahi)
  #:use-module (guix config)
  #:use-module (guix scripts)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (guix scripts publish)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-37)
  #:export (read-substitute-urls

            guix-discover))

(define (show-help)
  (format #t (G_ "Usage: guix discover [OPTION]...
Discover Guix related services using Avahi.\n"))
  (display (G_ "
  -c, --cache=DIRECTORY     cache discovery results in DIRECTORY"))
  (display (G_ "
  -h, --help                display this help and exit"))
  (display (G_ "
  -V, --version             display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  (list (option '(#\c "cache") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'cache arg result)))
        (option '(#\h "help") #f #f
                (lambda _
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda _
                  (show-version-and-exit "guix discover")))))

(define %default-options
  `((cache . ,%state-directory)))


;;;
;;; Publish servers.
;;;

(define %publish-services
  ;; Set of discovered publish services.
  (make-hash-table))

(define (publish-file cache-directory)
  "Return the name of the file storing the discovered publish services inside
CACHE-DIRECTORY."
  (let ((directory (string-append cache-directory "/discover")))
    (string-append directory "/publish")))

(define %publish-file
  (make-parameter (publish-file %state-directory)))

(define* (write-publish-file #:key (file (%publish-file)))
  "Dump the content of %PUBLISH-SERVICES hash table into FILE.  Use a write
lock on FILE to synchronize with any potential readers."
  (with-atomic-file-output file
    (lambda (port)
      (hash-for-each
       (lambda (name service)
         (format port "http://~a:~a~%"
                 (avahi-service-address service)
                 (avahi-service-port service)))
       %publish-services)))
  (chmod file #o644))

(define* (read-substitute-urls #:key (file (%publish-file)))
  "Read substitute urls list from FILE and return it.  Use a read lock on FILE
to synchronize with the writer."
  (if (file-exists? file)
      (call-with-input-file file
        (lambda (port)
          (let loop ((url (read-line port))
                     (urls '()))
            (if (eof-object? url)
                urls
                (loop (read-line port) (cons url urls))))))
      '()))


;;;
;;; Entry point.
;;;

(define %services
  ;; List of services we want to discover.
  (list publish-service-type))

(define (service-proc action service)
  (let ((name (avahi-service-name service))
        (type (avahi-service-type service)))
    (when (string=? type publish-service-type)
      (case action
        ((new-service)
         (hash-set! %publish-services name service))
        ((remove-service)
         (hash-remove! %publish-services name)))
      (write-publish-file))))

(define-command (guix-discover . args)
  (category internal)
  (synopsis "discover Guix related services using Avahi")

  (with-error-handling
    (let* ((opts (parse-command-line args %options (list %default-options)
                                     #:build-options? #f
                                     #:argument-handler
                                     (lambda (arg result)
                                       (leave (G_ "~A: extraneous argument~%") arg))))
           (cache (assoc-ref opts 'cache))
           (publish-file (publish-file cache)))
      (parameterize ((%publish-file publish-file))
        (mkdir-p (dirname publish-file))
        (false-if-exception (delete-file publish-file))
        (avahi-browse-service-thread service-proc
                                     #:types %services)))))
