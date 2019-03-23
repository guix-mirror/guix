;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (guix import launchpad)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (web uri)
  #:use-module ((guix download) #:prefix download:)
  #:use-module (guix import json)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:export (%launchpad-updater))

(define (find-extension url)
  "Return the extension of the archive e.g. '.tar.gz' given a URL, or
false if none is recognized"
  (find (lambda (x) (string-suffix? x url))
        (list ".tar.gz" ".tar.bz2" ".tar.xz"
              ".zip" ".tar" ".tgz" ".tbz" ".love")))

(define (updated-launchpad-url old-package new-version)
  ;; Return a url for the OLD-PACKAGE with NEW-VERSION.  If no source url in
  ;; the OLD-PACKAGE is a Launchpad url, then return false.

  (define (updated-url url)
    (and (string-prefix? "https://launchpad.net/" url)
         (let ((ext (or (find-extension url) ""))
               (name (package-name old-package))
               (version (package-version old-package))
               (repo (launchpad-repository url)))
           (cond
            ((and
              (>= (length (string-split version #\.)) 2)
              (string=? (string-append "https://launchpad.net/"
                                       repo "/" (version-major+minor version)
                                       "/" version "/+download/" repo "-" version ext)
                        url))
             (string-append "https://launchpad.net/"
                            repo "/" (version-major+minor new-version)
                            "/" new-version "/+download/" repo "-" new-version ext))
            (#t #f))))) ; Some URLs are not recognised.

  (let ((source-uri (and=> (package-source old-package) origin-uri))
        (fetch-method (and=> (package-source old-package) origin-method)))
    (cond
     ((eq? fetch-method download:url-fetch)
      (match source-uri
             ((? string?)
              (updated-url source-uri))
             ((source-uri ...)
              (find updated-url source-uri))))
     (else #f))))

(define (launchpad-package? package)
  "Return true if PACKAGE is a package from Launchpad, else false."
  (->bool (updated-launchpad-url package "1.0.0")))

(define (launchpad-repository url)
  "Return a string e.g. linuxdcpp of the name of the repository, from a string
URL of the form
'https://launchpad.net/linuxdcpp/1.1/1.1.0/+download/linuxdcpp-1.1.0.tar.bz2'"
  (match (string-split (uri-path (string->uri url)) #\/)
    ((_ repo . rest) repo)))

(define (latest-released-version package-name)
  "Return a string of the newest released version name given the PACKAGE-NAME,
for example, 'linuxdcpp'. Return #f if there is no releases."
  (define (pre-release? x)
    ;; Versions containing anything other than digit characters and "." (for
    ;; example, "5.1.0-rc1") are assumed to be pre-releases.
    (not (string-every (char-set-union (char-set #\.)
                                       char-set:digit)
                       (hash-ref x "version"))))

  (hash-ref
   (last (remove
          pre-release?
          (hash-ref (json-fetch
                     (string-append "https://api.launchpad.net/1.0/"
                                    package-name "/releases"))
                    "entries")))
   "version"))

(define (latest-release pkg)
  "Return an <upstream-source> for the latest release of PKG."
  (define (origin-github-uri origin)
    (match (origin-uri origin)
      ((? string? url) url) ; surely a Launchpad URL
      ((urls ...)
       (find (cut string-contains <> "launchpad.net") urls))))

  (let* ((source-uri (origin-github-uri (package-source pkg)))
         (name (package-name pkg))
         (newest-version (latest-released-version name)))
    (if newest-version
        (upstream-source
         (package name)
         (version newest-version)
         (urls (list (updated-launchpad-url pkg newest-version))))
        #f))) ; On Launchpad but no proper releases

(define %launchpad-updater
  (upstream-updater
   (name 'launchpad)
   (description "Updater for Launchpad packages")
   (pred launchpad-package?)
   (latest latest-release)))
