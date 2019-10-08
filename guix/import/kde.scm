;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix import kde)
  #:use-module (guix http-client)
  #:use-module (guix memoization)
  #:use-module (guix gnu-maintenance)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-11)
  #:use-module (web uri)

  #:export (%kde-updater))

;;; Commentary:
;;;
;;; This package provides not an actual importer but simply an updater for
;;; KDE packages.  It grabs available files from the 'ls-lR.bz2' file
;;; available on download.kde.org.
;;;
;;; Code:

(define (tarball->version tarball)
  "Return the version TARBALL corresponds to.  TARBALL is a file name like
\"coreutils-8.23.tar.xz\"."
  (let-values (((name version)
                (gnu-package-name->name+version
                 (tarball-sans-extension tarball))))
    version))

(define %kde-file-list-uri
  ;; URI of the file list (ls -lR format) for download.kde.org.
  (string->uri "https://download.kde.org/ls-lR.bz2"))

(define (download.kde.org-files)
  ;;"Return the list of files available at download.kde.org."

    (define (ls-lR-line->filename path line)
      ;; Remove mode, blocks, user, group, size, date, time and one space,
      ;; then prepend PATH
      (regexp-substitute
       #f (string-match "^(\\S+\\s+){6}\\S+\\s" line) path 'post))

    (define (canonicalize path)
      (let* ((path (if (string-prefix? "/srv/archives/ftp/" path)
                       (string-drop path (string-length "/srv/archives/ftp"))
                       path))
             (path (if (string-suffix? ":" path)
                       (string-drop-right path 1)
                       path))
             (path (if (not (string-suffix? "/" path))
                       (string-append path "/")
                       path)))
        path))

    (define (write-cache input cache)
      "Read bzipped ls-lR from INPUT, and write it as a list of file paths to
CACHE."
      (call-with-decompressed-port 'bzip2 input
        (lambda (input)
          (let loop_dirs ((files '()))
            ;; process a new directory block
            (let ((path (read-line input)))
              (if
               (or (eof-object? path) (string= path ""))
               (write (reverse files) cache)
               (let loop_entries ((path (canonicalize path))
                                  (files files))
                 ;; process entries within the directory block
                 (let ((line (read-line input)))
                   (cond
                    ((eof-object? line)
                     (write (reverse files) cache))
                    ((string-prefix? "-" line)
                     ;; this is a file entry: prepend to FILES, then re-enter
                     ;; the loop for remaining entries
                     (loop_entries path
                                   (cons (ls-lR-line->filename path line) files)
                                   ))
                    ((not (string= line ""))
                     ;; this is a non-file entry: ignore it, just re-enter the
                     ;; loop for remaining entries
                     (loop_entries path files))
                    ;; empty line: directory block end, re-enter the outer
                    ;; loop for the next block
                    (#t (loop_dirs files)))))))))))

  (define (cache-miss uri)
    (format (current-error-port) "fetching ~a...~%" (uri->string uri)))

  (let* ((port (http-fetch/cached %kde-file-list-uri
                                  #:ttl 3600
                                  #:write-cache write-cache
                                  #:cache-miss cache-miss))
         (files (read port)))
    (close-port port)
    files))

(define (uri->kde-path-pattern uri)
  "Build a regexp from the package's URI suitable for matching the package
path version-agnostic.

Example:
Input:
   mirror://kde//stable/frameworks/5.55/portingAids/kross-5.55.0.zip
Output:
   //stable/frameworks/[^/]+/portingAids/
"

  (define version-regexp
    ;; regexp for matching versions as used in the ld-lR file
    (make-regexp
     (string-join '("^([0-9]+\\.)+[0-9]+-?"   ;; 5.12.90, 4.2.0-preview
                    "^[0-9]+$"                ;; 20031002
                    ".*-([0-9]+\\.)+[0-9]+$") ;; kdepim-4.6.1
                    "|")))

  (define (version->pattern part)
    ;; If a path element might be a version, replace it by a catch-all part
    (if (regexp-exec version-regexp part)
        "[^/]+"
        part))

  (let* ((path (uri-path uri))
         (directory-parts (string-split (dirname path) #\/)))
    (make-regexp
     (string-append
      (string-join (map version->pattern directory-parts) "/")
      "/"))))

(define (latest-kde-release package)
  "Return the latest release of PACKAGE, a KDE package, or #f if it could
not be determined."
  (let* ((uri      (string->uri (origin-uri (package-source package))))
         (path-rx  (uri->kde-path-pattern uri))
         (name     (package-upstream-name package))
         (files    (download.kde.org-files))
         (relevant (filter (lambda (file)
                             (and (regexp-exec path-rx file)
                                  (release-file? name (basename file))))
                           files)))
    (match (sort relevant (lambda (file1 file2)
                            (version>? (tarball-sans-extension
                                        (basename file1))
                                       (tarball-sans-extension
                                        (basename file2)))))
           ((and tarballs (reference _ ...))
            (let* ((version  (tarball->version reference))
                   (tarballs (filter (lambda (file)
                                       (string=? (tarball-sans-extension
                                                  (basename file))
                                                 (tarball-sans-extension
                                                  (basename reference))))
                                     tarballs)))
              (upstream-source
               (package name)
               (version version)
               (urls (map (lambda (file)
                            (string-append "mirror://kde/" file))
                          tarballs)))))
           (()
            #f))))

(define %kde-updater
  (upstream-updater
    (name 'kde)
    (description "Updater for KDE packages")
    (pred (url-prefix-predicate "mirror://kde/"))
    (latest latest-kde-release)))
