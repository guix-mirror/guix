;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2010, 2011, 2012, 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
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

(define-module (guix upstream)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module ((guix download)
                #:select (download-to-store))
  #:use-module ((guix build utils)
                #:select (substitute))
  #:use-module (guix gnupg)
  #:use-module (guix packages)
  #:use-module (guix ui)
  #:use-module (guix base32)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (upstream-source
            upstream-source?
            upstream-source-package
            upstream-source-version
            upstream-source-urls
            upstream-source-signature-urls
            upstream-source-archive-types

            coalesce-sources

            upstream-updater
            upstream-updater?
            upstream-updater-name
            upstream-updater-description
            upstream-updater-predicate
            upstream-updater-latest

            download-tarball
            package-update-path
            package-update
            update-package-source))

;;; Commentary:
;;;
;;; This module provides tools to represent and manipulate a upstream source
;;; code, and to auto-update package recipes.
;;;
;;; Code:

;; Representation of upstream's source.  There can be several URLs--e.g.,
;; tar.gz, tar.gz, etc.  There can be correspond signature URLs, one per
;; source URL.
(define-record-type* <upstream-source>
  upstream-source make-upstream-source
  upstream-source?
  (package        upstream-source-package)        ;string
  (version        upstream-source-version)        ;string
  (urls           upstream-source-urls)           ;list of strings
  (signature-urls upstream-source-signature-urls  ;#f | list of strings
                  (default #f)))

(define (upstream-source-archive-types release)
  "Return the available types of archives for RELEASE---a list of strings such
as \"gz\" or \"xz\"."
  (map file-extension (upstream-source-urls release)))

(define (coalesce-sources sources)
  "Coalesce the elements of SOURCES, a list of <upstream-source>, that
correspond to the same version."
  (define (same-version? r1 r2)
    (string=? (upstream-source-version r1) (upstream-source-version r2)))

  (define (release>? r1 r2)
    (version>? (upstream-source-version r1) (upstream-source-version r2)))

  (fold (lambda (release result)
          (match result
            ((head . tail)
             (if (same-version? release head)
                 (cons (upstream-source
                        (inherit release)
                        (urls (append (upstream-source-urls release)
                                      (upstream-source-urls head)))
                        (signature-urls
                         (let ((one (upstream-source-signature-urls release))
                               (two (upstream-source-signature-urls head)))
                           (and one two (append one two)))))
                       tail)
                 (cons release result)))
            (()
             (list release))))
        '()
        (sort sources release>?)))


;;;
;;; Auto-update.
;;;

(define-record-type* <upstream-updater>
  upstream-updater make-upstream-updater
  upstream-updater?
  (name        upstream-updater-name)
  (description upstream-updater-description)
  (pred        upstream-updater-predicate)
  (latest      upstream-updater-latest))

(define (lookup-updater package updaters)
  "Return an updater among UPDATERS that matches PACKAGE, or #f if none of
them matches."
  (any (match-lambda
         (($ <upstream-updater> _ _ pred latest)
          (and (pred package) latest)))
       updaters))

(define (package-update-path package updaters)
  "Return an upstream source to update PACKAGE to, or #f if no update is
needed or known."
  (match (lookup-updater package updaters)
    ((? procedure? latest-release)
     (match (latest-release (package-name package))
       ((and source ($ <upstream-source> name version))
        (and (version>? version (package-version package))
             source))
       (_ #f)))
    (#f #f)))

(define* (download-tarball store url signature-url
                           #:key (key-download 'interactive))
  "Download the tarball at URL to the store; check its OpenPGP signature at
SIGNATURE-URL, unless SIGNATURE-URL is false.  On success, return the tarball
file name.  KEY-DOWNLOAD specifies a download policy for missing OpenPGP keys;
allowed values: 'interactive' (default), 'always', and 'never'."
  (let ((tarball (download-to-store store url)))
    (if (not signature-url)
        tarball
        (let* ((sig (download-to-store store signature-url))
               (ret (gnupg-verify* sig tarball #:key-download key-download)))
          (if ret
              tarball
              (begin
                (warning (_ "signature verification failed for `~a'~%")
                         url)
                (warning (_ "(could be because the public key is not in your keyring)~%"))
                #f))))))

(define (find2 pred lst1 lst2)
  "Like 'find', but operate on items from both LST1 and LST2.  Return two
values: the item from LST1 and the item from LST2 that match PRED."
  (let loop ((lst1 lst1) (lst2 lst2))
    (match lst1
      ((head1 . tail1)
       (match lst2
         ((head2 . tail2)
          (if (pred head1 head2)
              (values head1 head2)
              (loop tail1 tail2)))))
      (()
       (values #f #f)))))

(define* (package-update store package updaters
                         #:key (key-download 'interactive))
  "Return the new version and the file name of the new version tarball for
PACKAGE, or #f and #f when PACKAGE is up-to-date.  KEY-DOWNLOAD specifies a
download policy for missing OpenPGP keys; allowed values: 'always', 'never',
and 'interactive' (default)."
  (match (package-update-path package updaters)
    (($ <upstream-source> _ version urls signature-urls)
     (let*-values (((name)
                    (package-name package))
                   ((archive-type)
                    (match (and=> (package-source package) origin-uri)
                      ((? string? uri)
                       (or (file-extension uri) "gz"))
                      (_
                       "gz")))
                   ((url signature-url)
                    (find2 (lambda (url sig-url)
                             (string-suffix? archive-type url))
                           urls
                           (or signature-urls (circular-list #f)))))
       (let ((tarball (download-tarball store url signature-url
                                        #:key-download key-download)))
         (values version tarball))))
    (#f
     (values #f #f))))

(define (update-package-source package version hash)
  "Modify the source file that defines PACKAGE to refer to VERSION,
whose tarball has SHA256 HASH (a bytevector).  Return the new version string
if an update was made, and #f otherwise."
  (define (new-line line matches replacement)
    ;; Iterate over MATCHES and return the modified line based on LINE.
    ;; Replace each match with REPLACEMENT.
    (let loop ((m* matches)                       ; matches
               (o  0)                             ; offset in L
               (r  '()))                          ; result
      (match m*
        (()
         (let ((r (cons (substring line o) r)))
           (string-concatenate-reverse r)))
        ((m . rest)
         (loop rest
               (match:end m)
               (cons* replacement
                      (substring line o (match:start m))
                      r))))))

  (define (update-source file old-version version
                         old-hash hash)
    ;; Update source file FILE, replacing occurrences OLD-VERSION by VERSION
    ;; and occurrences of OLD-HASH by HASH (base32 representation thereof).

    ;; TODO: Currently this is a bit of a sledgehammer: if VERSION occurs in
    ;; different unrelated places, we may modify it more than needed, for
    ;; instance.  We should try to make changes only within the sexp that
    ;; corresponds to the definition of PACKAGE.
    (let ((old-hash (bytevector->nix-base32-string old-hash))
          (hash     (bytevector->nix-base32-string hash)))
      (substitute file
                  `((,(regexp-quote old-version)
                     . ,(cut new-line <> <> version))
                    (,(regexp-quote old-hash)
                     . ,(cut new-line <> <> hash))))
      version))

  (let ((name (package-name package))
        (loc  (package-field-location package 'version)))
    (if loc
        (let ((old-version (package-version package))
              (old-hash    (origin-sha256 (package-source package)))
              (file        (and=> (location-file loc)
                                  (cut search-path %load-path <>))))
          (if file
              (update-source file
                             old-version version
                             old-hash hash)
              (begin
                (warning (_ "~a: could not locate source file")
                         (location-file loc))
                #f)))
        (begin
          (format (current-error-port)
                  (_ "~a: ~a: no `version' field in source; skipping~%")
                  (location->string (package-location package))
                  name)))))

;;; upstream.scm ends here
