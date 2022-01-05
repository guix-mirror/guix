;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
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

(define-module (guix import git)
  #:use-module (guix build utils)
  #:use-module (guix diagnostics)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:export (%generic-git-updater

            ;; For tests.
            latest-git-tag-version))

;;; Commentary:
;;;
;;; This module provides a generic package updater for packages hosted on Git
;;; repositories.
;;;
;;; It tries to be smart about tag names, but if it is not automatically able
;;; to parse the tag names correctly, users can set the `release-tag-prefix',
;;; `release-tag-suffix' and `release-tag-version-delimiter' properties of the
;;; package to make the updater parse the Git tag name correctly.
;;;
;;; Possible improvements:
;;;
;;; * More robust method for trying to guess the delimiter.  Maybe look at the
;;;   previous version/tag combo to determine the delimiter.
;;;
;;; * Differentiate between "normal" versions, e.g., 1.2.3, and dates, e.g.,
;;;   2021.12.31.  Honor a `release-tag-date-scheme?' property?
;;;
;;; Code:

;;; Errors & warnings

(define-condition-type &git-no-valid-tags-error &error
  git-no-valid-tags-error?)

(define (git-no-valid-tags-error)
  (raise (condition (&message (message "no valid tags found"))
                    (&git-no-valid-tags-error))))

(define-condition-type &git-no-tags-error &error
  git-no-tags-error?)

(define (git-no-tags-error)
  (raise (condition (&message (message "no tags were found"))
                    (&git-no-tags-error))))


;;; Updater

(define %pre-release-words
  '("alpha" "beta" "rc" "dev" "test" "pre"))

(define %pre-release-rx
  (map (lambda (word)
         (make-regexp (string-append ".+" word) regexp/icase))
       %pre-release-words))

(define* (version-mapping tags #:key prefix suffix delim pre-releases?)
  "Given a list of Git TAGS, return an association list where the car is the
version corresponding to the tag, and the cdr is the name of the tag."
  (define (guess-delimiter)
    (let ((total (length tags))
          (dots (reduce + 0 (map (cut string-count <> #\.) tags)))
          (dashes (reduce + 0 (map (cut string-count <> #\-) tags)))
          (underscores (reduce + 0 (map (cut string-count <> #\_) tags))))
      (cond
       ((>= dots (* total 0.35)) ".")
       ((>= dashes (* total 0.8)) "-")
       ((>= underscores (* total 0.8)) "_")
       (else ""))))

  (define delim-rx (regexp-quote (or delim (guess-delimiter))))
  (define suffix-rx  (string-append (or suffix "") "$"))
  (define prefix-rx (string-append "^" (or prefix "[^[:digit:]]*")))
  (define pre-release-rx
    (if pre-releases?
        (string-append "(.*(" (string-join %pre-release-words "|") ").*)")
        ""))

  (define tag-rx
    (string-append prefix-rx "([[:digit:]][^" delim-rx "[:punct:]]*"
                   "(" delim-rx "[^[:punct:]" delim-rx "]+)"
                   ;; If there are no delimiters, it could mean that the
                   ;; version just contains one number (e.g., "2"), thus, use
                   ;; "*" instead of "+" to match zero or more numbers.
                   (if (string=? delim-rx "") "*" "+") ")"
                   ;; We don't want the pre-release stuff (e.g., "-alpha") be
                   ;; part of the first group; otherwise, the "-" in "-alpha"
                   ;; might be interpreted as a delimiter, and thus replaced
                   ;; with "."
                   pre-release-rx suffix-rx))



  (define (get-version tag)
    (let ((tag-match (regexp-exec (make-regexp tag-rx) tag)))
      (and=> (and tag-match
                  (regexp-substitute/global
                   #f delim-rx (match:substring tag-match 1)
                   ;; If there were no delimiters, don't insert ".".
                   'pre (if (string=? delim-rx "") "" ".") 'post))
             (lambda (version)
               (if pre-releases?
                   (string-append version (match:substring tag-match 3))
                   version)))))

  (define (entry<? a b)
    (eq? (version-compare (car a) (car b)) '<))

  (stable-sort (filter-map (lambda (tag)
                             (let ((version (get-version tag)))
                               (and version (cons version tag))))
                           tags)
               entry<?))

(define* (latest-tag url #:key prefix suffix delim pre-releases?)
  "Return the latest version and corresponding tag available from the Git
repository at URL."
  (define (pre-release? tag)
    (any (cut regexp-exec <> tag)
         %pre-release-rx))

  (let* ((tags (map (cut string-drop <> (string-length "refs/tags/"))
                    (remote-refs url #:tags? #t)))
         (versions->tags
          (version-mapping (if pre-releases?
                               tags
                               (filter (negate pre-release?) tags))
                           #:prefix prefix
                           #:suffix suffix
                           #:delim delim
                           #:pre-releases? pre-releases?)))
    (cond
     ((null? tags)
      (git-no-tags-error))
     ((null? versions->tags)
      (git-no-valid-tags-error))
     (else
      (match (last versions->tags)
        ((version . tag)
         (values version tag)))))))

(define (latest-git-tag-version package)
  "Given a PACKAGE, return the latest version of it and the corresponding git
tag, or #false and #false if the latest version could not be determined."
  (guard (c ((or (git-no-tags-error? c) (git-no-valid-tags-error? c))
             (warning (or (package-field-location package 'source)
                          (package-location package))
                      (G_ "~a for ~a~%")
                      (condition-message c)
                      (package-name package))
             (values #f #f))
            ((eq? (exception-kind c) 'git-error)
             (warning (or (package-field-location package 'source)
                          (package-location package))
                      (G_ "failed to fetch Git repository for ~a~%")
                      (package-name package))
             (values #f #f)))
    (let* ((source (package-source package))
           (url (git-reference-url (origin-uri source)))
           (property (cute assq-ref (package-properties package) <>)))
      (latest-tag url
                  #:prefix (property 'release-tag-prefix)
                  #:suffix (property 'release-tag-suffix)
                  #:delim (property 'release-tag-version-delimiter)
                  #:pre-releases? (property 'accept-pre-releases?)))))

(define (git-package? package)
  "Return true if PACKAGE is hosted on a Git repository."
  (match (package-source package)
    ((? origin? origin)
     (and (eq? (origin-method origin) git-fetch)
          (git-reference? (origin-uri origin))))
    (_ #f)))

(define (latest-git-release package)
  "Return an <upstream-source> for the latest release of PACKAGE."
  (let* ((name (package-name package))
         (old-version (package-version package))
         (old-reference (origin-uri (package-source package)))
         (new-version new-version-tag (latest-git-tag-version package)))
    (and new-version new-version-tag
         (upstream-source
          (package name)
          (version new-version)
          (urls (git-reference
                 (url (git-reference-url old-reference))
                 (commit new-version-tag)
                 (recursive? (git-reference-recursive? old-reference))))))))

(define %generic-git-updater
  (upstream-updater
   (name 'generic-git)
   (description "Updater for packages hosted on Git repositories")
   (pred git-package?)
   (latest latest-git-release)))
