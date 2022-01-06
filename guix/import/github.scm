;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (guix import github)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-71)
  #:use-module (guix utils)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module ((guix download) #:prefix download:)
  #:use-module ((guix git-download) #:prefix download:)
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module (json)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix http-client)
  #:use-module (web uri)
  #:export (%github-api %github-updater))

;; For tests.
(define %github-api (make-parameter "https://api.github.com"))

(define (find-extension url)
  "Return the extension of the archive e.g. '.tar.gz' given a URL, or
false if none is recognized"
  (find (lambda (x) (string-suffix? x url))
        (list ".tar.gz" ".tar.bz2" ".tar.xz" ".zip" ".tar"
              ".tgz" ".tbz" ".love")))

(define (updated-github-url old-package new-version)
  ;; Return a url for the OLD-PACKAGE with NEW-VERSION.  If no source url in
  ;; the OLD-PACKAGE is a GitHub url, then return false.

  (define (updated-url url)
    (if (string-prefix? "https://github.com/" url)
        (let ((ext     (or (find-extension url) ""))
              (name    (package-upstream-name old-package))
              (version (package-version old-package))
              (prefix  (string-append "https://github.com/"
                                      (github-user-slash-repository url)))
              (repo    (github-repository url)))
          (cond
           ((string-suffix? (string-append "/tarball/v" version) url)
            (string-append prefix "/tarball/v" new-version))
           ((string-suffix? (string-append "/tarball/" version) url)
            (string-append prefix "/tarball/" new-version))
           ((string-suffix? (string-append "/archive/v" version ext) url)
            (string-append prefix "/archive/v" new-version ext))
           ((string-suffix? (string-append "/archive/" version ext) url)
            (string-append prefix "/archive/" new-version ext))
           ((string-suffix? (string-append "/archive/" name "-" version ext)
                            url)
            (string-append prefix "/archive/" name "-" new-version ext))
           ((string-suffix? (string-append "/releases/download/v" version "/"
                                           name "-" version ext)
                            url)
            (string-append prefix "/releases/download/v" new-version "/" name
                           "-" new-version ext))
           ((string-suffix? (string-append "/releases/download/" version "/"
                                           name "-" version ext)
                            url)
            (string-append prefix "/releases/download/" new-version "/" name
                           "-" new-version ext))
           ((string-suffix? (string-append "/releases/download/" version "/"
                                           repo "-" version ext)
                            url)
            (string-append prefix "/releases/download/" new-version "/" repo
                           "-" new-version ext))
           ((string-suffix? (string-append "/releases/download/" repo "-"
                                           version "/" repo "-" version ext)
                            url)
            (string-append "/releases/download/" repo "-" version "/" repo "-"
                           version ext))
           (#t #f))) ; Some URLs are not recognised.
        #f))

  (match (package-source old-package)
    ((? origin? origin)
     (let ((source-uri   (origin-uri origin))
           (fetch-method (origin-method origin)))
       (cond
        ((eq? fetch-method download:url-fetch)
         (match source-uri
           ((? string?)
            (updated-url source-uri))
           ((source-uri ...)
            (find updated-url source-uri))))
        ((and (eq? fetch-method download:git-fetch)
              (string-prefix? "https://github.com/"
                              (download:git-reference-url source-uri)))
         (download:git-reference-url source-uri))
        (else #f))))
    (_ #f)))

(define (github-package? package)
  "Return true if PACKAGE is a package from GitHub, else false."
  (->bool (updated-github-url package "dummy")))

(define (github-repository url)
  "Return a string e.g. bedtools2 of the name of the repository, from a string
URL of the form 'https://github.com/arq5x/bedtools2/archive/v2.24.0.tar.gz'"
  (match (string-split (uri-path (string->uri url)) #\/)
    ((_ owner project . rest)
     (string-append (basename project ".git")))))

(define (github-user-slash-repository url)
  "Return a string e.g. arq5x/bedtools2 of the owner and the name of the
repository separated by a forward slash, from a string URL of the form
'https://github.com/arq5x/bedtools2/archive/v2.24.0.tar.gz'"
  (match (string-split (uri-path (string->uri url)) #\/)
    ((_ owner project . rest)
     (string-append owner "/" (basename project ".git")))))

(define %github-token
  ;; Token to be passed to Github.com to avoid the 60-request per hour
  ;; limit, or #f.
  (make-parameter (getenv "GUIX_GITHUB_TOKEN")))

(define (fetch-releases-or-tags url)
  "Fetch the list of \"releases\" or, if it's empty, the list of tags for the
repository at URL.  Return the corresponding JSON dictionaries (alists),
or #f if the information could not be retrieved.

We look at both /releases and /tags because the \"release\" feature of GitHub
is little used; often, people simply provide a tag.  What's confusing is that
tags show up in the \"Releases\" tab of the web UI.  For instance,
'https://github.com/aconchillo/guile-json/releases' shows a number of
\"releases\" (really: tags), whereas
'https://api.github.com/repos/aconchillo/guile-json/releases' returns the
empty list."
  (define release-url
    (string-append (%github-api) "/repos/"
                   (github-user-slash-repository url)
                   "/releases"))
  (define tag-url
    (string-append (%github-api) "/repos/"
                   (github-user-slash-repository url)
                   "/tags"))

  (define headers
    ;; Ask for version 3 of the API as suggested at
    ;; <https://developer.github.com/v3/>.
    `((Accept . "application/vnd.github.v3+json")
      (user-agent . "GNU Guile")
      ,@(if (%github-token)
            `((Authorization . ,(string-append "token " (%github-token))))
            '())))

  (guard (c ((and (http-get-error? c)
                  (= 404 (http-get-error-code c)))
             (warning (G_ "~a is unreachable (~a)~%")
                      release-url (http-get-error-code c))
             '#()))                               ;return an empty release set
    (let* ((port   (http-fetch release-url #:headers headers))
           (result (json->scm port)))
      (close-port port)
      (match result
        (#()
         ;; We got the empty list, presumably because the user didn't use GitHub's
         ;; "release" mechanism, but hopefully they did use Git tags.
         (json-fetch tag-url #:headers headers))
        (x x)))))

(define (latest-released-version url package-name)
  "Return the newest released version and its tag given a string URL like
'https://github.com/arq5x/bedtools2/archive/v2.24.0.tar.gz' and the name of
the package e.g. 'bedtools2'.  Return #f (two values) if there are no
releases."
  (define (pre-release? x)
    (assoc-ref x "prerelease"))

  ;; This procedure returns (version . tag) pair, or #f
  ;; if RELEASE doesn't seyem to correspond to a version.
  (define (release->version release)
    (let ((tag (or (assoc-ref release "tag_name") ;a "release"
                   (assoc-ref release "name")))   ;a tag
          (name-length (string-length package-name)))
      (cond
       ;; some tags include the name of the package e.g. "fdupes-1.51"
       ;; so remove these
       ((and (< name-length (string-length tag))
             (string=? (string-append package-name "-")
                       (substring tag 0 (+ name-length 1))))
        (cons (substring tag (+ name-length 1)) tag))
       ;; some tags start with a "v" e.g. "v0.25.0"
       ;; or with the word "version" e.g. "version.2.1"
       ;; where some are just the version number
       ((string-prefix? "version" tag)
        (cons (if (char-set-contains? char-set:digit (string-ref tag 7))
                  (substring tag 7)
                  (substring tag 8)) tag))
       ((string-prefix? "v" tag)
        (cons (substring tag 1) tag))
       ;; Finally, reject tags that don't start with a digit:
       ;; they may not represent a release.
       ((and (not (string-null? tag))
             (char-set-contains? char-set:digit
                                 (string-ref tag 0)))
        (cons tag tag))
       (else #f))))

  (let* ((json (and=> (fetch-releases-or-tags url)
                      vector->list)))
    (if (eq? json #f)
        (if (%github-token)
            (error "Error downloading release information through the GitHub
API when using a GitHub token")
            (error "Error downloading release information through the GitHub
API. This may be fixed by using an access token and setting the environment
variable GUIX_GITHUB_TOKEN, for instance one procured from
https://github.com/settings/tokens"))
        (match (sort (filter-map release->version
                                 (match (remove pre-release? json)
                                   (() json) ; keep everything
                                   (releases releases)))
                     (lambda (x y) (version>? (car x) (car y))))
          (((latest-version . tag) . _) (values latest-version tag))
          (() (values #f #f))))))

(define (latest-release pkg)
  "Return an <upstream-source> for the latest release of PKG."
  (define (github-uri uri)
    (match uri
      ((? string? url)
       url)                                       ;surely a github.com URL
      ((? download:git-reference? ref)
       (download:git-reference-url ref))
      ((urls ...)
       (find (cut string-contains <> "github.com") urls))))

  (let* ((original-uri (origin-uri (package-source pkg)))
         (source-uri (github-uri original-uri))
         (name (package-name pkg))
         (newest-version version-tag
                         (latest-released-version source-uri name)))
    (if newest-version
        (upstream-source
         (package name)
         (version newest-version)
         (urls (if (download:git-reference? original-uri)
                   (download:git-reference
                    (inherit original-uri)
                    (commit version-tag))
                   (list (updated-github-url pkg newest-version)))))
        #f))) ; On GitHub but no proper releases

(define %github-updater
  (upstream-updater
   (name 'github)
   (description "Updater for GitHub packages")
   (pred github-package?)
   (latest latest-release)))


