;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Maxime Devos <maximedevos@telenet.be>
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

(define-module (guix import minetest)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module ((guix packages) #:prefix package:)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (guix ui)
  #:use-module (guix i18n)
  #:use-module (guix memoization)
  #:use-module (guix serialization)
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module ((gcrypt hash) #:select (open-sha256-port port-sha256))
  #:use-module (json)
  #:use-module (guix base32)
  #:use-module (guix git)
  #:use-module ((guix git-download) #:prefix download:)
  #:use-module (guix hash)
  #:use-module (guix store)
  #:export (%default-sort-key
            %contentdb-api
            json->package
            contentdb-fetch
            elaborate-contentdb-name
            minetest-package?
            latest-minetest-release
            minetest->guix-package
            minetest-recursive-import
            sort-packages
            %minetest-updater))

;; The ContentDB API is documented at
;; <https://content.minetest.net>.

(define %contentdb-api
  (make-parameter "https://content.minetest.net/api/"))

(define (string-or-false x)
  (and (string? x) x))

(define (natural-or-false x)
  (and (exact-integer? x) (>= x 0) x))

;; Descriptions on ContentDB use carriage returns, but Guix doesn't.
(define (delete-cr text)
  (string-delete #\cr text))



;;;
;;; JSON mappings
;;;

;; Minetest package.
;;
;; API endpoint: /packages/AUTHOR/NAME/
(define-json-mapping <package> make-package package?
  json->package
  (author            package-author) ; string
  (creation-date     package-creation-date ; string
                     "created_at")
  (downloads         package-downloads) ; integer
  (forums            package-forums "forums" natural-or-false)
  (issue-tracker     package-issue-tracker "issue_tracker") ; string
  (license           package-license) ; string
  (long-description  package-long-description "long_description") ; string
  (maintainers       package-maintainers ; list of strings
                     "maintainers" vector->list)
  (media-license     package-media-license "media_license") ; string
  (name              package-name) ; string
  (provides          package-provides ; list of strings
                     "provides" vector->list)
  (release           package-release) ; integer
  (repository        package-repository "repo" string-or-false)
  (score             package-score) ; flonum
  (screenshots       package-screenshots "screenshots" vector->list) ; list of strings
  (short-description package-short-description "short_description") ; string
  (state             package-state) ; string
  (tags              package-tags "tags" vector->list) ; list of strings
  (thumbnail         package-thumbnail) ; string
  (title             package-title) ; string
  (type              package-type) ; string
  (url               package-url) ; string
  (website           package-website "website" string-or-false))

(define-json-mapping <release> make-release release?
  json->release
  ;; If present, a git commit identified by its hash
  (commit               release-commit "commit" string-or-false)
  (downloads            release-downloads) ; integer
  (id                   release-id) ; integer
  (max-minetest-version release-max-minetest-version string-or-false)
  (min-minetest-version release-min-minetest-version string-or-false)
  (release-date         release-data) ; string
  (title                release-title) ; string
  (url                  release-url)) ; string

(define-json-mapping <dependency> make-dependency dependency?
  json->dependency
  (optional? dependency-optional? "is_optional") ; bool
  (name dependency-name) ; string
  (packages dependency-packages "packages" vector->list)) ; list of strings

;; A structure returned by the /api/packages/?fmt=keys endpoint
(define-json-mapping <package-keys> make-package-keys package-keys?
  json->package-keys
  (author package-keys-author) ; string
  (name package-keys-name)     ; string
  (type package-keys-type))    ; string

(define (package-mod? package)
  "Is the ContentDB package PACKAGE a mod?"
  ;; ContentDB also has ‘games’ and ‘texture packs’.
  (string=? (package-type package) "mod"))



;;;
;;; Manipulating names of packages
;;;
;;; There are three kind of names:
;;;
;;;   * names of guix packages, e.g. minetest-basic-materials.
;;;   * names of mods on ContentDB, e.g. basic_materials
;;;   * a combination of author and mod name on ContentDB, e.g. VanessaE/basic_materials
;;;

(define (%construct-full-name author name)
  (string-append author "/" name))

(define (package-full-name package)
  "Given a <package> object, return the corresponding AUTHOR/NAME string."
  (%construct-full-name (package-author package) (package-name package)))

(define (package-keys-full-name package)
  "Given a <package-keys> object, return the corresponding AUTHOR/NAME string."
  (%construct-full-name (package-keys-author package)
                        (package-keys-name package)))

(define (contentdb->package-name author/name)
  "Given the AUTHOR/NAME of a package on ContentDB, return a Guix-compliant
name for the package."
  ;; The author is not included, as the names of popular mods
  ;; tend to be unique.
  (string-append "minetest-" (snake-case (author/name->name author/name))))

(define (author/name->name author/name)
  "Extract NAME from the AUTHOR/NAME string, or raise an error if AUTHOR/NAME
is ill-formatted."
  (match (string-split author/name #\/)
    ((author name)
     (when (string-null? author)
       (leave
        (G_ "In ~a: author names must consist of at least a single character.~%")
        author/name))
     (when (string-null? name)
       (leave
        (G_ "In ~a: mod names must consist of at least a single character.~%")
        author/name))
     name)
    ((too many . components)
     (leave
      (G_ "In ~a: author names and mod names may not contain forward slashes.~%")
      author/name))
    ((name)
     (if (string-null? name)
         (leave (G_ "mod names may not be empty.~%"))
         (leave (G_ "The name of the author is missing in ~a.~%")
                author/name)))))

(define* (elaborate-contentdb-name name #:key (sort %default-sort-key))
  "If NAME is an AUTHOR/NAME string, return it.  Otherwise, try to determine
the author and return an appropriate AUTHOR/NAME string.  If that fails,
raise an exception."
  (if (or (string-contains name "/") (string-null? name))
      ;; Call 'author/name->name' to verify that NAME seems reasonable
      ;; and raise an appropriate exception if it isn't.
      (begin
        (author/name->name name)
        name)
      (let* ((package-keys (contentdb-query-packages name #:sort sort))
             (correctly-named
              (filter (lambda (package-key)
                        (string=? name (package-keys-name package-key)))
                      package-keys)))
        (match correctly-named
          ((one) (package-keys-full-name one))
          ((too . many)
           (warning (G_ "~a is ambiguous, presuming ~a (other options include: ~a)~%")
                    name (package-keys-full-name too)
                    (map package-keys-full-name many))
           (package-keys-full-name too))
          (()
           (leave (G_ "No mods with name ~a were found.~%") name))))))



;;;
;;; API endpoints
;;;

(define contentdb-fetch
  (mlambda (author/name)
    "Return a <package> record for package AUTHOR/NAME, or #f on failure."
    (and=> (json-fetch
            (string-append (%contentdb-api) "packages/" author/name "/"))
           json->package)))

(define (contentdb-fetch-releases author/name)
  "Return a list of <release> records for package NAME by AUTHOR, or #f
on failure."
  (and=> (json-fetch (string-append (%contentdb-api) "packages/" author/name
                                    "/releases/"))
         (lambda (json)
           (map json->release (vector->list json)))))

(define (latest-release author/name)
  "Return the latest source release for package NAME by AUTHOR,
or #f if this package does not exist."
  (and=> (contentdb-fetch-releases author/name)
         car))

(define (contentdb-fetch-dependencies author/name)
  "Return an alist of lists of <dependency> records for package NAME by AUTHOR
and possibly some other packages as well, or #f on failure."
  (define url (string-append (%contentdb-api) "packages/" author/name
                             "/dependencies/"))
  (and=> (json-fetch url)
         (lambda (json)
           (map (match-lambda
                  ((key . value)
                   (cons key (map json->dependency (vector->list value)))))
                json))))

(define* (contentdb-query-packages q #:key
                                   (type "mod")
                                   (limit 50)
                                   (sort %default-sort-key)
                                   (order "desc"))
  "Search ContentDB for Q (a string).  Sort by SORT, in ascending order
if ORDER is \"asc\" or descending order if ORDER is \"desc\".  TYPE must
be \"mod\", \"game\" or \"txp\", restricting the search results to
respectively mods, games and texture packs.  Limit to at most LIMIT
results.  The return value is a list of <package-keys> records."
  ;; XXX does Guile have something for constructing (and, when necessary,
  ;; escaping) query strings?
  (define url (string-append (%contentdb-api) "packages/?type=" type
                             "&q=" q "&fmt=keys"
                             "&limit=" (number->string limit)
                             "&order=" order
                             "&sort=" sort))
  (let ((json (json-fetch url)))
    (if json
        (map json->package-keys (vector->list json))
        (leave
         (G_ "The package search API doesn't exist anymore.~%")))))



;; XXX copied from (guix import elpa)
(define* (download-git-repository url ref)
  "Fetch the given REF from the Git repository at URL."
  (with-store store
    (latest-repository-commit store url #:ref ref)))

(define (make-minetest-sexp author/name version repository commit
                            inputs home-page synopsis
                            description media-license license)
  "Return a S-expression for the minetest package with the given author/NAME,
VERSION, REPOSITORY, COMMIT, INPUTS, HOME-PAGE, SYNOPSIS, DESCRIPTION,
MEDIA-LICENSE and LICENSE."
  `(package
     (name ,(contentdb->package-name author/name))
     (version ,version)
     (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url ,repository)
                (commit ,commit)))
         (sha256
          (base32
           ;; The git commit is not always available.
           ,(and commit
                 (bytevector->nix-base32-string
                  (file-hash*
                   (download-git-repository repository
                                            `(commit . ,commit))
                   ;; 'download-git-repository' already filtered out the '.git'
                   ;; directory.
                   #:select? (const #true)
                   #:recursive? #true)))))
         (file-name (git-file-name name version))))
     (build-system minetest-mod-build-system)
     ,@(maybe-propagated-inputs (map contentdb->package-name inputs))
     (home-page ,home-page)
     (synopsis ,(delete-cr synopsis))
     (description ,(beautify-description (delete-cr description)))
     (license ,(if (eq? media-license license)
                   license
                   `(list ,media-license ,license)))
     ;; The Minetest updater (not yet in Guix; it requires not-yet-submitted
     ;; patches to (guix upstream) that require some work) needs to know both
     ;; the author name and mod name for efficiency.
     (properties ,(list 'quasiquote `((upstream-name . ,author/name))))))

(define (package-home-page package)
  "Guess the home page of the ContentDB package PACKAGE.

In order of preference, try the 'website', the forum topic on the
official Minetest forum and the Git repository (if any)."
  (define (topic->url-sexp topic)
    ;; 'minetest-topic' is a procedure defined in (gnu packages minetest)
    `(minetest-topic ,topic))
  (or (package-website package)
      (and=> (package-forums package) topic->url-sexp)
      (package-repository package)))

(define (release-version release)
  "Guess the version of RELEASE from the release title."
  (define title (release-title release))
  (if (string-prefix? "v" title)
      ;; Remove "v" prefix from release titles like ‘v1.0.1’.
      (substring title 1)
      title))

(define (version-style version)
  "Determine the kind of version number VERSION is -- a date, or a conventional
conventional version number."
  (define dots? (->bool (string-index version #\.)))
  (define hyphens? (->bool (string-index version #\-)))
  (match (cons dots? hyphens?)
    ((#true . #false) 'regular) ; something like "0.1"
    ((#false . #false) 'regular) ; single component version number
    ((#true . #true) 'regular) ; result of 'git-version'
    ((#false . #true) 'date))) ; something like "2021-01-25"

;; If the default sort key is changed, make sure to modify 'show-help'
;; in (guix scripts import minetest) appropriately as well.
(define %default-sort-key "score")

(define* (sort-packages packages #:key (sort %default-sort-key))
  "Sort PACKAGES by SORT, in descending order."
  (define package->key
    (match sort
      ("score" package-score)
      ("downloads" package-downloads)))
  (define (greater x y)
    (> (package->key x) (package->key y)))
  (sort-list packages greater))

(define builtin-mod?
  (let ((%builtin-mods
         (alist->hash-table
          (map (lambda (x) (cons x #t))
               '("beds" "binoculars" "boats" "bones" "bucket" "butterflies"
                 "carts" "creative" "default" "doors" "dungeon_loot" "dye"
                 "env_sounds" "farming" "fire" "fireflies" "flowers"
                 "game_commands" "give_initial_stuff" "map" "mtg_craftguide"
                 "player_api" "screwdriver" "sethome" "sfinv" "spawn" "stairs"
                 "tnt" "vessels" "walls" "weather" "wool" "xpanes")))))
    (lambda (mod)
      "Is MOD provided by the default minetest subgame?"
      (hash-ref %builtin-mods mod))))

(define* (important-dependencies dependencies author/name
                                 #:key (sort %default-sort-key))
  "Return the hard dependencies of AUTHOR/NAME in the association list
DEPENDENCIES as a list of AUTHOR/NAME strings."
  (define dependency-list
    (assoc-ref dependencies author/name))
  ;; A mod can have multiple dependencies implemented by the same mod,
  ;; so remove duplicate mod names.
  (define (filter-deduplicate-map f list)
    (delete-duplicates (filter-map f list)))
  (filter-deduplicate-map
   (lambda (dependency)
     (and (not (dependency-optional? dependency))
          (not (builtin-mod? (dependency-name dependency)))
          ;; The dependency information contains symbolic names
          ;; that can be ‘provided’ by multiple mods, so we need to choose one
          ;; of the implementations.
          (let* ((implementations
                  (par-map contentdb-fetch (dependency-packages dependency)))
                 ;; Fetching package information about the packages is racy:
                 ;; some packages might be removed from ContentDB between the
                 ;; construction of DEPENDENCIES and the call to
                 ;; 'contentdb-fetch'.  So filter out #f.
                 ;;
                 ;; Filter out ‘games’ that include the requested mod -- it's
                 ;; the mod itself we want.
                 (mods (filter (lambda (p) (and=> p package-mod?))
                               implementations))
                 (sorted-mods (sort-packages mods #:sort sort)))
            (match sorted-mods
              ((package) (package-full-name package))
              ((too . many)
               (warning
                (G_ "The dependency ~a of ~a has multiple different implementations ~a.~%")
                (dependency-name dependency)
                author/name
                (map package-full-name sorted-mods))
               (match sort
                 ("score"
                  (warning
                   (G_ "The implementation with the highest score will be choosen!~%")))
                 ("downloads"
                  (warning
                   (G_ "The implementation that has been downloaded the most will be choosen!~%"))))
               (package-full-name too))
              (()
               (warning
                (G_ "The dependency ~a of ~a does not have any implementation.  It will be ignored!~%")
                (dependency-name dependency) author/name)
               #f)))))
   dependency-list))

(define* (%minetest->guix-package author/name #:key (sort %default-sort-key))
  "Fetch the metadata for AUTHOR/NAME from https://content.minetest.net, and
return the 'package' S-expression corresponding to that package, or raise an
exception on failure.  On success, also return the upstream dependencies as a
list of AUTHOR/NAME strings."
  ;; Call 'author/name->name' to verify that AUTHOR/NAME seems reasonable.
  (author/name->name author/name)
  (define package (contentdb-fetch author/name))
  (unless package
    (leave (G_ "no package metadata for ~a on ContentDB~%") author/name))
  (define dependencies (contentdb-fetch-dependencies author/name))
  (unless dependencies
    (leave (G_ "no dependency information for ~a on ContentDB~%") author/name))
  (define release (latest-release author/name))
  (unless release
    (leave (G_ "no release of ~a on ContentDB~%") author/name))
  (define important-upstream-dependencies
    (important-dependencies dependencies author/name #:sort sort))
  (values (make-minetest-sexp author/name
                              (release-version release)
                              (package-repository package)
                              (release-commit release)
                              important-upstream-dependencies
                              (package-home-page package)
                              (package-short-description package)
                              (package-long-description package)
                              (spdx-string->license
                               (package-media-license package))
                              (spdx-string->license
                               (package-license package)))
          important-upstream-dependencies))

(define minetest->guix-package
  (memoize %minetest->guix-package))

(define* (minetest-recursive-import author/name #:key (sort %default-sort-key))
  (define* (minetest->guix-package* author/name #:key repo version)
    (minetest->guix-package author/name #:sort sort))
  (recursive-import author/name
                    #:repo->guix-package minetest->guix-package*
                    #:guix-name contentdb->package-name))

(define (minetest-package? pkg)
  "Is PKG a Minetest mod on ContentDB?"
  (and (string-prefix? "minetest-" (package:package-name pkg))
       (assq-ref (package:package-properties pkg) 'upstream-name)))

(define (latest-minetest-release pkg)
  "Return an <upstream-source> for the latest release of the package PKG,
or #false if the latest release couldn't be determined."
  (define author/name
    (assq-ref (package:package-properties pkg) 'upstream-name))
  (define contentdb-package (contentdb-fetch author/name)) ; TODO warn if #f?
  (define release (latest-release author/name))
  (define source (package:package-source pkg))
  (and contentdb-package release
       (release-commit release) ; not always set
       ;; Only continue if both the old and new version number are both
       ;; dates or regular version numbers, as two different styles confuses
       ;; the logic for determining which version is newer.
       (eq? (version-style (release-version release))
            (version-style (package:package-version pkg)))
       (upstream-source
        (package (package:package-name pkg))
        (version (release-version release))
        (urls (download:git-reference
               (url (package-repository contentdb-package))
               (commit (release-commit release)))))))

(define %minetest-updater
  (upstream-updater
    (name 'minetest)
    (description "Updater for Minetest packages on ContentDB")
    (pred minetest-package?)
    (latest latest-minetest-release)))
