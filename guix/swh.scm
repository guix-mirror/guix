;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
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

(define-module (guix swh)
  #:use-module (guix base16)
  #:use-module (guix build utils)
  #:use-module ((guix build syscalls) #:select (mkdtemp!))
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module ((ice-9 ftw) #:select (scandir))
  #:export (%swh-base-url
            %verify-swh-certificate?
            %allow-request?

            request-rate-limit-reached?

            origin?
            origin-type
            origin-url
            origin-visits
            lookup-origin

            visit?
            visit-date
            visit-origin
            visit-url
            visit-snapshot-url
            visit-status
            visit-number
            visit-snapshot

            snapshot?
            snapshot-id
            snapshot-branches
            lookup-snapshot-branch

            branch?
            branch-name
            branch-target

            release?
            release-id
            release-name
            release-message
            release-target

            revision?
            revision-id
            revision-date
            revision-directory
            lookup-revision
            lookup-origin-revision

            content?
            content-checksums
            content-data-url
            content-length
            lookup-content

            directory-entry?
            directory-entry-name
            directory-entry-type
            directory-entry-checksums
            directory-entry-length
            directory-entry-permissions
            lookup-directory
            directory-entry-target

            save-reply?
            save-reply-origin-url
            save-reply-origin-type
            save-reply-request-date
            save-reply-request-status
            save-reply-task-status
            save-origin
            save-origin-status

            vault-reply?
            vault-reply-id
            vault-reply-fetch-url
            vault-reply-progress-message
            vault-reply-status
            vault-reply-swhid
            query-vault
            request-cooking
            vault-fetch

            commit-id?

            swh-download-directory
            swh-download))

;;; Commentary:
;;;
;;; This module provides bindings to the HTTP interface of Software Heritage.
;;; It allows you to browse the archive, look up revisions (such as SHA1
;;; commit IDs), "origins" (code hosting URLs), content (files), etc.  See
;;; <https://archive.softwareheritage.org/api/> for more information.
;;;
;;; The high-level 'swh-download' procedure allows you to download a Git
;;; revision from Software Heritage, provided it is available.
;;;
;;; Code:

(define %swh-base-url
  ;; Presumably we won't need to change it.
  (make-parameter "https://archive.softwareheritage.org"))

(define %verify-swh-certificate?
  ;; Whether to verify the X.509 HTTPS certificate for %SWH-BASE-URL.
  (make-parameter #t))

;; Token from an account to the Software Heritage Authentication service
;; <https://archive.softwareheritage.org/api/>
(define %swh-token
  (make-parameter (and=> (getenv "GUIX_SWH_TOKEN")
                         string->symbol)))

(define (swh-url path . rest)
  ;; URLs returned by the API may be relative or absolute. This has changed
  ;; without notice before. Handle both cases by detecting whether the path
  ;; starts with a domain.
  (define root
    (if (string-prefix? "/" path)
      (string-append (%swh-base-url) path)
      path))

  (define url
    (string-append root (string-join rest "/" 'prefix)))

  ;; Ensure there's a trailing slash or we get a redirect.
  (if (string-suffix? "/" url)
      url
      (string-append url "/")))

;; XXX: Work around a bug in Guile 3.0.2 where #:verify-certificate? would
;; be ignored (<https://bugs.gnu.org/40486>).
(define* (http-get* uri #:rest rest)
  (apply http-request uri #:method 'GET rest))
(define* (http-post* uri #:rest rest)
  (apply http-request uri #:method 'POST rest))

(define %date-regexp
  ;; Match strings like "2014-11-17T22:09:38+01:00" or
  ;; "2018-09-30T23:20:07.815449+00:00"".
  (make-regexp "^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})((\\.[0-9]+)?)([+-][0-9]{2}):([0-9]{2})$"))

(define (string->date* str)
  "Return a SRFI-19 date parsed from STR, a date string as returned by
Software Heritage."
  ;; We can't use 'string->date' because of the timezone format: SWH returns
  ;; "+01:00" when the '~z' template expects "+0100".  So we roll our own!
  (or (and=> (regexp-exec %date-regexp str)
             (lambda (match)
               (define (ref n)
                 (string->number (match:substring match n)))

               (make-date (let ((ns (match:substring match 8)))
                            (if ns
                                (string->number (string-drop ns 1))
                                0))
                          (ref 6) (ref 5) (ref 4)
                          (ref 3) (ref 2) (ref 1)
                          (+ (* 3600 (ref 9))     ;time zone
                             (if (< (ref 9) 0)
                                 (- (ref 10))
                                 (ref 10))))))
      str))                                       ;oops!

(define (maybe-null proc)
  (match-lambda
    ((? null?) #f)
    ('null #f)
    (obj (proc obj))))

(define string*
  ;; Converts "string or #nil" coming from JSON to "string or #f".
  (match-lambda
    ((? string? str) str)
    ((? null?) #f)                                ;Guile-JSON 3.x
    ('null #f)))                                  ;Guile-JSON 4.x

(define %allow-request?
  ;; Takes a URL and method (e.g., the 'http-get' procedure) and returns true
  ;; to keep going.  This can be used to disallow requests when
  ;; 'request-rate-limit-reached?' returns true, for instance.
  (make-parameter (const #t)))

;; The time when the rate limit for "/origin/save" POST requests and that of
;; other requests will be reset.
;; See <https://archive.softwareheritage.org/api/#rate-limiting>.
(define %save-rate-limit-reset-time 0)
(define %general-rate-limit-reset-time 0)

(define (request-rate-limit-reached? url method)
  "Return true if the rate limit has been reached for URI."
  (define uri
    (string->uri url))

  (define reset-time
    (if (and (eq? method http-post*)
             (string-prefix? "/api/1/origin/save/" (uri-path uri)))
        %save-rate-limit-reset-time
        %general-rate-limit-reset-time))

  (< (car (gettimeofday)) reset-time))

(define (update-rate-limit-reset-time! url method response)
  "Update the rate limit reset time for URL and METHOD based on the headers in
RESPONSE."
  (let ((uri (string->uri url)))
    (match (assq-ref (response-headers response) 'x-ratelimit-reset)
      ((= string->number (? number? reset))
       (if (and (eq? method http-post*)
                (string-prefix? "/api/1/origin/save/" (uri-path uri)))
           (set! %save-rate-limit-reset-time reset)
           (set! %general-rate-limit-reset-time reset)))
      (_
       #f))))

(define* (call url decode #:optional (method http-get*)
               #:key (false-if-404? #t))
  "Invoke the endpoint at URL using METHOD.  Decode the resulting JSON body
using DECODE, a one-argument procedure that takes an input port.  When
FALSE-IF-404? is true, return #f upon 404 responses."
  (and ((%allow-request?) url method)
       (let*-values (((response port)
                      (method url #:streaming? #t
                              #:headers
                              (if (%swh-token)
                                  `((authorization . (Bearer ,(%swh-token))))
                                  '())
                              #:verify-certificate?
                              (%verify-swh-certificate?))))
         ;; See <https://archive.softwareheritage.org/api/#rate-limiting>.
         (match (assq-ref (response-headers response) 'x-ratelimit-remaining)
           (#f #t)
           ((? (compose zero? string->number))
            (update-rate-limit-reset-time! url method response)
            (throw 'swh-error url method response))
           (_ #t))

         (cond ((= 200 (response-code response))
                (let ((result (decode port)))
                  (close-port port)
                  result))
               ((and false-if-404?
                     (= 404 (response-code response)))
                (close-port port)
                #f)
               (else
                (close-port port)
                (throw 'swh-error url method response))))))

(define-syntax define-query
  (syntax-rules (path)
    "Define a procedure that performs a Software Heritage query."
    ((_ (name args ...) docstring (path components ...)
        json->value)
     (define (name args ...)
       docstring
       (call (swh-url components ...) json->value)))))

;; <https://archive.softwareheritage.org/api/1/origin/https://github.com/guix-mirror/guix/get>
(define-json-mapping <origin> make-origin origin?
  json->origin
  (visits-url origin-visits-url "origin_visits_url")
  (type origin-type)
  (url origin-url))

;; <https://archive.softwareheritage.org/api/1/origin/52181937/visits/>
(define-json-mapping <visit> make-visit visit?
  json->visit
  (date visit-date "date" string->date*)
  (origin visit-origin)
  (url visit-url "origin_visit_url")
  (snapshot-url visit-snapshot-url "snapshot_url" string*) ;string | #f
  (status visit-status "status" string->symbol)   ;'full | 'partial | 'ongoing
  (number visit-number "visit"))

;; <https://archive.softwareheritage.org/api/1/snapshot/4334c3ed4bb208604ed780d8687fe523837f1bd1/>
(define-json-mapping <snapshot> make-snapshot snapshot?
  json->snapshot
  (id       snapshot-id)
  (branches snapshot-branches "branches" json->branches))

;; This is used for the "branches" field of snapshots.
(define-record-type <branch>
  (make-branch name target-type target-url)
  branch?
  (name         branch-name)
  (target-type  branch-target-type)               ;release | revision
  (target-url   branch-target-url))

(define (json->branches branches)
  (map (match-lambda
         ((key . value)
          (make-branch key
                       (string->symbol
                        (assoc-ref value "target_type"))
                       (assoc-ref value "target_url"))))
       branches))

;; <https://archive.softwareheritage.org/api/1/release/1f44934fb6e2cefccbecd4fa347025349fa9ff76/>
(define-json-mapping <release> make-release release?
  json->release
  (id          release-id)
  (name        release-name)
  (message     release-message)
  (target-type release-target-type "target_type" string->symbol)
  (target-url  release-target-url "target_url"))

;; <https://archive.softwareheritage.org/api/1/revision/359fdda40f754bbf1b5dc261e7427b75463b59be/>
;; Note: Some revisions, such as those for "nixguix" origins (e.g.,
;; <https://archive.softwareheritage.org/api/1/revision/b8dbc65475bbedde8e015d4730ade8864c38fad3/>),
;; have their 'date' field set to null.
(define-json-mapping <revision> make-revision revision?
  json->revision
  (id            revision-id)
  (date          revision-date "date" (maybe-null string->date*))
  (directory     revision-directory)
  (directory-url revision-directory-url "directory_url"))

;; <https://archive.softwareheritage.org/api/1/content/>
(define-json-mapping <content> make-content content?
  json->content
  (checksums     content-checksums "checksums" json->checksums)
  (data-url      content-data-url "data_url")
  (file-type-url content-file-type-url "filetype_url")
  (language-url  content-language-url "language_url")
  (length        content-length)
  (license-url   content-license-url "license_url"))

(define (json->checksums checksums)
  (map (match-lambda
         ((key . value)
          (cons key (base16-string->bytevector value))))
       checksums))

;; <https://archive.softwareheritage.org/api/1/directory/27c69c5d298a43096a53affbf881e7b13f17bdcd/>
(define-json-mapping <directory-entry> make-directory-entry directory-entry?
  json->directory-entry
  (name          directory-entry-name)
  (type          directory-entry-type "type"
                 (match-lambda
                   ("dir" 'directory)
                   (str   (string->symbol str))))
  (checksums     directory-entry-checksums "checksums"
                 (match-lambda
                   (#f  #f)
                   ((? unspecified?) #f)
                   (lst (json->checksums lst))))
  (id            directory-entry-id "dir_id")
  (length        directory-entry-length)
  (permissions   directory-entry-permissions "perms")
  (target-url    directory-entry-target-url "target_url"))

;; <https://archive.softwareheritage.org/api/1/origin/save/>
(define-json-mapping <save-reply> make-save-reply save-reply?
  json->save-reply
  (origin-url     save-reply-origin-url "origin_url")
  (origin-type    save-reply-origin-type "origin_type")
  (request-date   save-reply-request-date "save_request_date"
                  string->date*)
  (request-status save-reply-request-status "save_request_status"
                  string->symbol)
  (task-status    save-reply-task-status "save_task_status"
                  (match-lambda
                    ("not created" 'not-created)
                    ((? string? str) (string->symbol str)))))

;; <https://docs.softwareheritage.org/devel/swh-vault/api.html#vault-api-ref>
(define-json-mapping <vault-reply> make-vault-reply vault-reply?
  json->vault-reply
  (id             vault-reply-id)
  (fetch-url      vault-reply-fetch-url "fetch_url")
  (progress-message vault-reply-progress-message "progress_message")
  (status         vault-reply-status "status" string->symbol)
  (swhid          vault-reply-swhid))


;;;
;;; RPCs.
;;;

(define-query (lookup-origin url)
  "Return an origin for URL."
  (path "/api/1/origin" url "get")
  json->origin)

(define-query (lookup-content hash type)
  "Return a content for HASH, of the given TYPE--e.g., \"sha256\"."
  (path "/api/1/content"
        (string-append type ":"
                       (bytevector->base16-string hash)))
  json->content)

(define-query (lookup-revision id)
  "Return the revision with the given ID, typically a Git commit SHA1."
  (path "/api/1/revision" id)
  json->revision)

(define-query (lookup-directory id)
  "Return the directory with the given ID."
  (path "/api/1/directory" id)
  json->directory-entries)

(define (json->directory-entries port)
  (map json->directory-entry
       (vector->list (json->scm port))))

(define (origin-visits origin)
  "Return the list of visits of ORIGIN, a record as returned by
'lookup-origin'."
  (call (swh-url (origin-visits-url origin))
        (lambda (port)
          (map json->visit (vector->list (json->scm port))))))

(define (visit-snapshot visit)
  "Return the snapshot corresponding to VISIT or #f if no snapshot is
available."
  (and (visit-snapshot-url visit)
       (call (swh-url (visit-snapshot-url visit))
             json->snapshot)))

(define (snapshot-url snapshot branch-count first-branch)
  "Return the URL of SNAPSHOT such that it contains information for
BRANCH-COUNT branches, starting at FIRST-BRANCH."
  (string-append (swh-url "/api/1/snapshot" (snapshot-id snapshot))
                 "?branches_count=" (number->string branch-count)
                 "&branches_from=" (uri-encode first-branch)))

(define (lookup-snapshot-branch snapshot name)
  "Look up branch NAME on SNAPSHOT.  Return the branch, or return #f if it
could not be found."
  (or (find (lambda (branch)
              (string=? (branch-name branch) name))
            (snapshot-branches snapshot))

      ;; There's no API entry point to look up a snapshot branch by name.
      ;; Work around that by using the paginated list of branches provided by
      ;; the /api/1/snapshot API: ask for one branch, and start pagination at
      ;; NAME.
      (let ((snapshot (call (snapshot-url snapshot 1 name)
                            json->snapshot)))
        (match (snapshot-branches snapshot)
          ((branch)
           (and (string=? (branch-name branch) name)
                branch))
          (_ #f)))))

(define (branch-target branch)
  "Return the target of BRANCH, either a <revision> or a <release>."
  (match (branch-target-type branch)
    ('release
     (call (swh-url (branch-target-url branch))
           json->release))
    ('revision
     (call (swh-url (branch-target-url branch))
           json->revision))))

(define (lookup-origin-revision url tag)
  "Return a <revision> corresponding to the given TAG for the repository
coming from URL.  Example:

  (lookup-origin-revision \"https://github.com/guix-mirror/guix/\" \"v0.8\")
  => #<<revision> id: \"44941…\" …>

The information is based on the latest visit of URL available.  Return #f if
URL could not be found."
  (match (lookup-origin url)
    (#f #f)
    (origin
      (match (filter (lambda (visit)
                       ;; Return #f if (visit-snapshot VISIT) would return #f.
                       (and (visit-snapshot-url visit)
                            (eq? 'full (visit-status visit))))
                     (origin-visits origin))
        ((visit . _)
         (let ((snapshot (visit-snapshot visit)))
           (match (and=> (find (lambda (branch)
                                 (or
                                  ;; Git specific.
                                  (string=? (string-append "refs/tags/" tag)
                                            (branch-name branch))
                                  ;; Hg specific.
                                  (string=? tag
                                            (branch-name branch))))
                               (snapshot-branches snapshot))
                         branch-target)
             ((? release? release)
              (release-target release))
             ((? revision? revision)
              revision)
             (#f                                  ;tag not found
              #f))))
        (()
         #f)))))

(define (release-target release)
  "Return the revision that is the target of RELEASE."
  (match (release-target-type release)
    ('revision
     (call (swh-url (release-target-url release))
           json->revision))))

(define (directory-entry-target entry)
  "If ENTRY, a directory entry, has type 'directory, return its list of
directory entries; if it has type 'file, return its <content> object."
  (call (swh-url (directory-entry-target-url entry))
        (match (directory-entry-type entry)
          ('file json->content)
          ('directory json->directory-entries))))

(define* (save-origin url #:optional (type "git"))
  "Request URL to be saved."
  (call (swh-url "/api/1/origin/save" type "url" url) json->save-reply
        http-post*))

(define-query (save-origin-status url type)
  "Return the status of a /save request for URL and TYPE (e.g., \"git\")."
  (path "/api/1/origin/save" type "url" url)
  json->save-reply)

(define* (vault-url id kind #:optional (archive-type 'flat))
  "Return the vault query/cooking URL for ID and KIND.  Normally, ID is an
SWHID and KIND is #f; the deprecated convention is to set ID to a raw
directory or revision ID and KIND to 'revision or 'directory."
  ;; Note: /api/1/vault/directory/ID was deprecated in favor of
  ;; /api/1/vault/flat/SWHID; this procedure "converts" automatically.
  (let ((id (match kind
              ('directory (string-append "swh:1:dir:" id))
              ('revision  (string-append "swh:1:rev:" id))
              (#f         id))))
    (swh-url "/api/1/vault" (symbol->string archive-type) id)))

(define* (query-vault id #:optional kind #:key (archive-type 'flat))
  "Ask the availability of object ID (an SWHID) to the vault.  Return #f if it
could not be found, or a <vault-reply> on success.  ARCHIVE-TYPE can be 'flat
for a tarball containing a directory, or 'git-bare for a tarball containing a
bare Git repository corresponding to a revision.

Passing KIND (one of 'directory or 'revision) together with a raw revision or
directory identifier is deprecated."
  (call (vault-url id kind archive-type)
        json->vault-reply))

(define* (request-cooking id #:optional kind #:key (archive-type 'flat))
  "Request the cooking of object ID, an SWHID.  Return a <vault-reply>.
ARCHIVE-TYPE can be 'flat for a tarball containing a directory, or 'git-bare
for a tarball containing a bare Git repository corresponding to a revision.

Passing KIND (one of 'directory or 'revision) together with a raw revision or
directory identifier is deprecated."
  (call (vault-url id kind archive-type)
        json->vault-reply
        http-post*))

(define* (vault-fetch id
                      #:optional kind
                      #:key
                      (archive-type 'flat)
                      (log-port (current-error-port)))
  "Return an input port from which a bundle of the object with the given ID,
an SWHID, or #f if the object could not be found.

ARCHIVE-TYPE can be 'flat for a tarball containing a directory, or 'git-bare
for a tarball containing a bare Git repository corresponding to a revision."
  (let loop ((reply (query-vault id kind
                                 #:archive-type archive-type)))
    (match reply
      (#f
       (and=> (request-cooking id kind
                               #:archive-type archive-type)
              loop))
      (_
       (match (vault-reply-status reply)
         ('done
          ;; Fetch the bundle.
          (let-values (((response port)
                        (http-get* (swh-url (vault-reply-fetch-url reply))
                                   #:streaming? #t
                                   #:verify-certificate?
                                   (%verify-swh-certificate?))))
            (if (= (response-code response) 200)
                port
                (begin                            ;shouldn't happen
                  (close-port port)
                  #f))))
         ('failed
          ;; Upon failure, we're supposed to try again.
          (format log-port "SWH vault: failure: ~a~%"
                  (vault-reply-progress-message reply))
          (format log-port "SWH vault: retrying...~%")
          (loop (request-cooking id kind
                                 #:archive-type archive-type)))
         ((and (or 'new 'pending) status)
          ;; Wait until the bundle shows up.
          (let ((message (vault-reply-progress-message reply)))
            (when (eq? 'new status)
              (format log-port "SWH vault: \
requested bundle cooking, waiting for completion...~%"))
            (when (string? message)
              (format log-port "SWH vault: ~a~%" message))

            ;; Wait long enough so we don't exhaust our maximum number of
            ;; requests per hour too fast (as of this writing, the limit is 60
            ;; requests per hour per IP address.)
            (sleep (if (eq? status 'new) 60 30))

            (loop (query-vault id kind
                               #:archive-type archive-type)))))))))


;;;
;;; High-level interface.
;;;

(define (call-with-temporary-directory proc)      ;FIXME: factorize
  "Call PROC with a name of a temporary directory; close the directory and
delete it when leaving the dynamic extent of this call."
  (let* ((directory (or (getenv "TMPDIR") "/tmp"))
         (template  (string-append directory "/guix-directory.XXXXXX"))
         (tmp-dir   (mkdtemp! template)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc tmp-dir))
      (lambda ()
        (false-if-exception (delete-file-recursively tmp-dir))))))

(define* (swh-download-archive swhid output
                               #:key
                               (archive-type 'flat)
                               (log-port (current-error-port)))
  "Download from Software Heritage the directory or revision with the given
SWID, in the ARCHIVE-TYPE format (one of 'flat or 'git-bare), and unpack it to
OUTPUT.  Return #t on success and #f on failure."
  (call-with-temporary-directory
   (lambda (directory)
     (match (vault-fetch swhid
                         #:archive-type archive-type
                         #:log-port log-port)
       (#f
        (format log-port
                "SWH: object ~a could not be fetched from the vault~%"
                swhid)
        #f)
       ((? port? input)
        (let ((tar (open-pipe* OPEN_WRITE "tar" "-C" directory
                               (match archive-type
                                 ('flat "-xzvf")     ;gzipped
                                 ('git-bare "-xvf")) ;uncompressed
                               "-")))
          (dump-port input tar)
          (close-port input)
          (let ((status (close-pipe tar)))
            (unless (zero? status)
              (error "tar extraction failure" status)))

          (match (scandir directory)
            (("." ".." sub-directory)
             (copy-recursively (string-append directory "/" sub-directory)
                               output
                               #:log (%make-void-port "w"))
             #t))))))))

(define* (swh-download-directory id output
                                 #:key (log-port (current-error-port)))
  "Download from Software Heritage the directory with the given ID, and
unpack it to OUTPUT.  Return #t on success and #f on failure."
  (swh-download-archive (string-append "swh:1:dir:" id) output
                        #:archive-type 'flat
                        #:log-port log-port))

(define (commit-id? reference)
  "Return true if REFERENCE is likely a commit ID, false otherwise---e.g., if
it is a tag name.  This is based on a simple heuristic so use with care!"
  (and (= (string-length reference) 40)
       (string-every char-set:hex-digit reference)))

(define* (swh-download url reference output
                       #:key
                       (archive-type 'flat)
                       (log-port (current-error-port)))
  "Download from Software Heritage a checkout (if ARCHIVE-TYPE is 'flat) or a
full Git repository (if ARCHIVE-TYPE is 'git-bare) of the Git tag or commit
REFERENCE originating from URL, and unpack it in OUTPUT.  Return #t on success
and #f on failure.

This procedure uses the \"vault\", which contains \"cooked\" directories in
the form of tarballs.  If the requested directory is not cooked yet, it will
wait until it becomes available, which could take several minutes."
  (match (if (commit-id? reference)
             (lookup-revision reference)
             (lookup-origin-revision url reference))
    ((? revision? revision)
     (format log-port "SWH: found revision ~a with directory at '~a'~%"
             (revision-id revision)
             (swh-url (revision-directory-url revision)))
     (swh-download-archive (match archive-type
                             ('flat
                              (string-append
                               "swh:1:dir:" (revision-directory revision)))
                             ('git-bare
                              (string-append
                               "swh:1:rev:" (revision-id revision))))
                           output
                           #:archive-type archive-type
                           #:log-port log-port))
    (#f
     (format log-port
             "SWH: revision ~s originating from ~a could not be found~%"
             reference url)
     #f)))
