;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
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
  #:use-module (guix json)
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
            vault-reply-object-id
            vault-reply-object-type
            vault-reply-progress-message
            vault-reply-status
            query-vault
            request-cooking
            vault-fetch

            commit-id?

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

(define string*
  ;; Converts "string or #nil" coming from JSON to "string or #f".
  (match-lambda
    ((? string? str) str)
    ((? null?) #f)))

(define %allow-request?
  ;; Takes a URL and method (e.g., the 'http-get' procedure) and returns true
  ;; to keep going.  This can be used to disallow a requests when
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
    (if (and (eq? method http-post)
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
       (if (and (eq? method http-post)
                (string-prefix? "/api/1/origin/save/" (uri-path uri)))
           (set! %save-rate-limit-reset-time reset)
           (set! %general-rate-limit-reset-time reset)))
      (_
       #f))))

(define* (call url decode #:optional (method http-get)
               #:key (false-if-404? #t))
  "Invoke the endpoint at URL using METHOD.  Decode the resulting JSON body
using DECODE, a one-argument procedure that takes an input port.  When
FALSE-IF-404? is true, return #f upon 404 responses."
  (and ((%allow-request?) url method)
       (let*-values (((response port)
                      (method url #:streaming? #t)))
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
(define-json-mapping <revision> make-revision revision?
  json->revision
  (id            revision-id)
  (date          revision-date "date" string->date*)
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
  (object-id      vault-reply-object-id "obj_id")
  (object-type    vault-reply-object-type "obj_type" string->symbol)
  (progress-message vault-reply-progress-message "progress_message")
  (status         vault-reply-status "status" string->symbol))


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
      (match (filter visit-snapshot-url (origin-visits origin))
        ((visit . _)
         (let ((snapshot (visit-snapshot visit)))
           (match (and=> (find (lambda (branch)
                                 (string=? (string-append "refs/tags/" tag)
                                           (branch-name branch)))
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
        http-post))

(define-query (save-origin-status url type)
  "Return the status of a /save request for URL and TYPE (e.g., \"git\")."
  (path "/api/1/origin/save" type "url" url)
  json->save-reply)

(define-query (query-vault id kind)
  "Ask the availability of object ID and KIND to the vault, where KIND is
'directory or 'revision.  Return #f if it could not be found, or a
<vault-reply> on success."
  ;; <https://docs.softwareheritage.org/devel/swh-vault/api.html#vault-api-ref>
  ;; There's a single format supported for directories and revisions and for
  ;; now, the "/format" bit of the URL *must* be omitted.
  (path "/api/1/vault" (symbol->string kind) id)
  json->vault-reply)

(define (request-cooking id kind)
  "Request the cooking of object ID and KIND (one of 'directory or 'revision)
to the vault.  Return a <vault-reply>."
  (call (swh-url "/api/1/vault" (symbol->string kind) id)
        json->vault-reply
        http-post))

(define* (vault-fetch id kind
                      #:key (log-port (current-error-port)))
  "Return an input port from which a bundle of the object with the given ID
and KIND (one of 'directory or 'revision) can be retrieved, or #f if the
object could not be found.

For a directory, the returned stream is a gzip-compressed tarball.  For a
revision, it is a gzip-compressed stream for 'git fast-import'."
  (let loop ((reply (query-vault id kind)))
    (match reply
      (#f
       (and=> (request-cooking id kind) loop))
      (_
       (match (vault-reply-status reply)
         ('done
          ;; Fetch the bundle.
          (let-values (((response port)
                        (http-get (swh-url (vault-reply-fetch-url reply))
                                  #:streaming? #t)))
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
          (loop (request-cooking id kind)))
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

            (loop (query-vault id kind)))))))))


;;;
;;; High-level interface.
;;;

(define (commit-id? reference)
  "Return true if REFERENCE is likely a commit ID, false otherwise---e.g., if
it is a tag name.  This is based on a simple heuristic so use with care!"
  (and (= (string-length reference) 40)
       (string-every char-set:hex-digit reference)))

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

(define* (swh-download url reference output
                       #:key (log-port (current-error-port)))
  "Download from Software Heritage a checkout of the Git tag or commit
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
     (call-with-temporary-directory
      (lambda (directory)
        (match (vault-fetch (revision-directory revision) 'directory
                            #:log-port log-port)
          (#f
           (format log-port
                   "SWH: directory ~a could not be fetched from the vault~%"
                   (revision-directory revision))
           #f)
          ((? port? input)
           (let ((tar (open-pipe* OPEN_WRITE "tar" "-C" directory "-xzvf" "-")))
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
    (#f
     #f)))
