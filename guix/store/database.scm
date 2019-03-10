;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Caleb Ristvedt <caleb.ristvedt@cune.org>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix store database)
  #:use-module (sqlite3)
  #:use-module (guix config)
  #:use-module (guix serialization)
  #:use-module (guix store deduplication)
  #:use-module (guix base16)
  #:use-module (guix progress)
  #:use-module (guix build syscalls)
  #:use-module ((guix build utils)
                #:select (mkdir-p executable-file?))
  #:use-module (guix build store-copy)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:export (sql-schema
            %default-database-file
            with-database
            path-id
            sqlite-register
            register-path
            register-items
            %epoch
            reset-timestamps))

;;; Code for working with the store database directly.

(define sql-schema
  ;; Name of the file containing the SQL scheme or #f.
  (make-parameter #f))

(define sqlite-exec
  ;; XXX: This is was missing from guile-sqlite3 until
  ;; <https://notabug.org/guile-sqlite3/guile-sqlite3/commit/b87302f9bcd18a286fed57b2ea521845eb1131d7>.
  (let ((exec (pointer->procedure
               int
               (dynamic-func "sqlite3_exec" (@@ (sqlite3) libsqlite3))
               '(* * * * *))))
    (lambda (db text)
      (let ((ret (exec ((@@ (sqlite3) db-pointer) db)
                       (string->pointer text)
                       %null-pointer %null-pointer %null-pointer)))
        (unless (zero? ret)
          ((@@ (sqlite3) sqlite-error) db "sqlite-exec" ret))))))

(define (initialize-database db)
  "Initializing DB, an empty database, by creating all the tables and indexes
as specified by SQL-SCHEMA."
  (define schema
    (or (sql-schema)
        (search-path %load-path "guix/store/schema.sql")))

  (sqlite-exec db (call-with-input-file schema get-string-all)))

(define (call-with-database file proc)
  "Pass PROC a database record corresponding to FILE.  If FILE doesn't exist,
create it and initialize it as a new database."
  (let ((new? (not (file-exists? file)))
        (db   (sqlite-open file)))
    ;; Turn DB in "write-ahead log" mode, which should avoid SQLITE_LOCKED
    ;; errors when we have several readers: <https://www.sqlite.org/wal.html>.
    (sqlite-exec db "PRAGMA journal_mode=WAL;")

    ;; Install a busy handler such that, when the database is locked, sqlite
    ;; retries until 30 seconds have passed, at which point it gives up and
    ;; throws SQLITE_BUSY.
    (sqlite-exec db "PRAGMA busy_timeout = 30000;")

    (dynamic-wind noop
                  (lambda ()
                    (when new?
                      (initialize-database db))
                    (proc db))
                  (lambda ()
                    (sqlite-close db)))))

;; XXX: missing in guile-sqlite3@0.1.0
(define SQLITE_BUSY 5)

(define (call-with-transaction db proc)
  "Start a transaction with DB (make as many attempts as necessary) and run
PROC.  If PROC exits abnormally, abort the transaction, otherwise commit the
transaction after it finishes."
  (catch 'sqlite-error
    (lambda ()
      ;; We use begin immediate here so that if we need to retry, we
      ;; figure that out immediately rather than because some SQLITE_BUSY
      ;; exception gets thrown partway through PROC - in which case the
      ;; part already executed (which may contain side-effects!) would be
      ;; executed again for every retry.
      (sqlite-exec db "begin immediate;")
      (let ((result (proc)))
        (sqlite-exec db "commit;")
        result))
    (lambda (key who error description)
      (if (= error SQLITE_BUSY)
          (call-with-transaction db proc)
          (begin
            (sqlite-exec db "rollback;")
            (throw 'sqlite-error who error description))))))

(define %default-database-file
  ;; Default location of the store database.
  (string-append %store-database-directory "/db.sqlite"))

(define-syntax-rule (with-database file db exp ...)
  "Open DB from FILE and close it when the dynamic extent of EXP... is left.
If FILE doesn't exist, create it and initialize it as a new database."
  (call-with-database file (lambda (db) exp ...)))

(define (last-insert-row-id db)
  ;; XXX: (sqlite3) currently lacks bindings for 'sqlite3_last_insert_rowid'.
  ;; Work around that.
  (let* ((stmt   (sqlite-prepare db "SELECT last_insert_rowid();"
                                 #:cache? #t))
         (result (sqlite-fold cons '() stmt)))
    (sqlite-finalize stmt)
    (match result
      ((#(id)) id)
      (_ #f))))

(define path-id-sql
  "SELECT id FROM ValidPaths WHERE path = :path")

(define* (path-id db path)
  "If PATH exists in the 'ValidPaths' table, return its numerical
identifier.  Otherwise, return #f."
  (let ((stmt (sqlite-prepare db path-id-sql #:cache? #t)))
    (sqlite-bind-arguments stmt #:path path)
    (let ((result (sqlite-fold cons '() stmt)))
      (sqlite-finalize stmt)
      (match result
        ((#(id) . _) id)
        (_ #f)))))

(define update-sql
  "UPDATE ValidPaths SET hash = :hash, registrationTime = :time, deriver =
:deriver, narSize = :size WHERE id = :id")

(define insert-sql
  "INSERT INTO ValidPaths (path, hash, registrationTime, deriver, narSize)
VALUES (:path, :hash, :time, :deriver, :size)")

(define* (update-or-insert db #:key path deriver hash nar-size time)
  "The classic update-if-exists and insert-if-doesn't feature that sqlite
doesn't exactly have... they've got something close, but it involves deleting
and re-inserting instead of updating, which causes problems with foreign keys,
of course. Returns the row id of the row that was modified or inserted."
  (let ((id (path-id db path)))
    (if id
        (let ((stmt (sqlite-prepare db update-sql #:cache? #t)))
          (sqlite-bind-arguments stmt #:id id
                                 #:deriver deriver
                                 #:hash hash #:size nar-size #:time time)
          (sqlite-fold cons '() stmt)
          (sqlite-finalize stmt)
          (last-insert-row-id db))
        (let ((stmt (sqlite-prepare db insert-sql #:cache? #t)))
          (sqlite-bind-arguments stmt
                                 #:path path #:deriver deriver
                                 #:hash hash #:size nar-size #:time time)
          (sqlite-fold cons '() stmt)             ;execute it
          (sqlite-finalize stmt)
          (last-insert-row-id db)))))

(define add-reference-sql
  "INSERT OR REPLACE INTO Refs (referrer, reference) VALUES (:referrer, :reference);")

(define (add-references db referrer references)
  "REFERRER is the id of the referring store item, REFERENCES is a list
ids of items referred to."
  (let ((stmt (sqlite-prepare db add-reference-sql #:cache? #t)))
    (for-each (lambda (reference)
                (sqlite-reset stmt)
                (sqlite-bind-arguments stmt #:referrer referrer
                                       #:reference reference)
                (sqlite-fold cons '() stmt)       ;execute it
                (last-insert-row-id db))
              references)
    (sqlite-finalize stmt)))

(define* (sqlite-register db #:key path (references '())
                          deriver hash nar-size time)
  "Registers this stuff in DB.  PATH is the store item to register and
REFERENCES is the list of store items PATH refers to; DERIVER is the '.drv'
that produced PATH, HASH is the base16-encoded Nix sha256 hash of
PATH (prefixed with \"sha256:\"), and NAR-SIZE is the size in bytes PATH after
being converted to nar form.  TIME is the registration time to be recorded in
the database or #f, meaning \"right now\".

Every store item in REFERENCES must already be registered."
  (let ((id (update-or-insert db #:path path
                              #:deriver deriver
                              #:hash hash
                              #:nar-size nar-size
                              #:time (time-second
                                      (or time
                                          (current-time time-utc))))))
    ;; Call 'path-id' on each of REFERENCES.  This ensures we get a
    ;; "non-NULL constraint" failure if one of REFERENCES is unregistered.
    (add-references db id
                    (map (cut path-id db <>) references))))


;;;
;;; High-level interface.
;;;

(define (reset-timestamps file)
  "Reset the modification time on FILE and on all the files it contains, if
it's a directory.  While at it, canonicalize file permissions."
  ;; Note: We're resetting to one second after the Epoch like 'guix-daemon'
  ;; has always done.
  (let loop ((file file)
             (type (stat:type (lstat file))))
    (case type
      ((directory)
       (chmod file #o555)
       (utime file 1 1 0 0)
       (let ((parent file))
         (for-each (match-lambda
                     (("." . _) #f)
                     ((".." . _) #f)
                     ((file . properties)
                      (let ((file (string-append parent "/" file)))
                        (loop file
                              (match (assoc-ref properties 'type)
                                ((or 'unknown #f)
                                 (stat:type (lstat file)))
                                (type type))))))
                   (scandir* parent))))
      ((symlink)
       (utime file 1 1 0 0 AT_SYMLINK_NOFOLLOW))
      (else
       (chmod file (if (executable-file? file) #o555 #o444))
       (utime file 1 1 0 0)))))

(define* (register-path path
                        #:key (references '()) deriver prefix
                        state-directory (deduplicate? #t)
                        (reset-timestamps? #t)
                        (schema (sql-schema)))
  "Register PATH as a valid store file, with REFERENCES as its list of
references, and DERIVER as its deriver (.drv that led to it.)  If PREFIX is
given, it must be the name of the directory containing the new store to
initialize; if STATE-DIRECTORY is given, it must be a string containing the
absolute file name to the state directory of the store being initialized.
Return #t on success.

Use with care as it directly modifies the store!  This is primarily meant to
be used internally by the daemon's build hook."
  (register-items (list (store-info path deriver references))
                  #:prefix prefix #:state-directory state-directory
                  #:deduplicate? deduplicate?
                  #:reset-timestamps? reset-timestamps?
                  #:schema schema
                  #:log-port (%make-void-port "w")))

(define %epoch
  ;; When it all began.
  (make-time time-utc 0 1))

(define* (register-items items
                         #:key prefix state-directory
                         (deduplicate? #t)
                         (reset-timestamps? #t)
                         registration-time
                         (schema (sql-schema))
                         (log-port (current-error-port)))
  "Register all of ITEMS, a list of <store-info> records as returned by
'read-reference-graph', in the database under PREFIX/STATE-DIRECTORY.  ITEMS
must be in topological order (with leaves first.)  If the database is
initially empty, apply SCHEMA to initialize it.  REGISTRATION-TIME must be the
registration time to be recorded in the database; #f means \"now\".
Write a progress report to LOG-PORT."

  ;; Priority for options: first what is given, then environment variables,
  ;; then defaults. %state-directory, %store-directory, and
  ;; %store-database-directory already handle the "environment variables /
  ;; defaults" question, so we only need to choose between what is given and
  ;; those.

  (define db-dir
    (cond (state-directory
           (string-append state-directory "/db"))
          (prefix
           (string-append prefix %localstatedir "/guix/db"))
          (else
           %store-database-directory)))

  (define store-dir
    (if prefix
        (string-append prefix %storedir)
        %store-directory))

  (define (register db item)
    (define to-register
      (if prefix
          (string-append %storedir "/" (basename (store-info-item item)))
          ;; note: we assume here that if path is, for example,
          ;; /foo/bar/gnu/store/thing.txt and prefix isn't given, then an
          ;; environment variable has been used to change the store directory
          ;; to /foo/bar/gnu/store, since otherwise real-path would end up
          ;; being /gnu/store/thing.txt, which is probably not the right file
          ;; in this case.
          (store-info-item item)))

    (define real-file-name
      (string-append store-dir "/" (basename (store-info-item item))))


    ;; When TO-REGISTER is already registered, skip it.  This makes a
    ;; significant differences when 'register-closures' is called
    ;; consecutively for overlapping closures such as 'system' and 'bootcfg'.
    (unless (path-id db to-register)
      (when reset-timestamps?
        (reset-timestamps real-file-name))
      (let-values (((hash nar-size) (nar-sha256 real-file-name)))
        (sqlite-register db #:path to-register
                         #:references (store-info-references item)
                         #:deriver (store-info-deriver item)
                         #:hash (string-append "sha256:"
                                               (bytevector->base16-string hash))
                         #:nar-size nar-size
                         #:time registration-time)
        (when deduplicate?
          (deduplicate real-file-name hash #:store store-dir)))))

  (mkdir-p db-dir)
  (parameterize ((sql-schema schema))
    (with-database (string-append db-dir "/db.sqlite") db
      (call-with-transaction db
          (lambda ()
            (let* ((prefix   (format #f "registering ~a items" (length items)))
                   (progress (progress-reporter/bar (length items)
                                                    prefix log-port)))
              (call-with-progress-reporter progress
                (lambda (report)
                  (for-each (lambda (item)
                              (register db item)
                              (report))
                            items)))))))))
