;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Caleb Ristvedt <caleb.ristvedt@cune.org>
;;; Copyright © 2018, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (guix gexp)
  #:use-module (guix serialization)
  #:use-module (guix store deduplication)
  #:use-module (guix base16)
  #:use-module (guix progress)
  #:use-module (guix build syscalls)
  #:use-module ((guix build utils)
                #:select (mkdir-p executable-file?))
  #:use-module (guix utils)
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
            store-database-file
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

(define* (store-database-directory #:key prefix state-directory)
  "Return the store database directory, taking PREFIX and STATE-DIRECTORY into
account when provided."
  ;; Priority for options: first what is given, then environment variables,
  ;; then defaults. %state-directory, %store-directory, and
  ;; %store-database-directory already handle the "environment variables /
  ;; defaults" question, so we only need to choose between what is given and
  ;; those.
  (cond (state-directory
         (string-append state-directory "/db"))
        (prefix
         (string-append prefix %localstatedir "/guix/db"))
        (else
         %store-database-directory)))

(define* (store-database-file #:key prefix state-directory)
  "Return the store database file name, taking PREFIX and STATE-DIRECTORY into
account when provided."
  (string-append (store-database-directory #:prefix prefix
                                           #:state-directory state-directory)
                 "/db.sqlite"))

(define (initialize-database db)
  "Initializing DB, an empty database, by creating all the tables and indexes
as specified by SQL-SCHEMA."
  (define schema
    (or (sql-schema)
        (search-path %load-path "guix/store/schema.sql")))

  (sqlite-exec db (call-with-input-file schema get-string-all)))

(define* (call-with-database file proc #:key (wal-mode? #t))
  "Pass PROC a database record corresponding to FILE.  If FILE doesn't exist,
create it and initialize it as a new database.  Unless WAL-MODE? is set to #f,
set journal_mode=WAL."
  (let ((new? (and (not (file-exists? file))
                   (begin
                     (mkdir-p (dirname file))
                     #t)))
        (db   (sqlite-open file)))
    ;; Using WAL breaks for the Hurd <https://bugs.gnu.org/42151>.
    (when wal-mode?
      ;; Turn DB in "write-ahead log" mode, which should avoid SQLITE_LOCKED
      ;; errors when we have several readers: <https://www.sqlite.org/wal.html>.
      (sqlite-exec db "PRAGMA journal_mode=WAL;"))

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

(define (call-with-SQLITE_BUSY-retrying thunk)
  "Call THUNK, retrying as long as it exits abnormally due to SQLITE_BUSY
errors."
  (catch 'sqlite-error
    thunk
    (lambda (key who code errmsg)
      (if (= code SQLITE_BUSY)
          (call-with-SQLITE_BUSY-retrying thunk)
          (throw key who code errmsg)))))



(define* (call-with-transaction db proc #:key restartable?)
  "Start a transaction with DB and run PROC.  If PROC exits abnormally, abort
the transaction, otherwise commit the transaction after it finishes.
RESTARTABLE? may be set to a non-#f value when it is safe to run PROC multiple
times.  This may reduce contention for the database somewhat."
  (define (exec sql)
    (with-statement db sql stmt
      (sqlite-fold cons '() stmt)))
  ;; We might use begin immediate here so that if we need to retry, we figure
  ;; that out immediately rather than because some SQLITE_BUSY exception gets
  ;; thrown partway through PROC - in which case the part already executed
  ;; (which may contain side-effects!) might have to be executed again for
  ;; every retry.
  (exec (if restartable? "begin;" "begin immediate;"))
  (catch #t
    (lambda ()
      (let-values ((result (proc)))
        (exec "commit;")
        (apply values result)))
    (lambda args
      ;; The roll back may or may not have occurred automatically when the
      ;; error was generated. If it has occurred, this does nothing but signal
      ;; an error. If it hasn't occurred, this needs to be done.
      (false-if-exception (exec "rollback;"))
      (apply throw args))))

(define* (call-with-savepoint db proc
                              #:optional (savepoint-name "SomeSavepoint"))
  "Call PROC after creating a savepoint named SAVEPOINT-NAME.  If PROC exits
abnormally, rollback to that savepoint.  In all cases, remove the savepoint
prior to returning."
  (define (exec sql)
    (with-statement db sql stmt
      (sqlite-fold cons '() stmt)))

  (dynamic-wind
    (lambda ()
      (exec (string-append "SAVEPOINT " savepoint-name ";")))
    (lambda ()
      (catch #t
        proc
        (lambda args
          (exec (string-append "ROLLBACK TO " savepoint-name ";"))
          (apply throw args))))
    (lambda ()
      (exec (string-append "RELEASE " savepoint-name ";")))))

(define* (call-with-retrying-transaction db proc #:key restartable?)
  (call-with-SQLITE_BUSY-retrying
   (lambda ()
     (call-with-transaction db proc #:restartable? restartable?))))

(define* (call-with-retrying-savepoint db proc
                                       #:optional (savepoint-name
                                                   "SomeSavepoint"))
  (call-with-SQLITE_BUSY-retrying
   (lambda ()
     (call-with-savepoint db proc savepoint-name))))

(define %default-database-file
  ;; Default location of the store database.
  (string-append %store-database-directory "/db.sqlite"))

(define-syntax with-database
  (syntax-rules ()
    "Open DB from FILE and close it when the dynamic extent of EXP... is left.
If FILE doesn't exist, create it and initialize it as a new database.  Pass
#:wal-mode? to call-with-database."
    ((_ file db #:wal-mode? wal-mode? exp ...)
     (call-with-database file (lambda (db) exp ...) #:wal-mode? wal-mode?))
    ((_ file db exp ...)
     (call-with-database file (lambda (db) exp ...)))))

(define (sqlite-finalize stmt)
  ;; As of guile-sqlite3 0.1.0, cached statements aren't reset when
  ;; sqlite-finalize is invoked on them (see
  ;; https://notabug.org/guile-sqlite3/guile-sqlite3/issues/12).  This can
  ;; cause problems with automatically-started transactions, so we work around
  ;; it by wrapping sqlite-finalize so that sqlite-reset is always called.
  ;; This always works, because resetting a statement twice has no adverse
  ;; effects.  We can remove this once the fixed guile-sqlite3 is widespread.
  (sqlite-reset stmt)
  ((@ (sqlite3) sqlite-finalize) stmt))

(define (call-with-statement db sql proc)
  (let ((stmt (sqlite-prepare db sql #:cache? #t)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc stmt))
      (lambda ()
        (sqlite-finalize stmt)))))

(define-syntax-rule (with-statement db sql stmt exp ...)
  "Run EXP... with STMT bound to a prepared statement corresponding to the sql
string SQL for DB."
  (call-with-statement db sql
                       (lambda (stmt) exp ...)))

(define (last-insert-row-id db)
  ;; XXX: (sqlite3) currently lacks bindings for 'sqlite3_last_insert_rowid'.
  ;; Work around that.
  (with-statement db "SELECT last_insert_rowid();" stmt
    (match (sqlite-fold cons '() stmt)
      ((#(id)) id)
      (_ #f))))

(define path-id-sql
  "SELECT id FROM ValidPaths WHERE path = :path")

(define* (path-id db path)
  "If PATH exists in the 'ValidPaths' table, return its numerical
identifier.  Otherwise, return #f."
  (with-statement db path-id-sql stmt
    (sqlite-bind-arguments stmt #:path path)
    (match (sqlite-fold cons '() stmt)
      ((#(id) . _) id)
      (_ #f))))

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

  ;; It's important that querying the path-id and the insert/update operation
  ;; take place in the same transaction, as otherwise some other
  ;; process/thread/fiber could register the same path between when we check
  ;; whether it's already registered and when we register it, resulting in
  ;; duplicate paths (which, due to a 'unique' constraint, would cause an
  ;; exception to be thrown). With the default journaling mode this will
  ;; prevent writes from occurring during that sensitive time, but with WAL
  ;; mode it will instead arrange to return SQLITE_BUSY when a write occurs
  ;; between the start of a read transaction and its upgrading to a write
  ;; transaction (see https://sqlite.org/rescode.html#busy_snapshot).
  ;; Experimentally, it seems this SQLITE_BUSY will ignore a busy_timeout and
  ;; immediately return (makes sense, since waiting won't change anything).

  ;; Note that when that kind of SQLITE_BUSY error is returned, it will keep
  ;; being returned every time we try to upgrade the same outermost
  ;; transaction to a write transaction.  So when retrying, we have to restart
  ;; the *outermost* write transaction.  We can't inherently tell whether
  ;; we're the outermost write transaction, so we leave the retry-handling to
  ;; the caller.
  (call-with-savepoint db
    (lambda ()
      (let ((id (path-id db path)))
        (if id
            (with-statement db update-sql stmt
              (sqlite-bind-arguments stmt #:id id
                                     #:deriver deriver
                                     #:hash hash #:size nar-size #:time time)
              (sqlite-fold cons '() stmt))
            (with-statement db insert-sql stmt
              (sqlite-bind-arguments stmt
                                     #:path path #:deriver deriver
                                     #:hash hash #:size nar-size #:time time)
              (sqlite-fold cons '() stmt)))
        (last-insert-row-id db)))))

(define add-reference-sql
  "INSERT OR REPLACE INTO Refs (referrer, reference) VALUES (:referrer, :reference);")

(define (add-references db referrer references)
  "REFERRER is the id of the referring store item, REFERENCES is a list
ids of items referred to."
  (with-statement db add-reference-sql stmt
    (for-each (lambda (reference)
                (sqlite-reset stmt)
                (sqlite-bind-arguments stmt #:referrer referrer
                                       #:reference reference)
                (sqlite-fold cons '() stmt))
              references)))

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

(define* (reset-timestamps file #:key preserve-permissions?)
  "Reset the modification time on FILE and on all the files it contains, if
it's a directory.  Canonicalize file permissions unless PRESERVE-PERMISSIONS?
is true."
  ;; Note: We're resetting to one second after the Epoch like 'guix-daemon'
  ;; has always done.
  (let loop ((file file)
             (type (stat:type (lstat file))))
    (case type
      ((directory)
       (unless preserve-permissions?
         (chmod file #o555))
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
       (unless preserve-permissions?
         (chmod file (if (executable-file? file) #o555 #o444)))
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
  (define db-file
    (store-database-file #:prefix prefix
                         #:state-directory state-directory))

  (parameterize ((sql-schema schema))
    (with-database db-file db
      (register-items db (list (store-info path deriver references))
                      #:prefix prefix
                      #:deduplicate? deduplicate?
                      #:reset-timestamps? reset-timestamps?
                      #:log-port (%make-void-port "w")))))

(define %epoch
  ;; When it all began.
  (make-time time-utc 0 1))

(define* (register-items db items
                         #:key prefix
                         (deduplicate? #t)
                         (reset-timestamps? #t)
                         registration-time
                         (log-port (current-error-port)))
  "Register all of ITEMS, a list of <store-info> records as returned by
'read-reference-graph', in DB.  ITEMS must be in topological order (with
leaves first.)  REGISTRATION-TIME must be the registration time to be recorded
in the database; #f means \"now\".  Write a progress report to LOG-PORT."
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

  (call-with-retrying-transaction db
      (lambda ()
        (let* ((prefix   (format #f "registering ~a items" (length items)))
               (progress (progress-reporter/bar (length items)
                                                prefix log-port)))
          (call-with-progress-reporter progress
            (lambda (report)
              (for-each (lambda (item)
                          (register db item)
                          (report))
                        items)))))))
