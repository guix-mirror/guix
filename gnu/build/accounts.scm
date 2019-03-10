;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu build accounts)
  #:use-module (guix records)
  #:use-module (guix combinators)
  #:use-module (gnu system accounts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 rdelim)
  #:export (password-entry
            password-entry?
            password-entry-name
            password-entry-uid
            password-entry-gid
            password-entry-real-name
            password-entry-directory
            password-entry-shell

            shadow-entry
            shadow-entry?
            shadow-entry-name
            shadow-entry-minimum-change-period
            shadow-entry-maximum-change-period
            shadow-entry-change-warning-time
            shadow-entry-maximum-inactivity
            shadow-entry-expiration

            group-entry
            group-entry?
            group-entry-name
            group-entry-gid
            group-entry-members

            write-group
            write-passwd
            write-shadow
            read-group
            read-passwd
            read-shadow

            %id-min
            %id-max
            %system-id-min
            %system-id-max

            user+group-databases))

;;; Commentary:
;;;
;;; This modules provides functionality equivalent to the C library's
;;; <shadow.h>, <pwd.h>, and <grp.h> routines, as well as a subset of the
;;; functionality of the Shadow command-line tools.  It can parse and write
;;; /etc/passwd, /etc/shadow, and /etc/group.  It can also take care of UID
;;; and GID allocation in a way similar to what 'useradd' does.
;;;
;;; The benefit is twofold: less code is involved, and the ID allocation
;;; strategy and state preservation is made explicit.
;;;
;;; Code:


;;;
;;; Machinery to define user and group databases.
;;;

(define-syntax serialize-field
  (syntax-rules (serialization)
    ((_ entry (field get (serialization ->string string->) _ ...))
     (->string (get entry)))
    ((_ entry (field get _ ...))
     (get entry))))

(define-syntax deserialize-field
  (syntax-rules (serialization)
    ((_ str (field get (serialization ->string string->) _ ...))
     (string-> str))
    ((_ str (field get _ ...))
     str)))

(define-syntax let/fields
  (syntax-rules ()
    ((_ (((name get attributes ...) rest ...) lst) body ...)
     (let ((l lst))
       (let ((name (deserialize-field (car l)
                                      (name get attributes ...))))
         (let/fields ((rest ...) (cdr l)) body ...))))
    ((_ (() lst) body ...)
     (begin body ...))))

(define-syntax define-database-entry
  (syntax-rules (serialization)
    "Define a record data type, as per 'define-record-type*', with additional
information on how to serialize and deserialize the whole database as well as
each field."
    ((_ <record> record make-record record?
        (serialization separator entry->string string->entry)
        fields ...)
     (let-syntax ((field-name
                   (syntax-rules ()
                     ((_ (name _ (... ...))) name))))
       (define-record-type* <record> record make-record
         record?
         fields ...)

       (define (entry->string entry)
         (string-join (list (serialize-field entry fields) ...)
                      (string separator)))

       (define (string->entry str)
         (let/fields ((fields ...) (string-split str #\:))
                     (make-record (field-name fields) ...)))))))


(define number->string*
  (match-lambda
    ((? number? number) (number->string number))
    (_ "")))

(define (false-if-string=? false-string)
  (lambda (str)
    (if (string=? str false-string)
        #f
        str)))

(define (string-if-false str)
  (lambda (obj)
    (if (not obj) str obj)))

(define (comma-separated->list str)
  (string-tokenize str (char-set-complement (char-set #\,))))

(define (list->comma-separated lst)
  (string-join lst ","))


;;;
;;; Database definitions.
;;;

(define-database-entry <password-entry>           ;<pwd.h>
  password-entry make-password-entry
  password-entry?
  (serialization #\: password-entry->string string->password-entry)

  (name       password-entry-name)
  (password   password-entry-password
              (serialization (const "x") (const #f))
              (default "x"))
  (uid        password-entry-uid
              (serialization number->string string->number))
  (gid        password-entry-gid
              (serialization number->string string->number))
  (real-name  password-entry-real-name
              (default ""))
  (directory  password-entry-directory)
  (shell      password-entry-shell
              (default "/bin/sh")))

(define-database-entry <shadow-entry>             ;<shadow.h>
  shadow-entry make-shadow-entry
  shadow-entry?
  (serialization #\: shadow-entry->string string->shadow-entry)

  (name                  shadow-entry-name)       ;string
  (password              shadow-entry-password    ;string | #f
                         (serialization (string-if-false "!")
                                        (false-if-string=? "!"))
                         (default #f))
  (last-change           shadow-entry-last-change ;days since 1970-01-01
                         (serialization number->string* string->number)
                         (default 0))
  (minimum-change-period shadow-entry-minimum-change-period
                         (serialization number->string* string->number)
                         (default #f))            ;days | #f
  (maximum-change-period shadow-entry-maximum-change-period
                         (serialization number->string* string->number)
                         (default #f))            ;days | #f
  (change-warning-time   shadow-entry-change-warning-time
                         (serialization number->string* string->number)
                         (default #f))            ;days | #f
  (maximum-inactivity    shadow-entry-maximum-inactivity
                         (serialization number->string* string->number)
                         (default #f))             ;days | #f
  (expiration            shadow-entry-expiration
                         (serialization number->string* string->number)
                         (default #f))            ;days since 1970-01-01 | #f
  (flags                 shadow-entry-flags       ;"reserved"
                         (serialization number->string* string->number)
                         (default #f)))

(define-database-entry <group-entry>              ;<grp.h>
  group-entry make-group-entry
  group-entry?
  (serialization #\: group-entry->string string->group-entry)

  (name            group-entry-name)
  (password        group-entry-password
                   (serialization (string-if-false "x")
                                  (false-if-string=? "x"))
                   (default #f))
  (gid             group-entry-gid
                   (serialization number->string string->number))
  (members         group-entry-members
                   (serialization list->comma-separated comma-separated->list)
                   (default '())))

(define (database-writer file mode entry->string)
  (lambda* (entries #:optional (file-or-port file))
    "Write ENTRIES to FILE-OR-PORT.  When FILE-OR-PORT is a file name, write
to it atomically and set the appropriate permissions."
    (define (write-entries port)
      (for-each (lambda (entry)
                  (display (entry->string entry) port)
                  (newline port))
                entries))

    (if (port? file-or-port)
        (write-entries file-or-port)
        (let* ((template (string-append file-or-port ".XXXXXX"))
               (port     (mkstemp! template)))
          (dynamic-wind
            (const #t)
            (lambda ()
              (chmod port mode)
              (write-entries port)
              (rename-file template file-or-port))
            (lambda ()
              (close-port port)
              (when (file-exists? template)
                (delete-file template))))))))

(define write-passwd
  (database-writer "/etc/passwd" #o644 password-entry->string))
(define write-shadow
  (database-writer "/etc/shadow" #o600 shadow-entry->string))
(define write-group
  (database-writer "/etc/group" #o644 group-entry->string))

(define (database-reader file string->entry)
  (lambda* (#:optional (file-or-port file))
    (define (read-entries port)
      (let loop ((entries '()))
        (match (read-line port)
          ((? eof-object?)
           (reverse entries))
          (line
           (loop (cons (string->entry line) entries))))))

    (if (port? file-or-port)
        (read-entries file-or-port)
        (call-with-input-file file-or-port
          read-entries))))

(define read-passwd
  (database-reader "/etc/passwd" string->password-entry))
(define read-shadow
  (database-reader "/etc/shadow" string->shadow-entry))
(define read-group
  (database-reader "/etc/group" string->group-entry))


;;;
;;; Building databases.
;;;

(define-record-type* <allocation>
  allocation make-allocation
  allocation?
  (ids            allocation-ids (default vlist-null))
  (next-id        allocation-next-id (default %id-min))
  (next-system-id allocation-next-system-id (default %system-id-max)))

;; Trick to avoid name clashes...
(define-syntax %allocation (identifier-syntax allocation))

;; Minimum and maximum UIDs and GIDs (from find_new_uid.c and find_new_gid.c
;; in Shadow.)
(define %id-min 1000)
(define %id-max 60000)

(define %system-id-min 100)
(define %system-id-max 999)

(define (system-id? id)
  (and (> id %system-id-min)
       (<= id %system-id-max)))

(define (user-id? id)
  (and (>= id %id-min)
       (< id %id-max)))

(define* (allocate-id assignment #:key system?)
  "Return two values: a newly allocated ID, and an updated <allocation> record
based on ASSIGNMENT.  If SYSTEM? is true, return a system ID."
  (define next
    ;; Return the next available ID, looping if necessary.
    (if system?
        (lambda (id)
          (let ((next-id (- id 1)))
            (if (< next-id %system-id-min)
                %system-id-max
                next-id)))
        (lambda (id)
          (let ((next-id (+ id 1)))
            (if (>= next-id %id-max)
                %id-min
                next-id)))))

  (let loop ((id (if system?
                     (allocation-next-system-id assignment)
                     (allocation-next-id assignment))))
    (if (vhash-assv id (allocation-ids assignment))
        (loop (next id))
        (let ((taken (vhash-consv id #t (allocation-ids assignment))))
          (values (if system?
                      (allocation (inherit assignment)
                                  (next-system-id (next id))
                                  (ids taken))
                      (allocation (inherit assignment)
                                  (next-id (next id))
                                  (ids taken)))
                  id)))))

(define* (reserve-ids allocation ids #:key (skip? #t))
  "Mark the numbers listed in IDS as reserved in ALLOCATION.  When SKIP? is
true, start allocation after the highest (or lowest, depending on whether it's
a system ID allocation) number among IDS."
  (%allocation
   (inherit allocation)
   (next-id (if skip?
                (+ (reduce max
                           (- (allocation-next-id allocation) 1)
                           (filter user-id? ids))
                   1)
                (allocation-next-id allocation)))
   (next-system-id
    (if skip?
        (- (reduce min
                   (+ 1 (allocation-next-system-id allocation))
                   (filter system-id? ids))
           1)
        (allocation-next-system-id allocation)))
   (ids (fold (cut vhash-consv <> #t <>)
              (allocation-ids allocation)
              ids))))

(define (allocated? allocation id)
  "Return true if ID is already allocated as part of ALLOCATION."
  (->bool (vhash-assv id (allocation-ids allocation))))

(define (lookup-procedure lst key)
  "Return a lookup procedure for the elements of LST, calling KEY to obtain
the key of each element."
  (let ((table (fold (lambda (obj table)
                       (vhash-cons (key obj) obj table))
                     vlist-null
                     lst)))
    (lambda (key)
      (match (vhash-assoc key table)
        (#f #f)
        ((_ . value) value)))))

(define* (allocate-groups groups members
                          #:optional (current-groups '()))
  "Return a list of group entries for GROUPS, a list of <user-group>.  Members
for each group are taken from MEMBERS, a vhash that maps group names to member
names.  GIDs and passwords found in CURRENT-GROUPS, a list of group entries,
are reused."
  (define gids
    ;; Mark all the currently-used GIDs and the explicitly requested GIDs as
    ;; reserved.
    (reserve-ids (reserve-ids (allocation)
                              (map group-entry-gid current-groups))
                 (filter-map user-group-id groups)
                 #:skip? #f))

  (define previous-entry
    (lookup-procedure current-groups group-entry-name))

  (reverse
   (fold2 (lambda (group result allocation)
            (let ((name         (user-group-name group))
                  (password     (user-group-password group))
                  (requested-id (user-group-id group))
                  (system?      (user-group-system? group)))
              (let*-values (((previous)
                             (previous-entry name))
                            ((allocation id)
                             (cond
                              ((number? requested-id)
                               (values (reserve-ids allocation
                                                    (list requested-id))
                                       requested-id))
                              (previous
                               (values allocation
                                       (group-entry-gid previous)))
                              (else
                               (allocate-id allocation
                                            #:system? system?)))))
                (values (cons (group-entry
                               (name name)
                               (password
                                (if previous
                                    (group-entry-password previous)
                                    password))
                               (gid id)
                               (members (vhash-fold* cons '() name members)))
                              result)
                        allocation))))
          '()
          gids
          groups)))

(define* (allocate-passwd users groups #:optional (current-passwd '()))
  "Return a list of password entries for USERS, a list of <user-account>.
Take GIDs from GROUPS, a list of group entries.  Reuse UIDs from
CURRENT-PASSWD, a list of password entries, when possible; otherwise allocate
new UIDs."
  (define uids
    (reserve-ids (reserve-ids (allocation)
                              (map password-entry-uid current-passwd))
                 (filter-map user-account-uid users)
                 #:skip? #f))

  (define previous-entry
    (lookup-procedure current-passwd password-entry-name))

  (define (group-id name)
    (or (any (lambda (entry)
               (and (string=? (group-entry-name entry) name)
                    (group-entry-gid entry)))
             groups)
        (error "group not found" name)))

  (reverse
   (fold2 (lambda (user result allocation)
            (let ((name         (user-account-name user))
                  (requested-id (user-account-uid user))
                  (group        (user-account-group user))
                  (real-name    (user-account-comment user))
                  (directory    (user-account-home-directory user))
                  (shell        (user-account-shell user))
                  (system?      (user-account-system? user)))
              (let*-values (((previous)
                             (previous-entry name))
                            ((allocation id)
                             (cond
                              ((number? requested-id)
                               (values (reserve-ids allocation
                                                    (list requested-id))
                                       requested-id))
                              (previous
                               (values allocation
                                       (password-entry-uid previous)))
                              (else
                               (allocate-id allocation
                                            #:system? system?)))))
                (values (cons (password-entry
                               (name name)
                               (uid id)
                               (directory directory)
                               (gid (if (number? group) group (group-id group)))
                               (real-name (if previous
                                              (password-entry-real-name previous)
                                              real-name))
                               (shell (if previous
                                          (password-entry-shell previous)
                                          shell)))
                              result)
                        allocation))))
          '()
          uids
          users)))

(define* (days-since-epoch #:optional (current-time current-time))
  "Return the number of days elapsed since the 1st of January, 1970."
  (let* ((now   (current-time time-utc))
         (epoch (make-time time-utc 0 0))
         (diff  (time-difference now epoch)))
    (quotient (time-second diff) (* 24 3600))))

(define* (passwd->shadow users passwd #:optional (current-shadow '())
                         #:key (current-time current-time))
  "Return a list of shadow entries for the password entries listed in PASSWD.
Reuse shadow entries from CURRENT-SHADOW when they exist, and take the initial
password from USERS."
  (define previous-entry
    (lookup-procedure current-shadow shadow-entry-name))

  (define now
    (days-since-epoch current-time))

  (map (lambda (user passwd)
         (or (previous-entry (password-entry-name passwd))
             (shadow-entry (name (password-entry-name passwd))
                           (password (user-account-password user))
                           (last-change now))))
       users passwd))

(define (empty-if-not-found thunk)
  "Call THUNK and return the empty list if that throws to ENOENT."
  (catch 'system-error
    thunk
    (lambda args
      (if (= ENOENT (system-error-errno args))
          '()
          (apply throw args)))))

(define* (user+group-databases users groups
                               #:key
                               (current-passwd
                                (empty-if-not-found read-passwd))
                               (current-groups
                                (empty-if-not-found read-group))
                               (current-shadow
                                (empty-if-not-found read-shadow))
                               (current-time current-time))
  "Return three values: the list of group entries, the list of password
entries, and the list of shadow entries corresponding to USERS and GROUPS.
Preserve stateful bits from CURRENT-PASSWD, CURRENT-GROUPS, and
CURRENT-SHADOW: UIDs, GIDs, passwords, user shells, etc."
  (define members
    ;; Map group name to user names.
    (fold (lambda (user members)
            (fold (cute vhash-cons <> (user-account-name user) <>)
                  members
                  (user-account-supplementary-groups user)))
          vlist-null
          users))

  (define group-entries
    (allocate-groups groups members current-groups))

  (define passwd-entries
    (allocate-passwd users group-entries current-passwd))

  (define shadow-entries
    (passwd->shadow users passwd-entries current-shadow
                    #:current-time current-time))

  (values group-entries passwd-entries shadow-entries))
