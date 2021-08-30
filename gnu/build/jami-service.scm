;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

;;; Commentary:
;;;
;;; This module contains helpers used as part of the jami-service-type
;;; definition.
;;;
;;; Code:

(define-module (gnu build jami-service)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (rnrs io ports)
  #:autoload (shepherd service) (fork+exec-command)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (account-fingerprint?
            account-details->recutil
            get-accounts
            get-usernames
            set-account-details
            add-account
            account->username
            username->account
            username->contacts
            enable-account
            disable-account

            add-contact
            remove-contact

            set-all-moderators
            set-moderator
            username->all-moderators?
            username->moderators

            dbus-available-services
            dbus-service-available?

            %send-dbus-binary
            %send-dbus-bus
            %send-dbus-user
            %send-dbus-group
            %send-dbus-debug
            send-dbus

            with-retries))

;;;
;;; Utilities.
;;;

(define-syntax-rule (with-retries n delay body ...)
  "Retry the code in BODY up to N times until it doesn't raise an exception
nor return #f, else raise an error.  A delay of DELAY seconds is inserted
before each retry."
  (let loop ((attempts 0))
    (catch #t
      (lambda ()
        (let ((result (begin body ...)))
          (if (not result)
              (error "failed attempt" attempts)
              result)))
      (lambda args
        (if (< attempts n)
            (begin
              (sleep delay)             ;else wait and retry
              (loop (+ 1 attempts)))
            (error "maximum number of retry attempts reached"
                   body ... args))))))

(define (alist->list alist)
  "Flatten ALIST into a list."
  (append-map (match-lambda
                (() '())
                ((key . value)
                 (list key value)))
              alist))

(define account-fingerprint-rx (make-regexp "[0-9A-Fa-f]{40}"))

(define (account-fingerprint? val)
  "A Jami account fingerprint is 40 characters long and only contains
hexadecimal characters."
  (and (string? val)
       (regexp-exec account-fingerprint-rx val)))


;;;
;;; D-Bus reply parser.
;;;

(define (parse-dbus-reply reply)
  "Return the parse tree of REPLY, a string returned by the 'dbus-send'
command."
  ;; Refer to 'man 1 dbus-send' for the grammar reference.  Note that the
  ;; format of the replies doesn't match the format of the input, which is the
  ;; one documented, but it gives an idea.  For an even better reference, see
  ;; the `print_iter' procedure of the 'dbus-print-message.c' file from the
  ;; 'dbus' package sources.
  (define-peg-string-patterns
    "contents <- header (item / container (item / container*)?)
     item <-- WS type WS value NL
     container <- array / dict / variant
     array <-- array-start (item / container)* array-end
     dict <-- array-start dict-entry* array-end
     dict-entry <-- dict-entry-start item item dict-entry-end
     variant <-- variant-start item
     type <-- 'string' / 'int16' / 'uint16' / 'int32' / 'uint32' / 'int64' /
              'uint64' / 'double' / 'byte' / 'boolean' / 'objpath'
     value <-- (!NL .)* NL
     header < (!NL .)* NL
     variant-start < WS 'variant'
     array-start < WS 'array [' NL
     array-end < WS ']' NL
     dict-entry-start < WS 'dict entry(' NL
     dict-entry-end < WS ')' NL
     DQ < '\"'
     WS < ' '*
     NL < '\n'*")

  (peg:tree (match-pattern contents reply)))

(define (strip-quotes text)
  "Strip the leading and trailing double quotes (\") characters from TEXT."
  (let* ((text* (if (string-prefix? "\"" text)
                    (string-drop text 1)
                    text))
         (text** (if (string-suffix? "\"" text*)
                     (string-drop-right text* 1)
                     text*)))
    text**))

(define (deserialize-item item)
  "Return the value described by the ITEM parse tree as a Guile object."
  ;; Strings are printed wrapped in double quotes (see the print_iter
  ;; procedure in dbus-print-message.c).
  (match item
    (('item ('type "string") ('value value))
     (strip-quotes value))
    (('item ('type "boolean") ('value value))
     (if (string=? "true" value)
         #t
         #f))
    (('item _ ('value value))
     value)))

(define (serialize-boolean bool)
  "Return the serialized format expected by dbus-send for BOOL."
  (format #f "boolean:~:[false~;true~]" bool))

(define (dict->alist dict-parse-tree)
  "Translate a dict parse tree to an alist."
  (define (tuples->alist tuples)
    (map (lambda (x) (apply cons x)) tuples))

  (match dict-parse-tree
    ('dict
     '())
    (('dict ('dict-entry keys values) ...)
     (let ((keys* (map deserialize-item keys))
           (values* (map deserialize-item values)))
       (tuples->alist (zip keys* values*))))))

(define (array->list array-parse-tree)
  "Translate an array parse tree to a list."
  (match array-parse-tree
    ('array
     '())
    (('array items ...)
     (map deserialize-item items))))


;;;
;;; Low-level, D-Bus-related procedures.
;;;

;;; The following parameters are used in the jami-service-type service
;;; definition to conveniently customize the behavior of the send-dbus helper,
;;; even when called indirectly.
(define %send-dbus-binary (make-parameter "dbus-send"))
(define %send-dbus-bus (make-parameter #f))
(define %send-dbus-user (make-parameter #f))
(define %send-dbus-group (make-parameter #f))
(define %send-dbus-debug (make-parameter #f))

(define* (send-dbus #:key service path interface method
                    bus
                    dbus-send
                    user group
                    timeout
                    arguments)
  "Return the response of DBUS-SEND, else raise an error.  Unless explicitly
provided, DBUS-SEND takes the value of the %SEND-DBUS-BINARY parameter.  BUS
can be used to specify the bus address, such as 'unix:path=/var/run/jami/bus'.
Alternatively, the %SEND-DBUS-BUS parameter can be used.  ARGUMENTS can be
used to pass input values to a D-Bus method call.  TIMEOUT is the amount of
time to wait for a reply in milliseconds before giving up with an error.  USER
and GROUP allow choosing under which user/group the DBUS-SEND command is
executed.  Alternatively, the %SEND-DBUS-USER and %SEND-DBUS-GROUP parameters
can be used instead."
  (let* ((command `(,(if dbus-send
                         dbus-send
                         (%send-dbus-binary))
                    ,@(if (or bus (%send-dbus-bus))
                          (list (string-append "--bus="
                                               (or bus (%send-dbus-bus))))
                          '())
                    "--print-reply"
                    ,@(if timeout
                          (list (format #f "--reply-timeout=~d" timeout))
                          '())
                    ,(string-append "--dest=" service) ;e.g., cx.ring.Ring
                    ,path            ;e.g., /cx/ring/Ring/ConfigurationManager
                    ,(string-append interface "." method)
                    ,@(or arguments '())))
         (temp-port (mkstemp! (string-copy "/tmp/dbus-send-output-XXXXXXX")))
         (temp-file (port-filename temp-port)))
    (dynamic-wind
      (lambda ()
        (let* ((uid (or (and=> (or user (%send-dbus-user))
                               (compose passwd:uid getpwnam)) -1))
               (gid (or (and=> (or group (%send-dbus-group))
                               (compose group:gid getgrnam)) -1)))
          (chown temp-port uid gid)))
      (lambda ()
        (let ((pid (fork+exec-command command
                                      #:user (or user (%send-dbus-user))
                                      #:group (or group (%send-dbus-group))
                                      #:log-file temp-file)))
          (match (waitpid pid)
            ((_ . status)
             (let ((exit-status (status:exit-val status))
                   (output (call-with-port temp-port get-string-all)))
               (if (= 0 exit-status)
                   output
                   (error "the send-dbus command exited with: "
                          command exit-status output)))))))
      (lambda ()
        (false-if-exception (delete-file temp-file))))))

(define (parse-account-ids reply)
  "Return the Jami account IDs from REPLY, which is assumed to be the output
of the Jami D-Bus `getAccountList' method."
  (array->list (parse-dbus-reply reply)))

(define (parse-account-details reply)
  "Parse REPLY, which is assumed to be the output of the Jami D-Bus
`getAccountDetails' method, and return its content as an alist."
  (dict->alist (parse-dbus-reply reply)))

(define (parse-contacts reply)
  "Parse REPLY, which is assumed to be the output of the Jamid D-Bus
`getContacts' method, and return its content as an alist."
  (match (parse-dbus-reply reply)
    ('array
     '())
    (('array dicts ...)
     (map dict->alist dicts))))


;;;
;;; Higher-level, D-Bus-related procedures.
;;;

(define (validate-fingerprint fingerprint)
  "Validate that fingerprint is 40 characters long."
  (unless (account-fingerprint? fingerprint)
    (error "Account fingerprint is not valid:" fingerprint)))

(define (dbus-available-services)
  "Return the list of available (acquired) D-Bus services."
  (let ((reply (parse-dbus-reply
                (send-dbus #:service "org.freedesktop.DBus"
                           #:path "/org/freedesktop/DBus"
                           #:interface "org.freedesktop.DBus"
                           #:method "ListNames"))))
    ;; Remove entries such as ":1.7".
    (remove (cut string-prefix? ":" <>)
            (array->list reply))))

(define (dbus-service-available? service)
  "Predicate to check for the D-Bus SERVICE availability."
  (member service (dbus-available-services)))

(define* (send-dbus/configuration-manager #:key method arguments timeout)
  "Query the Jami D-Bus ConfigurationManager service."
  (send-dbus #:service "cx.ring.Ring"
             #:path "/cx/ring/Ring/ConfigurationManager"
             #:interface "cx.ring.Ring.ConfigurationManager"
             #:method method
             #:arguments arguments
             #:timeout timeout))

;;; The following methods are for internal use; they make use of the account
;;; ID, an implementation detail of Jami the user should not need to be
;;; concerned with.
(define (get-account-ids)
  "Return the available Jami account identifiers (IDs).  Account IDs are an
implementation detail used to identify the accounts in Jami."
  (parse-account-ids
   (send-dbus/configuration-manager #:method "getAccountList")))

(define (id->account-details id)
  "Retrieve the account data associated with the given account ID."
  (parse-account-details
   (send-dbus/configuration-manager
    #:method "getAccountDetails"
    #:arguments (list (string-append "string:" id)))))

(define (id->volatile-account-details id)
  "Retrieve the account data associated with the given account ID."
  (parse-account-details
   (send-dbus/configuration-manager
    #:method "getVolatileAccountDetails"
    #:arguments (list (string-append "string:" id)))))

(define (id->account id)
  "Retrieve the complete account data associated with the given account ID."
  (append (id->volatile-account-details id)
          (id->account-details id)))

(define %username-to-id-cache #f)

(define (invalidate-username-to-id-cache!)
  (set! %username-to-id-cache #f))

(define (username->id username)
  "Return the first account ID corresponding to USERNAME."
  (unless (assoc-ref %username-to-id-cache username)
    (set! %username-to-id-cache
          (append-map
           (lambda (id)
             (let* ((account (id->account id))
                    (username (assoc-ref account "Account.username"))
                    (registered-name (assoc-ref account
                                                "Account.registeredName")))
               `(,@(if username
                       (list (cons username id))
                       '())
                 ,@(if registered-name
                       (list (cons registered-name id))
                       '()))))
           (get-account-ids))))
  (or (assoc-ref %username-to-id-cache username)
      (let ((message (format #f "Could not retrieve a local account ID\
 for ~:[username~;fingerprint~]" (account-fingerprint? username))))
        (error message username))))

(define (account->username account)
  "Return USERNAME, the registered username associated with ACCOUNT, else its
public key fingerprint."
  (or (assoc-ref account "Account.registeredName")
      (assoc-ref account "Account.username")))

(define (id->username id)
  "Return USERNAME, the registered username associated with ID, else its
public key fingerprint, else #f."
  (account->username (id->account id)))

(define (get-accounts)
  "Return the list of all accounts, as a list of alists."
  (map id->account (get-account-ids)))

(define (get-usernames)
  "Return the list of the usernames associated with the present accounts."
  (map account->username (get-accounts)))

(define (username->account username)
  "Return the first account associated with USERNAME, else #f.
USERNAME can be either the account 40 characters public key fingerprint or a
registered username."
  (find (lambda (account)
          (member username
                  (list (assoc-ref account "Account.username")
                        (assoc-ref account "Account.registeredName"))))
        (get-accounts)))

(define (add-account archive)
  "Import the Jami account ARCHIVE and return its account ID.  The archive
should *not* be encrypted with a password.  Return the username associated
with the account."
  (invalidate-username-to-id-cache!)
  (let ((reply (send-dbus/configuration-manager
                #:method "addAccount"
                #:arguments (list (string-append
                                   "dict:string:string:Account.archivePath,"
                                   archive
                                   ",Account.type,RING")))))
    ;; The account information takes some time to be populated.
    (let ((id (deserialize-item (parse-dbus-reply reply))))
      (with-retries 20 1
        (let ((username (id->username id)))
          (if (string-null? username)
              #f
              username))))))

(define (remove-account username)
  "Delete the Jami account associated with USERNAME, the account 40 characters
fingerprint or a registered username."
  (let ((id (username->id username)))
    (send-dbus/configuration-manager
     #:method "removeAccount"
     #:arguments (list (string-append "string:" id))))
  (invalidate-username-to-id-cache!))

(define* (username->contacts username)
  "Return the contacts associated with the account of USERNAME as two values;
the first one being the regular contacts and the second one the banned
contacts.  USERNAME can be either the account 40 characters public key
fingerprint or a registered username.  The contacts returned are represented
using their 40 characters fingerprint."
  (let* ((id (username->id username))
         (reply (send-dbus/configuration-manager
                 #:method "getContacts"
                 #:arguments (list (string-append "string:" id))))
         (all-contacts (parse-contacts reply))
         (banned? (lambda (contact)
                    (and=> (assoc-ref contact "banned")
                           (cut string=? "true" <>))))
         (banned (filter banned? all-contacts))
         (not-banned (filter (negate banned?) all-contacts))
         (fingerprint (cut assoc-ref <> "id")))
    (values (map fingerprint not-banned)
            (map fingerprint banned))))

(define* (remove-contact contact username #:key ban?)
  "Remove CONTACT, the 40 characters public key fingerprint of a contact, from
the account associated with USERNAME (either a fingerprint or a registered
username).  When BAN? is true, also mark the contact as banned."
  (validate-fingerprint contact)
  (let ((id (username->id username)))
    (send-dbus/configuration-manager
     #:method "removeContact"
     #:arguments (list (string-append "string:" id)
                       (string-append "string:" contact)
                       (serialize-boolean ban?)))))

(define (add-contact contact username)
  "Add CONTACT, the 40 characters public key fingerprint of a contact, to the
account of USERNAME (either a fingerprint or a registered username)."
  (validate-fingerprint contact)
  (let ((id (username->id username)))
    (send-dbus/configuration-manager
     #:method "addContact"
     #:arguments (list (string-append "string:" id)
                       (string-append "string:" contact)))))

(define* (set-account-details details username #:key timeout)
  "Set DETAILS, an alist containing the key value pairs to set for the account
of USERNAME, a registered username or account fingerprint.  The value of the
parameters not provided are unchanged.  TIMEOUT is a value in milliseconds to
pass to the `send-dbus/configuration-manager' procedure."
  (let* ((id (username->id username))
         (current-details (id->account-details id))
         (updated-details (map (match-lambda
                                 ((key . value)
                                  (or (and=> (assoc-ref details key)
                                             (cut cons key <>))
                                      (cons key value))))
                               current-details))
         ;; dbus-send does not permit sending null strings (it throws a
         ;; "malformed dictionary" error).  Luckily they seem to have the
         ;; semantic of "default account value" in Jami; so simply drop them.
         (updated-details* (remove (match-lambda
                                     ((_ . value)
                                      (string-null? value)))
                                   updated-details)))
    (send-dbus/configuration-manager
     #:timeout timeout
     #:method "setAccountDetails"
     #:arguments
     (list (string-append "string:" id)
           (string-append "dict:string:string:"
                          (string-join (alist->list updated-details*)
                                       ","))))))

(define (set-all-moderators enabled? username)
  "Set the 'AllModerators' property to enabled? for the account of USERNAME, a
registered username or account fingerprint."
  (let ((id (username->id username)))
    (send-dbus/configuration-manager
     #:method "setAllModerators"
     #:arguments
     (list (string-append "string:" id)
           (serialize-boolean enabled?)))))

(define (username->all-moderators? username)
  "Return the 'AllModerators' property for the account of USERNAME, a
registered username or account fingerprint."
  (let* ((id (username->id username))
         (reply (send-dbus/configuration-manager
                 #:method "isAllModerators"
                 #:arguments
                 (list (string-append "string:" id)))))
    (deserialize-item (parse-dbus-reply reply))))

(define (username->moderators username)
  "Return the moderators for the account of USERNAME, a registered username or
account fingerprint."
  (let* ((id (username->id username))
         (reply (send-dbus/configuration-manager
                 #:method "getDefaultModerators"
                 #:arguments
                 (list (string-append "string:" id)))))
    (array->list (parse-dbus-reply reply))))

(define (set-moderator contact enabled? username)
  "Set the moderator flag to ENABLED? for CONTACT, the 40 characters public
key fingerprint of a contact for the account of USERNAME, a registered
username or account fingerprint."
  (validate-fingerprint contact)
  (let* ((id (username->id username)))
    (send-dbus/configuration-manager #:method "setDefaultModerator"
                                     #:arguments
                                     (list (string-append "string:" id)
                                           (string-append "string:" contact)
                                           (serialize-boolean enabled?)))))

(define (disable-account username)
  "Disable the account known by USERNAME, a registered username or account
fingerprint."
  (set-account-details '(("Account.enable" . "false")) username
                       ;; Waiting for the reply on this command takes a very
                       ;; long time that trips the default D-Bus timeout value
                       ;; (25 s), for some reason.
                        #:timeout 60000))

(define (enable-account username)
  "Enable the account known by USERNAME, a registered username or account
fingerprint."
  (set-account-details '(("Account.enable" . "true")) username))


;;;
;;; Presentation procedures.
;;;

(define (.->_ text)
  "Map each period character to underscore characters."
  (string-map (match-lambda
                (#\. #\_)
                (c c))
              text))

(define (account-details->recutil account-details)
  "Serialize the account-details alist into a recutil string.  Period
characters in the keys are normalized to underscore to meet Recutils' format
requirements."
  (define (pair->recutil-property pair)
    (match pair
      ((key . value)
       (string-append (.->_ key) ": " value))))

  (define sorted-account-details
    ;; Have the account username, display name and alias appear first, for
    ;; convenience.
    (let ((first-items '("Account.username"
                         "Account.displayName"
                         "Account.alias")))
      (append (map (cut assoc <> account-details) first-items)
              (fold alist-delete account-details first-items))))

  (string-join (map pair->recutil-property sorted-account-details) "\n"))

;; Local Variables:
;; eval: (put 'with-retries 'scheme-indent-function 2)
;; End:
