;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
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

(define-module (gnu services getmail)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages tls)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (getmail-retriever-configuration
            getmail-retriever-configuration-extra-parameters
            getmail-destination-configuration
            getmail-options-configuration
            getmail-configuration-file
            getmail-configuration
            getmail-service-type))

;;; Commentary:
;;;
;;; Service for the getmail mail retriever.
;;;
;;; Code:

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (string-join (string-split (if (string-suffix? "?" str)
                                   (substring str 0 (1- (string-length str)))
                                   str)
                               #\-)
                 "_")))

(define (serialize-field field-name val)
  #~(let ((val '#$val))
      (format #f "~a = ~a\n"
              #$(uglify-field-name field-name)
              (cond
               ((list? val)
                (string-append
                 "("
                 (string-concatenate
                  (map (lambda (list-val)
                         (format #f "\"~a\", " list-val))
                       val))
                 ")"))
               (else
                val)))))

(define (serialize-string field-name val)
  (if (string=? val "")
      ""
      (serialize-field field-name val)))

(define (string-or-filelike? val)
  (or (string? val)
      (file-like? val)))
(define (serialize-string-or-filelike field-name val)
  (if (equal? val "")
      ""
      (serialize-field field-name val)))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val "true" "false")))

(define (non-negative-integer? val)
  (and (exact-integer? val) (not (negative? val))))
(define (serialize-non-negative-integer field-name val)
  (serialize-field field-name val))

(define serialize-list serialize-field)

(define parameter-alist? list?)
(define (serialize-parameter-alist field-name val)
  #~(string-append
     #$@(map (match-lambda
               ((key . value)
                (serialize-field key value)))
             val)))

(define (serialize-getmail-retriever-configuration field-name val)
  (serialize-configuration val getmail-retriever-configuration-fields))

(define-configuration getmail-retriever-configuration
  (type
   (string "SimpleIMAPSSLRetriever")
   "The type of mail retriever to use.  Valid values include
@samp{passwd} and @samp{static}.")
  (server
   (string 'unset)
   "Space separated list of arguments to the userdb driver.")
  (username
   (string 'unset)
   "Space separated list of arguments to the userdb driver.")
  (port
   (non-negative-integer #f)
   "Space separated list of arguments to the userdb driver.")
  (password
   (string "")
   "Override fields from passwd.")
  (password-command
   (list '())
   "Override fields from passwd.")
  (keyfile
   (string "")
   "PEM-formatted key file to use for the TLS negotiation")
  (certfile
   (string "")
   "PEM-formatted certificate file to use for the TLS negotiation")
  (ca-certs
   (string "")
   "CA certificates to use")
  (extra-parameters
   (parameter-alist '())
   "Extra retriever parameters"))

(define (serialize-getmail-destination-configuration field-name val)
  (serialize-configuration val getmail-destination-configuration-fields))

(define-configuration getmail-destination-configuration
  (type
   (string 'unset)
   "The type of mail destination.  Valid values include @samp{Maildir},
@samp{Mboxrd} and @samp{MDA_external}.")
  (path
   (string-or-filelike "")
   "The path option for the mail destination.  The behaviour depends on the
chosen type.")
  (extra-parameters
   (parameter-alist '())
   "Extra destination parameters"))

(define (serialize-getmail-options-configuration field-name val)
  (serialize-configuration val getmail-options-configuration-fields))

(define-configuration getmail-options-configuration
  (verbose
   (non-negative-integer 1)
   "If set to @samp{0}, getmail will only print warnings and errors.  A value
of @samp{1} means that messages will be printed about retrieving and deleting
messages. If set to @samp{2}, getmail will print messages about each of it's
actions.")
  (read-all
   (boolean #t)
   "If true, getmail will retrieve all available messages.  Otherwise it will
only retrieve messages it hasn't seen previously.")
  (delete
   (boolean #f)
   "If set to true, messages will be deleted from the server after retrieving
and successfully delivering them.  Otherwise, messages will be left on the
server.")
  (delete-after
   (non-negative-integer 0)
   "Getmail will delete messages this number of days after seeing them, if
they have not been delivered.  This means messages will be left on the server
this number of days after delivering them.  A value of @samp{0} disabled this
feature.")
  (delete-bigger-than
   (non-negative-integer 0)
   "Delete messages larger than this of bytes after retrieving them, even if
the delete and delete-after options are disabled.  A value of @samp{0}
disables this feature.")
  (max-bytes-per-session
   (non-negative-integer 0)
   "Retrieve messages totalling up to this number of bytes before closing the
session with the server.  A value of @samp{0} disables this feature.")
  (max-message-size
   (non-negative-integer 0)
   "Don't retrieve messages larger than this number of bytes.  A value of
@samp{0} disables this feature.")
  (delivered-to
   (boolean #t)
   "If true, getmail will add a Delivered-To header to messages.")
  (received
   (boolean #t)
   "If set, getmail adds a Received header to the messages.")
  (message-log
   (string "")
   "Getmail will record a log of its actions to the named file.  A value of
@samp{\"\"} disables this feature.")
  (message-log-syslog
   (boolean #t)
   "If true, getmail will record a log of its actions using the system
logger.")
  (message-log-verbose
   (boolean #t)
   "If true, getmail will log information about messages not retrieved and the
reason for not retrieving them, as well as starting and ending information
lines.")
  (extra-parameters
   (parameter-alist '())
   "Extra options to include."))

(define (serialize-getmail-configuration-file field-name val)
  (match val
    (($ <getmail-configuration-file> location
                                     retriever destination options)
     #~(string-append
        "[retriever]\n"
        #$(serialize-getmail-retriever-configuration #f retriever)
        "\n[destination]\n"
        #$(serialize-getmail-destination-configuration #f destination)
        "\n[options]\n"
        #$(serialize-getmail-options-configuration #f options)))))

(define-configuration getmail-configuration-file
  (retriever
   (getmail-retriever-configuration (getmail-retriever-configuration))
   "What mail account to retrieve mail from, and how to access that account.")
  (destination
   (getmail-destination-configuration (getmail-destination-configuration))
   "What to do with retrieved messages.")
  (options
   (getmail-options-configuration (getmail-options-configuration))
   "Configure getmail."))

(define (serialize-symbol field-name val) "")
(define (serialize-getmail-configuration field-name val) "")

(define-configuration getmail-configuration
  (name
   (symbol "unset")
   "A symbol to identify the getmail service.")
  (package
   (package getmail)
   "The getmail package to use.")
  (user
   (string "getmail")
   "The user to run getmail as.")
  (group
   (string "getmail")
   "The group to run getmail as.")
  (directory
   (string "/var/lib/getmail/default")
   "The getmail directory to use.")
  (rcfile
   (getmail-configuration-file (getmail-configuration-file))
   "The getmail configuration file to use.")
  (idle
   (list '())
   "A list of mailboxes that getmail should wait on the server for new mail
notifications.  This depends on the server supporting the IDLE extension.")
  (environment-variables
   (list '())
   "Environment variables to set for getmail."))

(define (generate-getmail-documentation)
  (generate-documentation
   `((getmail-configuration
      ,getmail-configuration-fields
      (rcfile getmail-configuration-file))
     (getmail-configuration-file
      ,getmail-configuration-file-fields
      (retriever getmail-retriever-configuration)
      (destination getmail-destination-configuration)
      (options getmail-options-configuration))
     (getmail-retriever-configuration ,getmail-retriever-configuration-fields)
     (getmail-destination-configuration ,getmail-destination-configuration-fields)
     (getmail-options-configuration ,getmail-options-configuration-fields))
   'getmail-configuration))

(define-gexp-compiler (getmail-configuration-file-compiler
                       (rcfile <getmail-configuration-file>) system target)
  (gexp->derivation
   "getmailrc"
   #~(call-with-output-file #$output
       (lambda (port)
         (display #$(serialize-getmail-configuration-file #f rcfile)
                  port)))
   #:system system
   #:target target))

(define (getmail-accounts configs)
  (let ((users (delete-duplicates
                (map getmail-configuration-user
                     configs)))
        (groups (delete-duplicates
                 (map getmail-configuration-group
                      configs))))
    (append
     (map (lambda (group)
            (user-group
             (name group)
             (system? #t)))
          groups)
     (map (lambda (user)
            (user-account
             (name user)
             (group (getmail-configuration-group
                     (find (lambda (config)
                             (and
                              (string=? user (getmail-configuration-user config))
                              (getmail-configuration-group config)))
                           configs)))
             (system? #t)
             (comment "Getmail user")
             (home-directory "/var/empty")
             (shell (file-append shadow "/sbin/nologin"))))
          users))))

(define (getmail-activation configs)
  "Return the activation GEXP for CONFIGS."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        #$@(map
            (lambda (config)
              #~(let* ((pw (getpw #$(getmail-configuration-user config)))
                       (uid (passwd:uid pw))
                       (gid (passwd:gid pw))
                       (getmaildir #$(getmail-configuration-directory config)))
                  (mkdir-p getmaildir)
                  (chown getmaildir uid gid)))
            configs))))

(define (getmail-shepherd-services configs)
  "Return a list of <shepherd-service> for CONFIGS."
  (map (match-lambda
         (($ <getmail-configuration> location name package
                                     user group directory rcfile idle
                                     environment-variables)
          (shepherd-service
           (documentation "Run getmail.")
           (provision (list (symbol-append 'getmail- name)))
           (requirement '(networking))
           (start #~(make-forkexec-constructor
                     `(#$(file-append package "/bin/getmail")
                       ,(string-append "--getmaildir=" #$directory)
                       #$@(map (lambda (idle)
                                 (string-append "--idle=" idle))
                               idle)
                       ,(string-append "--rcfile=" #$rcfile))
                     #:user #$user
                     #:group #$group
                     #:environment-variables
                     (list #$@environment-variables)
                     #:log-file
                     #$(string-append "/var/log/getmail-"
                                      (symbol->string name)))))))
       configs))

(define getmail-service-type
  (service-type
   (name 'getmail)
   (extensions
    (list (service-extension shepherd-root-service-type
                             getmail-shepherd-services)
          (service-extension activation-service-type
                             getmail-activation)
          (service-extension account-service-type
                             getmail-accounts)))
   (description
    "Run @command{getmail}, a mail retriever program.")
   (default-value '())
   (compose concatenate)
   (extend append)))
