;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Andy Wingo <wingo@pobox.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu services cups)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages tls)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (append-map))
  #:export (cups-service-type
            cups-configuration
            opaque-cups-configuration

            files-configuration
            policy-configuration
            location-access-control
            operation-access-control
            method-access-control))

;;; Commentary:
;;;
;;; Service defininition for the CUPS printing system.
;;;
;;; Code:

(define %cups-accounts
  (list (user-group (name "lp") (system? #t))
        (user-group (name "lpadmin") (system? #t))
        (user-account
         (name "lp")
         (group "lp")
         (system? #t)
         (comment "System user for invoking printing helper programs")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (string-concatenate
     (map string-titlecase
          (string-split (if (string-suffix? "?" str)
                            (substring str 0 (1- (string-length str)))
                            str)
                        #\-)))))

(define (serialize-field field-name val)
  (format #t "~a ~a\n" (uglify-field-name field-name) val))

(define (serialize-string field-name val)
  (serialize-field field-name val))

(define (multiline-string-list? val)
  (and (list? val)
       (and-map (lambda (x)
                  (and (string? x) (not (string-index x #\space))))
                val)))
(define (serialize-multiline-string-list field-name val)
  (for-each (lambda (str) (serialize-field field-name str)) val))

(define (space-separated-string-list? val)
  (and (list? val)
       (and-map (lambda (x)
                  (and (string? x) (not (string-index x #\space))))
                val)))
(define (serialize-space-separated-string-list field-name val)
  (serialize-field field-name (string-join val " ")))

(define (space-separated-symbol-list? val)
  (and (list? val) (and-map symbol? val)))
(define (serialize-space-separated-symbol-list field-name val)
  (serialize-field field-name (string-join (map symbol->string val) " ")))

(define (file-name? val)
  (and (string? val)
       (string-prefix? "/" val)))
(define (serialize-file-name field-name val)
  (serialize-string field-name val))

(define (serialize-boolean field-name val)
  (serialize-string field-name (if val "yes" "no")))

(define (non-negative-integer? val)
  (and (exact-integer? val) (not (negative? val))))
(define (serialize-non-negative-integer field-name val)
  (serialize-field field-name val))

(define-syntax define-enumerated-field-type
  (lambda (x)
    (define (id-append ctx . parts)
      (datum->syntax ctx (apply symbol-append (map syntax->datum parts))))
    (syntax-case x ()
      ((_ name (option ...))
       #`(begin
           (define (#,(id-append #'name #'name #'?) x)
             (memq x '(option ...)))
           (define (#,(id-append #'name #'serialize- #'name) field-name val)
             (serialize-field field-name val)))))))

(define-enumerated-field-type access-log-level
  (config actions all))
(define-enumerated-field-type browse-local-protocols
  (all dnssd none))
(define-enumerated-field-type default-auth-type
  (Basic Negotiate))
(define-enumerated-field-type default-encryption
  (Never IfRequested Required))
(define-enumerated-field-type error-policy
  (abort-job retry-job retry-this-job stop-printer))
(define-enumerated-field-type log-level
  (none emerg alert crit error warn notice info debug debug2))
(define-enumerated-field-type log-time-format
  (standard usecs))
(define-enumerated-field-type server-tokens
  (None ProductOnly Major Minor Minimal OS Full))
(define-enumerated-field-type method
  (DELETE GET HEAD OPTIONS POST PUT TRACE))
(define-enumerated-field-type sandboxing
  (relaxed strict))

(define (method-list? val)
  (and (list? val) (and-map method? val)))
(define (serialize-method-list field-name val)
  (serialize-field field-name (string-join (map symbol->string val) " ")))

(define (host-name-lookups? val)
  (memq val '(#f #t 'double)))
(define (serialize-host-name-lookups field-name val)
  (serialize-field field-name
                   (match val (#f "No") (#t "Yes") ('double "Double"))))
  
(define (host-name-list-or-*? x)
    (or (eq? x '*)
        (and (list? x) (and-map string? x))))
(define (serialize-host-name-list-or-* field-name val)
  (serialize-field field-name (match val
                                ('* '*)
                                (names (string-join names " ")))))

(define (boolean-or-non-negative-integer? x)
  (or (boolean? x) (non-negative-integer? x)))
(define (serialize-boolean-or-non-negative-integer field-name x)
  (if (boolean? x)
      (serialize-boolean field-name x)
      (serialize-non-negative-integer field-name x)))

(define (ssl-options? x)
  (and (list? x)
       (and-map (lambda (elt) (memq elt '(AllowRC4 AllowSSL3))) x)))
(define (serialize-ssl-options field-name val)
  (serialize-field field-name
                   (match val
                     (() "None")
                     (opts (string-join (map symbol->string opts) " ")))))

(define (serialize-access-control x)
  (display x)
  (newline))
(define (serialize-access-control-list field-name val)
  (for-each serialize-access-control val))
(define (access-control-list? val)
  (and (list? val) (and-map string? val)))

(define-configuration operation-access-control
  (operations
   (space-separated-symbol-list '())
   "IPP operations to which this access control applies.")
  (access-controls
   (access-control-list '())
   "Access control directives, as a list of strings.  Each string should be one directive, such as \"Order allow,deny\"."))

(define-configuration method-access-control
  (reverse?
   (boolean #f)
   "If @code{#t}, apply access controls to all methods except the listed
methods.  Otherwise apply to only the listed methods.")
  (methods
   (method-list '())
   "Methods to which this access control applies.")
  (access-controls
   (access-control-list '())
   "Access control directives, as a list of strings.  Each string should be one directive, such as \"Order allow,deny\"."))

(define (serialize-operation-access-control x)
  (format #t "<Limit ~a>\n"
          (string-join (map symbol->string
                            (operation-access-control-operations x)) " "))
  (serialize-configuration
   x
   (filter (lambda (field)
             (not (eq? (configuration-field-name field) 'operations)))
           operation-access-control-fields))
  (format #t "</Limit>\n"))

(define (serialize-method-access-control x)
  (let ((limit (if (method-access-control-reverse? x) "LimitExcept" "Limit")))
    (format #t "<~a ~a>\n" limit
            (string-join (map symbol->string
                              (method-access-control-methods x)) " "))
    (serialize-configuration
     x
     (filter (lambda (field)
               (case (configuration-field-name field)
                 ((reverse? methods) #f)
                 (else #t)))
             method-access-control-fields))
    (format #t "</~a>\n" limit)))

(define (operation-access-control-list? val)
  (and (list? val) (and-map operation-access-control? val)))
(define (serialize-operation-access-control-list field-name val)
  (for-each serialize-operation-access-control val))

(define (method-access-control-list? val)
  (and (list? val) (and-map method-access-control? val)))
(define (serialize-method-access-control-list field-name val)
  (for-each serialize-method-access-control val))

(define-configuration location-access-control
  (path
   (file-name (configuration-missing-field 'location-access-control 'path))
   "Specifies the URI path to which the access control applies.")
  (access-controls
   (access-control-list '())
   "Access controls for all access to this path, in the same format as the
@code{access-controls} of @code{operation-access-control}.")
  (method-access-controls
   (method-access-control-list '())
   "Access controls for method-specific access to this path."))

(define (serialize-location-access-control x)
  (format #t "<Location ~a>\n" (location-access-control-path x))
  (serialize-configuration
   x
   (filter (lambda (field)
             (not (eq? (configuration-field-name field) 'path)))
           location-access-control-fields))
  (format #t "</Location>\n"))

(define (location-access-control-list? val)
  (and (list? val) (and-map location-access-control? val)))
(define (serialize-location-access-control-list field-name val)
  (for-each serialize-location-access-control val))

(define-configuration policy-configuration
  (name
   (string (configuration-missing-field 'policy-configuration 'name))
   "Name of the policy.")
  (job-private-access
   (string "@OWNER @SYSTEM")
   "Specifies an access list for a job's private values.  @code{@@ACL} maps to
the printer's requesting-user-name-allowed or requesting-user-name-denied
values.  @code{@@OWNER} maps to the job's owner.  @code{@@SYSTEM} maps to the
groups listed for the @code{system-group} field of the @code{files-config}
configuration, which is reified into the @code{cups-files.conf(5)} file.
Other possible elements of the access list include specific user names, and
@code{@@@var{group}} to indicate members of a specific group.  The access list
may also be simply @code{all} or @code{default}.")
  (job-private-values
   (string (string-join '("job-name" "job-originating-host-name"
                          "job-originating-user-name" "phone")))
   "Specifies the list of job values to make private, or @code{all},
@code{default}, or @code{none}.")

  (subscription-private-access
   (string "@OWNER @SYSTEM")
   "Specifies an access list for a subscription's private values.
@code{@@ACL} maps to the printer's requesting-user-name-allowed or
requesting-user-name-denied values.  @code{@@OWNER} maps to the job's owner.
@code{@@SYSTEM} maps to the groups listed for the @code{system-group} field of
the @code{files-config} configuration, which is reified into the
@code{cups-files.conf(5)} file.  Other possible elements of the access list
include specific user names, and @code{@@@var{group}} to indicate members of a
specific group.  The access list may also be simply @code{all} or
@code{default}.")
  (subscription-private-values
   (string (string-join '("notify-events" "notify-pull-method"
                          "notify-recipient-uri" "notify-subscriber-user-name"
                          "notify-user-data")
                        " "))
   "Specifies the list of job values to make private, or @code{all},
@code{default}, or @code{none}.")

  (access-controls
   (operation-access-control-list '())
   "Access control by IPP operation."))

(define (serialize-policy-configuration x)
  (format #t "<Policy ~a>\n" (policy-configuration-name x))
  (serialize-configuration
   x
   (filter (lambda (field)
             (not (eq? (configuration-field-name field) 'name)))
           policy-configuration-fields))
  (format #t "</Policy>\n"))

(define (policy-configuration-list? x)
  (and (list? x) (and-map policy-configuration? x)))
(define (serialize-policy-configuration-list field-name x)
  (for-each serialize-policy-configuration x))

(define (log-location? x)
  (or (file-name? x)
      (eq? x 'stderr)
      (eq? x 'syslog)))
(define (serialize-log-location field-name x)
  (if (string? x)
      (serialize-file-name field-name x)
      (serialize-field field-name x)))

(define-configuration files-configuration
  (access-log
   (log-location "/var/log/cups/access_log")
   "Defines the access log filename.  Specifying a blank filename disables
access log generation.  The value @code{stderr} causes log entries to be sent
to the standard error file when the scheduler is running in the foreground, or
to the system log daemon when run in the background.  The value @code{syslog}
causes log entries to be sent to the system log daemon.  The server name may
be included in filenames using the string @code{%s}, as in
@code{/var/log/cups/%s-access_log}.")
  (cache-dir
   (file-name "/var/cache/cups")
   "Where CUPS should cache data.")
  (config-file-perm
   (string "0640")
   "Specifies the permissions for all configuration files that the scheduler
writes.

Note that the permissions for the printers.conf file are currently masked to
only allow access from the scheduler user (typically root).  This is done
because printer device URIs sometimes contain sensitive authentication
information that should not be generally known on the system.  There is no way
to disable this security feature.")
  ;; Not specifying data-dir and server-bin options as we handle these
  ;; manually.  For document-root, the CUPS package has that path
  ;; preconfigured.
  (error-log
   (log-location "/var/log/cups/error_log")
   "Defines the error log filename.  Specifying a blank filename disables
access log generation.  The value @code{stderr} causes log entries to be sent
to the standard error file when the scheduler is running in the foreground, or
to the system log daemon when run in the background.  The value @code{syslog}
causes log entries to be sent to the system log daemon.  The server name may
be included in filenames using the string @code{%s}, as in
@code{/var/log/cups/%s-error_log}.")
  (fatal-errors
   (string "all -browse")
   "Specifies which errors are fatal, causing the scheduler to exit.  The kind
strings are:
@table @code
@item none
No errors are fatal.
@item all
All of the errors below are fatal.
@item browse
Browsing initialization errors are fatal, for example failed connections to
the DNS-SD daemon.
@item config
Configuration file syntax errors are fatal.
@item listen
Listen or Port errors are fatal, except for IPv6 failures on the loopback or
@code{any} addresses.
@item log
Log file creation or write errors are fatal.
@item permissions
Bad startup file permissions are fatal, for example shared TLS certificate and
key files with world-read permissions.
@end table")
  (file-device?
   (boolean #f)
   "Specifies whether the file pseudo-device can be used for new printer
queues.  The URI @url{file:///dev/null} is always allowed.")
  (group
   (string "lp")
   "Specifies the group name or ID that will be used when executing external
programs.")
  (log-file-perm
   (string "0644")
   "Specifies the permissions for all log files that the scheduler writes.")
  (page-log
   (log-location "/var/log/cups/page_log")
   "Defines the page log filename.  Specifying a blank filename disables
access log generation.  The value @code{stderr} causes log entries to be sent
to the standard error file when the scheduler is running in the foreground, or
to the system log daemon when run in the background.  The value @code{syslog}
causes log entries to be sent to the system log daemon.  The server name may
be included in filenames using the string @code{%s}, as in
@code{/var/log/cups/%s-page_log}.")
  (remote-root
   (string "remroot")
   "Specifies the username that is associated with unauthenticated accesses by
clients claiming to be the root user.  The default is @code{remroot}.")
  (request-root
   (file-name "/var/spool/cups")
   "Specifies the directory that contains print jobs and other HTTP request
data.")
  (sandboxing
   (sandboxing 'strict)
   "Specifies the level of security sandboxing that is applied to print
filters, backends, and other child processes of the scheduler; either
@code{relaxed} or @code{strict}.  This directive is currently only
used/supported on macOS.")
  (server-keychain
   (file-name "/etc/cups/ssl")
   "Specifies the location of TLS certificates and private keys.  CUPS will
look for public and private keys in this directory: a @code{.crt} files for
PEM-encoded certificates and corresponding @code{.key} files for PEM-encoded
private keys.")
  (server-root
   (file-name "/etc/cups")
   "Specifies the directory containing the server configuration files.")
  (sync-on-close?
   (boolean #f)
   "Specifies whether the scheduler calls fsync(2) after writing configuration
or state files.")
  (system-group
   (space-separated-string-list '("lpadmin" "wheel" "root"))
   "Specifies the group(s) to use for @code{@@SYSTEM} group authentication.")
  (temp-dir
   (file-name "/var/spool/cups/tmp")
   "Specifies the directory where temporary files are stored.")
  (user
   (string "lp")
   "Specifies the user name or ID that is used when running external
programs."))

(define (serialize-files-configuration field-name val)
  #f)

(define (environment-variables? vars)
  (space-separated-string-list? vars))
(define (serialize-environment-variables field-name vars)
  (unless (null? vars)
    (serialize-space-separated-string-list field-name vars)))

(define (package-list? val)
  (and (list? val) (and-map package? val)))
(define (serialize-package-list field-name val)
  #f)

(define-configuration cups-configuration
  (cups
   (package cups)
   "The CUPS package.")
  (extensions
   (package-list (list cups-filters))
   "Drivers and other extensions to the CUPS package.")
  (files-configuration
   (files-configuration (files-configuration))
   "Configuration of where to write logs, what directories to use for print
spools, and related privileged configuration parameters.")
  (access-log-level
   (access-log-level 'actions)
   "Specifies the logging level for the AccessLog file.  The @code{config}
level logs when printers and classes are added, deleted, or modified and when
configuration files are accessed or updated.  The @code{actions} level logs
when print jobs are submitted, held, released, modified, or canceled, and any
of the conditions for @code{config}.  The @code{all} level logs all
requests.")
  (auto-purge-jobs?
   (boolean #f)
   "Specifies whether to purge job history data automatically when it is no
longer required for quotas.")
  (browse-local-protocols
   (browse-local-protocols 'dnssd)
   "Specifies which protocols to use for local printer sharing.")
  (browse-web-if?
   (boolean #f)
   "Specifies whether the CUPS web interface is advertised.")
  (browsing?
   (boolean #f)
   "Specifies whether shared printers are advertised.")
  (classification
   (string "")
   "Specifies the security classification of the server.
Any valid banner name can be used, including \"classified\", \"confidential\",
\"secret\", \"topsecret\", and \"unclassified\", or the banner can be omitted
to disable secure printing functions.")
  (classify-override?
   (boolean #f)
   "Specifies whether users may override the classification (cover page) of
individual print jobs using the @code{job-sheets} option.")
  (default-auth-type
    (default-auth-type 'Basic)
    "Specifies the default type of authentication to use.")
  (default-encryption
    (default-encryption 'Required)
    "Specifies whether encryption will be used for authenticated requests.")
  (default-language
    (string "en")
    "Specifies the default language to use for text and web content.")
  (default-paper-size
    (string "Auto")
    "Specifies the default paper size for new print queues.  @samp{\"Auto\"}
uses a locale-specific default, while @samp{\"None\"} specifies there is no
default paper size.  Specific size names are typically @samp{\"Letter\"} or
@samp{\"A4\"}.")
  (default-policy
    (string "default")
    "Specifies the default access policy to use.")
  (default-shared?
    (boolean #t)
    "Specifies whether local printers are shared by default.")
  (dirty-clean-interval
   (non-negative-integer 30)
   "Specifies the delay for updating of configuration and state files, in
seconds.  A value of 0 causes the update to happen as soon as possible,
typically within a few milliseconds.")
  (error-policy
   (error-policy 'stop-printer)
   "Specifies what to do when an error occurs.  Possible values are
@code{abort-job}, which will discard the failed print job; @code{retry-job},
which will retry the job at a later time; @code{retry-this-job}, which retries
the failed job immediately; and @code{stop-printer}, which stops the
printer.")
  (filter-limit
   (non-negative-integer 0)
   "Specifies the maximum cost of filters that are run concurrently, which can
be used to minimize disk, memory, and CPU resource problems.  A limit of 0
disables filter limiting.  An average print to a non-PostScript printer needs
a filter limit of about 200.  A PostScript printer needs about half
that (100).  Setting the limit below these thresholds will effectively limit
the scheduler to printing a single job at any time.")
  (filter-nice
   (non-negative-integer 0)
   "Specifies the scheduling priority of filters that are run to print a job.
The nice value ranges from 0, the highest priority, to 19, the lowest
priority.")
  ;; Add this option if the package is built with Kerberos support.
  ;; (gss-service-name
  ;;  (string "http")
  ;;  "Specifies the service name when using Kerberos authentication.")
  (host-name-lookups
   (host-name-lookups #f)
   "Specifies whether to do reverse lookups on connecting clients.
The @code{double} setting causes @code{cupsd} to verify that the hostname
resolved from the address matches one of the addresses returned for that
hostname.  Double lookups also prevent clients with unregistered addresses
from connecting to your server.  Only set this option to @code{#t} or
@code{double} if absolutely required.")
  ;; Add this option if the package is built with launchd/systemd support.
  ;;   (idle-exit-timeout
  ;;    (non-negative-integer 60)
  ;;    "Specifies the length of time to wait before shutting down due to
  ;; inactivity.  Note: Only applicable when @code{cupsd} is run on-demand
  ;; (e.g., with @code{-l}).")
  (job-kill-delay
   (non-negative-integer 30)
   "Specifies the number of seconds to wait before killing the filters and
backend associated with a canceled or held job.")
  (job-retry-interval
   (non-negative-integer 30)
   "Specifies the interval between retries of jobs in seconds.  This is
typically used for fax queues but can also be used with normal print queues
whose error policy is @code{retry-job} or @code{retry-current-job}.")
  (job-retry-limit
   (non-negative-integer 5)
   "Specifies the number of retries that are done for jobs.  This is typically
used for fax queues but can also be used with normal print queues whose error
policy is @code{retry-job} or @code{retry-current-job}.")
  (keep-alive?
   (boolean #t)
   "Specifies whether to support HTTP keep-alive connections.")
  (keep-alive-timeout
   (non-negative-integer 30)
   "Specifies how long an idle client connection remains open, in seconds.")
  (limit-request-body
   (non-negative-integer 0)
   "Specifies the maximum size of print files, IPP requests, and HTML form
data.  A limit of 0 disables the limit check.")
  (listen
   (multiline-string-list '("localhost:631" "/var/run/cups/cups.sock"))
   "Listens on the specified interfaces for connections.  Valid values are of
the form @var{address}:@var{port}, where @var{address} is either an IPv6
address enclosed in brackets, an IPv4 address, or @code{*} to indicate all
addresses.  Values can also be file names of local UNIX domain sockets.  The
Listen directive is similar to the Port directive but allows you to restrict
access to specific interfaces or networks.")
  (listen-back-log
   (non-negative-integer 128)
   "Specifies the number of pending connections that will be allowed.  This
normally only affects very busy servers that have reached the MaxClients
limit, but can also be triggered by large numbers of simultaneous connections.
When the limit is reached, the operating system will refuse additional
connections until the scheduler can accept the pending ones.")
  (location-access-controls
   (location-access-control-list
    (list (location-access-control
           (path "/")
           (access-controls '("Order allow,deny"
                              "Allow localhost")))
          (location-access-control
           (path "/admin")
           (access-controls '("Order allow,deny"
                              "Allow localhost")))
          (location-access-control
           (path "/admin/conf")
           (access-controls '("Order allow,deny"
                              "AuthType Basic"
                              "Require user @SYSTEM"
                              "Allow localhost")))))
   "Specifies a set of additional access controls.")
  (log-debug-history
   (non-negative-integer 100)
   "Specifies the number of debugging messages that are retained for logging
if an error occurs in a print job.  Debug messages are logged regardless of
the LogLevel setting.")
  (log-level
   (log-level 'info)
   "Specifies the level of logging for the ErrorLog file.  The value
@code{none} stops all logging while @code{debug2} logs everything.")
  (log-time-format
   (log-time-format 'standard)
   "Specifies the format of the date and time in the log files.  The value
@code{standard} logs whole seconds while @code{usecs} logs microseconds.")
  (max-clients
   (non-negative-integer 100)
   "Specifies the maximum number of simultaneous clients that are allowed by
the scheduler.")
  (max-clients-per-host
   (non-negative-integer 100)
   "Specifies the maximum number of simultaneous clients that are allowed from
a single address.")
  (max-copies
   (non-negative-integer 9999)
   "Specifies the maximum number of copies that a user can print of each
job.")
  (max-hold-time
   (non-negative-integer 0)
   "Specifies the maximum time a job may remain in the @code{indefinite} hold
state before it is canceled.  A value of 0 disables cancellation of held
jobs.")
  (max-jobs
   (non-negative-integer 500)
   "Specifies the maximum number of simultaneous jobs that are allowed.  Set
to 0 to allow an unlimited number of jobs.")
  (max-jobs-per-printer
   (non-negative-integer 0)
   "Specifies the maximum number of simultaneous jobs that are allowed per
printer.  A value of 0 allows up to MaxJobs jobs per printer.")
  (max-jobs-per-user
   (non-negative-integer 0)
   "Specifies the maximum number of simultaneous jobs that are allowed per
user.  A value of 0 allows up to MaxJobs jobs per user.")
  (max-job-time
   (non-negative-integer 10800)
   "Specifies the maximum time a job may take to print before it is canceled,
in seconds.  Set to 0 to disable cancellation of \"stuck\" jobs.")
  (max-log-size
   (non-negative-integer 1048576)
   "Specifies the maximum size of the log files before they are rotated, in
bytes.  The value 0 disables log rotation.")
  (multiple-operation-timeout
   (non-negative-integer 300)
   "Specifies the maximum amount of time to allow between files in a multiple
file print job, in seconds.")
  (page-log-format
   (string "")
   "Specifies the format of PageLog lines.  Sequences beginning with
percent (@samp{%}) characters are replaced with the corresponding information,
while all other characters are copied literally.  The following percent
sequences are recognized:

@table @samp
@item %%
insert a single percent character
@item %@{name@}
insert the value of the specified IPP attribute
@item %C
insert the number of copies for the current page
@item %P
insert the current page number
@item %T
insert the current date and time in common log format
@item %j
insert the job ID
@item %p
insert the printer name
@item %u
insert the username
@end table

A value of the empty string disables page logging.  The string @code{%p %u %j
%T %P %C %@{job-billing@} %@{job-originating-host-name@} %@{job-name@}
%@{media@} %@{sides@}} creates a page log with the standard items.")
  (environment-variables
   (environment-variables '())
   "Passes the specified environment variable(s) to child processes; a list of
strings.")
  (policies
   (policy-configuration-list
    (list (policy-configuration
           (name "default")
           (access-controls
            (list
             (operation-access-control
              (operations
               '(Send-Document
                 Send-URI Hold-Job Release-Job Restart-Job Purge-Jobs
                 Cancel-Job Close-Job Cancel-My-Jobs Set-Job-Attributes
                 Create-Job-Subscription Renew-Subscription
                 Cancel-Subscription Get-Notifications
                 Reprocess-Job Cancel-Current-Job Suspend-Current-Job
                 Resume-Job CUPS-Move-Job Validate-Job
                 CUPS-Get-Document))
              (access-controls '("Require user @OWNER @SYSTEM"
                                 "Order deny,allow")))
             (operation-access-control
              (operations
               '(Pause-Printer
                 Cancel-Jobs
                 Resume-Printer Set-Printer-Attributes Enable-Printer
                 Disable-Printer Pause-Printer-After-Current-Job
                 Hold-New-Jobs Release-Held-New-Jobs Deactivate-Printer
                 Activate-Printer Restart-Printer Shutdown-Printer
                 Startup-Printer Promote-Job Schedule-Job-After
                 CUPS-Authenticate-Job CUPS-Add-Printer
                 CUPS-Delete-Printer CUPS-Add-Class CUPS-Delete-Class
                 CUPS-Accept-Jobs CUPS-Reject-Jobs CUPS-Set-Default))
              (access-controls '("AuthType Basic"
                                 "Require user @SYSTEM"
                                 "Order deny,allow")))
             (operation-access-control
              (operations '(All))
              (access-controls '("Order deny,allow"))))))))
   "Specifies named access control policies.")
  #;
  (port
   (non-negative-integer 631)
   "Listens to the specified port number for connections.")
  (preserve-job-files
   (boolean-or-non-negative-integer 86400)
   "Specifies whether job files (documents) are preserved after a job is
printed.  If a numeric value is specified, job files are preserved for the
indicated number of seconds after printing.  Otherwise a boolean value applies
indefinitely.")
  (preserve-job-history
   (boolean-or-non-negative-integer #t)
   "Specifies whether the job history is preserved after a job is printed.
If a numeric value is specified, the job history is preserved for the
indicated number of seconds after printing.  If @code{#t}, the job history is
preserved until the MaxJobs limit is reached.")
  (reload-timeout
   (non-negative-integer 30)
   "Specifies the amount of time to wait for job completion before restarting
the scheduler.")
  (rip-cache
   (string "128m")
   "Specifies the maximum amount of memory to use when converting documents into bitmaps for a printer.")
  (server-admin
   (string "root@localhost.localdomain")
   "Specifies the email address of the server administrator.")
  (server-alias
   (host-name-list-or-* '*)
   "The ServerAlias directive is used for HTTP Host header validation when
clients connect to the scheduler from external interfaces.  Using the special
name @code{*} can expose your system to known browser-based DNS rebinding
attacks, even when accessing sites through a firewall.  If the auto-discovery
of alternate names does not work, we recommend listing each alternate name
with a ServerAlias directive instead of using @code{*}.")
  (server-name
   (string "localhost")
   "Specifies the fully-qualified host name of the server.")
  (server-tokens
   (server-tokens 'Minimal)
   "Specifies what information is included in the Server header of HTTP
responses.  @code{None} disables the Server header.  @code{ProductOnly}
reports @code{CUPS}.  @code{Major} reports @code{CUPS 2}.  @code{Minor}
reports @code{CUPS 2.0}.  @code{Minimal} reports @code{CUPS 2.0.0}.  @code{OS}
reports @code{CUPS 2.0.0 (@var{uname})} where @var{uname} is the output of the
@code{uname} command.  @code{Full} reports @code{CUPS 2.0.0 (@var{uname})
IPP/2.0}.")
  (set-env
   (string "variable value")
   "Set the specified environment variable to be passed to child processes.")
  (ssl-listen
   (multiline-string-list '())
   "Listens on the specified interfaces for encrypted connections.  Valid
values are of the form @var{address}:@var{port}, where @var{address} is either
an IPv6 address enclosed in brackets, an IPv4 address, or @code{*} to indicate
all addresses.")
  (ssl-options
   (ssl-options '())
   "Sets encryption options.
By default, CUPS only supports encryption using TLS v1.0 or higher using known
secure cipher suites.  The @code{AllowRC4} option enables the 128-bit RC4
cipher suites, which are required for some older clients that do not implement
newer ones.  The @code{AllowSSL3} option enables SSL v3.0, which is required
for some older clients that do not support TLS v1.0.")
  #;
  (ssl-port
   (non-negative-integer 631)
   "Listens on the specified port for encrypted connections.")
  (strict-conformance?
   (boolean #f)
   "Specifies whether the scheduler requires clients to strictly adhere to the
IPP specifications.")
  (timeout
   (non-negative-integer 300)
   "Specifies the HTTP request timeout, in seconds.")
  (web-interface?
   (boolean #f)
   "Specifies whether the web interface is enabled."))

(define-configuration opaque-cups-configuration
  (cups
   (package cups)
   "The CUPS package.")
  (extensions
   (package-list '())
   "Drivers and other extensions to the CUPS package.")
  (cupsd.conf
   (string (configuration-missing-field 'opaque-cups-configuration
                                        'cupsd.conf))
   "The contents of the @code{cupsd.conf} to use.")
  (cups-files.conf
   (string (configuration-missing-field 'opaque-cups-configuration
                                        'cups-files.conf))
   "The contents of the @code{cups-files.conf} to use."))

(define %cups-activation
  ;; Activation gexp.
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (define (mkdir-p/perms directory owner perms)
          (mkdir-p directory)
          (chown "/var/run/cups" (passwd:uid owner) (passwd:gid owner))
          (chmod directory perms))
        (define (build-subject parameters)
          (string-concatenate
           (map (lambda (pair)
                  (let ((k (car pair)) (v (cdr pair)))
                    (define (escape-char str chr)
                      (string-join (string-split str chr) (string #\\ chr)))
                    (string-append "/" k "="
                                   (escape-char (escape-char v #\=) #\/))))
                (filter (lambda (pair) (cdr pair)) parameters))))
        (define* (create-self-signed-certificate-if-absent
                  #:key private-key public-key (owner (getpwnam "root"))
                  (common-name (gethostname))
                  (organization-name "Guix")
                  (organization-unit-name "Default Self-Signed Certificate")
                  (subject-parameters `(("CN" . ,common-name)
                                        ("O" . ,organization-name)
                                        ("OU" . ,organization-unit-name)))
                  (subject (build-subject subject-parameters)))
          ;; Note that by default, OpenSSL outputs keys in PEM format.  This
          ;; is what we want.
          (unless (file-exists? private-key)
            (cond
             ((zero? (system* (string-append #$openssl "/bin/openssl")
                              "genrsa" "-out" private-key "2048"))
              (chown private-key (passwd:uid owner) (passwd:gid owner))
              (chmod private-key #o400))
             (else
              (format (current-error-port)
                      "Failed to create private key at ~a.\n" private-key))))
          (unless (file-exists? public-key)
            (cond
             ((zero? (system* (string-append #$openssl "/bin/openssl")
                              "req" "-new" "-x509" "-key" private-key
                              "-out" public-key "-days" "3650"
                              "-batch" "-subj" subject))
              (chown public-key (passwd:uid owner) (passwd:gid owner))
              (chmod public-key #o444))
             (else
              (format (current-error-port)
                      "Failed to create public key at ~a.\n" public-key)))))
        (let ((user (getpwnam "lp")))
          (mkdir-p/perms "/var/run/cups" user #o755)
          (mkdir-p/perms "/var/spool/cups" user #o755)
          (mkdir-p/perms "/var/spool/cups/tmp" user #o755)
          (mkdir-p/perms "/var/log/cups" user #o755)
          (mkdir-p/perms "/etc/cups" user #o755)
          (mkdir-p/perms "/etc/cups/ssl" user #o700)
          ;; This certificate is used for HTTPS connections to the CUPS web
          ;; interface.
          (create-self-signed-certificate-if-absent
           #:private-key "/etc/cups/ssl/localhost.key"
           #:public-key "/etc/cups/ssl/localhost.crt"
           #:owner (getpwnam "root")
           #:common-name (format #f "CUPS service on ~a" (gethostname)))))))

(define (union-directory name packages paths)
  (computed-file
   name
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils)
                      (srfi srfi-1))
         (mkdir #$output)
         (for-each
          (lambda (package)
            (for-each
             (lambda (path)
               (for-each
                (lambda (src)
                  (let* ((tail (substring src (string-length package)))
                         (dst (string-append #$output tail)))
                    (mkdir-p (dirname dst))
                    ;; CUPS currently symlinks in some data from cups-filters
                    ;; to its output dir.  Probably we should stop doing this
                    ;; and instead rely only on the CUPS service to union the
                    ;; relevant set of CUPS packages.
                    (if (file-exists? dst)
                        (format (current-error-port) "warning: ~a exists\n" dst)
                        (symlink src dst))))
                (find-files (string-append package path) #:stat stat)))
             (list #$@paths)))
          (list #$@packages))
         #t))))

(define (cups-server-bin-directory extensions)
  "Return the CUPS ServerBin directory, containing binaries for CUPS and all
extensions that it uses."
  (union-directory "cups-server-bin" extensions
                   ;; /bin
                   '("/lib/cups" "/share/ppd" "/share/cups")))

(define (cups-shepherd-service config)
  "Return a list of <shepherd-service> for CONFIG."
  (let* ((cupsd.conf-str
          (cond
           ((opaque-cups-configuration? config)
            (opaque-cups-configuration-cupsd.conf config))
           (else
            (with-output-to-string
              (lambda ()
                (serialize-configuration config
                                         cups-configuration-fields))))))
         (cups-files.conf-str
          (cond
           ((opaque-cups-configuration? config)
            (opaque-cups-configuration-cups-files.conf config))
           (else
            (with-output-to-string
              (lambda ()
                (serialize-configuration
                 (cups-configuration-files-configuration config)
                 files-configuration-fields))))))
         (cups (if (opaque-cups-configuration? config)
                   (opaque-cups-configuration-cups config)
                   (cups-configuration-cups config)))
         (server-bin
          (cups-server-bin-directory
           (cons cups
                 (cond
                  ((opaque-cups-configuration? config)
                   (opaque-cups-configuration-extensions config))
                  (else
                   (cups-configuration-extensions config))))))
         ;;"SetEnv PATH " server-bin "/bin" "\n"
         (cupsd.conf
          (plain-file "cupsd.conf" cupsd.conf-str))
         (cups-files.conf
          (mixed-text-file
           "cups-files.conf"
           cups-files.conf-str
           "CacheDir /var/cache/cups\n"
           "StateDir /var/run/cups\n"
           "DataDir " server-bin "/share/cups" "\n"
           "ServerBin " server-bin "/lib/cups" "\n")))
    (list (shepherd-service
           (documentation "Run the CUPS print server.")
           (provision '(cups))
           (requirement '(networking))
           (start #~(make-forkexec-constructor
                     (list (string-append #$cups "/sbin/cupsd")
                           "-f" "-c" #$cupsd.conf "-s" #$cups-files.conf)))
           (stop #~(make-kill-destructor))))))

(define cups-service-type
  (service-type (name 'cups)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          cups-shepherd-service)
                       (service-extension activation-service-type
                                          (const %cups-activation))
                       (service-extension account-service-type
                                          (const %cups-accounts))))

                ;; Extensions consist of lists of packages (representing CUPS
                ;; drivers, etc) that we just concatenate.
                (compose append)

                ;; Add extension packages by augmenting the cups-configuration
                ;; 'extensions' field.
                (extend
                 (lambda (config extensions)
                   (cond
                    ((cups-configuration? config)
                     (cups-configuration
                      (inherit config)
                      (extensions
                       (append (cups-configuration-extensions config)
                               extensions))))
                    (else
                     (opaque-cups-configuration
                      (inherit config)
                      (extensions
                       (append (opaque-cups-configuration-extensions config)
                               extensions)))))))

                (default-value (cups-configuration))
                (description
                 "Run the CUPS print server.")))

;; A little helper to make it easier to document all those fields.
(define (generate-cups-documentation)
  (generate-documentation
    `((cups-configuration
       ,cups-configuration-fields
       (files-configuration files-configuration)
       (policies policy-configuration)
       (location-access-controls location-access-controls))
      (files-configuration ,files-configuration-fields)
      (policy-configuration
       ,policy-configuration-fields
       (operation-access-controls operation-access-controls))
      (location-access-controls
       ,location-access-control-fields
       (method-access-controls method-access-controls))
      (operation-access-controls ,operation-access-control-fields)
      (method-access-controls ,method-access-control-fields))
    'cups-configuration))
