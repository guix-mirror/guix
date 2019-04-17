;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu services authentication)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages openldap)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (fprintd-configuration
            fprintd-configuration?
            fprintd-service-type

            nslcd-configuration
            nslcd-configuration?
            nslcd-service-type))

(define-configuration fprintd-configuration
  (fprintd      (package fprintd)
                "The fprintd package"))

(define (fprintd-dbus-service config)
  (list (fprintd-configuration-fprintd config)))

(define fprintd-service-type
  (service-type (name 'fprintd)
                (extensions
                 (list (service-extension dbus-root-service-type
                                          fprintd-dbus-service)))
                (default-value (fprintd-configuration))
                (description
                 "Run fprintd, a fingerprint management daemon.")))


;;;
;;; NSS Pam LDAP service (nslcd)
;;;

(define (uglify-field-name name)
  (match name
    ('filters "filter")
    ('maps "map")
    (_ (string-map (match-lambda
                     (#\- #\_)
                     (chr chr))
                   (symbol->string name)))))

(define (value->string val)
  (cond
   ((boolean? val)
    (if val "on" "off"))
   ((number? val)
    (number->string val))
   ((symbol? val)
    (string-map (match-lambda
                     (#\- #\_)
                     (chr chr))
                   (symbol->string val)))
   (else val)))

(define (serialize-field field-name val)
  (if (eq? field-name 'pam-services)
      #t
      (format #t "~a ~a\n"
              (uglify-field-name field-name)
              (value->string val))))

(define serialize-string serialize-field)
(define serialize-boolean serialize-field)
(define serialize-number serialize-field)
(define (serialize-list field-name val)
  (map (cut serialize-field field-name <>) val))
(define-maybe string)
(define-maybe boolean)
(define-maybe number)

(define (ssl-option? val)
  (or (boolean? val)
      (eq? val 'start-tls)))
(define serialize-ssl-option serialize-field)
(define-maybe ssl-option)

(define (tls-reqcert-option? val)
  (member val '(never allow try demand hard)))
(define serialize-tls-reqcert-option serialize-field)
(define-maybe tls-reqcert-option)

(define (deref-option? val)
  (member val '(never searching finding always)))
(define serialize-deref-option serialize-field)
(define-maybe deref-option)

(define (comma-separated-list-of-strings? val)
  (and (list? val)
       (every string? val)))
(define (ignore-users-option? val)
  (or (comma-separated-list-of-strings? val)
      (eq? 'all-local val)))
(define (serialize-ignore-users-option field-name val)
  (serialize-field field-name (if (eq? 'all-local val)
                                  val
                                  (string-join val ","))))
(define-maybe ignore-users-option)

(define (log-option? val)
  (let ((valid-scheme? (lambda (scheme)
                         (or (string? scheme)
                             (member scheme '(none syslog))))))
    (match val
      ((scheme level)
       (and (valid-scheme? scheme)
            (member level '(crit error warning notice info debug))))
      ((scheme)
       (valid-scheme? scheme)))))
(define (serialize-log-option field-name val)
  (serialize-field field-name
                   (string-join (map (cut format #f "~a" <>) val))))

(define (valid-map? val)
  "Is VAL a supported map name?"
  (member val
          '(alias aliases ether ethers group host hosts netgroup network networks
            passwd protocol protocols rpc service services shadow)))

(define (scope-option? val)
  (let ((valid-scopes '(subtree onelevel base children)))
    (match val
      ((map-name scope)
       (and (valid-map? map-name)
            (member scope valid-scopes)))
      ((scope)
       (member scope valid-scopes)))))
(define (serialize-scope-option field-name val)
  (serialize-field field-name
                   (string-join (map (cut format #f "~a" <>) val))))

(define (map-entry? val)
  (match val
    (((? valid-map? map-name)
      (? string? attribute)
      (? string? new-attribute)) #t)
    (_ #f)))

(define (list-of-map-entries? val)
  (and (list? val)
       (every map-entry? val)))

(define (filter-entry? val)
  (match val
    (((? valid-map? map-name)
      (? string? filter-expression)) #t)
    (_ #f)))

(define (list-of-filter-entries? val)
  (and (list? val)
       (every filter-entry? val)))

(define (serialize-filter-entry field-name val)
  (serialize-field 'filter
                   (match val
                     (((? valid-map? map-name)
                       (? string? filter-expression))
                      (string-append (symbol->string map-name)
                                     " " filter-expression)))))

(define (serialize-list-of-filter-entries field-name val)
  (for-each (cut serialize-filter-entry field-name <>) val))

(define (serialize-map-entry field-name val)
  (serialize-field 'map
                   (match val
                     (((? valid-map? map-name)
                       (? string? attribute)
                       (? string? new-attribute))
                      (string-append (symbol->string map-name)
                                     " " attribute
                                     " " new-attribute)))))

(define (serialize-list-of-map-entries field-name val)
  (for-each (cut serialize-map-entry field-name <>) val))


(define-configuration nslcd-configuration
  (nss-pam-ldapd
   (package nss-pam-ldapd)
   "The NSS-PAM-LDAPD package to use.")

  ;; Runtime options
  (threads
   (maybe-number 'disabled)
   "The number of threads to start that can handle requests and perform LDAP
queries.  Each thread opens a separate connection to the LDAP server.  The
default is to start 5 threads.")
  (uid
   (string "nslcd")
   "This specifies the user id with which the daemon should be run.")
  (gid
   (string "nslcd")
   "This specifies the group id with which the daemon should be run.")
  (log
   (log-option '("/var/log/nslcd" info))
   "This option controls the way logging is done via a list containing SCHEME
and LEVEL.  The SCHEME argument may either be the symbols \"none\" or
\"syslog\", or an absolute file name.  The LEVEL argument is optional and
specifies the log level.  The log level may be one of the following symbols:
\"crit\", \"error\", \"warning\", \"notice\", \"info\" or \"debug\".  All
messages with the specified log level or higher are logged.")

  ;; LDAP connection settings
  (uri
   (list '("ldap://localhost:389/"))
   "The list of LDAP server URIs.  Normally, only the first server will be
used with the following servers as fall-back.")
  (ldap-version
   (maybe-string 'disabled)
   "The version of the LDAP protocol to use.  The default is to use the
maximum version supported by the LDAP library.")
  (binddn
   (maybe-string 'disabled)
   "Specifies the distinguished name with which to bind to the directory
server for lookups.  The default is to bind anonymously.")
  (bindpw
   (maybe-string 'disabled)
   "Specifies the credentials with which to bind.  This option is only
applicable when used with binddn.")
  (rootpwmoddn
   (maybe-string 'disabled)
   "Specifies the distinguished name to use when the root user tries to modify
a user's password using the PAM module.")
  (rootpwmodpw
   (maybe-string 'disabled)
   "Specifies the credentials with which to bind if the root user tries to
change a user's password.  This option is only applicable when used with
rootpwmoddn")

  ;; SASL authentication options
  (sasl-mech
   (maybe-string 'disabled)
   "Specifies the SASL mechanism to be used when performing SASL
authentication.")
  (sasl-realm
   (maybe-string 'disabled)
   "Specifies the SASL realm to be used when performing SASL authentication.")
  (sasl-authcid
   (maybe-string 'disabled)
   "Specifies the authentication identity to be used when performing SASL
authentication.")
  (sasl-authzid
   (maybe-string 'disabled)
   "Specifies the authorization identity to be used when performing SASL
authentication.")
  (sasl-canonicalize?
   (maybe-boolean 'disabled)
   "Determines whether the LDAP server host name should be canonicalised.  If
this is enabled the LDAP library will do a reverse host name lookup.  By
default, it is left up to the LDAP library whether this check is performed or
not.")

  ;; Kerberos authentication options
  (krb5-ccname
   (maybe-string 'disabled)
   "Set the name for the GSS-API Kerberos credentials cache.")

  ;; Search / mapping options
  (base
   (string "dc=example,dc=com")
   "The directory search base.")
  (scope
   (scope-option '(subtree))
   "Specifies the search scope (subtree, onelevel, base or children).  The
default scope is subtree; base scope is almost never useful for name service
lookups; children scope is not supported on all servers.")
  (deref
   (maybe-deref-option 'disabled)
   "Specifies the policy for dereferencing aliases.  The default policy is to
never dereference aliases.")
  (referrals
   (maybe-boolean 'disabled)
   "Specifies whether automatic referral chasing should be enabled.  The
default behaviour is to chase referrals.")
  (maps
   (list-of-map-entries '())
   "This option allows for custom attributes to be looked up instead of the
default RFC 2307 attributes.  It is a list of maps, each consisting of the
name of a map, the RFC 2307 attribute to match and the query expression for
the attribute as it is available in the directory.")
  (filters
   (list-of-filter-entries '())
   "A list of filters consisting of the name of a map to which the filter
applies and an LDAP search filter expression.")

  ;; Timing / reconnect options
  (bind-timelimit
   (maybe-number 'disabled)
   "Specifies the time limit in seconds to use when connecting to the
directory server.  The default value is 10 seconds.")
  (timelimit
   (maybe-number 'disabled)
   "Specifies the time limit (in seconds) to wait for a response from the LDAP
server.  A value of zero, which is the default, is to wait indefinitely for
searches to be completed.")
  (idle-timelimit
   (maybe-number 'disabled)
   "Specifies the period if inactivity (in seconds) after which the con‐
nection to the LDAP server will be closed.  The default is not to time out
connections.")
  (reconnect-sleeptime
   (maybe-number 'disabled)
   "Specifies the number of seconds to sleep when connecting to all LDAP
servers fails.  By default one second is waited between the first failure and
the first retry.")
  (reconnect-retrytime
   (maybe-number 'disabled)
   "Specifies the time after which the LDAP server is considered to be
permanently unavailable.  Once this time is reached retries will be done only
once per this time period.  The default value is 10 seconds.")

  ;; TLS options
  (ssl
   (maybe-ssl-option 'disabled)
   "Specifies whether to use SSL/TLS or not (the default is not to).  If
'start-tls is specified then StartTLS is used rather than raw LDAP over SSL.")
  (tls-reqcert
   (maybe-tls-reqcert-option 'disabled)
   "Specifies what checks to perform on a server-supplied certificate.
The meaning of the values is described in the ldap.conf(5) manual page.")
  (tls-cacertdir
   (maybe-string 'disabled)
   "Specifies the directory containing X.509 certificates for peer authen‐
tication.  This parameter is ignored when using GnuTLS.")
  (tls-cacertfile
   (maybe-string 'disabled)
   "Specifies the path to the X.509 certificate for peer authentication.")
  (tls-randfile
   (maybe-string 'disabled)
   "Specifies the path to an entropy source.  This parameter is ignored when
using GnuTLS.")
  (tls-ciphers
   (maybe-string 'disabled)
   "Specifies the ciphers to use for TLS as a string.")
  (tls-cert
   (maybe-string 'disabled)
   "Specifies the path to the file containing the local certificate for client
TLS authentication.")
  (tls-key
   (maybe-string 'disabled)
   "Specifies the path to the file containing the private key for client TLS
authentication.")

  ;; Other options
  (pagesize
   (maybe-number 'disabled)
   "Set this to a number greater than 0 to request paged results from the LDAP
server in accordance with RFC2696.  The default (0) is to not request paged
results.")
  (nss-initgroups-ignoreusers
   (maybe-ignore-users-option 'disabled)
   "This option prevents group membership lookups through LDAP for the
specified users.  Alternatively, the value 'all-local may be used.  With that
value nslcd builds a full list of non-LDAP users on startup.")
  (nss-min-uid
   (maybe-number 'disabled)
   "This option ensures that LDAP users with a numeric user id lower than the
specified value are ignored.")
  (nss-uid-offset
   (maybe-number 'disabled)
   "This option specifies an offset that is added to all LDAP numeric user
ids.  This can be used to avoid user id collisions with local users.")
  (nss-gid-offset
   (maybe-number 'disabled)
   "This option specifies an offset that is added to all LDAP numeric group
ids.  This can be used to avoid user id collisions with local groups.")
  (nss-nested-groups
   (maybe-boolean 'disabled)
   "If this option is set, the member attribute of a group may point to
another group.  Members of nested groups are also returned in the higher level
group and parent groups are returned when finding groups for a specific user.
The default is not to perform extra searches for nested groups.")
  (nss-getgrent-skipmembers
   (maybe-boolean 'disabled)
   "If this option is set, the group member list is not retrieved when looking
up groups.  Lookups for finding which groups a user belongs to will remain
functional so the user will likely still get the correct groups assigned on
login.")
  (nss-disable-enumeration
   (maybe-boolean 'disabled)
   "If this option is set, functions which cause all user/group entries to be
loaded from the directory will not succeed in doing so.  This can dramatically
reduce LDAP server load in situations where there are a great number of users
and/or groups.  This option is not recommended for most configurations.")
  (validnames
   (maybe-string 'disabled)
   "This option can be used to specify how user and group names are verified
within the system.  This pattern is used to check all user and group names
that are requested and returned from LDAP.")
  (ignorecase
   (maybe-boolean 'disabled)
   "This specifies whether or not to perform searches using case-insensitive
matching.  Enabling this could open up the system to authorization bypass
vulnerabilities and introduce nscd cache poisoning vulnerabilities which allow
denial of service.")
  (pam-authc-ppolicy
   (maybe-boolean 'disabled)
   "This option specifies whether password policy controls are requested and
handled from the LDAP server when performing user authentication.")
  (pam-authc-search
   (maybe-string 'disabled)
   "By default nslcd performs an LDAP search with the user's credentials after
BIND (authentication) to ensure that the BIND operation was successful.  The
default search is a simple check to see if the user's DN exists.  A search
filter can be specified that will be used instead.  It should return at least
one entry.")
  (pam-authz-search
   (maybe-string 'disabled)
   "This option allows flexible fine tuning of the authorisation check that
should be performed.  The search filter specified is executed and if any
entries match, access is granted, otherwise access is denied.")
  (pam-password-prohibit-message
   (maybe-string 'disabled)
   "If this option is set password modification using pam_ldap will be denied
and the specified message will be presented to the user instead.  The message
can be used to direct the user to an alternative means of changing their
password.")

  ;; Options for extension of pam-root-service-type.
  (pam-services
   (list '())
   "List of pam service names for which LDAP authentication should suffice."))

(define %nslcd-accounts
  (list (user-group
         (name "nslcd")
         (system? #t))
        (user-account
         (name "nslcd")
         (group "nslcd")
         (comment "NSLCD service account")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin"))
         (system? #t))))

(define (nslcd-config-file config)
  "Return an NSLCD configuration file."
  (plain-file "nslcd.conf"
              (with-output-to-string
                (lambda ()
                  (serialize-configuration config nslcd-configuration-fields)
                  ;; The file must end with a newline character.
                  (format #t "\n")))))

;; XXX: The file should only be readable by root if it contains a "bindpw"
;; declaration.  Unfortunately, this etc-service-type extension does not
;; support setting file modes, so we do this in the activation service.
(define (nslcd-etc-service config)
  `(("nslcd.conf" ,(nslcd-config-file config))))

(define (nslcd-shepherd-service config)
  (list (shepherd-service
         (documentation "Run the nslcd service for resolving names from LDAP.")
         (provision '(nslcd))
         (requirement '(networking user-processes))
         (start #~(make-forkexec-constructor
                   (list (string-append #$(nslcd-configuration-nss-pam-ldapd config)
                                        "/sbin/nslcd")
                         "--nofork")
                   #:pid-file "/var/run/nslcd/nslcd.pid"
                   #:environment-variables
                   (list (string-append "LD_LIBRARY_PATH="
                                        #$(nslcd-configuration-nss-pam-ldapd config)
                                        "/lib"))))
         (stop #~(make-kill-destructor)))))

(define (pam-ldap-pam-service config)
  "Return a PAM service for LDAP authentication."
  (define pam-ldap-module
    #~(string-append #$(nslcd-configuration-nss-pam-ldapd config)
                     "/lib/security/pam_ldap.so"))
  (lambda (pam)
    (if (member (pam-service-name pam)
                (nslcd-configuration-pam-services config))
        (let ((sufficient
               (pam-entry
                (control "sufficient")
                (module pam-ldap-module))))
          (pam-service
           (inherit pam)
           (auth (cons sufficient (pam-service-auth pam)))
           (session (cons sufficient (pam-service-session pam)))
           (account (cons sufficient (pam-service-account pam)))))
        pam)))

(define (pam-ldap-pam-services config)
  (list (pam-ldap-pam-service config)))

(define nslcd-service-type
  (service-type
   (name 'nslcd)
   (description "Run the NSLCD service for looking up names from LDAP.")
   (extensions
    (list (service-extension account-service-type
                             (const %nslcd-accounts))
          (service-extension etc-service-type
                             nslcd-etc-service)
          (service-extension activation-service-type
                             (const #~(begin
                                        (use-modules (guix build utils))
                                        (let ((rundir "/var/run/nslcd")
                                              (user (getpwnam "nslcd")))
                                          (mkdir-p rundir)
                                          (chown rundir (passwd:uid user) (passwd:gid user))
                                          (chmod rundir #o755)
                                          (when (file-exists? "/etc/nslcd.conf")
                                            (chmod "/etc/nslcd.conf" #o400))))))
          (service-extension pam-root-service-type
                             pam-ldap-pam-services)
          (service-extension nscd-service-type
                             (const (list nss-pam-ldapd)))
          (service-extension shepherd-root-service-type
                             nslcd-shepherd-service)))
   (default-value (nslcd-configuration))))

(define (generate-nslcd-documentation)
  (generate-documentation
   `((nslcd-configuration ,nslcd-configuration-fields))
   'nslcd-configuration))
