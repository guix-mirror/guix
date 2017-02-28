;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
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

(define-module (gnu services kerberos)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu system pam)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (pam-krb5-configuration
            pam-krb5-configuration?
            pam-krb5-service-type

            krb5-realm
            krb5-realm?

            krb5-configuration
            krb5-configuration?
            krb5-service-type))



(define unset-field (list 'unset-field))

(define (predicate/unset pred)
  (lambda (x) (or (eq? x unset-field) (pred x))))

(define string/unset? (predicate/unset string?))
(define boolean/unset? (predicate/unset boolean?))
(define integer/unset? (predicate/unset integer?))

(define (uglify-field-name field-name)
  "Return FIELD-NAME with all instances of '-' replaced by '_' and any
trailing '?' removed."
  (let ((str (symbol->string field-name)))
    (string-join (string-split (if (string-suffix? "?" str)
                                   (substring str 0 (1- (string-length str)))
                                   str)
                               #\-)
                 "_")))

(define (serialize-field* field-name val)
  (format #t "~a = ~a\n" (uglify-field-name field-name) val))

(define (serialize-string/unset field-name val)
  (unless (eq? val unset-field)
      (serialize-field* field-name val)))

(define (serialize-integer/unset field-name val)
  (unless (eq? val unset-field)
      (serialize-field* field-name val)))

(define (serialize-boolean/unset field-name val)
  (unless (eq? val unset-field)
      (serialize-field* field-name
                        (if val "true" "false"))))


;; An end-point is an address such as "192.168.0.1"
;; or an address port pair ("foobar.example.com" . 109)
(define (end-point? val)
  (match val
    ((? string?) #t)
    (((? string?) . (? integer?)) #t)
    (_ #f)))

(define (serialize-end-point field-name val)
  (serialize-field* field-name
                    (match val
                      ((host . port)
                       ;; The [] are needed in the case of IPv6 addresses
                       (format #f "[~a]:~a" host port))
                      (host
                       (format #f "~a" host)))))

(define (serialize-space-separated-string-list/unset field-name val)
  (unless (eq? val unset-field)
      (serialize-field* field-name (string-join val " "))))

(define (space-separated-string-list? val)
  (and (list? val)
       (and-map (lambda (x)
                  (and (string? x) (not (string-index x #\space))))
                val)))

(define space-separated-string-list/unset?
  (predicate/unset space-separated-string-list?))

(define comma-separated-integer-list/unset?
  (predicate/unset (lambda (val)
                     (and (list? val)
                          (and-map (lambda (x) (integer? x))
                                   val)))))

(define (serialize-comma-separated-integer-list/unset field-name val)
  (unless (eq? val unset-field)
      (serialize-field* field-name
                       (string-drop ; Drop the leading comma
                        (fold
                         (lambda (i prev)
                           (string-append prev "," (number->string i)))
                         "" val) 1))))

(define file-name? (predicate/unset
                    (lambda (val)
                      (string-prefix? "/" val))))

(define (serialize-field field-name val)
  (format #t "~a ~a\n" (uglify-field-name field-name) val))

(define (serialize-string field-name val)
  (serialize-field field-name val))

(define (serialize-file-name field-name val)
  (unless (eq? val unset-field)
    (serialize-string field-name val)))

(define (serialize-space-separated-string-list field-name val)
  (serialize-field field-name (string-join val " ")))

(define (non-negative-integer? val)
  (and (exact-integer? val) (not (negative? val))))

(define (serialize-non-negative-integer/unset field-name val)
  (unless (eq? val unset-field)
    (serialize-field* field-name val)))

(define (free-form-fields? val)
  (match val
    (() #t)
    ((((? symbol?) . (? string)) . val) (free-form-fields? val))
    (_ #f)))

(define (serialize-free-form-fields field-name val)
  (for-each (match-lambda ((k . v) (serialize-field* k v))) val))

(define non-negative-integer/unset? (predicate/unset non-negative-integer?))

(define (realm-list? val)
  (and (list? val)
       (and-map (lambda (x) (krb5-realm? x)) val)))

(define (serialize-realm-list field-name val)
  (format #t "\n[~a]\n" field-name)
  (for-each (lambda (realm)
              (format #t "\n~a = {\n" (krb5-realm-name realm))
              (for-each (lambda (field)
                          (unless (eq? 'name (configuration-field-name field))
                            ((configuration-field-serializer field)
                             (configuration-field-name field)
                             ((configuration-field-getter field)
                              realm)))) krb5-realm-fields)

              (format #t "}\n")) val))



;; For a more detailed explanation of these fields see man 5 krb5.conf
(define-configuration krb5-realm
  (name
   (string/unset unset-field)
   "The name of the realm.")

  (kdc
   (end-point unset-field)
   "The host and port on which the realm's Key Distribution Server listens.")

  (admin-server
   (string/unset unset-field)
   "The Host running the administration server for the realm.")

  (master-kdc
   (string/unset unset-field)
   "If an attempt to get credentials fails because of an invalid password, 
the client software will attempt to contact the master KDC.")

  (kpasswd-server
   (string/unset unset-field)
   "The server where password changes are performed.")

  (auth-to-local
   (free-form-fields '())
   "Rules to map between principals and local users.")

  (auth-to-local-names
   (free-form-fields '())
   "Explicit mappings between principal names and local user names.")

  (http-anchors
   (free-form-fields '())
   "Useful only when http proxy is used to access KDC or KPASSWD.")

  ;; The following are useful only for working with V4 services
  (default-domain
    (string/unset unset-field)
    "The domain used to expand host names when translating Kerberos 4 service
principals to Kerberos 5 principals")

  (v4-instance-convert
   (free-form-fields '())
   "Exceptions to the default-domain mapping rule.")

  (v4-realm
   (string/unset unset-field)
   "Used  when the V4 realm name and the V5 realm name are not the same, but
still share the same principal names and passwords"))



;; For a more detailed explanation of these fields see man 5 krb5.conf
(define-configuration krb5-configuration
  (allow-weak-crypto?
   (boolean/unset unset-field)
   "If true, permits access to services which only offer weak encryption.")

  (ap-req-checksum-type
   (non-negative-integer/unset unset-field)
   "The type of the AP-REQ checksum.")

  (canonicalize?
   (boolean/unset unset-field)
   "Should principals in initial ticket requests be canonicalized?")

  (ccache-type
   (non-negative-integer/unset unset-field)
   "The format of the credential cache type.")

  (clockskew
   (non-negative-integer/unset unset-field)
   "Maximum allowable clock skew in seconds (default 300).")

  (default-ccache-name
    (file-name unset-field)
    "The name of the default credential cache.")

  (default-client-keytab-name
    (file-name unset-field)
    "The name of the default keytab for client credentials.")

  (default-keytab-name
    (file-name unset-field)
    "The name of the default keytab file.")

  (default-realm
    (string/unset unset-field)
    "The realm to be accessed if not explicitly specified by clients.")

  (default-tgs-enctypes
    (free-form-fields '())
    "Session key encryption types when making TGS-REQ requests.")

  (default-tkt-enctypes
    (free-form-fields '())
    "Session key encryption types when making AS-REQ requests.")

  (dns-canonicalize-hostname?
   (boolean/unset  unset-field)
   "Whether name lookups will be used to canonicalize host names for use in 
service principal names.")

  (dns-lookup-kdc?
   (boolean/unset unset-field)
 "Should DNS SRV records should be used to locate the KDCs and other servers 
not appearing in the realm specification")

  (err-fmt
   (string/unset unset-field)
   "Custom error message formatting. If not #f error messages will be formatted 
by substituting a normal error message for %M and an error code for %C in the 
value.")

  (forwardable?
   (boolean/unset unset-field)
   "Should initial tickets be forwardable by default?")

  (ignore-acceptor-hostname?
   (boolean/unset unset-field)
   "When accepting GSSAPI or krb5 security contexts for host-based service 
principals, ignore any hostname passed by the calling application, and allow 
clients to authenticate to any service principal in the keytab matching the 
service name and realm name.")

  (k5login-authoritative?
   (boolean/unset unset-field)
   "If this flag is true, principals must be listed in a local user's k5login
file to be granted login access, if a ~/.k5login file exists.")

  (k5login-directory
   (string/unset unset-field)
   "If not #f, the library will look for a local user's @file{k5login} file 
within the named directory (instead of the user's home directory), with a 
file name corresponding to the local user name.")

  (kcm-mach-service
   (string/unset unset-field)
   "The name of the bootstrap service used to contact the KCM daemon for the 
KCM credential cache type.")

  (kcm-socket
   (file-name unset-field)
 "Path to the Unix domain socket used to access the KCM daemon for the KCM 
credential cache type.")

  (kdc-default-options
   (non-negative-integer/unset unset-field)
   "Default KDC options (logored for multiple values) when requesting initial 
tickets.")

  (kdc-timesync
   (non-negative-integer/unset unset-field)
   "Attempt to compensate for clock skew between the KDC and client.")

  (kdc-req-checksum-type
   (non-negative-integer/unset unset-field)
   "The type of checksum to use for the KDC requests. Relevant only for DES 
keys")

  (noaddresses?
   (boolean/unset unset-field)
   "If true, initial ticket requests will not be made with address restrictions.
This enables their use across NATs.")

  (permitted-enctypes
   (space-separated-string-list/unset unset-field)
   "All encryption types that are permitted for use in session key encryption.")

  (plugin-base-dir
   (file-name unset-field)
   "The directory where krb5 plugins are located.")

  (preferred-preauth-types
   (comma-separated-integer-list/unset unset-field)
   "The preferred pre-authentication types which the client will attempt before 
others.")

  (proxiable?
   (boolean/unset unset-field)
   "Should initial tickets be proxiable by default?")

  (rdns?
   (boolean/unset unset-field)
   "Should reverse DNS lookup be used in addition to forward name lookup to 
canonicalize host names for use in service principal names.")

  (realm-try-domains
   (integer/unset unset-field)
   "Should a host's domain components should be used to determine the Kerberos 
realm of the host.")

  (renew-lifetime
   (non-negative-integer/unset unset-field)
   "The default renewable lifetime for initial ticket requests.")

  (safe-checksum-type
   (non-negative-integer/unset unset-field)
   "The type of checksum to use for the KRB-SAFE requests.")

  (ticket-lifetime
   (non-negative-integer/unset unset-field)
   "The default lifetime for initial ticket requests.")

  (udp-preference-limit
   (non-negative-integer/unset unset-field)
   "When sending messages to the KDC, the library will try using TCP
before UDP if the size of the message greater than this limit.")

  (verify-ap-rereq-nofail?
   (boolean/unset unset-field)
 "If true, then attempts to verify initial credentials will fail if the client
machine does not have a keytab.")

  (realms
   (realm-list '())
   "The list of realms which clients may access."))


(define (krb5-configuration-file config)
  "Create a Kerberos 5 configuration file based on CONFIG"
  (mixed-text-file "krb5.conf"
                   "[libdefaults]\n\n"
                   (with-output-to-string
                     (lambda ()
                       (serialize-configuration config
                                                krb5-configuration-fields)))))

(define (krb5-etc-service config)
  (list `("krb5.conf" ,(krb5-configuration-file config))))


(define krb5-service-type
  (service-type (name 'krb5)
                (extensions
                 (list (service-extension etc-service-type
                                          krb5-etc-service)))))




(define-record-type* <pam-krb5-configuration>
  pam-krb5-configuration  make-pam-krb5-configuration
  pam-krb5-configuration?
  (pam-krb5               pam-krb5-configuration-pam-krb5
                          (default pam-krb5))
  (minimum-uid            pam-krb5-configuration-minimum-uid
                          (default 1000)))

(define (pam-krb5-pam-service config)
  "Return a PAM service for Kerberos authentication."
  (lambda (pam)
    (define pam-krb5-module
      #~(string-append #$(pam-krb5-configuration-pam-krb5 config)
                       "/lib/security/pam_krb5.so"))

    (let ((pam-krb5-sufficient
           (pam-entry
            (control "sufficient")
            (module pam-krb5-module)
            (arguments
             (list
              (format #f "minimum_uid=~a"
                      (pam-krb5-configuration-minimum-uid config)))))))
      (pam-service
       (inherit pam)
       (auth (cons* pam-krb5-sufficient
                    (pam-service-auth pam)))
       (session (cons* pam-krb5-sufficient
                       (pam-service-session pam)))
       (account (cons* pam-krb5-sufficient
                       (pam-service-account pam)))))))

(define (pam-krb5-pam-services config)
  (list (pam-krb5-pam-service config)))

(define pam-krb5-service-type
  (service-type (name 'pam-krb5)
                (extensions
                 (list
                  (service-extension pam-root-service-type
                                     pam-krb5-pam-services)))))
