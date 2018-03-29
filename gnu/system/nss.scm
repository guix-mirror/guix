;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system nss)
  #:use-module (rnrs enums)
  #:use-module (guix records)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (name-service-switch?
            name-service-switch
            name-service?
            name-service

            lookup-specification

            %default-nss
            %mdns-host-lookup-nss

            %files
            %compat
            %dns

            name-service-switch->string))

;;; Commentary:
;;;
;;; Bindings for libc's name service switch (NSS) configuration.
;;;
;;; Code:

(define-record-type* <name-service> name-service
  make-name-service
  name-service?
  (name     name-service-name)
  (reaction name-service-reaction
            (default (lookup-specification))))

;; Lookup specification (info "(libc) Actions in the NSS Configuration").

(define-enumeration lookup-action
  (return continue)
  make-lookup-action)

(define-enumeration lookup-status
  (success
   not-found
   unavailable
   try-again)
  make-lookup-status)

(define-record-type <lookup-status-negation>
  (lookup-status-negation status)
  lookup-status-negation?
  (status lookup-status-negation-status))

(define-record-type <lookup-reaction>
  (make-lookup-reaction status action)
  lookup-reaction?
  (status  lookup-reaction-status)
  (action  lookup-reaction-action))

(define-syntax lookup-reaction
  (syntax-rules (not =>)
    ((_ ((not status) => action))
     (make-lookup-reaction (lookup-status-negation (lookup-status status))
                           (lookup-action action)))
    ((_ (status => action))
     (make-lookup-reaction (lookup-status status)
                           (lookup-action action)))))

(define-syntax-rule (lookup-specification reaction ...)
  "Return an NSS lookup specification."
  (list (lookup-reaction reaction) ...))


;;;
;;; Common name services and default NSS configuration.
;;;

(define %compat
  ;; Note: Starting from version 2.26, libc no longer provides libnss_compat
  ;; so this specification has become useless.
  (name-service
    (name "compat")
    (reaction (lookup-specification (not-found => return)))))

(define %files
  (name-service (name "files")))

(define %dns
  ;; DNS is supposed to be authoritative, so unless it's unavailable, return
  ;; what it finds.
  (name-service
    (name "dns")
    (reaction (lookup-specification ((not unavailable) => return)))))

;; The NSS.  We list all the databases here because that allows us to
;; statically ensure that the user's configuration refers to existing
;; databases.  See libc/nss/databases.def for the list of databases.  Default
;; values obtained by looking for "DEFAULT_CONFIG" in libc/nss/*.c.
;;
;; Although libc places 'dns' before 'files' in the default configurations of
;; the 'hosts' and 'networks' databases, we choose to put 'files' before 'dns'
;; by default, so that users can override host/address mappings in /etc/hosts
;; and bypass DNS to improve their privacy and escape NSA's MORECOWBELL.
(define-record-type* <name-service-switch> name-service-switch
  make-name-service-switch
  name-service-switch?
  (aliases    name-service-switch-aliases
              (default '()))
  (ethers     name-service-switch-ethers
              (default '()))
  (group      name-service-switch-group
              (default (list %files)))
  (gshadow    name-service-switch-gshadow
              (default '()))
  (hosts      name-service-switch-hosts
              (default (list %files %dns)))
  (initgroups name-service-switch-initgroups
              (default '()))
  (netgroup   name-service-switch-netgroup
              (default '()))
  (networks   name-service-switch-networks
              (default (list %files %dns)))
  (password   name-service-switch-password
              (default (list %files)))
  (public-key name-service-switch-public-key
              (default '()))
  (rpc        name-service-switch-rpc
              (default '()))
  (services   name-service-switch-services
              (default '()))
  (shadow     name-service-switch-shadow
              (default (list %files))))

(define %default-nss
  ;; Default NSS configuration.
  (name-service-switch))

(define %mdns-host-lookup-nss
  (name-service-switch
    (hosts (list %files                           ;first, check /etc/hosts

                 ;; If the above did not succeed, try with 'mdns_minimal'.
                 (name-service
                   (name "mdns_minimal")

                   ;; 'mdns_minimal' is authoritative for '.local'.  When it
                   ;; returns "not found", no need to try the next methods.
                   (reaction (lookup-specification
                              (not-found => return))))

                 ;; Then fall back to DNS.
                 (name-service
                   (name "dns"))

                 ;; Finally, try with the "full" 'mdns'.
                 (name-service
                   (name "mdns"))))))


;;;
;;; Serialization.
;;;

(define (lookup-status->string status)
  (match status
    ('success     "SUCCESS")
    ('not-found   "NOTFOUND")
    ('unavailable "UNAVAIL")
    ('try-again   "TRYAGAIN")
    (($ <lookup-status-negation> status)
     (string-append "!" (lookup-status->string status)))))

(define lookup-reaction->string
  (match-lambda
   (($ <lookup-reaction> status action)
    (string-append (lookup-status->string status) "="
                   (symbol->string action)))))

(define name-service->string
  (match-lambda
   (($ <name-service> name ())
    name)
   (($ <name-service> name reactions)
    (string-append name " ["
                   (string-join (map lookup-reaction->string reactions))
                   "]"))))

(define (name-service-switch->string nss)
  "Return the 'nsswitch.conf' contents for NSS as a string.  See \"NSS
Configuration File\" in the libc manual."
  (let-syntax ((->string
                (syntax-rules ()
                  ((_ name field)
                   (match (field nss)
                     (()                          ;keep the default config
                      "")
                     ((services (... ...))
                      (string-append name ":\t"
                                     (string-join
                                      (map name-service->string services))
                                     "\n")))))))
    (string-append (->string "aliases"    name-service-switch-aliases)
                   (->string "ethers"     name-service-switch-ethers)
                   (->string "group"      name-service-switch-group)
                   (->string "gshadow"    name-service-switch-gshadow)
                   (->string "hosts"      name-service-switch-hosts)
                   (->string "initgroups" name-service-switch-initgroups)
                   (->string "netgroup"   name-service-switch-netgroup)
                   (->string "networks"   name-service-switch-networks)
                   (->string "passwd"     name-service-switch-password)
                   (->string "publickey"  name-service-switch-public-key)
                   (->string "rpc"        name-service-switch-rpc)
                   (->string "services"   name-service-switch-services)
                   (->string "shadow"     name-service-switch-shadow))))

;;; Local Variables:
;;; eval: (put 'name-service 'scheme-indent-function 0)
;;; eval: (put 'name-service-switch 'scheme-indent-function 0)
;;; End:

;;; nss.scm ends here
