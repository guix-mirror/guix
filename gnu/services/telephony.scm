;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 nee  <nee-git@hidamari.blue>
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

(define-module (gnu services telephony)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages telephony)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (murmur-configuration
            make-murmur-configuration
            murmur-configuration?
            murmur-configuration-package
            murmur-configuration-user
            murmur-configuration-group
            murmur-configuration-port
            murmur-configuration-welcome-text
            murmur-configuration-server-password
            murmur-configuration-max-users
            murmur-configuration-max-user-bandwidth
            murmur-configuration-database-file
            murmur-configuration-log-file
            murmur-configuration-pid-file
            murmur-configuration-autoban-attempts
            murmur-configuration-autoban-timeframe
            murmur-configuration-autoban-time
            murmur-configuration-opus-threshold
            murmur-configuration-channel-nesting-limit
            murmur-configuration-channelname-regex
            murmur-configuration-username-regex
            murmur-configuration-text-message-length
            murmur-configuration-image-message-length
            murmur-configuration-cert-required?
            murmur-configuration-remember-channel?
            murmur-configuration-allow-html?
            murmur-configuration-allow-ping?
            murmur-configuration-bonjour?
            murmur-configuration-send-version?
            murmur-configuration-log-days
            murmur-configuration-obfuscate-ips?
            murmur-configuration-ssl-cert
            murmur-configuration-ssl-key
            murmur-configuration-ssl-dh-params
            murmur-configuration-ssl-ciphers
            murmur-configuration-public-registration
            murmur-configuration-file

            murmur-public-registration-configuration
            make-murmur-public-registration-configuration
            murmur-public-registration-configuration?
            murmur-public-registration-configuration-name
            murmur-public-registration-configuration-url
            murmur-public-registration-configuration-password
            murmur-public-registration-configuration-hostname

            murmur-service-type))

;; https://github.com/mumble-voip/mumble/blob/master/scripts/murmur.ini

(define-record-type* <murmur-configuration> murmur-configuration
  make-murmur-configuration
  murmur-configuration?
  (package               murmur-configuration-package ;<package>
                         (default mumble))
  (user                  murmur-configuration-user
                         (default "murmur"))
  (group                 murmur-configuration-group
                         (default "murmur"))
  (port                  murmur-configuration-port
                         (default 64738))
  (welcome-text          murmur-configuration-welcome-text
                         (default ""))
  (server-password       murmur-configuration-server-password
                         (default ""))
  (max-users             murmur-configuration-max-users
                         (default 100))
  (max-user-bandwidth    murmur-configuration-max-user-bandwidth
                         (default #f))
  (database-file         murmur-configuration-database-file
                         (default "/var/lib/murmur/db.sqlite"))
  (log-file              murmur-configuration-log-file
                         (default "/var/log/murmur/murmur.log"))
  (pid-file              murmur-configuration-pid-file
                         (default "/var/run/murmur/murmur.pid"))
  (autoban-attempts      murmur-configuration-autoban-attempts
                         (default 10))
  (autoban-timeframe     murmur-configuration-autoban-timeframe
                         (default 120))
  (autoban-time          murmur-configuration-autoban-time
                         (default 300))
  (opus-threshold        murmur-configuration-opus-threshold
                         (default 100)) ; integer percent
  (channel-nesting-limit murmur-configuration-channel-nesting-limit
                         (default 10))
  (channelname-regex     murmur-configuration-channelname-regex
                         (default #f))
  (username-regex        murmur-configuration-username-regex
                         (default #f))
  (text-message-length   murmur-configuration-text-message-length
                         (default 5000))
  (image-message-length  murmur-configuration-image-message-length
                         (default (* 128 1024))) ; 128 Kilobytes
  (cert-required?         murmur-configuration-cert-required?
                          (default #f))
  (remember-channel?     murmur-configuration-remember-channel?
                         (default #f))
  (allow-html?           murmur-configuration-allow-html?
                         (default #f))
  (allow-ping?           murmur-configuration-allow-ping?
                         (default #f))
  (bonjour?              murmur-configuration-bonjour?
                         (default #f))
  (send-version?         murmur-configuration-send-version?
                         (default #f))
  (log-days              murmur-configuration-log-days
                         (default 31))
  (obfuscate-ips?        murmur-obfuscate-ips?
                         (default #t))
  (ssl-cert              murmur-configuration-ssl-cert
                         (default #f))
  (ssl-key               murmur-configuration-ssl-key
                         (default #f))
  (ssl-dh-params         murmur-configuration-ssl-dh-params
                         (default #f))
  (ssl-ciphers           murmur-configuration-ssl-ciphers
                         (default #f))
  (public-registration   murmur-configuration-public-registration
                         (default #f))  ; <murmur-public-registration-configuration>
  (file                  murmur-configuration-file
                         (default #f)))

(define-record-type* <murmur-public-registration-configuration>
  murmur-public-registration-configuration
  make-murmur-public-registration-configuration
  murmur-public-registration-configuration?
  (name         murmur-public-registration-configuration-name)
  (password     murmur-public-registration-configuration-password)
  (url          murmur-public-registration-configuration-url)
  (hostname     murmur-public-registration-configuration-hostname
                (default #f)))

(define (flatten . lst)
  "Return a list that recursively concatenates all sub-lists of LST."
  (define (flatten1 head out)
    (if (list? head)
        (fold-right flatten1 out head)
        (cons head out)))
  (fold-right flatten1 '() lst))

(define (default-murmur-config config)
  (match-record
   config
   <murmur-configuration>
   (user port welcome-text server-password max-users max-user-bandwidth
    database-file log-file pid-file autoban-attempts autoban-timeframe
    autoban-time opus-threshold channel-nesting-limit channelname-regex
    username-regex text-message-length image-message-length cert-required?
    remember-channel? allow-html? allow-ping? bonjour? send-version?
    log-days obfuscate-ips? ssl-cert ssl-key ssl-dh-params ssl-ciphers
    public-registration)
   (apply mixed-text-file "murmur.ini"
          (flatten
           "welcometext=" welcome-text "\n"
           "port=" (number->string port) "\n"
           (if server-password (list "serverpassword=" server-password "\n") '())
           (if max-user-bandwidth (list "bandwidth="
                                        (number->string max-user-bandwidth) "\n")
               '())
           "users=" (number->string max-users) "\n"
           "uname=" user "\n"
           "database=" database-file "\n"
           "logfile=" log-file "\n"
           "pidfile=" pid-file "\n"
           (if autoban-attempts (list "autobanAttempts=" (number->string autoban-attempts) "\n") '())
           (if autoban-timeframe (list "autobanTimeframe=" (number->string autoban-timeframe) "\n") '())
           (if autoban-time (list "autobanTime=" (number->string autoban-time) "\n") '())
           (if opus-threshold (list "opusthreshold=" (number->string opus-threshold) "\n") '())
           (if channel-nesting-limit (list "channelnestinglimit=" (number->string channel-nesting-limit) "\n") '())
           (if channelname-regex (list "channelname=" channelname-regex "\n") '())
           (if username-regex (list "username=" username-regex "\n") '())
           (if text-message-length (list "textmessagelength=" (number->string text-message-length) "\n") '())
           (if image-message-length (list "imagemessagelength=" (number->string image-message-length) "\n") '())
           (if log-days (list "logdays=" (number->string log-days) "\n") '())
           "obfuscate=" (if obfuscate-ips? "true" "false") "\n"
           "certrequired=" (if cert-required? "true" "false") "\n"
           "rememberchannel=" (if remember-channel? "true" "false") "\n"
           "allowhtml=" (if allow-html? "true" "false") "\n"
           "allowping=" (if allow-ping? "true" "false") "\n"
           "bonjour=" (if bonjour? "true" "false") "\n"
           "sendversion=" (if send-version? "true" "false") "\n"
           (cond ((and ssl-cert ssl-key)
                  (list
                   "sslCert=" ssl-cert "\n"
                   "sslKey=" ssl-key "\n"))
                 ((or ssl-cert ssl-key)
                  (error "ssl-cert and ssl-key must both be set"
                         ssl-cert ssl-key))
                 (else '()))
           (if ssl-dh-params (list "sslDHParams=" ssl-dh-params) '())
           (if ssl-ciphers (list "sslCiphers=" ssl-ciphers) '())

           (match public-registration
             (#f '())
             (($ <murmur-public-registration-configuration>
                 name password url hostname)
              (if (and (or (not server-password) (string-null? server-password))
                       allow-ping?)
                  (list
                   "registerName=" name "\n"
                   "registerPassword=" password "\n"
                   "registerUrl=" url "\n"
                   (if hostname
                       (string-append "registerHostname=" hostname "\n")
                       ""))
                  (error "To publicly register your murmur server your server must be publicy visible
and users must be able to join without a password. To fix this set:
(allow-ping? #t)
(server-password \"\")
Or set public-registration to #f"))))))))

(define (murmur-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let* ((log-dir (dirname #$(murmur-configuration-log-file config)))
             (pid-dir (dirname #$(murmur-configuration-pid-file config)))
             (db-dir (dirname #$(murmur-configuration-database-file config)))
             (user (getpwnam #$(murmur-configuration-user config)))
             (init-dir
              (lambda (name dir)
                (format #t "creating murmur ~a directory '~a'\n" name dir)
                (mkdir-p dir)
                (chown dir (passwd:uid user) (passwd:gid user))
                (chmod dir #o700)))
             (ini #$(or (murmur-configuration-file config)
                        (default-murmur-config config))))
        (init-dir "log" log-dir)
        (init-dir "pid" pid-dir)
        (init-dir "database" db-dir)

        (format #t "murmur: use config file: ~a~%\n" ini)
        (format #t "murmur: to set the SuperUser password run:
    `~a -ini ~a -readsupw`\n"
                #$(file-append (murmur-configuration-package config)
                               "/bin/murmurd") ini)
        #t)))

(define murmur-accounts
  (match-lambda
    (($ <murmur-configuration> _ user group)
     (list
      (user-group
       (name group)
       (system? #t))
      (user-account
       (name user)
       (group group)
       (system? #t)
       (comment "Murmur Daemon")
       (home-directory "/var/empty")
       (shell (file-append shadow "/sbin/nologin")))))))

(define (murmur-shepherd-service config)
  (list (shepherd-service
         (provision '(murmur))
         (documentation "Run the Murmur Mumble server.")
         (requirement '(networking))
         (start #~(make-forkexec-constructor
                   '(#$(file-append (murmur-configuration-package config)
                                    "/bin/murmurd")
                     "-ini"
                     #$(or (murmur-configuration-file config)
                           (default-murmur-config config)))
                   #:pid-file #$(murmur-configuration-pid-file config)))
         (stop #~(make-kill-destructor)))))

(define murmur-service-type
  (service-type (name 'murmur)
                (description
                 "Run the Murmur voice-over-IP (VoIP) server of the Mumble
suite.")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          murmur-shepherd-service)
                       (service-extension activation-service-type
                                          murmur-activation)
                       (service-extension account-service-type
                                          murmur-accounts)))
                (default-value (murmur-configuration))))
