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

(define-module (tests services telephony)
  #:use-module (gnu build jami-service)
  #:use-module (gnu services telephony)
  #:use-module (srfi srfi-64))

;;; Tests for the (gnu services telephony) and related modules.

(test-begin "jami-service")

(define parse-dbus-reply
  (@@ (gnu build jami-service) parse-dbus-reply))

(define parse-account-ids
  (@@ (gnu build jami-service) parse-account-ids))

(define parse-account-details
  (@@ (gnu build jami-service) parse-account-details))

(define parse-contacts
  (@@ (gnu build jami-service) parse-contacts))

(define jami-account->alist
  (@@ (gnu services telephony) jami-account->alist))

;; $ dbus-send --print-reply --dest="cx.ring.Ring" \
;; "/cx/ring/Ring/ConfigurationManager" \
;; "cx.ring.Ring.ConfigurationManager.getAccountList"
(define getAccountList-reply "\
method return time=1622217253.386711 sender=:1.7 -> destination=:1.14 serial=140 reply_serial=2
   array [
      string \"addf37fbb558d6a0\"
      string \"d5cbeb7d08c98a65\"
      string \"398af0c6b74ce101\"
   ]
")

(test-equal "parse-account-ids"
  '("addf37fbb558d6a0" "d5cbeb7d08c98a65" "398af0c6b74ce101")
  (parse-account-ids getAccountList-reply))

;; $ dbus-send --print-reply --dest="cx.ring.Ring" \
;; "/cx/ring/Ring/ConfigurationManager" \
;; "cx.ring.Ring.ConfigurationManager.getAccountDetails" \
;; 'string:398af0c6b74ce101'
(define getAccountDetails-reply "\
method return time=1622254991.789588 sender=:1.7 -> destination=:1.19 serial=145 reply_serial=2
   array [
      dict entry(
         string \"Account.accountDiscovery\"
         string \"false\"
      )
      dict entry(
         string \"Account.accountPublish\"
         string \"false\"
      )
      dict entry(
         string \"Account.activeCallLimit\"
         string \"-1\"
      )
      dict entry(
         string \"Account.alias\"
         string \"some-rendezvous-point-name\"
      )
      dict entry(
         string \"Account.allModeratorEnabled\"
         string \"true\"
      )
      dict entry(
         string \"Account.allowCertFromContact\"
         string \"true\"
      )
      dict entry(
         string \"Account.allowCertFromHistory\"
         string \"true\"
      )
      dict entry(
         string \"Account.allowCertFromTrusted\"
         string \"true\"
      )
      dict entry(
         string \"Account.archiveHasPassword\"
         string \"false\"
      )
      dict entry(
         string \"Account.audioPortMax\"
         string \"32766\"
      )
      dict entry(
         string \"Account.audioPortMin\"
         string \"16384\"
      )
      dict entry(
         string \"Account.autoAnswer\"
         string \"false\"
      )
      dict entry(
         string \"Account.defaultModerators\"
         string \"\"
      )
      dict entry(
         string \"Account.deviceID\"
         string \"94b4070fc7a8afa8482c777a9822c52e6af2e1bd\"
      )
      dict entry(
         string \"Account.deviceName\"
         string \"some-device\"
      )
      dict entry(
         string \"Account.dhtProxyListUrl\"
         string \"https://config.jami.net/proxyList\"
      )
      dict entry(
         string \"Account.displayName\"
         string \"some-rendezvous-point-name\"
      )
      dict entry(
         string \"Account.dtmfType\"
         string \"overrtp\"
      )
      dict entry(
         string \"Account.enable\"
         string \"true\"
      )
      dict entry(
         string \"Account.hasCustomUserAgent\"
         string \"false\"
      )
      dict entry(
         string \"Account.hostname\"
         string \"bootstrap.jami.net\"
      )
      dict entry(
         string \"Account.localInterface\"
         string \"default\"
      )
      dict entry(
         string \"Account.localModeratorsEnabled\"
         string \"true\"
      )
      dict entry(
         string \"Account.mailbox\"
         string \"\"
      )
      dict entry(
         string \"Account.managerUri\"
         string \"\"
      )
      dict entry(
         string \"Account.managerUsername\"
         string \"\"
      )
      dict entry(
         string \"Account.peerDiscovery\"
         string \"false\"
      )
      dict entry(
         string \"Account.presenceSubscribeSupported\"
         string \"true\"
      )
      dict entry(
         string \"Account.proxyEnabled\"
         string \"false\"
      )
      dict entry(
         string \"Account.proxyPushToken\"
         string \"\"
      )
      dict entry(
         string \"Account.proxyServer\"
         string \"dhtproxy.jami.net:[80-95]\"
      )
      dict entry(
         string \"Account.publishedAddress\"
         string \"\"
      )
      dict entry(
         string \"Account.publishedPort\"
         string \"5060\"
      )
      dict entry(
         string \"Account.publishedSameAsLocal\"
         string \"true\"
      )
      dict entry(
         string \"Account.rendezVous\"
         string \"true\"
      )
      dict entry(
         string \"Account.ringtoneEnabled\"
         string \"true\"
      )
      dict entry(
         string \"Account.ringtonePath\"
         string \"/usr/share/ring/ringtones/default.opus\"
      )
      dict entry(
         string \"Account.type\"
         string \"RING\"
      )
      dict entry(
         string \"Account.upnpEnabled\"
         string \"true\"
      )
      dict entry(
         string \"Account.useragent\"
         string \"\"
      )
      dict entry(
         string \"Account.username\"
         string \"ccb8bbe2382343f7feb140710ab48aaf1b55634e\"
      )
      dict entry(
         string \"Account.videoEnabled\"
         string \"true\"
      )
      dict entry(
         string \"Account.videoPortMax\"
         string \"65534\"
      )
      dict entry(
         string \"Account.videoPortMin\"
         string \"49152\"
      )
      dict entry(
         string \"DHT.PublicInCalls\"
         string \"true\"
      )
      dict entry(
         string \"DHT.port\"
         string \"7766\"
      )
      dict entry(
         string \"RingNS.account\"
         string \"3989b55313a911b6f0c004748b49b254f35c9ef6\"
      )
      dict entry(
         string \"RingNS.uri\"
         string \"\"
      )
      dict entry(
         string \"SRTP.enable\"
         string \"true\"
      )
      dict entry(
         string \"SRTP.keyExchange\"
         string \"sdes\"
      )
      dict entry(
         string \"SRTP.rtpFallback\"
         string \"false\"
      )
      dict entry(
         string \"STUN.enable\"
         string \"false\"
      )
      dict entry(
         string \"STUN.server\"
         string \"\"
      )
      dict entry(
         string \"TLS.certificateFile\"
         string \"/var/lib/jami/.local/share/jami/398af0c6b74ce101/ring_device.crt\"
      )
      dict entry(
         string \"TLS.certificateListFile\"
         string \"\"
      )
      dict entry(
         string \"TLS.ciphers\"
         string \"\"
      )
      dict entry(
         string \"TLS.method\"
         string \"Automatic\"
      )
      dict entry(
         string \"TLS.negotiationTimeoutSec\"
         string \"-1\"
      )
      dict entry(
         string \"TLS.password\"
         string \"\"
      )
      dict entry(
         string \"TLS.privateKeyFile\"
         string \"/var/lib/jami/.local/share/jami/398af0c6b74ce101/ring_device.key\"
      )
      dict entry(
         string \"TLS.requireClientCertificate\"
         string \"true\"
      )
      dict entry(
         string \"TLS.serverName\"
         string \"\"
      )
      dict entry(
         string \"TLS.verifyClient\"
         string \"true\"
      )
      dict entry(
         string \"TLS.verifyServer\"
         string \"true\"
      )
      dict entry(
         string \"TURN.enable\"
         string \"true\"
      )
      dict entry(
         string \"TURN.password\"
         string \"ring\"
      )
      dict entry(
         string \"TURN.realm\"
         string \"ring\"
      )
      dict entry(
         string \"TURN.server\"
         string \"turn.jami.net\"
      )
      dict entry(
         string \"TURN.username\"
         string \"ring\"
      )
   ]
")

(test-equal "parse-account-details; username, alias and display name"
  '("ccb8bbe2382343f7feb140710ab48aaf1b55634e" ;username
    "some-rendezvous-point-name"               ;alias
    "some-rendezvous-point-name")              ;displayName
  (let ((account-details (parse-account-details getAccountDetails-reply)))
    (list (assoc-ref account-details "Account.username")
          (assoc-ref account-details "Account.alias")
          (assoc-ref account-details "Account.displayName"))))

(define getContacts-reply "\
method return time=1627014042.752673 sender=:1.113 -> destination=:1.186 serial=220 reply_serial=2
   array [
      array [
         dict entry(
            string \"added\"
            string \"1578883327\"
         )
         dict entry(
            string \"confirmed\"
            string \"true\"
         )
         dict entry(
            string \"id\"
            string \"1c7d5a09464223442549fef172a3cf6f4de9b01c\"
         )
      ]
      array [
         dict entry(
            string \"added\"
            string \"1623107941\"
         )
         dict entry(
            string \"confirmed\"
            string \"true\"
         )
         dict entry(
            string \"id\"
            string \"5903c6c9ac5cb863c64e559add3d5d1c8c563449\"
         )
      ]
      array [
         dict entry(
            string \"added\"
            string \"1595996256\"
         )
         dict entry(
            string \"confirmed\"
            string \"true\"
         )
         dict entry(
            string \"id\"
            string \"ff2d72a548693214fb3a0f0f7a943b5e2bb9be03\"
         )
      ]
   ]")

(test-equal "parse-account-contacts"
  '((("added" . "1578883327")
     ("confirmed" . "true")
     ("id" . "1c7d5a09464223442549fef172a3cf6f4de9b01c"))
    (("added" . "1623107941")
     ("confirmed" . "true")
     ("id" . "5903c6c9ac5cb863c64e559add3d5d1c8c563449"))
    (("added" . "1595996256")
     ("confirmed" . "true")
     ("id" . "ff2d72a548693214fb3a0f0f7a943b5e2bb9be03")))
  (parse-contacts getContacts-reply))

(define getContacts-empty-reply "\
method return time=1627400787.873988 sender=:1.1197 -> destination=:1.1463 serial=2127 reply_serial=2
   array [
   ]")

(test-equal "parse-account-contacts, empty array"
  '()
  (parse-contacts getContacts-empty-reply))

(define %dummy-jami-account (jami-account
                             (archive "/tmp/dummy.gz")))

(define %dummy-jami-account-2 (jami-account
                               (archive "/tmp/dummy.gz")
                               (rendezvous-point? #t)
                               (peer-discovery? #f)
                               (bootstrap-hostnames '("bootstrap.me"
                                                      "fallback.another.host"))
                               (name-server-uri "https://my.name.server")))

(test-equal "jami-account->alist, no account detail value set"
  '()
  (jami-account->alist %dummy-jami-account))

(test-equal "jami-account->alist, with account detail values"
  '(("Account.hostname" . "bootstrap.me;fallback.another.host")
    ("Account.peerDiscovery" . "false")
    ("Account.rendezVous" . "true")
    ("RingNS.uri" . "https://my.name.server"))
  (sort (jami-account->alist %dummy-jami-account-2)
        (lambda (x y)
          (string<=? (car x) (car y)))))

(test-end)
