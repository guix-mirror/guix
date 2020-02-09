;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
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

(define-module (gnu machine digital-ocean)
  #:use-module (gnu machine ssh)
  #:use-module (gnu machine)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system pam)
  #:use-module (guix base32)
  #:use-module (guix derivations)
  #:use-module (guix i18n)
  #:use-module (guix import json)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix ssh)
  #:use-module (guix store)
  #:use-module (ice-9 iconv)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ssh key)
  #:use-module (ssh sftp)
  #:use-module (ssh shell)
  #:use-module (web client)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (digital-ocean-configuration
            digital-ocean-configuration?

            digital-ocean-configuration-ssh-key
            digital-ocean-configuration-tags
            digital-ocean-configuration-region
            digital-ocean-configuration-size
            digital-ocean-configuration-enable-ipv6?

            digital-ocean-environment-type))

;;; Commentary:
;;;
;;; This module implements a high-level interface for provisioning "droplets"
;;; from the Digital Ocean virtual private server (VPS) service.
;;;
;;; Code:

(define %api-base "https://api.digitalocean.com")

(define %digital-ocean-token
  (make-parameter (getenv "GUIX_DIGITAL_OCEAN_TOKEN")))

(define* (post-endpoint endpoint body)
  "Encode BODY as JSON and send it to the Digital Ocean API endpoint
ENDPOINT. This procedure is quite a bit more specialized than 'http-post', as
it takes care to set headers such as 'Content-Type', 'Content-Length', and
'Authorization' appropriately."
  (let* ((uri (string->uri (string-append %api-base endpoint)))
         (body (string->bytevector (scm->json-string body) "UTF-8"))
         (headers `((User-Agent . "Guix Deploy")
                    (Accept . "application/json")
                    (Content-Type . "application/json")
                    (Authorization . ,(format #f "Bearer ~a"
                                              (%digital-ocean-token)))
                    (Content-Length . ,(number->string
                                        (bytevector-length body)))))
         (port (open-socket-for-uri uri))
         (request (build-request uri
                                 #:method 'POST
                                 #:version '(1 . 1)
                                 #:headers headers
                                 #:port port))
         (request (write-request request port)))
    (write-request-body request body)
    (force-output (request-port request))
    (let* ((response (read-response port))
           (body (read-response-body response)))
      (unless (= 2 (floor/ (response-code response) 100))
        (raise
         (condition (&message
                     (message (format
                               #f
                               (G_ "~a: HTTP post failed: ~a (~s)")
                               (uri->string uri)
                               (response-code response)
                               (response-reason-phrase response)))))))
      (close-port port)
      (bytevector->string body "UTF-8"))))

(define (fetch-endpoint endpoint)
  "Return the contents of the Digital Ocean API endpoint ENDPOINT as an
alist. This procedure is quite a bit more specialized than 'json-fetch', as it
takes care to set headers such as 'Accept' and 'Authorization' appropriately."
  (define headers
    `((user-agent . "Guix Deploy")
      (Accept . "application/json")
      (Authorization . ,(format #f "Bearer ~a" (%digital-ocean-token)))))
  (json-fetch (string-append %api-base endpoint) #:headers headers))


;;;
;;; Parameters for droplet creation.
;;;

(define-record-type* <digital-ocean-configuration> digital-ocean-configuration
  make-digital-ocean-configuration
  digital-ocean-configuration?
  this-digital-ocean-configuration
  (ssh-key     digital-ocean-configuration-ssh-key)      ; string
  (tags        digital-ocean-configuration-tags)         ; list of strings
  (region      digital-ocean-configuration-region)       ; string
  (size        digital-ocean-configuration-size)         ; string
  (enable-ipv6? digital-ocean-configuration-enable-ipv6?)) ; boolean

(define (read-key-fingerprint file-name)
  "Read the private key at FILE-NAME and return the key's fingerprint as a hex
string."
  (let* ((privkey (private-key-from-file file-name))
         (pubkey (private-key->public-key privkey))
         (hash (get-public-key-hash pubkey 'md5)))
    (bytevector->hex-string hash)))

(define (machine-droplet machine)
  "Return an alist describing the droplet allocated to MACHINE."
  (let ((tags (digital-ocean-configuration-tags
               (machine-configuration machine))))
    (find (lambda (droplet)
            (equal? (assoc-ref droplet "tags") (list->vector tags)))
          (vector->list
           (assoc-ref (fetch-endpoint "/v2/droplets") "droplets")))))

(define (machine-public-ipv4-network machine)
  "Return the public IPv4 network interface of the droplet allocated to
MACHINE as an alist. The expected fields are 'ip_address', 'netmask', and
'gateway'."
  (and-let* ((droplet (machine-droplet machine))
             (networks (assoc-ref droplet "networks"))
             (network (find (lambda (network)
                              (string= "public" (assoc-ref network "type")))
                            (vector->list (assoc-ref networks "v4")))))
    network))


;;;
;;; Remote evaluation.
;;;

(define (digital-ocean-remote-eval target exp)
  "Internal implementation of 'machine-remote-eval' for MACHINE instances with
an environment type of 'digital-ocean-environment-type'."
  (let* ((network (machine-public-ipv4-network target))
         (address (assoc-ref network "ip_address"))
         (ssh-key (digital-ocean-configuration-ssh-key
                   (machine-configuration target)))
         (delegate (machine
                    (inherit target)
                    (environment managed-host-environment-type)
                    (configuration
                     (machine-ssh-configuration
                      (host-name address)
                      (identity ssh-key)
                      (system "x86_64-linux"))))))
    (machine-remote-eval delegate exp)))


;;;
;;; System deployment.
;;;

;; The following script was adapted from the guide available at
;; <https://wiki.pantherx.org/Installation-digital-ocean/>.
(define (guix-infect network)
  "Given NETWORK, an alist describing the Droplet's public IPv4 network
interface, return a Bash script that will install the Guix system."
  (format #f "#!/bin/bash

apt-get update
apt-get install xz-utils -y
wget https://ftp.gnu.org/gnu/guix/guix-binary-1.0.1.x86_64-linux.tar.xz
cd /tmp
tar --warning=no-timestamp -xf ~~/guix-binary-1.0.1.x86_64-linux.tar.xz
mv var/guix /var/ && mv gnu /
mkdir -p ~~root/.config/guix
ln -sf /var/guix/profiles/per-user/root/current-guix ~~root/.config/guix/current
export GUIX_PROFILE=\"`echo ~~root`/.config/guix/current\" ;
source $GUIX_PROFILE/etc/profile
groupadd --system guixbuild
for i in `seq -w 1 10`; do
   useradd -g guixbuild -G guixbuild         \
           -d /var/empty -s `which nologin`  \
           -c \"Guix build user $i\" --system  \
           guixbuilder$i;
done;
cp ~~root/.config/guix/current/lib/systemd/system/guix-daemon.service /etc/systemd/system/
systemctl start guix-daemon && systemctl enable guix-daemon
mkdir -p /usr/local/bin
cd /usr/local/bin
ln -s /var/guix/profiles/per-user/root/current-guix/bin/guix
mkdir -p /usr/local/share/info
cd /usr/local/share/info
for i in /var/guix/profiles/per-user/root/current-guix/share/info/*; do
    ln -s $i;
done
guix archive --authorize < ~~root/.config/guix/current/share/guix/ci.guix.gnu.org.pub
# guix pull
guix package -i glibc-utf8-locales
export GUIX_LOCPATH=\"$HOME/.guix-profile/lib/locale\"
guix package -i openssl
cat > /etc/bootstrap-config.scm << EOF
(use-modules (gnu))
(use-service-modules networking ssh)

(operating-system
  (host-name \"gnu-bootstrap\")
  (timezone \"Etc/UTC\")
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target \"/dev/vda\")
               (terminal-outputs '(console))))
  (file-systems (cons (file-system
                        (mount-point \"/\")
                        (device \"/dev/vda1\")
                        (type \"ext4\"))
                      %base-file-systems))
  (services
   (append (list (static-networking-service \"eth0\" \"~a\"
                    #:netmask \"~a\"
                    #:gateway \"~a\"
                    #:name-servers '(\"84.200.69.80\" \"84.200.70.40\"))
                 (simple-service 'guile-load-path-in-global-env
                  session-environment-service-type
                  \\`((\"GUILE_LOAD_PATH\"
                     . \"/run/current-system/profile/share/guile/site/2.2\")
                    (\"GUILE_LOAD_COMPILED_PATH\"
                     . ,(string-append \"/run/current-system/profile/lib/guile/2.2/site-ccache:\"
                                       \"/run/current-system/profile/share/guile/site/2.2\"))))
                 (service openssh-service-type
                          (openssh-configuration
                           (log-level 'debug)
                           (permit-root-login 'without-password))))
           %base-services)))
EOF
# guix pull
guix system build /etc/bootstrap-config.scm
guix system reconfigure /etc/bootstrap-config.scm
mv /etc /old-etc
mkdir /etc
cp -r /old-etc/{passwd,group,shadow,gshadow,mtab,guix,bootstrap-config.scm} /etc/
guix system reconfigure /etc/bootstrap-config.scm"
          (assoc-ref network "ip_address")
          (assoc-ref network "netmask")
          (assoc-ref network "gateway")))

(define (machine-wait-until-available machine)
  "Block until the initial Debian image has been installed on the droplet
named DROPLET-NAME."
  (and-let* ((droplet (machine-droplet machine))
             (droplet-id (assoc-ref droplet "id"))
             (endpoint (format #f "/v2/droplets/~a/actions" droplet-id)))
    (let loop ()
      (let ((actions (assoc-ref (fetch-endpoint endpoint) "actions")))
        (unless (every (lambda (action)
                         (string= "completed" (assoc-ref action "status")))
                       (vector->list actions))
          (sleep 5)
          (loop))))))

(define (wait-for-ssh address ssh-key)
  "Block until the an SSH session can be made as 'root' with SSH-KEY at ADDRESS."
  (let loop ()
    (catch #t
      (lambda ()
        (open-ssh-session address #:user "root" #:identity ssh-key))
      (lambda args
        (sleep 5)
        (loop)))))

(define (add-static-networking target network)
  "Return an <operating-system> based on TARGET with a static networking
configuration for the public IPv4 network described by the alist NETWORK."
  (operating-system
    (inherit (machine-operating-system target))
    (services (cons* (static-networking-service "eth0"
                        (assoc-ref network "ip_address")
                        #:netmask (assoc-ref network "netmask")
                        #:gateway (assoc-ref network "gateway")
                        #:name-servers '("84.200.69.80" "84.200.70.40"))
                    (simple-service 'guile-load-path-in-global-env
                                    session-environment-service-type
                                    `(("GUILE_LOAD_PATH"
                                       . "/run/current-system/profile/share/guile/site/2.2")
                                      ("GUILE_LOAD_COMPILED_PATH"
                                       . ,(string-append "/run/current-system/profile/lib/guile/2.2/site-ccache:"
                                                         "/run/current-system/profile/share/guile/site/2.2"))))
                    (operating-system-user-services
                     (machine-operating-system target))))))

(define (deploy-digital-ocean target)
  "Internal implementation of 'deploy-machine' for 'machine' instances with an
environment type of 'digital-ocean-environment-type'."
  (maybe-raise-missing-api-key-error)
  (maybe-raise-unsupported-configuration-error target)
  (let* ((config (machine-configuration target))
         (name (machine-display-name target))
         (region (digital-ocean-configuration-region config))
         (size (digital-ocean-configuration-size config))
         (ssh-key (digital-ocean-configuration-ssh-key config))
         (fingerprint (read-key-fingerprint ssh-key))
         (enable-ipv6? (digital-ocean-configuration-enable-ipv6? config))
         (tags (digital-ocean-configuration-tags config))
         (request-body `(("name" . ,name)
                         ("region" . ,region)
                         ("size" . ,size)
                         ("image" . "debian-9-x64")
                         ("ssh_keys" . ,(vector fingerprint))
                         ("backups" . #f)
                         ("ipv6" . ,enable-ipv6?)
                         ("user_data" . #nil)
                         ("private_networking" . #nil)
                         ("volumes" . #nil)
                         ("tags" . ,(list->vector tags))))
         (response (post-endpoint "/v2/droplets" request-body)))
    (machine-wait-until-available target)
    (let* ((network (machine-public-ipv4-network target))
           (address (assoc-ref network "ip_address")))
      (wait-for-ssh address ssh-key)
      (let* ((ssh-session (open-ssh-session address #:user "root" #:identity ssh-key))
             (sftp-session (make-sftp-session ssh-session)))
        (call-with-remote-output-file sftp-session "/tmp/guix-infect.sh"
                                      (lambda (port)
                                        (display (guix-infect network) port)))
        (rexec ssh-session "/bin/bash /tmp/guix-infect.sh")
        ;; Session will close upon rebooting, which will raise 'guile-ssh-error.
        (catch 'guile-ssh-error
          (lambda () (rexec ssh-session "reboot"))
          (lambda args #t)))
      (wait-for-ssh address ssh-key)
      (let ((delegate (machine
                       (operating-system (add-static-networking target network))
                       (environment managed-host-environment-type)
                       (configuration
                        (machine-ssh-configuration
                         (host-name address)
                         (identity ssh-key)
                         (system "x86_64-linux"))))))
        (deploy-machine delegate)))))


;;;
;;; Roll-back.
;;;

(define (roll-back-digital-ocean target)
  "Internal implementation of 'roll-back-machine' for MACHINE instances with an
environment type of 'digital-ocean-environment-type'."
  (let* ((network (machine-public-ipv4-network target))
         (address (assoc-ref network "ip_address"))
         (ssh-key (digital-ocean-configuration-ssh-key
                   (machine-configuration target)))
         (delegate (machine
                    (inherit target)
                    (environment managed-host-environment-type)
                    (configuration
                     (machine-ssh-configuration
                      (host-name address)
                      (identity ssh-key)
                      (system "x86_64-linux"))))))
    (roll-back-machine delegate)))


;;;
;;; Environment type.
;;;

(define digital-ocean-environment-type
  (environment-type
   (machine-remote-eval digital-ocean-remote-eval)
   (deploy-machine      deploy-digital-ocean)
   (roll-back-machine   roll-back-digital-ocean)
   (name                'digital-ocean-environment-type)
   (description         "Provisioning of \"droplets\": virtual machines
 provided by the Digital Ocean virtual private server (VPS) service.")))


(define (maybe-raise-missing-api-key-error)
  (unless (%digital-ocean-token)
    (raise (condition
            (&message
             (message (G_ "No Digital Ocean access token was provided. This \
may be fixed by setting the environment variable GUIX_DIGITAL_OCAEN_TOKEN to \
one procured from https://cloud.digitalocean.com/account/api/tokens.")))))))

(define (maybe-raise-unsupported-configuration-error machine)
  "Raise an error if MACHINE's configuration is not an instance of
<digital-ocean-configuration>."
  (let ((config (machine-configuration machine))
        (environment (environment-type-name (machine-environment machine))))
    (unless (and config (digital-ocean-configuration? config))
      (raise (condition
              (&message
               (message (format #f (G_ "unsupported machine configuration '~a'
for environment of type '~a'")
                                config
                                environment))))))))
