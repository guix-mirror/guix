;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
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

(define-module (gnu build chromium-extension)
  #:use-module (gcrypt base16)
  #:use-module ((gcrypt hash) #:prefix hash:)
  #:use-module (ice-9 iconv)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system trivial)
  #:export (make-chromium-extension))

;;; Commentary:
;;;
;;; Tools to deal with Chromium extensions.
;;;
;;; Code:

(define (make-signing-key seed)
  "Return a derivation for a deterministic PKCS #8 private key using SEED."

  (define sha256sum
    (bytevector->base16-string (hash:sha256 (string->bytevector seed "UTF-8"))))

  ;; certtool.c wants a 56 byte seed for a 2048 bit key.
  (define size 2048)
  (define normalized-seed (string-take sha256sum 56))

  (computed-file (string-append seed "-signing-key.pem")
                 #~(system* #$(file-append gnutls "/bin/certtool")
                            "--generate-privkey"
                            "--key-type=rsa"
                            "--pkcs8"
                            ;; Use the provable FIPS-PUB186-4 algorithm for
                            ;; deterministic results.
                            "--provable"
                            "--password="
                            "--no-text"
                            (string-append "--bits=" #$(number->string size))
                            (string-append "--seed=" #$normalized-seed)
                            "--outfile" #$output)
                 #:local-build? #t))

(define* (make-crx signing-key package #:optional (package-output "out"))
  "Create a signed \".crx\" file from the unpacked Chromium extension residing
in PACKAGE-OUTPUT of PACKAGE.  The extension will be signed with SIGNING-KEY."
  (define name (package-name package))
  (define version (package-version package))

  (with-imported-modules '((guix build utils))
    (computed-file
     (string-append name "-" version ".crx")
     #~(begin
         ;; This is not great.  We pull Xorg and Chromium just to Zip and
         ;; sign an extension.  This should be implemented with something
         ;; lighter.  (TODO: where is the CRXv3 documentation..?)
         (use-modules (guix build utils))
         (let ((chromium #$(file-append ungoogled-chromium "/bin/chromium"))
               (xvfb #$(file-append xorg-server "/bin/Xvfb"))
               (packdir "/tmp/extension"))
           (mkdir-p (dirname packdir))
           (copy-recursively (ungexp package package-output) packdir)
           (system (string-append xvfb " :1 &"))
           (setenv "DISPLAY" ":1")
           (sleep 2)                    ;give Xorg some time to initialize...
           ;; Chromium stores the current time in the .crx Zip archive.
           ;; Use a fixed timestamp for deterministic behavior.
           ;; FIXME (core-updates): faketime is missing an absolute reference
           ;; to 'date', hence the need to set PATH.
           (setenv "PATH" #$(file-append coreutils "/bin"))
           (invoke #$(file-append libfaketime "/bin/faketime")
                   "2000-01-01 00:00:00"
                   chromium
                   "--user-data-dir=/tmp/signing-profile"
                   (string-append "--pack-extension=" packdir)
                   (string-append "--pack-extension-key=" #$signing-key))
           (copy-file (string-append packdir ".crx") #$output)))
     #:local-build? #t)))

(define* (crx->chromium-json crx version)
  "Return a derivation that creates a Chromium JSON settings file for the
extension given as CRX.  VERSION is used to signify the CRX version, and
must match the version listed in the extension manifest.json."
  ;; See chrome/browser/extensions/external_provider_impl.cc and
  ;; extensions/common/extension.h for documentation on the JSON format.
  (computed-file "extension.json"
                 #~(call-with-output-file #$output
                     (lambda (port)
                       (format port "{
  \"external_crx\": \"~a\",
  \"external_version\": \"~a\"
}
"
                               #$crx #$version)))
                 #:local-build? #t))


(define (signing-key->public-der key)
  "Return a derivation for a file containing the public key of KEY in DER
format."
  (computed-file "der"
                 #~(system* #$(file-append gnutls "/bin/certtool")
                            "--load-privkey" #$key
                            "--pubkey-info"
                            "--outfile" #$output
                            "--outder")
                 #:local-build? #t))

(define (chromium-json->profile-object json signing-key)
  "Return a derivation that installs JSON to the directory searched by
Chromium, using a file name (aka extension ID) derived from SIGNING-KEY."
  (define der (signing-key->public-der signing-key))

  (with-extensions (list guile-gcrypt)
    (with-imported-modules '((guix build utils))
      (computed-file
       "chromium-extension"
       #~(begin
           (use-modules (guix build utils)
                        (gcrypt base16)
                        (gcrypt hash))
           (define (base16-string->chromium-base16 str)
             ;; Translate STR, a hexadecimal string, to a Chromium-style
             ;; representation using the letters a-p (where a=0, p=15).
             (define s1 "0123456789abcdef")
             (define s2 "abcdefghijklmnop")
             (let loop ((chars (string->list str))
                        (converted '()))
               (if (null? chars)
                   (list->string (reverse converted))
                   (loop (cdr chars)
                         (cons (string-ref s2 (string-index s1 (car chars)))
                               converted)))))

           (let* ((checksum (bytevector->base16-string (file-sha256 #$der)))
                  (file-name (base16-string->chromium-base16
                              (string-take checksum 32)))
                  (extension-directory (string-append #$output
                                                      "/share/chromium/extensions")))
             (mkdir-p extension-directory)
             (symlink #$json (string-append extension-directory "/"
                                            file-name ".json"))))
       #:local-build? #t))))

(define* (make-chromium-extension p #:optional (output "out"))
  "Create a Chromium extension from package P and return a package that,
when installed, will make the extension contained in P available as a
Chromium browser extension.  OUTPUT specifies which output of P to use."
  (let* ((pname (package-name p))
         (version (package-version p))
         (signing-key (make-signing-key pname)))
    (package
      (inherit p)
      (name (string-append pname "-chromium"))
      (source #f)
      (build-system trivial-build-system)
      (native-inputs '())
      (inputs
       `(("extension" ,(chromium-json->profile-object
                        (crx->chromium-json (make-crx signing-key p output)
                                            version)
                        signing-key))))
      (propagated-inputs '())
      (outputs '("out"))
      (arguments
       '(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively (assoc-ref %build-inputs "extension")
                             (assoc-ref %outputs "out"))))))))
