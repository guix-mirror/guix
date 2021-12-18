;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
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
  #:use-module (guix gexp)
  #:use-module (guix packages)
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
  (computed-file
   (string-append seed "-signing-key.pem")
   (with-extensions (list guile-gcrypt)
     #~(begin
         (use-modules (gcrypt base16) (gcrypt hash) (ice-9 iconv))
         (let* ((sha256sum (bytevector->base16-string
                            (sha256 (string->bytevector #$seed "UTF-8"))))
                ;; certtool.c wants a 56 byte seed for a 2048 bit key.
                (key-size 2048)
                (normalized-seed (string-take sha256sum 56)))

           (system* #$(file-append gnutls "/bin/certtool")
                    "--generate-privkey"
                    "--key-type=rsa"
                    "--pkcs8"
                    ;; Use the provable FIPS-PUB186-4 algorithm for
                    ;; deterministic results.
                    "--provable"
                    "--password="
                    "--no-text"
                    (string-append "--bits=" (number->string key-size))
                    (string-append "--seed=" normalized-seed)
                    "--outfile" #$output))))
   #:local-build? #t))

(define* (make-crx signing-key package #:optional (package-output "out"))
  "Create a signed \".crx\" file from the unpacked Chromium extension residing
in PACKAGE-OUTPUT of PACKAGE.  The extension will be signed with SIGNING-KEY."
  (define name (package-name package))
  (define version (package-version package))

  (computed-file
   (string-append name "-" version ".crx")
   (with-imported-modules '((guix build utils))
     #~(begin
         ;; This is not great.  We pull Xorg and Chromium just to Zip and
         ;; sign an extension.  This should be implemented with something
         ;; lighter.  (TODO: where is the CRXv3 documentation..?)
         (use-modules (guix build utils))
         (let ((chromium #$(file-append ungoogled-chromium "/bin/chromium"))
               (xvfb #$(file-append xorg-server "/bin/Xvfb"))
               (packdir (string-append (getcwd) "/extension")))
           (mkdir packdir)
           (copy-recursively (ungexp package package-output) packdir
                             ;; Ensure consistent file modification times.
                             #:keep-mtime? #t)
           (system (string-append xvfb " :1 &"))
           (setenv "DISPLAY" ":1")
           (sleep 2)                    ;give Xorg some time to initialize...
           (invoke chromium
                   "--user-data-dir=chromium-profile"
                   (string-append "--pack-extension=" packdir)
                   (string-append "--pack-extension-key=" #$signing-key))
           (copy-file (string-append packdir ".crx") #$output))))
   #:local-build? #t))

(define (crx->chromium-json crx version)
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

(define (file-sha256sum file)
  (with-extensions (list guile-gcrypt)
    #~(begin
        (use-modules (gcrypt base16) (gcrypt hash))
        (bytevector->base16-string (file-sha256 #$file)))))

(define* (make-chromium-extension pkg #:optional (pkg-output "out"))
  "Create a Chromium extension from package PKG and return a package that,
when installed, will make the extension contained in PKG available as a
Chromium browser extension.  PKG-OUTPUT specifies which output of PKG to use."
  (let* ((name (package-name pkg))
         (version (package-version pkg))
         (private-key (make-signing-key name))
         (public-key (signing-key->public-der private-key))
         (checksum (file-sha256sum public-key))
         (crx (make-crx private-key pkg pkg-output))
         (json (crx->chromium-json crx version)))
    (package
      (inherit pkg)
      (name (string-append name "-chromium"))
      (source #f)
      (native-inputs '())
      (inputs '())
      (propagated-inputs '())
      (outputs '("out"))
      (build-system trivial-build-system)
      (arguments
       (list #:modules '((guix build utils))
             #:builder
             #~(begin
                 (use-modules (guix build utils))
                 (define (base16-char->chromium-base16 char)
                   ;; Translate CHAR, a hexadecimal character, to a Chromium-style
                   ;; representation using the letters a-p (where a=0, p=15).
                   (string-ref "abcdefghijklmnop"
                               (string-index "0123456789abcdef" char)))
                 (let ((file-name (string-map base16-char->chromium-base16
                                              (string-take #$checksum 32)))
                       (extension-directory
                        (string-append #$output
                                       "/share/chromium/extensions")))
                   (mkdir-p extension-directory)
                   (symlink #$json (string-append extension-directory "/"
                                                  file-name ".json")))))))))
