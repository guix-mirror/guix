;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages debian)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gnupg))

(define-public debian-archive-keyring
  (package
    (name "debian-archive-keyring")
    (version "2017.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://debian/pool/main/d/" name "/"
                            name "_" version ".tar.xz"))
        (sha256
         (base32
          "1pdwgipfi0y4svhxlw8arhq792f1g3vlmw4raphizy7sa65vd4ca"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "verify-results"
       #:parallel-build? #f ; has race conditions
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apt (string-append out "/etc/apt/trusted.gpg.d/"))
                    (key (string-append out "/share/keyrings/")))
               (install-file "keyrings/debian-archive-keyring.gpg" key)
               (install-file "keyrings/debian-archive-removed-keys.gpg" key)
               (for-each (lambda (file)
                           (install-file file apt))
                         (find-files "trusted.gpg" "\\.gpg$")))
             #t)))))
    (native-inputs
     `(("gnupg" ,gnupg)
       ("jetring" ,jetring)))
    (home-page "https://packages.qa.debian.org/d/debian-archive-keyring.html")
    (synopsis "GnuPG archive keys of the Debian archive")
    (description
     "The Debian project digitally signs its Release files.  This package
contains the archive keys used for that.")
    (license (list license:public-domain ; the keys
                   license:gpl2+)))) ; see debian/copyright
