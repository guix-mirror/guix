;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages cryptsetup)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux))

(define-public cryptsetup
  (package
   (name "cryptsetup")
   (version "1.7.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://kernel.org/linux/utils/cryptsetup/v"
                                (version-major+minor version)
                                "/cryptsetup-" version ".tar.xz"))
            (sha256
             (base32
              "1gail831j826lmpdx2gsc83lp3br6wfnwh3vqwxaa1nn1lfwsc1b"))))
   (build-system gnu-build-system)
   (inputs
    `(("libgcrypt" ,libgcrypt)
      ("lvm2" ,lvm2)
      ("util-linux" ,util-linux)
      ("popt" ,popt)))
   (native-inputs
    `(("python" ,python-wrapper)))
   (synopsis "Hard disk encryption tool")
   (description
    "LUKS (Linux Unified Key Setup)/Cryptsetup provides a standard on-disk
encryption format, which does not only facilitate compatibility among
distributions, but which also provides secure management of multiple user
passwords.  In contrast to existing solutions, LUKS stores all setup necessary
setup information in the partition header, enabling the users to transport
or migrate their data seamlessly.")
   (license license:gpl2)
   (home-page "https://gitlab.com/cryptsetup/cryptsetup")))

(define (static-library library)
  "Return a variant of package LIBRARY that provides static libraries ('.a'
files).  This assumes LIBRARY uses Libtool."
  (package
    (inherit library)
    (name (string-append (package-name library) "-static"))
    (arguments
     (substitute-keyword-arguments (package-arguments library)
       ((#:configure-flags flags ''())
        `(append '("--disable-shared" "--enable-static")
                 ,flags))))))

(define-public cryptsetup-static
  ;; Stripped-down statically-linked 'cryptsetup' command for use in initrds.
  (package
    (inherit cryptsetup)
    (name "cryptsetup-static")
    (arguments
     '(#:configure-flags '("--disable-shared"
                           "--enable-static-cryptsetup"

                           ;; 'libdevmapper.a' pulls in libpthread, libudev and libm.
                           "LIBS=-ludev -pthread -lm")

       #:allowed-references ()                  ;this should be self-contained

       #:modules ((ice-9 ftw)
                  (ice-9 match)
                  (guix build utils)
                  (guix build gnu-build-system))

       #:phases (modify-phases %standard-phases
                  (add-after 'install 'remove-cruft
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Remove everything except the 'cryptsetup' command and
                      ;; its friend.
                      (let ((out (assoc-ref outputs "out")))
                        (with-directory-excursion out
                          (let ((dirs (scandir "."
                                               (match-lambda
                                                 ((or "." "..") #f)
                                                 (_ #t)))))
                            (for-each delete-file-recursively
                                      (delete "sbin" dirs))
                            (for-each (lambda (file)
                                        (rename-file (string-append file
                                                                    ".static")
                                                     file)
                                        (remove-store-references file))
                                      '("sbin/cryptsetup" "sbin/veritysetup"))
                            #t))))))))
    (inputs
     (let ((libgcrypt-static
            (package
              (inherit (static-library libgcrypt))
              (propagated-inputs
               `(("libgpg-error-host" ,(static-library libgpg-error)))))))
       `(("libgcrypt" ,libgcrypt-static)
         ("lvm2" ,lvm2-static)
         ("util-linux" ,util-linux "static")
         ("util-linux" ,util-linux)
         ("popt" ,popt))))
    (synopsis "Hard disk encryption tool (statically linked)")))
