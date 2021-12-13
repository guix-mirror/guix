;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages synergy)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (gpl2))
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-26))

(define-public synergy
  (package
    (name "synergy")
    (version "1.11.1")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/symless/synergy-core")
            (commit (string-append "v" version "-stable"))))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "0dn0h3mdqy0mbg4yyhsh4rhvvsssqlknnln3naplc97my10lk2a0"))
      (modules '((guix build utils)))
      (snippet
       ;; Remove unnecessary bundled source and binaries
       '(begin
          (delete-file-recursively "ext/openssl")
          #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; there is no test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-headers
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (assoc-ref inputs "avahi")
                                    "/include/avahi-compat-libdns_sd:"
                                    (or (getenv "CPLUS_INCLUDE_PATH") "")))
             #t))
         (add-after 'install 'patch-desktop
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (string-append out "/share/applications/synergy.desktop")
                 (("/usr") out))
               #t))))))
    (native-inputs
     (list qttools))           ; for Qt5LinguistTools
    (inputs
     `(("avahi" ,avahi)
       ("python"  ,python-wrapper)
       ("openssl" ,openssl)
       ("curl"    ,curl)
       ("libxi"   ,libxi)
       ("libx11"  ,libx11)
       ("libxtst" ,libxtst)
       ("qtbase" ,qtbase-5)))
    (home-page "https://symless.com/synergy")
    (synopsis "Mouse and keyboard sharing utility")
    (description
     "Synergy brings your computers together in one cohesive experience; it's
software for sharing one mouse and keyboard between multiple computers on your
desk.")
    (license gpl2)))
