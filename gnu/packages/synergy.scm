;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zip)
  #:use-module (srfi srfi-26))

(define-public synergy
  (package
    (name "synergy")
    (version "1.7.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/synergy/synergy/archive/"
                          "v" version "-stable.tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "098y71fiw1n5i7g1p6vjfs5rz472j192p9izz2axxxhfvcyzrvx4"))
      (modules '((guix build utils)))
      (snippet
       ;; Remove ~14MB of unnecessary bundled source and binaries
       '(for-each delete-file-recursively
                  `("ext/bonjour"
                    "ext/LICENSE (OpenSSL)"
                    ,@(find-files "ext" "openssl-.*\\.tar\\.gz")
                    "ext/openssl-osx"
                    "ext/openssl-win32"
                    "ext/openssl-win64")))))
    (build-system cmake-build-system)
    (native-inputs `(("unzip" ,unzip)))
    (inputs
     `(("python"  ,python-wrapper)
       ("openssl" ,openssl)
       ("curl"    ,curl)
       ("libxi"   ,libxi)
       ("libx11"  ,libx11)
       ("libxtst" ,libxtst)
       ("xinput"  ,xinput)))
    (arguments
     `(#:phases
       (let ((srcdir (string-append "../synergy-" ,version "-stable")))
         (alist-cons-before
          'configure 'unpack-aux-src
          ;; TODO: package and use from system
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((unzip (string-append
                          (assoc-ref inputs "unzip")
                          "/bin/unzip")))
              (with-directory-excursion "ext"
                (for-each
                 (lambda (f)
                   (system* unzip "-d" f (string-append f ".zip")))
                 '("gmock-1.6.0" "gtest-1.6.0")))))
          (alist-replace
           'check
           ;; Don't run "integtests" as it requires network and X an display.
           (lambda _
             (zero? (system* (string-append srcdir "/bin/unittests"))))
           (alist-replace
            'install
            ;; There currently is no installation process, see:
            ;; http://synergy-project.org/spit/issues/details/3317/
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (ex  (string-append out "/share/doc/synergy-"
                                         ,version "/examples")))
                (begin
                  (mkdir-p bin)
                  (for-each
                   (lambda (f)
                     (copy-file (string-append srcdir "/bin/" f)
                                (string-append bin "/" f)))
                   '("synergyc" "synergys" "synergyd"
                     "usynergy" "syntool"))
                  ;; Install example configuration files
                  (mkdir-p ex)
                  (for-each
                   (lambda (e)
                     (copy-file (string-append srcdir "/doc/" e)
                                (string-append ex "/" e)))
                   '("synergy.conf.example"
                     "synergy.conf.example-advanced"
                     "synergy.conf.example-basic")))))
            %standard-phases))))))
    (home-page "http://www.synergy-project.org")
    (synopsis "Mouse and keyboard sharing utility")
    (description
     "Synergy brings your computers together in one cohesive experience; its
software for sharing one mouse and keyboard between multiple computers on your
desk.")
    (license gpl2)))
