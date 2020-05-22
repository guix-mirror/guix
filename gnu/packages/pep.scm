;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages pep)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mail) ; for libetpan
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sequoia)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public yml2
  (package
    (name "yml2")
    (version "2.6.3")
    (source (origin
       (method hg-fetch)
       (uri (hg-reference
             (url "https://pep.foundation/dev/repos/yml2")
             (changeset version)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "10jjjyq1mz18zkzvxd62aba00h69gd9cglisqcvb81j67ml2v1bx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-lxml" ,python-lxml)))
    (home-page "https://fdik.org/yml/")
    (synopsis "Use a Domain Specific Language for XML without defining
a grammar")
    (description "The YML compiler is a small Python script.  It
provides the command line front end yml2c.  As default, it compiles
your script and outputs to stdout, that usually is the terminal.  Your
shell provides options to redirect the output into a pipe or a file.")
    (license license:gpl2)))

(define fdik-libetpan
  ;; pEp Engine requires libetpan with a set of patches that have not been
  ;; upstreamed yet.
  (let ((commit "210ba2b3b310b8b7a6ee4a4e35e50f7fa379643f") ; 2020-06-03
        (checksum "00000nij3ray7nssvq0lzb352wmnab8ffzk7dgff2c68mvjbh1l6")
        (revision "5"))
   (package
    (inherit libetpan)
    (name "fdik-libetpan")
    (version (string-append "1.6-" revision "." (string-take commit 8)))
    (source
     (origin
       (inherit (package-source libetpan))
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fdik/libetpan")
             (commit commit)))
       (file-name (string-append name "-" version))
       (sha256 (base32 checksum)))))))

(define sequoia4pEp
  ;; Currently pEp Engine requires sequoia in not-so-current version
  (package/inherit sequoia
    (name "sequoia")
    (version "0.15.0-pEp")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/sequoia-pgp/sequoia.git")
             (commit "0eb1b6cd846ea8c36b3dfdf01ec88383fc64f2fe")))
       (sha256
        (base32 "06dqs9whwp9lfibwp8dqm0aw4nm3s3v4jp2n4fz51zcvsld40nfh"))
       (file-name (git-file-name name version))))))

(define-public pep-engine
  (package
    (name "pep-engine")
    (version "2.0.6")
    (source
     (origin
       (method hg-fetch)
       (uri (hg-reference
             (url "https://pep.foundation/dev/repos/pEpEngine")
             (changeset "ebb62ba262dd"))) ;; r4721
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "0ljf79j4ng7l8w6pbdcrfzb4yk51zslypvq0n72ib1d7grqvnagi"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; pEpEngie does not use autotools and configure,
           ;; but a local.conf. We need to tweak the values there.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (yml2 (assoc-ref inputs "yml2")))
               (with-output-to-file "local.conf"
                 (lambda ()
                   (format #t "
PREFIX=~a
PER_MACHINE_DIRECTORY=${PREFIX}/share/pEp
SYSTEM_DB=~a/share/pEp/system.db
ASN1C=~a
YML2_PATH=~a
OPENPGP=SEQUOIA
"
                           out out (which "asn1c")
                           (string-append yml2 "/bin"))))
               #t)))
         (delete 'check)
         (add-after 'install 'install-db
           (lambda _
             (invoke "make" "-C" "db" "install"))))))
    (native-inputs
     `(("asn1c" ,asn1c) ; >= 0.9.27
       ("pkg-config" ,pkg-config)
       ("yml2" ,yml2)))
    (inputs
     `(("libetpan" ,fdik-libetpan)
       ("libiconv" ,libiconv)
       ("nettle" ,nettle)
       ("openssl" ,openssl)
       ("sequoia" ,sequoia4pEp)
       ("sqlite3" ,sqlite)
       ("util-linux" ,util-linux "lib"))) ;; uuid.h
    (home-page "https://pep.foundation/")
    (synopsis "Library for automatic key management and encryption of
messages")
    (description "The p≡p engine is the core part of p≡p (pretty Easy
privacy).")
    (license ;; code: GPL 3, docs: CC-BY-SA
     (list license:gpl3 license:cc-by-sa3.0))))

(define-public libpepadapter
  (package
    (name "libpepadapter")
    (version "2.0.2")
    (source
     (origin
       (method hg-fetch)
       (uri (hg-reference
             (url "https://pep.foundation/dev/repos/libpEpAdapter")
             (changeset "e8fe371c870a"))) ;; r168
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "1mlpavjbnmslvmr5jxcvpjgb2x40nhmxjb10hza3kn4qzj0k1pjz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:tests? #f ;; building the tests fails
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; libpEpAdapter does not use autotools and configure,
           ;; but a local.conf. We need to tweak the values there.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (engine (assoc-ref inputs "pep-engine")))
               (with-output-to-file "local.conf"
                 (lambda _ ;()
                   (format #t "
PREFIX=~a
ENGINE_LIB_PATH=~a/lib
ENGINE_INC_PATH=~a/include
" out engine engine))))
             #t)))))
    (inputs
     `(("pep-engine" ,pep-engine)))
    (home-page "https://pep.foundation/")
    (synopsis "Library for building p≡p adapters")
    (description "This C++ library provides common structures used in p≡p
(pretty Easy privacy) adapters.")
    (license license:bsd-3)))
