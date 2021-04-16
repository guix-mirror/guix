;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Lars-Dominik Braun <ldb@leibniz-psychology.org>
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

(define-module (gnu packages acl)
  #:use-module (guix licenses)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages perl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

(define-public acl
  (package
    (name "acl")
    (version "2.2.53")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://savannah/acl/acl-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1ir6my3w74s6nfbgbqgzj6w570sn0qjf3524zx8xh67lqrjrigh6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((ice-9 ftw)
                  ,@%gnu-build-system-modules)
       #:configure-flags '("--disable-static")
       #:tests? ,(not (or (%current-target-system)
                          (hurd-target?)))
       #:phases
       (modify-phases %standard-phases
         ;; XXX After repacking the sources the timestamps are reset to the
         ;; epoch, which leads to a failure in gzipping the CHANGES file.
         (add-after 'unpack 'ensure-no-mtimes-pre-1980
           (lambda _
             (let ((early-1980 315619200)) ; 1980-01-02 UTC
               (ftw "." (lambda (file stat flag)
                          (unless (<= early-1980 (stat:mtime stat))
                            (utime file early-1980 early-1980))
                          #t))
               #t)))
         (add-after 'build 'patch-exec-bin-sh
           (lambda _
             (substitute* "test/run"
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'check 'patch-tests
           (lambda _
             ;; The coreutils do not have an ACL bit to remove from their
             ;; output, so the sed expression that removes the bit is disabled.
             (substitute* "test/sbits-restore.test"
                          (("\\| sed.*'") ""))
             ;; These tests require the existence of a user named "bin", but
             ;; this user does not exist within Guix's build environment.
             (substitute* "Makefile.in"
               ((".*test/misc\\.test.*") "")
               ((".*test/cp\\.test.*") "")
               ((".*test/setfacl-X\\.test.*") ""))
             #t)))))
    (inputs `(("attr" ,attr)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("perl" ,perl)))
    (home-page "https://savannah.nongnu.org/projects/acl")
    (synopsis
     "Library and tools for manipulating access control lists")
    (description
     "Library and tools for manipulating access control lists.")
    (license (list gpl2+ lgpl2.1+))))

(define-public python-pylibacl
  (package
    (name "python-pylibacl")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pylibacl" version))
        (sha256
          (base32
            "1zyrk2m20p5b6bdwxhrwib273i6i71zyr5hzssbxfqis5qra9848"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-tests
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; These tests operate on real files, but our tempfs does not support
             ;; ACLs
             (substitute* "tests/test_acls.py"
               (("( *)def test_applyto(_extended(_mixed)?)?" match indent)
                (string-append indent "@pytest.mark.skip(reason=\"guix\")\n" match)))
             #t))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "tests"))
               #t)))))
    (inputs `(("acl" ,acl)))
    (native-inputs `(("python-pytest" ,python-pytest)))
    (home-page "https://pylibacl.k1024.org/")
    (synopsis "POSIX.1e ACLs for python")
    (description "Python 3.4+ extension module that allows you to manipulate
the POSIX.1e Access Control Lists present in some OS/file-systems
combinations.")
    (license lgpl2.1+)))
