;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages package-management)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages compression) #:select (bzip2 gzip))
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages pkg-config))

(define-public guix
  (package
    (name "guix")
    (version "0.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://alpha.gnu.org/gnu/guix/guix-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0xpfdmlfkkpmgrb8lpaqs5wxx31m4jslajs6b9waz5wp91zk7fix"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list
                          "--localstatedir=/var"
                          (string-append "--with-libgcrypt-prefix="
                                         (assoc-ref %build-inputs
                                                    "libgcrypt")))
       #:phases (alist-cons-before
                 'configure 'copy-bootstrap-guile
                 (lambda* (#:key system inputs #:allow-other-keys)
                   (define (copy arch)
                     (let ((guile  (assoc-ref inputs
                                              (string-append "boot-guile/"
                                                             arch)))
                           (target (string-append "gnu/packages/bootstrap/"
                                                  arch "-linux/"
                                                  "/guile-2.0.7.tar.xz")))
                       (copy-file guile target)))

                   (copy "i686")
                   (copy "x86_64")
                   #t)
                 %standard-phases)))
    (inputs
     (let ((boot-guile (lambda (arch hash)
                         (origin
                          (method url-fetch)
                          (uri (string-append
                                "http://alpha.gnu.org/gnu/guix/bootstrap/"
                                arch "-linux"
                                "/20130105/guile-2.0.7.tar.xz"))
                          (sha256 hash)))))
       `(("bzip2" ,bzip2)
         ("gzip" ,gzip)

         ("sqlite" ,sqlite)
         ("libgcrypt" ,libgcrypt)
         ("guile" ,guile-2.0)
         ("pkg-config" ,pkg-config)

         ("boot-guile/i686"
          ,(boot-guile "i686"
                       (base32
                        "0z11rlyclnh9palrsk0xhgm84rmvzza0gkwvlsiazsjnqpscd9zr")))
         ("boot-guile/x86_64"
          ,(boot-guile "x86_64"
                       (base32
                        "0b5a2ngd9a7z2wnm01wc27rlwb61x854ndadxwmj8v8lrl6j2hxw"))))))
    (home-page "http://www.gnu.org/software/guix")
    (synopsis "Managing installed software packages and versions")
    (description
     "GNU Guix is a purely functional package manager for the GNU system,
and a distribution thereof.

In addition to standard package management features, Guix supports
transactional upgrades and roll-backs, unprivileged package management,
per-user profiles, and garbage collection.

It provides Guile Scheme APIs, including high-level embedded domain-specific
languages (EDSLs), to describe how packages are built and composed.

A user-land free software distribution for GNU/Linux comes as part of Guix.

Guix is based on the Nix package manager.")
    (license gpl3+)))

(define-public guix-0.4
  ;; XXX: Hack to allow the use of a 0.4ish tarball.  This assumes that you
  ;; have run 'make dist' in your build tree.  Remove when 0.4 is out.
  (let* ((builddir (dirname
                    (canonicalize-path
                     (dirname (search-path %load-path
                                           "guix/config.scm")))))
         (tarball  (string-append builddir "/guix-0.4.tar.gz")))
    (package (inherit guix)
      (version "0.4rc")
      (source (if (file-exists? tarball)
                  tarball
                  (package-source guix))))))
