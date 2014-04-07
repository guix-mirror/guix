;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <dthompson2@worcester.edu>
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

(define-module (gnu packages man)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gdbm)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages less)
  #:use-module (gnu packages lynx)
  #:use-module (gnu packages pkg-config))

(define-public libpipeline
  (package
    (name "libpipeline")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://savannah/libpipeline/libpipeline-"
                    version ".tar.gz"))
              (sha256
               (base32
                "12d6ldcj7kv2nv832b23v97g7035d0ybq0ig7h0yr7xk9czd3z7i"))))
    (build-system gnu-build-system)
    (home-page "http://libpipeline.nongnu.org/")
    (synopsis "C library for manipulating pipelines of subprocesses")
    (description
     "libpipeline is a C library for manipulating pipelines of subprocesses in
a flexible and convenient way.")
    (license gpl3+)))

(define-public man-db
  (package
    (name "man-db")
    (version "2.6.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/man-db/man-db-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1hv6byj6sg6cp3jyf08gbmdm4pwhvd5hzmb94xl0w7prin6hzabx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (alist-cons-after
        'patch-source-shebangs 'patch-test-shebangs
        (lambda* (#:key outputs #:allow-other-keys)
          ;; Patch shebangs in test scripts.
          (let ((out (assoc-ref outputs "out")))
            (for-each (lambda (file)
                        (substitute* file
                          (("#! /bin/sh")
                           (string-append "#!" (which "sh")))))
                      (remove file-is-directory?
                              (find-files "src/tests" ".*")))))
        %standard-phases)
       #:configure-flags
       (let ((groff (assoc-ref %build-inputs "groff"))
             (less  (assoc-ref %build-inputs "less"))
             (gzip  (assoc-ref %build-inputs "gzip"))
             (bzip2  (assoc-ref %build-inputs "bzip2"))
             (xz  (assoc-ref %build-inputs "xz")))
         ;; Invoke groff, less, gzip, bzip2, and xz directly from the store.
         (append (list "--disable-setuid" ;; Disable setuid man user.
                       (string-append "--with-pager=" less "/bin/less")
                       (string-append "--with-gzip=" gzip "/bin/gzip")
                       (string-append "--with-bzip2=" bzip2 "/bin/gzip")
                       (string-append "--with-xz=" xz "/bin/xz"))
                 (map (lambda (prog)
                        (string-append "--with-" prog "=" groff "/bin/" prog))
                      '("nroff" "eqn" "neqn" "tbl" "refer" "pic"))))
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("flex" ,flex)
       ("gdbm" ,gdbm)
       ("groff" ,groff)
       ("less" ,less)
       ("libpipeline" ,libpipeline)))
    (home-page "http://man-db.nongnu.org/")
    (synopsis "Standard Unix documentation system")
    (description
     "Man-db is an implementation of the standard Unix documentation system
accessed using the man command.  It uses a Berkeley DB database in place of
the traditional flat-text whatis databases.")
    (license gpl2+)))
