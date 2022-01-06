;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages digest)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

(define-public wyhash
  (package
    (name "wyhash")
    (version "5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wangyi-fudan/wyhash")
                    (commit (string-append "wyhash_v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "03ljs5iw9zrm3bydwggjvpwrcwmsd75h3dv1j4am4hw3h22cjdjc"))))
    (build-system trivial-build-system) ;; source-only package
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (string-append (assoc-ref %outputs "out")))
                (include (string-append out "/include"))
                (doc (string-append out "/share/doc/" ,name "-" ,version))
                (source (assoc-ref %build-inputs "source")))
           (with-directory-excursion source
             (install-file "wyhash.h" include)
             (install-file "LICENSE" doc)
             (install-file "README.md" doc))
           #t))))
    (home-page "https://github.com/wangyi-fudan/wyhash")
    (synopsis "Embeddable hash function and random number generator")
    (description "This package provides a portable hash function and random
number generator suitable for use in data structures.  Provided by default in
Zig, V, and Nim programming language standard libraries.")
    (license license:unlicense)))

(define-public xxhash
  (package
    (name "xxhash")
    ;; XXX Remove the 'fix-man-page-links phase when updating.
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Cyan4973/xxHash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h6080lvcr5mpbvy4fhb4i7wvhpy72nrixk3djmpai4hxq41hsnr"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list #$(string-append "CC=" (cc-for-target))
                   #$(match (or (%current-target-system)
                                (%current-system))
                       ;; Detect vector instruction set at run time.
                       ((or "i686-linux" "x86_64-linux") "DISPATCH=1")
                       (_ "DISPATCH=0"))
                   "XXH_FORCE_MEMORY_ACCESS=1" ; improved performance with GCC
                   (string-append "prefix=" (assoc-ref %outputs "out")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-man-page-links
                 ;; https://github.com/Cyan4973/xxHash/issues/647
                 (lambda _
                   (substitute* "Makefile"
                     (("ln -sf \\$\\(MAN\\)")
                      "ln -sf xxhsum.1"))))
               (delete 'configure))))         ; no configure script
    (home-page "https://cyan4973.github.io/xxHash/")
    (synopsis "Extremely fast hash algorithm")
    (description
     "xxHash is an extremely fast non-cryptographic hash algorithm.  It works
at speeds close to RAM limits, and comes in both 32- and 64-bit flavours.
The code is highly portable, and hashes of the same length are identical on all
platforms (both big and little endian).")
    (license (list license:bsd-2        ; xxhash library (xxhash.[ch])
                   license:gpl2+))))    ; xxhsum.c

(define-public python-xxhash
  (package
    (name "python-xxhash")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xxhash" version))
       (sha256
        (base32
         "0jbvz19acznq00544gcsjg05fkvrmwbnwdfgrvwss3i1ys6avgmp"))))
    (build-system python-build-system)
    (home-page "https://github.com/ifduyue/python-xxhash")
    (synopsis "Python binding for xxHash")
    (description "This package provides Python bindings for the xxHash hash
algorithm.")
    (license license:bsd-3)))
