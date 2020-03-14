;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public xxhash
  (package
    (name "xxhash")
    (version "0.7.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Cyan4973/xxHash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bin0jch6lbzl4f8y052a7azfgq2n7iwqihzgqmcccv5vq4vcx5a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             "XXH_FORCE_MEMORY_ACCESS=1" ; improved performance with GCC
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
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
