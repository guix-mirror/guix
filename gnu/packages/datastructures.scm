;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages datastructures)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu))

(define-public gdsl
  (package
    (name "gdsl")
    (version "1.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.gna.org/gdsl/"
                                  "gdsl-" version ".tar.gz"))
              (sha256
               (base32
                "1v64jvlnj8jfpphphgjgb36p0kv50kwfyqncf0y12f16v8ydyiaw"))))
    (build-system gnu-build-system)
    (home-page "http://home.gna.org/gdsl/")
    (synopsis "Generic data structures library")
    (description "The Generic Data Structures Library (GDSL) is a collection
of routines for generic data structures manipulation.  It is a re-entrant
library fully written from scratch in pure ANSI C.  It is designed to offer
for C programmers common data structures with powerful algorithms, and hidden
implementation.  Available structures are lists, queues, stacks, hash tables,
binary trees, binary search trees, red-black trees, 2D arrays, permutations
and heaps.")
    (license license:gpl2+)))

(define-public marisa
  (package
    (name "marisa")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/s-yata/marisa-trie"
                           "/releases/download/v" version "/" name "-"
                           version ".tar.gz"))
       (sha256
        (base32 "19ifrcmnbr9whaaf4ly3s9ndyiq9sjqhnfkrxbz9zsb44w2n36hf"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/s-yata/marisa-trie")
    (synopsis "Trie data structure C++ library")
    (description "Matching Algorithm with Recursively Implemented
StorAge (MARISA) is a static and space-efficient trie data structure C++
library.")

    ;; Dual-licensed, according to docs/readme.en.html (source files lack
    ;; copyright/license headers.)
    (license (list license:bsd-2 license:lgpl2.1+))))

(define-public sparsehash
  (package
    (name "sparsehash")
    (version "2.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sparsehash/sparsehash.git")
                     (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m3f0cnpnpf6aak52wn8xbrrdw8p0yhq8csgc8nlvf9zp8c402na"))))
    (build-system gnu-build-system)
    (synopsis "Memory-efficient hashtable implementations")
    (description
     "This library contains several hash-map implementations, similar in API
to SGI's @code{hash_map} class, but with different performance
characteristics.  @code{sparse_hash_map} uses very little space overhead, 1-2
bits per entry.  @code{dense_hash_map} is very fast, particularly on lookup.
@code{sparse_hash_set} and @code{dense_hash_set} are the set versions of these
routines.  All these implementation use a hashtable with internal quadratic
probing.  This method is space-efficient -- there is no pointer overhead --
and time-efficient for good hash functions.")
    (home-page "https://github.com/sparsehash/sparsehash")
    (license license:bsd-3)))

(define-public ssdeep
  (package
    (name "ssdeep")
    (version "2.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ssdeep-project/ssdeep/"
                           "releases/download/release-" version "/"
                           "ssdeep-" version ".tar.gz"))
       (sha256
        (base32 "04qkjc6kksxkv7xbnk32rwmf3a8czdv2vvrdzfs0kw06h73snbpz"))))
    (build-system gnu-build-system)
    (home-page "https://ssdeep-project.github.io")
    (synopsis "Context-triggered piecewise hashing algorithm")
    (description "ssdeep computes and matches context triggered piecewise
hashes (CTPH), also called fuzzy checksums.  It can identify similar files
that have sequences of identical bytes in the same order, even though bytes
in between these sequences may be different in both content and length.")
    (license license:gpl2+)))

(define-public liburcu
  (package
    (name "liburcu")
    (version "0.11.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.lttng.org/files/urcu/"
                                  "userspace-rcu-" version ".tar.bz2"))
              (sha256
               (base32
                "0l1kxgzch4m8fxiz2hc8fwg56hrvzzspp7n0svnl7i7iycdrgfcj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)))                 ; for tests
    (home-page "https://liburcu.org/")
    (synopsis "User-space RCU data synchronisation library")
    (description "liburcu is a user-space @dfn{Read-Copy-Update} (RCU) data
synchronisation library.  It provides read-side access that scales linearly
with the number of cores.  liburcu-cds provides efficient data structures
based on RCU and lock-free algorithms.  These structures include hash tables,
queues, stacks, and doubly-linked lists.")
    (license license:lgpl2.1+)))

(define-public uthash
  (package
    (name "uthash")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/troydhanson/uthash.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0kslz8k6lssh7fl7ayzwlj62p0asxs3dq03357ls5ywjad238gqg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)))
    (arguments
     `(#:make-flags
       (list "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; nothing to configure
         (delete 'build)                ; nothing to build
         (replace 'check
           (lambda* (#:key make-flags #:allow-other-keys)
             (with-directory-excursion "tests"
               (apply invoke "make" make-flags))))
         (replace 'install
           ;; There is no top-level Makefile to do this for us.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name))
                    (include (string-append out "/include")))
               ;; Don't install HTML files: they're just the below .txt files
               ;; dolled up, can be stale, and regeneration requires asciidoc.
               (for-each (λ (file) (install-file file doc))
                         (find-files "doc" "\\.txt$"))
               (for-each (λ (file) (install-file file include))
                         (find-files "src" "\\.h$"))
               #t))))))
    (home-page "https://troydhanson.github.io/uthash/")
    (synopsis
     "Hash tables, lists, and other data structures implemented as C macros")
    (description
     "uthash implements a hash table and a few other basic data structures
as C preprocessor macros.  It aims to be minimalistic and efficient: it's
around 1,000 lines of code which, being macros, inline automatically.

Unlike function calls with fixed prototypes, macros operate on untyped
arguments.  Thus, they are able to work with any type of structure and key.
Any C structure can be stored in a hash table by adding @code{UT_hash_handle}
to the structure and choosing one or more fields to act as the key.")
    (license license:bsd-2)))

(define-public sdsl-lite
  (package
    (name "sdsl-lite")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/simongog/sdsl-lite/"
                                  "releases/download/v" version "/"
                                  "sdsl-lite-" version
                                  ".tar.gz.offline.install.gz"))
              (sha256
               (base32
                "1v86ivv3mmdy802i9xkjpxb4cggj3s27wb19ja4sw1klnivjj69g"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "external") #t))
              (patches
                (list (origin
                        (method url-fetch)
                        (uri "https://salsa.debian.org/science-team/libsdsl/raw/debian/2.1.1+dfsg-2/debian/patches/0001-Patch-cmake-files.patch")
                        (file-name "sdsl-lite-dont-use-bundled-libraries.patch")
                        (sha256
                         (base32
                          "0m542xpys54bni29zibgrfpgpd0zgyny4h131virxsanixsbz52z")))))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-file "lib/libsdsl_static.a"
                          (string-append out "/lib/libsdsl.a")))
             #t)))))
    (native-inputs
     `(("libdivsufsort" ,libdivsufsort)))
    (home-page "https://github.com/simongog/sdsl-lite")
    (synopsis "Succinct data structure library")
    (description "The Succinct Data Structure Library (SDSL) is a powerful and
flexible C++11 library implementing succinct data structures.  In total, the
library contains the highlights of 40 research publications.  Succinct data
structures can represent an object (such as a bitvector or a tree) in space
close to the information-theoretic lower bound of the object while supporting
operations of the original object efficiently.  The theoretical time
complexity of an operation performed on the classical data structure and the
equivalent succinct data structure are (most of the time) identical.")
    (license license:gpl3+)))

(define-public libdivsufsort
  (package
    (name "libdivsufsort")
    (version "2.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/y-256/libdivsufsort.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fgdz9fzihlvjjrxy01md1bv9vh12rkgkwbm90b1hj5xpbaqp7z2"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; there are no tests
       #:configure-flags
       ;; Needed for rapmap and sailfish.
       '("-DBUILD_DIVSUFSORT64=ON")))
    (home-page "https://github.com/y-256/libdivsufsort")
    (synopsis "Lightweight suffix-sorting library")
    (description "libdivsufsort is a software library that implements a
lightweight suffix array construction algorithm.  This library provides a
simple and an efficient C API to construct a suffix array and a
Burrows-Wheeler transformed string from a given string over a constant-size
alphabet.  The algorithm runs in O(n log n) worst-case time using only 5n+O(1)
bytes of memory space, where n is the length of the string.")
    (license license:expat)))
