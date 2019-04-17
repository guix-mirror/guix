;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages boost)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages shells))

(define-public boost
  (package
    (name "boost")
    (version "1.70.0")
    (source (origin
              (method url-fetch)
              (uri (let ((version-with-underscores
                          (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)))
                     (list (string-append "mirror://sourceforge/boost/boost/" version
                                          "/boost_" version-with-underscores ".tar.bz2")
                           (string-append "https://dl.bintray.com/boostorg/release/"
                                          version "/source/boost_"
                                          version-with-underscores ".tar.bz2"))))
              (sha256
               (base32
                "0y47nc7w0arwgj4x1phadxbvl7wyfcgknbz5kv8lzpl98wsyh2j3"))
              (patches (search-patches "boost-fix-icu-build.patch"))))
    (build-system gnu-build-system)
    (inputs `(("icu4c" ,icu4c)
              ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)
       ("tcsh" ,tcsh)))
    (arguments
     `(#:tests? #f
       #:make-flags
       (list "threading=multi" "link=shared"
             "cxxflags=-std=c++14"

             ;; Set the RUNPATH to $libdir so that the libs find each other.
             (string-append "linkflags=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((icu (assoc-ref inputs "icu4c"))
                   (out (assoc-ref outputs "out")))
               (substitute* '("libs/config/configure"
                              "libs/spirit/classic/phoenix/test/runtest.sh"
                              "tools/build/src/engine/execunix.c"
                              "tools/build/src/engine/Jambase"
                              "tools/build/src/engine/jambase.c")
                 (("/bin/sh") (which "sh")))

               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))

               (invoke "./bootstrap.sh"
                       (string-append "--prefix=" out)
                       ;; Auto-detection looks for ICU only in traditional
                       ;; install locations.
                       (string-append "--with-icu=" icu)
                       "--with-toolset=gcc"))))
         (replace 'build
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "./b2"
                    (format #f "-j~a" (parallel-job-count))
                    make-flags)))
         (replace 'install
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "./b2" "install" make-flags)))
         (add-after 'install 'provide-libboost_python
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Boost can build support for both Python 2 and Python 3 since
               ;; version 1.67.0, and suffixes each library with the Python
               ;; version.  Many consumers only check for libboost_python
               ;; however, so we provide it here as suggested in
               ;; <https://github.com/boostorg/python/issues/203>.
               (with-directory-excursion (string-append out "/lib")
                 (symlink "libboost_python27.so" "libboost_python.so"))
               #t))))))

    (home-page "https://www.boost.org")
    (synopsis "Peer-reviewed portable C++ source libraries")
    (description
     "A collection of libraries intended to be widely useful, and usable
across a broad spectrum of applications.")
    (license (license:x11-style "https://www.boost.org/LICENSE_1_0.txt"
                                "Some components have other similar licences."))))

(define-public boost-for-mysql
  ;; Older version for MySQL 5.7.23.
  (package
    (inherit boost)
    (version "1.59.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost/" version "/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "1jj1aai5rdmd72g90a3pd8sw9vi32zad46xv5av8fhnr48ir6ykj"))))
    (arguments (substitute-keyword-arguments (package-arguments boost)
      ((#:phases phases)
       `(modify-phases ,phases
          ;; This was removed after boost-1.67.
          (add-before 'configure 'more-bin-sh-patching
            (lambda _
              (substitute* "tools/build/doc/bjam.qbk"
                (("/bin/sh") (which "sh")))))
          (delete 'provide-libboost_python)))))
    (properties '((hidden? . #t)))))

(define-public boost-sync
  (let ((commit "c72891d9b90e2ceb466ec859f640cd012b2d8709")
        (version "1.55")
        (revision "1"))
    (package
      (name "boost-sync")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/boostorg/sync.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "197mp5z048vz5kv1m4v3jm447l2gqsyv0rbfz11dz0ns343ihbyx"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let ((source (assoc-ref %build-inputs "source")))
             (copy-recursively (string-append source "/include")
                               (string-append %output "/include"))))))
      (home-page "https://github.com/boostorg/sync")
      (synopsis "Boost.Sync library")
      (description "The Boost.Sync library provides mutexes, semaphores, locks
and events and other thread related facilities.  Boost.Sync originated from
Boost.Thread.")
      (license (license:x11-style "https://www.boost.org/LICENSE_1_0.txt")))))

(define-public boost-signals2
  (package
    (name "boost-signals2")
    (version (package-version boost))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/boostorg/signals2.git")
                    (commit (string-append "boost-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nayaqshhzr1n6jj43bpvvay36d5gn075h0b95psii5x8ingszdk"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source (assoc-ref %build-inputs "source")))
           (copy-recursively (string-append source "/include")
                             (string-append %output "/include"))))))
    (home-page "https://github.com/boostorg/signals2")
    (synopsis "Boost.Signals2 library")
    (description "The Boost.Signals2 library is an implementation of a managed
signals and slots system.")
    (license (license:x11-style "https://www.boost.org/LICENSE_1_0.txt"))))

(define-public mdds
  (package
    (name "mdds")
    (version "1.4.3")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://kohei.us/files/mdds/src/mdds-" version ".tar.bz2"))
             (sha256
              (base32
               "10cw6irdm6d15nxnys2v5akp8yz52qijpcjvw0frwq7nz5d3vki5"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("boost" ,boost))) ; inclusion of header files
    (home-page "https://gitlab.com/mdds/mdds")
    (synopsis "Multi-dimensional C++ data structures and indexing algorithms")
    (description "Mdds (multi-dimensional data structure) provides a
collection of multi-dimensional data structures and indexing algorithms
for C++.  It includes flat segment trees, segment trees, rectangle sets,
point quad trees, multi-type vectors and multi-type matrices.")
    (license license:expat)))
