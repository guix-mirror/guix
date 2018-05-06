;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
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
    (version "1.66.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost/" version "/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "1aaw48cmimsskzgiclwn0iifp62a5iw9cbqrhfari876af1828ap"))
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

             ;; Set the RUNPATH to $libdir so that the libs find each other.
             (string-append "linkflags=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib")

             ;; Boost's 'context' library is not yet supported on mips64, so
             ;; we disable it.  The 'coroutine' library depends on 'context',
             ;; so we disable that too.
             ,@(if (string-prefix? "mips64" (or (%current-target-system)
                                                (%current-system)))
                   '("--without-context"
                     "--without-coroutine" "--without-coroutine2")
                   '()))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((icu (assoc-ref inputs "icu4c"))
                   (out (assoc-ref outputs "out")))
               (substitute* '("libs/config/configure"
                              "libs/spirit/classic/phoenix/test/runtest.sh"
                              "tools/build/doc/bjam.qbk"
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
             (apply invoke "./b2" "install" make-flags))))))

    (home-page "https://www.boost.org")
    (synopsis "Peer-reviewed portable C++ source libraries")
    (description
     "A collection of libraries intended to be widely useful, and usable
across a broad spectrum of applications.")
    (license (license:x11-style "https://www.boost.org/LICENSE_1_0.txt"
                                "Some components have other similar licences."))))

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

(define-public mdds
  (package
    (name "mdds")
    (version "1.3.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://kohei.us/files/mdds/src/mdds-" version ".tar.bz2"))
             (sha256
              (base32
               "18g511z1lgfxrga2ld9yr95phmyfbd3ymbv4q5g5lyjn4ljcvf6w"))))
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
