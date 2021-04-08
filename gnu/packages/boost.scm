;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2021 Franck Pérignon <franck.perignon@univ-grenoble-alpes.fr>
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
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages mpi)
  #:use-module (srfi srfi-1))

(define (version-with-underscores version)
  (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version))

(define (boost-patch name version hash)
  (origin
    (method url-fetch)
    (uri (string-append "https://www.boost.org/patches/"
                        (version-with-underscores version) "/" name))
    (file-name (string-append "boost-" name))
    (sha256 (base32 hash))))

(define-public boost
  (package
    (name "boost")
    (version "1.72.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dl.bintray.com/boostorg/release/"
                                  version "/source/boost_"
                                  (version-with-underscores version) ".tar.bz2"))
              (patches
               (list (boost-patch
                      ;; 1.72.0 was released with a faulty coroutine submodule:
                      ;; <https://github.com/boostorg/coroutine/issues/46>.
                      "0001-revert-cease-dependence-on-range.patch" version
                      "1zcqxzh56m1s635wqwk15j3zcs2gmjvjy2f0hid7i78s4pgm0yfs")))
              (sha256
               (base32
                "08h7cv61fd0lzb4z50xanfqn0pdgvizjrpd1kcdgj725pisb5jar"))))
    (build-system gnu-build-system)
    (inputs `(("icu4c" ,icu4c)
              ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ,@(if (%current-target-system)
             '()
             `(("python" ,python-wrapper)))
       ("tcsh" ,tcsh)))
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%gnu-build-system-modules)
       #:modules (((guix build python-build-system) #:select (python-version))
                  ,@%gnu-build-system-modules)
       #:tests? #f
       #:make-flags
       (list "threading=multi" "link=shared"

             ;; Set the RUNPATH to $libdir so that the libs find each other.
             (string-append "linkflags=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib")
             ,@(if (%current-target-system)
                   `("--user-config=user-config.jam"
                     ;; Python is not supported when cross-compiling.
                     "--without-python"
                     "binary-format=elf"
                     "target-os=linux"
                     ,@(cond
                        ((string-prefix? "arm" (%current-target-system))
                         '("abi=aapcs"
                           "address-model=32"
                           "architecture=arm"))
                        ((string-prefix? "aarch64" (%current-target-system))
                         '("abi=aapcs"
                           "address-model=64"
                           "architecture=arm"))
                        (else '())))
                   '()))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((icu (assoc-ref inputs "icu4c"))
                   (python (assoc-ref inputs "python"))
                   (out (assoc-ref outputs "out")))
               (substitute* '("libs/config/configure"
                              "libs/spirit/classic/phoenix/test/runtest.sh"
                              "tools/build/src/engine/execunix.cpp"
                              "tools/build/src/engine/Jambase")
                 (("/bin/sh") (which "sh")))

               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))

               ,@(if (%current-target-system)
                     `((call-with-output-file "user-config.jam"
                          (lambda (port)
                            (format port
                                    "using gcc : cross : ~a-c++ ;"
                                    ,(%current-target-system)))))
                     '())

               ;; Change an #ifdef __MACH__ that really targets macOS.
               ;; TODO: Inline this on the next rebuild cycle.
               ,@(if (hurd-target?)
                     '((substitute* "boost/test/utils/timer.hpp"
                         (("defined\\(__MACH__\\)")
                          "(defined __MACH__ && !defined __GNU__)"))
                       #t)
                     '())

               (invoke "./bootstrap.sh"
                       (string-append "--prefix=" out)
                       ;; Auto-detection looks for ICU only in traditional
                       ;; install locations.
                       (string-append "--with-icu=" icu)
                       ;; Ditto for Python.
                       ,@(if (%current-target-system)
                             '()
                             `((string-append "--with-python-root=" python)
                               (string-append "--with-python=" python "/bin/python")
                               (string-append "--with-python-version="
                                              (python-version python))))
                       "--with-toolset=gcc"))))
         (replace 'build
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "./b2"
                    (format #f "-j~a" (parallel-job-count))
                    make-flags)))
         (replace 'install
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "./b2" "install" make-flags)))
         ,@(if (%current-target-system)
               '()
               '((add-after 'install 'provide-libboost_python
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (python-version (python-version
                                              (assoc-ref inputs "python")))
                             (libboost_pythonNN.so
                              (string-append "libboost_python"
                                             (string-join (string-split
                                                           python-version #\.)
                                                          "")
                                             ".so")))
                        (with-directory-excursion (string-append out "/lib")
                          (symlink libboost_pythonNN.so "libboost_python.so")
                          ;; Some packages only look for the major version.
                          (symlink libboost_pythonNN.so
                                   (string-append "libboost_python"
                                                  (string-take python-version 1)
                                                  ".so")))
                        #t))))))))

    (home-page "https://www.boost.org")
    (synopsis "Peer-reviewed portable C++ source libraries")
    (description
     "A collection of libraries intended to be widely useful, and usable
across a broad spectrum of applications.")
    (license (license:x11-style "https://www.boost.org/LICENSE_1_0.txt"
                                "Some components have other similar licences."))))

(define-public boost-with-python2
  (package/inherit boost
    (name "boost-python2")
    (native-inputs
     `(("python" ,python-2)
       ,@(alist-delete "python" (package-native-inputs boost))))))

(define-public boost-with-python3
  (deprecated-package "boost-with-python3" boost))

(define-public boost-static
  (package
    (inherit boost)
    (name "boost-static")
    (arguments
     (substitute-keyword-arguments (package-arguments boost)
       ((#:make-flags flags)
        `(cons "link=static" (delete "link=shared" ,flags)))
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'provide-libboost_python
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (python-version (python-version
                                       (assoc-ref inputs "python")))
                      (libboost_pythonNN.a
                       (string-append "libboost_python"
                                      (string-join (string-split
                                                    python-version #\.)
                                                   "")
                                      ".a")))
                 (with-directory-excursion (string-append out "/lib")
                   (symlink libboost_pythonNN.a "libboost_python.a"))
                 #t)))))))))

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
          (replace 'configure
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((icu (assoc-ref inputs "icu4c"))
                    (out (assoc-ref outputs "out")))
                (substitute* (append
                               (find-files "tools/build/src/engine/" "execunix\\.c.*")
                               '("libs/config/configure"
                                 "libs/spirit/classic/phoenix/test/runtest.sh"
                                 "tools/build/doc/bjam.qbk"
                                 "tools/build/src/engine/Jambase"))
                  (("/bin/sh") (which "sh")))

                (setenv "SHELL" (which "sh"))
                (setenv "CONFIG_SHELL" (which "sh"))

                ,@(if (%current-target-system)
                    `((call-with-output-file "user-config.jam"
                        (lambda (port)
                          (format port
                                  "using gcc : cross : ~a-c++ ;"
                                  ,(%current-target-system)))))
                    '())

                (invoke "./bootstrap.sh"
                        (string-append "--prefix=" out)
                        ;; Auto-detection looks for ICU only in traditional
                        ;; install locations.
                        (string-append "--with-icu=" icu)
                        "--with-toolset=gcc"))))
          (delete 'provide-libboost_python)))
      ((#:make-flags make-flags)
       `(cons* "--without-python" ,make-flags))))
    (native-inputs
     (alist-delete "python" (package-native-inputs boost)))
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
                      (url "https://github.com/boostorg/sync")
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
                    (url "https://github.com/boostorg/signals2")
                    (commit (string-append "boost-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13i5j43nggb46i6qpaf7gk53i7zp7pimphl7sydyfqz2m9yx5cdy"))))
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


(define-public boost-mpi
  (package
    (inherit boost)
    (name "boost-mpi")
    (native-inputs
     `(("perl" ,perl)
       ,@(if (%current-target-system)
             '()
             `(("python" ,python-wrapper)))
       ("openmpi" , openmpi)))
    (arguments
     (substitute-keyword-arguments (package-arguments boost)
      ((#:phases phases)
       `(modify-phases ,phases
          (add-after 'configure 'update-jam
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((output-port (open-file "project-config.jam" "a")))
                (display "using mpi ;" output-port)
                (newline output-port)
                (close output-port))))))))
    (home-page "https://www.boost.org")
    (synopsis "Message Passing Interface (MPI) library for C++")))

(define-public mdds
  (package
    (name "mdds")
    (version "1.5.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://kohei.us/files/mdds/src/mdds-" version ".tar.bz2"))
             (sha256
              (base32
               "03b8i43pw4m767mm0cnbi77x7qhpkzpi9b1f6dpp4cmyszmnsk8l"))))
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
