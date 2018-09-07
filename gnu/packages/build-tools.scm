;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Corentin Bocquillon <corentin@nybble.fr>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Tomáš Čech <sleep_walker@gnu.org>
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

(define-module (gnu packages build-tools)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages ninja)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

(define-public bam
  (package
    (name "bam")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://github.com/downloads/matricks/"
                                  "bam/bam-" version ".tar.bz2"))
              (sha256
               (base32
                "0z90wvyd4nfl7mybdrv9dsd4caaikc6fxw801b72gqi1m9q0c0sn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (zero? (system* "bash" "make_unix.sh"))))
         (replace 'check
           (lambda _
             (zero? (system* "python" "scripts/test.py"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (install-file "bam" bin)
               #t))))))
    (native-inputs
     `(("python" ,python-2)))
    (home-page "https://matricks.github.io/bam/")
    (synopsis "Fast and flexible build system")
    (description "Bam is a fast and flexible build system.  Bam uses Lua to
describe the build process.  It takes its inspiration for the script files
from scons.  While scons focuses on being 100% correct when building, bam
makes a few sacrifices to acquire fast full and incremental build times.")
    (license license:bsd-3)))

(define-public bear
  (package
    (name "bear")
    (version "2.3.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rizsotto/Bear")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zzz2yiiny9pm4h6ayb82xzxc2j5djcpf8va2wagcw92m7w6miqw"))))
    (build-system cmake-build-system)
    (inputs
     `(("python" ,python-wrapper)))
    (home-page "https://github.com/rizsotto/Bear")
    (synopsis "Tool for generating a compilation database")
    (description "A JSON compilation database is used in the Clang project to
provide information on how a given compilation unit is processed.  With this,
it is easy to re-run the compilation with alternate programs.  Bear is used to
generate such a compilation database.")
    (license license:gpl3+)))

(define-public meson
  (package
    (name "meson")
    (version "0.47.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mesonbuild/meson/"
                                  "releases/download/" version  "/meson-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1swmycf6p9p0ag6yiywyyri42ffkxxj38r2ic7in24km47cszn4j"))))
    (build-system python-build-system)
    (arguments
     `(;; FIXME: Tests require many additional inputs, a fix for the RUNPATH
       ;; patch in meson-for-build, and patching many hard-coded file system
       ;; locations in "run_unittests.py".
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  ;; Meson calls the various executables in out/bin through the
                  ;; Python interpreter, so we cannot use the shell wrapper.
                  (delete 'wrap))))
    (inputs `(("ninja" ,ninja)))
    (propagated-inputs `(("python" ,python)))
    (home-page "https://mesonbuild.com/")
    (synopsis "Build system designed to be fast and user-friendly")
    (description
     "The Meson build system is focused on user-friendliness and speed.
It can compile code written in C, C++, Fortran, Java, Rust, and other
languages.  Meson provides features comparable to those of the
Autoconf/Automake/make combo.  Build specifications, also known as @dfn{Meson
files}, are written in a custom domain-specific language (@dfn{DSL}) that
resembles Python.")
    (license license:asl2.0)))

(define-public meson-for-build
  (package
    (inherit meson)
    (name "meson-for-build")
    (source (origin
              (inherit (package-source meson))
              (patches (search-patches "meson-for-build-rpath.patch"))))

    ;; People should probably install "meson", not "meson-for-build".
    (properties `((hidden? . #t)))))

(define-public premake4
  (package
    (name "premake")
    (version "4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/premake/Premake/"
                                  version "/premake-" version "-src.zip"))
              (sha256
               (base32
                "1017rd0wsjfyq2jvpjjhpszaa7kmig6q1nimw76qx3cjz2868lrn"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("unzip" ,unzip))) ; for unpacking the source
    (arguments
     `(#:make-flags '("CC=gcc")
       #:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "build/gmake.unix") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "../../bin/release/premake4"
                           (string-append (assoc-ref outputs "out") "/bin"))
             #t)))))
    (synopsis "Portable software build tool")
    (description "@code{premake4} is a command line utility that reads a
scripted definition of a software project and outputs @file{Makefile}s or
other lower-level build files.")
    (home-page "https://premake.github.io")
    (license license:bsd-3)))

(define-public osc
  (package
    (name "osc")
    (version "0.162.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/openSUSE/" name
                           "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b4kpm96ns4smqyfjysbk2p78d36x44xprpna8zz85q1y5xn57aj"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; Module is python2 only.
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'fix-filename-and-remove-unused
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               ;; Main osc tool is renamed in spec file, not setup.py, let's
               ;; do that too.
               (rename-file
                (string-append bin "osc-wrapper.py")
                (string-append bin "osc"))
               ;; Remove unused and broken script.
               (delete-file (string-append bin "osc_hotshot.py"))
             #t))))))
    (inputs
     `(("python2-m2crypto" ,python2-m2crypto)
       ("python2-pycurl" ,python2-pycurl)
       ("python2-urlgrabber" ,python2-urlgrabber)))
    (home-page "https://github.com/openSUSE/osc")
    (synopsis "Open Build Service command line tool")
    (description "@command{osc} is a command line interface to the Open Build
Service.  It allows you to checkout, commit, perform reviews etc.  The vast
majority of the OBS functionality is available via commands and the rest can
be reached via direct API calls.")
    (license license:gpl2+)))
