;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages hurd)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages base)
  #:use-module (gnu packages texinfo)
  #:use-module (guix git-download)
  #:export (hurd-triplet?
            hurd-target?))

(define (hurd-triplet? triplet)
  (and (string-suffix? "-gnu" triplet)
       (not (string-contains triplet "linux"))))

(define (hurd-target?)
  "Return true if the cross-compilation target or the current system is
GNU/Hurd."
  (or (and=> (%current-target-system) hurd-triplet?)
      (string-suffix? (%current-system) "-gnu")))

(define (hurd-source-url version)
  (string-append "mirror://gnu/hurd/hurd-"
                 version ".tar.gz"))

(define-public gnumach-headers
   (let ((commit "097f9cf735ffa1212b828682ad92f0f6c5f1c552")
         (revision "1"))
     (package
       (name "gnumach-headers")
       (version (git-version "1.8" revision commit))
       (source
         (origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://git.savannah.gnu.org/git/hurd/gnumach.git")
                  (commit commit)))
           (file-name (git-file-name "gnumach" version))
           (sha256
            (base32
             "0q36z7k02bykrld90zaxbhyzxlmwlqqs4divgir6ix38zsp6icqk"))))
       (build-system gnu-build-system)
       (arguments
        `(#:phases
          (modify-phases %standard-phases
            (replace 'install
              (lambda _
                (invoke "make" "install-data")))
            (delete 'build))

           ;; GNU Mach supports only IA32 currently, so cheat so that we can at
           ;; least install its headers.
           ,@(if (%current-target-system)
               '()
               ;; See <http://lists.gnu.org/archive/html/bug-hurd/2015-06/msg00042.html>
               ;; <http://lists.gnu.org/archive/html/guix-devel/2015-06/msg00716.html>
               '(#:configure-flags '("--build=i586-pc-gnu"
                                     "--host=i686-linux-gnu")))

           #:tests? #f))
       (native-inputs
        `(("autoconf" ,autoconf)
          ("automake" ,automake)
          ("texinfo" ,texinfo-4)))
       (home-page "https://www.gnu.org/software/hurd/microkernel/mach/gnumach.html")
       (synopsis "GNU Mach kernel headers")
       (description
        "Headers of the GNU Mach kernel.")
       (license gpl2+))))

(define-public mig
  (package
    (name "mig")
    (version "1.8")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/mig/mig-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1gyda8sq6b379nx01hkpbd85lz39irdvz2b9wbr63gicicx8i706"))))
    (build-system gnu-build-system)
    ;; Flex is needed both at build and run time.
    (inputs `(("gnumach-headers" ,gnumach-headers)
              ("flex" ,flex)))
    (native-inputs
     `(("flex" ,flex)
       ("bison" ,bison)))
    (arguments `(#:tests? #f))
    (home-page "https://www.gnu.org/software/hurd/microkernel/mach/mig/gnu_mig.html")
    (synopsis "Mach 3.0 interface generator for the Hurd")
    (description
     "GNU MIG is the GNU distribution of the Mach 3.0 interface generator
MIG, as maintained by the GNU Hurd developers for the GNU project.
You need this tool to compile the GNU Mach and GNU Hurd distributions,
and to compile the GNU C library for the Hurd.  Also, you will need it
for other software in the GNU system that uses Mach-based inter-process
communication.")
    (license gpl2+)))

(define-public hurd-headers
  ;; Resort to a post-0.9 snapshot that provides the 'file_utimens' and
  ;; 'file_exec_paths' RPCs that glibc 2.28 expects.
  (let ((revision "1")
        (commit "91a51672ff4cfe1f1a0712b4c542ded3081c825b"))
   (package
     (name "hurd-headers")
     (version (git-version "0.9" revision commit))
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://git.savannah.gnu.org/git/hurd/hurd.git")
                     (commit commit)))
               (sha256
                (base32
                 "16k9wkahz9wasviijz53n6i13nmiwa9fs64ikf1jqh8rl60hw7cz"))
               (file-name (git-file-name name version))))
     (build-system gnu-build-system)
     (native-inputs
      `(("mig" ,mig)
        ("autoconf" ,autoconf)
        ("automake" ,automake)))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (replace 'install
            (lambda _
              (invoke "make" "install-headers" "no_deps=t")))
          (delete 'build))

        #:configure-flags '( ;; Pretend we're on GNU/Hurd; 'configure' wants
                            ;; that.
                            ,@(if (%current-target-system)
                                  '()
                                  '("--host=i586-pc-gnu"))

                            ;; Reduce set of dependencies.
                            "--without-parted"
                            "--disable-ncursesw"
                            "--disable-test"
                            "--without-libbz2"
                            "--without-libcrypt"
                            "--without-libz"
                            ;; Skip the clnt_create check because it expects
                            ;; a working glibc causing a circular dependency.
                            "ac_cv_search_clnt_create=no"

                            ;; Annihilate the checks for the 'file_exec_paths'
                            ;; & co. libc functions to avoid "link tests are
                            ;; not allowed after AC_NO_EXECUTABLES" error.
                            "ac_cv_func_file_exec_paths=no"
                            "ac_cv_func_exec_exec_paths=no"
                            "ac_cv_func__hurd_exec_paths=no"
                            "ac_cv_func_file_futimens=no")

        #:tests? #f))
     (home-page "https://www.gnu.org/software/hurd/hurd.html")
     (synopsis "GNU Hurd headers")
     (description
      "This package provides C headers of the GNU Hurd, used to build the GNU C
Library and other user programs.")
     (license gpl2+))))

(define-public hurd-minimal
  (package (inherit hurd-headers)
    (name "hurd-minimal")
    (inputs `(("glibc-hurd-headers" ,glibc/hurd-headers)))
    (arguments
     (substitute-keyword-arguments (package-arguments hurd-headers)
       ((#:phases _)
        '(modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; We need to copy libihash.a to the output directory manually,
                 ;; since there is no target for that in the makefile.
                 (mkdir-p (string-append out "/include"))
                 (copy-file "libihash/ihash.h"
                            (string-append out "/include/ihash.h"))
                 (mkdir-p (string-append out "/lib"))
                 (copy-file "libihash/libihash.a"
                            (string-append out "/lib/libihash.a"))
                 #t)))
           (replace 'build
             (lambda _
               ;; Install <assert-backtrace.h> & co.
               (invoke "make" "-Clibshouldbeinlibc"
                       "../include/assert-backtrace.h")

               ;; Build libihash.
               (invoke "make" "-Clibihash" "libihash.a")))))))
    (home-page "https://www.gnu.org/software/hurd/hurd.html")
    (synopsis "GNU Hurd libraries")
    (description
     "This package provides libihash, needed to build the GNU C
Library for GNU/Hurd.")
    (license gpl2+)))

(define-public hurd-core-headers
  (package
    (name "hurd-core-headers")
    (version (package-version hurd-headers))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (match %build-inputs
                     (((names . directories) ...)
                      (union-build (assoc-ref %outputs "out")
                                   directories)
                      #t)))))
    (inputs `(("gnumach-headers" ,gnumach-headers)
              ("hurd-headers" ,hurd-headers)
              ("hurd-minimal" ,hurd-minimal)))
    (synopsis "Union of the Hurd headers and libraries")
    (description
     "This package contains the union of the Mach and Hurd headers and the
Hurd-minimal package which are needed for both glibc and GCC.")
    (home-page (package-home-page hurd-headers))
    (license (package-license hurd-headers))))

(define-public gnumach
  (package
    (inherit gnumach-headers)
    (name "gnumach")
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'produce-image
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out  (assoc-ref outputs "out"))
                             (boot (string-append out "/boot")))
                        (invoke "make" "gnumach.gz")
                        (install-file "gnumach.gz" boot)
                        #t))))))
    (native-inputs
     `(("mig" ,mig)
       ("perl" ,perl)))
    (supported-systems (cons "i686-linux" %hurd-systems))
    (synopsis "Microkernel of the GNU system")
    (description
     "GNU Mach is the microkernel upon which a GNU Hurd system is based.")))

(define-public hurd
  (package
    (name "hurd")
    (version "0.9")
    (source (origin
              (method url-fetch)
              (uri (hurd-source-url version))
              (sha256
               (base32
                "1nw9gly0n7pyv3cpfm4mmxy4yccrx4g0lyrvd3vk2vil26jpbggw"))
              (patches (search-patches "hurd-fix-eth-multiplexer-dependency.patch"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
                     (lambda _
                       ;; Don't change the ownership of any file at this time.
                       (substitute* '("daemons/Makefile" "utils/Makefile")
                         (("-o root -m 4755") ""))
                       #t)))
       #:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              %output "/lib")
                          "--disable-ncursesw"
                          "--without-libbz2"
                          "--without-libz"
                          "--without-parted")))
    (build-system gnu-build-system)
    (inputs `(("glibc-hurd-headers" ,glibc/hurd-headers)))
    (native-inputs
     `(("mig" ,mig)
       ("perl" ,perl)))
    (supported-systems %hurd-systems)
    (home-page "https://www.gnu.org/software/hurd/hurd.html")
    (synopsis "The kernel servers for the GNU operating system")
    (description
     "The Hurd is the kernel for the GNU system, a replacement and
augmentation of standard Unix kernels.  It is a collection of protocols for
system interaction (file systems, networks, authentication), and servers
implementing them.")
    (license gpl2+)))
