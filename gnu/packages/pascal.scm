;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2017 Kei Kebreau <address@hidden>
;;; Copyright © 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages pascal)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 match))

(define %fpc-version "3.2.2")
(define %fpc-release-date "2021/05/19")

;;; FIXME: Bootstrap properly; these are prebuilt binaries.
(define fpc-bootstrap-i386
  (origin
    (method url-fetch)
    (uri (string-append "mirror://sourceforge/freepascal/Linux/"
                        %fpc-version "/fpc-" %fpc-version ".i386-linux.tar"))
    (sha256
     (base32
      "0n4r85dsr86zlk7r4hbd4nj14sda6rwgdgzxg4gj4q981fn80agn"))))

(define fpc-bootstrap-x86_64
  (origin
    (method url-fetch)
    (uri (string-append "mirror://sourceforge/freepascal/Linux/"
                        %fpc-version "/fpc-" %fpc-version ".x86_64-linux.tar"))
    (sha256
     (base32
      "10qywczzz4qlcmmzxb7axnvwniq76ky130vd8iv6ljskll4c7njs"))))

(define-public fpc
  (package
    (name "fpc")
    (version %fpc-version)
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/freepascal/Source/"
                                  version "/fpcbuild-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07qna2pvlpa7j0i2wdixjxpizdvffv51nbr1waczk0xv8cq9kvw5"))
              (patches (search-patches "fpc-reproducibility.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (rename-file "install/doc" "install-doc")
                  (rename-file "install/man" "install-man")
                  ;; Contains executables--some of them created by
                  ;; closed-source compilers.
                  (delete-file-recursively "install")
                  (mkdir-p "install")
                  (rename-file "install-doc" "install/doc")
                  (rename-file "install-man" "install/man")
                  (delete-file "fpcsrc/tests/utils/dosbox/exitcode.exe")))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs
     (list expat glibc ld-wrapper ncurses zlib))
    (native-inputs
     ;; FPC is built with FPC, so we need bootstrap binaries.
     `(("fpc-binary" ,(match (or (%current-target-system)
                                 (%current-system))
                       ("i686-linux" fpc-bootstrap-i386)
                       ;;("powerpc64le-linux" fpc-bootstrap-ppc64le)
                       ;;("powerpc-linux" fpc-bootstrap-ppc)
                       ("x86_64-linux" fpc-bootstrap-x86_64)
                       ;; XXX: Wrong, but innocuous so long
                       ;; `supported-systems' is kept in sync.
                       (_ fpc-bootstrap-x86_64)))))
    (arguments
     `(#:tests? #f ; no tests available
       #:phases
       (let ((fpc-bootstrap-path
              (string-append (getcwd) "/" ,name "-" ,version "/fpc-bin"))
             (arch ,(match (or (%current-target-system)
                               (%current-system))
                     ("i686-linux" "i386")
                     ("x86_64-linux" "x86_64")
                     (_ "unknown"))))
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-bin
             (lambda* (#:key inputs #:allow-other-keys)
               (mkdir-p fpc-bootstrap-path)
               (with-directory-excursion fpc-bootstrap-path
                 (invoke "tar" "xvf" (assoc-ref inputs "fpc-binary")))))
           (add-after 'unpack-bin 'install-bin
             (lambda* (#:key inputs #:allow-other-keys)
               (with-directory-excursion
                 (string-append fpc-bootstrap-path "/fpc-" ,version "."
                                arch "-linux")
                 (let ((binary-tarball
                        (string-append "binary." arch "-linux.tar"))
                       (compiler-tarball
                        (string-append "base." arch "-linux.tar.gz"))
                       (fpcmake-tarball
                        (string-append "utils-fpcm." arch "-linux.tar.gz")))
                   ;; Only the base compiler and fpcmake are needed.
                   (invoke "tar" "xvf" binary-tarball compiler-tarball
                           fpcmake-tarball)
                   (invoke "tar" "xvf" compiler-tarball "-C..")
                   (invoke "tar" "xvf" fpcmake-tarball "-C..")))))
           (add-after 'patch-source-shebangs 'patch-inline-shebangs
             (lambda _
               (substitute* "fpcsrc/compiler/cscript.pas"
                 (("#!/bin/sh") (string-append "#!" (which "sh"))))))
           (add-before 'build 'patch-release-date
             (lambda _                  ; reproducibility
               (substitute* (list "fpcdocs/prog.tex"
                                  "fpcsrc/packages/amunits/examples/sortdemo.pas"
                                  "fpcsrc/packages/libogcfpc/src/ogc/libversion.inc"
                                  "fpcsrc/utils/fpcres/fpcjres.pas"
                                  "fpcsrc/utils/fpcres/fpcres.pas"
                                  "fpcsrc/utils/fpcm/fpcmmain.pp"
                                  "fpcsrc/utils/fpcreslipo/fpcreslipo.pp"
                                  "fpcsrc/compiler/version.pas")
                 (("\\{\\$I(NCLUDE)? %DATE%\\}")
                  (format #f "'~a'" ,%fpc-release-date)))))
           (replace 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* "fpcsrc/compiler/systems/t_linux.pas"
                 ;; Point to the current glibc dynamic linker.
                 (("/lib/ld-linux.so.2")
                  (search-input-file inputs ,(glibc-dynamic-linker)))
                 (("/lib64/ld-linux-x86-64.so.2")
                  (search-input-file inputs ,(glibc-dynamic-linker)))
                 ; TODO: /lib/ld-linux-armhf.so.3
                 ; TODO: /lib/ld-linux-aarch64.so.1
                 ; TODO: /lib64/ld64.so.2
                 ;; Add glibc to ld's search path.
                 (("if \\(isdll\\) then")
                  (string-append
                   "Add('SEARCH_DIR(\""
                   (assoc-ref inputs "libc") "/lib"
                   "\")');\n"
                   "if (isdll) then")))
               (substitute* "fpcsrc/compiler/options.pas"
                 (("exepath\\+'../etc/'")
                  (string-append "'" (assoc-ref outputs "out") "/etc'")))))
           (replace 'build
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((fpc-bin (string-append fpc-bootstrap-path "/bin"))
                      (fpc (string-append fpc-bin "/fpc"))
                      (fpcmake (string-append fpc-bin "/fpcmake")))
                 ;; The fpc binary needs to run the ppc[arch] binary (which
                 ;; does the actual compiling) in this directory.
                 (setenv "PATH"
                        (string-append (getenv "PATH") ":"
                                       fpc-bootstrap-path
                                       "/lib/fpc/" ,version))
                 (setenv "FPC" fpc)
                 ;; Specify target operating system using "-T" option
                 (invoke fpcmake (string-append "-T" arch "-linux"))
                 (invoke "make" "build" "NOGDB=1"))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                     ;; This is the suffix of the ppc[arch] binary.
                     (suffix (if (string= arch "x86_64")
                                 "x64"
                                 "386"))
                     (ppc (string-append "ppc" suffix)))
                 (invoke "make" "install" "NOGDB=1"
                         (string-append "INSTALL_PREFIX=" out))
                 ;; Remove files that fail RUNPATH validation.
                 ;; TODO: Fix it instead.
                 (delete-file (string-append out "/lib/libpas2jslib.so"))
                 ;; Add a symlink to the ppc[arch] binary so fpc works.
                 (symlink (string-append out "/lib/fpc/" ,version "/" ppc)
                          (string-append out "/bin/" ppc))
                 ;; Install the example configuration file.
                 (mkdir (string-append out "/etc"))
                 (invoke
                   (string-append out "/lib/fpc/" ,version "/samplecfg")
                   (string-append out "/lib/fpc/" ,version)
                   (string-append out "/etc")))))
           (add-after 'install 'wrap
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (fpc (string-append out "/bin/fpc"))
                      (ld (assoc-ref inputs "ld-wrapper"))
                      (glibc (assoc-ref inputs "glibc")))
                 (wrap-program fpc
                   `("PATH" ":" prefix (,(string-append ld "/bin")))
                   `("LIBRARY_PATH" ":" prefix
                     (,(string-append glibc "/lib")))))))))))
    ;; fpc invokes gcc, so make sure LIBRARY_PATH et.al are set.
    ;(native-search-paths (package-native-search-paths gcc))
    (home-page "https://www.freepascal.org")
    (synopsis "The Free Pascal Compiler")
    (description
     "Free Pascal is a professional Object Pascal compiler.  It supports the
Turbo Pascal 7.0, Delphi, and Mac Pascal dialects.  Free Pascal also supports
many useful extensions to the Pascal programming language.")
    ;; The majority of the software included is licensed under the GPLv2
    ;; or later.  For more licensing details, see the appropriate files in
    ;; the install/doc directory of the source distribution.
    (license license:gpl2+)))

(define-public p2c
  (package
    (name "p2c")
    (version "2.01")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://alum.mit.edu/www/toms/p2c/p2c-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "03x72lv6jrvikbrpz4kfq1xp61l2jw5ki6capib71lxs65zndajn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "CC=" ,(cc-for-target))
               (string-append "HOMEDIR=" out "/lib/p2c")
               (string-append "INCDIR=" out "/include/p2c")
               (string-append "BINDIR=" out "/bin")
               (string-append "LIBDIR=" out "/lib")
               (string-append "MANDIR=" out "/share/man/man1")
               "MANFILE=p2c.man.inst"))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'mkdir
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/share/man"))
               (mkdir-p (string-append out "/lib"))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/include")))
             #t))
         (add-before 'build 'chdir
           (lambda* (#:key make-flags #:allow-other-keys)
             (chdir "src")
             #t)))))
    (native-inputs
     (list perl which))
    (synopsis "p2c converts Pascal programs to C programs--which you can then
compile using gcc")
    (description "This package provides @command{p2c}, a program to convert
Pascal source code to C source code, and @command{p2cc}, a compiler for
Pascal programs.")
    (home-page "http://users.fred.net/tds/lab/p2c/")
    (license license:gpl2+)))
