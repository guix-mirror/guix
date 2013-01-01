;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012, 2013 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro packages make-bootstrap)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix build-system gnu) #:select (package-with-explicit-inputs))
  #:use-module ((distro) #:select (search-patch))
  #:use-module (distro packages base)
  #:use-module (distro packages bash)
  #:use-module (distro packages compression)
  #:use-module (distro packages gawk)
  #:use-module (distro packages guile)
  #:use-module (distro packages linux)
  #:use-module (distro packages multiprecision)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%bootstrap-binaries-tarball
            %binutils-bootstrap-tarball
            %glibc-bootstrap-tarball
            %gcc-bootstrap-tarball
            %guile-bootstrap-tarball))

;;; Commentary:
;;;
;;; This modules provides tools to build tarballs of the "bootstrap binaries"
;;; used in (distro packages bootstrap).  These statically-linked binaries are
;;; taken for granted and used as the root of the whole bootstrap procedure.
;;;
;;; Code:

(define* (static-package p #:optional (loc (current-source-location)))
  "Return a statically-linked version of package P."
  ;; TODO: Move to (guix build-system gnu).
  (let ((args (package-arguments p)))
    (package (inherit p)
      (location (source-properties->location loc))
      (arguments
       (let ((augment (lambda (args)
                        (let ((a (default-keyword-arguments args
                                   '(#:configure-flags '()
                                     #:strip-flags #f))))
                          (substitute-keyword-arguments a
                            ((#:configure-flags flags)
                             `(cons* "--disable-shared"
                                     "LDFLAGS=-static"
                                     ,flags))
                            ((#:strip-flags _)
                             ''("--strip-all")))))))
         (if (procedure? args)
             (lambda x
               (augment (apply args x)))
             (augment args)))))))

(define %glibc-with-relocatable-system
  ;; A libc whose `system' and `popen' functions looks for `sh' in $PATH.
  (package (inherit glibc-final)
    (arguments
     (lambda (system)
       (substitute-keyword-arguments ((package-arguments glibc-final) system)
         ((#:patches patches)
          `(cons (assoc-ref %build-inputs "patch/system")
                 ,patches)))))
    (inputs
     `(("patch/system" ,(search-patch "glibc-bootstrap-system.patch"))
       ,@(package-inputs glibc-final)))))

(define %standard-inputs-with-relocatable-glibc
  ;; Standard inputs with the above libc and corresponding GCC.
  `(("libc", %glibc-with-relocatable-system)
    ("gcc" ,(package-with-explicit-inputs
             gcc-4.7
             `(("libc",%glibc-with-relocatable-system)
               ,@(alist-delete "libc" %final-inputs))
             (current-source-location)))
    ,@(fold alist-delete %final-inputs '("libc" "gcc"))))

(define %bash-static
  (static-package bash-light))

(define %static-inputs
  ;; Packages that are to be used as %BOOTSTRAP-INPUTS.
  (let ((coreutils (package (inherit coreutils)
                     (arguments
                      `(#:configure-flags
                        '("--disable-nls"
                          "--disable-silent-rules"
                          "--enable-no-install-program=stdbuf,libstdbuf.so"
                          "LDFLAGS=-static -pthread")
                        ,@(package-arguments coreutils)))))
        (bzip2 (package (inherit bzip2)
                 (arguments
                  (substitute-keyword-arguments (package-arguments bzip2)
                    ((#:phases phases)
                     `(alist-cons-before
                       'build 'dash-static
                       (lambda _
                         (substitute* "Makefile"
                           (("^LDFLAGS[[:blank:]]*=.*$")
                            "LDFLAGS = -static")))
                       ,phases))))))
        (xz (package (inherit xz)
              (arguments
               `(#:strip-flags '("--strip-all")
                 #:phases (alist-cons-before
                           'configure 'static-executable
                           (lambda _
                             ;; Ask Libtool for a static executable.
                             (substitute* "src/xz/Makefile.in"
                               (("^xz_LDADD =")
                                "xz_LDADD = -all-static")))
                           %standard-phases)))))
        (gawk (package (inherit gawk)
                (arguments
                 (lambda (system)
                   `(#:patches (list (assoc-ref %build-inputs "patch/sh"))
                     #:phases (alist-cons-before
                               'build 'no-export-dynamic
                               (lambda* (#:key outputs #:allow-other-keys)
                                 ;; Since we use `-static', remove
                                 ;; `-export-dynamic'.
                                 (substitute* "configure"
                                   (("-export-dynamic") "")))
                               %standard-phases)
                     ,@((package-arguments gawk) system))))
                (inputs `(("patch/sh" ,(search-patch "gawk-shell.patch"))))))
        (finalize (lambda (p)
                    (static-package (package-with-explicit-inputs
                                     p
                                     %standard-inputs-with-relocatable-glibc)
                                    (current-source-location)))))
    `(,@(map (match-lambda
              ((name package)
               (list name (finalize package))))
             `(("tar" ,tar)
               ("gzip" ,gzip)
               ("bzip2" ,bzip2)
               ("xz" ,xz)
               ("patch" ,patch)
               ("coreutils" ,coreutils)
               ("sed" ,sed)
               ("grep" ,grep)
               ("gawk" ,gawk)))
      ("bash" ,%bash-static)
      ;; ("ld-wrapper" ,ld-wrapper)
      ;; ("binutils" ,binutils-final)
      ;; ("gcc" ,gcc-final)
      ;; ("libc" ,glibc-final)
      )))

(define %static-binaries
  (package
    (name "static-binaries")
    (version "0")
    (build-system trivial-build-system)
    (source #f)
    (inputs %static-inputs)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (ice-9 ftw)
                      (ice-9 match)
                      (srfi srfi-1)
                      (srfi srfi-26)
                      (guix build utils))

         (let ()
          (define (directory-contents dir)
            (map (cut string-append dir "/" <>)
                 (scandir dir (negate (cut member <> '("." ".."))))))

          (define (copy-directory source destination)
            (for-each (lambda (file)
                        (format #t "copying ~s...~%" file)
                        (copy-file file
                                   (string-append destination "/"
                                                  (basename file))))
                      (directory-contents source)))

          (let* ((out (assoc-ref %outputs "out"))
                 (bin (string-append out "/bin")))
            (mkdir-p bin)

            ;; Copy Coreutils binaries.
            (let* ((coreutils (assoc-ref %build-inputs "coreutils"))
                   (source    (string-append coreutils "/bin")))
              (copy-directory source bin))

            ;; For the other inputs, copy just one binary, which has the
            ;; same name as the input.
            (for-each (match-lambda
                       ((name . dir)
                        (let ((source (string-append dir "/bin/" name)))
                          (format #t "copying ~s...~%" source)
                          (copy-file source
                                     (string-append bin "/" name)))))
                      (alist-delete "coreutils" %build-inputs))

            ;; But of course, there are exceptions to this rule.
            (let ((grep (assoc-ref %build-inputs "grep")))
              (copy-file (string-append grep "/bin/fgrep")
                         (string-append bin "/fgrep"))
              (copy-file (string-append grep "/bin/egrep")
                         (string-append bin "/egrep")))

            ;; Clear references to the store path.
            (for-each remove-store-references
                      (directory-contents bin))

            (with-directory-excursion bin
              ;; Programs such as Perl's build system want these aliases.
              (symlink "bash" "sh")
              (symlink "gawk" "awk"))

            #t)))))
    (synopsis "Statically-linked bootstrap binaries")
    (description
     "Binaries used to bootstrap the distribution.")
    (license #f)
    (home-page #f)))

(define %binutils-static
  ;; Statically-linked Binutils.
  (package (inherit binutils)
    (name "binutils-static")
    (arguments
     `(#:configure-flags '("--disable-gold" "--with-lib-path=/no-ld-lib-path")
       #:strip-flags '("--strip-all")
       #:phases (alist-cons-before
                 'configure 'all-static
                 (lambda _
                   ;; The `-all-static' libtool flag can only be passed
                   ;; after `configure', since configure tests don't use
                   ;; libtool, and only for executables built with libtool.
                   (substitute* '("binutils/Makefile.in"
                                  "gas/Makefile.in"
                                  "ld/Makefile.in")
                     (("^LDFLAGS =(.*)$" line)
                      (string-append line
                                     "\nAM_LDFLAGS = -static -all-static\n"))))
                 %standard-phases)))))

(define %binutils-static-stripped
  ;; The subset of Binutils that we need.
  (package (inherit %binutils-static)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (setvbuf (current-output-port) _IOLBF)
         (let* ((in  (assoc-ref %build-inputs "binutils"))
                (out (assoc-ref %outputs "out"))
                (bin (string-append out "/bin")))
           (mkdir-p bin)
           (for-each (lambda (file)
                       (let ((target (string-append bin "/" file)))
                         (format #t "copying `~a'...~%" file)
                         (copy-file (string-append in "/bin/" file)
                                    target)
                         (remove-store-references target)))
                     '("ar" "as" "ld" "nm"  "objcopy" "objdump"
                       "ranlib" "readelf" "size" "strings" "strip"))
           #t))))
    (inputs `(("binutils" ,%binutils-static)))))

(define %glibc-stripped
  ;; GNU libc's essential shared libraries, dynamic linker, and headers,
  ;; with all references to store directories stripped.  As a result,
  ;; libc.so is unusable and need to be patched for proper relocation.
  (let ((glibc %glibc-with-relocatable-system))
    (package (inherit glibc)
      (name "glibc-stripped")
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))

           (setvbuf (current-output-port) _IOLBF)
           (let* ((out    (assoc-ref %outputs "out"))
                  (libdir (string-append out "/lib"))
                  (incdir (string-append out "/include"))
                  (libc   (assoc-ref %build-inputs "libc"))
                  (linux  (assoc-ref %build-inputs "linux-headers")))
             (mkdir-p libdir)
             (for-each (lambda (file)
                         (let ((target (string-append libdir "/"
                                                      (basename file))))
                           (copy-file file target)
                           (remove-store-references target)))
                       (find-files (string-append libc "/lib")
                                   "^(crt.*|ld.*|lib(c|m|dl|rt|pthread|nsl|util).*\\.so(\\..*)?|libc_nonshared\\.a)$"))

             (copy-recursively (string-append libc "/include") incdir)

             ;; Copy some of the Linux-Libre headers that glibc headers
             ;; refer to.
             (mkdir (string-append incdir "/linux"))
             (for-each (lambda (file)
                         (copy-file (string-append linux "/include/linux/" file)
                                    (string-append incdir "/linux/"
                                                   (basename file))))
                       '("limits.h" "errno.h" "socket.h" "kernel.h"
                         "sysctl.h" "param.h" "ioctl.h" "types.h"
                         "posix_types.h" "stddef.h"))

             (copy-recursively (string-append linux "/include/asm")
                               (string-append incdir "/asm"))
             (copy-recursively (string-append linux "/include/asm-generic")
                               (string-append incdir "/asm-generic"))
             #t))))
      (inputs `(("libc" ,glibc)
                ("linux-headers" ,linux-libre-headers))))))

(define %gcc-static
  ;; A statically-linked GCC, with stripped-down functionality.
  (package-with-explicit-inputs
   (package (inherit gcc-final)
     (name "gcc-static")
     (arguments
      (lambda (system)
        `(#:modules ((guix build utils)
                     (guix build gnu-build-system)
                     (srfi srfi-1)
                     (srfi srfi-26)
                     (ice-9 regex))
          ,@(substitute-keyword-arguments ((package-arguments gcc-final) system)
              ((#:guile _) #f)
              ((#:implicit-inputs? _) #t)
              ((#:configure-flags flags)
               `(append (list
                         "--disable-shared"
                         "--disable-plugin"
                         "--enable-languages=c"
                         "--disable-libmudflap"
                         "--disable-libgomp"
                         "--disable-libssp"
                         "--disable-libquadmath"
                         "--disable-decimal-float")
                        (remove (cut string-match "--(.*plugin|enable-languages)" <>)
                                ,flags)))
              ((#:make-flags flags)
               `(cons "BOOT_LDFLAGS=-static" ,flags))))))
     (inputs `(("gmp-source" ,(package-source gmp))
               ("mpfr-source" ,(package-source mpfr))
               ("mpc-source" ,(package-source mpc))
               ("binutils" ,binutils-final)
               ,@(package-inputs gcc-4.7))))
   %standard-inputs-with-relocatable-glibc))

(define %gcc-stripped
  ;; The subset of GCC files needed for bootstrap.
  (package (inherit gcc-4.7)
    (name "gcc-stripped")
    (build-system trivial-build-system)
    (source #f)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (srfi srfi-1)
                      (srfi srfi-26)
                      (guix build utils))

         (setvbuf (current-output-port) _IOLBF)
         (let* ((out        (assoc-ref %outputs "out"))
                (bindir     (string-append out "/bin"))
                (libdir     (string-append out "/lib"))
                (libexecdir (string-append out "/libexec"))
                (gcc        (assoc-ref %build-inputs "gcc")))
           (copy-recursively (string-append gcc "/bin") bindir)
           (for-each remove-store-references
                     (find-files bindir ".*"))

           (copy-recursively (string-append gcc "/lib") libdir)
           (for-each remove-store-references
                     (remove (cut string-suffix? ".h" <>)
                             (find-files libdir ".*")))

           (copy-recursively (string-append gcc "/libexec")
                             libexecdir)
           (for-each remove-store-references
                     (find-files libexecdir ".*"))
           #t))))
    (inputs `(("gcc" ,%gcc-static)))))

(define %guile-static
  ;; A statically-linked Guile that is relocatable--i.e., it can search
  ;; .scm and .go files relative to its installation directory, rather
  ;; than in hard-coded configure-time paths.
  (let ((guile (package (inherit guile-2.0)
                 (inputs
                  `(("patch/relocatable"
                     ,(search-patch "guile-relocatable.patch"))
                    ("patch/utf8"
                     ,(search-patch "guile-default-utf8.patch"))
                    ,@(package-inputs guile-2.0)))
                 (arguments
                  `(;; When `configure' checks for ltdl availability, it
                    ;; doesn't try to link using libtool, and thus fails
                    ;; because of a missing -ldl.  Work around that.
                    #:configure-flags '("LDFLAGS=-ldl")

                    #:phases (alist-cons-before
                              'configure 'static-guile
                              (lambda _
                                (substitute* "libguile/Makefile.in"
                                  ;; Create a statically-linked `guile'
                                  ;; executable.
                                  (("^guile_LDFLAGS =")
                                   "guile_LDFLAGS = -all-static")

                                  ;; Add `-ldl' *after* libguile-2.0.la.
                                  (("^guile_LDADD =(.*)$" _ ldadd)
                                   (string-append "guile_LDADD = "
                                                  (string-trim-right ldadd)
                                                  " -ldl\n"))))
                              %standard-phases)

                    ;; Allow Guile to be relocated, as is needed during
                    ;; bootstrap.
                    #:patches
                    (list (assoc-ref %build-inputs "patch/relocatable")
                          (assoc-ref %build-inputs "patch/utf8"))

                    ;; There are uses of `dynamic-link' in
                    ;; {foreign,coverage}.test that don't fly here.
                    #:tests? #f)))))
    (package-with-explicit-inputs (static-package guile)
                                  %standard-inputs-with-relocatable-glibc
                                  (current-source-location))))

(define %guile-static-stripped
  ;; A stripped static Guile binary, for use during bootstrap.
  (package (inherit %guile-static)
    (name "guile-static-stripped")
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let ()
         (use-modules (guix build utils))

         (let ((in  (assoc-ref %build-inputs "guile"))
               (out (assoc-ref %outputs "out")))
           (mkdir-p (string-append out "/share/guile/2.0"))
           (copy-recursively (string-append in "/share/guile/2.0")
                             (string-append out "/share/guile/2.0"))

           (mkdir-p (string-append out "/lib/guile/2.0/ccache"))
           (copy-recursively (string-append in "/lib/guile/2.0/ccache")
                             (string-append out "/lib/guile/2.0/ccache"))

           (mkdir (string-append out "/bin"))
           (copy-file (string-append in "/bin/guile")
                      (string-append out "/bin/guile"))
           (remove-store-references (string-append out "/bin/guile"))
           #t))))
    (inputs `(("guile" ,%guile-static)))))

(define (tarball-package pkg)
  "Return a package containing a tarball of PKG."
  (package (inherit pkg)
    (location (source-properties->location (current-source-location)))
    (name (string-append (package-name pkg) "-tarball"))
    (build-system trivial-build-system)
    (inputs `(("tar" ,tar)
              ("xz" ,xz)
              ("input" ,pkg)))
    (arguments
     (lambda (system)
       (let ((name    (package-name pkg))
             (version (package-version pkg)))
         `(#:modules ((guix build utils))
           #:builder
           (begin
             (use-modules (guix build utils))
             (let ((out   (assoc-ref %outputs "out"))
                   (input (assoc-ref %build-inputs "input"))
                   (tar   (assoc-ref %build-inputs "tar"))
                   (xz    (assoc-ref %build-inputs "xz")))
               (mkdir out)
               (set-path-environment-variable "PATH" '("bin") (list tar xz))
               (with-directory-excursion input
                 (zero? (system* "tar" "cJvf"
                                 (string-append out "/"
                                                ,name "-" ,version
                                                "-" ,system ".tar.xz")
                                 ".")))))))))))

(define %bootstrap-binaries-tarball
  ;; A tarball with the statically-linked bootstrap binaries.
  (tarball-package %static-binaries))

(define %binutils-bootstrap-tarball
  ;; A tarball with the statically-linked Binutils programs.
  (tarball-package %binutils-static-stripped))

(define %glibc-bootstrap-tarball
  ;; A tarball with GNU libc's shared libraries, dynamic linker, and headers.
  (tarball-package %glibc-stripped))

(define %gcc-bootstrap-tarball
  ;; A tarball with a dynamic-linked GCC and its headers.
  (tarball-package %gcc-stripped))

(define %guile-bootstrap-tarball
  ;; A tarball with the statically-linked, relocatable Guile.
  (tarball-package %guile-static-stripped))

;;; make-bootstrap.scm ends here
