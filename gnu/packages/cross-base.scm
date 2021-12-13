;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019 Carl Dong <contact@carldong.me>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu packages cross-base)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages mingw)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (cross-binutils
            cross-libc
            cross-gcc
            cross-newlib?
            cross-kernel-headers))

(define-syntax %xgcc
  ;; GCC package used as the basis for cross-compilation.  It doesn't have to
  ;; be 'gcc' and can be a specific variant such as 'gcc-4.8'.
  ;;
  ;; Note: This is a macro so that we do not refer to 'gcc' from the top
  ;; level, which would lead to circular-dependency issues.
  (identifier-syntax gcc))

(define %gcc-include-paths
  ;; Environment variables for header search paths.
  ;; Note: See <http://bugs.gnu.org/22186> for why not 'CPATH'.
  '("C_INCLUDE_PATH"
    "CPLUS_INCLUDE_PATH"
    "OBJC_INCLUDE_PATH"
    "OBJCPLUS_INCLUDE_PATH"))

(define %gcc-cross-include-paths
  ;; Search path for target headers when cross-compiling.
  (map (cut string-append "CROSS_" <>) %gcc-include-paths))

(define (cross p target)
  (package (inherit p)
    (name (string-append (package-name p) "-cross-" target))
    (arguments
     (substitute-keyword-arguments (package-arguments p)
       ((#:configure-flags flags)
        `(cons ,(string-append "--target=" target)
               ,flags))))))

(define* (cross-binutils target #:optional (binutils binutils))
  "Return a cross-Binutils for TARGET using BINUTILS."
  (let ((binutils (package (inherit binutils)
                    (arguments
                     (substitute-keyword-arguments (package-arguments
                                                    binutils)
                       ((#:configure-flags flags)
                        ;; Build with `--with-sysroot' so that ld honors
                        ;; DT_RUNPATH entries when searching for a needed
                        ;; library.  This works because as a side effect
                        ;; `genscripts.sh' sets `USE_LIBPATH=yes', which tells
                        ;; elf32.em to use DT_RUNPATH in its search list.
                        ;; See <http://sourceware.org/ml/binutils/2013-05/msg00312.html>.
                        ;;
                        ;; In theory choosing / as the sysroot could lead ld
                        ;; to pick up native libs instead of target ones.  In
                        ;; practice the RUNPATH of target libs only refers to
                        ;; target libs, not native libs, so this is safe.
                        `(cons "--with-sysroot=/" ,flags)))))))

    ;; For Xtensa, apply Qualcomm's patch.
    (cross (cond ((string-prefix? "xtensa-" target)
                  (package-with-patches binutils
                                        (search-patches
                                         "ath9k-htc-firmware-binutils.patch")))
                 ((target-mingw? target)
                  (package-with-extra-patches
                   binutils
                   (search-patches "binutils-mingw-w64-timestamp.patch"
                                   "binutils-mingw-w64-deterministic.patch")))
                 (else binutils))
           target)))

(define (cross-gcc-arguments target xgcc libc)
  "Return build system arguments for a cross-gcc for TARGET, using XGCC as the
base compiler and using LIBC (which may be either a libc package or #f.)"
  ;; Set the current target system so that 'glibc-dynamic-linker' returns the
  ;; right name.
  (parameterize ((%current-target-system target))
    ;; Disable stripping as this can break binaries, with object files of
    ;; libgcc.a showing up as having an unknown architecture.  See
    ;; <http://lists.fedoraproject.org/pipermail/arm/2010-August/000663.html>
    ;; for instance.
    (let ((args `(#:strip-binaries? #f
                  ,@(package-arguments xgcc))))
     (substitute-keyword-arguments args
       ((#:configure-flags flags)
        `(append (list ,(string-append "--target=" target)
                       ,@(if libc
                             `( ;; Disable libcilkrts because it is not
                                ;; ported to GNU/Hurd.
                               "--disable-libcilkrts"
                               ;; When building a cross compiler, --with-sysroot is
                               ;; implicitly set to "$gcc_tooldir/sys-root".  This does
                               ;; not work for us, because --with-native-system-header-dir
                               ;; is searched for relative to this location.  Thus, we set
                               ;; it to "/" so GCC is able to find the target libc headers.
                               ;; This is safe because in practice GCC uses CROSS_CPATH
                               ;; & co to separate target and host libraries.
                               "--with-sysroot=/")
                             `( ;; Disable features not needed at this stage.
                               "--disable-shared" "--enable-static"
                               "--enable-languages=c,c++"

                               ;; libstdc++ cannot be built at this stage
                               ;; ("Link tests are not allowed after
                               ;; GCC_NO_EXECUTABLES.").
                               "--disable-libstdc++-v3"

                               "--disable-threads" ;libgcc, would need libc
                               "--disable-libatomic"
                               "--disable-libmudflap"
                               "--disable-libgomp"
                               "--disable-libmpx"
                               "--disable-libssp"
                               "--disable-libquadmath"
                               "--disable-decimal-float" ;would need libc
                               "--disable-libcilkrts"

                               ;; When target is any OS other than 'none' these
                               ;; libraries will fail if there is no libc
                               ;; present. See
                               ;; <https://lists.gnu.org/archive/html/guix-devel/2016-02/msg01311.html>
                               "--disable-libitm"
                               "--disable-libvtv"
                               "--disable-libsanitizer"
                                ))

                       ;; Install cross-built libraries such as libgcc_s.so in
                       ;; the "lib" output.
                       ,@(if libc
                             `((string-append "--with-toolexeclibdir="
                                              (assoc-ref %outputs "lib")
                                              "/" ,target "/lib"))
                             '())
                       ;; For a newlib (non-glibc) target
                       ,@(if (cross-newlib? target)
                             '("--with-newlib")
                             '()))

                 ,(if libc
                      flags
                      `(remove (cut string-match "--enable-languages.*" <>)
                               ,flags))))
       ((#:make-flags flags)
        (if libc
            `(let ((libc (assoc-ref %build-inputs "libc")))
               ;; FLAGS_FOR_TARGET are needed for the target libraries to receive
               ;; the -Bxxx for the startfiles.
               (cons (string-append "FLAGS_FOR_TARGET=-B" libc "/lib")
                     ,flags))
            flags))
       ((#:phases phases)
        `(cross-gcc-build-phases ,target ,phases))))))

(define (cross-gcc-patches xgcc target)
  "Return GCC patches needed for XGCC and TARGET."
  (cond ((string-prefix? "xtensa-" target)
         ;; Patch by Qualcomm needed to build the ath9k-htc firmware.
         (search-patches "ath9k-htc-firmware-gcc.patch"))
        ((target-mingw? target)
         (append (search-patches "gcc-4.9.3-mingw-gthr-default.patch")
                 (if (version>=? (package-version xgcc) "7.0")
                     (search-patches "gcc-7-cross-mingw.patch")
                    '())))
        (else '())))

(define (cross-gcc-snippet target)
  "Return GCC snippet needed for TARGET."
  `(begin
     ,@(if (target-mingw? target)
           '((copy-recursively "libstdc++-v3/config/os/mingw32-w64"
                               "libstdc++-v3/config/os/newlib"))
           '())
     ;; TOOLDIR_BASE_PREFIX is erroneous when using a separate "lib"
     ;; output. Specify it correctly, otherwise GCC won't find its shared
     ;; libraries installed in the "lib" output.  See:
     ;; https://lists.gnu.org/archive/html/bug-guix/2020-03/msg00196.html.
     (substitute* "gcc/Makefile.in"
       (("-DTOOLDIR_BASE_PREFIX=[^ ]*")
        "-DTOOLDIR_BASE_PREFIX=\\\"../../../../\\\""))
     #t))

(define* (cross-gcc target
                    #:key
                    (xgcc %xgcc)
                    (xbinutils (cross-binutils target))
                    (libc #f))
  "Return a cross-compiler for TARGET, where TARGET is a GNU triplet.  Use
XGCC as the base compiler.  Use XBINUTILS as the associated cross-Binutils.
If LIBC is false, then build a GCC that does not target a libc; otherwise,
target that libc."
  (package (inherit xgcc)
    (name (string-append "gcc-cross-"
                         (if libc "" "sans-libc-")
                         target))
    (source
     (origin
       (inherit (package-source xgcc))
       (patches
        (append
         (origin-patches (package-source xgcc))
         (append (cond
                  ((version>=? (package-version xgcc) "10.0")
                   (search-patches "gcc-10-cross-environment-variables.patch"
                                   "gcc-cross-gxx-include-dir.patch"))
                  ((version>=? (package-version xgcc) "8.0")
                   (search-patches "gcc-8-cross-environment-variables.patch"))
                  ((version>=? (package-version xgcc) "6.0")
                   (search-patches "gcc-7-cross-toolexeclibdir.patch"
                                   "gcc-6-cross-environment-variables.patch"))
                  (else
                   (search-patches "gcc-cross-environment-variables.patch")))
                 (cross-gcc-patches xgcc target))))
       (modules '((guix build utils)))
       (snippet
        (cross-gcc-snippet target))))

    (outputs '("out" "lib"))

    (arguments
     `(#:implicit-inputs? #f
       #:imported-modules ((gnu build cross-toolchain)
                           ,@%gnu-build-system-modules)
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (gnu build cross-toolchain)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 regex))

       ,@(cross-gcc-arguments target xgcc libc)))

    (native-inputs
     `(("ld-wrapper-cross" ,(make-ld-wrapper
                             (string-append "ld-wrapper-" target)
                             #:target (const target)
                             #:binutils xbinutils))
       ("binutils-cross" ,xbinutils)

       ,@(let ((inputs (append (package-inputs xgcc)
                               (fold alist-delete (%final-inputs)
                                     '("libc" "libc:static"))

                               ;; Call it differently so that the builder can
                               ;; check whether the "libc" input is #f.
                               `(("libc-native"
                                  ,@(assoc-ref (%final-inputs) "libc"))
                                 ("libc-native:static"
                                  ,@(assoc-ref (%final-inputs)
                                               "libc:static"))))))
           (cond
            ((target-mingw? target)
             (if libc
                 `(,@inputs
                   ("libc" ,libc))
                 `(,@inputs
                   ("mingw-source" ,(package-source mingw-w64)))))
            (libc
             `(,@inputs
               ("libc" ,libc)
               ("libc:static" ,libc "static")
               ("xkernel-headers"                ;the target headers
                ,@(assoc-ref (package-propagated-inputs libc)
                             "kernel-headers"))))
            (else inputs)))))

    (inputs '())

    ;; Only search target inputs, not host inputs.
    (search-paths (cons (search-path-specification
                         (variable "CROSS_LIBRARY_PATH")
                         (files '("lib" "lib64")))
                        (map (lambda (variable)
                               (search-path-specification
                                (variable variable)

                                ;; Add 'include/c++' here so that <cstdlib>'s
                                ;; "#include_next <stdlib.h>" finds GCC's
                                ;; <stdlib.h>, not libc's.
                                (files (match variable
                                         ("CROSS_CPLUS_INCLUDE_PATH"
                                          '("include/c++" "include"))
                                         (_
                                          '("include"))))))
                             %gcc-cross-include-paths)))
    (native-search-paths '())))

(define* (cross-kernel-headers target
                               #:optional
                               (linux-headers linux-libre-headers)
                               (xgcc (cross-gcc target))
                               (xbinutils (cross-binutils target)))
  "Return headers depending on TARGET."

  (define xlinux-headers
    (package (inherit linux-headers)
      (name (string-append (package-name linux-headers)
                           "-cross-" target))
      (arguments
       (substitute-keyword-arguments
           `(#:implicit-cross-inputs? #f
             ,@(package-arguments linux-headers))
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'build
               (lambda _
                 (setenv "ARCH" ,(system->linux-architecture target))
                 (format #t "`ARCH' set to `~a' (cross compiling)~%"
                         (getenv "ARCH"))

                 (invoke "make" ,(system->defconfig target))
                 (invoke "make" "mrproper"
                         ,@(if (version>=? (package-version linux-headers) "5.3")
                               '("headers")
                               '("headers_check")))))))))
      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ,@(package-native-inputs linux-headers)))))

  (define xgnumach-headers
    (package (inherit gnumach-headers)
      (name (string-append (package-name gnumach-headers)
                           "-cross-" target))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ,@(package-native-inputs gnumach-headers)))))

  (define xmig
    (package (inherit mig)
      (name (string-append "mig-cross"))
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-26))
         #:phases (modify-phases %standard-phases
                    (add-before 'configure 'set-cross-headers-path
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let* ((mach (assoc-ref inputs "cross-gnumach-headers"))
                               (cpath (string-append mach "/include")))
                          (for-each (cut setenv <> cpath)
                                    ',%gcc-cross-include-paths)
                          #t))))
         #:configure-flags (list ,(string-append "--target=" target))
         #:tests? #f))

      (propagated-inputs `(("cross-gnumach-headers" ,xgnumach-headers)))
      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ,@(package-native-inputs mig)))))

  (define xhurd-headers
    (package (inherit hurd-headers)
      (name (string-append (package-name hurd-headers)
                           "-cross-" target))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ("cross-mig" ,xmig)
                       ,@(alist-delete "mig"(package-native-inputs hurd-headers))))))

   (define xglibc/hurd-headers
    (package (inherit glibc/hurd-headers)
      (name (string-append (package-name glibc/hurd-headers)
                           "-cross-" target))

      (arguments
       (substitute-keyword-arguments
           `(#:modules ((guix build gnu-build-system)
                        (guix build utils)
                        (srfi srfi-26))
             ,@(package-arguments glibc/hurd-headers))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'set-cross-headers-path
               (lambda* (#:key inputs #:allow-other-keys)
                 (let* ((mach (assoc-ref inputs "gnumach-headers"))
                        (hurd (assoc-ref inputs "hurd-headers"))
                        (cpath (string-append mach "/include:"
                                              hurd "/include")))
                   (for-each (cut setenv <> cpath)
                             ',%gcc-cross-include-paths)
                   #t)))))))

      (propagated-inputs `(("gnumach-headers" ,xgnumach-headers)
                           ("hurd-headers" ,xhurd-headers)))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ("cross-mig" ,xmig)
                       ,@(alist-delete "mig"(package-native-inputs glibc/hurd-headers))))))

  (define xhurd-minimal
    (package (inherit hurd-minimal)
      (name (string-append (package-name hurd-minimal)
                           "-cross-" target))
      (arguments
       (substitute-keyword-arguments
         `(#:modules ((guix build gnu-build-system)
                      (guix build utils)
                      (srfi srfi-26))
           ,@(package-arguments hurd-minimal))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'configure 'set-cross-headers-path
               (lambda* (#:key inputs #:allow-other-keys)
                 (let* ((glibc-headers (assoc-ref inputs "cross-glibc-hurd-headers"))
                        (cpath (string-append glibc-headers "/include")))
                   (for-each (cut setenv <> cpath)
                             ',%gcc-cross-include-paths)
                   #t)))))))

      (inputs `(("cross-glibc-hurd-headers" ,xglibc/hurd-headers)))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ("cross-mig" ,xmig)
                       ,@(alist-delete "mig"(package-native-inputs hurd-minimal))))))

  (define xhurd-core-headers
    (package (inherit hurd-core-headers)
      (name (string-append (package-name hurd-core-headers)
                           "-cross-" target))

      (inputs `(("gnumach-headers" ,xgnumach-headers)
                ("hurd-headers" ,xhurd-headers)
                ("hurd-minimal" ,xhurd-minimal)))

      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ("cross-mig" ,xmig)
                       ,@(package-native-inputs hurd-core-headers)))))

  (match target
    ((or "i586-pc-gnu" "i586-gnu") xhurd-core-headers)
    (_ xlinux-headers)))

(define* (cross-libc target
                     #:optional
                     (libc glibc)
                     (xgcc (cross-gcc target))
                     (xbinutils (cross-binutils target))
                     (xheaders (cross-kernel-headers target)))
  "Return LIBC cross-built for TARGET, a GNU triplet. Use XGCC and XBINUTILS
and the cross tool chain."
  (if (cross-newlib? target libc)
      (native-libc target libc
                   #:xgcc xgcc
                   #:xbinutils xbinutils)
      (package
        (inherit libc)
        (name (string-append "glibc-cross-" target))
        (arguments
         (substitute-keyword-arguments
             `( ;; Disable stripping (see above.)
               #:strip-binaries? #f

               ;; This package is used as a target input, but it should not have
               ;; the usual cross-compilation inputs since that would include
               ;; itself.
               #:implicit-cross-inputs? #f

               ;; We need SRFI 26.
               #:modules ((guix build gnu-build-system)
                          (guix build utils)
                          (srfi srfi-26))

               ,@(package-arguments libc))
           ((#:configure-flags flags)
            `(cons ,(string-append "--host=" target)
                   ,(if (target-hurd? target)
                        `(cons "--disable-werror" ,flags)
                        flags)))
           ((#:phases phases)
            `(modify-phases ,phases
               (add-before 'configure 'set-cross-kernel-headers-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((kernel (assoc-ref inputs "kernel-headers"))
                          (cpath (string-append kernel "/include")))
                     (for-each (cut setenv <> cpath)
                               ',%gcc-cross-include-paths)
                     (setenv "CROSS_LIBRARY_PATH"
                             (string-append kernel "/lib")) ; for Hurd's libihash
                     #t)))
               ,@(if (target-hurd? target)
                     '((add-after 'install 'augment-libc.so
                         (lambda* (#:key outputs #:allow-other-keys)
                           (let* ((out (assoc-ref outputs "out")))
                             (substitute* (string-append out "/lib/libc.so")
                               (("/[^ ]+/lib/libc.so.0.3")
                                (string-append out "/lib/libc.so.0.3"
                                               " libmachuser.so libhurduser.so"))))
                           #t)))
                     '())))))

        ;; Shadow the native "kernel-headers" because glibc's recipe expects the
        ;; "kernel-headers" input to point to the right thing.
        (propagated-inputs `(("kernel-headers" ,xheaders)))

        (native-inputs `(("cross-gcc" ,xgcc)
                         ("cross-binutils" ,xbinutils)
                         ,@(if (target-hurd? target)
                               `(("cross-mig"
                                  ,@(assoc-ref (package-native-inputs xheaders)
                                               "cross-mig")))
                               '())
                         ,@(package-inputs libc)  ;FIXME: static-bash
                         ,@(package-native-inputs libc))))))

(define* (native-libc target
                     #:optional
                     (libc glibc)
                     #:key
                     xgcc
                     xbinutils)
  (if (target-mingw? target)
      (let ((machine (substring target 0 (string-index target #\-))))
        (make-mingw-w64 machine
                        #:xgcc xgcc
                        #:xbinutils xbinutils))
      libc))

(define* (cross-newlib? target
                       #:optional
                       (libc glibc))
  (not (eq? (native-libc target libc) libc)))


;;; Concrete cross tool chains are instantiated like this:
;;
;; (define-public xgcc-armhf
;;   (let ((triplet "arm-linux-gnueabihf"))
;;     (cross-gcc triplet
;;                #:xbinutils (cross-binutils triplet)
;;                #:libc (cross-libc triplet))))
;;
;;; We don't do that here because we'd be referring to bindings from (gnu
;;; packages gcc) from the top level, which doesn't play well with circular
;;; dependencies among modules.
