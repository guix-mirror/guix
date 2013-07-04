;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (cross-binutils
            cross-libc
            cross-gcc))

(define (cross p target)
  (package (inherit p)
    (location (source-properties->location (current-source-location)))
    (name (string-append (package-name p) "-cross-" target))
    (arguments
     (substitute-keyword-arguments (package-arguments p)
       ((#:configure-flags flags)
        `(cons ,(string-append "--target=" target)
               ,flags))))))

(define (cross-binutils target)
  "Return a cross-Binutils for TARGET."
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
    (cross binutils target)))

(define* (cross-gcc target
                    #:optional (xbinutils (cross-binutils target)) libc)
  "Return a cross-compiler for TARGET, where TARGET is a GNU triplet.  Use
XBINUTILS as the associated cross-Binutils.  If LIBC is false, then build a
GCC that does not target a libc; otherwise, target that libc."
  (package (inherit gcc-4.7)
    (name (string-append "gcc-cross-"
                         (if libc "" "sans-libc-")
                         target))
    (arguments
     `(#:implicit-inputs? #f
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 regex)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:patches (list (assoc-ref %build-inputs "patch/cross-env-vars"))

       ,@(substitute-keyword-arguments (package-arguments gcc-4.7)
           ((#:configure-flags flags)
            `(append (list ,(string-append "--target=" target)
                           ,@(gcc-configure-flags-for-triplet target)
                           ,@(if libc
                                 '()
                                 `( ;; Disable features not needed at this stage.
                                   "--disable-shared" "--enable-static"

                                   ;; Disable C++ because libstdc++'s
                                   ;; configure script otherwise fails with
                                   ;; "Link tests are not allowed after
                                   ;; GCC_NO_EXECUTABLES."
                                   "--enable-languages=c"

                                   "--disable-threads" ; libgcc, would need libc
                                   "--disable-libmudflap"
                                   "--disable-libgomp"
                                   "--disable-libssp"
                                   "--disable-libquadmath"
                                   "--disable-decimal-float" ; would need libc
                                   )))

                     ,(if libc
                          flags
                          `(remove (cut string-match "--enable-languages.*" <>)
                                   ,flags))))
           ((#:make-flags flags)
            (if libc
                `(let ((libc (assoc-ref %build-inputs "libc")))
                   ;; FLAGS_FOR_TARGET are needed for the target libraries to
                   ;; receive the -Bxxx for the startfiles.
                   (cons (string-append "FLAGS_FOR_TARGET=-B" libc "/lib")
                         ,flags))
                flags))
           ((#:phases phases)
            (let ((phases
                   `(alist-cons-after
                     'install 'make-cross-binutils-visible
                     (lambda* (#:key outputs inputs #:allow-other-keys)
                       (let* ((out      (assoc-ref outputs "out"))
                              (libexec  (string-append out "/libexec/gcc/"
                                                       ,target))
                              (binutils (string-append
                                         (assoc-ref inputs "binutils-cross")
                                         "/bin/" ,target "-")))
                         (for-each (lambda (file)
                                     (symlink (string-append binutils file)
                                              (string-append libexec "/"
                                                             file)))
                                   '("as" "ld" "nm"))
                         #t))
                     ,phases)))
             (if libc
                 `(alist-cons-before
                   'configure 'set-cross-path
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; Add the cross Linux headers to CROSS_CPATH, and remove
                     ;; them from CPATH.
                     (let ((libc  (assoc-ref inputs "libc"))
                           (linux (assoc-ref inputs
                                             "libc/cross-linux-headers")))
                       (define (cross? x)
                         ;; Return #t if X is a cross-libc or cross Linux.
                         (or (string-prefix? libc x)
                             (string-prefix? linux x)))

                       (setenv "CROSS_CPATH"
                               (string-append libc "/include:"
                                              linux "/include"))
                       (setenv "CROSS_LIBRARY_PATH"
                               (string-append libc "/lib"))

                       (let ((cpath   (search-path-as-string->list
                                       (getenv "CPATH")))
                             (libpath (search-path-as-string->list
                                       (getenv "LIBRARY_PATH"))))
                         (setenv "CPATH"
                                 (list->search-path-as-string
                                  (remove cross? cpath) ":"))
                         (setenv "LIBRARY_PATH"
                                 (list->search-path-as-string
                                  (remove cross? libpath) ":"))
                         #t)))
                   ,phases)
                 phases)))
           ((#:strip-binaries? _)
            ;; Disable stripping as this can break binaries, with object files
            ;; of libgcc.a showing up as having an unknown architecture.  See
            ;; <http://lists.fedoraproject.org/pipermail/arm/2010-August/000663.html>
            ;; for instance.
            #f))))

    (native-inputs
     `(("patch/cross-env-vars"
        ,(search-patch "gcc-cross-environment-variables.patch"))

       ("binutils-cross" ,xbinutils)

       ;; Call it differently so that the builder can check whether the "libc"
       ;; input is #f.
       ("libc-native" ,@(assoc-ref %final-inputs "libc"))

       ;; Remaining inputs.
       ,@(let ((inputs (append (package-inputs gcc-4.7)
                               (alist-delete "libc" %final-inputs))))
           (if libc
               `(("libc" ,libc)
                 ,@inputs)
               inputs))))

    (inputs '())

    ;; Only search target inputs, not host inputs.
    (search-paths
     (list (search-path-specification
            (variable "CROSS_CPATH")
            (directories '("include")))
           (search-path-specification
            (variable "CROSS_LIBRARY_PATH")
            (directories '("lib" "lib64")))))
    (native-search-paths '())))

(define* (cross-libc target
                     #:optional
                     (xgcc (cross-gcc target))
                     (xbinutils (cross-binutils target)))
  "Return a libc cross-built for TARGET, a GNU triplet.  Use XGCC and
XBINUTILS and the cross tool chain."
  (define xlinux-headers
    (package (inherit linux-libre-headers)
      (name (string-append (package-name linux-libre-headers)
                           "-cross-" target))
      (arguments
       (substitute-keyword-arguments (package-arguments linux-libre-headers)
         ((#:phases phases)
          `(alist-replace
            'build
            (lambda _
              (setenv "ARCH" ,(system->linux-architecture target))
              (format #t "`ARCH' set to `~a' (cross compiling)~%" (getenv "ARCH"))

              (and (zero? (system* "make" "defconfig"))
                   (zero? (system* "make" "mrproper" "headers_check"))))
            ,phases))))
      (native-inputs `(("cross-gcc" ,xgcc)
                       ("cross-binutils" ,xbinutils)
                       ,@(package-native-inputs linux-libre-headers)))))

  (package (inherit glibc)
    (name (string-append "glibc-cross-" target))
    (arguments
     (substitute-keyword-arguments
         `(#:strip-binaries? #f                ; disable stripping (see above)
           ,@(package-arguments glibc))
       ((#:configure-flags flags)
        `(cons ,(string-append "--host=" target)
               ,flags))
       ((#:phases phases)
        `(alist-cons-before
          'configure 'set-cross-linux-headers-path
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((linux (assoc-ref inputs "cross-linux-headers")))
              (setenv "CROSS_CPATH"
                      (string-append linux "/include"))
              #t))
          ,phases))))

    (propagated-inputs `(("cross-linux-headers" ,xlinux-headers)))
    (native-inputs `(("cross-gcc" ,xgcc)
                     ("cross-binutils" ,xbinutils)
                     ,@(package-native-inputs glibc)))))


;;;
;;; Concrete cross toolchains.
;;;

(define-public xgcc-mips64el
  (let ((triplet "mips64el-linux-gnuabi64"))      ; N64 ABI
    (cross-gcc triplet
               (cross-binutils triplet)
               (cross-libc triplet))))

;; (define-public xgcc-armel
;;   (let ((triplet "armel-linux-gnueabi"))
;;     (cross-gcc triplet
;;                (cross-binutils triplet)
;;                (cross-libc triplet))))
