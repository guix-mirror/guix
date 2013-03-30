;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages bootstrap)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix store) #:select (add-to-store add-text-to-store))
  #:use-module ((guix derivations) #:select (derivation))
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (bootstrap-origin
            package-with-bootstrap-guile
            glibc-dynamic-linker

            %bootstrap-guile
            %bootstrap-coreutils&co
            %bootstrap-binutils
            %bootstrap-gcc
            %bootstrap-glibc
            %bootstrap-inputs))

;;; Commentary:
;;;
;;; Pre-built packages that are used to bootstrap the
;;; distribution--i.e., to build all the core packages from scratch.
;;;
;;; Code:



;;;
;;; Helper procedures.
;;;

(define (bootstrap-origin source)
  "Return a variant of SOURCE, an <origin> instance, whose method uses
%BOOTSTRAP-GUILE to do its job."
  (define (boot fetch)
    (lambda* (store url hash-algo hash
              #:optional name #:key system)
      (fetch store url hash-algo hash
             #:guile %bootstrap-guile
             #:system system)))

  (let ((orig-method (origin-method source)))
    (origin (inherit source)
      (method (cond ((eq? orig-method url-fetch)
                     (boot url-fetch))
                    (else orig-method))))))

(define (package-from-tarball name* source* program-to-test description*)
  "Return a package that correspond to the extraction of SOURCE*.
PROGRAM-TO-TEST is a program to run after extraction of SOURCE*, to
check whether everything is alright."
  (package
    (name name*)
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (let ((out     (assoc-ref %outputs "out"))
             (tar     (assoc-ref %build-inputs "tar"))
             (xz      (assoc-ref %build-inputs "xz"))
             (tarball (assoc-ref %build-inputs "tarball")))
         (use-modules (guix build utils))

         (mkdir out)
         (copy-file tarball "binaries.tar.xz")
         (system* xz "-d" "binaries.tar.xz")
         (let ((builddir (getcwd)))
           (with-directory-excursion out
             (and (zero? (system* tar "xvf"
                                  (string-append builddir "/binaries.tar")))
                  (zero? (system* (string-append "bin/" ,program-to-test)
                                  "--version"))))))))
    (inputs
     `(("tar" ,(search-bootstrap-binary "tar" (%current-system)))
       ("xz"  ,(search-bootstrap-binary "xz" (%current-system)))
       ("tarball" ,(bootstrap-origin (source* (%current-system))))))
    (synopsis description*)
    (description #f)
    (home-page #f)
    (license #f)))

(define package-with-bootstrap-guile
  (memoize
   (lambda (p)
    "Return a variant of P such that all its origins are fetched with
%BOOTSTRAP-GUILE."
    (define rewritten-input
      (match-lambda
       ((name (? origin? o))
        `(,name ,(bootstrap-origin o)))
       ((name (? package? p) sub-drvs ...)
        `(,name ,(package-with-bootstrap-guile p) ,@sub-drvs))
       (x x)))

    (package (inherit p)
      (source (match (package-source p)
                ((? origin? o) (bootstrap-origin o))
                (s s)))
      (inputs (map rewritten-input
                   (package-inputs p)))
      (native-inputs (map rewritten-input
                          (package-native-inputs p)))
      (propagated-inputs (map rewritten-input
                              (package-propagated-inputs p)))))))

(define* (glibc-dynamic-linker #:optional (system (%current-system)))
  "Return the name of Glibc's dynamic linker for SYSTEM."
  (cond ((string=? system "x86_64-linux") "/lib/ld-linux-x86-64.so.2")
        ((string=? system "i686-linux") "/lib/ld-linux.so.2")
        ((string=? system "mips64el-linux") "/lib/ld.so.1")
        (else (error "dynamic linker name not known for this system"
                     system))))


;;;
;;; Bootstrap packages.
;;;

(define %bootstrap-guile
  ;; The Guile used to run the build scripts of the initial derivations.
  ;; It is just unpacked from a tarball containing a pre-built binary.
  ;; This is typically built using %GUILE-BOOTSTRAP-TARBALL below.
  ;;
  ;; XXX: Would need libc's `libnss_files2.so' for proper `getaddrinfo'
  ;; support (for /etc/services).
  (let ((raw (build-system
              (name "raw")
              (description "Raw build system with direct store access")
              (build (lambda* (store name source inputs
                                     #:key outputs system search-paths)
                       (define (->store file)
                         (add-to-store store file #t "sha256"
                                       (or (search-bootstrap-binary file
                                                                    system)
                                           (error "bootstrap binary not found"
                                                  file system))))

                       (let* ((tar   (->store "tar"))
                              (xz    (->store "xz"))
                              (mkdir (->store "mkdir"))
                              (bash  (->store "bash"))
                              (guile (->store "guile-2.0.7.tar.xz"))
                              (builder
                               (add-text-to-store store
                                                  "build-bootstrap-guile.sh"
                                                  (format #f "
echo \"unpacking bootstrap Guile to '$out'...\"
~a $out
cd $out
~a -dc < ~a | ~a xv

# Sanity check.
$out/bin/guile --version~%"
                                                          mkdir xz guile tar)
                                                  (list mkdir xz guile tar))))
                         (derivation store name system
                                     bash `(,builder) '()
                                     `((,bash) (,builder)))))))))
   (package
     (name "guile-bootstrap")
     (version "2.0")
     (source #f)
     (build-system raw)
     (synopsis "Bootstrap Guile")
     (description "Pre-built Guile for bootstrapping purposes.")
     (home-page #f)
     (license lgpl3+))))

(define %bootstrap-base-urls
  ;; This is where the initial binaries come from.
  '("http://alpha.gnu.org/gnu/guix/bootstrap"
    "http://www.fdn.fr/~lcourtes/software/guix/packages"))

(define %bootstrap-coreutils&co
  (package-from-tarball "bootstrap-binaries"
                        (lambda (system)
                          (origin
                           (method url-fetch)
                           (uri (map (cut string-append <> "/" system
                                          "/20130105/static-binaries.tar.xz")
                                     %bootstrap-base-urls))
                           (sha256
                            (match system
                              ("x86_64-linux"
                               (base32
                                "0md23alzy6nc5f16pric7mkagczdzr8xbh074sb3rjzrls06j1ls"))
                              ("i686-linux"
                               (base32
                                "0nzj1lmm9b94g7k737cr4w1dv282w5nmhb53238ikax9r6pkc0yb"))))))
                        "true"                    ; the program to test
                        "Bootstrap binaries of Coreutils, Awk, etc."))

(define %bootstrap-binutils
  (package-from-tarball "binutils-bootstrap"
                        (lambda (system)
                          (origin
                           (method url-fetch)
                           (uri (map (cut string-append <> "/" system
                                          "/20130105/binutils-2.22.tar.xz")
                                     %bootstrap-base-urls))
                           (sha256
                            (match system
                              ("x86_64-linux"
                               (base32
                                "1ffmk2yy2pxvkqgzrkzp3s4jpn4qaaksyk3b5nsc5cjwfm7qkgzh"))
                              ("i686-linux"
                               (base32
                                "1rafk6aq4sayvv3r3d2khn93nkyzf002xzh0xadlyci4mznr6b0a"))))))
                        "ld"                      ; the program to test
                        "Bootstrap binaries of the GNU Binutils"))

(define %bootstrap-glibc
  ;; The initial libc.
  (package
    (name "glibc-bootstrap")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (let ((out     (assoc-ref %outputs "out"))
             (tar     (assoc-ref %build-inputs "tar"))
             (xz      (assoc-ref %build-inputs "xz"))
             (tarball (assoc-ref %build-inputs "tarball")))
         (use-modules (guix build utils))

         (mkdir out)
         (copy-file tarball "binaries.tar.xz")
         (system* xz "-d" "binaries.tar.xz")
         (let ((builddir (getcwd)))
           (with-directory-excursion out
             (system* tar "xvf"
                      (string-append builddir
                                     "/binaries.tar"))
             (chmod "lib" #o755)

             ;; Patch libc.so so it refers to the right path.
             (substitute* "lib/libc.so"
               (("/[^ ]+/lib/(libc|ld)" _ prefix)
                (string-append out "/lib/" prefix))))))))
    (inputs
     `(("tar" ,(search-bootstrap-binary "tar" (%current-system)))
       ("xz"  ,(search-bootstrap-binary "xz" (%current-system)))
       ("tarball" ,(bootstrap-origin
                    (origin
                     (method url-fetch)
                     (uri (map (cut string-append <> "/" (%current-system)
                                    "/20130105/glibc-2.17.tar.xz")
                               %bootstrap-base-urls))
                     (sha256
                      (match (%current-system)
                        ("x86_64-linux"
                         (base32
                          "18kv1z9d8dr1j3hm9w7663kchqw9p6rsx11n1m143jgba2jz6jy3"))
                        ("i686-linux"
                         (base32
                          "08hv8i0axwnihrcgbz19x0a7s6zyv3yx38x8r29liwl8h82x9g88")))))))))
    (synopsis "Bootstrap binaries and headers of the GNU C Library")
    (description #f)
    (home-page #f)
    (license lgpl2.1+)))

(define %bootstrap-gcc
  ;; The initial GCC.  Uses binaries from a tarball typically built by
  ;; %GCC-BOOTSTRAP-TARBALL.
  (package
    (name "gcc-bootstrap")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (let ((out     (assoc-ref %outputs "out"))
             (tar     (assoc-ref %build-inputs "tar"))
             (xz      (assoc-ref %build-inputs "xz"))
             (bash    (assoc-ref %build-inputs "bash"))
             (libc    (assoc-ref %build-inputs "libc"))
             (tarball (assoc-ref %build-inputs "tarball")))
         (use-modules (guix build utils)
                      (ice-9 popen))

         (mkdir out)
         (copy-file tarball "binaries.tar.xz")
         (system* xz "-d" "binaries.tar.xz")
         (let ((builddir (getcwd))
               (bindir   (string-append out "/bin")))
           (with-directory-excursion out
             (system* tar "xvf"
                      (string-append builddir "/binaries.tar")))

           (with-directory-excursion bindir
             (chmod "." #o755)
             (rename-file "gcc" ".gcc-wrapped")
             (call-with-output-file "gcc"
               (lambda (p)
                 (format p "#!~a
exec ~a/bin/.gcc-wrapped -B~a/lib \
     -Wl,-rpath -Wl,~a/lib \
     -Wl,-dynamic-linker -Wl,~a/~a \"$@\"~%"
                         bash
                         out libc libc libc
                         ,(glibc-dynamic-linker))))

             (chmod "gcc" #o555))))))
    (inputs
     `(("tar" ,(search-bootstrap-binary "tar" (%current-system)))
       ("xz"  ,(search-bootstrap-binary "xz" (%current-system)))
       ("bash" ,(search-bootstrap-binary "bash" (%current-system)))
       ("libc" ,%bootstrap-glibc)
       ("tarball" ,(bootstrap-origin
                    (origin
                     (method url-fetch)
                     (uri (map (cut string-append <> "/" (%current-system)
                                    "/20130105/gcc-4.7.2.tar.xz")
                               %bootstrap-base-urls))
                     (sha256
                      (match (%current-system)
                        ("x86_64-linux"
                         (base32
                          "1x1p7han5crnbw906iwdifykr6grzm0w27dy9gz75j0q1b32i4px"))
                        ("i686-linux"
                         (base32
                          "06wqs0xxnpw3hn0xjb4c9cs0899p1xwkcysa2rvzhvpra0c5vsg2")))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "CPATH")
            (directories '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (directories '("lib" "lib64")))))
    (synopsis "Bootstrap binaries of the GNU Compiler Collection")
    (description #f)
    (home-page #f)
    (license gpl3+)))

(define %bootstrap-inputs
  ;; The initial, pre-built inputs.  From now on, we can start building our
  ;; own packages.
  `(("libc" ,%bootstrap-glibc)
    ("gcc" ,%bootstrap-gcc)
    ("binutils" ,%bootstrap-binutils)
    ("coreutils&co" ,%bootstrap-coreutils&co)

    ;; In gnu-build-system.scm, we rely on the availability of Bash.
    ("bash" ,%bootstrap-coreutils&co)))

;;; bootstrap.scm ends here
