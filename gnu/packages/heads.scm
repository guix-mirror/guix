;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu packages heads)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages python)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages file)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module ((guix build utils) #:select (alist-replace)))

(define-public musl-cross
  (let ((revision "3")
        (commit "a8a66490dae7f23a2cf5e256f3a596d1ccfe1a03"))
  (package
    (name "musl-cross")
    (version (git-version "0.1" revision commit))
    (source
     (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/GregorR/musl-cross")
              (commit commit)))
        (file-name "musl-cross-checkout")
        (sha256
         (base32
          "1xvl9y017wb2qaphy9zqh3vrhm8hklr8acvzzcjc35d1jjhyl58y"))
        (patches (search-patches "musl-cross-locale.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests in main project.
       #:modules
       ((guix build utils)
        (guix build gnu-build-system)
        (srfi srfi-1)) ; drop
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (setenv "SHELL" "bash")
             (setenv "CONFIG_SHELL" "bash")
             #t))
         (add-after 'unpack 'unpack-dependencies
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (define (install-file* source-key destination-directory
                                    destination-suffix)
               (let* ((source-file (assoc-ref inputs source-key))
                      (source-basename (basename source-file))
                      (source-parts (string-split source-basename #\-))
                      (destination-file
                       (string-join (drop source-parts 1) "-")))
                 (copy-file source-file
                  (string-append destination-directory "/"
                                 destination-file destination-suffix))))
             (for-each (lambda (name)
                         (install-file* name "tarballs" ""))
                       '("binutils" "target-gcc-5" "linux-headers" "musl"))
             (copy-file (string-append (assoc-ref inputs "config.sub")
                                       "/share/automake-1.16/config.sub")
                        "tarballs/config.sub;hb=3d5db9ebe860")
             (copy-file (string-append (assoc-ref inputs "config.sub")
                                       "/share/automake-1.16/config.guess")
                        "tarballs/config.guess;hb=3d5db9ebe860")
             (substitute* "config.sh"
              (("^CC_BASE_PREFIX=.*")
               (string-append "CC_BASE_PREFIX=" (assoc-ref outputs "out")
                              "/crossgcc\n")))
             ;; Note: Important: source/gcc-5.3.0/gcc/exec-tool.in
             ;; Note: Important: source/kernel-headers-3.12.6-5/tools/install.sh
             ;; Note: Important: move-if-change (twice)
             ;; Make sure that shebangs are patched after new extractions.
             (substitute* "defs.sh"
              (("touch \"[$]2/extracted\"")
               (string-append "touch \"$2/extracted\"
for s in mkinstalldirs move-if-change compile depcomp callprocs configure \\
mkdep compile libtool-ldflags config.guess install-sh missing config.sub \\
config.rpath progtest.m4 lib-ld.m4 acx.m4 gen-fixed.sh mkheader.sh ylwrap \\
merge.sh godeps.sh lock-and-run.sh print-sysroot-suffix.sh mkconfig.sh \\
genmultilib exec-tool.in install.sh
do
  find . -name $s -exec sed -i -e 's;!/bin/sh;!" (assoc-ref inputs "bash")
"/bin/sh;' '{}' ';'
  find . -name $s -exec sed -i -e 's; /bin/sh; " (assoc-ref inputs "bash")
"/bin/sh;' '{}' ';'
done
" )))
             #t))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "./build.sh")))
         (delete 'install))))
    (native-inputs
     `(("config.sub" ,automake)
       ("bash" ,bash)
       ("flex" ,flex)
       ("gmp" ,gmp)
       ("mpfr" ,mpfr)
       ("mpc" ,mpc)
       ("binutils"
        ,(origin
           (method url-fetch)
           (uri "https://ftpmirror.gnu.org/gnu/binutils/binutils-2.27.tar.bz2")
           (sha256
            (base32 "125clslv17xh1sab74343fg6v31msavpmaa1c1394zsqa773g5rn"))))
       ("target-gcc-5"
        ,(origin
           (method url-fetch)
           (uri "https://ftpmirror.gnu.org/gnu/gcc/gcc-5.3.0/gcc-5.3.0.tar.bz2")
           (sha256
            (base32 "1ny4smkp5bzs3cp8ss7pl6lk8yss0d9m4av1mvdp72r1x695akxq"))))
       ("linux-headers"
        ,(origin
           (method url-fetch)
           (uri "http://ftp.barfooze.de/pub/sabotage/tarballs/linux-headers-4.19.88.tar.xz")
           (sha256
            (base32 "1srgi2nqw892jb6yd4kzacf2xzwfvzhsv2957xfh1nvbs7varwyk"))))
       ("musl"
        ,(origin
           (method url-fetch)
           (uri "http://www.musl-libc.org/releases/musl-1.1.24.tar.gz")
           (sha256
            (base32 "18r2a00k82hz0mqdvgm7crzc7305l36109c0j9yjmkxj2alcjw0k"))))))
    (home-page "https://github.com/osresearch/heads")
    (synopsis "Musl-cross gcc 5 toolchain")
    (description "Musl-cross toolchain: binutils, gcc 5 and musl.")
    (license license:isc))))

;; This package provides a "dev.cpio" file usable as a base for booting Heads.
(define-public heads-dev-cpio
  (package
    (name "heads-dev-cpio")
    (version "0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix cpio))
       #:builder (begin
                   (use-modules (guix build utils)
                                (guix cpio)
                                (srfi srfi-26))
                   (mkdir-p "dev") ; input directory.
                   (let* ((out (assoc-ref %outputs "out"))
                          (libexec (string-append out "/libexec")))
                     (mkdir-p libexec)
                     (call-with-output-file (string-append libexec "/dev.cpio")
                      (lambda (port)
                        (write-cpio-archive '("dev" "dev/console") port
                         #:file->header
                         (lambda (name)
                           (if (string=? "dev/console" name)
                               (special-file->cpio-header* name 'char-special 5 1 #o600)
                               (file->cpio-header* name))))))
                     #t))))
    (synopsis "@file{dev.cpio} for Heads")
    (description "This package provides a @file{dev.cpio} file usable as a
base for heads' initrd.")
    (home-page "https://osresearch.net/")
    (license license:bsd-2)))
