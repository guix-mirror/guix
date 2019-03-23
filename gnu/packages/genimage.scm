;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu packages genimage)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages virtualization))

(define-public genimage
  (package
    (name "genimage")
    (version "10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pengutronix/genimage.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0had00p2why2l1fl14mq7nbhmmfbd3na4qnnpg36akdy05g67jbn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'guixify
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Note to maintainers: Check ".def =" lines in source.
             (substitute* "config.c"
              (("\\.def = \"cpio\"")
               (string-append ".def = \""
                              (assoc-ref inputs "cpio")
                              "/bin/cpio\""))
              (("\\.def = \"dd\"")
               (string-append ".def = \""
                              (assoc-ref inputs "coreutils")
                              "/bin/dd\""))
              (("\\.def = \"debugfs\"")
               (string-append ".def = \""
                              (assoc-ref inputs "e2fsprogs")
                              "/sbin/debugfs\""))
              (("\\.def = \"e2fsck\"")
               (string-append ".def = \""
                              (assoc-ref inputs "e2fsprogs")
                              "/sbin/e2fsck\""))
              (("\\.def = \"genext2fs\"")
               (string-append ".def = \""
                              (assoc-ref inputs "genext2fs")
                              "/bin/genext2fs\""))
              (("\\.def = \"genisoimage\"")
               (string-append ".def = \""
                              (assoc-ref inputs "cdrkit-libre")
                              "/bin/genisoimage\""))
              (("\\.def = \"mcopy\"")
               (string-append ".def = \""
                              (assoc-ref inputs "mtools")
                              "/bin/mcopy\""))
              (("\\.def = \"mmd\"")
               (string-append ".def = \""
                              (assoc-ref inputs "mtools")
                              "/bin/mmd\""))
              ;;; Note: mkcramfs is obsolete.
              (("\\.def = \"mkdosfs\"")
               (string-append ".def = \""
                              (assoc-ref inputs "dosfstools")
                              "/sbin/mkfs.fat\""))
              (("\\.def = \"mke2fs\"")
               (string-append ".def = \""
                              (assoc-ref inputs "e2fsprogs")
                              "/sbin/mke2fs\""))
              (("\\.def = \"mkfs\\.jffs2\"")
               (string-append ".def = \""
                              (assoc-ref inputs "mtd-utils")
                              "/sbin/mkfs.jffs2\""))
              (("\\.def = \"mkfs\\.ubifs\"")
               (string-append ".def = \""
                              (assoc-ref inputs "mtd-utils")
                              "/sbin/mkfs.ubifs\""))
              (("\\.def = \"mksquashfs\"")
               (string-append ".def = \""
                              (assoc-ref inputs "squashfs-tools")
                              "/bin/mksquashfs\""))
              (("\\.def = \"qemu-img\"")
               (string-append ".def = \""
                              (assoc-ref inputs "qemu")
                              "/bin/qemu-img\""))
              (("\\.def = \"tar\"")
               (string-append ".def = \""
                              (assoc-ref inputs "tar")
                              "/bin/tar\""))
              (("\\.def = \"tune2fs\"")
               (string-append ".def = \""
                              (assoc-ref inputs "e2fsprogs")
                              "/sbin/tune2fs\""))
              (("\\.def = \"ubinize\"")
               (string-append ".def = \""
                              (assoc-ref inputs "mtd-utils")
                              "/sbin/ubinize\""))
              (("\\.def = \"mkimage\"")
               (string-append ".def = \""
                              (assoc-ref inputs "u-boot-tools")
                              "/bin/mkimage\"")))
             (substitute* "test/basic-images.test"
              ;; Work around bug in sharness.sh.
              (("mkdosfs")
               "mkfs.fat")
              ;; Work around bug in sharness.sh.
              (("dd,mkfs\\.fat,mcopy")
               "dd,mkfs_fat,mcopy")
              ;; Should be in the next upstream release.
              (("qemu_img") "qemu-img"))
             (substitute* "util.c"
              (("\"/bin/sh\"")
               (string-append "\"" (assoc-ref inputs "bash") "/bin/sh\"")))
             ;; We don't have /etc/passwd so uid 0 is not known as "root".
             ;; Thus patch it out.
             (substitute* '("test/ext2test.dump"
                            "test/ext3test.dump"
                            "test/ext4test.dump"
                            "test/mke2fs.dump")
              (("root") "unknown"))
             #t))
         (add-before 'check 'setenv-check
           (lambda _
             ;; Our container doesn't provide access to /etc/mtab
             (setenv "EXT2FS_NO_MTAB_OK" "1")
             ;; Make test reproducible
             (setenv "GENIMAGE_MKFJFFS2" "mkfs.jffs2 -U")
             (setenv "GENIMAGE_MKE2FS" "mke2fs -E no_copy_xattrs")
             #t))
         (replace 'check
           (lambda _
             (invoke "make" "TEST_LOG_COMPILER=" "check"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ;;; Note: cramfs is obsolete.
       ("dtc" ,dtc) ; for the tests
       ("fdisk" ,fdisk) ; for the tests
       ("pkg-config" ,pkg-config)
       ("util-linux" ,util-linux))) ; for the tests
    (inputs
     `(("bash" ,bash)
       ("cdrkit-libre" ,cdrkit-libre)
       ("cpio" ,cpio)
       ;; Note: invoked by final executable.
       ("coreutils" ,coreutils) ; chmod, dd
       ("dosfstools" ,dosfstools)
       ("e2fsprogs" ,e2fsprogs)
       ("genext2fs" ,genext2fs)
       ("libconfuse" ,libconfuse)
       ("mtd-utils" ,mtd-utils)
       ("mtools" ,mtools)
       ("qemu" ,qemu-minimal)
       ("squashfs-tools" ,squashfs-tools)
       ("tar" ,tar)
       ("u-boot-tools" ,u-boot-tools)))
    (synopsis "Create Flash images according to specification")
    (description "@command{genimage} creates Flash images according to a
specification file.")
    (home-page "https://github.com/pengutronix/genimage")
    (license license:gpl2)))
