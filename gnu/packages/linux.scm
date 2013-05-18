;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages linux)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module ((gnu packages compression)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (gnu packages flex)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public (system->linux-architecture arch)
  "Return the Linux architecture name for ARCH, a Guix system name such as
\"x86_64-linux\"."
  (let ((arch (car (string-split arch #\-))))
    (cond ((string=? arch "i686") "i386")
          ((string-prefix? "mips" arch) "mips")
          ((string-prefix? "arm" arch) "arm")
          (else arch))))

(define-public linux-libre-headers
  (let* ((version* "3.3.8")
         (build-phase
          (lambda (arch)
            `(lambda _
               (setenv "ARCH" ,(system->linux-architecture arch))
               (format #t "`ARCH' set to `~a'~%" (getenv "ARCH"))

               (and (zero? (system* "make" "defconfig"))
                    (zero? (system* "make" "mrproper" "headers_check"))))))
         (install-phase
          `(lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (and (zero? (system* "make"
                                    (string-append "INSTALL_HDR_PATH=" out)
                                    "headers_install"))
                    (mkdir (string-append out "/include/config"))
                    (call-with-output-file
                        (string-append out
                                       "/include/config/kernel.release")
                      (lambda (p)
                        (format p "~a-default~%" ,version*))))))))
   (package
    (name "linux-libre-headers")
    (version version*)
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://linux-libre.fsfla.org/pub/linux-libre/releases/3.3.8-gnu/linux-libre-"
                   version "-gnu.tar.xz"))
             (sha256
              (base32
               "0jkfh0z1s6izvdnc3njm39dhzp1cg8i06jv06izwqz9w9qsprvnl"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases (alist-replace
                 'build ,(build-phase (%current-system))
                 (alist-replace
                  'install ,install-phase
                  (alist-delete 'configure %standard-phases)))
       #:tests? #f))
    (synopsis "GNU Linux-Libre kernel headers")
    (description "Headers of the Linux-Libre kernel.")
    (license gpl2)
    (home-page "http://www.gnu.org/software/linux-libre/"))))

(define-public module-init-tools
  (package
    (name "module-init-tools")
    (version "3.16")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kernel.org/linux/utils/kernel/module-init-tools/module-init-tools-"
                   version ".tar.bz2"))
             (sha256
              (base32
               "0jxnz9ahfic79rp93l5wxcbgh4pkv85mwnjlbv1gz3jawv5cvwp1"))))
    (build-system gnu-build-system)
    (inputs
     ;; The upstream tarball lacks man pages, and building them would require
     ;; DocBook & co.  Thus, use Gentoo's pre-built man pages.
     `(("man-pages"
        ,(origin
          (method url-fetch)
          (uri (string-append
                "http://distfiles.gentoo.org/distfiles/module-init-tools-" version
                "-man.tar.bz2"))
          (sha256
           (base32
            "1j1nzi87kgsh4scl645fhwhjvljxj83cmdasa4n4p5krhasgw358"))))))
    (arguments
     '(#:phases (alist-cons-before
                 'unpack 'unpack-man-pages
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((man-pages (assoc-ref inputs "man-pages")))
                     (zero? (system* "tar" "xvf" man-pages))))
                 %standard-phases)))
    (home-page "http://www.kernel.org/pub/linux/utils/kernel/module-init-tools/")
    (synopsis "Tools for loading and managing Linux kernel modules")
    (description
     "Tools for loading and managing Linux kernel modules, such as `modprobe',
`insmod', `lsmod', and more.")
    (license gpl2+)))

(define-public linux-libre
  (let* ((version* "3.3.8")
         (build-phase
          '(lambda* (#:key system #:allow-other-keys #:rest args)
             (let ((arch (car (string-split system #\-))))
               (setenv "ARCH"
                       (cond ((string=? arch "i686") "i386")
                             (else arch)))
               (format #t "`ARCH' set to `~a'~%" (getenv "ARCH")))

             (let ((build (assoc-ref %standard-phases 'build)))
               (and (zero? (system* "make" "defconfig"))
                    (begin
                      (format #t "enabling additional modules...~%")
                      (substitute* ".config"
                        (("^# CONFIG_CIFS.*$")
                         "CONFIG_CIFS=m\n"))
                      (zero? (system* "make" "oldconfig")))

                    ;; Call the default `build' phase so `-j' is correctly
                    ;; passed.
                    (apply build #:make-flags "all" args)))))
         (install-phase
          `(lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (moddir (string-append out "/lib/modules"))
                    (mit    (assoc-ref inputs "module-init-tools")))
               (mkdir-p moddir)
               (for-each (lambda (file)
                           (copy-file file
                                      (string-append out "/" (basename file))))
                         (find-files "." "^(bzImage|System\\.map)$"))
               (copy-file ".config" (string-append out "/config"))
               (zero? (system* "make"
                               (string-append "DEPMOD=" mit "/sbin/depmod")
                               (string-append "MODULE_DIR=" moddir)
                               (string-append "INSTALL_PATH=" out)
                               (string-append "INSTALL_MOD_PATH=" out)
                               "modules_install"))))))
   (package
    (name "linux-libre")
    (version version*)
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://linux-libre.fsfla.org/pub/linux-libre/releases/3.3.8-gnu/linux-libre-"
                   version "-gnu.tar.xz"))
             (sha256
              (base32
               "0jkfh0z1s6izvdnc3njm39dhzp1cg8i06jv06izwqz9w9qsprvnl"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)
                     ("module-init-tools" ,module-init-tools)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (ice-9 match))
       #:phases (alist-replace
                 'build ,build-phase
                 (alist-replace
                  'install ,install-phase
                  (alist-delete 'configure %standard-phases)))
       #:tests? #f))
    (synopsis "100% free redistribution of a cleaned Linux kernel")
    (description "Linux-Libre operating system kernel.")
    (license gpl2)
    (home-page "http://www.gnu.org/software/linux-libre/"))))

(define-public linux-pam
  (package
    (name "linux-pam")
    (version "1.1.6")
    (source
     (origin
      (method url-fetch)
      (uri (list (string-append "http://www.linux-pam.org/library/Linux-PAM-"
                                version ".tar.bz2")
                 (string-append "mirror://kernel.org/linux/libs/pam/library/Linux-PAM-"
                                version ".tar.bz2")))
      (sha256
       (base32
        "1hlz2kqvbjisvwyicdincq7nz897b9rrafyzccwzqiqg53b8gf5s"))))
    (build-system gnu-build-system)
    (inputs
     `(("flex" ,flex)

       ;; TODO: optional dependencies
       ;; ("libxcrypt" ,libxcrypt)
       ;; ("cracklib" ,cracklib)
       ))
    (arguments
     ;; XXX: Tests won't run in chroot, presumably because /etc/pam.d
     ;; isn't available.
     '(#:tests? #f))
    (home-page "http://www.linux-pam.org/")
    (synopsis "Pluggable authentication modules for Linux")
    (description
     "A *Free* project to implement OSF's RFC 86.0.
Pluggable authentication modules are small shared object files that can
be used through the PAM API to perform tasks, like authenticating a user
at login.  Local and dynamic reconfiguration are its key features")
    (license bsd-3)))

(define-public psmisc
  (package
    (name "psmisc")
    (version "22.20")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/psmisc/psmisc/psmisc-"
                          version ".tar.gz"))
      (sha256
       (base32
        "052mfraykmxnavpi8s78aljx8w87hyvpx8mvzsgpjsjz73i28wmi"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://psmisc.sourceforge.net/")
    (synopsis
     "set of utilities that use the proc filesystem, such as fuser, killall, and pstree")
    (description
     "This PSmisc package is a set of some small useful utilities that
use the proc filesystem. We're not about changing the world, but
providing the system administrator with some help in common tasks.")
    (license gpl2+)))

(define-public util-linux
  (package
    (name "util-linux")
    (version "2.21")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kernel.org/linux/utils/"
                          name "/v" version "/"
                          name "-" version ".2" ".tar.xz"))
      (sha256
       (base32
        "1rpgghf7n0zx0cdy8hibr41wvkm2qp1yvd8ab1rxr193l1jmgcir"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-use-tty-group")
       #:phases (alist-cons-after
                 'install 'patch-chkdupexe
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* (string-append out "/bin/chkdupexe")
                       ;; Allow 'patch-shebang' to do its work.
                       (("@PERL@") "/bin/perl"))))
                 %standard-phases)))
    (inputs `(("zlib" ,guix:zlib)
              ("ncurses" ,ncurses)
              ("perl" ,perl)))
    (home-page "https://www.kernel.org/pub/linux/utils/util-linux/")
    (synopsis "Collection of utilities for the Linux kernel")
    (description
     "util-linux is a random collection of utilities for the Linux kernel.")

    ;; Note that util-linux doesn't use the same license for all the
    ;; code.  GPLv2+ is the default license for a code without an
    ;; explicitly defined license.
    (license (list gpl3+ gpl2+ gpl2 lgpl2.0+
                   bsd-4 public-domain))))

(define-public procps
  (package
    (name "procps")
    (version "3.2.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://procps.sourceforge.net/procps-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0d8mki0q4yamnkk4533kx8mc0jd879573srxhg6r2fs3lkc6iv8i"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("patch/make-3.82" ,(search-patch "procps-make-3.82.patch"))))
    (arguments
     '(#:patches (list (assoc-ref %build-inputs "patch/make-3.82"))
       #:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; No `configure', just a single Makefile.
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* "Makefile"
                       (("/usr/") "/")
                       (("--(owner|group) 0") "")
                       (("ldconfig") "true")
                       (("^LDFLAGS[[:blank:]]*:=(.*)$" _ value)
                        ;; Add libproc to the RPATH.
                        (string-append "LDFLAGS := -Wl,-rpath="
                                       out "/lib" value))))
                   (setenv "CC" "gcc"))
                 (alist-replace
                  'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out")))
                      (and (zero?
                            (system* "make" "install"
                                     (string-append "DESTDIR=" out)))

                           ;; Sanity check.
                           (zero?
                            (system* (string-append out "/bin/ps")
                                     "--version")))))
                  %standard-phases))

       ;; What did you expect?  Tests?
       #:tests? #f))
    (home-page "http://procps.sourceforge.net/")
    (synopsis "Utilities that give information about processes")
    (description
     "procps is the package that has a bunch of small useful utilities
that give information about processes using the Linux /proc file system.
The package includes the programs ps, top, vmstat, w, kill, free,
slabtop, and skill.")
    (license gpl2)))

(define-public usbutils
  (package
    (name "usbutils")
    (version "006")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kernel.org/linux/utils/usb/usbutils/"
                          "usbutils-" version ".tar.xz"))
      (sha256
       (base32
        "03pd57vv8c6x0hgjqcbrxnzi14h8hcghmapg89p8k5zpwpkvbdfr"))))
    (build-system gnu-build-system)
    (inputs
     `(("libusb" ,libusb) ("pkg-config" ,pkg-config)))
    (home-page "http://www.linux-usb.org/")
    (synopsis
     "Tools for working with USB devices, such as lsusb")
    (description
     "Tools for working with USB devices, such as lsusb.")
    (license gpl2+)))

(define-public e2fsprogs
  (package
    (name "e2fsprogs")
    (version "1.42.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/e2fsprogs/e2fsprogs-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0ibkkvp6kan0hn0d1anq4n2md70j5gcm7mwna515w82xwyr02rfw"))))
    (build-system gnu-build-system)
    (inputs `(("util-linux" ,util-linux)
              ("pkg-config" ,pkg-config)))
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'patch-shells
                 (lambda _
                   (substitute* "configure"
                     (("/bin/sh (.*)parse-types.sh" _ dir)
                      (string-append (which "sh") " " dir
                                     "parse-types.sh")))
                   (substitute* (find-files "." "^Makefile.in$")
                     (("#!/bin/sh")
                      (string-append "#!" (which "sh")))))
                 %standard-phases)

       ;; FIXME: Tests work by comparing the stdout/stderr of programs, that
       ;; they fail because we get an extra line that says "Can't check if
       ;; filesystem is mounted due to missing mtab file".
       #:tests? #f))
    (home-page "http://e2fsprogs.sourceforge.net/")
    (synopsis "Creating and checking ext2/ext3/ext4 file systems")
    (description
     "This package provides tools for manipulating ext2/ext3/ext4 file systems.")
    (license (list gpl2                           ; programs
                   lgpl2.0                        ; libext2fs
                   x11))))                        ; libuuid
