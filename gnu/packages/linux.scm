;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((guix licenses)
                #:hide (zlib))
  #:use-module (gnu packages)
  #:use-module ((gnu packages compression)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (gnu packages flex)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages bdb)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages autotools)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

(define-public (system->linux-architecture arch)
  "Return the Linux architecture name for ARCH, a Guix system name such as
\"x86_64-linux\"."
  (let ((arch (car (string-split arch #\-))))
    (cond ((string=? arch "i686") "i386")
          ((string-prefix? "mips" arch) "mips")
          ((string-prefix? "arm" arch) "arm")
          (else arch))))

(define (linux-libre-urls version)
  "Return a list of URLs for Linux-Libre VERSION."
  (list (string-append
         "http://linux-libre.fsfla.org/pub/linux-libre/releases/"
         version "-gnu/linux-libre-" version "-gnu.tar.xz")

        ;; XXX: Work around <http://bugs.gnu.org/14851>.
        (string-append
         "ftp://alpha.gnu.org/gnu/guix/mirror/linux-libre-"
         version "-gnu.tar.xz")

        ;; Maybe this URL will become valid eventually.
        (string-append
         "mirror://gnu/linux-libre/" version "-gnu/linux-libre-"
         version "-gnu.tar.xz")))

(define-public linux-libre-headers
  (let* ((version "3.3.8")
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
                        (format p "~a-default~%" ,version))))))))
   (package
    (name "linux-libre-headers")
    (version version)
    (source (origin
             (method url-fetch)
             (uri (linux-libre-urls version))
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
                 'build ,(build-phase (or (%current-target-system)
                                          (%current-system)))
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
    (arguments
     ;; FIXME: The upstream tarball lacks man pages, and building them would
     ;; require DocBook & co.  We used to use Gentoo's pre-built man pages,
     ;; but they vanished.  In the meantime, fake it.
     '(#:phases (alist-cons-before
                 'configure 'fake-docbook
                 (lambda _
                   (substitute* "Makefile.in"
                     (("^DOCBOOKTOMAN.*$")
                      "DOCBOOKTOMAN = true\n")))
                 %standard-phases)))
    (home-page "http://www.kernel.org/pub/linux/utils/kernel/module-init-tools/")
    (synopsis "Tools for loading and managing Linux kernel modules")
    (description
     "Tools for loading and managing Linux kernel modules, such as `modprobe',
`insmod', `lsmod', and more.")
    (license gpl2+)))

(define-public linux-libre
  (let* ((version "3.13")
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
                         "CONFIG_CIFS=m\n")
                        (("^# CONFIG_([[:graph:]]*)VIRTIO([[:graph:]]*) .*$"
                          _ before after)
                         (string-append "CONFIG_" before "VIRTIO"
                                        after "=m\n")))
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
    (version version)
    (source (origin
             (method url-fetch)
             (uri (linux-libre-urls version))
             (sha256
              (base32
               "15pdizzxnnvpxmdb1lbi01kpingmdvj17b01vzbyjymi4vwfws3f"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)
                     ("bc" ,bc)
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
    (description
     "GNU Linux-Libre is a free (as in freedom) variant of the Linux kernel.
It has been modified to remove all non-free binary blobs.")
    (license gpl2)
    (home-page "http://www.gnu.org/software/linux-libre/"))))


;;;
;;; Pluggable authentication modules (PAM).
;;;

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
    (native-inputs
     `(("flex" ,flex)

       ;; TODO: optional dependencies
       ;; ("libxcrypt" ,libxcrypt)
       ;; ("cracklib" ,cracklib)
       ))
    (arguments
     '(;; Most users, such as `shadow', expect the headers to be under
       ;; `security'.
       #:configure-flags (list (string-append "--includedir="
                                              (assoc-ref %outputs "out")
                                              "/include/security"))

       ;; XXX: Tests won't run in chroot, presumably because /etc/pam.d
       ;; isn't available.
       #:tests? #f))
    (home-page "http://www.linux-pam.org/")
    (synopsis "Pluggable authentication modules for Linux")
    (description
     "A *Free* project to implement OSF's RFC 86.0.
Pluggable authentication modules are small shared object files that can
be used through the PAM API to perform tasks, like authenticating a user
at login.  Local and dynamic reconfiguration are its key features")
    (license bsd-3)))


;;;
;;; Miscellaneous.
;;;

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
              ("ncurses" ,ncurses)))
    (native-inputs
     `(("perl" ,perl)))
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
               "0d8mki0q4yamnkk4533kx8mc0jd879573srxhg6r2fs3lkc6iv8i"))
             (patches (list (search-patch "procps-make-3.82.patch")))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (arguments
     '(#:phases (alist-replace
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
     `(("libusb" ,libusb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
    (inputs `(("util-linux" ,util-linux)))
    (native-inputs `(("pkg-config" ,pkg-config)))
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

(define-public strace
  (package
    (name "strace")
    (version "4.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/strace/strace-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "158iwk0pl2mfw93m1843xb7a2zb8p6lh0qim07rca6f1ff4dk764"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)))
    (home-page "http://strace.sourceforge.net/")
    (synopsis "System call tracer for Linux")
    (description
     "strace is a system call tracer, i.e. a debugging tool which prints out a
trace of all the system calls made by a another process/program.")
    (license bsd-3)))

(define-public alsa-lib
  (package
    (name "alsa-lib")
    (version "1.0.27.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "ftp://ftp.alsa-project.org/pub/lib/alsa-lib-"
                   version ".tar.bz2"))
             (sha256
              (base32
               "0fx057746dj7rjdi0jnvx2m9b0y1lgdkh1hks87d8w32xyihf3k9"))
             (patches (list (search-patch "alsa-lib-mips-atomic-fix.patch")))))
    (build-system gnu-build-system)
    (home-page "http://www.alsa-project.org/")
    (synopsis "The Advanced Linux Sound Architecture libraries")
    (description
     "The Advanced Linux Sound Architecture (ALSA) provides audio and
MIDI functionality to the Linux-based operating system.")
    (license lgpl2.1+)))

(define-public alsa-utils
  (package
    (name "alsa-utils")
    (version "1.0.27.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp.alsa-project.org/pub/utils/alsa-utils-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1sjjngnq50jv5ilwsb4zys6smifni3bd6fn28gbnhfrg14wsrgq2"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: Disable man page creation until we have DocBook.
     '(#:configure-flags (list "--disable-xmlto"
                               (string-append "--with-udev-rules-dir="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))
       #:phases (alist-cons-before
                 'install 'pre-install
                 (lambda _
                   ;; Don't try to mkdir /var/lib/alsa.
                   (substitute* "Makefile"
                     (("\\$\\(MKDIR_P\\) .*ASOUND_STATE_DIR.*")
                      "true\n")))
                 %standard-phases)))
    (inputs
     `(("libsamplerate" ,libsamplerate)
       ("ncurses" ,ncurses)
       ("alsa-lib" ,alsa-lib)
       ("xmlto" ,xmlto)
       ("gettext" ,gnu-gettext)))
    (home-page "http://www.alsa-project.org/")
    (synopsis "Utilities for the Advanced Linux Sound Architecture (ALSA)")
    (description
     "The Advanced Linux Sound Architecture (ALSA) provides audio and
MIDI functionality to the Linux-based operating system.")

    ;; This is mostly GPLv2+ but a few files such as 'alsactl.c' are
    ;; GPLv2-only.
    (license gpl2)))

(define-public iptables
  (package
    (name "iptables")
    (version "1.4.16.2")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.netfilter.org/projects/iptables/files/iptables-"
                   version ".tar.bz2"))
             (sha256
              (base32
               "0vkg5lzkn4l3i1sm6v3x96zzvnv9g7mi0qgj6279ld383mzcws24"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))                    ; no test suite
    (home-page "http://www.netfilter.org/projects/iptables/index.html")
    (synopsis "Program to configure the Linux IP packet filtering rules")
    (description
     "iptables is the userspace command line program used to configure the
Linux 2.4.x and later IPv4 packet filtering ruleset.  It is targeted towards
system administrators.  Since Network Address Translation is also configured
from the packet filter ruleset, iptables is used for this, too.  The iptables
package also includes ip6tables.  ip6tables is used for configuring the IPv6
packet filter.")
    (license gpl2+)))

(define-public iproute
  (package
    (name "iproute2")
    (version "3.12.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kernel.org/linux/utils/net/iproute2/iproute2-"
                   version ".tar.xz"))
             (sha256
              (base32
               "04gi11gh087bg2nlxhj0lxrk8l9qxkpr88nsiil23917bm3h1xj4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                                ; no test suite
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "DESTDIR="
                            (string-append "LIBDIR=" out "/lib")
                            (string-append "SBINDIR=" out "/sbin")
                            (string-append "CONFDIR=" out "/etc")
                            (string-append "DOCDIR=" out "/share/doc/"
                                           ,name "-" ,version)
                            (string-append "MANDIR=" out "/share/man")))
       #:phases (alist-cons-before
                 'install 'pre-install
                 (lambda _
                   ;; Don't attempt to create /var/lib/arpd.
                   (substitute* "Makefile"
                     (("^.*ARPDDIR.*$") "")))
                 %standard-phases)))
    (inputs
     `(("iptables" ,iptables)
       ("db4" ,bdb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("flex" ,flex)
       ("bison" ,bison)))
    (home-page
     "http://www.linuxfoundation.org/collaborate/workgroups/networking/iproute2")
    (synopsis
     "A collection of utilities for controlling TCP/IP networking and traffic control in Linux")
    (description
     "Iproute2 is a collection of utilities for controlling TCP/IP
networking and traffic with the Linux kernel.

Most network configuration manuals still refer to ifconfig and route as the
primary network configuration tools, but ifconfig is known to behave
inadequately in modern network environments.  They should be deprecated, but
most distros still include them.  Most network configuration systems make use
of ifconfig and thus provide a limited feature set.  The /etc/net project aims
to support most modern network technologies, as it doesn't use ifconfig and
allows a system administrator to make use of all iproute2 features, including
traffic control.

iproute2 is usually shipped in a package called iproute or iproute2 and
consists of several tools, of which the most important are ip and tc.  ip
controls IPv4 and IPv6 configuration and tc stands for traffic control.  Both
tools print detailed usage messages and are accompanied by a set of
manpages.")
    (license gpl2+)))

(define-public net-tools
  ;; XXX: This package is basically unmaintained, but it provides a few
  ;; commands not yet provided by Inetutils, such as 'route', so we have to
  ;; live with it.
  (package
    (name "net-tools")
    (version "1.60")
    (home-page "http://www.tazenda.demon.co.uk/phil/net-tools/")
    (source (origin
             (method url-fetch)
             (uri (string-append home-page "/" name "-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0yvxrzk0mzmspr7sa34hm1anw6sif39gyn85w4c5ywfn8inxvr3s"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-after
                 'unpack 'patch
                 (lambda* (#:key inputs #:allow-other-keys)
                   (define (apply-patch file)
                     (zero? (system* "patch" "-p1" "--batch"
                                     "--input" file)))

                   (let ((patch.gz (assoc-ref inputs "patch")))
                     (format #t "applying Debian patch set '~a'...~%"
                             patch.gz)
                     (system (string-append "gunzip < " patch.gz " > the-patch"))
                     (pk 'here)
                     (and (apply-patch "the-patch")
                          (for-each apply-patch
                                    (find-files "debian/patches"
                                                "\\.patch")))))
                 (alist-replace
                  'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out")))
                      (mkdir-p (string-append out "/bin"))
                      (mkdir-p (string-append out "/sbin"))

                      ;; Pretend we have everything...
                      (system "yes | make config")

                      ;; ... except we don't have libdnet, so remove that
                      ;; definition.
                      (substitute* '("config.make" "config.h")
                        (("^.*HAVE_AFDECnet.*$") ""))))
                  %standard-phases))

       ;; Binaries that depend on libnet-tools.a don't declare that
       ;; dependency, making it parallel-unsafe.
       #:parallel-build? #f

       #:tests? #f                                ; no test suite
       #:make-flags (list "CC=gcc"
                          (string-append "BASEDIR="
                                         (assoc-ref %outputs "out")))))

    ;; Use the big Debian patch set (the thing does not even compile out of
    ;; the box.)
    (inputs `(("patch" ,(origin
                         (method url-fetch)
                         (uri
                          "http://ftp.de.debian.org/debian/pool/main/n/net-tools/net-tools_1.60-24.2.diff.gz")
                         (sha256
                          (base32
                           "0p93lsqx23v5fv4hpbrydmfvw1ha2rgqpn2zqbs2jhxkzhjc030p"))))))
    (native-inputs `(("gettext" ,gnu-gettext)))

    (synopsis "Tools for controlling the network subsystem in Linux")
    (description
     "This package includes the important tools for controlling the network
subsystem of the Linux kernel.  This includes arp, hostname, ifconfig,
netstat, rarp and route.  Additionally, this package contains utilities
relating to particular network hardware types (plipconfig, slattach) and
advanced aspects of IP configuration (iptunnel, ipmaddr).")
    (license gpl2+)))

(define-public libcap
  (package
    (name "libcap")
    (version "2.22")
    (source (origin
             (method url-fetch)

             ;; Tarballs used to be available from
             ;; <https://www.kernel.org/pub/linux/libs/security/linux-privs/>
             ;; but they never came back after kernel.org was compromised.
             (uri (string-append
                   "mirror://debian/pool/main/libc/libcap2/libcap2_"
                   version ".orig.tar.gz"))
             (sha256
              (base32
               "07vjhkznm82p8dm4w6j8mmg7h5c70lp5s9bwwfdmgwpbixfydjp1"))))
    (build-system gnu-build-system)
    (arguments '(#:phases (alist-delete 'configure %standard-phases)
                 #:tests? #f                      ; no 'check' target
                 #:make-flags (list "lib=lib"
                                    (string-append "prefix="
                                                   (assoc-ref %outputs "out"))
                                    "RAISE_SETFCAP=no")))
    (native-inputs `(("perl" ,perl)))
    (inputs `(("attr" ,attr)))
    (home-page "https://sites.google.com/site/fullycapable/")
    (synopsis "Library for working with POSIX capabilities")
    (description
     "libcap2 provides a programming interface to POSIX capabilities on
Linux-based operating systems.")

    ;; License is BSD-3 or GPLv2, at the user's choice.
    (license gpl2)))

(define-public bridge-utils
  (package
    (name "bridge-utils")
    (version "1.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/bridge/bridge-utils-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "12367cwqmi0yqphi6j8rkx97q8hw52yq2fx4k0xfclkcizxybya2"))))
    (build-system gnu-build-system)

    ;; The tarball lacks all the generated files.
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)))
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'bootstrap
                 (lambda _
                   (zero? (system* "autoreconf" "-vf")))
                 %standard-phases)
       #:tests? #f))                              ; no 'check' target

    (home-page
     "http://www.linuxfoundation.org/collaborate/workgroups/networking/bridge")
    (synopsis "Manipulate Ethernet bridges")
    (description
     "Utilities for Linux's Ethernet bridging facilities.  A bridge is a way
to connect two Ethernet segments together in a protocol independent way.
Packets are forwarded based on Ethernet address, rather than IP address (like
a router).  Since forwarding is done at Layer 2, all protocols can go
transparently through a bridge.")
    (license gpl2+)))

(define-public libnl
  (package
    (name "libnl")
    (version "3.2.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.infradead.org/~tgr/libnl/files/libnl-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1ydw42lsd572qwrfgws97n76hyvjdpanwrxm03lysnhfxkna1ssd"))))
    (build-system gnu-build-system)
    (native-inputs `(("flex" ,flex) ("bison" ,bison)))
    (home-page "http://www.infradead.org/~tgr/libnl/")
    (synopsis "NetLink protocol library suite")
    (description
     "The libnl suite is a collection of libraries providing APIs to netlink
protocol based Linux kernel interfaces.  Netlink is an IPC mechanism primarly
between the kernel and user space processes.  It was designed to be a more
flexible successor to ioctl to provide mainly networking related kernel
configuration and monitoring interfaces.")

    ;; Most files are LGPLv2.1-only, but some are GPLv2-only (like
    ;; 'nl-addr-add.c'), so the result is GPLv2-only.
    (license gpl2)))

(define-public powertop
  (package
    (name "powertop")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://01.org/powertop/sites/default/files/downloads/powertop-"
             version ".tar.gz"))
       (sha256
        (base32
         "02rwqbpasdayl201v0549gbp2f82rd0hqiv3i111r7npanjhhb4b"))))
    (build-system gnu-build-system)
    (inputs
     ;; TODO: Add pciutils.
     `(("zlib" ,guix:zlib)
       ;; ("pciutils" ,pciutils)
       ("ncurses" ,ncurses)
       ("libnl" ,libnl)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "https://01.org/powertop/")
    (synopsis "Analyze power consumption on Intel-based laptops")
    (description
     "PowerTOP is a Linux tool to diagnose issues with power consumption and
power management.  In addition to being a diagnostic tool, PowerTOP also has
an interactive mode where the user can experiment various power management
settings for cases where the operating system has not enabled these
settings.")
    (license gpl2)))

(define-public aumix
  (package
    (name "aumix")
    (version "2.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.jpj.net/~trevor/aumix/releases/aumix-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0a8fwyxnc5qdxff8sl2sfsbnvgh6pkij4yafiln0fxgg6bal7knj"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://www.jpj.net/~trevor/aumix.html")
    (synopsis "Audio mixer for X and the console")
    (description
     "Aumix adjusts an audio mixer from X, the console, a terminal,
the command line or a script.")
    (license gpl2+)))

(define-public iotop
  (package
    (name "iotop")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://guichaz.free.fr/iotop/files/iotop-"
                           version ".tar.gz"))
       (sha256 (base32
                "1kp8mqg2pbxq4xzpianypadfxcsyfgwcaqgqia6h9fsq6zyh4z0s"))))
    (build-system python-build-system)
    (arguments
     ;; The setup.py script expects python-2.
     `(#:python ,python-2
       ;; There are currently no checks in the package.
       #:tests? #f))
    (native-inputs `(("python" ,python-2)))
    (home-page "http://guichaz.free.fr/iotop/")
    (synopsis
     "Displays the IO activity of running processes")
    (description
     "Iotop is a Python program with a top like user interface to show the
processes currently causing I/O.")
    (license gpl2+)))
