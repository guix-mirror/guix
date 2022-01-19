;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2018. 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Guy Fleury Iteriteka <hoonandon@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020, 2021 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Dion Mendel <guix@dm9.info>
;;; Copyright © 2021 Andrew Whatson <whatson@gmail.com>
;;; Copyright © 2021 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2022 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu packages virtualization)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cluster)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages figlet)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define (qemu-patch commit file-name sha256-bv)
  "Return an origin for COMMIT."
  (origin
    (method url-fetch)
    (uri (string-append
          "http://git.qemu.org/?p=qemu.git;a=commitdiff_plain;h="
          commit))
    (hash (content-hash sha256-bv sha256))
    (file-name file-name)))

(define-public qemu
  (package
    (name "qemu")
    (version "6.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.qemu.org/qemu-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0iavlsy9hin8k38230j8lfmyipx3965zljls1dp34mmc8n75vqb8"))
       (patches (search-patches "qemu-build-info-manual.patch"
                                "qemu-fix-agent-paths.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete the bundled meson copy.
           (delete-file-recursively "meson")))))
    (outputs '("out" "static" "doc"))   ;5.3 MiB of HTML docs
    (build-system gnu-build-system)
    (arguments
     ;; FIXME: Disable tests on i686 to work around
     ;; <https://bugs.gnu.org/40527>.
     `(#:tests? ,(or (%current-target-system)
                     (not (string=? "i686-linux" (%current-system))))
       #:configure-flags
       (let ((gcc (search-input-file %build-inputs "/bin/gcc"))
             (out (assoc-ref %outputs "out")))
         (list (string-append "--cc=" gcc)
               ;; Some architectures insist on using HOST_CC.
               (string-append "--host-cc=" gcc)
               (string-append "--prefix=" out)
               "--sysconfdir=/etc"
               (string-append "--smbd=" out "/libexec/samba-wrapper")
               "--disable-debug-info"   ;for space considerations
               ;; The binaries need to be linked against -lrt.
               (string-append "--extra-ldflags=-lrt")))
       ;; Make build and test output verbose to facilitate investigation upon failure.
       #:make-flags '("V=1")
       #:modules ((srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 ftw)
                  (ice-9 match)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'extend-test-time-outs
           (lambda _
             ;; These tests can time out on heavily-loaded and/or slow storage.
             (substitute* (cons* "tests/qemu-iotests/common.qemu"
                                 (find-files "tests/qemu-iotests" "^[0-9]+$"))
               (("QEMU_COMM_TIMEOUT=[0-9]+" match)
                (string-append match "9")))))
         (add-after 'unpack 'disable-unusable-tests
           (lambda _
             (substitute* "tests/unit/meson.build"
               ;; Comment out the test-qga test, which needs /sys and
               ;; fails within the build environment.
               (("tests.*test-qga.*$" all)
                (string-append "# " all))
               ;; Comment out the test-char test, which needs networking and
               ;; fails within the build environment.
               ((".*'test-char':.*" all)
                (string-append "# " all)))))
         (add-after 'patch-source-shebangs 'patch-embedded-shebangs
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             ;; Ensure the executables created by these source files reference
             ;; /bin/sh from the store so they work inside the build container.
             (substitute* '("block/cloop.c" "migration/exec.c"
                            "net/tap.c" "tests/qtest/libqtest.c"
                            "tests/qtest/vhost-user-blk-test.c")
               (("/bin/sh") (search-input-file inputs "/bin/sh")))
             (substitute* "tests/qemu-iotests/testenv.py"
               (("#!/usr/bin/env python3")
                (string-append "#!" (search-input-file (or native-inputs inputs)
                                                       "/bin/python3"))))))
         (add-before 'configure 'fix-optionrom-makefile
           (lambda _
             ;; Work around the inability of the rules defined in this
             ;; Makefile to locate the firmware files (e.g.: No rule to make
             ;; target 'multiboot.bin') by extending the VPATH.
             (substitute* "pc-bios/optionrom/Makefile"
               (("^VPATH = \\$\\(SRC_DIR\\)")
                "VPATH = $(SRC_DIR):$(TOPSRC_DIR)/pc-bios"))))
         ;; XXX ./configure is being re-run at beginning of build phase...
         (replace 'configure
           (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
             ;; The `configure' script doesn't understand some of the
             ;; GNU options.  Thus, add a new phase that's compatible.
             (let ((out (assoc-ref outputs "out")))
               (setenv "SHELL" (which "bash"))
               ;; Ensure config.status gets the correct shebang off the bat.
               ;; The build system gets confused if we change it later and
               ;; attempts to re-run the whole configuration, and fails.
               (substitute* "configure"
                 (("#!/bin/sh")
                  (string-append "#!" (which "sh"))))
               (mkdir-p "b/qemu")
               (chdir "b/qemu")
               (apply invoke "../../configure" configure-flags))))
         ;; Configure, build and install QEMU user-emulation static binaries.
         (add-after 'configure 'configure-user-static
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((gcc (search-input-file inputs "/bin/gcc"))
                    (static (assoc-ref outputs "static"))
                    ;; This is the common set of configure flags; it is
                    ;; duplicated here to isolate this phase from manipulations
                    ;; to the #:configure-flags build argument, as done in
                    ;; derived packages such as qemu-minimal.
                    (configure-flags (list (string-append "--cc=" gcc)
                                           (string-append "--host-cc=" gcc)
                                           "--sysconfdir=/etc"
                                           "--disable-debug-info")))
               (mkdir-p "../user-static")
               (with-directory-excursion "../user-static"
                 (apply invoke "../../configure"
                        "--static"
                        "--disable-docs" ;already built
                        "--disable-system"
                        "--enable-linux-user"
                        (string-append "--prefix=" static)
                        configure-flags)))))
         (add-after 'build 'build-user-static
           (lambda args
             (with-directory-excursion "../user-static"
               (apply (assoc-ref %standard-phases 'build) args))))
         (add-after 'install 'install-user-static
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((static (assoc-ref outputs "static"))
                    (bin (string-append static "/bin")))
               (with-directory-excursion "../user-static"
                 (for-each (cut install-file <> bin)
                           (append-map (cut find-files <> "^qemu-" #:stat stat)
                                       (scandir "."
                                                (cut string-suffix?
                                                     "-linux-user" <>))))))))
         ;; Create a wrapper for Samba. This allows QEMU to use Samba without
         ;; pulling it in as an input. Note that you need to explicitly install
         ;; Samba in your Guix profile for Samba support.
         (add-after 'install 'create-samba-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (libexec (string-append out "/libexec")))
               (call-with-output-file "samba-wrapper"
                 (lambda (port)
                   (format port "#!/bin/sh
exec smbd $@")))
               (chmod "samba-wrapper" #o755)
               (install-file "samba-wrapper" libexec))))
         (add-after 'install 'move-html-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc"))
                    (qemu-doc (string-append doc "/share/doc/qemu-" ,version)))
               (mkdir-p qemu-doc)
               (rename-file (string-append out "/share/doc/qemu")
                            (string-append qemu-doc "/html"))))))))
    (inputs
     (list alsa-lib
           bash-minimal
           glib
           gtk+
           libaio
           libcacard                    ;smartcard support
           attr libcap-ng               ;VirtFS support
           libdrm
           libepoxy
           libjpeg-turbo
           libpng
           libseccomp
           libusb                       ;USB pass-through support
           mesa
           ncurses
           ;; ("pciutils" ,pciutils)
           pixman
           pulseaudio
           sdl2
           spice
           usbredir
           util-linux
           vde2
           virglrenderer
           zlib))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ;gtester, etc.
           perl
           flex
           bison
           meson
           ninja
           pkg-config
           python-wrapper
           python-sphinx
           python-sphinx-rtd-theme
           texinfo
           ;; The following static libraries are required to build
           ;; the static output of QEMU.
           `(,glib "static")
           `(,pcre "static")
           `(,zlib "static")))
    (home-page "https://www.qemu.org")
    (synopsis "Machine emulator and virtualizer")
    (description
     "QEMU is a generic machine emulator and virtualizer.

When used as a machine emulator, QEMU can run OSes and programs made for one
machine (e.g. an ARM board) on a different machine---e.g., your own PC.  By
using dynamic translation, it achieves very good performance.

When used as a virtualizer, QEMU achieves near native performances by
executing the guest code directly on the host CPU.  QEMU supports
virtualization when executing under the Xen hypervisor or using
the KVM kernel module in Linux.  When using KVM, QEMU can virtualize x86,
server and embedded PowerPC, and S390 guests.")

    ;; Many files are GPLv2+, but some are GPLv2-only---e.g., `memory.c'.
    (license license:gpl2)

    ;; Several tests fail on MIPS; see <http://hydra.gnu.org/build/117914>.
    (supported-systems (fold delete %supported-systems
                             '("mips64el-linux" "i586-gnu")))))

(define-public qemu-minimal
  ;; QEMU without GUI support, only supporting the host's architecture
  (package
    (inherit qemu)
    (name "qemu-minimal")
    (outputs '("out" "doc"))
    (synopsis
     "Machine emulator and virtualizer (without GUI) for the host architecture")
    (arguments
     (substitute-keyword-arguments (package-arguments qemu)
       ((#:configure-flags configure-flags '(list))
        ;; Restrict to the host's architecture.
        (let* ((system (or (%current-target-system)
                           (%current-system)))
               (target-list-arg
                (match system
                  ((? (cut string-prefix? "i686" <>))
                   "--target-list=i386-softmmu")
                  ((? (cut string-prefix? "x86_64" <>))
                   "--target-list=i386-softmmu,x86_64-softmmu")
                  ((? (cut string-prefix? "mips64" <>))
                   (string-append "--target-list=mips-softmmu,mipsel-softmmu,"
                                  "mips64-softmmu,mips64el-softmmu"))
                  ((? (cut string-prefix? "mips" <>))
                   "--target-list=mips-softmmu,mipsel-softmmu")
                  ((? (cut string-prefix? "aarch64" <>))
                   "--target-list=arm-softmmu,aarch64-softmmu")
                  ((? (cut string-prefix? "arm" <>))
                   "--target-list=arm-softmmu")
                  ((? (cut string-prefix? "alpha" <>))
                   "--target-list=alpha-softmmu")
                  ((? (cut string-prefix? "powerpc64" <>))
                   "--target-list=ppc-softmmu,ppc64-softmmu")
                  ((? (cut string-prefix? "powerpc" <>))
                   "--target-list=ppc-softmmu")
                  ((? (cut string-prefix? "s390" <>))
                   "--target-list=s390x-softmmu")
                  ((? (cut string-prefix? "riscv" <>))
                   "--target-list=riscv32-softmmu,riscv64-softmmu")
                  (else       ; An empty list actually builds all the targets.
                   '()))))
          `(cons ,target-list-arg ,configure-flags)))
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'configure-user-static)
           (delete 'build-user-static)
           (delete 'install-user-static)))))

    ;; Remove dependencies on optional libraries, notably GUI libraries.
    (native-inputs (filter (lambda (input)
                             (match input
                               ;; Work around the fact that modify-inputs can not
                               ;; delete specific outputs; i.e. here we should keep
                               ;; `(,glib "bin"), but not `(,glib "static").
                               ((label package output)
                                (not (string=? "static" output)))
                               (_ input)))
                           (modify-inputs (package-native-inputs qemu)
                             (delete "gettext-minimal"))))
    (inputs (modify-inputs (package-inputs qemu)
              (delete "libusb"
                      "mesa"
                      "sdl2"
                      "spice"
                      "virglrenderer"
                      "gtk+"
                      "usbredir"
                      "libdrm"
                      "libepoxy"
                      "pulseaudio"
                      "vde2"
                      "libcacard")))))

(define (system->qemu-target system)
  (cond
   ((string-prefix? "i686" system)
    "qemu-system-i386")
   ((string-prefix? "arm" system)
    "qemu-system-arm")
   (else
    (string-append "qemu-system-" (match (string-split system #\-)
                                    ((arch kernel) arch)
                                    (_ system))))))

(define-public libx86emu
  (package
    (name "libx86emu")
    (version "3.5")
    (home-page "https://github.com/wfeldt/libx86emu")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url home-page)
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11nj3y7maz9ch15b1c2b69gd8d7mpaha377zpdbvfsmg5w9zz93l"))
       (modules
        '((guix build utils)))
       (snippet
        `(begin
           ;; Remove git2log program file.
           (delete-file "git2log")
           ;; Remove variables that depends on git2log.
           (substitute* "Makefile"
             (("GIT2LOG.*=.*$") "")
             (("GITDEPS.*=.*$") "")
             (("BRANCH.*=.*$") ""))))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (include (string-append out "/include"))
                    (lib (string-append out "/lib")))
               ;; Correct the values of version and install directories.
               (substitute* "Makefile"
                 (("VERSION.*=.*$")
                  (string-append "VERSION := "
                                 ,version "\n"))
                 (("PREFIX.*=.*$")
                  (string-append "PREFIX := " out "\n"))
                 (("MAJOR_VERSION.*=.*$")
                  (string-append "MAJOR_VERSION := "
                                 ,(version-major version) "\n"))
                 (("LIBDIR.*=.*$")
                  (string-append "LIBDIR = " lib "\n"))
                 (("/usr/include") include)))))
         (delete 'configure))))         ; no configure script
    (native-inputs
     (list nasm perl))
    (synopsis "Library for x86 emulation")
    (description "Libx86emu is a small library to emulate x86 instructions.  The
focus here is not a complete emulation but to cover enough for typical
firmware blobs.  You can
@enumerate
@item intercept any memory access or directly map real memory ranges
@item intercept any i/o access, map real i/o ports, or block any real i/o
@item intercept any interrupt
@item add a hook to run after each instruction
@item recognize a special x86 instruction that can trigger logging
@item use integrated logging
@end enumerate")
    (license (license:x11-style "file://LICENSE"))))

(define-public ganeti
  (package
    (name "ganeti")
    (version "3.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ganeti/ganeti")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "1i7gx0sdx9316fnldbv738s0ihym1370nhc1chk0biandkl8vvq0"))
              (file-name (git-file-name name version))
              (patches (search-patches "ganeti-shepherd-support.patch"
                                       "ganeti-shepherd-master-failover.patch"
                                       "ganeti-sphinx-compat.patch"
                                       "ganeti-haskell-compat.patch"
                                       "ganeti-haskell-pythondir.patch"
                                       "ganeti-disable-version-symlinks.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:imported-modules (,@%gnu-build-system-modules
                           (guix build haskell-build-system)
                           (guix build python-build-system))
       #:modules (,@%gnu-build-system-modules
                  ((guix build haskell-build-system) #:prefix haskell:)
                  ((guix build python-build-system) #:select (site-packages))
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 match)
                  (ice-9 rdelim))

       ;; The default test target includes a lot of checks that are only really
       ;; relevant for developers such as NEWS file checking, line lengths, etc.
       ;; We are only interested in the "py-tests" and "hs-tests" targets: this
       ;; is the closest we've got even though it includes a little more.
       #:test-target "check-TESTS"

       #:configure-flags
       (list "--localstatedir=/var"
             "--sharedstatedir=/var"
             "--sysconfdir=/etc"
             "--enable-haskell-tests"

             ;; By default, the build system installs everything to versioned
             ;; directories such as $libdir/3.0 and relies on a $libdir/default
             ;; symlink pointed from /etc/ganeti/{lib,share} to actually function.
             ;; This is done to accommodate installing multiple versions in
             ;; parallel, but is of little use to us as Guix users can just
             ;; roll back and forth.  Thus, disable it for simplicity.
             "--disable-version-links"

             ;; Ganeti can optionally take control over SSH host keys and
             ;; distribute them to nodes as they are added, and also rotate keys
             ;; with 'gnt-cluster renew-crypto --new-ssh-keys'.  Thus it needs to
             ;; know how to restart the SSH daemon.
             "--with-sshd-restart-command='herd restart ssh-daemon'"

             ;; Look for OS definitions in this directory by default.  It can
             ;; be changed in the cluster configuration.
             "--with-os-search-path=/run/current-system/profile/share/ganeti/os"

             ;; The default QEMU executable to use.  We don't use the package
             ;; here because this entry is stored in the cluster configuration.
             (string-append "--with-kvm-path=/run/current-system/profile/bin/"
                            ,(system->qemu-target (%current-system))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-version-constraints
           (lambda _
             ;; Loosen version constraints for compatibility with Stackage 18.10.
             (substitute* "cabal/ganeti.template.cabal"
               (("(.*base64-bytestring.*) < 1\\.1" _ match)
                (string-append match " < 1.2"))
               (("(.*QuickCheck.*) < 2\\.14" _ match)
                (string-append match " < 2.15")))))
         (add-after 'unpack 'pyparsing-compat
           (lambda _
             ;; Adjust for Pyparsing 3.0.  Remove for Ganeti 3.0.2+.
             (substitute* "lib/qlang.py"
               (("operatorPrecedence")
                "infixNotation"))))
         (add-after 'unpack 'create-vcs-version
           (lambda _
             ;; If we are building from a git checkout, we need to create a
             ;; 'vcs-version' file manually because the build system does
             ;; not have access to the git repository information.
             (unless (file-exists? "vcs-version")
               (call-with-output-file "vcs-version"
                 (lambda (port)
                   (format port "v~a~%" ,version))))))
         (add-after 'unpack 'patch-absolute-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("lib/utils/process.py"
                            "lib/utils/text.py"
                            "src/Ganeti/Constants.hs"
                            "src/Ganeti/HTools/CLI.hs"
                            "test/py/ganeti.config_unittest.py"
                            "test/py/ganeti.hooks_unittest.py"
                            "test/py/ganeti.utils.process_unittest.py"
                            "test/py/ganeti.utils.text_unittest.py"
                            "test/py/ganeti.utils.wrapper_unittest.py")
               (("/bin/sh") (search-input-file inputs "/bin/sh"))
               (("/bin/bash") (search-input-file inputs "/bin/bash"))
               (("/usr/bin/env") (search-input-file inputs "/bin/env"))
               (("/bin/true") (search-input-file inputs "/bin/true")))

             ;; This script is called by the node daemon at startup to perform
             ;; sanity checks on the cluster IP addresses, and it is also used
             ;; in a master-failover scenario.  Add absolute references to
             ;; avoid propagating these executables.
             (substitute* "tools/master-ip-setup"
               (("arping") (search-input-file inputs "/bin/arping"))
               (("ndisc6") (search-input-file inputs "/bin/ndisc6"))
               (("fping") (search-input-file inputs "/sbin/fping"))
               (("grep") (search-input-file inputs "/bin/grep"))
               (("ip addr") (string-append (search-input-file inputs "/sbin/ip")
                                           " addr")))))
         (add-after 'unpack 'override-builtin-PATH
           (lambda _
             ;; Ganeti runs OS install scripts and similar with a built-in
             ;; hard coded PATH.  Patch so it works on Guix System.
             (substitute* "src/Ganeti/Constants.hs"
               (("/sbin:/bin:/usr/sbin:/usr/bin")
                "/run/setuid-programs:/run/current-system/profile/sbin:\
/run/current-system/profile/bin"))))
         (add-after 'bootstrap 'patch-sphinx-version-detection
           (lambda _
             ;; The build system runs 'sphinx-build --version' to verify that
             ;; the Sphinx is recent enough, but does not expect the
             ;; .sphinx-build-real executable name created by the Sphinx wrapper.
             (substitute* "configure"
               (("\\$SPHINX --version 2>&1")
                "$SPHINX --version 2>&1 \
| sed 's/.sphinx-build-real/sphinx-build/g'"))))

         ;; The build system invokes Cabal and GHC, which do not work with
         ;; GHC_PACKAGE_PATH: <https://github.com/haskell/cabal/issues/3728>.
         ;; Tweak the build system to do roughly what haskell-build-system does.
         (add-before 'configure 'configure-haskell
           (assoc-ref haskell:%standard-phases 'setup-compiler))
         (add-after 'configure 'do-not-use-GHC_PACKAGE_PATH
           (lambda _
             (unsetenv "GHC_PACKAGE_PATH")
             (substitute* "Makefile"
               (("\\$\\(CABAL\\)")
                "$(CABAL) --package-db=../package.conf.d")
               (("\\$\\(GHC\\)")
                "$(GHC) -package-db=../package.conf.d"))))
         (add-after 'configure 'make-ghc-use-shared-libraries
           (lambda _
             (substitute* "Makefile"
               (("HFLAGS =") "HFLAGS = -dynamic -fPIC"))))
         (add-after 'configure 'fix-installation-directories
           (lambda _
             (substitute* "Makefile"
               ;; Do not attempt to create /var during install.
               (("\\$\\(DESTDIR\\)\\$\\{localstatedir\\}")
                "$(DESTDIR)${prefix}${localstatedir}")
               ;; Similarly, do not attempt to install the sample ifup scripts
               ;; to /etc/ganeti.
               (("\\$\\(DESTDIR\\)\\$\\(ifupdir\\)")
                "$(DESTDIR)${prefix}$(ifupdir)"))))
         (add-before 'build 'adjust-tests
           (lambda _
             ;; Disable tests that can not run.  Do it early to prevent
             ;; touching the Makefile later and triggering a needless rebuild.
             (substitute* "Makefile"
               ;; These tests expect the presence of a 'root' user (via
               ;; ganeti/runtime.py), which fails in the build environment.
               (("test/py/ganeti\\.asyncnotifier_unittest\\.py") "")
               (("test/py/ganeti\\.backend_unittest\\.py") "")
               (("test/py/ganeti\\.daemon_unittest\\.py") "")
               (("test/py/ganeti\\.tools\\.ensure_dirs_unittest\\.py") "")
               (("test/py/ganeti\\.utils\\.io_unittest-runasroot\\.py") "")
               ;; Disable the bash_completion test, as it requires the full
               ;; bash instead of bash-minimal.
               (("test/py/bash_completion\\.bash")
                "")
               ;; This test requires networking.
               (("test/py/import-export_unittest\\.bash")
                ""))))
         (add-after 'build 'build-bash-completions
           (lambda _
             (setenv "PYTHONPATH" ".")
             (invoke "./autotools/build-bash-completion")
             (unsetenv "PYTHONPATH")))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Set TZDIR so that time zones are found.
             (setenv "TZDIR" (search-input-directory inputs "share/zoneinfo"))

             (substitute* "test/py/ganeti.utils.process_unittest.py"
               ;; This test attempts to run an executable with
               ;; RunCmd(..., reset_env=True), which fails because the default
               ;; PATH from Constants.hs does not exist in the build container.
               ((".*def testResetEnv.*" all)
                (string-append "  @unittest.skipIf(True, "
                               "\"cannot reset env in the build container\")\n"
                               all))

               ;; XXX: Somehow this test fails in the build container, but
               ;; works in 'guix environment -C', even without /bin/sh?
               ((".*def testPidFile.*" all)
                (string-append "  @unittest.skipIf(True, "
                               "\"testPidFile fails in the build container\")\n"
                               all)))

             ;; XXX: Why are these links not added automatically.
             (with-directory-excursion "test/hs"
               (for-each (lambda (file)
                           (symlink "../../src/htools" file))
                         '("hspace" "hscan" "hinfo" "hbal" "hroller"
                           "hcheck" "hail" "hsqueeze")))))
         (add-after 'install 'install-bash-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (compdir (string-append out "/etc/bash_completion.d")))
               (mkdir-p compdir)
               (copy-file "doc/examples/bash_completion"
                          (string-append compdir "/ganeti"))
               ;; The one file contains completions for many different
               ;; executables.  Create symlinks for found completions.
               (with-directory-excursion compdir
                 (for-each
                  (lambda (prog) (symlink "ganeti" prog))
                  (call-with-input-file "ganeti"
                    (lambda (port)
                      (let loop ((line (read-line port))
                                 (progs '()))
                        (if (eof-object? line)
                            progs
                            (if (string-prefix? "complete" line)
                                (loop (read-line port)
                                      ;; Extract "prog" from lines of the form:
                                      ;; "complete -F _prog -o filenames prog".
                                      ;; Note that 'burnin' is listed with the
                                      ;; absolute file name, which is why we
                                      ;; run everything through 'basename'.
                                      (match (string-split line #\ )
                                        ((commands ... prog)
                                         (cons (basename prog) progs))))
                                (loop (read-line port) progs)))))))))))
         ;; Wrap all executables with GUIX_PYTHONPATH.  We can't borrow
         ;; the phase from python-build-system because we also need to wrap
         ;; the scripts in $out/lib/ganeti such as "node-daemon-setup".
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin"))
                    (lib (string-append out "/lib"))
                    (PYTHONPATH (string-append (site-packages inputs outputs)
                                               ":" (getenv "GUIX_PYTHONPATH"))))
               (define (shell-script? file)
                 (call-with-ascii-input-file file
                   (lambda (port)
                     (let ((shebang (false-if-exception (read-line port))))
                       (and shebang
                            (string-prefix? "#!" shebang)
                            (or (string-contains shebang "/bin/bash")
                                (string-contains shebang "/bin/sh")))))))

               (define* (wrap? file #:rest _)
                 ;; Do not wrap shell scripts because some are meant to be
                 ;; sourced, which breaks if they are wrapped.  We do wrap
                 ;; the Haskell executables because some call out to Python
                 ;; directly.
                 (and (executable-file? file)
                      (not (symbolic-link? file))
                      (not (shell-script? file))))

               (for-each (lambda (file)
                           (wrap-program file
                             `("GUIX_PYTHONPATH" ":" prefix
                               (,PYTHONPATH))))
                         (append-map (cut find-files <> wrap?)
                                     (list (string-append lib "/ganeti")
                                           sbin)))))))))
    (native-inputs
     `(("haskell" ,ghc)
       ("cabal" ,cabal-install)
       ("m4" ,m4)

       ;; These inputs are necessary to bootstrap the package, because we
       ;; have patched the build system.
       ("autoconf" ,autoconf)
       ("automake" ,automake)

       ;; For the documentation.
       ("python-docutils" ,python-docutils)
       ("sphinx" ,python-sphinx)
       ("pandoc" ,pandoc)
       ("dot" ,graphviz)

       ;; Test dependencies.
       ("fakeroot" ,fakeroot)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("python-mock" ,python-mock)
       ("python-pyyaml" ,python-pyyaml)
       ("openssh" ,openssh)
       ("procps" ,procps)
       ("shelltestrunner" ,shelltestrunner)
       ("tzdata" ,tzdata-for-tests)))
    (inputs
     (list iputils                      ;for 'arping'
           curl
           fping
           iproute
           ndisc6
           socat
           qemu-minimal                 ;for qemu-img
           ghc-attoparsec
           ghc-base64-bytestring
           ghc-cryptonite
           ghc-curl
           ghc-hinotify
           ghc-hslogger
           ghc-json
           ghc-lens
           ghc-lifted-base
           ghc-network
           ghc-old-time
           ghc-psqueue
           ghc-regex-pcre
           ghc-utf8-string
           ghc-zlib
           ;; For the optional metadata daemon.
           ghc-snap-core
           ghc-snap-server
           python
           python-pyopenssl
           python-simplejson
           python-pyparsing
           python-pyinotify
           python-pycurl
           python-bitarray
           python-paramiko
           python-psutil))
    (home-page "https://www.ganeti.org/")
    (synopsis "Cluster-based virtual machine management system")
    (description
     "Ganeti is a virtual machine management tool built on top of existing
virtualization technologies such as Xen or KVM.  Ganeti controls:

@itemize @bullet
@item Disk creation management;
@item Operating system installation for instances (in co-operation with
OS-specific install scripts); and
@item Startup, shutdown, and failover between physical systems.
@end itemize

Ganeti is designed to facilitate cluster management of virtual servers and
to provide fast and simple recovery after physical failures, using
commodity hardware.")
    (license license:bsd-2)))

(define-public ganeti-instance-guix
  (package
    (name "ganeti-instance-guix")
    (version "0.6")
    (home-page "https://github.com/mbakke/ganeti-instance-guix")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0aa08irpcpns6mhjgsplc5f0p8ab1qcr9ah1gj5z66kxgqyflzrp"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--sysconfdir=/etc" "--localstatedir=/var")))
    (native-inputs
     (list autoconf automake))
    (inputs
     `(("util-linux" ,util-linux)
       ("qemu-img" ,qemu-minimal)))
    (synopsis "Guix OS integration for Ganeti")
    (description
     "This package provides a guest OS definition for Ganeti that uses
Guix to build virtual machines.")
    (license license:gpl3+)))

(define-public ganeti-instance-debootstrap
  (package
    (name "ganeti-instance-debootstrap")
    ;; We need two commits on top of the latest release for compatibility
    ;; with newer sfdisk, as well as gnt-network integration.
    (version "0.16-2-ge145396")
    (home-page "https://github.com/ganeti/instance-debootstrap")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f2isw9d8lawzj21rrq1q9xhq8xfa65rqbhqmrn59z201x9q1336"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--sysconfdir=/etc" "--localstatedir=/var")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'add-absolute-references
                    (lambda _
                      (substitute* "common.sh.in"
                        (("/sbin/blkid") (which "blkid"))
                        (("kpartx -")
                         (string-append (which "kpartx") " -")))
                      (substitute* "import"
                        (("restore -r")
                         (string-append (which "restore") " -r")))
                      (substitute* "export"
                        (("dump -0")
                         (string-append (which "dump") " -0")))
                      (substitute* "create"
                        (("debootstrap") (which "debootstrap"))
                        (("`which run-parts`") (which "run-parts"))
                        ;; Here we actually need to hard code /bin/passwd
                        ;; because it's called via chroot, which fails if
                        ;; "/bin" is not in PATH.
                        (("passwd") "/bin/passwd"))
                      #t))
                  (add-after 'unpack 'set-dpkg-arch
                    (lambda* (#:key system #:allow-other-keys)
                      ;; The create script passes --arch to debootstrap,
                      ;; and defaults to `dpkg --print-architecture` when
                      ;; ARCH is not set in variant.conf.  Hard code the
                      ;; build-time architecture to avoid the dpkg dependency.
                      (let ((dpkg-arch
                             (cond ((string-prefix? "x86_64" system)
                                    "amd64")
                                   ((string-prefix? "i686" system)
                                    "i386")
                                   ((string-prefix? "aarch64" system)
                                    "arm64")
                                   (else (car (string-split system #\-))))))
                        (substitute* "create"
                          (("`dpkg --print-architecture`")
                           dpkg-arch))
                        #t)))
                  (add-after 'configure 'adjust-Makefile
                    (lambda _
                      ;; Do not attempt to create /etc/ganeti/instance-debootstrap
                      ;; and /etc/default/ganeti-instance-debootstrap during install.
                      ;; They are created by the Ganeti service.
                      (substitute* "Makefile"
                        (("\\$\\(variantsdir\\)")
                         "$(prefix)/etc/ganeti/instance-debootstrap/variants")
                        (("\\$\\(defaultsdir\\)")
                         "$(prefix)/etc/default/ganeti-instance-debootstrap"))
                      #t))
                  (add-after 'install 'make-variants.list-symlink
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; The Ganeti OS API mandates a variants.list file that
                      ;; describes all supported "variants" of this OS.
                      ;; Guix generates this file, so make the original file
                      ;; a symlink to it.
                      (with-directory-excursion (string-append
                                                 (assoc-ref outputs "out")
                                                 "/share/ganeti/os/debootstrap")
                        (delete-file "variants.list")
                        (symlink "/etc/ganeti/instance-debootstrap/variants/variants.list"
                                 "variants.list"))
                      #t)))))
    (native-inputs
     (list autoconf automake))
    (inputs
     `(("debianutils" ,debianutils)
       ("debootstrap" ,debootstrap)
       ("dump" ,dump)
       ("kpartx" ,multipath-tools)
       ("util-linux" ,util-linux)))
    (synopsis "Debian OS integration for Ganeti")
    (description
     "This package provides a guest OS definition for Ganeti.  It installs
Debian or a derivative using @command{debootstrap}.")
    (license license:gpl2+)))

(define-public libosinfo
  (package
    (name "libosinfo")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.pagure.org/libosinfo/libosinfo-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0nd360c9ampw8hb6xh5g45q858df2r4jj9q88bcl6gzgaj0l3wxl"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-Dwith-usb-ids-path="
                            (assoc-ref %build-inputs "usb.ids"))
             (string-append "-Dwith-pci-ids-path="
                            (assoc-ref %build-inputs "pci.ids")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-osinfo-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "osinfo/osinfo_loader.c"
               (("path = DATA_DIR.*")
                (string-append "path = \"" (assoc-ref inputs "osinfo-db")
                               "/share/osinfo\";"))))))))
    (inputs
     `(("libsoup" ,libsoup-minimal-2)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("osinfo-db" ,osinfo-db)))
    (native-inputs
     `(("glib" ,glib "bin")  ; glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)
       ("vala" ,vala)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("pci.ids"
        ,(origin
           (method url-fetch)
           (uri "https://github.com/pciutils/pciids/raw/ad02084f0bc143e3c15e31a6152a3dfb1d7a3156/pci.ids")
           (sha256
            (base32
             "0kfhpj5rnh24hz2714qhfmxk281vwc2w50sm73ggw5d15af7zfsw"))))
       ("usb.ids"
        ,(origin
           (method url-fetch)
           (uri "https://svn.code.sf.net/p/linux-usb/repo/trunk/htdocs/usb.ids?r=2681")
           (file-name "usb.ids")
           (sha256
            (base32
             "1m6yhvz5k8aqzxgk7xj3jkk8frl1hbv0h3vgj4wbnvnx79qnvz3r"))))))
    (home-page "https://libosinfo.org/")
    (synopsis "Operating system information database")
    (description "libosinfo is a GObject based library API for managing
information about operating systems, hypervisors and the (virtual) hardware
devices they can support.  It includes a database containing device metadata
and provides APIs to match/identify optimal devices for deploying an operating
system on a hypervisor.  Via GObject Introspection, the API is available in
all common programming languages.  Vala bindings are also provided.")
    ;; The library files are released under LGPLv2.1 or later; the source
    ;; files in the "tools" directory are released under GPLv2+.
    (license (list license:lgpl2.1+ license:gpl2+))))

(define-public lxc
  (package
    (name "lxc")
    (version "4.0.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://linuxcontainers.org/downloads/lxc/lxc-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0b7hv4n8b3lndhr0jf9j1gkbzxm8897a1myjsfgwzad9gkhq395g"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config docbook2x))
    (inputs
     (list gnutls libcap libseccomp libselinux))
    (arguments
     (list #:configure-flags
           #~(list (string-append "--docdir=" #$output "/share/doc/"
                                  #$name "-" #$version)
                   "--sysconfdir=/etc"
                   "--localstatedir=/var")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda _
                   (invoke "make" "install"
                           (string-append "bashcompdir=" #$output
                                          "/etc/bash_completion.d")
                           ;; Don't install files into /var and /etc.
                           "LXCPATH=/tmp/var/lib/lxc"
                           "localstatedir=/tmp/var"
                           "sysconfdir=/tmp/etc"
                           "sysconfigdir=/tmp/etc/default"))))))
    (synopsis "Linux container tools")
    (home-page "https://linuxcontainers.org/")
    (description
     "LXC is a userspace interface for the Linux kernel containment features.
Through a powerful API and simple tools, it lets Linux users easily create and
manage system or application containers.")
    (license license:lgpl2.1+)))

(define-public lxcfs
  (package
    (name "lxcfs")
    (version "4.0.11")
    (home-page "https://github.com/lxc/lxcfs")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "lxcfs-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02cgzh97cgxh9iyf7gkn5ikdc0sfzqfjj6al0hikdf9rbwcscqwd"))))
    (arguments
     '(#:configure-flags '("--localstatedir=/var")))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list fuse))
    (build-system gnu-build-system)
    (synopsis "FUSE-based file system for LXC")
    (description "LXCFS is a small FUSE file system written with the intention
of making Linux containers feel more like a virtual machine.
It started as a side project of LXC but can be used by any run-time.")
    (license license:lgpl2.1+)))

(define-public lxd
  (package
    (name "lxd")
    (version "4.22")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/lxc/lxd/releases/download/"
                    "lxd-" version "/lxd-" version ".tar.gz"))
              (sha256
               (base32
                "119345936fcm1vv06k82k9hvj5yjf9jdrwqm9ccphhl5mswf8xq9"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/lxc/lxd"
       #:tests? #f ;; tests fail due to missing /var, cgroups, etc.
       #:modules ((guix build go-build-system)
                  (guix build union)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-dist
           (lambda* (#:key import-path #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               ;; Move all the dependencies into the src directory.
               (copy-recursively "_dist/src" "../../.."))))
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               (invoke "make" "build" "CC=gcc" "TAG_SQLITE3=libsqlite3"))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (when tests?
               (with-directory-excursion (string-append "src/" import-path)
                 (invoke "make" "check" "CC=gcc" "TAG_SQLITE3=libsqlite3")))))
         (replace 'install
           (lambda* (#:key inputs outputs import-path #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin-dir
                     (string-append out "/bin/"))
                    (doc-dir
                     (string-append out "/share/doc/lxd-" ,version))
                    (completions-dir
                     (string-append out "/share/bash-completion/completions")))
               (with-directory-excursion (string-append "src/" import-path)
                 ;; Wrap lxd with run-time dependencies.
                 (wrap-program (string-append bin-dir "lxd")
                   `("PATH" ":" prefix
                     ,(fold (lambda (input paths)
                              (let* ((in (assoc-ref inputs input))
                                     (bin (string-append in "/bin"))
                                     (sbin (string-append in "/sbin")))
                                (append (filter file-exists?
                                                (list bin sbin)) paths)))
                            '()
                            '("bash" "acl" "rsync" "tar" "xz" "btrfs-progs"
                              "gzip" "dnsmasq" "squashfs-tools" "iproute2"
                              "criu" "iptables"))))
                 ;; Remove unwanted binaries.
                 (for-each (lambda (prog)
                             (delete-file (string-append bin-dir prog)))
                           '("deps" "macaroon-identity" "generate"))
                 ;; Install documentation.
                 (for-each (lambda (file)
                             (install-file file doc-dir))
                           (find-files "doc"))
                 ;; Install bash completion.
                 (rename-file "scripts/bash/lxd-client" "scripts/bash/lxd")
                 (install-file "scripts/bash/lxd" completions-dir))))))))
    (native-inputs
     (list ;; Test dependencies:
           ;; ("go-github-com-rogpeppe-godeps" ,go-github-com-rogpeppe-godeps)
           ;; ("go-github-com-tsenart-deadcode" ,go-github-com-tsenart-deadcode)
           ;; ("go-golang-org-x-lint" ,go-golang-org-x-lint)
           pkg-config))
    (inputs
     `(("acl" ,acl)
       ("eudev" ,eudev)
       ("libdqlite" ,libdqlite)
       ("libraft" ,libraft)
       ("libcap" ,libcap)
       ("lxc" ,lxc)
       ;; Run-time dependencies.
       ("bash" ,bash-minimal)
       ("rsync" ,rsync)
       ("tar" ,tar)
       ("xz" ,xz)
       ("btrfs-progs" ,btrfs-progs)
       ("gzip" ,gzip)
       ("dnsmasq" ,dnsmasq)
       ("squashfs-tools" ,squashfs-tools)
       ("iproute2" ,iproute)
       ("criu" ,criu)
       ("iptables" ,iptables)))
    (synopsis "Daemon based on liblxc offering a REST API to manage containers")
    (home-page "https://linuxcontainers.org/lxd/")
    (description "LXD is a next generation system container manager.  It
offers a user experience similar to virtual machines but using Linux
containers instead.  It's image based with pre-made images available for a
wide number of Linux distributions and is built around a very powerful, yet
pretty simple, REST API.")
    (license license:asl2.0)))

(define-public libvirt
  (package
    (name "libvirt")
    (version "7.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://libvirt.org/sources/libvirt-"
                           version ".tar.xz"))
       (sha256
        (base32 "131fyxb05rrcr9ih4mhhjyw3cgsxh5l12vj4y109q9vlynsz5742"))
       (patches (search-patches "libvirt-add-install-prefix.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list "-Ddriver_qemu=enabled"
             "-Dqemu_user=nobody"
             "-Dqemu_group=kvm"
             "-Dstorage_disk=enabled"
             "-Dstorage_dir=enabled"
             "-Dpolkit=enabled"
             ;; XXX The default, but required to make -Dsasl ‘stick’.
             ;; See <https://gitlab.com/libvirt/libvirt/-/issues/185>
             "-Ddriver_remote=enabled"
             "-Dnls=enabled"            ;translations
             (string-append "-Ddocdir=" (assoc-ref %outputs "out") "/share/doc/"
                            ,name "-" ,version)
             "-Dbash_completion=enabled"
             (string-append "-Dinstall_prefix=" (assoc-ref %outputs "out"))
             "--sysconfdir=/etc"
             "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-directory-confusion
           (lambda _
             ;; Don't try to install an (unused) /var outside of the store.
             (substitute* "scripts/meson-install-dirs.py"
               (("destdir = .*")
                "destdir = '/tmp'"))))
         (add-before 'configure 'disable-broken-tests
           (lambda _
             (let ((tests (list "commandtest"           ; hangs idly
                                "qemuxml2argvtest"      ; fails
                                "virnetsockettest")))   ; tries to network
               (substitute* "tests/meson.build"
                 (((format #f ".*'name': '(~a)'.*" (string-join tests "|")))
                  ""))))))))
    (inputs
     `(("acl" ,acl)
       ("attr" ,attr)
       ("fuse" ,fuse)
       ("libxml2" ,libxml2)
       ("eudev" ,eudev)
       ("libpciaccess" ,libpciaccess)
       ("gnutls" ,gnutls)
       ("dbus" ,dbus)
       ("libpcap" ,libpcap)
       ("libnl" ,libnl)
       ("libssh2" ,libssh2)             ;optional
       ("libtirpc" ,libtirpc)           ;for <rpc/rpc.h>
       ("libuuid" ,util-linux "lib")
       ("lvm2" ,lvm2)                   ;for libdevmapper
       ("curl" ,curl)
       ("openssl" ,openssl)
       ("readline" ,readline)
       ("cyrus-sasl" ,cyrus-sasl)
       ("libyajl" ,libyajl)
       ("audit" ,audit)
       ("dmidecode" ,dmidecode)
       ("dnsmasq" ,dnsmasq)
       ("ebtables" ,ebtables)
       ("parted" ,parted)
       ("iproute" ,iproute)
       ("iptables" ,iptables)))
    (native-inputs
     `(("bash-completion" ,bash-completion)
       ("gettext" ,gettext-minimal)
       ("xsltproc" ,libxslt)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("polkit" ,polkit)
       ("python" ,python-wrapper)
       ("python-docutils" ,python-docutils) ;for rst2html
       ("rpcsvc-proto" ,rpcsvc-proto)))     ;for rpcgen
    (home-page "https://libvirt.org")
    (synopsis "Simple API for virtualization")
    (description "Libvirt is a C toolkit to interact with the virtualization
capabilities of recent versions of Linux.  The library aims at providing long
term stable C API initially for the Xen paravirtualization but should be able
to integrate other virtualization mechanisms if needed.")
    (license license:lgpl2.1+)))

(define-public libvirt-glib
  (package
    (name "libvirt-glib")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://libvirt.org/libvirt/glib/"
                                  "libvirt-glib-" version ".tar.xz"))
              (sha256
               (base32
                "1gdcvqz88qkp402zra9csc6391f2xki1270x683n6ixakl3gf8w4"))))
    (build-system meson-build-system)
    (inputs
     (list openssl cyrus-sasl lvm2 ; for libdevmapper
           libyajl))
    (native-inputs
     (list pkg-config intltool
           `(,glib "bin") vala))
    (propagated-inputs
     ;; ‘Required:’ by the installed .pc files.
     (list glib libvirt libxml2 gobject-introspection))
    (home-page "https://libvirt.org")
    (synopsis "GLib wrapper around libvirt")
    (description "libvirt-glib wraps the libvirt library to provide a
high-level object-oriented API better suited for glib-based applications, via
three libraries:

@enumerate
@item libvirt-glib - GLib main loop integration & misc helper APIs
@item libvirt-gconfig - GObjects for manipulating libvirt XML documents
@item libvirt-gobject - GObjects for managing libvirt objects
@end enumerate
")
    (license license:lgpl2.1+)))

(define-public python-libvirt
  (package
    (name "python-libvirt")
    (version "7.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://libvirt.org/sources/python/libvirt-python-"
                           version ".tar.gz"))
       (sha256
        (base32 "0nakisj2ady5a41k4zc95k0kp749f4ppmxgr91b1h1dzbzxcydc5"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               ;; No reason to explicity invoke Python on a wrapped pytest.
               (substitute* "setup.py"
                 (("sys\\.executable, pytest") "pytest"))
               (add-installed-pythonpath inputs outputs)
               (setenv "LIBVIRT_API_COVERAGE" "whynot")
               (invoke "python" "setup.py" "test")))))))
    (inputs
     (list libvirt))
    (propagated-inputs
     (list python-lxml))
    (native-inputs
     (list pkg-config python-pytest))
    (home-page "https://libvirt.org")
    (synopsis "Python bindings to libvirt")
    (description "This package provides Python bindings to the libvirt
virtualization library.")
    (license license:lgpl2.1+)))

(define-public virt-manager
  (package
    (name "virt-manager")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://virt-manager.org/download/sources"
                                  "/virt-manager/virt-manager-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "11kvpzcmyir91qz0dsnk7748jbb4wr8mrc744w117qc91pcy6vrb"))))
    (build-system python-build-system)
    (arguments
     `(#:use-setuptools? #f          ; uses custom distutils 'install' command
       #:tests? #f                      ; TODO The tests currently fail
                                        ; RuntimeError: Loop condition wasn't
                                        ; met
       #:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
       #:modules ((ice-9 match)
                  (srfi srfi-26)
                  (guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-setup
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "virtinst/buildconfig.py"
               (("/usr") (assoc-ref outputs "out")))
             #t))
         (add-after 'unpack 'fix-default-uri
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Xen is not available for now - so only patch qemu.
             (substitute* "virtManager/createconn.py"
               (("/usr(/bin/qemu-system-[a-zA-Z0-9_-]+)" _ suffix)
                (search-input-file inputs suffix)))
             #t))
         (add-before 'wrap 'wrap-with-GI_TYPELIB_PATH
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin       (string-append (assoc-ref outputs "out") "/bin"))
                    (bin-files (find-files bin ".*"))
                    (paths     (map (match-lambda
                                      ((output . directory)
                                       (let* ((girepodir (string-append
                                                          directory
                                                          "/lib/girepository-1.0")))
                                         (if (file-exists? girepodir)
                                             girepodir #f))))
                                    inputs)))
               (for-each (lambda (file)
                           (format #t "wrapping ~a\n" file)
                           (wrap-program file
                             `("GI_TYPELIB_PATH" ":" prefix
                               ,(filter identity paths))))
                         bin-files))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" "/tmp")
               (setenv "XDG_CACHE_HOME" "/tmp")
               (system "Xvfb :1 &")
               (setenv "DISPLAY" ":1")
               ;; Dogtail requires that Assistive Technology support be enabled
               (setenv "GTK_MODULES" "gail:atk-bridge")
               (invoke "dbus-run-session" "--" "pytest" "--uitests"))
             #t))
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'wrap 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (inputs
     (list dconf
           gtk+
           gtk-vnc
           gtksourceview
           libvirt
           libvirt-glib
           libosinfo
           vte
           python-libvirt
           python-requests
           python-pycairo
           python-pygobject
           python-libxml2
           spice-gtk))
    ;; virt-manager searches for qemu-img or kvm-img in the PATH.
    (propagated-inputs
     (list qemu))
    (native-inputs
     `(("glib" ,glib "bin")             ; glib-compile-schemas
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+ "bin")             ; gtk-update-icon-cache
       ("perl" ,perl)                   ; pod2man
       ("intltool" ,intltool)
       ("rst2man" ,python-docutils)
       ;; The following are required for running the tests
       ;; ("python-pytest" ,python-pytest)
       ;; ("python-dogtail" ,python-dogtail)
       ;; ("xvfb" ,xorg-server-for-tests)
       ;; ("dbus" ,dbus)
       ;; ("at-spi2-core" ,at-spi2-core)
       ;; ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ))
    (home-page "https://virt-manager.org/")
    (synopsis "Manage virtual machines")
    (description
     "The virt-manager application is a desktop user interface for managing
virtual machines through libvirt.  It primarily targets KVM VMs, but also
manages Xen and LXC (Linux containers).  It presents a summary view of running
domains, their live performance and resource utilization statistics.")
    (license license:gpl2+)))

(define-public criu
  (package
    (name "criu")
    (version "3.16.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/checkpoint-restore/criu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1riw15197fnrs254jl7wks9x8bdml76kf1vnqkkgyypr13dnq55g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:tests? #f ; tests require mounting as root
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "LIBDIR=" (assoc-ref %outputs "out")
                            "/lib")
             (string-append "ASCIIDOC="
                            (search-input-file %build-inputs
                                               "/bin/asciidoc"))
             (string-append "PYTHON=python3")
             (string-append "XMLTO="
                            (search-input-file %build-inputs
                                               "/bin/xmlto")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'unpack 'fix-documentation
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "Documentation/Makefile"
               (("-m custom.xsl")
                (string-append
                 "-m custom.xsl --skip-validation -x "
                 (assoc-ref inputs "docbook-xsl") "/xml/xsl/"
                 ,(package-name docbook-xsl) "-"
                 ,(package-version docbook-xsl)
                 "/manpages/docbook.xsl")))))
         (add-after 'unpack 'hardcode-variables
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Hardcode arm version detection
             (substitute* "Makefile"
               (("ARMV.*:=.*") "ARMV := 7\n"))))
         (add-before 'build 'fix-symlink
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The file 'images/google/protobuf/descriptor.proto' points to
             ;; /usr/include/..., which obviously does not exist.
             (let* ((file "google/protobuf/descriptor.proto")
                    (target (string-append "images/" file))
                    (source (search-input-file
                             inputs
                             (string-append "include/" file))))
               (delete-file target)
               (symlink source target))))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'crit' runs with the correct PYTHONPATH.
             (let* ((out  (assoc-ref outputs "out"))
                    (site (string-append out "/lib/python"
                                         ,(version-major+minor
                                           (package-version python))
                                         "/site-packages"))
                    (path (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/crit")
                 `("GUIX_PYTHONPATH" ":" prefix (,site ,path))))))
         (add-after 'install 'delete-static-libraries
           ;; Not building/installing these at all doesn't seem to be supported.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each delete-file (find-files out "\\.a$"))))))))
    (inputs
     `(("protobuf" ,protobuf)
       ("python" ,python)
       ("python-protobuf" ,python-protobuf)
       ("iproute" ,iproute)
       ("libaio" ,libaio)
       ("libcap" ,libcap)
       ("libnet" ,libnet)
       ("libnl" ,libnl)
       ("libbsd" ,libbsd)
       ("nftables" ,nftables)))
    (native-inputs
     (list pkg-config
           perl
           protobuf-c
           asciidoc
           xmlto
           docbook-xml
           docbook-xsl))
    (home-page "https://criu.org")
    (synopsis "Checkpoint and restore in user space")
    (description "Using this tool, you can freeze a running application (or
part of it) and checkpoint it to a hard drive as a collection of files.  You
can then use the files to restore and run the application from the point it
was frozen at.  The distinctive feature of the CRIU project is that it is
mainly implemented in user space.")
    ;; The project is licensed under GPLv2; files in the lib/ directory are
    ;; LGPLv2.1.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public qmpbackup
  (package
    (name "qmpbackup")
    (version "0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/abbbi/qmpbackup")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0swhp5byz44brhyis1a39p11fyn9q84xz5q6v2fah29r7d71kmmx"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (home-page "https://github.com/abbbi/qmpbackup")
    (synopsis "Backup and restore QEMU machines")
    (description "qmpbackup is designed to create and restore full and
incremental backups of running QEMU virtual machines via QMP, the QEMU
Machine Protocol.")
    (license license:gpl3+)))

(define-public looking-glass-client
  (package
    (name "looking-glass-client")
    (version "B5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gnif/LookingGlass")
             (commit version)
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09mn544x5hg1z31l92ksk7fi7yj9r8xdk0dcl9fk56ivcr452ylm"))))
    (build-system cmake-build-system)
    (inputs
     (list bash-minimal
           fontconfig
           freetype
           glu
           gmp
           libglvnd
           libiberty
           libx11
           libxcursor
           libxfixes
           libxi
           libxinerama
           libxkbcommon
           libxpresent
           libxrandr
           libxscrnsaver
           mesa
           openssl
           sdl2
           sdl2-ttf
           spice-protocol
           wayland
           wayland-protocols
           `(,zlib "static")))
    (native-inputs (list libconfig nettle pkg-config))
    (arguments
     `(#:tests? #f ;; No tests are available.
       #:make-flags '("CC=gcc")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'chdir-to-client
                    (lambda* (#:key outputs #:allow-other-keys)
                      (chdir "client")
                      #t))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (install-file "looking-glass-client"
                                    (string-append (assoc-ref outputs "out")
                                                   "/bin"))
                      #t))
                  (add-after 'install 'wrapper
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (wrap-program
                          (string-append (assoc-ref outputs "out")
                                         "/bin/looking-glass-client")
                        `("LD_LIBRARY_PATH" ":" prefix
                          ,(map (lambda (name)
                                  (let ((input (assoc-ref inputs name)))
                                    (string-append input "/lib")))
                                '("gmp"
                                  "libxi"
                                  "nettle"
                                  "mesa"
                                  "wayland"
                                  "fontconfig-minimal"
                                  "freetype"
                                  "libx11"
                                  "libxfixes"
                                  "libxscrnsaver"
                                  "libxinerama"))))
                      #t)))))
    (home-page "https://looking-glass.io/")
    (synopsis "KVM Frame Relay (KVMFR) implementation")
    (description "Looking Glass allows the use of a KVM (Kernel-based Virtual
Machine) configured for VGA PCI Pass-through without an attached physical
monitor, keyboard or mouse.  It displays the VM's rendered contents on your
main monitor/GPU.")
    ;; This package requires SSE instructions.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:gpl2+)))

(define-public runc
  (package
    (name "runc")
    (version "1.0.0-rc93")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/opencontainers/runc/releases/"
                    "download/v" version "/runc.tar.xz"))
              (file-name (string-append name "-" version ".tar.xz"))
              (sha256
               (base32
                "0b90r1bkvlqli53ca1yc1l488dba0isd3i6l7nlhszxi8p7hzvkh"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/opencontainers/runc"
       #:install-source? #f
       ;; XXX: 20/139 tests fail due to missing /var, cgroups and apparmor in
       ;; the build environment.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               (invoke "make" "all" "man"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "make" "localunittest"))))
         (replace 'install
           (lambda* (#:key import-path outputs #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               (let ((out (assoc-ref outputs "out")))
                 (invoke "make" "install" "install-bash" "install-man"
                         (string-append "PREFIX=" out)))))))))
    (native-inputs
     `(("go-md2man" ,go-github-com-go-md2man)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list libseccomp))
    (synopsis "Open container initiative runtime")
    (home-page "https://opencontainers.org/")
    (description
     "@command{runc} is a command line client for running applications
packaged according to the
@uref{https://github.com/opencontainers/runtime-spec/blob/master/spec.md, Open
Container Initiative (OCI) format} and is a compliant implementation of the
Open Container Initiative specification.")
    (license license:asl2.0)))

(define-public umoci
  (package
    (name "umoci")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/opencontainers/umoci/releases/download/v"
             version "/umoci.tar.xz"))
       (file-name (string-append "umoci-" version ".tar.xz"))
       (sha256
        (base32 "0fvljj9k4f83wbqzd8nbijz0p1zaq633f8yxyvl5sy3wjf03ffk9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/opencontainers/umoci"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source import-path #:allow-other-keys)
             ;; Unpack the tarball into 'umoci' instead of "runc-${version}".
             (let ((dest (string-append "src/" import-path)))
               (mkdir-p dest)
               (invoke "tar" "-C" (string-append "src/" import-path)
                       "--strip-components=1"
                       "-xvf" source))))
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               ;; TODO: build manpages with 'go-md2man'.
               (invoke "make" "SHELL=bash"))))
         (replace 'install
           (lambda* (#:key import-path outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bindir (string-append out "/bin")))
               (install-file (string-append "src/" import-path "/umoci")
                             bindir)
               #t))))))
    (home-page "https://umo.ci/")
    (synopsis "Tool for modifying Open Container images")
    (description
     "@command{umoci} is a tool that allows for high-level modification of an
Open Container Initiative (OCI) image layout and its tagged images.")
    (license license:asl2.0)))

(define-public skopeo
  (package
    (name "skopeo")
    (version "1.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/containers/skopeo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n22sdif437ddg5ch0ipwim3fg0n6ihc9bfi52qkhy3r1grz04hs"))))
    (build-system go-build-system)
    (native-inputs
     (list pkg-config go-github-com-go-md2man))
    (inputs
     (list btrfs-progs
           eudev
           libassuan
           libselinux
           libostree
           lvm2
           glib
           gpgme))
    (arguments
     '(#:import-path "github.com/containers/skopeo"
       #:install-source? #f
       #:tests? #f                                ; The tests require Docker
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               (invoke "make" "bin/skopeo"))))
         (add-after 'build 'build-docs
           (lambda* (#:key import-path #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               (invoke "make" "docs"))))
         (replace 'install
           (lambda* (#:key import-path outputs #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "default-policy.json"
                               (string-append out "/etc/containers"))
                 (invoke "make" "install-binary" "install-completions" "install-docs"
                         (string-append "PREFIX=" out)))))))))
    (home-page "https://github.com/containers/skopeo")
    (synopsis "Interact with container images and container image registries")
    (description
     "@command{skopeo} is a command line utility providing various operations
with container images and container image registries.  It can:
@enumerate

@item Copy container images between various containers image stores,
converting them as necessary.

@item Convert a Docker schema 2 or schema 1 container image to an OCI image.

@item Inspect a repository on a container registry without needlessly pulling
the image.

@item Sign and verify container images.

@item Delete container images from a remote container registry.

@end enumerate")
    (license license:asl2.0)))

(define-public python-vagrant
  (package
    (name "python-vagrant")
    (version "0.5.15")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-vagrant" version))
        (sha256
         (base32
          "1ikrh6canhcxg5y7pzmkcnnydikppv7s6sm9prfx90nk0ac8m6mg"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; tests involve running vagrant.
    (home-page "https://github.com/todddeluca/python-vagrant")
    (synopsis "Python bindings for Vagrant")
    (description
     "Python-vagrant is a Python module that provides a thin wrapper around the
@code{vagrant} command line executable, allowing programmatic control of Vagrant
virtual machines.")
    (license license:expat)))

(define-public bubblewrap
  (package
    (name "bubblewrap")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/containers/bubblewrap/"
                                  "releases/download/v" version "/bubblewrap-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0608l2sjwhnb1c0mslah1h6yjvqr17wk60by6i710qwxg4rszz8n"))
               (patches (search-patches "bubblewrap-fix-locale-in-tests.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Tests try to access /var/tmp, which is not possible in our build
             ;; environment.  Let's give them another directory.
             ;; /tmp gets overriden in some tests, so we need another directory.
             ;; the only possibility is the output directory.
             (let ((tmp-dir (string-append (assoc-ref outputs "out") "/tmp")))
               (mkdir-p tmp-dir)
               (substitute* "tests/test-run.sh"
                 (("/var/tmp") tmp-dir)
                 ;; Tests create a temporary python script, so fix its shebang.
                 (("/usr/bin/env python3") (which "python3"))
                 ;; Tests call /usr/bin/env, so fix its path.
                 (("/usr/bin/env") (which "env"))
                 ;; Some tests try to access /usr, but that doesn't exist.
                 ;; Give them /gnu instead.
                 (("/usr") "/gnu")
                 (("--ro-bind /bin /bin") "--ro-bind /gnu /bin")
                 (("--ro-bind /sbin /sbin") "--ro-bind /gnu /sbin")
                 (("--ro-bind /lib /lib") "--ro-bind /gnu /lib")
                 (("  */bin/bash") (which "bash"))
                 (("/bin/sh") (which "sh"))
                 (("findmnt") (which "findmnt")))
               (substitute* "tests/libtest.sh"
                 (("/var/tmp") tmp-dir)
                 (("/usr") "/gnu")
                 (("--ro-bind /bin /bin") "--ro-bind /gnu /bin")
                 (("--ro-bind /sbin /sbin") "--ro-bind /gnu /sbin")
                 (("--ro-bind /lib /lib") "--ro-bind /gnu /lib")))
             #t))
         ;; Remove the directory we gave to tests to have a clean package.
         (add-after 'check 'remove-tmp-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively (string-append (assoc-ref outputs "out") "/tmp"))
             #t)))))
    (inputs (list libcap))
    (native-inputs (list python-wrapper util-linux))
    (home-page "https://github.com/containers/bubblewrap")
    (synopsis "Unprivileged sandboxing tool")
    (description "Bubblewrap is aimed at running applications in a sandbox,
restricting their access to parts of the operating system or user data such as
the home directory.  Bubblewrap always creates a new mount namespace, and the
user can specify exactly what parts of the file system should be made visible
in the sandbox.  These directories are mounted with the @code{nodev} option
by default and can be made read-only.")
    (license license:lgpl2.0+)))

(define-public bochs
  (package
    (name "bochs")
    (version "2.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/bochs/files/bochs/"
                           version "/bochs-" version ".tar.gz"))
       (sha256
        (base32 "0ymiwnfqg5npq2dk9ngidbbfn3qw8z6i491finhcaan7zldsn450"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests exist
    (inputs
     (list libxrandr))
    (home-page "http://bochs.sourceforge.net/")
    (synopsis "Emulator for x86 PC")
    (description
     "Bochs is an emulator which can emulate Intel x86 CPU, common I/O
devices, and a custom BIOS.  It can also be compiled to emulate many different
x86 CPUs, from early 386 to the most recent x86-64 Intel and AMD processors.
Bochs can run most Operating Systems inside the emulation including Linux,
DOS or Microsoft Windows.")
    (license license:lgpl2.0+)))

(define-public xen
  (package
    (name "xen")
    (version "4.14.1")               ; please update the mini-os input as well
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://xenbits.xen.org/git-http/xen.git")
                    (commit (string-append "RELEASE-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r90rvypw76ya9clqw5p02gm1k8hxz73f7gr95ca778nnzvb7xjw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-rpath"
             "--disable-qemu-traditional" ; It tries to do "git clone"
             "--disable-rombios" ; would try to "git clone" via etherboot.
             ;; TODO: Re-enable stubdom (it's "more secure" to use it).
             "--disable-stubdom" ; tries to "git clone" old patched newlib.
             (string-append "--with-initddir="
                            (assoc-ref %outputs "out")
                            "/etc/init.d")
             (string-append "--with-system-qemu="
                            (assoc-ref %build-inputs "qemu")
                            "/bin/qemu-system-i386")
             (string-append "--with-system-seabios="
                            (assoc-ref %build-inputs "seabios")
                            "/share/firmware/bios.bin")
             (string-append "--with-system-ovmf="
                            (assoc-ref %build-inputs "ovmf")
                            "/share/firmware/ovmf_ia32.bin"))
       #:make-flags (list "-j" "1"
                          "XEN_BUILD_DATE=Thu Jan  1 01:00:01 CET 1970"
                          "XEN_BUILD_TIME=01:00:01"
                          "XEN_BUILD_HOST="
                          "ETHERBOOT_NICS="
                          "SMBIOS_REL_DATE=01/01/1970"
                          "VGABIOS_REL_DATE=01 Jan 1970"
                          ; QEMU_TRADITIONAL_LOC
                          ; QEMU_UPSTREAM_LOC
                          "SYSCONFIG_DIR=/tmp/etc/default"
                          (string-append "BASH_COMPLETION_DIR="
                                         (assoc-ref %outputs "out")
                                         "/etc/bash_completion.d")
                          (string-append "BOOT_DIR="
                                         (assoc-ref %outputs "out")
                                         "/boot")
                          (string-append "DEBUG_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/debug")
                          (string-append "EFI_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/efi") ; TODO lib64 ?
                          "MINIOS_UPSTREAM_URL="
                          ;(string-append "DISTDIR="
                          ;               (assoc-ref %outputs "out"))
)
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'unpack-mini-os
          (lambda* (#:key inputs #:allow-other-keys)
            (copy-recursively (assoc-ref inputs "mini-os") "extras/mini-os")
            #t))
        (add-after 'unpack-mini-os 'patch
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "tools/firmware/Rules.mk"
             (("override XEN_TARGET_ARCH = x86_32")
              (string-append "override XEN_TARGET_ARCH = x86_32
override CC = " (assoc-ref inputs "cross-gcc") "/bin/i686-linux-gnu-gcc"))
             (("^CFLAGS =$")
              (string-append "CFLAGS=-I" (assoc-ref inputs "cross-libc")
                             "/include\n")))
            (substitute* "config/x86_32.mk"
             (("CFLAGS += -m32 -march=i686")
              (string-append "CFLAGS += -march=i686 -I"
                             (assoc-ref inputs "cross-libc")
                             "/include")))
            ;; /var is not in /gnu/store , so don't try to create it.
            (substitute* '("tools/Makefile"
                           "tools/xenstore/Makefile"
                           "tools/xenpaging/Makefile")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(DUMP|LOG|RUN|LIB|PAGING)_DIR.*")
              "\n")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(RUN|LIB)_STORED.*")
              "\n"))
            ;; Prevent xen from creating /etc .
            (substitute* "tools/examples/Makefile"
             ((" install-readmes") "")
             ((" install-configs") ""))
            ;; Set rpath.
            (substitute* "tools/pygrub/setup.py"
             (("library_dirs =")
              ; TODO: extra_link_args = ['-Wl,-rpath=/opt/foo'],
              (string-append "runtime_library_dirs = ['"
                             (assoc-ref outputs "out")
                             "/lib'],\nlibrary_dirs =")))
            #t))
        (add-before 'configure 'patch-xen-script-directory
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* '("configure"
                           "tools/configure"
                           "docs/configure")
             (("XEN_SCRIPT_DIR=.*")
              (string-append "XEN_SCRIPT_DIR="
                             (assoc-ref outputs "out")
                             "/etc/xen/scripts")))
            #t))
        (add-before 'configure 'set-environment-up
          (lambda* (#:key make-flags #:allow-other-keys)
             (define (cross? x)
               (string-contains x "cross-i686-linux"))
             (define (filter-environment! filter-predicate
                                          environment-variable-names)
               (for-each
                (lambda (env-name)
                  (let* ((env-value (getenv env-name))
                         (search-path (search-path-as-string->list env-value))
                         (new-search-path (filter filter-predicate
                                                  search-path))
                         (new-env-value (list->search-path-as-string
                                         new-search-path ":")))
                    (setenv env-name new-env-value)))
                environment-variable-names))
             (setenv "CROSS_C_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))
             (setenv "CROSS_CPLUS_INCLUDE_PATH" (getenv "CPLUS_INCLUDE_PATH"))
             (setenv "CROSS_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
             (filter-environment! cross?
              '("CROSS_C_INCLUDE_PATH" "CROSS_CPLUS_INCLUDE_PATH"
                "CROSS_LIBRARY_PATH"))
             (filter-environment! (lambda (e) (not (cross? e)))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
             ;; Guix tries to be helpful and automatically adds
             ;; mini-os-git-checkout/include to the include path,
             ;; but actually we don't want it to be there (yet).
             (filter-environment! (lambda (e)
                                    (not
                                     (string-contains e
                                      "mini-os-git-checkout")))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
            (setenv "EFI_VENDOR" "guix")
             #t))
        (replace 'build
          (lambda* (#:key make-flags #:allow-other-keys)
            (apply invoke "make" "world" make-flags))))))
    (inputs
     `(("acpica" ,acpica) ; TODO: patch iasl invocation.
       ("bridge-utils" ,bridge-utils) ; TODO: patch invocations.
       ("glib" ,glib)
       ("iproute" ,iproute) ; TODO: patch invocations.
       ("libaio" ,libaio)
       ("libx11" ,libx11)
       ("libyajl" ,libyajl)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("ovmf" ,ovmf)
       ("pixman" ,pixman)
       ("qemu" ,qemu-minimal)
       ("seabios" ,seabios)
       ("util-linux" ,util-linux "lib") ; uuid
       ; TODO: ocaml-findlib, ocaml-nox.
       ("xz" ,xz) ; for liblzma
       ("zlib" ,zlib)))
    (native-inputs
     `(("dev86" ,dev86)
       ("bison" ,bison)
       ("cmake" ,cmake-minimal)
       ("figlet" ,figlet)
       ("flex" ,flex)
       ("gettext" ,gettext-minimal)
       ("libnl" ,libnl)
       ("mini-os"
       ,(origin
         (method git-fetch)
         (uri (git-reference
               (url "https://xenbits.xen.org/git-http/mini-os.git")
               ;; This corresponds to (string-append "xen-RELEASE-" version))
               ;; at time of packaging, but upstream has unfortunately modified
               ;; existing tags in the past.
               (commit "0b4b7897e08b967a09bed2028a79fabff82342dd")))
         (sha256
          (base32 "1i8pcl19n60i2m9vlg79q3nknpj209c9ic5x10wxaicx45kc107f"))
         (file-name "mini-os-git-checkout")))
       ("perl" ,perl)
       ; TODO: markdown
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("wget" ,wget)
       ("cross-gcc" ,(cross-gcc "i686-linux-gnu"
                                #:xbinutils (cross-binutils "i686-linux-gnu")
                                #:libc (cross-libc "i686-linux-gnu")))
       ("cross-libc" ,(cross-libc "i686-linux-gnu")) ; header files
       ("cross-libc-static" ,(cross-libc "i686-linux-gnu") "static")))
    (home-page "https://xenproject.org/")
    (synopsis "Xen Virtual Machine Monitor")
    (description "This package provides the Xen Virtual Machine Monitor
which is a hypervisor.")
    ;; TODO: Some files are licensed differently.  List those.
    (license license:gpl2)
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))))

(define-public osinfo-db-tools
  (package
    (name "osinfo-db-tools")
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.pagure.org/libosinfo/osinfo-db-tools-"
                                  version ".tar.xz"))

              (sha256
               (base32
                "1h23a8nzdxjyvw44dwh903563n3b1z5skx8g0b1p1v5cif3iqpr5"))))
    (build-system meson-build-system)
    (inputs
     (list libsoup-minimal-2 libxml2 libxslt json-glib libarchive))
    (native-inputs
     (list perl
           gobject-introspection
           gettext-minimal
           pkg-config
           ;; Tests
           python
           python-pytest
           python-requests))
    (home-page "https://gitlab.com/libosinfo/osinfo-db-tools")
    (synopsis "Tools for managing the osinfo database")
    (description "This package contains a set of tools to assist
administrators and developers in managing the database.")
    (license license:lgpl2.0+)))

(define-public osinfo-db
  (package
    (name "osinfo-db")
    (version "20211216")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.pagure.org/libosinfo/osinfo-db-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "10hhpciqk4lzsj66zkdvghd1i5zh6hg1fn9as4qhwcr1wnqfgv09"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (osinfo-dir (string-append out "/share/osinfo"))
                (source (assoc-ref %build-inputs "source"))
                (osinfo-db-import
                 (string-append (assoc-ref %build-inputs "osinfo-db-tools")
                                "/bin/osinfo-db-import")))
           (mkdir-p osinfo-dir)
           (invoke osinfo-db-import "--dir" osinfo-dir source)))))
    (native-inputs
     (list intltool osinfo-db-tools))
    (home-page "https://gitlab.com/libosinfo/osinfo-db")
    (synopsis "Database of information about operating systems")
    (description "Osinfo-db provides the database files for use with the
libosinfo library.  It provides information about guest operating systems for
use with virtualization provisioning tools")
    (license license:lgpl2.0+)))

(define-public python-transient
  (package
    (name "python-transient")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "transient" version))
       (sha256
        (base32
         "148yiqrmcscsi6787y0f27i1y9cf0gcw3mqfv5frhpmsmv62mv5z"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Requires behave
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-dependencies
                    (lambda _
                      (substitute* "setup.py"
                        (("==")
                         ">="))
                      #t)))))
    (propagated-inputs
     (list python-beautifultable
           python-click
           python-importlib-resources
           python-lark-parser
           python-marshmallow
           python-progressbar2
           python-requests
           python-toml))
    (native-inputs
     (list python-black python-mypy python-pyhamcrest python-twine))
    (home-page
     "https://github.com/ALSchwalm/transient")
    (synopsis
     "QEMU Wrapper written in Python")
    (description
     "@code{transient} is a wrapper for QEMU allowing the creation of virtual
machines with shared folder, ssh, and disk creation support.")
    (license license:expat)))
