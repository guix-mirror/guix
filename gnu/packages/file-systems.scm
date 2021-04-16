;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2017, 2018, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2021 raid5atemyhomework <raid5atemyhomework@protonmail.com>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
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

(define-module (gnu packages file-systems)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sssd)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml))

(define-public autofs
  (package
    (name "autofs")
    (version "5.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kernel.org/linux/daemons/autofs/"
                           "v" (version-major version) "/"
                           "autofs-" version ".tar.xz"))
       (sha256
        (base32 "1myfz6a3wj2c4j9h5g44zj796fdi82jhp1s92w2hg6xp2632csx3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-ignore-busy"     ; during shutdown
             "--enable-sloppy-mount"    ; support mount(8) -s
             "--with-libtirpc"
             (string-append "--with-openldap="
                            (assoc-ref %build-inputs "openldap"))
             (string-append "--with-sasl="
                            (assoc-ref %build-inputs "cyrus-sasl"))
             "HAVE_SSS_AUTOFS=1"        ; required to make sssldir click
             (string-append "sssldir="
                            (assoc-ref %build-inputs "sssd")
                            "/lib/sssd/modules"))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-hard-coded-search-path
           (lambda _
             (substitute* "configure"
               (("^searchpath=\".*\"")
                "searchpath=\"$PATH\""))
             #t))
         (add-before 'configure 'fix-rpath
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile.rules"
                 (("^AUTOFS_LIB_LINK.*=" match)
                  (string-append match " -Wl,-rpath=" out "/lib"))))))
         (add-before 'install 'omit-obsolete-lookup_nis.so-link
           ;; Building lookup_yp.so depends on $(YPCLNT) but this doesn't,
           ;; leading to a make error.  Since it's broken, comment it out.
           (lambda _
             (substitute* "modules/Makefile"
               (("ln -fs lookup_yp.so" match)
                (string-append "# " match)))
             #t)))))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)
       ("rpcsvc-proto" ,rpcsvc-proto)))
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("e2fsprogs" ,e2fsprogs)         ; for e[234]fsck
       ("libtirpc" ,libtirpc)
       ("libxml2" ,libxml2)             ; needed for LDAP, SASL
       ("mit-krb5" ,mit-krb5)           ; needed for LDAP, SASL
       ("nfs-utils" ,nfs-utils)         ; for mount.nfs
       ("openldap" ,openldap)
       ("openssl" ,openssl)             ; needed for SASL
       ("sssd" ,sssd)
       ("util-linux" ,util-linux)))     ; for mount, umount
    ;; XXX A directory index is the closest thing this has to a home page.
    (home-page "https://www.kernel.org/pub/linux/daemons/autofs/")
    (synopsis "Kernel-based automounter for Linux")
    (description
     "Autofs is a kernel-based automounter for use with the Linux autofs4
module.  It automatically mounts selected file systems when they are used and
unmounts them after a set period of inactivity.  This provides
centrally-managed, consistent file names for users and applications, even in a
large and/or frequently changing (network) environment.")
    ;; fedfs/ is GPL-2-only but not built.
    (license (list license:bsd-3        ; modules/cyrus-sasl.c
                   license:gpl2+))))    ; the rest

(define-public bindfs
  (package
    (name "bindfs")
    (version "1.15.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bindfs.org/downloads/bindfs-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1av8dj9i1g0105fs5r9srqqsp7yahlhwc0yl8i1szyfdls23bp84"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: The tests have no hope of passing until there is a "nogroup"
     ;; entry (or at least some group to which the guix builder does
     ;; not belong) in the /etc/group file of the build environment.
     ;; Currently we do not have such a group.  Disable tests for now.
     '(#:tests? #f))
    (native-inputs
       ;; Native inputs to run the tests
       ;; ("ruby" ,ruby)
       ;; ("valgrind" ,valgrind)
       ;; ("which" ,which)
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fuse" ,fuse)))
    (home-page "https://bindfs.org")
    (synopsis "Bind mount a directory and alter permission bits")
    (description
     "@command{bindfs} is a FUSE file system for mounting a directory to
another location, similar to @command{mount --bind}.  It can be used for:
@itemize
@item Making a directory read-only.
@item Making all executables non-executable.
@item Sharing a directory with a list of users (or groups).
@item Modifying permission bits using rules with chmod-like syntax.
@item Changing the permissions with which files are created.
@end itemize ")
    (license license:gpl2+)))

(define-public davfs2
  (package
    (name "davfs2")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.savannah.nongnu.org/releases/"
                           "davfs2/davfs2-" version ".tar.gz"))
       (sha256
        (base32 "0l1vnv5lfigciwg17p10zxwhzj4qw2d9kw30prr7g4dxhmb6fsrf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--sysconfdir=/etc"        ; so man pages & binaries contain /etc
             (string-append "--docdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version)
             (string-append "ssbindir=" (assoc-ref %outputs "out") "/sbin")
             ;; The default ‘davfs2’ user and group don't exist on most systems.
             "dav_user=nobody"
             "dav_group=nogroup")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'omit-redundancy
           ;; Don't install redundant copies of /etc examples into /share.
           (lambda _
             (substitute* "etc/Makefile.in"
               (("(dist_pkgdata_DATA =.*) davfs2.conf secrets(.*)"
                 _ prefix suffix)
                (string-append prefix suffix)))
             #t))
         (add-after 'unpack 'patch-file-names
           (lambda _
             ;; Don't auto-load the FUSE kernel module.  That's up to root.
             ;; XXX If/when we restore the previous behaviour, make sure not
             ;; to introduce a security hole when mount.davfs is setuid.
             (substitute* "src/kernel_interface.c"
               (("/sbin/modprobe") "/modprobe/disabled"))
             #t))
         (replace 'install
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (apply invoke "make" "install"
                      (string-append "pkgsysconfdir=" out "/etc")
                      make-flags)))))))
    (inputs
     `(("neon" ,neon)

       ;; Neon requires but doesn't propagate zlib, nor would we want that.
       ;; XZ as well, but that's already present in the build environment.
       ("zlib" ,zlib)))
    (home-page "https://savannah.nongnu.org/projects/davfs2")
    (synopsis "Mount remote WebDAV resources in the local file system")
    (description
     "The @acronym{WebDAV, Web Distributed Authoring and Versioning} extension
to the HTTP protocol defines a standard way to author resources on a remote Web
server.  Davfs2 exposes such resources as a typical file system which can be
used by standard applications with no built-in support for WebDAV, such as the
GNU coreutils (@command{cp}, @command{mv}, etc.) or a graphical word processor.

Davfs2 works with most WebDAV servers with no or little configuration.  It
supports TLS (HTTPS), HTTP proxies, HTTP basic and digest authentication, and
client certificates.  It performs extensive caching to avoid unnecessary network
traffic, stay responsive even over slow or unreliable connections, and prevent
data loss.  It aims to make use by unprivileged users as easy and secure as
possible.

However, davfs2 is not a full-featured WebDAV client.  The file system interface
and the WebDAV protocol are quite different.  Translating between the two is not
always possible.")
    (license (list license:bsd-2        ; src/fuse_kernel.h
                   license:gpl3+))))    ; everything else

(define-public fsarchiver
  (package
    (name "fsarchiver")
    (version "0.8.6")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/fdupoux/fsarchiver")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ry2sdkfbg4bwcldk42g1i3wa3z4pr9yh9dil6ilhwcvhqiw41zc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("bzip2" ,bzip2)
       ("e2fsprogs" ,e2fsprogs)
       ("libgcrypt" ,libgcrypt)
       ("lz4" ,lz4)
       ("lzo" ,lzo)
       ("util-linux" ,util-linux "lib")
       ("xz" ,xz)
       ("zlib" ,zlib)
       ("zstd:lib" ,zstd "lib")))
    (synopsis "File system back-up, deployment, and migration tool")
    (description
     "FSArchiver saves the contents of a file system to a compressed archive
file, and restores it to a different file system and/or partition.  This
partition can be of a different size than the original and FSArchiver will
create a new file system if none exists.

All standard file attributes supported by the kernel are preserved, including
file permissions, timestamps, symbolic and hard links, and extended attributes.

Each file in the archive is protected by a checksum.  If part of the archive
is corrupted you'll lose the affected file(s) but not the whole back-up.")
    (home-page "https://www.fsarchiver.org/")
    (license license:gpl2)))

(define-public gphotofs
  (package
    (name "gphotofs")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/gphoto/gphotofs/" version
                       "/gphotofs-0.5.tar.gz"))
       (sha256
        (base32
         "04slwhr6ap9xcc27wphk22ad8yn79ngyy5z10lxams3k5liahvc2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fuse" ,fuse)
       ("glib" ,glib)
       ("libgphoto2" ,libgphoto2)))
    (synopsis "Virtual file system for libgphoto2 using FUSE")
    (description "GPhotoFS is a FUSE file system module to mount your camera as
a file system on Linux.  This allow using your camera with any tool able to read
from a mounted file system.")
    (home-page "http://www.gphoto.org/proj/gphotofs/")
    (license license:gpl2+)))

(define-public bcachefs-tools
  (let ((commit "bb6eccc2ecd4728871bfc70462d3a4a20daa9d68")
        (revision "4"))
    (package
      (name "bcachefs-tools")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://evilpiepirate.org/git/bcachefs-tools.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ziqmcxbrak6bjck6s46hqrqx44zc97yaj0kbk3amsxf18rsfs0n"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags
         (list ,(string-append "VERSION=" version) ; bogus vX.Y-nogit otherwise
               (string-append "PREFIX=" (assoc-ref %outputs "out"))
               "INITRAMFS_DIR=$(PREFIX)/share/initramfs-tools"
               "CC=gcc"
               "PYTEST=pytest")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (add-after 'install 'promote-mount.bcachefs.sh
             ;; XXX The (optional) mount.bcachefs helper requires rust:cargo.
             ;; This alternative shell script does the job well enough for now.
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion (string-append out "/sbin")
                 (rename-file "mount.bcachefs.sh" "mount.bcachefs")
                 ;; WRAP-SCRIPT causes bogus ‘Insufficient arguments’ errors.
                 (wrap-program "mount.bcachefs"
                   `("PATH" ":" prefix
                     ,(cons (string-append out "/sbin")
                            (map (lambda (input)
                                     (string-append (assoc-ref inputs input)
                                                    "/bin"))
                                   (list "coreutils"
                                         "gawk"
                                         "util-linux"))))))))))
         #:tests? #f))                  ; XXX 6 valgrind tests fail
      (native-inputs
       `(("pkg-config" ,pkg-config)

         ;; For tests.
         ("python-pytest" ,python-pytest)
         ("valgrind" ,valgrind)))
      (inputs
       `(("eudev" ,eudev)
         ("keyutils" ,keyutils)
         ("libaio" ,libaio)
         ("libscrypt" ,libscrypt)
         ("libsodium" ,libsodium)
         ("liburcu" ,liburcu)
         ("util-linux:lib" ,util-linux "lib") ; lib{blkid,uuid}
         ("lz4" ,lz4)
         ("zlib" ,zlib)
         ("zstd:lib" ,zstd "lib")

         ;; Only for mount.bcachefs.sh.
         ("coreutils" ,coreutils-minimal)
         ("gawk" ,gawk)
         ("util-linux" ,util-linux)))
      (home-page "https://bcachefs.org/")
      (synopsis "Tools to create and manage bcachefs file systems")
      (description
       "The bcachefs-tools are command-line utilities for creating, checking,
and otherwise managing bcachefs file systems.

Bcachefs is a @acronym{CoW, copy-on-write} file system supporting native
encryption, compression, snapshots, and (meta)data checksums.  It can use
multiple block devices for replication and/or performance, similar to RAID.

In addition, bcachefs provides all the functionality of bcache, a block-layer
caching system, and lets you assign different roles to each device based on its
performance and other characteristics.")
      (license license:gpl2+))))

(define-public bcachefs-tools/static
   (package
     (inherit bcachefs-tools)
     (name "bcachefs-tools-static")
     (arguments
      (substitute-keyword-arguments (package-arguments bcachefs-tools)
        ((#:make-flags make-flags)
         `(append ,make-flags
                  (list "LDFLAGS=-static")))))
     (inputs
      `(("eudev:static" ,eudev "static")
        ("libscrypt:static" ,libscrypt "static")
        ("lz4:static" ,lz4 "static")
        ("util-linux:static" ,util-linux "static") ; lib{blkid,uuid}
        ("zlib" ,zlib "static")
        ("zstd:static" ,zstd "static")
        ,@(package-inputs bcachefs-tools)))))

(define-public bcachefs/static
  (package
    (name "bcachefs-static")
    (version (package-version bcachefs-tools))
    (build-system trivial-build-system)
    (source #f)
    (inputs
     `(("bcachefs-tools" ,bcachefs-tools/static)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (srfi srfi-26))
         (let* ((bcachefs-tools (assoc-ref %build-inputs "bcachefs-tools"))
                (out (assoc-ref %outputs "out")))
           (mkdir-p out)
           (with-directory-excursion out
             (install-file (string-append bcachefs-tools
                                          "/sbin/bcachefs")
                           "sbin")
             (remove-store-references "sbin/bcachefs")
             (invoke "sbin/bcachefs" "version") ; test suite
             #t)))))
    (home-page (package-home-page bcachefs-tools))
    (synopsis "Statically-linked bcachefs command from bcachefs-tools")
    (description "This package provides the statically-linked @command{bcachefs}
from the bcachefs-tools package.  It is meant to be used in initrds.")
    (license (package-license bcachefs-tools))))

(define-public exfatprogs
  (package
    (name "exfatprogs")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/exfatprogs/exfatprogs")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ciy28lx7c1vr1f138qi0mkz88pzlkay6nlwmp1yjzd830x48549"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/exfatprogs/exfatprogs")
    (synopsis "Tools to create, check, and repair exFAT file systems")
    (description
     "These are command-line user space tools for the @acronym{exFAT,
Extensible File Allocation Table} file systems.  Included are
@command{mkfs.exfat} to create (format) new exFAT file systems, and
@command{fsck.exfat} to check their consistency and repair them.")
    (license license:gpl2+)))

(define-public httpfs2
  (package
    (name "httpfs2")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/httpfs/httpfs2/"
                           "httpfs2-" version ".tar.gz"))
       (sha256
        (base32
         "1h8ggvhw30n2r6w11n1s458ypggdqx6ldwd61ma4yd7binrlpjq1"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("docbook-xml" ,docbook-xml)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("fuse" ,fuse)
       ("gnutls" ,gnutls)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'install
           ;; There's no ‘install’ target. Install all variants manually.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1")))
               (mkdir-p bin)
               (mkdir-p man1)
               (for-each
                (lambda (variant)
                  (let ((man1-page (string-append variant ".1")))
                    (install-file variant bin)
                    (install-file man1-page man1)))
                (list "httpfs2"
                      "httpfs2-mt"
                      "httpfs2-ssl"
                      "httpfs2-ssl-mt")))
             #t)))
       #:make-flags (list "CC=gcc")
       #:parallel-build? #f             ; can result in missing man pages
       #:tests? #f))                    ; no tests
    (home-page "https://sourceforge.net/projects/httpfs/")
    (synopsis "Mount remote files over HTTP")
    (description "httpfs2 is a @code{fuse} file system for mounting any
@dfn{HyperText} (HTTP or HTTPS) URL.  It uses HTTP/1.1 byte ranges to request
arbitrary bytes from the web server, without needing to download the entire
file.  This is particularly useful with large archives such as ZIP files and
ISO images when you only need to inspect their contents or extract specific
files.  Since the HTTP protocol itself has no notion of directories, only a
single file can be mounted.")
    (license license:gpl2+)))

(define-public jfsutils
  (package
    (name "jfsutils")
    (version "1.1.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://jfs.sourceforge.net/project/pub/jfsutils-"
                           version ".tar.gz"))
       (sha256
        (base32 "0kbsy2sk1jv4m82rxyl25gwrlkzvl3hzdga9gshkxkhm83v1aji4"))
       (patches (search-patches "jfsutils-add-sysmacros.patch"
                                "jfsutils-include-systypes.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("util-linux" ,util-linux "lib")))
    (home-page "http://jfs.sourceforge.net/home.html")
    (synopsis "Utilities for managing JFS file systems")
    (description
     "The JFSutils are a collection of utilities for managing the @acronym{JFS,
Journaled File System}, a 64-bit journaling file system created by IBM and later
ported to the kernel Linux.  The following commands are available:
@enumerate
@item @command{fsck.jfs}: check and repair a JFS file system or replay its
transaction log.
@item @command{logdump}: dump the JFS journal log.
@item @command{logredo}: replay the JFS journal log.
@item @command{mkfs.jfs}: create a new JFS file system.
@item @command{xchklog}: save a JFS fsck log to a file.
@item @command{xchkdmp}: dump the contents of such a log file.
@item @command{xpeek}: a JFS file system editor with a shell-like interface.
@end enumerate\n")
    (license license:gpl3+)))          ; no explicit version given

(define-public jfsutils/static
  (static-package
   (package
     (inherit jfsutils)
     (name "jfsutils-static")
     (inputs
      `(("util-linux:static" ,util-linux "static")
        ,@(package-inputs jfsutils))))))

(define-public jfs_fsck/static
  (package
    (name "jfs_fsck-static")
    (version (package-version jfsutils))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (srfi srfi-26))
         (let* ((jfsutils (assoc-ref %build-inputs "jfsutils"))
                (fsck     "jfs_fsck")
                (out      (assoc-ref %outputs "out"))
                (sbin     (string-append out "/sbin")))
           (mkdir-p sbin)
           (with-directory-excursion sbin
             (install-file (string-append jfsutils "/sbin/" fsck)
                           ".")
             (remove-store-references fsck)
             (chmod fsck #o555))
           #t))))
    (inputs
     `(("jfsutils" ,jfsutils/static)))
    (home-page (package-home-page jfsutils))
    (synopsis "Statically-linked jfs_fsck command from jfsutils")
    (description "This package provides statically-linked jfs_fsck command taken
from the jfsutils package.  It is meant to be used in initrds.")
    (license (package-license jfsutils))))

(define-public disorderfs
  (package
    (name "disorderfs")
    (version "0.5.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://salsa.debian.org/reproducible-builds/disorderfs.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1pnrj0h8sgqwgsc18vz3fkqsp6vhigdbi75vdj0si1r6wgslnr7z"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fuse" ,fuse)
       ("attr" ,attr)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))  ; no configure script
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)))
       #:test-target "test"
       ;; FIXME: Tests require 'run-parts' which is not in Guix yet.
       #:tests? #f))
    (home-page "https://salsa.debian.org/reproducible-builds/disorderfs")
    (synopsis "FUSE file system that introduces non-determinism")
    (description
     "An overlay FUSE file system that introduces non-determinism
into file system metadata.  For example, it can randomize the order
in which directory entries are read.  This is useful for detecting
non-determinism in the build process.")
    (license license:gpl3+)))

(define-public glusterfs
  (package
    (name "glusterfs")
    (version "7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.gluster.org/pub/gluster/glusterfs/"
                           (version-major version) "/"
                           (version-major+minor version) "/"
                           "glusterfs-" version ".tar.gz"))
       (sha256
        (base32
         "0yzhx710ypj0j3m5dcgmmgvkp7p0rmmp2p7ld0axrm4vpwc2b1wa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (let ((out (assoc-ref %outputs "out"))
             (p2 (assoc-ref %build-inputs "python-2")))
         (list (string-append "PYTHON=" p2 "/bin/python")
               (string-append "--with-initdir=" out "/etc/init.d")
               (string-append "--with-mountutildir=" out "/sbin")
               "--enable-cmocka"  ; unit tests
               ;; "--enable-debug"  ; debug build options
               ;; "--enable-asan"  ; Address Sanitizer
               ;; "--enable-tsan"  ; ThreadSanitizer
               ))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autogen
           (lambda _ (invoke "./autogen.sh"))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libtirpc" ,libtirpc)
       ("rpcsvc-proto" ,rpcsvc-proto)
       ("python-2" ,python-2) ; must be version 2
       ("flex" ,flex)
       ("bison" ,bison)
       ("libtool" ,libtool)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("cmocka" ,cmocka)))
    (inputs
     `(("acl" ,acl)
       ("fuse" ,fuse)
       ("openssl" ,openssl)
       ("liburcu" ,liburcu)
       ("libuuid" ,util-linux "lib")
       ("libxml2" ,libxml2)
       ("readline" ,readline)
       ("zlib" ,zlib)
       ("libaio" ,libaio)
       ("rdma-core" ,rdma-core)))
    (home-page "https://www.gluster.org")
    (synopsis "Distributed file system")
    (description "GlusterFS is a distributed scalable network file system
suitable for data-intensive tasks such as cloud storage and media streaming.
It allows rapid provisioning of additional storage based on your storage
consumption needs.  It incorporates automatic failover as a primary feature.
All of this is accomplished without a centralized metadata server.")
    ;; The user may choose either LGPLv3+ or GPLv2 only.
    (license (list license:lgpl3+ license:gpl2+))))

(define-public curlftpfs
  (package
    (name "curlftpfs")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/curlftpfs/curlftpfs/" version
                           "/curlftpfs-" version ".tar.gz"))
       (sha256
        (base32
         "0n397hmv21jsr1j7zx3m21i7ryscdhkdsyqpvvns12q7qwwlgd2f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test
           (lambda _
             ;; One of the 512-Byte block counts is definitely wrong.
             ;; See <https://sourceforge.net/p/curlftpfs/bugs/73/>.
             (substitute* "tests/ftpfs-ls_unittest.c"
              (("4426192") "12814800"))
             #t)))))
    (inputs
     `(("curl" ,curl)
       ("glib" ,glib)
       ("fuse" ,fuse)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://curlftpfs.sourceforge.net/")
    (synopsis "Mount remote file systems over FTP")
    (description
     "This is a file system client based on the FTP File Transfer Protocol.")
    (license license:gpl2+)))

(define-public libnfs
  (package
    (name "libnfs")
    (version "3.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sahlberg/libnfs")
                    (commit (string-append "libnfs-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "115p55y2cbs92z5lmcnjx1v29lwinpgq4sha9v1kq1vd8674h404"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/sahlberg/libnfs")
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (synopsis "Client library for accessing NFS shares")
    (description "LIBNFS is a client library for accessing NFS shares over a
network.  LIBNFS offers three different APIs, for different use :

@enumerate
@item RAW, a fully asynchronous low level RPC library for NFS protocols.  This
  API provides very flexible and precise control of the RPC issued.
@item NFS ASYNC, a fully asynchronous library for high level vfs functions
@item NFS SYNC, a synchronous library for high level vfs functions.
@end enumerate\n")
    (license (list license:lgpl2.1+ ; library
                   license:gpl3+    ; tests
                   license:bsd-3    ; copied nsf4 files
                   ))))

(define-public apfs-fuse
  ;; Later versions require FUSE 3.
  (let ((commit "7b89418e8dc27103d3c4f8fa348086ffcd634c17")
        (revision "1"))
    (package
      (name "apfs-fuse")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/sgan81/apfs-fuse")
                       (recursive? #t) ; for lzfse
                       (commit commit)))
         (sha256
          (base32
           "0x2siy3cmnm9wsdfazg3xc8r3kbg73gijmnn1vjw33pp71ckylxr"))
         (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ; No test suite
         #:configure-flags
         '("-DUSE_FUSE3=OFF") ; FUSE 3 is not packaged yet.
         #:phases
         (modify-phases %standard-phases
           ;; No 'install' target in CMakeLists.txt
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (lib (string-append out "/lib"))
                      (doc (string-append out "/share/doc/"
                                          (string-append ,name "-" ,version))))
                 (install-file "apfs-dump" bin)
                 (install-file "apfs-dump-quick" bin)
                 (install-file "apfs-fuse" bin)
                 (install-file "libapfs.a" lib)
                 (install-file "../source/README.md" doc)
                 #t))))))
      (inputs
       `(("bzip2" ,bzip2)
         ("fuse" ,fuse)
         ("zlib" ,zlib)))
      (synopsis "Read-only FUSE driver for the APFS file system")
      (description "APFS-FUSE is a read-only FUSE driver for the @dfn{Apple File
System} (APFS).  It is currently in an experimental state — it may not be able
to read all files, and it does not support all the compression methods in
APFS.")
      (home-page "https://github.com/sgan81/apfs-fuse")
      (license license:gpl2+))))

(define-public zfs
  (package
    (name "zfs")
    (version "2.0.4")
    (outputs '("out" "module" "src"))
    (source
      (origin
        (method url-fetch)
          (uri (string-append "https://github.com/zfsonlinux/zfs/releases"
                              "/download/zfs-" version
                              "/zfs-" version ".tar.gz"))
          (sha256
           (base32 "0v2zshimz5miyj8mbskb52pnzyl1s4rhpr6208zq549v8g2l84vx"))))
    (build-system linux-module-build-system)
    (arguments
     `(;; The ZFS kernel module should not be downloaded since the license
       ;; terms don't allow for distributing it, only building it locally.
       #:substitutable? #f
       ;; Tests cannot run in an unprivileged build environment.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'really-configure
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "configure"
                 (("-/bin/sh") (string-append "-" (which "sh"))))
               (invoke "./configure"
                       "--with-config=all"
                       (string-append "--prefix=" out)
                       (string-append "--with-dracutdir=" out "/lib/dracut")
                       (string-append "--with-udevdir=" out "/lib/udev")
                       (string-append "--with-mounthelperdir=" out "/sbin")
                       (string-append "--with-linux="
                                      (assoc-ref inputs "linux-module-builder")
                                      "/lib/modules/build")))))
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out        (assoc-ref outputs "out"))
                   (src        (assoc-ref outputs "src"))
                   (util-linux (assoc-ref inputs "util-linux"))
                   (nfs-utils  (assoc-ref inputs "nfs-utils"))
                   (kmod       (assoc-ref inputs "kmod-runtime")))
               (substitute* "etc/Makefile.in"
                 ;; This just contains an example configuration file for
                 ;; configuring ZFS on traditional init systems, skip it
                 ;; since we cannot use it anyway; the install target becomes
                 ;; misdirected.
                 (("= default ") "= "))
               (substitute* "lib/libzfs/os/linux/libzfs_util_os.c"
                 ;; Use path to /gnu/store/*-kmod in actual path that is exec'ed.
                 (("\"/sbin/modprobe\"")
                  (string-append "\"" kmod "/bin/modprobe" "\""))
                 ;; Just use 'modprobe' in message to user, since Guix
                 ;; does not have a traditional /sbin/
                 (("'/sbin/modprobe ") "'modprobe "))
               (substitute* "contrib/Makefile.in"
                 ;; This is not configurable nor is its hard-coded /usr prefix.
                 ((" initramfs") ""))
               (substitute* "module/os/linux/zfs/zfs_ctldir.c"
                 (("/usr/bin/env\", \"umount")
                  (string-append util-linux "/bin/umount\", \"-n"))
                 (("/usr/bin/env\", \"mount")
                  (string-append util-linux "/bin/mount\", \"-n")))
               (substitute* "lib/libzfs/os/linux/libzfs_mount_os.c"
                 (("/bin/mount") (string-append util-linux "/bin/mount"))
                 (("/bin/umount") (string-append util-linux "/bin/umount")))
               (substitute* "lib/libshare/os/linux/nfs.c"
                 (("/usr/sbin/exportfs")
                  (string-append nfs-utils "/sbin/exportfs")))
               (substitute* "config/zfs-build.m4"
                 (("\\$sysconfdir/init.d") (string-append out "/etc/init.d")))
               (substitute* '("etc/zfs/Makefile.am"
                              "cmd/zed/Makefile.am")
                 (("\\$\\(sysconfdir)") (string-append out "/etc")))
               (substitute* "cmd/vdev_id/vdev_id"
                 (("PATH=/bin:/sbin:/usr/bin:/usr/sbin")
                  (string-append "PATH="
                                 (dirname (which "chmod")) ":"
                                 (dirname (which "grep")) ":"
                                 (dirname (which "sed")) ":"
                                 (dirname (which "gawk")))))
               (substitute* "contrib/pyzfs/Makefile.in"
                 ((".*install-lib.*") ""))
               (substitute* '("Makefile.am" "Makefile.in")
                 (("\\$\\(prefix)/src") (string-append src "/src")))
               (substitute* (find-files "udev/rules.d/" ".rules.in$")
                 (("/sbin/modprobe") (string-append kmod "/bin/modprobe"))))
             #t))
         (replace 'build
           (lambda _ (invoke "make")))
         (replace 'install
           (lambda* (#:key outputs inputs native-inputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (moddir (assoc-ref outputs "module"))
                    (kmod   (assoc-ref (or native-inputs inputs) "kmod")))
               (invoke "make" "install"
                       (string-append "DEFAULT_INITCONF_DIR=" out "/etc/default")
                       (string-append "DEPMOD=" kmod "/bin/depmod")
                       (string-append "INSTALL_PATH=" out)
                       (string-append "INSTALL_MOD_PATH=" moddir)
                       "INSTALL_MOD_STRIP=1")
               (install-file "contrib/bash_completion.d/zfs"
                             (string-append out "/share/bash-completion/completions"))
               #t))))))
    (native-inputs
     `(("attr" ,attr)
       ("kmod" ,kmod)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("eudev" ,eudev)
       ("kmod-runtime" ,kmod)
       ("libaio" ,libaio)
       ("libtirpc" ,libtirpc)
       ("nfs-utils" ,nfs-utils)
       ("openssl" ,openssl)
       ("python" ,python)
       ("python-cffi" ,python-cffi)
       ("util-linux" ,util-linux)
       ("util-linux:lib" ,util-linux "lib")
       ("zlib" ,zlib)))
    (home-page "https://zfsonlinux.org/")
    (synopsis "OpenZFS on Linux")
    (description
     "OpenZFS is an advanced file system and volume manager which was
originally developed for Solaris and is now maintained by the OpenZFS
community.")
    (license license:cddl1.0)))

(define-public mergerfs
  (package
    (name "mergerfs")
    (version "2.32.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/trapexit/mergerfs/releases/download/"
                           version "/mergerfs-" version ".tar.gz"))
       (sha256
        (base32
         "0yz7nljx6axcj6hb09sgc0waspgfhp535228rjqvqgyd8y74jc3s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No tests exist.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "CC" "gcc")
             ;; These were copied from the package libfuse.
             (substitute* '("libfuse/lib/mount_util.c" "libfuse/util/mount_util.c")
               (("/bin/(u?)mount" _ maybe-u)
                (string-append (assoc-ref inputs "util-linux")
                               "/bin/" maybe-u "mount")))
             (substitute* '("libfuse/util/mount.mergerfs.c")
               (("/bin/sh")
                (which "sh")))
             ;; The Makefile does not allow overriding PREFIX via make variables.
             (substitute* '("Makefile" "libfuse/Makefile")
               (("= /usr/local") (string-append "= " (assoc-ref outputs "out")))
               (("= /sbin") "= $(EXEC_PREFIX)/sbin")
               ;; cannot chown as build user
               (("chown root(:root)?") "true"))
             #t)))))
    ;; mergerfs bundles a heavily modified copy of libfuse.
    (inputs `(("util-linux" ,util-linux)))
    (home-page "https://github.com/trapexit/mergerfs")
    (synopsis "Featureful union file system")
    (description "mergerfs is a union file system geared towards simplifying
storage and management of files across numerous commodity storage devices.  It
is similar to mhddfs, unionfs, and aufs.")
    (license (list
              license:isc                   ; mergerfs
              license:gpl2 license:lgpl2.0  ; Imported libfuse code.
              ))))

(define-public mergerfs-tools
  (let ((commit "480296ed03d1c3c7909697d7ef96d35840ee26b8")
        (revision "2"))
    (package
      (name "mergerfs-tools")
      ;; No released version exists.
      (version (git-version "0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/trapexit/mergerfs-tools")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xr06gi4xcr832rzy0hkp5c1n231s7w5iq1nkjvx9kvm0dl7chpq"))))
      (build-system copy-build-system)
      (inputs
       `(("python" ,python)
         ("python-xattr" ,python-xattr)
         ("rsync" ,rsync)))
      (arguments
       '(#:install-plan
         '(("src/" "bin/"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* (find-files "src" "^mergerfs\\.")
                 (("'rsync'")
                  (string-append "'" (assoc-ref inputs "rsync") "/bin/rsync'"))
                 (("'rm'")
                  (string-append "'" (assoc-ref inputs "coreutils") "/bin/rm'")))
               (substitute* "src/mergerfs.mktrash"
                 (("xattr")
                  (string-append (assoc-ref inputs "python-xattr") "/bin/xattr"))
                 (("mkdir")
                  (string-append (assoc-ref inputs "coreutils") "/bin/mkdir")))
               #t)))))
      (synopsis "Tools to help manage data in a mergerfs pool")
      (description "mergerfs-tools is a suite of programs that can audit
permissions and ownership of files and directories on a mergerfs volume,
duplicates files and directories across branches in its pool, find and remove
duplicate files, balance pool drives, consolidate files in a single mergerfs
directory onto a single drive and create FreeDesktop.org Trash specification
compatible directories.")
      (home-page "https://github.com/trapexit/mergerfs-tools")
      (license license:isc))))

(define-public python-dropbox
  (package
    (name "python-dropbox")
    (version "11.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dropbox" version))
        (sha256
         (base32
          "16bxx9xqx2s4d9khrw57a0bj4q7nc6kq355wl4pfddn9cqvh9rg2"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))  ; Tests require a network connection.
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (propagated-inputs
     `(("python-certifi" ,python-certifi)
       ("python-chardet" ,python-chardet)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)
       ("python-stone" ,python-stone)
       ("python-urllib3" ,python-urllib3)))
    (home-page "https://www.dropbox.com/developers")
    (synopsis "Official Dropbox API Client")
    (description "This package provides a Python SDK for integrating with the
Dropbox API v2.")
    (license license:expat)))

(define-public dbxfs
  (package
    (name "dbxfs")
    (version "1.0.51")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dbxfs" version))
        (sha256
         (base32
          "1zz82d0mnql55397x4jx7z5rn857rf9zhjv895j93wpxdq10xwvk"))
        (patches (search-patches "dbxfs-remove-sentry-sdk.patch"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; tests requires safefs
    (propagated-inputs
     `(("python-appdirs" ,python-appdirs)
       ("python-block-tracing" ,python-block-tracing)
       ("python-dropbox" ,python-dropbox)
       ("python-keyring" ,python-keyring)
       ("python-keyrings.alt" ,python-keyrings.alt)
       ("python-privy" ,python-privy)
       ("python-userspacefs" ,python-userspacefs)))
  (home-page "https://github.com/rianhunter/dbxfs")
  (synopsis "User-space file system for Dropbox")
  (description
   "@code{dbxfs} allows you to mount your Dropbox folder as if it were a
local file system using FUSE.")
  (license license:gpl3+)))

(define-public go-github-com-hanwen-fuse
  (package
    (name "go-github-com-hanwen-fuse")
    (version "2.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hanwen/go-fuse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1y44d08fxyis99s6jxdr6dbbw5kv3wb8lkhq3xmr886i4w41lz03"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/hanwen/go-fuse"))
    (propagated-inputs
     `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/hanwen/go-fuse")
    (synopsis "FUSE bindings for Go")
    (description
     "This package provides Go native bindings for the FUSE kernel module.")
    (license license:bsd-3)))

(define-public tmsu
  (package
    (name "tmsu")
    (version "0.7.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oniony/TMSU")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0834hah7p6ad81w60ifnxyh9zn09ddfgrll04kwjxwp7ypbv38wq"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/oniony/TMSU"
       #:unpack-path ".."
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; The go build system produces /bin/TMSU -> install as /bin/tmsu
               (rename-file (string-append out "/bin/TMSU")
                            (string-append out "/bin/tmsu"))))))))
    (inputs
     `(("go-github-com-mattn-go-sqlite3" ,go-github-com-mattn-go-sqlite3)
       ("go-github-com-hanwen-fuse" ,go-github-com-hanwen-fuse)))
    (home-page "https://github.com/oniony/TMSU")
    (synopsis "Tag files and access them through a virtual file system")
    (description
     "TMSU is a tool for tagging your files.  It provides a simple
command-line utility for applying tags and a virtual file system to give you a
tag-based view of your files from any other program.  TMSU does not alter your
files in any way: they remain unchanged on disk, or on the network, wherever
your put them.  TMSU maintains its own database and you simply gain an
additional view, which you can mount where you like, based upon the tags you
set up.")
    (license license:gpl3+)))
