;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2020 Timotej Lazar <timotej.lazar@araneo.si>
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

(define-module (gnu packages cdrom)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (lgpl2.1+ gpl2 gpl2+ gpl3+ cddl1.0))
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages base)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages python)
  #:use-module (gnu packages image)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xiph))

(define-public libcddb
  (package
    (name "libcddb")
    (version "1.3.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/libcddb/libcddb/" version
                                 "/libcddb-" version ".tar.bz2"))
             (sha256
              (base32
               "0fr21a7vprdyy1bq6s99m0x420c9jm5fipsd63pqv8qyfkhhxkim"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))      ; tests rely on access to external servers
    (home-page "http://libcddb.sourceforge.net/")
    (synopsis "C library to access data on a CDDB server")
    (description
     "Libcddb is a C library to access data on a CDDB server (freedb.org).  It
allows you to:

 1. search the database for possible CD matches;

 2. retrieve detailed information about a specific CD;

 3. submit new CD entries to the database.

Libcddb supports both the custom CDDB protocol and tunnelling the query and
read operations over plain HTTP.  It is also possible to use an HTTP proxy
server.  If you want to speed things up, you can make use of the built-in
caching facility provided by the library.")
    (license lgpl2.1+)))

(define-public libcdio
  (package
    (name "libcdio")
    (version "2.1.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/libcdio/libcdio-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0avi6apv5ydjy6b9c3z9a46rvp5i57qyr09vr7x4nndxkmcfjl45"))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses libcddb))
    (native-inputs
     (list help2man pkg-config))
    (home-page "https://www.gnu.org/software/libcdio/")
    (synopsis "CD Input and Control library")
    (description
     "The GNU Compact Disc Input and Control Library (libcdio) is a library
for CD-ROM and CD image file access.  It allows the developer to add CD
access to an application without having to worry about the OS- and
device-dependent properties of CD-ROM or the specific details of CD image
formats.  It includes pycdio, a Python interface to libcdio, and
libcdio-paranoia, a library providing jitter-free and error-free audio
extraction from CDs.")
    (license gpl3+)))

(define-public libcdio-paranoia
  (package
    (name "libcdio-paranoia")
    (version "10.2+2.0.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/libcdio/libcdio-paranoia-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "12hfnrq7amv9qjzc92cr265m7kh0a1hpasck8cxx1gygbhqczc9k"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (propagated-inputs (list libcdio))
    (home-page "https://www.gnu.org/software/libcdio/")
    (synopsis "Jitter- and error-tolerant CD audio extraction")
    (description
     "libcdio-paranoia is an implementation of CD paranoia libraries based on
libcdio.")
    (license gpl3+)))

;; Xorriso is used by Guix for creating ISO images. If you change this package,
;; please make sure the Guix functionality still works by running some related
;; system tests.
;; For example, try running `make check-system TESTS=iso-image-installer`.
(define-public xorriso
  (package
    (name "xorriso")
    (version "1.5.2")
    (outputs '("out" "gui"))
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/xorriso/xorriso-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1rqpzj95f70jfwpn4lamasfgqpizjsipz12aprdhri777b4zas9v"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-frontends
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin")))
               (install-file "frontend/grub-mkrescue-sed.sh" out-bin)
               #t)))
         (add-after 'install 'move-gui-to-separate-output
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gui (assoc-ref outputs "gui")))
               (for-each
                 (lambda (file)
                   (mkdir-p (string-append gui (dirname file)))
                   (rename-file (string-append out file)
                                (string-append gui file)))
                 (list "/bin/xorriso-tcltk"
                       "/share/info/xorriso-tcltk.info"
                       "/share/man/man1/xorriso-tcltk.1"))
               (wrap-program (string-append gui "/bin/xorriso-tcltk")
                 `("PATH" ":" prefix (,(string-append out "/bin"))))
               #t))))))
    (inputs
     (list acl readline tk zlib))
    (home-page "https://www.gnu.org/software/xorriso/")
    (synopsis "Create, manipulate, burn ISO-9660 file systems")
    (description
     "GNU Xorriso is a tool for copying files to and from ISO 9660 Rock
Ridge, a.k.a. Compact Disc File System, file systems and it allows
session-wise manipulation of them.  It features a formatter and burner for
CD, DVD and BD.  It can operate on existing ISO images or it can create new
ones.  xorriso can then be used to copy files directly into or out of ISO
files.")
    (license gpl3+)))

(define-public cdparanoia
  (package
    (name "cdparanoia")
    (version "10.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://downloads.xiph.org/releases/"
                                 "cdparanoia/cdparanoia-III-"
                                 version ".src.tgz"))
             (sha256
              (base32
               "1pv4zrajm46za0f6lv162iqffih57a8ly4pc69f7y0gfyigb8p80"))
             (patches (search-patches "cdparanoia-fpic.patch"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 ;; Make libraries respect LDFLAGS.
                 (substitute* '("paranoia/Makefile.in" "interface/Makefile.in")
                   (("-Wl,-soname") "$(LDFLAGS) -Wl,-soname"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there is no check target
       #:parallel-build? #f             ;randomly fails to link
       #:configure-flags ; Add $libdir to the RUNPATH of all the executables.
       (list (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       ;; Building in parallel is flaky: “ld: […]/cachetest.c:393: undefined
       ;; reference to `paranoia_free'”.
       #:parallel-build? #f))
    (home-page "https://www.xiph.org/paranoia/")
    (synopsis "Audio CD reading utility")
    (description "Cdparanoia retrieves audio tracks from CDDA capable CDROM
drives.  The data can be saved to a file or directed to standard output
in WAV, AIFF, AIFF-C or raw format.  Most ATAPI, SCSI and several
proprietary CDROM drive makes are supported; cdparanoia can determine if the
target drive is CDDA capable.  In addition to simple reading, cdparanoia adds
extra-robust data verification, synchronization, error handling and scratch
reconstruction capability.")
    (license gpl2))) ; libraries under lgpl2.1

(define-public cdrdao
  (package
    (name "cdrdao")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cdrdao/cdrdao")
             (commit
              (string-append "rel_" (string-replace-substring version "." "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gcl8ibyylamy2d1piq3749nw3xrlp12r0spzp2gmni57b8a6b7j"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list
        ;; GCDMaster depends on obsolete libgnomeuimm, see
        ;; <https://github.com/cdrdao/cdrdao/issues/3>.
        "--without-gcdmaster"
        ;; Use the native SCSI interface.
        "--without-scglib")
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'fix-configure.ac
           (lambda _
             ;; Remove reference to missing macro.
             (substitute* "configure.ac" (("^AM_GCONF_SOURCE_2.*") ""))
             #t)))))
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list ao lame libmad libvorbis))
    (home-page "http://cdrdao.sourceforge.net")
    (synopsis "Read and write CDs in disk-at-once mode")
    (description "cdrdao records audio or data CDs in disk-at-once (DAO) mode,
based on a textual description of the contents.  This mode writes the complete
disc – lead-in, one or more tracks, and lead-out – in a single step and is
commonly used with audio CDs.  @code{cdrdao} can also handle the bin/cue
format, commonly used for VCDs or disks with subchannel data.")
    (license gpl2+)))

(define-public cdrtools
  (package
    (name "cdrtools")
    (version "3.01")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/cdrtools/cdrtools-" version ".tar.bz2"))
              (sha256
               (base32
                "03w6ypsmwwy4d7vh6zgwpc60v541vc5ywp8bdb758hbc4yv2wa7d"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; By default 'cdda2wav --help' would print a string like
                  ;; "Version 3.01_linux_4.19.10-gnu_x86_64_x86_64".  Change
                  ;; it to not capture the kernel version of the build
                  ;; machine, to allow for reproducible builds.
                  (substitute* "cdda2wav/local.cnf.in"
                    (("^VERSION_OS=.*")
                     (string-append
                      "actual_os := $(shell uname -o)\n"
                      "actual_arch := $(shell uname -m)\n"
                      "VERSION_OS = _$(actual_os)_$(actual_arch)\n")))
                  #t))
              (patches (search-patches "cdrtools-3.01-mkisofs-isoinfo.patch"))))
    (build-system gnu-build-system)
    ;; XXX cdrtools bundles a modified, relicensed early version of cdparanoia.
    (arguments
     `(#:make-flags
       (list "RM=rm" "LN=ln" "SYMLINK=ln -s"
             "CONFIG_SHELL=sh" "CCOM=gcc"
             (string-append "INS_BASE=" (assoc-ref %outputs "out"))
             (string-append "INS_RBASE=" (assoc-ref %outputs "out")))
       ;; Parallel builds appear to be unsafe, see
       ;; https://hydra.gnu.org/build/3346840/log/raw
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'set-linux-headers
           (lambda _
             (substitute* "autoconf/configure"
               (("/usr/src/linux")
                (assoc-ref %build-inputs "kernel-headers")))
             #t))
         (add-before 'build 'substitute-dirs
           (lambda _
             (substitute* (append (find-files "DEFAULTS" "^Defaults\\.")
                                  (find-files "DEFAULTS_ENG" "^Defaults\\.")
                                  (find-files "TEMPLATES" "^Defaults\\."))
               (("/opt/schily") (assoc-ref %outputs "out")))
             #t)))
       #:tests? #f))  ; no tests
   (synopsis "Command line utilities to manipulate and burn CD/DVD/BD images")
   (description "cdrtools is a collection of command line utilities to create
CD's, DVD's or Blue Ray discs.  The most important components are
@command{cdrecord}, a burning program, @command{cdda2wav}, a CD audio ripper
which uses libparanoia, and @command{mkisofs}, which can create various disc
images.")
   (home-page "http://cdrtools.sourceforge.net/private/cdrecord.html")

   ;; mkisofs is GPL, the other programs are CDDL.
   (license (list cddl1.0 gpl2))))

(define-public dvd+rw-tools
  (package
    (name "dvd+rw-tools")
    (version "7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://fy.chalmers.se/~appro/linux/DVD+RW/tools/dvd+rw-tools-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1jkjvvnjcyxpql97xjjx0kwvy70kxpiznr2zpjy2hhci5s10zmpq"))
              (patches (search-patches "dvd+rw-tools-add-include.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list cdrtools))
    (native-inputs
     (list m4))
    (arguments
     `(#:tests? #f ; No tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-glibc-compatability
           (lambda* (#:key inputs #:allow-other-keys)
             ;; We use sed --in-place because substitute* cannot handle the
             ;; character encoding used by growisofs.c.
             (invoke "sed" "-i" "-e"
                     (string-append
                       "s,<sys/stat.h>,"
                       "<sys/stat.h>\\\n#include <sys/sysmacros.h>,")
                     "growisofs.c")))
         (replace 'configure
           (lambda _ (setenv "prefix" (assoc-ref %outputs "out")) #t))
         (add-before 'build 'embed-mkisofs
           (lambda* (#:key inputs #:allow-other-keys)
             ;; We use sed --in-place because substitute* cannot handle the
             ;; character encoding used by growisofs.c.
             (invoke "sed" "-i" "-e"
                     (string-append
                       "s,\"mkisofs\","
                       "\"" (which "mkisofs") "\",")
                     "growisofs.c"))))))
    (home-page "http://fy.chalmers.se/~appro/linux/DVD+RW/")
    (synopsis "DVD and Blu-ray Disc burning tools")
    (description "dvd+rw-tools, mostly known for its command
@command{growisofs}, is a collection of DVD and Blu-ray Disc burning tools.
It requires another program, such as @command{mkisofs}, @command{genisoimage},
or @command{xorrisofs} to create ISO 9660 images.")
    (license gpl2)))

(define-public dvdisaster
  (package
    (name "dvdisaster")
    (version "0.79.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dvdisaster.jcea.es/downloads/dvdisaster-"
                           version ".tar.bz2"))
       (sha256
        (base32 "1hz3fvqfdrwb7dn6ggqkpcgyjag37ivm1layw27ncjz9glklxjbr"))))
    (build-system gnu-build-system)
    (inputs
     (list gtk+-2))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (arguments
     `(;; Parallel builds appear to be unsafe, see
       ;; <http://hydra.gnu.org/build/49331/nixlog/1/raw>.
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (with-directory-excursion "regtest"
               (substitute* "common.bash"
                 (("ISODIR=/var/tmp/regtest") "ISODIR=/tmp"))
               (for-each invoke (find-files "." "rs.*\\.bash")))))
         (add-after 'install 'install-desktop
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((datadir (string-append (assoc-ref outputs "out") "/share")))
               (substitute* "contrib/dvdisaster.desktop"
                 (("dvdisaster48.png") "dvdisaster.png"))
               (install-file "contrib/dvdisaster.desktop"
                             (string-append datadir "/applications"))
               (for-each
                (lambda (png)
                  (let* ((size (substring png
                                          (string-index png char-set:digit)
                                          (string-rindex png #\.)))
                         (icondir (string-append datadir "/icons/"
                                                 size "x" size "/apps")))
                    (mkdir-p icondir)
                    (copy-file png (string-append icondir "/dvdisaster.png"))))
                (find-files "contrib" "dvdisaster[0-9]*\\.png"))
               (mkdir-p (string-append datadir "/pixmaps"))
               (copy-file "contrib/dvdisaster48.xpm"
                          (string-append datadir "/pixmaps/dvdisaster.xpm")))))
         (add-after 'install 'remove-uninstall-script
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (delete-file
                (string-append out "/bin/dvdisaster-uninstall.sh"))))))))
    (home-page "https://dvdisaster.jcea.es/")
    (synopsis "Error correcting codes for optical media images")
    (description "Optical media (CD,DVD,BD) keep their data only for a
finite time (typically for many years).  After that time, data loss develops
slowly with read errors growing from the outer media region towards the
inside.

Dvdisaster stores data on CD/DVD/BD (supported media) in a way that it is
fully recoverable even after some read errors have developed.  This enables
you to rescue the complete data to a new medium.

Data loss is prevented by using error correcting codes.  Error correction
data is either added to the medium or kept in separate error correction
files.  Dvdisaster works at the image level so that the recovery does not
depend on the file system of the medium.  The maximum error correction
capacity is user-selectable.")
    (license gpl2+)))

(define-public dvdstyler
  (package
    (name "dvdstyler")
    (version "3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/dvdstyler/dvdstyler/"
                            version "/DVDStyler-" version ".tar.bz2"))
       (sha256
        (base32
         "0lwc0hn94m9r8fi07sjqz3fr618l6lnw3zsakxw7nlgnxbjsk7pi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "XMLTO="
                            (assoc-ref %build-inputs "xmlto")
                            "/bin/xmlto --searchpath "
                            (assoc-ref %build-inputs "docbook-xsl")
                            "/xml/xsl/docbook-xsl-" ,(package-version docbook-xsl)
                            "/htmlhelp:"
                            (assoc-ref %build-inputs "docbook-xml")
                            "/xml/dtd/docbook"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out") "/bin/dvdstyler")
               `("PATH" ":" prefix
                 (,(string-join
                    (map (lambda (in) (string-append (assoc-ref inputs in) "/bin"))
                         '("cdrtools" "dvdauthor" "dvd+rw-tools" "ffmpeg"))
                    ":"))))
             #t)))
       #:tests? #f)) ; No tests.
    (inputs ; TODO package bundled wxvillalib
     `(("wxwidgets" ,wxwidgets-3.1)
       ("wssvg" ,wxsvg)
       ("dbus" ,dbus)
       ("cdrtools" ,cdrtools)
       ("dvd+rw-tools" ,dvd+rw-tools)
       ("dvdauthor" ,dvdauthor)
       ("eudev" ,eudev)
       ("fontconfig" ,fontconfig)
       ("libexif" ,libexif)
       ("libjpeg" ,libjpeg-turbo)
       ("ffmpeg" ,ffmpeg-3.4)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("flex" ,flex)
       ("python" ,python-2)
       ("xmlto" ,xmlto)
       ("gettext" ,gettext-minimal)
       ("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("zip" ,zip)))
    (synopsis "DVD authoring application")
    (description "DVDStyler is a DVD authoring application which allows users
to burn video files in many formats to DVD discs, complete with individually
designed menus.  It can be used to create professional-looking DVD's with
custom buttons, backgrounds and animations, from within a user-friendly
graphical interface.")
    (home-page "https://www.dvdstyler.org")
    (license gpl2)))

(define-public libcue
  (package
    (name "libcue")
    (version "2.2.1")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/lipnitsk/libcue")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1iqw4n01rv2jyk9lksagyxj8ml0kcfwk67n79zy1r6zv1xfp5ywm"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
    (native-inputs
     (list bison flex))
    (home-page "https://github.com/lipnitsk/libcue")
    (synopsis "C library to parse cue sheets")
    (description "Libcue is a C library to parse so-called @dfn{cue sheets}
which contain meta-data for CD/DVD tracks.  It provides an API to manipulate
the data.")
    (license gpl2+)))

(define-public cd-discid
  (package
    (name "cd-discid")
    (version "1.4")
    (home-page "http://linukz.org/cd-discid.shtml")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://linukz.org/download/cd-discid-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0qrcvn7227qaayjcd5rm7z0k5q89qfy5qkdgwr5pd7ih0va8rmpz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Makefile"
                    (("/usr/bin/install")
                     "install"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases (delete 'configure))
       #:make-flags (list ,(string-append "CC=" (cc-for-target))
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))))
    (synopsis "Get CDDB discid information from an audio CD")
    (description
     "cd-discid is a command-line tool to retrieve CDDB discid information
from an audio CD.")
    (license gpl2+)))

(define-public abcde
  (package
    (name "abcde")
    (version "2.9.3")
    (home-page "https://abcde.einval.com/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/download/abcde-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "091ip2iwb6b67bhjsj05l0sxyq2whqjycbzqpkfbpm4dlyxx0v04"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Makefile"
                    (("/usr/bin/install")
                     "install"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("^prefix = .*$")
                (string-append "prefix = "
                               (assoc-ref outputs "out")
                               "\n"))
               (("^sysconfdir = .*$")
                (string-append "sysconfdir = "
                               (assoc-ref outputs "out")
                               "/etc/\n")))
             #t))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((wget   (assoc-ref inputs "wget"))
                   (vorbis (assoc-ref inputs "vorbis-tools"))
                   (parano (assoc-ref inputs "cdparanoia"))
                   (which  (assoc-ref inputs "which"))
                   (discid (assoc-ref inputs "cd-discid"))
                   (perl-discid (assoc-ref inputs "perl-musicbrainz-discid"))
                   (perl-ws (assoc-ref inputs "perl-webservice-musicbrainz"))
                   (perl-mojo (assoc-ref inputs "perl-mojolicious"))
                   (flac   (assoc-ref inputs "flac"))
                   (out    (assoc-ref outputs "out")))
               (define (wrap file)
                 (wrap-program file
                               `("PATH" ":" prefix
                                 (,(string-append out "/bin:"
                                                  wget "/bin:"
                                                  flac "/bin:"
                                                  which "/bin:"
                                                  vorbis "/bin:"
                                                  discid "/bin:"
                                                  parano "/bin")))
                               `("PERL5LIB" ":" prefix
                                 (,(string-append perl-discid
                                                  "/lib/perl5/site_perl:"
                                                  perl-ws
                                                  "/lib/perl5/site_perl:"
                                                  perl-mojo
                                                  "/lib/perl5/site_perl")))))

               (for-each wrap
                         (find-files (string-append out "/bin")
                                     ".*")))
             #t)))
       #:tests? #f)) ; no test target

    (inputs (list wget
                  which
                  cdparanoia
                  cd-discid
                  vorbis-tools
                  flac
                  perl-musicbrainz-discid
                  perl-webservice-musicbrainz
                  perl-mojolicious ;indirect dependency
                  ;; A couple of Python and Perl scripts are included.
                  python
                  perl))

    (synopsis "Command-line audio CD ripper")
    (description
     "abcde is a front-end command-line utility (actually, a shell script)
that grabs tracks off a CD, encodes them to Ogg/Vorbis, MP3, FLAC, Ogg/Speex
and/or MPP/MP+ (Musepack) format, and tags them, all in one go.")
    (license gpl2+)))

(define-public geteltorito
  (package
    (name "geteltorito")
    (version "0.6")
    (home-page
     "https://userpages.uni-koblenz.de/~krienke/ftp/noarch/geteltorito/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gkbm9ahj2mgqrkrfpibzclsriqgsbsvjh19fr815vpd9f6snkxv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "geteltorito"
                             (string-append out "/bin"))))))))
    (inputs (list perl))
    (synopsis "Extract the boot image from a CD-ROM")
    (description
     "@command{geteltorito} can extract the initial/default boot
image from CDs (and ISOs) that follow the El Torito specification
for bootable CD-ROMs.

Image data is written to standard output by default and all other
information is written to standard error.")
    (license gpl2+)))

(define-public asunder
  (package
    (name "asunder")
    (version "2.9.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "http://www.littlesvr.ca/asunder/releases/asunder-"
                       version ".tar.bz2"))
       (sha256
        (base32 "1x3l308ss0iqhz90qyjb94gyd8b4piyrm2nzjmg5kf049k9prjf1"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:out-of-source? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((program (string-append (assoc-ref outputs "out")
                                                    "/bin/asunder")))
                        (define (bin-directory input-name)
                          (string-append (assoc-ref inputs input-name) "/bin"))
                        (wrap-program program
                          `("PATH" ":" prefix
                            ,(map bin-directory (list "cdparanoia"
                                                      "lame"
                                                      "vorbis-tools"
                                                      "flac"
                                                      "opus-tools"
                                                      "wavpack"))))))))))
    (native-inputs (list intltool pkg-config))
    ;; TODO: Add the necessary packages for Musepack encoding.
    (inputs `(("gtk+-2" ,gtk+-2)
              ("glib" ,glib)
              ("libcddb" ,libcddb)
              ("cdparanoia" ,cdparanoia)
              ("lame" ,lame)
              ("vorbis-tools" ,vorbis-tools)
              ("flac" ,flac)
              ("opus-tools" ,opus-tools)
              ("wavpack" ,wavpack)))
    (home-page "http://www.littlesvr.ca/asunder/")
    (synopsis "Graphical audio CD ripper and encoder")
    (description
     "Asunder is a graphical audio CD ripper and encoder.  It can save audio
tracks as WAV, MP3, Ogg Vorbis, FLAC, Opus, Wavpack, and Musepack.  It can use
CDDB to name and tag each track automatically, and it allows for each track to
be by a different artist.  Asunder can encode to multiple formats in one
session, and it can create M3U playlists.")
    (license gpl2)))

(define-public ripit
  (package
    (name "ripit")
    (version "3.9.0")
    (source
     (origin
       (method url-fetch)
       ;; The original suwald.com domain has expired.
       (uri (list
             (string-append "https://web.archive.org/web/20160327050927/"
                            "http://suwald.com/ripit/ripit-" version ".tar.gz")
             (string-append "https://ponce.cc/slackware/sources/repo/ripit-"
                            version ".tar.gz")))
       (sha256
        (base32 "0ap71x477jy9c4jiqazb3y45hxdxm3jbq24x05g3vjyqzigi4x1b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-usr-bin-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr/bin/install") (string-append
                                      (assoc-ref inputs "coreutils")
                                      "/bin/install"))
               (("\\$\\(DESTDIR\\)/usr/local") (assoc-ref outputs "out"))
               (("../../etc") "etc")))))))
    (native-inputs
     (list coreutils))
    (inputs
     (list perl))
    (propagated-inputs
     (list cdparanoia flac vorbis-tools wavpack perl-cddb-get))
    (home-page (string-append "https://web.archive.org/web/20170119092156/"
                              "http://www.suwald.com/ripit/about.php"))
    (synopsis "Command-line program to extract audio CDs")
    (description "RipIT is used to extract audio from CDs.")
    (license gpl2)))

(define-public ccd2cue
  (package
    (name "ccd2cue")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://gnu/ccd2cue/ccd2cue-" version
             ".tar.gz"))
       (sha256
        (base32
         "1icrkg25hwx4gsn3dski2172ia4ywjh8m1sa17zmjclnrgdwy9c7"))))
    (build-system gnu-build-system)
    (synopsis "CCD to CUE sheet conversion")
    (description
     "GNU ccd2cue is a preprocessor for CD burning software that allows
the conversion of the proprietary CCD format to the CUE format, which
is well-supported by free software.  These files are commonly
distributed with CD images and are used to describe how tracks are
laid out on the image.")
    (home-page "https://www.gnu.org/software/ccd2cue/")
    (license gpl3+)))

(define-public libburn
  (package
    (name "libburn")
    (version "1.5.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://files.libburnia-project.org/releases/"
                                 "libburn-" version ".tar.gz"))
             (sha256
              (base32
               "0m1vyry6pi115nysfgb0cg313qqhnlxqdg7f920wpiar0z8mjl2j"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (home-page "https://dev.lovelyhq.com/libburnia/libburn")
    (synopsis "Library for reading and writing optical discs")
    (description
     "Libburn is a library for reading and writing optical discs.
Supported media are: CD-R, CD-RW, DVD-RAM, DVD+RW, DVD+R, DVD+R/DL,
DVD-RW, DVD-R, DVD-R/DL, BD-R, and BD-RE.")
    (license gpl2)))

(define-public libisofs
  (package
    (name "libisofs")
    (version "1.5.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://files.libburnia-project.org/releases/"
                                 "libisofs-" version ".tar.gz"))
             (sha256
              (base32
               "13m82l13cb5d7ca53dv3akma1jr9gw0hnnshdwqpj6ahly0fv85a"))))
    (build-system gnu-build-system)
    (inputs
     (list zlib acl))
    (native-inputs
     (list pkg-config))
    (home-page "https://dev.lovelyhq.com/libburnia/libisofs")
    (synopsis "Library to create ISO 9660 images")
    (description
     "Libisofs creates ISO 9660 (also known as ECMA-119) file system images
which can either be written to POSIX file objects or handed over to
libburn for being written directly to optical media.
It can read metadata of ISO 9660 filesystems, manipulate them, and use them
to produce new complete file system images or add-on images to be appended
to the read file system image.
Supported extensions to ISO 9660 are Rock Ridge, Joliet, AAIP, zisofs.")
    (license gpl2+)))

(define-public cdrkit-libre
  (package
    (name "cdrkit-libre")
    (version "1.1.11")
    (source (origin
              (method url-fetch)
              ;; cdrkit.org is dead.
              ;;
              ;; ‘cdrkit-libre’ removes a couple of problematic files,
              ;; see <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32165#14>.
              (uri (string-append
                    "https://repo.parabola.nu/other/cdrkit-libre/cdrkit-libre-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0g2zyzb56czh429qy87lvaddzjnlcq8c616ddxsmsshz3clhyzrh"))
              (patches (search-patches "cdrkit-libre-cross-compile.patch"))
              (modules '((guix build utils)))
              (snippet
                #~(begin
                    ;; Fix building with gcc-10.
                    (substitute* "genisoimage/genisoimage.h"
                      (("char\t\t\\*outfile")
                       "extern char\t*outfile"))))))
    (build-system cmake-build-system)
    (inputs
     (list bzip2 libcap perl zlib))
    (arguments
     `(#:tests? #f ;no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'old-cdrecord
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion (string-append (assoc-ref outputs "out")
                                                      "/bin")
               (symlink "genisoimage" "mkisofs")
               (symlink "wodim" "cdrecord"))
             #t)))))
    (home-page "https://repo.parabola.nu/other/cdrkit-libre/")
    (synopsis "Command-line CD/DVD recorder")
    (description "Cdrkit is a suite of programs for recording CDs and DVDs,
blanking CD-RW media, creating ISO-9660 file system images, extracting audio
CD data, and more.  It's mostly compatible with @code{cdrtools}.")
    (license gpl2+)))

(define-public libmirage
  (package
    (name "libmirage")
    (version "3.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/cdemu/libmirage/libmirage-"
                    version ".tar.xz"))
              (sha256
               (base32
                "19pjdmxhzl8y3brhg8fsv99b6jg4lfnl8jvcjgm4jmqrr684czr5"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list glib))
    (arguments
     ;; No tests.
     '(#:tests? #f))
    (home-page "https://cdemu.sourceforge.io/")
    (synopsis "CD-ROM image access library")
    (description "libMirage is a CD-ROM image access library.  It supports the
following formats: B6T, C2D, CCD, CDI, CIF, CUE, ISO, MDS, MDX, NRG, TOC.  It
is written in C and based on GLib.  Its aim is to provide uniform access to
the data stored in various image formats.")
    (license gpl2+)))

(define-public cdemu-daemon
  (package
    (name "cdemu-daemon")
    (version "3.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/cdemu/cdemu-daemon/"
                    "cdemu-daemon-" version ".tar.xz"))
              (sha256
               (base32
                "13vxhl7ik3h5qnfh6m0zxywb8qzx1n46akrm6rp19ikmxzih9r56"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list libmirage glib ao))
    (arguments
     ;; No tests.
     '(#:tests? #f))
    (home-page "https://cdemu.sourceforge.io/")
    (synopsis "CD/DVD-ROM device emulator")
    (description "CDemu is a software suite designed to emulate an optical
drive and disc (including CD-ROMs and DVD-ROMs).")
    (license gpl2+)))

(define-public cdemu-client
  (package
    (name "cdemu-client")
    (version "3.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/cdemu/cdemu-client/cdemu-client-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1prrdhv0ia0axc6b73crszqzh802wlkihz6d100yvg7wbgmqabd7"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list python python-pygobject cdemu-daemon))
    (arguments
     ;; No tests.
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'patch-shebang
           (lambda* (#:key outputs #:allow-other-keys)
             (patch-shebang (string-append (assoc-ref outputs "out")
                                           "/bin/cdemu"))
             #t))
         (add-after 'patch-shebang 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/cdemu")))
               (wrap-program prog
                 `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH"))))
               #t))))))
    (home-page "https://cdemu.sourceforge.io/")
    (synopsis "Command-line client for controlling cdemu-daemon")
    (description "CDEmu client is a simple command-line client for controlling
CDEmu daemon.

It provides a way to perform the key tasks related to controlling the CDEmu
daemon, such as loading and unloading devices, displaying devices' status and
retrieving/setting devices' debug masks.")
    (license gpl2+)))
