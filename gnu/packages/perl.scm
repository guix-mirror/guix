;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2019, 2020 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2017, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2019, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Jochem Raat <jchmrt@riseup.net>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016 Alex Sassmannshausen <alex@pompo.co>
;;; Copyright © 2016, 2018, 2020 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Raoul J.P. Bonnal <ilpuccio.febo@gmail.com>
;;; Copyright © 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017, 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Stephen J. Scheck <sscheck@cpan.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages perl)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages image)
  #:use-module (gnu packages less)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages perl-maths)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;


(define-public perl
  ;; Yeah, Perl...  It is required early in the bootstrap process by Linux.
  (package
    (name "perl")
    (version "5.30.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://cpan/src/5.0/perl-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "128nfdxcvxfn5kq55qcfrx2851ys8hv794dcdxbyny8rm7w7vnv6"))
             (patches (search-patches
                       "perl-no-sys-dirs.patch"
                       "perl-autosplit-default-time.patch"
                       "perl-deterministic-ordering.patch"
                       "perl-reproducible-build-date.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (let ((out  (assoc-ref %outputs "out"))
             (libc (assoc-ref %build-inputs "libc")))
         (list
          (string-append "-Dprefix=" out)
          (string-append "-Dman1dir=" out "/share/man/man1")
          (string-append "-Dman3dir=" out "/share/man/man3")
          "-de" "-Dcc=gcc"
          "-Uinstallusrbinperl"
          "-Dinstallstyle=lib/perl5"
          "-Duseshrplib"
          (string-append "-Dlocincpth=" libc "/include")
          (string-append "-Dloclibpth=" libc "/lib")
          "-Dusethreads"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'setup-configure
           (lambda _
             ;; Use the right path for `pwd'.
             ;; TODO: use coreutils from INPUTS instead of 'which'
             ;; in next rebuild cycle, see fixup below.
             (substitute* "dist/PathTools/Cwd.pm"
               (("/bin/pwd")
                (which "pwd")))

             ;; Build in GNU89 mode to tolerate C++-style comment in libc's
             ;; <bits/string3.h>.
             (substitute* "cflags.SH"
               (("-std=c89")
                "-std=gnu89"))
             #t))
         ,@(if (%current-target-system)
               `((add-after 'unpack 'unpack-cross
                   (lambda* (#:key native-inputs inputs #:allow-other-keys)
                     (let ((cross-checkout
                            (assoc-ref native-inputs "perl-cross"))
                           (cross-patch
                            (assoc-ref native-inputs "perl-cross-patch")))
                       (rename-file "Artistic" "Artistic.perl")
                       (rename-file "Copying" "Copying.perl")
                       (copy-recursively cross-checkout ".")
                       (format #t "Applying ~a\n" cross-patch)
                       (invoke "patch" "-p1" "-i" cross-patch))
                     (let ((bash (assoc-ref inputs "bash")))
                       (substitute* '("Makefile.config.SH"
                                      "cnf/config.guess"
                                      "cnf/config.sub"
                                      "cnf/configure"
                                      "cnf/configure_misc.sh"
                                      "miniperl_top")
                         (("! */bin/sh") (string-append "! " bash "/bin/bash"))
                         ((" /bin/sh") (string-append bash "/bin/bash")))
                       (substitute* '("ext/Errno/Errno_pm.PL")
                         (("\\$cpp < errno.c") "$Config{cc} -E errno.c")))
                       #t))
                 (replace 'configure
                   (lambda* (#:key configure-flags outputs inputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (store-directory (%store-directory))
                            (configure-flags
                             (cons*
                              ;; `perl-cross' confuses target and host
                              (string-append "--target=" ,(%current-target-system))
                              (string-append "--prefix=" out)
                              (string-append "-Dcc=" ,(%current-target-system) "-gcc")
                              "-Dbyteorder=1234"
                              (filter (negate
                                       (lambda (x) (or (string-prefix? "-d" x)
                                                       (string-prefix? "-Dcc=" x))))
                                      configure-flags)))
                            (bash (assoc-ref inputs "bash"))
                            (coreutils (assoc-ref inputs "coreutils")))
                       (format (current-error-port)
                               "running ./configure ~a\n" (string-join configure-flags))
                       (apply invoke (cons "./configure" configure-flags))
                       (substitute* "config.sh"
                         (((string-append store-directory "/[^/]*-bash-[^/]*"))
                          bash))
                       (substitute* '("config.h")
                         (("^#define SH_PATH .*")
                          (string-append  "#define SH_PATH \"" bash "/bin/bash\"\n")))
                       ;;TODO: fix this in setup-configure next rebuild cycle
                       (substitute* "dist/PathTools/Cwd.pm"
                         (((string-append store-directory "/[^/]*-coreutils-[^/]*"))
                          coreutils))
                       #t)))
                 (add-after 'build 'touch-non-built-files-for-install
                   (lambda _
                     ;; `make install' wants to install these although they do
                     ;; not get built...
                     (with-directory-excursion "cpan"
                       (mkdir-p "Pod-Usage/blib/script")
                       (mkdir-p "Pod-Parser/blib/script")
                       (for-each (lambda (file)
                                   (call-with-output-file file
                                     (lambda (port) (display "" port))))
                                 '("Pod-Usage/blib/script/pod2text"
                                   "Pod-Usage/blib/script/pod2usage"
                                   "Pod-Checker/blib/script/podchecker"
                                   "Pod-Parser/blib/script/podselect")))
                     #t)))
               `((replace 'configure
                   (lambda* (#:key configure-flags #:allow-other-keys)
                     (format #t "Perl configure flags: ~s~%" configure-flags)
                     (apply invoke "./Configure" configure-flags)))))
         (add-after 'install 'remove-extra-references
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (libc    (assoc-ref inputs
                                        ,(if (%current-target-system)
                                             "cross-libc" "libc")))
                    (config1 (car (find-files (string-append out "/lib/perl5")
                                              "^Config_heavy\\.pl$")))
                    (config2 (find-files (string-append out "/lib/perl5")
                                         "^Config\\.pm$")))
               ;; Force the library search path to contain only libc because
               ;; it is recorded in Config.pm and Config_heavy.pl; we don't
               ;; want to keep a reference to everything that's in
               ;; $LIBRARY_PATH at build time (GCC, Binutils, bzip2, file,
               ;; etc.)
               (substitute* config1
                 (("^incpth=.*$")
                  (string-append "incpth='" libc "/include'\n"))
                 (("^(libpth|plibpth|libspath)=.*$" _ variable)
                  (string-append variable "='" libc "/lib'\n")))

               (for-each (lambda (file)
                           (substitute* config2
                             (("libpth => .*$")
                              (string-append "libpth => '" libc
                                             "/lib',\n"))))
                         config2)
               #t))))))
    (inputs
     (if (%current-target-system)
         `(("bash" ,bash-minimal)
           ("coreutils" ,coreutils))
         '()))
    (native-inputs
     (if (%current-target-system)
         `(("perl-cross"
            ,(origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/arsv/perl-cross")
                     (commit "1.3.3")))
               (file-name (git-file-name "perl-cross" "1.3.3"))
               (sha256
                (base32 "065qbl1x44maykaj8p8za0b6qxj74bz7fi2zsrlydir1mqb1js3d"))))
           ("perl-cross-patch" ,@(search-patches "perl-cross.patch")))
         '()))
    (native-search-paths (list (search-path-specification
                                (variable "PERL5LIB")
                                (files '("lib/perl5/site_perl")))))
    (synopsis "Implementation of the Perl programming language")
    (description
     "Perl is a general-purpose programming language originally developed for
text manipulation and now used for a wide range of tasks including system
administration, web development, network programming, GUI development, and
more.")
    (home-page "https://www.perl.org/")
    (license license:gpl1+)))                          ; or "Artistic"

(define-public perl-algorithm-c3
  (package
    (name "perl-algorithm-c3")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Algorithm-C3-" version ".tar.gz"))
       (sha256
        (base32 "02ck52cf0yyk57354rd1rp5l0kbfwi1pvg2lh3jadvjxfrkq9x5a"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Algorithm-C3")
    (synopsis "Module for merging hierarchies using the C3 algorithm")
    (description "This module implements the C3 algorithm, which aims to
provide a sane method resolution order under multiple inheritance.")
    (license (package-license perl))))

(define-public perl-algorithm-diff
  (package
    (name "perl-algorithm-diff")
    (version "1.1903")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TY/TYEMQ/"
                           "Algorithm-Diff-" version ".tar.gz"))
       (sha256
        (base32
         "0l8pk7ziz72d022hsn4xldhhb9f5649j5cgpjdibch0xng24ms1h"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Algorithm-Diff")
    (synopsis "Compute differences between two files or lists")
    (description "This is a module for computing the difference between two
files, two strings, or any other two lists of things.  It uses an intelligent
algorithm similar to (or identical to) the one used by the Unix \"diff\"
program.  It is guaranteed to find the *smallest possible* set of
differences.")
    (license (package-license perl))))

(define-public perl-aliased
  (package
    (name "perl-aliased")
    (version "0.34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "aliased-" version ".tar.gz"))
       (sha256
        (base32
         "1syyqzy462501kn5ma9gl6xbmcahqcn4qpafhsmpz0nd0x2m4l63"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/aliased")
    (synopsis "Use shorter versions of class names")
    (description "The alias module loads the class you specify and exports
into your namespace a subroutine that returns the class name.  You can
explicitly alias the class to another name or, if you prefer, you can do so
implicitly.")
    (license (package-license perl))))

(define-public perl-alien-sdl
  (package
    (name "perl-alien-sdl")
    (version "1.446")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FR/FROGGS/"
                           "Alien-SDL-" version ".tar.gz"))
       (sha256
        (base32 "0ajipk43syhlmw0zinbj1i6r46vdlkr06wkx7ivqjgf6qffjran9"))))
    (build-system perl-build-system)
    (arguments
     `(#:module-build-flags
       ;; XXX: For some reason, `sdl-config' reports stand-alone SDL
       ;; directory, not SDL-union provided as an input to the
       ;; package.  We force the latter with "--prefix=" option.
       (list (let ((sdl (assoc-ref %build-inputs "sdl")))
               (string-append "--with-sdl-config=" sdl "/bin/sdl-config"
                              " --prefix=" sdl)))
       #:phases
       (modify-phases %standard-phases
         ;; Fix "unrecognized option: --with-sdl-config" during build.
         ;; Reported upstream as
         ;; <https://github.com/PerlGameDev/SDL/issues/261>.  See also
         ;; <https://github.com/PerlGameDev/SDL/issues/272>.
         (add-after 'unpack 'fix-build.pl
           (lambda _
             (substitute* "Build.PL"
               (("use Getopt::Long;") "")
               (("GetOptions\\( \"travis\" => \\\\\\$travis \\);") ""))
             #t)))))
    (native-inputs
     `(("perl-archive-extract" ,perl-archive-extract)
       ("perl-archive-zip" ,perl-archive-zip)
       ("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-file-which" ,perl-file-which)
       ("perl-module-build" ,perl-module-build)
       ("perl-text-patch" ,perl-text-patch)))
    (inputs
     `(("freetype" ,freetype)
       ("fontconfig" ,fontconfig)
       ("pango" ,pango)
       ("sdl" ,(sdl-union
                (list sdl sdl-gfx sdl-image sdl-mixer sdl-net sdl-ttf
                      sdl-pango)))
       ("zlib" ,zlib)))
    (home-page "https://metacpan.org/release/Alien-SDL")
    (synopsis "Get, build and use SDL libraries")
    (description
     "Alien::SDL can be used to detect and get configuration settings from an
installed SDL and related libraries.  Based on your platform it offers the
possibility to download and install prebuilt binaries or to build SDL & co.@:
from source codes.")
    (license license:perl-license)))

(define-public perl-any-moose
  (package
    (name "perl-any-moose")
    (version "0.27")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                                  "Any-Moose-" version ".tar.gz"))
              (sha256
               (base32
                "0dc55mpayrixwx8dwql0vj0jalg4rlb3k64rprc84bl0z8vkx9m8"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-mouse" ,perl-mouse)
       ("perl-moose" ,perl-moose)))
    (home-page "https://metacpan.org/release/Any-Moose")
    (synopsis "Transparently use Moose or Mouse modules")
    (description
     "This module facilitates using @code{Moose} or @code{Mouse} modules
without changing the code.  By default, Mouse will be provided to libraries,
unless Moose is already loaded, or explicitly requested by the end-user.  End
users can force the decision of which backend to use by setting the environment
variable ANY_MOOSE to be Moose or Mouse.")
    (license (package-license perl))))

(define-public perl-appconfig
  (package
    (name "perl-appconfig")
    (version "1.71")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "AppConfig-" version ".tar.gz"))
       (sha256
        (base32
         "03vvi3mk4833mx2c6dkm9zhvakf02mb2b7wz9pk9xc7c4mq04xqi"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-pod" ,perl-test-pod)))
    (home-page "https://metacpan.org/release/AppConfig")
    (synopsis "Configuration files and command line parsing")
    (description "AppConfig is a bundle of Perl5 modules for reading
configuration files and parsing command line arguments.")
    (license (package-license perl))))

(define-public perl-array-utils
  (package
    (name "perl-array-utils")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/Z/ZM/ZMIJ/Array/Array-Utils-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0w1pwvnjdpb0n6k07zbknxwx6v7y75p4jxrs594pjhwvrmzippc9"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Array-Utils")
    (synopsis "Small utils for array manipulation")
    (description "@code{Array::Utils} is a small pure-perl module containing
list manipulation routines.")
    (license (package-license perl))))

(define-public perl-async-interrupt
  (package
    (name "perl-async-interrupt")
    (version "1.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/"
                                  "Async-Interrupt-" version ".tar.gz"))
              (sha256
               (base32
                "0nq8wqy0gsnwhiw23wsp1dmgzzbf2q1asi85yd0d7cmg4haxsmib"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-canary-stability" ,perl-canary-stability)))
    (propagated-inputs
     `(("perl-common-sense" ,perl-common-sense)))
    (home-page "https://metacpan.org/release/Async-Interrupt")
    (synopsis "Allow C/XS libraries to interrupt perl asynchronously")
    (description
     "@code{Async::Interrupt} implements a single feature only of interest
to advanced perl modules, namely asynchronous interruptions (think \"UNIX
signals\", which are very similar).

Sometimes, modules wish to run code asynchronously (in another thread,
or from a signal handler), and then signal the perl interpreter on
certain events.  One common way is to write some data to a pipe and use
an event handling toolkit to watch for I/O events.  Another way is to
send a signal.  Those methods are slow, and in the case of a pipe, also
not asynchronous - it won't interrupt a running perl interpreter.

This module implements asynchronous notifications that enable you to
signal running perl code from another thread, asynchronously, and
sometimes even without using a single syscall.")
    (license (package-license perl))))

(define-public perl-attribute-util
  (package
    (name "perl-attribute-util")
    (version "1.07")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://cpan.metacpan.org/authors/id/D/DA/DANKOGAI/"
                    "Attribute-Util-" version ".tar.gz"))
              (sha256
               (base32
                "1z79d845dy96lg0pxw0kr2za0gniwnpn963r7ccajfpj6k7jfw07"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/pod/Attribute::Util")
    (synopsis "Assorted general utility attributes")
    (description "This package provides various utility functions.  When used
without argument, this module provides four universally accessible attributes
of general interest as follows:
@itemize
@item Abstract
@item Alias
@item Memoize
@item Method
@item SigHandler
@end itemize")
    (license (package-license perl))))

(define-public perl-authen-dechpwd
  (package
    (name "perl-authen-dechpwd")
    (version "2.007")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Authen-DecHpwd-"
            version ".tar.gz"))
      (sha256
       (base32
        "0xzind7zr2prjq3zbs2j18snfpshd4xrd7igv4kp67xl0axr6fpl"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (propagated-inputs
     `(("perl-data-integer" ,perl-data-integer)
       ("perl-digest-crc" ,perl-digest-crc)
       ("perl-scalar-string" ,perl-scalar-string)))
    (home-page "https://metacpan.org/release/Authen-DecHpwd")
    (synopsis "DEC VMS password hashing")
    (description "@code{Authen::DecHpwd} implements the
SYS$HASH_PASSWORD password hashing function from VMS (also known as
LGI$HPWD) and some associated VMS username and password handling
functions.  The password hashing function is implemented in XS with a
pure Perl backup version for systems that cannot handle XS.")
    (license license:gpl2+)))

(define-public perl-authen-passphrase
  (package
    (name "perl-authen-passphrase")
    (version "0.008")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Authen-Passphrase-"
            version ".tar.gz"))
      (sha256
       (base32
        "0qq4krap687rxf6xr31bg5nj5dqmm1frcm7fq249v1bxc4h4bnsm"))))
  (build-system perl-build-system)
  (native-inputs
   `(("perl-module-build" ,perl-module-build)
     ("perl-test-pod" ,perl-test-pod)
     ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
  (propagated-inputs
   `(("perl-authen-dechpwd" ,perl-authen-dechpwd)
     ("perl-crypt-des" ,perl-crypt-des)
     ("perl-crypt-eksblowfish" ,perl-crypt-eksblowfish)
     ("perl-crypt-mysql" ,perl-crypt-mysql)
     ("perl-crypt-passwdmd5" ,perl-crypt-passwdmd5)
     ("perl-crypt-unixcrypt_xs" ,perl-crypt-unixcrypt_xs)
     ("perl-data-entropy" ,perl-data-entropy)
     ("perl-digest-md4" ,perl-digest-md4)
     ("perl-module-runtime" ,perl-module-runtime)
     ("perl-params-classify" ,perl-params-classify)))
  (home-page "https://metacpan.org/release/Authen-Passphrase")
  (synopsis "Hashed passwords/passphrases as objects")
  (description "@code{Authen-Passphrase} is the base class for a
system of objects that encapsulate passphrases.  An object of this
type is a passphrase recogniser; its job is to recognise whether an
offered passphrase is the right one.  For security such passphrase
recognisers usually do not themselves know the passphrase they are
looking for; they can merely recognise it when they see it.  There are
many schemes in use to achieve this effect and the intent of this
class is to provide a consistent interface to them all.  In addition
to the base class, this module also contains implementations of
several specific passphrase schemes.")
  (license license:perl-license)))

(define-public perl-autovivification
  (package
    (name "perl-autovivification")
    (version "0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/V/VP/VPIT/"
                           "autovivification-" version ".tar.gz"))
       (sha256
        (base32
         "01giacr2sx6b9bgfz6aqw7ndcnf08j8n6kwhm7880a94hmb9g69d"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/autovivification")
    (synopsis "Lexically disable autovivification")
    (description "When an undefined variable is dereferenced, it gets silently
upgraded to an array or hash reference (depending of the type of the
dereferencing).  This behaviour is called autovivification and usually does
what you mean but it may be unnatural or surprising because your variables get
populated behind your back.  This is especially true when several levels of
dereferencing are involved, in which case all levels are vivified up to the
last, or when it happens in intuitively read-only constructs like
@code{exists}.  The pragma provided by this package lets you disable
autovivification for some constructs and optionally throws a warning or an
error when it would have happened.")
    (license (package-license perl))))

(define-public perl-bareword-filehandles
  (package
    (name "perl-bareword-filehandles")
    (version "0.006")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/I/IL/ILMARI/bareword-filehandles-"
             version ".tar.gz"))
       (sha256
        (base32
         "1yxz6likpfshpyfrgwyi7dw6ig1wjhh0vnvbcs6ypr62pv00fv5d"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-b-hooks-op-check" ,perl-b-hooks-op-check)
       ("perl-extutils-depends" ,perl-extutils-depends)))
    (propagated-inputs
     `(("perl-b-hooks-op-check" ,perl-b-hooks-op-check)
       ("perl-lexical-sealrequirehints" ,perl-lexical-sealrequirehints)))
    (home-page "https://metacpan.org/release/bareword-filehandles")
    (synopsis "Disables bareword filehandles")
    (description "This module disables bareword filehandles.")
    (license (package-license perl))))

(define-public perl-base
  (deprecated-package "perl-base" perl))

(define-public perl-browser-open
  (package
    (name "perl-browser-open")
    (version "0.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CF/CFRANKS/Browser-Open-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0rv80n5ihy9vnrzsc3l7wlk8880cwabiljrydrdnxq1gg0lk3sxc"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Browser-Open")
    (synopsis "Open a browser in a given URL")
    (description "The functions exported by this module allow you to open URLs
in the user's browser.  A set of known commands per OS-name is tested for
presence, and the first one found is executed.  With an optional parameter,
all known commands are checked.")
    (license (package-license perl))))

(define-public perl-b-hooks-endofscope
  (package
    (name "perl-b-hooks-endofscope")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "B-Hooks-EndOfScope-" version ".tar.gz"))
       (sha256
        (base32
         "1imcqxp23yc80a7p0h56sja9glbrh4qyhgzljqd4g9habpz3vah3"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-module-runtime" ,perl-module-runtime)
       ("perl-module-implementation" ,perl-module-implementation)
       ("perl-sub-exporter-progressive" ,perl-sub-exporter-progressive)
       ("perl-variable-magic" ,perl-variable-magic)))
    (home-page "https://metacpan.org/release/B-Hooks-EndOfScope")
    (synopsis "Execute code after a scope finished compilation")
    (description "This module allows you to execute code when perl finished
compiling the surrounding scope.")
    (license (package-license perl))))

(define-public perl-b-hooks-op-check
  (package
    (name "perl-b-hooks-op-check")
    (version "0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/E/ET/ETHER/B-Hooks-OP-Check-"
             version ".tar.gz"))
       (sha256
        (base32
         "1kfdv25gn6yik8jrwik4ajp99gi44s6idcvyyrzhiycyynzd3df7"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-extutils-depends" ,perl-extutils-depends)))
    (home-page "https://metacpan.org/release/B-Hooks-OP-Check")
    (synopsis "Wrap OP check callbacks")
    (description "This module allows you to wrap OP check callbacks.")
    (license (package-license perl))))

(define-public perl-b-keywords
  (package
    (name "perl-b-keywords")
    (version "1.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RU/RURBAN/B-Keywords-"
                           version ".tar.gz"))
       (sha256
        (base32 "12jvx5gnypqxal4valkf9lidba9nz7kjk2wvm07q3hkmdqxw1zk0"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/B-Keywords")
    (synopsis "Lists of reserved barewords and symbol names")
    (description "@code{B::Keywords} supplies several arrays of exportable
keywords: @code{@@Scalars, @@Arrays, @@Hashes, @@Filehandles, @@Symbols,
@@Functions, @@Barewords, @@TieIOMethods, @@UNIVERSALMethods and
@@ExporterSymbols}.")
    ;; GPLv2 only
    (license license:gpl2)))

(define-public perl-benchmark-timer
  (package
    (name "perl-benchmark-timer")
    (version "0.7102")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/D/DC/DCOPPIT/"
                                  "Benchmark-Timer-" version ".tar.gz"))
              (sha256
               (base32
                "1gl9ybm9hgia3ld5s11b7bv2p2hmx5rss5hxcfy6rmbzrjcnci01"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    ;; The optional input module Statistics::PointEstimation (from
    ;; Statistics-TTest) lists no license.
    (synopsis "Benchmarking with statistical confidence")
    (description
     "The Benchmark::Timer class allows you to time portions of code
conveniently, as well as benchmark code by allowing timings of repeated
trials.  It is perfect for when you need more precise information about the
running time of portions of your code than the Benchmark module will give you,
but don't want to go all out and profile your code.")
    (home-page "https://metacpan.org/release/Benchmark-Timer")
    (license license:gpl2)))

(define-public perl-bit-vector
  (package
    (name "perl-bit-vector")
    (version "7.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/ST/STBEY/"
                           "Bit-Vector-" version ".tar.gz"))
       (sha256
        (base32
         "09m96p8c0ipgz42li2ywdgy0vxb57mb5nf59j9gw7yzc3xkslv9w"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-carp-clan" ,perl-carp-clan)))
    (home-page "https://metacpan.org/release/Bit-Vector")
    (synopsis "Bit vector library")
    (description "Bit::Vector is an efficient C library which allows you to
handle bit vectors, sets (of integers), \"big integer arithmetic\" and boolean
matrices, all of arbitrary sizes.  The package also includes an
object-oriented Perl module for accessing the C library from Perl, and
optionally features overloaded operators for maximum ease of use.  The C
library can nevertheless be used stand-alone, without Perl.")
    (license (list (package-license perl) license:lgpl2.0+))))

(define-public perl-boolean
  (package
    (name "perl-boolean")
    (version "0.46")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IN/INGY/"
                           "boolean-" version ".tar.gz"))
       (sha256
        (base32 "0shmiw8pmshnwj01cz8g94867hjf4vc1dkp61xlbz0rybh48ih4m"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/boolean")
    (synopsis "Boolean support for Perl")
    (description "This module provides basic Boolean support, by defining two
special objects: true and false.")
    (license (package-license perl))))

(define-public perl-business-isbn-data
  (package
    (name "perl-business-isbn-data")
    (version "20140910.003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/"
                           "Business-ISBN-Data-" version ".tar.gz"))
       (sha256
        (base32
         "1jc5jrjwkr6pqga7998zkgw0yrxgb5n1y7lzgddawxibkf608mn7"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Business-ISBN-Data")
    (synopsis "Data files for Business::ISBN")
    (description "This package provides a data pack for @code{Business::ISBN}.
These data are generated from the RangeMessage.xml file provided by the ISBN
Agency.")
    (license (package-license perl))))

(define-public perl-business-isbn
  (package
    (name "perl-business-isbn")
    (version "3.004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/"
                           "Business-ISBN-" version ".tar.gz"))
       (sha256
        (base32
         "07l3zfv8hagv37i3clvj5a1zc2jarr5phg80c93ks35zaz6llx9i"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-business-isbn-data" ,perl-business-isbn-data)
       ("perl-mojolicious" ,perl-mojolicious)))
    (home-page "https://metacpan.org/release/Business-ISBN")
    (synopsis "Work with International Standard Book Numbers")
    (description "This modules provides tools to deal with International
Standard Book Numbers, including ISBN-10 and ISBN-13.")
    (license license:artistic2.0)))

(define-public perl-business-issn
  (package
    (name "perl-business-issn")
    (version "1.003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/"
                           "Business-ISSN-" version ".tar.gz"))
       (sha256
        (base32
         "1lcr9dabwqssjpff97ki6w8mjhvh8kfbj3csbyy28ylk35n4awhj"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Business-ISSN")
    (synopsis "Work with International Standard Serial Numbers")
    (description "This modules provides tools to deal with International
Standard Serial Numbers.")
    (license (package-license perl))))

(define-public perl-business-ismn
  (package
    (name "perl-business-ismn")
    (version "1.201")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/"
                           "Business-ISMN-" version ".tar.gz"))
       (sha256
        (base32 "1cpcfyaz1fl6fnm076jx2jsphw147wj6aszj2yzqrgsncjhk2cja"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-tie-cycle" ,perl-tie-cycle)))
    (home-page "https://metacpan.org/release/Business-ISMN")
    (synopsis "Work with International Standard Music Numbers")
    (description "This modules provides tools to deal with International
Standard Music Numbers.")
    (license (package-license perl))))

(define-public perl-cache-cache
  (package
    (name "perl-cache-cache")
    (version "1.08")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                                  "Cache-Cache-" version ".tar.gz"))
              (sha256
               (base32
                "1s6i670dc3yb6ngvdk48y6szdk5n1f4icdcjv2vi1l2xp9fzviyj"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-digest-sha1" ,perl-digest-sha1)
       ("perl-error" ,perl-error)
       ("perl-ipc-sharelite" ,perl-ipc-sharelite)))
    (home-page "https://metacpan.org/release/Cache-Cache")
    (synopsis "Cache interface for Perl")
    (description "The Cache modules are designed to assist a developer in
persisting data for a specified period of time.  Often these modules are used
in web applications to store data locally to save repeated and redundant
expensive calls to remote machines or databases.  People have also been known
to use Cache::Cache for its straightforward interface in sharing data between
runs of an application or invocations of a CGI-style script or simply as an
easy to use abstraction of the file system or shared memory.")
    (license (package-license perl))))

(define-public perl-cache-fastmmap
  (package
    (name "perl-cache-fastmmap")
    (version "1.48")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RO/ROBM/"
                           "Cache-FastMmap-" version ".tar.gz"))
       (sha256
        (base32 "118y5lxwa092zrii7mcwnqypff7424w1dpgfkg8zlnz7h2mmnd9c"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Cache-FastMmap")
    (synopsis "Shared memory interprocess cache via mmap")
    (description "A shared memory cache through an mmap'ed file.  It's core is
written in C for performance.  It uses fcntl locking to ensure multiple
processes can safely access the cache at the same time.  It uses a basic LRU
algorithm to keep the most used entries in the cache.")
    (license (package-license perl))))

(define-public perl-capture-tiny
  (package
    (name "perl-capture-tiny")
    (version "0.48")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DA/DAGOLDEN/Capture-Tiny-"
             version ".tar.gz"))
       (sha256
        (base32
         "069yrikrrb4vqzc3hrkkfj96apsh7q0hg8lhihq97lxshwz128vc"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Capture-Tiny")
    (synopsis "Capture STDOUT and STDERR from Perl, XS or external programs")
    (description
     "Capture::Tiny provides a simple, portable way to capture almost anything
sent to STDOUT or STDERR, regardless of whether it comes from Perl, from XS
code or from an external program.  Optionally, output can be teed so that it
is captured while being passed through to the original file handles.")
    (license license:asl2.0)))

(define-public perl-canary-stability
  (package
    (name "perl-canary-stability")
    (version "2013")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/"
                                  "Canary-Stability-" version ".tar.gz"))
              (sha256
               (base32
                "1smnsx371x9zrqmylgq145991xh8561mraqfyrlbiz4mrxi1rjd5"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Canary-Stability")
    (synopsis "Check compatibility with the installed perl version")
    (description
     "This module is used by Schmorp's modules during configuration stage
to test the installed perl for compatibility with his modules.")
    (license (package-license perl))))

(define-public perl-carp
  (package
    (name "perl-carp")
    (version "1.50")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/X/XS/XSAWYERX/Carp-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1ngbpjyd9qi7n4h5r3q3qibd8by7rfiv7364jqlv4lbd3973n9zm"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Carp")
    (synopsis "Alternative warn and die for modules")
    (description "The @code{Carp} routines are useful in your own modules
because they act like @code{die()} or @code{warn()}, but with a message
which is more likely to be useful to a user of your module.  In the case
of @code{cluck}, @code{confess}, and @code{longmess} that context is a
summary of every call in the call-stack.  For a shorter message you can use
@code{carp} or @code{croak} which report the error as being from where your
module was called.  There is no guarantee that that is where the error was,
but it is a good educated guess.")
    (license (package-license perl))))

(define-public perl-carp-always
  (package
    (name "perl-carp-always")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FE/FERREIRA/Carp-Always-"
                           version ".tar.gz"))
       (sha256
        (base32 "1wb6b0qjga7kvn4p8df6k4g1pl2yzaqiln1713xidh3i454i3alq"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-base" ,perl-test-base)))
    (home-page "https://metacpan.org/release/Carp-Always")
    (synopsis "Warns and dies noisily with stack backtraces/")
    (description "This module is meant as a debugging aid.  It can be used to
make a script complain loudly with stack backtraces when @code{warn()}-ing or
@code{die()}ing.")
    (license (package-license perl))))

(define-public perl-carp-assert
  (package
    (name "perl-carp-assert")
    (version "0.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Carp-Assert-" version ".tar.gz"))
       (sha256
        (base32
         "0km5fc6r6whxh6h5yd7g1j0bi96sgk0gkda6cardicrw9qmqwkwj"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Carp-Assert")
    (synopsis "Executable comments for Perl")
    (description "Carp::Assert is intended for a purpose like the ANSI C
library assert.h.")
    (license (package-license perl))))

(define-public perl-carp-assert-more
  (package
    (name "perl-carp-assert-more")
    (version "1.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PETDANCE/"
                           "Carp-Assert-More-" version ".tar.gz"))
       (sha256
        (base32 "16jnhdjgfwymrc5fki4xlf1rlziszf9k6q0245g976124k708ac5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-carp-assert" ,perl-carp-assert)))
    (home-page "https://metacpan.org/release/Carp-Assert-More")
    (synopsis "Convenience wrappers around Carp::Assert")
    (description "Carp::Assert::More is a set of handy assertion functions for
Perl.")
    (license license:artistic2.0)))

(define-public perl-carp-clan
  (package
    (name "perl-carp-clan")
    (version "6.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Carp-Clan-" version ".tar.gz"))
       (sha256
        (base32 "0237xx3rqa72sr4vdvws9r1m453h5f25bl85mdjmmk128kir4py7"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)))
    (home-page "https://metacpan.org/release/Carp-Clan")
    (synopsis "Report errors from a \"clan\" of modules")
    (description "This module allows errors from a clan (or family) of modules
to appear to originate from the caller of the clan.  This is necessary in
cases where the clan modules are not classes derived from each other, and thus
the Carp.pm module doesn't help.")
    (license (package-license perl))))

(define-public perl-cddb-get
  (package
    (name "perl-cddb-get")
    (version "2.28")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/F/FO/FONKIE/CDDB_get-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1jfrwvfasylcafbvb0jjm94ad4v6k99a7rf5i4qwzhg4m0gvmk5x"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/CDDB_get")
    (synopsis "Read the CDDB entry for an audio CD in your drive")
    (description "This module can retrieve information from the CDDB.")
    ;; Either GPLv2 or the "Artistic" license.
    (license (list license:gpl2 license:artistic2.0))))

(define-public circos
  (package
    (name "circos")
    (version "0.69-9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://circos.ca/distribution/circos-" version ".tgz"))
              (sha256
               (base32 "1ll9yxbk0v64813np0qz6h8bc53qlnhg9y1053b57xgkxgmxgn1l"))
              (patches (list (search-patch "circos-remove-findbin.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (datapath (string-append out "/share/Circos"))
                    (error (string-append out "/share/Circos/error"))
                    (fonts (string-append out "/share/Circos/fonts"))
                    (data (string-append out "/share/Circos/data"))
                    (tiles (string-append out "/share/Circos/tiles"))
                    (etc (string-append out "/share/Circos/etc"))
                    (lib (string-append out "/lib/perl5/site_perl/"
                                        ,(package-version perl)))
                    (install-directory (lambda (source target)
                                         (mkdir-p target)
                                         (copy-recursively source target))))
               ;; Circos looks into a relative path for its configuration
               ;; files.  We need to provide an absolute path towards the
               ;; corresponding paths in the store.
               (substitute* '("bin/circos" "etc/colors_fonts_patterns.conf"
                              "etc/gddiag.conf" "etc/brewer.conf" "README")
                 (("<<include etc") (string-append "<<include " etc)))
               (substitute* '("etc/colors.conf" "etc/image.black.conf"
                              "etc/patterns.conf" "etc/image.conf")
                 (("<<include ") (string-append "<<include " etc "/")))
               (substitute* '("etc/fonts.conf" "fonts/README.fonts")
                 (("= fonts") (string-append "= " fonts)))
               (substitute* "etc/patterns.conf"
                 (("= tiles") (string-append "= " tiles)))
               (substitute* "lib/Circos/Error.pm"
                 (("error/configuration.missing.txt")
                  (string-append error "/configuration.missing.txt")))
               (substitute* "etc/housekeeping.conf"
                 (("# data_path = /home/martink/circos-tutorials ")
                  (string-append "data_path = " datapath)))
               (substitute* "lib/Circos/Configuration.pm"
                 (("my @possibilities = \\(")
                  (string-append "my @possibilities = ("
                                 "catfile( \"" datapath "\", $arg ), "
                                 "catfile( \"" etc "\", $arg ), "
                                 "catfile( \"" etc "/tracks\", $arg ), ")))
               (for-each install-directory
                         (list "error" "fonts" "data" "tiles" "etc" "lib")
                         (list error fonts data tiles etc lib))
               (install-file "bin/circos" bin)
               #t))))))
    (propagated-inputs
     `(("perl" ,perl)
       ("perl-carp" ,perl-carp)
       ("perl-clone" ,perl-clone)
       ("perl-config-general" ,perl-config-general)
       ("perl-digest-md5" ,perl-digest-md5)
       ("perl-file-temp" ,perl-file-temp)
       ("perl-font-ttf" ,perl-font-ttf)
       ("perl-gd" ,perl-gd)
       ("perl-getopt-long" ,perl-getopt-long)
       ("perl-list-allutils" ,perl-list-allutils)
       ("perl-math-bezier" ,perl-math-bezier)
       ("perl-math-round" ,perl-math-round)
       ("perl-math-vecstat" ,perl-math-vecstat)
       ("perl-memoize" ,perl-memoize)
       ("perl-number-format" ,perl-number-format)
       ("perl-params-validate" ,perl-params-validate)
       ("perl-readonly" ,perl-readonly)
       ("perl-regexp-common" ,perl-regexp-common)
       ("perl-set-intspan" ,perl-set-intspan)
       ("perl-statistics-basic" ,perl-statistics-basic)
       ("perl-svg" ,perl-svg)
       ("perl-text-balanced" ,perl-text-balanced)
       ("perl-text-format" ,perl-text-format)
       ("perl-time-hires" ,perl-time-hires)))
    (home-page "http://circos.ca/")
    (synopsis "Generation of circularly composited renditions")
    (description
     "Circos is a program for the generation of publication-quality, circularly
composited renditions of genomic data and related annotations.")
    (license license:gpl2+)))

(define-public perl-class-accessor
  (package
    (name "perl-class-accessor")
    (version "0.51")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KA/KASEI/"
                           "Class-Accessor-" version ".tar.gz"))
       (sha256
        (base32
         "07215zzr4ydf49832vn54i3gf2q5b97lydkv8j56wb2svvjs64mz"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-sub-name" ,perl-sub-name)))
    (home-page "https://metacpan.org/release/Class-Accessor")
    (synopsis "Automated accessor generation")
    (description "This module automagically generates accessors/mutators for
your class.")
    (license (package-license perl))))

(define-public perl-class-accessor-chained
  (package
    (name "perl-class-accessor-chained")
    (version "0.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Class-Accessor-Chained-" version ".tar.gz"))
       (sha256
        (base32
         "1lilrjy1s0q5hyr0888kf0ifxjyl2iyk4vxil4jsv0sgh39lkgx5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-class-accessor" ,perl-class-accessor)))
    (home-page "https://metacpan.org/release/Class-Accessor-Chained")
    (synopsis "Faster, but less expandable, chained accessors")
    (description "A chained accessor is one that always returns the object
when called with parameters (to set), and the value of the field when called
with no arguments.  This module subclasses Class::Accessor in order to provide
the same mk_accessors interface.")
    (license (package-license perl))))

(define-public perl-class-accessor-grouped
  (package
    (name "perl-class-accessor-grouped")
    (version "0.10014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Class-Accessor-Grouped-" version ".tar.gz"))
       (sha256
        (base32 "1fy48hx56n5kdn1gz66awg465qf34r0n5jam64x7zxh9zhzb1m9m"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-class-xsaccessor" ,perl-class-xsaccessor)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-sub-name" ,perl-sub-name)))
    (home-page "https://metacpan.org/release/Class-Accessor-Grouped")
    (synopsis "Build groups of accessors")
    (description "This class lets you build groups of accessors that will call
different getters and setters.")
    (license (package-license perl))))

(define-public perl-class-c3
  (package
    (name "perl-class-c3")
    (version "0.35")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Class-C3-" version ".tar.gz"))
       (sha256
        (base32 "0gp3czp6y0jxx4448kz37f7gdxq4vw514bvc0l98rk4glvqkq1c4"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-algorithm-c3" ,perl-algorithm-c3)))
    (home-page "https://metacpan.org/release//Class-C3")
    (synopsis "Pragma to use the C3 method resolution order algorithm")
    (description "This is pragma to change Perl 5's standard method resolution
order from depth-first left-to-right (a.k.a - pre-order) to the more
sophisticated C3 method resolution order.")
    (license (package-license perl))))

(define-public perl-class-c3-adopt-next
  (package
    (name "perl-class-c3-adopt-next")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Class-C3-Adopt-NEXT-" version ".tar.gz"))
       (sha256
        (base32 "1xsbydmiskpa1qbmnf6n39cb83nlb432xgkad9kfhxnvm8jn4rw5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-mro-compat" ,perl-mro-compat)))
    (home-page "https://metacpan.org/release/Class-C3-Adopt-NEXT")
    (synopsis "Drop-in replacement for NEXT")
    (description "This module is intended as a drop-in replacement for NEXT,
supporting the same interface, but using Class::C3 to do the hard work.")
    (license (package-license perl))))

(define-public perl-class-c3-componentised
  (package
    (name "perl-class-c3-componentised")
    (version "1.001002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Class-C3-Componentised-" version ".tar.gz"))
       (sha256
        (base32 "14wn1g45z3b5apqq7dcai5drk01hfyqydsd2m6hsxzhyvi3b2l9h"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-class-c3" ,perl-class-c3)
       ("perl-class-inspector" ,perl-class-inspector)
       ("perl-mro-compat" ,perl-mro-compat)))
    (home-page "https://metacpan.org/release/Class-C3-Componentised")
    (synopsis "Load mix-ins or components to your C3-based class")
    (description "This module will inject base classes to your module using
the Class::C3 method resolution order.")
    (license (package-license perl))))

(define-public perl-class-data-inheritable
  (package
    (name "perl-class-data-inheritable")
    (version "0.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TM/TMTM/"
                           "Class-Data-Inheritable-" version ".tar.gz"))
       (sha256
        (base32
         "0jpi38wy5xh6p1mg2cbyjjw76vgbccqp46685r27w8hmxb7gwrwr"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-Data-Inheritable")
    (synopsis "Inheritable, overridable class data")
    (description "Class::Data::Inheritable is for creating accessor/mutators
to class data.  That is, if you want to store something about your class as a
whole (instead of about a single object).  This data is then inherited by your
subclasses and can be overridden.")
    (license (package-license perl))))

(define-public perl-class-date
  (package
    (name "perl-class-date")
    (version "1.1.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/Y/YA/YANICK/"
                           "Class-Date-" version ".tar.gz"))
       (sha256
        (base32 "1h7dfjxkpqbfymrf1bn7699i4fx6pbv5wvvi5zszfr8sqqkax1yf"))))
    (build-system perl-build-system)
    (arguments `(#:tests? #f))          ;timezone tests in chroot
    (home-page "https://metacpan.org/release/Class-Date")
    (synopsis "Class for easy date and time manipulation")
    (description "This module provides a general-purpose date and datetime
type for perl.")
    (license (package-license perl))))

(define-public perl-class-errorhandler
  (package
    (name "perl-class-errorhandler")
    (version "0.04")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/T/TO/TOKUHIROM/"
                                  "Class-ErrorHandler-" version ".tar.gz"))
              (sha256
               (base32
                "00j5f0z4riyq7i95jww291dpmbn0hmmvkcbrh7p0p8lpqz7jsb9l"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-ErrorHandler")
    (synopsis "Base class for error handling")
    (description
     "@code{Class::ErrorHandler} provides an error-handling mechanism that is generic
enough to be used as the base class for a variety of OO classes.  Subclasses inherit
its two error-handling methods, error and errstr, to communicate error messages back
to the calling program.")
    (license (package-license perl))))

(define-public perl-class-factory-util
  (package
    (name "perl-class-factory-util")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Class-Factory-Util-" version ".tar.gz"))
       (sha256
        (base32
         "09ifd6v0c94vr20n9yr1dxgcp7hyscqq851szdip7y24bd26nlbc"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/Class-Factory-Util")
    (synopsis "Utility methods for factory classes")
    (description "This module exports methods useful for factory classes.")
    (license (package-license perl))))

(define-public perl-class-inspector
  (package
    (name "perl-class-inspector")
    (version "1.36")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PL/PLICEASE/"
                           "Class-Inspector-" version ".tar.gz"))
       (sha256
        (base32
         "0kk900bp8iq7bw5jyllfb31gvf93mmp24n4x90j7qs3jlhimsafc"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-Inspector")
    (synopsis "Get information about a class and its structure")
    (description "Class::Inspector allows you to get information about a
loaded class.")
    (license (package-license perl))))

(define-public perl-class-load
  (package
    (name "perl-class-load")
    (version "0.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Class-Load-" version ".tar.gz"))
       (sha256
        (base32 "13sz4w8kwljhfcy7yjjgrgg5hv3wccr8n3iqarhyb5sjkdvzlj1a"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-needs" ,perl-test-needs)
       ("perl-test-without-module" ,perl-test-without-module)))
    (propagated-inputs
     `(("perl-package-stash" ,perl-package-stash)
       ("perl-data-optlist" ,perl-data-optlist)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-module-implementation" ,perl-module-implementation)))
    (home-page "https://metacpan.org/release/Class-Load")
    (synopsis "Working (require \"Class::Name\") and more")
    (description "\"require EXPR\" only accepts Class/Name.pm style module
names, not Class::Name.  For that, this module provides \"load_class
'Class::Name'\".")
    (license (package-license perl))))

(define-public perl-class-load-xs
  (package
    (name "perl-class-load-xs")
    (version "0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Class-Load-XS-" version ".tar.gz"))
       (sha256
        (base32
         "1ldd4a306hjagm5v9j0gjg8y7km4v3q45bxxqmj2bzgb6vsjrhjv"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-needs" ,perl-test-needs)
       ("perl-test-without-module" ,perl-test-without-module)))
    (inputs `(("perl-class-load" ,perl-class-load)))
    (home-page "https://metacpan.org/release/Class-Load-XS")
    (synopsis "XS implementation of parts of Class::Load")
    (description "This module provides an XS implementation for portions of
Class::Load.")
    (license license:artistic2.0)))

(define-public perl-class-methodmaker
  (package
    (name "perl-class-methodmaker")
    (version "2.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SC/SCHWIGON/"
                           "class-methodmaker/Class-MethodMaker-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0a03i4k3a33qqwhykhz5k437ld5mag2vq52vvsy03gbynb65ivsy"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-MethodMaker")
    (synopsis "Create generic methods for OO Perl")
    (description "This module solves the problem of having to continually
write accessor methods for your objects that perform standard tasks.")
    (license (package-license perl))))

(define-public perl-class-method-modifiers
  (package
    (name "perl-class-method-modifiers")
    (version "2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Class-Method-Modifiers-" version ".tar.gz"))
       (sha256
        (base32 "0qzx83mgd71hlc2m1kpw15dqsjzjq7b2cj3sdgg45a0q23vhfn5b"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-needs" ,perl-test-needs)))
    (home-page "https://metacpan.org/release/Class-Method-Modifiers")
    (synopsis "Moose-like method modifiers")
    (description "Class::Method::Modifiers provides three modifiers:
@code{before}, @code{around}, and @code{after}.  @code{before} and @code{after}
are run just before and after the method they modify, but can not really affect
that original method.  @code{around} is run in place of the original method,
with a hook to easily call that original method.")
    (license (package-license perl))))

(define-public perl-class-mix
  (package
    (name "perl-class-mix")
    (version "0.006")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Class-Mix-"
            version ".tar.gz"))
      (sha256
       (base32
        "02vwzzqn1s24g525arbrjh9s9j0y1inp3wbr972gh51ri51zciw7"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (propagated-inputs
     `(("perl-params-classify" ,perl-params-classify)))
    (home-page "https://metacpan.org/release/Class-Mix")
    (synopsis "Dynamic class mixing")
    (description "The @code{mix_class} function provided by this
module dynamically generates anonymous classes with specified
inheritance.  This is useful where an incomplete class requires use of
a mixin in order to become instantiable.")
    (license license:perl-license)))

(define-public perl-class-singleton
  (package
    (name "perl-class-singleton")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHAY/"
                           "Class-Singleton-" version ".tar.gz"))
       (sha256
        (base32
         "0y7ngrjf551bjgmijp5rsidbkq6c8hb5lmy2jcqq0fify020s8iq"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-Singleton")
    (synopsis "Implementation of a singleton class for Perl")
    (description "This module implements a Singleton class from which other
classes can be derived.  By itself, the Class::Singleton module does very
little other than manage the instantiation of a single object.")
    (license (package-license perl))))

(define-public perl-class-tiny
  (package
    (name "perl-class-tiny")
    (version "1.006")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "Class-Tiny-" version ".tar.gz"))
       (sha256
        (base32
         "0knbi1agcfc9d7fca0szvxr6335pb22pc5n648q1vrcba8qvvz1f"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-Tiny")
    (synopsis "Minimalist class construction")
    (description "This module offers a minimalist class construction kit.  It
uses no non-core modules for any recent Perl.")
    (license license:asl2.0)))

(define-public perl-class-unload
  (package
    (name "perl-class-unload")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "Class-Unload-" version ".tar.gz"))
       (sha256
        (base32 "0pqa98z3ij6a3v9wkmvc8b410kv30y0xxqf0i6if3lp4lx3rgqjj"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-class-inspector" ,perl-class-inspector)))
    (home-page "https://metacpan.org/release/Class-Unload")
    (synopsis "Unload a class")
    (description "Class:Unload unloads a given class by clearing out its
symbol table and removing it from %INC.")
    (license (package-license perl))))

(define-public perl-class-xsaccessor
  (package
    (name "perl-class-xsaccessor")
    (version "1.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SM/SMUELLER/"
                           "Class-XSAccessor-" version ".tar.gz"))
       (sha256
        (base32
         "1wm6013il899jnm0vn50a7iv9v6r4nqywbqzj0csyf8jbwwnpicr"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-XSAccessor")
    (synopsis "Generate fast XS accessors without runtime compilation")
    (description "Class::XSAccessor implements fast read, write, and
read/write accessors in XS.  Additionally, it can provide predicates such as
\"has_foo()\" for testing whether the attribute \"foo\" is defined in the
object.  It only works with objects that are implemented as ordinary hashes.
Class::XSAccessor::Array implements the same interface for objects that use
arrays for their internal representation.")
    (license (package-license perl))))

(define-public perl-clone
  (package
    (name "perl-clone")
    (version "0.43")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AT/ATOOMIC/"
                                  "Clone-" version ".tar.gz"))
              (sha256
               (base32
                "1npf5s4b90ds6lv8gn76b2w4bdh0z5ni5zk4skgc2db5d12560lr"))))
    (build-system perl-build-system)
    (synopsis "Recursively copy Perl datatypes")
    (description
     "This module provides a clone() method which makes recursive copies of
nested hash, array, scalar and reference types, including tied variables and
objects.")
    (home-page "https://metacpan.org/release/Clone")
    (license (package-license perl))))

(define-public perl-clone-choose
  (package
    (name "perl-clone-choose")
    (version "0.010")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HE/HERMES/"
                           "Clone-Choose-" version ".tar.gz"))
       (sha256
        (base32
         "0cin2bjn5z8xhm9v4j7pwlkx88jnvz8al0njdjwyvs6fb0glh8sn"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-clone" ,perl-clone)
       ("perl-clone-pp" ,perl-clone-pp)
       ("perl-test-without-module" ,perl-test-without-module)))
    (propagated-inputs
     `(("perl-module-runtime" ,perl-module-runtime)))
    (home-page "https://metacpan.org/release/Clone-Choose")
    (synopsis "Choose appropriate Perl @code{clone} utility")
    (description "This @code{Clone::Choose} module checks several different
modules which provide a @code{clone()} function and selects an appropriate
one.")
    (license license:perl-license)))

(define-public perl-clone-pp
  (package
    (name "perl-clone-pp")
    (version "1.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/Clone-PP-"
                           version ".tar.gz"))
       (sha256
        (base32 "0y7m25fksiavzg4xj4cm9zkz8rmnk4iqy7lm01m4nmyqlna3082p"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Clone-PP")
    (synopsis "Recursively copy Perl datatypes")
    (description "This module provides a general-purpose @code{clone} function
to make deep copies of Perl data structures.  It calls itself recursively to
copy nested hash, array, scalar and reference types, including tied variables
and objects.")
    (license (package-license perl))))

(define-public perl-common-sense
  (package
    (name "perl-common-sense")
    (version "3.75")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/"
                           "common-sense-" version ".tar.gz"))
       (sha256
        (base32
         "0zhfp8f0czg69ycwn7r6ayg6idm5kyh2ai06g5s6s07kli61qsm8"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/common-sense")
    (synopsis "Sane defaults for Perl programs")
    (description "This module implements some sane defaults for Perl programs,
as defined by two typical specimens of Perl coders.")
    (license (package-license perl))))

(define-public perl-conf-libconfig
  (package
    (name "perl-conf-libconfig")
    (version "0.100")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CN/CNANGEL/"
                           "Conf-Libconfig-" version ".tar.gz"))
       (sha256
        (base32 "0qdypqd7mx96bwdjlv13fn6p96bs4w0yv94yv94xa7z5lqkdj4rg"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-extutils-pkgconfig" ,perl-extutils-pkgconfig)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-exception" ,perl-test-exception)
       ("perl-test-warn" ,perl-test-warn)))
    (inputs
     `(("libconfig" ,libconfig)))
    (home-page "https://metacpan.org/release/Conf-Libconfig")
    (synopsis "Perl extension for libconfig")
    (description
     "Conf::Libconfig is a Perl interface to the libconfig configuration file
library.  It support scalar, array, and hash data structures just like its C/C++
counterpart.  It reduces the effort required to implement a configuration file
parser in your Perl programme and allows sharing configuration files between
languages.")
    (license license:bsd-3)))

(define-public perl-config-grammar
  (package
    (name "perl-config-grammar")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DS/DSCHWEI/"
                           "Config-Grammar-" version ".tar.gz"))
       (sha256
        (base32 "1qynf5bk6mnk90nggm3z8rdz2535kmqg46s0vj93pi68r6ia7cx8"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Config-Grammar")
    (synopsis "Grammar-based config parser")
    (description
     "Config::Grammar is a module to parse configuration files.  The
configuration may consist of multiple-level sections with assignments and
tabular data.")
    (license (package-license perl))))

(define-public perl-config-any
  (package
    (name "perl-config-any")
    (version "0.32")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Config-Any-" version ".tar.gz"))
       (sha256
        (base32
         "0l31sg7dwh4dwwnql42hp7arkhcm15bhsgfg4i6xvbjzy9f2mnk8"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-module-pluggable" ,perl-module-pluggable)))
    (home-page "https://metacpan.org/release/Config-Any")
    (synopsis "Load configuration from different file formats")
    (description "Config::Any provides a facility for Perl applications and
libraries to load configuration data from multiple different file formats.  It
supports XML, YAML, JSON, Apache-style configuration, and Perl code.")
    (license (package-license perl))))

(define-public perl-config-inifiles
  (package
    (name "perl-config-inifiles")
    (version "3.000002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cpan.metacpan.org/authors/id/S/SH/SHLOMIF/"
                           "Config-IniFiles-" version ".tar.gz"))
       (sha256
        (base32 "02dsz3inh5jwgaxmbcz8qxwgin8mkhm6vj9jyzfmm3dr5pnxcbnr"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-io-stringy",perl-io-stringy)))
    (home-page "https://metacpan.org/pod/Config::IniFiles")
    (synopsis "Package for configuration files outside your Perl script")
    (description "This package provides a way to have readable configuration
files outside your Perl script.  Configurations can be imported, sections
can be grouped, and settings can be accessed from a tied hash.")
    (license (package-license perl))))

(define-public perl-config-autoconf
  (package
    (name "perl-config-autoconf")
    (version "0.317")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "Config-AutoConf-" version ".tar.gz"))
       (sha256
        (base32
         "1qcwib4yaml5z2283qy5khjcydyibklsnk8zrk9wzdzc5wnv5r01"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)))
    (home-page "https://metacpan.org/release/Config-AutoConf")
    (synopsis "Module to implement some AutoConf macros in Perl")
    (description "Config::AutoConf is intended to provide the same
opportunities to Perl developers as GNU Autoconf does for Shell developers.")
    (license (package-license perl))))

(define-public perl-config-general
  (package
    (name "perl-config-general")
    (version "2.63")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TL/TLINDEN/"
                           "Config-General-" version ".tar.gz"))
       (sha256
        (base32 "1bbg3wp0xcpj04cmm86j1x0j5968jqi5s2c87qs7dgmap1vzk6qa"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Config-General")
    (synopsis "Generic Config Module")
    (description "This module opens a config file and parses its contents for
you.  The format of config files supported by Config::General is inspired by
the well known Apache config format and is 100% compatible with Apache
configs, but you can also just use simple name/value pairs in your config
files.  In addition to the capabilities of an Apache config file it supports
some enhancements such as here-documents, C-style comments, and multiline
options.")
    (license (package-license perl))))

(define-public perl-config-gitlike
  (package
    (name "perl-config-gitlike")
    (version "1.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AL/ALEXMV/Config-GitLike-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0kp57na9mk6yni693h2fwap6l1ndbcj97l4860r9vkzx2jw0fjk7"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-moo" ,perl-moo)
       ("perl-moox-types-mooselike" ,perl-moox-types-mooselike)))
    (home-page "https://metacpan.org/release/Config-GitLike")
    (synopsis "Parse Git style configuration files")
    (description
     "This module handles parsing, modifying and creating configuration files
of the style used by the Git version control system.")
    (license license:perl-license)))

(define-public perl-config-ini
  (package
    (name "perl-config-ini")
    (version "0.025")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/R/RJ/RJBS/Config-INI-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0clphq6a17chvb663fvjnxqvyvh26g03x0fl4bg9vy4ibdnzg2v2"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-mixin-linewise" ,perl-mixin-linewise)
       ("perl-perlio-utf8_strict" ,perl-perlio-utf8_strict)
       ("perl-sub-exporter" ,perl-sub-exporter)))
    (home-page "https://metacpan.org/release/Config-INI")
    (synopsis "Simple .ini-file format reader and writer")
    (description "@code{Config::INI} is a module that facilates the reading
and writing of @code{.ini}-style configuration files.")
    (license (package-license perl))))

(define-public perl-context-preserve
  (package
    (name "perl-context-preserve")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Context-Preserve-" version ".tar.gz"))
       (sha256
        (base32
         "07zxgmb11bn4zj3w9g1zwbb9iv4jyk5q7hc0nv59knvv5i64m489"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)
       ("perl-test-simple" ,perl-test-simple)))
    (home-page "https://metacpan.org/release/Context-Preserve")
    (synopsis "Preserve context during subroutine call")
    (description "This module runs code after a subroutine call, preserving
the context the subroutine would have seen if it were the last statement in
the caller.")
    (license (package-license perl))))

(define-public perl-convert-binhex
  (package
    (name "perl-convert-binhex")
    (version "1.125")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/S/ST/STEPHEN/Convert-BinHex-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "15v3489k179cx0fz3lix79ssjid0nhhpf6c33swpxga6pss92dai"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-file-slurp" ,perl-file-slurp)
       ("perl-test-most" ,perl-test-most)))
    (home-page
     "https://metacpan.org/release/Convert-BinHex")
    (synopsis "Extract data from Macintosh BinHex files")
    (description
     "BinHex is a format for transporting files safely through electronic
mail, as short-lined, 7-bit, semi-compressed data streams.  This module
provides a means of converting those data streams back into into binary
data.")
    (license license:perl-license)))

(define-public perl-cpan-changes
  (package
    (name "perl-cpan-changes")
    (version "0.400002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/H/HA/HAARG/CPAN-Changes-"
             version ".tar.gz"))
       (sha256
        (base32
         "13dy78amkhwg278sv5im0ylyskhxpfivyl2aissqqih71nlxxvh1"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/CPAN-Changes")
    (synopsis "Read and write @file{Changes} files")
    (description
     "@code{CPAN::Changes} helps users programmatically read and write
@file{Changes} files that conform to a common specification.")
    (license license:perl-license)))

(define-public perl-cpan-distnameinfo
  (package
    (name "perl-cpan-distnameinfo")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/G/GB/GBARR/CPAN-DistnameInfo-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0d94kx596w7k328cvq4y96z1gz12hdhn3z1mklkbrb7fyzlzn91g"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/CPAN-DistnameInfo")
    (synopsis "Extract the name and version from a distribution filename")
    (description
     "@code{CPAN::DistnameInfo} uses heuristics to extract the distribution
name and version from filenames.")
    (license license:perl-license)))

(define-public perl-cpan-meta-check
  (package
    (name "perl-cpan-meta-check")
    (version "0.014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "CPAN-Meta-Check-" version ".tar.gz"))
       (sha256
        (base32
         "07rmdbz1rbnb7w33vswn1wixlyh947sqr93xrvcph1hwzhmmg818"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-test-deep" ,perl-test-deep)))
    (propagated-inputs `(("perl-cpan-meta" ,perl-cpan-meta)))
    (home-page "https://metacpan.org/release/CPAN-Meta-Check")
    (synopsis "Verify requirements in a CPAN::Meta object")
    (description "This module verifies if requirements described in a
CPAN::Meta object are present.")
    (license (package-license perl))))

(define-public perl-cpanel-json-xs
  (package
    (name "perl-cpanel-json-xs")
    (version "4.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RU/RURBAN/"
                           "Cpanel-JSON-XS-" version ".tar.gz"))
       (sha256
        (base32 "061940vyj9y3rzwq47z2a3f5i5rfpa90ccz7fgz228zr7njkvfpr"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-common-sense" ,perl-common-sense)))
    (home-page "https://metacpan.org/release/Cpanel-JSON-XS")
    (synopsis "JSON::XS for Cpanel")
    (description "This module converts Perl data structures to JSON and vice
versa.")
    (license (package-license perl))))

(define-public perl-crypt-cbc
  (package
    (name "perl-crypt-cbc")
    (version "2.33")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/L/LD/LDS/Crypt-CBC-"
            version ".tar.gz"))
      (sha256
       (base32
        "0ig698lmpjz7fslnznxm0609lvlnvf4f3s370082nzycnqhxww3a"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-crypt-rijndael" ,perl-crypt-rijndael)))
    (home-page "https://metacpan.org/release/Crypt-CBC")
    (synopsis "Encrypt Data with Cipher Block Chaining Mode")
    (description "@code{Crypt::CBC} is a Perl-only implementation of
the cryptographic Cipher Block Chaining (CBC) mode.  In combination
with a block cipher such as @code{Crypt::Rijndael} you can encrypt and
decrypt messages of arbitrarily long length.  The encrypted messages
are compatible with the encryption format used by SSLeay.")
    (license license:perl-license)))

(define-public perl-crypt-des
  (package
    (name "perl-crypt-des")
    (version "2.07")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/D/DP/DPARIS/Crypt-DES-"
            version ".tar.gz"))
      (sha256
       (base32
        "1rypxlhpd1jc0c327aghgl9y6ls47drmpvn0a40b4k3vhfsypc9d"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-crypt-cbc" ,perl-crypt-cbc)))
    (home-page "https://metacpan.org/release/Crypt-DES")
    (synopsis "DES encryption module")
    (description "@code{Crypt::DES} is an XS-based implementation of
the DES cryptography algorithm.  The module implements the
@code{Crypt::CBC} interface which has blocksize, keysize, encrypt and
decrypt functions.")
    (license license:bsd-3)))

(define-public perl-crypt-eksblowfish
  (package
    (name "perl-crypt-eksblowfish")
    (version "0.009")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Crypt-Eksblowfish-"
            version ".tar.gz"))
      (sha256
       (base32
        "0k01aw3qb2s4m1w4dqsc9cycyry1zg3wabdym4vp4421b1ni5irw"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (propagated-inputs
     `(("perl-class-mix" ,perl-class-mix)))
    (home-page "https://metacpan.org/release/Crypt-Eksblowfish")
    (synopsis "The Eksblowfish block cipher")
    (description "Eksblowfish is a variant of the Blowfish cipher,
modified to make the key setup very expensive.  This doesn't make it
significantly cryptographically stronger but is intended to hinder
brute-force attacks.  Eksblowfish is a parameterised (family-keyed)
cipher.  It takes a cost parameter that controls how expensive the key
scheduling is.  It also takes a family key, known as the \"salt\".
Cost and salt parameters together define a cipher family.  Within each
family, the key determines the encryption function.  This distribution
also includes an implementation of @code{bcrypt}, the Unix crypt()
password hashing algorithm based on Eksblowfish.")
    (license license:perl-license)))

(define-public perl-crypt-mysql
  (package
    (name "perl-crypt-mysql")
    (version "0.04")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/I/IK/IKEBE/Crypt-MySQL-"
            version ".tar.gz"))
      (sha256
       (base32
        "1qyx6ha13r0rh80ldv5wy2bq2pa74igwh8817xlapsfgxymdzswk"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-dbd-mysql" ,perl-dbd-mysql)))
    (propagated-inputs
     `(("perl-digest-sha1" ,perl-digest-sha1)))
    (home-page "https://metacpan.org/release/Crypt-MySQL")
    (synopsis "Emulate the MySQL PASSWORD() function")
    (description "@code{Crypt::MySQL} emulates the MySQL PASSWORD()
function.  The module does not depend on an interface to the MySQL
database server.  This enables the comparison of encrypted passwords
without the need for a real MySQL environment.")
    (license license:perl-license)))

(define-public perl-crypt-passwdmd5
  (package
    (name "perl-crypt-passwdmd5")
    (version "1.40")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/R/RS/RSAVAGE/Crypt-PasswdMD5-"
            version ".tgz"))
      (sha256
       (base32
        "0j0r74f18nk63phddzqbf7wqma2ci4p4bxvrwrxsy0aklbp6lzdp"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/Crypt-PasswdMD5")
    (synopsis "Interoperable MD5-based crypt() functions")
    (description "@code{Crypt::PasswdMD5} provides various
crypt()-compatible interfaces to the MD5-based crypt() function found
in various *nixes.  It is based on the implementation found on FreeBSD
2.2.[56]-RELEASE.")
    (license license:perl-license)))

(define-public perl-crypt-randpasswd
  (package
    (name "perl-crypt-randpasswd")
    (version "0.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Crypt-RandPasswd-" version ".tar.gz"))
       (sha256
        (base32
         "0ca8544371wp4vvqsa19lnhl02hczpkbwkgsgm65ziwwim3r1gdi"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Crypt-RandPasswd")
    (synopsis "Random password generator")
    (description "Crypt::RandPasswd provides three functions that can be used
to generate random passwords, constructed from words, letters, or characters.
This code is a Perl implementation of the Automated Password Generator
standard, like the program described in \"A Random Word Generator For
Pronounceable Passwords\".  This code is a re-engineering of the program
contained in Appendix A of FIPS Publication 181, \"Standard for Automated
Password Generator\".")
    (license (package-license perl))))

(define-public perl-crypt-rijndael
  (package
    (name "perl-crypt-rijndael")
    (version "1.16")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/L/LE/LEONT/Crypt-Rijndael-"
            version ".tar.gz"))
      (sha256
       (base32 "0h2dr1bd15y0sipxsdh1k4hx5bccywn15haj0xpjmf0471g0hh35"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Crypt-Rijndael")
    (synopsis "Crypt::CBC compliant Rijndael encryption module")
    (description "This module implements the Rijndael cipher which has
been selected as the Advanced Encryption Standard.  The keysize for
Rijndael is 32 bytes.  The blocksize is 16 bytes (128 bits).  The
supported encryption modes are:

@itemize
@item @code{MODE_CBC}---Cipher Block Chaining
@item @code{MODE_CFB}---Cipher feedback
@item @code{MODE_CTR}---Counter mode
@item @code{MODE_ECB}---Electronic cookbook mode
@item @code{MODE_OFB}---Output feedback
@end itemize")
    (license license:gpl3)))

(define-public perl-crypt-rc4
  (package
    (name "perl-crypt-rc4")
    (version "2.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/S/SI/SIFUKURT/Crypt-RC4-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1sp099cws0q225h6j4y68hmfd1lnv5877gihjs40f8n2ddf45i2y"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release//Crypt-RC4")
    (synopsis "Perl implementation of the RC4 encryption algorithm")
    (description "A pure Perl implementation of the RC4 algorithm.")
    (license (package-license perl))))

(define-public perl-crypt-unixcrypt_xs
  (package
    (name "perl-crypt-unixcrypt_xs")
    (version "0.11")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/B/BO/BORISZ/Crypt-UnixCrypt_XS-"
            version ".tar.gz"))
      (sha256
       (base32
        "1ajg3x6kwxy4x9p3nw1j36qjxpjvdpi9wkca5gfd86y9q8939sv2"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Crypt-UnixCrypt_XS")
    (synopsis "XS interface for a portable traditional crypt function")
    (description "@code{Crypt::UnixCrypt_XS} implements the DES-based
Unix @code{crypt} function.  For those who need to construct
non-standard variants of @code{crypt}, the various building blocks
used in @code{crypt} are also supplied separately.")
    ;; Files in the 'fcrypt' directory are covered by a BSD licence.
    (license (list license:perl-license license:bsd-3))))

(define-public perl-cwd-guard
  (package
    (name "perl-cwd-guard")
    (version "0.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/K/KA/KAZEBURO/"
                                  "Cwd-Guard-" version ".tar.gz"))
              (sha256
               (base32
                "0xwf4rmii55k3lp19mpbh00mbgby7rxdk2lk84148bjhp6i7rz3s"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-requires" ,perl-test-requires)))
    (home-page "https://metacpan.org/release/Cwd-Guard")
    (synopsis "Temporarily change working directory")
    (description
     "@code{Cwd::Guard} changes the current directory using a limited scope.
It returns to the previous working directory when the object is destroyed.")
    (license (package-license perl))))

(define-public perl-czplib
  (package
    (name "perl-czplib")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/czplib/czplib.v"
                           version ".tgz"))
       (sha256
        (base32
         "12kln8l5h406r1ss6zbazgcshmys9nvabkrhvk2zwrrgl1saq1kf"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove .git directory
           (delete-file-recursively ".git")
           #t))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (copy-recursively "."
                              (string-append (assoc-ref outputs "out")
                                             "/lib/perl5/site_perl/"
                                             ,(package-version perl)))
            #t)))))
    (home-page "https://sourceforge.net/projects/czplib/")
    (synopsis "Library for genomic analysis")
    (description "Chaolin Zhang's Perl Library (czplib) contains assorted
functions and data structures for processing and analysing genomic and
bioinformatics data.")
    (license license:gpl3+)))

(define-public perl-data
  (package
    (name "perl-data")
    (version "0.002009")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MA/MATTP/"
                           "Data-Perl-" version ".tar.gz"))
       (sha256
        (base32
         "12vgqdjbfqf2qfg21x22wg88xnwxfbw2ki3qzcb3nb0chwjj4axn"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-deep" ,perl-test-deep)
       ("perl-test-output" ,perl-test-output)
       ("perl-test-fatal" ,perl-test-fatal)))
    (inputs
     `(("perl-class-method-modifiers" ,perl-class-method-modifiers)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-role-tiny" ,perl-role-tiny)
       ("perl-strictures" ,perl-strictures)))
    (home-page "https://metacpan.org/release/Data-Perl")
    (synopsis "Base classes wrapping fundamental Perl data types")
    (description "Collection of classes that wrap fundamental data types that
exist in Perl.  These classes and methods as they exist today are an attempt
to mirror functionality provided by Moose's Native Traits.  One important
thing to note is all classes currently do no validation on constructor
input.")
    (license (package-license perl))))

(define-public perl-data-compare
  (package
    (name "perl-data-compare")
    (version "1.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DC/DCANTRELL/"
                           "Data-Compare-" version ".tar.gz"))
       (sha256
        (base32 "1gg8rqbv3x6a1lrpabv6vnlab53zxmpwz2ygad9fcx4gygqj12l1"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-clone" ,perl-clone)
       ("perl-file-find-rule" ,perl-file-find-rule)))
    (home-page "https://metacpan.org/release/Data-Compare")
    (synopsis "Compare Perl data structures")
    (description "This module compares arbitrary data structures to see if
they are copies of each other.")
    (license (package-license perl))))

(define-public perl-data-entropy
  (package
    (name "perl-data-entropy")
    (version "0.007")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Data-Entropy-"
            version ".tar.gz"))
      (sha256
       (base32
        "1r176jjzir2zg5kidx85f7vzi6jsw7ci9vd4kvbr9183lfhw8496"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (propagated-inputs
     `(("perl-crypt-rijndael" ,perl-crypt-rijndael)
       ("perl-data-float" ,perl-data-float)
       ("perl-http-lite" ,perl-http-lite)
       ("perl-params-classify" ,perl-params-classify)))
    (home-page "https://metacpan.org/release/Data-Entropy")
    (synopsis "Entropy (randomness) management")
    (description "@code{Data::Entropy} provides modules relating to
the generation and use of entropy.  The Data::Entropy::Source class
manages the entropy coming from a particular source.  This class acts
as a layer over a raw entropy source, which may be a normal I/O handle
or a special-purpose class.  The Data::Entropy::RawSource::* classes
provide fundamental sources of entropy.  The sources specially
supported are an OS-supplied entropy collector, downloads from servers
on the Internet, and cryptographic fake entropy.  The
Data::Entropy::Algorithms module contains a collection of fundamental
algorithms that use entropy.  There are random number generators and
functions to shuffle arrays.")
    (license license:perl-license)))

(define-public perl-data-integer
  (package
    (name "perl-data-integer")
    (version "0.006")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Data-Integer-"
            version ".tar.gz"))
      (sha256
       (base32
        "0m53zxhx9sn49yqh7azlpyy9m65g54v8cd2ha98y77337gg7xdv3"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (home-page "https://metacpan.org/release/Data-Integer")
    (synopsis "Details of the native integer data type")
    (description "This module is about the native integer numerical
data type.  A native integer is one of the types of datum that can
appear in the numeric part of a Perl scalar.  This module supplies
constants describing the native integer type.  Both signed and
unsigned representations are handled.")
    (license license:perl-license)))

(define-public perl-data-uniqid
  (package
    (name "perl-data-uniqid")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MW/MWX/Data-Uniqid-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1jsc6acmv97pzsvx1fqywz4qvxxpp7kwmb78ygyqpsczkfj9p4dn"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Data-Uniqid")
    (synopsis "Perl extension for generating unique identifiers")
    (description "@code{Data::Uniqid} provides three simple routines for
generating unique ids.  These ids are coded with a Base62 system to make them
short and handy (e.g. to use it as part of a URL).")
    (license (package-license perl))))

(define-public perl-data-dump
  (package
    (name "perl-data-dump")
    (version "1.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GA/GAAS/"
                           "Data-Dump-" version ".tar.gz"))
       (sha256
        (base32
         "0r9ba52b7p8nnn6nw0ygm06lygi8g68piri78jmlqyrqy5gb0lxg"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Data-Dump")
    (synopsis "Pretty printing of data structures")
    (description "This module provide functions that takes a list of values as
their argument and produces a string as its result.  The string contains Perl
code that, when \"eval\"ed, produces a deep copy of the original arguments.")
    (license (package-license perl))))

(define-public perl-data-dumper
  (package
    (name "perl-data-dumper")
    (version "2.173")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/X/XS/XSAWYERX/"
                           "Data-Dumper-" version ".tar.gz"))
       (sha256
        (base32
         "1yknbp86md6mjlhbs1lzz6mals3iyizndgiij58qx61hjfrhhxk9"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Data-Dumper")
    (synopsis "Convert data structures to strings")
    (description "Given a list of scalars or reference variables,
@code{Data::Dumper} writes out their contents in Perl syntax.  The references
can also be objects.  The content of each variable is output in a single Perl
statement.  It handles self-referential structures correctly.")
    (license license:perl-license)))

(define-public perl-data-dumper-concise
  (package
    (name "perl-data-dumper-concise")
    (version "2.023")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Data-Dumper-Concise-" version ".tar.gz"))
       (sha256
        (base32
         "0lsqbl1mxhkj0qnjfa1jrvx8wwbyi81bgwfyj1si6cdg7h8jzhm6"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Data-Dumper-Concise")
    (synopsis "Concise data dumper")
    (description "Data::Dumper::Concise provides a dumper with Less
indentation and newlines plus sub deparsing.")
    (license (package-license perl))))

(define-public perl-data-float
  (package
    (name "perl-data-float")
    (version "0.013")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Data-Float-"
            version ".tar.gz"))
      (sha256
       (base32
        "12ji4yf3nc965rqqgfhr96w7irpm6n1g15nivfxvhc49hlym5cg2"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (home-page "https://metacpan.org/release/Data-Float")
    (synopsis "Details of the floating point data type")
    (description "@code{Data::Float} is about the native floating
point numerical data type.  A floating point number is one of the
types of datum that can appear in the numeric part of a Perl scalar.
This module supplies constants describing the native floating point
type, classification functions and functions to manipulate floating
point values at a low level.")
    (license license:perl-license)))

(define-public perl-data-optlist
  (package
    (name "perl-data-optlist")
    (version "0.110")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/R/RJ/RJBS/Data-OptList-"
             version ".tar.gz"))
       (sha256
        (base32
         "1hzmgr2imdg1fc3hmwx0d56fhsdfyrgmgx7jb4jkyiv6575ifq9n"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-sub-install" ,perl-sub-install)
       ("perl-params-util" ,perl-params-util)))
    (home-page "https://metacpan.org/release/Data-OptList")
    (synopsis "Parse and validate simple name/value option pairs")
    (description
     "Data::OptList provides a simple syntax for name/value option pairs.")
    (license (package-license perl))))

(define-public perl-data-page
  (package
    (name "perl-data-page")
    (version "2.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Data-Page-" version ".tar.gz"))
       (sha256
        (base32 "12rxrr2b11qjk0c437cisw2kfqkafw1awcng09cv6yhzglb55yif"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-class-accessor-chained" ,perl-class-accessor-chained)))
    (home-page "https://metacpan.org/release/Data-Page")
    (synopsis "Help when paging through sets of results")
    (description "When searching through large amounts of data, it is often
the case that a result set is returned that is larger than we want to display
on one page.  This results in wanting to page through various pages of data.
The maths behind this is unfortunately fiddly, hence this module.")
    (license (package-license perl))))

(define-public perl-data-perl
  (package
    (name "perl-data-perl")
    (version "0.002009")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MA/MATTP/Data-Perl-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "12vgqdjbfqf2qfg21x22wg88xnwxfbw2ki3qzcb3nb0chwjj4axn"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-deep" ,perl-test-deep)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-output" ,perl-test-output)))
    (inputs
     `(("perl-class-method-modifiers"
        ,perl-class-method-modifiers)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-role-tiny" ,perl-role-tiny)
       ("perl-strictures" ,perl-strictures)))
    (propagated-inputs
     `(("perl-list-moreutils" ,perl-list-moreutils)))
    (home-page
     "https://metacpan.org/release/Data-Perl")
    (synopsis "Base classes wrapping fundamental Perl data types")
    (description
     "@code{Data::Perl} is a container class for the following classes:
@itemize
@item @code{Data::Perl::Collection::Hash}
@item @code{Data::Perl::Collection::Array}
@item @code{Data::Perl::String}
@item @code{Data::Perl::Number}
@item @code{Data::Perl::Counter}
@item @code{Data::Perl::Bool}
@item @code{Data::Perl::Code}
@end itemize")
    (license license:perl-license)))

(define-public perl-data-printer
  (package
    (name "perl-data-printer")
    (version "0.40")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GA/GARU/Data-Printer-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0njjh8zp5afc4602jrnmg89icj7gfsil6i955ypcqxc2gl830sb0"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-clone-pp" ,perl-clone-pp)
       ("perl-file-homedir" ,perl-file-homedir)
       ("perl-package-stash" ,perl-package-stash)
       ("perl-sort-naturally" ,perl-sort-naturally)))
    (home-page "https://metacpan.org/release/Data-Printer")
    (synopsis "Colored pretty-print of Perl data structures and objects")
    (description "Display Perl variables and objects on screen, properly
formatted (to be inspected by a human).")
    (license (package-license perl))))

(define-public perl-data-record
  (package
    (name "perl-data-record")
    (version "0.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OV/OVID/"
                           "Data-Record-" version ".tar.gz"))
       (sha256
        (base32
         "1gwyhjwg4lrnfsn8wb6r8msb4yh0y4wca4mz3z120xbnl9nycshx"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)
       ("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-sub-uplevel" ,perl-sub-uplevel)))
    (home-page "https://metacpan.org/release/Data-Record")
    (synopsis "Conditionally split data into records")
    (description "This Perl module allows you to split data into records by
not only specifying what you wish to split the data on, but also by specifying
an \"unless\" regular expression.  If the text in question matches the
\"unless\" regex, it will not be split there.  This allows us to do things
like split on newlines unless newlines are embedded in quotes.")
    (license (package-license perl))))

(define-public perl-data-section
  (package
    (name "perl-data-section")
    (version "0.200007")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/R/RJ/RJBS/Data-Section-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1pmlxca0a8sv2jjwvhwgqavq6iwys6kf457lby4anjp3f1dpx4yd"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-failwarnings" ,perl-test-failwarnings)))
    (propagated-inputs
     `(("perl-mro-compat" ,perl-mro-compat)
       ("perl-sub-exporter" ,perl-sub-exporter)))
    (home-page "https://metacpan.org/release/Data-Section")
    (synopsis "Read multiple hunks of data out of your DATA section")
    (description "This package provides a Perl library to read multiple hunks
of data out of your DATA section.")
    (license (package-license perl))))

(define-public perl-data-section-simple
  (package
    (name "perl-data-section-simple")
    (version "0.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Data-Section-Simple-" version ".tar.gz"))
       (sha256
        (base32 "1jx9g5sxcw0i2zkm2z895k422i49kpx0idnnvvvs36lhvgzkac0b"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-requires" ,perl-test-requires)))
    (home-page "https://metacpan.org/release/Data-Section-Simple")
    (synopsis "Read data from __DATA__")
    (description
     "Data::Section::Simple is a simple module to extract data from __DATA__
section of the file.")
    (license license:perl-license)))

(define-public perl-data-stag
  (package
    (name "perl-data-stag")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CM/CMUNGALL/"
                           "Data-Stag-" version ".tar.gz"))
       (sha256
        (base32
         "0ncf4l39ka23nb01jlm6rzxdb5pqbip01x0m38bnvf1gim825caa"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-io-string" ,perl-io-string)))
    (home-page "https://metacpan.org/release/Data-Stag")
    (synopsis "Structured tags datastructures")
    (description
     "This module is for manipulating data as hierarchical tag/value
pairs (Structured TAGs or Simple Tree AGgregates).  These datastructures can
be represented as nested arrays, which have the advantage of being native to
Perl.")
    (license (package-license perl))))

(define-public perl-data-stream-bulk
  (package
    (name "perl-data-stream-bulk")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                           "Data-Stream-Bulk-" version ".tar.gz"))
       (sha256
        (base32
         "05q9ygcv7r318j7daxz42rjr5b99j6whjmwjdih0axxrlqr89q06"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-moose" ,perl-moose)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-path-class" ,perl-path-class)
       ("perl-sub-exporter" ,perl-sub-exporter)))
    (home-page "https://metacpan.org/release/Data-Stream-Bulk")
    (synopsis "N at a time iteration API")
    (description "This module tries to find middle ground between one at a
time and all at once processing of data sets.  The purpose of this module is
to avoid the overhead of implementing an iterative api when this isn't
necessary, without breaking forward compatibility in case that becomes
necessary later on.")
    (license (package-license perl))))

(define-public perl-data-tumbler
  (package
    (name "perl-data-tumbler")
    (version "0.010")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "Data-Tumbler-" version ".tar.gz"))
       (sha256
        (base32 "15pgvmf7mf9fxsg2l4l88xwvs41218d0bvawhlk15sx06qqp0kwb"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-most" ,perl-test-most)))
    (propagated-inputs
     `(("perl-file-homedir" ,perl-file-homedir)))
    (home-page "https://metacpan.org/release/Data-Tumbler")
    (synopsis "Dynamic generation of nested combinations of variants")
    (description "Data::Tumbler - Dynamic generation of nested combinations of
variants.")
    (license (package-license perl))))

(define-public perl-data-visitor
  (package
    (name "perl-data-visitor")
    (version "0.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                           "Data-Visitor-" version ".tar.gz"))
       (sha256
        (base32
         "0m7d1505af9z2hj5aw020grcmjjlvnkjpvjam457d7k5qfy4m8lf"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-class-load" ,perl-class-load)
       ("perl-moose" ,perl-moose)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-task-weaken" ,perl-task-weaken)
       ("perl-tie-toobject" ,perl-tie-toobject)))
    (home-page "https://metacpan.org/release/Data-Visitor")
    (synopsis "Visitor style traversal of Perl data structures")
    (description "This module is a simple visitor implementation for Perl
values.  It has a main dispatcher method, visit, which takes a single perl
value and then calls the methods appropriate for that value.  It can
recursively map (cloning as necessary) or just traverse most structures, with
support for per-object behavior, circular structures, visiting tied
structures, and all ref types (hashes, arrays, scalars, code, globs).")
    (license (package-license perl))))

(define-public perl-date-calc
  (package
    (name "perl-date-calc")
    (version "6.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/ST/STBEY/"
                           "Date-Calc-" version ".tar.gz"))
       (sha256
        (base32
         "1barz0jgdaan3jm7ciphs5n3ahwkl42imprs3y8c1dwpwyr3gqbw"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-bit-vector" ,perl-bit-vector)
       ("perl-carp-clan" ,perl-carp-clan)))
    (home-page "https://metacpan.org/release/Date-Calc")
    (synopsis "Gregorian calendar date calculations")
    (description "This package consists of a Perl module for date calculations
based on the Gregorian calendar, thereby complying with all relevant norms and
standards: ISO/R 2015-1971, DIN 1355 and, to some extent, ISO 8601 (where
applicable).")
    (license (package-license perl))))

(define-public perl-date-calc-xs
  (package
    (name "perl-date-calc-xs")
    (version "6.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/ST/STBEY/"
                           "Date-Calc-XS-" version ".tar.gz"))
       (sha256
        (base32
         "1cssi9rmd31cgaafgp4m70jqbm1mgh3aphxsxz1dwdz8h283n6jz"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-bit-vector" ,perl-bit-vector)
       ("perl-carp-clan" ,perl-carp-clan)
       ("perl-date-calc" ,perl-date-calc)))
    (home-page "https://metacpan.org/release/Date-Calc-XS")
    (synopsis "XS wrapper for Date::Calc")
    (description "Date::Calc::XS is an XS wrapper and C library plug-in for
Date::Calc.")
    (license (list (package-license perl) license:lgpl2.0+))))

(define-public perl-date-manip
  (package
    (name "perl-date-manip")
    (version "6.82")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SB/SBECK/"
                           "Date-Manip-" version ".tar.gz"))
       (sha256
        (base32 "0ak72kpydwhq2z03mhdfwm3ganddzb8gawzh6crpsjvb9kwvr5ps"))))
    (build-system perl-build-system)
    (arguments
     ;; Tests would require tzdata for timezone information, but tzdata is in
     ;; (gnu packages base) which would create a circular dependency.  TODO:
     ;; Maybe put this package elsewhere so we can turn on tests.
     '(#:tests? #f))
    (home-page "https://metacpan.org/release/Date-Manip")
    (synopsis "Date manipulation routines")
    (description "Date::Manip is a series of modules for common date/time
operations, such as comparing two times, determining a date a given amount of
time from another, or parsing international times.")
    (license (package-license perl))))

(define-public perl-date-simple
  (package
    (name "perl-date-simple")
    (version "3.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IZ/IZUT/"
                           "Date-Simple-" version ".tar.gz"))
       (sha256
        (base32
         "016x17r9wi6ffdc4idwirzd1sxqcb4lmq5fn2aiq25nf2iir5899"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Date-Simple")
    (synopsis "Simple date handling")
    (description "Dates are complex enough without times and timezones.  This
module may be used to create simple date objects.  It handles validation,
interval arithmetic, and day-of-week calculation.  It does not deal with
hours, minutes, seconds, and time zones.")
    ;; Can be used with either license.
    (license (list (package-license perl) license:gpl2+))))

(define-public perl-datetime
  (package
    (name "perl-datetime")
    (version "1.52")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "DateTime-" version ".tar.gz"))
       (sha256
        (base32 "1z1xpifh2kpyw7rlc8ivg9rl0qmabjq979gjp0s9agdjf9hqp0k7"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-cpan-meta-check" ,perl-cpan-meta-check)
       ("perl-module-build" ,perl-module-build)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-warnings" ,perl-test-warnings)))
    (propagated-inputs
     `(("perl-datetime-locale" ,perl-datetime-locale)
       ("perl-datetime-timezone" ,perl-datetime-timezone)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-params-validate" ,perl-params-validate)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "https://metacpan.org/release/DateTime")
    (synopsis "Date and time object for Perl")
    (description "DateTime is a class for the representation of date/time
combinations.  It represents the Gregorian calendar, extended backwards in
time before its creation (in 1582).")
    (license license:artistic2.0)))

(define-public perl-datetime-calendar-julian
  (package
    (name "perl-datetime-calendar-julian")
    (version "0.102")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/W/WY/WYANT/"
                           "DateTime-Calendar-Julian-" version ".tar.gz"))
       (sha256
        (base32 "0j95dhma66spjyb04zi6rwy7l33hibnrx02mn0znd9m89aiq52s6"))))
    (build-system perl-build-system)
    ;; Only needed for tests
    (native-inputs
     `(("perl-datetime" ,perl-datetime)))
    (home-page "https://metacpan.org/release/DateTime-Calendar-Julian")
    (synopsis "Dates in the Julian calendar")
    (description "This package is a companion module to @code{DateTime.pm}.
It implements the Julian calendar.  It supports everything that
@code{DateTime.pm} supports and more: about one day per century more, to be
precise.")
    (license (package-license perl))))

(define-public perl-datetime-set
  (package
    (name "perl-datetime-set")
    (version "0.3900")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FG/FGLOCK/"
                           "DateTime-Set-" version ".tar.gz"))
       (sha256
        (base32
         "0ih9pi6myg5i26hjpmpzqn58s0yljl2qxdd6gzpy9zda4hwirx4l"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-params-validate" ,perl-params-validate)
       ("perl-set-infinite" ,perl-set-infinite)))
    (home-page "https://metacpan.org/release/DateTime-Set")
    (synopsis "DateTime set objects")
    (description "The DateTime::Set module provides a date/time sets
implementation.  It allows, for example, the generation of groups of dates,
like \"every wednesday\", and then find all the dates matching that pattern,
within a time range.")
    (license (package-license perl))))

(define-public perl-datetime-event-ical
  (package
    (name "perl-datetime-event-ical")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FG/FGLOCK/"
                           "DateTime-Event-ICal-" version ".tar.gz"))
       (sha256
        (base32
         "1skmykxbrf98ldi72d5s1v6228gfdr5iy4y0gpl0xwswxy247njk"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-datetime-event-recurrence" ,perl-datetime-event-recurrence)))
    (home-page "https://metacpan.org/release/DateTime-Event-ICal")
    (synopsis "DateTime rfc2445 recurrences")
    (description "This module provides convenience methods that let you easily
create DateTime::Set objects for RFC 2445 style recurrences.")
    (license (package-license perl))))

(define-public perl-datetime-event-recurrence
  (package
    (name "perl-datetime-event-recurrence")
    (version "0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FG/FGLOCK/"
                           "DateTime-Event-Recurrence-" version ".tar.gz"))
       (sha256
        (base32
         "19dms2vg9hvfx80p85m8gkn2ww0yxjrjn8qsr9k7f431lj4qfh7r"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-datetime-set" ,perl-datetime-set)))
    (home-page "https://metacpan.org/release/DateTime-Event-Recurrence")
    (synopsis "DateTime::Set extension for basic recurrences")
    (description "This module provides convenience methods that let you easily
create DateTime::Set objects for various recurrences, such as \"once a month\"
or \"every day\".  You can also create more complicated recurrences, such as
\"every Monday, Wednesday and Thursday at 10:00 AM and 2:00 PM\".")
    (license (package-license perl))))

(define-public perl-datetime-format-builder
  (package
    (name "perl-datetime-format-builder")
    (version "0.82")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "DateTime-Format-Builder-" version ".tar.gz"))
       (sha256
        (base32
         "18qw5rn1qbji3iha8gmpgldbjv9gvn97j9d5cp57fb4r5frawgrq"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-class-factory-util" ,perl-class-factory-util)
       ("perl-datetime" ,perl-datetime)
       ("perl-datetime-format-strptime" ,perl-datetime-format-strptime)
       ("perl-params-validate" ,perl-params-validate)))
    (home-page "https://metacpan.org/release/DateTime-Format-Builder")
    (synopsis "Create DateTime parser classes and objects")
    (description "DateTime::Format::Builder creates DateTime parsers.  Many
string formats of dates and times are simple and just require a basic regular
expression to extract the relevant information.  Builder provides a simple way
to do this without writing reams of structural code.")
    (license license:artistic2.0)))

(define-public perl-datetime-format-flexible
  (package
    (name "perl-datetime-format-flexible")
    (version "0.32")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TH/THINC/"
                           "DateTime-Format-Flexible-" version ".tar.gz"))
       (sha256
        (base32 "1vnq3a8bwhidcv3z9cvcmfiq2qa84hikr993ffr19fw7nbzbk9sh"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)
       ("perl-test-nowarnings" ,perl-test-nowarnings)
       ("perl-test-mocktime" ,perl-test-mocktime)))
    (propagated-inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-datetime-format-builder" ,perl-datetime-format-builder)
       ("perl-datetime-timezone" ,perl-datetime-timezone)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-module-pluggable" ,perl-module-pluggable)))
    (home-page "https://metacpan.org/release/DateTime-Format-Flexible")
    (synopsis "Parse date and time strings")
    (description "DateTime::Format::Flexible attempts to take any string you
give it and parse it into a DateTime object.")
    (license (package-license perl))))

(define-public perl-datetime-format-ical
  (package
    (name "perl-datetime-format-ical")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "DateTime-Format-ICal-" version ".tar.gz"))
       (sha256
        (base32
         "0cvwk7pigj7czsp81z35h7prxvylkrlk2l0kwvq0v72ykx9zc2cb"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-datetime-event-ical" ,perl-datetime-event-ical)
       ("perl-datetime-set" ,perl-datetime-set)
       ("perl-datetime-timezone" ,perl-datetime-timezone)
       ("perl-params-validate" ,perl-params-validate)))
    (home-page "https://metacpan.org/release/DateTime-Format-ICal")
    (synopsis "Parse and format iCal datetime and duration strings")
    (description "This module understands the ICal date/time and duration
formats, as defined in RFC 2445.  It can be used to parse these formats in
order to create the appropriate objects.")
    (license (package-license perl))))

(define-public perl-datetime-format-iso8601
  (package
    (name "perl-datetime-format-iso8601")
    (version "0.08")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/J/JH/JHOBLITT/DateTime-Format-ISO8601-"
            version ".tar.gz"))
      (sha256
       (base32
        "1syccqd5jlwms8v78ksnf68xijzl97jky5vbwhnyhxi5gvgfx8xk"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-datetime-format-builder" ,perl-datetime-format-builder)
       ("perl-file-find-rule" ,perl-file-find-rule)
       ("perl-test-distribution" ,perl-test-distribution)
       ("perl-test-pod" ,perl-test-pod)))
    (home-page "https://metacpan.org/release/DateTime-Format-ISO8601")
    (synopsis "Parse ISO8601 date and time formats")
    (description "@code{DateTime::Format::ISO8601} is a DateTime
extension that parses almost all ISO8601 date and time formats.")
    (license license:perl-license)))

(define-public perl-datetime-format-natural
  (package
    (name "perl-datetime-format-natural")
    (version "1.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SC/SCHUBIGER/"
                           "DateTime-Format-Natural-" version ".tar.gz"))
       (sha256
        (base32 "1n68b5hnw4n55q554v7y4ffwiypz6rk40mh0r550fxwv69bvyky0"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-module-util" ,perl-module-util)
       ("perl-test-mocktime" ,perl-test-mocktime)))
    (propagated-inputs
     `(("perl-boolean" ,perl-boolean)
       ("perl-clone" ,perl-clone)
       ("perl-date-calc" ,perl-date-calc)
       ("perl-date-calc-xs" ,perl-date-calc-xs)
       ("perl-datetime" ,perl-datetime)
       ("perl-datetime-timezone" ,perl-datetime-timezone)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-params-validate" ,perl-params-validate)))
    (home-page "https://metacpan.org/release/DateTime-Format-Natural")
    (synopsis "Machine-readable date/time with natural parsing")
    (description "DateTime::Format::Natural takes a string with a human
readable date/time and creates a machine readable one by applying natural
parsing logic.")
    (license (package-license perl))))

(define-public perl-datetime-format-strptime
  (package
    (name "perl-datetime-format-strptime")
    (version "1.77")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "DateTime-Format-Strptime-" version ".tar.gz"))
       (sha256
        (base32 "0jiy2yc9h9932ykb8x2l1j3ff8ms3p4426m947r5clygis1kr91g"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-datetime-locale" ,perl-datetime-locale)
       ("perl-datetime-timezone" ,perl-datetime-timezone)
       ("perl-package-deprecationmanager" ,perl-package-deprecationmanager)
       ("perl-params-validate" ,perl-params-validate)
       ("perl-sub-name" ,perl-sub-name)
       ("perl-test-warnings" ,perl-test-warnings)))
    (home-page "https://metacpan.org/release/DateTime-Format-Strptime")
    (synopsis "Parse and format strp and strf time patterns")
    (description "This module implements most of `strptime(3)`, the POSIX
function that is the reverse of `strftime(3)`, for `DateTime`.  While
`strftime` takes a `DateTime` and a pattern and returns a string, `strptime`
takes a string and a pattern and returns the `DateTime` object associated.")
    (license license:artistic2.0)))

(define-public perl-datetime-locale
  (package
    (name "perl-datetime-locale")
    (version "1.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "DateTime-Locale-" version ".tar.gz"))
       (sha256
        (base32
         "05f0jchminv5g2nrvsx5v1ihc5919fzzhh4f82dxi5ns8bkq2nis"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-ipc-system-simple" ,perl-ipc-system-simple)
       ("perl-test-file-sharedir-dist" ,perl-test-file-sharedir-dist)
       ("perl-test-warnings" ,perl-test-warnings)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)
       ("perl-file-sharedir-install" ,perl-file-sharedir-install)
       ("perl-cpan-meta-check" ,perl-cpan-meta-check)
       ("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-params-validationcompiler" ,perl-params-validationcompiler)))
    (home-page "https://metacpan.org/release/DateTime-Locale")
    (synopsis "Localization support for DateTime.pm")
    (description "The DateTime::Locale modules provide localization data for
the DateTime.pm class.")
    (license (package-license perl))))

(define-public perl-datetime-timezone
  (package
    (name "perl-datetime-timezone")
    (version "2.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "DateTime-TimeZone-" version ".tar.gz"))
       (sha256
        (base32
         "0kz5kz47awf2bhb85xx5rbajkr093ipm2d2vkhqs8lqq0f305r3a"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tzdata
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/DateTime/TimeZone/Local/Unix.pm"
               (("our \\$ZoneinfoDir = '\\/usr\\/share\\/zoneinfo';")
                (string-append "our $ZoneinfoDir = '"
                               (assoc-ref inputs "tzdata") "/share/zoneinfo"
                               "';")))
             #t)))))
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (inputs
     `(("tzdata" ,tzdata)))
    (propagated-inputs
     `(("perl-class-singleton" ,perl-class-singleton)
       ("perl-list-allutils" ,perl-list-allutils)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)
       ("perl-params-validationcompiler" ,perl-params-validationcompiler)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "https://metacpan.org/release/DateTime-TimeZone")
    (synopsis "Time zone object for Perl")
    (description "This class is the base class for all time zone objects.  A
time zone is represented internally as a set of observances, each of which
describes the offset from GMT for a given time period.  Note that without the
DateTime module, this module does not do much.  It's primary interface is
through a DateTime object, and most users will not need to directly use
DateTime::TimeZone methods.")
    (license (package-license perl))))

(define-public perl-datetimex-easy
  (package
    (name "perl-datetimex-easy")
    (version "0.089")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RO/ROKR/"
                           "DateTimeX-Easy-" version ".tar.gz"))
       (sha256
        (base32
         "0ybs9175h4s39x8a23ap129cgqwmy6w7psa86194jq5cww1d5rhp"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-most" ,perl-test-most)))
    (propagated-inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-datetime-format-flexible" ,perl-datetime-format-flexible)
       ("perl-datetime-format-ical" ,perl-datetime-format-ical)
       ("perl-datetime-format-natural" ,perl-datetime-format-natural)
       ("perl-timedate" ,perl-timedate)))
    (home-page "https://metacpan.org/release/DateTimeX-Easy")
    (synopsis "Parse date/time strings")
    (description "DateTimeX::Easy uses a variety of DateTime::Format packages
to create DateTime objects, with some custom tweaks to smooth out the rough
edges (mainly concerning timezone detection and selection).")
    (license (package-license perl))))

(define-public perl-datetime-format-mail
  (package
    (name "perl-datetime-format-mail")
    (version "0.403")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/B/BO/BOOK/"
                                  "DateTime-Format-Mail-" version ".tar.gz"))
              (sha256
               (base32
                "1c7wapbi9g9p2za52l3skhh31vg4da5kx2yfqzsqyf3p8iff7y4d"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-params-validate" ,perl-params-validate)))
    (home-page "https://metacpan.org/release/DateTime-Format-Mail")
    (synopsis "Convert between DateTime and RFC2822/822 formats")
    (description "RFCs 2822 and 822 specify date formats to be used by email.
This module parses and emits such dates.")
    (license (package-license perl))))

(define-public perl-datetime-format-w3cdtf
  (package
    (name "perl-datetime-format-w3cdtf")
    (version "0.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/G/GW/GWILLIAMS/"
                                  "DateTime-Format-W3CDTF-" version ".tar.gz"))
              (sha256
               (base32
                "0s32lb1k80p3b3sb7w234zgxnrmadrwbcg41lhaal7dz3dk2p839"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-datetime" ,perl-datetime)))
    (native-inputs
     `(("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (home-page "https://metacpan.org/release/DateTime-Format-W3CDTF")
    (synopsis "Parse and format W3CDTF datetime strings")
    (description
     "This module understands the W3CDTF date/time format, an ISO 8601 profile,
defined at https://www.w3.org/TR/NOTE-datetime.  This format is the native date
format of RSS 1.0.  It can be used to parse these formats in order to create
the appropriate objects.")
    (license (package-license perl))))

(define-public perl-devel-callchecker
  (package
    (name "perl-devel-callchecker")
    (version "0.008")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Devel-CallChecker-"
            version ".tar.gz"))
      (sha256
       (base32
        "1p0ij2k2i81zhl7064h9ghld1w5xy2zsbghkpdzm2hjryl5lwn2x"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (propagated-inputs
     `(("perl-b-hooks-op-check" ,perl-b-hooks-op-check)
       ("perl-dynaloader-functions" ,perl-dynaloader-functions)))
    (home-page "https://metacpan.org/release/Devel-CallChecker")
    (synopsis "Custom op checking attached to subroutines")
    (description "This module makes some new features of the Perl
5.14.0 C API available to XS modules running on older versions of
Perl.  The features are centred around the function
@code{cv_set_call_checker}, which allows XS code to attach a magical
annotation to a Perl subroutine, resulting in resolvable calls to that
subroutine being mutated at compile time by arbitrary C code.  This
module makes @code{cv_set_call_checker} and several supporting
functions available.")
    (license license:perl-license)))

(define-public perl-devel-caller
  (package
    (name "perl-devel-caller")
    (version "2.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Devel-Caller-" version ".tar.gz"))
       (sha256
        (base32
         "1pxpimifzmnjnvf4icclx77myc15ahh0k56sj1djad1855mawwva"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-padwalker" ,perl-padwalker)))
    (home-page "https://metacpan.org/release/Devel-Caller")
    (synopsis "Meatier version of caller")
    (description "Devel::Caller provides meatier version of caller.")
    (license (package-license perl))))

(define-public perl-devel-checkbin
  (package
    (name "perl-devel-checkbin")
    (version "0.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOKUHIROM/"
                           "Devel-CheckBin-" version ".tar.gz"))
       (sha256
        (base32
         "1r735yzgvsxkj4m6ks34xva5m21cfzp9qiis2d4ivv99kjskszqm"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/Devel-CheckBin")
    (synopsis "Check that a command is available")
    (description "Devel::CheckBin is a perl module that checks whether a
particular command is available.")
    (license (package-license perl))))

(define-public perl-devel-checklib
  (package
    (name "perl-devel-checklib")
    (version "1.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MA/MATTN/Devel-CheckLib-"
             version ".tar.gz"))
       (sha256
        (base32 "15621qh5gaan1sgmk9y9svl70nm8viw17x5h1kf0zknkk8lmw77j"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-mock-config" ,perl-mock-config)))
    (home-page "https://metacpan.org/release/Devel-CheckLib")
    (synopsis "Check that a library is available")
    (description
     "@code{Devel::CheckLib} is a Perl module that checks whether a particular
C library and its headers are available.  You can also check for the presence of
particular functions in a library, or even that those functions return
particular results.")
    (license license:perl-license)))

(define-public perl-devel-checkcompiler
  (package
  (name "perl-devel-checkcompiler")
  (version "0.07")
  (source (origin
            (method url-fetch)
            (uri (string-append "mirror://cpan/authors/id/S/SY/SYOHEX/"
                                "Devel-CheckCompiler-" version ".tar.gz"))
            (sha256
             (base32
              "1db973a4dbyknjxq608hywil5ai6vplnayshqxrd7m5qnjbpd2vn"))))
  (build-system perl-build-system)
  (native-inputs
   `(("perl-module-build-tiny" ,perl-module-build-tiny)))
  (home-page "https://metacpan.org/release/Devel-CheckCompiler")
  (synopsis "Check compiler availability")
  (description "@code{Devel::CheckCompiler} is a tiny module to check
whether a compiler is available.  It can test for a C99 compiler, or
you can tell it to compile a C source file with optional linker flags.")
  (license (package-license perl))))

(define-public perl-devel-cycle
  (package
    (name "perl-devel-cycle")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/L/LD/LDS/Devel-Cycle-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1hhb77kz3dys8yaik452j22cm3510zald2mpvfyv5clqv326aczx"))))
    (build-system perl-build-system)
    (home-page
     "https://metacpan.org/release/Devel-Cycle")
    (synopsis "Find memory cycles in objects")
    (description
     "@code{Devel::Cycle} This is a tool for finding circular references in
objects and other types of references.  Because of Perl's reference-count
based memory management, circular references will cause memory leaks.")
    (license license:perl-license)))

(define-public perl-devel-globaldestruction
  (package
    (name "perl-devel-globaldestruction")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Devel-GlobalDestruction-" version ".tar.gz"))
       (sha256
        (base32
         "1aslj6myylsvzr0vpqry1cmmvzbmpbdcl4v9zrl18ccik7rabf1l"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-sub-exporter-progressive" ,perl-sub-exporter-progressive)))
    (home-page "https://metacpan.org/release/Devel-GlobalDestruction")
    (synopsis "Provides equivalent of ${^GLOBAL_PHASE} eq 'DESTRUCT' for older perls")
    (description "Devel::GlobalDestruction provides a function returning the
equivalent of \"$@{^GLOBAL_PHASE@} eq 'DESTRUCT'\" for older perls.")
    (license (package-license perl))))

(define-public perl-devel-hide
  (package
    (name "perl-devel-hide")
    (version "0.0010")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FE/FERREIRA/Devel-Hide-"
                           version ".tar.gz"))
       (sha256
        (base32 "10jyv9nmv513hs75rls5yx2xn82513xnnhjir3dxiwgb1ykfyvvm"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (home-page "https://metacpan.org/release/Devel-Hide")
    (synopsis "Forces the unavailability of specified Perl modules (for testing)")
    (description "Given a list of Perl modules/filenames, this module makes
@code{require} and @code{use} statements fail (no matter whether the specified
files/modules are installed or not).")
    (license (package-license perl))))

(define-public perl-devel-leak
  (package
    (name "perl-devel-leak")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NI/NI-S/"
                           "Devel-Leak-" version ".tar.gz"))
       (sha256
        (base32
         "0lkj2xwc3lhxv7scl43r8kfmls4am0b98sqf5vmf7d72257w6hkg"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Devel-Leak")
    (synopsis "Utility for looking for perl objects that are not reclaimed")
    (description
     "This module provides a basic way to discover if a piece of perl code is
allocating perl data and not releasing them again.")
    (license license:perl-license)))

(define-public perl-devel-lexalias
  (package
    (name "perl-devel-lexalias")
    (version "0.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Devel-LexAlias-" version ".tar.gz"))
       (sha256
        (base32
         "0wpfpjqlrncslnmxa37494sfdy0901510kj2ds2k6q167vadj2jy"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-devel-caller" ,perl-devel-caller)))
    (home-page "https://metacpan.org/release/Devel-LexAlias")
    (synopsis "Alias lexical variables")
    (description "Devel::LexAlias provides the ability to alias a lexical
variable in a subroutines scope to one of your choosing.")
    (license (package-license perl))))

(define-public perl-devel-overloadinfo
  (package
    (name "perl-devel-overloadinfo")
    (version "0.005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "Devel-OverloadInfo-" version ".tar.gz"))
       (sha256
        (base32
         "1rx6g8pyhi7lx6z130b7vlf8syzrq92w9ky8mpw4d6bwlkzy5zcb"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-package-stash" ,perl-package-stash)
       ("perl-sub-identify" ,perl-sub-identify)
       ("perl-mro-compat" ,perl-mro-compat)))
    (home-page "https://metacpan.org/release/Devel-OverloadInfo")
    (synopsis "Introspect overloaded operators")
    (description "Devel::OverloadInfo returns information about overloaded
operators for a given class (or object), including where in the inheritance
hierarchy the overloads are declared and where the code implementing it is.")
    (license (package-license perl))))

(define-public perl-devel-partialdump
  (package
    (name "perl-devel-partialdump")
    (version "0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Devel-PartialDump-" version ".tar.gz"))
       (sha256
        (base32
         "0i1khiyi4h4h8vfwn7xip5c53z2hb2rk6407f3csvrdsiibvy53q"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-test-warn" ,perl-test-warn)
       ("perl-test-simple" ,perl-test-simple)))
    (propagated-inputs
     `(("perl-class-tiny" ,perl-class-tiny)
       ("perl-sub-exporter" ,perl-sub-exporter)
       ("perl-namespace-clean" ,perl-namespace-clean)))
    (home-page "https://metacpan.org/release/Devel-PartialDump")
    (synopsis "Partial dumping of data structures")
    (description "This module is a data dumper optimized for logging of
arbitrary parameters.")
    (license (package-license perl))))

(define-public perl-devel-stacktrace
  (package
    (name "perl-devel-stacktrace")
    (version "2.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Devel-StackTrace-" version ".tar.gz"))
       (sha256
        (base32 "0mb8bngjq7s3kbh95h3ig4p3jfb156c4r0d53z344gbxaknh6g6d"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Devel-StackTrace")
    (synopsis "Object representing a stack trace")
    (description "The Devel::StackTrace module contains two classes,
Devel::StackTrace and Devel::StackTrace::Frame.  These objects encapsulate the
information that can be retrieved via Perl's caller() function, as well as
providing a simple interface to this data.")
    (license license:artistic2.0)))

(define-public perl-devel-stacktrace-ashtml
  (package
    (name "perl-devel-stacktrace-ashtml")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Devel-StackTrace-AsHTML-" version ".tar.gz"))
       (sha256
        (base32
         "0iri5nb2lb76qv5l9z0vjpfrq5j2fyclkd64kh020bvy37idp0v2"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-devel-stacktrace" ,perl-devel-stacktrace)))
    (home-page "https://metacpan.org/release/Devel-StackTrace-AsHTML")
    (synopsis "Displays stack trace in HTML")
    (description "Devel::StackTrace::AsHTML adds as_html method to
Devel::StackTrace which displays the stack trace in beautiful HTML, with code
snippet context and function parameters.  If you call it on an instance of
Devel::StackTrace::WithLexicals, you even get to see the lexical variables of
each stack frame.")
    (license (package-license perl))))

(define-public perl-devel-symdump
  (package
    (name "perl-devel-symdump")
    (version "2.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AN/ANDK/"
                           "Devel-Symdump-" version ".tar.gz"))
       (sha256
        (base32
         "1h3n0w23camhj20a97nw7v40rqa7xcxx8vkn2qjjlngm0yhq2vw2"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Devel-Symdump")
    (synopsis "Dump symbol names or the symbol table")
    (description "Devel::Symdump provides access to the perl symbol table.")
    (license (package-license perl))))

(define-public perl-digest-crc
  (package
    (name "perl-digest-crc")
    (version "0.22")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/O/OL/OLIMAUL/Digest-CRC-"
            version ".2.tar.gz"))
      (sha256
       (base32
        "1jvqcyrbi11cj3vlfc9sq2g6rv9caizyjkjqsksvmxn6zgvm0aqi"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Digest-CRC")
    (synopsis "Generic CRC functions")
    (description "The @code{Digest::CRC} module calculates CRC sums of
all sorts.  It contains wrapper functions with the correct parameters
for CRC-CCITT, CRC-16 and CRC-32.")
    (license license:public-domain)))

(define-public perl-digest-hmac
  (package
    (name "perl-digest-hmac")
    (version "1.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AR/ARODLAND/"
                           "Digest-HMAC-" version ".tar.gz"))
       (sha256
        (base32 "1m4fn0w3hb4vn7k5kja508a5hjmcrm28zhdpjkbl8p17m9b83g6n"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Digest-HMAC")
    (synopsis "Keyed-Hashing for Message Authentication")
    (description "The Digest::HMAC module follows the common Digest::
interface for the RFC 2104 HMAC mechanism.")
    (license (package-license perl))))

(define-public perl-digest-md4
  (package
    (name "perl-digest-md4")
    (version "1.9")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/M/MI/MIKEM/DigestMD4/Digest-MD4-"
            version ".tar.gz"))
      (sha256
       (base32
        "19ma1hmvgiznq95ngzvm6v4dfxc9zmi69k8iyfcg6w14lfxi0lb6"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Digest-MD4")
    (synopsis "Interface to the MD4 Algorithm")
    (description "The @code{Digest::MD4} module allows you to use the
RSA Data Security Inc.@: MD4 Message Digest algorithm from within Perl
programs.  The algorithm takes as input a message of arbitrary length
and produces as output a 128-bit \"fingerprint\" or \"message digest\"
of the input.  MD4 is described in RFC 1320.")
    (license license:perl-license)))

(define-public perl-digest-md5
  (package
    (name "perl-digest-md5")
    (version "2.58")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TODDR/Digest-MD5-"
                           version ".tar.gz"))
       (sha256
        (base32 "057psy6k7im0pr3344ny6k5rsnbqj8aizkmwgw53kbbngabh20kx"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Digest-MD5")
    (synopsis "Perl interface to the MD-5 algorithm")
    (description
     "The @code{Digest::MD5} module allows you to use the MD5 Message Digest
algorithm from within Perl programs.  The algorithm takes as
input a message of arbitrary length and produces as output a
128-bit \"fingerprint\" or \"message digest\" of the input.")
    (license (package-license perl))))

(define-public perl-digest-sha1
  (package
    (name "perl-digest-sha1")
    (version "2.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/G/GA/GAAS/"
                                  "Digest-SHA1-" version ".tar.gz"))
              (sha256
               (base32
                "1k23p5pjk42vvzg8xcn4iwdii47i0qm4awdzgbmz08bl331dmhb8"))))
    (build-system perl-build-system)
    (synopsis "Perl implementation of the SHA-1 message digest algorithm")
    (description
     "This package provides @code{Digest::SHA1}, an implementation of the NIST
SHA-1 message digest algorithm for use by Perl programs.")
    (home-page "https://metacpan.org/release/Digest-SHA1")
    (license (package-license perl))))

(define-public perl-dist-checkconflicts
  (package
    (name "perl-dist-checkconflicts")
    (version "0.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                                  "Dist-CheckConflicts-" version ".tar.gz"))
              (sha256
               (base32
                "1i7dr9jpdiy2nijl2p4q5zg2q2s9ckbj2hs4kmnnckf9hsb4p17a"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-module-runtime" ,perl-module-runtime)))
    (home-page "https://metacpan.org/release/Dist-CheckConflicts")
    (synopsis "Declare version conflicts for your dist")
    (description "This module allows you to specify conflicting versions of
modules separately and deal with them after the module is done installing.")
    (license (package-license perl))))

(define-public perl-dynaloader-functions
  (package
    (name "perl-dynaloader-functions")
    (version "0.003")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/DynaLoader-Functions-"
            version ".tar.gz"))
      (sha256
       (base32
        "10x13q920j9kid7vmbj6fiaz153042dy4mwdmpzrdrxw2ir39ciy"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (home-page "https://metacpan.org/release/DynaLoader-Functions")
    (synopsis "Deconstructed dynamic C library loading")
    (description "This module provides a function-based interface to
dynamic loading as used by Perl.  Some details of dynamic loading are
very platform-dependent, so correct use of these functions requires
the programmer to be mindfulof the space of platform variations.")
    (license license:perl-license)))

(define-public perl-encode-detect
  (package
    (name "perl-encode-detect")
    (version "1.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JG/JGMYERS/"
                           "Encode-Detect-" version ".tar.gz"))
       (sha256
        (base32
         "1wdv9ffgs4xyfh5dnh09dqkmmlbf5m1hxgdgb3qy6v6vlwx8jkc3"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/Encode-Detect")
    (synopsis "Detect the encoding of data")
    (description "This package provides a class @code{Encode::Detect} to detect
the encoding of data.")
    (license license:mpl1.1)))

(define-public perl-encode-eucjpascii
  (package
    (name "perl-encode-eucjpascii")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEZUMI/"
                           "Encode-EUCJPASCII-" version ".tar.gz"))
       (sha256
        (base32
         "0qg8kmi7r9jcf8326b4fyq5sdpqyim2a11h7j77q577xam6x767r"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Encode-EUCJPASCII")
    (synopsis "ASCII mapping for eucJP encoding")
    (description "This package provides an ASCII mapping for the eucJP
encoding.")
    (license (package-license perl))))

(define-public perl-encode-jis2k
  (package
    (name "perl-encode-jis2k")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DANKOGAI/"
                           "Encode-JIS2K-" version ".tar.gz"))
       (sha256
        (base32
         "1k1mdj4rd9m1z4h7qd2dl92ky0r1rk7mmagwsvdb9pirvdr4vj0y"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Encode-JIS2K")
    (synopsis "JIS X 0212 (aka JIS 2000) encodings")
    (description "This package provides encodings for JIS X 0212, which is
also known as JIS 2000.")
    (license (package-license perl))))

(define-public perl-encode-hanextra
  (package
    (name "perl-encode-hanextra")
    (version "0.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AU/AUDREYT/"
                           "Encode-HanExtra-" version ".tar.gz"))
       (sha256
        (base32
         "0fj4vd8iva2i0j6s2fyhwgr9afrvhr6gjlzi7805h257mmnb1m0z"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _ (setenv "PERL_USE_UNSAFE_INC" "1") #t)))))
    (home-page "https://metacpan.org/release/Encode-HanExtra")
    (synopsis "Additional Chinese encodings")
    (description "This Perl module provides Chinese encodings that are not
part of Perl by default, including \"BIG5-1984\", \"BIG5-2003\", \"BIG5PLUS\",
\"BIG5EXT\", \"CCCII\", \"EUC-TW\", \"CNS11643-*\", \"GB18030\", and
\"UNISYS\".")
    (license license:expat)))

(define-public perl-env-path
  (package
    (name "perl-env-path")
    (version "0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DS/DSB/Env-Path-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1qhmj15a66h90pjl2dgnxsb9jj3b1r5mpvnr87cafcl8g69z0jr4"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Env-Path")
    (synopsis "Advanced operations on path variables")
    (description "@code{Env::Path} presents an object-oriented interface to
path variables, defined as that subclass of environment variables which name
an ordered list of file system elements separated by a platform-standard
separator.")
    (license (package-license perl))))

(define-public perl-error
  (package
    (name "perl-error")
    (version "0.17028")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                                  "Error-" version ".tar.gz"))
              (sha256
               (base32
                "0q796nwwiarfc6pga97380c9z8xva5545632001qj75kb1g5rn1s"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/Error")
    (synopsis "OO-ish Error/Exception handling for Perl")
    (description "The Error package provides two interfaces.  Firstly Error
provides a procedural interface to exception handling.  Secondly Error is a
base class for errors/exceptions that can either be thrown, for subsequent
catch, or can simply be recorded.")
    (license (package-license perl))))

(define-public perl-eval-closure
  (package
    (name "perl-eval-closure")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                           "Eval-Closure-" version ".tar.gz"))
       (sha256
        (base32
         "1bcc47r6zm3hfr6ccsrs72kgwxm3wkk07mgnpsaxi67cypr482ga"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-devel-lexalias" ,perl-devel-lexalias)))
    (home-page "https://metacpan.org/release/Eval-Closure")
    (synopsis "Safely and cleanly create closures via string eval")
    (description "String eval is often used for dynamic code generation.  For
instance, Moose uses it heavily, to generate inlined versions of accessors and
constructors, which speeds code up at runtime by a significant amount.  String
eval is not without its issues however - it's difficult to control the scope
it's used in (which determines which variables are in scope inside the eval),
and it's easy to miss compilation errors, since eval catches them and sticks
them in $@@ instead.  This module attempts to solve these problems.  It
provides an eval_closure function, which evals a string in a clean
environment, other than a fixed list of specified variables.  Compilation
errors are rethrown automatically.")
    (license (package-license perl))))

(define-public perl-eval-withlexicals
  (package
    (name "perl-eval-withlexicals")
    (version "1.003006")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/H/HA/HAARG/Eval-WithLexicals-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0x09mq0q745cxkw3xgr0h7dil7p1pdq3l5299kj3mk2ijkk2gwb6"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap-tinyrepl
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (wrap-program (string-append out "/bin/tinyrepl")
                          `("PERL5LIB" ":" prefix
                            (,(getenv "PERL5LIB")
                             ,(string-append out "/lib/perl5/site_perl"))))
                        #t))))))
    (propagated-inputs
     `(("perl-moo" ,perl-moo)
       ("perl-strictures" ,perl-strictures)))
    (home-page "https://metacpan.org/release/Eval-WithLexicals")
    (synopsis "Lexical scope evaluation library for Perl")
    (description "The Eval::WithLexicals Perl library provides support for
lexical scope evaluation.  This package also includes the @command{tinyrepl}
command, which can be used as a minimal Perl read-eval-print loop (REPL).")
    (license (package-license perl))))

(define-public perl-exception-class
  (package
    (name "perl-exception-class")
    (version "1.44")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Exception-Class-" version ".tar.gz"))
       (sha256
        (base32
         "03gf4cdgrjnljgrlxkvbh2cahsyzn0zsh2zcli7b1lrqn7wgpwrk"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-devel-stacktrace" ,perl-devel-stacktrace)
       ("perl-class-data-inheritable" ,perl-class-data-inheritable)))
    (home-page "https://metacpan.org/release/Exception-Class")
    (synopsis "Allows you to declare real exception classes in Perl")
    (description "Exception::Class allows you to declare exception hierarchies
in your modules in a \"Java-esque\" manner.")
    (license (package-license perl))))

(define-public perl-exporter-lite
  (package
    (name "perl-exporter-lite")
    (version "0.08")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                                  "Exporter-Lite-" version ".tar.gz"))
              (sha256
               (base32
                "1hns15imih8z2h6zv3m1wwmv9fiysacsb52y94v6zf2cmw4kjny0"))))
    (build-system perl-build-system)
    (synopsis "Lightweight exporting of functions and variables")
    (description
     "Exporter::Lite is an alternative to Exporter, intended to provide a
lightweight subset of the most commonly-used functionality.  It supports
import(), @@EXPORT and @@EXPORT_OK and not a whole lot else.")
    (home-page "https://metacpan.org/release/Exporter-Lite")
    (license (package-license perl))))

(define-public perl-exporter-tiny
  (package
    (name "perl-exporter-tiny")
    (version "1.002001")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOBYINK/"
                           "Exporter-Tiny-" version ".tar.gz"))
       (sha256
        (base32 "13f4sd9n9iyi15r5rbjbmawajxlgfdvvyrvwlyg0yjyf09636b58"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Exporter-Tiny")
    (synopsis "Exporter with the features of Sub::Exporter but only core dependencies")
    (description "Exporter::Tiny supports many of Sub::Exporter's
external-facing features including renaming imported functions with the `-as`,
`-prefix` and `-suffix` options; explicit destinations with the `into` option;
and alternative installers with the `installler` option.  But it's written in
only about 40% as many lines of code and with zero non-core dependencies.")
    (license (package-license perl))))

(define-public perl-extutils-installpaths
  (package
    (name "perl-extutils-installpaths")
    (version "0.012")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "ExtUtils-InstallPaths-" version ".tar.gz"))
       (sha256
        (base32
         "1v9lshfhm9ck4p0v77arj5f7haj1mmkqal62lgzzvcds6wq5www4"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-extutils-config" ,perl-extutils-config)))
    (home-page "https://metacpan.org/release/ExtUtils-InstallPaths")
    (synopsis "Build.PL install path logic made easy")
    (description "This module tries to make install path resolution as easy as
possible.")
    (license (package-license perl))))

(define-public perl-extutils-config
  (package
    (name "perl-extutils-config")
    (version "0.008")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "ExtUtils-Config-" version ".tar.gz"))
       (sha256
        (base32
         "130s5zk4krrymbynqxx62g13jynnb7xi7vdpg65cw3b56kv08ldf"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/ExtUtils-Config")
    (synopsis "Wrapper for perl's configuration")
    (description "ExtUtils::Config is an abstraction around the %Config hash.
By itself it is not a particularly interesting module by any measure, however
it ties together a family of modern toolchain modules.")
    (license (package-license perl))))

(define-public perl-extutils-cppguess
  (package
    (name "perl-extutils-cppguess")
    (version "0.20")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/E/ET/ETJ/ExtUtils-CppGuess-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0q9ynigk600fv95xac6aslrg2k19m6qbzf5hqfsnall8113r3gqj"))))
    (build-system perl-build-system)
    (native-inputs
      `(("perl-capture-tiny" ,perl-capture-tiny)
        ("perl-module-build" ,perl-module-build)))
    (propagated-inputs
      `(("perl-capture-tiny" ,perl-capture-tiny)))
    (home-page
      "https://metacpan.org/release/ExtUtils-CppGuess")
    (synopsis "Tool for guessing C++ compiler and flags")
    (description "ExtUtils::CppGuess attempts to guess the C++ compiler that
is compatible with the C compiler used to build perl.")
    (license (package-license perl))))

(define-public perl-extutils-depends
  (package
    (name "perl-extutils-depends")
    (version "0.405")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/"
                                  "ExtUtils-Depends-" version ".tar.gz"))
              (sha256
               (base32
                "0b4ab9qmcihsfs2ajhn5qzg7nhazr68v3r0zvb7076smswd41mla"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-number-delta" ,perl-test-number-delta)))
    (home-page "https://metacpan.org/release/ExtUtils-Depends")
    (synopsis "Easily build XS extensions that depend on XS extensions")
    (description
     "This module tries to make it easy to build Perl extensions that use
functions and typemaps provided by other perl extensions.  This means that a
perl extension is treated like a shared library that provides also a C and an
XS interface besides the perl one.")
    (license (package-license perl))))

(define-public perl-extutils-helpers
  (package
    (name "perl-extutils-helpers")
    (version "0.026")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "ExtUtils-Helpers-" version ".tar.gz"))
       (sha256
        (base32
         "05ilqcj1rg5izr09dsqmy5di4fvq6ph4k0chxks7qmd4j1kip46y"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/ExtUtils-Helpers")
    (synopsis "Various portability utilities for module builders")
    (description "This module provides various portable helper functions for
module building modules.")
    (license (package-license perl))))

(define-public perl-extutils-libbuilder
  (package
    (name "perl-extutils-libbuilder")
    (version "0.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AM/AMBS/"
                           "ExtUtils-LibBuilder-" version ".tar.gz"))
       (sha256
        (base32
         "1lmmfcjxvsvhn4f3v2lyylgr8dzcf5j7mnd1pkq3jc75dph724f5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/ExtUtils-LibBuilder")
    (synopsis "Tool to build C libraries")
    (description "Some Perl modules need to ship C libraries together with
their Perl code.  Although there are mechanisms to compile and link (or glue)
C code in your Perl programs, there isn't a clear method to compile standard,
self-contained C libraries.  This module main goal is to help in that task.")
    (license (package-license perl))))

(define-public perl-extutils-parsexs
  (package
    (name "perl-extutils-parsexs")
    (version "3.35")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/S/SM/SMUELLER/ExtUtils-ParseXS-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "077fqiyabydm8j34wxzxwxskyidh8nmwq9gskaxai8kq298z1pj1"))))
    (build-system perl-build-system)
    (home-page
      "https://metacpan.org/release/ExtUtils-ParseXS")
    (synopsis "Module to convert Perl XS code into C code")
    (description "The package contains the ExtUtils::ParseXS module to
convert Perl XS code into C code, the ExtUtils::Typemaps module to
handle Perl/XS typemap files, and their submodules.")
    (license (package-license perl))))

(define-public perl-extutils-pkgconfig
  (package
    (name "perl-extutils-pkgconfig")
    (version "1.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/"
                                  "ExtUtils-PkgConfig-" version ".tar.gz"))
              (sha256
               (base32
                "0vhwh0731rhh1sswmvagq0myn754dnkab8sizh6d3n6pjpcwxsmv"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://metacpan.org/release/ExtUtils-PkgConfig")
    (synopsis "Simplistic interface to pkg-config")
    (description
     "@code{ExtUtils::PkgConfig} is a very simplistic interface to the
@command{pkg-config} utility, intended for use in the @file{Makefile.PL}
of perl extensions which bind libraries that @command{pkg-config} knows.
It is really just boilerplate code that you would have written yourself.")
    (license license:lgpl2.1+)))

(define-public perl-extutils-typemaps-default
  (package
    (name "perl-extutils-typemaps-default")
    (version "1.05")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/S/SM/SMUELLER/ExtUtils-Typemaps-Default-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1phmha0ks95kvzl00r1kgnd5hvg7qb1q9jmzjmw01p5zgs1zbyix"))))
    (build-system perl-build-system)
    (native-inputs
      `(("perl-module-build" ,perl-module-build)))
    (home-page
      "https://metacpan.org/release/ExtUtils-Typemaps-Default")
    (synopsis "Set of useful typemaps")
    (description "The package provides a number of useful typemaps as
submodules of ExtUtils::Typemaps.")
    (license (package-license perl))))

(define-public perl-extutils-xspp
  (package
    (name "perl-extutils-xspp")
    (version "0.18")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/S/SM/SMUELLER/ExtUtils-XSpp-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1zx84f93lkymqz7qa4d63gzlnhnkxm5i3gvsrwkvvqr9cxjasxli"))))
    (build-system perl-build-system)
    (native-inputs
      `(("perl-module-build" ,perl-module-build)
        ("perl-test-base" ,perl-test-base)
        ("perl-test-differences" ,perl-test-differences)))
    (home-page
      "https://metacpan.org/release/ExtUtils-XSpp")
    (synopsis "XS for C++")
    (description "This module implements the Perl foreign function
interface XS for C++; it is a thin layer over plain XS.")
    (license (package-license perl))))

(define-public perl-file-changenotify
  (package
    (name "perl-file-changenotify")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "File-ChangeNotify-" version ".tar.gz"))
       (sha256
        (base32
         "090i265f73jlcl5rv250791vw32j9vvl4nd5abc7myg0klb8109w"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-class-load" ,perl-class-load)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-module-pluggable" ,perl-module-pluggable)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-params-validate" ,perl-moosex-params-validate)
       ("perl-moosex-semiaffordanceaccessor"
        ,perl-moosex-semiaffordanceaccessor)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page "https://metacpan.org/release/File-ChangeNotify")
    (synopsis "Watch for changes to files")
    (description "This module provides a class to monitor a directory for
changes made to any file.")
    (license license:artistic2.0)))

(define-public perl-file-configdir
  (package
    (name "perl-file-configdir")
    (version "0.021")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "File-ConfigDir-" version ".tar.gz"))
       (sha256
        (base32
         "1ihlhdbwaybyj3xqfxpx4ii0ypa41907b6zdh94rvr4wyqa5lh3b"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-homedir" ,perl-file-homedir)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-test-without-module" ,perl-test-without-module)))
    (home-page "https://metacpan.org/release/File-ConfigDir")
    (synopsis "Get directories of configuration files")
    (description "This module is a helper for installing, reading and finding
configuration file locations.  @code{File::ConfigDir} is a module to help out
when Perl modules (especially applications) need to read and store
configuration files from more than one location.")
    (license (package-license perl))))

(define-public perl-file-copy-recursive
  (package
    (name "perl-file-copy-recursive")
    (version "0.38")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DM/DMUEY/"
                           "File-Copy-Recursive-" version ".tar.gz"))
       (sha256
        (base32
         "1syyyvylr51iicialdmv0dw06q49xzv8zrkb5cn8ma4l73gvvk44"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/File-Copy-Recursive")
    (synopsis "Recursively copy files and directories")
    (description "This module has 3 functions: one to copy files only, one to
copy directories only, and one to do either depending on the argument's
type.")
    (license (package-license perl))))

(define-public perl-file-find-rule
  (package
    (name "perl-file-find-rule")
    (version "0.34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "File-Find-Rule-" version ".tar.gz"))
       (sha256
        (base32
         "1znachnhmi1w5pdqx8dzgfa892jb7x8ivrdy4pzjj7zb6g61cvvy"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-text-glob" ,perl-text-glob)
       ("perl-number-compare" ,perl-number-compare)))
    (home-page "https://metacpan.org/release/File-Find-Rule")
    (synopsis "Alternative interface to File::Find")
    (description "File::Find::Rule is a friendlier interface to File::Find.
It allows you to build rules which specify the desired files and
directories.")
    (license (package-license perl))))

(define-public perl-file-find-rule-perl
  (package
    (name "perl-file-find-rule-perl")
    (version "1.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "File-Find-Rule-Perl-" version ".tar.gz"))
       (sha256
        (base32
         "19iy8spzrvh71x33b5yi16wjw5jjvs12jvjj0f7f3370hqzl6j4s"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-find-rule" ,perl-file-find-rule)
       ("perl-params-util" ,perl-params-util)
       ("perl-parse-cpan-meta" ,perl-parse-cpan-meta)))
    (home-page "https://metacpan.org/release/File-Find-Rule-Perl")
    (synopsis "Common rules for searching for Perl things")
    (description "File::Find::Rule::Perl provides methods for finding various
types Perl-related files, or replicating search queries run on a distribution
in various parts of the CPAN ecosystem.")
    (license (package-license perl))))

(define-public perl-file-grep
  (package
    (name "perl-file-grep")
    (version "0.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MN/MNEYLON/File-Grep-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0cjnz3ak7s3x3y3q48xb9ka2q9d7xvch58vy80hqa9xn9qkiabj6"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/File-Grep")
    (synopsis "Matches patterns in a series of files")
    (description "@code{File::Grep} provides similar functionality as perl's
builtin @code{grep}, @code{map}, and @code{foreach} commands, but iterating
over a passed filelist instead of arrays.  While trivial, this module can
provide a quick dropin when such functionality is needed.")
    (license (package-license perl))))

(define-public perl-file-homedir
  (package
    (name "perl-file-homedir")
    (version "1.004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "File-HomeDir-" version ".tar.gz"))
       (sha256
        (base32
         "1bciyzwv7gwsnaykqz0czj6mlbkkg4hg1s40s1q7j2p6nlmpxxj5"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-which" ,perl-file-which)))
    (arguments `(#:tests? #f))          ;Not appropriate for chroot
    (home-page "https://metacpan.org/release/File-HomeDir")
    (synopsis "Find your home and other directories on any platform")
    (description "File::HomeDir is a module for locating the directories that
are @code{owned} by a user (typically your user) and to solve the various issues
that arise trying to find them consistently across a wide variety of
platforms.")
    (license (package-license perl))))

(define-public perl-file-path
  (package
    (name "perl-file-path")
    (version "2.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/J/JK/JKEENAN/File-Path-"
             version
             ".tar.gz"))
       (sha256
        (base32 "01gsysg9mjkh1ckk7jhj3y8vs291a5ynkgzhqmcz90f3b6dxdxr1"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/File-Path")
    (synopsis "Create or remove directory trees")
    (description "This module provide a convenient way to create directories
of arbitrary depth and to delete an entire directory subtree from the
file system.")
    (license (package-license perl))))

(define-public perl-file-pushd
  (package
    (name "perl-file-pushd")
    (version "1.016")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DA/DAGOLDEN/File-pushd-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1p3wz5jnddd87wkwl4x3fc3ncprahdxdzwqd4scb10r98h4pyfnp"))))
    (build-system perl-build-system)
    (home-page
     "https://metacpan.org/release/File-pushd")
    (synopsis
     "Change directory temporarily for a limited scope")
    (description "@code{File::pushd} does a temporary @code{chdir} that is
easily and automatically reverted, similar to @code{pushd} in some Unix
command shells.  It works by creating an object that caches the original
working directory.  When the object is destroyed, the destructor calls
@code{chdir} to revert to the original working directory.  By storing the
object in a lexical variable with a limited scope, this happens automatically
at the end of the scope.")
    (license license:asl2.0)))

(define-public perl-file-list
  (package
    (name "perl-file-list")
    (version "0.3.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/D/DO/DOPACKI/File-List-"
                   version ".tar.gz"))
             (sha256
              (base32
               "00m5ax4aq59hdvav6yc4g63vhx3a57006rglyypagvrzfxjvm8s8"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'cd
           (lambda _ (chdir "List") #t)))))
    (license (package-license perl))
    (synopsis "Perl extension for crawling directory trees and compiling
lists of files")
    (description
     "The File::List module crawls the directory tree starting at the
provided base directory and can return files (and/or directories if desired)
matching a regular expression.")
    (home-page "https://metacpan.org/release/File-List")))

(define-public perl-file-readbackwards
  (package
    (name "perl-file-readbackwards")
    (version "1.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/U/UR/URI/File-ReadBackwards-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0vldy5q0zyf1cwzwb1gv14f8vg2f21bw96b8wvkw6z2hhypn3cl2"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/File-ReadBackwards")
    (synopsis "Read a file backwards by lines")
    (description "This module reads a file backwards line by line. It is
simple to use, memory efficient and fast.  It supports both an object and a
tied handle interface.

It is intended for processing log and other similar text files which typically
have their newest entries appended to them.  By default files are assumed to
be plain text and have a line ending appropriate to the OS.  But you can set
the input record separator string on a per file basis.")
    (license license:perl-license)))

(define-public perl-file-remove
  (package
    (name "perl-file-remove")
    (version "1.58")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                           "File-Remove-" version ".tar.gz"))
       (sha256
        (base32
         "1n6h5w3sp2bs4cfrifdx2z15cfpb4r536179mx1a12xbmj1yrxl1"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/File-Remove")
    (synopsis "Remove files and directories in Perl")
    (description "@code{File::Remove::remove} removes files and directories.
It acts like @code{/bin/rm}, for the most part.  Although @code{unlink} can be
given a list of files, it will not remove directories; this module remedies
that.  It also accepts wildcards, * and ?, as arguments for file names.")
    (license (package-license perl))))

(define-public perl-file-sharedir
  (package
    (name "perl-file-sharedir")
    (version "1.116")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "File-ShareDir-" version ".tar.gz"))
       (sha256
        (base32 "0a43rfb0a1fpxh4d2dayarkdxw4cx9a2krkk87zmcilcz7yhpnar"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-file-sharedir-install" ,perl-file-sharedir-install)))
    (propagated-inputs
     `(("perl-class-inspector" ,perl-class-inspector)))
    (home-page "https://metacpan.org/release/File-ShareDir")
    (synopsis "Locate per-dist and per-module shared files")
    (description "The intent of File::ShareDir is to provide a companion to
Class::Inspector and File::HomeDir.  Quite often you want or need your Perl
module to have access to a large amount of read-only data that is stored on
the file-system at run-time.  Once the files have been installed to the
correct directory, you can use File::ShareDir to find your files again after
the installation.")
    (license (package-license perl))))

(define-public perl-file-sharedir-dist
  (package
    (name "perl-file-sharedir-dist")
    (version "0.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PL/PLICEASE/"
                           "File-ShareDir-Dist-" version ".tar.gz"))
       (sha256
        (base32 "0vg8kxzgz4hf6221jb4v5bx1zhsnplnw5bcmxx0iyd92xv8fazwd"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/File-ShareDir-Dist")
    (synopsis "Locate per-dist shared files")
    (description "File::ShareDir::Dist finds share directories for
distributions.  It is a companion module to File::ShareDir.")
    (license (package-license perl))))

(define-public perl-file-sharedir-install
  (package
    (name "perl-file-sharedir-install")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "File-ShareDir-Install-" version ".tar.gz"))
       (sha256
        (base32
         "1yc0wlkav2l2wr36a53n4mnhsy2zv29z5nm14mygxgjwv7qgvgj5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/File-ShareDir-Install")
    (synopsis "Install shared files")
    (description "File::ShareDir::Install allows you to install read-only data
files from a distribution.  It is a companion module to File::ShareDir, which
allows you to locate these files after installation.")
    (license (package-license perl))))

(define-public perl-file-slurp
  (package
    (name "perl-file-slurp")
    (version "9999.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CA/CAPOEIRAB/"
                           "File-Slurp-" version ".tar.gz"))
       (sha256
        (base32 "1vkwh880lbyr2qcrfka7yb3z4yz9id4va52gfjgdnyfb1c0wx1q5"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/File-Slurp")
    (synopsis "Reading/Writing/Modifying of complete files")
    (description "File::Slurp provides subroutines to read or write entire
files with a simple call.  It also has a subroutine for reading the list of
file names in a directory.")
    (license (package-license perl))))

(define-public perl-file-slurper
  (package
    (name "perl-file-slurper")
    (version "0.012")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/L/LE/LEONT/File-Slurper-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0y5518ji60yfkx9ggjp309j6g8vfri4ka4zqlsys245i2sj2xysf"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-warnings" ,perl-test-warnings)))
    (propagated-inputs
     `(("perl-perlio-utf8_strict" ,perl-perlio-utf8_strict)))
    (home-page "https://metacpan.org/release/File-Slurper")
    (synopsis "Simple, sane and efficient module to slurp a file")
    (description "This module provides functions for fast and correct file
slurping and spewing.  All functions are optionally exported.")
    (license (package-license perl))))

(define-public perl-file-slurp-tiny
  (package
    (name "perl-file-slurp-tiny")
    (version "0.004")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                                  "File-Slurp-Tiny-" version ".tar.gz"))
              (sha256
               (base32
                "07kzfmibl43dq4c803f022g2rcfv4nkjgipxclz943mzxaz9aaa5"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/File-Slurp-Tiny")
    (synopsis "Simple file reader and writer")
    (description
     "This module provides functions for fast reading and writing of files.")
    (license (package-license perl))))

(define-public perl-file-temp
  (package
    (name "perl-file-temp")
    (version "0.2309")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "File-Temp-" version ".tar.gz"))
       (sha256
        (base32 "0pr3wrxrk93wy7dz9gsb1sgl77icrs8rh2mah6wms5cdi2ll5ch1"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/File-Temp")
    (synopsis "Return name and handle of a temporary file safely")
    (description "File::Temp can be used to create and open temporary files in
a safe way.")
    (license (package-license perl))))

(define-public perl-file-which
  (package
    (name "perl-file-which")
    (version "1.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/P/PL/PLICEASE/"
                                  "File-Which-" version ".tar.gz"))
              (sha256
               (base32
                "0y70qh5kn2hyrrvbsfhg0iws2qggk5vkpz37f7rbd5rd9cjc57dp"))))
    (build-system perl-build-system)
    (native-inputs `(("test-script" ,perl-test-script)))
    (synopsis "Portable implementation of the `which' utility")
    (description
     "File::Which was created to be able to get the paths to executable
programs on systems under which the `which' program wasn't implemented in the
shell.")
    (home-page "https://metacpan.org/release/File-Which")
    (license (package-license perl))))

(define-public perl-file-zglob
  (package
    (name "perl-file-zglob")
    (version "0.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/T/TO/TOKUHIROM/File-Zglob-"
                    version ".tar.gz"))
              (sha256
               (base32
                "16v61rn0yimpv5kp6b20z2f1c93n5kpsyjvr0gq4w2dc43gfvc8w"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (home-page "https://metacpan.org/release/File-Zglob")
    (synopsis "Extended Unix style glob functionality")
    (description "@code{File::Zglob} provides a traditional Unix @code{glob}
functionality; it returns a list of file names that match the given pattern.
For instance, it supports the @code{**/*.pm} form.")
    (license (package-license perl))))

(define-public perl-filesys-notify-simple
  (package
    (name "perl-filesys-notify-simple")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Filesys-Notify-Simple-" version ".tar.gz"))
       (sha256
        (base32
         "18jv96k1pf8wqf4vn2ahs7dv44lc9cyqj0bja9z17qici3dx7qxd"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-sharedfork" ,perl-test-sharedfork)))
    (home-page "https://metacpan.org/release/Filesys-Notify-Simple")
    (synopsis "Simple and dumb file system watcher")
    (description
     "@code{Filesys::Notify::Simple} is a simple but unified interface to get
notifications of changes to a given file system path.  It uses inotify2 on
Linux, fsevents on OS X, @code{kqueue} on FreeBSD, and
@code{FindFirstChangeNotification} on Windows if they're installed, and falls
back to a full directory scan if none of these are available.")
    (license license:perl-license)))

(define-public perl-getopt-long
  (package
    (name "perl-getopt-long")
    (version "2.51")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JV/JV/"
                           "Getopt-Long-" version ".tar.gz"))
       (sha256
        (base32 "0r659i6rkz8zkfgdccbn29zmd4bk9lcdc4y20ng6w2glqaa3pd10"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Getopt-Long")
    (synopsis "Module to handle parsing command line options")
    (description "The @code{Getopt::Long} module implements an extended getopt
function called @code{GetOptions()}.  It parses the command line from
@code{ARGV}, recognizing and removing specified options and their possible
values.

This function adheres to the POSIX syntax for command line options, with GNU
extensions.  In general, this means that options have long names instead of
single letters, and are introduced with a double dash \"--\".  Support for
bundling of command line options, as was the case with the more traditional
single-letter approach, is provided but not enabled by default.")
    ;; Can be used with either license.
    (license (list (package-license perl) license:gpl2+))))

(define-public perl-getopt-long-descriptive
  (package
    (name "perl-getopt-long-descriptive")
    (version "0.103")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Getopt-Long-Descriptive-" version ".tar.gz"))
       (sha256
        (base32
         "1cpl240qxmh7jf85ai9sfkp3nzm99syya4jxidizp7aa83kvmqbh"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-cpan-meta-check" ,perl-cpan-meta-check)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-warnings" ,perl-test-warnings)))
    (propagated-inputs
     `(("perl-params-validate" ,perl-params-validate)
       ("perl-sub-exporter" ,perl-sub-exporter)))
    (home-page "https://metacpan.org/release/Getopt-Long-Descriptive")
    (synopsis "Getopt::Long, but simpler and more powerful")
    (description "Getopt::Long::Descriptive is yet another Getopt library.
It's built atop Getopt::Long, and gets a lot of its features, but tries to
avoid making you think about its huge array of options.  It also provides
usage (help) messages, data validation, and a few other useful features.")
    (license (package-license perl))))

(define-public perl-getopt-tabular
  (package
    (name "perl-getopt-tabular")
    (version "0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/G/GW/GWARD/"
                                  "Getopt-Tabular-" version ".tar.gz"))
              (sha256
               (base32
                "0xskl9lcj07sdfx5dkma5wvhhgf5xlsq0khgh8kk34dm6dv0dpwv"))))
    (build-system perl-build-system)
    (synopsis "Table-driven argument parsing for Perl")
    (description
     "Getopt::Tabular is a Perl 5 module for table-driven argument parsing,
vaguely inspired by John Ousterhout's Tk_ParseArgv.")
    (home-page "https://metacpan.org/release/Getopt-Tabular")
    (license (package-license perl))))

(define-public perl-graph
  (package
    (name "perl-graph")
    (version "0.9704")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/J/JH/JHI/Graph-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "099a1gca0wj5zs0cffncjqp2mjrdlk9i6325ks89ml72gfq8wpij"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Graph")
    (synopsis "Graph data structures and algorithms")
    (description "This is @code{Graph}, a Perl module for dealing with graphs,
the abstract data structures.")
    (license (package-license perl))))

(define-public perl-guard
  (package
    (name "perl-guard")
    (version "1.023")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/Guard-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1p6i9mfmbs9cw40jqdv71ihv2xfi0vvlv8bdv2810gf93zwxvi1l"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Guard")
    (synopsis "Safe cleanup blocks implemented as guards")
    (description "@code{Guard} implements so-called @dfn{guards}.  A guard is
something (usually an object) that \"guards\" a resource, ensuring that it is
cleaned up when expected.

Specifically, this module supports two different types of guards: guard
objects, which execute a given code block when destroyed, and scoped guards,
which are tied to the scope exit.")
    (license (package-license perl))))

(define-public perl-hash-fieldhash
  (package
    (name "perl-hash-fieldhash")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GF/GFUJI/"
                           "Hash-FieldHash-" version ".tar.gz"))
       (sha256
        (base32
         "1wg8nzczfxif55j2nbymbhyd25pjy7dqs4bvd6jrcds3ll3mflaw"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-perl-search-path
           (lambda _
             ;; Work around "dotless @INC" build failure.
             (setenv "PERL5LIB"
                     (string-append (getcwd) ":"
                                    (getenv "PERL5LIB")))
             #t)))))
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-leaktrace" ,perl-test-leaktrace)))
    (home-page "https://metacpan.org/release/Hash-FieldHash")
    (synopsis "Lightweight field hash for inside-out objects")
    (description "@code{Hash::FieldHash} provides the field hash mechanism
which supports the inside-out technique.  It is an alternative to
@code{Hash::Util::FieldHash} with a simpler interface, higher performance, and
relic support.")
    (license (package-license perl))))

(define-public perl-hash-merge
  (package
    (name "perl-hash-merge")
    (version "0.302")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HE/HERMES/"
                           "Hash-Merge-" version ".tar.gz"))
       (sha256
        (base32 "0i46agids6pk445gfck80f8z7q3pjvkp0ip1vmhqnq1rcpvj41df"))))
    (build-system perl-build-system)
    (native-inputs
     ;; For tests only.
     `(("perl-clone" ,perl-clone)
       ("perl-clone-pp" ,perl-clone-pp)))
    (propagated-inputs
     `(("perl-clone-choose" ,perl-clone-choose)))
    (home-page "https://metacpan.org/release/Hash-Merge")
    (synopsis "Merge arbitrarily deep hashes into a single hash")
    (description "Hash::Merge merges two arbitrarily deep hashes into a single
hash.  That is, at any level, it will add non-conflicting key-value pairs from
one hash to the other, and follows a set of specific rules when there are key
value conflicts.  The hash is followed recursively, so that deeply nested
hashes that are at the same level will be merged when the parent hashes are
merged.")
    (license (package-license perl))))

(define-public perl-hash-multivalue
  (package
    (name "perl-hash-multivalue")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AR/ARISTOTLE/"
                           "Hash-MultiValue-" version ".tar.gz"))
       (sha256
        (base32
         "1x3k7h542xnigz0b8vsfiq580p5r325wi5b8mxppiqk8mbvis636"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Hash-MultiValue")
    (synopsis "Store multiple values per key")
    (description "Hash::MultiValue is an object (and a plain hash reference)
that may contain multiple values per key, inspired by MultiDict of WebOb.")
    (license (package-license perl))))

(define-public perl-hook-lexwrap
  (package
    (name "perl-hook-lexwrap")
    (version "0.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Hook-LexWrap-" version ".tar.gz"))
       (sha256
        (base32 "0bgc6w8zs45n6ksgk0zisn9a2vcr3lmzipkan2a94kzrk1gxq2xn"))))
    (build-system perl-build-system)
    (home-page
     "https://metacpan.org/release/Hook-LexWrap")
    (synopsis "Lexically scoped subroutine wrappers")
    (description
     "Hook::LexWrap allows you to install a pre- or post-wrapper (or
both) around an existing subroutine.  Unlike other modules that
provide this capacity (e.g., Hook::PreAndPost and Hook::WrapSub),
Hook::LexWrap implements wrappers in such a way that the standard
caller function works correctly within the wrapped subroutine.")
    (license license:perl-license)))

(define-public perl-importer
  (package
    (name "perl-importer")
    (version "0.025")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/E/EX/EXODIST/Importer-"
                            version ".tar.gz"))
        (sha256
         (base32
          "0iirw6csfbycr6z5s6lgd1zdqdjhb436zcxy1hyh6x3x92616i87"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Importer")
    (synopsis "Alternative but compatible interface to modules that export symbols")
    (description "This module acts as a layer between Exporter and modules which
consume exports.  It is feature-compatible with Exporter, plus some much needed
extras.  You can use this to import symbols from any exporter that follows
Exporters specification.  The exporter modules themselves do not need to use or
inherit from the Exporter module, they just need to set @@EXPORT and/or other
variables.")
    (license (package-license perl))))

(define-public perl-import-into
  (package
    (name "perl-import-into")
    (version "1.002005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Import-Into-" version ".tar.gz"))
       (sha256
        (base32
         "0rq5kz7c270q33jq6hnrv3xgkvajsc62ilqq7fs40av6zfipg7mx"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-module-runtime" ,perl-module-runtime)))
    (home-page "https://metacpan.org/release/Import-Into")
    (synopsis "Import packages into other packages")
    (description "Writing exporters is a pain.  Some use Exporter, some use
Sub::Exporter, some use Moose::Exporter, some use Exporter::Declare ... and
some things are pragmas.  Exporting on someone else's behalf is harder.  The
exporters don't provide a consistent API for this, and pragmas need to have
their import method called directly, since they effect the current unit of
compilation.  Import::Into provides global methods to make this painless.")
    (license (package-license perl))))

(define-public perl-inc-latest
  (package
    (name "perl-inc-latest")
    (version "0.500")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "inc-latest-" version ".tar.gz"))
       (sha256
        (base32
         "04f6qf6ll2hkdsr9aglykg3wlgsnf0w4f264nzg4i9y6cgrhbafs"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/inc-latest")
    (synopsis "Use modules in inc/ if newer than installed")
    (description "The inc::latest module helps bootstrap configure-time
dependencies for CPAN distributions.  These dependencies get bundled into the
inc directory within a distribution and are used by Makefile.PL or Build.PL.")
    (license license:asl2.0)))

(define-public perl-indirect
  (package
    (name "perl-indirect")
    (version "0.39")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/V/VP/VPIT/indirect-"
             version ".tar.gz"))
       (sha256
        (base32 "1r971mykvvsrzrp6a9ccl649ihr84h254jmlfpazv64f6i63qwvi"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/indirect")
    (synopsis "Lexically warn about using the indirect method call syntax")
    (description
     "Indirect warns about using the indirect method call syntax.")
    (license (package-license perl))))

(define-public perl-inline
  (package
   (name "perl-inline")
   (version "0.81")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/T/TI/TINITA/Inline-"
           version ".tar.gz"))
     (sha256
      (base32
       "1qxi0xvn8rqj4sca9gwb1xkm6bdz33x57li5kfls6mnavil3i5qz"))))
   (build-system perl-build-system)
   (native-inputs
    `(("perl-test-warn" ,perl-test-warn)))
   (home-page "https://metacpan.org/release/Inline")
   (synopsis "Write Perl subroutines in other programming languages")
   (description "The @code{Inline} module allows you to put source code
from other programming languages directly (inline) in a Perl script or
module.  The code is automatically compiled as needed, and then loaded
for immediate access from Perl.")
   (license (package-license perl))))

(define-public perl-inline-c
  (package
    (name "perl-inline-c")
    (version "0.78")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/T/TI/TINITA/Inline-C-"
             version ".tar.gz"))
       (sha256
        (base32
         "1izv7vswd17glffh8h83bi63gdk208mmhxi17l3qd8q1bkc08y4s"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-file-copy-recursive" ,perl-file-copy-recursive)
       ("perl-file-sharedir-install" ,perl-file-sharedir-install)
       ("perl-test-warn" ,perl-test-warn)
       ("perl-yaml-libyaml" ,perl-yaml-libyaml)))
    (propagated-inputs
     `(("perl-inline" ,perl-inline)
       ("perl-parse-recdescent" ,perl-parse-recdescent)
       ("perl-pegex" ,perl-pegex)))
    (home-page "https://metacpan.org/release/Inline-C")
    (synopsis "C Language Support for Inline")
    (description "The @code{Inline::C} module allows you to write Perl
subroutines in C.  Since version 0.30 the @code{Inline} module supports
multiple programming languages and each language has its own support module.
This document describes how to use Inline with the C programming language.
It also goes a bit into Perl C internals.")
    (license (package-license perl))))

(define-public perl-io-all
  (package
    (name "perl-io-all")
    (version "0.87")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/F/FR/FREW/IO-All-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0nsd9knlbd7if2v6zwj4q978axq0w5hk8ymp61z14a821hjivqjl"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-mimeinfo" ,perl-file-mimeinfo)
       ("perl-file-readbackwards" ,perl-file-readbackwards)))
    (home-page "https://metacpan.org/release/IO-All")
    (synopsis "@code{IO::All} to Larry Wall!")
    (description "@code{IO::All} combines all of the best Perl IO modules into
a single nifty object oriented interface to greatly simplify your everyday
Perl IO idioms.  It exports a single function called io, which returns a new
@code{IO::All} object.  And that object can do it all!")
    (license license:perl-license)))

(define-public perl-io-captureoutput
  (package
    (name "perl-io-captureoutput")
    (version "1.1105")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DA/DAGOLDEN/IO-CaptureOutput-"
             version
             ".tar.gz"))
       (sha256
        (base32 "11zlfbahac09q3jvwmpijmkwgihwxps85jwy2q7q0wqjragh16df"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/IO-CaptureOutput")
    (synopsis "Capture STDOUT and STDERR from Perl code, subprocesses or XS")
    (description "@code{IO::CaptureOutput} provides routines for capturing
@code{STDOUT} and @code{STDERR} from perl subroutines, forked system
calls (e.g. @code{system()}, @code{fork()}) and from XS or C modules.

This module is no longer recommended by its maintainer.  Users are advised to
try @code{Capture::Tiny} instead.")
    (license (package-license perl))))

(define-public perl-io-interactive
  (package
    (name "perl-io-interactive")
    (version "1.022")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/"
                           "IO-Interactive-" version ".tar.gz"))
       (sha256
        (base32 "1p7b3z877am99qn9b3n2whgcv77256sbg28divlpgs1sx653pm8f"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/IO-Interactive")
    (synopsis "Utilities for interactive I/O")
    (description "This module provides three utility subroutines that make it
easier to develop interactive applications: is_interactive(), interactive(),
and busy().")
    (license (package-license perl))))

(define-public perl-io-pager
  (package
    (name "perl-io-pager")
    (version "0.44")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/J/JP/JPIERCE/IO-Pager-"
             version
             ".tgz"))
       (sha256
        (base32 "0h52gplhc3rij18xc4ngpg5kqv6mylxfzig18xll1aqda8iwa8kl"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-less
           (lambda _
             (substitute* "lib/IO/Pager.pm"
               (("/usr/local/bin/less', '/usr/bin/less")
                (which "less")))
             #t)))))
    (propagated-inputs
     `(("perl-file-which" ,perl-file-which)))
    (inputs
     `(("less" ,less)))
    (home-page "https://metacpan.org/release/IO-Pager")
    (synopsis "Select a pager and pipe text to it")
    (description
     "@code{IO::Pager} can be used to locate an available pager and use it to
display output if a TTY is in use.")
    (license (package-license perl))))

(define-public perl-io-string
  (package
    (name "perl-io-string")
    (version "1.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GA/GAAS/"
                           "IO-String-" version ".tar.gz"))
       (sha256
        (base32
         "18755m410yl70s17rgq3m0hyxl8r5mr47vsq1rw7141d8kc4lgra"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/IO-String")
    (synopsis "Emulate file interface for in-core strings")
    (description "IO::String is an IO::File (and IO::Handle) compatible class
that reads or writes data from in-core strings.")
    (license (package-license perl))))

(define-public perl-io-stringy
  (package
    (name "perl-io-stringy")
    (version "2.111")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DS/DSKOLL/"
                           "IO-stringy-" version ".tar.gz"))
       (sha256
        (base32
         "178rpx0ym5l2m9mdmpnr92ziscvchm541w94fd7ygi6311kgsrwc"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/IO-stringy")
    (synopsis "IO:: interface for reading/writing an array of lines")
    (description "This toolkit primarily provides modules for performing both
traditional and object-oriented i/o) on things *other* than normal
filehandles; in particular, IO::Scalar, IO::ScalarArray, and IO::Lines.")
    (license (package-license perl))))

(define-public perl-io-tty
  (package
    (name "perl-io-tty")
    (version "1.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/T/TO/TODDR/IO-Tty-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1dcmxdhrapxvvzlfp6yzz7655f3c6x8jrw0md8ndp2qj27iy9wsi"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/IO-Tty")
    (synopsis "Perl interface to pseudo ttys")
    (description
     "This package provides the @code{IO::Pty} and @code{IO::Tty} Perl
interfaces to pseudo ttys.")
    (license (package-license perl))))

(define-public perl-ipc-cmd
  (package
    (name "perl-ipc-cmd")
    (version "1.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BI/BINGOS/IPC-Cmd-"
                           version ".tar.gz"))
       (sha256
        (base32 "0qvh0qpvc22r4kysfy8srxnhni677lvc8hr18kjrdkmb58jjj8ah"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/IPC-Cmd")
    (synopsis "Run interactive command-line programs")
    (description "@code{IPC::Cmd} allows for the searching and execution of
any binary on your system.  It adheres to verbosity settings and is able to
run interactively.  It also has an option to capture output/error buffers.")
    (license (package-license perl))))

(define-public perl-ipc-run
  (package
    (name "perl-ipc-run")
    (version "20180523.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TODDR/"
                           "IPC-Run-" version ".tar.gz"))
       (sha256
        (base32 "0bvckcs1629ifqfb68xkapd4a74fd5qbg6z9qs8i6rx4z3nxfl1q"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-io-tty" ,perl-io-tty)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before
                   'check 'disable-w32-test
                   (lambda _
                     ;; This test fails, and we're not really interested in
                     ;; it, so disable it.
                     (delete-file "t/win32_compile.t")
                     #t)))))
    (home-page "https://metacpan.org/release/IPC-Run")
    (synopsis "Run system() and background procs w/ piping, redirs, ptys")
    (description "IPC::Run allows you run and interact with child processes
using files, pipes, and pseudo-ttys.  Both system()-style and scripted usages
are supported and may be mixed.  Likewise, functional and OO API styles are
both supported and may be mixed.")
    (license (package-license perl))))

(define-public perl-ipc-run3
  (package
    (name "perl-ipc-run3")
    (version "0.048")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                                  "IPC-Run3-" version ".tar.gz"))
              (sha256
               (base32
                "0r9m8q78bg7yycpixd7738jm40yz71p2q7inm766kzsw3g6c709x"))))
    (build-system perl-build-system)
    (synopsis "Run a subprocess with input/output redirection")
    (description
     "The IPC::Run3 module allows you to run a subprocess and redirect stdin,
stdout, and/or stderr to files and perl data structures.  It aims to satisfy
99% of the need for using system, qx, and open3 with a simple, extremely
Perlish API and none of the bloat and rarely used features of IPC::Run.")
    (home-page "https://metacpan.org/release/IPC-Run3")
    ;; "You may use this module under the terms of the BSD, Artistic, or GPL
    ;; licenses, any version."
    (license (list license:bsd-3 license:gpl3+))))

(define-public perl-ipc-sharelite
  (package
    (name "perl-ipc-sharelite")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AN/ANDYA/"
                           "IPC-ShareLite-" version ".tar.gz"))
       (sha256
        (base32
         "1gz7dbwxrzbzdsjv11kb49jlf9q6lci2va6is0hnavd93nwhdm0l"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/IPC-ShareLite")
    (synopsis "Lightweight interface to shared memory")
    (description "IPC::ShareLite provides a simple interface to shared memory,
allowing data to be efficiently communicated between processes.")
    (license (package-license perl))))

(define-public perl-ipc-system-simple
  (package
    (name "perl-ipc-system-simple")
    (version "1.26")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/J/JK/JKEENAN/IPC-System-Simple-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1zb5ni8ikaq6s60amwdsq69nz8gxl484yiga6ax5nqp8v0hpy5sp"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/IPC-System-Simple")
    (synopsis "Run commands simply, with detailed diagnostics")
    (description "Calling Perl's in-built @code{system} function is easy,
determining if it was successful is hard.  Let's face it, @code{$?} isn't the
nicest variable in the world to play with, and even if you do check it,
producing a well-formatted error string takes a lot of work.

@code{IPC::System::Simple} takes the hard work out of calling external
commands.")
    (license (package-license perl))))

(define-public perl-json
  (package
    (name "perl-json")
    (version "4.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IS/ISHIGAKI/"
                           "JSON-" version ".tar.gz"))
       (sha256
        (base32
         "0z32x2lijij28c9fhmzgxc41i9nw24fyvd2a8ajs5zw9b9sqhjj4"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-json-xs" ,perl-json-xs))) ;recommended
    (home-page "https://metacpan.org/release/JSON")
    (synopsis "JSON encoder/decoder for Perl")
    (description "This module converts Perl data structures to JSON and vice
versa using either JSON::XS or JSON::PP.")
    (license (package-license perl))))

(define-public perl-json-any
  (package
    (name "perl-json-any")
    (version "1.39")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "JSON-Any-" version ".tar.gz"))
       (sha256
        (base32
         "1hspg6khjb38syn59cysnapc1q77qgavfym3fqr6l2kiydf7ajdf"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-test-warnings" ,perl-test-warnings)
       ("perl-test-without-module" ,perl-test-without-module)))
    (propagated-inputs
     `(("perl-namespace-clean" ,perl-namespace-clean)))
    (home-page "https://metacpan.org/release/JSON-Any")
    (synopsis "Wrapper for Perl JSON classes")
    (description
     "This module tries to provide a coherent API to bring together the
various JSON modules currently on CPAN.  This module will allow you to code to
any JSON API and have it work regardless of which JSON module is actually
installed.")
    (license (package-license perl))))

(define-public perl-json-maybexs
  (package
    (name "perl-json-maybexs")
    (version "1.004003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "JSON-MaybeXS-" version ".tar.gz"))
       (sha256
        (base32
         "1grg8saa318bs4x2wqnww7y0nra7azrzg35bk5pgvkwxzwbkpvjv"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-needs" ,perl-test-needs)))
    (inputs
     `(("perl-cpanel-json-xs" ,perl-cpanel-json-xs)))
    (home-page "https://metacpan.org/release/JSON-MaybeXS")
    (synopsis "Cpanel::JSON::XS with fallback")
    (description "This module first checks to see if either Cpanel::JSON::XS
or JSON::XS is already loaded, in which case it uses that module.  Otherwise
it tries to load Cpanel::JSON::XS, then JSON::XS, then JSON::PP in order, and
either uses the first module it finds or throws an error.")
    (license (package-license perl))))

(define-public perl-json-xs
  (package
    (name "perl-json-xs")
    (version "4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/"
                           "JSON-XS-" version ".tar.gz"))
       (sha256
        (base32
         "0118yrzagwlcfj5yldn3h23zzqs2rx282jlm068nf7fjlvy4m7s7"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-canary-stability" ,perl-canary-stability)))
    (propagated-inputs
     `(("perl-common-sense" ,perl-common-sense)
       ("perl-types-serialiser" ,perl-types-serialiser)))
    (home-page "https://metacpan.org/release/JSON-XS")
    (synopsis "JSON serialising/deserialising for Perl")
    (description "This module converts Perl data structures to JSON and vice
versa.")
    (license (package-license perl))))

(define-public perl-lexical-sealrequirehints
  (package
    (name "perl-lexical-sealrequirehints")
    (version "0.011")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Lexical-SealRequireHints-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0fh1arpr0hsj7skbn97yfvbk22pfcrpcvcfs15p5ss7g338qx4cy"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/Lexical-SealRequireHints")
    (synopsis "Prevent leakage of lexical hints")
    (description
     "Lexical::SealRequireHints prevents leakage of lexical hints")
    (license (package-license perl))))

(define-public perl-locale-maketext-lexicon
  (package
    (name "perl-locale-maketext-lexicon")
    (version "1.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DRTECH/"
                           "Locale-Maketext-Lexicon-" version ".tar.gz"))
       (sha256
        (base32 "0z6w3m6f3r29ljicdigsyvpa9w9j2m65l4gjxcw0wgwdll26ngxp"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-html-parser" ,perl-html-parser)
       ("perl-lingua-en-sentence" ,perl-lingua-en-sentence)
       ("perl-ppi" ,perl-ppi)
       ("perl-template-toolkit" ,perl-template-toolkit)
       ("perl-text-haml" ,perl-text-haml)
       ("perl-yaml" ,perl-yaml)))
    (home-page "https://metacpan.org/release/Locale-Maketext-Lexicon")
    (synopsis "Use other catalog formats in Maketext")
    (description
     "This module provides lexicon-handling modules to read from other
localization formats, such as Gettext, Msgcat, and so on.")
    (license license:x11)))

(define-public perl-log-any
  (package
    (name "perl-log-any")
    (version "1.707")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PR/PREACTION/Log-Any-"
                           version ".tar.gz"))
       (sha256
        (base32 "1wb55ib4gvk8h5pjb6hliqg7li1xjk420q3w5r33f9p1ps60ylbl"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Log-Any")
    (synopsis "Bringing loggers and listeners together")
    (description "@code{Log::Any} provides a standard log production API for
modules.  @code{Log::Any::Adapter} allows applications to choose the mechanism
for log consumption, whether screen, file or another logging mechanism like
@code{Log::Dispatch} or @code{Log::Log4perl}.

A CPAN module uses @code{Log::Any} to get a log producer object.  An
application, in turn, may choose one or more logging mechanisms via
@code{Log::Any::Adapter}, or none at all.

@code{Log::Any} has a very tiny footprint and no dependencies beyond Perl
itself, which makes it appropriate for even small CPAN modules to use.  It
defaults to @code{null} logging activity, so a module can safely log without
worrying about whether the application has chosen (or will ever choose) a
logging mechanism.")
    (license (package-license perl))))

(define-public perl-log-any-adapter-log4perl
  (package
    (name "perl-log-any-adapter-log4perl")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/P/PR/PREACTION/Log-Any-Adapter-Log4perl-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "19f1drqnzr6g4xwjm6jk4iaa3zmiax8bzxqch04f4jr12bjd75qi"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-log-any" ,perl-log-any)
       ("perl-log-log4perl" ,perl-log-log4perl)))
    (home-page
     "https://metacpan.org/release/Log-Any-Adapter-Log4perl")
    (synopsis "Log::Any adapter for Log::Log4perl")
    (description "@code{Log::Any::Adapter::Log4perl} provides a
@code{Log::Any} adapter using @code{Log::Log4perl} for logging.")
    (license (package-license perl))))

(define-public perl-log-log4perl
  (package
    (name "perl-log-log4perl")
    (version "1.49")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MS/MSCHILLI/Log-Log4perl-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "05ifhx1lmv91dbs9ck2zbjrkhh8z9g32gi6gxdmwnilia5zihfdp"))))
    (build-system perl-build-system)
    (home-page
     "https://metacpan.org/release/Log-Log4perl")
    (synopsis "Log4j implementation for Perl")
    (description "@code{Log::Log4perl} lets you remote-control and fine-tune
the logging behaviour of your system from the outside.  It implements the
widely popular (Java-based) Log4j logging package in pure Perl.")
    (license (package-license perl))))

(define-public perl-log-report-optional
  (package
    (name "perl-log-report-optional")
    (version "1.06")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "Log-Report-Optional-" version ".tar.gz"))
              (sha256
               (base32
                "11ciiaq8vy186m7mzj8pcncwi8p9qp13wblvk427g1pnqjzlda0g"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-string-print" ,perl-string-print)))
    (home-page "https://metacpan.org/release/Log-Report-Optional")
    (synopsis "Log::Report in the lightest form")
    (description
     "This module allows libraries to have a dependency to a small module
instead of the full Log-Report distribution.  The full power of
@code{Log::Report} is only released when the main program uses that module.
In that case, the module using the @code{Optional} will also use the full
@code{Log::Report}, otherwise the dressed-down @code{Log::Report::Minimal}
version.")
    (license (package-license perl))))

(define-public perl-log-report
  (package
    (name "perl-log-report")
    (version "1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "Log-Report-" version ".tar.gz"))
              (sha256
               (base32
                "1jjx1ari3a7ixsyan91b6n7lmjq6dy5223k3x2ah18qbxvw4caap"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-devel-globaldestruction" ,perl-devel-globaldestruction)
       ("perl-log-report-optional" ,perl-log-report-optional)
       ("perl-string-print" ,perl-string-print)))
    (home-page "https://metacpan.org/release/Log-Report")
    (synopsis "Get messages to users and logs")
    (description
     "@code{Log::Report} combines three tasks which are closely related in
one: logging, exceptions, and translations.")
    (license (package-license perl))))

(define-public perl-libintl-perl
  (package
    (name "perl-libintl-perl")
    (version "1.32")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GU/GUIDO/"
                           "libintl-perl-" version ".tar.gz"))
       (sha256
        (base32 "19gbbh9w3rl805mv6mg1q80fsrg610h098qhf7ycnkjnyac84440"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-perl-search-path
           (lambda _
             ;; Work around "dotless @INC" build failure.
             (setenv "PERL5LIB" (string-append (getcwd) ":"
                                               (getenv "PERL5LIB")))
             #t)))))
    (propagated-inputs
     `(("perl-file-sharedir" ,perl-file-sharedir)))
    (home-page "https://metacpan.org/release/libintl-perl")
    (synopsis "High-level interface to Uniforum message translation")
    (description "This package is an internationalization library for Perl
that aims to be compatible with the Uniforum message translations system as
implemented for example in GNU gettext.")
    (license license:gpl3+)))

(define-public perl-lingua-en-sentence
  (package
    (name "perl-lingua-en-sentence")
    (version "0.31")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KI/KIMRYAN/"
                           "Lingua-EN-Sentence-" version ".tar.gz"))
       (sha256
        (base32 "11hlg92khd2azbxndnffsj9lggbxb3lqfdbwc6asr1c9lxlqddms"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/Lingua-EN-Sentence")
    (synopsis "Split text into sentences")
    (description
     "The Lingua::EN::Sentence module contains the function get_sentences,
which splits text into its constituent sentences, based on a regular
expression and a list of abbreviations (built in and given).")
    (license license:perl-license)))

(define-public perl-lingua-translit
  (package
    (name "perl-lingua-translit")
    (version "0.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AL/ALINKE/"
                           "Lingua-Translit-" version ".tar.gz"))
       (sha256
        (base32
         "1qgap0j0ixmif309dvbqca7sy8xha9xgnj9s2lvh8qrczkc92gqi"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Lingua-Translit")
    (synopsis "Transliterate text between writing systems")
    (description "@code{Lingua::Translit} can be used to convert text from one
writing system to another, based on national or international transliteration
tables.  Where possible a reverse transliteration is supported.")
    (license (package-license perl))))

(define-public perl-list-allutils
  (package
    (name "perl-list-allutils")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "List-AllUtils-" version ".tar.gz"))
       (sha256
        (base32
         "1qmfpmly0pghc94k6ifnd1vwzlv8nks27qkqs6h4p7vcricn7zjc"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-warnings" ,perl-test-warnings)))
    (propagated-inputs
     `(("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-scalar-list-utils" ,perl-scalar-list-utils)))
    (home-page "https://metacpan.org/release/List-AllUtils")
    (synopsis "Combination of List::Util and List::MoreUtils")
    (description "This module exports all of the functions that either
List::Util or List::MoreUtils defines, with preference to List::Util.")
    (license (package-license perl))))

(define-public perl-list-compare
  (package
    (name "perl-list-compare")
    (version "0.53")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/J/JK/JKEENAN/List-Compare-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0l451yqhx1hlm7f2c3bjsl3n8w6l1jngrxzyfm2d8d9iggv4zgzx"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-io-captureoutput" ,perl-io-captureoutput)))
    (home-page "https://metacpan.org/release/List-Compare")
    (synopsis "Compare elements of two or more lists")
    (description "@code{List::Compare} provides a module to perform
comparative operations on two or more lists.  Provided operations include
intersections, unions, unique elements, complements and many more.")
    (license (package-license perl))))

(define-public perl-list-moreutils
  (package
    (name "perl-list-moreutils")
    (version "0.430")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "List-MoreUtils-" version ".tar.gz"))
       (sha256
        (base32 "09v5cipjf634a1176wy2wicibzz51lry0d0yim9rnbfl5j2ggcb3"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-perl-search-path
           (lambda _
             ;; Work around "dotless @INC" build failure.
             (setenv "PERL5LIB"
                     (string-append (getcwd) ":"
                                    (getenv "PERL5LIB")))
             #t)))))
    (native-inputs
     `(("perl-config-autoconf" ,perl-config-autoconf)
       ("perl-test-leaktrace" ,perl-test-leaktrace)))
    (propagated-inputs
     `(("perl-exporter-tiny" ,perl-exporter-tiny)
       ("perl-list-moreutils-xs" ,perl-list-moreutils-xs)))
    (home-page "https://metacpan.org/release/List-MoreUtils")
    (synopsis "Provide the stuff missing in List::Util")
    (description "List::MoreUtils provides some trivial but commonly needed
functionality on lists which is not going to go into List::Util.")
    (license (package-license perl))))

(define-public perl-list-moreutils-xs
  (package
    (name "perl-list-moreutils-xs")
    (version "0.430")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/List-MoreUtils-XS-"
                           version ".tar.gz"))
       (sha256
        (base32 "0hmjkhmk1qlzbg8skq7g1zral07k1x0fk4w2fpcfr7hpgkaldkp8"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-config-autoconf" ,perl-config-autoconf)
       ("perl-inc-latest" ,perl-inc-latest)
       ("perl-test-leaktrace" ,perl-test-leaktrace)))
    (home-page "https://metacpan.org/release/List-MoreUtils-XS")
    (synopsis "Provide the stuff missing in List::Util in XS")
    (description "@code{List::MoreUtils::XS} provides some trivial but
commonly needed functionality on lists which is not going to go into
@code{List::Util}.")
    (license license:asl2.0)))

(define-public perl-list-someutils
  (package
    (name "perl-list-someutils")
    (version "0.56")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DR/DROLSKY/List-SomeUtils-"
             version
             ".tar.gz"))
       (sha256
        (base32 "1xw9dzg949997b10y6zgzrmhmk2ap274qivnk0wc1033x2fdk9za"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-leaktrace" ,perl-test-leaktrace)))
    (inputs
     `(("perl-exporter-tiny" ,perl-exporter-tiny)
       ("perl-module-implementation"
        ,perl-module-implementation)))
    (home-page "https://metacpan.org/release/List-SomeUtils")
    (synopsis "Provide the stuff missing in List::Util")
    (description "@code{List::SomeUtils} provides some trivial but commonly
needed functionality on lists which is not going to go into @code{List::Util}.

All of the below functions are implementable in only a couple of lines of Perl
code.  Using the functions from this module however should give slightly
better performance as everything is implemented in C.  The pure-Perl
implementation of these functions only serves as a fallback in case the C
portions of this module couldn't be compiled on this machine.")
    (license (package-license perl))))

(define-public perl-mailtools
  (package
    (name "perl-mailtools")
    (version "2.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MA/MARKOV/MailTools-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1js43bp2dnd8n2rv8clsv749166jnyqnc91k4wkkmw5n4rlbvnaa"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-timedate" ,perl-timedate)))
    (home-page
     "https://metacpan.org/release/MailTools")
    (synopsis "Bundle of ancient email modules")
    (description "MailTools contains the following modules:
@table @asis
@item Mail::Address
Parse email address from a header line.
@item Mail::Cap
Interpret mailcap files: mappings of file-types to applications as used by
many command-line email programs.
@item Mail::Field
Simplifies access to (some) email header fields.  Used by Mail::Header.
@item Mail::Filter
Process Mail::Internet messages.
@item Mail::Header
Collection of Mail::Field objects, representing the header of a Mail::Internet
object.
@item Mail::Internet
Represents a single email message, with header and body.
@item Mail::Mailer
Send Mail::Internet emails via direct smtp or local MTA's.
@item Mail::Send
Build a Mail::Internet object, and then send it out using Mail::Mailer.
@item Mail::Util
\"Smart functions\" you should not depend on.
@end table")
    (license license:perl-license)))

(define-public perl-mail-sendmail
  (package
    (name "perl-mail-sendmail")
    (version "0.80")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/N/NE/NEILB/Mail-Sendmail-"
            version
            ".tar.gz"))
      (sha256
       (base32
        "1r38qbkj7jwj8cqy1rnqzkk81psxi08b1aiq392817f3bk5ri2jv"))))
    (build-system perl-build-system)
    (arguments `(#:tests? #f))      ;socket not available during build
    (home-page "https://metacpan.org/release/Mail-Sendmail")
    (synopsis "Simple platform independent mailer")
    (description "Mail::Sendmail is a pure perl module that provides a
simple means to send email from a perl script.  The module only
requires Perl5 and a network connection.")
    (license license:perl-license)))

(define-public perl-math-bezier
  (package
    (name "perl-math-bezier")
    (version "0.01")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/A/AB/ABW/Math-Bezier-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1f5qwrb7vvf8804myb2pcahyxffqm9zvfal2n6myzw7x8py1ba0i"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Math-Bezier")
    (synopsis "Solution of bezier curves")
    (description "This module implements the algorithm for the solution of Bezier
curves as presented by Robert D Miller in Graphics Gems V, \"Quick and Simple
Bezier Curve Drawing\".")
    (license license:perl-license)))

(define-public perl-math-round
  (package
    (name "perl-math-round")
    (version "0.07")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/G/GR/GROMMEL/Math-Round-"
                    version ".tar.gz"))
              (sha256
               (base32
                "09wkvqj4hfq9y0fimri967rmhnq90dc2wf20lhlmqjp5hsd359vk"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Math-Round")
    (synopsis "Perl extension for rounding numbers")
    (description "@code{Math::Round} provides functions to round numbers,
both positive and negative, in various ways.")
    (license license:perl-license)))

(define-public perl-math-vecstat
  (package
    (name "perl-math-vecstat")
    (version "0.08")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/A/AS/ASPINELLI/Math-VecStat-"
                    version ".tar.gz"))
              (sha256
               (base32
                "03bdcl9pn2bc9b50c50nhnr7m9wafylnb3v21zlch98h9c78x6j0"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Math-VecStat")
    (synopsis "Basic numeric stats on vectors")
    (description "This package provides some basic statistics on numerical
vectors.  All the subroutines can take a reference to the vector to be
operated on.")
    (license (package-license perl))))

(define-public perl-memoize
  (package
    (name "perl-memoize")
    (version "1.03")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/M/MJ/MJD/Memoize-"
                    version".tgz"))
              (sha256
               (base32
                "1wysq3wrmf1s7s3phimzn7n0dswik7x53apykzgb0l2acigwqfaj"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Memoize")
    (synopsis "Make functions faster by trading space for time")
    (description "This package transparently speeds up functions by caching
return values, trading space for time.")
    (license license:perl-license)))

(define-public perl-memoize-expirelru
  (package
    (name "perl-memoize-expirelru")
    (version "0.56")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Memoize-ExpireLRU-" version ".tar.gz"))
       (sha256
        (base32
         "1xnp3jqabl4il5kfadlqimbxhzsbm7gpwrgw0m5s5fdsrc0n70zf"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Memoize-ExpireLRU")
    (synopsis "Expiry plug-in for Memoize that adds LRU cache expiration")
    (description "This module implements an expiry policy for Memoize that
follows LRU semantics, that is, the last n results, where n is specified as
the argument to the CACHESIZE parameter, will be cached.")
    (license (package-license perl))))

(define-public perl-mime-charset
  (package
    (name "perl-mime-charset")
    (version "1.012.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/N/NE/NEZUMI/"
                                  "MIME-Charset-" version ".tar.gz"))
              (sha256
               (base32
                "04qxgcg9mvia121i3zcqxgp20y0d9kg0qv6hddk93ian0af7g347"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/MIME-Charset")
    (synopsis "Charset information for MIME messages")
    (description
     "@code{MIME::Charset} provides information about character sets used for
MIME messages on Internet.")
    (license (package-license perl))))

(define-public perl-mime-tools
  (package
    (name "perl-mime-tools")
    (version "5.509")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DS/DSKOLL/MIME-tools-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0wv9rzx5j1wjm01c3dg48qk9wlbm6iyf91j536idk09xj869ymv4"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-deep" ,perl-test-deep)))
    (inputs
     `(("perl-convert-binhex" ,perl-convert-binhex)))
    (propagated-inputs
     `(("perl-mailtools" ,perl-mailtools)))
    (home-page
     "https://metacpan.org/release/MIME-tools")
    (synopsis "Tools to manipulate MIME messages")
    (description
     "MIME-tools is a collection of Perl5 MIME:: modules for parsing,
decoding, and generating single- or multipart (even nested multipart) MIME
messages.")
    (license license:perl-license)))

(define-public perl-mime-types
  (package
    (name "perl-mime-types")
    (version "2.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                           "MIME-Types-" version ".tar.gz"))
       (sha256
        (base32
         "1xlg7q6h8zyb8534sy0iqn90py18kilg419q6051bwqz5zadfkp0"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/MIME-Types")
    (synopsis "Definition of MIME types")
    (description "This module provides a list of known mime-types, combined
from various sources.  For instance, it contains all IANA types and the
knowledge of Apache.")
    (license (package-license perl))))

(define-public perl-mixin-linewise
  (package
    (name "perl-mixin-linewise")
    (version "0.108")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/R/RJ/RJBS/Mixin-Linewise-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1wmfr19w9y8qys7b32mnj1vmps7qwdahqas71a9p62ac8xw0dwkx"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-perlio-utf8_strict" ,perl-perlio-utf8_strict)
       ("perl-sub-exporter" ,perl-sub-exporter)))
    (home-page "https://metacpan.org/release/Mixin-Linewise")
    (synopsis "Write your linewise code for handles; this does the rest")
    (description "It's boring to deal with opening files for IO, converting
strings to handle-like objects, and all that.  With
@code{Mixin::Linewise::Readers} and @code{Mixin::Linewise::Writers}, you can
just write a method to handle handles, and methods for handling strings and
file names are added for you.")
    (license (package-license perl))))

(define-public perl-modern-perl
  (package
    (name "perl-modern-perl")
    (version "1.20181021")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/C/CH/CHROMATIC/Modern-Perl-"
             version ".tar.gz"))
       (sha256
        (base32 "1if9jbh66z2vm4wwnky41ljnhdlwrh7vzl6pd3w60v3wix92nj0x"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page
     "https://metacpan.org/release/Modern-Perl")
    (synopsis
     "Enable all of the features of Modern Perl with one import")
    (description "@code{Modern::Perl} provides a simple way to enable
multiple, by now, standard libraries in a Perl program.")
    (license (package-license perl))))

(define-public perl-module-build-tiny
  (package
    (name "perl-module-build-tiny")
    (version "0.039")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "Module-Build-Tiny-" version ".tar.gz"))
       (sha256
        (base32
         "077ijxbvamybph4ymamy1i9q2993xb46vf1npxaybjz0mkv0yn3x"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-extutils-installpaths" ,perl-extutils-installpaths)
       ("perl-extutils-config" ,perl-extutils-config)
       ("perl-extutils-helpers" ,perl-extutils-helpers)
       ("perl-test-harness" ,perl-test-harness)))
    (propagated-inputs
     `(("perl-extutils-installpaths" ,perl-extutils-installpaths)
       ("perl-extutils-config" ,perl-extutils-config)
       ("perl-extutils-helpers" ,perl-extutils-helpers)
       ("perl-test-harness" ,perl-test-harness)))
    (home-page "https://metacpan.org/release/Module-Build-Tiny")
    (synopsis "Tiny replacement for Module::Build")
    (description "Many Perl distributions use a Build.PL file instead of a
Makefile.PL file to drive distribution configuration, build, test and
installation.  Traditionally, Build.PL uses Module::Build as the underlying
build system.  This module provides a simple, lightweight, drop-in
replacement.  Whereas Module::Build has over 6,700 lines of code; this module
has less than 120, yet supports the features needed by most distributions.")
    (license (package-license perl))))

(define-public perl-module-build-withxspp
  (package
    (name "perl-module-build-withxspp")
    (version "0.14")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/S/SM/SMUELLER/Module-Build-WithXSpp-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0d39fjg9c0n820bk3fb50vvlwhdny4hdl69xmlyzql5xzp4cicsk"))))
    (build-system perl-build-system)
    (native-inputs
      `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
      `(("perl-extutils-cppguess" ,perl-extutils-cppguess)
        ("perl-extutils-xspp" ,perl-extutils-xspp)
        ("perl-module-build" ,perl-module-build)))
    (home-page
      "https://metacpan.org/release/Module-Build-WithXSpp")
    (synopsis
      "The module provides an XS++ enhanced flavour of Module::Build")
    (description "This subclass of Module::Build adds some tools and
processes to make it easier to use for wrapping C++ using XS++
(ExtUtils::XSpp).")
    (license (package-license perl))))

(define-public perl-module-build-xsutil
  (package
    (name "perl-module-build-xsutil")
    (version "0.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/H/HI/HIDEAKIO/"
                                  "Module-Build-XSUtil-" version ".tar.gz"))
              (sha256
               (base32
                "1nrs0b6hmwl3sw3g50b9857qgp5cbbbpl716zwn30h9vwjj2yxhm"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-cwd-guard" ,perl-cwd-guard)
       ("perl-file-copy-recursive" ,perl-file-copy-recursive)
       ("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-devel-checkcompiler" ,perl-devel-checkcompiler)))
    (home-page "https://metacpan.org/release/Module-Build-XSUtil")
    (synopsis "Module::Build class for building XS modules")
    (description
     "@code{Module::Build::XSUtil} is subclass of @code{Module::Build}
for support building XS modules.

This is a list of a new parameters in the @code{Module::Build::new} method:

@enumerate
@item @code{needs_compiler_c99}: This option checks C99 compiler availability.
@item @code{needs_compiler_cpp}: This option checks C++ compiler availability.
Can also pass @code{extra_compiler_flags} and @code{extra_linker_flags} for C++.
@item @code{generate_ppport_h}: Generate @file{ppport.h} by @code{Devel::PPPort}.
@item @code{generate_xshelper_h}: Generate @file{xshelper.h} which is a helper
header file to include @file{EXTERN.h}, @file{perl.h}, @file{XSUB.h} and
@file{ppport.h}, and defines some portability stuff which are not supported by
@file{ppport.h}.

It is ported from @code{Module::Install::XSUtil}.
@item @code{cc_warnings}: Toggle compiler warnings.  Enabled by default.
@item @code{-g options}: Invoke @file{Build.PL} with @code{-g} to enable
debug options.
@end enumerate")
    (license (package-license perl))))

(define-public perl-module-find
  (package
    (name "perl-module-find")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CR/CRENZ/"
                           "Module-Find-" version ".tar.gz"))
       (sha256
        (base32
         "0s45y5lvd9k89g7lds83c0bn1p29c13hfsbrd7x64jfaf8h8cisa"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Module-Find")
    (synopsis "Find and use installed modules in a (sub)category")
    (description "Module::Find lets you find and use modules in categories.
This can be useful for auto-detecting driver or plugin modules.  You can
differentiate between looking in the category itself or in all
subcategories.")
    (license (package-license perl))))

(define-public perl-module-implementation
  (package
    (name "perl-module-implementation")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Module-Implementation-" version ".tar.gz"))
       (sha256
        (base32
         "0vfngw4dbryihqhi7g9ks360hyw8wnpy3hpkzyg0q4y2y091lpy1"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-module-runtime" ,perl-module-runtime)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "https://metacpan.org/release/Module-Implementation")
    (synopsis "Loads alternate underlying implementations for a module")
    (description "This module abstracts out the process of choosing one of
several underlying implementations for a module.  This can be used to provide
XS and pure Perl implementations of a module, or it could be used to load an
implementation for a given OS or any other case of needing to provide multiple
implementations.")
    (license license:artistic2.0)))

(define-public perl-module-install
  (package
    (name "perl-module-install")
    (version "1.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Module-Install-" version ".tar.gz"))
       (sha256
        (base32
         "06q12cm97yh4p7qbm0a2p96996ii6ss59qy57z0f7f9svy6sflqs"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-yaml-tiny" ,perl-yaml-tiny)))
    (propagated-inputs
     `(("perl-archive-zip" ,perl-archive-zip)
       ("perl-file-homedir" ,perl-file-homedir)
       ("perl-file-remove" ,perl-file-remove)
       ("perl-json" ,perl-json)
       ;; The LWP::Simple and LWP::UserAgent modules are recommended, but
       ;; would cause a circular dependency with (gnu packages web), so we
       ;; leave it out.  It may be resolved at runtime, however.
       ;("perl-libwww-perl" ,perl-libwww-perl)
       ("perl-module-scandeps" ,perl-module-scandeps)
       ("perl-par-dist" ,perl-par-dist)
       ("perl-yaml-tiny" ,perl-yaml-tiny)))
    ;; TODO: One test requires Test::More >= 0.99, another fails with unicode
    ;; character handling.
    (arguments `(#:tests? #f))
    (home-page "https://metacpan.org/release/Module-Install")
    (synopsis "Standalone, extensible Perl module installer")
    (description "Module::Install is a package for writing installers for
CPAN (or CPAN-like) distributions that are clean, simple, minimalist, act in a
strictly correct manner with ExtUtils::MakeMaker, and will run on any Perl
installation version 5.005 or newer.")
    (license (package-license perl))))

(define-public perl-module-manifest
  (package
    (name "perl-module-manifest")
    (version "1.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/E/ET/ETHER/Module-Manifest-"
             version ".tar.gz"))
       (sha256
        (base32
         "16skpm804a19gsgxzn1wba3lmvc7cx5q8ly4srpyd82yy47zi5d3"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)
       ("perl-test-warn" ,perl-test-warn)))
    (propagated-inputs
     `(("perl-params-util" ,perl-params-util)))
    (home-page "https://metacpan.org/release/Module-Manifest")
    (synopsis "Parse and examine a Perl distribution @file{MANIFEST} file")
    (description
     "@code{Module::Manifest} is a simple utility module created originally for
use in @code{Module::Inspector}.

It can load a @file{MANIFEST} file that comes in a Perl distribution tarball,
examine the contents, and perform some simple tasks.  It can also load the
@file{MANIFEST.SKIP} file and check that.")
    (license license:perl-license)))

(define-public perl-module-pluggable
  (package
    (name "perl-module-pluggable")
    (version "5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SI/SIMONW/"
                           "Module-Pluggable-" version ".tar.gz"))
       (sha256
        (base32
         "1px6qmszmfc69v36vd8d92av4nkrif6xf4nrj3xv647xwi2svwmk"))
       (patches (search-patches "perl-module-pluggable-search.patch"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Module-Pluggable")
    (synopsis "Give your Perl module the ability to have plugins")
    (description "This module provides a simple but extensible way of having
@code{plugins} for your Perl module.")
    (license (package-license perl))))

(define-public perl-module-runtime
  (package
    (name "perl-module-runtime")
    (version "0.016")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/Z/ZE/ZEFRAM/"
                           "Module-Runtime-" version ".tar.gz"))
       (sha256
        (base32
         "097hy2czwkxlppri32m599ph0xfvfsbf0a5y23a4fdc38v32wc38"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/Module-Runtime")
    (synopsis "Perl runtime module handling")
    (description "The functions exported by this module deal with runtime
handling of Perl modules, which are normally handled at compile time.")
    (license (package-license perl))))

(define-public perl-module-runtime-conflicts
  (package
    (name "perl-module-runtime-conflicts")
    (version "0.003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Module-Runtime-Conflicts-" version ".tar.gz"))
       (sha256
        (base32
         "0x9qfg4pq70v1rl9dfk775fmca7ia308m24vfy8zww4c0dsxqz3h"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-module-runtime" ,perl-module-runtime)
       ("perl-dist-checkconflicts" ,perl-dist-checkconflicts)))
    (home-page "https://metacpan.org/release/Module-Runtime-Conflicts")
    (synopsis "Provide information on conflicts for Module::Runtime")
    (description "This module provides conflicts checking for Module::Runtime,
which had a recent release that broke some versions of Moose.  It is called
from Moose::Conflicts and moose-outdated.")
    (license (package-license perl))))

(define-public perl-module-scandeps
  (package
    (name "perl-module-scandeps")
    (version "1.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RS/RSCHUPP/"
                           "Module-ScanDeps-" version ".tar.gz"))
       (sha256
        (base32
         "0j6r9r99x5p0i6fv06i44wpsvjxj32amjkiqf6pmqpj80jff2k7f"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-requires" ,perl-test-requires)))
    (home-page "https://metacpan.org/release/Module-ScanDeps")
    (synopsis "Recursively scan Perl code for dependencies")
    (description "Module::ScanDeps is a module to recursively scan Perl
programs for dependencies.")
    (license (package-license perl))))

(define-public perl-module-util
  (package
    (name "perl-module-util")
    (version "1.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MA/MATTLAW/"
                           "Module-Util-" version ".tar.gz"))
       (sha256
        (base32
         "1ip2yg3x517gg8c48crhd52ba864vmyimvm0ibn4ci068mmcpyvc"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build))) ; >= 0.40
    (home-page "https://metacpan.org/release/Module-Util")
    (synopsis "Module name tools and transformations")
    (description "This module provides a few useful functions for manipulating
module names.  Its main aim is to centralise some of the functions commonly
used by modules that manipulate other modules in some way, like converting
module names to relative paths.")
    (license (package-license perl))))

(define-public perl-moo
  (package
    (name "perl-moo")
    (version "1.007000")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Moo-" version ".tar.gz"))
       (sha256
        (base32
         "0y9s6s9jjd519wgal6lwc9id4sadrvfn8gjb51dl602d0kk0l7n5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-class-method-modifiers" ,perl-class-method-modifiers)
       ("perl-class-xsaccessor" ,perl-class-xsaccessor)
       ("perl-devel-globaldestruction" ,perl-devel-globaldestruction)
       ("perl-import-into" ,perl-import-into)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-role-tiny" ,perl-role-tiny)
       ("perl-strictures" ,perl-strictures)))
    (home-page "https://metacpan.org/release/Moo")
    (synopsis "Minimalist Object Orientation (with Moose compatibility)")
    (description "Moo is an extremely light-weight Object Orientation system.
It allows one to concisely define objects and roles with a convenient syntax
that avoids the details of Perl's object system.  Moo contains a subset of
Moose and is optimised for rapid startup.")
    (license (package-license perl))))

;; Some packages don't yet work with this newer version of ‘Moo’.
(define-public perl-moo-2
  (package
    (inherit perl-moo)
    (name "perl-moo-2")
    (version "2.003006")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Moo-" version ".tar.gz"))
       (sha256
        (base32 "0wi4gyp5kn4lbags0hrax3c9jj9spxg4d11fbrdh0ican4m0kcmw"))))
    (propagated-inputs
     `(("perl-role-tiny" ,perl-role-tiny-2)
       ("perl-sub-name" ,perl-sub-name)
       ("perl-sub-quote" ,perl-sub-quote)
       ("perl-strictures" ,perl-strictures-2)
       ,@(alist-delete "perl-strictures"
                       (alist-delete "perl-role-tiny"
                                     (package-propagated-inputs perl-moo)))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-perl-search-path
           (lambda _
             ;; Use perl-strictures for testing.
             (setenv "MOO_FATAL_WARNINGS" "=1")
             #t)))))))

(define-public perl-moose
  (package
    (name "perl-moose")
    (version "2.2015")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                                  "Moose-" version ".tar.gz"))
              (sha256
               (base32
                "05gma3q3l15igqrqi8ax8v5cmmvy7s939q3xzs45l1rc7sfx6yd6"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-cpan-meta-check" ,perl-cpan-meta-check)
       ("perl-dist-checkconflicts" ,perl-dist-checkconflicts)
       ("perl-test-cleannamespaces" ,perl-test-cleannamespaces)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-test-warnings" ,perl-test-warnings)))
    ;; XXX::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ;; # === Other Modules ===
    ;; #
    ;; #     Module                       Want    Have
    ;; #     ---------------------------- ---- -------
    ;; #     Algorithm::C3                 any missing
    ;; #     DBM::Deep                     any missing
    ;; #     DateTime                      any missing
    ;; #     DateTime::Calendar::Mayan     any missing
    ;; #     DateTime::Format::MySQL       any missing
    ;; #     Declare::Constraints::Simple  any missing
    ;; #     Dist::CheckConflicts          any    0.11
    ;; #     HTTP::Headers                 any missing
    ;; #     IO::File                      any    1.16
    ;; #     IO::String                    any missing
    ;; #     Locale::US                    any missing
    ;; #     Module::Refresh               any missing
    ;; #     MooseX::NonMoose              any missing
    ;; #     Params::Coerce                any missing
    ;; #     Regexp::Common                any missing
    ;; #     SUPER                         any missing
    ;; #     Test::Deep                    any missing
    ;; #     Test::DependentModules        any missing
    ;; #     Test::LeakTrace               any missing
    ;; #     Test::Output                  any missing
    ;; #     URI                           any missing
    (propagated-inputs
     `(("perl-class-load" ,perl-class-load)
       ("perl-class-load-xs" ,perl-class-load-xs)
       ("perl-data-optlist" ,perl-data-optlist)
       ("perl-devel-globaldestruction" ,perl-devel-globaldestruction)
       ("perl-devel-overloadinfo" ,perl-devel-overloadinfo)
       ("perl-devel-partialdump" ,perl-devel-partialdump)
       ("perl-devel-stacktrace" ,perl-devel-stacktrace)
       ("perl-dist-checkconflicts" ,perl-dist-checkconflicts)
       ("perl-eval-closure" ,perl-eval-closure)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-module-runtime-conflicts" ,perl-module-runtime-conflicts)
       ("perl-mro-compat" ,perl-mro-compat)
       ("perl-package-deprecationmanager" ,perl-package-deprecationmanager)
       ("perl-package-stash" ,perl-package-stash)
       ("perl-package-stash-xs" ,perl-package-stash-xs)
       ("perl-params-util" ,perl-params-util)
       ("perl-scalar-list-utils" ,perl-scalar-list-utils)
       ("perl-sub-exporter" ,perl-sub-exporter)
       ("perl-sub-name" ,perl-sub-name)
       ("perl-task-weaken" ,perl-task-weaken)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "https://metacpan.org/release/Moose")
    (synopsis "Postmodern object system for Perl 5")
    (description
     "Moose is a complete object system for Perl 5.  It provides keywords for
attribute declaration, object construction, inheritance, and maybe more.  With
Moose, you define your class declaratively, without needing to know about
blessed hashrefs, accessor methods, and so on.  You can concentrate on the
logical structure of your classes, focusing on \"what\" rather than \"how\".
A class definition with Moose reads like a list of very concise English
sentences.")
    (license (package-license perl))))

(define-public perl-moosex-emulate-class-accessor-fast
  (package
    (name "perl-moosex-emulate-class-accessor-fast")
    (version "0.009032")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "MooseX-Emulate-Class-Accessor-Fast-"
                           version ".tar.gz"))
       (sha256
        (base32 "153r30nggcyyx7ai15dbnba2h5145f8jdsh6wj54298d3zpvgvl2"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-moose" ,perl-moose)))
    (home-page "https://metacpan.org/release/MooseX-Emulate-Class-Accessor-Fast")
    (synopsis "Emulate Class::Accessor::Fast behavior using Moose attributes")
    (description "This module attempts to emulate the behavior of
Class::Accessor::Fast as accurately as possible using the Moose attribute
system.  The public API of Class::Accessor::Fast is wholly supported, but the
private methods are not.")
    (license (package-license perl))))

(define-public perl-moosex-getopt
  (package
    (name "perl-moosex-getopt")
    (version "0.75")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "MooseX-Getopt-" version ".tar.gz"))
       (sha256
        (base32 "1j7b2jnf0blxr4czp3vfcnv1h5zj601mrfdm92g1wf5wn9dvxwv3"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-path-tiny" ,perl-path-tiny)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-needs" ,perl-test-needs)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-test-trap" ,perl-test-trap)
       ("perl-test-warnings" ,perl-test-warnings)))
    (propagated-inputs
     `(("perl-getopt-long-descriptive" ,perl-getopt-long-descriptive)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-role-parameterized" ,perl-moosex-role-parameterized)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page "https://metacpan.org/release/MooseX-Getopt")
    (synopsis "Moose role for processing command line options")
    (description "This is a Moose role which provides an alternate constructor
for creating objects using parameters passed in from the command line.")
    (license (package-license perl))))

(define-public perl-moosex-markasmethods
  (package
    (name "perl-moosex-markasmethods")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RS/RSRCHBOY/"
                           "MooseX-MarkAsMethods-" version ".tar.gz"))
       (sha256
        (base32
         "1y3yxwcjjajm66pvca54cv9fax7a6dy36xqr92x7vzyhfqrw3v69"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-moose" ,perl-moose)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page "https://metacpan.org/release/MooseX-MarkAsMethods")
    (synopsis "Mark overload code symbols as methods")
    (description "MooseX::MarkAsMethods allows one to easily mark certain
functions as Moose methods.  This will allow other packages such as
namespace::autoclean to operate without blowing away your overloads.  After
using MooseX::MarkAsMethods your overloads will be recognized by Class::MOP as
being methods, and class extension as well as composition from roles with
overloads will \"just work\".")
    (license license:lgpl2.1)))

(define-public perl-moosex-methodattributes
  (package
    (name "perl-moosex-methodattributes")
    (version "0.31")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "MooseX-MethodAttributes-" version ".tar.gz"))
       (sha256
        (base32
         "1whd10w7bm3dwaj7gpgw40bci9vvb2zmxs4349ifji91hvinwqck"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-moose" ,perl-moose)
       ("perl-moosex-types" ,perl-moosex-types)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page "https://metacpan.org/release/MooseX-MethodAttributes")
    (synopsis "Code attribute introspection")
    (description "This module allows code attributes of methods to be
introspected using Moose meta method objects.")
    (license (package-license perl))))

(define-public perl-moosex-nonmoose
(package
  (name "perl-moosex-nonmoose")
  (version "0.26")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                          "MooseX-NonMoose-" version ".tar.gz"))
      (sha256
        (base32
          "0zdaiphc45s5xj0ax5mkijf5d8v6w6yccb3zplgj6f30y7n55gnb"))))
  (build-system perl-build-system)
  (native-inputs
    `(("perl-moose" ,perl-moose)
      ("perl-test-fatal" ,perl-test-fatal)))
  (propagated-inputs
    `(("perl-list-moreutils" ,perl-list-moreutils)
      ("perl-module-runtime" ,perl-module-runtime)
      ("perl-moose" ,perl-moose)
      ("perl-try-tiny" ,perl-try-tiny)))
  (home-page "https://metacpan.org/release/MooseX-NonMoose")
  (synopsis "Subclassing of non-Moose classes")
  (description "MooseX::NonMoose allows for easily subclassing non-Moose
classes with Moose, taking care of the details connected with doing this, such
as setting up proper inheritance from Moose::Object and installing (and
inlining, at make_immutable time) a constructor that makes sure things like
BUILD methods are called.  It tries to be as non-intrusive as possible.")
  (license (package-license perl))))

(define-public perl-moosex-params-validate
  (package
    (name "perl-moosex-params-validate")
    (version "0.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "MooseX-Params-Validate-" version ".tar.gz"))
       (sha256
        (base32 "1n9ry6gnskkp9ir6s7d5jirn3mh14ydgpmwqz6wcp6d9md358ac8"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-moose" ,perl-moose)
       ("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-devel-caller" ,perl-devel-caller)
       ("perl-moose" ,perl-moose)
       ("perl-params-validate" ,perl-params-validate)
       ("perl-sub-exporter" ,perl-sub-exporter)))
    (home-page "https://metacpan.org/release/MooseX-Params-Validate")
    (synopsis "Extension of Params::Validate using Moose's types")
    (description "This module fills a gap in Moose by adding method parameter
validation to Moose.")
    (license (package-license perl))))

(define-public perl-moosex-relatedclassroles
  (package
    (name "perl-moosex-relatedclassroles")
    (version "0.004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HD/HDP/"
                           "MooseX-RelatedClassRoles-" version ".tar.gz"))
       (sha256
        (base32
         "17vynkf6m5d039qkr4in1c9lflr8hnwp1fgzdwhj4q6jglipmnrh"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-moose" ,perl-moose)
       ("perl-moosex-role-parameterized" ,perl-moosex-role-parameterized)))
    (home-page "https://metacpan.org/release/MooseX-RelatedClassRoles")
    (synopsis "Apply roles to a related Perl class")
    (description "This module applies roles to make a subclass instead of
manually setting up a subclass.")
    (license (package-license perl))))

(define-public perl-moosex-role-parameterized
  (package
    (name "perl-moosex-role-parameterized")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "MooseX-Role-Parameterized-" version ".tar.gz"))
       (sha256
        (base32 "0plx25n80mv9qwhix52z79md0qil616nbcryk2f4216kghpw2ij8"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-cpan-meta-check" ,perl-cpan-meta-check)
       ("perl-module-build" ,perl-module-build)
       ("perl-moosex-role-withoverloading" ,perl-moosex-role-withoverloading)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-moose" ,perl-moose)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page "https://metacpan.org/release/MooseX-Role-Parameterized")
    (synopsis "Moose roles with composition parameters")
    (description "Because Moose roles serve many different masters, they
usually provide only the least common denominator of functionality.  To
empower roles further, more configurability than -alias and -excludes is
required.  Perhaps your role needs to know which method to call when it is
done processing, or what default value to use for its url attribute.
Parameterized roles offer a solution to these (and other) kinds of problems.")
    (license (package-license perl))))

(define-public perl-moosex-role-withoverloading
  (package
    (name "perl-moosex-role-withoverloading")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "MooseX-Role-WithOverloading-" version ".tar.gz"))
       (sha256
        (base32
         "0rb8k0dp1a55bm2pr6r0vsi5msvjl1dslfidxp1gj80j7zbrbc4j"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-aliased" ,perl-aliased)
       ("perl-moose" ,perl-moose)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page "https://metacpan.org/release/MooseX-Role-WithOverloading")
    (synopsis "Roles which support overloading")
    (description "MooseX::Role::WithOverloading allows you to write a
Moose::Role which defines overloaded operators and allows those overload
methods to be composed into the classes/roles/instances it's compiled to,
where plain Moose::Roles would lose the overloading.")
    (license (package-license perl))))

(define-public perl-moosex-semiaffordanceaccessor
  (package
    (name "perl-moosex-semiaffordanceaccessor")
    (version "0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "MooseX-SemiAffordanceAccessor-" version ".tar.gz"))
       (sha256
        (base32
         "1mdil9ckgmgr78z59p8wfa35ixn5855ndzx14y01dvfxpiv5gf55"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-moose" ,perl-moose)))
    (home-page "https://metacpan.org/release/MooseX-SemiAffordanceAccessor")
    (synopsis "Name your accessors foo() and set_foo()")
    (description "This module does not provide any methods.  Simply loading it
changes the default naming policy for the loading class so that accessors are
separated into get and set methods.  The get methods have the same name as the
accessor, while set methods are prefixed with \"_set_\".")
    (license license:artistic2.0)))

(define-public perl-moosex-strictconstructor
  (package
    (name "perl-moosex-strictconstructor")
    (version "0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "MooseX-StrictConstructor-" version ".tar.gz"))
       (sha256
        (base32
         "0ccawja1kabgglrkdw5v82m1pbw189a0mnd33l43rs01d70p6ra8"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-moose" ,perl-moose)
       ("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-moose" ,perl-moose)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page "https://metacpan.org/release/MooseX-StrictConstructor")
    (synopsis "Strict object constructors for Moose")
    (description "Simply loading this module makes your constructors
\"strict\".  If your constructor is called with an attribute init argument
that your class does not declare, then it calls Moose->throw_error().")
    (license license:artistic2.0)))

(define-public perl-moosex-traits-pluggable
  (package
    (name "perl-moosex-traits-pluggable")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RK/RKITOVER/"
                           "MooseX-Traits-Pluggable-" version ".tar.gz"))
       (sha256
        (base32
         "1jjqmcidy4kdgp5yffqqwxrsab62mbhbpvnzdy1rpwnb1savg5mb"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-moose" ,perl-moose)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-class-load" ,perl-class-load)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-moose" ,perl-moose)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page
     "https://metacpan.org/release/MooseX-Traits-Pluggable")
    (synopsis "Trait loading and resolution for Moose")
    (description "Adds support on top of MooseX::Traits for class precedence
search for traits and some extra attributes.")
    (license (package-license perl))))

(define-public perl-moosex-types
  (package
    (name "perl-moosex-types")
    (version "0.45")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "MooseX-Types-" version ".tar.gz"))
       (sha256
        (base32
         "1iq90s1f0xbmr194q0mhnp9wxqxwwilkbdml040ibqbqvfiz87yh"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-carp-clan" ,perl-carp-clan)
       ("perl-moose" ,perl-moose)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page "https://metacpan.org/release/MooseX-Types")
    (synopsis "Organise your Moose types in libraries")
    (description "This package lets you declare types using short names, but
behind the scenes it namespaces all your type declarations, effectively
prevent name clashes between packages.")
    (license (package-license perl))))

(define-public perl-moosex-types-datetime
  (package
    (name "perl-moosex-types-datetime")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "MooseX-Types-DateTime-" version ".tar.gz"))
       (sha256
        (base32
         "1iir3mdvz892kbbs2q91vjxnhas7811m3d3872m7x8gn6rka57xq"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-moose" ,perl-moose)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-simple" ,perl-test-simple)))
    (propagated-inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-datetime-locale" ,perl-datetime-locale)
       ("perl-datetime-timezone" ,perl-datetime-timezone)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-types" ,perl-moosex-types)
       ("perl-namespace-clean" ,perl-namespace-clean)))
    (home-page "https://metacpan.org/release/MooseX-Types-DateTime")
    (synopsis "DateTime related constraints and coercions for Moose")
    (description "This module packages several Moose::Util::TypeConstraints
with coercions, designed to work with the DateTime suite of objects.")
    (license (package-license perl))))

(define-public perl-moosex-types-datetime-morecoercions
  (package
    (name "perl-moosex-types-datetime-morecoercions")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "MooseX-Types-DateTime-MoreCoercions-"
                           version ".tar.gz"))
       (sha256
        (base32 "15ip1rgaana2p4vww355jb5jxyawim0k58gadkdqx20rfxckmfr1"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-simple" ,perl-test-simple)))
    (propagated-inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-datetimex-easy" ,perl-datetimex-easy)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-types" ,perl-moosex-types)
       ("perl-moosex-types-datetime" ,perl-moosex-types-datetime)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-time-duration-parse" ,perl-time-duration-parse)))
    (home-page
     "https://metacpan.org/release/MooseX-Types-DateTime-MoreCoercions")
    (synopsis "Extensions to MooseX::Types::DateTime")
    (description "This module builds on MooseX::Types::DateTime to add
additional custom types and coercions.  Since it builds on an existing type,
all coercions and constraints are inherited.")
    (license (package-license perl))))

(define-public perl-moosex-types-loadableclass
  (package
    (name "perl-moosex-types-loadableclass")
    (version "0.015")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "MooseX-Types-LoadableClass-" version ".tar.gz"))
       (sha256
        (base32 "1x1vb96hcrd96bzs73w0lb04jr0fvax1ams38qlzkp2kh9vx6dz0"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-moose" ,perl-moose)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-class-load" ,perl-class-load)))
    (propagated-inputs
     `(("perl-module-runtime" ,perl-module-runtime)
       ("perl-moosex-types" ,perl-moosex-types)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)))
    (home-page "https://metacpan.org/release/MooseX-Types-LoadableClass")
    (synopsis "ClassName type constraints for Moose")
    (description "MooseX::Types::LoadableClass provides a ClassName type
constraint with coercion to load the class.")
    (license (package-license perl))))

(define-public perl-moox
  (package
    (name "perl-moox")
    (version "0.101")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/G/GE/GETTY/MooX-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1m9jvrqcidiabdih211byadwnnkygafq54r2ljnf1akqdrjimy9g"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-data-optlist" ,perl-data-optlist)
       ("perl-import-into" ,perl-import-into)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-moo" ,perl-moo)))
    (home-page "https://metacpan.org/release/MooX")
    (synopsis
     "Using Moo and MooX:: packages the most lazy way")
    (description "Contains the MooX and MooX::Role packages.")
    (license license:perl-license)))

(define-public perl-moox-cmd
  (package
    (name "perl-moox-cmd")
    (version "0.017")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/MooX-Cmd-"
                           version ".tar.gz"))
       (sha256
        (base32 "1xbhmq07v9z371ygkyghva9aryhc22kwbzn5qwkp72c0ma6z4gwl"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-list-moreutils" ,perl-list-moreutils)))
    (propagated-inputs
     `(("perl-module-pluggable" ,perl-module-pluggable)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-moo" ,perl-moo)
       ("perl-package-stash" ,perl-package-stash)
       ("perl-params-util" ,perl-params-util)
       ("perl-regexp-common" ,perl-regexp-common)))
    (home-page "https://metacpan.org/release/MooX-Cmd")
    (synopsis "Giving an easy Moo style way to make command organized CLI apps")
    (description "This package eases the writing of command line utilities,
accepting commands and subcommands and so on.  These commands can form a tree,
which is mirrored in the package structure.  On invocation, each command along
the path through the tree (starting from the top-level command through to the
most specific one) is instantiated.")
    (license (package-license perl))))

(define-public perl-moox-configfromfile
  (package
    (name "perl-moox-configfromfile")
    (version "0.008")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "MooX-ConfigFromFile-" version ".tar.gz"))
       (sha256
        (base32
         "1zrpz4mzngnhaap6988is0w0aarilfj4kb1yc8hvfqna69lywac0"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-hash-merge" ,perl-hash-merge)
       ("perl-json" ,perl-json)
       ("perl-moox-cmd" ,perl-moox-cmd)))
    (propagated-inputs
     `(("perl-config-any" ,perl-config-any)
       ("perl-file-configdir" ,perl-file-configdir)
       ("perl-file-find-rule" ,perl-file-find-rule)
       ("perl-hash-merge" ,perl-hash-merge)
       ("perl-moo" ,perl-moo)
       ("perl-moox-file-configdir" ,perl-moox-file-configdir)
       ("perl-namespace-clean" ,perl-namespace-clean)))
    (home-page "https://metacpan.org/release/MooX-ConfigFromFile")
    (synopsis "Moo eXtension for initializing objects from config file")
    (description "This module is intended to easily load initialization values
for attributes on object construction from an appropriate config file.  The
building is done in @code{MooX::ConfigFromFile::Role}---using
@code{MooX::ConfigFromFile} ensures that the role is applied.")
    (license (package-license perl))))

(define-public perl-moox-file-configdir
  (package
    (name "perl-moox-file-configdir")
    (version "0.008")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "MooX-File-ConfigDir-" version ".tar.gz"))
       (sha256
        (base32 "1b033injzk9d8clgip67ps5j5bpkrnag28q89ddwhrgqx12i3m7q"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-configdir" ,perl-file-configdir)
       ("perl-moo" ,perl-moo)
       ("perl-namespace-clean" ,perl-namespace-clean)))
    (home-page "https://metacpan.org/release/MooX-File-ConfigDir")
    (synopsis "Moo eXtension for @code{File::ConfigDir}")
    (description "This module is a helper for easily finding configuration
file locations.  This information can be used to find a suitable place for
installing configuration files or for finding any piece of settings.")
    (license (package-license perl))))

(define-public perl-moox-handlesvia
  (package
    (name "perl-moox-handlesvia")
    (version "0.001009")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/T/TO/TOBYINK/MooX-HandlesVia-"
             version
             ".tar.gz"))
       (sha256
        (base32 "04kcyflg49rclxa1nm035c05jpyvhdacjyy1wklbgv4li3im6qvi"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-moox-types-mooselike"
        ,perl-moox-types-mooselike)
       ("perl-test-exception" ,perl-test-exception)
       ("perl-test-fatal" ,perl-test-fatal)))
    (inputs
     `(("perl-class-method-modifiers"
        ,perl-class-method-modifiers)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-moo" ,perl-moo)
       ("perl-role-tiny" ,perl-role-tiny)))
    (propagated-inputs
     `(("perl-data-perl" ,perl-data-perl)))
    (home-page
     "https://metacpan.org/release/MooX-HandlesVia")
    (synopsis "NativeTrait-like behavior for Moo")
    (description
     "@code{MooX::HandlesVia} is an extension of Moo's @code{handles}
attribute functionality.  It provides a means of proxying functionality from
an external class to the given attribute.")
    (license license:perl-license)))

(define-public perl-moox-late
  (package
    (name "perl-moox-late")
    (version "0.016")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/T/TO/TOBYINK/MooX-late-"
             version ".tar.gz"))
       (sha256
        (base32 "0kjy86rrpzfy6w5r9ykjq7njwdnvp7swd6r2k4gfrh3picz3kdhz"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (inputs
     `(("perl-moo" ,perl-moo)
       ("perl-moox" ,perl-moox)
       ("perl-moox-handlesvia" ,perl-moox-handlesvia)))
    (propagated-inputs
     `(("perl-type-tiny" ,perl-type-tiny)))
    (home-page "https://metacpan.org/release/MooX-late")
    (synopsis "Easily translate Moose code to Moo")
    (description
     "MooX::late does the following:
@enumerate
@item Supports isa => $stringytype
@item Supports does => $rolename
@item Supports lazy_build => 1
@item Exports blessed and confess functions to your namespace.
@item Handles certain attribute traits
Currently Hash, Array and Code are supported.  This feature requires
MooX::HandlesVia.
@end enumerate")
    (license license:perl-license)))

(define-public perl-moox-options
  (package
    (name "perl-moox-options")
    (version "4.023")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CE/CELOGEEK/"
                           "MooX-Options-" version ".tar.gz"))
       (sha256
        (base32
         "14kz51hybxx8vcm4wg36f0qa64aainw7i2sqmqxg20c3qvczyvj2"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-import-into" ,perl-import-into)
       ("perl-module-build" ,perl-module-build)
       ("perl-moo" ,perl-moo)
       ("perl-moose" ,perl-moose)
       ("perl-moox-cmd" ,perl-moox-cmd)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-role-tiny" ,perl-role-tiny)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-test-trap" ,perl-test-trap)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-try-tiny" ,perl-try-tiny)))
    (propagated-inputs
     `(("perl-config-any" ,perl-config-any)
       ("perl-moox-configfromfile" ,perl-moox-configfromfile)
       ("perl-data-record" ,perl-data-record)
       ("perl-file-configdir" ,perl-file-configdir)
       ("perl-file-find-rule" ,perl-file-find-rule)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-getopt-long-descriptive" ,perl-getopt-long-descriptive)
       ("perl-json-maybexs" ,perl-json-maybexs)
       ("perl-libintl-perl" ,perl-libintl-perl)
       ("perl-moox-configfromfile" ,perl-moox-configfromfile)
       ("perl-moox-file-configdir" ,perl-moox-file-configdir)
       ("perl-path-class" ,perl-path-class)
       ("perl-regexp-common" ,perl-regexp-common)
       ("perl-term-size-any" ,perl-term-size-any)
       ("perl-unicode-linebreak" ,perl-unicode-linebreak)))
    (home-page "https://metacpan.org/release/MooX-Options")
    (synopsis "Explicit Options eXtension for Object Class")
    (description "Create a command line tool with your Mo, Moo, Moose objects.
You have an @code{option} keyword to replace the usual @code{has} to
explicitly use your attribute on the command line.  The @code{option} keyword
takes additional parameters and uses @code{Getopt::Long::Descriptive} to
generate a command line tool.")
    (license (package-license perl))))

(define-public perl-moox-strictconstructor
  (package
    (name "perl-moox-strictconstructor")
    (version "0.010")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/H/HA/HARTZELL/MooX-StrictConstructor-"
               version
               ".tar.gz"))
        (sha256
         (base32
          "0vvjgz7xbfmf69yav7sxsxmvklqv835xvh7h47w0apxmlkm9fjgr"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-class-method-modifiers" ,perl-class-method-modifiers)
       ("perl-moo" ,perl-moo)
       ("perl-strictures" ,perl-strictures)))
    (home-page "https://metacpan.org/release/MooX-StrictConstructor")
    (synopsis "Make Moo-based object constructors blow up on unknown attributes")
    (description
     "Loading @code{MooX::StrictConstructor} makes your constructors \"strict\".
If your constructor is called with an attribute init argument that your class
does not declare, then it dies.")
    (license license:perl-license)))

(define-public perl-moox-types-mooselike
  (package
    (name "perl-moox-types-mooselike")
    (version "0.29")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MA/MATEU/"
                           "MooX-Types-MooseLike-" version ".tar.gz"))
       (sha256
        (base32 "1d6jg9x3p7gm2r0xmbcag374a44gf5pcga2swvxhlhzakfm80dqx"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-moo" ,perl-moo)
       ("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-module-runtime" ,perl-module-runtime)
       ("perl-strictures" ,perl-strictures)))
    (home-page "https://metacpan.org/release/MooX-Types-MooseLike")
    (synopsis "Moosish types and type builder")
    (description "MooX::Types::MooseLike provides a possibility to build your
own set of Moose-like types.  These custom types can then be used to describe
fields in Moo-based classes.")
    (license (package-license perl))))

(define-public perl-mouse
  (package
  (name "perl-mouse")
  (version "2.5.6")
  (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://cpan/authors/id/S/SK/SKAJI/Mouse-v"
                  version
                  ".tar.gz"))
            (sha256
             (base32
              "1j3048ip691j91rdig6wrlg6i4jdzhszxmz5pi2g7n355rl2w00l"))))
  (build-system perl-build-system)
  (native-inputs
   `(("perl-module-build" ,perl-module-build)
     ("perl-module-build-xsutil" ,perl-module-build-xsutil)
     ("perl-test-exception" ,perl-test-exception)
     ("perl-test-fatal" ,perl-test-fatal)
     ("perl-test-leaktrace" ,perl-test-leaktrace)
     ("perl-test-output" ,perl-test-output)
     ("perl-test-requires" ,perl-test-requires)
     ("perl-try-tiny" ,perl-try-tiny)))
  (home-page "https://github.com/gfx/p5-Mouse")
  (synopsis "Fast Moose-compatible object system for perl5")
  (description
   "Mouse is a @code{Moose} compatible object system that implements a
subset of the functionality for reduced startup time.")
  (license (package-license perl))))

(define-public perl-mousex-nativetraits
  (package
    (name "perl-mousex-nativetraits")
    (version "1.09")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/G/GF/GFUJI/"
                                  "MouseX-NativeTraits-" version ".tar.gz"))
              (sha256
               (base32
                "0pnbchkxfz9fwa8sniyjqp0mz75b3k2fafq9r09znbbh51dbz9gq"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-any-moose" ,perl-any-moose)
       ("perl-module-install" ,perl-module-install)
       ("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-mouse" ,perl-mouse)))
    (home-page "https://metacpan.org/release/MouseX-NativeTraits")
    (synopsis "Extend attribute interfaces for Mouse")
    (description
     "While @code{Mouse} attributes provide a way to name your accessors,
readers, writers, clearers and predicates, @code{MouseX::NativeTraits}
provides commonly used attribute helper methods for more specific types
of data.")
    (license (package-license perl))))

(define-public perl-mozilla-ca
  (package
    (name "perl-mozilla-ca")
    (version "20180117")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AB/ABH/Mozilla-CA-"
                           version ".tar.gz"))
       (sha256
        (base32
         "01p4ykyilk1639dxgjaa2n7rz1f0zbqxkq11yc9n6xcz26z9zk7j"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Mozilla-CA")
    (synopsis "Mozilla's CA cert bundle in PEM format")
    (description "@code{Mozilla::CA} provides a copy of Mozilla's bundle of
Certificate Authority certificates in a form that can be consumed by modules
and libraries based on OpenSSL.")
    (license license:mpl2.0)))

(define-public perl-multidimensional
  (package
    (name "perl-multidimensional")
    (version "0.014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/I/IL/ILMARI/multidimensional-"
             version ".tar.gz"))
       (sha256
        (base32
         "0prchsg547ziysjl8ghiid6ph3m2xnwpsrwrjymibga7fhqi9sqj"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-b-hooks-op-check" ,perl-b-hooks-op-check)
       ("perl-extutils-depends" ,perl-extutils-depends)))
    (propagated-inputs
     `(("perl-b-hooks-op-check" ,perl-b-hooks-op-check)
       ("perl-lexical-sealrequirehints" ,perl-lexical-sealrequirehints)))
    (home-page "https://metacpan.org/release/multidimensional")
    (synopsis "Disable multidimensional array emulation")
    (description
     "Multidimensional disables multidimensional array emulation.")
    (license (package-license perl))))

(define-public perl-mro-compat
  (package
    (name "perl-mro-compat")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "MRO-Compat-" version ".tar.gz"))
       (sha256
        (base32
         "1y547lr6zccf7919vx01v22zsajy528psanhg5aqschrrin3nb4a"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/MRO-Compat")
    (synopsis "MRO interface compatibility for Perls < 5.9.5")
    (description "The \"mro\" namespace provides several utilities for dealing
with method resolution order and method caching in general in Perl 5.9.5 and
higher.  This module provides those interfaces for earlier versions of
Perl (back to 5.6.0).")
    (license (package-license perl))))

(define-public perl-namespace-autoclean
  (package
    (name "perl-namespace-autoclean")
    (version "0.29")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "namespace-autoclean-" version ".tar.gz"))
       (sha256
        (base32 "012qqs561xyyhm082znmzsl8lz4n299fa6p0v246za2l9bkdiss5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-needs" ,perl-test-needs)))
    (propagated-inputs
     `(("perl-b-hooks-endofscope" ,perl-b-hooks-endofscope)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-sub-identify" ,perl-sub-identify)))
    (home-page "https://metacpan.org/release/namespace-autoclean")
    (synopsis "Keep imports out of your namespace")
    (description "The namespace::autoclean pragma will remove all imported
symbols at the end of the current package's compile cycle.  Functions called
in the package itself will still be bound by their name, but they won't show
up as methods on your class or instances.  It is very similar to
namespace::clean, except it will clean all imported functions, no matter if
you imported them before or after you used the pragma.  It will also not touch
anything that looks like a method.")
    (license (package-license perl))))

(define-public perl-namespace-clean
  (package
    (name "perl-namespace-clean")
    (version "0.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RI/RIBASUSHI/"
                           "namespace-clean-" version ".tar.gz"))
       (sha256
        (base32
         "17dg64pd4bwi2ad3p8ykwys1zha7kg8a8ykvks7wfg8q7qyah44a"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-package-stash" ,perl-package-stash)
       ("perl-b-hooks-endofscope" ,perl-b-hooks-endofscope)))
    (home-page "https://metacpan.org/release/namespace-clean")
    (synopsis "Keep imports and functions out of your namespace")
    (description "The namespace::clean pragma will remove all previously
declared or imported symbols at the end of the current package's compile
cycle.  Functions called in the package itself will still be bound by their
name, but they won't show up as methods on your class or instances.")
    (license (package-license perl))))

(define-public perl-net-bgp
  (package
    (name "perl-net-bgp")
    (version "0.17")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/S/SS/SSCHECK/Net-BGP-" version ".tar.gz"))
        (sha256 (base32 "0za8x9cn5n2hasb14p7dr537lggvrcsl23pgldxf5y03wmk6h35y"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Net-BGP")
    (synopsis "Object-oriented API to the BGP protocol")
    (description
      "This module is an implementation of the BGP-4 inter-domain routing protocol.
It encapsulates all of the functionality needed to establish and maintain a
BGP peering session and exchange routing update information with the peer.
It aims to provide a simple API to the BGP protocol for the purposes of
automation, logging, monitoring, testing, and similar tasks using the
power and flexibility of perl.  The module does not implement the
functionality of a RIB (Routing Information Base) nor does it modify the
kernel routing table of the host system.  However, such operations could be
implemented using the API provided by the module.")
  (license license:perl-license)))

(define-public perl-net-dns-native
  (package
    (name "perl-net-dns-native")
    (version "0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/O/OL/OLEG/Net-DNS-Native-"
             version ".tar.gz"))
       (sha256
        (base32 "1m9hbj83ikg52wvq7z8bjm78i50qvqk5alh11mmazzxrpbnrv38h"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Net-DNS-Native")
    (synopsis "Non-blocking system DNS resolver")
    (description
     "This class provides several methods for host name resolution.  It is
designed to be used with event loops.  Names are resolved by your system's
native @code{getaddrinfo(3)} implementation, called in a separate thread to
avoid blocking the entire application.  Threading overhead is limited by using
system threads instead of Perl threads.")
    (license license:perl-license)))

(define-public perl-net-idn-encode
  (package
    (name "perl-net-idn-encode")
    (version "2.500")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CF/CFAERBER/"
                           "Net-IDN-Encode-" version ".tar.gz"))
       (sha256
        (base32 "1aiy7adirk3wpwlczd8sldi9k1dray0jrg1lbcrcw97zwcrkciam"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-nowarnings" ,perl-test-nowarnings)))
    (home-page "https://metacpan.org/release/Net-IDN-Encode")
    (synopsis "Internationalizing Domain Names in Applications (IDNA)")
    (description
     "Internationalized Domain Names (IDNs) use characters drawn from a large
repertoire (Unicode), but IDNA allows the non-ASCII characters to be
represented using only the ASCII characters already allowed in so-called host
names today (letter-digit-hyphen, /[A-Z0-9-]/i).

Use this module if you just want to convert domain names (or email addresses),
using whatever IDNA standard is the best choice at the moment.")
    (license license:perl-license)))

(define-public perl-net-statsd
  (package
   (name "perl-net-statsd")
   (version "0.12")
   (source
    (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/C/CO/COSIMO/Net-Statsd-"
            version
            ".tar.gz"))
      (sha256
       (base32
        "0p2nhrwamic2fyj094y583q088ixv9gbb82c3invqrd17mh57r33"))))
   (build-system perl-build-system)
   (home-page
    "https://metacpan.org/release/Net-Statsd")
   (synopsis "Perl client for Etsy's statsd daemon")
   (description "This module implement a UDP client for the statsd statistics
collector daemon in use at Etsy.com.")
   (license (package-license perl))))

(define-public perl-number-compare
  (package
    (name "perl-number-compare")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Number-Compare-" version ".tar.gz"))
       (sha256
        (base32
         "09q8i0mxvr7q9vajwlgawsi0hlpc119gnhq4hc933d03x0vkfac3"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Number-Compare")
    (synopsis "Numeric comparisons")
    (description "Number::Compare compiles a simple comparison to an anonymous
subroutine, which you can call with a value to be tested against.")
    (license (package-license perl))))

(define-public perl-number-format
  (package
    (name "perl-number-format")
    (version "1.75")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/W/WR/WRW/Number-Format-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1wspw9fybik76jq9w1n1gmvfixd4wvlrq6ni8kyn85s62v5mkml2"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Number-Format")
    (synopsis "Convert numbers to strings with pretty formatting")
    (description "@code{Number::Format} is a library for formatting numbers.
Functions are provided for converting numbers to strings in a variety of ways,
and to convert strings that contain numbers back into numeric form.  The
output formats may include thousands separators - characters inserted between
each group of three characters counting right to left from the decimal point.
The characters used for the decimal point and the thousands separator come from
the locale information or can be specified by the user.")
    (license license:perl-license)))

(define-public perl-number-range
  (package
    (name "perl-number-range")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/L/LA/LARRYSH/Number-Range-"
             version ".tar.gz"))
       (sha256
        (base32
         "0999xvs3w2xprs14q4shqndjf2m6mzvhzdljgr61ddjaqhd84gj3"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Number-Range")
    (synopsis "Perl extension defining ranges of numbers")
    (description "Number::Range is an object-oriented interface to test if a
number exists in a given range, and to be able to manipulate the range.")
    (license (package-license perl))))

(define-public perl-object-signature
  (package
    (name "perl-object-signature")
    (version "1.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Object-Signature-" version ".tar.gz"))
       (sha256
        (base32 "12k90c19ly93ib1p6sm3k7sbnr2h5dbywkdmnff2ngm99p4m68c4"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (home-page "https://metacpan.org/release/Object-Signature")
    (synopsis "Generate cryptographic signatures for objects")
    (description "Object::Signature is an abstract base class that you can
inherit from in order to allow your objects to generate unique cryptographic
signatures.")
    (license (package-license perl))))

(define-public perl-ole-storage-lite
  (package
    (name "perl-ole-storage-lite")
    (version "0.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/J/JM/JMCNAMARA/OLE-Storage_Lite-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1fpqhhgb8blj4hhs97fsbnbhk29s9yms057a9s9yl20f3hbsc65b"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/OLE-Storage_Lite")
    (synopsis "Read and write OLE storage files")
    (description "This module allows you to read and write
an OLE-Structured file.  @dfn{OLE} (Object Linking and Embedding) is a
technology to store hierarchical information such as links to other
documents within a single file.")
    (license (package-license perl))))

(define-public perl-opengl
  (package
    (name "perl-opengl")
    (version "0.70")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/C/CH/CHM/OpenGL-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1q3lz168q081iwl9jg21fbzhp9la79gav9mv6nmh2jab83s2l3mj"))))
    (build-system perl-build-system)
    (inputs `(("freeglut" ,freeglut)
              ("libxi" ,libxi)
              ("libxmu" ,libxmu)))
    (arguments
     '(#:tests? #f ; test.pl fails with our empty glversion.txt, while
                   ; the package still seems to work on the examples
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'glversion
           ;; Building utils/glversion.txt fails, and is probably
           ;; dependent on the graphics card in the build system.
           ;; Replace it by a content-free file; while this breaks
           ;; the tests, the examples in the examples/ subdirectory
           ;; can be run.
           (lambda _
             (substitute* "Makefile.PL"
               (("unlink") "# unlink") ; prevent utils/glversion.txt
                                       ; from being deleted once...
               (("\\.\"\\$make_ver clean\"") "")) ; ...and twice...
             (substitute* "utils/Makefile"
               (("all: glversion.txt") "all: ")) ; ...and thrice.
             (call-with-output-file "utils/glversion.txt"
               (lambda (port)
                 (display (string-append "FREEGLUT=\nGLUT=\nVERSION=\n"
                                         "VENDOR=\nRENDERER=\n"
                                         "EXTENSIONS=\n")
                          port)))
             #t))
         (add-before 'configure 'fix-library-flags
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile.PL"
               (("-L/usr/local/freeglut/lib")
                (string-append "-L" (assoc-ref inputs "freeglut") "/lib\n"
                               "-L" (assoc-ref inputs "glu") "/lib\n"
                               "-L" (assoc-ref inputs "mesa") "/lib\n")))
             #t)))))
    (home-page "https://metacpan.org/release/OpenGL")
    (synopsis
      "Perl bindings to the OpenGL API, GLU, and GLUT/FreeGLUT")
    (description "The package provides Perl bindings to OpenGL, GLU
and FreeGLUT.")
    (license (package-license perl))))

(define-public perl-package-anon
  (package
    (name "perl-package-anon")
    (version "0.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AU/AUGGY/"
                           "Package-Anon-" version ".tar.gz"))
       (sha256
        (base32
         "1fj1fakkfklf2iwzsl64vfgshya3jgm6vhxiphw12wlac9g2il0m"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-sub-exporter" ,perl-sub-exporter)
       ("perl-params-util" ,perl-params-util)))
    (home-page "https://metacpan.org/release/Package-Anon")
    (synopsis "Anonymous packages")
    (description "This module allows for anonymous packages that are
independent of the main namespace and only available through an object
instance, not by name.")
    (license (package-license perl))))

(define-public perl-package-deprecationmanager
  (package
    (name "perl-package-deprecationmanager")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Package-DeprecationManager-" version ".tar.gz"))
       (sha256
        (base32
         "0jv8svfh1c1q4vxlkf8vjfbdq3n2sj3nx5llv1qrhp1b93d3lx0x"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-test-output" ,perl-test-output)))
    (propagated-inputs
     `(("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-params-util" ,perl-params-util)
       ("perl-sub-install" ,perl-sub-install)))
    (arguments `(#:tests? #f))          ;XXX: Failing for some reason...
    (home-page "https://metacpan.org/release/Package-DeprecationManager")
    (synopsis "Manage deprecation warnings for your distribution")
    (description "This module allows you to manage a set of deprecations for
one or more modules.")
    (license license:artistic2.0)))

(define-public perl-package-stash
  (package
    (name "perl-package-stash")
    (version "0.38")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Package-Stash-" version ".tar.gz"))
       (sha256
        (base32 "0zrs4byhlpq5ybnl0fd3y6pfzair6i2dyvzn7f7a7pgj9n2fi3n5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-dist-checkconflicts" ,perl-dist-checkconflicts)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-package-anon" ,perl-package-anon)))
    (propagated-inputs
     `(("perl-module-implementation" ,perl-module-implementation)
       ("perl-dist-checkconflicts" ,perl-dist-checkconflicts)
       ("perl-package-stash-xs" ,perl-package-stash-xs)))
    (home-page "https://metacpan.org/release/Package-Stash")
    (synopsis "Routines for manipulating stashes")
    (description "Manipulating stashes (Perl's symbol tables) is occasionally
necessary, but incredibly messy, and easy to get wrong.  This module hides all
of that behind a simple API.")
    (license (package-license perl))))

(define-public perl-package-stash-xs
  (package
    (name "perl-package-stash-xs")
    (version "0.29")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Package-Stash-XS-" version ".tar.gz"))
       (sha256
        (base32 "1akqk10qxwk798qppajqbczwmhy4cs9g0lg961m3vq218slnnryk"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-package-anon" ,perl-package-anon)))
    (home-page "https://metacpan.org/release/Package-Stash-XS")
    (synopsis "Faster implementation of the Package::Stash API")
    (description "This is a backend for Package::Stash, which provides the
functionality in a way that's less buggy and much faster.  It will be used by
default if it's installed, and should be preferred in all environments with a
compiler.")
    (license (package-license perl))))

(define-public perl-padwalker
  (package
    (name "perl-padwalker")
    (version "2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RO/ROBIN/"
                           "PadWalker-" version ".tar.gz"))
       (sha256
        (base32 "1kw8cnfyh6jbngm9q1kn003g08gis6l82h77d12yaq88c3xl8v1a"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/PadWalker")
    (synopsis "Play with other peoples' lexical variables")
    (description "PadWalker is a module which allows you to inspect (and even
change) lexical variables in any subroutine which called you.  It will only
show those variables which are in scope at the point of the call.  PadWalker
is particularly useful for debugging.")
    (license (package-license perl))))

(define-public perl-parallel-forkmanager
  (package
    (name "perl-parallel-forkmanager")
    (version "1.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/Y/YA/YANICK/Parallel-ForkManager-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0wm4wp6p3ah5z212jl12728z68nmxmfr0f03z1jpvdzffnc2xppi"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-warn" ,perl-test-warn)))
    (home-page "https://metacpan.org/release/Parallel-ForkManager")
    (synopsis "Simple parallel processing fork manager")
    (description "@code{Parallel::ForkManager} is intended for use in
operations that can be done in parallel where the number of
processes to be forked off should be limited.")
    (license (package-license perl))))

(define-public perl-params-classify
  (package
    (name "perl-params-classify")
    (version "0.015")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Params-Classify-"
            version ".tar.gz"))
      (sha256
       (base32
        "052r198xyrsv8wz21gijdigz2cgnidsa37nvyfzdiz4rv1fc33ir"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (propagated-inputs
     `(("perl-devel-callchecker" ,perl-devel-callchecker)))
    (home-page "https://metacpan.org/release/Params-Classify")
    (synopsis "Argument type classification")
    (description "This module provides various type-testing functions.
These are intended for functions that care what type of data they are
operating on.  There are two flavours of function.  Functions of the
first flavour provide type classification only.  Functions of the
second flavour also check that an argument is of an expected type.
The type enforcement functions handle only the simplest requirements
for arguments of the types handled by the classification functions.
Enforcement of more complex types may be built using the
classification functions, or it may be more convenient to use a module
designed for the more complex job, such as @code{Params::Validate}")
    (license license:perl-license)))

(define-public perl-params-util
  (package
    (name "perl-params-util")
    (version "1.102")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/R/RE/REHSACK/Params-Util-"
             version ".tar.gz"))
       (sha256
        (base32
         "00kl154zisf2zsl8yl6xa6yw54nhd9cja5d5fyigs96vhasb36s9"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Params-Util")
    (synopsis "Simple, compact and correct param-checking functions")
    (description
     "Params::Util provides a basic set of importable functions that makes
checking parameters easier.")
    (license (package-license perl))))

(define-public perl-params-validate
  (package
    (name "perl-params-validate")
    (version "1.29")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Params-Validate-" version ".tar.gz"))
       (sha256
        (base32
         "0cwpf8yxwyxbnwhf6rx4wnaq1q38j38i34a78a005shb8gxqv9j9"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-module-implementation" ,perl-module-implementation)))
    (home-page "https://metacpan.org/release/Params-Validate")
    (synopsis "Validate method/function parameters")
    (description "The Params::Validate module allows you to validate method or
function call parameters to an arbitrary level of specificity.")
    (license license:artistic2.0)))

(define-public perl-params-validationcompiler
  (package
    (name "perl-params-validationcompiler")
    (version "0.30")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                            "Params-ValidationCompiler-" version ".tar.gz"))
        (sha256
         (base32 "1jqn1l4m4i341g14kmjsf3a1kn7vv6z89cix0xjjgr1v70iywnyw"))))
    (build-system perl-build-system)
    (native-inputs
     ;; For tests.
     `(("perl-test-without-module" ,perl-test-without-module)
       ("perl-test2-plugin-nowarnings" ,perl-test2-plugin-nowarnings)
       ("perl-test2-suite" ,perl-test2-suite)
       ("perl-type-tiny" ,perl-type-tiny)))
    (propagated-inputs
     `(("perl-eval-closure" ,perl-eval-closure)
       ("perl-exception-class" ,perl-exception-class)
       ("perl-specio" ,perl-specio)))
    (home-page "https://github.com/houseabsolute/Params-ValidationCompiler")
    (synopsis "Build an optimized subroutine parameter validator")
    (description "This module creates a customized, highly efficient
parameter checking subroutine.  It can handle named or positional
parameters, and can return the parameters as key/value pairs or a list
of values.  In addition to type checks, it also supports parameter
defaults, optional parameters, and extra \"slurpy\" parameters.")
    (license license:artistic2.0)))

(define-public perl-par-dist
  (package
    (name "perl-par-dist")
    (version "0.49")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RS/RSCHUPP/"
                           "PAR-Dist-" version ".tar.gz"))
       (sha256
        (base32
         "078ycyn8pw3rba4k3qwcqrqfcym5c1pivymwa0bvs9sab45j4iwy"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/PAR-Dist")
    (synopsis "Create and manipulate PAR distributions")
    (description "PAR::Dist is a toolkit to create and manipulate PAR
distributions.")
    (license (package-license perl))))

(define-public perl-parent
  (deprecated-package "perl-parent" perl))

(define-public perl-path-class
  (package
    (name "perl-path-class")
    (version "0.37")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KW/KWILLIAMS/"
                           "Path-Class-" version ".tar.gz"))
       (sha256
        (base32
         "1kj8q8dmd8jci94w5arav59nkp0pkxrkliz4n8n6yf02hsa82iv5"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/Path-Class")
    (synopsis "Path specification manipulation")
    (description "Path::Class is a module for manipulation of file and
directory specifications in a cross-platform manner.")
    (license (package-license perl))))

(define-public perl-pathtools
  (package
    (name "perl-pathtools")
    (version "3.75")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/X/XS/XSAWYERX/PathTools-"
             version ".tar.gz"))
       (sha256
        (base32 "18j5z71xin9dsqddl6khm838d23p3843jcq7q0kwgy5ilqx50n55"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-pwd-path
           (lambda* (#:key inputs  #:allow-other-keys)
             (substitute* "Cwd.pm"
               (("'/bin/pwd'")
                (string-append "'" (assoc-ref inputs "coreutils")
                               "/bin/pwd'")))
             #t)))))
    (inputs
     `(("coreutils" ,coreutils)))
    (home-page "https://metacpan.org/release/PathTools")
    (synopsis "Tools for working with directory and file names")
    (description "This package provides functions to work with directory and
file names.")
    (license license:perl-license)))

(define-public perl-path-tiny
  (package
    (name "perl-path-tiny")
    (version "0.118")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                                  "Path-Tiny-" version ".tar.gz"))
              (sha256
               (base32
                "1zdhc3azw6wn21db3yyygs57vlqkx72ipyd8sa21m72c1y6qs4rj"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f)) ; Tests require additional test modules to be packaged
    ;; (native-inputs
    ;;  `(("perl-test-failwarnings" ,perl-test-failwarnings)
    ;;    ("perl-test-mockrandom" ,perl-test-mockrandom)))
    (inputs
     `(("perl-unicode-utf8" ,perl-unicode-utf8)))
    (home-page "https://metacpan.org/release/Path-Tiny")
    (synopsis "File path utility")
    (description "This module provides a small, fast utility for working
with file paths.")
    (license license:asl2.0)))

(define-public perl-pdf-api2
  (package
    (name "perl-pdf-api2")
    (version "2.039")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/S/SS/SSIMMS/PDF-API2-"
                    version ".tar.gz"))
              (sha256
               (base32
                "08x1anlvmxwym081flkrz7wr9q89y9wbq8ajqk72xsbzhhc479rn"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)
       ("perl-test-memory-cycle" ,perl-test-memory-cycle)))
    (propagated-inputs
     `(("perl-font-ttf" ,perl-font-ttf)))
    (home-page "https://metacpan.org/release/PDF-API2")
    (synopsis "Facilitates the creation and modification of PDF files")
    (description "This Perl module facilitates the creation and modification
of PDF files.")
    (license license:lgpl2.1)))

(define-public perl-perlio-utf8_strict
  (package
    (name "perl-perlio-utf8-strict")
    (version "0.007")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/L/LE/LEONT/PerlIO-utf8_strict-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1jw1ri8nkm4ck73arbsld1y2qgj2b9ir01y8mzb3mjs6w0pkz8w3"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)))
    (home-page
     "https://metacpan.org/release/PerlIO-utf8_strict")
    (synopsis "Fast and correct UTF-8 IO")
    (description "@code{PerlIO::utf8_strict} provides a fast and correct UTF-8
PerlIO layer.  Unlike Perl's default @code{:utf8} layer it checks the input
for correctness.")
    (license (package-license perl))))

(define-public perl-pegex
  (package
   (name "perl-pegex")
   (version "0.70")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/I/IN/INGY/Pegex-"
           version ".tar.gz"))
     (sha256
      (base32
       "1zd0zm6vxapw6bds3ipymkbzam70p3j3rm48794qy11620r22dgx"))))
   (build-system perl-build-system)
   (native-inputs
    `(("perl-file-sharedir-install" ,perl-file-sharedir-install)
      ("perl-yaml-libyaml" ,perl-yaml-libyaml)))
   (home-page "https://metacpan.org/release/Pegex")
   (synopsis "Acmeist PEG Parser Framework")
   (description "Pegex is an Acmeist parser framework.  It allows you to easily
create parsers that will work equivalently in lots of programming languages.
The inspiration for Pegex comes from the parsing engine upon which the
postmodern programming language Perl 6 is based on.  Pegex brings this beauty
to the other justmodern languages that have a normal regular expression engine
available.")
   (license (package-license perl))))

(define-public perl-pod-coverage
  (package
    (name "perl-pod-coverage")
    (version "0.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Pod-Coverage-" version ".tar.gz"))
       (sha256
        (base32
         "01xifj83dv492lxixijmg6va02rf3ydlxly0a9slmx22r6qa1drh"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-devel-symdump" ,perl-devel-symdump)))
    (home-page "https://metacpan.org/release/Pod-Coverage")
    (synopsis "Check for comprehensive documentation of a module")
    (description "This module provides a mechanism for determining if the pod
for a given module is comprehensive.")
    (license (package-license perl))))

(define-public perl-pod-simple
  (package
    (name "perl-pod-simple")
    (version "3.42")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/K/KH/KHW/"
                                  "Pod-Simple-" version ".tar.gz"))
              (sha256
               (base32
                "1icagrjqw1azmff82h17cbrhqgql7rg21gz64mjpiqqq0cpfpz59"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Pod-Simple")
    (synopsis "Parsing library for text in Pod format")
    (description "@code{Pod::Simple} is a Perl library for parsing text in
the @dfn{Pod} (plain old documentation) markup language that is typically
used for writing documentation for Perl and for Perl modules.")
    (license (package-license perl))))

(define-public perl-posix-strftime-compiler
  (package
    (name "perl-posix-strftime-compiler")
    (version "0.42")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KA/KAZEBURO/"
                           "POSIX-strftime-Compiler-" version ".tar.gz"))
       (sha256
        (base32
         "04dcn2n4rfkj8p24vj2p17vvis40l87pf2vdqp0vqm5jg3fjnn16"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-module-build" ,perl-module-build)))
    (arguments `(#:tests? #f))          ; TODO: Timezone test failures
    (home-page "https://metacpan.org/release/POSIX-strftime-Compiler")
    (synopsis "GNU C library compatible strftime for loggers and servers")
    (description "POSIX::strftime::Compiler provides GNU C library compatible
strftime(3).  But this module is not affected by the system locale.  This
feature is useful when you want to write loggers, servers, and portable
applications.")
    (license (package-license perl))))

(define-public perl-ppi
  (package
    (name "perl-ppi")
    (version "1.270")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MITHALDU/"
                           "PPI-" version ".tar.gz"))
       (sha256
        (base32 "0mzlz9rxqx93rqgy16jcfxwkplvhzr0f1gvvvwmmvf0vg266jak2"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f))                    ;FIXME: some tests fail
    (native-inputs
     `(("perl-class-inspector" ,perl-class-inspector)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-nowarnings" ,perl-test-nowarnings)
       ("perl-test-object" ,perl-test-object)
       ("perl-test-subcalls" ,perl-test-subcalls)))
    (propagated-inputs
     `(("perl-clone" ,perl-clone)
       ("perl-io-string" ,perl-io-string)
       ("perl-params-util" ,perl-params-util)
       ("perl-task-weaken" ,perl-task-weaken)))
    (home-page "https://metacpan.org/release/PPI")
    (synopsis "Parse, analyze and manipulate Perl (without Perl)")
    (description "The PPI module parses, analyzes and manipulates Perl
code.")
    (license license:perl-license)))

(define-public perl-probe-perl
  (package
    (name "perl-probe-perl")
    (version "0.03")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/K/KW/KWILLIAMS/"
                                  "Probe-Perl-" version ".tar.gz"))
              (sha256
               (base32
                "0c9wiaz0mqqknafr4jdr0g2gdzxnn539182z0icqaqvp5qgd5r6r"))))
    (build-system perl-build-system)
    (synopsis "Information about the currently running perl")
    (description
     "Probe::Perl provides methods for obtaining information about the
currently running perl interpreter.  It originally began life as code in the
Module::Build project, but has been externalized here for general use.")
    (home-page "https://metacpan.org/release/Probe-Perl")
    (license (package-license perl))))

(define-public perl-proc-invokeeditor
  (package
    (name "perl-proc-invokeeditor")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MS/MSTEVENS/Proc-InvokeEditor-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0xc1416kvhq904ribpwh2lbxryh41dzl2glzpgr32b68s4fbwbaa"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-EDITOR
           (lambda _ (setenv "EDITOR" "echo") #t)))))
    (propagated-inputs
     `(("perl-carp-assert" ,perl-carp-assert)))
    (home-page "https://metacpan.org/release/Proc-InvokeEditor")
    (synopsis "Interface to external editor from Perl")
    (description "This module provides the ability to supply some text to an
external text editor, have it edited by the user, and retrieve the results.")
    (license (package-license perl))))

(define-public perl-readonly
  (package
    (name "perl-readonly")
    (version "2.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SA/SANKO/"
                           "Readonly-" version ".tar.gz"))
       (sha256
        (base32
         "165zcf9lpijdpkx82za0g9rx8ckjnhipmcivdkyzshl8jmp1bl4v"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/Readonly")
    (synopsis "Create read-only scalars, arrays, hashes")
    (description "This module provides a facility for creating non-modifiable
variables in Perl.  This is useful for configuration files, headers, etc.  It
can also be useful as a development and debugging tool for catching updates to
variables that should not be changed.")
    (license (package-license perl))))

(define-public perl-ref-util-xs
  (package
    (name "perl-ref-util-xs")
    (version "0.117")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/X/XS/XSAWYERX/"
                           "Ref-Util-XS-" version ".tar.gz"))
       (sha256
        (base32
         "0g33cndhj353h5xjihvgjc2h6vxwkyyzw63r4l06czvq4flcar7v"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Ref-Util-XS")
    (synopsis "XS implementation for Ref::Util")
    (description "@code{Ref::Util::XS} is the XS implementation of
@code{Ref::Util}, which provides several functions to help identify references
in a more convenient way than the usual approach of examining the return value
of @code{ref}.")
    (license license:x11)))

(define-public perl-regexp-common
  (package
    (name "perl-regexp-common")
    (version "2017060201")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AB/ABIGAIL/"
                                  "Regexp-Common-" version ".tar.gz"))
              (sha256
               (base32
                "16q8d7mx0c4nbjrvj69jdn4q33d1k40imgxn83h11wq6xqx8a1zf"))))
    (build-system perl-build-system)
    (synopsis "Provide commonly requested regular expressions")
    (description
     "This module exports a single hash (@code{%RE}) that stores or generates
commonly needed regular expressions.  Patterns currently provided include:
balanced parentheses and brackets, delimited text (with escapes), integers and
floating-point numbers in any base (up to 36), comments in 44 languages,
offensive language, lists of any pattern, IPv4 addresses, URIs, and Zip
codes.")
    (home-page "https://metacpan.org/release/Regexp-Common")
    ;; Quad-licensed: Perl Artistic, Perl Artistic 2.0, X11, and BSD.
    (license (list (package-license perl) license:x11 license:bsd-3))))

(define-public perl-regexp-util
  (package
    (name "perl-regexp-util")
    (version "0.003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOBYINK/"
                           "Regexp-Util-" version ".tar.gz"))
       (sha256
        (base32
         "01n1cggiflsnp9f6adkcxzkc0qpgssz60cwnyyd8mzavh2ximr5a"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Regexp-Util")
    (synopsis "Selection of general-utility regexp subroutines")
    (description "This package provides a selection of regular expression
subroutines including @code{is_regexp}, @code{regexp_seen_evals},
@code{regexp_is_foreign}, @code{regexp_is_anchored}, @code{serialize_regexp},
and @code{deserialize_regexp}.")
    (license (package-license perl))))

(define-public perl-role-tiny
  (package
    (name "perl-role-tiny")
    (version "1.003004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Role-Tiny-" version ".tar.gz"))
       (sha256
        (base32
         "0ak60hakn0ixmsiw403si0lf5pagq5r6wjgl7p0pr979nlcikfmd"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-namespace-autoclean" ,perl-namespace-autoclean)
       ("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-class-method-modifiers" ,perl-class-method-modifiers)))
    (home-page "https://metacpan.org/release/Role-Tiny")
    (synopsis "Roles, as a slice of Moose")
    (description "Role::Tiny is a minimalist role composition tool.")
    (license (package-license perl))))

;; Some packages don't yet work with this newer version of ‘Role::Tiny’.
(define-public perl-role-tiny-2
  (package
    (inherit perl-role-tiny)
    (version "2.001004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Role-Tiny-" version ".tar.gz"))
       (sha256
        (base32 "11qn516352yhi794www3ykwa9xv2gxpfnhn9jcn10x0ahl95gflj"))))))

(define-public perl-safe-isa
  (package
    (name "perl-safe-isa")
    (version "1.000010")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Safe-Isa-" version ".tar.gz"))
       (sha256
        (base32
         "0sm6p1kw98s7j6n92vvxjqf818xggnmjwci34xjmw7gzl2519x47"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Safe-Isa")
    (synopsis "Call isa, can, does, and DOES safely")
    (description "This module allows you to call isa, can, does, and DOES
safely on things that may not be objects.")
    (license (package-license perl))))

(define-public perl-scalar-string
  (package
    (name "perl-scalar-string")
    (version "0.003")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Scalar-String-"
            version ".tar.gz"))
      (sha256
       (base32
        "0llbsqk7rsg9p7l1f4yk6iv7wij91gvavprsqhnb04w7nz4ifjpm"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (home-page "https://metacpan.org/release/Scalar-String")
    (synopsis "String aspects of scalars")
    (description "@code{Scalar::String} is about the string part of
plain Perl scalars.  A scalar has a string value, which is notionally
a sequence of Unicode codepoints but may be internally encoded in
either ISO-8859-1 or UTF-8.  In places, more so in older versions of
Perl, the internal encoding shows through.  To fully understand Perl
strings it is necessary to understand these implementation details.
This module provides functions to classify a string by encoding and to
encode a string in a desired way.  The module is implemented in XS,
with a pure Perl backup version for systems that cannot handle XS.")
    (license license:perl-license)))

(define-public perl-scope-guard
  (package
    (name "perl-scope-guard")
    (version "0.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CH/CHOCOLATE/"
                           "Scope-Guard-" version ".tar.gz"))
       (sha256
        (base32
         "0y6jfzvxiz8h5yfz701shair0ilypq2mvimd7wn8wi2nbkm1p6wc"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Scope-Guard")
    (synopsis "Lexically-scoped resource management")
    (description "This module provides a convenient way to perform cleanup or
other forms of resource management at the end of a scope.  It is particularly
useful when dealing with exceptions: the Scope::Guard constructor takes a
reference to a subroutine that is guaranteed to be called even if the thread
of execution is aborted prematurely.  This effectively allows lexically-scoped
\"promises\" to be made that are automatically honoured by perl's garbage
collector.")
    (license (package-license perl))))

(define-public perl-set-infinite
  (package
    (name "perl-set-infinite")
    (version "0.65")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FG/FGLOCK/"
                           "Set-Infinite-" version ".tar.gz"))
       (sha256
        (base32
         "07vyp0jpndcxkbyjk432nillxxk22wrmm2rs985y8ba96h3qig07"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Set-Infinite")
    (synopsis "Infinite sets")
    (description "Set::Infinite is a set theory module for infinite sets.")
    (license (package-license perl))))

(define-public perl-set-intspan
  (package
    (name "perl-set-intspan")
    (version "1.19")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/S/SW/SWMCD/Set-IntSpan-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1l6znd40ylzvfwl02rlqzvakv602rmvwgm2xd768fpgc2fdm9dqi"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Set-IntSpan")
    (synopsis "Manage sets of integers")
    (description "@code{Set::IntSpan} manages sets of integers.  It is
optimized for sets that have long runs of consecutive integers.")
    (license license:perl-license)))

(define-public perl-set-object
  (package
    (name "perl-set-object")
    (version "1.39")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RU/RURBAN/"
                           "Set-Object-" version ".tar.gz"))
       (sha256
        (base32 "040q819l9x55j0hjhfvc153451syvjffw3d22gs398sd23mwzzsy"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-moose" ,perl-moose)
       ("perl-test-leaktrace" ,perl-test-leaktrace)))
    (home-page "https://metacpan.org/release/Set-Object")
    (synopsis "Unordered collections of Perl Objects")
    (description "Set::Object provides efficient sets, unordered collections
of Perl objects without duplicates for scalars and references.")
    (license license:artistic2.0)))

(define-public perl-set-scalar
  (package
    (name "perl-set-scalar")
    (version "1.29")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAVIDO/"
                           "Set-Scalar-" version ".tar.gz"))
       (sha256
        (base32
         "07aiqkyi1p22drpcyrrmv7f8qq6fhrxh007achy2vryxyck1bp53"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Set-Scalar")
    (synopsis "Set operations for Perl")
    (description "The first priority of Set::Scalar is to be a convenient
interface to sets (as in: unordered collections of Perl scalars).  While not
designed to be slow or big, neither has it been designed to be fast or
compact.")
    (license (package-license perl))))

(define-public perl-sort-key
  (package
    (name "perl-sort-key")
    (version "1.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SA/SALVA/Sort-Key-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1kqs10s2plj6c96srk0j8d7xj8dxk1704r7mck8rqk09mg7lqspd"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Sort-Key")
    (synopsis "Sort arrays by one or multiple calculated keys")
    (description "This Perl module provides various functions to quickly sort
arrays by one or multiple calculated keys.")
    (license (package-license perl))))

(define-public perl-sort-naturally
  (package
    (name "perl-sort-naturally")
    (version "1.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BI/BINGOS/Sort-Naturally-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0ip7q5g8d3lr7ri3ffcbrpk1hzzsiwgsn14k10k7hnjphxf1raza"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Sort-Naturally")
    (synopsis "Sort lexically, but sort numeral parts numerically")
    (description "This module exports two functions, @code{nsort} and
@code{ncmp}; they are used in implementing a \"natural sorting\" algorithm.
Under natural sorting, numeric substrings are compared numerically, and other
word-characters are compared lexically.")
    (license (package-license perl))))

(define-public perl-specio
  (package
    (name "perl-specio")
    (version "0.38")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Specio-" version ".tar.gz"))
       (sha256
        (base32
         "1s5xd9awwrzc94ymimjkxqs6jq513wwlmwwarxaklvg2hk4lps0l"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-devel-stacktrace" ,perl-devel-stacktrace)
       ("perl-eval-closure" ,perl-eval-closure)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-mro-compat" ,perl-mro-compat)
       ("perl-role-tiny" ,perl-role-tiny)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-needs" ,perl-test-needs)))
    (home-page "https://metacpan.org/release/Specio")
    (synopsis "Classes for representing type constraints and coercion")
    (description "The Specio distribution provides classes for representing type
constraints and coercion, along with syntax sugar for declaring them.  Note that
this is not a proper type system for Perl. Nothing in this distribution will
magically make the Perl interpreter start checking a value's type on assignment
to a variable. In fact, there's no built-in way to apply a type to a variable at
all.  Instead, you can explicitly check a value against a type, and optionally
coerce values to that type.")
    (license license:artistic2.0)))

(define-public perl-spiffy
  (package
    (name "perl-spiffy")
    (version "0.46")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IN/INGY/"
                           "Spiffy-" version ".tar.gz"))
       (sha256
        (base32
         "18qxshrjh0ibpzjm2314157mxlibh3smyg64nr4mq990hh564n4g"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Spiffy")
    (synopsis "Spiffy Perl Interface Framework For You")
    (description "Spiffy is a framework and methodology for doing object
oriented (OO) programming in Perl.  Spiffy combines the best parts of
Exporter.pm, base.pm, mixin.pm and SUPER.pm into one magic foundation class.
It attempts to fix all the nits and warts of traditional Perl OO, in a clean,
straightforward and (perhaps someday) standard way.  Spiffy borrows ideas from
other OO languages like Python, Ruby, Java and Perl 6.")
    (license (package-license perl))))

(define-public perl-want
  (package
    (name "perl-want")
    (version "0.29")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/R/RO/ROBIN/Want-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1xsjylbxxcbkjazqms49ipi94j1hd2ykdikk29cq7dscil5p9r5l"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Want")
    (synopsis "Generalization of wantarray")
    (description "This module generalises the mechanism of the
@code{wantarray} function, allowing a function to determine in some detail how
its return value is going to be immediately used.")
    (license license:perl-license)))

(define-public perl-contextual-return
  (package
    (name "perl-contextual-return")
    (version "0.004014")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/D/DC/DCONWAY/Contextual-Return-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0y4zf5qkpayp4kkg7lw9ydbbin1z99m6xvy02fgacjbfw4ai9zh9"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-want" ,perl-want)))
    (home-page "https://metacpan.org/release/Contextual-Return")
    (synopsis "Create context-sensitive return values")
    (description "This module provides a collection of named blocks that allow
a return statement to return different values depending on the context in
which it is called.")
    (license license:perl-license)))

(define-public perl-statistics-basic
  (package
    (name "perl-statistics-basic")
    (version "1.6611")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/J/JE/JETTERO/Statistics-Basic-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1ywl398z42hz9w1k0waf1caa6agz8jzsjlf4rzs1lgpx2mbcwmb8"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-number-format" ,perl-number-format)))
    (home-page "https://metacpan.org/release/Statistics-Basic")
    (synopsis "Collection of very basic statistics modules")
    (description "This package provides basic statistics functions like
@code{median()}, @code{mean()}, @code{variance()} and @code{stddev()}.")
    (license license:lgpl2.0)))

(define-public perl-statistics-pca
  (package
    (name "perl-statistics-pca")
    (version "0.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/D/DS/DSTH/Statistics-PCA-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1i3bskwibp54c9a2wx8gzr3hyds6mmhr3d550g8j6893005v3bgq"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-contextual-return" ,perl-contextual-return)
       ("perl-math-cephes" ,perl-math-cephes)
       ("perl-math-matrixreal" ,perl-math-matrixreal)
       ("perl-text-simpletable" ,perl-text-simpletable)))
    (home-page "https://metacpan.org/release/Statistics-PCA")
    (synopsis "Perl implementation of Principal Component Analysis")
    (description "This package provides the Statistics::PCA module, an
implementation of @dfn{Principal Component Analysis} (PCA).")
    (license license:perl-license)))

(define-public perl-stream-buffered
  (package
    (name "perl-stream-buffered")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                           "Stream-Buffered-" version ".tar.gz"))
       (sha256
        (base32
         "0fs2n9zw6isfkha2kbqrvl9mwg572x1x0jlfaps0qsyynn846bcv"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Stream-Buffered")
    (synopsis "Temporary buffer to save bytes")
    (description "Stream::Buffered is a buffer class to store arbitrary length
of byte strings and then get a seekable filehandle once everything is
buffered.  It uses PerlIO and/or temporary file to save the buffer depending
on the length of the size.")
    (license (package-license perl))))

(define-public perl-strictures
  (package
    (name "perl-strictures")
    (version "1.005005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "strictures-" version ".tar.gz"))
       (sha256
        (base32
         "1bmpv8wr9jbc1lfj634xhq3y42nm28hh01jfsyzxhqhqf6dkdz59"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/strictures")
    (synopsis "Turn on strict and make all warnings fatal")
    (description "Strictures turns on strict and make all warnings fatal when
run from within a source-controlled directory.")
    (license (package-license perl))))

;; Some packages don't yet work with this newer version of ‘strictures’.
(define-public perl-strictures-2
  (package
    (inherit perl-strictures)
    (version "2.000006")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "strictures-" version ".tar.gz"))
       (sha256
        (base32 "0mwd9xqz4n8qfpi5h5581lbm33qhf7agww18h063icnilrs7km89"))))))

(define-public perl-string-camelcase
  (package
    (name "perl-string-camelcase")
    (version "0.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HI/HIO/"
                           "String-CamelCase-" version ".tar.gz"))
       (sha256
        (base32 "1a8i4yzv586svd0pbxls7642vvmyiwzh4x2xyij8gbnfxsydxhw9"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-perl-search-path
           (lambda _
             ;; Work around "dotless @INC" build failure.
             (setenv "PERL5LIB"
                     (string-append (getcwd) ":"
                                    (getenv "PERL5LIB")))
             #t)))))
    (home-page "https://metacpan.org/release/String-CamelCase")
    (synopsis "Camelcase and de-camelcase")
    (description "This module may be used to convert from under_score text to
CamelCase and back again.")
    (license (package-license perl))))

(define-public perl-string-escape
  (package
    (name "perl-string-escape")
    (version "2010.002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/E/EV/EVO/String-Escape-"
             version ".tar.gz"))
       (sha256
        (base32
         "12ls7f7847i4qcikkp3skwraqvjphjiv2zxfhl5d49326f5myr7x"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/String-Escape")
    (synopsis "Backslash escapes, quoted phrase, word elision, etc.")
    (description "This module provides a flexible calling interface to some
frequently-performed string conversion functions, including applying and
expanding standard C/Unix-style backslash escapes like \n and \t, wrapping and
removing double-quotes, and truncating to fit within a desired length.")
    (license (package-license perl))))

(define-public perl-string-formatter
  (package
    (name "perl-string-formatter")
    (version "0.102084")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/R/RJ/RJBS/String-Formatter-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0mlwm0rirv46gj4h072q8gdync5zxxsxy8p028gdyrhczl942dc3"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-params-util" ,perl-params-util)
       ("perl-sub-exporter" ,perl-sub-exporter)))
    (home-page "https://metacpan.org/release/String-Formatter")
    (synopsis "Build your own sprintf-like functions")
    (description
     "@code{String::Formatter} is a tool for building sprintf-like formatting
routines.  It supports named or positional formatting, custom conversions,
fixed string interpolation, and simple width-matching.")
    (license license:gpl2)))

(define-public perl-string-rewriteprefix
  (package
    (name "perl-string-rewriteprefix")
    (version "0.007")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "String-RewritePrefix-" version ".tar.gz"))
       (sha256
        (base32
         "18nxl1vgkcx0r7ifkmbl9fp73f8ihiqhqqf3vq6sj5b3cgawrfsw"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-sub-exporter" ,perl-sub-exporter)))
    (home-page "https://metacpan.org/release/String-RewritePrefix")
    (synopsis "Rewrite strings based on a set of known prefixes")
    (description "This module allows you to rewrite strings based on a set of
known prefixes.")
    (license (package-license perl))))

(define-public perl-string-shellquote
  (package
    (name "perl-string-shellquote")
    (version "1.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/R/RO/ROSCH/String-ShellQuote-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0dfxhr6hxc2majkkrm0qbx3qcbykzpphbj2ms93dc86f7183c1p6"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/String-ShellQuote")
    (synopsis "Quote strings for passing through a shell")
    (description
     "@code{shell-quote} lets you pass arbitrary strings through the shell so
that they won't be changed.")
    (license (package-license perl))))

(define-public perl-string-print
  (package
    (name "perl-string-print")
    (version "0.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "String-Print-" version ".tar.gz"))
              (sha256
               (base32
                "1n9lc5dr66sg89hym47764fyfms7vrxrhwvdps2x8x8gxly7rsdl"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-unicode-linebreak" ,perl-unicode-linebreak)))
    (home-page "https://metacpan.org/release/String-Print")
    (synopsis "String printing alternatives to printf")
    (description
     "This module inserts values into (translated) strings.  It provides
@code{printf} and @code{sprintf} alternatives via both an object-oriented and
a functional interface.")
    (license (package-license perl))))

(define-public perl-sub-exporter
  (package
    (name "perl-sub-exporter")
    (version "0.987")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/R/RJ/RJBS/Sub-Exporter-"
             version ".tar.gz"))
       (sha256
        (base32
         "1ml3n1ck4ln9qjm2mcgkczj1jb5n1fkscz9c4x23v4db0glb4g2l"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-data-optlist" ,perl-data-optlist)
       ("perl-params-util" ,perl-params-util)))
    (home-page "https://metacpan.org/release/Sub-Exporter")
    (synopsis "Sophisticated exporter for custom-built routines")
    (description
     "Sub::Exporter provides a sophisticated alternative to Exporter.pm for
custom-built routines.")
    (license (package-license perl))))

(define-public perl-sub-exporter-progressive
  (package
    (name "perl-sub-exporter-progressive")
    (version "0.001013")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FR/FREW/"
                           "Sub-Exporter-Progressive-" version ".tar.gz"))
       (sha256
        (base32
         "0mn0x8mkh36rrsr58s1pk4srwxh2hbwss7sv630imnk49navfdfm"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-sub-exporter" ,perl-sub-exporter)))
    (home-page "https://metacpan.org/release/Sub-Exporter-Progressive")
    (synopsis "Only use Sub::Exporter if you need it")
    (description "Sub::Exporter is an incredibly powerful module, but with
that power comes great responsibility, as well as some runtime penalties.
This module is a \"Sub::Exporter\" wrapper that will let your users just use
Exporter if all they are doing is picking exports, but use \"Sub::Exporter\"
if your users try to use \"Sub::Exporter\"'s more advanced features, like
renaming exports, if they try to use them.")
    (license (package-license perl))))

(define-public perl-sub-identify
  (package
    (name "perl-sub-identify")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RG/RGARCIA/"
                           "Sub-Identify-" version ".tar.gz"))
       (sha256
        (base32
         "0vxdxyfh6037xy88ic7500wydzmsxldhp95n8bld2kaihqh2g386"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Sub-Identify")
    (synopsis "Retrieve names of code references")
    (description "Sub::Identify allows you to retrieve the real name of code
references.")
    (license (package-license perl))))

(define-public perl-sub-info
  (package
    (name "perl-sub-info")
    (version "0.002")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/E/EX/EXODIST/Sub-Info-"
                            version ".tar.gz"))
        (sha256
         (base32
          "1snhrmc6gpw2zjnj7zvvqj69mlw711bxah6kk4dg5vxxjvb5cc7a"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-importer" ,perl-importer)))
    (home-page "https://metacpan.org/release/Sub-Info")
    (synopsis "Tool to inspect subroutines")
    (description "This package provides tools for inspecting subroutines
in Perl.")
    (license (package-license perl))))

(define-public perl-sub-install
  (package
    (name "perl-sub-install")
    (version "0.928")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/R/RJ/RJBS/Sub-Install-"
             version ".tar.gz"))
       (sha256
        (base32
         "03zgk1yh128gciyx3q77zxzxg9kf8yy2gm46gdxqi24mcykngrb1"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Sub-Install")
    (synopsis "Install subroutines into packages easily")
    (description
     "Sub::Install makes it easy to install subroutines into packages without
the unsightly mess of C<no strict> or typeglobs lying about where just anyone
can see them.")
    (license (package-license perl))))

(define-public perl-sub-name
  (package
    (name "perl-sub-name")
    (version "0.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Sub-Name-" version ".tar.gz"))
       (sha256
        (base32
         "05viq8scqk29g964fsfvls2rhvlb8myz3jblwh5c2ivhw3gfjcmx"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-devel-checkbin" ,perl-devel-checkbin)))
    (home-page "https://metacpan.org/release/Sub-Name")
    (synopsis "(Re)name a sub")
    (description "Assigns a new name to referenced sub.  If package
specification is omitted in the name, then the current package is used.  The
return value is the sub.")
    (license (package-license perl))))

(define-public perl-sub-quote
  (package
    (name "perl-sub-quote")
    (version "2.006006")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/H/HA/HAARG/Sub-Quote-"
             version ".tar.gz"))
       (sha256
        (base32 "17fq4iskrisnqs96amrz493vxikwvqbj9s7014k6vyl84gs2lkkf"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-sub-name" ,perl-sub-name)))
    (home-page "https://metacpan.org/release/Sub-Quote")
    (synopsis "Efficient generation of subroutines via string eval")
    (description "Sub::Quote provides an efficient generation of subroutines
via string eval.")
    (license (package-license perl))))

(define-public perl-sub-uplevel
  (package
    (name "perl-sub-uplevel")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "Sub-Uplevel-" version ".tar.gz"))
       (sha256
        (base32
         "1yzxqsim8vpavzqm2wfksh8dpmy6qbr9s3hdqqicp38br3lzd4qg"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Sub-Uplevel")
    (synopsis "Apparently run a function in a higher stack frame")
    (description "Like Tcl's uplevel() function, but not quite so dangerous.
The idea is just to fool caller().  All the really naughty bits of Tcl's
uplevel() are avoided.")
    (license (package-license perl))))

(define-public perl-super
  (package
    (name "perl-super")
    (version "1.20190531")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CH/CHROMATIC/"
                           "SUPER-" version ".tar.gz"))
       (sha256
        (base32 "16nk2za9fwyg7mcifacr69qi075iz1yvy8r9jh3903kzdvkiwpb8"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-sub-identify" ,perl-sub-identify)))
    (home-page "https://metacpan.org/release/SUPER")
    (synopsis "Control superclass method dispatching")
    (description
     "When subclassing a class, you may occasionally want to dispatch control to
the superclass---at least conditionally and temporarily.  This module provides
nicer equivalents to the native Perl syntax for calling superclasses, along with
a universal @code{super} method to determine a class' own superclass, and better
support for run-time mix-ins and roles.")
    (license license:perl-license)))

(define-public perl-svg
  (package
    (name "perl-svg")
    (version "2.84")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MA/MANWAR/SVG-"
                           version ".tar.gz"))
       (sha256
        (base32 "1br8dwh2363s6r0qgy7vv30gv5kj456vj5m6x83savx4wzfnsggc"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/SVG")
    (synopsis "Perl extension for generating SVG documents")
    (description "SVG is a Perl module which generates a nested data structure
containing the DOM representation of an SVG (Scalable Vector Graphics) image.
Using SVG, you can generate SVG objects, embed other SVG instances into it,
access the DOM object, create and access Javascript, and generate SMIL
animation content.")
    (license (package-license perl))))

(define-public perl-switch
  (package
    (name "perl-switch")
    (version "2.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CH/CHORNY/Switch-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0xbdjdgzfj9zwa4j3ipr8bfk7bcici4hk89hq5d27rhg2isljd9i"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Switch")
    (synopsis "Switch statement for Perl")
    (description "Switch is a Perl module which implements a generalized case
mechanism.  The module augments the standard Perl syntax with two new
statements: @code{switch} and @code{case}.")
    (license (package-license perl))))

(define-public perl-sys-cpu
  (package
    (name "perl-sys-cpu")
    (version "0.61")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MZ/MZSANFORD/"
                                  "Sys-CPU-" version ".tar.gz"))
              (sha256
               (base32
                "1r6976bs86j7zp51m5vh42xlyah951jgdlkimv202413kjvqc2i5"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; The contents of /proc/cpuinfo can differ and confuse the
                  ;; cpu_clock and cpu_type methods, so we replace the test
                  ;; with one that marks cpu_clock and cpu_type as TODO.
                  ;; Borrowed from Debian.
                  (call-with-output-file "t/Sys-CPU.t"
                    (lambda (port)
                      (format port "#!/usr/bin/perl

use Test::More tests => 4;

BEGIN { use_ok('Sys::CPU'); }

$number = &Sys::CPU::cpu_count();
ok( defined($number), \"CPU Count: $number\" );

TODO: {
    local $TODO = \"/proc/cpuinfo doesn't always report 'cpu MHz' or 'clock' or 'bogomips' ...\";
    $speed = &Sys::CPU::cpu_clock();
    ok( defined($speed), \"CPU Speed: $speed\" );
}

TODO: {
    local $TODO = \"/proc/cpuinfo doesn't always report 'model name' or 'machine' ...\";
    $type = &Sys::CPU::cpu_type();
    ok( defined($type), \"CPU Type:  $type\" );
}~%")))
                  #t))))
    (build-system perl-build-system)
    (synopsis "Perl extension for getting CPU information")
    (description
     "Sys::CPU is a module for counting the number of CPUs on a system, and
determining their type and clock speed.")
    (home-page "https://metacpan.org/release/MZSANFORD/Sys-CPU-0.61")
    (license (package-license perl))))

(define-public perl-sys-hostname-long
  (package
    (name "perl-sys-hostname-long")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SC/SCOTT/"
                           "Sys-Hostname-Long-" version ".tar.gz"))
       (sha256
        (base32
         "1jv5n8jv48c1p8svjsigyxndv1ygsq8wgwj9c7ypx1vaf3rns679"))))
    (build-system perl-build-system)
    (arguments `(#:tests? #f))          ;no `hostname' during build
    (home-page "https://metacpan.org/release/Sys-Hostname-Long")
    (synopsis "Get full hostname in Perl")
    (description "Sys::Hostname::Long tries very hard to get the full hostname
of a system.")
    (license (package-license perl))))

(define-public perl-sys-syscall
  (package
    (name "perl-sys-syscall")
    (version "0.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BR/BRADFITZ/"
                           "Sys-Syscall-" version ".tar.gz"))
       (sha256
        (base32
         "1r8k4q04dhs191zgdfgiagvbra770hx0bm6x24jsykxn0c6ghi8y"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Sys-Syscall")
    (synopsis
     "Access system calls that Perl doesn't normally provide access to")
    (description
     "Sys::Syscall allows one to use epoll and sendfile system calls from
Perl.  Support is mostly Linux-only for now, but other syscalls/OSes are
planned for the future.")
    (license license:perl-license)))

(define-public perl-task-weaken
  (package
    (name "perl-task-weaken")
    (version "1.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Task-Weaken-" version ".tar.gz"))
       (sha256
        (base32
         "1gk6rmnp4x50lzr0vfng41khf0f8yzxlm0pad1j69vxskpdzx0r3"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-search-path
                    (lambda _
                      ;; Work around "dotless @INC" build failure.
                      (setenv "PERL5LIB"
                              (string-append (getcwd) ":"
                                             (getenv "PERL5LIB")))
                      #t)))))
    (home-page "https://metacpan.org/release/Task-Weaken")
    (synopsis "Ensure that a platform has weaken support")
    (description "One recurring problem in modules that use Scalar::Util's
weaken function is that it is not present in the pure-perl variant.  If
Scalar::Util is not available at all, it will issue a normal dependency on the
module.  However, if Scalar::Util is relatively new ( it is >= 1.19 ) and the
module does not have weaken, the install will bail out altogether with a long
error encouraging the user to seek support.")
    (license (package-license perl))))

(define-public perl-template-toolkit
  (package
    (name "perl-template-toolkit")
    (version "2.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AT/ATOOMIC/"
                           "Template-Toolkit-" version ".tar.gz"))
       (sha256
        (base32
         "1msxg3j1hx5wsc7vr81x5gs9gdbn4y0x6cvyj3pq4dgi1603dbvi"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-appconfig" ,perl-appconfig)
       ("perl-test-leaktrace" ,perl-test-leaktrace)))
    (home-page "https://metacpan.org/release/Template-Toolkit")
    (synopsis "Template processing system for Perl")
    (description "The Template Toolkit is a collection of modules which
implement an extensible template processing system.  It was originally
designed and remains primarily useful for generating dynamic web content, but
it can be used equally well for processing any other kind of text based
documents: HTML, XML, POD, PostScript, LaTeX, and so on.")
    (license (package-license perl))))

(define-public perl-template-timer
  (package
    (name "perl-template-timer")
    (version "1.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PETDANCE/"
                           "Template-Timer-" version ".tar.gz"))
       (sha256
        (base32
         "1d3pbcx1kz73ncg8s8lx3ifwphz838qy0m40gdar7790cnrlqcdp"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-template-toolkit" ,perl-template-toolkit)))
    (home-page "https://metacpan.org/release/Template-Timer")
    (synopsis "Profiling for Template Toolkit")
    (description "Template::Timer provides inline profiling of the template
processing in Perl code.")
    (license (list license:gpl3 license:artistic2.0))))

(define-public perl-template-tiny
  (package
    (name "perl-template-tiny")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AD/ADAMK/Template-Tiny-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0jhadxbc8rzbk2v8qvjrbhnvfp0m56iqar6d4nvxyl8bccn0cgh7"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Template-Tiny")
    (synopsis "Template Toolkit reimplemented in as little code as possible")
    (description
     "@code{Template::Tiny} is a reimplementation of a subset of the
functionality from Template Toolkit in as few lines of code as possible.

It is intended for use in light-usage, low-memory, or low-cpu templating
situations, where you may need to upgrade to the full feature set in the
future, or if you want the retain the familiarity of TT-style templates.")
    (license license:perl-license)))

(define-public perl-term-encoding
  (package
    (name "perl-term-encoding")
    (version "0.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Term-Encoding-" version ".tar.gz"))
       (sha256
        (base32
         "1k6g4q7snxggv5fdqnzw29al4mwbwg0hl0skzfnczh508qiyfx7j"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (home-page "https://metacpan.org/release/Term-Encoding")
    (synopsis "Detect encoding of the current terminal")
    (description "Term::Encoding is a simple module to detect the encoding of
the current terminal expects in various ways.")
    (license (package-license perl))))

(define-public perl-term-progressbar
  (package
    (name "perl-term-progressbar")
    (version "2.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SZ/SZABGAB/"
                           "Term-ProgressBar-" version ".tar.gz"))
       (sha256
        (base32
         "15pn42zf793dplpfnmawh7v7xc4qm38s1jhvn1agx4cafcn61q61"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-class-methodmaker" ,perl-class-methodmaker)
       ("perl-term-readkey" ,perl-term-readkey)))
    (home-page "https://metacpan.org/release/Term-ProgressBar")
    (synopsis "Progress meter on a standard terminal")
    (description "Term::ProgressBar provides a simple progress bar on the
terminal, to let the user know that something is happening, roughly how much
stuff has been done, and maybe an estimate at how long remains.")
    (license (package-license perl))))

(define-public perl-term-progressbar-quiet
  (package
    (name "perl-term-progressbar-quiet")
    (version "0.31")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LB/LBROCARD/"
                           "Term-ProgressBar-Quiet-" version ".tar.gz"))
       (sha256
        (base32
         "19l4476iinwz19vh360k3rss38m9gmkg633i5v9jkg48yn954rr5"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-io-interactive" ,perl-io-interactive)
       ("perl-term-progressbar" ,perl-term-progressbar)
       ("perl-test-mockobject" ,perl-test-mockobject)))
    (home-page "https://metacpan.org/release/Term-ProgressBar-Quiet")
    (synopsis "Progress meter if run interactively")
    (description "Term::ProgressBar is a wonderful module for showing progress
bars on the terminal.  This module acts very much like that module when it is
run interactively.  However, when it is not run interactively (for example, as
a cron job) then it does not show the progress bar.")
    (license (package-license perl))))

(define-public perl-term-progressbar-simple
  (package
    (name "perl-term-progressbar-simple")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/EV/EVDB/"
                           "Term-ProgressBar-Simple-" version ".tar.gz"))
       (sha256
        (base32
         "19kr6l2aflwv9yph5xishkpag038qb8wd4mkzb0x1psvgp3b63d2"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-term-progressbar-quiet" ,perl-term-progressbar-quiet)))
    (home-page "https://metacpan.org/release/Term-ProgressBar-Simple")
    (synopsis "Simple progress bars")
    (description "Term::ProgressBar::Simple tells you how much work has been
done, how much is left to do, and estimate how long it will take.")
    (license (package-license perl))))

(define-public perl-term-readkey
  (package
    (name "perl-term-readkey")
    (version "2.38")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JS/JSTOWE/"
                           "TermReadKey-" version ".tar.gz"))
       (sha256
        (base32
         "143jlibah1g14bym7sj3gphvqkpj1w4vn7sqc4vc62jpviw5hr2s"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/TermReadKey")
    (synopsis "Simple terminal control")
    (description "This module, ReadKey, provides ioctl control for terminals
so the input modes can be changed (thus allowing reads of a single character
at a time), and also provides non-blocking reads of stdin, as well as several
other terminal related features, including retrieval/modification of the
screen size, and retrieval/modification of the control characters.")
    (license (package-license perl))))

(define-public perl-term-readline-gnu
  (package
    (name "perl-term-readline-gnu")
    (version "1.36")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAYASHI/"
                           "Term-ReadLine-Gnu-" version ".tar.gz"))
       (sha256
        (base32
         "09b9mcmp09kdfh5jaqdr528yny8746hvn3f185aqd6rw06jgf24s"))))
    (build-system perl-build-system)
    (inputs
     `(("readline" ,readline)
       ("ncurses" ,ncurses)))
    (arguments
     `(#:tests? #f ; Tests fail without other Term::ReadLine interfaces present
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'patch-search-lib
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "Makefile.PL"
                        ;; The configuration provides no way easy was to pass
                        ;; additional directories to search for libraries, so
                        ;; just patch in the flags.
                        (("-lreadline" &)
                         (format #f "-L~a/lib ~a" (assoc-ref inputs "readline") &))
                        (("&search_lib\\('-lncurses'\\)")
                         (string-append "'-L" (assoc-ref inputs "ncurses") "/lib"
                                        " -lncurses'"))))))))
    (home-page "https://metacpan.org/release/Term-ReadLine-Gnu")
    (synopsis "GNU Readline/History Library interface for Perl")
    (description "This module implements an interface to the GNU Readline
library.  It gives you input line editing facilities, input history management
facilities, completion facilities, etc.  Term::ReadLine::Gnu is upwards
compatible with Term::ReadLine.")
    (license (package-license perl))))

(define-public perl-term-size-any
  (package
    (name "perl-term-size-any")
    (version "0.002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FE/FERREIRA/"
                           "Term-Size-Any-" version ".tar.gz"))
       (sha256
        (base32
         "1lnynd8pwjp3g85bl4nav6yigg2lag3sx5da989j7a733bdmzyk4"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-devel-hide" ,perl-devel-hide)))
    (propagated-inputs
     `(("perl-term-size-perl" ,perl-term-size-perl)))
    (home-page "https://metacpan.org/release/Term-Size-Any")
    (synopsis "Retrieve terminal size")
    (description "This is a unified interface to retrieve terminal size.  It
loads one module of a list of known alternatives, each implementing some way
to get the desired terminal information.  This loaded module will actually do
the job on behalf of @code{Term::Size::Any}.")
    (license (package-license perl))))

(define-public perl-term-size-perl
  (package
    (name "perl-term-size-perl")
    (version "0.031")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FE/FERREIRA/"
                           "Term-Size-Perl-" version ".tar.gz"))
       (sha256
        (base32 "17i05y186l977bhp32b24c8rqasmg1la934dizf5sc0vrd36g6mf"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Term-Size-Perl")
    (synopsis "Perl extension for retrieving terminal size (Perl version)")
    (description "This is yet another implementation of @code{Term::Size}.
Now in pure Perl, with the exception of a C probe run at build time.")
    (license (package-license perl))))

(define-public perl-term-table
  (package
    (name "perl-term-table")
    (version "0.008")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/E/EX/EXODIST/Term-Table-"
                            version ".tar.gz"))
        (sha256
         (base32
          "0gi4lyvs6n8y6hjwmflfpamfl65y7mb1g39zi0rx35nclj8xb370"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-importer" ,perl-importer)))
    (home-page "https://metacpan.org/release/Term-Table")
    (synopsis "Format a header and rows into a table")
    (description "This module is able to generically format rows of data
into tables.")
    (license (package-license perl))))

(define-public perl-text-aligner
  (package
    (name "perl-text-aligner")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                           "Text-Aligner-" version ".tar.gz"))
       (sha256
        (base32 "1vry21jrh91l2pkajnrps83bnr1fn6zshbzi80mcrnggrn9iq776"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/Text-Aligner")
    (synopsis "Align text")
    (description "Text::Aligner exports a single function, align(), which is
used to justify strings to various alignment styles.")
    (license license:x11)))

(define-public perl-text-balanced
  (package
    (name "perl-text-balanced")
    (version "2.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHAY/"
                           "Text-Balanced-" version ".tar.gz"))
       (sha256
        (base32
         "1j4jjw6bg6ik8cn1mimw54rvg4h0qf4hm9k63y9572sny3w56xq5"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Text-Balanced")
    (synopsis "Extract delimited text sequences from strings")
    (description "The Text::Balanced module can be used to extract delimited
text sequences from strings.")
    (license (package-license perl))))

(define-public perl-text-csv
  (package
    (name "perl-text-csv")
    (version "2.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IS/ISHIGAKI/"
                           "Text-CSV-" version ".tar.gz"))
       (sha256
        (base32 "1hmjrc8h622nybdq8lpqi3hlrcjvb474s4a4b2cjs8h5b0cxkjwc"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Text-CSV")
    (synopsis "Manipulate comma-separated values")
    (description "Text::CSV provides facilities for the composition and
decomposition of comma-separated values.  An instance of the Text::CSV class
can combine fields into a CSV string and parse a CSV string into fields.")
    (license (package-license perl))))

(define-public perl-text-csv-xs
  (package
    (name "perl-text-csv-xs")
    (version "1.44")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HM/HMBRAND/"
                           "Text-CSV_XS-" version ".tgz"))
       (sha256
        (base32 "1i4viyf61lzss474ndnmqhdqlhksn9hcxhjbqhv4frg2m3f2v0f4"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Text-CSV_XS")
    (synopsis "Routines for manipulating CSV files")
    (description "@code{Text::CSV_XS} provides facilities for the composition
and decomposition of comma-separated values.  An instance of the
@code{Text::CSV_XS} class will combine fields into a CSV string and parse a
CSV string into fields.  The module accepts either strings or files as input
and support the use of user-specified characters for delimiters, separators,
and escapes.")
    (license (package-license perl))))

(define-public perl-text-diff
  (package
    (name "perl-text-diff")
    (version "1.45")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Text-Diff-" version ".tar.gz"))
       (sha256
        (base32
         "013g13prdghxvrp5754gyc7rmv1syyxrhs33yc5f0lrz3dxs1fp8"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-algorithm-diff" ,perl-algorithm-diff)))
    (home-page "https://metacpan.org/release/Text-Diff")
    (synopsis "Perform diffs on files and record sets")
    (description "Text::Diff provides a basic set of services akin to the GNU
diff utility.  It is not anywhere near as feature complete as GNU diff, but it
is better integrated with Perl and available on all platforms.  It is often
faster than shelling out to a system's diff executable for small files, and
generally slower on larger files.")
    (license (package-license perl))))

(define-public perl-text-format
  (package
    (name "perl-text-format")
    (version "0.62")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/S/SH/SHLOMIF/Text-Format-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0104z7jjv46kqh77rnx8kvmsbr5dy0s56xm01dckq4ly65br0hkx"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (home-page "https://metacpan.org/release/Text-Format")
    (synopsis "Various subroutines to format text")
    (description "This package provides functions to format text in various
ways like centering, paragraphing, and converting tabs to spaces and spaces
to tabs.")
    (license license:perl-license)))

(define-public perl-text-glob
  (package
    (name "perl-text-glob")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Text-Glob-" version ".tar.gz"))
       (sha256
        (base32
         "11sj62fynfgwrlgkv5a051cq6yn0pagxqjsz27dxx8phsd4wv706"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/Text-Glob")
    (synopsis "Match globbing patterns against text")
    (description "Text::Glob implements glob(3) style matching that can be
used to match against text, rather than fetching names from a file system.  If
you want to do full file globbing use the File::Glob module instead.")
    (license (package-license perl))))

(define-public perl-text-haml
  (package
    (name "perl-text-haml")
    (version "0.990118")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/V/VT/VTI/"
                           "Text-Haml-" version ".tar.gz"))
       (sha256
        (base32 "1siq8hgj7s8gwpf3n3h1is5v50rwi6av8lfb19khiyyqz0rp7a57"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)))
    (propagated-inputs
     `(("perl-data-section-simple" ,perl-data-section-simple)
       ("perl-uri" ,perl-uri)))
    (home-page "https://metacpan.org/release/Text-Haml")
    (synopsis "Haml Perl implementation")
    (description
     "Text::Haml implements Haml
@url{http://haml.info/docs/yardoc/file.REFERENCE.html} specification.")
    (license license:artistic2.0)))

(define-public perl-text-neattemplate
  (package
    (name "perl-text-neattemplate")
    (version "0.1101")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cpan.metacpan.org/authors/id/R/RU/RUBYKAT/"
             "Text-NeatTemplate-" version ".tar.gz"))
       (sha256
        (base32
         "129msa57jzxxi2x7z9hgzi48r48y65w77ycfk1w733zz2m8nr8y3"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page
     "https://metacpan.org/release/Text-NeatTemplate")
    (synopsis "Fast, middleweight template engine")
    (description
     "Text::NeatTemplate provides a simple, middleweight but fast
template engine, for when you need speed rather than complex features,
yet need more features than simple variable substitution.")
    (license (package-license perl))))

(define-public perl-text-patch
  (package
    (name "perl-text-patch")
    (version "1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CA/CADE/"
                           "Text-Patch-" version ".tar.gz"))
       (sha256
        (base32 "1k1xbhxwn9fymqqwnam9pm7hr2p5ikq6dk578qw18gkap9hqxwga"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-text-diff" ,perl-text-diff)))
    (home-page "https://metacpan.org/release/Text-Patch")
    (synopsis "Patches text with given patch")
    (description "Text::Patch combines source text with given
diff (difference) data.  Diff data is produced by Text::Diff module or
by the standard @code{diff} utility.")
    (license license:gpl2+)))

(define-public perl-text-roman
  (package
    (name "perl-text-roman")
    (version "3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SY/SYP/Text-Roman-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0sh47svzz0wm993ywfgpn0fvhajl2sj5hcnf5zxjz02in6ihhjnb"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Text-Roman")
    (synopsis "Convert between Roman and Arabic algorisms")
    (description "This package provides functions to convert between Roman and
Arabic algorisms.  It supports both conventional Roman algorisms (which range
from 1 to 3999) and Milhar Romans, a variation which uses a bar across the
algorism to indicate multiplication by 1000.")
    (license (package-license perl))))

(define-public perl-text-simpletable
  (package
    (name "perl-text-simpletable")
    (version "2.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MR/MRAMBERG/"
                           "Text-SimpleTable-" version ".tar.gz"))
       (sha256
        (base32 "1v8r8qpzg283p2pqqr8dqrak2bxray1b2jmib0qk75jffqw3yv95"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Text-SimpleTable")
    (synopsis "Simple ASCII tables")
    (description "Text::SimpleTable draws simple ASCII tables.")
    (license license:artistic2.0)))

(define-public perl-text-table
  (package
    (name "perl-text-table")
    (version "1.133")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                           "Text-Table-" version ".tar.gz"))
       (sha256
        (base32
         "04kh5x5inq183rdg221wlqaaqi1ipyj588mxsslik6nhc14f17nd"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-text-aligner" ,perl-text-aligner)))
    (home-page "https://metacpan.org/release/Text-Table")
    (synopsis "Organize Data in Tables")
    (description "Text::Table renders plaintext tables.")
    (license license:x11)))

(define-public perl-text-template
  (package
    (name "perl-text-template")
    (version "1.55")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MS/MSCHOUT/Text-Template-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "12zi08mwmlbfbnsialmppk75s6dkg765dvmay3wif3158plqp554"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-more-utf8" ,perl-test-more-utf8)
       ("perl-test-warnings" ,perl-test-warnings)))
    (home-page
     "https://metacpan.org/release/Text-Template")
    (synopsis
     "Expand template text with embedded Perl")
    (description
     "This is a library for generating letters, building HTML pages, or
filling in templates generally.  A template is a piece of text that has little
Perl programs embedded in it here and there.  When you fill in a template, you
evaluate the little programs and replace them with their values.")
    (license license:perl-license)))

(define-public perl-text-unidecode
  (package
    (name "perl-text-unidecode")
    (version "1.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SB/SBURKE/"
                           "Text-Unidecode-" version ".tar.gz"))
       (sha256
        (base32 "1imii0p6wvhrxsr5z2zhazpx5vl4l4ybf1y2c5hy480xvi6z293c"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Text-Unidecode")
    (synopsis "Provide plain ASCII transliterations of Unicode text")
    (description "Text::Unidecode provides a function, unidecode(...) that
takes Unicode data and tries to represent it in US-ASCII characters (i.e., the
universally displayable characters between 0x00 and 0x7F).  The representation
is almost always an attempt at transliteration-- i.e., conveying, in Roman
letters, the pronunciation expressed by the text in some other writing
system.")
    (license (package-license perl))))

(define-public perl-threads
  (package
    (name "perl-threads")
    (version "2.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JD/JDHEDDEN/threads-"
                           version ".tar.gz"))
       (sha256
        (base32 "047i22mdnf7fa0h9w5jhqrjbg561l5jxk8xqzwh6zbmwlac4qf98"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/threads")
    (synopsis "Perl interpreter-based threads")
    (description "This module exposes interpreter threads to the Perl level.")
    (license license:perl-license)))

(define-public perl-throwable
  (package
    (name "perl-throwable")
    (version "0.200013")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Throwable-" version ".tar.gz"))
       (sha256
        (base32
         "184gdcwxqwnkrx5md968v1ny70pq6blzpkihccm3bpdxnpgd11wr"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-devel-stacktrace" ,perl-devel-stacktrace)))
    (propagated-inputs
     `(("perl-devel-stacktrace" ,perl-devel-stacktrace)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-moo" ,perl-moo)))
    (home-page "https://metacpan.org/release/Throwable")
    (synopsis "Role for classes that can be thrown")
    (description "Throwable is a role for classes that are meant to be thrown
as exceptions to standard program flow.")
    (license (package-license perl))))

(define-public perltidy
  (package
    (name "perltidy")
    (version "20180220")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/perltidy/" version
                                  "/Perl-Tidy-" version ".tar.gz"))
              (sha256
               (base32
                "0w1k5ffcrpx0fm9jgprrwy0290k6cmy7dyk83s61063migi3r5z9"))))
    (build-system perl-build-system)
    (home-page "http://perltidy.sourceforge.net/")
    (synopsis "Perl script tidier")
    (description "This package contains a Perl script which indents and
reformats Perl scripts to make them easier to read.   The formatting can be
controlled with command line parameters.  The default parameter settings
approximately follow the suggestions in the Perl Style Guide.")
    (license license:gpl2+)))

(define-public perl-tie-cycle
  (package
    (name "perl-tie-cycle")
    (version "1.225")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/Tie-Cycle-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0i9xq2qm50p2ih24265jndp2x8hfq7ap0d88nrlv5yaad4hxhc7k"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Tie-Cycle")
    (synopsis "Cycle through a list of values")
    (description "You use @code{Tie::Cycle} to go through a list over and over
again.  Once you get to the end of the list, you go back to the beginning.")
    (license (package-license perl))))

(define-public perl-tie-ixhash
  (package
  (name "perl-tie-ixhash")
  (version "1.23")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://cpan/authors/id/C/CH/CHORNY/"
                          "Tie-IxHash-" version ".tar.gz"))
      (sha256
        (base32
          "0mmg9iyh42syal3z1p2pn9airq65yrkfs66cnqs9nz76jy60pfzs"))))
  (build-system perl-build-system)
  (native-inputs `(("perl-module-build" ,perl-module-build)))
  (home-page "https://metacpan.org/release/Tie-IxHash")
  (synopsis "Ordered associative arrays for Perl")
  (description "This Perl module implements Perl hashes that preserve the
order in which the hash elements were added.  The order is not affected when
values corresponding to existing keys in the IxHash are changed.  The elements
can also be set to any arbitrary supplied order.  The familiar perl array
operations can also be performed on the IxHash.")
  (license (package-license perl))))

(define-public perl-tie-handle-offset
  (package
    (name "perl-tie-handle-offset")
    (version "0.004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DA/DAGOLDEN/Tie-Handle-Offset-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "17m8s8314wi4g0wasdxk15rf12vzsgzmcbr598jam5f6bl2kk7zf"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Tie-Handle-Offset")
    (synopsis "Special file handle that hides the beginning of a file")
    (description
     "This modules provides a file handle that hides the beginning of a file,
by modifying the @code{seek()} and @code{tell()} calls.")
    (license license:asl2.0)))

(define-public perl-tie-simple
  (package
    (name "perl-tie-simple")
    (version "1.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HANENKAMP/"
                           "Tie-Simple-" version ".tar.gz"))
       (sha256
        (base32 "04lbh07nlxhpp03gl20f9w8hxjb2vzlb7w85y9w6q12i749y5s99"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Tie-Simple")
    (synopsis "Variable ties made much easier")
    (description
     "This module adds the ability to quickly create new types of tie objects
without creating a complete class.  It does so in such a way as to try and
make the programmers life easier when it comes to single-use ties that I find
myself wanting to use from time-to-time.

The Tie::Simple package is actually a front-end to other classes which really
do all the work once tied, but this package does the dwimming to automatically
figure out what you're trying to do.")
    (license license:perl-license)))

(define-public perl-tie-toobject
  (package
    (name "perl-tie-toobject")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NU/NUFFIN/"
                           "Tie-ToObject-" version ".tar.gz"))
       (sha256
        (base32
         "1x1smn1kw383xc5h9wajxk9dlx92bgrbf7gk4abga57y6120s6m3"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-test-simple" ,perl-test-simple)))
    (home-page "https://metacpan.org/release/Tie-ToObject")
    (synopsis "Tie to an existing Perl object")
    (description "This class provides a tie constructor that returns the
object it was given as it's first argument.  This way side effects of calling
$object->TIEHASH are avoided.")
    (license (package-license perl))))

(define-public perl-time-duration
  (package
    (name "perl-time-duration")
    (version "1.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Time-Duration-" version ".tar.gz"))
       (sha256
        (base32 "1f59z2svfydxgd1gzrb5k3hl6d432kzmskk7jhv2dyb5hyx0wd7y"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (home-page "https://metacpan.org/release/Time-Duration")
    (synopsis "English expression of durations")
    (description "This module provides functions for expressing durations in
rounded or exact terms.")
    (license (package-license perl))))

(define-public perl-time-duration-parse
  (package
    (name "perl-time-duration-parse")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Time-Duration-Parse-" version ".tar.gz"))
       (sha256
        (base32 "10g39bbrxkabbsfq4rv7f5b5x7h3jba08j4pg8gwr0b9iqx19n31"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-time-duration" ,perl-time-duration)))
    (propagated-inputs
     `(("perl-exporter-lite" ,perl-exporter-lite)))
    (home-page "https://metacpan.org/release/Time-Duration-Parse")
    (synopsis "Parse time duration strings")
    (description "Time::Duration::Parse is a module to parse human readable
duration strings like \"2 minutes\" and \"3 seconds\" to seconds.")
    (license (package-license perl))))

(define-public perl-time-hires
  (package
    (name "perl-time-hires")
    (version "1.9764")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/A/AT/ATOOMIC/Time-HiRes-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1cfp078spid9z5g7xiswkpkjbkh4mkjvarz25wgwvdxzhxavwhcq"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Time-HiRes")
    (synopsis "High-resolution alarm, sleep, gettimeofday, and interval timers")
    (description "This package implements @code{usleep}, @code{ualarm}, and
@code{gettimeofday} for Perl, as well as wrappers to implement @code{time},
@code{sleep}, and @code{alarm} that know about non-integral seconds.")
    (license license:perl-license)))

(define-public perl-time-local
  (package
    (name "perl-time-local")
    (version "1.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Time-Local-" version ".tar.gz"))
       (sha256
        (base32
         "1jr0i57jqm0spdd98gp5mzdnrqdyf7rls0ygwb9ldfc655mlyx67"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Time-Local")
    (synopsis "Efficiently compute time from local and GMT time")
    (description "This module provides functions that are the inverse of
built-in perl functions localtime() and gmtime().  They accept a date as a
six-element array, and return the corresponding time(2) value in seconds since
the system epoch.")
    (license (package-license perl))))

(define-public perl-time-piece
  (package
    (name "perl-time-piece")
    (version "1.3203")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/E/ES/ESAYM/Time-Piece-"
             version ".tar.gz"))
       (sha256
        (base32 "0hbg99v8xqy3nx6nrjpwh1w6xwqpfflz0djkbdd72kvf8zvglwb9"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Time-Piece")
    (synopsis "Object-Oriented time objects")
    (description
     "This module replaces the standard @code{localtime} and @code{gmtime}
functions with implementations that return objects.  It does so in a
backwards-compatible manner, so that using these functions as documented will
still work as expected.")
    (license license:perl-license)))

(define-public perl-timedate
  (package
    (name "perl-timedate")
    (version "2.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AT/ATOOMIC/"
                           "TimeDate-" version ".tar.gz"))
       (sha256
        (base32 "1cjyc0yi873597r7xcp9yz0l1c46ik2kxwfrn00zbrlx0d5rrdn0"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/TimeDate")
    (synopsis "Date parsing/formatting subroutines")
    (description "This module provides routines for parsing date string into
time values and formatting dates into ASCII strings.")
    (license (package-license perl))))

(define-public perl-time-mock
  (package
    (name "perl-time-mock")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/EW/EWILHELM/"
                           "Time-Mock-v" version ".tar.gz"))
       (sha256
        (base32
         "0bwqyg8z98m8cjw1qcm4wg502n225k33j2fp8ywxkgfjdd1zgllv"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-timedate" ,perl-timedate))) ;For Date::Parse
    (home-page "https://metacpan.org/release/Time-Mock")
    (synopsis "Shift and scale time")
    (description "This module allows you to speed up your sleep(), alarm(),
and time() calls.")
    (license (package-license perl))))

(define-public perl-tree-simple
  (package
    (name "perl-tree-simple")
    (version "1.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RS/RSAVAGE/"
                           "Tree-Simple-" version ".tgz"))
       (sha256
        (base32 "1alnwb6c7n4al91m9cyknvcyvdz521lh22dz1hyk4v7c50adffnv"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-scalar-list-utils" ,perl-scalar-list-utils)))
    (home-page "https://metacpan.org/release/Tree-Simple")
    (synopsis "Simple tree object")
    (description "This module in a fully object-oriented implementation of a
simple n-ary tree.")
    (license (package-license perl))))

(define-public perl-tree-simple-visitorfactory
  (package
    (name "perl-tree-simple-visitorfactory")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RS/RSAVAGE/"
                           "Tree-Simple-VisitorFactory-" version ".tgz"))
       (sha256
        (base32 "06y2vazkl307k59hnkp9h5bp3p7711kgmp1qdhb2lgnfwzn84zin"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-tree-simple" ,perl-tree-simple)))
    (home-page "https://metacpan.org/release/Tree-Simple-VisitorFactory")
    (synopsis "Factory object for dispensing Visitor objects")
    (description "This module is a factory for dispensing
Tree::Simple::Visitor::* objects.")
    (license (package-license perl))))

(define-public perl-try-tiny
  (package
    (name "perl-try-tiny")
    (version "0.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Try-Tiny-" version ".tar.gz"))
       (sha256
        (base32
         "0szgvlz19yz3mq1lbzmwh8w5dh6agg5s16xv22zrnl83r7ax0nys"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Try-Tiny")
    (synopsis "Minimal try/catch with proper preservation of $@@")
    (description "This module provides bare bones try/catch/finally statements
that are designed to minimize common mistakes with eval blocks, and nothing
else.")
    (license license:x11)))

(define-public perl-type-tie
  (package
    (name "perl-type-tie")
    (version "0.014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOBYINK/"
                           "Type-Tie-" version ".tar.gz"))
       (sha256
        (base32 "1ri23xb3rdb59lk984hnjqi4pb97zqnv4ppn0zpd70pfp0a9addm"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-exporter-tiny" ,perl-exporter-tiny)
       ("perl-hash-fieldhash" ,perl-hash-fieldhash)))
    (home-page "https://metacpan.org/release/Type-Tie")
    (synopsis "Tie a variable to a type constraint")
    (description "This module exports a single function: @code{ttie}.  It ties
a variable to a type constraint, ensuring that whatever values stored in the
variable will conform to the type constraint.  If the type constraint has
coercions, these will be used if necessary to ensure values assigned to the
variable conform.")
    (license (package-license perl))))

(define-public perl-type-tiny
  (package
    (name "perl-type-tiny")
    (version "1.008003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOBYINK/"
                           "Type-Tiny-" version ".tar.gz"))
       (sha256
        (base32 "1x80rlnh7kl4xgm4qvyfbgahcyla4wbyh3b759nm21czn8x6wkm4"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-warnings" ,perl-test-warnings)))
    (propagated-inputs
     `(("perl-devel-lexalias" ,perl-devel-lexalias)
       ("perl-devel-stacktrace" ,perl-devel-stacktrace)
       ("perl-exporter-tiny" ,perl-exporter-tiny)
       ("perl-moo" ,perl-moo)
       ("perl-moose" ,perl-moose)
       ("perl-mouse" ,perl-mouse)
       ("perl-ref-util-xs" ,perl-ref-util-xs)
       ("perl-regexp-util" ,perl-regexp-util)
       ("perl-type-tie" ,perl-type-tie)))
    (home-page "https://metacpan.org/release/Type-Tiny")
    (synopsis "Tiny, yet Moo(se)-compatible type constraint")
    (description "@code{Type::Tiny} is a small class for writing type
constraints, inspired by Moose's type constraint API.  It has only one
non-core dependency (and even that is simply a module that was previously
distributed as part of @code{Type::Tiny} but has since been spun off), and can
be used with Moose, Mouse and Moo (or none of the above).")
    (license (package-license perl))))

(define-public perl-type-tiny-xs
  (package
    (name "perl-type-tiny-xs")
    (version "0.014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOBYINK/Type-Tiny-XS-"
                           version ".tar.gz"))
       (sha256
        (base32 "1bbvghd2wmm9z1jx9qs9yz4l3r4izs8sz87z87sis7n3ydjdx2w2"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Type-Tiny-XS")
    (synopsis "Provides an XS boost for some of Type::Tiny's built-in type constraints")
    (description "This module is optionally used by @code{Type::Tiny} to
provide faster, C-based implementations of some type constraints.  This
package has only core dependencies, and does not depend on @code{Type::Tiny},
so other data validation frameworks might also consider using it.")
    (license license:perl-license)))

(define-public perl-types-path-tiny
  (package
    (name "perl-types-path-tiny")
    (version "0.006")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "Types-Path-Tiny-" version ".tar.gz"))
       (sha256
        (base32 "1072vwcbx2bldfg8xpxc9iqs3rzqd18yik60b432hsdwxpxcjgsr"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-pushd" ,perl-file-pushd)
       ("perl-path-tiny" ,perl-path-tiny)
       ("perl-type-tiny" ,perl-type-tiny)
       ("perl-exporter-tiny" ,perl-exporter-tiny)))
    (home-page "https://metacpan.org/release/Types-Path-Tiny")
    (synopsis "Types and coercions for Moose and Moo")
    (description "This module provides @code{Path::Tiny} types for Moose, Moo,
etc.  It handles two important types of coercion: coercing objects with
overloaded stringification, and coercing to absolute paths.  It also can check
to ensure that files or directories exist.")
    (license license:artistic2.0)))

(define-public perl-types-serialiser
  (package
    (name "perl-types-serialiser")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/"
                           "Types-Serialiser-" version ".tar.gz"))
       (sha256
        (base32
         "03bk0hm5ys8k7265dkap825ybn2zmzb1hl0kf1jdm8yq95w39lvs"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-common-sense" ,perl-common-sense)))
    (home-page "https://metacpan.org/release/Types-Serialiser")
    (synopsis "Data types for common serialisation formats")
    (description "This module provides some extra datatypes that are used by
common serialisation formats such as JSON or CBOR.")
    (license (package-license perl))))

(define-public perl-unicode-normalize
  (package
    (name "perl-unicode-normalize")
    (version "1.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KH/KHW/"
                           "Unicode-Normalize-" version ".tar.gz"))
       (sha256
        (base32
         "0gvpmrfrvb3sxqq4pnqfmbpf9q0q2an6a2ba4ara95cvx1s6zpms"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-search-path
                    (lambda _
                      ;; Work around "dotless @INC" build failure.
                      (setenv "PERL5LIB"
                              (string-append (getcwd) ":"
                                             (getenv "PERL5LIB")))
                      #t)))))
    (home-page "https://metacpan.org/release/Unicode-Normalize")
    (synopsis "Unicode normalization forms")
    (description "This Perl module provides Unicode normalization forms.")
    (license (package-license perl))))

(define-public perl-unicode-collate
  (package
    (name "perl-unicode-collate")
    (version "1.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SA/SADAHIRO/"
                           "Unicode-Collate-" version ".tar.gz"))
       (sha256
        (base32 "12df4n46yri6via4x9jb918v1hk6yrlzqk9srq6fnz5kviylnxbf"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-perl-search-path
           (lambda _
             ;; Work around "dotless @INC" build failure.
             (setenv "PERL5LIB"
                     (string-append (getcwd) ":"
                                    (getenv "PERL5LIB")))
             #t)))))
    (propagated-inputs
     `(("perl-unicode-normalize" ,perl-unicode-normalize)))
    (home-page "https://metacpan.org/release/Unicode-Collate")
    (synopsis "Unicode collation algorithm")
    (description "This package provides tools for sorting and comparing
Unicode data.")
    ;; The file Unicode/Collate/allkeys.txt is released under the Expat
    ;; license.
    (license (list (package-license perl) license:expat))))

(define-public perl-unicode-linebreak
  (package
    (name "perl-unicode-linebreak")
    (version "2019.001")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/N/NE/NEZUMI/"
                                  "Unicode-LineBreak-" version ".tar.gz"))
              (sha256
               (base32
                "12iinva5gqc9g7qzxrvmh45n714z0ad9g7wq2dxwgp6drbj64rs8"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-mime-charset" ,perl-mime-charset)))
    (home-page "https://metacpan.org/release/Unicode-LineBreak")
    (synopsis "Unicode line breaking algorithm")
    (description
     "@code{Unicode::LineBreak} implements the line breaking algorithm
described in Unicode Standard Annex #14.  The @code{East_Asian_Width} property
defined by Annex #11 is used to determine breaking positions.")
    (license (package-license perl))))

(define-public perl-unicode-utf8
  (package
    (name "perl-unicode-utf8")
    (version "0.62")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/C/CH/CHANSEN/"
                                  "Unicode-UTF8-" version ".tar.gz"))
              (sha256
               (base32
                "1xnhazbdvpyfpnxd90krzhxkvabf8fa2ji6xzlrf75j6nz8251zs"))))
    (build-system perl-build-system)
    ;; FIXME: Tests fail on 32-bit architectures:
    ;; <https://rt.cpan.org/Public/Bug/Display.html?id=127007>.
    (arguments `(#:tests? ,(target-64bit?)))
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-leaktrace" ,perl-test-leaktrace)
       ("perl-variable-magic" ,perl-variable-magic)
       ("perl-test-pod" ,perl-test-pod)))
    (home-page "https://metacpan.org/release/Unicode-UTF8")
    (synopsis "Encoding and decoding of UTF-8 encoding form")
    (description
     "This module provides functions to encode and decode UTF-8 encoding form
as specified by Unicode and ISO/IEC 10646:2011.")
    (license (package-license perl))))

(define-public perl-universal-can
  (package
    (name "perl-universal-can")
    (version "1.20140328")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CH/CHROMATIC/"
                           "UNIVERSAL-can-" version ".tar.gz"))
       (sha256
        (base32
         "03wr25zznbfn1g8zmmq3g6a6288xr30priwvm75y4vvqfkrajbaj"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/UNIVERSAL-can")
    (synopsis "UNIVERSAL::can() reimplementation")
    (description "This module attempts to work around people calling
UNIVERSAL::can() as a function, which it is not.")
    (license (package-license perl))))

(define-public perl-universal-isa
  (package
    (name "perl-universal-isa")
    (version "1.20171012")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "UNIVERSAL-isa-" version ".tar.gz"))
       (sha256
        (base32
         "0avzv9j32aab6l0rd63n92v0pgliz1p4yabxxjfq275hdh1mcsfi"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)))
    (home-page "https://metacpan.org/release/UNIVERSAL-isa")
    (synopsis "UNIVERSAL::isa() reimplementation")
    (description "This module attempts to recover from people calling
UNIVERSAL::isa as a function.")
    (license (package-license perl))))

(define-public perl-universal-require
  (package
    (name "perl-universal-require")
    (version "0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/N/NE/NEILB/UNIVERSAL-require-"
             version ".tar.gz"))
       (sha256
        (base32
         "1v9qdg80ng6dzyzs7cn8sb6mn8ym042i32lcnpd478b7g6l3d9xj"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/UNIVERSAL-require")
    (synopsis "Require modules from a variable")
    (description "This module lets you require other modules where the module
name is in a variable, something you can't do with the @code{require}
built-in.")
    (license (package-license perl))))

(define-public perl-variable-magic
  (package
    (name "perl-variable-magic")
    (version "0.62")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/V/VP/VPIT/"
                           "Variable-Magic-" version ".tar.gz"))
       (sha256
        (base32
         "0p31dclnj47k4hj35rzay9pzxasl3gq46kzwqalhdw1kgr8ii6iz"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Variable-Magic")
    (synopsis "Associate user-defined magic to variables from Perl")
    (description "Magic is Perl's way of enhancing variables.  This mechanism
lets the user add extra data to any variable and hook syntactical
operations (such as access, assignment or destruction) that can be applied to
it.  With this module, you can add your own magic to any variable without
having to write a single line of XS.")
    (license (package-license perl))))

(define-public perl-xml-writer
  (package
    (name "perl-xml-writer")
    (version "0.900")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/J/JO/JOSEPHW/XML-Writer-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "07qd806kcs7si7qakx3x5p68xq2jdmkxdrns987kaayg7syzbj3k"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/XML-Writer")
    (synopsis "Easily generate well-formed, namespace-aware XML")
    (description "@code{XML::Writer} is a simple Perl module for writing XML
documents: it takes care of constructing markup and escaping data correctly.
By default, it also performs a significant amount of well-formedness checking
on the output to make certain (for example) that start and end tags match,
that there is exactly one document element, and that there are not duplicate
attribute names.")
    ;; Redistribution and use in source and compiled forms, with or without
    ;; modification, are permitted under any circumstances.  No warranty.
    (license license:public-domain)))

(define-public perl-xs-object-magic
  (package
    (name "perl-xs-object-magic")
    (version "0.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                                  "XS-Object-Magic-" version ".tar.gz"))
              (sha256
               (base32
                "0njyy4y0zax4zz55y82dlm9cly1pld1lcxb281s12bp9rrhf9j9x"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-extutils-depends" ,perl-extutils-depends)
       ("perl-module-install" ,perl-module-install)
       ("perl-test-fatal" ,perl-test-fatal)))
    (home-page "https://metacpan.org/release/XS-Object-Magic")
    (synopsis "Opaque, extensible XS pointer backed objects using sv_magic")
    (description
     "This way of associating structs with Perl space objects is designed to
supersede Perl's builtin @code{T_PTROBJ} with something that is extensible
(structs can be associated with any data type) and opaque (the C pointer is
neither visible nor modifiable from Perl space).")
    (license (package-license perl))))

(define-public perl-yaml
  (package
    (name "perl-yaml")
    (version "1.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TI/TINITA/"
                           "YAML-" version ".tar.gz"))
       (sha256
        (base32 "1kbrfksjg4k4vmx1i337m5n69m00m0m5bgsh61c15bzzrgbacc2h"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-yaml" ,perl-test-yaml)))
    (home-page "https://metacpan.org/release/YAML")
    (synopsis "YAML for Perl")
    (description "The YAML.pm module implements a YAML Loader and Dumper based
on the YAML 1.0 specification.")
    (license (package-license perl))))

(define-public perl-yaml-libyaml
  (package
    (name "perl-yaml-libyaml")
    (version "0.82")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/T/TI/TINITA/YAML-LibYAML-"
             version ".tar.gz"))
       (sha256
        (base32 "0j7yhxkaasccynl5iq1cqpf4x253p4bi5wsq6qbwwv2wjsiwgd02"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/YAML-LibYAML")
    (synopsis "Perl YAML Serialization using XS and libyaml")
    (description
     "@code{YAML::XS} is a Perl XS binding to libyaml which offers Perl the
best YAML support to date.")
    (license license:perl-license)))

(define-public perl-yaml-tiny
  (package
    (name "perl-yaml-tiny")
    (version "1.73")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "YAML-Tiny-" version ".tar.gz"))
       (sha256
        (base32
         "0i3p4nz8ysrsrs6vlzc6gkjcfpcaf05xjc7lwbjkw7lg5shmycdw"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-json-maybexs" ,perl-json-maybexs)
       ("perl-module-build-tiny" ,perl-module-build-tiny)))
    (arguments
     `(#:tests? #f))                    ;requires Test::More >= 0.99
    (home-page "https://metacpan.org/release/YAML-Tiny")
    (synopsis "Read/Write YAML files")
    (description "YAML::Tiny is a perl class for reading and writing
YAML-style files, written with as little code as possible, reducing load time
and memory overhead.")
    (license (package-license perl))))

(define-public perl-parse-recdescent
  (package
    (name "perl-parse-recdescent")
    (version "1.967015")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/J/JT/JTBRAUN/Parse-RecDescent-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0dvfcn2xvj9r4ra5xqgasl847nsm1iy85w1kly41fkxm9im36hqr"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page
     "https://metacpan.org/release/Parse-RecDescent")
    (synopsis "Generate recursive-descent parsers")
    (description
     "@code{Parse::RecDescent} can incrementally generate top-down
recursive-descent text parsers from simple yacc-like grammar specifications.")
    (license license:perl-license)))

(define-public perl-parse-yapp
  (package
    (name "perl-parse-yapp")
    (version "1.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/W/WB/WBRASWELL/Parse-Yapp-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1r8kbyk0qd4ficmabj753kjpq0ib0csk01169w7jxflg62cfj41q"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Parse-Yapp")
    (synopsis "Generate and use LALR parsers")
    (description "This package compiles yacc-like @dfn{Look Ahead LR} (LALR)
grammars to generate Perl object oriented parser modules.")
    (license (package-license perl))))


;;; Some packaged modules need versions of core modules that are newer than
;;; those in our perl 5.16.1.

(define-public perl-cpan-meta
  (package
    (name "perl-cpan-meta")
    (version "2.150010")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "CPAN-Meta-" version ".tar.gz"))
       (sha256
        (base32
         "1mm3dfw3ffyzb2ikpqn9l6zyqrxijb4vyywmbx2l21ryqwp0zy74"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-cpan-meta-requirements" ,perl-cpan-meta-requirements)
       ("perl-cpan-meta-yaml" ,perl-cpan-meta-yaml)
       ("perl-parse-cpan-meta" ,perl-parse-cpan-meta)))
    (home-page "https://metacpan.org/release/CPAN-Meta")
    (synopsis "Distribution metadata for a CPAN dist")
    (description "Software distributions released to the CPAN include a
META.json or, for older distributions, META.yml, which describes the
distribution, its contents, and the requirements for building and installing
the distribution.  The data structure stored in the META.json file is
described in CPAN::Meta::Spec.  CPAN::Meta provides a simple class to
represent this distribution metadata (or distmeta), along with some helpful
methods for interrogating that data.")
    (license (package-license perl))))

(define-public perl-cpan-meta-requirements
  (package
    (name "perl-cpan-meta-requirements")
    (version "2.140")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "CPAN-Meta-Requirements-" version ".tar.gz"))
       (sha256
        (base32
         "1a8zflgaayycmn3zvd3n64yypa4jyl1va0h51wpr5w46irg69608"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/CPAN-Meta-Requirements")
    (synopsis "Set of version requirements for a CPAN dist")
    (description "A CPAN::Meta::Requirements object models a set of version
constraints like those specified in the META.yml or META.json files in CPAN
distributions, and as defined by CPAN::Meta::Spec.  It can be built up by
adding more and more constraints, and will reduce them to the simplest
representation.")
    (license (package-license perl))))

(define-public perl-cpan-meta-yaml
  (package
    (name "perl-cpan-meta-yaml")
    (version "0.018")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "CPAN-Meta-YAML-" version ".tar.gz"))
       (sha256
        (base32
         "150jh9l7baddl2587m23qs2l0pb395qsx9bhsgdsnn6y9k4zgjik"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f))                    ;Tests require Test::More >= 0.99
    (home-page "https://metacpan.org/release/CPAN-Meta-YAML")
    (synopsis "Read and write a subset of YAML for CPAN Meta files")
    (description "This module implements a subset of the YAML specification
for use in reading and writing CPAN metadata files like META.yml and
MYMETA.yml.")
    (license (package-license perl))))

(define-public perl-module-build
  (package
    (name "perl-module-build")
    (version "0.4229")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "Module-Build-" version ".tar.gz"))
       (sha256
        (base32
         "064c03wxia7jz0i578awj4srykj0nnigm4p5r0dv0559rnk93r0z"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-cpan-meta" ,perl-cpan-meta)))
    (home-page "https://metacpan.org/release/Module-Build")
    (synopsis "Build and install Perl modules")
    (description "@code{Module::Build} is a system for building, testing, and
installing Perl modules; it used to be part of Perl itself until version 5.22,
which dropped it.  It is meant to be an alternative to
@code{ExtUtils::MakeMaker}.  Developers may alter the behavior of the module
through subclassing in a much more straightforward way than with
@code{MakeMaker}.  It also does not require a @command{make} on your
system---most of the @code{Module::Build} code is pure-Perl.")
    (license (package-license perl))))

(define-public perl-parse-cpan-meta
  (package
    (name "perl-parse-cpan-meta")
    (version "2.150010")
    (source
     (origin
       (method url-fetch)
       ;; This module is now known as CPAN::Meta on CPAN.
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "CPAN-Meta-" version ".tar.gz"))
       (sha256
        (base32
         "1mm3dfw3ffyzb2ikpqn9l6zyqrxijb4vyywmbx2l21ryqwp0zy74"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-cpan-meta-yaml" ,perl-cpan-meta-yaml)))
    (home-page "https://metacpan.org/release/DAGOLDEN/Parse-CPAN-Meta-1.4422")
    (synopsis "Parse META.yml and META.json CPAN metadata files")
    (description "Parse::CPAN::Meta is a parser for META.json and META.yml
files, using JSON::PP and/or CPAN::Meta::YAML.")
    (license (package-license perl))))

(define-public perl-scalar-list-utils
  (package
    (name "perl-scalar-list-utils")
    (version "1.56")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PEVANS/"
                           "Scalar-List-Utils-" version ".tar.gz"))
       (sha256
        (base32 "0nxb29x7i2w6kjxq188n131b56bsqj1ykrxjcjp6sgpv81ym7f0m"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Scalar-List-Utils")
    (synopsis "Common Scalar and List utility subroutines")
    (description "This package contains a selection of subroutines that people
have expressed would be nice to have in the perl core, but the usage would not
really be high enough to warrant the use of a keyword, and the size so small
such that being individual extensions would be wasteful.")
    (license (package-license perl))))

(define-public perl-sdl
  (package
    (name "perl-sdl")
    (version "2.548")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FR/FROGGS/"
                           "SDL-" version ".tar.gz"))
       (sha256
        (base32 "1dagpmcpjnwvd4g6mmnc312rqpd4qcwx21rpi2j7084wz8mijai5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-alien-sdl" ,perl-alien-sdl)
       ("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-module-build" ,perl-module-build)
       ("perl-test-most" ,perl-test-most)
       ("perl-tie-simple" ,perl-tie-simple)))
    (inputs
     `(("freeglut" ,freeglut)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libsmpeg" ,libsmpeg)
       ("libtiff" ,libtiff)
       ("mesa" ,mesa)
       ("sdl" ,(sdl-union
                (list sdl sdl-gfx sdl-image sdl-mixer sdl-pango sdl-ttf)))))
    (propagated-inputs
     `(("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-tie-simple" ,perl-tie-simple)))
    (home-page "https://metacpan.org/release/SDL")
    (synopsis "SDL bindings to Perl")
    (description
     "SDL Perl is a package of Perl modules that provide both functional and
object oriented interfaces to the Simple DirectMedia Layer for Perl5.  This
package takes some liberties with the SDL API, and attempts to adhere to the
spirit of both the SDL and Perl.")
    (license license:lgpl2.1)))

(define-public perl-sgmls
  (package
    (name "perl-sgmls")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/R/RA/RAAB/SGMLSpm-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1gdjf3mcz2bxir0l9iljxiz6qqqg3a9gg23y5wjg538w552r432m"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap-script
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (site (string-append out "/lib/perl5/site_perl")))
                        (with-directory-excursion out
                          (rename-file "bin/sgmlspl.pl" "bin/sgmlspl")
                          (wrap-program "bin/sgmlspl"
                            `("PERL5LIB" suffix (,site))))
                        #t))))))
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page "https://metacpan.org/release/RAAB/SGMLSpm-1.1")
    (synopsis "Perl module for processing SGML parser output")
    (description "This package contains @code{SGMLS.pm}, a perl5 class library
for parsing the output from an SGML parser such as OpenSP.  It also includes
the @command{sgmlspl} command, an Perl script showcasing how the library can
be used.")
    (license license:gpl2+)))

(define-public perl-shell-command
  (package
    (name "perl-shell-command")
    (version "0.06")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/F/FL/FLORA/Shell-Command-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1lgc2rb3b5a4lxvbq0cbg08qk0n2i88srxbsz93bwi3razpxxr7k"))))
    (build-system perl-build-system)
    (home-page
      "https://metacpan.org/release/Shell-Command")
    (synopsis
      "Cross-platform functions emulating common shell commands")
    (description
      "Shell::Command is a thin wrapper around ExtUtils::Command.")
    (license (package-license perl))))

;;; END: Core module overrides

(define-public perl-file-find-object
 (package
  (name "perl-file-find-object")
  (version "0.2.13")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/S/SH/SHLOMIF/File-Find-Object-v"
             version ".tar.gz"))
      (sha256
        (base32
          "0gf13b76b824s73r5rp00v8xrd6dnb5yi5jjavfc394scqv6ldh4"))))
  (build-system perl-build-system)
  (native-inputs
    `(("perl-module-build" ,perl-module-build)))
  (inputs
    `(("perl-class-xsaccessor" ,perl-class-xsaccessor)))
  (home-page
    "https://metacpan.org/release/File-Find-Object")
  (synopsis
    "Object-oriented File::Find replacement in Perl")
  (description "File::Find::Object is an object-oriented
File::Find replacement in Perl.")
  (license license:artistic2.0)))

(define-public perl-file-find-object-rule
 (package
  (name "perl-file-find-object-rule")
  (version "0.0311")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/S/SH/SHLOMIF/File-Find-Object-Rule-"
             version
             ".tar.gz"))
      (sha256
        (base32 "0gjzfd5fz7mhr5abafxr7qic7nwhk7y9iv17as6l880973j952h3"))))
  (build-system perl-build-system)
  (native-inputs
    `(("perl-module-build" ,perl-module-build)))
  (inputs
    `(("perl-class-xsaccessor" ,perl-class-xsaccessor)
      ("perl-file-find-object" ,perl-file-find-object)
      ("perl-number-compare" ,perl-number-compare)
      ("perl-text-glob" ,perl-text-glob)))
  (home-page
    "https://metacpan.org/release/File-Find-Object-Rule")
  (synopsis
    "Alternative interface to File::Find::Object")
  (description "File::Find::Object::Rule is an alternative Perl
interface to File::Find::Object.")
  (license (package-license perl))))

(define-public perl-file-finder
  (package
    (name "perl-file-finder")
    (version "0.53")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/ME/MERLYN/File-Finder-"
             version ".tar.gz"))
       (sha256
        (base32
         "0x3a2xgzrka73lcmmwalq2mmpzxa7s6pm01ahxf677ksqsdc3jrf"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-text-glob" ,perl-text-glob)))
    (home-page "https://metacpan.org/release/File-Finder")
    (synopsis "Wrapper for @code{File::Find} ala @code{find(1)}")
    (description
     "@code{File::Find} is great, but constructing the wanted routine can
sometimes be a pain.  @code{File::Finder} provides a wanted-writer, using
syntax that is directly mappable to the @code{find(1)} command's syntax.

A @code{File::Finder} object contains a hash of @code{File::Find} options, and
a series of steps that mimic find's predicates.  Initially, a
@code{File::Finder} object has no steps.  Each step method clones the previous
object's options and steps, and then adds the new step, returning the new
object.  In this manner, an object can be grown, step by step, by chaining
method calls.  Furthermore, a partial sequence can be created and held, and
used as the head of many different sequences.")
    (license license:perl-license)))

(define-public perl-font-ttf
  (package
    (name "perl-font-ttf")
    (version "1.06")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/B/BH/BHALLISSY/Font-TTF-"
                    version ".tar.gz"))
              (sha256
               (base32
                "14y29ja3lsa3yw0ll20lj96f3zz5zydjqi1c5nh9wxar8927ssab"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-io-string" ,perl-io-string)))
    (home-page "https://metacpan.org/release/Font-TTF")
    (synopsis "TTF font support for Perl")
    (description "This package provides a Perl module for TrueType/OpenType
font hacking.  It supports reading, processing and writing of the following
tables: GDEF, GPOS, GSUB, LTSH, OS/2, PCLT, bsln, cmap, cvt, fdsc, feat,
fpgm, glyf, hdmx, head, hhea, hmtx, kern, loca, maxp, mort, name, post, prep,
prop, vhea, vmtx and the reading and writing of all other table types.")
    (license license:artistic2.0)))

(define-public perl-libtime-parsedate
  (package
    (name "perl-libtime-parsedate")
    (version "2015.103")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MU/MUIR/modules/Time-ParseDate-"
             version ".tar.gz"))
       (sha256
        (base32 "1lgfr87j4qwqnln0hyyzgik5ixqslzdaksn9m8y824gqbcihc6ic"))))
    (build-system perl-build-system)
    (arguments
     `(;; XXX: We'd like to use #:disallowed-references 'perl-build-system'
       ;; doesn't support it yet.
       ;;
       ;; #:disallowed-references (,tzdata-for-tests)

       #:phases
       (modify-phases %standard-phases
         ;; This is needed for tests
         (add-after 'unpack 'set-TZDIR
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZDIR" (string-append (assoc-ref inputs "tzdata")
                                            "/share/zoneinfo"))
             #t)))))
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("tzdata" ,tzdata-for-tests)))
    (home-page "https://metacpan.org/release/Time-ParseDate")
    (synopsis "Collection of Perl modules for time/date manipulation")
    (description "Provides several perl modules for date/time manipulation:
@code{Time::CTime.pm}, @code{Time::JulianDay.pm}, @code{Time::ParseDate.pm},
@code{Time::Timezone.pm}, and @code{Time::DaysInMonth.pm}.")
    ;; License text:
    ;;   "License hereby granted for anyone to use, modify or redistribute this
    ;;   module at their own risk. Please feed useful changes back to
    ;;   cpan@dave.sharnoff.org."
    (license (license:non-copyleft "http://metadata.ftp-master.debian.org/\
changelogs/main/libt/libtime-parsedate-perl/\
libtime-parsedate-perl_2015.103-2_copyright"))))

(define-public perl-libtime-period
  (package
    (name "perl-libtime-period")
    (version "1.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://http.debian.net/debian/pool/main/libt/"
             "libtime-period-perl/libtime-period-perl_"
             version ".orig.tar.gz"))
       (sha256
        (base32 "0c0yd999h0ikj88c9j95wa087m87i0qh7vja3715y2kd7vixkci2"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    ;; Unless some other homepage is out there...
    (home-page "https://packages.debian.org/stretch/libtime-period-perl")
    (synopsis "Perl library for testing if a time() is in a specific period")
    (description "This Perl library provides a function which tells whether a
specific time falls within a specified time period.  Its syntax for specifying
time periods allows you to test for conditions like \"Monday to Friday, 9am
till 5pm\" and \"on the second Tuesday of the month\" and \"between 4pm and
4:15pm\" and \"in the first half of each minute\" and \"in January of
1998\".")
    (license license:perl-license)))

(define-public perl-path-iterator-rule
  (package
    (name "perl-path-iterator-rule")
    (version "1.014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DA/DAGOLDEN/Path-Iterator-Rule-"
             version ".tar.gz"))
       (sha256
        (base32 "19mik0r5v1cmxfxm0h4lwqyj0nmq6jgnvvq96hqcjgylpvc02x1z"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-file-pushd" ,perl-file-pushd)
       ("perl-path-tiny" ,perl-path-tiny)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-filename" ,perl-test-filename)))
    (propagated-inputs
     `(("perl-number-compare" ,perl-number-compare)
       ("perl-text-glob" ,perl-text-glob)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "https://metacpan.org/release/Path-Iterator-Rule")
    (synopsis "Iterative, recursive file finder")
    (description "Path::Iterator::Rule iterates over files and directories to
identify ones matching a user-defined set of rules.  The API is based heavily
on File::Find::Rule, but with more explicit distinction between matching rules
and options that influence how directories are searched.  A
Path::Iterator::Rule object is a collection of rules (match criteria) with
methods to add additional criteria.  Options that control directory traversal
are given as arguments to the method that generates an iterator.

A summary of features for comparison to other file finding modules:

@itemize
@item provides many helper methods for specifying rules
@item offers (lazy) iterator and flattened list interfaces
@item custom rules implemented with callbacks
@item breadth-first (default) or pre- or post-order depth-first searching
@item follows symlinks (by default, but can be disabled)
@item directories visited only once (no infinite loop; can be disabled)
@item doesn't chdir during operation
@item provides an API for extensions
@end itemize

As a convenience, the PIR module is an empty subclass of this one that is less
arduous to type for one-liners.")
    (license license:asl2.0)))

(define-public perl-pod-constants
  (package
    (name "perl-pod-constants")
    (version "0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MG/MGV/Pod-Constants-"
             version ".tar.gz"))
       (sha256
        (base32
         "1njgr2zly9nrwvfrjhgk9dqq48as1pmbb2rs4bh3irvla75v7azg"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Pod-Constants")
    (synopsis "Include constants from POD")
    (description "This module allows you to specify those constants that
should be documented in your POD, and pull them out a run time in a fairly
arbitrary fashion.

Pod::Constants uses Pod::Parser to do the parsing of the source file.  It has
to open the source file it is called from, and does so directly either by
lookup in %INC or by assuming it is $0 if the caller is @code{main}
(or it can't find %INC{caller()}).")
    (license license:artistic2.0)))

(define-public perl-text-soundex
  (package
    (name "perl-text-soundex")
    (version "3.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/Text-Soundex-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1vb0vg1109gfzaak74ynw5s00ml28f33j612g2lxw98b52s5bpgn"))))
    (build-system perl-build-system)
    (home-page
     "https://metacpan.org/release/Text-Soundex")
    (synopsis "Implementation of the soundex algorithm.")
    (description "Soundex is a phonetic algorithm for indexing names by sound,
as pronounced in English.  The goal is for names with the same pronunciation to
be encoded to the same representation so that they can be matched despite
minor differences in spelling.

This module implements the original soundex algorithm developed by Robert
Russell and Margaret Odell, patented in 1918 and 1922, as well as a variation
called \"American Soundex\" used for US census data, and current maintained by
the National Archives and Records Administration (NARA).")
    (license license:perl-license)))

(define-public perl-regexp-pattern
  (package
    (name "perl-regexp-pattern")
    (version "0.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/P/PE/PERLANCAR/Regexp-Pattern-"
             version ".tar.gz"))
       (sha256
        (base32 "064igp2wxgsz4yb33v1r90i8clwjzs2xnpvw9niqlqrbzzrd4q1l"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)))
    (home-page "https://metacpan.org/release/Regexp-Pattern")
    (synopsis "Collection of regexp patterns")
    (description "Regexp::Pattern is a convention for organizing reusable
regexp patterns in modules.")
    (license (package-license perl))))

(define-public perl-data-sexpression
  (package
    (name "perl-data-sexpression")
    (version "0.41")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/N/NE/NELHAGE/Data-SExpression-"
             version ".tar.gz"))
       (sha256
        (base32
         "16qls1yqcmhxrcx9agsmaypxa1nirq4nvbyzbww9984589m44ql1"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-test-deep" ,perl-test-deep)))
    (propagated-inputs
     `(("perl-class-accessor" ,perl-class-accessor)))
    (home-page "https://metacpan.org/release/Data-SExpression")
    (synopsis "Parse Lisp S-Expressions into Perl data structures")
    (description "Data::SExpression parses Lisp S-Expressions into Perl data
structures.")
    (license license:perl-license)))
