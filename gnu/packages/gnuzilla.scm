;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages gnuzilla)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libreoffice)  ;for hunspell
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages readline))

(define-public mozjs
  (package
    (name "mozjs")
    (version "17.0.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/js/"
                   name version ".tar.gz"))
             (sha256
              (base32
               "1fig2wf4f10v43mqx67y68z6h77sy900d1w0pz9qarrqx57rc7ij"))
             (patches (search-patches "mozjs17-aarch64-support.patch"))
             (modules '((guix build utils)))
             (snippet
              ;; Fix incompatibility with Perl 5.22+.
              '(begin
                 (substitute* '("js/src/config/milestone.pl")
                   (("defined\\(@TEMPLATE_FILE)") "@TEMPLATE_FILE"))
                 #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)))
    (propagated-inputs
     `(("nspr" ,nspr))) ; in the Requires.private field of mozjs-17.0.pc
    (inputs
     `(("zlib" ,zlib)))
    (arguments
     `(;; XXX: parallel build fails, lacking:
       ;;   mkdir -p "system_wrapper_js/"
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-timedout-test
           ;; This test times out on slower hardware.
           (lambda _
             (delete-file "js/src/jit-test/tests/basic/bug698584.js")
             #t))
         (add-before 'configure 'chdir
           (lambda _
             (chdir "js/src")
             #t))
         (replace 'configure
           ;; configure fails if it is followed by SHELL and CONFIG_SHELL
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))
               (invoke "./configure" (string-append "--prefix=" out)
                       ,@(if (string=? "aarch64-linux"
                                       (%current-system))
                             '("--host=aarch64-unknown-linux-gnu")
                             '()))))))))
    (home-page
     "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey")
    (synopsis "Mozilla javascript engine")
    (description "SpiderMonkey is Mozilla's JavaScript engine written
in C/C++.")
    (license license:mpl2.0))) ; and others for some files

(define-public mozjs-24
  (package (inherit mozjs)
    (name "mozjs")
    (version "24.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://ftp.mozilla.org/pub/mozilla.org/js/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1n1phk8r3l8icqrrap4czplnylawa0ddc2cc4cgdz46x3lrkybz6"))
              (modules '((guix build utils)))
              (patches (search-patches "mozjs24-aarch64-support.patch"))
              (snippet
               ;; Fix incompatibility with Perl 5.22+.
               '(begin
                  (substitute* '("js/src/config/milestone.pl")
                    (("defined\\(@TEMPLATE_FILE)") "@TEMPLATE_FILE"))
                  #t))))
    (arguments
      (substitute-keyword-arguments (package-arguments mozjs)
        ((#:phases phases)
         `(modify-phases ,phases
            (replace 'configure
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  ;; configure fails if it is followed by SHELL and CONFIG_SHELL
                  (setenv "SHELL" (which "sh"))
                  (setenv "CONFIG_SHELL" (which "sh"))
                  (invoke "./configure"
                          (string-append "--prefix=" out)
                          "--with-system-nspr"
                          "--enable-system-ffi"
                          "--enable-threadsafe"
                          ,@(if (string=? "aarch64-linux"
                                          (%current-system))
                                '("--host=aarch64-unknown-linux-gnu")
                                '())))))))))
    (inputs
     `(("libffi" ,libffi)
       ("zlib" ,zlib)))))

(define-public mozjs-38
  (package
    (inherit mozjs)
    (name "mozjs")
    (version "38.2.1.rc0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://people.mozilla.org/~sstangl/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0p4bmbpgkfsj54xschcny0a118jdrdgg0q29rwxigg3lh5slr681"))
              (patches
               (search-patches
                ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1269317 for
                ;; GCC 6 compatibility.

                "mozjs38-version-detection.patch" ; for 0ad
                "mozjs38-tracelogger.patch"

                ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1339931.
                "mozjs38-pkg-config-version.patch"
                "mozjs38-shell-version.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Fix incompatibility with sed 4.4.
                  (substitute* "js/src/configure"
                    (("\\^\\[:space:\\]") "^[[:space:]]"))

                  ;; The headers are symlinks to files that are in /tmp, so they
                  ;; end up broken.  Copy them instead.
                  (substitute*
                      "python/mozbuild/mozbuild/backend/recursivemake.py"
                    (("\\['dist_include'\\].add_symlink")
                     "['dist_include'].add_copy"))

                  ;; Remove bundled libraries.
                  (for-each delete-file-recursively
                            '("intl"
                              "js/src/ctypes/libffi"
                              "js/src/ctypes/libffi-patches"
                              "modules/zlib"))
                  #t))))
    (arguments
     `(;; XXX: parallel build fails, lacking:
       ;;   mkdir -p "system_wrapper_js/"
       #:parallel-build? #f
       ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1008470.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (chdir "js/src")
               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       "--enable-ctypes"
                       "--enable-gcgenerational"
                       "--enable-optimize"
                       "--enable-pie"
                       "--enable-readline"
                       "--enable-shared-js"
                       "--enable-system-ffi"
                       "--enable-threadsafe"
                       "--enable-xterm-updates"
                       "--with-system-icu"
                       "--with-system-nspr"
                       "--with-system-zlib"

                       ;; Intl API requires bundled ICU.
                       "--without-intl-api")))))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)))
    (inputs
     `(("libffi" ,libffi)
       ("readline" ,readline)
       ("icu4c" ,icu4c)
       ("zlib" ,zlib)))))

(define-public mozjs-52
  ;; No releases yet at <https://archive.mozilla.org/pub/spidermonkey/releases/>.
  ;; While we could take a snapshot of the complete mozilla-esr52 repository at
  ;; <https://treeherder.mozilla.org/#/jobs?repo=mozilla-esr52&filter-searchStr=sm-tc>,
  ;; we take the Debian version instead, because it is easier to work with.
  (let ((commit "6507e63cc416fd7a3269e390efe712f8b56f374a")
        (revision "1"))
    (package (inherit mozjs-38)
      (version (git-version "52.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://salsa.debian.org/gnome-team/mozjs52.git")
                      (commit commit)))
                (file-name (git-file-name "mozjs" version))
                (sha256
                 (base32
                  "1ny0s53r8wn4byys87h784xrq1xg767akmfm6gqrbvrz57mlm3q2"))))
      (arguments
       `(#:tests? #f ; depends on repository metadata
         #:configure-flags
         '("--enable-ctypes"
           "--enable-optimize"
           "--enable-pie"
           "--enable-readline"
           "--enable-shared-js"
           "--enable-system-ffi"
           "--with-system-icu"
           "--with-system-nspr"
           "--with-system-zlib"

           ;; Intl API requires bundled ICU.
           "--without-intl-api")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-and-chdir
             (lambda* (#:key inputs #:allow-other-keys)
               ;; This patch prevents a segfault when executing JS_Init().
               ;; The build does not fail without this patch, but the
               ;; configure phase of the gjs package would fail.
               ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1176787
               (make-file-writable "js/src/old-configure.in")
               (make-file-writable "js/src/old-configure")
               (make-file-writable "mozglue/build/moz.build")
               (invoke "patch" "-p1" "--force"
                       "--input" "debian/patches/disable-mozglue.patch")
               (invoke "touch" "js/src/configure")
               (chdir "js/src")
               #t))
           (replace 'configure
             (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
               ;; The configure script does not accept environment variables
               ;; as arguments.
               (let ((out (assoc-ref outputs "out")))
                 (setenv "SHELL" (which "sh"))
                 (setenv "CONFIG_SHELL" (which "sh"))
                 (setenv "AUTOCONF" (string-append (assoc-ref inputs "autoconf")
                                                   "/bin/autoconf"))
                 (apply invoke "./configure"
                        (cons (string-append "--prefix=" out)
                              configure-flags))))))))
      (native-inputs
       `(("autoconf" ,autoconf-2.13)
         ("automake" ,automake)
         ,@(package-native-inputs mozjs-38))))))

(define-public nspr
  (package
    (name "nspr")
    (version "4.19")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                   version "/src/nspr-" version ".tar.gz"))
             (sha256
              (base32
               "0agpv3f17h8kmzi0ifibaaxc1k3xc0q61wqw3l6r2xr2z8bmkn9f"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)))
    (arguments
     `(#:tests? #f ; no check target
       #:configure-flags (list "--enable-64bit"
                               (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))
       ;; Use fixed timestamps for reproducibility.
       #:make-flags '("SH_DATE='1970-01-01 00:00:01'"
                      ;; This is epoch 1 in microseconds.
                      "SH_NOW=100000")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'chdir
                    (lambda _ (chdir "nspr") #t)))))
    (home-page
     "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSPR")
    (synopsis "Netscape API for system level and libc-like functions")
    (description "Netscape Portable Runtime (@dfn{NSPR}) provides a
platform-neutral API for system level and libc-like functions.  It is used
in the Mozilla clients.")
    (license license:mpl2.0)))

(define-public nss
  (package
    (name "nss")
    (version "3.38")
    (source (origin
              (method url-fetch)
              (uri (let ((version-with-underscores
                          (string-join (string-split version #\.) "_")))
                     (string-append
                      "https://ftp.mozilla.org/pub/mozilla.org/security/nss/"
                      "releases/NSS_" version-with-underscores "_RTM/src/"
                      "nss-" version ".tar.gz")))
              (sha256
               (base32
                "0qigcy3d169cf67jzv3rbai0m6dn34vp8h2z696mz4yn10y3sr1c"))
              ;; Create nss.pc and nss-config.
              (patches (search-patches "nss-pkgconfig.patch"
                                       "nss-increase-test-timeout.patch"))))
    (build-system gnu-build-system)
    (outputs '("out" "bin"))
    (arguments
     `(#:parallel-build? #f ; not supported
       #:make-flags
       (let* ((out (assoc-ref %outputs "out"))
              (nspr (string-append (assoc-ref %build-inputs "nspr")))
              (rpath (string-append "-Wl,-rpath=" out "/lib/nss")))
         (list "-C" "nss" (string-append "PREFIX=" out)
               "NSDISTMODE=copy"
               "NSS_USE_SYSTEM_SQLITE=1"
               (string-append "NSPR_INCLUDE_DIR=" nspr "/include/nspr")
               ;; Add $out/lib/nss to RPATH.
               (string-append "RPATH=" rpath)
               (string-append "LDFLAGS=" rpath)))
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (setenv "CC" "gcc")
             ;; Tells NSS to build for the 64-bit ABI if we are 64-bit system.
             ,@(match (%current-system)
                 ((or "x86_64-linux" "aarch64-linux")
                  `((setenv "USE_64" "1")))
                 (_
                  '()))
             #t))
         (replace 'check
           (lambda _
             ;; Use 127.0.0.1 instead of $HOST.$DOMSUF as HOSTADDR for testing.
             ;; The later requires a working DNS or /etc/hosts.
             (setenv "DOMSUF" "(none)")
             (setenv "USE_IP" "TRUE")
             (setenv "IP_ADDRESS" "127.0.0.1")
             (invoke "./nss/tests/all.sh")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append (assoc-ref outputs "bin") "/bin"))
                      (inc (string-append out "/include/nss"))
                      (lib (string-append out "/lib/nss"))
                      (obj (match (scandir "dist" (cut string-suffix? "OBJ" <>))
                             ((obj) (string-append "dist/" obj)))))
                 ;; Install nss-config to $out/bin.
                 (install-file (string-append obj "/bin/nss-config")
                               (string-append out "/bin"))
                 (delete-file (string-append obj "/bin/nss-config"))
                 ;; Install nss.pc to $out/lib/pkgconfig.
                 (install-file (string-append obj "/lib/pkgconfig/nss.pc")
                               (string-append out "/lib/pkgconfig"))
                 (delete-file (string-append obj "/lib/pkgconfig/nss.pc"))
                 (rmdir (string-append obj "/lib/pkgconfig"))
                 ;; Install other files.
                 (copy-recursively "dist/public/nss" inc)
                 (copy-recursively (string-append obj "/bin") bin)
                 (copy-recursively (string-append obj "/lib") lib)

                 ;; FIXME: libgtest1.so is installed in the above step, and it's
                 ;; (unnecessarily) linked with several NSS libraries, but
                 ;; without the needed rpaths, causing the 'validate-runpath'
                 ;; phase to fail.  Here we simply delete libgtest1.so, since it
                 ;; seems to be used only during the tests.
                 (delete-file (string-append lib "/libgtest1.so"))
                 (delete-file (string-append lib "/libgtestutil.so"))

                 #t))))))
    (inputs
     `(("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (propagated-inputs `(("nspr" ,nspr))) ; required by nss.pc.
    (native-inputs `(("perl" ,perl)))

    ;; The NSS test suite takes around 48 hours on Loongson 3A (MIPS) when
    ;; another build is happening concurrently on the same machine.
    (properties '((timeout . 216000)))  ; 60 hours

    (home-page
     "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSS")
    (synopsis "Network Security Services")
    (description
     "Network Security Services (@dfn{NSS}) is a set of libraries designed to
support cross-platform development of security-enabled client and server
applications.  Applications built with NSS can support SSL v2 and v3, TLS,
PKCS #5, PKCS #7, PKCS #11, PKCS #12, S/MIME, X.509 v3 certificates, and other
security standards.")
    (license license:mpl2.0)))

(define (mozilla-patch file-name changeset hash)
  "Return an origin for CHANGESET from the mozilla-esr52 repository."
  (origin
    (method url-fetch)
    (uri (string-append "https://hg.mozilla.org/releases/mozilla-esr52/raw-rev/"
                        changeset))
    (sha256 (base32 hash))
    (file-name file-name)))

(define-public icecat
  (package
    (name "icecat")
    (version "52.6.0-gnu1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnuzilla/"
                          (first (string-split version #\-))
                          "/" name "-" version ".tar.bz2"))
      (sha256
       (base32
        "09fn54glqg1aa93hnz5zdcy07cps09dbni2b4200azh6nang630a"))
      (patches
       (list
        (search-patch "icecat-avoid-bundled-libraries.patch")
        (search-patch "icecat-use-system-harfbuzz.patch")
        (search-patch "icecat-use-system-graphite2.patch")
        (mozilla-patch "icecat-bug-546387.patch"         "d13e3fefb76e" "1b760r0bg2ydbl585wlmajljh1nlisrwxvjws5b28a3sgjy01i6k")
        (mozilla-patch "icecat-bug-1350152.patch"        "f822bda79c28" "1wf56169ca874shr6r7qx40s17h2gwj7ngmpyylrpmd1c6hipvsj")
        (mozilla-patch "icecat-bug-1411708.patch"        "34c968767eb7" "0l2jy201ikj3m3h66mvlsj4y0ki7cpm7x7nnfygbwnfxg42s1sip")
        (mozilla-patch "icecat-bug-1375217.patch"        "00fc630c9a46" "17pcprp452nslk6sac6sili0p74zh8w3g0v1wsdn0ikm9xmnphhv")
        (mozilla-patch "icecat-CVE-2018-5145.patch"      "f0ec180993d2" "0jiazxcwki83wr00fyh2g518ynsd33p7nk65zk4d1682gn22lc8v")
        (mozilla-patch "icecat-CVE-2018-5130.patch"      "a6a9e26688c1" "0cvizvilb4k422j2gzqcbakznvsffmk6n6xn1ayj5rgxfaizkkqk")
        (mozilla-patch "icecat-CVE-2018-5125-pt1.patch"  "198ad052621e" "1721zx8hifdlflrhvw6hmkdgjbvsmxl9n84iji5qywhlp2krdk9r")
        (mozilla-patch "icecat-bug-1426087.patch"        "391ea77ebfdb" "1fhkvd0z6mvdkj7m0d3jlj42rsdw5r4x122c1wb1i428228ifw6n")
        (mozilla-patch "icecat-bug-1416307.patch"        "54f2f7f93b30" "1ncjir16mqya37wgf6fy2rqki3vl433c4grjr3fypmlig6xfgg1l")
        (mozilla-patch "icecat-CVE-2018-5127.patch"      "2c4d7a59041b" "178c6gid89cvw52yqs43i6x6s5w0hslj0rfa2r8b4762ij3civ92")
        (mozilla-patch "icecat-CVE-2018-5125-pt2.patch"  "f87ef3774d5e" "0payf3az2w93nzl5qknqx290jbxk8v39rwhdgq7wyd5f245dywxk")
        (mozilla-patch "icecat-CVE-2018-5125-pt3.patch"  "ac743923f81d" "0msyr45xr1j5q4x6ah4r907pwjngyi0k6pp9y8ixk21cnwbzrdwx")
        (mozilla-patch "icecat-CVE-2018-5129.patch"      "456913d7e8b5" "0fx0s06kxxj7g4hllinaskgh41z3k48zml6yqqzxx485qk3hdh9x")
        (mozilla-patch "icecat-bug-1334465-pt1.patch"    "f95c5b881442" "0iaddhf65jd9cycj4bw0b207n2jiqkr4q84jifzyqn4ygs75wdqd")
        (mozilla-patch "icecat-bug-1334465-pt2.patch"    "8a4265c8fb41" "1d9zfdbrlw9wzr84b7pj7lxgy487lsx0kfd89287hjk0al8m6vrw")
        (mozilla-patch "icecat-bug-1398021.patch"        "28855df568d8" "1kmq836gniplxpjnvq8lhbcc1aqi56al628r1mzdy94b5yb0lis3")
        (mozilla-patch "icecat-bug-1388020.patch"        "e8ab2736499b" "0n28vcd65rxsyq3z22rfcfksryfndhm1i3g6ah3akg11jnagqf5v")
        (mozilla-patch "icecat-CVE-2018-5125-pt4.patch"  "014877bf17ea" "0hk90pnf7h7kvidji6ydvva1zpyraipn03pjhvprdqr7k2fqzmsz")
        (mozilla-patch "icecat-CVE-2018-5125-pt5.patch"  "5b3a5de48912" "1ifya05rcd34ryp9zawdacihhkkf2m0xn2q8m8c6v78bvxj0mgig")
        (mozilla-patch "icecat-CVE-2018-5144.patch"      "1df9b4404acd" "1sd59vsarfsbh3vlrzrqv6n1ni7vxdzm83j6s6g0fygl1h8kwijg")
        (mozilla-patch "icecat-bug-1430173-pt1.patch"    "9124c3972e2b" "13ns5yy39yzfx7lrkv4rgwdz6s6q0z4i09wkbxdvnkfsz17cd17i")
        (mozilla-patch "icecat-bug-1430173-pt2.patch"    "9f6dc031be51" "0bv2p98z5ahp3x9wxnhwxn87g21djvzzp7jy55ik90hqixsbhwdl")
        (mozilla-patch "icecat-CVE-2018-5131.patch"      "3102fbb97b32" "0kg0183v92gxjb9255xjwhxyd6gl77l9c0civx3040k975fybwlp")
        (mozilla-patch "icecat-CVE-2018-5125-pt6.patch"  "4904c0f4a645" "0lsq62ynksy1fbw0m87f1d741fyvrrp1vrznx5hx0l2p4g4frhv3")
        (mozilla-patch "icecat-CVE-2018-5125-pt7.patch"  "16b8073d5c30" "1dv94qqah1wjd3bxjvrkmjbb2f95d3d11zpm8mggdk52il575bwl")
        (mozilla-patch "icecat-bug-1442127-pt1.patch"    "f931f85b09da" "02s380w8a73g4w2wm810lbigh4z4rrlfy10ywwhv4lpkbk8xg7pr")
        (mozilla-patch "icecat-bug-1442127-pt2.patch"    "da5792b70f30" "116k9qja5ir9b3laazasp43f5jx59qq72nknmq5bn5v1ixya9r4l")
        (mozilla-patch "icecat-CVE-2018-5125-pt8.patch"  "62b831df8269" "109pn0hqn7s27580glv4z7qv1pmjzii9szvf3wkn97k5wybrzgkx")
        (mozilla-patch "icecat-bug-1442504.patch"        "8954ce68a364" "0bl65zw82bwqg0mmcri94pxqq6ibff7y5rclkzapb081p6yvf73q")
        (mozilla-patch "icecat-CVE-2018-5125-pt9.patch"  "8a16f439117c" "108iarql6z7h1r4rlzac6n6lrzs78x7kcdbfa0b5dbr5xc66jmgb")
        (mozilla-patch "icecat-bug-1426603.patch"        "ca0b92ecedee" "0dc3mdl4a3hrq4j384zjavf3splj6blv4masign710hk7svlgbhq")
        (mozilla-patch "icecat-CVE-2018-5146.patch"      "494e5d5278ba" "1yb4lxjw499ppwhk31vz0vzl0cfqvj9d4jwqag7ayj53ybwsqgjr")
        (mozilla-patch "icecat-CVE-2018-5147.patch"      "5cd5586a2f48" "10s774pwvj6xfk3kk6ivnhp2acc8x9sqq6na8z47nkhgwl2712i5")
        (mozilla-patch "icecat-CVE-2018-5148.patch"      "c3e447e07077" "0gmwy631f8ip4gr1mpbjk8bx1n1748wdls5zq4y8hpmpnq5g1wyx")
        (mozilla-patch "icecat-CVE-2018-5178.patch"      "17201199b18d" "1d0hcim1fwh0bklwpmnal1mv9d9kmyif1m15aj1nqkf1n3x4xc37")
        (mozilla-patch "icecat-bug-1361699.patch"        "a07d6c3ff262" "1z8mjg2487r8pxi0x951v6fwwr696q84f6hlzimc3r7bn5ds9r83")
        (mozilla-patch "icecat-CVE-2018-5150-pt01.patch" "7127ccf8f88c" "0m4my7aflpp0wlqilr2m4axd7k2fyrs7jqdcz2rrz5pwivz1anvd")
        (mozilla-patch "icecat-bug-1444231.patch"        "57bd35fa8618" "0pl6x5amc5x6nhwl7qnmnff3jjjxmbs8r365bfzj58g7q5ihqwvf")
        (mozilla-patch "icecat-CVE-2018-5150-pt02.patch" "2f3e1ccf1661" "0azl8g81kpc0w2xpjpgm1154ll12g0a8n6i7bl3s9nnrk2i26n74")
        (mozilla-patch "icecat-CVE-2018-5159.patch"      "8ff2c4d68e36" "0kz1rqhnz8ca4z20hnpcafidhsrwhnm0h2gmlgchni33h8pisr1f")
        (mozilla-patch "icecat-CVE-2018-5154.patch"      "b8c430253efd" "1arjcaps9axhxh5ff84n9bydhhzrihn7hbq7v69nvqwqrjp3lgg9")
        (mozilla-patch "icecat-CVE-2018-5155.patch"      "05cadfa3ac39" "0q0vh7vy7x0l8jp6376fn10qljfp4mnp4m9zfn90j4m19pfl86a0")
        (mozilla-patch "icecat-CVE-2018-5168.patch"      "48a678d7cb81" "1yfh7kxxxvqck2hpn98pwag4splyc6c9brc5haq28fp8x9r9qvlk")
        (mozilla-patch "icecat-CVE-2018-5150-pt03.patch" "112032576872" "1x1hxyggbxlnlj0n9cbp03hjnfvm6cq8nqj0jizrd8cfyd5aig8p")
        (mozilla-patch "icecat-CVE-2018-5150-pt04.patch" "ad9a885b0df4" "1hrk1q9mk59jww55g4lqmaflznk87x3vvjn2mxfgfbbjs8l1cyz4")
        (mozilla-patch "icecat-bug-1452416.patch"        "f89ab96a2532" "1dqchxdyznhgyxhfq0hm0vg1p597hjqflfzigc7j3s5vxf9rg2nv")
        (mozilla-patch "icecat-CVE-2018-5150-pt05.patch" "af885a1bd293" "1wfpqhm2dp4fsx6zbrncngsqz7g2x09b625zcighixrbpvybyww3")
        (mozilla-patch "icecat-CVE-2018-5150-pt06.patch" "666fc84ec72d" "0lml2wqd4yqidhi364x8r90f78397k2y0kq5z5bv8l8j4bhcnb9v")
        (search-patch  "icecat-CVE-2018-5157-and-CVE-2018-5158.patch")
        (mozilla-patch "icecat-CVE-2018-5150-pt07.patch" "1ab40761a856" "1kgwypy7k5b33jwkni4025za4kcnv5m6klsx4wsswlixmljmkbc7")
        (mozilla-patch "icecat-bug-1453339.patch"        "0edb8dca7087" "0b30pipqryh311sc97rcmwnx9n8qdlbbz90b2hkybjnprmbhfxrm")
        (mozilla-patch "icecat-CVE-2018-5150-pt08.patch" "134c728799c1" "16hbwx6fx1hrddsyjjbd3z954ql3pg348xs13h9riyblq8crzmam")
        (mozilla-patch "icecat-CVE-2018-5150-pt09.patch" "14eab155eaa8" "0wr4xgblxzk4c2gvlnpl7ic1196mrhry1hgwdl1jivq0ji5cbvbd")
        (mozilla-patch "icecat-bug-1452619.patch"        "2b75d55ccf0e" "1g87aybw6ggv6hyk385bplv0lx63n020gwyq0d6d4pqld48hsm1i")
        (mozilla-patch "icecat-CVE-2018-5156-pt1.patch"  "89857f35df29" "0gzi47svrw5ajdlm3i12193psm702zx70x5h1rwp4gb7gxh4m4d9")
        (mozilla-patch "icecat-CVE-2018-5150-pt10.patch" "3f2ec03c0405" "0w02952dlxd2gmwghck2nm4rjjmc5ylg62bw6m1rvi35kcr134lr")
        (mozilla-patch "icecat-CVE-2018-5183.patch"      "f729bf78fb3a" "0xkj6jwxwdqkvb5c7wi16b8cm8qrnlrd3s9jnd46jg03iykrx56f")
        (mozilla-patch "icecat-CVE-2018-5188-pt01.patch" "eb896089db47" "10lppk4x2d3pim71a36ky1dmg08rs5ckfiljwvfnr1cw6934qxl4")
        (mozilla-patch "icecat-CVE-2018-5188-pt02.patch" "2374dca97bde" "0y1g55wvj44nzb1qfkl271jcf8s1ik8lcl1785z0zim4qzn7qkpa")
        (mozilla-patch "icecat-CVE-2018-5188-pt03.patch" "70b6298e0c9e" "0n5jfy6c421dkybk8m18vd61y95zz0r64g1p1zlya3fps5knfaqi")
        (mozilla-patch "icecat-CVE-2018-12365-pt1.patch" "4ef79fe9b3b7" "1c32z1ki1i6xj1nbb0xlxwqnmz48ikmy8dmp37rkjz8ssn04wgfg")
        (mozilla-patch "icecat-CVE-2018-12365-pt2.patch" "9ad16112044a" "0ayya67sx7avcb8bplfdxb92l9g4mjrb1s3hby283llhqv0ikg9b")
        (mozilla-patch "icecat-CVE-2018-12359.patch"     "11d8a87fb6d6" "1rkmdk18llw0x1jakix75hlhy0hpsmlminnflagbzrzjli81gwm1")
        (mozilla-patch "icecat-CVE-2018-5188-pt04.patch" "407b10ad1273" "16qzsfirw045xag96f1qvpdlibm8lwdj9l1mlli4n1vz0db91v9q")
        (mozilla-patch "icecat-CVE-2018-6126.patch"      "e76e2e481b17" "0hnx13msjy28n3bpa2c24kpzalam4bdk5gnp0f9k671l48rs9yb3")
        (mozilla-patch "icecat-CVE-2018-5188-pt05.patch" "2c75bfcd465c" "1pjinj8qypafqm2fk68s3hzcbzcijn09qzrpcxvzq6bl1yfc1xfd")
        (mozilla-patch "icecat-CVE-2018-5188-pt06.patch" "042f80f3befd" "0av918kin4bkrq7gnjz0h9w8kkq8rk9l93250lfl5kqrinza1gsk")
        (mozilla-patch "icecat-CVE-2018-5188-pt07+bugs-1455071+1433642+1456604+1458320.patch"
                                                         "bb0451c9c4a0" "1lhm1b2a7c6jwhzsg3c830hfhp17p8j9zbcmgchpb8c5jkc3vw0x")
        (mozilla-patch "icecat-CVE-2018-5188-pt08.patch" "8189b262e3b9" "13rh86ddwmj1bhv3ibbil3sv5xbqq1c9v1czgbsna5hxxkzc1y3b")
        (mozilla-patch "icecat-CVE-2018-5188-pt09.patch" "9f81ae3f6e1d" "05vfg8a8jrzd93n1wvncmvdmqgf9cgsl8ryxgjs3032gbbjkga7q")
        (mozilla-patch "icecat-CVE-2018-12360.patch"     "face7a3dd5d7" "0jclw30mf693w8lrmvn0iankggj21nh4j3zh51q5363rj5xncdzx")
        (mozilla-patch "icecat-CVE-2018-5188-pt10.patch" "7afb58c046c8" "1r0569r76712x7x1sw6xr0x06ilv6iw3fncb0f8r8b9mp6wrpx34")
        (mozilla-patch "icecat-CVE-2018-12362-pt1.patch" "f1a745f8c42d" "11q73pb7a8f09xjzil4rhg5nr49zrnz1vb0prni0kqvrnppf5s40")
        (mozilla-patch "icecat-CVE-2018-12362-pt2.patch" "1f9a430881cc" "0f79rv7njliqxx33z07n60b50jg0a596d1km7ayz2hivbl2d0168")
        (mozilla-patch "icecat-CVE-2018-5188-pt11.patch" "28f4fc0a5141" "1a8f9z6c80in8ccj82ysdrcr2lqypp29l4acs50kwncm0c0b01zl")
        (mozilla-patch "icecat-CVE-2018-12363.patch"     "ad5a53a1d2b1" "0rhl4r39ydb3lkfp5pkwvhhzqgfh33s9r7b7jccgkrx6f13xyq78")
        (mozilla-patch "icecat-CVE-2018-5188-pt12.patch" "0ddfc03c0454" "1b0xw2kj9765lvpl8iwr3wwcz40bdfp3dp4y9f546a61qsi9q9d6")
        (mozilla-patch "icecat-CVE-2018-5156-pt2.patch"  "dbf36189a364" "1awbyhy0r79i03sns2p0m78f9hb6c7kp4hwia2khx4qszlsr4j95")
        (mozilla-patch "icecat-CVE-2018-5188-pt13.patch" "32509dfde003" "0cc3c92dgf5qynk093prq610c9x815l2fa24ddrw9czdzbwblsdq")
        (mozilla-patch "icecat-bug-1462912.patch"        "f18535a212da" "0zkqz9il89f1s1yrp5c6hj6kysy2x02iy50vgwdj30lr56gkpzmk")
        (mozilla-patch "icecat-CVE-2018-5188-pt14.patch" "e8e9e1ef79f2" "0dc8p6fsppq3bhbpmp41f8mjxbr31pvgpga0a73dqdaicq5ydgj4")
        (search-patch  "icecat-bug-1413868-pt1.patch")
        (mozilla-patch "icecat-CVE-2018-5188-pt15.patch" "9d4d31b2630d" "1lcbmsyi09kp80h1jgxj5l45zl24xn22h1lq7drbyjxsn1kggq4g")
        (mozilla-patch "icecat-CVE-2018-12366-pt1.patch" "edf2c7dff493" "06xmyk7nm54cm9m6qc59wz8cxxfa5r25mf2xzdzy74iq5hwa1ac8")
        (mozilla-patch "icecat-CVE-2018-5188-pt16.patch" "05549a4d1b80" "10q68cllshmmhlrbirm9h4gyc3ffrcpsxihfpcbxh90nv2h16jci")
        (mozilla-patch "icecat-CVE-2018-12364.patch"     "67b2d8924841" "197riigbb6l30959pygr0zlv7vaims78dg1mh0pg33pa7cbna0ds")
        (mozilla-patch "icecat-CVE-2018-12366-pt2.patch" "528d4d997bb3" "0f375i96a404dkn0fanmd9pgfj3wyrhjfc5dwslw2s44gwfjhljb")
        (mozilla-patch "icecat-bug-1369771.patch"        "fab16ad7f256" "0kd8qm04sjgfgfg8yw3ivcxazb1d7v430g86chw4n64qybsh9ka3")
        (mozilla-patch "icecat-CVE-2018-5188-pt17.patch" "068e249d02b4" "1iy9by1mg5qhp8502h31m8zm99aq2hx0c5n3hadd5pk11lfnq6ll")
        (mozilla-patch "icecat-bug-1413868-pt2.patch"    "755067c14b06" "089dwqwzcdg1l6aimi0i65q4dgb2iny5h8yjx63h9zgv77n0700a")))
      (modules '((guix build utils)))
      (snippet
       '(begin
          (use-modules (ice-9 ftw))
          ;; Remove bundled libraries that we don't use, since they may
          ;; contain unpatched security flaws, they waste disk space and
          ;; network bandwidth, and may cause confusion.
          (for-each delete-file-recursively
                    '(;; FIXME: Removing the bundled icu breaks configure.
                      ;;   * The bundled icu headers are used in some places.
                      ;;   * The version number is taken from the bundled copy.
                      ;;"intl/icu"
                      ;;
                      ;; FIXME: A script from the bundled nspr is used.
                      ;;"nsprpub"
                      ;;
                      ;; TODO: Use system media libraries.  Waiting for:
                      ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=517422>
                      ;;   * libogg
                      ;;   * libtheora
                      ;;   * libvorbis
                      ;;   * libtremor (not yet in guix)
                      ;;   * libopus
                      ;;   * speex
                      ;;   * soundtouch (not yet in guix)
                      ;;
                      "modules/freetype2"
                      "modules/zlib"
                      "modules/libbz2"
                      "ipc/chromium/src/third_party/libevent"
                      "media/libjpeg"
                      "media/libvpx"
                      "security/nss"
                      "gfx/cairo"
                      "gfx/harfbuzz"
                      "gfx/graphite2"
                      "js/src/ctypes/libffi"
                      "db/sqlite3"))
          ;; Delete .pyc files, typically present in icecat source tarballs
          (for-each delete-file (find-files "." "\\.pyc$"))
          ;; Delete obj-* directories, sometimes present in icecat tarballs
          (for-each delete-file-recursively
                    (scandir "." (lambda (name)
                                   (string-prefix? "obj-" name))))
          #t))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bzip2" ,bzip2)
       ("cairo" ,cairo)
       ("cups" ,cups)
       ("dbus-glib" ,dbus-glib)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("graphite2" ,graphite2)
       ("pango" ,pango)
       ("freetype" ,freetype)
       ("harfbuzz" ,harfbuzz)
       ("hunspell" ,hunspell)
       ("libcanberra" ,libcanberra)
       ("libgnome" ,libgnome)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libxft" ,libxft)
       ("libevent" ,libevent-2.0)
       ("libxinerama" ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxcomposite" ,libxcomposite)
       ("libxt" ,libxt)
       ("libffi" ,libffi)
       ("ffmpeg" ,ffmpeg-3.4)
       ("libvpx" ,libvpx)
       ("icu4c" ,icu4c)
       ("pixman" ,pixman)
       ("pulseaudio" ,pulseaudio)
       ("mesa" ,mesa)
       ("mit-krb5" ,mit-krb5)
       ("nspr" ,nspr)
       ("nss" ,nss)
       ("sqlite" ,sqlite)
       ("startup-notification" ,startup-notification)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2) ; Python 3 not supported
       ("python2-pysqlite" ,python2-pysqlite)
       ("yasm" ,yasm)
       ("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf-2.13)
       ("which" ,which)))
    (arguments
     `(#:tests? #f          ; no check target
       #:out-of-source? #t  ; must be built outside of the source directory

       ;; XXX: There are RUNPATH issues such as
       ;; $prefix/lib/icecat-31.6.0/plugin-container NEEDing libmozalloc.so,
       ;; which is not in its RUNPATH, but they appear to be harmless in
       ;; practice somehow.  See <http://hydra.gnu.org/build/378133>.
       #:validate-runpath? #f

       #:configure-flags '("--enable-default-toolkit=cairo-gtk3"

                           "--with-distribution-id=org.gnu"

                           "--enable-gio"
                           "--enable-startup-notification"
                           "--enable-pulseaudio"

                           "--disable-tests"
                           "--disable-updater"
                           "--disable-crashreporter"
                           "--disable-maintenance-service"
                           "--disable-eme"
                           "--disable-gconf"
                           "--disable-gnomeui"

                           ;; Building with debugging symbols takes ~5GiB, so
                           ;; disable it.
                           "--disable-debug"
                           "--disable-debug-symbols"

                           ;; Hack to work around missing
                           ;; "unofficial" branding in icecat.
                           "--enable-official-branding"

                           ;; Avoid bundled libraries.
                           "--with-system-zlib"
                           "--with-system-bz2"
                           "--with-system-jpeg"        ; must be libjpeg-turbo
                           "--with-system-libevent"
                           "--with-system-libvpx"
                           "--with-system-icu"
                           "--with-system-nspr"
                           "--with-system-nss"
                           "--with-system-harfbuzz"
                           "--with-system-graphite2"
                           "--enable-system-pixman"
                           "--enable-system-cairo"
                           "--enable-system-ffi"
                           "--enable-system-hunspell"
                           "--enable-system-sqlite"

                           ;; Fails with "--with-system-png won't work because
                           ;; the system's libpng doesn't have APNG support".
                           ;; According to
                           ;; http://sourceforge.net/projects/libpng-apng/ ,
                           ;; "the Animated Portable Network Graphics (APNG)
                           ;; is an unofficial extension of the Portable
                           ;; Network Graphics (PNG) format";
                           ;; we probably do not wish to support it.
                           ;; "--with-system-png"
                           )

       #:modules ((ice-9 ftw)
                  (ice-9 rdelim)
                  (ice-9 match)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'ensure-no-mtimes-pre-1980
          (lambda _
            ;; Without this, the 'source/test/addons/packed.xpi' and
            ;; 'source/test/addons/simple-prefs.xpi' targets fail while trying
            ;; to create zip archives.
            (let ((early-1980 315619200)) ; 1980-01-02 UTC
              (ftw "." (lambda (file stat flag)
                         (unless (<= early-1980 (stat:mtime stat))
                           (utime file early-1980 early-1980))
                         #t))
              #t)))
         (add-after
          'unpack 'link-libxul-with-libraries
          (lambda _
            ;; libxul.so dynamically opens libraries, so here we explicitly
            ;; link them into libxul.so instead.
            ;;
            ;; TODO: It might be preferable to patch in absolute file names in
            ;; calls to dlopen or PR_LoadLibrary, but that didn't seem to
            ;; work.  More investigation is needed.
            (substitute* "toolkit/library/moz.build"
              (("^# This library needs to be last" all)
               (string-append "OS_LIBS += [
    'GL', 'gnome-2', 'canberra', 'Xss', 'cups', 'gssapi_krb5',
    'avcodec', 'avutil', 'pulse' ]\n\n"
                              all)))
            #t))
         (replace
          'configure
          ;; configure does not work followed by both "SHELL=..." and
          ;; "CONFIG_SHELL=..."; set environment variables instead
          (lambda* (#:key outputs configure-flags #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bash (which "bash"))
                   (abs-srcdir (getcwd))
                   (srcdir (string-append "../" (basename abs-srcdir)))
                   (flags `(,(string-append "--prefix=" out)
                            ,(string-append "--with-l10n-base="
                                            abs-srcdir "/l10n")
                            ,@configure-flags)))
              (setenv "SHELL" bash)
              (setenv "CONFIG_SHELL" bash)
              (setenv "AUTOCONF" (which "autoconf")) ; must be autoconf-2.13
              (mkdir "../build")
              (chdir "../build")
              (format #t "build directory: ~s~%" (getcwd))
              (format #t "configure flags: ~s~%" flags)
              (apply invoke bash
                     (string-append srcdir "/configure")
                     flags))))
         (add-before 'configure 'install-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Install the '.desktop' file.
             (define (swallow-%%-directives input output)
               ;; Interpret '%%ifdef' directives found in the '.desktop' file.
               (let loop ((state 'top))
                 (match (read-line input 'concat)
                   ((? eof-object?)
                    #t)
                   ((? string? line)
                    (cond ((string-prefix? "%%ifdef" line)
                           (loop 'ifdef))
                          ((string-prefix? "%%else" line)
                           (loop 'else))
                          ((string-prefix? "%%endif" line)
                           (loop 'top))
                          (else
                           (case state
                             ((top else)
                              (display line output)
                              (loop state))
                             (else
                              (loop state)))))))))

             (let* ((out (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications")))
               (call-with-input-file "debian/icecat.desktop.in"
                 (lambda (input)
                   (call-with-output-file "debian/icecat.desktop"
                     (lambda (output)
                       (swallow-%%-directives input output)))))

               (substitute* "debian/icecat.desktop"
                 (("@MOZ_DISPLAY_NAME@")
                  "GNU IceCat")
                 (("^Exec=@MOZ_APP_NAME@")
                  (string-append "Exec=" out "/bin/icecat"))
                 (("@MOZ_APP_NAME@")
                  "icecat"))
               (install-file "debian/icecat.desktop" applications)
               #t)))
         (add-after 'install-desktop-entry 'install-icons
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "browser/branding/official"
                 (for-each
                  (lambda (file)
                    (let* ((size (string-filter char-numeric? file))
                           (icons (string-append out "/share/icons/hicolor/"
                                                 size "x" size "/apps")))
                      (mkdir-p icons)
                      (copy-file file (string-append icons "/icecat.png"))))
                  '("default16.png" "default22.png" "default24.png"
                    "default32.png" "default48.png" "content/icon64.png"
                    "mozicon128.png" "default256.png"))
                 #t))))
         ;; This fixes the file chooser crash that happens with GTK 3.
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (gtk (assoc-ref inputs "gtk+"))
                    (gtk-share (string-append gtk "/share")))
               (wrap-program (car (find-files lib "^icecat$"))
                 `("XDG_DATA_DIRS" ":" prefix (,gtk-share)))
               #t))))))
    (home-page "https://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons.  It also
features built-in privacy-protecting features.")
    (license license:mpl2.0)     ;and others, see toolkit/content/license.html
    (properties
     `((ftp-directory . "/gnu/gnuzilla")
       (cpe-name . "firefox_esr")
       (cpe-version . ,(first (string-split version #\-)))))))
