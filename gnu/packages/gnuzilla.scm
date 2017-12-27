;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 ng0 <ng0@infotropique.org>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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
              '(substitute* '("js/src/config/milestone.pl")
                 (("defined\\(@TEMPLATE_FILE)") "@TEMPLATE_FILE")))))
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
           (lambda _ (delete-file "js/src/jit-test/tests/basic/bug698584.js")))
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
               (zero? (system*
                       "./configure" (string-append "--prefix=" out)
                                     ,@(if (string=? "aarch64-linux"
                                                     (%current-system))
                                         '("--host=aarch64-unknown-linux-gnu")
                                         '())))))))))
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
               '(substitute* '("js/src/config/milestone.pl")
                  (("defined\\(@TEMPLATE_FILE)") "@TEMPLATE_FILE")))))
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
                  (zero? (system* "./configure"
                                  (string-append "--prefix=" out)
                                  "--with-system-nspr"
                                  "--enable-system-ffi"
                                  "--enable-threadsafe"
                                  ,@(if (string=? "aarch64-linux"
                                                  (%current-system))
                                      '("--host=aarch64-unknown-linux-gnu")
                                      '()))))))))))
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
               (zero? (system* "./configure"
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
                               "--without-intl-api"))))))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)))
    (inputs
     `(("libffi" ,libffi)
       ("readline" ,readline)
       ("icu4c" ,icu4c)
       ("zlib" ,zlib)))))

(define-public nspr
  (package
    (name "nspr")
    (version "4.17")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                   version "/src/nspr-" version ".tar.gz"))
             (sha256
              (base32
               "158hdn285dsb5rys8wl1wi32dd1axwhqq0r8fwny4aj157m0l2jr"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)))
    (arguments
     `(#:tests? #f ; no check target
       #:configure-flags (list "--enable-64bit"
                               (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'chdir
                    (lambda _ (chdir "nspr") #t)))))
    (home-page
     "https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSPR")
    (synopsis "Netscape API for system level and libc-like functions")
    (description "Netscape Portable Runtime (NSPR) provides a
platform-neutral API for system level and libc-like functions.  It is used
in the Mozilla clients.")
    (license license:mpl2.0)))

(define-public nss
  (package
    (name "nss")
    (version "3.34")
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
                "1x9acn47iva9j42kxfamgvn99lrnqv47fgn3rz3j6c1ph50rai8d"))
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
             (zero? (system* "./nss/tests/all.sh"))))
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
    (version "52.3.0-gnu1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnuzilla/"
                          (first (string-split version #\-))
                          "/" name "-" version ".tar.bz2"))
      (sha256
       (base32
        "00jki754d6310fxj1b7dbhqj69y5igck6gqg6rgfya243nsb56k9"))
      (patches
       (list
        (search-patch "icecat-avoid-bundled-libraries.patch")
        (mozilla-patch "icecat-bug-546387.patch"         "d13e3fefb76e" "1b760r0bg2ydbl585wlmajljh1nlisrwxvjws5b28a3sgjy01i6k")
        (mozilla-patch "icecat-bug-1350152.patch"        "f822bda79c28" "1wf56169ca874shr6r7qx40s17h2gwj7ngmpyylrpmd1c6hipvsj")
        (mozilla-patch "icecat-bug-1388166.patch"        "fbb0bdb191d5" "1y8wpj38vw1dd6f375s9i0mrk9bd8z8gz5g70p4qynfllpkn072d")
        (mozilla-patch "icecat-CVE-2017-7810-pt1.patch"  "fbddb5cdd3c7" "0k5nyl2z1y2rx9fwqyfj64678yv6v3pnmshgk552pbzqmaf8i1hq")
        (mozilla-patch "icecat-CVE-2017-7810-pt2.patch"  "76c25987a275" "095b9vwsiza9ikbnnppfcld16h75x5bxjfxc73913y04n0i42ifh")
        (mozilla-patch "icecat-CVE-2017-7810-pt3.patch"  "32eec29a85a5" "057simakqg56jvas1wkskg5kszn96m74nca26x08d5w7rzmbv1q2")
        (mozilla-patch "icecat-bug-1373222.patch"        "ecef71fa933f" "0vsymgy5j702lamvh2riahni7rdj9ba3bd6i4a2m22d638rwp1i2")
        (mozilla-patch "icecat-CVE-2017-7814.patch"      "68a444daf85b" "1faaadaajidzb9i00710zxdyv370hlrdg1l5rw2ymfmzbjj4jqyd")
        (mozilla-patch "icecat-bug-1376825.patch"        "eeeec9cafc4e" "188qf6zi9kyxb33330yia6wmrd5mdyqn5hr1cl38zy7m3akv8srh")
        (mozilla-patch "icecat-bug-1385272.patch"        "d68fa12fbffc" "13gh97vz9n2b7303jcvr1072iy8bghy9chvbmxzvw82prvkscavw")
        (mozilla-patch "icecat-bug-1390002.patch"        "c24e6fc9f689" "0aswhy5fz2f6wzd5j5gg3nqvz707ip344089h2z2chcp146vxmf4")
        (mozilla-patch "icecat-CVE-2017-7810-pt4.patch"  "ae110cf77596" "0gdrkfq9wy9cfcdgbj14ci86xgh2prkbz69pfy97r9igd8059syw")
        (mozilla-patch "icecat-CVE-2017-7810-pt5.patch"  "b8417112486d" "1hya6lccz7vm51v4f6ww072794cwzmfn9xhxmvrnqbiyspxx5fz4")
        (mozilla-patch "icecat-bug-1386905.patch"        "badbf4308211" "0fj1pyjqfdsbrlfykwmkzav2nvdj1f4grwq3cal9f7ay6wjnfs9b")
        (mozilla-patch "icecat-CVE-2017-7810-pt6.patch"  "d78675515c78" "03w5hqy40xkckbaf5bm9kdbdqvp9ffvpk9mlrc9lja6b7qa4pjhg")
        (mozilla-patch "icecat-bug-1382303.patch"        "f01155fe4d54" "0hnz1kvmvspg6453kydsklbvrjgzn8x3djvrym3f2xl2yinaf90d")
        (mozilla-patch "icecat-bug-1393467.patch"        "4eec2a60622c" "1h006mwcsagq7mz7haymwgr7fn4zj14n5lxbjcmhdqrxdvma2hjj")
        (mozilla-patch "icecat-bug-1384801.patch"        "9556e792f905" "0i74r807189s8i78483caiifw68cn7fs543i4cys6k3gn12dhhjy")
        (mozilla-patch "icecat-CVE-2017-7823.patch"      "bd284765b5bc" "1c4hss87kc4qwx30magbqczm9h7zmwirjfc8zimqbrnwv9bbsfh3")
        (mozilla-patch "icecat-CVE-2017-7805.patch"      "113da8d46aa4" "1vy0lw659mwixmb57mgybga152rdwqd5zj1g7nfw1zgp15pfwr75")
        (mozilla-patch "icecat-bug-1376399.patch"        "58a574502ca9" "1zmg91pg0s5nwngc32a4fywidvxyaayvx1h052fsv0i4cfm16l9v")
        (mozilla-patch "icecat-bug-1396570.patch"        "24db61862c54" "0af1jjfma042xvn0xhgims1yvb2b51nhn4m0pcfwg3fn0llmka03")
        (mozilla-patch "icecat-CVE-2017-7819.patch"      "1a02f11c6efe" "18a9qvdvrqw34qw3lljg6gkn358jl23lyimhmbc964023rhs36sz")
        (mozilla-patch "icecat-CVE-2017-7810-pt7.patch"  "002686d9536f" "065g0d759wfiaj69b1sqg7l08p2knc0q9m9hvkgwwsf0r78xcbjj")
        (mozilla-patch "icecat-CVE-2017-7810-pt8.patch"  "eaadb31758d8" "0b3k3la6ykac5mbp9gyqqgjbmj19vx9sl1b0wp387qar0p12nyaz")
        (mozilla-patch "icecat-bug-1368269.patch"        "0cff5e66e0f4" "0jb0wqi7c0ih4441s1908j6gv18v4inh7k2w47h3c9nhz4rgyrw7")
        (mozilla-patch "icecat-CVE-2017-7793.patch"      "6ff3c82962f0" "0bw82034kdmrpznigbavzzsiybzrw8giyf8v0z2cxf6mwl72bf9k")
        (mozilla-patch "icecat-bug-1400399.patch"        "d6f78b1349b7" "0i3gwr2al3xl65yfa3nimvy8dp0jzpx21f6bjw18xwn7zkkh9j54")
        (mozilla-patch "icecat-bug-1400721.patch"        "285cde398833" "0a1i32zl30wfyw7zkqj595s94n6wdlg5c495m0910pd05pjg3qam")
        (mozilla-patch "icecat-CVE-2017-7826-pt01.patch" "98b3988592a6" "03wy173lj6mvmh5q92brf596h8676h0zasgqfnndpvsmsiaih120")
        (mozilla-patch "icecat-CVE-2017-7826-pt02.patch" "47590f0c274b" "0zsys6dcyhfb4a8k2dhsls7425jg6r1ijlrsn1lc5smwyf62zx5v")
        (mozilla-patch "icecat-CVE-2017-7826-pt03.patch" "55b435cbbb55" "1gcasaqrxa13a55v05bkxl3d1md829kpfhqiaws83wn08x28l0my")
        (mozilla-patch "icecat-CVE-2017-7826-pt04.patch" "8549cf2dab3e" "168gs32ncavaj9xn4gwhh9i01cbpnhgx9yn333apsrc1gwknpvsr")
        (mozilla-patch "icecat-CVE-2017-7826-pt05.patch" "349acf56ff49" "1vwn87rdryfjsn809pl50xmr82q98gz3vz9h6clkd905vbd9rwz7")
        (mozilla-patch "icecat-CVE-2017-7826-pt06.patch" "3af5bf8bdea0" "07az28dnpxr36j7i3llxkrlkrmg0bwk4f3sm75x1f0r1v5575p3p")
        (mozilla-patch "icecat-CVE-2017-7826-pt07.patch" "592df6088926" "1gy27idik4b6wcg4szww08cmpcljssja8wql6w1d807h7ni65lr7")
        (mozilla-patch "icecat-CVE-2017-7826-pt08.patch" "77a2d4610275" "13ysbwflnysj4rs45ibckd621s0vyg1s8dvannlvanvrz1g72zcz")
        (mozilla-patch "icecat-CVE-2017-7826-pt09.patch" "2b30335d0b95" "0hs5cwickvfw7r5dn7y148jgr2b21hl613qp83k56634d0y64qwp")
        (mozilla-patch "icecat-CVE-2017-7826-pt10.patch" "d6f008f95598" "0xclxrbg7pv8pa2j15p0gy9c8sigy2i9j0kvazl5fbyg6jsg3xgd")
        (mozilla-patch "icecat-CVE-2017-7826-pt11.patch" "ab9b51cd75ac" "08jy3rbkyh934aw261ls0s87947d2mhss7xqk1xfdir9crij2g27")
        (mozilla-patch "icecat-bug-1343147-pt1.patch"    "971d6345bc3a" "13791cvc51i991i5qyz6gp94vwzwkx479bnr8fsf8dw7z72wrsch")
        (mozilla-patch "icecat-bug-1343147-pt2.patch"    "60df7db06669" "0r372g1zksvkzyz0qpq0mp30frilgsfxxx2xida8xc08wgxp5lh9")
        (mozilla-patch "icecat-CVE-2017-7826-pt12.patch" "df49c25e6e4c" "0j77xbkxpflqf4jlccrv61vq0jgp4lfn8kb0zw1lswp3cyd6ml4i")
        (mozilla-patch "icecat-CVE-2017-7826-pt13.patch" "3b899f872623" "01zjcpm8yp4s8yf4mj1bzq01aylmi69kd8qv0rrcl9hmj4g3pzr2")
        (mozilla-patch "icecat-CVE-2017-7826-pt14.patch" "3d6d558ae6a6" "17wynknvs5wi7m9g5vn43rjmivbg1l6pnv8jymz1ccidy27qgdqi")
        (mozilla-patch "icecat-CVE-2017-7826-pt15.patch" "8426754b7130" "0bbm2294bkvld55rdbpsc8b82ljqdcxpbg6cwdzvwfhqayl2pnqm")
        (mozilla-patch "icecat-CVE-2017-7828-pt1.patch"  "5ddd5d2aa769" "12z5i8h6qwjb1h1fvp2426bgsnsxx539d8k2is9x1q4133356niy")
        (mozilla-patch "icecat-CVE-2017-7826-pt16.patch" "dd068f4e132a" "17qy9c1vfkz3pj6y8qmqbic73wrangsbdlylk2s54nbzhhp9cj1g")
        (mozilla-patch "icecat-CVE-2017-7826-pt17.patch" "e6bd533b57e9" "1mmqav9yhxd0j47yffcdykaqjibfwjsk0jn0f44099s87y8qn9zy")
        (mozilla-patch "icecat-CVE-2017-7826-pt18.patch" "2a87fb6b9c07" "0z0scw4y1vqhqkbw1ag14g8xrif14l95x7fd50q2sw425lli29lc")
        (mozilla-patch "icecat-bug-1404910.patch"        "5007f2472f64" "0ns1l4yipwbb52sps2xzg30qd1rkpwykxq4chjg3wllhmcxbvvpw")
        (mozilla-patch "icecat-CVE-2017-7830.patch"      "04e3b5c1f0b2" "0nmv3jnx94ykxg64xkiwc8kx4df8zw7y5yzjnxz1sll2v88b9hmf")
        (mozilla-patch "icecat-CVE-2017-7828-pt2.patch"  "2f48c03d9b3f" "16qdy3rahmkhsjnzqjcgzg1a4k7czd40m04gs9i75cd88kbripri")
        (mozilla-patch "icecat-bug-1348660-pt1.patch"    "a352bfcbaf55" "1j3kxnhci9fh3lj8rizbcfv8xzn5kikxwpfy8a091d51sdn20873")
        (mozilla-patch "icecat-bug-1348660-pt2.patch"    "57f43e2ab9b5" "1jva4y79zb85npak3mddrx5rsf4mxczb314kcr8yhlkwqv0nx5sp")
        (mozilla-patch "icecat-bug-1348660-pt3.patch"    "917d65bb8896" "0k29y8i96lanqjjm6vybg0s6gjbk1mz5bfnga6aj1g0hnb7c3s8d")
        (mozilla-patch "icecat-bug-1348660-pt4.patch"    "28934912eede" "1mhxw26050l3d09n8w912a86df87afcshvsk9k1k375anfk0927x")
        (search-patch  "icecat-bug-1348660-pt5.patch")
        (mozilla-patch "icecat-bug-1348660-pt6.patch"    "556ff3bfb9fc" "0kckjc8jp885xfaiwx2b9qnk1plqjhi0mwhjjcmfajvh3l3mrl8h")
        (mozilla-patch "icecat-bug-1350564.patch"        "2abf26abb2a2" "0axdzp9g9k74wpkwrsdx263h01sv9bd3rarhhl68xnvc7n6i45lx")
        (mozilla-patch "icecat-bug-1404787.patch"        "8335e1d7b140" "17d7kb8ginzflhdkrbi60vh2b907spbzmvwih7a595gqpihmnqqn")
        (mozilla-patch "icecat-CVE-2017-7826-pt19.patch" "de336078d36b" "0gyzbap8hr1iywk0x2x0h7z7zp7q89hi56h8c03vvc7771dkvjkf")
        (mozilla-patch "icecat-bug-1047098-pt1.patch"    "088577f0c46e" "0y3sz6kx07ls7jsvhqhhrl6v69a94wqvv3lz7hnplah23y06h17z")
        (mozilla-patch "icecat-bug-1047098-pt2.patch"    "c7e3abf74023" "11dcjzx56v4yb2dvm23j2g86q8yva2hc69lmb7s5w18l6ygwwhzr")
        (mozilla-patch "icecat-bug-1047098-pt3.patch"    "36bd15d14c5a" "0cb3l3hpsgk674f08kfamxhqfga3ah5v904gpxq9ag006vzd2cxz")
        (mozilla-patch "icecat-bug-1404105.patch"        "2909ba991f31" "126vssj57dc800347f075wlnjzcwamnxxmgxl9w78jpb0hj9gf16")
        (search-patch  "icecat-bug-1415133.patch")
        (mozilla-patch "icecat-bug-1355576.patch"        "cf34a0574e58" "1z7sa1d12hypgivm5xxn32s58afpjcij97jvnafcgnfvxywrgr1m")
        (mozilla-patch "icecat-CVE-2017-7843.patch"      "f6216ea8b8fc" "0jnhdkj0ch9mj01mzlvhjgf8zsxlbg6m7yvpq99qr7xmg0pzbgwl")
        (mozilla-patch "icecat-bug-1413741.patch"        "4e00ce2897c4" "0k95vi31glia2i03djidkc0gkwp9qldy34fz1rxcj56a1iphbq7w")
        (mozilla-patch "icecat-bug-1224396.patch"        "92d450811409" "0xsvggnr0y65nd52nkbjvpcbs5nd84pvbayk5vinbx1mnk2wh2vy")
        (mozilla-patch "icecat-bug-1415582.patch"        "7eba7d14704a" "1vi17qmjzh3kji14iz370kvs4425asgp93ns2chf5ldlq5b9196g")
        (mozilla-patch "icecat-bug-1417797.patch"        "457d023c167e" "11g8hg8yp20lsn52dx1ym8r4yjsnsmx0h182d6nbl6ab9wp7d1m9")
        (mozilla-patch "icecat-bug-1410134.patch"        "5e7b16213198" "14c4x6c3mygf8p77n9bia5rndjpngbvik1r1ylk97k3ggy4fj6zh")
        (mozilla-patch "icecat-bug-1419363.patch"        "0712b6cbbdc8" "0rllsq6ckpms7g9k6qky1gr5rz1gav4widrha6w1s9f88cbrqgk5")
        (mozilla-patch "icecat-bug-1408276.patch"        "084c427ccf99" "0sjdy2iang09a9g6liavpjgry04dp6smjgj0y7lp5lfqijdr8q2d")
        (mozilla-patch "icecat-bug-1382366.patch"        "1bfb3d8d4510" "0c2dcxj74ijs6qf9sqcbj8w998hblic66vy41818z7xnw46j5j1j")
        (mozilla-patch "icecat-bug-1414425.patch"        "5623e01e63a8" "08dn3v96bsb61hy3wfxz43fhn1mk9vlm5ydvdjgi3wiqadvacgzs")
        (mozilla-patch "icecat-bug-1409951.patch"        "14a389d40329" "0f4gbak5bd2walxrxs3myig28v9lhvplf3a1nws1a4ajx80slzq1")
        (mozilla-patch "icecat-bug-1415441.patch"        "7339297cddb7" "017lbw0mn5rwzb2abfw6qrk07m3r96vwbj81cmqvdfnmprcjni5j")
        (mozilla-patch "icecat-bug-1418922.patch"        "aa55d4cdaee5" "1l3qwjfx0jsbbw2dg8bsnx7k47zibamgswndq0d1bchnmary62aw")
        (mozilla-patch "icecat-bug-1382358.patch"        "762f4e53889a" "0n61zrb6rz9bhhdsqs5ziwaiy81pq52c76p9qmi9hrxbn24ism1k")
        (mozilla-patch "icecat-bug-1399520.patch"        "0152d097672f" "16ybg718calvciv00kil8s97lhh11hj6gx0acf73r44xfkvm8nfg")
        (search-patch  "icecat-bug-1414945.patch")
        (mozilla-patch "icecat-bug-1414452.patch"        "079356ed5317" "107c0b93g2k743wvhwz2ps3j6p09qld7d0raljijv5y5n8q4wp92")
        (mozilla-patch "icecat-bug-1418854.patch"        "93e4994a892c" "00r2qxw3619529vy9d04dl9kcziqy3fv3iawgy9svzygyx1kj5wx")
        (mozilla-patch "icecat-bug-1422389.patch"        "f8a6e1864832" "1wbxn0v50637yjg8b8675k01x9cyx95jpjxpyqfaa97762qkznba")
        (mozilla-patch "icecat-bug-1415598.patch"        "0cc1c9068714" "1qmqpi14zs7c95k3c7396gpp6apb622k0mgv553kw4rr81nj1yac")
        (mozilla-patch "icecat-bug-1418447.patch"        "ce6f3fb2bf58" "1b1msb5d5jsgrqa2hkbsrm0n54qdmx1b2bf65v44v17appa03lra")
        (mozilla-patch "icecat-bug-1423159.patch"        "6b4d3c5d5e51" "074p93dhwr1ckhypkjpblnmg9hg44a9030g1glqffi9dyn3iq3k4")
        (mozilla-patch "icecat-bug-1411745.patch"        "1a510ee578a0" "1imb7glh2m1zwvvpvr4k4iddms5byqzr35j7kv3y5is77aiwl4z5")
        (mozilla-patch "icecat-bug-1411708.patch"        "34c968767eb7" "0l2jy201ikj3m3h66mvlsj4y0ki7cpm7x7nnfygbwnfxg42s1sip")
        (mozilla-patch "icecat-bug-1423086.patch"        "bc166be85bb4" "0w1lrjzfrfflaw4l6sfi3ir81iyi9gyfck5g41dwp0jc1b59jzvg")
        (mozilla-patch "icecat-bug-1412145.patch"        "66cfc3c4047d" "05j8ic4lv2d2ygr6d62rkdlfyg2rpljalwrkkhllinw2dfi3n15b")
        (mozilla-patch "icecat-bug-1399400.patch"        "3236ffdf0ced" "1kvk4qyslaj1ldgs1wpxnf79zajcihzcd1zvbrg990i3hgyn3gk3")
        (mozilla-patch "icecat-bug-1424373-pt1.patch"    "320032aaa068" "1ch282qibprz1q0f2imvynh4sg7gads6sf3ayhjcd62zjncpgyz7")
        (search-patch  "icecat-bug-1424373-pt2.patch")))
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
                      ;; TODO: Use system harfbuzz.  Waiting for:
                      ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=847568>
                      ;;
                      ;; TODO: Use system graphite2.
                      ;;
                      "modules/freetype2"
                      "modules/zlib"
                      "modules/libbz2"
                      "ipc/chromium/src/third_party/libevent"
                      "media/libjpeg"
                      "media/libvpx"
                      "security/nss"
                      "gfx/cairo"
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
       ("pango" ,pango)
       ("freetype" ,freetype)
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
       ("ffmpeg" ,ffmpeg)
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
                           "--enable-gio"
                           "--enable-startup-notification"
                           "--enable-pulseaudio"

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
          'unpack 'use-skia-by-default
          (lambda _
            ;; Use the bundled Skia library by default, since IceCat appears
            ;; to be far more stable when using it than when using our system
            ;; Cairo.
            (let ((out (open "browser/app/profile/icecat.js"
                              (logior O_WRONLY O_APPEND))))
              (format out "~%// Use Skia by default~%")
              (format out "pref(~s, ~s);~%" "gfx.canvas.azure.backends" "skia")
              (format out "pref(~s, ~s);~%" "gfx.content.azure.backends" "skia")
              (close-port out))
            #t))
         (add-after
          'unpack 'arrange-to-link-libxul-with-libraries-it-might-dlopen
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
              (zero? (apply system* bash
                            (string-append srcdir "/configure")
                            flags)))))
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
                    "mozicon128.png" "default256.png"))))))
         ;; This fixes the file chooser crash that happens with GTK 3.
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (gtk (assoc-ref inputs "gtk+"))
                    (gtk-share (string-append gtk "/share")))
               (wrap-program (car (find-files lib "^icecat$"))
                 `("XDG_DATA_DIRS" ":" prefix (,gtk-share)))))))))
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
