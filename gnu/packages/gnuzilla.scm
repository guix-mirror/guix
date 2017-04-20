;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
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
  #:use-module (gnu packages zip))

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

(define-public nspr
  (package
    (name "nspr")
    (version "4.14")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                   version "/src/nspr-" version ".tar.gz"))
             (sha256
              (base32
               "1m8p9l3prabhfxz6zs889fl7gmcka72a62i46a8klh2pca11iz34"))))
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
    (version "3.30.2")
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
                "096frzvyp3z257x84rxknscfgsbavzh2a0gyibx7kvmw4vzpfjhd"))
              ;; Create nss.pc and nss-config.
              (patches (search-patches "nss-pkgconfig.patch"
                                       "nss-disable-long-b64-tests.patch"
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
     "Network Security Services (NSS) is a set of libraries designed to support
cross-platform development of security-enabled client and server applications.
Applications built with NSS can support SSL v2 and v3, TLS, PKCS #5, PKCS #7,
PKCS #11, PKCS #12, S/MIME, X.509 v3 certificates, and other security
standards.")
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
    (version "52.0.2-gnu1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnuzilla/"
                          (first (string-split version #\-))
                          "/" name "-" version ".tar.bz2"))
      (sha256
       (base32
        "0asaba04y6rwc7nx898p89jdxkbnsb3dxjvpdi8xb1rbgyms80c9"))
      (patches
       (list
        (search-patch "icecat-avoid-bundled-libraries.patch")
        (mozilla-patch "icecat-CVE-2017-5443.patch"      "6daaaff9f1f6" "0jvb6y5fiwr13fyx58k49n81kv6h03vcch502g57y6nsx2wsqng6")
        (mozilla-patch "icecat-bug-1319087.patch"        "82297fcc6f19" "02qcbg2r2smswgnwj7fs5bcrr3rlqbpsh2nmcbsjyblp5fk1ag36")
        (mozilla-patch "icecat-CVE-2017-5429-pt01.patch" "dd526ebe7e58" "1rj0pz6iql59zrynz48njcfg8i0v55bjdndplss9wl37lfydl7ca")
        (mozilla-patch "icecat-CVE-2017-5447-pt1.patch"  "3bc981f85a17" "0am9k3mii2r05lp6xpizxp356mb8xrbqs9kmx0wx5wyy08wjzmks")
        (mozilla-patch "icecat-CVE-2017-5447-pt2.patch"  "4f752b0e5920" "183s5dwzd57b299grvyvn139fsp9am0smd3yb4shw8g0iwzz61nf")
        (mozilla-patch "icecat-CVE-2017-5449.patch"      "1714eda3de9b" "0ncngdpzvffvpw4c1mi0dda5l02lwyil4rnq3i6salnwlrq9x32z")
        (mozilla-patch "icecat-CVE-2017-5455.patch"      "b10922304d81" "0rglbavb8rx7hl53ksgypazz27263b1yn97gznpdsq89zhirfw3m")
        (mozilla-patch "icecat-CVE-2017-5446.patch"      "d98de46f8f27" "040agykr4w4wsbi0xm3rrrjxk48iwz8l1hn11vfv45nzsx2f1hzq")
        ;; The next patch is for CVE-2017-5436 in the bundled graphite2.
        ;; TODO: apply additional fixes from our system graphite2 to the
        ;; bundled copy, or upgrade it in place.
        (mozilla-patch "icecat-CVE-2017-5436.patch"      "e6132f638311" "07w9pijx42psgmkj2i6i87lf30gl0yyb5caz6wz7fm8phi8wwy9p")
        (mozilla-patch "icecat-bug-1342395.patch"        "0e0e8abe2153" "1xlnq2fd50kf0rz9dibz5vlaa9zj2pifjvky2fdykcan62xz75hy")
        (mozilla-patch "icecat-bug-1342841.patch"        "623afac083f8" "1pv86j0dxdmi7g3rx4zqplz4gxq5lfyzpdssq83naypcxic6zafb")
        (mozilla-patch "icecat-bug-1344644.patch"        "cac0735c228f" "0695f0hvxnzgcirgxx3axn5nhkywqxjcvnrlhg7jwfann4mnbsfn")
        (mozilla-patch "icecat-bug-1322660.patch"        "9d6d60e64255" "0ds74ilhyc9qkkjgkm0xk7ay3926971rzwfh2avhhkfargn7idib")
        (mozilla-patch "icecat-bug-1343330.patch"        "6f23bd449bc7" "1igz6yhx803hygf7cii8bchx7bfw1niq8s0nc5l9i5rb8ml2b7f0")
        (mozilla-patch "icecat-bug-1346961.patch"        "3a2dc54cf986" "0dfp3s7d43zx3svajbkhvi73b71hhr7vrc9yz0iz37pykg40c4hn")
        (mozilla-patch "icecat-bug-1318070.patch"        "a68d6d9b87d0" "1yqgkgv7i0xy5dm0pgg1cbav4qglsdk8brzcjcpfz65bmn1pqrhh")
        (mozilla-patch "icecat-CVE-2017-5448.patch"      "6684a3c7f834" "0agyynvcjk28d7l2l4cqz67ddg9xw7ymiirb0npabd1si9zj27xb")
        (mozilla-patch "icecat-bug-1336345.patch"        "590416f46ec8" "1q2svqjd735rickr9i3kdkd0la6ikdphhmzr19h1r84nrl6a87ia")
        (mozilla-patch "icecat-bug-1336356.patch"        "00ba83ac39be" "1h6qsfv4r9mlc2ihjm9kmzi76aijdnnyx1g2r30ia87xha106pnk")
        (mozilla-patch "icecat-bug-1342363.patch"        "10285b4a6b71" "0l2ww19y6qbarcp9brjgbpf4vi3k38r6ak8is5736vqz0c17dim0")
        (mozilla-patch "icecat-bug-1343787.patch"        "28287b7f0938" "1w85s9rqh0dyfx6qn5plypbypz9casig03b6yiy9bpiq7ckrxz56")
        (mozilla-patch "icecat-bug-1292803.patch"        "adbf7b59a405" "1l1p0b5rc05czk6kr3k3k99m1fkwphj2jrd092gdbib8q4m4cvzv")
        (mozilla-patch "icecat-bug-1313869.patch"        "eba25396310b" "1ws0dr0kwclzbc2m0sihd3aqvbbg57ycia0fg6y294k6qipcxv38")
        (mozilla-patch "icecat-bug-1141756-pt1.patch"    "f7c262517722" "0r1zzbxf47q5w8vcy402yin105ngam3csb2q7h7n8axm97307ykp")
        (mozilla-patch "icecat-bug-1141756-pt2.patch"    "420396d5e26d" "0yv1pmpydzkirfwrxrgbw98dm4a9a4s0izha0wabrp4lb3655jv5")
        (mozilla-patch "icecat-bug-1343210.patch"        "ed9521749d6f" "1j2zzi00qyqjgh15ingvl6f88zlk4imp31m5jmf7w5f9jqi5ly3k")
        (mozilla-patch "icecat-bug-1342442.patch"        "775b6f85ef81" "00h9dgds7jv9n4cznj5qhh8liaf1b3wvnlqc2z7a3kj07ijllwzb")
        (mozilla-patch "icecat-bug-1344527.patch"        "d4612b14c907" "1n3a0mp351a7xgvshm6688gh89hg0xci3y621zs2pyqsfm114366")
        (mozilla-patch "icecat-CVE-2017-5442.patch"      "5f1aa2336998" "1y2marhrglc66vchd6z0jdmhg0pmkxp1cwim63bp9l6pj7lxyjma")
        (mozilla-patch "icecat-CVE-2017-5430-pt01.patch" "512604631b23" "171nzxr4av4818d0fyg9hcsdxkai61sghl45xnsr2al34l28wsw3")
        (mozilla-patch "icecat-CVE-2017-5430-pt02.patch" "16772200ad6f" "087j16rcbs5kgvpa096kd6jarwwwfrhwph54wzjn671wr1vnsvvd")
        (mozilla-patch "icecat-CVE-2017-5441.patch"      "c744e9d57250" "0m70157lczf17hxb2pabsl3grhcjqallbdfpsd58q8q6fk99k6x3")
        (mozilla-patch "icecat-CVE-2017-5433.patch"      "b4fc7a4cb5e0" "12q6mr5prpgqg5xnrww09qjm3jx2amb8zig62cd46ba8n9z2j9ab")
        (mozilla-patch "icecat-CVE-2017-5429-pt02.patch" "21eac0b4fd2f" "1a6v0hwcc26gnlxygplc11dfzc8bykhh44j4gsz88kl5c5jqhlk9")
        (mozilla-patch "icecat-CVE-2017-5432.patch"      "62df7046e959" "1qvxbpkf87g4vnl8hxqvwb1ydrpkqq3rbkivr8q4029rvgalf4rf")
        (mozilla-patch "icecat-bug-1350599.patch"        "f6a978b2fcec" "0rkbbmw52mxgrmn1xny4jkn3slwb5jsqs4yr07ffhz7r801jy9iz")
        (mozilla-patch "icecat-bug-1332839.patch"        "2ad0f87f5dba" "04458jidri521hgf3r63pl736zz4gmgv6b8spa32anfb7gryj8fy")
        (mozilla-patch "icecat-bug-1337548.patch"        "29a1ad09a6ec" "0pld81bpc34w6g2ara54sx30msas55kwzr537pvxxc002lpvzs57")
        (mozilla-patch "icecat-CVE-2017-5430-pt03.patch" "5dec7534760f" "1xh0y7srl7nznb6szpfiykd6r1ibyxrdvasc36w0chqjdmq7xr32")
        (mozilla-patch "icecat-bug-1343851.patch"        "e104d53316d7" "1yhv3qvzzi3kr881ji1dnm8ydnr3snh2vzl3c4vdzmvrjx8q5rcb")
        (mozilla-patch "icecat-bug-1345222.patch"        "864644fadcb0" "0qpplxyfn87bigzdkwlrhj9isd5gfafhjgqfckb239a09wwrblf3")
        (mozilla-patch "icecat-bug-1348584.patch"        "7cee9ad555af" "0856bpa3n71a3y5m4gilcdb9ghb60p545xkv9zbr245r20mj32ds")
        (mozilla-patch "icecat-bug-1346720.patch"        "6a597a9cd494" "091a5sanw3w3gl0jcmf8d60m59vwbh5v36vnar20m0hl7xrv4v7p")
        (mozilla-patch "icecat-CVE-2017-5430-pt04.patch" "09693629803f" "18fhmsghq0232mhh8j10cy0a4979nmkbh43jlcyrg3l63l7795k4")
        (mozilla-patch "icecat-CVE-2017-5430-pt05.patch" "2b8268ea97a9" "0l0f54krxdmqbgldikwjncxvn6irihcljldd3z039himrvplisjg")
        (mozilla-patch "icecat-bug-1347700-pt1.patch"    "ee706896916c" "0m85x80y98c154hyis08kcy81kbw3v34na1v862vxzs939d3mc0n")
        (mozilla-patch "icecat-bug-1347700-pt2.patch"    "08ecc2d92f81" "1s6411ccifw9l22hhmf32nhm8r5hbclnhy7jm2n228sqfr4h971g")
        (mozilla-patch "icecat-bug-1337682.patch"        "15af6a323161" "1nxbwd0574gscnkxfyhzv3yqvxiccb2d0rmba9vi6i62646l2pd5")
        (mozilla-patch "icecat-CVE-2017-5451.patch"      "d91260f0069a" "15w4rzz51hps2fr8djf5z1rzdwxshclk936mxv5anx1skkwms0y8")
        (mozilla-patch "icecat-CVE-2017-5444.patch"      "7740cf7e121b" "1706mx4zmnib336p2wmfp9ncyl66lk2da82f28xvcw262mg1c8lw")
        (mozilla-patch "icecat-bug-1347164-pt1.patch"    "b35a6d6dcdca" "077r0pns58fw3xd3qnbhib4q21vvw0aynpa8iyn1pycg8mppmd0f")
        (mozilla-patch "icecat-bug-1347164-pt2.patch"    "a42fc05969b9" "1ijq8ccsk5k56h77sv5kqv48w7csj3vbakzq98awgbvypzfdyhss")
        (mozilla-patch "icecat-bug-1347164-pt3.patch"    "f78ac1ac0a37" "0kj6jq482cqwyngy1kmb69zpq35xah8h33kml8i4l7andiyaq3zm")
        (mozilla-patch "icecat-bug-1347164-pt4.patch"    "795a3d48a775" "18lw99hmrr93k95hk6v6bx5rcf22aa902x2yf5p6wxdqg56nc0zp")
        (mozilla-patch "icecat-bug-1338699.patch"        "94ce63191069" "0rdivablincah3gbgl4wzjmqlraazivmr8bhqxdpy8dk0a6fvv4s")
        (mozilla-patch "icecat-bug-1342301.patch"        "e640e758a7cd" "17f36vvf82n6shlaip7ji8qsy9861f9a5r79h000p3wb3bb7lbfs")
        (mozilla-patch "icecat-bug-1342170.patch"        "df7ed78b7c0a" "1kq256i66hcm2k9d37i5ws354ksv3bbglmscdjv2v5f7wg3y967v")
        (mozilla-patch "icecat-bug-1342634.patch"        "d72e56823bbb" "0c186d77lyyg0hjxw15d44rybw6yr5aw8g9m3311xfdn5wiygijb")
        (mozilla-patch "icecat-bug-1348796-pt1.patch"    "cef01720769e" "0h57372lxanjs5zw9b3vwr2x36yz9gj73swyg50aqp13j4rcbpmy")
        (mozilla-patch "icecat-bug-1348796-pt2.patch"    "7d3584b75f20" "1a4hvpsvn39832g54hsxhqs24cq8v4nd69jqskkgc1ybs09ncmr3")
        (mozilla-patch "icecat-bug-1192800.patch"        "e56b0938ea0f" "1hlbxhjzj65s6p2v6f66zdfb3gw5yx77msgq5idsv9jip2w88mpq")
        (mozilla-patch "icecat-bug-1309438.patch"        "1f30d97563c8" "0rvq729fg9j959ha9qvw5wv7r6vw70qvpy7ynifgqhgrpa749n70")
        (mozilla-patch "icecat-bug-1315332.patch"        "66495c8d9459" "0vzlx8i0cidpymm6ar07h3yk63fxf64f0b2vb0pihd72h0jzd5s9")
        (mozilla-patch "icecat-bug-1346439.patch"        "a9fcc2dc324a" "13991jijwa84yczkmc212s23w269r8b1a4yiygqgwaily29l1dc5")
        (mozilla-patch "icecat-CVE-2017-5469.patch"      "3dcc5f5c2df4" "0b36m6rgxc05h39l6wkzi6dlmq9brcigk7xjrifs4786f0z564hz")
        (mozilla-patch "icecat-CVE-2017-5430-pt06.patch" "ac0ca89b5a6b" "1646y9y2wmq8pxb081x3076dq9ana7hh5fxwbsnn17v5wqhi8gfb")
        (mozilla-patch "icecat-CVE-2017-5467.patch"      "6ed26e6c1a09" "0r1n1dwb4l8xwlns0aifyka6mldb6cy2crhh2qkap64cpj3bzl9s")
        (mozilla-patch "icecat-CVE-2017-5439.patch"      "2fde528ca7b6" "0iv0sjhnh7br0z3pcpk346wbj162ynacfk3p9309hg6kr1cd92fp")
        (mozilla-patch "icecat-CVE-2017-5440.patch"      "d88bd03d1234" "1pls63djh4w5023ag3fwjk79cpx816ilgajl5l1qlqyacl8c0v4p")
        (mozilla-patch "icecat-bug-1349987.patch"        "3282e8f6a121" "1dyc84h7v0l9gndmbiwfqk33f703zr3fv96mwbn58msdf20ma9l2")
        (mozilla-patch "icecat-CVE-2017-5434.patch"      "ee0a7b55e470" "01vs4p56p0ii0fvmg0kn7gaz6gwf2kwmv6v4pa6v68hwxx1phaag")
        (mozilla-patch "icecat-CVE-2017-5430-pt07.patch" "a4e1e04c88ee" "0q07qwzxf2iisrhknjbn1zksv2rr6qzzh6w8ibzlj1sqbdg3h852")
        (mozilla-patch "icecat-bug-1335043.patch"        "a49419f75b9c" "0pkh5yimnj3p1sd2g9vndgcn11zdx6yhpa88s8vk7fqbs8gf1fz3")
        (mozilla-patch "icecat-bug-1299500-pt01.patch"   "5fdd36b4400a" "1gdrsbf03wf9v90f1bd2sp9ac38a9lzpzfrv8l8f7gvy70acjxmb")
        (mozilla-patch "icecat-bug-1299500-pt02.patch"   "34776df5ce44" "15mlf59ii0rk97j8mlf3wz1q0w28ma5mll47dvci6cv3dziai9f1")
        (mozilla-patch "icecat-bug-1299500-pt03.patch"   "26189af0f504" "1wh1s2xd1w03zi5jdaagk6j5i8v9xsm9360xmv446wdraygkqbci")
        (mozilla-patch "icecat-bug-1299500-pt04.patch"   "798a8fe17e7b" "0vlalanffq3paa7zab003v1d377x5pvcsy8nc8fr5pdlvi622jll")
        (mozilla-patch "icecat-bug-1299500-pt05.patch"   "daf2e4f2bd5c" "1rxbjbyr1a6dxjb0qj6900g3kqjphir40pis4qcfl8q811y18jwk")
        (mozilla-patch "icecat-bug-1299500-pt06.patch"   "1187091c3134" "0r8zz4zbglxg6sl0ybz9lyq1c5w2nqp0xcn2d3rz9bvyj8byqc7m")
        (mozilla-patch "icecat-bug-1299500-pt07.patch"   "a908f2c2fe30" "1fvwy3fxfrdi9y8hmf4f9aa72i0g6s55s8cp0w22gllsl1f6gvyf")
        (mozilla-patch "icecat-bug-1299500-pt08.patch"   "e95a26cf7a42" "0pd0kcn7dqd1gy1si85as5zzc96v7vq0v8n3g3gjzms5rdnk085l")
        (mozilla-patch "icecat-bug-1299500-pt09.patch"   "d63f3b14e571" "0cqd7dal6prsrj7bn2d699idbq4fzjry9vqlbmm9dkyn5683sdy1")
        (search-patch  "icecat-bug-1299500-pt10.patch") ; Adapted for GNU IceCat, based on:
                                                        ;"08f2bc167ae8" "07d1i23ffvi74a5558bb0645vbrap6qlrpcwfyb7dm3llbfnfycy")
        (mozilla-patch "icecat-bug-1299500-pt11.patch"   "263f27805689" "0nczkvyvlpdjif3xfvj7g2mfz6j06w99x2sblqfmqq6mwrlavpq0")
        (mozilla-patch "icecat-CVE-2017-5456.patch"      "538e0b382cc2" "0wq2ywn4a7i4ypcx03hl23a4xx3lavz7y505m9kw43fx15r4070r")
        (mozilla-patch "icecat-bug-1280079.patch"        "6fbcb6a4b91e" "0qcwz9js1bwlnwyv3vhkm0hvahd043lm2bijqsmm0jy20dbslga4")
        (mozilla-patch "icecat-CVE-2017-5435.patch"      "a362e1205ba4" "127i4ybfb4dk5axp4dxcl7ag7zyx7b517myvs6q4yd8981d1jjd3")
        (mozilla-patch "icecat-bug-1341960.patch"        "b24ce30e8cfa" "0a521wn8hbaliawmxs21b8wc1gkha8iih62j4zyrfg5rm7ff6p6s")
        (mozilla-patch "icecat-CVE-2017-5454.patch"      "ac40d4a4e414" "0dnzz95vpq32bsh6hajk4hrcrxwd4w6m7kayl2iziryny86jgak2")
        (mozilla-patch "icecat-CVE-2017-5429-pt03.patch" "e469af8e9ccc" "0yn8zqakr9yw0jvysxyc8if09kqf4fr5rq4p9qdkb1p81p4dpmp5")
        (mozilla-patch "icecat-bug-1351094.patch"        "4c1383e76adc" "0wdldx88qabyhrwnnii44pggmfgqylzxy6ckwzgq86r2yipi4rsq")
        (mozilla-patch "icecat-bug-1336527.patch"        "b9f53baeabb3" "0y1l641ffbr4i85p0wc1ir6bcsy6h94bchbfc7ppxfijva4fjgvd")
        (mozilla-patch "icecat-bug-1345716.patch"        "2569af645a98" "1d6lx85ij90j6q6ixwp0h3w7y424yvkz0njsi0my727akbli5rsn")
        (mozilla-patch "icecat-bug-1208957.patch"        "2b68880d8f6b" "1pl0vkv7clyjchi9kg4995z82sr8xv7cbz1kvsg1v66md6pmp4s4")
        (mozilla-patch "icecat-bug-1208957.patch"        "bc646835442b" "0f29r5yvlb5w84nvvn6j9r9dq5314jgygjmsna3grzigpkb88gyj")
        (mozilla-patch "icecat-bug-1347944.patch"        "47cb652ddc25" "0n7871958zwndwz53xvzwjv41v5ar1vxaam8kzr5dkbqmprddimx")
        (mozilla-patch "icecat-bug-1347632.patch"        "7d8f7a52a108" "0gkbkzkz989j7pk3ia1rfvyjg3si8hnnadwkb2rw13qjxdzhx2zn")
        (mozilla-patch "icecat-CVE-2017-5438.patch"      "154c93b9435b" "00f8lr5s8h68392bb45zi0xfgqrgfkdxbzwdypp10d89784fvjvd")
        (mozilla-patch "icecat-bug-1347486.patch"        "15dbaf157058" "1mwgfnx1zsvhp0pgmc8577yw6lnf7g3ikdfj0r21fgffrn76bp69")
        (mozilla-patch "icecat-bug-1218437.patch"        "e13692bfd5f5" "10jrbs26m8l1vchw6svssrb5h8p82acrcmkx92ybvv4qbaq2bcl0")
        (mozilla-patch "icecat-bug-1345853.patch"        "5fa27dc4c4a3" "1sqqa4hir2bsnnwnlr34has62kpncmw6l9mylwprd09fxmzzgrd7")
        (mozilla-patch "icecat-CVE-2017-5429-pt04.patch" "00c051cd38c7" "1d4aa4nqyjc01mg3jvdjjp7z05c2qhdjj85dhdrd9c18gfiyv4fi")
        (mozilla-patch "icecat-bug-1349921.patch"        "c6897adc4037" "0acvcdy8awdmpz84243jzf82agrm73wqa198fjbns1p1v3s425z2")
        (mozilla-patch "icecat-bug-1338623.patch"        "edcafd42dd52" "1xqgjy7a62jsyz1b5mibrcnd7zpb4gdaas0a6z5dwfvz52j4xa16")
        (mozilla-patch "icecat-bug-1294799.patch"        "0617b074ec3d" "19h7dj44shvdzzj87svpv5q97cikxyxhiwfzf9rnqj1b7fw0xrdh")
        (mozilla-patch "icecat-bug-1345049.patch"        "88466b911357" "16pgd13mw9a0snyhq6vxmjc7kr9mikvhazkgbc6vpykwi0i0z85b")
        (mozilla-patch "icecat-bug-1339999.patch"        "b7cb8f8b0877" "0zv1kxcva699ahb9s36l4d9mlrkm0b7hmh6g1422j6iijn136vxb")
        (mozilla-patch "icecat-bug-1350868.patch"        "ddd6c44790c0" "182ii4wsz2vdd1q4dszd5hka8i2n0ghmqk7l39bd02d3zfibhhvc")
        (mozilla-patch "icecat-bug-1342360.patch"        "416681a239ef" "0ngs8xgmdhz9ag4dlrqhr0vmanqxr9q2vf16jpm3cimyc06zjxz4")
        (mozilla-patch "icecat-CVE-2017-5429-pt05.patch" "a76e626ae6db" "0zn2j8fmhp7502kx1jhrvh85vsys5x6x6gw3v4gl0h8px354v6yw")
        (mozilla-patch "icecat-CVE-2017-5429-pt06.patch" "0ce4196ab86e" "0isczy8261qz2zsdxax4j51gypz0gi39q7nfwxg88sl81kc5vym8")
        (mozilla-patch "icecat-CVE-2017-5429-pt07.patch" "39da731d80ed" "0vswnv1hqa7r8iz6y5ja7i6w3cyq5xrcd66c1q29ac6n4gn7x338")
        (mozilla-patch "icecat-CVE-2017-5430-pt08.patch" "1b148cf9c545" "0ilrib0c2c7mfycpz2hq3vrfdf6sf8lcdbfjk6r4xyxv54vh3lwk")
        (mozilla-patch "icecat-bug-1325841.patch"        "74e9f13c554f" "0glzcgjsy71y78zaccn33w8djs96i6dd3gafyzkihnkpfddd5cij")
        (mozilla-patch "icecat-CVE-2017-5445.patch"      "d7d87adfe186" "02p705si2j69ya8n5a916x58nycs07ja0sfpxrwl16f4n2plc91h")
        (mozilla-patch "icecat-bug-1346424.patch"        "5ede402f494f" "0kbx8yn8ppv7099ic6nhw32f7h42pnwk6dpvb179ilw90ah902q7")
        (mozilla-patch "icecat-CVE-2017-5430-pt09.patch" "da44c5cfab2e" "16i4dz5sfkhh3a0khrcf8zn5w20rkf4aqwygjj3cp4qhdh7wnr75")
        (mozilla-patch "icecat-CVE-2017-5430-pt10.patch" "0f966927bd55" "07pkhc6l6ylwrzgfm7i1galrvjawqqrhvhk6jcw4b30sfhi0bxq1")
        (mozilla-patch "icecat-CVE-2017-5429-pt08.patch" "f0f591f82cc0" "18p091503vpfpp4l3d7hkqj78zavv6qj1lynhlfx67zdp3xqcf8r")
        (mozilla-patch "icecat-CVE-2017-5464.patch"      "1852dc0beba4" "1zdnkrsqjfv1q2jhj4829ypiwyg78n4jv54yn3b74vwcf5zvzx8m")
        (mozilla-patch "icecat-bug-1083140.patch"        "6913f0537208" "0vaf61ryp0bzkz6l1w73alhglibbgm0jcgccxvvm43ni67pcxqbq")
        ;; The patch commented out below updates the bundled tzdata,
        ;; but we can't use it because it contains a GIT binary patch.
        ;; TODO: Consider updating the bundled tzdata, or unbundling it.
        ;; (mozilla-patch "icecat-bug-1343493.patch"       "35496444b380" "1wa79az7121xw078cgpczxavrqy0fsp4ib2nb69giha6acxcaqas")
        (mozilla-patch "icecat-CVE-2017-5430-pt11.patch" "64495dfa29db" "0m7vklnwnaf7sw97m87bm4lb9pjmlh1vvrbaf1931db8nhd6m737")
        (mozilla-patch "icecat-bug-1350783.patch"        "26cd34db3c14" "15vq3lrilg3n9j80cdjmk7xib2iq5gcx9ypq8xs7f5ya9ibasqlx")
        (mozilla-patch "icecat-CVE-2017-5429-pt09.patch" "6cd77a0d7ac0" "0kxlbl5m3gffxqrv7ky3swsbyg1ykj0wjhlfl9amsb4g8gra3zkj")
        (mozilla-patch "icecat-CVE-2017-5460-pt1.patch"  "a803be74843c" "1ywwakzjkfr714i9pfn152n86c6rp427chzdys8phdkcvp5d5p45")
        (mozilla-patch "icecat-CVE-2017-5460-pt2.patch"  "73762c1392ae" "18jy9ccqvn6l6hznvq5xsqm1pc7i81svc2grgv21wfwg9sd6zwwh")
        (mozilla-patch "icecat-bug-1337392-pt1.patch"    "4ab6d5c43036" "07pygzngssra9wnmqqrs24d6gc5kfh20fkzvpcasxh4f2hi21z9b")
        (mozilla-patch "icecat-bug-1337392-pt2.patch"    "13f2d85da9a7" "1iwfz7dp5i93bhjspy4kyz0vqrl8x8ndg5kxdyzwb1b339xim9qy")
        (mozilla-patch "icecat-CVE-2017-5429-pt10.patch" "7a30cddfcd54" "1773pijh6gi086l930cn1a0k7kvy7f3cnirfblw98sq7h9qfyy33")
        (mozilla-patch "icecat-bug-1345873-pt1.patch"    "75cea353ad78" "14cig2y7d3p033hx3096gxzlqwgddq8d0ig0g3l8p1b0xwvvyryl")
        (mozilla-patch "icecat-bug-1345873-pt2.patch"    "b08ef5a82f89" "0afz01jv850x09df85d7ycqkcdlafi4w2xi5k155lk2b92w8lhpj")
        (mozilla-patch "icecat-bug-1340163.patch"        "f3f2a995a239" "1ydsj4ja475jscalkw6ggdxgbsp5l2mam5109k0y7c98abzqraxk")
        (mozilla-patch "icecat-bug-1348174-pt1.patch"    "330904d6f0dc" "19wnp4d8481w86xkk78n7c7wrr99rq6cq3v09hd8am4n0mzwzaja")
        (mozilla-patch "icecat-bug-1348174-pt2.patch"    "c61b99483c4b" "0mjsahi8ly24415ri2sylas6g0kb8wawi095idqiq019c3r7q9cq")
        (mozilla-patch "icecat-bug-1348601.patch"        "1848bd238064" "1f5kadhn6w1rs26sdrcc3mq0zzlmmsm6ymqhshkzn57nrj6akm7b")
        (mozilla-patch "icecat-bug-1345991.patch"        "2008a4b89d9a" "07fkg9r2rxbk362ckv2h8inhd2dadvzigshm6zsjfjs2fyzp95hp")
        (mozilla-patch "icecat-bug-1344498-pt1.patch"    "9acd0103d67f" "1f0j667g05h9ydmc924cs8mzif1n7s56wixsgnyqc3s231dswhml")
        (mozilla-patch "icecat-bug-1344498-pt2.patch"    "49aadb25b1ec" "0s618m802b1x5pyqh5mj1azaxch7ggxq9503b7mwhg90vz8qw7ki")
        (mozilla-patch "icecat-bug-1344205.patch"        "34b453085dc0" "02h1bh24f9i5sm3my07m2q58cpzqfhagwwv11l9fidxcm9dmzmrd")
        (mozilla-patch "icecat-bug-1349862.patch"        "864ff0c36b6b" "1i3wmigv982x9hzkfg25jhyvkynmar69x6cj6r4g9zkk5f5ypdh5")
        (mozilla-patch "icecat-CVE-2017-5459.patch"      "5ec6fbedb420" "07flhha4rkjbry5590yr5by36ypb1k33qm3rzkbmw0vk5gyak8dp")
        (mozilla-patch "icecat-CVE-2017-5465.patch"      "2b95de78a92c" "0vvq1fz84yyw7za929x6ki25paivlwd4ng1ddkcb2bw6da6yp12k")
        (mozilla-patch "icecat-CVE-2017-5466.patch"      "a5ec5e70abf1" "1jjviyk6db8iccd7997mwmgs188fsyrzivap3ffjf8m6j4mf9cra")
        (mozilla-patch "icecat-bug-1347646.patch"        "1b50711a46ce" "1i3505zzgf0mvg2405y2gzq36xc8ic2ga8w6d3n9kqryxj0mc7bh")))
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
                      "dom/devicestorage"  ; Removed in ESR 52.1, awkward to patch out
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
                    "mozicon128.png" "default256.png")))))))))
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
