;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 ng0 <ng0@no-reply.pragmatique.xyz>
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
  #:use-module (gnu packages zip)
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
    (version "4.15")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                   version "/src/nspr-" version ".tar.gz"))
             (sha256
              (base32
               "101dksqm1z0hzd7ap82ccbxjr48s6q3xhshdl81qkj6hqdmy1p97"))))
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
    (version "3.31")
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
                "0pd643a8ns7q5az5ai3ascrw666i2kbfiyy1c9hlhw9jd8jn21g9"))
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
    (version "52.1.0-gnu1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnuzilla/"
                          (first (string-split version #\-))
                          "/" name "-" version ".tar.bz2"))
      (sha256
       (base32
        "1wr4bc5806xzyqpi6m4rjaf61za6ylpx4g0kfk95c6yw9yhg5vqb"))
      (patches
       (list
        (search-patch "icecat-avoid-bundled-libraries.patch")
        (mozilla-patch "icecat-bug-1342366.patch"        "fb43f6690a26" "1vnkjpq2bcqwzmjkgyqv8wj0ndrrsyix3qy1rsb5is6pjmi9sbaa")
        (mozilla-patch "icecat-bug-1343818.patch"        "90f870bbec29" "0mbki955f71n4yr9p0yc7kh5jwq7vs4bs4rhaazdncirbr564hm6")
        (mozilla-patch "icecat-bug-1348454.patch"        "c1cd8a02669f" "1wf0107763rw45kxkak7478vlax06ay7076cbm7ysxl7vijbr52w")
        (mozilla-patch "icecat-bug-1297111.patch"        "2553531f83b9" "0ibf59pa8czdyhc25sas6zhh2gf1k8vr8fklis2b1ms3n1qnzrha")
        (mozilla-patch "icecat-bug-1355873.patch"        "9ee455ddcd68" "0d38hi4556635g9ag805vfyffdgfsp4a8v3d9ldffdp99ypv2ixj")
        (mozilla-patch "icecat-bug-1348424-pt1.patch"    "6472c7006a73" "1fgydas23fzj49n4g43133bgjn98b2h38bii4knl7z7pm3fs2wws")
        (mozilla-patch "icecat-bug-1348424-pt2.patch"    "0d5a26b29816" "03mkghl9i83jk1axr8bvw8la6shbggkabf23if8a9vi5jdv8182x")
        (mozilla-patch "icecat-bug-1357092.patch"        "e78c943af07f" "0r830k6hja8z9rjk2nqjg8zfzr0wjcnic8rddh7jmc1inr1w3crm")
        (mozilla-patch "icecat-bug-1352093.patch"        "d7c06f2d0d13" "1ahyns5v37w91bilvb3pa8kkdzkkn3fcxmi49jr5bycjlawljrm4")
        (mozilla-patch "icecat-bug-1349595.patch"        "9071c7d4cc9c" "12128sf8s3zwv2w16kfl5jry9d6ky7hvps2006184rg23p32aj6n")
        (mozilla-patch "icecat-bug-1336979.patch"        "8bbc7b586d68" "0c13imyp1nq18in3yb1zcyi41b69svh4fn8msyj0c2lhbf8qnqcw")
        (mozilla-patch "icecat-bug-1352556.patch"        "6d80ca63ff8b" "0s893fn6v0p323lcnl4cbkg1zd7gs1p0bw76ki6cmiapkn63gs13")
        (mozilla-patch "icecat-bug-1359547.patch"        "43d7b98d8743" "1dhgy1jkvn3c4k27hbv8p16w7l09b8hd4w9zzpk8dpn4h78ncs3h")
        (mozilla-patch "icecat-CVE-2017-5031.patch"      "bd4fcdee9a06" "0xz1r342023a0bsllhjbzn6v75lpqznwacqyikb7q8i4hxkxh78a")
        (mozilla-patch "icecat-bug-1346499.patch"        "747fd6c81983" "00iscyn4wr69205ppiaghlnd32845f5lcsl303v0fcdd4d1v04vc")
        (mozilla-patch "icecat-bug-1334443-pt1.patch"    "16201e8478df" "1k91xaai25vn1svkaldnsd2s8br3fgvnk5l54k3n3lk3m5vj55hv")
        (mozilla-patch "icecat-bug-1334443-pt2.patch"    "f100e5cf3bcb" "1cgbbbnkrd3ydfw99rhnpqdp5zq65537mg8sa1s9ajxkjjd1dkwj")
        (mozilla-patch "icecat-bug-1354810.patch"        "e579ef6e8d11" "0cmrh8dl85lzjxpbni08xbs8qq15sljnpg70a7rsl0jdbgih3mdx")
        (mozilla-patch "icecat-bug-1356755.patch"        "4a3fce67b52d" "126i9nwxsb3sjwb7dvhafacq86glnhx7r7jjv0h9v21s1w0kx4wj")
        (mozilla-patch "icecat-bug-1273265.patch"        "7902fea300b8" "1jkrl8hdycsi17dd1m1vvl6gm1skhpf10q2m29zwfr8l40fd6a3q")
        (mozilla-patch "icecat-bug-1353204.patch"        "b5a21502aeff" "13rbrhvr37w95av9d4hkgi913nq0j6k2iijydylvprcn18cwibp0")
        (mozilla-patch "icecat-bug-1028195.patch"        "69a5ca2bf867" "0q8cgi6837ikpg7gsvywmzhq0i102845apcbrd6mw0205qqsnw5c")
        (mozilla-patch "icecat-bug-1347835.patch"        "bc635f45af37" "1fny422l6yc80901x6swybr8nk0in1wxfgy97ky4bdkcqlnmzpqv")
        (mozilla-patch "icecat-bug-1241066.patch"        "b922ca70cce5" "09hcf9rm7ng3vj5y267w0c9h6pqinnz8gjlkwx1337xh43mdvqjv")
        (mozilla-patch "icecat-bug-1346012.patch"        "1ce6d0652921" "163ji64a86h682frh1jq016w1mjf8g24r8cni0irsdmiihis7zxc")
        (mozilla-patch "icecat-bug-1324140.patch"        "8886f9cd5dd3" "0byabs9md8r3pc4r67sv2759427n1za0gfayln40nx47n2p52kmg")
        (mozilla-patch "icecat-bug-1342552.patch"        "ad995e90916b" "02nq9sg675p26z99nr2pykbz51hi2phf0gmrb1bjpq9pjbll7gsa")
        (mozilla-patch "icecat-bug-1355039.patch"        "4ae71415fecf" "0yfkkdkkimad9a3w734xx85lb7hrl870c8k8an7w78fq3vl3fjnd")
        (mozilla-patch "icecat-bug-1363396.patch"        "24cbb7f2e0ff" "006f0zhz5nxs72q9plwzhq4l79b47svzizvv510m5g2krsfrccza")
        (mozilla-patch "icecat-bug-1356558.patch"        "89c7fb6c5be3" "19650nmc4vn1prbpn5h06kz9d1al279xkc23v39577h4zhdrknkj")
        (mozilla-patch "icecat-bug-1337810.patch"        "0f6dd3564c76" "1sxajqh6r7fjs45xhvjwg94smpvyvplh3rdvq11d3q5m9v4kg7mz")
        (mozilla-patch "icecat-bug-1347748.patch"        "145905da25d3" "0c2q9f000snpm9x0qda2y0awrsm313iwxbv0kh33ca0kpza49a76")
        (mozilla-patch "icecat-bug-1345355.patch"        "c5012009a0b2" "0m772bgrwb8iwv2bdgx694ybg5wgbf58xg5v245x0p7gwhgwiwmr")
        (mozilla-patch "icecat-bug-1351340.patch"        "047f19a1b9a0" "0qjnhybibs3cpcba3ga4g7d4c0w716xa9jf87y2ir8yz7dw1f9vl")
        (mozilla-patch "icecat-bug-1056322.patch"        "f076a30f6c29" "0xgskjl6zmxi3v4l0f3wlas0qb2403fin5lv1hi3jf2142ihpaml")
        (mozilla-patch "icecat-bug-1355414.patch"        "28e09d4ac3e9" "06clr2kwz28nyjlj13y036x6rxwh6frdh11aq6kbm1nj6s01i9zl")
        (mozilla-patch "icecat-bug-1313977.patch"        "4c0b09f70aea" "04jq1xrlhj04n5bgh93xkbqwnh01pswfjhv81zk7i87c7xz6h92q")
        (mozilla-patch "icecat-bug-1357366.patch"        "0b855945ce34" "0va8kqlgx6qhq2qrawkcm66kqrwwpmxblyjp3c7ifplxd0j0ijaf")
        (mozilla-patch "icecat-bug-1338574-pt0.patch"    "243d7bffa4f1" "1d1v68amhnygc0g4w1afs374pjs7z5fx5inyq8idawbh4kxfncq7")
        (mozilla-patch "icecat-bug-1338574-pt1.patch"    "337398a83aa5" "1141n7dhy9rh70sww8v58cbkba74xm5i75j1sgm5qwvkhh69qr5h")
        (mozilla-patch "icecat-bug-1338574-pt2.patch"    "50e120d7ac64" "0dbcaq27vsjlh7vm30c88rlhkx8c1195rnr01six40mymh09rhym")
        (mozilla-patch "icecat-bug-1338574-pt3.patch"    "2d4da5a366e8" "1761npkpw5zsm4q8rzfrg8m1ayrf8c857iq3vdd8rbqcswzv6xq0")
        (mozilla-patch "icecat-bug-1338574-pt4.patch"    "b10d9b0c187f" "044zq9gzw4v5r3ki8dwfjg9dznai0jch29y0xrxzb2rfr6yvx0sb")
        (mozilla-patch "icecat-bug-1338574-pt5.patch"    "697713a6841c" "1m9q4rh4ny945xsx3p3f5bg1izs9q58d71la5drj31z6kvbhnsi2")
        (mozilla-patch "icecat-bug-1338574-pt6.patch"    "1d14abf37cf8" "1xyja9hjb7qfqi7kh85bw5nxkhyzw1rijjhnh5pgr5z0v718kjyc")
        (mozilla-patch "icecat-bug-1338574-pt7.patch"    "5e85bc599d0c" "1pmhs3hmhkgj6q19padcbpi5qvgnhx6ib09zpcwxr8ll6lllxhig")
        (mozilla-patch "icecat-bug-1152353.patch"        "d893dea8e7b4" "1pbayv7np6z7hlkk1dhvx3ppkni7f8n3cz8hs67l3nssw214c1ih")
        (mozilla-patch "icecat-bug-1345893.patch"        "3a747480ead1" "0sxd23y9g77mx5indjs9isxnnrkin835qrh6dn62dlvbll8lgqi2")
        (mozilla-patch "icecat-bug-1343172.patch"        "c7b064f6b93a" "1sh10j3h8cnqi3rpr70lv2yz14zhy1v9ms4f64fmrbjlz7q09j6q")
        (mozilla-patch "icecat-bug-1352348.patch"        "1d86e96610a1" "02ybn2608v57pjh8kjgnhkg157asawjk5xia59qb63m5vfvrinwv")
        (mozilla-patch "icecat-bug-1354308.patch"        "c8ba3f911eb1" "0w70b8dmvqjn1d8sphfkwnbwii8nh2q5k48clkvbhn7kpc2890mi")
        (mozilla-patch "icecat-bug-1335904.patch"        "366cdd623cfb" "0gcmld4bplaakx6d50gw223lg1jjcni7866q1f2hxm0h1r9wwd3k")
        (mozilla-patch "icecat-bug-1355340.patch"        "6b174b41fa44" "0zdgfy0zsrs3cvfkmrhxw0mrfibpnb58xp3z8fapx5ja59wmcabs")
        (mozilla-patch "icecat-bug-1360574-pt1.patch"    "237eee780619" "1iw6z762zdc42kwjvv58a2cjc0s4kzwwy7838apl7y7cq85g0jg2")
        (mozilla-patch "icecat-bug-1360574-pt2.patch"    "46a5a4aac189" "1i553f9qvav0fn5avbp8912995pqbhzbzamxxfz8gn2ik17y3xly")
        (mozilla-patch "icecat-bug-1358776.patch"        "bd35fa23f79a" "12nicgwhcn63knmlcl0c2askn9sj35bfclaab3826pkd9yq5g4p5")
        (mozilla-patch "icecat-bug-1362590.patch"        "c1314a709b41" "0klgrcyc20fig6rbm9znjpcnfsz6xnlp1v03fbvaww0riy2qm42k")
        (mozilla-patch "icecat-bug-1359859.patch"        "e38948fb79d6" "1sfyc5s9ndv6q72k8n9x0rvj4sz40k51iljrs42gwykzkjm2fx5m")
        (mozilla-patch "icecat-bug-1342057.patch"        "278bef1d7a64" "0zk18s9pnbwz9ankmc9mj4197s55j1jvax04ansqymmmc3a5ciif")
        (mozilla-patch "icecat-bug-1325513.patch"        "218e0963406f" "0wqms5nany4sx2g4p01lbam7la2dyazz87dhv5hcsf8ifxrfww11")
        (mozilla-patch "icecat-bug-1304566.patch"        "188e39630fcd" "1bfxfgj5ywx4bcf91kwyrjh5ppiv59gadx4445achyabdi639l8d")
        (mozilla-patch "icecat-bug-1356601.patch"        "8191e403fedf" "1k4zmq0923f5dc3dwbz1q0bkcbm90ldwkczym366hgwadb2305nd")
        (mozilla-patch "icecat-bug-1334097.patch"        "fe2a2c7e88cb" "1rppaivaddigwk65krn8m9f9mcdkiiv28ws9n9zj62n0rc1shyvc")
        (mozilla-patch "icecat-bug-1359051.patch"        "8d7dbe5c6587" "14zh74bbld4s0jy0a48fi9acxkc236mh9wjid3vrf72yj6bi5xnp")
        (mozilla-patch "icecat-bug-1359697.patch"        "ca2b5274549f" "1ns7v70i1hfkxqnjhf9fp0lk9095hdcllg94j3dl1nfaif4w6vbf")
        (mozilla-patch "icecat-bug-1343256.patch"        "a30dd7dd6617" "1k078176fp8vz871wirjz9d3yx9l2lfl8p75c4905n3j3zv2297q")
        (mozilla-patch "icecat-bug-1349310.patch"        "81b3ce7d37b3" "0ad0wqczy4kpggj6m3b8bzxi6ax340mik1mfawhkq89a1h2sfpxv")
        (mozilla-patch "icecat-bug-1356179.patch"        "66d8893f37f0" "0izl31lagvdv4qpb9gkjxvgpmxzw50x5bviap4l7bbnb56cv7d8p")
        (mozilla-patch "icecat-bug-1365602.patch"        "aad883966edd" "058axnrwrbvy2h9r9pb766lyky45hb92rap142sbp17yz0sxfmww")
        (mozilla-patch "icecat-bug-1355520.patch"        "7ca2d8839f7a" "1xbmpvr2x720x9ghd5wgbg6lknbnhcyqmkkfamdf97mqcyizyr21")
        (mozilla-patch "icecat-bug-1358469.patch"        "4d432638c0f9" "0qpjmwik3dryjwmgfwmkqk0rs9rb2lafb2k9fc3pkjnrq5y0l9xg")
        (mozilla-patch "icecat-bug-1356025.patch"        "f5967db0a0f3" "045wbvkm21kbm314dd6lbq2disiaf26kmsxi6brf442fd0028gwq")
        (mozilla-patch "icecat-bug-1345910.patch"        "ec6b6720e54e" "0lm15jl46mdlsds6947jsiyvhf9agb8hcdrqj2svc3kn9kzvyr2n")
        (mozilla-patch "icecat-bug-1359639.patch"        "a4f8d8a12afa" "0d7sjc21af074rvgvijj42gmpjvcb1v1zlpgb3s7ky7w6wjr35vx")
        (mozilla-patch "icecat-bug-1357090.patch"        "d07f24a72ce4" "1qbwska76b2zslb95wnx9v04znb6k9fqylr4ajyfqpwk1sr363hg")
        (mozilla-patch "icecat-bug-1364283.patch"        "a6caa7628e36" "1yv5f4h8js9bry9krcx130w6ic8rdmmq4fap6va24kfx8qflg70h")
        (mozilla-patch "icecat-bug-1237868.patch"        "41138235d4ea" "0mcj4x2kmagwf5hp8xhczf04sxm995pk1zarc9yffk84z7fcrxkj")
        (mozilla-patch "icecat-bug-1331335.patch"        "b724283e3b31" "1xbb1vcdzfpcmrmxm8ihwzslh2vz15k0k601nvyhh6vgx270h1wn")
        (mozilla-patch "icecat-bug-1367267.patch"        "4c2f4d8b693e" "1hrndhfnz0vnjnspwh5mbvgl2j8d1cs62awp04wx2w6z4l4wrmbv")
        (mozilla-patch "icecat-bug-1366595.patch"        "cce3fd607206" "1z97jw8jpfyx61jxf0j8nsplnna2c5bwihwnl9cvlc2cspp3kgp5")
        (mozilla-patch "icecat-bug-1349266.patch"        "dc4e3c64d781" "1zd666k4qpdamly3av09k602pmirjcs9l6la6ba0qq9w9vfan3g5")
        (mozilla-patch "icecat-bug-1366140.patch"        "379c348250e8" "0kvsyhi9j3bjx14ffr13dslqp8ghcgrz6ds2fikdkrrrk4syskd5")))
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
