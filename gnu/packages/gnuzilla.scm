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
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
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
  "Return an origin for CHANGESET from the mozilla-esr45 repository."
  (origin
    (method url-fetch)
    (uri (string-append "https://hg.mozilla.org/releases/mozilla-esr45/raw-rev/"
                        changeset))
    (sha256 (base32 hash))
    (file-name file-name)))

(define-public icecat
  (package
    (name "icecat")
    (version "45.7.0-gnu1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnuzilla/"
                          (first (string-split version #\-))
                          "/" name "-" version ".tar.bz2"))
      (sha256
       (base32
        "1mn73liylqzxk441f28wk326yglqs8zcwqs4zz51s8i2id2jsnv3"))
      (patches
       (list
        (search-patch "icecat-avoid-bundled-libraries.patch")
        (search-patch "icecat-binutils.patch")
        (mozilla-patch "icecat-CVE-2017-5398-pt01.patch" "1a39a54b5fea" "0k3sbf2w2yng2rpv6wl9zrm5cbsgq3pslr19xwrk8sk753as79fp")
        (mozilla-patch "icecat-CVE-2017-5402.patch"      "9828c3bb7b73" "0zgks0v9sqhwwkmry4daswvjwk6aqln6abx0iac1vwqqpg6swff6")
        (mozilla-patch "icecat-CVE-2017-5398-pt02.patch" "fa3268a1147e" "1jyd1hvp42pz5l15agmb1jhw74b38x8xnj9ih5v4pskv41bgmyg5")
        (mozilla-patch "icecat-CVE-2017-5400.patch"      "347c10e4d6d1" "1w6yrm97l477q4ripbj0nimc87p4jscabvihpncxqbq9xzc4km7p")
        (mozilla-patch "icecat-CVE-2017-5410.patch"      "fe4a2cda54ad" "0spcs93hpz13d8670jgvww80f0ynrbhwbh62fkv27lpr6wmqwqh1")
        (mozilla-patch "icecat-CVE-2017-5401.patch"      "c38f8546be5f" "1sa22w9kzgynsn4c6zh4d66byskk5kffzbvlzrhyzvqjddypf9p8")
        (mozilla-patch "icecat-CVE-2017-5398-pt03.patch" "41c80ecafa99" "0r33arr5wcgl00zgncasiyl65bmm6jy45clxnbb75nzjmsd1zx1s")
        (mozilla-patch "icecat-CVE-2017-5405.patch"      "381552c888b4" "1sjhh390cx1jqx74lxk6qd8f8ccppqgagqfhc9pnbm2m67hxvkj9")
        (mozilla-patch "icecat-CVE-2017-5407.patch"      "4ba337cdb998" "0vyknizid2z9nvl31m08c7fknizhv8dh8m54apm39k8lx77vf70p")
        (mozilla-patch "icecat-CVE-2017-5398-pt04.patch" "886650fac531" "18fsr5dmav96ja0dah7mj34n8mjpckp0bbc32zjyaj5qx0m4h5cw")
        (mozilla-patch "icecat-CVE-2017-5409.patch"      "0a22becb23cd" "19fshrq4qkj5s0mjrads6by84gy7rsq3k57gha6sw6rvx8chjaz6")
        (mozilla-patch "icecat-CVE-2017-5398-pt05.patch" "a0ead6ef09eb" "1hpsq81hhhq2a2dcq2dfndiwx93vvp5rfq0cgv6kwk2bsrq77wqq")
        (mozilla-patch "icecat-CVE-2017-5398-pt06.patch" "d3fede027d06" "1aw02p367cm0ayijdiiawlb7qhab6jwqwkakj317yd1cjnmkalwr")
        (mozilla-patch "icecat-CVE-2017-5398-pt07.patch" "ffca0f060bb4" "0qwisfp7idjj5nc1vp1afrf5lj66l2gp7rllkjmrqpz6cyfc708v")
        (mozilla-patch "icecat-CVE-2017-5398-pt08.patch" "4aa65b44dcb9" "07j6dz2b7hp1bkfvkxwgpn2wc3hqrgjgwpaz96fcpz8yadg2fssw")
        (mozilla-patch "icecat-bug-1318914.patch"        "30e2382d800f" "0w8zky5i7zc5q943x37rdvi4wbcing0q7w9fcgvnnh5li2sbrsy8")
        (mozilla-patch "icecat-CVE-2017-5408.patch"      "403d2300adc2" "06r4j48rc1fd9gvmvqy68mlqah5xfxpkvwmxk0gnqc364kpq9slk")
        (mozilla-patch "icecat-CVE-2017-5398-pt09.patch" "546ab5e99568" "05rdb9bm3n4lj0zq5a95xnwsb0vzirb9mbc2wf9xbi4xlamsgvvw")
        (mozilla-patch "icecat-bug-1311380.patch"        "ef6eeb7f8846" "1w19is5blbrwf3wlmy6wzgabih8sxp2kmkffqcj2g4jypfwyqn73")
        (mozilla-patch "icecat-CVE-2017-5398-pt10.patch" "eec69810d80e" "1r20abhw7b38igsrdpkhcfwx9i9gmcxikv4y3sjr4wkbp684f7av")
        (mozilla-patch "icecat-CVE-2017-5398-pt11.patch" "fec35ce6e68b" "1imdfrs8dxz44rhsmvydh29w5j64cij6g5ggrmhvz3386xvlil2v")
        (mozilla-patch "icecat-CVE-2017-5398-pt12.patch" "725e2a217722" "06gfhi2ich279rjnxi15fb4igimsxnv5w6bx4g91js8wbvp2r3v0")
        (mozilla-patch "icecat-CVE-2017-5398-pt13.patch" "d905a2e3a4d9" "1ibxi2s0czj47b739zmmjzbln8lpn27hdg4b17w58vhbhzkq31cx")
        (mozilla-patch "icecat-CVE-2017-5398-pt14.patch" "0032560ae945" "0md3p5cix6nzbj5m199awc9gk52pygy5s9lx3a38vh3xvd92lsbj")
        (mozilla-patch "icecat-CVE-2017-5398-pt15.patch" "91dda1e79ad8" "0b5h8fhagczfqkdgby982w6qgkw9y11zxxpdbn89rwmjpyp9nghx")
        (mozilla-patch "icecat-CVE-2017-5404.patch"      "556dd9e4a9e3" "0mbdx4xn1xs67n47ys9m42lc5ny96rz21ala848yajpdlxsz680g")
        (mozilla-patch "icecat-bug-1341137-pt1.patch"    "e86e0423dad1" "0dk1v7lcs61nx76qxcibha3ygqri15ldcvwwsrsayff9fq6k0v4y")
        (mozilla-patch "icecat-bug-1341137-pt2.patch"    "9aebee8b8cb9" "0m7p5iprhhwdv89aqqg7fla5szw6v7x2sll4ns0zg60pk4vm6izq")
        (mozilla-patch "icecat-bug-1341137-pt3.patch"    "69f3d44bdb48" "1ad7rw6nmg3c49ylqxlqqkb6cm2f0ygfzrigs6b60a2zkjqhbl0h")
        (mozilla-patch "icecat-bug-1341137-pt4.patch"    "22546e2cee64" "0gbwxa3p7qkq53hwnvxcqhx8h34qmnjdxy0h3ajik4mw76vrna9s")
        (mozilla-patch "icecat-bug-1341137-pt5.patch"    "e5083d8a855a" "1247vbpqzf007nigbxxqd6nwgr1dxd4p8cd0dr45afqh19vhlapj")
        (mozilla-patch "icecat-bug-1339122.patch"        "b0d156c7445e" "026jp5bb565yvhkmmicgygcn1lmak85p0466yl1vnjlx1rc8n724")
        (mozilla-patch "icecat-bug-1319087.patch"        "9cd44507fd65" "0mcfvby53r2150libazgrgaqrdyvl0g6cr1f01dsya3cgmc9mkcn")
        (mozilla-patch "icecat-bug-1342661.patch"        "d449995ef7d9" "1kz8k2jxvhqpjgrsj7r0kqq79036lrkfnx5pvdnsl59np9128j81") ; CVE-2017-5443
        (mozilla-patch "icecat-bug-1343261.patch"        "9b5374019b58" "0v5w50r5ys4jjy1lpks280cq8paw7wdy9mrk7szzq7nlcxz90is7") ; CVE-2017-5429 (pt1)
        (mozilla-patch "icecat-bug-1343552-pt1.patch"    "08bc7a3330e4" "1hsvffscqc4zflni866ilylgi3a13wz0n882z85xplbhwhc9lcfj") ; CVE-2017-5447 (pt1)
        (mozilla-patch "icecat-bug-1343552-pt2.patch"    "8c61ebe37f1b" "1fjsr6bzfyd1zqzz2pglwh2ckys95h21wy3j4rlwkz66057z53qq") ; CVE-2017-5447 (pt2)
        (mozilla-patch "icecat-bug-1340718.patch"        "bfa75fc20c2b" "08gksd06lwbb5ykdrk9gh2cb9bximwxhbxl3rprz64jj2bnmd3dq")
        (mozilla-patch "icecat-bug-1345461.patch"        "bcd5e51251dd" "1ms2ad8j04lz761cvdwi9rj5qv3qbjmg0zwyp3fykcf01a323ygd") ; CVE-2017-5436
        (mozilla-patch "icecat-bug-1343505.patch"        "290f10412f92" "1dsj22fkz60zfa6isnxj54clg32dwzapwh5f1vz6jsin9r67ik2p") ; CVE-2017-5446
        (mozilla-patch "icecat-bug-1346648.patch"        "9369ede30cc1" "1wrdn2aixbzifz7wyqnfi65gaiva8i746pi53z6w62lmn1hwd3ji") ; CVE-2017-5448
        (mozilla-patch "icecat-bug-1347979.patch"        "4ae2261bfab0" "1yi3jicwjy7w8f0sv5di4rx05bfpkhcwj3r6dhl5315yz4ifqy30") ; CVE-2017-5442
        (mozilla-patch "icecat-bug-1343795.patch"        "dcf468969700" "0syfq35s2r86ajmnqsxlfanvxd9ax57qkfmxpkvmk447s3mxsk08") ; CVE-2017-5441
        (mozilla-patch "icecat-bug-1347168.patch"        "5a6390274b64" "1lg5px4sncalh82y61ni9rlg50d83jmmrrvn0944x4zfrzlfaz8x") ; CVE-2017-5433
        (mozilla-patch "icecat-bug-1341096.patch"        "64158495e5ae" "1lyh8m159hhzrxj5hr0yib2sb8rkd20qxpykrf398v18s3yc08cx") ; CVE-2017-5429 (pt2)
        (mozilla-patch "icecat-bug-1346654.patch"        "f359ec604627" "0j6rzbnzlz8x9sj2r79d1zr4p89c5zq7y49xa4kn6am5ay3ws0ri") ; CVE-2017-5432
        (mozilla-patch "icecat-bug-1344461.patch"        "6f14d2ef7981" "0n24hqvjj7vxqdvxhk38swnmvcv7h7vvn5invbidhv22m0qqzs2c") ; CVE-2017-5444
        (mozilla-patch "icecat-bug-1292534.patch"        "c709d4b36145" "18cdck3fr4a1ygszb6qk07g6fi3kv6i697pjfcipvqrk358qb0hq") ; CVE-2017-5469
        (mozilla-patch "icecat-bug-1336830.patch"        "18e355831dd5" "042487xhq9zkky3pxiqy1rpy69z0j20w0jnl7kwg2j1bzfbnniip") ; CVE-2017-5439
        (mozilla-patch "icecat-bug-1336832.patch"        "ebeb0b45a84b" "17ch2aqsrnkiwbnkf6x7a1cpi8jgfjhwr6wp0bsa89s8v1dax6w4") ; CVE-2017-5440
        (mozilla-patch "icecat-bug-1349946.patch"        "ccbecbe17a45" "19vwmhvqarpzai8mcq6i7szkrp1h9m8v5lyimkmmdlmagrivjw7f") ; CVE-2017-5434
        (mozilla-patch "icecat-bug-1350683.patch"        "00ed655efad1" "0nfk9345lshim8xl29qka5man73jgxcppv3pzfrgjhk97z7h6ifq") ; CVE-2017-5435
        (mozilla-patch "icecat-bug-1342823.patch"        "609145968cfe" "1y5kw8khzxnx5cbrv4zmdd1nzha85r3cmxn3inami9fx8vikxjq8") ; CVE-2017-5429 (pt3)
        (mozilla-patch "icecat-bug-1336828.patch"        "982cfe33c513" "0amj3qx5399mrdcqakvfn5pabp562i1s87a8zd65jyqs4mwgcjap") ; CVE-2017-5438
        (mozilla-patch "icecat-bug-1348894.patch"        "eed8086d0af7" "18p567nhj7nvh740fhh3l0mqx0b7370b45005j43ll08rf2nhygl") ; CVE-2017-5429 (pt4)
        (mozilla-patch "icecat-bug-1344467.patch"        "38664f88d8f5" "0zdkmiqjr6h1bfs4qw74p5bnw74kcx9fxr4mcnshpavv2gvc6dn4") ; CVE-2017-5445
        (mozilla-patch "icecat-bug-1350844.patch"        "c071fab59d05" "16hf5c4appca8vwxl5yvl5ig5bw8cb8hl8apvknsn5rdsjwjlrpr") ; CVE-2017-5429 (pt5)
        (mozilla-patch "icecat-bug-1352926.patch"        "8fade3eebca2" "165v18545g4br1j6nbbhq2h9098iqvqpbd54zmgwwk9c973qhp3c") ; CVE-2017-5429 (pt6)
        (mozilla-patch "icecat-bug-1343642.patch"        "6172686bf59c" "0iwihvx11am28cbmgg68llf3mr4ghrclimr51vms1nq9ji767wdb") ; CVE-2017-5460
        (mozilla-patch "icecat-bug-1349340.patch"        "260b50fb6d39" "0lq08bkj1ihhwmf0yhxcnvngzym222q3n66ql9fbda4n7prlfhzl") ; CVE-2017-5429 (pt7)
        (mozilla-patch "icecat-bug-1353088.patch"        "44a90ca714b9" "1rb27bnrm9a5nnwrsxx7z36yhhz8x6lv0df98jv1128zvd373blp") ; CVE-2017-5429 (pt8)
        (mozilla-patch "icecat-bug-1347617.patch"        "e40b00161221" "0nm6jqbkb6rdahzd39gnhmy1gwawa5jbd7i80c7g1igj3x8ab50y") ; CVE-2017-5465
        (mozilla-patch "icecat-bug-1278157.patch"        "a7803c36d797" "10l8jbqlmfkvi4jj0vhkl0a9vdsh3niy5rjr26br3633nyyr4747")
        (mozilla-patch "icecat-bug-1348941.patch"        "4fe9b979b84d" "069rzn60vn90gcck2wakr6m83q0cy32x5r54ccia9nc26a01p6p5") ; CVE-2017-5429 (pt9)
        (mozilla-patch "icecat-bug-1347075.patch"        "a017569d3535" "1j7q02q2ybpfx9cr6dn9x27jva1d6dzs4pdxx2a1xmk5va03lrmq") ; CVE-2017-5464
        (mozilla-patch "icecat-bug-1333858.patch"        "413dc18f25c8" "0g1q1y22m5gds8p07nq5c8f84jc152x4sac40b17gawj1005n5v9"))) ; CVE-2017-5459
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
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+-2)
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
       ("yasm" ,yasm)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2) ; Python 3 not supported
       ("python2-pysqlite" ,python2-pysqlite)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (arguments
     `(#:tests? #f          ; no check target
       #:out-of-source? #t  ; must be built outside of the source directory

       ;; XXX: There are RUNPATH issues such as
       ;; $prefix/lib/icecat-31.6.0/plugin-container NEEDing libmozalloc.so,
       ;; which is not in its RUNPATH, but they appear to be harmless in
       ;; practice somehow.  See <http://hydra.gnu.org/build/378133>.
       #:validate-runpath? #f

       #:configure-flags '("--enable-default-toolkit=cairo-gtk2"
                           "--enable-pango"
                           "--enable-gio"
                           "--enable-svg"
                           "--enable-canvas"
                           "--enable-mathml"
                           "--enable-startup-notification"
                           "--enable-pulseaudio"
                           "--enable-gstreamer=1.0"

                           "--disable-gnomevfs"
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
          'unpack 'remove-h264parse-from-blacklist
          (lambda _
            ;; Remove h264parse from gstreamer format helper blacklist.  It
            ;; was put there to work around a bug in a pre-1.0 version of
            ;; gstreamer.  See:
            ;; https://www.mozilla.org/en-US/security/advisories/mfsa2015-47/
            (substitute* "dom/media/gstreamer/GStreamerFormatHelper.cpp"
              (("^  \"h264parse\",\n") ""))
            #t))
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
              (("^# This needs to be last")
               "OS_LIBS += [
    'GL', 'gnome-2', 'canberra', 'Xss', 'cups', 'gssapi_krb5',
    'gstreamer-1.0', 'gstapp-1.0', 'gstvideo-1.0' ]\n\n"))
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
