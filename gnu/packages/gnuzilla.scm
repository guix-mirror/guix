;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
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
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages mit-krb5)
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
             (modules '((guix build utils)))
             (snippet
              ;; Fix incompatibility with Perl 5.22+.
              '(substitute* '("js/src/config/milestone.pl")
                 (("defined\\(@TEMPLATE_FILE)") "@TEMPLATE_FILE")))))
    (build-system gnu-build-system)
    (native-inputs
      `(("perl" ,perl)
        ("python" ,python-2)))
    (arguments
      `(;; XXX: parallel build fails, lacking:
        ;;   mkdir -p "system_wrapper_js/"
        #:parallel-build? #f
        #:phases
          (alist-cons-before
           'configure 'chdir
           (lambda _
             (chdir "js/src"))
           (alist-replace
            'configure
            ;; configure fails if it is followed by SHELL and CONFIG_SHELL
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (setenv "SHELL" (which "sh"))
                (setenv "CONFIG_SHELL" (which "sh"))
                (zero? (system*
                        "./configure" (string-append "--prefix=" out)))))
            %standard-phases))))
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
              (snippet
               ;; Fix incompatibility with Perl 5.22+.
               '(substitute* '("js/src/config/milestone.pl")
                  (("defined\\(@TEMPLATE_FILE)") "@TEMPLATE_FILE")))))
    (arguments
     '(;; XXX: parallel build fails, lacking:
       ;;   mkdir -p "system_wrapper_js/"
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (replace
          'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (chdir "js/src")
              ;; configure fails if it is follwed by SHELL and CONFIG_SHELL
              (setenv "SHELL" (which "sh"))
              (setenv "CONFIG_SHELL" (which "sh"))
              (zero? (system* "./configure"
                              (string-append "--prefix=" out)
                              "--with-system-nspr"
                              "--enable-system-ffi"
                              "--enable-threadsafe"))))))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)))
    (propagated-inputs
     `(("nspr" ,nspr))) ; in the Requires.private field of mozjs-24.pc
    (inputs
     `(("libffi" ,libffi)
       ("zlib" ,zlib)))))

(define-public nspr
  (package
    (name "nspr")
    (version "4.12")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.mozilla.org/pub/mozilla.org/nspr/releases/v"
                   version "/src/nspr-" version ".tar.gz"))
             (sha256
              (base32
               "1pk98bmc5xzbl62q5wf2d6mryf0v95z6rsmxz27nclwiaqg0mcg0"))))
    (build-system gnu-build-system)
    (native-inputs
      `(("perl" ,perl)))
    (arguments
     `(#:tests? #f ; no check target
       #:configure-flags (list "--enable-64bit"
                               (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))
       #:phases (alist-cons-before
                 'configure 'chdir
                 (lambda _
                   (chdir "nspr"))
                 %standard-phases)))
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
    (version "3.27.1")
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
                "0sraxk26swlgl7rl742rkfp5k251v5z3lqw9k8ikin0cjfhkfdpx"))
              ;; Create nss.pc and nss-config.
              (patches (search-patches "nss-pkgconfig.patch"))))
    (build-system gnu-build-system)
    (outputs '("out" "bin"))
    (arguments
     '(#:parallel-build? #f ; failed
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
       (alist-replace
        'configure
        (lambda* (#:key system inputs #:allow-other-keys)
          (setenv "CC" "gcc")
          ;; Tells NSS to build for the 64-bit ABI if we are 64-bit system.
          (when (string-prefix? "x86_64" system)
            (setenv "USE_64" "1"))
          #t)
        (alist-replace
         'check
         (lambda _
           ;; Use 127.0.0.1 instead of $HOST.$DOMSUF as HOSTADDR for testing.
           ;; The later requires a working DNS or /etc/hosts.
           (setenv "DOMSUF" "(none)")
           (setenv "USE_IP" "TRUE")
           (setenv "IP_ADDRESS" "127.0.0.1")
           (zero? (system* "./nss/tests/all.sh")))
         (alist-replace
          'install
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

              #t))
          %standard-phases)))))
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
    (version "45.3.0-gnu1-beta")
    (source
     (origin
      (method url-fetch)
      (uri (list (string-append "mirror://gnu/gnuzilla/" version "/"
                                name "-" version ".tar.bz2")
                 ;; XXX Temporary URI for 45.3 beta release.
                 ;;     Remove when no longer needed.
                 (string-append "http://jenkins.trisquel.info/icecat/binaries/"
                                "icecat-45.3.0-gnu1.tar.bz2")))
      (sha256
       (base32
        "1hk5lwaqm8nkfm43sq521mzyrx0x3iiwvlcy62m7cq7grz9wixp6"))
      (patches
       `(,(search-patch "icecat-avoid-bundled-libraries.patch")
         ,(search-patch "icecat-binutils.patch")
         ,(mozilla-patch "icecat-CVE-2016-5250.patch"     "6711ccb0184e" "1p0s91rw1j7ib6hy9gh5p0l33rja32rfgygh29jw4wq1hxfql8rk")
         ,(mozilla-patch "icecat-CVE-2016-5257-pt1.patch" "b08f28db372e" "0fmifimavawbff700rzjibsnr16am6902gp965scvi1iy78754ia")
         ,(mozilla-patch "icecat-CVE-2016-5257-pt2.patch" "a49fd7eb57ba" "1dyh0pjdmf64sjbj1x0mdjwfispacx9yny1kx9nzpf85myryr640")
         ,(mozilla-patch "icecat-CVE-2016-5257-pt3.patch" "9707c3423a1e" "12nn8av0akza4ml1is9mfy8f7368mrkxsl32ly97r4irzh0iryh1")
         ,(mozilla-patch "icecat-CVE-2016-5257-pt4.patch" "9d632865560a" "1msp1wqv0c317wqkm82hd9ajbg4a5mcr8pld5j8cx37ccv7f21g3")
         ,(mozilla-patch "icecat-CVE-2016-5257-pt5.patch" "90697781ec9f" "1h6gcrw5ykf7r59phxqkhpfs7jsgzqn509qm43sj7mbpcvqvk5mg")
         ,(mozilla-patch "icecat-CVE-2016-5257-pt6.patch" "dd9eb81853b9" "1lyqnn40sayziych8gqd5aj7il3zajf318y8ddj8dzz3c8id5dzc")
         ,(mozilla-patch "icecat-CVE-2016-5257-pt7.patch" "d91fc76079e0" "022lhixa8dxa6ny9a4bh2di282i0lhyq0glqr9n4q3r8msfmf0ba")
         ,(mozilla-patch "icecat-CVE-2016-5257-pt8.patch" "3e37ba5e0867" "1w8lncxaayq4xndhyp1hwlv00zggbayljq6rlypb8kdwgzfpi77w")
         ,(mozilla-patch "icecat-CVE-2016-5257-pt9.patch" "3c4958a98908" "16bc6ai5qddnpm3yw24lry5s7i05xs0ycixzxiir4wmcgwcaayiy")
         ,(mozilla-patch "icecat-CVE-2016-5261.patch"     "bc2f5467b33d" "0i4b8ydmqg4blx541f56g9qrlm7gp6ih4cs7ixbds724cwk83b9f")
         ,(mozilla-patch "icecat-CVE-2016-5270.patch"     "7cd50d56bb61" "15nbp5axr59mczxgf37nli37jbw0jdknmxz7z71772pzjd2z07r9")
         ,(mozilla-patch "icecat-CVE-2016-5272.patch"     "6e43a01fee3c" "025xp1wdnz1gc5l2rsgbrwsh1pbysjiyfgz0g6rvr390r7ag1n74")
         ,(mozilla-patch "icecat-CVE-2016-5274.patch"     "10c9453407de" "1wqh6hj0dpa7r3hhlyrflcv3r3cg0xq4rb0zvhysi6l7lwb8q51r")
         ,(mozilla-patch "icecat-CVE-2016-5276.patch"     "fc818ab03f15" "1q64ipl172dcmyy9p8p3l3ljpdh1q1jg48lai0qn2xgknl7sdpks")
         ,(mozilla-patch "icecat-CVE-2016-5277.patch"     "7b668c5cec92" "1qmchn6qifgjakzac6i4hgnivy062pzgz9p1l11c1m3an1rh0isg")
         ,(mozilla-patch "icecat-CVE-2016-5278.patch"     "fd5052e343df" "1nzmzlnsz61w9aw4mjvgmlkz88aqv1w858rr0mbv07hwyrljfi84")
         ,(mozilla-patch "icecat-CVE-2016-5280.patch"     "30673bc9730b" "1qz1684v1rp86ngadcaqd68iqf472flnrnk971ryg4fbsyy8g1za")
         ,(mozilla-patch "icecat-CVE-2016-5281-pt1.patch" "61405f1fd1df" "1fgmq67arwsl1nrl133fcb5cz6jbbcfjvbv8cd8cadhapin971a7")
         ,(mozilla-patch "icecat-CVE-2016-5281-pt2.patch" "7776b6ec7b92" "1f7k8f4lk7nyghwajsxf6nb7yvzsaw3jwpa3316znsva12m548mn")
         ,(mozilla-patch "icecat-CVE-2016-5284-pt1.patch" "55e768767416" "1gg7m12njbkn1jqf2gp2y7zd9ik3xhqkjb7znczna4l438h7ki83")
         ,(mozilla-patch "icecat-CVE-2016-5284-pt2.patch" "3c42249975a5" "0gnanndkmhsp49rldv4kh0smkdcs7616v46hn567kfw8yfwqvnli")
         ,(mozilla-patch "icecat-CVE-2016-5284-pt3.patch" "126e5d574811" "13gr08bzqy23adz0ciihb7cy9wdnkcn71i77a3y5b5apm6k54mwi")
         ,(mozilla-patch "icecat-CVE-2016-5284-pt4.patch" "7b8bd7aae1a8" "0mq5gpq6ni8czfcs1rif4is0igh0054aw41ga0jqkq58g7lavkrf")
         ,(mozilla-patch "icecat-CVE-2016-5284-pt5.patch" "0799490f4e6f" "1ypv6i48nabbhcqbach8fbgz9bmnhm7q5z9dnfid44z8g54l3f33")
         ,(mozilla-patch "icecat-CVE-2016-5284-pt6.patch" "fc990e4ae8bc" "1s2cj505ajwwiy4xvn5zlmyzqpgahxmqqvis0a6xm6mjbjh02gm4")
         ,(mozilla-patch "icecat-bug-1251088.patch"       "5ffa912ed83e" "0v5lpv3c89c2d4y5acn0kyijv096axdnrvaj5ya5yypzfcaqxv24")
         ,(mozilla-patch "icecat-CVE-2016-5290-pt1.patch" "d4b5b8f3e373" "0w8cxn6ryhgxryy8k8i06yw4mknv509ns9ff1avd0hmgxa83mcdp")
         ,(mozilla-patch "icecat-CVE-2016-5290-pt2.patch" "adce603ae36d" "0mgs85cwx61bk17r7npl311l4m8yn4667wyhgjmm2ajiys6nn0yl")
         ,(mozilla-patch "icecat-CVE-2016-5290-pt3.patch" "97268426bb6f" "1z7hg796cgag025gm9pp2szz7w870s7naagdri1dlsilj797v8hr")
         ,(mozilla-patch "icecat-CVE-2016-5290-pt4.patch" "fc055950b6b8" "05iml5k3rzc653jk4imd111sh18625jxfxkcj12kjdihl0gdr4x4")
         ,(mozilla-patch "icecat-CVE-2016-5290-pt5.patch" "6f845c23565b" "01dlbnmpsnwr448fajs276y62gl03r74k1hxnwsg6ihwhnfdvn5a")
         ,(mozilla-patch "icecat-CVE-2016-5290-pt6.patch" "e5d51ca7a3c0" "0hshcz24hc6pkz5pcqxhajm17ibwrlfn1s00frfnpjjy56vacfz0")
         ,(mozilla-patch "icecat-CVE-2016-5290-pt7.patch" "61d1463acd04" "1iig4a79dxmfcr6w82mdhyl88wy7d36g5n4p24632kbabgl9j9sz")
         ,(mozilla-patch "icecat-CVE-2016-5290-pt8.patch" "8e0bab4216de" "1knq8h5ni8crfndi3p78b2pyj5lzchqw67vk0yx061r76mq4wp4r")
         ,(mozilla-patch "icecat-CVE-2016-5290-pt9.patch" "bb10104dc89e" "1flvagckrzfk7hs2xzb5j3s5i0ck57ygyskh5494xmpa2a1nnsqj")
         ,(mozilla-patch "icecat-CVE-2016-5290-pt10.patch" "7006b275b829" "0sqagm247wx94mf51fyhdkn0vf1a1qy9i829shjnhssd79srxmnn")
         ,(mozilla-patch "icecat-CVE-2016-5290-pt11.patch" "32ce7be98543" "1y2r9i4p1qpqi75mlwmibr51whz5h1vj28c6mh6ik57dxkqxbclb")
         ,(mozilla-patch "icecat-CVE-2016-5291.patch"     "3ff0c89f3b26" "1prn74aglshaj27jfrpd2s2i4slpljw4rbzjxc1qgwjvkq4m6j6f")
         ,(mozilla-patch "icecat-CVE-2016-5297.patch"     "46b07bdbf8b2" "1n8y1c5l0ms81dra7jsx8mp633ak5qvx105drvlg9hn3m0fwv1lj")
         ,(search-patch  "icecat-CVE-2016-9064.patch") ; adapted for icecat based on:
                                        ;                 "00c2b7baaa0b" "0y02yb7r62656nq9dji9dnwils2lxqasjz5byv62j1xa87r7f9hp"
         ,(mozilla-patch "icecat-CVE-2016-9066.patch"     "576f1725a57e" "1lic9d3r8r1vcniw1g3ca71390lw3dmwjsw55dp6z96hyjbcq3fd")
         ,(mozilla-patch "icecat-bug-1212939.patch"       "4a0e851f83e4" "182vx1qxrr7r2175jjf0bcixwwm1khdj4sq0c8wnsyry7p9waq5q")
         ,(mozilla-patch "icecat-bug-1168743.patch"       "a1e06af61ab3" "07llk1ba6axjasiv30vicz96k55ff4mybxy21vjxk6j0asgyjz23")
         ,(mozilla-patch "icecat-bug-1287176.patch"       "0569d5dce9db" "1d41sqbq6jc3af73dz9w19win7v7c12kw1mp7j7b1gkadq46c4y7")
         ,(mozilla-patch "icecat-bug-1263665.patch"       "a79cafee93f4" "0bn7hpm8mh8qmkpz5wiridr792irrs5sjxyvryazy2i0p4pjh62p")
         ,(mozilla-patch "icecat-bug-1304962.patch"       "f61049d5f373" "04d1na31qqq7yq4jjvhq6vzqq3f23rwac8c6fw4h5fx1pdb3l997")
         ,(mozilla-patch "icecat-bug-1314574.patch"       "46b2558ca469" "00q8676xg4wb7p371wgi04nl05j7idkb2kna9a0l08k6lks9wdhh")))
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
          ;; Delete obj-* directories, found in icecat-45.3.0-gnu1-beta
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
       ("libevent" ,libevent)
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
       #:parallel-build? #f

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

                           ;; Temporary hack to work around missing
                           ;; "unofficial" branding in
                           ;; icecat-45.3.0-gnu1-beta.
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
    (home-page "http://www.gnu.org/software/gnuzilla/")
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
