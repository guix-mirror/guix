;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
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
  #:use-module (guix hg-download)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
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
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite))

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
       #:make-flags '("CXXFLAGS=-fpermissive")
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
                    "https://anduin.linuxfromscratch.org/BLFS/mozjs/"
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
           "--without-intl-api"

           ;; Without this gnome-shell will crash at runtime.
           "--disable-jemalloc")
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

(define-public mozjs-60
  ;; No releases yet at <https://archive.mozilla.org/pub/spidermonkey/releases/>.
  ;; While we could take a snapshot of the complete mozilla-esr60 repository at
  ;; <https://treeherder.mozilla.org/#/jobs?repo=mozilla-esr60&filter-searchStr=sm-tc>,
  ;; we take the Debian version instead, because it is easier to work with.
  (package
    (inherit mozjs-38)
    (version "60.2.3-2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://salsa.debian.org/gnome-team/mozjs60.git")
                    (commit (string-append "debian/" version))))
              (file-name (git-file-name "mozjs" version))
              (sha256
               (base32
                "091w050rwzrdcbgyi934k2viyccmlqxrp13sm2mql71mabb5dai6"))))
    (arguments
     `(#:tests? #f ; FIXME: all tests pass, but then the check phase fails anyway.
       #:test-target "check-jstests"
       #:configure-flags
       '("--enable-ctypes"
         "--enable-optimize"
         "--enable-pie"
         "--enable-readline"
         "--enable-shared-js"
         "--enable-system-ffi"
         "--with-system-nspr"
         "--with-system-zlib"
         "--with-system-icu"
         "--with-intl-api"
         ;; This is important because without it gjs will segfault during the
         ;; configure phase.  With jemalloc only the standalone mozjs console
         ;; will work.
         "--disable-jemalloc")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
             ;; The configure script does not accept environment variables as
             ;; arguments.  It also must be run from a different directory,
             ;; but not the root directory either.
             (let ((out (assoc-ref outputs "out")))
               (mkdir "run-configure-from-here")
               (chdir "run-configure-from-here")
               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))
               (setenv "AUTOCONF" (string-append (assoc-ref inputs "autoconf")
                                                 "/bin/autoconf"))
               (apply invoke "../js/src/configure"
                      (cons (string-append "--prefix=" out)
                            configure-flags))
               #t)))
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             ;; This test assumes that /bin exists and contains certain
             ;; executables.
             (delete-file "js/src/tests/shell/os.js")
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("which" ,which)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)))))

(define mozilla-compare-locales
  (origin
    (method hg-fetch)
    (uri (hg-reference
          (url "https://hg.mozilla.org/l10n/compare-locales/")
          (changeset "RELEASE_3_3_0")))
    (file-name "mozilla-compare-locales")
    (sha256 (base32 "0biazbq7vbi99b99rfn4szwyx032dkpi09c9z4zs6f1br0f86iy1"))))

(define (mozilla-locale locale changeset hash-string)
  (origin
    (method hg-fetch)
    (uri (hg-reference
          (url (string-append "https://hg.mozilla.org/l10n-central/"
                              locale))
          (changeset changeset)))
    (file-name (string-append "mozilla-locale-" locale))
    (sha256 (base32 hash-string))))

(define-syntax-rule (mozilla-locales (hash-string changeset locale) ...)
  (list (mozilla-locale locale changeset hash-string)
        ...))

(define all-mozilla-locales
  (mozilla-locales
   ;;                      sha256                            changeset    locale
   ;;---------------------------------------------------------------------------
   ("0pybx6j2ycbrr1xmv0spv19sd8a1dyzcs8kf6pzn71w8y6kiagcf" "35959cf2343c" "ach")
   ("0dixmkha738w7fkx20nx95xkfyrqb9vczpy6m03qnqfvb76xaxj5" "e8dc1010f909" "af")
   ("124j09va25gwfxdzyfixrli0skxv53c7niagjyp7g3a3kcv2lbhc" "4c67f6b96a7b" "an")
   ("0flgqll3xx0ym0zj0w9j2jw3fmhs6h9m4l5da6m0bpnk5ff80r06" "34cbea5f44a5" "ar")
   ("0kdb1yqfbfz508f4p77z3p1v6fwy190vs5ipj58hgdixjgbxkqay" "b4790b27633c" "ast")
   ("1vm5xw6wg12pygswd3p0qpkaxyryah6nif5n15chb4sb42c1gqcm" "96d341bf49d4" "az")
   ("1j2qrrws51qij6haz5b77n5vzqhsxgs1ppqqw4mdrkacwvz4ciwh" "4adaede00646" "be")
   ("0ydr8f9lbd51prgcbjb5yacb461j8va0s5bqfs0rnglkvhmk6ard" "d1140972aefe" "bg")
   ("0wyw90zjp8kpd1gljng00in9wr2cf59ww6z002lgx5k4gibnqcfd" "2b3ce92c2310" "bn")
   ("0kkq621h1qdmimyrmms9g5p70m54z2ddw4cd962nqbkrnmabq9vn" "426896350893" "br")
   ("0vibhnb3cbpbgf10db04g6vm372kb9i27p0jkwif019f7qprswd8" "7463f339ce07" "bs")
   ("1l8cn2fqfvx7bswzfy9vavv8cd32ha9ygdxxdbxi64wcgw0f80bf" "dab3f05125e4" "ca")
   ("0fik17y8zyg9w82lq501ic73a53c0q9r8v4zgn9bnzgsygig8qpq" "ebb9d989275a" "cak")
   ("0sj29v6144h39wzb4rvxph3cwgvs4gzkgpr0463d3fcs6jdi0kjs" "522352780348" "cs")
   ("1nz8jlx62l69jcdi59hlk8jysm15sh3d1cxqginjmx7w351wsidm" "0791b954c333" "cy")
   ("1vc01q1vlq26xm1vm1x0119jawxxp975p9k8ashmiwncl1bvqb48" "121f5f876f4c" "da")
   ("1iqny61rg57banfbbskc2y3pr6d35fabnxmynv7vxm9jd86pndz3" "95fb3e99a2bc" "de")
   ("06v9j8acx5h8za7m65v6qm0wjbkx6vm46m8sigcp69phyg3fjc96" "90e681b74587" "dsb")
   ("0lbk90x2dxdbh63fycqxspx6jqq2zlzys6grg45balw8yyvzqrkz" "58ba4c13fd42" "el")
   ("0c2ypvy0z8g78s5158v6h9khckq1xps34r5wbiiciix289m43dgl" "8953d8c98a30" "en-CA")
   ("0z3riz3w2z6p710p90ridmwwam4snnz5mn90gd4jc1h2n7vc9mr0" "5a2b9bca3f52" "en-GB")
   ("102gn3h4ap8c3x1p7vfc88vapkfiz6264y6byhxy1axxjk3x3a77" "e87cb1c61d6e" "eo")
   ("148wj6wsx0aq7cpaxk8njj7cb1wfjr2m96dgxq6b3qcv781ldvjn" "5db15fdf95d5" "es-AR")
   ("0r11d8vzvbyz17n371byvkrnszcv1zhr7rg64i58xra3y6d7is7n" "ce2ee0e51a92" "es-CL")
   ("1xmqa8p7lpqvkgg879hfnmf6kxcpawjk8z31cdzfp1hrdlmxg8n7" "7346617620f3" "es-ES")
   ("0jxv3jh2018lnybr9mzqrffvwmr87yab9bh8lxqjj294fxw1hrxm" "687f05eb0c58" "es-MX")
   ("1rpgv7pajv4xldsn1xxsia5j72vn3x8zl5wmbzkyw56lvn9fckvf" "839a5029c496" "et")
   ("0hxp4fr3y05rkpamdb1hlmybn6d3bv3rcawjm3axbpqxbyfdpfzc" "54e8d87230c9" "eu")
   ("1y50knymnmcihw8bhvahicc386mjm6dx4hx0j6fv8sl23wzx2h9m" "c5ffca960f9c" "fa")
   ("0pj9zgi0c3yl3myhvb5afiijayp2lqzhlk630ahxn5hgjgkz0lx7" "75c000a8538d" "ff")
   ("199jg0zv7wp1cq0ik2hf84j99jx5vq2jwac0gaayvjzkh2z83jqr" "f11b2e689e7b" "fi")
   ("1vxkiwwni7470ywy99arxxa56ljkhjrhxslsp1l1l61g6gdbbspr" "49ec4f791806" "fr")
   ("0d8gwdcj0jpjv03nhjds8jrg86pg371xpylaibwri76wlyl7m54i" "faa761a5cfdc" "fy-NL")
   ("0nipbxx11a2sjadzhbi88vgknw5hzr4nqy2722q3kc1212jbi754" "5bd9466f9f9d" "ga-IE")
   ("0bay8mrm65cvmnvqpwqgzr0h3cb18ifzg5kbsbxcvdfm9xv0zi9g" "a4f6a47e82dd" "gd")
   ("00kn5w3nnpw1pxg6hhrn9asf9hgpjd6ia4038iwzcqs68w887qcy" "6c2aa01ada4e" "gl")
   ("0jj13i0ach85c975vaz2rr83mibs29ipssa7qsjkb0y2ch6xya1k" "c2d607e36cb5" "gn")
   ("1nhqbgzilcb0pr7941dxkhg079bf8v7ldikp1s5xli34wf9sabm2" "f34465d6ac1c" "gu-IN")
   ("11bh0541d996cfin1zy72l66753q94i4idgv2waf0h40h9g3z1bm" "c2ecb2762274" "he")
   ("1lslji7hh5lx5ig1xgfjh4cdindsgh3n2a7qlvzwz96gda43lvv4" "94d2bb10ee03" "hi-IN")
   ("1nx5yw00l25i3m3grdm29mi9mi7h0cy5qx02pypir754pk3hiwcc" "08df0d94edd5" "hr")
   ("19yc9dk2pwqycynmx58d1ik6x4mnyfxscgr6sg676dpl613xd7nq" "21b614e77025" "hsb")
   ("0l3z64jlx6b6ivk1b5hwqyx9hm1m5721ywnb2m4zmg3g9fw4vn7f" "f82cad7170af" "hu")
   ("1sn0dxbbf2zwcpybwcw77qb4p0hf6fxapnsnn4avaab5g55dlgz4" "d94c30920396" "hy-AM")
   ("0c92cqxrhv4317kirmhpjk7mrq44yn6fp3v6syxnhz7xwxnhshjm" "6a5f176b0626" "ia")
   ("03gyg9gqsd6pwb9nydglhm46fi2wk2p2qygmhmrf8hnav3ba7n0r" "94e4302e0f85" "id")
   ("0ky8aaps92mn56rvkwn0i13wg8av8hzi1fvr0ahqhjcpj5sfgdwq" "eca348a59888" "is")
   ("01py0sfg7nljcsgpivryrvai4p4wzbcvhgc2ymr19r579nv1vw7g" "d541a6197359" "it")
   ("0iv7vmj43njmi7g1gjzsv68ax4j502d2wnkvbfz1rx11lrqs7yw1" "a5ab3a1d95b7" "ja")
   ("1sr9ccshcw6agbj4hbnpblxixb1jz0m36glas6f9ahxmi7m605si" "63763ffa5a94" "ja-JP-mac")
   ("1as33pzcsdkynrj16dv7w642vl6plbhk650am4l5djwm64f2rgms" "aa83e8555ddc" "ka")
   ("1jwaqb5qps3i5y9iw8l2hrwa0n8lfnx1k9x0p54y3jkh6p3q3fzc" "0e0e25c26247" "kab")
   ("0cjfiwv0q5i8d7fpwb4m2w5ahq687dqjlwlicgpa443yi2zsxr4s" "33117723ceb1" "kk")
   ("0k5b56cv39aaxf9r0p9c27f3fp6yq2ffd4w6qmd0ibpl69sm629d" "aff7b2a7825e" "km")
   ("1a0zg96jgq4zn9cz0h2qwc0vv1fbkfzs5qrgabg62wqgz286jvvv" "ea91638cb1dd" "kn")
   ("0jhmv2n3yx55r6fg3myg7j1c1nhsv25g016m6lh2j023xbr723gp" "88821009b5b3" "ko")
   ("06bybgv4m4i7r9p0qld65j31vbrnljhsdj649dl93msv2r69ilif" "88685d5f07b3" "lij")
   ("1bzjf8smw6ngi88j5g3fawrg54m8fifbhshwjbgkpj7rnrpjgh4w" "e046c7ffa7d0" "lt")
   ("18dmzmpavijb7fwzffas0j5nb6byqp8h6ki7hhf6qb35diqgfq6n" "c520ef4f576c" "lv")
   ("055zf7xj5h1h8mzxj1cjzhngpcvg2p5vs2dmffsa5zfprj02d0dm" "9e43723f18ad" "mk")
   ("1496fbyyzcl075gzcd3xy50h9jyhnzgb544k1scji56yhyfajacb" "ce615fef92c1" "mr")
   ("1wc1q8ksry181pvnysqsq4dhhsg5adw5vgqafmmq5sf6i2bwn2z0" "4fefe88cfaee" "ms")
   ("0awf6mrdwdhy2yvxynssvp1zg1nc2fqbmg2d2bhjcib69zx944xw" "3987a06866fd" "my")
   ("1hycvz7i4jd40hfs5abx6sgfdkafg0jhdgqih9b7lb08aqcl35pj" "2b3b8997d9a1" "nb-NO")
   ("048z1ib46izwryyy8l1x71kq4775n7l2ilbskhsyrbxqryma13k8" "f25324281615" "ne-NP")
   ("1qkxqpyr4la9bn1bqsgc2h9869arglh9n2kwpkq6722jzdbynkz2" "04c7d32c57f6" "nl")
   ("08gnmdll55dbqj7qs63gq1kljbvg24nzns6q4m0av3sszsic0jv5" "5587520e5019" "nn-NO")
   ("1yh2p4ipj5p2b7gh0xxj0n7ndvwn5bw2773ibrh7vz932mkzhhjn" "499386b02695" "oc")
   ("0kjbnixjzv9hvyba4ll20gs76vx84pviy134fvpjp9lfjpnpib55" "31c01c325675" "pa-IN")
   ("0g61imvr4639bbydyi0kwc1il7l1gzlfij4ywx7hdcmq2x6vgb9v" "fb5f3b8dea09" "pl")
   ("13n68d7z94d7943m6fwl4kizbqm3wp82xz69vng4w9vyqlvv7d41" "9a541cbdc748" "pt-BR")
   ("1j8afvrl1afmj2zixrp91rrhag5w4xw90raca1ic6mxyih9kvdi4" "edc959a685c2" "pt-PT")
   ("0wf4a6q9nvcmam2g8ksbymjdnrz59pdr5nirfpjprfhifjmxx4nn" "d2699db715cd" "rm")
   ("1k9qalir5pbh490w1mxyq31yhy9hbxsyrrk11hwlwlgn6syp9nvp" "b5460a9017bc" "ro")
   ("1avy6wyfa5lbvy36wai6mwhhh6x1y8a0jyjk8hvjn52yfxj1gypk" "59ffa8ad047a" "ru")
   ("1cakhm4jxcw1ij0l1vhxw74hsp5wg68i3319dkdncyyc5a2s1qv9" "8b3c8a7ebdfa" "si")
   ("0s534r09bqdfvw3q17y9b1035kzzlafjv656v73mqhyz3fkffsx5" "cb39dc77980e" "sk")
   ("1s58vgmnb9aiaiaqwwcivq3iyzpzj527w2aqh2nrh6xmaw7f43sr" "17d7969b1d9a" "sl")
   ("147qm7x5z8rkf24jpqvkdlqg0fjz1l3zwnaxvkh9y2jpzv7m0x7z" "c55b0e9ff99d" "son")
   ("0nn4r1rxi8cy7x9nmn5ljd8gcsn2rjl2ma2j7waxkafkm4rs6n20" "2bb3808072da" "sq")
   ("0jsb01b94z7qbm59yaj56nb7yx7a6hpgw8v6nzwhbvmnmcsird4p" "c323c0d02d61" "sr")
   ("1n7vv9y4sk3gig56rgfd2jk8jr2160grxk31bd1wkm7fvbndd259" "4220ce487cbc" "sv-SE")
   ("06270mq7gajxfrsb8gqd25v2dac68ask5vvlh6kkkp3hrgy02vid" "6a1dbc2fe1d3" "ta")
   ("10az7pd3npa7n8wq0qywvsj2qrx9592i2wffs3rnc1fviv1i1q0y" "028505b5ecd1" "te")
   ("0yj0c3iyibb3jyypvyiyhbr9asxa48v0nq21kcf9gphi8fnyp5if" "e44d38b6a67b" "th")
   ("1qc4nvhw834lx7p304fxma0fjdr4xfj4lf69dhh6biqz795lx45p" "1e0771d95708" "tr")
   ("1g4y2yq5xp61ncy7c08j7fqqr1jc0m1hjxmbg5659wzif3b3dkg4" "e3c96943e98f" "uk")
   ("1zbi28z1c3p5il7ndixyjsv4nrimzq36zjvlmq10am38ycqr9df8" "f35da1b02691" "ur")
   ("1jrxjjj8k771y0wljqbadxdj4pasg0771jmg4l3hvpgs929i3j9g" "6fd2084b3efe" "uz")
   ("1f8sqgxzgqmw6vzjv3f49lg43q09i3j62f471864vr71815agl8n" "33b5dfd0cd63" "vi")
   ("0ssnsbxw3q5k88fa081gkn1mbqn4j7bm6vb7yvz6h44j214xkz9x" "2d87c0740715" "xh")
   ("0kd3mrvvgczhsmw4rvpxxxc71bb469ayr8r4azf7gc0y5nmlm950" "a2b6625688d3" "zh-CN")
   ("0qy1asyfplkyc89z3g3gfm7b32aka92350b3ayv9d9dcgwxmfdwz" "4d6e959a13d1" "zh-TW")))

(define (mozilla-patch file-name changeset hash)
  "Return an origin for CHANGESET from the mozilla-esr60 repository."
  (origin
    (method url-fetch)
    (uri (string-append "https://hg.mozilla.org/releases/mozilla-esr60/raw-rev/"
                        changeset))
    (sha256 (base32 hash))
    (file-name file-name)))

(define* (computed-origin-method gexp-promise hash-algo hash
                                 #:optional (name "source")
                                 #:key (system (%current-system))
                                 (guile (default-guile)))
  "Return a derivation that executes the G-expression that results
from forcing GEXP-PROMISE."
  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "computed-origin")
                      (force gexp-promise)
                      #:graft? #f       ;nothing to graft
                      #:system system
                      #:guile-for-build guile)))

(define %icecat-version "68.7.0-guix0-preview1")
(define %icecat-build-id "20200406000000") ;must be of the form YYYYMMDDhhmmss

;; 'icecat-source' is a "computed" origin that generates an IceCat tarball
;; from the corresponding upstream Firefox ESR tarball, using the 'makeicecat'
;; script from the upstream IceCat project.
(define icecat-source
  (let* ((base-version (first (string-split %icecat-version #\-)))

         (major-version (first  (string-split base-version #\.)))
         (minor-version (second (string-split base-version #\.)))
         (sub-version   (third  (string-split base-version #\.)))

         (upstream-firefox-version (string-append base-version "esr"))
         (upstream-firefox-source
          (origin
            (method url-fetch)
            (uri (string-append
                  "https://ftp.mozilla.org/pub/firefox/releases/"
                  upstream-firefox-version "/source/"
                  "firefox-" upstream-firefox-version ".source.tar.xz"))
            (sha256
             (base32
              "0w3mad0r4khcd7hfmm3xix9x6mp5yp8g8kyh18vanfnjqdls0gmd"))))

         (upstream-icecat-base-version "68.7.0") ; maybe older than base-version
         ;;(gnuzilla-commit (string-append "v" upstream-icecat-base-version))
         (gnuzilla-commit "d185c5a67506311e19440fd4b824a822ce840369")
         (gnuzilla-source
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "git://git.savannah.gnu.org/gnuzilla.git")
                  (commit gnuzilla-commit)))
            (file-name (git-file-name "gnuzilla"
                                      ;;upstream-icecat-base-version
                                      (string-take gnuzilla-commit 8)))
            (sha256
             (base32
              "09skws692qv5kbhj8bvy3prj7v0iyfz68xjck4vbfxkahldfppqx"))))

         (makeicecat-patch
          (local-file (search-patch "icecat-makeicecat.patch"))))

    (origin
      (method computed-origin-method)
      (file-name (string-append "icecat-" %icecat-version ".tar.xz"))
      (sha256 #f)
      (uri
       (delay
        (with-imported-modules '((guix build utils))
          #~(begin
              (use-modules (guix build utils))
              (let ((firefox-dir
                     (string-append "firefox-" #$base-version))
                    (icecat-dir
                     (string-append "icecat-" #$%icecat-version)))

                (mkdir "/tmp/bin")
                (set-path-environment-variable
                 "PATH" '("bin")
                 (list "/tmp"
                       #+(canonical-package bash)
                       #+(canonical-package coreutils)
                       #+(canonical-package findutils)
                       #+(canonical-package patch)
                       #+(canonical-package xz)
                       #+(canonical-package sed)
                       #+(canonical-package grep)
                       #+(canonical-package bzip2)
                       #+(canonical-package gzip)
                       #+(canonical-package tar)
                       #+rename))

                (symlink #+(file-append rename "/bin/rename")
                         "/tmp/bin/prename")

                ;; We copy the gnuzilla source directory because it is
                ;; read-only in 'gnuzilla-source', and the makeicecat script
                ;; uses "cp -a" to copy parts of it and assumes that the
                ;; copies will be writable.
                (copy-recursively #+gnuzilla-source "/tmp/gnuzilla"
                                  #:log (%make-void-port "w"))

                (with-directory-excursion "/tmp/gnuzilla"
                  (make-file-writable "makeicecat")
                  (invoke "patch" "--force" "--no-backup-if-mismatch"
                          "-p1" "--input" #+makeicecat-patch)
                  (patch-shebang "makeicecat")
                  (substitute* "makeicecat"
                    (("^FFMAJOR=(.*)" all ffmajor)
                     (unless (string=? #$major-version
                                       (string-trim-both ffmajor))
                       ;; The makeicecat script cannot be expected to work
                       ;; properly on a different version of Firefox, even if
                       ;; no errors occur during execution.
                       (error "makeicecat major version mismatch"))
                     (string-append "FFMAJOR=" #$major-version "\n"))
                    (("^FFMINOR=.*")
                     (string-append "FFMINOR=" #$minor-version "\n"))
                    (("^FFSUB=.*")
                     (string-append "FFSUB=" #$sub-version "\n"))
                    (("^DATA=.*")
                     "DATA=/tmp/gnuzilla/data\n")
                    (("/bin/sed")
                     #+(file-append (canonical-package sed) "/bin/sed"))))

                (format #t "Unpacking upstream firefox tarball...~%")
                (force-output)
                (invoke "tar" "xf" #+upstream-firefox-source)
                (rename-file firefox-dir icecat-dir)

                (with-directory-excursion icecat-dir
                  (format #t "Populating l10n directory...~%")
                  (force-output)
                  (mkdir "l10n")
                  (with-directory-excursion "l10n"
                    (for-each
                     (lambda (locale-dir)
                       (let ((locale
                              (string-drop (basename locale-dir)
                                           (+ 32  ; length of hash
                                              (string-length "-mozilla-locale-")))))
                         (format #t "  ~a~%" locale)
                         (force-output)
                         (copy-recursively locale-dir locale
                                           #:log (%make-void-port "w"))
                         (for-each make-file-writable (find-files locale))
                         (with-directory-excursion locale
                           (when (file-exists? ".hgtags")
                             (delete-file ".hgtags"))
                           (mkdir-p "browser/chrome/browser/preferences")
                           (call-with-output-file
                               "browser/chrome/browser/preferences/advanced-scripts.dtd"
                             (lambda (port) #f)))))
                     '#+all-mozilla-locales)
                    (copy-recursively #+mozilla-compare-locales
                                      "compare-locales"
                                      #:log (%make-void-port "w"))
                    (delete-file "compare-locales/.gitignore")
                    (delete-file "compare-locales/.hgignore")
                    (delete-file "compare-locales/.hgtags"))

                  (format #t "Running makeicecat script...~%")
                  (force-output)
                  (invoke "bash" "/tmp/gnuzilla/makeicecat"))

                (format #t "Packing IceCat source tarball...~%")
                (force-output)
                (invoke "tar" "cfa" #$output
                        ;; Avoid non-determinism in the archive.  We set the
                        ;; mtime of files in the archive to early 1980 because
                        ;; the build process fails if the mtime of source
                        ;; files is pre-1980, due to the creation of zip
                        ;; archives.
                        "--mtime=@315619200" ; 1980-01-02 UTC
                        "--owner=root:0"
                        "--group=root:0"
                        "--sort=name"
                        icecat-dir)

                #t))))))))

(define-public icecat
  (package
    (name "icecat")
    (version %icecat-version)
    (source icecat-source)
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bzip2" ,bzip2)
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
       ("libcanberra" ,libcanberra)
       ("libgnome" ,libgnome)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libogg" ,libogg)
       ;; ("libtheora" ,libtheora) ; wants theora-1.2, not yet released
       ("libvorbis" ,libvorbis)
       ("libxft" ,libxft)
       ("libevent" ,libevent)
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
       ;; See <https://bugs.gnu.org/32833>
       ;;   and related comments in the 'remove-bundled-libraries' phase.
       ;; UNBUNDLE-ME! ("nspr" ,nspr)
       ;; UNBUNDLE-ME! ("nss" ,nss)
       ("shared-mime-info" ,shared-mime-info)
       ("sqlite" ,sqlite)
       ("startup-notification" ,startup-notification)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     ;; The following patches are specific to the Guix packaging of IceCat,
     ;; and therefore we prefer to leave them out of 'source', which should be
     ;; a tarball suitable for compilation on any system that IceCat supports.
     ;; (Bug fixes and security fixes, however, should go in 'source').
     `(;; XXX TODO: Adapt these patches to IceCat 68.
       ;; ("icecat-avoid-bundled-libraries.patch"
       ;;  ,(search-patch "icecat-avoid-bundled-libraries.patch"))
       ;; ("icecat-use-system-graphite2+harfbuzz.patch"
       ;;  ,(search-patch "icecat-use-system-graphite2+harfbuzz.patch"))
       ;; ("icecat-use-system-media-libs.patch"
       ;;  ,(search-patch "icecat-use-system-media-libs.patch"))

       ("patch" ,(canonical-package patch))

       ("rust" ,rust)
       ("cargo" ,rust "cargo")
       ("rust-cbindgen" ,rust-cbindgen)
       ("llvm" ,llvm)
       ("clang" ,clang)
       ("perl" ,perl)
       ("node" ,node)
       ("python" ,python)
       ("python-2" ,python-2)
       ("python2-pysqlite" ,python2-pysqlite)
       ("yasm" ,yasm)
       ("nasm" ,nasm)  ; XXX FIXME: only needed on x86_64 and i686
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

       #:configure-flags `("--enable-default-toolkit=cairo-gtk3"

                           "--with-distribution-id=org.gnu"

                           ;; Do not require addons in the global app
                           ;; directory to be signed by Mozilla.
                           "--with-unsigned-addon-scopes=app"

                           "--enable-startup-notification"
                           "--enable-pulseaudio"

                           "--disable-tests"
                           "--disable-updater"
                           "--disable-crashreporter"
                           "--disable-eme"
                           "--disable-gconf"

                           ;; Building with debugging symbols takes ~5GiB, so
                           ;; disable it.
                           "--disable-debug"
                           "--disable-debug-symbols"

                           ;; Clang is needed to build Stylo, Mozilla's new
                           ;; CSS engine.  We must specify the clang paths
                           ;; manually, because otherwise the Mozilla build
                           ;; system looks in the directories returned by
                           ;; llvm-config --bindir and llvm-config --libdir,
                           ;; which return paths in the llvm package where
                           ;; clang is not found.
                           ,(string-append "--with-clang-path="
                                           (assoc-ref %build-inputs "clang")
                                           "/bin/clang")
                           ,(string-append "--with-libclang-path="
                                           (assoc-ref %build-inputs "clang")
                                           "/lib")

                           ;; Hack to work around missing
                           ;; "unofficial" branding in icecat.
                           "--enable-official-branding"

                           ;; Avoid bundled libraries.
                           "--with-system-zlib"
                           "--with-system-bz2"
                           "--with-system-jpeg"        ; must be libjpeg-turbo
                           ;; UNBUNDLE-ME! "--with-system-libevent"
                           ;; UNBUNDLE-ME! "--with-system-ogg"
                           ;; UNBUNDLE-ME! "--with-system-vorbis"
                           ;; UNBUNDLE-ME! "--with-system-theora" ; wants theora-1.2, not yet released
                           ;; UNBUNDLE-ME! "--with-system-libvpx"
                           "--with-system-icu"
                           
                           ;; See <https://bugs.gnu.org/32833>
                           ;;   and related comments in the
                           ;;   'remove-bundled-libraries' phase below.
                           ;; UNBUNDLE-ME! "--with-system-nspr"
                           ;; UNBUNDLE-ME! "--with-system-nss"
                           
                           ;; UNBUNDLE-ME! "--with-system-harfbuzz"
                           ;; UNBUNDLE-ME! "--with-system-graphite2"
                           "--enable-system-pixman"
                           "--enable-system-ffi"
                           ;; UNBUNDLE-ME! "--enable-system-sqlite"

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

       #:imported-modules ,%cargo-utils-modules ;for `generate-all-checksums'

       #:modules ((ice-9 ftw)
                  (ice-9 rdelim)
                  (ice-9 regex)
                  (ice-9 match)
                  (srfi srfi-34)
                  (srfi srfi-35)
                  (rnrs bytevectors)
                  (rnrs io ports)
                  (guix elf)
                  (guix build gremlin)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'apply-guix-specific-patches
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (let ((patch (string-append (assoc-ref (or native-inputs inputs)
                                                    "patch")
                                         "/bin/patch")))
               (for-each (match-lambda
                           ((label . file)
                            (when (and (string-prefix? "icecat-" label)
                                       (string-suffix? ".patch" label))
                              (format #t "applying '~a'...~%" file)
                              (invoke patch "--force" "--no-backup-if-mismatch"
                                      "-p1" "--input" file))))
                         (or native-inputs inputs)))
             #t))
         (add-after 'apply-guix-specific-patches 'remove-bundled-libraries
           (lambda _
             ;; Remove bundled libraries that we don't use, since they may
             ;; contain unpatched security flaws, they waste disk space and
             ;; memory, and may cause confusion.
             (for-each (lambda (file)
                         (format #t "deleting '~a'...~%" file)
                         (delete-file-recursively file))
                       '(;; FIXME: Removing the bundled icu breaks configure.
                         ;;   * The bundled icu headers are used in some places.
                         ;;   * The version number is taken from the bundled copy.
                         ;;"intl/icu"
                         ;;
                         ;; FIXME: A script from the bundled nspr is used.
                         ;;"nsprpub"
                         ;;
                         ;; FIXME: With the update to IceCat 60, using system NSS
                         ;;        broke certificate validation.  See
                         ;;        <https://bugs.gnu.org/32833>.  For now, we use
                         ;;        the bundled NSPR and NSS.  TODO: Investigate,
                         ;;        and try to unbundle these libraries again.
                         ;; UNBUNDLE-ME! "security/nss"
                         ;;
                         ;; TODO: Use more system media libraries.  See:
                         ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=517422>
                         ;;   * libtheora: esr60 wants v1.2, not yet released.
                         ;;   * soundtouch: avoiding the bundled library would
                         ;;     result in some loss of functionality.  There's
                         ;;     also an issue with exception handling
                         ;;     configuration.  It seems that this is needed in
                         ;;     some moz.build:
                         ;;       DEFINES['ST_NO_EXCEPTION_HANDLING'] = 1
                         ;;   * libopus
                         ;;   * speex
                         ;;
                         "modules/freetype2"
                         "modules/zlib"
                         ;; "media/libjpeg"  ; needed for now, because media/libjpeg/moz.build is referenced from config/external/moz.build
                         ;; UNBUNDLE-ME! "ipc/chromium/src/third_party/libevent"
                         ;; UNBUNDLE-ME! "media/libvpx"
                         ;; UNBUNDLE-ME! "media/libogg"
                         ;; UNBUNDLE-ME! "media/libvorbis"
                         ;; UNBUNDLE-ME! "media/libtheora" ; wants theora-1.2, not yet released
                         ;; UNBUNDLE-ME! "media/libtremor"
                         ;; UNBUNDLE-ME! "gfx/harfbuzz"
                         ;; UNBUNDLE-ME! "gfx/graphite2"
                         "js/src/ctypes/libffi"
                         ;; UNBUNDLE-ME! "db/sqlite3"
                         ))
             #t))
         (add-after 'remove-bundled-libraries 'link-libxul-with-libraries
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
         (add-after 'link-libxul-with-libraries 'fix-ffmpeg-runtime-linker
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((ffmpeg (assoc-ref inputs "ffmpeg"))
                    (libavcodec (string-append ffmpeg "/lib/libavcodec.so")))
               ;; Arrange to load libavcodec.so by its absolute file name.
               (substitute* "dom/media/platforms/ffmpeg/FFmpegRuntimeLinker.cpp"
                 (("libavcodec\\.so")
                  libavcodec))
               ;; Populate the sandbox read-path whitelist as needed by ffmpeg.
               (let* ((mime-info (assoc-ref inputs "shared-mime-info"))
                      (libavcodec-runpath (call-with-input-file libavcodec
                                            (compose elf-dynamic-info-runpath
                                                     elf-dynamic-info
                                                     parse-elf
                                                     get-bytevector-all)))
                      (whitelist (cons (string-append mime-info "/share/mime/")
                                       (map (lambda (dir)
                                              (string-append dir "/"))
                                            libavcodec-runpath)))
                      (whitelist-string (string-join whitelist ","))
                      (port (open-file "browser/app/profile/icecat.js" "a")))
                 (format #t "setting 'security.sandbox.content.read_path_whitelist' to '~a'~%"
                         whitelist-string)
                 (format port "~%pref(\"security.sandbox.content.read_path_whitelist\", ~S);~%"
                         whitelist-string)
                 (close-output-port port))
               #t)))
         (replace 'bootstrap
           (lambda _
             (invoke "sh" "-c" "autoconf old-configure.in > old-configure")
             ;; 'configure' must be newer than 'old-configure.in', or else the
             ;; build system will raise an alarm and abort.
             (invoke "touch" "configure")))
         (add-after 'patch-source-shebangs 'patch-cargo-checksums
           (lambda _
             (use-modules (guix build cargo-utils))
             (let ((null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
               (substitute* '("Cargo.lock" "gfx/wr/Cargo.lock")
                 (("(\"checksum .* = )\".*\"" all name)
                  (string-append name "\"" null-hash "\"")))
               (generate-all-checksums "third_party/rust"))
             #t))
         (add-before 'configure 'augment-CPLUS_INCLUDE_PATH
           (lambda* (#:key build inputs #:allow-other-keys)
             ;; Here, we add additional entries to CPLUS_INCLUDE_PATH, to work
             ;; around a problem that otherwise occurs when attempting to
             ;; build Stylo, which requires Rust and Clang.  Without these
             ;; additional entries, errors occur during the build indicating
             ;; that the <cstddef> and "c++config.h" headers cannot be found.
             ;; Note that the 'build' keyword argument contains the GNU
             ;; triplet, e.g. "x86_64-unknown-linux-gnu".
             (let ((gcc (assoc-ref inputs "gcc")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-append gcc "/include/c++" ":"
                                      gcc "/include/c++/" build)))
             #t))
         (replace 'configure
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
               (setenv "CC" "gcc")  ; apparently needed when Stylo is enabled
               (setenv "MOZ_BUILD_DATE" ,%icecat-build-id) ; avoid timestamp
               (mkdir "../build")
               (chdir "../build")
               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" flags)
               (apply invoke bash
                      (string-append srcdir "/configure")
                      flags))))
         (replace 'build
           ;; The build system often spuriously fails.  See
           ;; <https://bugs.gentoo.org/show_bug.cgi?id=680934>.  To
           ;; work around this, we try the standard 'build' phase up
           ;; to 5 times.
           (lambda args
             (let ((build (assoc-ref %standard-phases 'build)))
               (let retry ((remaining-attempts 5))
                 (if (= remaining-attempts 1)
                     (apply build args)
                     (guard (c ((invoke-error? c)
                                (format #t "~%Retrying build! (~a attempts remaining)~%~%"
                                        (- remaining-attempts 1))
                                (force-output)
                                (retry (- remaining-attempts 1))))
                       (apply build args)))))))
         (add-after 'build 'neutralise-store-references
           (lambda _
             ;; Mangle the store references to compilers & other build tools in
             ;; about:buildconfig, reducing IceCat's closure by 1 GiB on x86-64.
             (substitute*
                 "dist/bin/chrome/toolkit/content/global/buildconfig.html"
               (((format #f "(~a/)([0-9a-df-np-sv-z]{32})"
                         (regexp-quote (%store-directory)))
                 _ store hash)
                (string-append store
                               (string-take hash 8)
                               "<!-- Guix: not a runtime dependency -->"
                               (string-drop hash 8))))
             #t))
         (add-before 'configure 'install-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Install the '.desktop' file.
             (let* ((desktop-file "taskcluster/docker/icecat-snap/icecat.desktop")
                    (out          (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications")))
               (substitute* desktop-file
                 (("^Exec=icecat")     (string-append "Exec=" out "/bin/icecat"))
                 (("IceCat")           "GNU IceCat")
                 (("Icon=.*")          "Icon=icecat\n")
                 (("NewWindow")        "new-window")
                 (("NewPrivateWindow") "new-private-window"))
               (install-file desktop-file applications)
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
                    (gtk-share (string-append gtk "/share"))
                    (pulseaudio (assoc-ref inputs "pulseaudio"))
                    (pulseaudio-lib (string-append pulseaudio "/lib")))
               (wrap-program (car (find-files lib "^icecat$"))
                 `("XDG_DATA_DIRS" prefix (,gtk-share))
                 `("LD_LIBRARY_PATH" prefix (,pulseaudio-lib)))
               #t))))))
    (home-page "https://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons.  It also
features built-in privacy-protecting features.

WARNING: IceCat 68 has not yet been released by the upstream IceCat project.
This is a preview release, and does not currently meet the privacy-respecting
standards of the IceCat project.")
    (license license:mpl2.0)     ;and others, see toolkit/content/license.html
    (properties
     `((ftp-directory . "/gnu/gnuzilla")
       (cpe-name . "firefox_esr")
       (cpe-version . ,(first (string-split version #\-)))))))

(define-public firefox-decrypt
  (package
    (name "firefox-decrypt")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Unode/firefox_decrypt.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17yyyxp47z4m8hnflcq34rc1y871515kr3f1y42j1l0yx3g0il07"))))
    (build-system trivial-build-system)
    (inputs
     `(("nss" ,nss)
       ("python" ,python)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (setenv "PATH"
                 (string-append
                  (assoc-ref %build-inputs "python") "/bin"))
         (copy-file (string-append (assoc-ref %build-inputs "source")
                                   "/firefox_decrypt.py")
                    "firefox_decrypt.py")
         (substitute* "firefox_decrypt.py"
           (("/usr/bin/env python") (which "python3"))
           (("libnss3.so") (string-append (assoc-ref %build-inputs "nss")
                                          "/lib/nss/libnss3.so")))
         (install-file "firefox_decrypt.py" (string-append %output "/bin"))
         #t)))
    (home-page "https://github.com/Unode/firefox_decrypt/")
    (synopsis "Tool to extract passwords from Mozilla profiles")
    (description "Firefox Decrypt is a tool to extract passwords from
Mozilla (Firefox, Waterfox, Thunderbird, SeaMonkey) profiles.")
    (license license:gpl3+)))
