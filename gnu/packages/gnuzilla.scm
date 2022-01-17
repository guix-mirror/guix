;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2019, 2020 Adrian Malacoda <malacoda@monarch-pass.net>
;;; Copyright © 2020, 2021, 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Baptiste Strazzul <bstrazzull@hotmail.fr>
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
  #:use-module (gnu packages m4)
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
  #:use-module (gnu packages fonts)
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
  #:use-module (gnu packages pciutils)
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
     (list nspr)) ; in the Requires.private field of mozjs-17.0.pc
    (inputs
     (list zlib))
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
     (list libffi zlib))))

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
     (list libffi readline icu4c zlib))))

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
                 (setenv "AUTOCONF" (which "autoconf"))
                 (apply invoke "./configure"
                        (cons (string-append "--prefix=" out)
                              configure-flags))))))))
      (native-inputs
       (modify-inputs (package-native-inputs mozjs-38)
         (prepend autoconf-2.13 automake))))))

(define-public mozjs-60
  ;; No releases yet at <https://archive.mozilla.org/pub/spidermonkey/releases/>.
  ;; While we could take a snapshot of the complete mozilla-esr60 repository at
  ;; <https://treeherder.mozilla.org/#/jobs?repo=mozilla-esr60&filter-searchStr=sm-tc>,
  ;; we take the Debian version instead, because it is easier to work with.
  (package
    (inherit mozjs-38)
    (version "60.2.3-4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://salsa.debian.org/gnome-team/mozjs60.git")
                    (commit (string-append "debian/" version))))
              (file-name (git-file-name "mozjs" version))
              (sha256
               (base32
                "1xl6avsj9gkgma71p56jzs7nasc767k3n1frnmri5pad4rj94bij"))))
    (arguments
     `(#:tests? #f ; FIXME: all tests pass, but then the check phase fails anyway.
       #:test-target "check-jstests"
       #:configure-flags
       ,#~(quasiquote
           ("--enable-ctypes"
            "--enable-optimize"
            "--enable-pie"
            "--enable-readline"
            "--enable-shared-js"
            "--enable-system-ffi"
            "--with-system-nspr"
            #$@(if (%current-target-system)
                   #~(,(string-append "--with-nspr-prefix="
                                      #$(this-package-input "nspr")))
                   #~())
            "--with-system-zlib"
            "--with-system-icu"
            "--with-intl-api"
            ;; This is important because without it gjs will segfault during the
            ;; configure phase.  With jemalloc only the standalone mozjs console
            ;; will work.
            "--disable-jemalloc"
            ;; Mozilla deviates from Autotools conventions due to historical
            ;; reasons.
            #$@(if (%current-target-system)
                   #~(#$(string-append
                         "--host="
                         (nix-system->gnu-triplet (%current-system)))
                      #$(string-append "--target=" (%current-target-system)))
                   #~())))
       #:phases
       (modify-phases %standard-phases
         ;; Make sure pkg-config will be found.
         ,@(if (%current-target-system)
               `((add-before 'configure 'set-PKG-CONFIG
                   (lambda _
                     (setenv "PKG_CONFIG" ,(pkg-config-for-target)))))
               '())
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
               (setenv "AUTOCONF" (which "autoconf"))
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

(define-public mozjs-78
  (package
    (inherit mozjs-60)
    (version "78.15.0")
    (source (origin
              (method url-fetch)
              ;; TODO: Switch to IceCat source once available on ftp.gnu.org.
              (uri (string-append "https://archive.mozilla.org/pub/firefox"
                                  "/releases/" version "esr/source/firefox-"
                                  version "esr.source.tar.xz"))
              (sha256
               (base32
                "0l91cxdc5v9fps79ckb1kid4gw6v5qng1jd9zvaacwaiv628shx4"))))
    (arguments
     `(#:imported-modules ,%cargo-utils-modules ;for `generate-all-checksums'
       #:modules ((guix build cargo-utils)
                  ,@%gnu-build-system-modules)
       #:test-target "check-jstests"
       #:configure-flags
       '(;; Disable debugging symbols to save space.
         "--disable-debug"
         "--disable-debug-symbols"
         ;; This is important because without it gjs will segfault during the
         ;; configure phase.  With jemalloc only the standalone mozjs console
         ;; will work.
         "--disable-jemalloc"
         "--enable-tests"
         "--enable-hardening"
         "--enable-optimize"
         "--enable-release"
         ;; FIXME: rust-simd is disabled otherwise the build fails with
         ;; "error: `[u32; 64]` is forbidden as the type of a const generic
         ;; parameter".
         "--disable-rust-simd"
         "--enable-readline"
         "--enable-shared-js"
         "--with-system-icu"
         "--with-system-nspr"
         "--with-system-zlib"
         "--with-intl-api")
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-cargo-checksums
           (lambda _
             (let ((null-hash
                    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
               (for-each (lambda (file)
                           (format #t "patching checksums in ~a~%" file)
                           (substitute* file
                             (("^checksum = \".*\"")
                              (string-append "checksum = \"" null-hash "\""))))
                         (find-files "." "Cargo\\.lock$"))
               (for-each generate-all-checksums
                         '("js" "third_party/rust"))
               #t)))
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
               (setenv "AUTOCONF" (which "autoconf"))
               (apply invoke "../js/src/configure"
                      (cons (string-append "--prefix=" out)
                            configure-flags))
               #t)))
         (add-after 'unpack 'adjust-for-icu-68
           (lambda _
             (with-directory-excursion "js/src/tests"
               ;; The test suite expects a lightly patched ICU 67.  Since
               ;; Guix is about to switch to ICU 68, massage the tests to
               ;; work with that instead of patching ICU.  Try removing this
               ;; phase for newer versions of mozjs.

               ;; These tests look up locale names and expects to get
               ;; "GB" instead of "UK".
               (substitute* "non262/Intl/DisplayNames/language.js"
                 (("Traditionell, GB")
                  "Traditionell, UK"))
               (substitute* "non262/Intl/DisplayNames/region.js"
                 (("\"GB\": \"GB\"")
                  "\"GB\": \"UK\""))

               ;; XXX: Some localized time formats have changed, and
               ;; substitution fails for accented characters, even though
               ;; it works in the REPL(?).  Just delete these for now.
               (delete-file "non262/Intl/Date/toLocaleString_timeZone.js")
               (delete-file "non262/Intl/Date/toLocaleDateString_timeZone.js")

               ;; Similarly, these get an unexpected "A" suffix when looking
               ;; up a time in the "ar-MA-u-ca-islamicc" locale, which is
               ;; tricky to substitute.
               (delete-file "non262/Intl/DateTimeFormat/format_timeZone.js")
               (delete-file "non262/Intl/DateTimeFormat/format.js")

               ;; This file compares a generated list of ICU locale names
               ;; with actual lookups.  Some have changed slightly, i.e.
               ;; daf-Latn-ZZ -> daf-Latn-CI, so drop it for simplicity.
               (delete-file "non262/Intl/Locale/likely-subtags-generated.js"))

             #t))
         (add-before 'check 'pre-check
           (lambda _
             (with-directory-excursion "../js/src/tests"
               (substitute* "shell/os.js"
                 ;; FIXME: Why does the killed process have an exit status?
                 ((".*killed process should not have exitStatus.*")
                  ""))

               ;; XXX: Delete all tests that test time zone functionality,
               ;; because the test suite uses /etc/localtime to figure out
               ;; the offset from the hardware clock, which does not work
               ;; in the build container.  See <tests/non262/Date/shell.js>.
               (delete-file-recursively "non262/Date")
               (delete-file "non262/Intl/DateTimeFormat/tz-environment-variable.js")

               (setenv "JSTESTS_EXTRA_ARGS"
                       (string-join
                        (list
                         ;; Do not run tests marked as "random".
                         "--exclude-random"
                         ;; Exclude web platform tests.
                         "--wpt=disabled"
                         ;; Respect the daemons configured number of jobs.
                         (string-append "--worker-count="
                                        (number->string (parallel-job-count)))))))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf-2.13)
       ("automake" ,automake)
       ("llvm" ,llvm)                   ;for llvm-objdump
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-3)
       ("rust" ,rust)
       ("cargo" ,rust "cargo")))
    (inputs
     (list icu4c readline zlib))))

(define mozilla-compare-locales
  (origin
    (method hg-fetch)
    (uri (hg-reference
          (url "https://hg.mozilla.org/l10n/compare-locales/")
          (changeset "RELEASE_8_1_0")))
    (file-name "mozilla-compare-locales")
    (sha256 (base32 "00bpkaqf2ng1nn9ajyb5mli0jq58q5fm2n3yy90jy0hp4q2gbs50"))))

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
   ("0f2lkv79gqf46f74905mab3zwyz1532chxlf9d28s548p1hw6lv9" "8312cd2e0777" "ach")
   ("1v5jpmd0b04mizm9pjffp4r9q121vpq3yzkkxcgmrcwj4gc5jb2y" "21bf766c19d8" "af")
   ("1fqjiq2la543z5pbbvd9kfs0wdc2phmqjbxascfsak854qy1z9f9" "34b6a4f0790d" "an")
   ("0pl6j99xnali25glyr3g9fmj67v9vqmhd9k74i97f8q5n4xmv3ym" "513c3e8dac97" "ar")
   ("0d1fbk9jcai19pi8b7i4y4r0gscqi1inr9dbahd6h0xbxfwc0zif" "19ca0cd0d1bf" "ast")
   ("08f9b63wxxy28zaimjn1ab9w51bvrarc3pp75s7v6kzm5bk1jwic" "44aa0dad7964" "az")
   ("15pksy9bgaxcbcnvvp8jwqnqxvvhq9vnljpai6jlh52yyrsglbwi" "5e4499355167" "be")
   ("0gdg84jp1i4il4nc6gwaswdhc4ljbb9inyip7vhkng09v3pmwm0h" "98d006107851" "bg")
   ("0i6vl0ag74phj4l38cvds1ds3jjdal1c8d7hy6pf4aqrp4ai32mh" "2ca52cbb680a" "bn")
   ("16yavargwq8rhipzpmrrvyh68g3a6disz9g5m8xbhxvixhcsi5fr" "ce92556a0a90" "br")
   ("1512dzp394pj66i13nsz76qh6fmjpz7r7fmvk8d0h7pjk0d6n0dy" "1c79dabdb120" "bs")
   ("19wg0kbr2ihbn4lscsxg9agz04r8bsih4692vfgb6nyn1z8vx2i0" "a22912c01617" "ca")
   ("1ixpzjb7caq1d9c7c044rxg3ymxs3hjw120kq85v004jrrb4d9c6" "34bf3fd631d4" "ca-valencia")
   ("1abizdmd1c56syni1aanwwrfvmgzz25fmimbj2324bcw801ma9h0" "e2e4ba2c37bc" "cak")
   ("0wgrg2wiz1jcj52nd4zl4shfkjhbngfj6p1gw1ywj266hk8g6pvw" "a60792bff6ff" "cs")
   ("132f29111jd2z9yxpakkyri80mi2ggnik6zxaxqrp4vlcrd1fxd2" "66369fcad8c7" "cy")
   ("04lpic3cxdj1imcwjkbk81avp9dpa1c9b1zcchrr1a4vyy4yfjpy" "af198d43f7d6" "da")
   ("1h0rkwlsqls8k1qi1y68plw6xk0z2c5xc2y4nqywiirl71yz7fi5" "816fcef20c05" "de")
   ("11lzdyl1h42zb1x2yxiv3r0wih8jy1gr0gzs5d1isdq53sa9cqnq" "962e5a544415" "dsb")
   ("146jgrhl41k6zbxg58hxh1s088padsjz7ny8nm59i9i8d00jpv0a" "e7c49fd88463" "el")
   ("1qppr44hdnrb1z1igcd1p0w5sy9vpihzilkl2w44y0v0v9rigppv" "5a0e35b08a48" "en-CA")
   ("1v2irfms24cx9g5s6r1glmp2c000wi91axyca1pn37s7c01r0n4b" "72f8c5d8305d" "en-GB")
   ("1y0rbbmax1w6jy4jacy27xgy4aqnd43izysw8qjwm0qhhfjvf3xh" "40994fca693d" "eo")
   ("0kmjvinqpb3y81mqda1qq8k202aa36as9z1z775745bx6sbvkggz" "6cf3c836995d" "es-AR")
   ("06rvd1z3l0r7hwnsnw90i0f5j2ysbv6wd3cl64g0bgifmwjk75hi" "8727dda3935e" "es-CL")
   ("0gzw1rn9nzpgcrrc10indnbqqpax87azczrfxv8mn5n56j734hc0" "5351aba49895" "es-ES")
   ("0bnmgisxfkb6rb12avhipbsj1yr0dyv56qjj9471gnc2ppq7k3c9" "dd834a4af402" "es-MX")
   ("1951975h9w813qxanbk3frjz7c8knzx5rvq9i82j9i6x6a3fh6fi" "1f2150796079" "et")
   ("1vkb5rd0k6vh9dbll3fhyzg8rfpkxxkd2rxwf66b8l3hg4fqd7f2" "2911663f4a7c" "eu")
   ("1cj9zblfp94h86m7zd762bfmcfz0yxc1q4ra35s4wnnlqajarzwj" "04ec3c3bbe92" "fa")
   ("1ininsyzaj7xd8ppmklm3zglgw1i0nhdy43iiyva32hb592zxy4m" "0cf866f9bc4e" "ff")
   ("1hc308d71iygarrpliv7pxjz49gxwqg10d0pv8j4sbb2pw39bprl" "04a600fc54a4" "fi")
   ("02nl54db9130rg59wmrrnh0z726fg5ir1njfcnhxagb4g5b7s69f" "64a3576df276" "fr")
   ("1qbby23qlkxjz3vwlk8pd2w1w4cxdff8cq7j0ipk44mijkxkry0c" "c84177dfa254" "fy-NL")
   ("1x23pg36ld5qnrmdn149rkl85ia8lmiqbzcac2bm0iqprnjdszii" "31590cffac56" "ga-IE")
   ("005fpyr1lvw23dja3yrzx4y8wdih7vx6ljjpisf4pd3k5zai4x81" "ba4f274ac4a1" "gd")
   ("0kdlb5q74n0fl5fxfy873s392kry69dmdl8b4gvqdyh5sy66vsqm" "56b41bdcd401" "gl")
   ("0v8r8b0nrk1l4xipzhq16klmh0pinzcrma0s0gyhgzqqli6z7aym" "59e2afb65c08" "gn")
   ("1grb1lq1f9p5jxgh9v78jl1wswxmsqxkcssm35l30k6y24pglr0m" "33b4409fb615" "gu-IN")
   ("172fg55y8l90ix4c8s5x7f8mg6rc7779p6l852cnnkfqjgqa5a5f" "92f9f8238189" "he")
   ("0i52nmg0yv3y1y3rkinxcyh51mgjksk7c3jlc8014801m3zvyxj7" "00090f7a2a3f" "hi-IN")
   ("074q8n00yq54kzk632dwbs64r0sygvybbvwpd6hbvfpn38lz8hxf" "6e4a3fdc01f3" "hr")
   ("1jxiz8zyxbpnwgk5xkwfxlr5f1zfyc88jcsajy8wcaifdkld3cwn" "7fd9bc25e49d" "hsb")
   ("0ca11pmkzyd9ccrnrbmk0jrpcb03k88v4zivxblnxj4w03g0hyhq" "ce3b378ce950" "hu")
   ("1yszljbh8f7w6lckfw22jwximy3yhis4430lfynfb8lh9nm0fw59" "c91a4a3e8821" "hy-AM")
   ("0hhl8b8szqlhjdxhsa81zmgr303kr0jx5d6rzbi840qwf01h12sz" "4f235acc839e" "ia")
   ("063yfk3nz3kignbri7r9sr3jwr5p9yyqc6sckgqs94hx6lvc355p" "fface86a34e4" "id")
   ("1qszxkgliall4haaq6v0xayxikq43ddcdsn1d4m1mwyp7gdpbry5" "5978ae767b44" "is")
   ("1fsfz9iz5pdd5r9ckdal6grvddchsc4r3r8gn4w6f2bja6vpbs05" "0c7d2a0e1304" "it")
   ("1phkw0apiicgbw2nq3g392xncw1v2c4yac595h3nchs4q6rp9pd9" "1c923f3e88b9" "ja")
   ("1y4gc4l6g72nphhsw4aqhcl80a7wi9qdy61h66c6jc6h14i7278d" "66f700940cfa" "ja-JP-mac")
   ("0iqwj0y19zqcdclnn92z490s1g9vh2qr9gmkyfnpsz69llxidx1j" "49b4a7c6bb2c" "ka")
   ("111nm2khff0zxnqqfc68rbi3j0b1nx5r4xai7b8yymyv1i0qbsxd" "4854facbd60b" "kab")
   ("12p7lg2p6pcqdlb4bqbz5isxilw2r9vb8344sh8zrvv9cb9jq04f" "d2fda8a1b287" "kk")
   ("142vpw9npqgc8b5zcg5cy2lh07s4mg8xfbaymg44fb4j3s6agw88" "3a5a1cae9b93" "km")
   ("00v1sd2byj2ksxw1020z142cdlxa7ri2v1rzjrhay9l465bj3k8x" "6ed122b33cac" "kn")
   ("1s07yhwzpxj0l5vj5lzbvgw115sjx1g2zgbqca5wln95dds39npl" "49bc07645a47" "ko")
   ("1b4brfww0w7x1h3ff19i6xi8xq2cb1hxysilira6yq4rb6vhlmly" "93f04df12616" "lij")
   ("1i30my0bbgm9z02rxxi7x4vc69bs3bnjs9l2q6jd8xvs1ga3vc7n" "e1f4e0d87509" "lt")
   ("1bjh4xlx6562hxq527cqcn4b5295dsdpwn4c4y5ci902nlh6fc8l" "daea4ba4e7f4" "lv")
   ("0vig42slcx6bhpisyc1bnyklr95lbv7vqd1ckiywp5c5qzfa9afi" "0c4ca0f4fc17" "mk")
   ("10qfnkqi2snmgvm4vcrmp2489klchv3hn2c4rmkdhi5bilfk42jx" "9be8d5951976" "mr")
   ("1bkn8ds3fyz710ck4gg4g5vpv23bi573ssm9xbiyl8y59pvbig9b" "0f44d0bcfda2" "ms")
   ("1j9af3axbijn0s8y96310mvgaq5a36m0r3ij550jv5bv52862crj" "ccbf0c4355d6" "my")
   ("0lspnp9jn3rrgz3djnmc0qrb6ymigvqnr3fb0jbhmf83r4yfs8z1" "a6cf2c6e3594" "nb-NO")
   ("1xp2884m9gqm9gaakc747bd9j55wssrh3qsf4y4k02ijh08sfmwb" "c1636a1a7507" "ne-NP")
   ("0y1fxp9pxia6x2wxdharh2ynvzdqcmzpya1gdb89pcv5qyrzqgcl" "60ed1c2397a6" "nl")
   ("1nbp924p7fp2c76ym71wp34vk96z89i4g2rfgpsi54cgv42a6vgq" "a7f9a3d35875" "nn-NO")
   ("1yrmhyvb6skypcyvi6j3py51sdkq9vn35zkryx6rdp8ygqwm1va7" "9ac29828960e" "oc")
   ("0rn2414ji1icar627arf5rjwl9r9vxhznbfsyxgrmrf0p6dp72fv" "87720cc32205" "pa-IN")
   ("09lam6yaprc8zyisq0pcdj4afpg930c2x3x47gdxnqfyhmcfygs7" "56b2c592ffba" "pl")
   ("0mbc1a8wvjs3k928qh0k800d230251i0rw0myhdpc6mk3xjna7i5" "89f1b173c9b1" "pt-BR")
   ("1v1qa5slllz78222rlqr4b8k7c8dapmx8d7s5nb90wdffic0zcqy" "27adc5dad781" "pt-PT")
   ("04k6h5d7c740lz1hx1vavj6p0fmhgn9mm3nwvk4p9iccy5nw5pmf" "83e2e4833fc3" "rm")
   ("0f0hz792wr17mgi6n190dliqx67479aqg3y2ak0jg0fiz2c8qr7p" "69cdc288966f" "ro")
   ("17yngj0v30687m7fa1ls0g86vjg8jzxs7bnsca91jpxf3ij44xp6" "03046775600f" "ru")
   ("1fvz23nphs9i4gdkx9fcy5ahdk2f879281pchclwi0qlciq7digr" "61ea93a50fff" "sco")
   ("0yc1iwqbnpnf1i7yz3zqrx6g8pinc7l53pvbczywfkq3rz6wmmf5" "2b134d24c046" "si")
   ("0kizljksn2vn4yw7qlv7c8h2m75f7n9ddg6n6ag3hwxxip83hn4y" "6d69c24dc8db" "sk")
   ("1ll936b18wy464623jc5smf3c6dpimh0qwn28a991h7zcw31n3kp" "29e195d1d160" "sl")
   ("14w9n994z9gf0wx7vrqisyxkngvhmmnayx8r0vwiq8k7jy81sxlm" "60a3b235fe1d" "son")
   ("0z7bcfvq6x6dxf4d4bbjsg36r5npkr89087b0j360ljampk6c2fn" "8588cbc89847" "sq")
   ("1fcq9g89lhzckzr2qb6x0w0z6q486n9jdqfi6h70dx8b069b3jfj" "198f7e89f10d" "sr")
   ("0ap3m1sbibnaj9s2l139m7l8cc8s2ky4jlxwgzbxdzvqxfz7n954" "b098e2ebc049" "sv-SE")
   ("0k93gnwfs5mahb9vz1c65ddcbkav0cwhxnrww3qk7gcmn7q88gqk" "25951b964d5b" "szl")
   ("0b0v5pcwvhvhg8vlzy26g3j0m1r8svrpq07hq0q7lxldwlb48phz" "94659c83c9d8" "ta")
   ("1v35cl6m3ikzsriq6zkjzjs3p9bhl8zmbp2gl14mga1f1zy4kcah" "eb59646a5d36" "te")
   ("0y3r30mjgxngjraqmf5cm00kgqx2pvhbvy8sfxb3fhypv9vda3yr" "10d257301a2d" "th")
   ("1inyqk495py20jwjvs4yl0n5ncf3hhb896bs1lwfsgpvbvr5hbq1" "9be6adca0e51" "tl")
   ("0vsirjs655cia9n8dys2r84bvg303hcxkk2w6a9j3dkbifq5ymk6" "51354c936bf6" "tr")
   ("050x882c0ywil290k4g861njw70ffw2y55dqk3w0kffi972mm909" "d7510f2d01dc" "trs")
   ("001ykwxkwibavbi1k42b1hmysb3gmrwcs68zmw96m1vc80p29nz1" "4d1fa878042b" "uk")
   ("0lsz6jlmmsqdg47pdx8b1djjvcavi0wlyr0mkpyhjgpginag21zx" "ad3035f28183" "ur")
   ("0d3ggm3q76pkpg4n3lz2ji4pbb47n885byqxbp7sk6n9vlgzir7l" "8ea7a5a051db" "uz")
   ("0v7d8y6xsr6yf2s1sjxnsjmw0hdnj91f0w2da6spkifb52lmbv6v" "67fe2acb306f" "vi")
   ("0j8l09vzqlw4zlkyp6wkh44c5gr1xgbcmnawks1zj7xz02ambwii" "abc22dfb6d84" "xh")
   ("1c01pbyswixkjg42714fvfy33b09mpdbf4d8f8kcs88c86jpf07z" "d6d6f9bb6113" "zh-CN")
   ("0yq8fqdz862pnlbvfsqfc152rhpsqgncvp8bqkd653mdfv4qkwr4" "0cbdc5f1a048" "zh-TW")))

;; XXXX: Workaround 'snippet' limitations.
(define computed-origin-method (@@ (guix packages) computed-origin-method))

(define %icecat-version "91.5.0-guix0-preview1")
(define %icecat-build-id "20220111000000") ;must be of the form YYYYMMDDhhmmss

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
              "04y8nj1f065b3dn354f1ns3cm9xp4kljr5ippvmfdqr7cb4xjp7l"))))

         (upstream-icecat-base-version "91.5.0") ; maybe older than base-version
         ;;(gnuzilla-commit (string-append "v" upstream-icecat-base-version))
         (gnuzilla-commit "c0a504578cb694522c65bb6c36396df8142d4a2a")
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
              "016g8vdr6w6six4f705cmbdrfknmy4bk1qjjrvsdpah4bf6c2s2c"))))

         ;; 'search-patch' returns either a valid file name or #f, so wrap it
         ;; in 'assume-valid-file-name' to avoid 'local-file' warnings.
         (gnuzilla-fixes-patch
          (local-file (assume-valid-file-name
                       (search-patch "icecat-use-older-reveal-hidden-html.patch"))))
         (makeicecat-patch
          (local-file (assume-valid-file-name
                       (search-patch "icecat-makeicecat.patch")))))

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

                (set-path-environment-variable
                 "PATH" '("bin")
                 (list #+rename
                       #+python
                       #+(canonical-package bash)
                       #+(canonical-package coreutils)
                       #+(canonical-package findutils)
                       #+(canonical-package patch)
                       #+(canonical-package xz)
                       #+(canonical-package sed)
                       #+(canonical-package grep)
                       #+(canonical-package bzip2)
                       #+(canonical-package gzip)
                       #+(canonical-package tar)))

                (set-path-environment-variable
                 "PYTHONPATH"
                 (list #+(format #f "lib/python~a/site-packages"
                                 (version-major+minor
                                  (package-version python))))
                 '#+(cons python-jsonschema
                          (map second
                               (package-transitive-propagated-inputs
                                python-jsonschema))))

                ;; Needed by the 'makeicecat' script.
                (setenv "RENAME_CMD" "rename")

                ;; We copy the gnuzilla source directory because it is
                ;; read-only in 'gnuzilla-source', and the makeicecat script
                ;; uses "cp -a" to copy parts of it and assumes that the
                ;; copies will be writable.
                (copy-recursively #+gnuzilla-source "/tmp/gnuzilla"
                                  #:log (%make-void-port "w"))

                (with-directory-excursion "/tmp/gnuzilla"
                  (make-file-writable "makeicecat")
                  (invoke "patch" "--force" "--no-backup-if-mismatch"
                          "-p1" "--input" #+gnuzilla-fixes-patch)
                  (invoke "patch" "--force" "--no-backup-if-mismatch"
                          "-p1" "--input" #+makeicecat-patch)
                  (patch-shebang "makeicecat")
                  (substitute* "makeicecat"
                    (("^readonly FFMAJOR=(.*)" all ffmajor)
                     (unless (string=? #$major-version
                                       (string-trim-both ffmajor))
                       ;; The makeicecat script cannot be expected to work
                       ;; properly on a different version of Firefox, even if
                       ;; no errors occur during execution.
                       (error "makeicecat major version mismatch"))
                     (string-append "readonly FFMAJOR=" #$major-version "\n"))
                    (("^readonly FFMINOR=.*")
                     (string-append "readonly FFMINOR=" #$minor-version "\n"))
                    (("^readonly FFSUB=.*")
                     (string-append "readonly FFSUB=" #$sub-version "\n"))
                    (("^readonly DATADIR=.*")
                     "readonly DATADIR=/tmp/gnuzilla/data\n")
                    (("^readonly SOURCEDIR=.*")
                     (string-append "readonly SOURCEDIR=" icecat-dir "\n"))
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
                    (delete-file "compare-locales/.hgtags")))

                (format #t "Running makeicecat script...~%")
                (force-output)
                (invoke "bash" "/tmp/gnuzilla/makeicecat")

                (format #t "Packing IceCat source tarball...~%")
                (force-output)
                (setenv "XZ_DEFAULTS" (string-join (%xz-parallel-args)))
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
                        icecat-dir)))))))))

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
       ;; UNBUNDLE-ME! ("graphite2" ,graphite2)
       ("cairo" ,cairo)
       ("pango" ,pango)
       ("freetype" ,freetype)
       ("font-dejavu" ,font-dejavu)
       ;; UNBUNDLE-ME! ("harfbuzz" ,harfbuzz)
       ("libcanberra" ,libcanberra)
       ("libgnome" ,libgnome)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng-apng" ,libpng-apng)
       ;; UNBUNDLE-ME! ("libogg" ,libogg)
       ;; UNBUNDLE-ME! ("libtheora" ,libtheora) ; wants theora-1.2, not yet released
       ;; UNBUNDLE-ME! ("libvorbis" ,libvorbis)
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
       ("pciutils" ,pciutils)
       ("mit-krb5" ,mit-krb5)
       ("hunspell" ,hunspell)
       ("libnotify" ,libnotify)
       ;; See <https://bugs.gnu.org/32833>
       ;;   and related comments in the 'remove-bundled-libraries' phase.
       ;; UNBUNDLE-ME! ("nspr" ,nspr)
       ;; UNBUNDLE-ME! ("nss" ,nss)
       ("shared-mime-info" ,shared-mime-info)
       ("sqlite" ,sqlite)
       ("eudev" ,eudev)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     ;; The following patches are specific to the Guix packaging of IceCat,
     ;; and therefore we prefer to leave them out of 'source', which should be
     ;; a tarball suitable for compilation on any system that IceCat supports.
     ;; (Bug fixes and security fixes, however, should go in 'source').
     `(;; XXX TODO: Adapt these patches to IceCat 91.
       ;; ("icecat-avoid-bundled-libraries.patch"
       ;;  ,(search-patch "icecat-avoid-bundled-libraries.patch"))
       ;; ("icecat-use-system-graphite2+harfbuzz.patch"
       ;;  ,(search-patch "icecat-use-system-graphite2+harfbuzz.patch"))
       ;; ("icecat-use-system-media-libs.patch"
       ;;  ,(search-patch "icecat-use-system-media-libs.patch"))

       ("patch" ,(canonical-package patch))

       ("rust" ,rust)
       ("cargo" ,rust "cargo")
       ("rust-cbindgen" ,rust-cbindgen-0.19)
       ("llvm" ,llvm-11)
       ("clang" ,clang-11)
       ("perl" ,perl)
       ("node" ,node)
       ("python" ,python)
       ("python-2" ,python-2)
       ("python2-pysqlite" ,python2-pysqlite)
       ("yasm" ,yasm)
       ("nasm" ,nasm)  ; XXX FIXME: only needed on x86_64 and i686
       ("pkg-config" ,pkg-config)
       ("m4" ,m4)
       ("which" ,which)))
    (arguments
     `(#:tests? #f  ;not worth the cost

       ;; Some dynamic lib was determined at runtime, so rpath check may fail.
       #:validate-runpath? #f

       #:configure-flags `("--enable-application=browser"
                           "--with-distribution-id=org.gnu"
                           "--enable-geckodriver"
                           ;; Do not require addons in the global app or
                           ;; system directories to be signed by Mozilla.
                           "--with-unsigned-addon-scopes=app,system"
                           "--allow-addon-sideload"

                           "--enable-pulseaudio"

                           "--disable-tests"
                           "--disable-updater"
                           "--disable-crashreporter"
                           "--disable-eme"

                           ;; Building with debugging symbols takes ~5GiB, so
                           ;; disable it.
                           "--disable-debug"
                           "--disable-debug-symbols"

                           "--enable-rust-simd"
                           "--enable-release"
                           "--enable-optimize"
                           "--enable-strip"
                           "--disable-elf-hack"

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
                           "--with-system-jpeg"        ; must be libjpeg-turbo
                           "--with-system-png"         ; must be libpng-apng
                           "--with-system-zlib"
                           ;; UNBUNDLE-ME! "--with-system-bz2"
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
                           )

       #:imported-modules ,%cargo-utils-modules ;for `generate-all-checksums'

       #:modules ((ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-1)
                  (srfi srfi-26)
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
                         (or native-inputs inputs)))))
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
                         ;; "media/libjpeg"  ; needed for now, because media/libjpeg/moz.build is referenced from config/external/moz.build
                         ;; UNBUNDLE-ME! "modules/zlib"
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
                         ))))
         (add-after 'remove-bundled-libraries 'fix-ffmpeg-runtime-linker
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((ffmpeg (assoc-ref inputs "ffmpeg"))
                    (libavcodec (string-append ffmpeg "/lib/libavcodec.so")))
               ;; Arrange to load libavcodec.so by its absolute file name.
               (substitute* "dom/media/platforms/ffmpeg/FFmpegRuntimeLinker.cpp"
                 (("libavcodec\\.so")
                  libavcodec)))))
         (add-after 'fix-ffmpeg-runtime-linker 'build-sandbox-whitelist
           (lambda* (#:key inputs #:allow-other-keys)
             (define (runpath-of lib)
               (call-with-input-file lib
                 (compose elf-dynamic-info-runpath
                          elf-dynamic-info
                          parse-elf
                          get-bytevector-all)))
             (define (runpaths-of-input label)
               (let* ((dir (string-append (assoc-ref inputs label) "/lib"))
                      (libs (find-files dir "\\.so$")))
                 (append-map runpath-of libs)))
             ;; Populate the sandbox read-path whitelist as needed by ffmpeg.
             (let* ((whitelist
                     (map (cut string-append <> "/")
                          (delete-duplicates
                           `(,(string-append (assoc-ref inputs "shared-mime-info")
                                             "/share/mime")
                             ,(string-append (assoc-ref inputs "font-dejavu")
                                             "/share/fonts")
                             "/run/current-system/profile/share/fonts"
                             ,@(append-map runpaths-of-input
                                           '("mesa" "ffmpeg"))))))
                    (whitelist-string (string-join whitelist ","))
                    (port (open-file "browser/app/profile/icecat.js" "a")))
               (format #t "setting 'security.sandbox.content.read_path_whitelist' to '~a'~%"
                       whitelist-string)
               (format port "~%pref(\"security.sandbox.content.read_path_whitelist\", ~S);~%"
                       whitelist-string)
               (close-output-port port))))
         (add-after 'patch-source-shebangs 'patch-cargo-checksums
           (lambda _
             (use-modules (guix build cargo-utils))
             (let ((null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
               (for-each (lambda (file)
                           (format #t "patching checksums in ~a~%" file)
                           (substitute* file
                             (("^checksum = \".*\"")
                              (string-append "checksum = \"" null-hash "\""))))
                         (find-files "." "Cargo.lock$"))
               (for-each generate-all-checksums
                         '("services"
                           "js"
                           "third_party/rust"
                           "dom/media"
                           "dom/webauthn"
                           "toolkit"
                           "gfx"
                           "storage"
                           "modules"
                           "xpcom/rust"
                           "media"
                           "mozglue/static/rust"
                           "netwerk"
                           "remote"
                           "intl"
                           "servo"
                           "security/manager/ssl"
                           "build")))))
         (delete 'bootstrap)
         (replace 'configure
           ;; configure does not work followed by both "SHELL=..." and
           ;; "CONFIG_SHELL=..."; set environment variables instead
           (lambda* (#:key outputs configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (which "bash"))
                    (abs-srcdir (getcwd))
                    (flags `(,(string-append "--prefix=" out)
                             ,(string-append "--with-l10n-base="
                                             abs-srcdir "/l10n")
                             ,@configure-flags)))
               (setenv "SHELL" bash)
               (setenv "CONFIG_SHELL" bash)

               (setenv "AR" "llvm-ar")
               (setenv "NM" "llvm-nm")
               (setenv "CC" "clang")
               (setenv "CXX" "clang++")
               (setenv "LDFLAGS" (string-append "-Wl,-rpath="
                                                (assoc-ref outputs "out")
                                                "/lib/icecat"))

               (setenv "MACH_USE_SYSTEM_PYTHON" "1")
               (setenv "MOZ_NOSPAM" "1")
               (setenv "MOZ_BUILD_DATE" ,%icecat-build-id) ; avoid timestamp

               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" flags)

               (call-with-output-file "mozconfig"
                 (lambda (out)
                   (for-each (lambda (flag)
                               (format out "ac_add_options ~a\n" flag))
                             flags)))

               (invoke "./mach" "configure"))))
         (replace 'build
           (lambda* (#:key (make-flags '()) (parallel-build? #t)
                     #:allow-other-keys)
             (apply invoke "./mach" "build"
                    ;; mach will use parallel build if possible by default
                    `(,@(if parallel-build?
                            '()
                            '("-j1"))
                      ,@make-flags))))
         (add-after 'build 'neutralise-store-references
           (lambda _
             ;; Mangle the store references to compilers & other build tools in
             ;; about:buildconfig, reducing IceCat's closure by 1 GiB on x86-64.
             (let* ((obj-dir (match (scandir "." (cut string-prefix? "obj-" <>))
                               ((dir) dir)))
                    (file (string-append
                           obj-dir
                           "/dist/bin/chrome/toolkit/content/global/buildconfig.html")))
               (substitute* file
                 (("[0-9a-df-np-sv-z]{32}" hash)
                  (string-append (string-take hash 8)
                                 "<!-- Guix: not a runtime dependency -->"
                                 (string-drop hash 8)))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "./mach" "install")
             ;; The geckodriver binary is not installed by the above, for some
             ;; reason.  Use 'find-files' to avoid having to deal with the
             ;; system/architecture-specific file name.
             (install-file (first (find-files "." "geckodriver"))
                           (string-append (assoc-ref outputs "out") "/bin"))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (gtk (assoc-ref inputs "gtk+"))
                    (gtk-share (string-append gtk "/share"))
                    (ld-libs (map (lambda (label)
                                    (string-append (assoc-ref inputs label)
                                                   "/lib"))
                              '("libpng-apng"
                                "libxscrnsaver"
                                "mesa"
                                "pciutils"
                                "mit-krb5"
                                "eudev"
                                "pulseaudio"
                                ;; For the integration of native notifications
                                "libnotify"))))
               (wrap-program (car (find-files lib "^icecat$"))
                 `("XDG_DATA_DIRS" prefix (,gtk-share))
                 ;; The following line is commented out because the icecat
                 ;; package on guix has been observed to be unstable when
                 ;; using wayland, and the bundled extensions stop working.
                 ;;   `("MOZ_ENABLE_WAYLAND" = ("1"))
                 `("LD_LIBRARY_PATH" prefix ,ld-libs)))))
         (add-after 'wrap-program 'install-desktop-entry
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
               (install-file desktop-file applications))))
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
features built-in privacy-protecting features.  This package also includes the
@command{geckodriver} command, which can be useful for automated web
testing.

WARNING: IceCat 91 has not yet been released by the upstream IceCat project.
This is a preview release, and does not currently meet the privacy-respecting
standards of the IceCat project.")
    (license license:mpl2.0)     ;and others, see toolkit/content/license.html
    (properties
     `((ftp-directory . "/gnu/gnuzilla")
       (cpe-name . "firefox_esr")
       (cpe-version . ,(first (string-split version #\-)))))))

;; Update this together with icecat!
(define %icedove-build-id "20220111000000") ;must be of the form YYYYMMDDhhmmss
(define-public icedove
  (package
    (name "icedove")
    (version "91.5")
    (source icecat-source)
    (properties
     `((cpe-name . "thunderbird_esr")))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                              ; no check target
       #:imported-modules ,%cargo-utils-modules ;for `generate-all-checksums'
       #:modules ((guix build utils)    ;find-files
                  (sxml simple)
                  (ice-9 regex)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-thunderbird-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir "comm")
             (copy-recursively (assoc-ref inputs "thunderbird-sources")
                               "comm")
             (delete-file "sourcestamp.txt")
             #t))
         (add-after 'patch-source-shebangs 'patch-cargo-checksums
           (lambda _
             (use-modules (guix build cargo-utils))
             (let ((null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
               (for-each (lambda (file)
                           (format #t "patching checksums in ~a~%" file)
                           (substitute* file
                             (("^checksum = \".*\"")
                              (string-append "checksum = \"" null-hash "\""))))
                         (find-files "." "Cargo.lock$"))
               (for-each generate-all-checksums
                         '("third_party/rust"
                           "toolkit/library/rust")))
             #t))
         ;; Fixes issue where each installation directory generates its own profile.
         ;; See e.g. https://trac.torproject.org/projects/tor/ticket/31457
         (add-after 'patch-source-shebangs 'fix-profile-setting
           (lambda _
             (substitute* "comm/mail/moz.configure"
               (("MOZ_DEDICATED_PROFILES, True")
                "MOZ_DEDICATED_PROFILES, False"))
             #t))
         (add-after 'prepare-thunderbird-sources 'rename-to-icedove
           (lambda _
             (substitute* "comm/mail/confvars.sh"
               (("MOZ_APP_NAME=thunderbird")
                "MOZ_APP_NAME=icedove")
               (("MOZ_UPDATER=1")
                "MOZ_UPDATER=0"))
             ;; Remove branding to comply with Mozilla's trademark policy
             (with-directory-excursion "comm/mail/branding/nightly"
               (delete-file "content/about-wordmark.svg")
               (call-with-output-file "content/about-wordmark.svg"
                 (lambda (port)
                   (sxml->xml '(svg (@ (xmlns "http://www.w3.org/2000/svg")
                                       (viewBox "0 0 789.1 90.78")
                                       (width "333")
                                       (height "48")
                                       (fill "#fff"))
                                    (text (@ (x "400") (y "70")
                                             (text-anchor "middle")
                                             (font-size "90"))
                                          "Icedove Daily"))
                              port)))
               (substitute* '("locales/en-US/brand.properties"
                              "locales/en-US/brand.ftl"
                              "locales/en-US/brand.dtd"
                              "configure.sh")
                 (("Thunderbird") "Icedove")
                 (("mozilla.org") "guix.gnu.org")))
             ;; Remove other mentions of Thunderbird in user-visible text.
             (with-directory-excursion "comm/mail/base/content"
               (substitute* '("overrides/app-license-name.html")
                 (("Thunderbird") "Icedove")))
             (with-directory-excursion "comm/mail/components/"
               (substitute* '("MailGlue.jsm"
                              "extensions/schemas/addressBook.json"
                              "extensions/schemas/tabs.json"
                              "extensions/schemas/cloudFile.json"
                              "extensions/schemas/chrome_settings_overrides.json"
                              "extensions/schemas/windows.json"
                              "extensions/parent/ext-mail.js"
                              "im/messages/mail/Info.plist"
                              "enterprisepolicies/moz.build"
                              "enterprisepolicies/helpers/moz.build"
                              "enterprisepolicies/schemas/moz.build")
                 (("Thunderbird") "Icedove")))
             (substitute* '("comm/mailnews/base/prefs/content/accountUtils.js"
                            "comm/mail/base/content/customizeToolbar.js"
                            "comm/suite/components/customizeToolbar.js")
               (("AppConstants.MOZ_APP_NAME (.)= \"thunderbird" _ e)
                (format #f "AppConstants.MOZ_APP_NAME ~a= \"icedove" e)))

             ;; Override addon URLs and settings
             (substitute* "comm/mail/app/profile/all-thunderbird.js"
               (("(pref\\(\"extensions.webservice.discoverURL\").*" _ m)
                (string-append m ", \"https://directory.fsf.org/wiki/Icedove\");"))
               (("(pref\\(\"extensions.getAddons.search.url\").*" _ m)
                (string-append m ", \"https://guix.gnu.org/packages\");"))
               (("(pref\\(\"extensions.update.enabled\").*" _ m)
                (string-append m ", false);"))
               (("(pref\\(\"extensions.systemAddon.update.enabled\").*" _ m)
                (string-append m ", false);"))
               (("(pref\\(\"lightweightThemes.update.enabled\").*" _ m)
                (string-append m ", false);")))
             #t))
         (add-after 'build 'neutralize-store-references
           (lambda _
             ;; Mangle the store references to compilers & other build tools in
             ;; about:buildconfig, reducing Icedove's closure significant.
             ;; The resulting files are saved in lib/thunderbird/omni.ja
             (substitute*
                 ;; Use find because the path "obj-x86_64-pc-linux-gnu" contains
                 ;; the architecture and the system -> more complicated.
                 (find-files "." "buildconfig.html")
               (((format #f "(~a/)([0-9a-df-np-sv-z]{32})"
                         (regexp-quote (%store-directory)))
                 _ store hash)
                (string-append store
                               (string-take hash 8)
                               "<!-- Guix: not a runtime dependency -->"
                               (string-drop hash 8))))
             #t))
         (delete 'bootstrap)
         (replace 'configure
           (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (which "bash"))
                    (abs-srcdir (getcwd))
                    (srcdir (string-append "../" (basename abs-srcdir)))
                    (flags `(,(string-append "--prefix=" out)
                             ,@configure-flags))
                    (mozconfig (string-append (getcwd) "/.mozconfig")))
               (setenv "SHELL" bash)
               (setenv "CONFIG_SHELL" bash)
               (setenv "QA_CONFIGURE_OPTIONS" ".*")
               (setenv "MOZBUILD_STATE_PATH"
                       (string-append (getcwd) "/mach_state"))
               (setenv "MOZCONFIG"
                       (string-append (getcwd) "/.mozconfig"))

               (setenv "AR" "llvm-ar")
               (setenv "NM" "llvm-nm")
               (setenv "CC" "clang")
               (setenv "CXX" "clang++")

               (setenv "MOZ_NOSPAM" "1")
               (setenv "MACH_USE_SYSTEM_PYTHON" "1")
               (setenv "PYTHON"
                       (search-input-file inputs "/bin/python"))
               (setenv "MOZ_BUILD_DATE" ,%icedove-build-id) ; avoid timestamp
               (setenv "MOZ_APP_NAME" "icedove")
               (setenv "LDFLAGS" (string-append "-Wl,-rpath="
                                                (assoc-ref outputs "out")
                                                "/lib/icedove"))
               (mkdir-p (string-append (getcwd) "/builddir"))
               (with-output-to-file mozconfig
                 (lambda ()
                   (display
                    (string-append
                     "ac_add_options --disable-crashreporter\n"
                     "ac_add_options --disable-debug\n"
                     "ac_add_options --disable-debug-symbols\n"
                     "ac_add_options --disable-elf-hack\n"
                     "ac_add_options --disable-jit\n"
                     "ac_add_options --disable-necko-wifi\n"
                     "ac_add_options --disable-official-branding\n"
                     "ac_add_options --disable-tests\n"
                     "ac_add_options --disable-updater\n"
                     "ac_add_options --disable-webrtc\n"
                     "ac_add_options --enable-application=comm/mail\n"
                     "ac_add_options --enable-default-toolkit=\"cairo-gtk3\"\n"
                     "ac_add_options --enable-optimize\n"
                     "ac_add_options --enable-pulseaudio\n"
                     "ac_add_options --enable-release\n"
                     "ac_add_options --enable-strip\n"
                     "ac_add_options --enable-system-ffi\n"
                     "ac_add_options --enable-system-pixman\n"
                     "ac_add_options --prefix=" out "\n"
                     "ac_add_options --with-clang-path=" (assoc-ref %build-inputs "clang") "/bin/clang\n"
                     "ac_add_options --with-distribution-id=org.gnu\n"
                     "ac_add_options --with-libclang-path=" (assoc-ref %build-inputs "clang") "/lib\n"
                     "ac_add_options --with-system-bz2\n"
                     "ac_add_options --with-system-icu\n"
                     "ac_add_options --with-system-jpeg\n"
                     "ac_add_options --with-system-libevent\n"
                     "ac_add_options --with-system-nspr\n"
                     ;"ac_add_options --with-system-nss\n"
                     "ac_add_options --with-system-zlib\n"
                     "ac_add_options --with-user-appdir=\\.icedove\n"
                     "mk_add_options MOZ_MAKE_FLAGS=-j"
                     (number->string (parallel-job-count)) "\n"))))
               (display (getcwd))
               (newline)
               (display "mach configure")
               (invoke "./mach" "configure"))))
         (replace 'build
           (lambda _ (invoke "./mach" "build")))
         (replace 'install
           (lambda _ (invoke "./mach" "install")))
         ;; Thunderbird doesn't provide any .desktop file.
         ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1637575
         (add-after 'install 'install-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apps (string-append out "/share/applications")))
               (mkdir-p apps)
               (with-output-to-file (string-append apps "/icedove.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                            Name=Icedove~@
                            Exec=~a/bin/icedove~@
                            Icon=icedove~@
                            GenericName=Mail/News Client~@
                            Categories=Network;Email;~@
                            Terminal=false~@
                            StartupNotify=true~@
                            MimeType=x-scheme-handler/mailto;~@
                            Type=Application~@
                            Actions=ComposeMessage;~@
                            [Desktop Action ComposeMessage]~@
                            Name=Write new message~@
                            Exec=~@*~a/bin/icedove -compose~%"
                           out))))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (gtk (assoc-ref inputs "gtk+"))
                    (gtk-share (string-append gtk "/share"))
                    (pulseaudio (assoc-ref inputs "pulseaudio"))
                    (pulseaudio-lib (string-append pulseaudio "/lib"))
                    (eudev (assoc-ref inputs "eudev"))
                    (eudev-lib (string-append eudev "/lib")))
               (wrap-program (car (find-files lib "^icedove$"))
                 `("XDG_DATA_DIRS" prefix (,gtk-share))
                 `("LD_LIBRARY_PATH" prefix (,pulseaudio-lib ,eudev-lib)))
               #t))))))
    (inputs
     `(("bzip2" ,bzip2)
       ("cairo" ,cairo)
       ("cups" ,cups)
       ("dbus-glib" ,dbus-glib)
       ("ffmpeg" ,ffmpeg)
       ("freetype" ,freetype)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("hunspell" ,hunspell)
       ("icu4c" ,icu4c)
       ("libcanberra" ,libcanberra)
       ("libevent" ,libevent)
       ("libffi" ,libffi)
       ("libgnome" ,libgnome)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng-apng" ,libpng-apng)
       ("libvpx" ,libvpx)
       ("libxcomposite" ,libxcomposite)
       ("libxft" ,libxft)
       ("libxinerama" ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxt" ,libxt)
       ("mesa" ,mesa)
       ("mit-krb5" ,mit-krb5)
       ("nspr" ,nspr-4.32)
       ; FIXME: create nss >= 3.68 after core-updates merge
       ;("nss" ,nss)
       ("pango" ,pango)
       ("pixman" ,pixman)
       ("pulseaudio" ,pulseaudio)
       ("sqlite" ,sqlite)
       ("startup-notification" ,startup-notification)
       ("eudev" ,eudev)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     `(("thunderbird-sources"
        ;; The changeset identifier is taken from the file "sourcestamp.txt"
        ;; in the Thunderbird release tarball.  We don't use the release
        ;; tarball because it duplicates the Icecat sources and only adds the
        ;; "comm" directory, which is provided by this repository.
        ,(let ((changeset "bcd2aab51cd0889d506d29455210d65602b97430"))
           (origin
             (method hg-fetch)
             (uri (hg-reference
                   (url "https://hg.mozilla.org/releases/comm-esr91")
                   (changeset changeset)))
             (file-name (string-append "thunderbird-" version "-checkout"))
             (sha256
              (base32
               "0aj8a8qbm71n34yi58y04bn4h9zz2rciz0cm3hh58rsmcqs1s9ym")))))
       ("cargo" ,rust "cargo")
       ("clang" ,clang-11)
       ("llvm" ,llvm-11)
       ("m4" ,m4)
       ("nasm" ,nasm)
       ("node" ,node)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("python2" ,python-2.7)
       ("rust" ,rust)
       ("rust-cbindgen" ,rust-cbindgen-0.19)
       ("which" ,which)
       ("yasm" ,yasm)))
    (home-page "https://www.thunderbird.net")
    (synopsis "Rebranded Mozilla Thunderbird email client")
    (description
     "This package provides an email client built based on Mozilla
Thunderbird.  It supports email, news feeds, chat, calendar and contacts.")
    (license license:mpl2.0)))

(define-public icedove/wayland
  (package
    (inherit icedove)
    (name "icedove-wayland")
    (native-inputs '())
    (inputs
     `(("bash" ,bash-minimal)
       ("icedove" ,icedove)))
    (build-system trivial-build-system)
    (arguments
      '(#:modules ((guix build utils))
        #:builder
        (begin
          (use-modules (guix build utils))
          (let* ((bash    (assoc-ref %build-inputs "bash"))
                 (icedove (assoc-ref %build-inputs "icedove"))
                 (out     (assoc-ref %outputs "out"))
                 (exe     (string-append out "/bin/icedove")))
            (mkdir-p (dirname exe))

            (call-with-output-file exe
              (lambda (port)
                (format port "#!~a
 MOZ_ENABLE_WAYLAND=1 exec ~a $@"
                        (string-append bash "/bin/bash")
                        (string-append icedove "/bin/icedove"))))
            (chmod exe #o555)

            ;; Provide the manual and .desktop file.
            (copy-recursively (string-append icedove "/share")
                              (string-append out "/share"))
            (substitute* (string-append
                          out "/share/applications/icedove.desktop")
              ((icedove) out))
            #t))))))

(define-public firefox-decrypt
  (package
    (name "firefox-decrypt")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Unode/firefox_decrypt")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17yyyxp47z4m8hnflcq34rc1y871515kr3f1y42j1l0yx3g0il07"))))
    (build-system trivial-build-system)
    (inputs
     (list nss python))
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

(define-public lz4json
  (package
    (name "lz4json")
    (version "2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/andikleen/lz4json")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xxn8yzr6j8j6prmbj6mxspdczigarfiv3vlm9k70yxmky65ijh3"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list lz4))
    (arguments
     `(#:tests? #f                              ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)                    ; no configure script
         (replace 'install                      ; no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "lz4jsoncat" bin)
               (install-file "lz4jsoncat.1" man)))))
       #:make-flags `(,(string-append "CC=" ,(cc-for-target)))))
    (home-page "https://github.com/andikleen/lz4json")
    (synopsis "C decompress tool for mozilla lz4json format")
    (description
     "@code{lz4json} is a little utility to unpack lz4json files as generated
by Firefox's bookmark backups and session restore.  This is a different format
from what the normal lz4 utility expects.  The data is dumped to stdout.")
    (license license:bsd-2)))
