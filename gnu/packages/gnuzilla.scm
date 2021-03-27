;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2019, 2020 Adrian Malacoda <malacoda@monarch-pass.net>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
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

(define-public mozjs-78
  (package
    (inherit mozjs-60)
    (version "78.5.0")
    (source (origin
              (method url-fetch)
              ;; TODO: Switch to IceCat source once available on ftp.gnu.org.
              (uri (string-append "https://archive.mozilla.org/pub/firefox"
                                  "/releases/" version "esr/source/firefox-"
                                  version "esr.source.tar.xz"))
              (sha256
               (base32
                "1442yjmwz69hkfcvh8kkb60jf4c9ms0pac04nc3xw2da13v4zxai"))))
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
         "--enable-rust-simd"
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
               (setenv "AUTOCONF" (string-append (assoc-ref inputs "autoconf")
                                                 "/bin/autoconf"))
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
     `(("icu4c" ,icu4c-68)
       ("readline" ,readline)
       ("zlib" ,zlib)))))

(define mozilla-compare-locales
  (origin
    (method hg-fetch)
    (uri (hg-reference
          (url "https://hg.mozilla.org/l10n/compare-locales/")
          (changeset "RELEASE_8_0_0")))
    (file-name "mozilla-compare-locales")
    (sha256 (base32 "0052wq92sg4i776x407b5838jx9h3phl9xy69m2q34f31n3gdyk2"))))

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
   ("1q1p7nl97478hkf1msbbcdh98k6fn87xwqihbw8np00ll5gk9k4v" "97cf3c155484" "ach")
   ("0sajpblp639l448xywx7xlybjr5dm0rfrzx511a8pa9wn7dma4mf" "46929b4dda4e" "af")
   ("04s756aaf1yyli1dn1vfdjymgyjs1pz2n7jvz76j27x4nzdgss7l" "96ffd2b04ba3" "an")
   ("16nw7slm7irsnjf81r6zl273kbzri8sdbimzgzxwm0vh6xr61rgs" "1c4231166ddf" "ar")
   ("0i0qrj4j8zjv55cisbmr21dp8mz933hc9wkpn3abz5k5pn84gpry" "bc4384b8be78" "ast")
   ("1r77mmwg9x1jfa7g5lqa31rgyfrnix1zj80ibn5rq9gyhhsdrwbd" "dd56aead51fa" "az")
   ("1kzjpy5bnvps8di72ksynhbhc9bpw1ml6hvphm74z8dz55ai4c18" "9d2bff64ddfb" "be")
   ("1fygvjgph3siknfm7l0fh9y4ava72z1rxkip1zgmcdgq7jz8wzpf" "3808f4fe4db6" "bg")
   ("1x9nlqia441xh04rhkmkw6qrgpwnyqw0grrf5n2qw96939wnmgl7" "5ca8752ed8df" "bn")
   ("18yyxqzab44bgqx7h052d5nxyhv0l5bidgr16z1b3ak2crsfyx9q" "4c5d30ca9bf2" "br")
   ("11bschjz7pgsm4r1qan5l4s3nkzm5jb0kivpp186wd1xsjci5bjb" "cf2478a7eae1" "bs")
   ("12bak64nl6qi092l55xv330vh38mfsicrrf4wi693nn7zqb3mbpw" "4aa2cc349211" "ca")
   ("0p97d7pnxqs971rr5c57i6cw3mx1mp3iasa0xdmdk0zpz9pjd3s4" "806914072144" "ca-valencia")
   ("1rligf98h9r16mw3r3n5jalzi74xn2dnh6hkiixp7s2bvhjicwa4" "db2163383129" "cak")
   ("18y5j8ljh72mj2nbx0m64mi3fyjbwyx992i004sv3zvs4d4z18w4" "d1d09eedddde" "cs")
   ("12i4m9q6f8sl8arm8ja4gs4sl9m59p3kddlqi68srpz9mk66rqxz" "1caf58c64227" "cy")
   ("16wacsd23zd4j0yi6vbj033wylia8hlpswx949x5hy1h9817j4vn" "9ef3b56aa243" "da")
   ("1ddxnqpfi1wnciwmzkvvjinb51h22bg70r0dn7db17q64p4271rk" "a050b0eaec0a" "de")
   ("0gw5h52rw3bc2lwffnv845sjwwj22gmabchmpa1rw2y6087dc7zk" "613135cb0759" "dsb")
   ("0v17da37w1kbs73i6xql4c9xng6rfachyylpy3w8hpibpvi7i30n" "903db7c76b31" "el")
   ("0ky1nj4kp070nriw0igsa64l39plcv2365zl8lqpbydyp6clwc3m" "6bb74673e9aa" "en-CA")
   ("15jck6si2l4h5lrs8zjx56y97p70njpi26nnq6xfmvj1dk5qb4g2" "c06bd83a9295" "en-GB")
   ("1gc400n8j8qk70vdw7fkf4m9yviia35cmj6361pxnyvsjrgm4qvm" "ae0fda5114c4" "eo")
   ("0y01lahdrd3krsh6nrr3iw36r2x4advh5qw54vf92w11l3aiyvfz" "0c294dc9ce40" "es-AR")
   ("16jjbsfa62dxk36ccziv7nmh26c1s0b05960in6zs5nnfrv4yh35" "7c30b2981fb4" "es-CL")
   ("0qh8c8f8dsv1v56n1mcpn84l39bpnqfl8v8iacq12zh322srr549" "ad1444f4f833" "es-ES")
   ("1vh3hm5r4ch6mk3ymbk12b9rppwc75fmbi0i7cml82kxsi77bzw5" "0a26cdb23536" "es-MX")
   ("159l92jxx48lgba1g8ig3mdzhwsf3jpg3nx1g9blq4majk5hqr6q" "65a38a830795" "et")
   ("1ha8y1wbgb66dqm26x1q2xwygy7bblkjz40y9vh5d2qlpr3fn0av" "21e0930b221d" "eu")
   ("0rq4pcw6klm0bbljm1wdfvna8bpa35cm47hh2s63i2xdax4scahf" "5a4bb020cf09" "fa")
   ("1py2594gsvpgrxnplz278ffb7grsf384kzjskrl1zyps0jw8fb1x" "4a4f712cd4aa" "ff")
   ("1dyd55ngsglp1w2gh0yaacwb0vsq23gdvnj76f2x6g39h1li9s0z" "9c51cd915e2b" "fi")
   ("0kimwivpq6pr63jh1k9fszsv8bi8vns3scg76mmnvbhh2ca8q7wj" "4f9e24a696ee" "fr")
   ("1sbbnnp12lgy5qan2kix02942830b1969bd7jxrv7piwqfch9i06" "9e21a0eeb5b1" "fy-NL")
   ("0dsvvyfrzkx5h44gqgdci7arb8h4lq48w20cnr8fc7j17grvnkzz" "999a995bc09d" "ga-IE")
   ("1487msbsgrfzmyijhf6a4wbqdjpd7b7ki9nwrjjjjlnbw0h0ljpb" "6a9ddcab3240" "gd")
   ("1kzc4fwr18kgisdzba2acj1ag8mxbifqpk5p30jv68nmvqfsvl8d" "51eb5e352db9" "gl")
   ("13gy3wn44kcxr7j3sbl69fp415875f4vb0gm91hx0fysqlvryhcs" "b9de1ffe3224" "gn")
   ("0w5nvzpjn5vr35c1852rlff581vpy71nc096cz125852kyqkzkc3" "5b3307475ada" "gu-IN")
   ("1ycakc4qpy9vcy50j3ricryjfnjr9v3a5ijj6bbfl4y6aks157fy" "c742df968ffd" "he")
   ("1b2jf83c500wm5wcdnijq0b7y4m8n6271smq8pygahn5nq17f0gq" "1a3039a52b8a" "hi-IN")
   ("19bbw8ix5m83cf4yarcmjl7jqa8xfabwqnh3nj6vi52rwvn7whk5" "8dc50e269ef3" "hr")
   ("12rrsvgg6bb2h8fhni7jbx8pv983q8ym5fhfjim957n9q2yp5db6" "67e40f48dec7" "hsb")
   ("0apyh713p3hrlj8041xwblcssahbmsqp9v9hbmb50ayf4z850kr1" "40073a597b1b" "hu")
   ("0q0r076lq250d3vmnz9d92wj137c2v8i362c2avmkgp5zr3mcl0z" "2ea33335afdb" "hy-AM")
   ("0qza33gdc1i9259dwd2f7vd78s0a6rg34aqdkhcn7f2l6ybw6xd6" "930041db15eb" "ia")
   ("1211h0gp7gianh3qf76w04gfzk4n2bnyc9i8dviyz0vh4cjbx11m" "08811a49b41c" "id")
   ("12lcr841g1j7453s7gb51vrvxmshx4ha3h1jx4vh8wr891xv8l6a" "2f7a8d31e0ba" "is")
   ("1x585g0r2kcv0d3phnxx85bk5g0pi1yl0hwp4idv19yc9hslr04s" "188357cc04b4" "it")
   ("09v35g9v7j6x0p1hggydm3a1rmq2fh4z7g1l88z3w5k6wq2nhj1b" "45cee0ba4771" "ja")
   ("0prs3vycfvvaffjigdgyxiq41ak2rc34lnan5a6cwdqjgy7z450s" "d60a19d9bf17" "ja-JP-mac")
   ("1nskzm8rgczrbgcxlzzq5zqdfd456ad0cylq27nf0wjiyq6kjzcm" "00cb00e78672" "ka")
   ("0g6zznmhiam172nm7g2qzfpk415mna8kiihm73z2sdns64xb3ymg" "77a293a4bced" "kab")
   ("17dld9lrym7rpvpvnkssivp4wx1f11zpk86wczbq1h52qgd70p55" "2c9b33a56d5d" "kk")
   ("1nlzl8930c8ql3yq425wyqlxvq8arrjv20xpm5g7yfxd54av89ac" "9cddd42af05c" "km")
   ("07hkrcdksvrqk816yimd036dlw15nc4sjk4gmw16ywbp093v0mqq" "e0c2969a8398" "kn")
   ("08aqggvk3qbv5bzks9i1iba9akhkpm01d2c9k0zf41mpr2r5yfg2" "827567d0dafc" "ko")
   ("0vagaiwy80bs1k3gkacshlzb5zchkcshx0ypwirvayc63sw4yl8l" "694b2a24e868" "lij")
   ("1r43kp1kzahrbza0hiyavqplk9h08pzsb6rpjy79jr6l1iqb89sy" "d6728db7e060" "lt")
   ("0sq2wbsj79xl7fi454k6q5xdhxyck1whjz315rv37vphqpx86b9b" "61e9b33f4d94" "lv")
   ("0q8jxg1af22hs9wjdf0jd3bqk4rafxyzvsjl35k75am7l2y1fl3c" "9e482f6dd72c" "mk")
   ("1zsfzjrzbc58d30a9yz12h5vphywgpw8xg6y6zn3677a785dvr20" "1fd2763336a4" "mr")
   ("1rzygkkpn1a59daygd3hdaqph2np6sqvpgh68j0xr4il958ymnsm" "67ddab62dab4" "ms")
   ("16jp6w5gizfxs7jvncg3ly13m59vqvh4rlmjd0q23m5g5ff9sklc" "3ed015b51bf3" "my")
   ("1wfv023j67lb4iyf49fsknwm4z3xy0xqcf25b2nzanggxj26n01x" "d01801233a8f" "nb-NO")
   ("1946vfia58vbjfippb5pfsskbjj95w7hb340smn6ry2vmza99mxp" "582defb08fb2" "ne-NP")
   ("12w5ywh4c3s55y3zqc48cp1gcpwwjg444yfh1bghhhb9ni1xkh5i" "05f6359a29a6" "nl")
   ("17jb076320cgkw1ypwmws2vjxsqlv2ww8aaisa3j334vbrw1m4zx" "50b41a1ddded" "nn-NO")
   ("1y840j0v5zdgj94cbacy6j1snf44rynmzxq3yk8i26arcar62akl" "a6a138531a44" "oc")
   ("0jq1hq4xhqxpa26r8pb1bgbaljgfkhn9l6p5pbnslkllpbh70q6l" "e70a3afaef25" "pa-IN")
   ("1hih138skwy2gb8q10ngg6zalrk3aa3d549mg79gqzxbi5zy19fw" "e035f33389eb" "pl")
   ("1hhif4hx4k351wm1bzykzycfzb5q8msxmiwh5r1cy32rh8wkxwhh" "54098495f37f" "pt-BR")
   ("0gkjs12rxjml1m3mljskpz1gr6aph0c31nwpwdqybfg54w9qslib" "3fdf021f624e" "pt-PT")
   ("0anyvwd9v6mr8y3ww33s6qnxkawqn5lz65vrxx3m3kzky63ai1xk" "794f9374eb87" "rm")
   ("1p4drj25gsfv7lwgm5saazh38iqrh53952n8i4bmxpn0nadbm2n5" "71ce18bcf6cc" "ro")
   ("17yssf4axd3zvncl4ka4wkfnwcn0z0arp3390vb9cps67na29p36" "3a9587227699" "ru")
   ("0xk6rksspcw1222v4rgk5a6gzrpx64k29hm7p9qkqwd70s34yj46" "c020582a72ce" "si")
   ("1ax5ibydyn7sj208r66zcxlcr8dxdqrw28vqyjif4jx583rp4lfp" "745a699b7f51" "sk")
   ("13rin7hm1dv8g0hbcv8mp2hiwpk1k5bhzvkqpqajkkik4lx523mc" "8e437e2351ef" "sl")
   ("0yh5jkl5zw3f7x1w2w6zfj3dyvcl4wj1zv4di7qsq2nl2yyizf7x" "2d99e2eff94f" "son")
   ("0vzq7s27jsdbw5k59wfykysg1kd8w229ab5d4zjdf30l59igkahh" "69bbdf07bd80" "sq")
   ("1mwivvs8vrk6vjq6i33kwlrlisra7dy35521ayps9p2rz2dll4rr" "215df5c5125c" "sr")
   ("0g97yz1rg5cfflj8vvq3sqliyvm06x818z9yldfh5rjg1y6n9fjd" "8be00a1a50d4" "sv-SE")
   ("0ii02jn3lh2i6d0s95whx9aj6w3x8axc7w1rlzj0lc2s9n52krz3" "170a84339dbe" "ta")
   ("1ss7symad2crapxjqc0xhc0n17l79r5vf7flvkgk7npjky4vb7nv" "72a79a304f7f" "te")
   ("11iqmg8zamgscnvs4n2xpw3g9azn6w38qs313wiwm86pyx6694ss" "8e91ce3064c5" "th")
   ("1zgkvn9flb8by62ip9r3gmpgxwgkww1zhml5mwa0djq3ppfdgi1c" "0f914d0cda56" "tl")
   ("1filkhdak6dbrd83q602x1qw1703nlm53nm9gcyab8s16gsx6ylz" "62ca6a8eaeba" "tr")
   ("0cgagdy0ixprk3knczdmkqxkmx4ybmshhh0956kxbd0iab0dfcf6" "f110ccac4cde" "trs")
   ("1f1ghk67zwnwc5x3nk82vcv94nf8glngkfya1hg074q3088sj9pa" "56c0102d5f1c" "uk")
   ("0iyw1b2jjylkdwxv9sxvj4ikxl64sx612b2dvvmf1di8bw86w74r" "7d53bce5ae98" "ur")
   ("1q83cp5pfgs8l03zirwi8r5qp8qyh4zvxdx1ilgaqqlyg42yql7c" "9b500e1a054d" "uz")
   ("1d4nkybz2hk64ay04k965b9lc5nhhpmzcs5ww3b6q4n93rf9c2z7" "2a000025928a" "vi")
   ("1cnrsfnyl3sw3sxsggmjwydvphb2diy0vzknvxdhpnvq3ln18hga" "74724087c25b" "xh")
   ("1j6l66v1xw27z8w78mpsnmqgv8m277mf4r0hgqcrb4zx7xc2vqyy" "527e5e090608" "zh-CN")
   ("1frwx35klpyz3sdwrkz7945ivb2dwaawhhyfnz4092h9hn7rc4ky" "6cd366ad2947" "zh-TW")))

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

(define %icecat-version "78.9.0-guix0-preview1")
(define %icecat-build-id "20210323000000") ;must be of the form YYYYMMDDhhmmss

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
              "0r28wrsk2k6pc922zfs5wljh8ziqm4a98lisn7409j2szhfsq0wf"))))

         (upstream-icecat-base-version "78.7.0") ; maybe older than base-version
         ;;(gnuzilla-commit (string-append "v" upstream-icecat-base-version))
         (gnuzilla-commit "abfe5eebaca3c2787f1a9505669393674493c177")
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
              "00ws3540x5whpicc5fx4k949ff73cqvajz6jp13ahn49wqdads47"))))

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
                          "-p1" "--input" #+gnuzilla-fixes-patch)
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
       ;; UNBUNDLE-ME! ("graphite2" ,graphite2)
       ("pango" ,pango)
       ("freetype" ,freetype)
       ;; UNBUNDLE-ME! ("harfbuzz" ,harfbuzz)
       ("libcanberra" ,libcanberra)
       ("libgnome" ,libgnome)
       ("libjpeg-turbo" ,libjpeg-turbo)
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
       ;; UNBUNDLE-ME! ("libvpx" ,libvpx)
       ("icu4c" ,icu4c-67)
       ("pixman" ,pixman)
       ("pulseaudio" ,pulseaudio)
       ("mesa" ,mesa)
       ("mit-krb5" ,mit-krb5)
       ;; See <https://bugs.gnu.org/32833>
       ;;   and related comments in the 'remove-bundled-libraries' phase.
       ;; UNBUNDLE-ME! ("nspr" ,nspr)
       ;; UNBUNDLE-ME! ("nss" ,nss)
       ("shared-mime-info" ,shared-mime-info)
       ;; UNBUNDLE-ME! ("sqlite" ,sqlite)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ;; UNBUNDLE-ME! ("zlib" ,zlib)
       ))
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

       ("rust" ,rust-1.41)
       ("cargo" ,rust-1.41 "cargo")
       ("rust-cbindgen" ,rust-cbindgen-0.14)
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

       #:configure-flags `("--enable-default-toolkit=cairo-gtk3-wayland"

                           "--with-distribution-id=org.gnu"

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
                           ;; UNBUNDLE-ME! "--with-system-zlib"
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
                           "build")))
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
                    (mesa (assoc-ref inputs "mesa"))
                    (mesa-lib (string-append mesa "/lib"))
                    (pulseaudio (assoc-ref inputs "pulseaudio"))
                    (pulseaudio-lib (string-append pulseaudio "/lib"))
                    (libxscrnsaver (assoc-ref inputs "libxscrnsaver"))
                    (libxscrnsaver-lib (string-append libxscrnsaver "/lib")))
               (wrap-program (car (find-files lib "^icecat$"))
                 `("XDG_DATA_DIRS" prefix (,gtk-share))
                 ;; The following line is commented out because the icecat
                 ;; package on guix has been observed to be unstable when
                 ;; using wayland, and the bundled extensions stop working.
                 ;;   `("MOZ_ENABLE_WAYLAND" = ("1"))
                 `("LD_LIBRARY_PATH" prefix (,pulseaudio-lib ,mesa-lib ,libxscrnsaver-lib)))
               #t))))))
    (home-page "https://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons.  It also
features built-in privacy-protecting features.

WARNING: IceCat 78 has not yet been released by the upstream IceCat project.
This is a preview release, and does not currently meet the privacy-respecting
standards of the IceCat project.")
    (license license:mpl2.0)     ;and others, see toolkit/content/license.html
    (properties
     `((ftp-directory . "/gnu/gnuzilla")
       (cpe-name . "firefox_esr")
       (cpe-version . ,(first (string-split version #\-)))))))

;; Update this together with icecat!
(define %icedove-build-id "20210323000000") ;must be of the form YYYYMMDDhhmmss
(define-public icedove
  (package
    (name "icedove")
    (version "78.9.0")
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
             (delete-file-recursively "obj-x86_64-pc-linux-gnu")
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
               (("'MOZ_DEDICATED_PROFILES', True")
                "'MOZ_DEDICATED_PROFILES', False"))
             #t))
         (add-after 'prepare-thunderbird-sources 'rename-to-icedove
           (lambda _
             (substitute* "comm/mail/confvars.sh"
               (("MOZ_APP_BASENAME=Thunderbird")
                "MOZ_APP_BASENAME=Icedove\nMOZ_APP_DISPLAYNAME=Icedove")
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
                            "comm/common/src/customizeToolbar.js")
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
               (setenv "AUTOCONF"
                       (string-append (assoc-ref %build-inputs
                                                 "autoconf")
                                      "/bin/autoconf"))
               (setenv "CONFIG_SHELL" bash)
               (setenv "QA_CONFIGURE_OPTIONS" ".*")
               (setenv "MOZBUILD_STATE_PATH"
                       (string-append (getcwd) "/mach_state"))
               (setenv "MOZCONFIG"
                       (string-append (getcwd) "/.mozconfig"))
               (setenv "CC" "gcc")
               (setenv "MOZ_NOSPAM" "1")
               (setenv "PYTHON"
                       (string-append (assoc-ref inputs "python2")
                                      "/bin/python"))
               (setenv "MOZ_BUILD_DATE" ,%icedove-build-id) ; avoid timestamp
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
                     "ac_add_options --enable-calendar\n"
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
                     "ac_add_options --with-system-nss\n"
                     "ac_add_options --with-system-zlib\n"
                     "ac_add_options --with-user-appdir=\\.icedove\n"))))
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
                    (pulseaudio-lib (string-append pulseaudio "/lib")))
               (wrap-program (car (find-files lib "^icedove$"))
                 `("XDG_DATA_DIRS" prefix (,gtk-share))
                 `("LD_LIBRARY_PATH" prefix (,pulseaudio-lib)))
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
       ("icu4c" ,icu4c-67)
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
       ("nspr" ,nspr)
       ("nss" ,nss)
       ("pango" ,pango)
       ("pixman" ,pixman)
       ("pulseaudio" ,pulseaudio)
       ("sqlite" ,sqlite)
       ("startup-notification" ,startup-notification)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     `(("thunderbird-sources"
        ;; The changeset identifier is taken from the file "sourcestamp.txt"
        ;; in the Thunderbird release tarball.  We don't use the release
        ;; tarball because it duplicates the Icecat sources and only adds the
        ;; "comm" directory, which is provided by this repository.
        ,(let ((changeset "1a5cd2aa11de609116f258b413afcf113ed72f3a"))
           (origin
             (method hg-fetch)
             (uri (hg-reference
                   (url "https://hg.mozilla.org/releases/comm-esr78")
                   (changeset changeset)))
             (file-name (string-append "thunderbird-" version "-checkout"))
             (sha256
              (base32
               "0qgz9qj8gbn2ccmhvk3259ahs9p435ipvkzsysn3xj8a6klbz02w")))))
       ("autoconf" ,autoconf-2.13)
       ("cargo" ,rust-1.41 "cargo")
       ("clang" ,clang)
       ("llvm" ,llvm)
       ("nasm" ,nasm)
       ("node" ,node)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("python2" ,python-2.7)
       ("rust" ,rust-1.41)
       ("rust-cbindgen" ,rust-cbindgen-0.14)
       ("which" ,which)
       ("yasm" ,yasm)))
    (home-page "https://www.thunderbird.net")
    (synopsis "Rebranded Mozilla Thunderbird email client")
    (description
     "This package provides an email client built based on Mozilla
Thunderbird.  It supports email, news feeds, chat, calendar and contacts.")
    (license license:mpl2.0)))

(define-public icedove/wayland
  (package/inherit icedove
    (name "icedove-wayland")
    (arguments
     (substitute-keyword-arguments (package-arguments icedove)
       ((#:phases phases)
        `(modify-phases ,phases
          (replace 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (gtk (assoc-ref inputs "gtk+"))
                    (gtk-share (string-append gtk "/share"))
                    (pulseaudio (assoc-ref inputs "pulseaudio"))
                    (pulseaudio-lib (string-append pulseaudio "/lib")))
               (wrap-program (car (find-files lib "^icedove$"))
                 `("MOZ_ENABLE_WAYLAND" = ("1"))
                 `("XDG_DATA_DIRS" prefix (,gtk-share))
                 `("LD_LIBRARY_PATH" prefix (,pulseaudio-lib)))
               #t)))))))))

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
