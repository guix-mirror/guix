;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
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

(define-module (gnu packages java)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnuzilla) ;nss
  #:use-module (gnu packages ghostscript) ;lcms
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux) ;alsa
  #:use-module (gnu packages wget)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages texinfo)
  #:use-module ((srfi srfi-1) #:select (fold alist-delete))
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match))

(define-public java-swt
  (package
    (name "java-swt")
    (version "4.6")
    (source
     ;; The types of many variables and procedures differ in the sources
     ;; dependent on whether the target architecture is a 32-bit system or a
     ;; 64-bit system.  Instead of patching the sources on demand in a build
     ;; phase we download either the 32-bit archive (which mostly uses "int"
     ;; types) or the 64-bit archive (which mostly uses "long" types).
     (let ((hash32 "0jmx1h65wqxsyjzs64i2z6ryiynllxzm13cq90fky2qrzagcw1ir")
           (hash64 "0wnd01xssdq9pgx5xqh5lfiy3dmk60dzzqdxzdzf883h13692lgy")
           (file32 "x86")
           (file64 "x86_64"))
       (let-values (((hash file)
                     (match (or (%current-target-system) (%current-system))
                       ("x86_64-linux" (values hash64 file64))
                       (_              (values hash32 file32)))))
         (origin
           (method url-fetch)
           (uri (string-append
                 "http://ftp-stud.fht-esslingen.de/pub/Mirrors/"
                 "eclipse/eclipse/downloads/drops4/R-" version
                 "-201606061100/swt-" version "-gtk-linux-" file ".zip"))
           (sha256 (base32 hash))))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "swt.jar"
       #:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (and (mkdir "swt")
                  (zero? (system* "unzip" source "-d" "swt"))
                  (chdir "swt")
                  (mkdir "src")
                  (zero? (system* "unzip" "src.zip" "-d" "src")))))
         ;; The classpath contains invalid icecat jars.  Since we don't need
         ;; anything other than the JDK on the classpath, we can simply unset
         ;; it.
         (add-after 'configure 'unset-classpath
           (lambda _ (unsetenv "CLASSPATH") #t))
         (add-before 'build 'build-native
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
               ;; Build shared libraries.  Users of SWT have to set the system
               ;; property swt.library.path to the "lib" directory of this
               ;; package output.
               (mkdir-p lib)
               (setenv "OUTPUT_DIR" lib)
               (with-directory-excursion "src"
                 (zero? (system* "bash" "build.sh"))))))
         (add-after 'install 'install-native
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
               (for-each (lambda (file)
                           (install-file file lib))
                         (find-files "." "\\.so$"))
               #t))))))
    (inputs
     `(("xulrunner" ,icecat)
       ("gtk" ,gtk+-2)
       ("libxtst" ,libxtst)
       ("libxt" ,libxt)
       ("mesa" ,mesa)
       ("glu" ,glu)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("unzip" ,unzip)))
    (home-page "https://www.eclipse.org/swt/")
    (synopsis "Widget toolkit for Java")
    (description
     "SWT is a widget toolkit for Java designed to provide efficient, portable
access to the user-interface facilities of the operating systems on which it
is implemented.")
    ;; SWT code is licensed under EPL1.0
    ;; Gnome and Gtk+ bindings contain code licensed under LGPLv2.1
    ;; Cairo bindings contain code under MPL1.1
    ;; XULRunner 1.9 bindings contain code under MPL2.0
    (license (list
              license:epl1.0
              license:mpl1.1
              license:mpl2.0
              license:lgpl2.1+))))

(define-public clojure
  (let* ((remove-archives '(begin
                             (for-each delete-file
                                       (find-files "." ".*\\.(jar|zip)"))
                             #t))
         (submodule (lambda (prefix version hash)
                      (origin
                        (method url-fetch)
                        (uri (string-append "https://github.com/clojure/"
                                            prefix version ".tar.gz"))
                        (sha256 (base32 hash))
                        (modules '((guix build utils)))
                        (snippet remove-archives)))))
    (package
      (name "clojure")
      (version "1.8.0")
      (source
       (origin
         (method url-fetch)
         (uri
          (string-append "http://repo1.maven.org/maven2/org/clojure/clojure/"
                         version "/clojure-" version ".zip"))
         (sha256
          (base32 "1nip095fz5c492sw15skril60i1vd21ibg6szin4jcvyy3xr6cym"))
         (modules '((guix build utils)))
         (snippet remove-archives)))
      (build-system ant-build-system)
      (arguments
       `(#:modules ((guix build ant-build-system)
                    (guix build utils)
                    (ice-9 ftw)
                    (ice-9 regex)
                    (srfi srfi-1)
                    (srfi srfi-26))
         #:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-submodule-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (for-each
                (lambda (name)
                  (mkdir-p name)
                  (with-directory-excursion name
                    (or (zero? (system* "tar"
                                        ;; Use xz for repacked tarball.
                                        "--xz"
                                        "--extract"
                                        "--verbose"
                                        "--file" (assoc-ref inputs name)
                                        "--strip-components=1"))
                        (error "failed to unpack tarball" name)))
                  (copy-recursively (string-append name "/src/main/clojure/")
                                    "src/clj/"))
                '("data-generators-src"
                  "java-classpath-src"
                  "test-check-src"
                  "test-generative-src"
                  "tools-namespace-src"
                  "tools-reader-src"))
               #t))
           ;; The javadoc target is not built by default.
           (add-after 'build 'build-doc
             (lambda _
               (zero? (system* "ant" "javadoc"))))
           ;; Needed since no install target is provided.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((java-dir (string-append (assoc-ref outputs "out")
                                              "/share/java/")))
                 ;; Install versioned to avoid collisions.
                 (install-file (string-append "clojure-" ,version ".jar")
                               java-dir)
                 #t)))
           ;; Needed since no install-doc target is provided.
           (add-after 'install 'install-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((doc-dir (string-append (assoc-ref outputs "out")
                                             "/share/doc/clojure-"
                                             ,version "/")))
                 (copy-recursively "doc/clojure" doc-dir)
                 (copy-recursively "target/javadoc/"
                                   (string-append doc-dir "javadoc/"))
                 (for-each (cut install-file <> doc-dir)
                           (filter (cut string-match
                                     ".*\\.(html|markdown|md|txt)"
                                     <>)
                                   (scandir "./")))
                 #t))))))
      ;; The native-inputs below are needed to run the tests.
      (native-inputs
       `(("data-generators-src"
          ,(submodule "data.generators/archive/data.generators-"
                      "0.1.2"
                      "0kki093jp4ckwxzfnw8ylflrfqs8b1i1wi9iapmwcsy328dmgzp1"))
         ("java-classpath-src"
          ,(submodule "java.classpath/archive/java.classpath-"
                      "0.2.3"
                      "0sjymly9xh1lkvwn5ygygpsfwz4dabblnlq0c9bx76rkvq62fyng"))
         ("test-check-src"
          ,(submodule "test.check/archive/test.check-"
                      "0.9.0"
                      "0p0mnyhr442bzkz0s4k5ra3i6l5lc7kp6ajaqkkyh4c2k5yck1md"))
         ("test-generative-src"
          ,(submodule "test.generative/archive/test.generative-"
                      "0.5.2"
                      "1pjafy1i7yblc7ixmcpfq1lfbyf3jaljvkgrajn70sws9xs7a9f8"))
         ("tools-namespace-src"
          ,(submodule "tools.namespace/archive/tools.namespace-"
                      "0.2.11"
                      "10baak8v0hnwz2hr33bavshm7y49mmn9zsyyms1dwjz45p5ymhy0"))
         ("tools-reader-src"
          ,(submodule "tools.reader/archive/tools.reader-"
                      "0.10.0"
                      "09i3lzbhr608h76mhdjm3932gg9xi8sflscla3c5f0v1nkc28cnr"))))
      (home-page "https://clojure.org/")
      (synopsis "Lisp dialect running on the JVM")
      (description "Clojure is a dynamic, general-purpose programming language,
combining the approachability and interactive development of a scripting
language with an efficient and robust infrastructure for multithreaded
programming.  Clojure is a compiled language, yet remains completely dynamic
– every feature supported by Clojure is supported at runtime.  Clojure
provides easy access to the Java frameworks, with optional type hints and type
inference, to ensure that calls to Java can avoid reflection.

Clojure is a dialect of Lisp, and shares with Lisp the code-as-data philosophy
and a powerful macro system.  Clojure is predominantly a functional programming
language, and features a rich set of immutable, persistent data structures.
When mutable state is needed, Clojure offers a software transactional memory
system and reactive Agent system that ensure clean, correct, multithreaded
designs.")
      ;; Clojure is licensed under EPL1.0
      ;; ASM bytecode manipulation library is licensed under BSD-3
      ;; Guava Murmur3 hash implementation is licensed under APL2.0
      ;; src/clj/repl.clj is licensed under CPL1.0
      ;;
      ;; See readme.html or readme.txt for details.
      (license (list license:epl1.0
                     license:bsd-3
                     license:asl2.0
                     license:cpl1.0)))))

(define-public ant
  (package
    (name "ant")
    ;; The 1.9.x series is the last that can be built with GCJ.  The 1.10.x
    ;; series requires Java 8.
    (version "1.9.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/ant/source/apache-ant-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "1k28mka0m3isy9yr8gz84kz1f3f879rwaxrd44vdn9xbfwvwk86n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (alist-cons-after
        'unpack 'remove-scripts
        ;; Remove bat / cmd scripts for DOS as well as the antRun and runant
        ;; wrappers.
        (lambda _
          (for-each delete-file
                    (find-files "src/script"
                                "(.*\\.(bat|cmd)|runant.*|antRun.*)")))
        (alist-replace
         'build
         (lambda _
           (setenv "JAVA_HOME" (string-append (assoc-ref %build-inputs "gcj")
                                              "/lib/jvm"))
           ;; Disable tests to avoid dependency on hamcrest-core, which needs
           ;; Ant to build.  This is necessary in addition to disabling the
           ;; "check" phase, because the dependency on "test-jar" would always
           ;; result in the tests to be run.
           (substitute* "build.xml"
             (("depends=\"jars,test-jar\"") "depends=\"jars\""))
           (zero? (system* "bash" "bootstrap.sh"
                           (string-append "-Ddist.dir="
                                          (assoc-ref %outputs "out")))))
         (alist-delete
          'configure
          (alist-delete 'install %standard-phases))))))
    (native-inputs
     `(("gcj" ,gcj)))
    (home-page "http://ant.apache.org")
    (synopsis "Build tool for Java")
    (description
     "Ant is a platform-independent build tool for Java.  It is similar to
make but is implemented using the Java language, requires the Java platform,
and is best suited to building Java projects.  Ant uses XML to describe the
build process and its dependencies, whereas Make uses Makefile format.")
    (license license:asl2.0)))

(define-public icedtea-7
  (let* ((version "2.6.9")
         (drop (lambda (name hash)
                 (origin
                   (method url-fetch)
                   (uri (string-append
                         "http://icedtea.classpath.org/download/drops/"
                         "/icedtea7/" version "/" name ".tar.bz2"))
                   (sha256 (base32 hash))))))
    (package
      (name "icedtea")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "http://icedtea.wildebeest.org/download/source/icedtea-"
                      version ".tar.xz"))
                (sha256
                 (base32
                  "1slmajiakq7sk137vgqq9c93r5s620a46lw2jwbnzxhysjw3wkwf"))
                (modules '((guix build utils)))
                (snippet
                 '(substitute* "Makefile.in"
                    ;; link against libgcj to avoid linker error
                    (("-o native-ecj")
                     "-lgcj -o native-ecj")
                    ;; do not leak information about the build host
                    (("DISTRIBUTION_ID=\"\\$\\(DIST_ID\\)\"")
                     "DISTRIBUTION_ID=\"\\\"guix\\\"\"")))))
      (build-system gnu-build-system)
      (outputs '("out"   ; Java Runtime Environment
                 "jdk"   ; Java Development Kit
                 "doc")) ; all documentation
      (arguments
       `(;; There are many test failures.  Some are known to
         ;; fail upstream, others relate to not having an X
         ;; server running at test time, yet others are a
         ;; complete mystery to me.

         ;; hotspot:   passed: 241; failed: 45; error: 2
         ;; langtools: passed: 1,934; failed: 26
         ;; jdk:       unknown
         #:tests? #f

         ;; The DSOs use $ORIGIN to refer to each other, but (guix build
         ;; gremlin) doesn't support it yet, so skip this phase.
         #:validate-runpath? #f

         ;; Apparently, the C locale is needed for some of the tests.
         #:locale "C"

         #:modules ((guix build utils)
                    (guix build gnu-build-system)
                    (ice-9 match)
                    (ice-9 popen)
                    (ice-9 rdelim)
                    (srfi srfi-19)
                    (srfi srfi-26))

         #:configure-flags
         (let* ((gcjdir (assoc-ref %build-inputs "gcj"))
                (ecj    (string-append gcjdir "/share/java/ecj.jar"))
                (jdk    (string-append gcjdir "/lib/jvm/"))
                (gcj    (string-append gcjdir "/bin/gcj")))
           ;; TODO: package pcsc and sctp, and add to inputs
           `("--disable-system-pcsc"
             "--disable-system-sctp"
             "--enable-bootstrap"
             "--enable-nss"
             "--without-rhino"
             "--disable-downloading"
             "--disable-tests"        ;they are run in the check phase instead
             "--with-openjdk-src-dir=./openjdk.src"
             ,(string-append "--with-javac=" jdk "/bin/javac")
             ,(string-append "--with-ecj-jar=" ecj)
             ,(string-append "--with-gcj=" gcj)
             ,(string-append "--with-jdk-home=" jdk)
             ,(string-append "--with-java=" jdk "/bin/java")))

         #:phases
         (modify-phases %standard-phases
           (replace 'unpack
             (lambda* (#:key source inputs #:allow-other-keys)
               (let ((target (string-append "icedtea-" ,version))
                     (unpack (lambda* (name #:optional dir)
                               (let ((dir (or dir
                                              (string-drop-right name 5))))
                                 (mkdir dir)
                                 (zero? (system* "tar" "xvf"
                                                 (assoc-ref inputs name)
                                                 "-C" dir
                                                 "--strip-components=1"))))))
                 (mkdir target)
                 (and
                  (zero? (system* "tar" "xvf" source
                                  "-C" target "--strip-components=1"))
                  (chdir target)
                  (unpack "openjdk-src" "openjdk.src")
                  (with-directory-excursion "openjdk.src"
                    (for-each unpack
                              (filter (cut string-suffix? "-drop" <>)
                                      (map (match-lambda
                                             ((name . _) name))
                                           inputs))))
                  #t))))
           (add-after 'unpack 'fix-x11-extension-include-path
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "openjdk.src/jdk/make/sun/awt/mawt.gmk"
                 (((string-append "\\$\\(firstword \\$\\(wildcard "
                                  "\\$\\(OPENWIN_HOME\\)"
                                  "/include/X11/extensions\\).*$"))
                  (string-append (assoc-ref inputs "libxrender")
                                 "/include/X11/extensions"
                                 " -I" (assoc-ref inputs "libxtst")
                                 "/include/X11/extensions"
                                 " -I" (assoc-ref inputs "libxinerama")
                                 "/include/X11/extensions"))
                 (("\\$\\(wildcard /usr/include/X11/extensions\\)\\)") ""))
               #t))
           (add-after 'unpack 'patch-paths
             (lambda _
               ;; buildtree.make generates shell scripts, so we need to replace
               ;; the generated shebang
               (substitute* '("openjdk.src/hotspot/make/linux/makefiles/buildtree.make")
                 (("/bin/sh") (which "bash")))

               (let ((corebin (string-append
                               (assoc-ref %build-inputs "coreutils") "/bin/"))
                     (binbin  (string-append
                               (assoc-ref %build-inputs "binutils") "/bin/"))
                     (grepbin (string-append
                               (assoc-ref %build-inputs "grep") "/bin/")))
                 (substitute* '("openjdk.src/jdk/make/common/shared/Defs-linux.gmk"
                                "openjdk.src/corba/make/common/shared/Defs-linux.gmk")
                   (("UNIXCOMMAND_PATH  = /bin/")
                    (string-append "UNIXCOMMAND_PATH = " corebin))
                   (("USRBIN_PATH  = /usr/bin/")
                    (string-append "USRBIN_PATH = " corebin))
                   (("DEVTOOLS_PATH *= */usr/bin/")
                    (string-append "DEVTOOLS_PATH = " corebin))
                   (("COMPILER_PATH *= */usr/bin/")
                    (string-append "COMPILER_PATH = "
                                   (assoc-ref %build-inputs "gcc") "/bin/"))
                   (("DEF_OBJCOPY *=.*objcopy")
                    (string-append "DEF_OBJCOPY = " (which "objcopy"))))

                 ;; fix path to alsa header
                 (substitute* "openjdk.src/jdk/make/common/shared/Sanity.gmk"
                   (("ALSA_INCLUDE=/usr/include/alsa/version.h")
                    (string-append "ALSA_INCLUDE="
                                   (assoc-ref %build-inputs "alsa-lib")
                                   "/include/alsa/version.h")))

                 ;; fix hard-coded utility paths
                 (substitute* '("openjdk.src/jdk/make/common/shared/Defs-utils.gmk"
                                "openjdk.src/corba/make/common/shared/Defs-utils.gmk")
                   (("ECHO *=.*echo")
                    (string-append "ECHO = " (which "echo")))
                   (("^GREP *=.*grep")
                    (string-append "GREP = " (which "grep")))
                   (("EGREP *=.*egrep")
                    (string-append "EGREP = " (which "egrep")))
                   (("CPIO *=.*cpio")
                    (string-append "CPIO = " (which "cpio")))
                   (("READELF *=.*readelf")
                    (string-append "READELF = " (which "readelf")))
                   (("^ *AR *=.*ar")
                    (string-append "AR = " (which "ar")))
                   (("^ *TAR *=.*tar")
                    (string-append "TAR = " (which "tar")))
                   (("AS *=.*as")
                    (string-append "AS = " (which "as")))
                   (("LD *=.*ld")
                    (string-append "LD = " (which "ld")))
                   (("STRIP *=.*strip")
                    (string-append "STRIP = " (which "strip")))
                   (("NM *=.*nm")
                    (string-append "NM = " (which "nm")))
                   (("^SH *=.*sh")
                    (string-append "SH = " (which "bash")))
                   (("^FIND *=.*find")
                    (string-append "FIND = " (which "find")))
                   (("LDD *=.*ldd")
                    (string-append "LDD = " (which "ldd")))
                   (("NAWK *=.*(n|g)awk")
                    (string-append "NAWK = " (which "gawk")))
                   (("XARGS *=.*xargs")
                    (string-append "XARGS = " (which "xargs")))
                   (("UNZIP *=.*unzip")
                    (string-append "UNZIP = " (which "unzip")))
                   (("ZIPEXE *=.*zip")
                    (string-append "ZIPEXE = " (which "zip")))
                   (("SED *=.*sed")
                    (string-append "SED = " (which "sed"))))

                 ;; Some of these timestamps cause problems as they are more than
                 ;; 10 years ago, failing the build process.
                 (substitute*
                     "openjdk.src/jdk/src/share/classes/java/util/CurrencyData.properties"
                   (("AZ=AZM;2005-12-31-20-00-00;AZN") "AZ=AZN")
                   (("MZ=MZM;2006-06-30-22-00-00;MZN") "MZ=MZN")
                   (("RO=ROL;2005-06-30-21-00-00;RON") "RO=RON")
                   (("TR=TRL;2004-12-31-22-00-00;TRY") "TR=TRY")))
               #t))
           (add-before 'configure 'set-additional-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (let ( ;; Get target-specific include directory so that
                     ;; libgcj-config.h is found when compiling hotspot.
                     (gcjinclude (let* ((port (open-input-pipe "gcj -print-file-name=include"))
                                        (str  (read-line port)))
                                   (close-pipe port)
                                   str)))
                 (substitute* "openjdk.src/jdk/make/common/shared/Sanity.gmk"
                   (("ALSA_INCLUDE=/usr/include/alsa/version.h")
                    (string-append "ALSA_INCLUDE="
                                   (assoc-ref inputs "alsa-lib")
                                   "/include/alsa/version.h")))
                 (setenv "CC" "gcc")
                 (setenv "CPATH"
                         (string-append gcjinclude ":"
                                        (assoc-ref inputs "libxcomposite")
                                        "/include/X11/extensions" ":"
                                        (assoc-ref inputs "libxrender")
                                        "/include/X11/extensions" ":"
                                        (assoc-ref inputs "libxtst")
                                        "/include/X11/extensions" ":"
                                        (assoc-ref inputs "libxinerama")
                                        "/include/X11/extensions" ":"
                                        (or (getenv "CPATH") "")))
                 (setenv "ALT_OBJCOPY" (which "objcopy"))
                 (setenv "ALT_CUPS_HEADERS_PATH"
                         (string-append (assoc-ref inputs "cups")
                                        "/include"))
                 (setenv "ALT_FREETYPE_HEADERS_PATH"
                         (string-append (assoc-ref inputs "freetype")
                                        "/include"))
                 (setenv "ALT_FREETYPE_LIB_PATH"
                         (string-append (assoc-ref inputs "freetype")
                                        "/lib")))
               #t))
           (add-before 'check 'fix-test-framework
             (lambda _
               ;; Fix PATH in test environment
               (substitute* "test/jtreg/com/sun/javatest/regtest/Main.java"
                 (("PATH=/bin:/usr/bin")
                  (string-append "PATH=" (getenv "PATH"))))
               (substitute* "test/jtreg/com/sun/javatest/util/SysEnv.java"
                 (("/usr/bin/env") (which "env")))
               (substitute* "openjdk.src/hotspot/test/test_env.sh"
                 (("/bin/rm") (which "rm"))
                 (("/bin/cp") (which "cp"))
                 (("/bin/mv") (which "mv")))
               #t))
           (add-before 'check 'fix-hotspot-tests
             (lambda _
               (with-directory-excursion "openjdk.src/hotspot/test/"
                 (substitute* "jprt.config"
                   (("PATH=\"\\$\\{path4sdk\\}\"")
                    (string-append "PATH=" (getenv "PATH")))
                   (("make=/usr/bin/make")
                    (string-append "make=" (which "make"))))
                 (substitute* '("runtime/6626217/Test6626217.sh"
                                "runtime/7110720/Test7110720.sh")
                   (("/bin/rm") (which "rm"))
                   (("/bin/cp") (which "cp"))
                   (("/bin/mv") (which "mv"))))
               #t))
           (add-before 'check 'fix-jdk-tests
             (lambda _
               (with-directory-excursion "openjdk.src/jdk/test/"
                 (substitute* "com/sun/jdi/JdbReadTwiceTest.sh"
                   (("/bin/pwd") (which "pwd")))
                 (substitute* "com/sun/jdi/ShellScaffold.sh"
                   (("/bin/kill") (which "kill")))
                 (substitute* "start-Xvfb.sh"
                   ;;(("/usr/bin/X11/Xvfb") (which "Xvfb"))
                   (("/usr/bin/nohup")    (which "nohup")))
                 (substitute* "javax/security/auth/Subject/doAs/Test.sh"
                   (("/bin/rm") (which "rm")))
                 (substitute* "tools/launcher/MultipleJRE.sh"
                   (("echo \"#!/bin/sh\"")
                    (string-append "echo \"#!" (which "rm") "\""))
                   (("/usr/bin/zip") (which "zip")))
                 (substitute* "com/sun/jdi/OnThrowTest.java"
                   (("#!/bin/sh") (string-append "#!" (which "sh"))))
                 (substitute* "java/lang/management/OperatingSystemMXBean/GetSystemLoadAverage.java"
                   (("/usr/bin/uptime") (which "uptime")))
                 (substitute* "java/lang/ProcessBuilder/Basic.java"
                   (("/usr/bin/env") (which "env"))
                   (("/bin/false") (which "false"))
                   (("/bin/true") (which "true"))
                   (("/bin/cp") (which "cp"))
                   (("/bin/sh") (which "sh")))
                 (substitute* "java/lang/ProcessBuilder/FeelingLucky.java"
                   (("/bin/sh") (which "sh")))
                 (substitute* "java/lang/ProcessBuilder/Zombies.java"
                   (("/usr/bin/perl") (which "perl"))
                   (("/bin/ps") (which "ps"))
                   (("/bin/true") (which "true")))
                 (substitute* "java/lang/Runtime/exec/ConcurrentRead.java"
                   (("/usr/bin/tee") (which "tee")))
                 (substitute* "java/lang/Runtime/exec/ExecWithDir.java"
                   (("/bin/true") (which "true")))
                 (substitute* "java/lang/Runtime/exec/ExecWithInput.java"
                   (("/bin/cat") (which "cat")))
                 (substitute* "java/lang/Runtime/exec/ExitValue.java"
                   (("/bin/sh") (which "sh"))
                   (("/bin/true") (which "true"))
                   (("/bin/kill") (which "kill")))
                 (substitute* "java/lang/Runtime/exec/LotsOfDestroys.java"
                   (("/usr/bin/echo") (which "echo")))
                 (substitute* "java/lang/Runtime/exec/LotsOfOutput.java"
                   (("/usr/bin/cat") (which "cat")))
                 (substitute* "java/lang/Runtime/exec/SleepyCat.java"
                   (("/bin/cat") (which "cat"))
                   (("/bin/sleep") (which "sleep"))
                   (("/bin/sh") (which "sh")))
                 (substitute* "java/lang/Runtime/exec/StreamsSurviveDestroy.java"
                   (("/bin/cat") (which "cat")))
                 (substitute* "java/rmi/activation/CommandEnvironment/SetChildEnv.java"
                   (("/bin/chmod") (which "chmod")))
                 (substitute* "java/util/zip/ZipFile/Assortment.java"
                   (("/bin/sh") (which "sh"))))
               #t))
           (replace 'check
             (lambda _
               ;; The "make check-*" targets always return zero, so we need to
               ;; check for errors in the associated log files to determine
               ;; whether any tests have failed.
               (use-modules (ice-9 rdelim))
               (let* ((error-pattern (make-regexp "^(Error|FAILED):.*"))
                      (checker (lambda (port)
                                 (let loop ()
                                   (let ((line (read-line port)))
                                     (cond
                                      ((eof-object? line) #t)
                                      ((regexp-exec error-pattern line) #f)
                                      (else (loop)))))))
                      (run-test (lambda (test)
                                  (system* "make" test)
                                  (call-with-input-file
                                      (string-append "test/" test ".log")
                                    checker))))
                 (or #t                 ; skip tests
                     (and (run-test "check-hotspot")
                          (run-test "check-langtools")
                          (run-test "check-jdk"))))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((doc (string-append (assoc-ref outputs "doc")
                                         "/share/doc/icedtea"))
                     (jre (assoc-ref outputs "out"))
                     (jdk (assoc-ref outputs "jdk")))
                 (copy-recursively "openjdk.build/docs" doc)
                 (copy-recursively "openjdk.build/j2re-image" jre)
                 (copy-recursively "openjdk.build/j2sdk-image" jdk))
               #t))
           ;; By default IcedTea only generates an empty keystore.  In order to
           ;; be able to use certificates in Java programs we need to generate a
           ;; keystore from a set of certificates.  For convenience we use the
           ;; certificates from the nss-certs package.
           (add-after 'install 'install-keystore
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((keystore  "cacerts")
                      (certs-dir (string-append (assoc-ref inputs "nss-certs")
                                                "/etc/ssl/certs"))
                      (keytool   (string-append (assoc-ref outputs "jdk")
                                                "/bin/keytool")))
                 (define (extract-cert file target)
                   (call-with-input-file file
                     (lambda (in)
                       (call-with-output-file target
                         (lambda (out)
                           (let loop ((line (read-line in 'concat))
                                      (copying? #f))
                             (cond
                              ((eof-object? line) #t)
                              ((string-prefix? "-----BEGIN" line)
                               (display line out)
                               (loop (read-line in 'concat) #t))
                              ((string-prefix? "-----END" line)
                               (display line out)
                               #t)
                              (else
                               (when copying? (display line out))
                               (loop (read-line in 'concat) copying?)))))))))
                 (define (import-cert cert)
                   (format #t "Importing certificate ~a\n" (basename cert))
                   (let ((temp "tmpcert"))
                     (extract-cert cert temp)
                     (let ((port (open-pipe* OPEN_WRITE keytool
                                             "-import"
                                             "-alias" (basename cert)
                                             "-keystore" keystore
                                             "-storepass" "changeit"
                                             "-file" temp)))
                       (display "yes\n" port)
                       (when (not (zero? (status:exit-val (close-pipe port))))
                         (format #t "failed to import ~a\n" cert)))
                     (delete-file temp)))

                 ;; This is necessary because the certificate directory contains
                 ;; files with non-ASCII characters in their names.
                 (setlocale LC_ALL "en_US.utf8")
                 (setenv "LC_ALL" "en_US.utf8")

                 (for-each import-cert (find-files certs-dir "\\.pem$"))
                 (mkdir-p (string-append (assoc-ref outputs "out")
                                         "/lib/security"))
                 (mkdir-p (string-append (assoc-ref outputs "jdk")
                                         "/jre/lib/security"))

                 ;; The cacerts files we are going to overwrite are chmod'ed as
                 ;; read-only (444) in icedtea-8 (which derives from this
                 ;; package).  We have to change this so we can overwrite them.
                 (chmod (string-append (assoc-ref outputs "out")
                                       "/lib/security/" keystore) #o644)
                 (chmod (string-append (assoc-ref outputs "jdk")
                                       "/jre/lib/security/" keystore) #o644)

                 (install-file keystore
                               (string-append (assoc-ref outputs "out")
                                              "/lib/security"))
                 (install-file keystore
                               (string-append (assoc-ref outputs "jdk")
                                              "/jre/lib/security"))
                 #t))))))
      (native-inputs
       `(("openjdk-src"
          ,(drop "openjdk"
                 "08a4d1sg5m9l99lc7gafc7dmzmf4d8jvij5pffxv8rf6pk7psk24"))
         ("corba-drop"
          ,(drop "corba"
                 "12br49cfrqgvms0bnaij7fvnakvb6q8dlpqja64rg5q5r3x4gps8"))
         ("jaxp-drop"
          ,(drop "jaxp"
                 "07v2y3pll6z2wma94qilgffwyn2n4jna01mrhqwkb27whfpjfkmz"))
         ("jaxws-drop"
          ,(drop "jaxws"
                 "18rw64jjpq14v56d0q1xvz8knl0kf02rcday7fvlaxrbbj19km55"))
         ("jdk-drop"
          ,(drop "jdk"
                 "1ig7xipi3vzm6cphy5fdraxi72p27xsg2qb51yqx9qwsmlrv1zj4"))
         ("langtools-drop"
          ,(drop "langtools"
                 "0sn9qv9nnhaan2smbhrv54lfhwsjhgd3b3h736p5d2hzpw8kicry"))
         ("hotspot-drop"
          ,(drop "hotspot"
                 "16ijxy8br8dla339m4i90wr9xpf7s8z3nrhfyxm7jahr8injpzyl"))
         ("ant" ,ant)
         ("attr" ,attr)
         ("autoconf" ,autoconf)
         ("automake" ,automake)
         ("coreutils" ,coreutils)
         ("diffutils" ,diffutils)       ;for tests
         ("gawk" ,gawk)
         ("grep" ,grep)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)
         ("wget" ,wget)
         ("which" ,which)
         ("cpio" ,cpio)
         ("zip" ,zip)
         ("unzip" ,unzip)
         ("fastjar" ,fastjar)
         ("libxslt" ,libxslt)           ;for xsltproc
         ("nss-certs" ,nss-certs)
         ("perl" ,perl)
         ("procps" ,procps) ;for "free", even though I'm not sure we should use it
         ("gcj" ,gcj)))
      (inputs
       `(("alsa-lib" ,alsa-lib)
         ("cups" ,cups)
         ("libx11" ,libx11)
         ("libxcomposite" ,libxcomposite)
         ("libxt" ,libxt)
         ("libxtst" ,libxtst)
         ("libxi" ,libxi)
         ("libxinerama" ,libxinerama)
         ("libxrender" ,libxrender)
         ("libjpeg" ,libjpeg)
         ("libpng" ,libpng)
         ("mit-krb5" ,mit-krb5)
         ("nss" ,nss)
         ("giflib" ,giflib)
         ("fontconfig" ,fontconfig)
         ("freetype" ,freetype)
         ("lcms" ,lcms)
         ("zlib" ,zlib)
         ("gtk" ,gtk+-2)))
      (home-page "http://icedtea.classpath.org")
      (synopsis "Java development kit")
      (description
       "This package provides the Java development kit OpenJDK built with the
IcedTea build harness.")
      ;; IcedTea is released under the GPL2 + Classpath exception, which is the
      ;; same license as both GNU Classpath and OpenJDK.
      (license license:gpl2+))))

(define-public icedtea-8
  (let* ((version "3.3.0")
         (drop (lambda (name hash)
                 (origin
                   (method url-fetch)
                   (uri (string-append
                         "http://icedtea.classpath.org/download/drops/"
                         "/icedtea8/" version "/" name ".tar.xz"))
                   (sha256 (base32 hash))))))
    (package (inherit icedtea-7)
      (version "3.3.0")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "http://icedtea.wildebeest.org/download/source/icedtea-"
                      version ".tar.xz"))
                (sha256
                 (base32
                  "02vmxa6gc6gizcri1fy797qmmm9y77vgi7gy9pwkk4agcw4zyr5p"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (substitute* "acinclude.m4"
                      ;; Do not embed build time
                      (("(DIST_ID=\"Custom build).*$" _ prefix)
                       (string-append prefix "\"\n"))
                      ;; Do not leak information about the build host
                      (("DIST_NAME=\"\\$build_os\"")
                       "DIST_NAME=\"guix\""))
                    #t))))
      (arguments
       (substitute-keyword-arguments (package-arguments icedtea-7)
         ((#:configure-flags flags)
          `(let ((jdk (assoc-ref %build-inputs "jdk")))
             `(;;"--disable-bootstrap"
               "--enable-bootstrap"
               "--enable-nss"
               "--disable-downloading"
               "--disable-system-pcsc"
               "--disable-system-sctp"
               "--disable-tests"      ;they are run in the check phase instead
               "--with-openjdk-src-dir=./openjdk.src"
               ,(string-append "--with-jdk-home=" jdk))))
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'fix-x11-extension-include-path)
             (delete 'patch-paths)
             (delete 'set-additional-paths)
             (delete 'patch-patches)
             (add-after 'unpack 'patch-jni-libs
               ;; Hardcode dynamically loaded libraries.
               (lambda _
                 (let* ((library-path (search-path-as-string->list
                                       (getenv "LIBRARY_PATH")))
                        (find-library (lambda (name)
                                        (search-path
                                         library-path
                                         (string-append "lib" name ".so")))))
                   (for-each
                    (lambda (file)
                      (catch 'encoding-error
                        (lambda ()
                          (substitute* file
                            (("VERSIONED_JNI_LIB_NAME\\(\"(.*)\", \"(.*)\"\\)"
                              _ name version)
                             (format #f "\"~a\""  (find-library name)))
                            (("JNI_LIB_NAME\\(\"(.*)\"\\)" _ name)
                             (format #f "\"~a\"" (find-library name)))))
                        (lambda _
                          ;; Those are safe to skip.
                          (format (current-error-port)
                                  "warning: failed to substitute: ~a~%"
                                  file))))
                    (find-files "openjdk.src/jdk/src/solaris/native"
                                "\\.c|\\.h"))
                   #t)))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((doc (string-append (assoc-ref outputs "doc")
                                           "/share/doc/icedtea"))
                       (jre (assoc-ref outputs "out"))
                       (jdk (assoc-ref outputs "jdk")))
                   (copy-recursively "openjdk.build/docs" doc)
                   (copy-recursively "openjdk.build/images/j2re-image" jre)
                   (copy-recursively "openjdk.build/images/j2sdk-image" jdk)
                   #t)))))))
      (native-inputs
       `(("jdk" ,icedtea-7 "jdk")
         ("openjdk-src"
          ,(drop "openjdk"
                 "0889n19w6rvpzxgmmk9hlgzdh9ya95qkc2ajgpnzr3h69g15nz48"))
         ("corba-drop"
          ,(drop "corba"
                 "0qcb72hhlsjgp6h9wd048qgyc88b7lfnxyc51xfyav0nhpfjnj8r"))
         ("jaxp-drop"
          ,(drop "jaxp"
                 "1vyc7dw10x5k45jmi348y8min6sg651ns12zzn30fjzhpfi36nds"))
         ("jaxws-drop"
          ,(drop "jaxws"
                 "1dki6p39z1ms94cjvj5hd9q75q75g244c0xib82pma3q74jg6hx4"))
         ("jdk-drop"
          ,(drop "jdk"
                 "17czby3nylcglp7l3d90a4pz1izc1sslifv8hrmynm9hn4m9d3k8"))
         ("langtools-drop"
          ,(drop "langtools"
                 "1h4azc21k58g9gn2y686wrvn9ahgac0ii7jhrrrmb5c1kjs0y2qv"))
         ("hotspot-drop"
          ,(drop "hotspot"
                 "12bfgwhrjfhgj6a2dsysdwhirg0jx88pi44y7s8a1bdan1mp03r8"))
         ("nashorn-drop"
          ,(drop "nashorn"
                 "0bg9r16jffc64fhyczn4jpx7bkfw7w62prw65mh66vshqk4lbh0f"))
         ("shenandoah-drop"
          ,(drop "shenandoah"
                 "0abjlsvz669i06mlks28wnh11mm55y5613990pn5j7hfbw8a34q5"))
         ,@(fold alist-delete (package-native-inputs icedtea-7)
                 '("gcj" "openjdk-src" "corba-drop" "jaxp-drop" "jaxws-drop"
                   "jdk-drop" "langtools-drop" "hotspot-drop")))))))

(define-public icedtea icedtea-7)

(define-public java-xz
  (package
   (name "java-xz")
   (version "1.6")
   (source (origin
     (method url-fetch)
     (uri (string-append "http://tukaani.org/xz/xz-java-" version ".zip"))
     (sha256
      (base32
       "1z3p1ri1gvl07inxn0agx44ck8n7wrzfmvkz8nbq3njn8r9wba8x"))))
   (build-system ant-build-system)
   (arguments
    `(#:tests? #f ; There are no tests to run.
      #:jar-name ,(string-append "xz-" version  ".jar")
      #:phases
      (modify-phases %standard-phases
        ;; The unpack phase enters the "maven" directory by accident.
        (add-after 'unpack 'chdir
          (lambda _ (chdir "..") #t)))))
   (native-inputs
    `(("unzip" ,unzip)))
   (home-page "http://tukaani.org/xz/java.html")
   (synopsis "Implementation of XZ data compression in pure Java")
   (description "This library aims to be a complete implementation of XZ data
compression in pure Java.  Single-threaded streamed compression and
decompression and random access decompression have been fully implemented.")
   (license license:public-domain)))

;; java-hamcrest-core uses qdox version 1.12.  We package this version instead
;; of the latest release.
(define-public java-qdox-1.12
  (package
    (name "java-qdox")
    (version "1.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/"
                                  "com/thoughtworks/qdox/qdox/" version
                                  "/qdox-" version "-sources.jar"))
              (sha256
               (base32
                "0hlfbqq2avf5s26wxkksqmkdyk6zp9ggqn37c468m96mjv0n9xfl"))))
    (build-system ant-build-system)
    (arguments
     `(;; Tests require junit
       #:tests? #f
       #:jar-name "qdox.jar"
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (mkdir "src")
             (with-directory-excursion "src"
               (zero? (system* "jar" "-xf" source)))))
         ;; At this point we don't have junit, so we must remove the API
         ;; tests.
         (add-after 'unpack 'delete-tests
           (lambda _
             (delete-file-recursively "src/com/thoughtworks/qdox/junit")
             #t)))))
    (home-page "http://qdox.codehaus.org/")
    (synopsis "Parse definitions from Java source files")
    (description
     "QDox is a high speed, small footprint parser for extracting
class/interface/method definitions from source files complete with JavaDoc
@code{@@tags}.  It is designed to be used by active code generators or
documentation tools.")
    (license license:asl2.0)))

(define-public java-jarjar
  (package
    (name "java-jarjar")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://storage.googleapis.com/google-code-archive-downloads/v2/"
                    "code.google.com/jarjar/jarjar-src-" version ".zip"))
              (sha256
               (base32
                "1v8irhni9cndcw1l1wxqgry013s2kpj0qqn57lj2ji28xjq8ndjl"))))
    (build-system ant-build-system)
    (arguments
     `(;; Tests require junit, which ultimately depends on this package.
       #:tests? #f
       #:build-target "jar"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/share/java")))
               (install-file (string-append "dist/jarjar-" ,version ".jar")
                             target))
             #t)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://code.google.com/archive/p/jarjar/")
    (synopsis "Repackage Java libraries")
    (description
     "Jar Jar Links is a utility that makes it easy to repackage Java
libraries and embed them into your own distribution.  Jar Jar Links includes
an Ant task that extends the built-in @code{jar} task.")
    (license license:asl2.0)))

(define-public java-hamcrest-core
  (package
    (name "java-hamcrest-core")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/hamcrest/JavaHamcrest/"
                                  "archive/hamcrest-java-" version ".tar.gz"))
              (sha256
               (base32
                "11g0s105fmwzijbv08lx8jlb521yravjmxnpgdx08fvg1kjivhva"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled thirds-party jar archives.
                  (delete-file-recursively "lib")
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; Tests require junit
       #:modules ((guix build ant-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:make-flags (list (string-append "-Dversion=" ,version))
       #:build-target "core"
       #:phases
       (modify-phases %standard-phases
         ;; Disable unit tests, because they require junit, which requires
         ;; hamcrest-core.  We also give a fixed value to the "Built-Date"
         ;; attribute from the manifest for reproducibility.
         (add-before 'configure 'patch-build.xml
           (lambda _
             (substitute* "build.xml"
               (("unit-test, ") "")
               (("\\$\\{build.timestamp\\}") "guix"))
             #t))
         ;; Java's "getMethods()" returns methods in an unpredictable order.
         ;; To make the output of the generated code deterministic we must
         ;; sort the array of methods.
         (add-after 'unpack 'make-method-order-deterministic
           (lambda _
             (substitute* "hamcrest-generator/src/main/java/org/hamcrest/generator/ReflectiveFactoryReader.java"
               (("import java\\.util\\.Iterator;" line)
                (string-append line "\n"
                               "import java.util.Arrays; import java.util.Comparator;"))
               (("allMethods = cls\\.getMethods\\(\\);" line)
                (string-append "_" line
                               "
private Method[] getSortedMethods() {
  Arrays.sort(_allMethods, new Comparator<Method>() {
    @Override
    public int compare(Method a, Method b) {
      return a.toString().compareTo(b.toString());
    }
  });
  return _allMethods;
}

private Method[] allMethods = getSortedMethods();")))))
         (add-before 'build 'do-not-use-bundled-qdox
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               (("lib/generator/qdox-1.12.jar")
                (string-append (assoc-ref inputs "java-qdox-1.12")
                               "/share/java/qdox.jar")))
             #t))
         ;; build.xml searches for .jar files in this directoy, which
         ;; we remove  from the source archive.
         (add-before 'build 'create-dummy-directories
           (lambda _
             (mkdir-p "lib/integration")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((target (string-append (assoc-ref outputs "out")
                                           "/share/java/"))
                    (version-suffix ,(string-append "-" version ".jar"))
                    (install-without-version-suffix
                     (lambda (jar)
                       (copy-file jar
                                  (string-append target
                                                 (basename jar version-suffix)
                                                 ".jar")))))
               (mkdir-p target)
               (for-each
                install-without-version-suffix
                (find-files "build"
                            (lambda (name _)
                              (and (string-suffix? ".jar" name)
                                   (not (string-suffix? "-sources.jar" name)))))))
             #t)))))
    (native-inputs
     `(("java-qdox-1.12" ,java-qdox-1.12)
       ("java-jarjar" ,java-jarjar)))
    (home-page "http://hamcrest.org/")
    (synopsis "Library of matchers for building test expressions")
    (description
     "This package provides a library of matcher objects (also known as
constraints or predicates) allowing @code{match} rules to be defined
declaratively, to be used in other frameworks.  Typical scenarios include
testing frameworks, mocking libraries and UI validation rules.")
    (license license:bsd-2)))

(define-public java-junit
  (package
    (name "java-junit")
    (version "4.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/junit-team/junit/"
                                  "archive/r" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "090dn5v1vs0b3acyaqc0gjf6p8lmd2h24wfzsbq7sly6b214anws"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled jar archives.
                  (delete-file-recursively "lib")
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:jar-name "junit.jar"))
    (inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://junit.org/")
    (synopsis "Test framework for Java")
    (description
     "JUnit is a simple framework to write repeatable tests for Java projects.
JUnit provides assertions for testing expected results, test fixtures for
sharing common test data, and test runners for running tests.")
    (license license:epl1.0)))

(define-public java-plexus-utils
  (package
    (name "java-plexus-utils")
    (version "3.0.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/codehaus-plexus/"
                                  "plexus-utils/archive/plexus-utils-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1mlwpc6fms24slygv5yvi6fi9hcha2fh0v73p5znpi78bg36i2js"))))
    (build-system ant-build-system)
    ;; FIXME: The default build.xml does not include a target to install
    ;; javadoc files.
    (arguments
     `(#:jar-name "plexus-utils.jar"
       #:source-dir "src/main"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-reference-to-/bin-and-/usr
           (lambda _
             (substitute* "src/main/java/org/codehaus/plexus/util/\
cli/shell/BourneShell.java"
               (("/bin/sh") (which "sh"))
               (("/usr/")   (getcwd)))
             #t))
         (add-after 'unpack 'fix-or-disable-broken-tests
           (lambda _
             (with-directory-excursion "src/test/java/org/codehaus/plexus/util"
               (substitute* '("cli/CommandlineTest.java"
                              "cli/shell/BourneShellTest.java")
                 (("/bin/sh")   (which "sh"))
                 (("/bin/echo") (which "echo")))

               ;; This test depends on MavenProjectStub, but we don't have
               ;; a package for Maven.
               (delete-file "introspection/ReflectionValueExtractorTest.java")

               ;; FIXME: The command line tests fail, maybe because they use
               ;; absolute paths.
               (delete-file "cli/CommandlineTest.java"))
             #t)))))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "http://codehaus-plexus.github.io/plexus-utils/")
    (synopsis "Common utilities for the Plexus framework")
    (description "This package provides various Java utility classes for the
Plexus framework to ease working with strings, files, command lines, XML and
more.")
    (license license:asl2.0)))

(define-public java-plexus-interpolation
  (package
    (name "java-plexus-interpolation")
    (version "1.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/codehaus-plexus/"
                                  "plexus-interpolation/archive/"
                                  "plexus-interpolation-" version ".tar.gz"))
              (sha256
               (base32
                "1w79ljwk42ymrgy8kqxq4l82pgdj6287gabpfnpkyzbrnclsnfrp"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-interpolation.jar"
       #:source-dir "src/main"))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://codehaus-plexus.github.io/plexus-interpolation/")
    (synopsis "Java components for interpolating ${} strings and the like")
    (description "Plexus interpolator is a modular, flexible interpolation
framework for the expression language style commonly seen in Maven, Plexus,
and other related projects.

It has its foundation in the @code{org.codehaus.plexus.utils.interpolation}
package within @code{plexus-utils}, but has been separated in order to allow
these two libraries to vary independently of one another.")
    (license license:asl2.0)))

(define-public java-asm
  (package
    (name "java-asm")
    (version "5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.forge.ow2.org/asm/"
                                  "asm-" version ".tar.gz"))
              (sha256
               (base32
                "0kxvmv5275rnjl7jv0442k3wjnq03ngkb7sghs78avf45pzm4qgr"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "compile"
       ;; The tests require an old version of Janino, which no longer compiles
       ;; with the JDK7.
       #:tests? #f
       ;; We don't need these extra ant tasks, but the build system asks us to
       ;; provide a path anyway.
       #:make-flags (list (string-append "-Dobjectweb.ant.tasks.path=foo"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'build-jars
           (lambda* (#:key make-flags #:allow-other-keys)
             ;; We cannot use the "jar" target because it depends on a couple
             ;; of unpackaged, complicated tools.
             (mkdir "dist")
             (zero? (system* "jar"
                             "-cf" (string-append "dist/asm-" ,version ".jar")
                             "-C" "output/build/tmp" "."))))
         (replace 'install
           (install-jars "dist")))))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "http://asm.ow2.org/")
    (synopsis "Very small and fast Java bytecode manipulation framework")
    (description "ASM is an all purpose Java bytecode manipulation and
analysis framework.  It can be used to modify existing classes or dynamically
generate classes, directly in binary form.  The provided common
transformations and analysis algorithms allow to easily assemble custom
complex transformations and code analysis tools.")
    (license license:bsd-3)))

(define-public java-cglib
  (package
    (name "java-cglib")
    (version "3.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cglib/cglib/archive/RELEASE_"
                    (string-map (lambda (c) (if (char=? c #\.) #\_ c)) version)
                    ".tar.gz"))
              (file-name (string-append "cglib-" version ".tar.gz"))
              (sha256
               (base32
                "162dvd4fln76ai8prfharf66pn6r56p3sxx683j5vdyccrd5hi1q"))))
    (build-system ant-build-system)
    (arguments
     `(;; FIXME: tests fail because junit runs
       ;; "net.sf.cglib.transform.AbstractTransformTest", which does not seem
       ;; to describe a test at all.
       #:tests? #f
       #:jar-name "cglib.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "cglib") #t)))))
    (inputs
     `(("java-asm" ,java-asm)
       ("java-junit" ,java-junit)))
    (home-page "https://github.com/cglib/cglib/")
    (synopsis "Java byte code generation library")
    (description "The byte code generation library CGLIB is a high level API
to generate and transform Java byte code.")
    (license license:asl2.0)))

(define-public java-objenesis
  (package
    (name "java-objenesis")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/easymock/objenesis/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append "objenesis-" version ".tar.gz"))
              (sha256
               (base32
                "1va5qz1i2wawwavhnxfzxnfgrcaflz9p1pg03irrjh4nd3rz8wh6"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "objenesis.jar"
       #:source-dir "main/src/"
       #:test-dir "main/src/test/"))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://objenesis.org/")
    (synopsis "Bypass the constructor when creating an object")
    (description "Objenesis is a small Java library that serves one purpose:
to instantiate a new object of a particular class.  It is common to see
restrictions in libraries stating that classes must require a default
constructor.  Objenesis aims to overcome these restrictions by bypassing the
constructor on object instantiation.")
    (license license:asl2.0)))

(define-public java-easymock
  (package
    (name "java-easymock")
    (version "3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/easymock/easymock/"
                                  "archive/easymock-" version ".tar.gz"))
              (sha256
               (base32
                "1yzg0kv256ndr57gpav46cyv4a1ns5sj722l50zpxk3j6sk9hnmi"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "easymock.jar"
       #:source-dir "core/src/main"
       #:test-dir "core/src/test"
       #:phases
       (modify-phases %standard-phases
         ;; FIXME: Android support requires the following packages to be
         ;; available: com.google.dexmaker.stock.ProxyBuilder
         (add-after 'unpack 'delete-android-support
           (lambda _
             (with-directory-excursion "core/src/main/java/org/easymock/internal"
               (substitute* "MocksControl.java"
                 (("AndroidSupport.isAndroid\\(\\)") "false")
                 (("return classProxyFactory = new AndroidClassProxyFactory\\(\\);") ""))
               (delete-file "AndroidClassProxyFactory.java"))
             #t))
         (add-after 'unpack 'delete-broken-tests
           (lambda _
             (with-directory-excursion "core/src/test/java/org/easymock"
               ;; This test depends on dexmaker.
               (delete-file "tests2/ClassExtensionHelperTest.java")

               ;; This is not a test.
               (delete-file "tests/BaseEasyMockRunnerTest.java")

               ;; This test should be executed with a different runner...
               (delete-file "tests2/EasyMockAnnotationsTest.java")
               ;; ...but deleting it means that we also have to delete these
               ;; dependent files.
               (delete-file "tests2/EasyMockRunnerTest.java")
               (delete-file "tests2/EasyMockRuleTest.java")

               ;; This test fails because the file "easymock.properties" does
               ;; not exist.
               (delete-file "tests2/EasyMockPropertiesTest.java"))
             #t)))))
    (inputs
     `(("java-asm" ,java-asm)
       ("java-cglib" ,java-cglib)
       ("java-objenesis" ,java-objenesis)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://easymock.org")
    (synopsis "Java library providing mock objects for unit tests")
    (description "EasyMock is a Java library that provides an easy way to use
mock objects in unit testing.")
    (license license:asl2.0)))

(define-public java-jopt-simple
  (package
    (name "java-jopt-simple")
    (version "5.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://repo1.maven.org/maven2/"
                                  "net/sf/jopt-simple/jopt-simple/"
                                  version "/jopt-simple-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1v8bzmwmw6qq20gm42xyay6vrd567dra4vqwhgjnqqjz1gs9f8qa"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:jar-name "jopt-simple.jar"))
    (home-page "https://pholser.github.io/jopt-simple/")
    (synopsis "Java library for parsing command line options")
    (description "JOpt Simple is a Java library for parsing command line
options, such as those you might pass to an invocation of @code{javac}.  In
the interest of striving for simplicity, as closely as possible JOpt Simple
attempts to honor the command line option syntaxes of POSIX @code{getopt} and
GNU @code{getopt_long}.  It also aims to make option parser configuration and
retrieval of options and their arguments simple and expressive, without being
overly clever.")
    (license license:expat)))

(define-public java-commons-math3
  (package
    (name "java-commons-math3")
    (version "3.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/math/source/"
                                  "commons-math3-" version "-src.tar.gz"))
              (sha256
               (base32
                "19l6yp44qc5g7wg816nbn5z3zq3xxzwimvbm4a8pczgvpi4i85s6"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jar"
       #:test-target "test"
       #:make-flags
       (let ((hamcrest (assoc-ref %build-inputs "java-hamcrest-core"))
             (junit    (assoc-ref %build-inputs "java-junit")))
         (list (string-append "-Djunit.jar=" junit "/share/java/junit.jar")
               (string-append "-Dhamcrest.jar=" hamcrest
                              "/share/java/hamcrest-core.jar")))
       #:phases
       (modify-phases %standard-phases
         ;; We want to build the jar in the build phase and run the tests
         ;; later in a separate phase.
         (add-after 'unpack 'untangle-targets
           (lambda _
             (substitute* "build.xml"
               (("name=\"jar\" depends=\"test\"")
                "name=\"jar\" depends=\"compile\""))
             #t))
         ;; There is no install target.
         (replace 'install
           (install-jars "target")))))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://commons.apache.org/math/")
    (synopsis "Apache Commons mathematics library")
    (description "Commons Math is a library of lightweight, self-contained
mathematics and statistics components addressing the most common problems not
available in the Java programming language or Commons Lang.")
    (license license:asl2.0)))

(define-public java-jmh
  (package
    (name "java-jmh")
    (version "1.17.5")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                    (url "http://hg.openjdk.java.net/code-tools/jmh/")
                    (changeset version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1fxyxhg9famwcg1prc4cgwb5wzyxqavn3cjm5vz8605xz7x5k084"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jmh-core.jar"
       #:source-dir "jmh-core/src/main"
       #:test-dir "jmh-core/src/test"
       #:phases
       (modify-phases %standard-phases
         ;; This seems to be a bug in the JDK.  It may not be necessary in
         ;; future versions of the JDK.
         (add-after 'unpack 'fix-bug
           (lambda _
             (with-directory-excursion
                 "jmh-core/src/main/java/org/openjdk/jmh/runner/options"
               (substitute* '("IntegerValueConverter.java"
                              "ThreadsValueConverter.java")
                 (("public Class<Integer> valueType")
                  "public Class<? extends Integer> valueType")))
             #t)))))
    (inputs
     `(("java-jopt-simple" ,java-jopt-simple)
       ("java-commons-math3" ,java-commons-math3)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://openjdk.java.net/projects/code-tools/jmh/")
    (synopsis "Benchmark harness for the JVM")
    (description "JMH is a Java harness for building, running, and analysing
nano/micro/milli/macro benchmarks written in Java and other languages
targetting the JVM.")
    ;; GPLv2 only
    (license license:gpl2)))

(define-public java-commons-collections4
  (package
    (name "java-commons-collections4")
    (version "4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/collections/source/"
                                  "commons-collections4-" version "-src.tar.gz"))
              (sha256
               (base32
                "1krfhvggympq4avk7gh6qafzf6b9ip6r1m4lmacikyx04039m0wl"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (let ((hamcrest (assoc-ref %build-inputs "java-hamcrest-core"))
             (junit    (assoc-ref %build-inputs "java-junit"))
             (easymock (assoc-ref %build-inputs "java-easymock")))
         (list (string-append "-Djunit.jar=" junit "/share/java/junit.jar")
               (string-append "-Dhamcrest.jar=" hamcrest
                              "/share/java/hamcrest-core.jar")
               (string-append "-Deasymock.jar=" easymock
                              "/share/java/easymock.jar")))
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars "target")))))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-easymock" ,java-easymock)))
    (home-page "http://commons.apache.org/collections/")
    (synopsis "Collections framework")
    (description "The Java Collections Framework is the recognised standard
for collection handling in Java.  Commons-Collections seek to build upon the
JDK classes by providing new interfaces, implementations and utilities.  There
are many features, including:

@itemize
@item @code{Bag} interface for collections that have a number of copies of
  each object
@item @code{BidiMap} interface for maps that can be looked up from value to
  key as well and key to value
@item @code{MapIterator} interface to provide simple and quick iteration over
  maps
@item Transforming decorators that alter each object as it is added to the
  collection
@item Composite collections that make multiple collections look like one
@item Ordered maps and sets that retain the order elements are added in,
  including an LRU based map
@item Reference map that allows keys and/or values to be garbage collected
  under close control
@item Many comparator implementations
@item Many iterator implementations
@item Adapter classes from array and enumerations to collections
@item Utilities to test or create typical set-theory properties of collections
  such as union, intersection, and closure.
@end itemize\n")
    (license license:asl2.0)))

(define-public java-commons-io
  (package
    (name "java-commons-io")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/commons/io/source/"
                           "commons-io-" version "-src.tar.gz"))
       (sha256
        (base32
         "0q5y41jrcjvx9hzs47x5kdhnasdy6rm4bzqd2jxl02w717m7a7v3"))))
    (build-system ant-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list (string-append "-Djunit.jar="
                            (assoc-ref %build-inputs "java-junit")
                            "/share/java/junit.jar"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-javadoc ant-build-javadoc)
         (replace 'install (install-jars "target"))
         (add-after 'install 'install-doc (install-javadoc "target/apidocs")))))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://commons.apache.org/io/")
    (synopsis "Common useful IO related classes")
    (description "Commons-IO contains utility classes, stream implementations,
file filters and endian classes.")
    (license license:asl2.0)))

(define-public java-commons-lang
  (package
    (name "java-commons-lang")
    (version "2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/commons/lang/source/"
                           "commons-lang-" version "-src.tar.gz"))
       (sha256
        (base32 "1mxwagqadzx1b2al7i0z1v0r235aj2njdyijf02szq0vhmqrfiq5"))))
    (build-system ant-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-javadoc ant-build-javadoc)
         (add-before 'check 'disable-failing-test
           (lambda _
             ;; Disable a failing test
             (substitute* "src/test/java/org/apache/commons/lang/\
time/FastDateFormatTest.java"
               (("public void testFormat\\(\\)")
                "public void disabled_testFormat()"))
             #t))
         (replace 'install (install-jars "target"))
         (add-after 'install 'install-doc (install-javadoc "target/apidocs")))))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "http://commons.apache.org/lang/")
    (synopsis "Extension of the java.lang package")
    (description "The Commons Lang components contains a set of Java classes
that provide helper methods for standard Java classes, especially those found
in the @code{java.lang} package in the Sun JDK.  The following classes are
included:

@itemize
@item StringUtils - Helper for @code{java.lang.String}.
@item CharSetUtils - Methods for dealing with @code{CharSets}, which are sets
  of characters such as @code{[a-z]} and @code{[abcdez]}.
@item RandomStringUtils - Helper for creating randomised strings.
@item NumberUtils - Helper for @code{java.lang.Number} and its subclasses.
@item NumberRange - A range of numbers with an upper and lower bound.
@item ObjectUtils - Helper for @code{java.lang.Object}.
@item SerializationUtils - Helper for serializing objects.
@item SystemUtils - Utility class defining the Java system properties.
@item NestedException package - A sub-package for the creation of nested
  exceptions.
@item Enum package - A sub-package for the creation of enumerated types.
@item Builder package - A sub-package for the creation of @code{equals},
  @code{hashCode}, @code{compareTo} and @code{toString} methods.
@end itemize\n")
    (license license:asl2.0)))

(define-public java-commons-lang3
  (package
    (name "java-commons-lang3")
    (version "3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/commons/lang/source/"
                           "commons-lang3-" version "-src.tar.gz"))
       (sha256
        (base32 "0xpshb9spjhplq5a7mr0y1bgfw8190ik4xj8f569xidfcki1d6kg"))))
    (build-system ant-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:test-target "test"
       #:make-flags
       (let ((hamcrest (assoc-ref %build-inputs "java-hamcrest-all"))
             (junit    (assoc-ref %build-inputs "java-junit"))
             (easymock (assoc-ref %build-inputs "java-easymock"))
             (io       (assoc-ref %build-inputs "java-commons-io")))
         (list (string-append "-Djunit.jar=" junit "/share/java/junit.jar")
               (string-append "-Dhamcrest.jar=" hamcrest
                              "/share/java/hamcrest-all.jar")
               (string-append "-Dcommons-io.jar=" io
                              "/share/java/commons-io-"
                              ,(package-version java-commons-io)
                              "-SNAPSHOT.jar")
               (string-append "-Deasymock.jar=" easymock
                              "/share/java/easymock.jar")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-javadoc ant-build-javadoc)
         (replace 'install (install-jars "target"))
         (add-after 'install 'install-doc (install-javadoc "target/apidocs")))))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-commons-io" ,java-commons-io)
       ("java-hamcrest-all" ,java-hamcrest-all)
       ("java-easymock" ,java-easymock)))
    (home-page "http://commons.apache.org/lang/")
    (synopsis "Extension of the java.lang package")
    (description "The Commons Lang components contains a set of Java classes
that provide helper methods for standard Java classes, especially those found
in the @code{java.lang} package.  The following classes are included:

@itemize
@item StringUtils - Helper for @code{java.lang.String}.
@item CharSetUtils - Methods for dealing with @code{CharSets}, which are sets of
  characters such as @code{[a-z]} and @code{[abcdez]}.
@item RandomStringUtils - Helper for creating randomised strings.
@item NumberUtils - Helper for @code{java.lang.Number} and its subclasses.
@item NumberRange - A range of numbers with an upper and lower bound.
@item ObjectUtils - Helper for @code{java.lang.Object}.
@item SerializationUtils - Helper for serializing objects.
@item SystemUtils - Utility class defining the Java system properties.
@item NestedException package - A sub-package for the creation of nested
   exceptions.
@item Enum package - A sub-package for the creation of enumerated types.
@item Builder package - A sub-package for the creation of @code{equals},
  @code{hashCode}, @code{compareTo} and @code{toString} methods.
@end itemize\n")
    (license license:asl2.0)))

(define-public java-commons-cli
  (package
    (name "java-commons-cli")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/cli/source/"
                                  "commons-cli-" version "-src.tar.gz"))
              (sha256
               (base32
                "1fkjn552i12vp3xxk21ws4p70fi0lyjm004vzxsdaz7gdpgyxxyl"))))
    (build-system ant-build-system)
    ;; TODO: javadoc
    (arguments
     `(#:jar-name "commons-cli.jar"))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://commons.apache.org/cli/")
    (synopsis "Command line arguments and options parsing library")
    (description "The Apache Commons CLI library provides an API for parsing
command line options passed to programs.  It is also able to print help
messages detailing the options available for a command line tool.

Commons CLI supports different types of options:

@itemize
@item POSIX like options (ie. tar -zxvf foo.tar.gz)
@item GNU like long options (ie. du --human-readable --max-depth=1)
@item Java like properties (ie. java -Djava.awt.headless=true Foo)
@item Short options with value attached (ie. gcc -O2 foo.c)
@item long options with single hyphen (ie. ant -projecthelp)
@end itemize

This is a part of the Apache Commons Project.")
    (license license:asl2.0)))

(define-public java-commons-codec
  (package
    (name "java-commons-codec")
    (version "1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/codec/source/"
                                  "commons-codec-" version "-src.tar.gz"))
              (sha256
               (base32
                "1w9qg30y4s0x8gnmr2fgj4lyplfn788jqxbcz27lf5kbr6n8xr65"))))
    (build-system ant-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:test-target "test"
       #:make-flags
       (let ((hamcrest (assoc-ref %build-inputs "java-hamcrest-core"))
             (junit    (assoc-ref %build-inputs "java-junit")))
         (list (string-append "-Djunit.jar=" junit "/share/java/junit.jar")
               (string-append "-Dhamcrest.jar=" hamcrest
                              "/share/java/hamcrest-core.jar")
               ;; Do not append version to jar.
               "-Dfinal.name=commons-codec"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-javadoc ant-build-javadoc)
         (replace 'install (install-jars "dist"))
         (add-after 'install 'install-doc (install-javadoc "dist/docs/api")))))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://commons.apache.org/codec/")
    (synopsis "Common encoders and decoders such as Base64, Hex, Phonetic and URLs")
    (description "The codec package contains simple encoder and decoders for
various formats such as Base64 and Hexadecimal.  In addition to these widely
used encoders and decoders, the codec package also maintains a collection of
phonetic encoding utilities.

This is a part of the Apache Commons Project.")
    (license license:asl2.0)))

(define-public java-commons-daemon
  (package
    (name "java-commons-daemon")
    (version "1.0.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/daemon/source/"
                                  "commons-daemon-" version "-src.tar.gz"))
              (sha256
               (base32
                "0ci46kq8jpz084ccwq0mmkahcgsmh20ziclp2jf5i0djqv95gvhi"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-javadoc ant-build-javadoc)
         (replace 'install (install-jars "dist"))
         (add-after 'install 'install-doc (install-javadoc "dist/docs/api")))))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "http://commons.apache.org/daemon/")
    (synopsis "Library to launch Java applications as daemons")
    (description "The Daemon package from Apache Commons can be used to
implement Java applications which can be launched as daemons.  For example the
program will be notified about a shutdown so that it can perform cleanup tasks
before its process of execution is destroyed by the operation system.

This package contains the Java library.  You will also need the actual binary
for your architecture which is provided by the jsvc package.

This is a part of the Apache Commons Project.")
    (license license:asl2.0)))
