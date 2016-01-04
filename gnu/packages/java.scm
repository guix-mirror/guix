;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
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
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
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
  #:use-module (gnu packages mit-krb5)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages texinfo)
  #:use-module ((srfi srfi-1) #:select (fold alist-delete)))

(define-public swt
  (package
    (name "swt")
    (version "4.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ftp-stud.fht-esslingen.de/pub/Mirrors/"
                    "eclipse/eclipse/downloads/drops4/R-" version
                    "-201502041700/swt-" version "-gtk-linux-x86.zip"))
              (sha256
               (base32
                "0lzyqr8k2zm5s8fmnrx5kxpslxfs0i73y26fwfms483x45izzwj8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("-f" "make_linux.mak")
       #:tests? #f ; no "check" target
       #:phases
       (alist-replace
        'unpack
        (lambda _
          (and (mkdir "swt")
               (zero? (system* "unzip" (assoc-ref %build-inputs "source") "-d" "swt"))
               (chdir "swt")
               (mkdir "src")
               (zero? (system* "unzip" "src.zip" "-d" "src"))
               (chdir "src")))
        (alist-replace
         'build
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
             (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))

             ;; Build shared libraries.  Users of SWT have to set the system
             ;; property swt.library.path to the "lib" directory of this
             ;; package output.
             (mkdir-p lib)
             (setenv "OUTPUT_DIR" lib)
             (zero? (system* "bash" "build.sh"))

             ;; build jar
             (mkdir "build")
             (for-each (lambda (file)
                         (format #t "Compiling ~s\n" file)
                         (system* "javac" "-d" "build" file))
                       (find-files "." "\\.java"))
             (zero? (system* "jar" "cvf" "swt.jar" "-C" "build" "."))))
         (alist-cons-after
          'install 'install-java-files
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((java (string-append (assoc-ref outputs "out")
                                       "/share/java")))
              (install-file "swt.jar" java)
              #t))
          (alist-delete 'configure %standard-phases))))))
    (inputs
     `(("xulrunner" ,icecat)
       ("gtk" ,gtk+-2)
       ("libxtst" ,libxtst)
       ("libxt" ,libxt)
       ("mesa" ,mesa)
       ("glu" ,glu)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("unzip" ,unzip)
       ("jdk" ,icedtea "jdk")))
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

(define-public ant
  (package
    (name "ant")
    (version "1.9.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/ant/source/apache-ant-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "1396wflczyxjxl603dhxjvd559f289lha9y2f04f71c7hapjl3am"))))
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

(define-public icedtea-6
  (package
    (name "icedtea")
    (version "1.13.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://icedtea.wildebeest.org/download/source/icedtea6-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1mq08sfyfjlfw0c1czjs47303zv4h91s1jc0nhdlra4rbbx0g2d0"))
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
     `(;; There are many failing tests and many are known to fail upstream.
       ;;
       ;; * Hotspot VM tests:
       ;;   FAILED: compiler/7082949/Test7082949.java
       ;;   FAILED: compiler/7088020/Test7088020.java
       ;;   FAILED: runtime/6929067/Test6929067.sh
       ;;   FAILED: serviceability/sa/jmap-hashcode/Test8028623.java
       ;;   => Test results: passed: 161; failed: 4
       ;;
       ;; * langtools tests:
       ;;   FAILED: com/sun/javadoc/testHtmlDefinitionListTag/TestHtmlDefinitionListTag.java
       ;;   FAILED: tools/javac/6627362/T6627362.java
       ;;   FAILED: tools/javac/7003595/T7003595.java
       ;;   FAILED: tools/javac/7024568/T7024568.java
       ;;   FAILED: tools/javap/4111861/T4111861.java
       ;;   FAILED: tools/javap/ListTest.java
       ;;   FAILED: tools/javap/OptionTest.java
       ;;   FAILED: tools/javap/T4884240.java
       ;;   FAILED: tools/javap/T4975569.java
       ;;     --> fails because of insignificant whitespace differences
       ;;         in output of javap
       ;;   FAILED: tools/javap/T6868539.java
       ;;   => Test results: passed: 1,445; failed: 10
       ;;
       ;; * JDK tests:
       ;;   Tests are incomplete because of a segfault after this test:
       ;;     javax/crypto/spec/RC5ParameterSpec/RC5ParameterSpecEquals.java
       ;;   A bug report has already been filed upstream:
       ;;     http://icedtea.classpath.org/bugzilla/show_bug.cgi?id=2188
       ;;
       ;;   The tests require xvfb-run, a wrapper script around Xvfb, which
       ;;   has not been packaged yet.  Without it many AWT tests fail, so I
       ;;   made no attempts to make a list of failing JDK tests.  At least
       ;;   222 tests are failing of which at least 132 are AWT tests.
       #:tests? #f

       ;; The DSOs use $ORIGIN to refer to each other, but (guix build
       ;; gremlin) doesn't support it yet, so skip this phase.
       #:validate-runpath? #f

       #:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 popen)
                  (ice-9 rdelim))

       #:configure-flags
       (let* ((gcjdir (assoc-ref %build-inputs "gcj"))
              (ecj    (string-append gcjdir "/share/java/ecj.jar"))
              (jdk    (string-append gcjdir "/lib/jvm/"))
              (gcj    (string-append gcjdir "/bin/gcj")))
         `("--enable-bootstrap"
           "--enable-nss"
           "--without-rhino"
           "--disable-downloading"
           "--disable-tests" ;they are run in the check phase instead
           "--with-openjdk-src-dir=./openjdk.src"
           ,(string-append "--with-javac=" jdk "/bin/javac")
           ,(string-append "--with-ecj-jar=" ecj)
           ,(string-append "--with-gcj=" gcj)
           ,(string-append "--with-jdk-home=" jdk)
           ,(string-append "--with-java=" jdk "/bin/java")))
       #:phases
       (alist-replace
        'unpack
        (lambda* (#:key source inputs #:allow-other-keys)
          (and (zero? (system* "tar" "xvf" source))
               (begin
                 (chdir (string-append "icedtea6-" ,version))
                 (mkdir "openjdk.src")
                 (with-directory-excursion "openjdk.src"
                   (copy-file (assoc-ref inputs "openjdk6-src")
                              "openjdk6-src.tar.xz")
                   (zero? (system* "tar" "xvf" "openjdk6-src.tar.xz"))))))
        (alist-cons-after
         'unpack 'patch-patches
         (lambda _
           ;; shebang in patches so that they apply cleanly
           (substitute* '("patches/jtreg-jrunscript.patch"
                          "patches/hotspot/hs23/drop_unlicensed_test.patch")
             (("#!/bin/sh") (string-append "#!" (which "sh"))))

           ;; fix path to alsa header in patch
           (substitute* "patches/openjdk/6799141-split_out_versions.patch"
             (("ALSA_INCLUDE=/usr/include/alsa/version.h")
              (string-append "ALSA_INCLUDE="
                             (assoc-ref %build-inputs "alsa-lib")
                             "/include/alsa/version.h"))))
         (alist-cons-after
          'unpack 'patch-paths
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
                (("TR=TRL;2004-12-31-22-00-00;TRY") "TR=TRY"))))
          (alist-cons-before
           'configure 'set-additional-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((gcjdir  (assoc-ref %build-inputs "gcj"))
                    (gcjlib  (string-append gcjdir "/lib"))
                    ;; Get target-specific include directory so that
                    ;; libgcj-config.h is found when compiling hotspot.
                    (gcjinclude (let* ((port (open-input-pipe "gcj -print-file-name=include"))
                                       (str  (read-line port)))
                                  (close-pipe port)
                                  str)))
               (setenv "CPATH"
                       (string-append gcjinclude ":"
                                      (assoc-ref %build-inputs "libxrender")
                                      "/include/X11/extensions" ":"
                                      (assoc-ref %build-inputs "libxtst")
                                      "/include/X11/extensions" ":"
                                      (assoc-ref %build-inputs "libxinerama")
                                      "/include/X11/extensions" ":"
                                      (or (getenv "CPATH") "")))
               (setenv "ALT_CUPS_HEADERS_PATH"
                       (string-append (assoc-ref %build-inputs "cups")
                                      "/include"))
               (setenv "ALT_FREETYPE_HEADERS_PATH"
                       (string-append (assoc-ref %build-inputs "freetype")
                                      "/include"))
               (setenv "ALT_FREETYPE_LIB_PATH"
                       (string-append (assoc-ref %build-inputs "freetype")
                                      "/lib"))))
           (alist-cons-before
            'check 'fix-test-framework
            (lambda _
              ;; Fix PATH in test environment
              (substitute* "src/jtreg/com/sun/javatest/regtest/Main.java"
                (("PATH=/bin:/usr/bin")
                 (string-append "PATH=" (getenv "PATH"))))
              (substitute* "src/jtreg/com/sun/javatest/util/SysEnv.java"
                (("/usr/bin/env") (which "env")))
              #t)
            (alist-cons-before
             'check 'fix-hotspot-tests
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
               #t)
             (alist-cons-before
              'check 'fix-jdk-tests
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
                #t)
              (alist-replace
               'check
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
                   (or #t ; skip tests
                       (and (run-test "check-hotspot")
                            (run-test "check-langtools")
                            (run-test "check-jdk")))))
               (alist-replace
                'install
                (lambda* (#:key outputs #:allow-other-keys)
                  (let ((doc (string-append (assoc-ref outputs "doc")
                                            "/share/doc/icedtea"))
                        (jre (assoc-ref outputs "out"))
                        (jdk (assoc-ref outputs "jdk")))
                    (copy-recursively "openjdk.build/docs" doc)
                    (copy-recursively "openjdk.build/j2re-image" jre)
                    (copy-recursively "openjdk.build/j2sdk-image" jdk)))
                %standard-phases)))))))))))
    (native-inputs
     `(("ant" ,ant)
       ("alsa-lib" ,alsa-lib)
       ("attr" ,attr)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("coreutils" ,coreutils)
       ("diffutils" ,diffutils) ;for tests
       ("gawk" ,gawk)
       ("grep" ,grep)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("cups" ,cups)
       ("wget" ,wget)
       ("which" ,which)
       ("cpio" ,cpio)
       ("zip" ,zip)
       ("unzip" ,unzip)
       ("fastjar" ,fastjar)
       ("libxslt" ,libxslt) ;for xsltproc
       ("mit-krb5" ,mit-krb5)
       ("nss" ,nss)
       ("libx11" ,libx11)
       ("libxt" ,libxt)
       ("libxtst" ,libxtst)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("libxrender" ,libxrender)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("giflib" ,giflib)
       ("perl" ,perl)
       ("procps" ,procps) ;for "free", even though I'm not sure we should use it
       ("openjdk6-src"
        ,(origin
           (method url-fetch)
           (uri "https://java.net/downloads/openjdk6/openjdk-6-src-b38-20_jan_2016.tar.gz")
           (sha256
            (base32
             "1fapj9w4ahzf5nwvdgi1dsxjyh9dqbcvf9638r60h1by13wjqk5p"))))
       ("lcms" ,lcms)
       ("zlib" ,zlib)
       ("gtk" ,gtk+-2)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gcj" ,gcj)))
    (home-page "http://icedtea.classpath.org")
    (synopsis "Java development kit")
    (description
     "The OpenJDK built with the IcedTea build harness.")
    ;; IcedTea is released under the GPL2 + Classpath exception, which is the
    ;; same license as both GNU Classpath and OpenJDK.
    (license license:gpl2+)))

(define-public icedtea-7
  (let* ((version "2.6.4")
         (drop (lambda (name hash)
                 (origin
                   (method url-fetch)
                   (uri (string-append
                         "http://icedtea.classpath.org/download/drops/"
                         "/icedtea7/" version "/" name ".tar.bz2"))
                   (sha256 (base32 hash))))))
    (package (inherit icedtea-6)
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "http://icedtea.wildebeest.org/download/source/icedtea-"
                      version ".tar.xz"))
                (sha256
                 (base32
                  "0r31h8nlsrbfdkgbjbb7phwgcwglc9siznzrr40lqnm9xrgkc2nj"))
                (modules '((guix build utils)))
                (snippet
                 '(substitute* "Makefile.in"
                    ;; link against libgcj to avoid linker error
                    (("-o native-ecj")
                     "-lgcj -o native-ecj")
                    ;; do not leak information about the build host
                    (("DISTRIBUTION_ID=\"\\$\\(DIST_ID\\)\"")
                     "DISTRIBUTION_ID=\"\\\"guix\\\"\"")))))
      (arguments
       `(;; There are many test failures.  Some are known to
         ;; fail upstream, others relate to not having an X
         ;; server running at test time, yet others are a
         ;; complete mystery to me.

         ;; hotspot:   passed: 241; failed: 45; error: 2
         ;; langtools: passed: 1,934; failed: 26
         ;; jdk:       unknown
         #:tests? #f
         ;; Apparently, the C locale is needed for some of the tests.
         #:locale "C"
         ,@(substitute-keyword-arguments (package-arguments icedtea-6)
             ((#:modules modules)
              `((ice-9 match)
                (srfi srfi-26)
                ,@modules))
             ((#:configure-flags flags)
              ;; TODO: package pcsc and sctp, and add to inputs
              `(append '("--disable-system-pcsc"
                         "--disable-system-sctp")
                       ,flags))
             ((#:phases phases)
              `(modify-phases ,phases
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
                 (replace
                  'set-additional-paths
                  (lambda* (#:key inputs #:allow-other-keys)
                    (let (;; Get target-specific include directory so that
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
                                             "/lib")))))
                 (add-after
                  'unpack 'fix-x11-extension-include-path
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
                 (replace
                  'fix-test-framework
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
                 (delete 'patch-patches))))))
      (native-inputs
       `(("openjdk-src"
          ,(drop "openjdk"
                 "1qjjf71nq80ac2d08hbaa8589d31vk313z3rkirnwq5df8cyf0mv"))
         ("corba-drop"
          ,(drop "corba"
                 "025warxhjal3nr7w1xyd16k0f32fwkchifpaslzyidsga3hgmfr6"))
         ("jaxp-drop"
          ,(drop "jaxp"
                 "0qiz6swb78w9c0mf88pf0gflgm5rp9k0l6fv6sdl7dki691b0z09"))
         ("jaxws-drop"
          ,(drop "jaxws"
                 "18fz4gl4fdlcmqvh1mlpd9h0gj0qizpfa7njkax97aysmsm08xns"))
         ("jdk-drop"
          ,(drop "jdk"
                 "0qsx5d9pgwlz9vbpapw4jwpajqc6rwk1150cjb33i4n3z709jccx"))
         ("langtools-drop"
          ,(drop "langtools"
                 "1k6plx96smf86z303gb30hncssa8f40qdryzsdv349iwqwacxc7r"))
         ("hotspot-drop"
          ,(drop "hotspot"
                 "0r9ffzyf5vxs8wg732szqcil0ksc8lcxzihdv3viz7d67dy42irp"))
         ,@(fold alist-delete (package-native-inputs icedtea-6)
                 '("openjdk6-src")))))))

(define-public icedtea icedtea-7)
