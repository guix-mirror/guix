;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages axoloti)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages flashing-tools)
  #:use-module (gnu packages java)
  #:use-module (gnu packages java-graphics)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml))

;; XXX The patch does not apply to libusb 1.0.24.
;; See https://github.com/axoloti/axoloti/issues/464
(define libusb-for-axoloti
  (package
    (inherit libusb)
    (version "1.0.23")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/libusb/libusb/"
                          "releases/download/v" version
                          "/libusb-" version ".tar.bz2"))
      (sha256
       (base32 "13dd2a9x290d1q8nb1lqiaf36grcvns5ripk5k2xm0lajmpc04fv"))
      (patches (list (search-patch "libusb-for-axoloti.patch")))))))

(define dfu-util-for-axoloti
  (package (inherit dfu-util)
    (name "axoloti-dfu-util")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://dfu-util.sourceforge.net/releases/"
                           "dfu-util-" version ".tar.gz"))
       (sha256
        (base32
         "0n7h08avlzin04j93m6hkq9id6hxjiiix7ff9gc2n89aw6dxxjsm"))))
    (inputs
     `(("libusb" ,libusb-for-axoloti)))))

(define-public axoloti-runtime
  (package
    (name "axoloti-runtime")
    (version "1.0.12-2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/axoloti/axoloti")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qffis277wshldr3i939b0r2x3a2mlr53samxqmr2nk1sfm2b4w9"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove pre-built Java binaries.
           (delete-file-recursively "lib/")
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 match)
                  (ice-9 regex))
       #:imported-modules ((guix build syscalls)
                           ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; prepare ChibiOS
             (invoke "unzip" "-o" (assoc-ref inputs "chibios"))
             (invoke "mv" "ChibiOS_2.6.9" "chibios")
             (with-directory-excursion "chibios/ext"
               (invoke "unzip" "-o" "fatfs-0.9-patched.zip"))

             ;; Remove source of non-determinism in ChibiOS
             (substitute* "chibios/os/various/shell.c"
               (("#ifdef __DATE__") "#if 0"))

             ;; Patch shell paths
             (substitute* '("src/main/java/qcmds/QCmdCompileFirmware.java"
                            "src/main/java/qcmds/QCmdCompilePatch.java"
                            "src/main/java/qcmds/QCmdFlashDFU.java")
               (("/bin/sh") (which "sh")))

             ;; Override cross compiler base name
             (substitute* "firmware/Makefile.patch"
               (("arm-none-eabi-(gcc|g\\+\\+|objcopy|objdump)" tool)
                (which tool)))

             ;; Hardcode full path to compiler tools
             (substitute* '("firmware/Makefile"
                            "firmware/flasher/Makefile"
                            "firmware/mounter/Makefile")
               (("TRGT =.*")
                (string-append "TRGT = "
                               (assoc-ref inputs "cross-toolchain")
                               "/bin/arm-none-eabi-\n")))

             ;; Hardcode path to "make"
             (substitute* '("firmware/compile_firmware_linux.sh"
                            "firmware/compile_patch_linux.sh")
               (("make") (which "make")))

             ;; Hardcode path to "dfu-util"
             (substitute* "platform_linux/upload_fw_dfu.sh"
               (("-f \"\\$\\{platformdir\\}/bin/dfu-util\"") "-z \"\"")
               (("\\./dfu-util") (which "dfu-util")))
             #t))
         (delete 'configure)
         (replace 'build
           ;; Build Axoloti firmware with cross-compiler
           (lambda _
             (with-directory-excursion "platform_linux"
               (invoke "sh" "compile_firmware.sh"))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share/axoloti/"))
                    (doc   (string-append share "doc"))
                    (dir   (getcwd))
                    (pats  '("/doc/[^/]+$"
                             "/patches/[^/]+/[^/]+$"
                             "/objects/[^/]+/[^/]+$"
                             "/firmware/.+"
                             "/chibios/[^/]+$"
                             "/chibios/boards/ST_STM32F4_DISCOVERY/[^/]+$"
                             "/chibios/(ext|os|docs)/.+"
                             "/CMSIS/[^/]+/[^/]+$"
                             "/patch/[^/]+/[^/]+$"
                             "/[^/]+\\.txt$"))
                    (pattern (string-append
                              "(" (string-join
                                   (map (cut string-append dir <>)
                                        pats)
                                   "|") ")"))
                    (files   (find-files dir
                                         (lambda (file stat)
                                           (and (eq? 'regular (stat:type stat))
                                                (string-match pattern file))))))
               (for-each (lambda (file)
                           (install-file file
                                         (string-append
                                          share
                                          (regexp-substitute
                                           #f
                                           (string-match dir (dirname file))
                                           'pre  'post))))
                         files)
               #t))))))
    (inputs
     `(("chibios"
        ,(origin
           (method url-fetch)
           (uri "mirror://sourceforge/chibios/ChibiOS%20GPL3/Version%202.6.9/ChibiOS_2.6.9.zip")
           (sha256
            (base32
             "0lb5s8pkj80mqhsy47mmq0lqk34s2a2m3xagzihalvabwd0frhlj"))))
       ;; for compiling patches
       ("make" ,gnu-make)
       ;; for compiling firmware
       ("cross-toolchain" ,arm-none-eabi-nano-toolchain-4.9)
       ;; for uploading compiled patches and firmware
       ("dfu-util" ,dfu-util-for-axoloti)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.axoloti.com/")
    (synopsis "Audio development environment for the Axoloti core board")
    (description
     "The Axoloti patcher offers a “patcher” environment similar to Pure Data
for sketching digital audio algorithms.  The patches run on a standalone
powerful microcontroller board: Axoloti Core.  This package provides the
runtime.")
    (license license:gpl3+)))

(define-public axoloti-patcher
  (package (inherit axoloti-runtime)
    (name "axoloti-patcher")
    (version (package-version axoloti-runtime))
    (arguments
     `(#:tests? #f ; no check target
       #:modules ((guix build gnu-build-system)
                  ((guix build ant-build-system) #:prefix ant:)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 match)
                  (ice-9 regex)
                  (sxml simple)
                  (sxml xpath)
                  (sxml transform))
       #:imported-modules ((guix build ant-build-system)
                           (guix build syscalls)
                           ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "JAVA_HOME" (assoc-ref inputs "icedtea"))
             ;; We want to use our own jar files instead of the pre-built
             ;; stuff in lib.  So we replace the zipfileset tags in the
             ;; build.xml with new ones that reference our jars.
             (let* ((build.xml (with-input-from-file "build.xml"
                                 (lambda _
                                   (xml->sxml #:trim-whitespace? #t))))
                    (jars      (append-map (match-lambda
                                             (((? (cut string-prefix? "java-" <>)
                                                  label) . directory)
                                              (find-files directory "\\.jar$"))
                                             (_ '()))
                                           inputs))
                    (classpath (string-join jars ":"))
                    (fileset   (map (lambda (jar)
                                      `(zipfileset (@ (excludes "META-INF/*.SF")
                                                      (src ,jar))))
                                    jars)))
               (call-with-output-file "build.xml"
                 (lambda (port)
                   (sxml->xml
                    (pre-post-order
                     build.xml
                     `(;; Remove all zipfileset tags from the "jar" tree and
                       ;; inject our own tags.
                       (jar . ,(lambda (tag . kids)
                                 `(jar ,@(append-map
                                          (filter (lambda (e)
                                                    (not (eq? 'zipfileset (car e)))))
                                          kids)
                                       ,@fileset)))
                       ;; Skip the "bundle" target (and the "-post-jar" target
                       ;; that depends on it), because we don't need it and it
                       ;; confuses sxml->xml.
                       (target . ,(lambda (tag . kids)
                                    (let ((name ((sxpath '(name *text*))
                                                 (car kids))))
                                      (if (or (member "bundle" name)
                                              (member "-post-jar" name))
                                          '() ; skip
                                          `(,tag ,@kids)))))
                       (*default*  . ,(lambda (tag . kids) `(,tag ,@kids)))
                       (*text*     . ,(lambda (_ txt)
                                        (match txt
                                          ;; Remove timestamp.
                                          ("${TODAY}" "(unknown)")
                                          (_ txt))))))
                    port)))

               ;; Build it!
               (invoke "ant"
                       (string-append "-Djavac.classpath=" classpath)
                       "-Dbuild.runtime=true"
                       "-Dbuild.time=01/01/1970 00:00:00"
                       "-Djavac.source=1.7"
                       "-Djavac.target=1.7"
                       (string-append "-Dtag.short.version="
                                      ,version)))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share/axoloti/")))
               (install-file "dist/Axoloti.jar" share)

               ;; We do this to ensure that this package retains references to
               ;; other Java packages' jar files.
               (install-file "build.xml" share)

               ;; Create a launcher script
               (mkdir (string-append out "/bin"))
               (let ((target (string-append out "/bin/Axoloti")))
                 (with-output-to-file target
                   (lambda ()
                     (let* ((dir       (string-append (assoc-ref outputs "out")
                                                      "/share/axoloti"))
                            (runtime   (string-append (assoc-ref inputs "axoloti-runtime")
                                                      "/share/axoloti"))
                            (toolchain (assoc-ref inputs "cross-toolchain"))
                            (includes  (string-append
                                        toolchain
                                        "/arm-none-eabi/include:"
                                        toolchain
                                        "/arm-none-eabi/include/arm-none-eabi/armv7e-m")))
                       (display
                        (string-append "#!" (which "sh") "\n"
                                       "export CROSS_CPATH=" includes "\n"
                                       "export CROSS_CPLUS_INCLUDE_PATH=" includes "\n"
                                       "export CROSS_LIBRARY_PATH="
                                       toolchain "/arm-none-eabi/lib" "\n"
                                       (which "java")
                                       " -Daxoloti_release=" runtime
                                       " -Daxoloti_runtime=" runtime
                                       " -jar " dir "/Axoloti.jar")))))
                 (chmod target #o555))
               #t)))
         (add-after 'install 'strip-jar-timestamps
           (assoc-ref ant:%standard-phases 'strip-jar-timestamps)))))
    (inputs
     `(("icedtea" ,icedtea "jdk")
       ("cross-toolchain" ,arm-none-eabi-nano-toolchain-4.9)
       ("java-simple-xml" ,java-simple-xml)
       ("java-rsyntaxtextarea" ,java-rsyntaxtextarea)
       ("java-usb4java" ,java-usb4java)
       ("java-jsch" ,java-jsch)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-jgit" ,java-jgit-4.2)
       ("axoloti-runtime" ,axoloti-runtime)))
    (native-inputs
     `(("ant" ,ant)
       ("zip" ,zip) ; for repacking the jar
       ("unzip" ,unzip)))
    (description
     "The Axoloti patcher offers a “patcher” environment similar to Pure Data
for sketching digital audio algorithms.  The patches run on a standalone
powerful microcontroller board: Axoloti Core.  This package provides the
patcher application.")))

(define-public axoloti-patcher-next
  (package
    (name "axoloti-patcher-next")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/axoloti/axoloti")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "022p993wf9l9mp6vg94x592vfqd1k1sjrq5f6x45h5r16qy6bdzh"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove pre-built Java binaries.
           (delete-file-recursively "lib/")
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there's a Java test which is run as part of the Java build.
       #:modules ((guix build gnu-build-system)
                  ((guix build ant-build-system) #:prefix ant:)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 match)
                  (ice-9 regex)
                  (sxml simple)
                  (sxml xpath)
                  (sxml transform))
       #:imported-modules ((guix build ant-build-system)
                           (guix build syscalls)
                           ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "elfloader")
                               "firmware/elfloader")
             (for-each make-file-writable (find-files "."))
             #t))
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; prepare ChibiOS
             (invoke "7z" "x" (assoc-ref inputs "chibios"))
             (with-directory-excursion "ChibiOS_19.1.3/ext"
               (invoke "7z" "x" "fatfs-0.13_patched.7z"))

             ;; Hardcode full path to compiler tools
             (substitute*
                 '("ChibiOS_19.1.3/os/common/startup/ARMCMx/compilers/GCC/mk/arm-none-eabi.mk"
                   "env/arm-none-eabi.mk")
               (("TRGT =.*")
                (string-append "TRGT = "
                               (assoc-ref inputs "cross-toolchain")
                               "/bin/arm-none-eabi-\n")))

             ;; Hardcode path to "make"
             (substitute* "src/main/java/axoloti/shell/ShellTask.java"
               (("return \"make\"")
                (string-append "return \"" (which "make") "\"")))

             ;; Hardcode path to "dfu-util"
             (substitute* "src/main/java/axoloti/shell/UploadFirmwareDFU.java"
               (("Axoloti.getReleaseDir\\(\\) \\+ \"/platform_.+/bin/dfu-util\"")
                (string-append "\"" (which "dfu-util") "\"")))
             #t))
         (delete 'configure)
         (replace 'build
           ;; Build Axoloti firmware with cross-compiler
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((toolchain (assoc-ref inputs "cross-toolchain"))
                    (headers   (string-append
                                toolchain
                                "/arm-none-eabi/include:"
                                toolchain
                                "/arm-none-eabi/include/arm-none-eabi/armv7e-m")))
               (setenv "CROSS_CPATH" headers)
               (setenv "CROSS_CPLUS_INCLUDE_PATH" headers)
               (setenv "CROSS_LIBRARY_PATH"
                       (string-append toolchain
                                      "/arm-none-eabi/lib")))
             (with-directory-excursion "platform_linux"
               (invoke "sh" "compile_firmware.sh"))))
         (add-after 'build 'build-patcher
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
             ;; We want to use our own jar files instead of the pre-built
             ;; stuff in lib.  So we replace the zipfileset tags in the
             ;; build.xml with new ones that reference our jars.
             (let* ((build.xml (with-input-from-file "build.xml"
                                 (lambda _
                                   (xml->sxml #:trim-whitespace? #t))))
                    (jars      (append-map (match-lambda
                                             (((? (cut string-prefix? "java-" <>)
                                                  label) . directory)
                                              (find-files directory "\\.jar$"))
                                             (_ '()))
                                           inputs))
                    (classpath (string-join jars ":"))
                    (fileset   (map (lambda (jar)
                                      `(zipfileset (@ (excludes "META-INF/*.SF")
                                                      (src ,jar))))
                                    jars)))
               (call-with-output-file "build.xml"
                 (lambda (port)
                   (sxml->xml
                    (pre-post-order
                     build.xml
                     `( ;; Remove all zipfileset tags from the "jar" tree and
                       ;; inject our own tags.
                       (jar . ,(lambda (tag . kids)
                                 `(jar ,@(append-map
                                          (filter (lambda (e)
                                                    (not (eq? 'zipfileset (car e)))))
                                          kids)
                                       ,@fileset)))
                       ;; Skip the "bundle" target (and the "-post-jar" target
                       ;; that depends on it), because we don't need it and it
                       ;; confuses sxml->xml.
                       (target . ,(lambda (tag . kids)
                                    (let ((name ((sxpath '(name *text*))
                                                 (car kids))))
                                      (if (or (member "bundle" name)
                                              (member "-post-jar" name))
                                          '() ; skip
                                          `(,tag ,@kids)))))
                       (*default*  . ,(lambda (tag . kids) `(,tag ,@kids)))
                       (*text*     . ,(lambda (_ txt)
                                        (match txt
                                          ;; Remove timestamp.
                                          ("${TODAY}" "(unknown)")
                                          (_ txt))))))
                    port)))

               ;; Build it!
               (invoke "ant"
                       (string-append "-Djavac.classpath=" classpath)
                       "-Dbuilt_by=guix"
                       "-Dbuild.time=01/01/1970 00:00:00"
                       "-Djavac.source=1.8"
                       "-Djavac.target=1.8"
                       (string-append "-Dshort.version="
                                      ,version)
                       ;; TODO: this is the output of:
                       ;; git describe --long --tags --dirty --always
                       (string-append "-Dbuild.version="
                                      ,version "-0-g88b60feb")))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share/axoloti/")))

               ;; TODO: don't copy *everything* from these directories.
               (for-each (lambda (dir)
                           (copy-recursively dir (string-append share "/" dir)))
                         '("api"
                           "archive"
                           "doc"
                           "env"
                           "firmware"
                           "nbproject"
                           "platform_linux"))
               (install-file "dist/Axoloti.jar" share)

               ;; Install old firmware
               (let ((target (string-append share "/old_firmware/firmware-1.0.12"))
                     (old-firmware
                      (string-append (assoc-ref inputs "axoloti-runtime")
                                     "/share/axoloti/firmware/")))
                 (mkdir-p target)
                 (install-file (string-append old-firmware
                                              "flasher/flasher_build/flasher.bin")
                               target)
                 ;; TODO: the old file differs from the file that is included
                 ;; in this repository as the old firmware.
                 (install-file (string-append old-firmware
                                              "build/axoloti.bin")
                               target))

               ;; We do this to ensure that this package retains references to
               ;; other Java packages' jar files.
               (install-file "build.xml" share)

               ;; Create a launcher script
               (mkdir (string-append out "/bin"))
               (let ((target (string-append out "/bin/Axoloti")))
                 (with-output-to-file target
                   (lambda ()
                     (let* ((dir       (string-append (assoc-ref outputs "out")
                                                      "/share/axoloti"))
                            (toolchain (assoc-ref inputs "cross-toolchain"))
                            (includes  (string-append
                                        toolchain
                                        "/arm-none-eabi/include:"
                                        toolchain
                                        "/arm-none-eabi/include/arm-none-eabi/thumb/v7e-m")))
                       (display
                        (string-append "#!" (which "sh") "\n"
                                       "export CROSS_CPATH=" includes "\n"
                                       "export CROSS_CPLUS_INCLUDE_PATH=" includes "\n"
                                       "export CROSS_LIBRARY_PATH="
                                       toolchain "/arm-none-eabi/lib" "\n"
                                       (which "java")

                                       ;; Renderer arguments
                                       " -Xbootclasspath/a:"
                                       (assoc-ref inputs "java-marlin-renderer")
                                       "/lib/marlin.jar"
                                       " -Dsun.java2d.renderer=org.marlin.pisces.MarlinRenderingEngine"
                                       " -Daxoloti_release=" share
                                       " -jar " dir "/Axoloti.jar")))))
                 (chmod target #o555))
               #t)))
         (add-after 'install 'strip-jar-timestamps
           (assoc-ref ant:%standard-phases 'strip-jar-timestamps)))))
    (inputs
     `(("jdk" ,icedtea "jdk")
       ("cross-toolchain" ,arm-none-eabi-nano-toolchain-7-2018-q2-update)
       ;; for compiling patches
       ("make" ,gnu-make)
       ;; for uploading compiled patches and firmware
       ("dfu-util" ,dfu-util-for-axoloti)
       ;; for org/apache/commons/lang3/tuple/ImmutablePair
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-jgit" ,java-jgit-4.2)
       ("java-jsch" ,java-jsch)
       ("java-marlin-renderer" ,java-marlin-renderer)
       ("java-piccolo2d-core" ,java-piccolo2d-core)
       ("java-piccolo2d-extras" ,java-piccolo2d-extras)
       ("java-rsyntaxtextarea" ,java-rsyntaxtextarea)
       ("java-simple-xml" ,java-simple-xml)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-usb4java" ,java-usb4java)))
    (native-inputs
     `(("ant" ,ant)
       ("axoloti-runtime" ,axoloti-runtime) ; for old firmware
       ("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("p7zip" ,p7zip)
       ("zip" ,zip)                     ; for repacking the jar
       ("elfloader"
        ,(let ((commit "d0f93acac6577a069b8739bf42c9500e9a862988"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/JohannesTaelman/elfloader")
                   (commit commit)))
             (file-name (git-file-name "elfloader" commit))
             (sha256
              (base32
               "0zsy6w784amfq4vr1qnx8xvy70v33gvffz1vljllwppdc8s3khmn")))))
       ("chibios"
        ,(origin
           (method url-fetch)
           (uri "http://rwthaachen.dl.osdn.jp/chibios/70739/ChibiOS_19.1.3.7z")
           (sha256
            (base32
             "1090alqp1z1bhj1z8ls04a350kbs8hsjcpkfddbprds73zbnnpk9"))))))
    (home-page "http://www.axoloti.com/")
    (synopsis "Audio development environment for the Axoloti core board")
    (description
     "The Axoloti patcher offers a “patcher” environment similar to Pure Data
for sketching digital audio algorithms.  The patches run on a standalone
powerful microcontroller board: Axoloti Core.")
    (license (list license:gpl3+     ; the whole project and GUI
                   license:bsd-3)))) ; the elfloader
