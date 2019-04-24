;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Stefan Handschuh <handschuh.stefan@googlemail.com>
;;; Copyright © 2015 Kai-Chung Yan <seamlikok@gmail.com>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages android)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system android-ndk)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))

(define-public android-make-stub
  (package
    (name "android-make-stub")
    (version "0.6.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/daym/android-make-stub.git")
            (commit (string-append "v" version))))
      (file-name (string-append "android-make-stub-"
                                version "-checkout"))
      (sha256
       (base32
        "0y1b2x96d37n6f1bp6dcx08bn08zac0cylmbfsx6mf2nahc02fhc"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; None exist.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (invoke "make" (string-append "prefix=" out) "install")
                 #t))))))
    (home-page "https://github.com/daym/android-make-stub")
    (synopsis "Stubs for the @command{make} system of the Android platform")
    (description "@code{android-make-stub} provides stubs for the
@command{make} system of the Android platform.  This allows us to
use their packages mostly unmodified in our Android NDK build system.")
    (license license:asl2.0)))

(define-public android-googletest
  (package (inherit googletest)
    (name "android-googletest")
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-host-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (symlink "libgtest.so"
                        (string-append lib "/libgtest_host.so"))
               (symlink "libgmock.so"
                        (string-append lib "/libgmock_host.so"))
               #t))))))))

;; The Makefiles that we add are largely based on the Debian
;; packages.  They are licensed under GPL-2 and have copyright:
;; 2012, Stefan Handschuh <handschuh.stefan@googlemail.com>
;; 2015, Kai-Chung Yan <seamlikok@gmail.com>
;; Big thanks to them for laying the groundwork.

;; The version tag is consistent between all repositories.
(define (android-platform-version) "7.1.2_r36")

(define (android-platform-system-core version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://android.googlesource.com/platform/system/core")
          (commit (string-append "android-" version))))
    (file-name (string-append "android-platform-system-core-"
                              version "-checkout"))
    (sha256
     (base32
      "1krnc2b9zfkzpdgs1dcbji59nszlx2qr723pg89m52622czc06hg"))
    (patches
     (search-patches "libbase-use-own-logging.patch"
                     "libbase-fix-includes.patch"
                     "libutils-remove-damaging-includes.patch"
                     "libutils-add-includes.patch"
                     "adb-add-libraries.patch"
                     "libziparchive-add-includes.patch"))))

(define (android-platform-system-extras version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://android.googlesource.com/platform/system/extras")
          (commit (string-append "android-" version))))
    (file-name (string-append "android-platform-system-extras-"
                              version "-checkout"))
    (sha256
     (base32
      "18130c23ybqcpgjc5v6f8kdbv2xn39hyiaj17dzldjb9rlwzcyy9"))))

(define (android-platform-bionic version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://android.googlesource.com/platform/bionic")
          (commit (string-append "android-" version))))
    (file-name (string-append "android-platform-bionic-"
                              version "-checkout"))
    (sha256
     (base32
      "15r4s20d7vw022f8vrc3jbghmqwdcqzprl7i2bfvdkz8z76wc1ps"))))

(define (android-platform-external version subdirectory checksum)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url
           (string-append "https://android.googlesource.com/platform/external/"
                          subdirectory))
          (commit (string-append "android-" version))))
    (file-name (string-append "android-platform-system-external-" subdirectory "-"
                              version "-checkout"))
    (sha256
     (base32
      checksum))))

(define android-liblog
  (package
    (name "android-liblog")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:make-flags '("LDLIBS=-lpthread")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "liblog") #t))
         (add-after 'install 'ldconfig
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (symlink "liblog.so.0" (string-append out "/lib/liblog.so"))
               #t))))))
    (home-page "https://developer.android.com/")
    (synopsis "Logging library from the Android platform.")
    (description "@code{liblog} represents an interface to the volatile Android
Logging system for NDK (Native) applications and libraries and contain
interfaces for either writing or reading logs.  The log buffers are divided up
in Main, System, Radio and Events sub-logs.")
    (license license:asl2.0)))

(define android-libbase
  (package
    (name "android-libbase")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:tests? #f ; Test failure: logging.UNIMPLEMENTED
       #:make-flags '("CXXFLAGS=-std=gnu++11")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "base") #t)))))
    (inputs `(("android-liblog" ,android-liblog)))
    (home-page "https://developer.android.com/")
    (synopsis "Android platform base library")
    (description "@code{libbase} is a library in common use by the
various Android core host applications.")
    (license license:asl2.0)))

(define android-libcutils
  (package
    (name "android-libcutils")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; TODO.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "libcutils") #t))
         (add-after 'enter-source 'create-Makefile
           (lambda _
             ;; No useful makefile is shipped, so we create one.
             (with-output-to-file "Makefile"
               (lambda _
                 (display
                  (string-append
                   "NAME = libcutils\n"
                   "SOURCES = load_file.o socket_local_client_unix.o"
                   " socket_loopback_client_unix.o socket_network_client_unix.o"
                   " socket_loopback_server_unix.o socket_local_server_unix.o"
                   " sockets_unix.o socket_inaddr_any_server_unix.o"
                   " sockets.o\n"
                   "CC = gcc\n"

                   "CFLAGS += -fPIC\n"
                   "CXXFLAGS += -std=gnu++11 -fPIC\n"
                   "CPPFLAGS += -Iinclude -I../include\n"
                   "LDFLAGS += -shared -Wl,-soname,$(NAME).so.0\n"

                   "build: $(SOURCES)\n"
                   "	$(CXX) $^ -o $(NAME).so.0 $(CXXFLAGS) $(CPPFLAGS)"
                   " $(LDFLAGS)\n"))
                 #t))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (install-file "libcutils.so.0" lib)
               (with-directory-excursion lib
                 (symlink "libcutils.so.0" "libcutils.so"))
               (copy-recursively "../include/cutils"
                                 (string-append include "/cutils"))
               #t))))))
    (home-page "https://developer.android.com/")
    (synopsis "Android platform c utils library")
    (description "@code{libcutils} is a library in common use by the
various Android core host applications.")
    (license license:asl2.0)))

(define-public android-libsparse
  (package
    (name "android-libsparse")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:make-flags '("CFLAGS=-Wno-error"
                      "CXXFLAGS=-fpermissive -Wno-error")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "libsparse") #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://developer.android.com/")
    (synopsis "Android platform sparse library")
    (description "@code{android-libsparse} is a library in common use by the
various Android core host applications.")
    (license license:asl2.0)))

(define-public android-libziparchive
  (package
    (name "android-libziparchive")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:make-flags '("CFLAGS=-Wno-error"
                      "CXXFLAGS=-fpermissive -Wno-error -std=gnu++11")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "libziparchive") #t))
         (add-before 'check 'setenv
           (lambda _
             (setenv "ziparchive_tests_host_PARAMS" "--test_data_dir=testdata")
             #t))
         (add-after 'install 'install-headers
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "../include/ziparchive"
                                 (string-append out "/include/ziparchive"))
               #t))))))
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("android-libbase" ,android-libbase)
       ("android-libutils" ,android-libutils)
       ("android-liblog" ,android-liblog)))
    (home-page "https://developer.android.com/")
    (synopsis "Android platform ZIP library")
    (description "@code{android-libziparchive} is a library in common use by the
various Android core host applications.")
    (license license:asl2.0)))

(define-public adb
  (package
    (name "adb")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:tests? #f ; Test failure: sysdeps_poll.fd_count
       #:make-flags
       (list "CFLAGS=-Wno-error"
             "CXXFLAGS=-fpermissive -Wno-error -std=gnu++14 -D_Nonnull= -D_Nullable= -I ."
             (string-append "LDFLAGS=-Wl,-rpath=" (assoc-ref %outputs "out") "/lib "
                            "-Wl,-rpath=" (assoc-ref %build-inputs "openssl") "/lib -L ."))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "adb") #t))
         (add-after 'enter-source 'glibc-compat
           (lambda _
             ;; Include sysmacros.h for "major" and "minor" in Glibc 2.28.
             (substitute* "usb_linux.cpp"
               (("#include <sys/types.h>" all)
                (string-append all "\n#include <sys/sysmacros.h>\n")))
             #t))
         (add-after 'enter-source 'make-libs-available
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "Android.mk"
              (("libcrypto_static") "libcrypto"))
             #t))
         (add-after 'install 'install-headers
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (install-file "diagnose_usb.h" (string-append (assoc-ref outputs "out") "/include"))
             #t)))))
    (inputs
     `(("android-libbase" ,android-libbase)
       ("android-libcutils" ,android-libcutils)
       ("android-liblog" ,android-liblog)
       ("openssl" ,openssl)))
    (home-page "https://developer.android.com/studio/command-line/adb.html")
    (synopsis "Android Debug Bridge")
    (description
     "@command{adb} is a versatile command line tool that lets you communicate
with an emulator instance or connected Android device.  It facilitates a variety
of device actions, such as installing and debugging apps, and it provides access
to a Unix shell that can run commands on the connected device or emulator.")
    (license license:asl2.0)))

(define-public mkbootimg
  (package
    (name "mkbootimg")
    (version (android-platform-version))
    (source (origin
              (inherit (android-platform-system-core version))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "mkbootimg") #t))
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (include (string-append out "/include")))
               (install-file "mkbootimg" bin)
               (install-file "bootimg.h" include)
               #t))))))
    (home-page "https://developer.android.com/studio/command-line/adb.html")
    (synopsis "Tool to create Android boot images")
    (description "This package provides a tool to create Android Boot
Images.")
    (license license:asl2.0)))

(define-public android-safe-iop
  (package
    (name "android-safe-iop")
    (version (android-platform-version))
    (source (android-platform-external version "safe-iop"
                                       "1nyyrs463advjhlq8xx1lm37m4g5afv7gy0csxrj7biwwl0v13qw"))
    (build-system android-ndk-build-system)
    (arguments
     `(#:make-flags '("CXXFLAGS=-fpermissive -Wno-error")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-host
           (lambda _
             ;; TODO: Cross-compile.
             (substitute* "Android.mk"
              (("BUILD_STATIC_LIBRARY") "BUILD_HOST_STATIC_LIBRARY"))
             #t)))))
    (home-page "https://developer.android.com/")
    (synopsis "Safe integers in C")
    (description "@code{android-safe-iop} provides a set of functions for
performing and checking safe integer operations.  Ensure that integer
operations do not result in silent overflow.")
    (license license:bsd-2)))

(define-public android-bionic-uapi
  (package
    (name "android-bionic-uapi")
    (version (android-platform-version))
    (source (android-platform-bionic version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'enter-source
           (lambda _ (chdir "libc") #t))
        (replace 'check
          (const #t))
        (replace 'build
          (const #t))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (out-sys (string-append out "/include/sys")))
              (mkdir-p out-sys)
              (install-file "include/sys/system_properties.h" out-sys)
              (install-file "include/sys/_system_properties.h" out-sys)
              (copy-recursively "kernel/uapi" (string-append out "/include"))
              #t))))))
    (home-page "https://developer.android.com/")
    (synopsis "Android Linux API that is safe for user space")
    (description "@code{android-bionic-uapi} provides the part of the Linux API
that is safe to use for user space.  It also includes
@code{system_properties.h} and @code{_system_properties.h}.")
    (license license:asl2.0)))

(define-public android-libselinux
  (package
    (name "android-libselinux")
    (version (android-platform-version))
    (source
     (android-platform-external version "libselinux"
                                "13m2q32gzdcs5d0zj1nwasjy1j8vsxsgbjg7m5sa9lfcjaj7nkm7"))
    (build-system android-ndk-build-system)
    (arguments
     ;; See logd/Android.mk for the *_LOG_TAG values.
     `(#:make-flags (list (string-append "CFLAGS=-Wno-error "
                                         "-I core/include "
                                         "-I core/libpackagelistparser/include "
                                         "-DAUDITD_LOG_TAG=1003 "
                                         "-DLOGD_LOG_TAG=1004 -D_GNU_SOURCE")
                          "LDFLAGS=-L . -lpcre")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack-core 'patch-HOST
           (lambda _
             ;; gettid duplicates otherwise.
             (substitute* "src/procattr.c"
              (("#ifdef HOST") "#ifdef XXX"))
             #t)))))
    (inputs
     `(("openssl" ,openssl)))
    (native-inputs
     `(("android-bionic-uapi" ,android-bionic-uapi)
       ;; pcre is inlined by our package.
       ("pcre" ,pcre)))
    (home-page "https://developer.android.com/")
    (synopsis (package-synopsis libselinux))
    (description (package-description libselinux))
    (license (package-license libselinux))))

(define-public android-ext4-utils
  (package
    (name "android-ext4-utils")
    (version (android-platform-version))
    (source (android-platform-system-extras version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CPPFLAGS="
                            ;"-Wno-error "
                            "-I "
                            (assoc-ref %build-inputs "android-libselinux")
                            "/include "
                            "-I " (assoc-ref %build-inputs "android-libsparse")
                            "/include "
                            "-I " (assoc-ref %build-inputs "android-libcutils")
                            "/include "
                            "-I " (assoc-ref %build-inputs "android-liblog") "/include "
                            "-I ../core/include")
             "CFLAGS=-Wno-error"
             "install-libext4_utils_host.a"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-core
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "core")
             (with-directory-excursion "core"
               (invoke "tar" "axf" (assoc-ref inputs "android-core")
                             "--strip-components=1"))
             #t))
         (add-after 'unpack-core 'enter-source
           (lambda _ (chdir "ext4_utils") #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "." (string-append out "/include")))
             #t)))))
    (inputs
     `(("android-libcutils" ,android-libcutils)
       ("android-liblog" ,android-liblog)
       ("android-libselinux" ,android-libselinux)
       ("android-libsparse" ,android-libsparse)
       ("zlib" ,zlib)))
    (native-inputs
     `(("android-core" ,(android-platform-system-core version))))
    (home-page "https://developer.android.com/")
    (synopsis "Android ext4 file system utilities")
    (description "@code{android-ext4-utils} is a library in common use by the
Android core.")
    (license license:asl2.0)))

(define-public android-f2fs-utils
  (package
    (name "android-f2fs-utils")
    (version (android-platform-version))
    (source (android-platform-system-extras version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "f2fs_utils") #t))
         (add-after 'install 'install-headers
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (copy-recursively "." (string-append (assoc-ref outputs "out")
                                                  "/include"))
             #t))
         (add-after 'install 'install-shell-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (patch-shebang "mkf2fsuserimg.sh")
               (substitute* "mkf2fsuserimg.sh"
                (("make_f2fs") (string-append bin "/make_f2fs")))
               (install-file "mkf2fsuserimg.sh" bin)
               #t))))))
    (inputs
     `(("f2fs-tools" ,f2fs-tools-1.7)
       ("android-libselinux" ,android-libselinux)
       ("android-libsparse" ,android-libsparse)
       ("android-libcutils" ,android-libcutils)
       ("zlib" ,zlib)))
    (home-page "https://developer.android.com/")
    (synopsis "Android f2fs utils")
    (description "@code{android-f2fs-utils} is a library in common use by the
Android core.  It allows the user to create images for the @code{f2fs} Flash
file system.")
    (license license:asl2.0)))

(define-public android-libutils
  (package
    (name "android-libutils")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:tests? #f ; TODO
       #:make-flags '("CXXFLAGS=-std=gnu++11 -Wno-error")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "libutils") #t))

         (add-after 'install 'install-headers
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (copy-recursively "../include/utils" (string-append (assoc-ref outputs "out") "/include/utils")))))))
    (inputs
     `(("android-safe-iop" ,android-safe-iop)
       ("android-libcutils" ,android-libcutils)))
    (native-inputs
     `(("android-bionic-uapi" ,android-bionic-uapi)
       ("android-liblog" ,android-liblog)))
    (home-page "https://developer.android.com/")
    (synopsis "Android utility library")
    (description "@code{android-libutils} provides utilities for Android NDK developers.")
    (license license:asl2.0)))

(define-public fastboot
  (package
    (name "fastboot")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:make-flags (list "CXXFLAGS=-std=gnu++11")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _
             (chdir "fastboot")
             #t))
         (add-after 'enter-source 'patch-source
           (lambda _
             (substitute* "Android.mk"
              (("libext4_utils_host") "libext4_utils_host libselinux libpcre"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (bin (string-append out "/bin")))
               (install-file "fastboot" bin)
               #t))))))
    (inputs
     `(("adb" ,adb)
       ("android-safe-iop" ,android-safe-iop)
       ("android-ext4-utils" ,android-ext4-utils)
       ("android-f2fs-utils" ,android-f2fs-utils)
       ("android-libbase" ,android-libbase)
       ("android-libcutils" ,android-libcutils)
       ("android-liblog" ,android-liblog)
       ("android-libutils" ,android-libutils)
       ("android-libsparse" ,android-libsparse)
       ("android-libziparchive" ,android-libziparchive)
       ("android-libselinux" ,android-libselinux)
       ("pcre" ,pcre)
       ("mkbootimg" ,mkbootimg)
       ("zlib" ,zlib)))
    (native-inputs
     `(("xz" ,xz)))
    (home-page "https://developer.android.com/studio/command-line/")
    (synopsis "Android image flasher")
    (description
     "This package provides @command{fastboot}, a tool to upload file system images to Android devices.")
    (license license:asl2.0)))

(define-public android-udev-rules
  (package
    (name "android-udev-rules")
    (version "20180112")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/M0Rf30/android-udev-rules")
             (commit version)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "13gj79nnd04szqlrrzzkdr6wi1fky08pi7x8xfbg0jj3d3v0giah"))))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)))
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/51-android.rules")
                         (string-append %output "/lib/udev/rules.d"))
           #t))))
    (home-page "https://github.com/M0Rf30/android-udev-rules")
    (synopsis "udev rules for Android devices")
    (description "Provides a set of udev rules to allow using Android devices
with tools such as @command{adb} and @command{fastboot} without root
privileges.  This package is intended to be added as a rule to the
@code{udev-service-type} in your @code{operating-system} configuration.
Additionally, an @code{adbusers} group must be defined and your user added to
it.

@emph{Simply installing this package will not have any effect.}  It is meant
to be passed to the @code{udev} service.")
    (license license:gpl3+)))

(define-public git-repo
  (package
    (name "git-repo")
    (version "1.12.37")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gerrit.googlesource.com/git-repo")
             (commit (string-append "v" version))))
       (file-name (string-append "git-repo-" version "-checkout"))
       (sha256
        (base32 "0qp7jqhblv7xblfgpcq4n18dyjdv8shz7r60c3vnjxx2fngkj2jd"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; code says: "Python 3 support is … experimental."
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-executable-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (git (assoc-ref inputs "git"))
                    (gpg (assoc-ref inputs "gnupg"))
                    (ssh (assoc-ref inputs "ssh")))
               (substitute* '("repo" "git_command.py")
                 (("^GIT = 'git' ")
                  (string-append "GIT = '" git "/bin/git' ")))
               (substitute* "repo"
                 ((" cmd = \\['gpg',")
                  (string-append " cmd = ['" gpg "/bin/gpg',")))
               (substitute* "git_config.py"
                 ((" command_base = \\['ssh',")
                  (string-append " command_base = ['" ssh "/bin/ssh',")))
               #t)))
         (add-before 'build 'do-not-clone-this-source
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (repo-dir (string-append out "/share/" ,name)))
               (substitute* "repo"
                 (("^def _FindRepo\\(\\):.*")
                  (format #f "
def _FindRepo():
  '''Look for a repo installation, starting at the current directory.'''
  # Use the installed version of git-repo.
  repo_main = '~a/main.py'
  curdir = os.getcwd()
  olddir = None
  while curdir != '/' and curdir != olddir:
    dot_repo = os.path.join(curdir, repodir)
    if os.path.isdir(dot_repo):
      return (repo_main, dot_repo)
    else:
      olddir = curdir
      curdir = os.path.dirname(curdir)
  return None, ''

  # The remaining of this function is dead code.  It was used to
  # find a git-checked-out version in the local project.\n" repo-dir))
                 ;; Neither clone, check out, nor verify the git repository
                 (("(^\\s+)_Clone\\(.*\\)") "")
                 (("(^\\s+)_Checkout\\(.*\\)") "")
                 ((" rev = _Verify\\(.*\\)") " rev = None"))
               #t)))
         (delete 'build) ; nothing to build
         (replace 'check
           (lambda _
             (invoke "python" "-m" "nose")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin-dir (string-append out "/bin"))
                    (repo-dir (string-append out "/share/" ,name)))
               (mkdir-p bin-dir)
               (mkdir-p repo-dir)
               (copy-recursively "." repo-dir)
               (delete-file-recursively (string-append repo-dir "/tests"))
               (symlink (string-append repo-dir "/repo")
                        (string-append bin-dir "/repo"))
               #t))))))
    (inputs
     ;; TODO: Add git-remote-persistent-https once it is available in guix
     `(("git" ,git)
       ("gnupg" ,gnupg)
       ("ssh" ,openssh)))
    (native-inputs
     `(("nose" ,python2-nose)))
    (home-page "https://code.google.com/p/git-repo/")
    (synopsis "Helps to manage many Git repositories.")
    (description "Repo is a tool built on top of Git.  Repo helps manage many
Git repositories, does the uploads to revision control systems, and automates
parts of the development workflow.  Repo is not meant to replace Git, only to
make it easier to work with Git.  The repo command is an executable Python
script that you can put anywhere in your path.")
    (license license:asl2.0)))

(define-public abootimg
  (package
    (name "abootimg")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://http.debian.net/debian/pool/main/a/abootimg/"
                           "abootimg_" version ".orig.tar.gz"))
       (sha256
        (base32 "0sfc2k011l1ymv97821w89391gnqdh8pp0haz4sdcm5hx0axv2ba"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
        (replace 'configure
          (lambda _
            (setenv "CC" "gcc")
            #t))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin")))
              (install-file "abootimg" bin)
              #t))))))
    (inputs
     `(("libblkid" ,util-linux)))
    (home-page "https://ac100.grandou.net/abootimg")
    (synopsis "Tool for manipulating Android Boot Images")
    (description "This package provides a tool for manipulating old Android
Boot Images.  @code{abootimg} can work directly on block devices, or, the
safest way, on a file image.")
    (license license:gpl2+)))

(define-public python-androguard
  (package
    (name "python-androguard")
    (version "3.2.1")
    (source
      (origin
        ;; The pypi release doesn't have the tests, but the tests use
        ;; packaged binaries, so we skip them.
        (method url-fetch)
        (uri (pypi-uri "androguard" version))
        (sha256
         (base32
          "0ndsw00pkyda4i2s3wi5ap8gbk6a9d23xhhxpdbk02padv8sxkfv"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; Adapted from .travis.yml
           (lambda _
             (invoke "nosetests" "--with-coverage" "--with-timer"
                     "--timer-top-n" "50"))))))
    (native-inputs
     `(("python-codecov" ,python-codecov)
       ("python-coverage" ,python-coverage)
       ("python-mock" ,python-mock)
       ("python-nose" ,python-nose)
       ("python-nose-timer" ,python-nose-timer)))
    (propagated-inputs
     `(("python-asn1crypto" ,python-asn1crypto)
       ("python-colorama" ,python-colorama)
       ("python-future" ,python-future)
       ("python-ipython" ,python-ipython)
       ("python-lxml" ,python-lxml)
       ("python-matplotlib" ,python-matplotlib)
       ("python-networkx" ,python-networkx)
       ("python-pygments" ,python-pygments)
       ("python-pyperclip" ,python-pyperclip)))
    (home-page "https://github.com/androguard/androguard")
    (synopsis "Python tool to play with Android files")
    (description
     "Androguard is a full Python tool to manipulate Android files.  It is
useful for reverse engineering, analysis of Android applications and more.")
    (license license:asl2.0)))

(define-public fdroidserver
  (package
    (name "fdroidserver")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fdroidserver" version))
        (sha256
         (base32
          "0fp7q8faicx6i6wxm717qqaham3jpilb23mvynpz6v73z7hm6wcg"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-versioning
           (lambda _
             (substitute* "setup.py"
               (("0.2.1") ,(package-version python-pyasn1-modules)))
             #t)))))
    (propagated-inputs
     `(("python-androguard" ,python-androguard)
       ("python-apache-libcloud" ,python-apache-libcloud)
       ("python-clint" ,python-clint)
       ("python-defusedxml" ,python-defusedxml)
       ("python-docker-py" ,python-docker-py)
       ("python-gitpython" ,python-gitpython)
       ("python-mwclient" ,python-mwclient)
       ("python-paramiko" ,python-paramiko)
       ("python-pillow" ,python-pillow)
       ("python-pyasn1" ,python-pyasn1)
       ("python-pyasn1-modules" ,python-pyasn1-modules)
       ("python-pyyaml" ,python-pyyaml)
       ("python-qrcode" ,python-qrcode)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-requests" ,python-requests)
       ("python-vagrant" ,python-vagrant)))
    (native-inputs
     `(("python-babel" ,python-babel)
       ("python-bcrypt" ,python-bcrypt)
       ("python-docker-pycreds" ,python-docker-pycreds)
       ("python-pynacl" ,python-pynacl)
       ("python-websocket-client" ,python-websocket-client)))
    (home-page "https://f-droid.org")
    (synopsis "F-Droid server tools")
    (description
     "The F-Droid server tools provide various scripts and tools that are used
to maintain F-Droid, the repository of free Android applications.  You can use
these same tools to create your own additional or alternative repository for
publishing, or to assist in creating, testing and submitting metadata to the
main repository.")
    (license license:agpl3+)))
