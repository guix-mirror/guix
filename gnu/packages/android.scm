;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Stefan Handschuh <handschuh.stefan@googlemail.com>
;;; Copyright © 2015 Kai-Chung Yan <seamlikok@gmail.com>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages tls))

;; The Makefiles that we add are largely based on the Debian
;; packages.  They are licensed under GPL-2 and have copyright:
;; 2012, Stefan Handschuh <handschuh.stefan@googlemail.com>
;; 2015, Kai-Chung Yan <seamlikok@gmail.com>
;; Big thanks to them for laying the groundwork.

;; The version tag is consistent between all repositories.
(define (android-platform-version) "7.1.2_r6")

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
      "0xc2n7jxrf1iw9cc278pijdfjix2fkiig5ws27f6rwp40zg5mrgg"))))

(define liblog
  (package
    (name "liblog")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; TODO.
       #:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "liblog") #t))
         (add-after 'enter-source 'create-Makefile
           (lambda _
             ;; No useful makefile is shipped, so we create one.
             (with-output-to-file "Makefile"
               (lambda _
                 (display
                  (string-append
                   "NAME = liblog\n"
                   "SOURCES = log_event_list.c log_event_write.c"
                   " logger_write.c config_write.c logger_name.c"
                   " logger_lock.c fake_log_device.c fake_writer.c"
                   " event_tag_map.c\n"

                   "CFLAGS += -fvisibility=hidden -fPIC\n"
                   "CPPFLAGS += -I../include -DFAKE_LOG_DEVICE=1"
                   ;; Keep these two in sync with "liblog/Android.bp".
                   " -DLIBLOG_LOG_TAG=1005"
                   " -DSNET_EVENT_LOG_TAG=1397638484\n"
                   "LDFLAGS += -shared -Wl,-soname,$(NAME).so.0 -lpthread\n"

                   "build: $(SOURCES)\n"
                   "	$(CC) $^ -o $(NAME).so.0 $(CFLAGS) $(CPPFLAGS) $(LDFLAGS)\n"))
                 #t))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (install-file "liblog.so.0" lib)
               (with-directory-excursion lib
                 (symlink "liblog.so.0" "liblog.so"))
               #t))))))
    (home-page "https://developer.android.com/")
    (synopsis "Logging library from the Android platform.")
    (description "@code{liblog} represents an interface to the volatile Android
Logging system for NDK (Native) applications and libraries and contain
interfaces for either writing or reading logs.  The log buffers are divided up
in Main, System, Radio and Events sub-logs.")
    (license license:asl2.0)))

(define libbase
  (package
    (name "libbase")
    (version (android-platform-version))
    (source (origin
              (inherit (android-platform-system-core version))
              (patches
               (search-patches "libbase-use-own-logging.patch"
                               "libbase-fix-includes.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; TODO.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "base") #t))
         (add-after 'enter-source 'create-Makefile
           (lambda _
             ;; No useful makefile is shipped, so we create one.
             (with-output-to-file "Makefile"
               (lambda _
                 (display
                  (string-append
                   "NAME = libbase\n"
                   "SOURCES = file.cpp logging.cpp parsenetaddress.cpp"
                   " stringprintf.cpp strings.cpp errors_unix.cpp\n"

                   "CXXFLAGS += -std=gnu++11 -fPIC\n"
                   "CPPFLAGS += -Iinclude -I../include\n"
                   "LDFLAGS += -shared -Wl,-soname,$(NAME).so.0"
                   " -L.. -llog\n"

                   "build: $(SOURCES)\n"
                   "	$(CXX) $^ -o $(NAME).so.0 $(CXXFLAGS) $(CPPFLAGS)"
                   " $(LDFLAGS)\n"))
                 #t))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (install-file "libbase.so.0" lib)
               (with-directory-excursion lib
                 (symlink "libbase.so.0" "libbase.so"))
               (copy-recursively "include" out)
               #t))))))
    (inputs `(("liblog" ,liblog)))
    (home-page "https://developer.android.com/")
    (synopsis "Android platform base library")
    (description "@code{libbase} is a library in common use by the
various Android core host applications.")
    (license license:asl2.0)))

(define libcutils
  (package
    (name "libcutils")
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
                    (lib (string-append out "/lib")))
               (install-file "libcutils.so.0" lib)
               (with-directory-excursion lib
                 (symlink "libcutils.so.0" "libcutils.so"))
               #t))))))
    (home-page "https://developer.android.com/")
    (synopsis "Android platform c utils library")
    (description "@code{libcutils} is a library in common use by the
various Android core host applications.")
    (license license:asl2.0)))

(define-public adb
  (package
    (name "adb")
    (version (android-platform-version))
    (source (origin
              (inherit (android-platform-system-core version))
              (patches
               (search-patches "libbase-use-own-logging.patch"
                               "libbase-fix-includes.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "adb") #t))
         (add-before 'build 'fix-clang
           (lambda _
             ;; adb_client.h contains _Nonnull and _Nullable attributes, that
             ;; are not understood by gcc.
             (substitute* "adb_client.h"
                   (("_Nonnull") "")
                   (("_Nullable") ""))
             #t))
         (add-before 'build 'fix-main
           (lambda _
             ;; main.cpp used to be adb_main.cpp in the current directory
             ;; rather than in its own subdirectory, but it was not fixed.
             ;; This leads to some header files not being found anymore.
             (copy-file "client/main.cpp" "adb_main.cpp")
             #t))
         (add-after 'enter-source 'create-Makefile
           (lambda* (#:key outputs #:allow-other-keys)
             ;; No useful makefile is shipped, so we create one.
             (with-output-to-file "Makefile"
               (lambda _
                 (display
                  (string-append
                   ;; Common for all components.
                   "CXXFLAGS += -std=gnu++14 -fpermissive\n"
                   "CPPFLAGS += -I../include -I../base/include -I. -DADB_HOST=1 "
                   "-DADB_REVISION='\"" ,version "\"' -fPIC\n"
                   "LDFLAGS += -lcrypto -lpthread -lbase -lcutils -L. -ladb\n"

                   ;; Libadb specifics.
                   "LIBADB_SOURCES = adb.cpp adb_auth.cpp adb_io.cpp "
                   "adb_listeners.cpp adb_trace.cpp adb_utils.cpp fdevent.cpp "
                   "sockets.cpp transport.cpp transport_local.cpp transport_usb.cpp "
                   "get_my_path_linux.cpp sysdeps_unix.cpp usb_linux.cpp "
                   "adb_auth_host.cpp diagnose_usb.cpp services.cpp "
                   "shell_service_protocol.cpp bugreport.cpp line_printer.cpp\n"

                   "LIBADB_LDFLAGS += -shared -Wl,-soname,libadb.so.0 "
                   "-lcrypto -lpthread -lbase\n"

                   ;; Adb specifics.
                   "ADB_SOURCES = adb_main.cpp console.cpp commandline.cpp "
                   "adb_client.cpp file_sync_client.cpp\n"
                   "ADB_LDFLAGS += -Wl,-rpath=" (assoc-ref outputs "out") "/lib\n"

                   "build: libadb $(ADB_SOURCES)\n"
                   "	$(CXX) $(ADB_SOURCES) -o adb $(CXXFLAGS) $(CPPFLAGS) "
                   "$(ADB_LDFLAGS) $(LDFLAGS)\n"

                   "libadb: $(LIBADB_SOURCES)\n"
                   "	$(CXX) $^ -o libadb.so.0 $(CXXFLAGS) $(CPPFLAGS) "
                   "$(LIBADB_LDFLAGS)\n"
                   "	ln -sv libadb.so.0 libadb.so\n"))
                 #t))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (bin (string-append out "/bin")))
               (install-file "libadb.so.0" lib)
               (install-file "adb" bin)
               (with-directory-excursion lib
                 (symlink "libadb.so.0" "libadb.so"))
               #t))))
       ;; Test suite must be run with attached devices
       #:tests? #f))
    (inputs
     `(("libbase" ,libbase)
       ("libcutils" ,libcutils)
       ("openssl" ,openssl)))
    (home-page "https://developer.android.com/studio/command-line/adb.html")
    (synopsis "Android Debug Bridge")
    (description
     "@command{adb} is a versatile command line tool that lets you communicate
with an emulator instance or connected Android device.  It facilitates a variety
of device actions, such as installing and debugging apps, and it provides access
to a Unix shell that can run commands on the connected device or emulator.")
    (license license:asl2.0)))
