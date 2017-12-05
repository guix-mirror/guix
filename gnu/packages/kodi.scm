;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu packages kodi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages assembly))

(define-public crossguid
  (let ((commit "8f399e8bd4252be9952f3dfa8199924cc8487ca4"))
    (package
      (name "crossguid")
      (version (string-append "0.0-1." (string-take commit 7)))
      ;; There's no official release.  Just a Git repository.
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/graeme-hill/crossguid.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1i29y207qqddvaxbn39pk2fbh3gx8zvdprfp35wasj9rw2wjk3s9"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (delete 'configure) ; no configure script
           ;; There's no build system here, so we have to do it ourselves.
           (replace 'build
             (lambda _
               (and (zero? (system* "g++" "-c" "guid.cpp" "-o" "guid.o"
                                    "-std=c++11" "-DGUID_LIBUUID"))
                    (zero? (system* "ar" "rvs" "libcrossguid.a" "guid.o")))))
           (replace 'check
             (lambda _
               (and (zero? (system* "g++" "-c" "test.cpp" "-o" "test.o"
                                    "-std=c++11"))
                    (zero? (system* "g++" "-c" "testmain.cpp" "-o" "testmain.o"
                                    "-std=c++11"))
                    (zero? (system* "g++" "test.o" "guid.o" "testmain.o"
                                    "-o" "test" "-luuid"))
                    (zero? (system* (string-append (getcwd) "/test"))))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "guid.h" (string-append out "/include"))
                 (install-file "libcrossguid.a"
                               (string-append out "/lib"))
                 #t))))))
      (inputs
       `(("util-linux" ,util-linux)))
      (synopsis "Lightweight universal identifier library")
      (description "CrossGuid is a minimal GUID/UUID
generator library for C++.")
      (home-page "https://github.com/graeme-hill/crossguid")
      (license license:expat))))

;; Kodi requires using their own special forks of these libraries.
;; In addition, it insists on downloading and building these as part
;; of the standard build process. To make things easier, we bootstrap
;; and patch shebangs here, so we don't have to worry about it later.
(define libdvdnav/kodi
  (let ((commit "981488f7f27554b103cca10c1fbeba027396c94a"))
    (package
      (name "libdvdnav-bootstrapped")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xbmc/libdvdnav.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "089pswc51l3avh95zl4cpsh7gh1innh7b2y4xgx840mcmy46ycr8"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'bootstrap
             (lambda _ (zero? (system* "autoreconf" "-vif"))))
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "." (assoc-ref outputs "out"))
               #t)))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/xbmc/libdvdnav")
      (synopsis (package-synopsis libdvdnav))
      (description (package-description libdvdnav))
      (license license:gpl2+))))

(define libdvdread/kodi
  (let ((commit "17d99db97e7b8f23077b342369d3c22a6250affd"))
    (package
      (name "libdvdread-bootstrapped")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xbmc/libdvdread.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1gr5aq1cjr3as9mnwrw29cxn4m6f6pfrxdahkdcjy70q3ldg90sl"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'bootstrap
             (lambda _ (zero? (system* "autoreconf" "-vif"))))
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "." (assoc-ref outputs "out"))
               #t)))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/xbmc/libdvdread")
      (synopsis (package-synopsis libdvdread))
      (description (package-description libdvdread))
      (license (list license:gpl2+ license:lgpl2.1+)))))

(define libdvdcss/kodi
  (let ((commit "2f12236bc1c92f73c21e973363f79eb300de603f"))
    (package
      (name "libdvdcss-bootstrapped")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xbmc/libdvdcss.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "198r0q73i55ga1dvyqq9nfcri0zq08b94hy8671lg14i3izx44dd"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'bootstrap
             (lambda _ (zero? (system* "autoreconf" "-vif"))))
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "." (assoc-ref outputs "out"))
               #t)))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/xbmc/libdvdcss")
      (synopsis (package-synopsis libdvdcss))
      (description (package-description libdvdcss))
      (license license:gpl2+))))

(define-public kodi
  ;; We package the git version because the current released
  ;; version was cut while the cmake transition was in turmoil.
  (let ((commit "67fd70f01a363002881f3519b50765b756716e3b")
        (revision "7"))
  (package
    (name "kodi")
    (version (string-append "18.0_alpha-" revision "-" (string-take commit 7)))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xbmc/xbmc.git")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "12975n4r982kmxc0r9w24n3lrj7aj3cs4fjkdjnn0r9jbnvfxhs3"))
              (snippet
               '(begin
                  (use-modules (guix build utils))
                  (for-each delete-file-recursively
                            '("project/BuildDependencies/bin/"
                              ;; TODO: Purge these jars.
                              ;;"tools/codegenerator/groovy"
                              ;; And these sources:
                              ;; "tools/depend/native/JsonSchemaBuilder"
                              ;; "tools/depend/native/TexturePacker"
                              ;; "lib/gtest"
                              ;; "lib/cpluff"
                              ;; "lib/libexif"
                              ;; "lib/libUPnP"
                              "lib/libUPnP/Neptune/ThirdParty"
                              "project/Win32BuildSetup/tools/7z"))
                  #t))
              (modules '((guix build utils)))))
    (build-system cmake-build-system)
    (arguments
     '(#:modules ((srfi srfi-1)
                  (guix build cmake-build-system)
                  (guix build utils))
       #:configure-flags
       (list "-DENABLE_INTERNAL_FFMPEG=OFF"
             "-DENABLE_INTERNAL_CROSSGUID=OFF"
             (string-append "-Dlibdvdread_URL="
                            (assoc-ref %build-inputs "libdvdread-bootstrapped"))
             (string-append "-Dlibdvdnav_URL="
                            (assoc-ref %build-inputs "libdvdnav-bootstrapped"))
             (string-append "-Dlibdvdcss_URL="
                            (assoc-ref %build-inputs "libdvdcss-bootstrapped"))
             (string-append "-DSYSTEM_LDFLAGS=-Wl,-rpath="
                            (assoc-ref %build-inputs "curl") "/lib"))
       #:phases
       (modify-phases %standard-phases
         ;; The build system tries to bootstrap these bundled components
         ;; during the regular build phase, which causes serious issues
         ;; because there's no time for shebangs to be patched.  So, we
         ;; bootstrap it on our own instead.
         (add-after 'unpack 'bootstrap-bundled-software
           (lambda _
             (let ((dirs '("tools/depends/native/JsonSchemaBuilder/src"
                           "lib/cpluff")))
               (every (lambda (third-party)
                        (with-directory-excursion third-party
                          (zero? (system* "autoreconf" "-vif"))))
                      dirs))))
         (add-after 'bootstrap-bundled-software 'patch-stuff
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Prevent the build scripts from calling autoreconf in the
             ;; build stage.  Otherwise, it would undo the bootstrapping
             ;; and shebang patching that we worked so hard for.
             (substitute* "cmake/modules/FindCpluff.cmake"
               (("autoreconf -vif") "true"))
             (substitute* "lib/cpluff/po/Makefile.in.in"
               (("/bin/sh") (which "sh")))
             (substitute* "cmake/modules/FindLibDvd.cmake"
               ;; The libdvd* sources that we bootstrapped separately are
               ;; unpacked in the build phase. This is our best opportunity
               ;; to make them writable before the build process starts.
               (("autoreconf -vif") "chmod -R u+w ."))

             (substitute* "xbmc/linux/LinuxTimezone.cpp"
               (("/usr/share/zoneinfo")
                (string-append (assoc-ref inputs "tzdata")
                               "/share/zoneinfo")))

             ;; Let's disable some tests that are known not to work here.
             ;; Doing this later while in the cmake "../build" directory
             ;; is trickier.
             (substitute* '("xbmc/utils/test/TestSystemInfo.cpp")
               (("TEST_F\\(TestSystemInfo, GetOsPrettyNameWithVersion\\)")
                "TEST_F(TestSystemInfo, DISABLED_GetOsPrettyNameWithVersion)")
               (("TEST_F\\(TestSystemInfo, GetOsName\\)")
                "TEST_F(TestSystemInfo, DISABLED_GetOsName)")
               (("TEST_F\\(TestSystemInfo, GetOsVersion\\)")
                "TEST_F(TestSystemInfo, DISABLED_GetOsVersion)"))
             #t))
         (add-before 'build 'set-build-environment
           (lambda _
             ;; Some bundled build scripts fall back to /bin/sh
             ;; if this is not set.
             (setenv "CONFIG_SHELL" (which "sh"))
             #t))
         (add-before 'check 'build-kodi-test
           (lambda _
             (zero? (system* "make" "kodi-test")))))))
    ;; TODO: Add dependencies for:
    ;; - nfs
    ;; - cec
    ;; - plist
    ;; - shairplay
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("icedtea" ,icedtea) ; needed at build-time only, mandatory
       ("libdvdcss-bootstrapped" ,libdvdcss/kodi)
       ("libdvdnav-bootstrapped" ,libdvdnav/kodi)
       ("libdvdread-bootstrapped" ,libdvdread/kodi)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("swig" ,swig)
       ("yasm" ,yasm)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("avahi" ,avahi)
       ("bluez" ,bluez)
       ("crossguid" ,crossguid)
       ("curl" ,curl)
       ("dcadec" ,dcadec)
       ("dbus" ,dbus)
       ("eudev" ,eudev)
       ("ffmpeg" ,ffmpeg)
       ("flac" ,flac)
       ("fmt" ,fmt)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("giflib" ,giflib)
       ("glew" ,glew)
       ("gnutls" ,gnutls)
       ("lame" ,lame)
       ("lcms" ,lcms)
       ("libass" ,libass)
       ("libbluray" ,libbluray)
       ("libcap" ,libcap)
       ("libcdio" ,libcdio)
       ("libdrm" ,libdrm)
       ("libgcrypt" ,libgcrypt)
       ("libjpeg" ,libjpeg)
       ("libltdl" ,libltdl)
       ("libmad" ,libmad)
       ("libmicrohttpd" ,libmicrohttpd)
       ("libmpeg2" ,libmpeg2)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libssh" ,libssh)
       ("libtiff" ,libtiff)
       ("libva" ,libva)
       ("libvorbis" ,libvorbis)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxslt" ,libxslt)
       ("lzo" ,lzo)
       ("mysql" ,mysql)
       ("openssl" ,openssl)
       ("pcre" ,pcre)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python-2)
       ("rapidjson" ,rapidjson)
       ("samba" ,samba)
       ("sqlite" ,sqlite)
       ("taglib" ,taglib)
       ("tinyxml" ,tinyxml)
       ("tzdata" ,tzdata)
       ("util-linux" ,util-linux)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (synopsis "Media center for home theater computers")
    (description "Kodi is a media center application for playing videos,
music, games, etc.  Kodi is highly customizable and features a theme and
plug-in system.")
    (home-page "http://kodi.tv")
    ;; XBMC is largely GPL2+, with some library components as LGPL2.1+, but
    ;; there are some other licenses spread throughout.
    (license (list license:gpl2+ license:lgpl2.1+
                   license:gpl3+                  ;WiiRemote client
                   license:expat                  ;cpluff, dbwrappers
                   license:public-domain          ;cpluff/examples
                   license:bsd-3                  ;misc, gtest
                   license:bsd-2)))))             ;xbmc/freebsd

(define-public kodi-cli
  (let ((commit "104dc23b2a993c8e6db8c46f4f8bec24b146549b") ; Add support for
        (revision "1"))                                     ; `$HOME/.kodirc'.
    (package
      (name "kodi-cli")
      (version (string-append "1.1-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference (url "https://github.com/nawar/kodi-cli")
                                    (commit commit)))
                (sha256
                 (base32
                  "1xjhasc5gngfxpr1dlzy6q24w0wpdfjx12p43fanjppxw4i49n5p"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system trivial-build-system)
      (inputs
       `(("bash"        ,bash)
         ("curl"        ,curl)
         ("mps-youtube" ,mps-youtube)))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (substitute* "kodi-cli"
             (("/bin/bash") (string-append (assoc-ref %build-inputs "bash")
                                           "/bin/bash"))
             (("output=\\$\\((curl)" all curl)
              (string-append "output=$("
                             (assoc-ref %build-inputs "curl")
                             "/bin/" curl))
             (("play_youtube `(mpsyt)" all mpsyt)
              (string-append "play_youtube `"
                             (assoc-ref %build-inputs "mps-youtube")
                             "/bin/" mpsyt)))
           (install-file "kodi-cli" (string-append %output "/bin"))
           #t)))
      (home-page "https://github.com/nawar/kodi-cli")
      (synopsis "Control Kodi from the command line")
      (description "@code{kodi-cli} is a tool for sending commands to a Kodi
server using JSON RPC.

Features:

@itemize
@item Play, pause, stop the currently playing item.
@item Skip forward or backward in the currently playing item.
@item Play or queue to the currently list of YouTube videos.
@item Interactive and noninteractive volume control.
@item Interactive navigation.
@item Send text to the Kodi keyboard.
@item Toggle fullscreen.
@item Update or clean Kodi libraries.
@end itemize\n")
      (license license:gpl2+))))
