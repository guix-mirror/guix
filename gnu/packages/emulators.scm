;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2018 Rutger Helling <rhelling@mykolab.com>
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

(define-module (gnu packages emulators)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake))

;; Building from recent Git because the official 5.0 release no longer builds.
(define-public dolphin-emu
  (let ((commit "d04b179111f8d863f360839474cb82c766f762b8")
        (revision "0"))
    (package
      (name "dolphin-emu")
      (version (git-version "5.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolphin-emu/dolphin.git")
               (commit commit)))
         (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                            ;; Remove external stuff we don't need.
                            '("Externals/LZO"
                              "Externals/OpenAL"
                              "Externals/Qt"
                              "Externals/SFML"
                              "Externals/SOIL"
                              "Externals/curl"
                              "Externals/ffmpeg"
                              "Externals/gettext"
                              "Externals/hidapi"
                              "Externals/libpng"
                              "Externals/libusb"
                              "Externals/mbedtls"
                              "Externals/miniupnpc"
                              "Externals/wxWidgets3"
                              "Externals/zlib"))
                  ;; Clean up source.
                  (for-each delete-file (find-files "." "\\.bin$"))
                  (for-each delete-file (find-files "." "\\.dsy$"))
                  (for-each delete-file (find-files "." "\\.exe$"))
                  (for-each delete-file (find-files "." "\\.jar$"))
                  (for-each delete-file (find-files "." "\\.rar$"))
                  #t))
         (sha256
          (base32
           "0g725wmhlim73zrhi47wmr1bmplpy4b7sbimd5pm8xpfhj5nm10l"))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f
         ;; The FindGTK2 cmake script only checks hardcoded directories for
         ;; glib/gtk headers.

         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'generate-fonts
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((fontfile
                      (string-append (assoc-ref inputs "font-wqy-microhei")
                                     "/share/fonts/truetype/wqy-microhei.ttc")))
                 (chdir "docs")
                 (invoke "bash" "-c" "g++ -O2 -std=c++11 $(freetype-config \
--cflags --libs) gc-font-tool.cpp -o gc-font-tool")
                 (invoke "./gc-font-tool" "a" fontfile "font_western.bin")
                 (invoke "./gc-font-tool" "s" fontfile "font_japanese.bin")
                 (copy-file "font_japanese.bin" "../Data/Sys/GC/font_japanese.bin")
                 (copy-file "font_western.bin" "../Data/Sys/GC/font_western.bin")
                 (chdir "..")
                 #t))))

         #:configure-flags
         (list (string-append "-DGTK2_GDKCONFIG_INCLUDE_DIR="
                              (assoc-ref %build-inputs "gtk+")
                              "/lib/gtk-2.0/include")
               (string-append "-DGTK2_GLIBCONFIG_INCLUDE_DIR="
                              (assoc-ref %build-inputs "glib")
                              "/lib/glib-2.0/include")
               (string-append "-DX11_INCLUDE_DIR="
                              (assoc-ref %build-inputs "libx11")
                              "/include")
               (string-append "-DX11_LIBRARIES="
                              (assoc-ref %build-inputs "libx11")
                              "/lib/libX11.so")
               "-DX11_FOUND=1")))
      ; TODO: Make Vulkan backend work.
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("gettext" ,gnu-gettext)))
      (inputs
       `(("alsa-lib" ,alsa-lib)
         ("ao" ,ao)
         ("bluez" ,bluez)
         ("curl" ,curl)
         ("eudev" ,eudev)
         ("ffmpeg" ,ffmpeg)
         ("font-wqy-microhei" ,font-wqy-microhei)
         ("freetype" ,freetype)
         ("glew" ,glew)
         ("glib" ,glib)
         ("glu" ,glu)
         ("gtk+" ,gtk+-2)
         ("hidapi" ,hidapi)
         ("libevdev" ,libevdev)
         ("libpng" ,libpng)
         ("libusb" ,libusb)
         ("libx11" ,libx11)
         ("libxi" ,libxi)
         ("libxrandr" ,libxrandr)
         ("lzo" ,lzo)
         ("mbedtls-apache" ,mbedtls-apache)
         ("mesa" ,mesa)
         ("miniupnpc" ,miniupnpc)
         ("openal" ,openal)
         ("pulseaudio" ,pulseaudio)
         ("qtbase" ,qtbase)
         ("sdl2" ,sdl2)
         ("sfml" ,sfml)
         ("soil" ,soil)
         ("soundtouch" ,soundtouch)
         ("wxwidgets" ,wxwidgets-gtk2-3.1)
         ("zlib" ,zlib)))
      (home-page "https://dolphin-emu.org/")
      (synopsis "Nintendo Wii and GameCube emulator")
      (description
       "Dolphin is an emulator for two Nintendo video game consoles: the
GameCube and the Wii.  It provides compatibility with all PC controllers,
turbo speed, networked multiplayer, and graphical enhancements.")
      (supported-systems '("x86_64-linux")) ; TODO: should also work on ARM64.
      ; dolphin/Data/Sys/GC/font_*.bin: Licensed under ASL2.0.
      (license (list license:gpl2+ license:asl2.0 license:fdl1.2+)))))
