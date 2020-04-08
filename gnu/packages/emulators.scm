;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2016 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2018 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2016 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2017, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
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

(define-module (gnu packages emulators)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages web)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu))

(define-public desmume
  (package
    (name "desmume")
    (version "0.9.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/desmume/desmume/"
             version "/desmume-" version ".tar.gz"))
       (sha256
        (base32
         "15l8wdw3q61fniy3h93d84dnm6s4pyadvh95a0j6d580rjk4pcrs"))
       (patches (search-patches "desmume-gcc6-fixes.patch"
                                "desmume-gcc7-fixes.patch"))))
    (build-system gnu-build-system)
    (arguments
     ;; Enable support for WiFi and microphone.
     `(#:configure-flags '("--enable-wifi"
                           "--enable-openal")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("zlib" ,zlib)
       ("sdl" ,sdl)
       ("glib" ,glib)
       ("gtk+" ,gtk+-2)
       ("glu" ,glu)))
    (home-page "http://desmume.org/")
    (synopsis "Nintendo DS emulator")
    (description
     "DeSmuME is an emulator for the Nintendo DS handheld gaming console.")
    (license license:gpl2)))

;; Building from recent Git because the official 5.0 release no longer builds.
(define-public dolphin-emu
  (let ((commit "a9745400ec5cea7e55d94955afbdc44d1a4982d1")
        (revision "7"))
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
             ;; Remove external stuff we don't need.
             (for-each (lambda (dir)
                         (delete-file-recursively
                           (string-append "Externals/" dir)))
                       '("LZO" "OpenAL" "Qt" "SFML" "curl" "ffmpeg"
                         "gettext" "hidapi" "libpng" "libusb" "mbedtls"
                         "miniupnpc" "MoltenVK" "zlib"))
             ;; Clean up source.
             (for-each delete-file (find-files "." ".*\\.(bin|dsy|exe|jar|rar)$"))
             #t))
         (sha256
          (base32
           "0ic08ii4vlqlmk2wkfc99jiy6nji2wfq56r7slj23wgvhznnaabk"))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'generate-fonts&hardcore-libvulkan-path
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((fontfile
                      (string-append (assoc-ref inputs "font-wqy-microhei")
                                     "/share/fonts/truetype/wqy-microhei.ttc"))
                     (libvulkan
                      (string-append (assoc-ref inputs "vulkan-loader")
                                     "/lib/libvulkan.so")))
                 (chdir "docs")
                 (invoke "bash" "-c" "g++ -O2 $(freetype-config \
--cflags --libs) gc-font-tool.cpp -o gc-font-tool")
                 (invoke "./gc-font-tool" "a" fontfile "font_western.bin")
                 (invoke "./gc-font-tool" "s" fontfile "font_japanese.bin")
                 (copy-file "font_japanese.bin" "../Data/Sys/GC/font_japanese.bin")
                 (copy-file "font_western.bin" "../Data/Sys/GC/font_western.bin")
                 (chdir "..")
                 (substitute* "Source/Core/VideoBackends/Vulkan/VulkanLoader.cpp"
                              (("\"vulkan\", 1") (string-append "\"vulkan\"")))
                 (substitute* "Source/Core/VideoBackends/Vulkan/VulkanLoader.cpp"
                              (("\"vulkan\"") (string-append "\"" libvulkan "\"")))
                 (substitute* "Source/Core/VideoBackends/Vulkan/VulkanLoader.cpp"
                              (("Common::DynamicLibrary::GetVersionedFilename") ""))
                 #t))))

         ;; The FindGTK2 cmake script only checks hardcoded directories for
         ;; glib/gtk headers.

         #:configure-flags
         (list (string-append "-DX11_INCLUDE_DIR="
                              (assoc-ref %build-inputs "libx11")
                              "/include")
               (string-append "-DX11_LIBRARIES="
                              (assoc-ref %build-inputs "libx11")
                              "/lib/libX11.so")
               "-DX11_FOUND=1")))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("gettext" ,gettext-minimal)))
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
         ("pugixml" ,pugixml)
         ("pulseaudio" ,pulseaudio)
         ("qtbase" ,qtbase)
         ("sdl2" ,sdl2)
         ("sfml" ,sfml)
         ("soil" ,soil)
         ("soundtouch" ,soundtouch)
         ("vulkan-loader" ,vulkan-loader)
         ("zlib" ,zlib)))
      (home-page "https://dolphin-emu.org/")
      (synopsis "Nintendo Wii and GameCube emulator")
      (description
       "Dolphin is an emulator for two Nintendo video game consoles: the
GameCube and the Wii.  It provides compatibility with all PC controllers,
turbo speed, networked multiplayer, and graphical enhancements.")
      (supported-systems '("x86_64-linux" "aarch64-linux"))
      ; dolphin/Data/Sys/GC/font_*.bin: Licensed under ASL2.0.
      (license (list license:gpl2+ license:asl2.0 license:fdl1.2+)))))

(define-public dosbox
  (package
    (name "dosbox")
    (version "0.74-3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sourceforge.net/projects/dosbox"
                                  "/files/dosbox/" version "/dosbox-"
                                  version ".tar.gz/download"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02i648i50dwicv1vaql15rccv4g8h5blf5g6inv67lrfxpbkvlf0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("sdl" ,sdl)
       ("libpng" ,libpng)
       ("zlib" ,zlib)
       ("alsa-lib" ,alsa-lib)
       ("glu" ,glu)
       ("mesa" ,mesa)))
    (home-page "https://www.dosbox.com")
    (synopsis "X86 emulator with CGA/EGA/VGA/etc. graphics and sound")
    (description "DOSBox is a DOS-emulator that uses the SDL library.  DOSBox
also emulates CPU:286/386 realmode/protected mode, Directory
FileSystem/XMS/EMS, Tandy/Hercules/CGA/EGA/VGA/VESA graphics, a
SoundBlaster/Gravis Ultra Sound card for excellent sound compatibility with
older games.")
    (license license:gpl2+)))

(define-public emulation-station
  ;; No release for a long time, new commits fix build issues
  (let ((commit "9cc42adff67946175d2b7e25c6ae69cc374e98a0")
        (revision "1"))
    (package
      (name "emulation-station")
      (version (git-version "2.0.1" revision commit))
      (source (origin
                (method git-fetch) ; no tarball available
                (uri (git-reference
                      (url "https://github.com/Aloshi/EmulationStation.git")
                      (commit commit))) ; no version tag
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1cva0ns650v17lfn8in095zci6lc43d23f1x3mlzc41qfqa6mbd1"))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f)) ; no tests
      (inputs
       `(("alsa-lib" ,alsa-lib)
         ("boost" ,boost)
         ("curl" ,curl)
         ("eigin" ,eigen)
         ("freeimage" ,freeimage)
         ("freetype" ,freetype)
         ("mesa" ,mesa)
         ("sdl2" ,sdl2)))
      (synopsis "Video game console emulator front-end")
      (description "EmulationStation provides a graphical front-end to a large
number of video game console emulators.  It features an interface that is
usable with any game controller that has at least 4 buttons, theming support,
and a game metadata scraper.")
      (home-page "https://emulationstation.org")
      (license license:expat))))

(define-public higan
  (package
    (name "higan")
    (version "110")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/higan-emu/higan.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11rvm53c3p2f6zk8xbyv2j51xp8zmqnch7zravhj3fk590qrjrr2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ao" ,ao)
       ("eudev" ,eudev)
       ("gtk+" ,gtk+-2)
       ("gtksourceview-2" ,gtksourceview-2)
       ("libxrandr" ,libxrandr)
       ("libxv" ,libxv)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("pulseaudio" ,pulseaudio)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (let ((build-phase (assoc-ref %standard-phases 'build))
             (install-phase (assoc-ref %standard-phases 'install)))
         (modify-phases %standard-phases
           ;; The higan build system has no configure phase.
           (delete 'configure)
           (add-before 'build 'chdir-to-higan
             (lambda _
               (chdir "higan")
               #t))
           (add-before 'install 'create-/share/applications
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; It seems the author forgot to do this in the Makefile.
                 (mkdir-p (string-append out "/share/applications"))
                 #t)))
           (add-after 'install 'chdir-to-icarus
             (lambda _
               (chdir "../icarus")
               #t))
           (add-after 'chdir-to-icarus 'build-icarus build-phase)
           (add-after 'build-icarus 'install-icarus install-phase)
           (add-after 'install-icarus 'wrap-higan-executable
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (higan (string-append bin "/higan"))
                      (higan-original (string-append higan "-original"))
                      (bash (string-append (assoc-ref inputs "bash")
                                           "/bin/bash"))
                      (coreutils (assoc-ref inputs "coreutils"))
                      (mkdir (string-append coreutils "/bin/mkdir"))
                      (cp (string-append coreutils "/bin/cp"))
                      (cp-r (string-append cp " -r --no-preserve=mode")))
                 ;; First, have the executable make sure ~/.local/share/higan
                 ;; contains up to date files.  Higan insists on looking there
                 ;; for these data files.
                 (rename-file higan higan-original)
                 (with-output-to-file higan
                   (lambda ()
                     (display
                      (string-append
                       "#!" bash "\n"
                       ;; higan doesn't respect $XDG_DATA_HOME
                       mkdir " -p ~/.local/share\n"
                       cp-r " " out "/share/higan ~/.local/share\n"
                       "exec " higan-original))))
                 (chmod higan #o555)
                 ;; Second, make sure higan will find icarus in PATH.
                 (wrap-program higan
                   `("PATH" ":" prefix (,bin)))
                 #t)))))
       #:make-flags
       (list "compiler=g++"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       ;; There is no test suite.
       #:tests? #f))
    (home-page "https://github.com/higan-emu/higan/")
    (synopsis "Nintendo multi-system emulator")
    (description
     "higan (formerly bsnes) is an emulator for multiple Nintendo video game
consoles, including the Nintendo Entertainment System (NES/Famicom), Super
Nintendo Entertainment System (SNES/Super Famicom), Game Boy, Game Boy
Color (GBC), and Game Boy Advance (GBA).  It also supports the subsystems
Super Game Boy, BS-X Satellaview, and Sufami Turbo.")
    (license license:gpl3+)))

(define-public mgba
  (package
    (name "mgba")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mgba-emu/mgba.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1if82mfaak3696w5d5yshynpzywrxgvg3ifdfi2rwlpvq1gpd429"))
       (modules '((guix build utils)))
       (snippet
        ;; Make sure we don't use the bundled software.
        '(begin
           (for-each
            (lambda (subdir)
              (let ((lib-subdir (string-append "src/third-party/" subdir)))
                (delete-file-recursively lib-subdir)))
            '("libpng" "lzma" "sqlite3" "zlib"))
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no "test" target
       #:configure-flags
       (list "-DUSE_LZMA=OFF"           ;do not use bundled LZMA
             "-DUSE_LIBZIP=OFF")))      ;use "zlib" instead
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("qttools" ,qttools)))
    (inputs `(("ffmpeg" ,ffmpeg)
              ("imagemagick" ,imagemagick)
              ("libedit" ,libedit)
              ("libelf" ,libelf)
              ("libepoxy" ,libepoxy)
              ("libpng" ,libpng)
              ("mesa" ,mesa)
              ("minizip" ,minizip)
              ("ncurses" ,ncurses)
              ("qtbase" ,qtbase)
              ("qtmultimedia" ,qtmultimedia)
              ("sdl2" ,sdl2)
              ("sqlite" ,sqlite)
              ("zlib" ,zlib)))
    (home-page "https://mgba.io")
    (synopsis "Game Boy Advance emulator")
    (description
     "mGBA is an emulator for running Game Boy Advance games.  It aims to be
faster and more accurate than many existing Game Boy Advance emulators, as
well as adding features that other emulators lack.  It also supports Game Boy
and Game Boy Color games.")
    ;; Code is mainly MPL 2.0. "blip_buf.c" is LGPL 2.1+, "inih.c" is
    ;; BSD-3, and "discord-rpc" is Expat.
    (license (list license:mpl2.0 license:lgpl2.1+ license:bsd-3 license:expat))))

(define-public sameboy
  (package
    (name "sameboy")
    (version "0.12.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LIJI32/SameBoy.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m5rv2x8qck1kr43xq186pp4kaiay7gd1x775n9qrljcd7z4x6fs"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("rgbds" ,rgbds)
       ("gcc" ,gcc-9)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("sdl2" ,sdl2)))
    (arguments
     `(#:tests? #f                      ; There are no tests
       #:make-flags `("CC=gcc" "CONF=release"
                      ,(string-append "DATA_DIR="
                                      (assoc-ref %outputs "out")
                                      "/share/sameboy/"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (data (string-append out "/share/sameboy/")))
               (with-directory-excursion "build/bin/SDL"
                 (install-file "sameboy" bin)
                 (delete-file "sameboy")
                 (copy-recursively "." data))
               #t))))))
    (home-page "https://sameboy.github.io/")
    (synopsis "Accurate Game Boy, Game Boy Color and Super Game Boy emulator")
    (description "SameBoy is a user friendly Game Boy, Game Boy Color
and Super Game Boy emulator.  SameBoy is accurate and includes a wide
range of debugging features.  It has all the features one would expect
from an emulator---from save states to scaling filters.")
    (license license:expat)))

(define-public mupen64plus-core
  (package
    (name "mupen64plus-core")
    (version "2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-core.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "116fndl6652zrp1r6ag4xv3dzp1x52mlvadj8xwflq07fd5rhri1"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("freetype" ,freetype)
       ("glu" ,glu)
       ("libpng" ,libpng)
       ("mesa" ,mesa)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'chdir-to-project-directory
          (lambda _
            (chdir "projects/unix")
            #t)))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "all" (string-append "PREFIX=" out)))
       ;; There are no tests.
       #:tests? #f))
    ;; As per the Makefile (in projects/unix/Makefile):
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Nintendo 64 emulator core library")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
core library.")
    (license license:gpl2+)))

(define-public mupen64plus-audio-sdl
  (package
    (name "mupen64plus-audio-sdl")
    (version "2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-audio-sdl.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z19amfg9vr2pqjjri1ipc7hs681fzjcnb0f9y7bjhp5n8d7p6bb"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
SDL audio plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-input-sdl
  (package
    (name "mupen64plus-input-sdl")
    (version "2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-input-sdl.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dyazfbdjycdfslq8jixqiqhziw0rlkvach2r9dz91229jmkyc9c"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)))
    (inputs
     `(("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
SDL input plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-rsp-hle
  (package
    (name "mupen64plus-rsp-hle")
    (version "2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-rsp-hle.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pi31qzjjp7aypdvvnz6ms18g09c4gqzxi6328zj8sji94b75gf0"))))
    (build-system gnu-build-system)
    (inputs
     `(("mupen64plus-core" ,mupen64plus-core)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
high-level emulation (HLE) RSP processor plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-rsp-z64
  (package
    (name "mupen64plus-rsp-z64")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-rsp-z64.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nfyjns9k8xbg3aqs7593nfaxvlj72h3l8h467442xlk8ajfcylx"))))
    (build-system gnu-build-system)
    (inputs
     `(("mupen64plus-core" ,mupen64plus-core)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Z64 RSP processor plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-arachnoid
  (package
    (name "mupen64plus-video-arachnoid")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-video-arachnoid.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v9fqwpb6pawr8z5cm2ki7bqkks4iyr5c4jy4v5khj6h8zcv55gc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("mesa" ,mesa)
       ("mupen64plus-core" ,mupen64plus-core)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice Video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Arachnoid video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-glide64
  (package
    (name "mupen64plus-video-glide64")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-video-glide64.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qn5za7g7796kh2ag3xpmhbqg0yf71g9liz6ks0rha8pz73lgs01"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("mesa" ,mesa)
       ("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix")))
         ;; XXX Should be unnecessary with the next release.
         (add-before
          'build 'use-sdl2
          (lambda _
            (substitute* "Makefile"
              (("SDL_CONFIG = (.*)sdl-config" all prefix)
               (string-append "SDL_CONFIG = " prefix "sdl2-config"))))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice Video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Glide64 video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-glide64mk2
  (package
    (name "mupen64plus-video-glide64mk2")
    (version "2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-video-glide64mk2.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08pm28a36dpr0cvr8pzw0n5ksdazp7jqvlmqfy2lwb4dm0cwhkqd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("boost" ,boost)
       ("libpng" ,libpng)
       ("mesa" ,mesa)
       ("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice Video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Glide64MK2 video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-rice
  (package
    (name "mupen64plus-video-rice")
    (version "2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-video-rice.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rpmbcq67gsj5h5jjis146378qc1mskskvx20y1ikx59yhbamh13"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("libpng" ,libpng)
       ("mesa" ,mesa)
       ("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice Video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Rice Video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-z64
  (package
    (name "mupen64plus-video-z64")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-video-z64.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04qa2fdd6dakpk2v0d4l80xh9b4h8gm71g80c0wyyxdhmhwk1r9c"))
       (patches (search-patches "mupen64plus-video-z64-glew-correct-path.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("glew" ,glew)
       ("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix")))
         ;; XXX Should be unnecessary with the next release.
         (add-before
          'build 'use-sdl2
          (lambda _
            (substitute* "Makefile"
              (("SDL_CONFIG = (.*)sdl-config" all prefix)
               (string-append "SDL_CONFIG = " prefix "sdl2-config"))))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Z64 video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Z64 video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-ui-console
  (package
    (name "mupen64plus-ui-console")
    (version "2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-ui-console.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vrf98qa6a0y3647kslsv644fag233dxh5dcr1yncjiiwickcr5a"))
       (patches (search-patches "mupen64plus-ui-console-notice.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("sdl2" ,sdl2)))
    ;; Mupen64Plus supports a single data directory and a single plugin
    ;; directory in its configuration, yet we need data and plugin files from
    ;; a variety of packages.  The best way to deal with this is to install
    ;; all packages from which data and plugin files are needed into one's
    ;; profile, and point the configuration there.  Hence, propagate the most
    ;; important packages here to save the user from the bother.  The patch
    ;; mupen64plus-ui-console-notice also gives users instructions on what
    ;; they need to do in order to point the configuration to their profile.
    (propagated-inputs
     `(("mupen64plus-core" ,mupen64plus-core)
       ("mupen64plus-audio-sdl" ,mupen64plus-audio-sdl)
       ("mupen64plus-input-sdl" ,mupen64plus-input-sdl)
       ("mupen64plus-rsp-hle" ,mupen64plus-rsp-hle)
       ("mupen64plus-video-glide64" ,mupen64plus-video-glide64)
       ("mupen64plus-video-glide64mk2" ,mupen64plus-video-glide64mk2)
       ("mupen64plus-video-rice" ,mupen64plus-video-rice)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")
               ;; Trailing slash matters here.
               (string-append "COREDIR=" m64p "/lib/")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus command line user interface")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
command line user interface.  Installing this package is the easiest way
towards a working Mupen64Plus for casual users.")
    (license license:gpl2+)))

(define-public nestopia-ue
  (package
    (name "nestopia-ue")
    (version "1.48")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rdanbrook/nestopia.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19c8vx5yxbysl0sszk5blfngwacshdgwbf44g1qaxvq8ywiyxmb4"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; We don't need libretro for the GNU/Linux build.
           (delete-file-recursively "libretro")
           #t))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("ao" ,ao)
       ("gtk+" ,gtk+)
       ("libarchive" ,libarchive)
       ("libepoxy" ,libepoxy)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; This fixes the file chooser crash that happens with GTK 3.
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (nestopia (string-append out "/bin/nestopia"))
                    (gtk (assoc-ref inputs "gtk+"))
                    (gtk-share (string-append gtk "/share")))
               (wrap-program nestopia
                 `("XDG_DATA_DIRS" ":" prefix (,gtk-share)))))))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://0ldsk00l.ca/nestopia/")
    (synopsis "Nintendo Entertainment System (NES/Famicom) emulator")
    (description
     "Nestopia UE (Undead Edition) is a fork of the Nintendo Entertainment
System (NES/Famicom) emulator Nestopia, with enhancements from members of the
emulation community.  It provides highly accurate emulation.")
    (license license:gpl2+)))

(define-public retroarch
  (package
    (name "retroarch")
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libretro/RetroArch.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y7rcpz7psf8k3agsrq277jdm651vbnn9xpqvmj2in1a786idya7"))
       (patches
        (search-patches "retroarch-disable-online-updater.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Don't suggest using the Online Updater if available: it never
           ;; is.  This disables translation of this particular message.
           (substitute* (find-files "menu/drivers" "\\.c$")
             (("msg_hash_to_str\\(MSG_MISSING_ASSETS\\)")
              "\"Warning: Missing assets, go get some\""))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (etc (string-append out "/etc"))
                    (vulkan (assoc-ref inputs "vulkan-loader"))
                    (wayland-protocols (assoc-ref inputs "wayland-protocols")))
               ;; Hard-code some store file names.
               (substitute* "gfx/common/vulkan_common.c"
                 (("libvulkan.so") (string-append vulkan "/lib/libvulkan.so")))
               (substitute* "gfx/common/wayland/generate_wayland_protos.sh"
                 (("/usr/local/share/wayland-protocols")
                 (string-append wayland-protocols "/share/wayland-protocols")))
               (substitute* "qb/qb.libs.sh"
                 (("/bin/true") (which "true")))

               ;; Use shared zlib.
               (substitute* '("libretro-common/file/archive_file_zlib.c"
                              "libretro-common/streams/trans_stream_zlib.c")
                 (("<compat/zlib.h>") "<zlib.h>"))

               ;; The configure script does not yet accept the extra arguments
               ;; (like ‘CONFIG_SHELL=’) passed by the default configure phase.
               (invoke
                 "./configure"
                 ,@(if (string-prefix? "armhf" (or (%current-target-system)
                                                  (%current-system)))
                       '("--enable-neon" "--enable-floathard")
                       '())
                 (string-append "--prefix=" out)
                 (string-append "--global-config-dir=" etc)
                 "--disable-builtinminiupnpc")))))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ffmpeg" ,ffmpeg)
       ("freetype" ,freetype)
       ("libxinerama" ,libxinerama)
       ("libxkbcommon" ,libxkbcommon)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxv" ,libxv)
       ("mesa" ,mesa)
       ("miniupnpc" ,miniupnpc)
       ("openal" ,openal)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python)
       ("qtbase" ,qtbase)
       ("sdl" ,sdl2)
       ("udev" ,eudev)
       ("vulkan-loader" ,vulkan-loader)
       ("wayland" ,wayland)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("wayland-protocols" ,wayland-protocols)
       ("which" ,which)))
    (home-page "https://www.libretro.com/")
    (synopsis "Reference frontend for the libretro API")
    (description
     "Libretro is a simple but powerful development interface that allows for
the easy creation of emulators, games and multimedia applications that can plug
straight into any libretro-compatible frontend.  RetroArch is the official
reference frontend for the libretro API, currently used by most as a modular
multi-system game/emulator system.")
    (license license:gpl3+)))

(define-public scummvm
  (package
    (name "scummvm")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.scummvm.org/frs/scummvm/" version
                           "/scummvm-" version ".tar.xz"))
       (sha256
        (base32 "1a6waf1ybp91nwva8g650cljlfb1di4l0jv13vg6yfgkas9pclsp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                                 ;require "git"
       #:configure-flags (list "--enable-release") ;for optimizations
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build
           ;; XXX: The following works around a build failure introduced when
           ;; Fluidsynth was updated to version 2.1.  It has been applied
           ;; upstream as 68758a879e0c8ecc0d40962516d4e808aa4e15e5 and can be
           ;; removed once this commit makes it into a release.
           (lambda _
             (substitute* "audio/softsynth/fluidsynth.cpp"
               (("#include <fluidsynth.h>") "")
               (("#include \"common/scummsys.h\"") "#include \"config.h\"")
               (("#include \"common/config-manager.h\"" line)
                (string-append "#include <fluidsynth.h>\n"
                               "#include \"common/scummsys.h\"\n"
                               line)))
             #t))
         (replace 'configure
           ;; configure does not work followed by both "SHELL=..." and
           ;; "CONFIG_SHELL=..."; set environment variables instead
           (lambda* (#:key outputs configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (which "bash"))
                    (flags `(,(string-append "--prefix=" out)
                             ,@configure-flags)))
               (setenv "SHELL" bash)
               (setenv "CONFIG_SHELL" bash)
               (apply invoke "./configure" flags)))))))
    (native-inputs
     `(("nasm" ,nasm)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("faad2" ,faad2)
       ("fluidsynth" ,fluidsynth)
       ("freetype" ,freetype)
       ("liba52" ,liba52)
       ("libflac" ,flac)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libmad" ,libmad)
       ("libmpeg2" ,libmpeg2)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("sdl2" ,(sdl-union (list sdl2 sdl2-net)))
       ("zlib" ,zlib)))
    (home-page "https://www.scummvm.org/")
    (synopsis "Engine for several graphical adventure games")
    (description "ScummVM is a program which allows you to run certain
classic graphical point-and-click adventure games, provided you
already have their data files.  The clever part about this: ScummVM
just replaces the executables shipped with the games, allowing you to
play them on systems for which they were never designed!")
    (license license:gpl2+)))

(define-public mame
  (package
    (name "mame")
    (version "0.220")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mamedev/mame.git")
             (commit (apply string-append "mame" (string-split version #\.)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x3yr195zi7xjr21p1c2l8c0vhg0a0af0mpz4i1w7q7r9krvcvz4"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled libraries.
        '(begin
           (with-directory-excursion "3rdparty"
             (for-each delete-file-recursively
                       '("asio" "expat" "glm" "libflac" "libjpeg" "lua"
                         "portaudio" "portmidi" "pugixml" "rapidjson" "SDL2"
                         "SDL2-override" "sqlite3" "utf8proc" "zlib")))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (cons*
        ;; A 'strict-overflow' error pops up on i686 so disable '-Werror'.
        "NOWERROR=1"
        (string-append "QT_HOME=" (assoc-ref %build-inputs "qtbase"))
        (string-append "SDL_INI_PATH="
                       (assoc-ref %outputs "out")
                       "/share/mame/ini")
        (map (lambda (lib)
               (string-append "USE_SYSTEM_LIB_" (string-upcase lib) "=1"))
             '("asio" "expat" "flac" "glm" "jpeg" "lua" "portaudio" "portmidi"
               "pugixml" "rapidjson" "sqlite3" "utf8proc" "zlib")))
       #:tests? #f                      ;no test in regular release
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'build 'build-documentation
           (lambda _ (invoke "make" "-C" "docs" "man" "info")))
         (replace 'install
           ;; Upstream does not provide an installation phase.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/mame")))
               ;; Install data.
               (for-each (lambda (dir)
                           (copy-recursively dir (string-append share "/" dir)))
                         '("artwork" "bgfx" "ctrlr" "hash" "ini" "language"
                           "plugins" "samples"))
               (let ((keymaps (string-append share "/keymaps")))
                 (for-each (lambda (file) (install-file file keymaps))
                           (find-files "keymaps" ".*LINUX\\.map")))
               (let ((fonts (string-append share "/fonts")))
                 (install-file "uismall.bdf" fonts))
               (when (file-exists? "mame64")
                 (rename-file "mame64" "mame"))
               (install-file "mame" (string-append out "/bin")))
             #t))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man/man1"))
                    (info (string-append out "/share/info")))
               (install-file "docs/build/man/MAME.1" man)
               (install-file "docs/build/texinfo/MAME.info" info))
             #t))
         (add-after 'install 'install-ini-file
           ;; Generate an ini file so as to set some directories (e.g., roms)
           ;; to a writable location, i.e., "$HOME/.mame/" and "$HOME/mame/".
           ;;
           ;; XXX: We need to insert absolute references to the store.  It can
           ;; be an issue if they leak into user's home directory, e.g., with
           ;; "mame -createconfig" and the package is later GC'ed.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/mame"))
                    (ini (string-append share "/ini")))
               (with-output-to-file (string-append ini "/mame.ini")
                 (lambda _
                   (format #t
                           "inipath              $HOME/.mame;~a/ini~@
                            homepath             $HOME/mame~@
                            rompath              $HOME/mame/roms~@
                            samplepath           $HOME/mame/samples;~a/samples~@
                            cheatpath            $HOME/mame/cheat~@
                            artpath              $HOME/mame/artwork;~a/artwork~@
                            crosshairpath        $HOME/mame/crosshair~@
                            snapshot_directory   $HOME/mame/snapshots~@
                            hashpath             ~a/hash~@
                            fontpath             $HOME/mame/fonts;~a/fonts~@
                            ctrlrpath            $HOME/mame/ctrlr;~a/ctrlr~@
                            bgfx_path            ~a/bgfx~@
                            pluginspath          $HOME/mame/plugins;~a/plugins~@
                            languagepath         ~a/language~@
                            cfg_directory        $HOME/.mame/cfg~@
                            nvram_directory      $HOME/.mame/nvram~@
                            input_directory      $HOME/.mame/inp~@
                            state_directory      $HOME/.mame/sta~@
                            diff_directory       $HOME/.mame/diff~@
                            comment_directory    $HOME/.mame/comments~%"
                           share share share share share share share share
                           share)))
               (with-output-to-file (string-append ini "/ui.ini")
                 (lambda _
                   (format #t
                           "historypath          $HOME/mame/history~@
                            categorypath         $HOME/mame/folders~@
                            cabinets_directory   $HOME/mame/cabinets~@
                            cpanels_directory    $HOME/mame/cpanel~@
                            pcbs_directory       $HOME/mame/pcb~@
                            flyers_directory     $HOME/mame/flyers~@
                            titles_directory     $HOME/mame/titles~@
                            ends_directory       $HOME/mame/ends~@
                            marquees_directory   $HOME/mame/marquees~@
                            artwork_preview_directory $HOME/mame/artpreview~@
                            bosses_directory     $HOME/mame/bosses~@
                            logos_directory      $HOME/mame/logo~@
                            scores_directory     $HOME/mame/scores~@
                            versus_directory     $HOME/mame/versus~@
                            gameover_directory   $HOME/mame/gameover~@
                            howto_directory      $HOME/mame/howto~@
                            select_directory     $HOME/mame/select~@
                            icons_directory      $HOME/mame/icons~@
                            covers_directory     $HOME/mame/covers~@
                            ui_path              $HOME/.mame/ui~%")))
               #t)))
         (add-after 'install 'install-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (desktop (string-append out "/share/applications"))
                    (executable (string-append out "/bin/mame")))
               (mkdir-p desktop)
               (with-output-to-file (string-append desktop "/mame.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                           Name=mame~@
                           Comment=Multi-purpose emulation framework~@
                           Exec=~a~@
                           TryExec=~@*~a~@
                           Terminal=false~@
                           Type=Application~@
                           Categories=Game;Emulator;~@
                           Keywords=Game;Emulator;Arcade;~%"
                           executable)))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("sphinx" ,python-sphinx)
       ("sphinxcontrib-svg2pdfconverter" ,python-sphinxcontrib-svg2pdfconverter)
       ("texinfo" ,texinfo)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("asio" ,asio)
       ("expat" ,expat)
       ("flac" ,flac)
       ("fontconfig" ,fontconfig)
       ("glm" ,glm)
       ("libjpeg" ,libjpeg-turbo)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("lua" ,lua)
       ("portaudio" ,portaudio)
       ("portmidi" ,portmidi)
       ("pugixml" ,pugixml)
       ("python-wrapper" ,python-wrapper)
       ("qtbase" ,qtbase)
       ("rapidjson" ,rapidjson)
       ("sdl" ,(sdl-union (list sdl2 sdl2-ttf)))
       ("sqlite" ,sqlite)
       ("utf8proc" ,utf8proc)
       ("zlib" ,zlib)))
    (home-page "https://www.mamedev.org")
    (synopsis "Multi-purpose emulation framework")
    (description "MAME's purpose is to preserve decades of software
history.  As electronic technology continues to rush forward, MAME
prevents this important @emph{vintage} software from being lost and
forgotten.  This is achieved by documenting the hardware and how it
functions.  The source code to MAME serves as this documentation.")
    ;; The MAME project as a whole is distributed under the terms of GPL2+.
    ;; However, over 90% of the files are under Expat license.  Also, artwork,
    ;; keymaps, languages and samples are under CC0.
    (license (list license:gpl2+ license:expat license:cc0))))

(define-public gnome-arcade
  (package
    (name "gnome-arcade")
    (version "0.218.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/strippato/gnome-arcade")
             (commit (string-append "v." version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1qc01a62p65qb6mwjfmxqsd6n3rglsfwrjhsp25nr7q54107n55l"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No tests.
       #:configure-flags (list
                          (string-append "-DMAME_BIN=\""
                                         (assoc-ref %build-inputs "mame")
                                         "/bin/mame\"")
                          (string-append "-DAPP_RES=\""
                                         (assoc-ref %outputs "out")
                                         "/share/gnome-arcade/\""))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (pk 'cwd (getcwd))
               (substitute* "../source/src/config.c"
                 (("/usr/share") (string-append out "/share"))))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (rom (string-append out "/share/gnome-arcade/data/rom"))
                    (tile (string-append out "/share/gnome-arcade/data/tile")))
               (mkdir-p bin)
               (install-file "../gnome-arcade" bin)
               (copy-recursively "../source/res"
                                 (string-append out "/share/gnome-arcade/res"))
               (mkdir-p rom)
               (install-file "../source/data/rom/ROM.TXT" rom)
               (mkdir-p tile)
               (install-file "../source/data/tile/TILE.TXT" tile))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("mame" ,mame)
       ("gtk" ,gtk+)
       ("libevdev" ,libevdev)
       ("libvlc" ,vlc)
       ("libarchive" ,libarchive)))
    (home-page "https://github.com/strippato/gnome-arcade")
    (synopsis "Minimal MAME frontend")
    (description
     "A minimal GTK+ frontend for MAME, the multi-purpose arcade and console
emulator.")
    (license license:gpl3+)))

(define-public pcsxr
  ;; No release since 2017.
  (let ((commit "6484236cb0281e8040ff6c8078c87899a3407534"))
    (package
      (name "pcsxr")
      ;; Version is tagged here: https://github.com/frealgagu/PCSX-Reloaded
      (version "1.9.95")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pcsxr/PCSX-Reloaded")
               (commit commit)))
         (sha256
          (base32
           "138mayp7zi9v4l3lm5f6xxkds619w1fgg769zm8s45c84jbz7dza"))
         (file-name (git-file-name name commit))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ;no "test" target
         #:configure-flags
         (list "-DSND_BACKEND=pulse"
               "-DENABLE_CCDDA='ON'"
               "-DUSE_LIBARCHIVE='ON'"
               "-DUSE_LIBCDIO='ON'")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'cd-subdir
             (lambda _ (chdir "pcsxr") #t))
           (add-before 'configure 'fix-cdio-lookup
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "cmake/FindCdio.cmake"
                 (("/usr/include/cdio")
                  (string-append (assoc-ref inputs "libcdio") "/include/cdio")))
               #t))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (wrap-program (string-append (assoc-ref outputs "out")
                                            "/bin/pcsxr")
                 ;; For GtkFileChooserDialog.
                 `("GSETTINGS_SCHEMA_DIR" =
                   (,(string-append (assoc-ref inputs "gtk+")
                                    "/share/glib-2.0/schemas"))))
               #t)))))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("intltool" ,intltool)
         ("glib" ,glib "bin")))
      (inputs
       `(("libcdio" ,libcdio)
         ("sdl2" ,sdl2)
         ("gtk+" ,gtk+)
         ("ffmpeg" ,ffmpeg)
         ("libxv" ,libxv)
         ("libarchive" ,libarchive)
         ("pulseaudio" ,pulseaudio)))
      (home-page "https://archive.codeplex.com/?p=pcsxr")
      (synopsis "PlayStation emulator")
      (description
       "A PlayStation emulator based on PCSX-df Project with bugfixes and
improvements.")
      (license license:gpl2+))))

(define-public gens-gs
  (package
    (name "gens-gs")
    (version "7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://retrocdn.net/images/6/6d/Gens-gs-r"
                           version ".tar.gz"))
       (sha256
        (base32
         "1ha5s6d3y7s9aq9f4zmn9p88109c3mrj36z2w68jhiw5xrxws833"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:system "i686-linux"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-CFLAGS
           (lambda* _
             ;; Remove GTK API deprecation flags that cause build errors.
             (substitute* "configure"
               (("GTK_CFLAGS=\"\\$GTK_CFLAGS .*\"") ""))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("nasm" ,nasm)))
    (inputs
     `(("sdl" ,sdl)
       ("gtk" ,gtk+-2)))
    (home-page "https://segaretro.org/Gens/GS")
    (synopsis "Emulator for Sega Genesis/Mega Drive systems")
    (description
     "Gens/GS is an emulator for the Mega Drive (also known as Sega Genesis),
derived from Gens.  Project goals include clean source code, combined features
from various forks of Gens, and improved platform portability.")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:gpl2+)))
