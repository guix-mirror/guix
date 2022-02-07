;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016, 2021 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2015, 2016 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2018 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2016 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017-2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2017, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Christopher Howard <christopher@librehacker.com>
;;; Copyright © 2021 Felipe Balbi <balbi@kernel.org>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
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
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python))

(define-public vice
  (package
    (name "vice")
    (version "3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/vice-emu/releases/"
                           "vice-" version ".tar.gz"))
       (sha256
        (base32 "1zfkl9j40v2417l1fmczdvl9yzh81jlpcy5cl2svjzb2rrffbgv5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-html-docs"
                           "--disable-pdf-docs")))
    (native-inputs
     (list bison
           dos2unix
           flex
           `(,glib "bin") ; for glib-genmarshal, etc.
           pkg-config))
    (inputs
     (list alsa-lib
           glew
           glib
           gtk+
           pulseaudio
           sdl
           sdl-image
           xa))
    (home-page "https://vice-emu.sourceforge.io/")
    (synopsis "The versatile Commodore emulator")
    (description
     "VICE is a program that emulates the C64, the C64DTV, the C128, the
VIC20, practically all PET models, the PLUS4 and the CBM-II (aka
C610/C510).  An extra emulator is provided for C64 expanded with the CMD
SuperCPU.")
    (license license:gpl2+)))

(define-public blastem
  (package
    (name "blastem")
    (version "0.6.2")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                    (url "https://www.retrodev.com/repos/blastem")
                    (changeset (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "08ycfisivh9rb9vmijlrpdryaw8spd81ck48960p15cnf8h2535q"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; TODO: Separately package and unbundle nuklear
                  (delete-file-recursively "zlib")))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target))
                          "HOST_ZLIB=1"
                          "HAS_PROC=-DHAS_PROC"
                          (string-append "CONFIG_PATH="
                                         %output "/share/blastem")
                          (string-append "DATA_PATH="
                                         %output "/share/blastem"))
       #:tests? #f ; No check target and custom tests don't seem to build
       #:imported-modules
       ((guix build copy-build-system)
        ,@%gnu-build-system-modules)
       #:modules
       (((guix build copy-build-system)
         #:prefix copy:)
        (guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-source
           (lambda _
             (substitute* (find-files "." ".*\\.[ch]")
               (("\"zlib/zlib.h\"") "<zlib.h>"))
             (substitute* "Makefile"
               (("CFLAGS:=-std=gnu99" all)
                (string-append all " -fcommon")))))
         (delete 'configure)
         (replace 'install
           (lambda* args
             (apply (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    '(("." "bin" #:include ("blastem" "vgmplay"))
                      ("." "share/blastem"
                       #:include ("default.cfg" "rom.db")
                       #:exclude ("android"))
                      ("shaders" "share/blastem/shaders"))
                    args))))))
    (inputs
     (list glew mesa sdl2 zlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.retrodev.com/blastem/")
    (synopsis "Genesis/Mega Drive emulator")
    (description "Blastem is an emulator for the Sega Genesis/Mega Drive
console.")
    (license license:gpl3+)))

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
     (list pkg-config intltool))
    (inputs
     (list zlib sdl glib gtk+-2 glu))
    (home-page "http://desmume.org/")
    (synopsis "Nintendo DS emulator")
    (description
     "DeSmuME is an emulator for the Nintendo DS handheld gaming console.")
    (license license:gpl2)))

;; Building from recent Git because the official 5.0 release no longer builds.
;; Following commits and revision numbers of beta versions listed at
;; https://dolphin-emu.org/download/.
(define-public dolphin-emu
  (let ((commit "a34823df61df65168aa40ef5e82e44defd4a0138")
        (revision "13178"))
    (package
      (name "dolphin-emu")
      (version (git-version "5.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolphin-emu/dolphin")
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
           "0j6hnj60iai366kl0kdbn1jkwc183l02g65mp2vq4qb2yd4399l1"))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'generate-fonts&hardcore-libvulkan-path
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((fontfile
                      (search-input-file inputs
                                         "/share/fonts/truetype/wqy-microhei.ttc"))
                     (libvulkan
                      (search-input-file inputs "/lib/libvulkan.so")))
                 (chdir "docs")
                 (invoke "bash" "-c" "g++ -O2 $(freetype-config \
--cflags --libs) gc-font-tool.cpp -o gc-font-tool")
                 (invoke "./gc-font-tool" "a" fontfile "font_western.bin")
                 (invoke "./gc-font-tool" "s" fontfile "font_japanese.bin")
                 (copy-file "font_japanese.bin" "../Data/Sys/GC/font_japanese.bin")
                 (copy-file "font_western.bin" "../Data/Sys/GC/font_western.bin")
                 (chdir "..")
                 (substitute* "Source/Core/VideoBackends/Vulkan/VulkanLoader.cpp"
                   (("\"vulkan\", 1") (string-append "\"vulkan\""))
                   (("\"vulkan\"") (string-append "\"" libvulkan "\""))
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
       (list alsa-lib
             ao
             bluez
             curl
             eudev
             ffmpeg
             font-wqy-microhei
             freetype
             glew
             glib
             glu
             gtk+-2
             hidapi
             libevdev
             libpng
             libusb
             libx11
             libxi
             libxrandr
             lzo
             mbedtls-apache
             mesa
             miniupnpc
             openal
             pugixml
             pulseaudio
             qtbase-5
             sdl2
             sfml
             soil
             soundtouch
             vulkan-loader
             zlib))
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
     (list autoconf automake))
    (inputs
     (list sdl
           libpng
           zlib
           alsa-lib
           glu
           mesa))
    (home-page "https://www.dosbox.com")
    (synopsis "X86 emulator with CGA/EGA/VGA/etc. graphics and sound")
    (description "DOSBox is a DOS-emulator that uses the SDL library.  DOSBox
also emulates CPU:286/386 realmode/protected mode, Directory
FileSystem/XMS/EMS, Tandy/Hercules/CGA/EGA/VGA/VESA graphics, a
SoundBlaster/Gravis Ultra Sound card for excellent sound compatibility with
older games.")
    (license license:gpl2+)))

(define-public dosbox-staging
  ;; This is not a patch staging area for DOSBox, but an unaffiliated fork.
  (package
    (name "dosbox-staging")
    (version "0.78.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dosbox-staging/dosbox-staging")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16byip1j9ckq0ik7ilrj0fc9dal3495s48xd21drpbb8q9jwb342"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~(list
              ;; These both try to git clone subprojects.
              "-Dunit_tests=disabled"   ; gtest
              "-Duse_mt32emu=false"
              ;; Not packaged.
              "-Duse_slirp=false")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-includes
                 (lambda _
                   (substitute* (find-files "." "\\.(cpp|h)")
                     (("^(#include <)(SDL[_.])" _ include file)
                      (string-append include "SDL2/" file))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fluidsynth" ,fluidsynth)
       ("libpng" ,libpng)
       ("mesa" ,mesa)
       ("opusfile" ,opusfile)
       ("sdl2" ,(sdl-union (list sdl2 sdl2-net)))
       ("zlib" ,zlib)))
    (home-page "https://dosbox-staging.github.io")
    (synopsis "DOS/x86 PC emulator focusing on ease of use")
    (description
     "The DOSBox Staging project attempts to modernize DOSBox.

DOSBox emulates an Intel x86 personal computer running an IBM PC compatible disk
operating system (@dfn{DOS}) in both real and protected modes.  It was primarily
designed to run old DOS games, but aims to be fully compatible with all DOS
programs and replicate the experience as accurately as possible.

This fork fixes some perceived issues with DOSBox and adds new features such as
Wayland support, PowerPC/POWER dynamic recompilation, and FluidSynth MIDI.
Other features may be removed: for example, physical CDs can no longer be
played, only emulated media.

Graphical emulation includes contemporary text mode, Hercules, CGA, EGA, VGA,
VESA, S3@tie{}Trio@tie{}64, and Tandy hardware.

Emulated legacy sound devices range from a rudimentary `PC speaker' buzzer to
the once state-of-the-art Gravis Utrasound sampling sound card.  The default is
a SoundBlaster 16 providing 16-bit stereo sound.  MIDI is forwarded to the host
through an emulated MPU-401.

An emulated hardware modem is also included, letting one host or dial a
@acronym{BBS, Bulletin Board System} across the Internet, network over IPX, and
emulate a serial nullmodem over TCP/IP.")
    (license license:gpl3+)))

(define-public qtmips
  (package
    (name "qtmips")
    (version "0.7.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cvut/QtMips")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fal7a8y5g0rqqjrk795jh1l50ihz01ppjnrfjrk9vkjbd59szbp"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake"
                     (string-append "PREFIX=" (assoc-ref outputs "out"))
                     "qtmips.pro")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (substitute* "tests/test.sh"
               (("qtchooser.*") ""))
             (substitute* '("tests/cpu_trap/test.sh"
                            "tests/registers/test.sh")
               (("sub-qtmips_cli") "qtmips_cli"))
             (if tests?
               (invoke "tests/run-all.sh")
               #t)))
         (replace 'install
           ;; There is no install target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (apps (string-append out "/share/applications"))
                    (icons (string-append out "/share/icons/hicolor")))
               (install-file "qtmips_gui/qtmips_gui" bin)
               (install-file "qtmips_cli/qtmips_cli" bin)
               (install-file "data/qtmips.desktop" apps)
               (install-file "data/icons/qtmips_gui.svg"
                             (string-append icons "/scalable/apps"))
               (install-file "data/icons/qtmips_gui.png"
                             (string-append icons "/48x48/apps"))
               #t))))
       #:tests? #f))    ; test suite wants mips toolchain
    (inputs
     (list elfutils qtbase-5))
    (home-page "https://github.com/cvut/QtMips")
    (synopsis "MIPS CPU emulator")
    (description "This package contains a MIPS CPU emulator.  The simulator
accepts ELF statically linked executables compiled for 32-bit big-endian
MIPS target, targeting mips-linux-gnu or mips-elf.")
    (license license:gpl2+)))   ; License file says GPL3

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
                      (url "https://github.com/Aloshi/EmulationStation")
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
             (url "https://github.com/higan-emu/higan")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11rvm53c3p2f6zk8xbyv2j51xp8zmqnch7zravhj3fk590qrjrr2"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
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
                      (bash (search-input-file inputs "/bin/bash"))
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
    (synopsis "Multi-system emulator")
    (description
     "higan is a multi-system emulator with an uncompromising focus on
accuracy and code readability.

It currently emulates the following systems: Famicom, Famicom Disk System,
Super Famicom, Super Game Boy, Game Boy, Game Boy Color, Game Boy Advance,
Game Boy Player, SG-1000, SC-3000, Master System, Game Gear, Mega Drive, Mega
CD, PC Engine, SuperGrafx, MSX, MSX2, ColecoVision, Neo Geo Pocket, Neo Geo
Pocket Color, WonderSwan, WonderSwan Color, SwanCrystal, Pocket Challenge
V2.")
    (license license:gpl3+)))

(define-public mednafen
  (package
    (name "mednafen")
    (version "1.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mednafen.github.io/releases/files/"
                           "mednafen-" version ".tar.xz"))
       (sha256
        (base32 "0binkxnki8w8jgiyf3xr9503m5cynlf5vylb087vxybp53qbqgys"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        ;; "--with-external-mpcdec"
        "--with-external-lzo")))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("alsa" ,alsa-lib)
       ("flac" ,flac)
       ;; ("libmpcdec" ,libmpcdec) FIXME: not recognized.
       ("libsndfile" ,libsndfile)
       ("lzo" ,lzo)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (home-page "https://mednafen.github.io/")
    (synopsis "Multi-system emulator utilizing OpenGL and SDL")
    (description
     "Mednafen is a portable, utilizing OpenGL and SDL, argument-driven
multi-system emulator.  Mednafen has the ability to remap hotkey functions and
virtual system inputs to a keyboard, a joystick, or both simultaneously.  Save
states are supported, as is real-time game rewinding.  Screen snapshots may be
taken, in the PNG file format, at the press of a button.  Mednafen can record
audiovisual movies in the QuickTime file format, with several different
lossless codecs supported.

The following systems are supported:

@itemize
@item Apple II/II+
@item Atari Lynx
@item Neo Geo Pocket (Color)
@item WonderSwan
@item GameBoy (Color)
@item GameBoy Advance
@item Nintendo Entertainment System
@item Super Nintendo Entertainment System/Super Famicom
@item Virtual Boy
@item PC Engine/TurboGrafx 16 (CD)
@item SuperGrafx
@item PC-FX
@item Sega Game Gear
@item Sega Genesis/Megadrive
@item Sega Master System
@item Sega Saturn (experimental, x86_64 only)
@item Sony PlayStation
@end itemize")
    ;; Main license is GPL2+.  Some parts are BSD-3.
    (license (list license:gpl2+ license:bsd-3))))

(define-public mgba
  (package
    (name "mgba")
    (version "0.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mgba-emu/mgba")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bg4ax5gjkr6d4cpzsgzv3bpa3i2c2b1ckwrjklqiy835b5ni6yi"))
       (modules '((guix build utils)))
       (snippet
        ;; Make sure we don't use the bundled software.
        '(begin
           (for-each
            (lambda (subdir)
              (let ((lib-subdir (string-append "src/third-party/" subdir)))
                (delete-file-recursively lib-subdir)))
            '("libpng" "lzma" "sqlite3" "zlib"))))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no "test" target
       #:configure-flags
       (list "-DUSE_LZMA=OFF"           ;do not use bundled LZMA
             "-DUSE_LIBZIP=OFF")))      ;use "zlib" instead
    (native-inputs
     (list pkg-config qttools))
    (inputs
     (list ffmpeg
           libedit
           libelf
           libepoxy
           libpng
           mesa
           minizip
           ncurses
           qtbase-5
           qtmultimedia
           sdl2
           sqlite
           zlib))
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
    (version "0.14.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LIJI32/SameBoy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r391jdh0gjnx8bwmr63dcdmq58lpm162mng2ncrx53ydbb13xxf"))))
    (build-system gnu-build-system)
    (native-inputs
     (list rgbds pkg-config))
    (inputs
     (list sdl2))
    (arguments
     `(#:tests? #f                      ; There are no tests
       #:make-flags `(,(string-append "CC=" ,(cc-for-target))
                      "NATIVE_CC=gcc" "CONF=release"
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
                 (copy-recursively "." data))))))))
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
             (url "https://github.com/mupen64plus/mupen64plus-core")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "116fndl6652zrp1r6ag4xv3dzp1x52mlvadj8xwflq07fd5rhri1"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list freetype
           glu
           libpng
           mesa
           sdl2
           zlib))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (replace 'configure
           (lambda _
             (substitute* "projects/unix/Makefile"
               (("\\$\\(CFLAGS\\)")
                "$(CFLAGS) -fcommon"))))
         ;; Makefile is in a subdirectory.
         (add-before 'build 'chdir-to-project-directory
           (lambda _ (chdir "projects/unix"))))
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
             (url "https://github.com/mupen64plus/mupen64plus-audio-sdl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z19amfg9vr2pqjjri1ipc7hs681fzjcnb0f9y7bjhp5n8d7p6bb"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list mupen64plus-core sdl2))
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
             (url "https://github.com/mupen64plus/mupen64plus-input-sdl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dyazfbdjycdfslq8jixqiqhziw0rlkvach2r9dz91229jmkyc9c"))))
    (build-system gnu-build-system)
    (native-inputs
     (list which))
    (inputs
     (list mupen64plus-core sdl2))
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
             (url "https://github.com/mupen64plus/mupen64plus-rsp-hle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pi31qzjjp7aypdvvnz6ms18g09c4gqzxi6328zj8sji94b75gf0"))))
    (build-system gnu-build-system)
    (inputs
     (list mupen64plus-core))
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
             (url "https://github.com/mupen64plus/mupen64plus-rsp-z64")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nfyjns9k8xbg3aqs7593nfaxvlj72h3l8h467442xlk8ajfcylx"))))
    (build-system gnu-build-system)
    (inputs
     (list mupen64plus-core))
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
             (url "https://github.com/mupen64plus/mupen64plus-video-arachnoid")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v9fqwpb6pawr8z5cm2ki7bqkks4iyr5c4jy4v5khj6h8zcv55gc"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list mesa mupen64plus-core))
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
             (url "https://github.com/mupen64plus/mupen64plus-video-glide64")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qn5za7g7796kh2ag3xpmhbqg0yf71g9liz6ks0rha8pz73lgs01"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list mesa mupen64plus-core sdl2))
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
             (url "https://github.com/mupen64plus/mupen64plus-video-glide64mk2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08pm28a36dpr0cvr8pzw0n5ksdazp7jqvlmqfy2lwb4dm0cwhkqd"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list boost
           libpng
           mesa
           mupen64plus-core
           sdl2
           zlib))
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
             (url "https://github.com/mupen64plus/mupen64plus-video-rice")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rpmbcq67gsj5h5jjis146378qc1mskskvx20y1ikx59yhbamh13"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list libpng mesa mupen64plus-core sdl2))
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
             (url "https://github.com/mupen64plus/mupen64plus-video-z64")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04qa2fdd6dakpk2v0d4l80xh9b4h8gm71g80c0wyyxdhmhwk1r9c"))
       (patches (search-patches "mupen64plus-video-z64-glew-correct-path.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list glew mupen64plus-core sdl2))
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
             (url "https://github.com/mupen64plus/mupen64plus-ui-console")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vrf98qa6a0y3647kslsv644fag233dxh5dcr1yncjiiwickcr5a"))
       (patches (search-patches "mupen64plus-ui-console-notice.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list sdl2))
    ;; Mupen64Plus supports a single data directory and a single plugin
    ;; directory in its configuration, yet we need data and plugin files from
    ;; a variety of packages.  The best way to deal with this is to install
    ;; all packages from which data and plugin files are needed into one's
    ;; profile, and point the configuration there.  Hence, propagate the most
    ;; important packages here to save the user from the bother.  The patch
    ;; mupen64plus-ui-console-notice also gives users instructions on what
    ;; they need to do in order to point the configuration to their profile.
    (propagated-inputs
     (list mupen64plus-core
           mupen64plus-audio-sdl
           mupen64plus-input-sdl
           mupen64plus-rsp-hle
           mupen64plus-video-glide64
           mupen64plus-video-glide64mk2
           mupen64plus-video-rice))
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
    (version "1.51.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rdanbrook/nestopia")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g19gz33jav00rwzkpcnynf5ps41vl64a9qx0xjd6lva4bgn8s57"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf autoconf-archive automake pkg-config))
    (inputs
     `(("fltk" ,fltk)
       ("fontconfig", fontconfig)
       ("libarchive" ,libarchive)
       ("libepoxy" ,libepoxy)
       ("libxft" ,libxft)
       ("libxrender" ,libxrender)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (arguments
     '(;; There are no tests.
       #:tests? #f))
    (home-page "http://0ldsk00l.ca/nestopia/")
    (synopsis "Nintendo Entertainment System (NES/Famicom) emulator")
    (description
     "Nestopia UE (Undead Edition) is a fork of the Nintendo Entertainment
System (NES/Famicom) emulator Nestopia, with enhancements from members of the
emulation community.  It provides highly accurate emulation.")
    (license license:gpl2+)))

(define-public libretro-lowresnx
  (package
    (name "libretro-lowresnx")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/timoinutilis/lowres-nx")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b0vg3iz342dpkffvf7frsnqh8inj8yzi8550bsx8vnbpq5r2ay5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                    ; no tests
       #:make-flags (list "-C" "platform/LibRetro"
                          (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)          ; no configure script
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libretrodir (string-append out "/lib/libretro")))
               (install-file "platform/LibRetro/lowresnx_libretro.so"
                             libretrodir)
               #t))))))
    (home-page "https://lowresnx.inutilis.com/")
    (synopsis "Libretro core for LowRES NX")
    (description "LowRES NX is a simulated retro game console, which can be
programmed in the classic BASIC language.  This package provides a libretro
core allowing the lowRES NX programs to be used with libretro frontends such
as RetroArch.")
    (license license:zlib)))

(define-public retroarch
  (package
    (name "retroarch")
    (version "1.9.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libretro/RetroArch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hd77kw1f655s40qcz1righdhd9czqyy40rf7gigdag1bkchdx6z"))
       (patches
        (search-patches "retroarch-LIBRETRO_DIRECTORY.patch"))))
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
                 ;; Non-free software are available through the core updater,
                 ;; disable it.  See <https://issues.guix.gnu.org/38360>.
                 "--disable-update_cores"
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
       ("qtbase" ,qtbase-5)
       ("sdl" ,sdl2)
       ("udev" ,eudev)
       ("vulkan-loader" ,vulkan-loader)
       ("wayland" ,wayland)
       ("zlib" ,zlib)))
    (native-inputs
     (list pkg-config wayland-protocols which))
    (native-search-paths
     (list (search-path-specification
            (variable "LIBRETRO_DIRECTORY")
            (separator #f)              ; single entry
            (files '("lib/libretro")))))
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
    (version "2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.scummvm.org/frs/scummvm/" version
                           "/scummvm-" version ".tar.xz"))
       (sha256
        (base32 "00az0dm85kh8rq0yqj74x0c5zrq4ybvwvnilijzl8qa5whwdpn4z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                                 ;require "git"
       #:configure-flags (list "--enable-release") ;for optimizations
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; configure does not work followed by both "SHELL=..." and
           ;; "CONFIG_SHELL=..."; set environment variables instead
           (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (search-input-file inputs "/bin/bash"))
                    (flags `(,(string-append "--prefix=" out)
                             ,@configure-flags)))
               (setenv "SHELL" bash)
               (setenv "CONFIG_SHELL" bash)
               (apply invoke "./configure" flags)))))))
    (native-inputs
     (list nasm pkg-config))
    (inputs
     (list alsa-lib
           faad2
           fluidsynth
           freetype
           fribidi
           glew
           giflib
           liba52
           flac
           libjpeg-turbo
           libmad
           libmpeg2
           libogg
           libpng
           libtheora
           libvorbis
           (sdl-union (list sdl2 sdl2-net))
           zlib))
    (home-page "https://www.scummvm.org/")
    (synopsis "Engine for several graphical adventure games")
    (description "ScummVM is a program which allows you to run certain
classic graphical point-and-click adventure games, provided you
already have their data files.  The clever part about this: ScummVM
just replaces the executables shipped with the games, allowing you to
play them on systems for which they were never designed!")
    (license license:gpl2+)))

(define-public libticables2
  (package
    (name "libticables2")
    (version "1.3.5")
    (source (origin
              (method url-fetch)
              (uri "https://www.ticalc.org/pub/unix/tilibs.tar.gz")
              (sha256
               (base32
                "07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-libusb10")
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "tar" "xvkf" source)
             (invoke "tar" "xvkf"
                     (string-append "tilibs2/libticables2-"
                                    ,version ".tar.bz2"))
             (chdir (string-append "libticables2-" ,version))
             #t)))))
    (native-inputs
     (list autoconf
           autogen
           automake
           gnu-gettext
           libtool
           pkg-config))
    (inputs
     (list glib libusb))
    (synopsis "Link cable library for TI calculators")
    (description
     "This package contains libticables, a library for operations on
@acronym{TI, Texas Instruments} calculator link cables.

This is a part of the TiLP project.")
    (home-page "http://lpg.ticalc.org/prj_tilp/")
    (license license:gpl2+)))

(define-public libticonv
  (package
    (name "libticonv")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri "https://www.ticalc.org/pub/unix/tilibs.tar.gz")
              (sha256
               (base32
                "07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg"))))
    (build-system gnu-build-system)
    (arguments
     ;; build fails with out --enable-iconv (...?)
     `(#:configure-flags (list "--enable-iconv")
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "tar" "xvkf" source)
             (invoke "tar" "xvkf"
                     (string-append "tilibs2/libticonv-"
                                    ,version ".tar.bz2"))
             (chdir (string-append "libticonv-" ,version))
             #t)))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list glib))
    (synopsis "Character conversion library for TI calculators")
    (description
     "This package contains libticonv, a library to support working with
@acronym{TI, Texas Instruments} calculator charsets.

This is a part of the TiLP project.")
    (home-page "http://lpg.ticalc.org/prj_tilp/")
    (license license:gpl2+)))

(define-public libtifiles2
  (package
    (name "libtifiles2")
    (version "1.1.7")
    (source (origin
              (method url-fetch)
              (uri "https://www.ticalc.org/pub/unix/tilibs.tar.gz")
              (sha256
               (base32
                "07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "tar" "xvkf" source)
             (invoke "tar" "xvkf"
                     (string-append "tilibs2/libtifiles2-"
                                    ,version ".tar.bz2"))
             (chdir (string-append "libtifiles2-" ,version))
             #t)))))
    (native-inputs
     (list autoconf automake gnu-gettext libtool pkg-config))
    (inputs
     (list glib libarchive libticonv))
    (synopsis "File functions library for TI calculators")
    (description
     "This package contains libticonv, a library to support working with
@acronym{TI, Texas Instruments} calculator files.

This is a part of the TiLP project.")
    (home-page "http://lpg.ticalc.org/prj_tilp/")
    (license license:gpl2+)))

(define-public libticalcs2
  (package
    (name "libticalcs2")
    (version "1.1.9")
    (source (origin
              (method url-fetch)
              (uri "https://www.ticalc.org/pub/unix/tilibs.tar.gz")
              (sha256
               (base32
                "07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "tar" "xvkf" source)
             (invoke "tar" "xvkf"
                     (string-append "tilibs2/libticalcs2-"
                                    ,version ".tar.bz2"))
             (chdir (string-append "libticalcs2-" ,version))
             #t)))))
    (native-inputs
     (list autoconf automake gnu-gettext libtool pkg-config))
    (inputs
     (list glib libarchive libticables2 libticonv libtifiles2))
    (synopsis "Support library for TI calculators")
    (description
     "This project aims to develop a multi-platform linking program for use
with all @acronym{TI, Texas Instruments} graphing calculators (TI73 to
V200PLT).

This is a part of the TiLP project.")
    (home-page "http://lpg.ticalc.org/prj_tilp/")
    (license license:gpl2+)))

(define-public mame
  (package
    (name "mame")
    (version "0.240")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mamedev/mame")
             (commit (apply string-append "mame" (string-split version #\.)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "141mj5kzafnbw6nqlc3ariwxqn8lq3s13dzypax6igpy4wfy75rm"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled libraries.
        '(begin
           (with-directory-excursion "3rdparty"
             (for-each delete-file-recursively
                       '("asio" "expat" "glm" "libflac" "libjpeg" "lua"
                         "portaudio" "portmidi" "pugixml" "rapidjson" "SDL2"
                         "SDL2-override" "sqlite3" "utf8proc" "zlib")))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(cons*
         ;; A 'strict-overflow' error pops up on i686 so disable '-Werror'.
         "NOWERROR=1"
         (string-append "QT_HOME=" #$(this-package-input "qtbase"))
         (string-append "SDL_INI_PATH=" #$output "/share/mame/ini")
         (map (lambda (lib)
                (string-append "USE_SYSTEM_LIB_" (string-upcase lib) "=1"))
              '("asio" "expat" "flac" "glm" "jpeg" "lua" "portaudio" "portmidi"
                "pugixml" "rapidjson" "sqlite3" "utf8proc" "zlib")))
      #:tests? #f                       ;no test in regular release
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'build 'build-documentation
            (lambda _ (invoke "make" "-C" "docs" "man" "info")))
          (replace 'install
            ;; Upstream does not provide an installation phase.
            (lambda _
              (let ((share (string-append #$output "/share/mame")))
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
                (install-file "mame" (string-append #$output "/bin")))))
          (add-after 'install 'install-documentation
            (lambda _
              (let ((man (string-append #$output "/share/man/man1"))
                    (info (string-append #$output "/share/info")))
                (install-file "docs/build/man/MAME.1" man)
                (install-file "docs/build/texinfo/MAME.info" info))))
          (add-after 'install 'install-ini-file
            ;; Generate an ini file so as to set some directories (e.g., roms)
            ;; to a writable location, i.e., "$HOME/.mame/" and "$HOME/mame/".
            ;;
            ;; XXX: We need to insert absolute references to the store.  It can
            ;; be an issue if they leak into user's home directory, e.g., with
            ;; "mame -createconfig" and the package is later GC'ed.
            (lambda _
              (let* ((share (string-append #$output "/share/mame"))
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
                            ui_path              $HOME/.mame/ui~%"))))))
          (add-after 'install 'install-desktop-file
            (lambda _
              (let ((desktop (string-append #$output "/share/applications"))
                    (executable (string-append #$output "/bin/mame")))
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
                            executable)))))))))
    (native-inputs
     (list pkg-config
           python-sphinx
           python-sphinxcontrib-svg2pdfconverter
           texinfo))
    (inputs
     (list alsa-lib
           asio
           expat
           flac
           fontconfig
           glm
           libjpeg-turbo
           libxi
           libxinerama
           lua
           portaudio
           portmidi
           pugixml
           pulseaudio
           python-wrapper
           qtbase-5
           rapidjson
           (sdl-union (list sdl2 sdl2-ttf))
           sqlite
           utf8proc
           zlib))
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
     (list
      #:tests? #f                       ; No tests.
      #:configure-flags
      #~(list
         (string-append "-DMAME_BIN=\""
                        #$(this-package-input "mame")
                        "/bin/mame\"")
         (string-append "-DAPP_RES=\"" #$output "/share/gnome-arcade/\""))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fix-paths
            (lambda _
              (substitute* "../source/src/config.c"
                (("/usr/share") (string-append #$output "/share")))))
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (rom (string-append #$output
                                        "/share/gnome-arcade/data/rom"))
                    (tile (string-append #$output
                                         "/share/gnome-arcade/data/tile")))
                (mkdir-p bin)
                (install-file "../gnome-arcade" bin)
                (copy-recursively "../source/res"
                                  (string-append #$output
                                                 "/share/gnome-arcade/res"))
                (mkdir-p rom)
                (install-file "../source/data/rom/ROM.TXT" rom)
                (mkdir-p tile)
                (install-file "../source/data/tile/TILE.TXT" tile)))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list gtk+ libarchive libevdev mame vlc))
    (home-page "https://github.com/strippato/gnome-arcade")
    (synopsis "Minimal MAME frontend")
    (description
     "Gnome Arcade is a minimal GTK+ frontend for MAME, the multi-purpose
arcade and console emulator.")
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
                  (search-input-directory inputs "/include/cdio")))))
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
       (list pkg-config intltool
             `(,glib "bin")))
      (inputs
       (list libcdio
             sdl2
             gtk+
             ffmpeg
             libxv
             libarchive
             pulseaudio))
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
     (list pkg-config nasm))
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

(define-public bsnes
  (package
    (name "bsnes")
    (version "115")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bsnes-emu/bsnes")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j054x38fwai61vj36sc04r3zkzay5acq2cgd9zqv5hs51s36g5b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "-C" "bsnes"
                          ;; Remove march=native
                          "local=false"
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; No tests.
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ao" ,ao)
       ("cairo" ,cairo)
       ("eudev" ,eudev)
       ("gtksourceview-2" ,gtksourceview-2)
       ("libxrandr" ,libxrandr)
       ("libxv" ,libxv)
       ("openal" ,openal)
       ("pulseaudio" ,pulseaudio)
       ("sdl2" ,sdl2)))
    (home-page "https://bsnes.dev/")
    (synopsis "Emulator for the Super Nintendo / Super Famicom systems")
    (description
     "bsnes is a Super Nintendo / Super Famicom emulator that focuses on
performance, features, and ease of use.")
    (license license:gpl3)))

;; python-pwntools requires a -rc release of unicorn
(define-public unicorn
  (let ((unless-x86
          (lambda (code)
            (if (member (%current-system) '("x86_64-linux" "i686-linux"))
              '()
              code))))
    (package
      (name "unicorn")
      (version "1.0.2-rc4")
      ;; NOTE: unicorn ships a bundled QEMU, but with a lot of custom modifications.
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/unicorn-engine/unicorn")
               (commit version)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "17nyccgk7hpc4hab24yn57f1xnmr7kq4px98zbp2bkwcrxny8gwy"))))
      (outputs '("out" "python"))
      ;; The main library is not written in Python, but the build process has
      ;; little in common with any defined build system, so we might as well
      ;; build on top of python-build-system and make use of all
      ;; the Python-specific phases that can be reused.
      (build-system python-build-system)
      (arguments
       `(#:modules ((srfi srfi-26)
                    (guix build python-build-system)
                    (guix build utils))
         #:phases
         (modify-phases %standard-phases
           (add-before 'build 'build-library
             (lambda* (#:key inputs #:allow-other-keys)
               (invoke "make"
                       "-j" (number->string (parallel-job-count))
                       "UNICORN_STATIC=no"
                       "CC=gcc")))
           (add-after 'build-library 'install-library
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke "make" "install"
                       "UNICORN_STATIC=no"
                       (string-append
                        "PREFIX="
                        (assoc-ref outputs "out")))))
           (add-before 'build 'prepare-bindings
             (lambda* (#:key outputs #:allow-other-keys)
               (chdir "bindings/python")
               ;; Set this environment variable so that the Python bindings
               ;; don't build their own copy of the shared object, but use
               ;; a dummy value such that the bindings test suite uses the
               ;; same mechanism for loading the library as any other user.
               (setenv "LIBUNICORN_PATH" "1")
               (substitute* "unicorn/unicorn.py"
                 (("_path_list = \\[.*")
                  (string-append
                   "_path_list = [\""
                   (assoc-ref outputs "out")
                   ;; eat the rest of the list
                   "/lib\"] + 0*[")))
               #t))
           (add-before 'check 'check-library
             (lambda* (#:key outputs #:allow-other-keys)
               (for-each
                 (lambda (suite)
                   (with-directory-excursion
                     (string-append "../../tests/" suite)
                     (invoke "make" "test" "CC=gcc"
                             ,@(unless-x86
                                '("AS=i686-unknown-linux-gnu-as"
                                  "OBJCOPY=i686-unknown-linux-gnu-objcopy")))))
                 '("unit" "regress"))
               #t))
           (add-after 'install 'install-samples
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((python-samples (find-files "." "sample_.*"))
                      (c-samples (find-files "../../samples" ".*\\.c"))
                      (python-docdir
                        (string-append (assoc-ref outputs "python")
                                       "/share/doc/unicorn/samples"))
                      (c-docdir
                        (string-append (assoc-ref outputs "out")
                                       "/share/doc/unicorn/samples")))
                 (for-each (cut install-file <> c-docdir) c-samples)
                 (for-each (cut install-file <> python-docdir) python-samples)
                 #t))))))
      (native-inputs
       ;; NOTE: cross-binutils needs to be wrapped with unless-x86, as otherwise
       ;; the linker provided by the package will be used, circumventing the ld-wrapper.
       `(,@(unless-x86
            `(("assembler-for-tests" ,(cross-binutils "i686-unknown-linux-gnu"))))
         ("cmocka" ,cmocka)
         ("hexdump-for-tests" ,util-linux)))
      (home-page "https://www.unicorn-engine.org")
      (synopsis "Unicorn CPU emulator framework")
      (description
       "Unicorn is a lightweight, multi-platform, multi-architecture CPU emulator
framework based on QEMU.")
      (license license:gpl2+))))

(define-public ppsspp
  (package
    (name "ppsspp")
    (version "1.12.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hrydgard/ppsspp")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1p6pmp0lhqhk9h5r9xsjicd0zn08bwx3y8533npps96ixwbm2y15"))
       (file-name (git-file-name name version))
       (patches
        (search-patches "ppsspp-disable-upgrade-and-gold.patch"))
       (modules '((guix build utils)))
       (snippet
        `(begin
           ;; The following is quite a heavy-handed way of unbundling PPSSPP.
           ;; There are still a number of external sources, that we don't
           ;; remove here.  Some may be packaged, others are not.
           ;; First, we patch existing sources to include the right headers.
           (substitute* (append (find-files "Common" ".*\\.(h|cpp)")
                                (find-files "Core" ".*\\.(h|cpp)")
                                (find-files "GPU" ".*\\.(h|cpp)")
                                (find-files "SDL" ".*\\.(h|cpp)")
                                (find-files "UI" ".*\\.(h|cpp)"))
             ;; These headers are all hard-coded in the original source.
             (("ext/cityhash/") "")
             (("ext/glslang/glslang/") "glslang/")
             (("ext/glslang/") "glslang/")
             (("ext/miniupnp/") "")
             (("ext/SPIRV-Cross/") "spirv_cross/")
             (("ext/vulkan/") "vulkan/")
             (("ext/xxhash.h") "xxhash.h")
             ;; These definitions do not actually exist in the Vulkan headers,
             ;; but PPSSPP defines them in ext/vulkan.
             (("VK_FORMAT_BEGIN_RANGE") "VK_FORMAT_UNDEFINED")
             (("VK_FORMAT_END_RANGE") "VK_FORMAT_ASTC_12x12_SRGB_BLOCK"))
           ;; Next, we patch CMakeLists.
           (substitute* "CMakeLists.txt"
             ;; Drop unnecessary includes and targets.
             (("include_directories\\(ext/glslang\\)") "")
             (("include_directories\\(ext/xxhash\\)") "")
             (("include_directories\\(ext/cityhash\\)") "")
             (("include_directories\\(ext/zstd.*") "")
             (("libzstd_static") "zstd")
             (("set_target_properties\\(cityhash .*\\)") "")
             ;; Fix linking to GLEW.
             (("TARGET Ext::GLEW") "true")
             (("target_link_libraries\\(native Ext::GLEW\\)")
              "find_package(GLEW)\ntarget_link_libraries(native GLEW::GLEW)")
             (("Ext::Snappy") "snappy")
             ;; Don't search for cityhash/xxhash, we already have them.
             (("add_library\\((city|xx)hash STATIC") "if()\nendif(")
             (("ext/xxhash\\.[ch]") "")
             (("ext/cityhash/.*\\.(cpp|h)") "")
             (("if\\(USE_MINIUPNPC\\)" all)
              (string-append all "
find_package(miniupnpc)
target_link_libraries(${CoreLibName} miniupnpc ${LDLIBS})
elseif(FALSE)"))
             ;; Link all of spirv-cross.
             (("spirv-cross-glsl" all)
              (string-append all
                             " spirv-cross-core spirv-cross-cpp"
                             " spirv-cross-reflect spirv-cross-util")))
           (substitute* "ext/CMakeLists.txt"
             (("add_subdirectory\\(glew.*") "")
             (("add_subdirectory\\(glslang.*") "")
             (("add_subdirectory\\(snappy.*") "")
             (("add_subdirectory\\(SPIRV-Cross-build.*") "")
             (("add_subdirectory\\(zstd.*") ""))
           ;; Finally, we can delete the bundled sources.
           (for-each delete-file-recursively
                     '("ext/cmake"
                       "ext/glew"
                       "ext/glslang" "ext/glslang-build"
                       "ext/miniupnp" "ext/miniupnp-build"
                       "ext/native"
                       "ext/snappy"
                       "ext/SPIRV-Cross" "ext/SPIRV-Cross-build"
                       "ext/vulkan"
                       "ext/xxhash.c"
                       "ext/xxhash.h"
                       "ext/zlib"
                       "ext/zstd"))
           ;; Since we are not including git as an input, PPSSPP is confused
           ;; about its version.  Let's fix that here.
           (substitute* "git-version.cmake"
             (("unknown") ,version))))))
    (build-system cmake-build-system)
    (native-inputs (list pkg-config python))
    (inputs (list bash
                  cityhash
                  ffmpeg
                  glew
                  glslang
                  libpng
                  libzip
                  mesa
                  miniupnpc
                  sdl2
                  snappy
                  spirv-cross
                  vulkan-headers
                  vulkan-loader
                  xxhash
                  zlib
                  `(,zstd "lib")))
    (arguments
     (list
      #:out-of-source? #f
      #:configure-flags #~(list "-DUSE_DISCORD=OFF"
                                "-DUSE_SYSTEM_FFMPEG=ON"
                                "-DUSE_SYSTEM_LIBZIP=ON"
                                ;; for testing
                                "-DUNITTEST=ON" "-DHEADLESS=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'add-external-sources
            (lambda* (#:key inputs #:allow-other-keys)
              ;; TODO: unbundle armips.
              (copy-recursively #$(package-source armips) "ext/armips")
              ;; Some tests are externalised, so we add them here.
              (copy-recursively
               #$(let ((commit "1047400eaec6bcbdb2a64d326375ef6a6617c4ac"))
                   (origin
                     (method git-fetch)
                     (uri (git-reference
                           (url "https://github.com/hrydgard/pspautotests")
                           (commit commit)))
                     (sha256
                      (base32 "0nxv1lskcr8zbg6nrfai21mxsw0n5vaqhbsa41c3cxfyx5c4w2pg"))
                     (file-name (git-file-name "pspautotests" commit))))
               "pspautotests")))
          (add-after 'unpack 'fix-unittest-build
            (lambda _
              (substitute* "CMakeLists.txt"
                (("unittest/TestVertexJit.cpp" all)
                 (string-append all " unittest/TestShaderGenerators.cpp")))
              (substitute* "unittest/TestVertexJit.cpp"
                (("#include \"unittest/UnitTest.h\"" all)
                 (string-append all "\n#include <cmath>")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (for-each
                 (lambda (t) (invoke "./unitTest" t))
                 '("Arm64Emitter" "ArmEmitter" "X64Emitter" "VertexJit" "Asin"
                   "SinCos" "VFPUSinCos" "MathUtil" "Parsers" "Jit"
                   "MatrixTranspose" "ParseLBN" "QuickTexHash" "CLZ"
                   #|"ShaderGenerators"|#))
                (invoke "python3" "test.py" "-g"))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin/ppsspp (string-append out "/bin/ppsspp"))
                     (share (string-append out "/share/ppsspp")))
                (copy-recursively "icons/hicolor"
                                  (string-append out "/share/icons/hicolor"))
                (install-file "PPSSPPSDL" share)
                (copy-recursively "assets" (string-append share "/assets"))

                (make-desktop-entry-file
                 (string-append out "/share/applications/ppsspp.desktop")
                 #:name "PPSSPP"
                 #:exec (string-append share "/PPSSPPSDL")
                 #:icon "ppsspp")
                (mkdir-p (string-append out "/bin"))
                (with-output-to-file bin/ppsspp
                  (lambda ()
                    (format #t "#!~a~%exec ~a/PPSSPPSDL \"$@\""
                            (search-input-file inputs "/bin/bash") share)))
                (chmod bin/ppsspp #o755)))))))
    (home-page "https://www.ppsspp.org/")
    (synopsis "PSP emulator")
    (description
     "PPSSPP is a ``high-level'' emulator simulating the PSP operating
system.")
    (license license:gpl2+)))

(define-public exomizer
  (package
    (name "exomizer")
    (version "3.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://bitbucket.org/magli143/exomizer.git")
                     (commit "6a152b5605648f7a41eadd4b011a93ec92f74dd8")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1ynhkb5p2dypkikipc3krzif264l9rmx1wnjzzgw8n88i4zkymzg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; No target exists
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (delete-file-recursively "exodecrs")
             (delete-file-recursively "rawdecrs")
             (chdir "src")
             ;; Those will be regenerated.
             (delete-file "asm.tab.h")
             (delete-file "asm.tab.c")
             (delete-file "lex.yy.c")
             #t))
         (replace 'configure
           (lambda _
             (setenv "CC" ,(cc-for-target))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out-bin (string-append (assoc-ref outputs "out") "/bin")))
               (install-file "exomizer" out-bin)
               (install-file "exobasic" out-bin))
             #t)))))
    (native-inputs
     (list flex bison))
    (synopsis "Compressor for use on Commodore home computers")
    (description "This program compresses files in a way that tries to be as
efficient as possible but still allows them to be decompressed in environments
where CPU speed and RAM are limited.  It also generate a self-extractor for use
on a Commodore C64, C128 etc.")
    (home-page "https://bitbucket.org/magli143/exomizer/wiki/Home")
    ;; Some files are LGPL 2.1--but we aren't building from or installing those.
    ;; zlib license with an (non-)advertising clause.
    (license license:zlib)))

(define-public cc65
  (package
    (name "cc65")
    (version "2.19")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cc65/cc65.git")
                     (commit (string-append "V" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "01a15yvs455qp20hri2pbg2wqvcip0d50kb7dibi9427hqk9cnj4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; No target exists.
       #:make-flags
       (list "BUILD_ID=V2.18 - Git 55528249"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key source #:allow-other-keys)
             ;; We include $SOURCE/include in C_INCLUDE_PATH.  Remove it.
             (setenv "C_INCLUDE_PATH"
               (string-join
                (filter (lambda (name)
                          (not (string=? name (string-append source "/include"))))
                        (string-split (getenv "C_INCLUDE_PATH") #\:))
                ":"))
             #t)))))
    (synopsis "Development environment for 6502 systems")
    (description "This package provides a development environment for 6502 systems, including macro assembler, C compiler, linker, librarian and several other tools.")
    (home-page "https://cc65.github.io/")
    (license license:zlib)))
