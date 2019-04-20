;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2015, 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Aljosha Papsch <misc@rpapsch.de>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Jessica Tallon <tsyesika@tsyesika.se>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016, 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2018 Thomas Sigurdsen <tonton@riseup.net>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
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

(define-module (gnu packages password-utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages file)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system python))

(define-public pwgen
  (package
    (name "pwgen")
    (version "2.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pwgen/pwgen/" version
                           "/pwgen-" version ".tar.gz"))
       (sha256
        (base32 "0yy90pqrr2pszzhb5hxjishq9qc7dqd290amiibqx9fm1b9kvc6s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no test suite
    (home-page "http://pwgen.sourceforge.net/")
    (synopsis "Password generator")
    (description "Pwgen generates passwords which can be easily memorized by a
human.")
    (license license:gpl2)))

(define-public keepassxc
  (package
    (name "keepassxc")
    (version "2.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/keepassxreboot/" name
                           "/releases/download/" version "/keepassxc-"
                           version "-src.tar.xz"))
       (sha256
        (base32
         "02kq0a7a7hpw824n03apma00yq1c6dds224g15mrnnqqjn4af90c"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DWITH_XC_NETWORKING=YES"
                           "-DWITH_XC_BROWSER=YES"
                           "-DWITH_XC_SSHAGENT=YES")))
    (inputs
     `(("argon2" ,argon2)
       ("curl" ,curl)                   ; XC_NETWORKING
       ("libgcrypt" ,libgcrypt)
       ("libsodium" ,libsodium)         ; XC_BROWSER
       ("libxi" ,libxi)
       ("libxtst" ,libxtst)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("zlib" ,zlib)))
    (native-inputs
     `(("qttools" ,qttools)))
    (home-page "https://www.keepassxc.org")
    (synopsis "Password manager")
    (description "KeePassXC is a password manager or safe which helps you to
manage your passwords in a secure way.  You can put all your passwords in one
database, which is locked with one master key or a key-file which can be stored
on an external storage device.  The databases are encrypted using the
algorithms AES or Twofish.")
    ;; Non-functional parts use various licences.
    (license license:gpl3)))

(define-public keepassx
  (package
    (name "keepassx")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.keepassx.org/releases/" version
                           "/keepassx-" version ".tar.gz"))
       (sha256
        (base32
         "1ia7cqx9ias38mnffsl7da7g1f66bcbjsi23k49sln0c6spb9zr3"))))
    (build-system cmake-build-system)
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("libxi" ,libxi)
       ("libxtst" ,libxtst)
       ("qt" ,qt-4)))
    (native-inputs
     `(("zlib" ,zlib)))
    (home-page "https://www.keepassx.org")
    (synopsis "Password manager")
    (description "KeePassX is a password manager or safe which helps you to
manage your passwords in a secure way.  You can put all your passwords in one
database, which is locked with one master key or a key-file which can be stored
on an external storage device.  The databases are encrypted using the
algorithms AES or Twofish.")
    ;; Non functional parts use various licences.
    (license license:gpl3)
    (properties `((superseded . ,keepassxc)))))

(define-public pwsafe
  (package
    (name "pwsafe")
    (version "3.48.0")
    (home-page "https://www.pwsafe.org/" )
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pwsafe/pwsafe.git")
             (commit version)))
       (sha256 (base32 "0hxv23yh76liggxbjn4m132z15sklra8ms341xgzl4n5vjx30ihi"))
       (file-name (string-append name "-" version "-checkout"))))
    (build-system cmake-build-system)
    (native-inputs `(("gettext" ,gettext-minimal)
                     ("perl" ,perl)
                     ("zip" ,zip)))
    (inputs `(("curl" ,curl)
              ("file" ,file)
              ("gtest" ,googletest)
              ("libuuid" ,util-linux)
              ("libxt" ,libxt)
              ("libxtst" ,libxtst)
              ("openssl" ,openssl)
              ("qrencode" ,qrencode)
              ("wxwidgets" ,wxwidgets)
              ("xerces-c" ,xerces-c)))
    (arguments '(#:configure-flags (list "-DNO_GTEST=YES")
                 #:phases (modify-phases %standard-phases
                            (add-after 'unpack 'add-gtest
                              (lambda* (#:key inputs #:allow-other-keys)
                                (chmod "CMakeLists.txt" #o644)
                                (let ((cmake-port (open-file "CMakeLists.txt"
                                                             "a")))
                                  (display "find_package(GTest)
add_subdirectory(src/test)\n" cmake-port)
                                  (close cmake-port)
                                  #t)))
                            (add-after 'add-gtest 'patch-executables
                              (lambda* (#:key inputs #:allow-other-keys)
                                (chmod "src/test/OSTest.cpp" #o644)
                                (substitute* "src/os/unix/media.cpp"
                                  (("/usr/bin/file")
                                   (string-append (assoc-ref inputs "file")
                                                  "/bin/file")))
                                #t)))))
    (synopsis "Password safe with automatic input and key generation")
    (description "pwsafe is a password manager originally designed by Bruce
Schneier.  It offers a simple UI to manage passwords for different services.
There are other programs that support the file format on different
platforms.")
    (license license:artistic2.0)))

(define-public shroud
  (package
    (name "shroud")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/shroud/shroud-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1y43yhgy2zbrk5bqj3qyx9rkcz2bma9sinlrg7dip3jqms9gq4lr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-shroud
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (ccachedir (string-append out "/lib/guile/2.0/ccache"))
                    (prog      (string-append out "/bin/shroud")))
               (wrap-program prog
                 `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,ccachedir)))
               #t))))))
    (inputs
     `(("guile" ,guile-2.0)
       ("gnupg" ,gnupg)
       ("xclip" ,xclip)))
    (synopsis "GnuPG-based secret manager")
    (description "Shroud is a simple secret manager with a command line
interface.  The password database is stored as a Scheme s-expression and
encrypted with a GnuPG key.  Secrets consist of an arbitrary number of
key/value pairs, making Shroud suitable for more than just password storage.
For copying and pasting secrets into web browsers and other graphical
applications, there is xclip integration." )
    (home-page "https://dthompson.us/projects/shroud.html")
    (license license:gpl3+)))

(define-public yapet
  (package
    (name "yapet")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.guengel.ch/myapps/yapet/downloads/yapet-"
                                  version
                                  ".tar.bz2"))
              (sha256
               (base32
                "1lq46mpxdsbl6qw4cj58hp9q7jckmyvbsi08p5zr77rjgqadxyyy"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)
       ("openssl" ,openssl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Yet Another Password Encryption Tool")
    (description "YAPET is a text based password manager using the Blowfish
encryption algorithm.  Because of its small footprint and very few library
dependencies, it is suited for installing on desktop and server systems alike.
The text based user interface allows you to run YAPET easily in a Secure Shell
session.  Two companion utilities enable users to convert CSV files to YAPET
and vice versa.")
    (home-page "http://www.guengel.ch/myapps/yapet/")
    (license license:gpl3+)))

(define-public cracklib
  (package
    (name "cracklib")
    (version "2.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cracklib/cracklib/"
                           "releases/download/v" version "/"
                           "cracklib-" version ".tar.bz2"))
       (sha256
        (base32 "1rimpjsdnmw8f5b7k558cic41p2qy2n2yrlqp5vh7mp4162hk0py"))))
    (build-system gnu-build-system)
    (synopsis "Password checking library")
    (home-page "https://github.com/cracklib/cracklib")
    (description
     "CrackLib is a library containing a C function which may be used in a
@command{passwd}-like program.  The idea is simple: try to prevent users from
choosing passwords that could easily be guessed (or \"cracked\") by filtering
them out, at the source.")
    (license license:lgpl2.1)))

(define-public libpwquality
  (package
    (name "libpwquality")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "https://github.com/" name "/" name
                                   "/releases/download/" name  "-" version
                                   "/" name "-" version ".tar.bz2")
                    (string-append "https://launchpad.net/libpwquality/trunk/"
                                   version "/+download/"
                                   name "-" version ".tar.bz2")))
              (sha256
               (base32
                "0syyz8r54l8mqmbb0mw19qz4z2cx8gdgidicb8k2s5zjdh2gzrhx"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: have RUNPATH issue.
     '(#:configure-flags '("--disable-python-bindings")))
    (inputs
     `(("cracklib" ,cracklib)))
    (synopsis "Password quality checker")
    (home-page "https://github.com/libpwquality/libpwquality")
    (description
     "Libpwquality is a library for password quality checking and generation of
random passwords that pass the checks.")
    (license license:gpl2+)))

(define-public assword
  (package
    (name "assword")
    (version "0.11")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append
                     "http://http.debian.net/debian/pool/main/a/assword/"
                     "assword_" version ".orig.tar.gz")))
              (sha256
               (base32
                "03gkb6kvsghznbcw5l7nmrc6mn3ixkjd5jcs96ni4zs9l47jf7yp"))))
    (arguments
     `(;; irritatingly, tests do run but not there are two problems:
       ;;  - "import gtk" fails for unknown reasons here despite it the
       ;;    program working (indeed, I've found I have to do a logout and log
       ;;    back in in after an install order for some mumbo jumbo environment
       ;;    variable mess to work with pygtk and assword... what's up with
       ;;    that?)
       ;;  - even when the tests fail, they don't return a nonzero status,
       ;;    so I'm not sure how to programmatically get that information
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-assword
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog            (string-append
                                     (assoc-ref outputs "out")
                                     "/bin/assword"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (wrap-program prog
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))
               #t)))
         (add-after 'install 'manpage
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Without this substitution, it fails with
             ;; ImportError: No module named 'gpg'
             (substitute* "Makefile"
               (("PYTHONPATH=.") ""))
             (invoke "make" "assword.1")
             (install-file
              "assword.1"
              (string-append (assoc-ref outputs "out") "/share/man/man1")))))))
    (build-system python-build-system)
    (native-inputs
     `(("txt2man" ,txt2man)))
    (inputs
     `(("gtk+" ,gtk+)
       ("python-xdo" ,python-xdo)
       ("python-gpg" ,python-gpg)
       ("python-pygobject" ,python-pygobject)))
    (propagated-inputs
     `(("xclip" ,xclip)))
    (home-page "https://finestructure.net/assword/")
    (synopsis "Password manager")
    (description "assword is a simple password manager using GPG-wrapped
JSON files.  It has a command line interface as well as a very simple
graphical interface, which can even \"type\" your passwords into
any X11 window.")
    (license license:gpl3+)))

(define-public password-store
  (package
    (name "password-store")
    (version "1.7.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://git.zx2c4.com/password-store/snapshot/"
                              name "-" version ".tar.xz"))
              (sha256
               (base32
                "1x53k5dn3cdmvy8m4fqdld4hji5n676ksl0ql4armkmsds26av1b"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'patch-system-extension-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (extension-dir (string-append out "/lib/password-store/extensions")))
               (substitute* "src/password-store.sh"
                 (("^SYSTEM_EXTENSION_DIR=.*$")
                  ;; lead with whitespace to prevent 'make install' from
                  ;; overwriting it again
                  (string-append " SYSTEM_EXTENSION_DIR=\""
                                 "${PASSWORD_STORE_SYSTEM_EXTENSION_DIR:-"
                                 extension-dir
                                 "}\"\n"))))
             #t))
         (add-before 'install 'patch-passmenu-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "contrib/dmenu/passmenu"
               (("dmenu") (string-append (assoc-ref inputs "dmenu")
                                         "/bin/dmenu"))
               (("xdotool") (string-append (assoc-ref inputs "xdotool")
                                           "/bin/xdotool")))
             #t))
         (add-after 'install 'install-passmenu
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "contrib/dmenu/passmenu" bin)
               #t)))
         (add-after 'install 'wrap-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (path (map (lambda (pkg)
                                (string-append (assoc-ref inputs pkg) "/bin"))
                              '("coreutils" "getopt" "git" "gnupg" "qrencode"
                                "sed" "tree" "which" "xclip"))))
               (wrap-program (string-append out "/bin/pass")
                 `("PATH" ":" prefix (,(string-join path ":"))))
               #t)))
         (add-after 'wrap-path 'install-shell-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out      (assoc-ref outputs "out"))
                    (bashcomp (string-append out "/etc/bash_completion.d")))
               ;; TODO: install fish and zsh completions.
               (mkdir-p bashcomp)
               (copy-file "src/completion/pass.bash-completion"
                          (string-append bashcomp "/pass"))
               #t))))
       #:make-flags (list "CC=gcc" (string-append "PREFIX=" %output))
       ;; Parallel tests may cause a race condition leading to a
       ;; timeout in some circumstances.
       #:parallel-tests? #f
       #:test-target "test"))
    (native-search-paths
     (list (search-path-specification
            (variable "PASSWORD_STORE_SYSTEM_EXTENSION_DIR")
            (files '("lib/password-store/extensions")))))
    (inputs
     `(("dmenu" ,dmenu)
       ("getopt" ,util-linux)
       ("git" ,git)
       ("gnupg" ,gnupg)
       ("qrencode" ,qrencode)
       ("sed" ,sed)
       ("tree" ,tree)
       ("which" ,which)
       ("xclip" ,xclip)
       ("xdotool" ,xdotool)))
    (home-page "https://www.passwordstore.org/")
    (synopsis "Encrypted password manager")
    (description "Password-store is a password manager which uses GnuPG to
store and retrieve passwords.  The tool stores each password in its own
GnuPG-encrypted file, allowing the program to be simple yet secure.
Synchronization is possible using the integrated git support, which commits
changes to your password database to a git repository that can be managed
through the pass command.")
    (license license:gpl2+)))

(define-public argon2
  (package
    (name "argon2")
    (version "20171227")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/P-H-C/phc-winner-argon2/archive/"
                       version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1n6w5y3va7lrcym7cxr0nikapldqm80wxjdns584bvplq5r03spa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags '("CC=gcc"
                      "OPTTEST=1")     ;disable CPU optimization
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Makefile
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("PREFIX = /usr") (string-append "PREFIX = " out)))
               (substitute* "libargon2.pc"
                 (("prefix=/usr") (string-append "prefix=" out))
                 (("@HOST_MULTIARCH@") "")
                 (("@UPSTREAM_VER@") ,version))
               #t)))
         (delete 'configure)
         (add-after 'install 'install-argon2.pc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "libargon2.pc"
                             (string-append out "/lib/pkgconfig"))
               #t))))))
    (home-page "https://www.argon2.com/")
    (synopsis "Password hashing library")
    (description "Argon2 provides a key derivation function that was declared
winner of the 2015 Password Hashing Competition.")
    ;; Argon2 is dual licensed under CC0 and ASL 2.0.  Some of the source
    ;; files are CC0 only; see README.md and LICENSE for details.
    (license (list license:cc0 license:asl2.0))))

(define-public pass-git-helper
  (package
    (name "pass-git-helper")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/languitar/pass-git-helper/archive/release-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0lz5ncy44pz7z1j2nnyildx8sq33zi3xvg5nkwg25n11nasqh2xn"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-pass-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((password-store (assoc-ref inputs "password-store"))
                    (pass (string-append password-store "/bin/pass")))
               (substitute* "pass-git-helper"
                 (("'pass'") (string-append "'" pass "'")))
               #t))))))
    (inputs
     `(("python-pyxdg" ,python-pyxdg)
       ("password-store" ,password-store)))
    (home-page "https://github.com/languitar/pass-git-helper")
    (synopsis "Git credential helper interfacing with pass")
    (description "pass-git-helper is a git credential helper which allows to
use pass, the standard unix password manager, as the credential backend for
your git repositories.  This is achieved by explicitly defining mappings
between hosts and entries in the password store.")
    (license license:lgpl3+)))

(define-public john-the-ripper-jumbo
  (let ((official-version "1.8.0")
        (jumbo-version "1"))
    (package
      (name "john-the-ripper-jumbo")
      (version (string-append official-version "-" jumbo-version))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "http://www.openwall.com/john/j/john-"
                             official-version "-jumbo-" jumbo-version ".tar.xz"))
         (sha256
          (base32
           "08q92sfdvkz47rx6qjn7qv57cmlpy7i7rgddapq5384mb413vjds"))
         (patches
          (list (origin
                  (method url-fetch)
                  (uri (string-append "https://github.com/magnumripper/"
                                      "JohnTheRipper/commit/"
                                      "e2e868db3e153b3f959e119a51703d4afb99c624.patch"))
                  (file-name "john-the-ripper-jumbo-gcc5-inline.patch")
                  (sha256
                   (base32
                    "1shvcf1y2097115mxhzdkm64dr106a8zr6pqjqyh171q5ng5vfra")))
                (origin
                  (method url-fetch)
                  (uri (string-append "https://github.com/magnumripper/"
                                      "JohnTheRipper/commit/"
                                      "480e95b0e449863be3e1a5b0bc634a67df28b618.patch"))
                  (file-name "john-the-ripper-jumbo-non-x86.patch")
                  (sha256
                   (base32
                    "1ffd9dvhk0sb6ss8dv5yalh01lz30i7rilqilf2xv68gax2hyjqx")))))))
      (build-system gnu-build-system)
      (inputs
       `(("gmp" ,gmp)
         ("krb5" ,mit-krb5)
         ("libpcap" ,libpcap)
         ("nss" ,nss)
         ("openssl" ,openssl)
         ("zlib" ,zlib)))
      (arguments
       `(#:configure-flags
         (list (string-append
                "CFLAGS=-O2 -g "
                "-DJOHN_SYSTEMWIDE=1 "
                "-DJOHN_SYSTEMWIDE_EXEC='\"" %output "/libexec/john\"' "
                "-DJOHN_SYSTEMWIDE_HOME='\"" %output "/share/john\"'")
               ;; For now, do not test for instruction set in configure, and
               ;; do not pass '-march=native' to gcc:
               "--disable-native-tests"
               "--disable-native-macro")
         #:tests? #f ;tests try to create '.john' in the build user's $HOME
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'chdir-src
             (lambda _ (chdir "src")))
           (replace 'install
             (lambda _
               (let ((bindir (string-append %output "/bin"))
                     (docdir (string-append %output "/share/doc/john"))
                     (execdir (string-append %output "/libexec/john"))
                     (homedir (string-append %output "/share/john"))
                     (install-file-to (lambda (dir)
                                        (lambda (f) (install-file f dir))))
                     (symlink? (lambda (_ s) (eq? (stat:type s) 'symlink))))
                 (with-directory-excursion "../run"
                   (for-each (install-file-to execdir)
                             (cons* "mailer" "benchmark-unify"
                                    (find-files "." ".*\\.(py|rb|pl)")))
                   (for-each (install-file-to homedir)
                             (append (find-files "." "(stats|dictionary.*)")
                                     (find-files "." "(.*\\.chr|.*\\.lst)")
                                     (find-files "." ".*\\.conf")))
                   (for-each (install-file-to bindir)
                             '("tgtsnarf" "genmkvpwd" "mkvcalcproba"
                               "raw2dyna" "luks2john" "vncpcap2john"
                               "uaf2john" "calc_stat" "wpapcap2john"
                               "cprepair" "relbench"  "SIPdump" "john"))
                   (for-each (lambda (f) ;install symlinked aliases
                               (symlink "john"
                                        (string-append bindir "/" (basename f))))
                             (find-files "." symlink?)))
                 (copy-recursively "../doc" docdir)
                 #t))))))
      (home-page "http://www.openwall.com/john/")
      (synopsis "Password cracker")
      (description "John the Ripper is a fast password cracker.  Its primary
purpose is to detect weak Unix passwords.  Besides several @code{crypt}
password hash types most commonly found on various Unix systems, supported out
of the box are Windows LM hashes, plus lots of other hashes and ciphers.  This
is the community-enhanced, \"jumbo\" version of John the Ripper.")
      (license license:gpl2+))))

(define-public sala
  (package
    (name "sala")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sala" version))
       (sha256
        (base32
         "13qgmc3i2a0cqp8jqrfl93lnphfagb32pgfikc1gza2a14asxzi8"))))
    (build-system python-build-system)
    (arguments
     ;; Sala is supposed to work with Python 3.2 or higher,
     ;; but it doesn't work with Python 3.6. Better stick
     ;; to Python 2, which works fine.
     `(#:python ,python-2))
    (propagated-inputs
     `(("gnupg" ,gnupg)
       ("pwgen" ,pwgen)))
    (home-page "http://www.digip.org/sala/")
    (synopsis "Encrypted plaintext password store")
    (description
     "Store passwords and other bits of sensitive plain-text information
to encrypted files on a directory hierarchy.  The information is protected
by GnuPG's symmetrical encryption.")
    (license license:expat)))

(define-public fpm2
  (package
    (name "fpm2")
    (version "0.79")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://als.regnet.cz/fpm2/download/fpm2-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "19sdy1lygfhkg5nxi2w9a4d9kwvw24nxp0ix0p0lz91qpvk9qpnm"))))
    (build-system gnu-build-system)
    (inputs `(("gtk2" ,gtk+-2)
              ("gnupg" ,gnupg)
              ("libxml2" ,libxml2)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("intltool" ,intltool)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           ;; The file po/POTFILES.in ends up missing for some reason in
           ;; both nix and guix builds. Adding the file with contents
           ;; found during troubleshooting.
           (lambda _
             (call-with-output-file "po/POTFILES.in"
               (lambda (port)
                 (format port "data/fpm2.desktop.in
data/fpm2.desktop.in.in
fpm2.glade
src/callbacks.c
src/fpm.c
src/fpm_file.c
src/interface.c
src/support.c
fpm2.glade
")))
             #t)))))
    (synopsis "Manage, generate and store passwords encrypted")
    (description "FPM2 is GTK2 port from Figaro's Password Manager
originally developed by John Conneely, with some new enhancements.

Upstream development seems to have stopped.  It is therefore recommended
to use a different password manager.")
    (home-page "https://als.regnet.cz/fpm2/")
    (license license:gpl2+)))

(define-public pass-rotate
  (package
    (name "pass-rotate")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/SirCmpwn/pass-rotate/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "1svm5nj8bczv2dg8lh2zqqhbsrljqsw9680r03qwgl9vlci90210"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system python-build-system)
    (inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-docopt" ,python-docopt)
       ("python-html5lib" ,python-html5lib)
       ("python-requests" ,python-requests)))
    (home-page "https://github.com/SirCmpwn/pass-rotate")
    (synopsis "Rotate password on online services")
    (description "pass-rotate is a command line utility and python library for
rotating passwords on various web services.  It makes it easier to rotate your
passwords, one at a time or in bulk, when security events or routine upkeep of
your online accounts makes it necessary.")
    (license license:expat)))

(define-public hashcat
  (package
    (name "hashcat")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hashcat.net/files/hashcat-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0f73y4cg8c7a6q7x34qvpfi4g3lw6j9bnn0a13g43aqyiskflfr8"))))
    (native-inputs
     `(("opencl-headers" ,opencl-headers)))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ;no tests
       #:make-flags (list (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://hashcat.net/hashcat/")
    (synopsis "Advanced password recovery utility")
    (description "Hashcat is an password recovery utility, supporting five
unique modes of attack for over 200 highly-optimized hashing algorithms.
Hashcat currently supports CPUs, GPUs, and other hardware accelerators on
Linux, Windows, and macOS, and has facilities to help enable distributed
password cracking.")
    (license license:expat)))

(define-public hashcat-utils
  (package
    (name "hashcat-utils")
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/hashcat/hashcat-utils/releases/"
                           "download/v" version "/"
                           "hashcat-utils-" version ".7z"))
       (sha256
        (base32 "0kq555kb338691qd7zjmi8vhq4km3apnsl2w63zh0igwzcjx6lx1"))))
    (native-inputs
     `(("p7zip" ,p7zip)))
    (inputs
     `(("perl" ,perl)))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags (list "CC=gcc"
                          ;; Upstream bug(?): "make all" seems to remove the
                          ;; Perl scripts from the source.
                          "native")
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "7z" "x" source)
             (chdir (string-append "hashcat-utils-" ,version "/src"))
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p out)
               (for-each
                (lambda (file)
                  (copy-file file (string-append out "/"
                                                 (basename file ".bin"))))
                (find-files "." "\\.bin$"))
               (for-each
                (lambda (file)
                  (copy-file file (string-append out "/"
                                                 (basename file ".pl"))))
                (find-files "../bin" "\\.pl$"))
               #t))))))
    (home-page "https://github.com/hashcat/hashcat-utils/")
    (synopsis "Small utilities that are useful in advanced password cracking")
    (description "Hashcat-utils are a set of small utilities that are useful
in advanced password cracking.  They all are packed into multiple stand-alone
binaries.  All of these utils are designed to execute only one specific
function.  Since they all work with @code{STDIN} and @code{STDOUT} you can
group them into chains.")
    (license license:expat)))
