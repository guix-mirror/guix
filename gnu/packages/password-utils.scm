;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
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
;;; Copyright © 2017, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2018 Thomas Sigurdsen <tonton@riseup.net>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2018, 2019 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019 Jens Mølgaard <jens@zete.tk>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
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
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages authentication)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
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
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages tcl)
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
    (version "2.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/keepassxreboot/keepassxc"
                           "/releases/download/" version "/keepassxc-"
                           version "-src.tar.xz"))
       (sha256
        (base32 "1sx647mp1xikig50p9bb6vxv18ymdfj3wkxj6qfdr1zfcv7gn005"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DWITH_XC_ALL=YES"
                           "-DWITH_XC_UPDATECHECK=NO")))
    (inputs
     `(("argon2" ,argon2)
       ("libgcrypt" ,libgcrypt)
       ("libsodium" ,libsodium)         ; XC_BROWSER
       ("libyubikey" ,libyubikey)       ; XC_YUBIKEY
       ("libxi" ,libxi)
       ("libxtst" ,libxtst)
       ("qrencode" ,qrencode)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("quazip" ,quazip)               ; XC_KEESHARE
       ("readline" ,readline)
       ("yubikey-personalization" ,yubikey-personalization) ; XC_YUBIKEY
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
    ;; While various parts of the software are licensed under different licenses,
    ;; the combined work falls under the GPLv3.
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
    (version "3.50.0")
    (home-page "https://www.pwsafe.org/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pwsafe/pwsafe.git")
             (commit version)))
       (sha256
        (base32 "01kfssd2vr64yh4dqhch58x36n3aj3hpj5n560f41rsxym69c6qs"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gtest" ,googletest)
       ("perl" ,perl)
       ("zip" ,zip)))
    (inputs `(("curl" ,curl)
              ("file" ,file)
              ("libuuid" ,util-linux "lib")
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
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/shroud/shroud-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1l2shrhvcwfzkar9qiwb75nhcqmx25iz55lzmz0c187nbjhqzi9p"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 popen)
                    (ice-9 rdelim))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-shroud
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (guile (assoc-ref inputs "guile"))
                    (effective (read-line
                                (open-pipe* OPEN_READ
                                            (string-append guile "/bin/guile")
                                            "-c" "(display (effective-version))")))
                    (ccachedir (string-append out
                                             "/lib/guile/" effective "/site-ccache"))
                    (prog      (string-append out "/bin/shroud")))
               (wrap-program prog
                 `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,ccachedir)))
               #t))))))
    (inputs
     `(("guile" ,guile-2.2)
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
    (version "2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://yapet.guengel.ch/downloads/yapet-"
                           version ".tar.xz"))
       (sha256
        (base32 "1fl4s7v1psl52ndd6i7716i9f493aj8ipl6lgmraadnn5h26l3pm"))))
    (build-system gnu-build-system)
    (inputs
     `(("argon2" ,argon2)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)))
    (native-inputs
     `(("cppunit" ,cppunit)
       ("pkg-config" ,pkg-config)))
    (synopsis "Yet Another Password Encryption Tool")
    (description "YAPET is a text based password manager using the Blowfish
encryption algorithm.  Because of its small footprint and very few library
dependencies, it is suited for installing on desktop and server systems alike.
The text based user interface allows you to run YAPET easily in a Secure Shell
session.  Two companion utilities enable users to convert CSV files to YAPET
and vice versa.")
    (home-page "https://yapet.guengel.ch/")
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
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-dict
           (lambda* (#:key make-flags #:allow-other-keys)
             (begin
               (chmod (string-append "util/cracklib-format") #o755)
               (apply invoke "make" "dict" make-flags)
               #t))))))
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
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "https://github.com/libpwquality/libpwquality"
                                   "/releases/download/libpwquality-" version
                                   "/libpwquality-" version ".tar.bz2")
                    (string-append "https://launchpad.net/libpwquality/trunk/"
                                   version "/+download/"
                                   "libpwquality-" version ".tar.bz2")))
              (sha256
               (base32
                "13hw532fmzc5xjpy75d74rlfdlxf2a8ibb4hyy9c0s92wsgf0qsj"))))
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
               #t))))
       #:make-flags (list "CC=gcc" (string-append "PREFIX=" %output)
                          "WITH_ALLCOMP=yes"
                          (string-append "BASHCOMPDIR="
                                         %output "/etc/bash_completion.d"))
       ;; Parallel tests may cause a race condition leading to a
       ;; timeout in some circumstances.
       #:parallel-tests? #f
       #:test-target "test"))
    (native-search-paths
     (list (search-path-specification
            (variable "PASSWORD_STORE_SYSTEM_EXTENSION_DIR")
            (separator #f)                        ;single entry
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

(define-public pass-otp
  (package
    (name "pass-otp")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/tadfisher/pass-otp/releases/"
                       "download/v" version "/pass-otp-" version ".tar.gz"))
       (sha256
        (base32
         "0rrs3iazq80dn0wbl20xkh270428jd8l99m5gd7hl93s4r4sc82p"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (let* ((out      (assoc-ref %outputs "out"))
              (bashcomp (string-append out "/etc/bash_completion.d")))
         (list (string-append "PREFIX=" %output)
               (string-append "BASHCOMPDIR=" bashcomp)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'build 'patch-oath-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "otp.bash"
               (("^OATH=.*$")
                (string-append
                 "OATH="
                 (assoc-ref inputs "oath-toolkit")
                 "/bin/oathtool\n")))
             #t)))
       #:test-target "test"))
    (inputs
     `(("oath-toolkit" ,oath-toolkit)))
    (native-inputs
     `(("password-store" ,password-store)
       ("expect" ,expect)
       ("git" ,git)
       ("gnupg" ,gnupg)
       ("which" ,which)))
    (home-page "https://github.com/tadfisher/pass-otp")
    (synopsis "Pass extension for managing one-time-password (OTP) tokens")
    (description
     "Pass OTP is an extension for password-store that allows adding
one-time-password (OTP) secrets, generating OTP codes, and displaying secret
key URIs using the standard otpauth:// scheme.")
    (license license:gpl3+)))

(define-public qtpass
  (package
    (name "qtpass")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/IJHack/QtPass.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1vfhfyccrxq9snyvayqfzm5rqik8ny2gysyv7nipc91kvhq3bhky"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; lupdate/lrelease need to find qmake.
               (setenv "QMAKE" "qmake")
               ;; qmake needs to find lrelease/lupdate.
               (invoke "qmake"
                       "QMAKE_LRELEASE=lrelease"
                       "QMAKE_LUPDATE=lupdate"
                       (string-append "PREFIX=" out)))))
         (add-after 'configure 'reset-resource-timestamps
           ;; Reset timestamps on localization files for a reproducible build.
           (lambda _
             (with-directory-excursion "localization"
               (for-each (lambda (file)
                           (let* ((base (basename file ".qm"))
                                  (src (string-append base ".ts"))
                                  (st (stat src)))
                             (set-file-time file st)))
                         (find-files "." ".*\\.qm")))
             #t))
         (add-after 'install 'install-auxilliary
           ;; Install man-page, icon and .desktop file.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications"))
                    (icons (string-append out "/share/icons/hicolor/scalable/apps"))
                    (man (string-append out "/share/man/man1")))
               (install-file "qtpass.desktop" applications)
               (install-file "artwork/icon.svg" icons)
               (rename-file (string-append icons "/icon.svg")
                            (string-append icons "/qtpass-icon.svg"))
               (install-file "qtpass.1" man)
               #t)))
         (add-before 'check 'check-setup
           ;; Make Qt render "offscreen", required for tests.
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (home-page "https://qtpass.org")
    (synopsis "GUI for password manager password-store")
    (description
     "Qt-based graphical user interface for the password manager
password-store also known as pass.  Can use either pass or gpg to interact
with password-store files.  Features configurable password generation,
templates, clipboard handling, and per folder settings for multi-recipient
encryption.")
    (license license:gpl3+)))

(define-public rofi-pass
  (package
    (name "rofi-pass")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://raw.githubusercontent.com/carnager/rofi-pass/"
                       version "/rofi-pass"))
       (sha256
        (base32 "0msldkndqp40nx1s5s7ggcr97ir4nshpmnyzvj5hqw1l7m3gvw6j"))
       (file-name name)))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source (string-append (assoc-ref %build-inputs "source")))
               (script "rofi-pass")
               (out (assoc-ref %outputs "out")))
           (copy-file source script)
           (chmod script #o555)
           (install-file script (string-append out "/bin"))))))
    (propagated-inputs
     `(("password-store" ,password-store)
       ("rofi" ,rofi)
       ("xdotool" ,xdotool)))
    (home-page "https://github.com/carnager/rofi-pass")
    (synopsis "Rofi frontend for password-store")
    (description "Rofi-pass provides a way to manipulate information stored
using password-store through rofi interface:
@enumerate
@item open URLs of entries with hotkey;
@item type any field from entry;
@item auto-typing of user and/or password fields;
@item auto-typing username based on path;
@item auto-typing of more than one field, using the autotype entry;
@item bookmarks mode (open stored URLs in browser, default: Alt+x).
@end enumerate")
    (license license:gpl3)))

(define-public argon2
  (package
    (name "argon2")
    (version "20190702")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/P-H-C/phc-winner-argon2")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01rwanr4wmr9vm6c712x411wig543q195z2icn388z892a93lc7p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "LIBRARY_REL=lib"
                          (string-append "ARGON2_VERSION=" ,version)
                          "OPTTEST=1")  ; disable CPU optimization
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; No configure script.
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
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/languitar/pass-git-helper")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "18nvwlp0w4aqj268wly60rnjzqw2d8jl0hbs6bkwp3hpzzz5g6yd"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-pass-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((password-store (assoc-ref inputs "password-store"))
                    (pass (string-append password-store "/bin/pass")))
               (substitute* '("passgithelper.py"
                              "test_passgithelper.py")
                 (("'pass'") (string-append "'" pass "'")))
               #t)))
         (replace 'check
           (lambda _
             (setenv "HOME" (getcwd))
             (invoke "pytest"))))))
    (inputs
     `(("python-pyxdg" ,python-pyxdg)
       ("password-store" ,password-store)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-mock" ,python-pytest-mock)))
    (home-page "https://github.com/languitar/pass-git-helper")
    (synopsis "Git credential helper interfacing with pass")
    (description "pass-git-helper is a git credential helper which allows to
use pass, the standard unix password manager, as the credential backend for
your git repositories.  This is achieved by explicitly defining mappings
between hosts and entries in the password store.")
    (license license:lgpl3+)))

(define-public john-the-ripper-jumbo
  (let ((official-version "1.9.0")
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
           "0fvz3v41hnaiv1ggpxanfykyfjq79cwp9qcqqn63vic357w27lgm"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("perl" ,perl)))
      (inputs
       `(("gmp" ,gmp)
         ("libpcap" ,libpcap)
         ("nss" ,nss)
         ("openssl" ,openssl-1.0)
         ("python" ,python-2)           ; For "python" and "python2" shebangs
         ("ruby" ,ruby)                 ; For genincstats.rb
         ("zlib" ,zlib)))
      (arguments
       `(#:configure-flags
         (list "--with-systemwide"
               ;; Do not test for instruction set in configure, and do not
               ;; pass '-march=native' to gcc:
               "--disable-native-tests"
               "--disable-native-march"
               ,(string-append
                 "--enable-simd="
                 (let ((system (or (%current-target-system)
                                   (%current-system))))
                   (cond
                    ((or (string-prefix? "x86_64" system)
                         (string-prefix? "i686" system)) "sse2")
                    ((string-prefix? "aarch" system) "neon")
                    (else "no")))))
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'chdir-src
             (lambda _ (chdir "src") #t))
           (replace 'install
             (lambda _
               (let ((bindir (string-append %output "/bin"))
                     (docdir (string-append %output "/share/doc/john"))
                     (execdir (string-append %output "/libexec/john"))
                     (datadir (string-append %output "/share/john"))
                     (install-file-to (lambda (dir)
                                        (lambda (f) (install-file f dir))))
                     (symlink? (lambda (_ s) (eq? (stat:type s) 'symlink))))
                 (with-directory-excursion "../run"
                   (for-each (install-file-to bindir)
                             (cons*
                              "john" "makechr" "cprepair" "SIPdump" "tgtsnarf"
                              "genmkvpwd" "mkvcalcproba" "calc_stat" "raw2dyna"
                              (find-files "." "(to|2)?john(-[^.]*)?$")))
                   (for-each (lambda (f) ; Install symlinked aliases
                               (let ((tgt (string-append bindir "/" (basename f))))
                                 ;; The use of install-file above dereferences
                                 ;; symlinks.  We'd rather have the symlinks
                                 ;; for clarity, so remove tgt before linking.
                                 (when (file-exists? tgt) (delete-file tgt))
                                 (symlink "john" tgt)))
                             (find-files "." symlink?))
                   (for-each (install-file-to execdir)
                             (cons* "mailer" "benchmark-unify" "relbench"
                                    (find-files "." ".*\\.js")))
                   (for-each (lambda (f)
                               (let* ((base (basename f))
                                      (name (substring base 0 (string-index base #\.)))
                                      (link (string-append bindir "/" name)))
                                 (install-file f execdir)
                                 (when (and (executable-file? f)
                                            (not (file-exists? link)))
                                   (symlink (string-append execdir "/" base) link))))
                             (find-files "." ".*\\.(pl|py|rb|lua)"))
                   (for-each (install-file-to datadir)
                             (append (find-files "." "(stats|dictionary.*)")
                                     (find-files "." "(.*\\.chr|.*\\.lst)")
                                     (find-files "." ".*\\.conf")))
                   (copy-recursively "rules" (string-append datadir "/rules")))
                 (copy-recursively "../doc" docdir)
                 #t)))
           (delete 'check) ; Tests need installed .conf files; move after install
           (add-after 'install 'check
             (lambda args
               (setenv "HOME" "/tmp")   ; Some tests need to write to ~/.john
               (setenv "OMP_NUM_THREADS" (number->string (parallel-job-count)))
               (apply (assoc-ref %standard-phases 'check) args))))))
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
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ddevault/pass-rotate")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1m067vvdlc85csbpkp8aw4s3ags7q8s3jszrr32kmj9qhk5c254f"))))
    (build-system python-build-system)
    (inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-docopt" ,python-docopt)
       ("python-html5lib" ,python-html5lib)
       ("python-requests" ,python-requests)))
    (home-page "https://github.com/ddevault/pass-rotate")
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

(define-public bruteforce-luks
  (package
    (name "bruteforce-luks")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/glv2/bruteforce-luks/releases/download/"
                           version
                           "/bruteforce-luks-"
                           version
                           ".tar.lz"))
       (sha256
        (base32 "0yawrlbbklhmvwr99wm7li3r0d5kxvpkwf33a12rji7z0ya5p340"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("lzip" ,lzip)))
    (inputs
     `(("cryptsetup" ,cryptsetup)))
    (synopsis "LUKS encrypted volume cracker")
    (description
     "This is a cracker for LUKS encrypted volumes.  It can be used either in
exhaustive mode to try every password given a charset or in dictionary mode to
try every password contained in a file.")
    (home-page "https://github.com/glv2/bruteforce-luks")
    (license license:gpl3+)))
