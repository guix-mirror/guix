;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2015, 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Aljosha Papsch <misc@rpapsch.de>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Jessica Tallon <tsyesika@tsyesika.se>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Konrad Hinsen <konrad.hinsen@fastmail.net>
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
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
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
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/keepassxreboot/" name
                           "/releases/download/" version "/keepassxc-"
                           version "-src.tar.xz"))
       (sha256
        (base32
         "1gdrbpzwbs56anc3k5vklvcackcn214pc8gm5xh5zcymsi8q4zff"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DWITH_XC_NETWORKING=YES"
                           "-DWITH_XC_BROWSER=YES"
                           "-DWITH_XC_SSHAGENT=YES")))
    (inputs
     `(("argon2" ,argon2)
       ("curl" ,curl) ; XC_NETWORKING
       ("libgcrypt" ,libgcrypt)
       ("libsodium" ,libsodium) ; XC_BROWSER
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
    ;; Non functional parts use various licences.
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
    (version "2.9.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/cracklib/cracklib/"
                                  "releases/download/" name "-" version "/"
                                  name "-" version ".tar.gz"))
              (patches (search-patches "cracklib-CVE-2016-6318.patch"
                                       "cracklib-fix-buffer-overflow.patch"))
              (sha256
               (base32
                "0hrkb0prf7n92w6rxgq0ilzkk6rkhpys2cfqkrbzswp27na7dkqp"))))
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
             (and
              ;; Without this substitution, it fails with
              ;; ImportError: No module named 'gpg'
              (substitute* "Makefile"
                (("PYTHONPATH=.") ""))
              (zero? (system* "make" "assword.1"))
              (install-file
               "assword.1"
               (string-append (assoc-ref outputs "out") "/share/man/man1"))))))))
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
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://git.zx2c4.com/password-store/snapshot/"
                              name "-" version ".tar.xz"))
              (sha256
               (base32
                "0scqkpll2q8jhzcgcsh9kqz0gwdpvynivqjmmbzax2irjfaiklpn"))
              (patches (search-patches "password-store-gnupg-compat.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
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
             (let ((out (assoc-ref outputs "out")))
               (copy-file "contrib/dmenu/passmenu"
                          (string-append out "/bin/passmenu"))
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
    (home-page "http://www.passwordstore.org/")
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
               #t)))
         (delete 'configure))))
    (home-page "https://www.argon2.com/")
    (synopsis "Password hashing library")
    (description "Argon2 provides a key derivation function that was declared
winner of the 2015 Password Hashing Competition.")
    ;; Argon2 is dual licensed under CC0 and ASL 2.0.  Some of the source
    ;; files are CC0 only; see README.md and LICENSE for details.
    (license (list license:cc0 license:asl2.0))))

(define-public python-bcrypt
  (package
    (name "python-bcrypt")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "bcrypt" version))
        (sha256
         (base32
          "1giy0dvd8gvq6flxh44np1v2nqwsji5qsnrz038mgwzgp7c20j75"))))
        (build-system python-build-system)
    (native-inputs
     `(("python-pycparser" ,python-pycparser)
       ("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-cffi" ,python-cffi)
       ("python-six" ,python-six)))
    (home-page "https://github.com/pyca/bcrypt/")
    (synopsis
     "Modern password hashing library")
    (description
     "Bcrypt is a Python module which provides a password hashing method based
on the Blowfish password hashing algorithm, as described in
@url{http://static.usenix.org/events/usenix99/provos.html,\"A Future-Adaptable
Password Scheme\"} by Niels Provos and David Mazieres.")
    (license license:asl2.0)))

(define-public python2-bcrypt
  (package-with-python2 python-bcrypt))

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
