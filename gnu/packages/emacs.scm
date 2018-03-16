;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2015, 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2017 ng0 <ng0@infotropique.org>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2017, 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016, 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2016, 2017, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 George Clemmer <myglc2@gmail.com>
;;; Copyright © 2017 Feng Shu <tumashu@163.com>
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017, 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2017, 2018 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages code)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages fontutils)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public emacs
  (package
    (name "emacs")
    (version "25.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/emacs/emacs-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "02y00y9q42g1iqgz5qhmsja75hwxd88yrn9zp14lanay0zkwafi5"))
             (patches (search-patches "emacs-exec-path.patch"
                                      "emacs-fix-scheme-indent-function.patch"
                                      "emacs-source-date-epoch.patch"))
             (modules '((guix build utils)))
             (snippet
              ;; Delete the bundled byte-compiled elisp files and
              ;; generated autoloads.
              '(with-directory-excursion "lisp"
                 (for-each delete-file
                           (append (find-files "." "\\.elc$")
                                   (find-files "." "loaddefs\\.el$")
                                   ;; This is the only "autoloads" file that
                                   ;; does not have "*loaddefs.el" name.
                                   '("eshell/esh-groups.el")))

                 ;; Make sure Tramp looks for binaries in the right places on
                 ;; remote GuixSD machines, where 'getconf PATH' returns
                 ;; something bogus.
                 (substitute* "net/tramp-sh.el"
                   ;; Patch the line after "(defcustom tramp-remote-path".
                   (("\\(tramp-default-remote-path")
                    (format #f "(tramp-default-remote-path ~s ~s ~s ~s "
                            "~/.guix-profile/bin" "~/.guix-profile/sbin"
                            "/run/current-system/profile/bin"
                            "/run/current-system/profile/sbin")))

                 ;; Make sure Man looks for C header files in the right
                 ;; places.
                 (substitute* "man.el"
                   (("\"/usr/local/include\"" line)
                    (string-join
                     (list line
                           "\"~/.guix-profile/include\""
                           "\"/var/guix/profiles/system/profile/include\"")
                     " ")))))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-/bin/pwd
           (lambda _
             ;; Use `pwd', not `/bin/pwd'.
             (substitute* (find-files "." "^Makefile\\.in$")
               (("/bin/pwd")
                "pwd"))))
         (add-after 'install 'install-site-start
           ;; Use 'guix-emacs' in "site-start.el".  This way, Emacs packages
           ;; provided by Guix and installed in
           ;; ~/.guix-profile/share/emacs/site-lisp/guix.d/PACKAGE-VERSION are
           ;; automatically found.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out      (assoc-ref outputs "out"))
                    (lisp-dir (string-append out "/share/emacs/site-lisp")))
               (copy-file (assoc-ref inputs "guix-emacs.el")
                          (string-append lisp-dir "/guix-emacs.el"))
               (with-output-to-file (string-append lisp-dir "/site-start.el")
                 (lambda ()
                   (display
                    (string-append "(when (require 'guix-emacs nil t)\n"
                                   "  (guix-emacs-autoload-packages))\n"))))
               #t))))))
    (inputs
     `(("gnutls" ,gnutls)
       ("ncurses" ,ncurses)

       ;; TODO: Add the optional dependencies.
       ("libx11" ,libx11)
       ("gtk+" ,gtk+)
       ("libxft" ,libxft)
       ("libtiff" ,libtiff)
       ("giflib" ,giflib)
       ("libjpeg" ,libjpeg-8)
       ("imagemagick" ,imagemagick)
       ("acl" ,acl)

       ;; When looking for libpng `configure' links with `-lpng -lz', so we
       ;; must also provide zlib as an input.
       ("libpng" ,libpng)
       ("zlib" ,zlib)

       ("librsvg" ,librsvg)
       ("libxpm" ,libxpm)
       ("libxml2" ,libxml2)
       ("libice" ,libice)
       ("libsm" ,libsm)
       ("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)

       ;; multilingualization support
       ("libotf" ,libotf)
       ("m17n-lib" ,m17n-lib)))
    (native-inputs
     `(("guix-emacs.el" ,(search-auxiliary-file "emacs/guix-emacs.el"))
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))

    (native-search-paths
     (list (search-path-specification
            (variable "INFOPATH")
            (files '("share/info")))))

    (home-page "https://www.gnu.org/software/emacs/")
    (synopsis "The extensible, customizable, self-documenting text editor")
    (description
     "GNU Emacs is an extensible and highly customizable text editor.  It is
based on an Emacs Lisp interpreter with extensions for text editing.  Emacs
has been extended in essentially all areas of computing, giving rise to a
vast array of packages supporting, e.g., email, IRC and XMPP messaging,
spreadsheets, remote server editing, and much more.  Emacs includes extensive
documentation on all aspects of the system, from basic editing to writing
large Lisp programs.  It has full Unicode support for nearly all human
languages.")
    (license license:gpl3+)))

(define-public emacs-minimal
  ;; This is the version that you should use as an input to packages that just
  ;; need to byte-compile .el files.
  (package (inherit emacs)
    (name "emacs-minimal")
    (synopsis "The extensible text editor (used only for byte-compilation)")
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'install-site-start)))))
    (inputs
     `(("ncurses" ,ncurses)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))))

(define-public emacs-no-x
  (package (inherit emacs)
    (name "emacs-no-x")
    (synopsis "The extensible, customizable, self-documenting text
editor (console only)")
    (build-system gnu-build-system)
    (inputs (fold alist-delete
                  (package-inputs emacs)
                  '("libx11" "gtk+" "libxft" "libtiff" "giflib" "libjpeg"
                    "imagemagick" "libpng" "librsvg" "libxpm" "libice"
                    "libsm"

                    ;; These depend on libx11, so remove them as well.
                    "libotf" "m17n-lib" "dbus")))))

(define-public emacs-no-x-toolkit
  (package (inherit emacs)
    (name "emacs-no-x-toolkit")
    (synopsis "The extensible, customizable, self-documenting text
editor (without an X toolkit)" )
    (build-system gnu-build-system)
    (inputs (append `(("inotify-tools" ,inotify-tools))
                    (alist-delete "gtk+" (package-inputs emacs))))
    (arguments (append '(#:configure-flags '("--with-x-toolkit=no"))
                       (package-arguments emacs)))))

(define-public guile-emacs
  (package (inherit emacs)
    (name "guile-emacs")
    (version "20150512.41120e0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.hcoop.net/git/bpt/emacs.git")
                    (commit "41120e0f595b16387eebfbf731fff70481de1b4b")))
              (file-name (string-append name "-" version "-checkout"))
              (patches (search-patches "guile-emacs-fix-configure.patch"))
              (sha256
               (base32
                "0lvcvsz0f4mawj04db35p1dvkffdqkz8pkhc0jzh9j9x2i63kcz6"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("guile" ,guile-for-guile-emacs)
       ,@(package-native-inputs emacs)))
    (arguments
     (substitute-keyword-arguments `(;; Build fails if we allow parallel build.
                                     #:parallel-build? #f
                                     ;; Tests aren't passing for now.
                                     #:tests? #f
                                     ,@(package-arguments emacs))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'autogen
                      (lambda _
                        (zero? (system* "sh" "autogen.sh"))))
           ;; Build sometimes fails: deps/dispnew.d: No such file or directory
           (add-before 'build 'make-deps-dir
             (lambda _
               (zero? (system* "mkdir" "-p" "src/deps"))))))))))


;;;
;;; Emacs hacking.
;;;

(define-public geiser
  (package
    (name "geiser")
    (version "0.9")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/geiser/" version
                                 "/geiser-" version ".tar.gz"))
             (sha256
              (base32
               "0phz9d8wjk4p13vqannv0003fwh8qqrp0gfzcs2hgq1mrmv1srss"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (symlink "geiser-install.el"
                      (string-append (assoc-ref outputs "out")
                                     "/share/emacs/site-lisp/"
                                     "geiser-autoloads.el"))
             #t)))))
    (inputs `(("guile" ,guile-2.0)))
    (native-inputs `(("emacs" ,emacs-minimal)))
    (home-page "https://nongnu.org/geiser/")
    (synopsis "Collection of Emacs modes for Guile and Racket hacking")
    (description
     "Geiser is a collection of Emacs major and minor modes that conspire with
one or more Scheme implementations to keep the Lisp Machine Spirit alive.  The
continuously running Scheme interpreter takes the center of the stage in
Geiser.  A bundle of Elisp shims orchestrates the dialog between the Scheme
implementation, Emacs and, ultimately, the schemer, giving them access to live
metadata.")
    (license license:bsd-3)))

(define-public geiser-next
  ;; This has become "geiser".
  (deprecated-package "geiser-next" geiser))

(define-public paredit
  (package
    (name "emacs-paredit")
    (version "24")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://mumble.net/~campbell/emacs/paredit-"
                                  version ".el"))
              (sha256
               (base32
                "0pp3n8q6kc70blqsaw0zlzp6bc327dpgdrjr0cnh7hqg1lras7ka"))))
    (build-system emacs-build-system)
    (home-page "http://mumble.net/~campbell/emacs/paredit/")
    (synopsis "Emacs minor mode for editing parentheses")
    (description
     "ParEdit (paredit.el) is a minor mode for performing structured editing
of S-expression data.  The typical example of this would be Lisp or Scheme
source code.

ParEdit helps **keep parentheses balanced** and adds many keys for moving
S-expressions and moving around in S-expressions.  Its behavior can be jarring
for those who may want transient periods of unbalanced parentheses, such as
when typing parentheses directly or commenting out code line by line.")
    (license license:gpl3+)))

(define-public paredit/old-name
  (deprecated-package "paredit" paredit))

(define-public git-modes
  (package
    (name "emacs-git-modes")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/magit/git-modes/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18z04wn5ird9l0h6n6x97v0kyzdj73832bj9qakm3fjjl7vcn0pw"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/magit/git-modes")
    (synopsis "Emacs major modes for Git configuration files")
    (description
     "This package provides Emacs major modes for editing various Git
configuration files, such as .gitattributes, .gitignore, and .git/config.")
    (license license:gpl3+)))

(define-public git-modes/old-name
  (deprecated-package "git-modes" git-modes))

(define-public emacs-with-editor
  (package
    (name "emacs-with-editor")
    (version "2.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/magit/with-editor/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kah7pv211zx9fsb5g4hd51bqcq2bxd1chdykd488ihvfz1l5y14"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)))
    (home-page "https://github.com/magit/with-editor")
    (synopsis "Emacs library for using Emacsclient as EDITOR")
    (description
     "This package provides an Emacs library to use the Emacsclient as
@code{$EDITOR} of child processes, making sure they know how to call home.
For remote processes a substitute is provided, which communicates with Emacs
on stdout instead of using a socket as the Emacsclient does.")
    (license license:gpl3+)))

(define-public magit
  (package
    (name "magit")
    (version "2.11.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/magit/magit/releases/download/"
                   version "/" name "-" version ".tar.gz"))
             (sha256
              (base32
               "11xly5bma9jc1jhs8fqbqrci8kz1y26yfq7dqjkqfy956wvfg6hz"))))
    (build-system gnu-build-system)
    (native-inputs `(("texinfo" ,texinfo)
                     ("emacs" ,emacs-minimal)))
    (inputs
     `(("git" ,git)
       ("perl" ,perl)))
    (propagated-inputs
     `(("dash" ,emacs-dash)
       ;; XXX Add 'magit-popup' dependency for the next release (after 2.11.0).
       ("with-editor" ,emacs-with-editor)))
    (arguments
     `(#:test-target "test"
       #:tests? #f               ; tests are not included in the release

       #:make-flags
       (list (string-append "PREFIX=" %output)
             ;; Don't put .el files in a sub-directory.
             (string-append "lispdir=" %output "/share/emacs/site-lisp")
             (string-append "DASH_DIR="
                            (assoc-ref %build-inputs "dash")
                            "/share/emacs/site-lisp/guix.d/dash-"
                            ,(package-version emacs-dash))
             (string-append "WITH_EDITOR_DIR="
                            (assoc-ref %build-inputs "with-editor")
                            "/share/emacs/site-lisp/guix.d/with-editor-"
                            ,(package-version emacs-with-editor)))

       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before
          'build 'patch-exec-paths
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((perl (assoc-ref inputs "perl")))
              (substitute* "lisp/magit-sequence.el"
                (("perl") (string-append perl "/bin/perl")))
              #t))))))
    (home-page "http://magit.github.io/")
    (synopsis "Emacs interface for the Git version control system")
    (description
     "With Magit, you can inspect and modify your Git repositories with Emacs.
You can review and commit the changes you have made to the tracked files, for
example, and you can browse the history of past changes.  There is support for
cherry picking, reverting, merging, rebasing, and other common Git
operations.")
    (license license:gpl3+)))

(define-public magit-svn
  (package
    (name "magit-svn")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/magit/magit-svn/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04y88j7q9h8xjbx5dbick6n5nr1522sn9i1znp0qwk3vjb4b5mzz"))))
    (build-system trivial-build-system)
    (native-inputs `(("emacs" ,emacs-minimal)
                     ("tar" ,tar)
                     ("gzip" ,gzip)))
    (propagated-inputs `(("dash" ,emacs-dash)
                         ("magit" ,magit)))
    (arguments
     `(#:modules ((guix build utils)
                  (guix build emacs-utils))

       #:builder
       (begin
         (use-modules (guix build utils)
                      (guix build emacs-utils))

         (let* ((tar      (string-append (assoc-ref %build-inputs "tar")
                                         "/bin/tar"))
                (PATH     (string-append (assoc-ref %build-inputs "gzip")
                                         "/bin"))
                (emacs    (string-append (assoc-ref %build-inputs "emacs")
                                         "/bin/emacs"))
                (magit    (string-append (assoc-ref %build-inputs "magit")
                                         "/share/emacs/site-lisp"))
                (dash     (string-append (assoc-ref %build-inputs "dash")
                                         "/share/emacs/site-lisp/guix.d/dash-"
                                         ,(package-version emacs-dash)))
                (source   (assoc-ref %build-inputs "source"))
                (lisp-dir (string-append %output "/share/emacs/site-lisp")))
           (setenv "PATH" PATH)
           (system* tar "xvf" source)

           (install-file (string-append ,name "-" ,version "/magit-svn.el")
                         lisp-dir)

           (with-directory-excursion lisp-dir
             (parameterize ((%emacs emacs))
               (emacs-generate-autoloads ,name lisp-dir)
               (setenv "EMACSLOADPATH"
                       (string-append ":" magit ":" dash))
               (emacs-batch-eval '(byte-compile-file "magit-svn.el"))))))))
    (home-page "https://github.com/magit/magit-svn")
    (synopsis "Git-SVN extension to Magit")
    (description
     "This package is an extension to Magit, the Git Emacs mode, providing
support for Git-SVN.")
    (license license:gpl3+)))

(define-public emacs-magit-popup
  (package
    (name "emacs-magit-popup")
    (version "2.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/magit/magit-popup/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dnk611f7lww6rb03hk8ijg2jwxx9f26pjfff4bwjmnjz7hnd6vz"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-info
           (lambda _
             (zero? (system* "make" "info")))))))
    (native-inputs
     `(("texinfo" ,texinfo)))
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)))
    (home-page "https://github.com/magit/magit-popup")
    (synopsis "Define prefix-infix-suffix command combos")
    (description
     "This library implements a generic interface for toggling switches and
setting options and then invoking an Emacs command which does something with
these arguments.  The prototypical use is for the command to call an external
process, passing on the arguments as command line arguments.")
    (license license:gpl3+)))

(define-public haskell-mode
  (package
    (name "haskell-mode")
    (version "16.1")
    (source (origin
              (method url-fetch)
              (file-name (string-append name "-" version ".tar.gz"))
              (uri (string-append
                    "https://github.com/haskell/haskell-mode/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32 "0g6lcjw7lcgavv3yrd8xjcyqgfyjl787y32r1z14amw2f009m78h"))))
    (inputs
     `(("emacs-el-search" ,emacs-el-search) ; for tests
       ("emacs-stream" ,emacs-stream)))     ; for tests
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)))
    (native-inputs
     `(("emacs" ,emacs-minimal)
       ("texinfo" ,texinfo)))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "EMACS="
                                         (assoc-ref %build-inputs "emacs")
                                         "/bin/emacs"))
       #:modules ((ice-9 match)
                  (srfi srfi-26)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before
          'build 'pre-build
          (lambda* (#:key inputs #:allow-other-keys)
            (define (el-dir store-dir)
              (match (find-files store-dir "\\.el$")
                ((f1 f2 ...) (dirname f1))
                (_ "")))

            (let ((sh (string-append (assoc-ref inputs "bash") "/bin/sh")))
              (define emacs-prefix? (cut string-prefix? "emacs-" <>))

              (setenv "SHELL" "sh")
              (setenv "EMACSLOADPATH"
                      (string-concatenate
                       (map (match-lambda
                              (((? emacs-prefix? name) . dir)
                               (string-append (el-dir dir) ":"))
                              (_ ""))
                            inputs)))
              (substitute* (find-files "." "\\.el") (("/bin/sh") sh))
              (substitute* "tests/haskell-code-conventions.el"
                ;; Function name recently changed in "emacs-el-search".
                (("el-search--search-pattern") "el-search-forward")
                ;; Don't contact home.
                (("\\(when \\(>= emacs-major-version 25\\)")
                 "(require 'el-search) (when nil"))
              #t)))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (el-dir (string-append out "/share/emacs/site-lisp"))
                   (doc (string-append
                         out "/share/doc/haskell-mode-" ,version))
                   (info (string-append out "/share/info")))
              (define (copy-to-dir dir files)
                (for-each (lambda (f)
                            (install-file f dir))
                          files))

              (with-directory-excursion "doc"
                (unless (zero? (system* "makeinfo" "haskell-mode.texi"))
                  (error "makeinfo failed"))
                (install-file "haskell-mode.info" info))
               (copy-to-dir doc '("CONTRIBUTING.md" "NEWS" "README.md"))
               (copy-to-dir el-dir (find-files "." "\\.elc?"))
               ;; These are part of other packages.
               (with-directory-excursion el-dir
                 (for-each delete-file '("dash.el" "ert.el")))
               #t))))))
    (home-page "https://github.com/haskell/haskell-mode")
    (synopsis "Haskell mode for Emacs")
    (description
     "This is an Emacs mode for editing, debugging and developing Haskell
programs.")
    (license license:gpl3+)))

(define-public flycheck
  (package
    (name "emacs-flycheck")
    (version "30")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/flycheck/flycheck/releases/download/"
                    version "/flycheck-" version ".tar"))
              (sha256
               (base32
                "1rxzkaqsj48z3nska5wsgwafvwkam014dzqd32baycmxjl0jxvy7"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)))
    (home-page "https://www.flycheck.org")
    (synopsis "On-the-fly syntax checking")
    (description
     "This package provides on-the-fly syntax checking for GNU Emacs.  It is a
replacement for the older Flymake extension which is part of GNU Emacs, with
many improvements and additional features.

Flycheck provides fully-automatic, fail-safe, on-the-fly background syntax
checking for over 30 programming and markup languages with more than 70
different tools.  It highlights errors and warnings inline in the buffer, and
provides an optional IDE-like error list.")
    (license license:gpl3+)))                     ;+GFDLv1.3+ for the manual


;;;
;;; Web browsing.
;;;

(define-public emacs-w3m
  (package
    (name "emacs-w3m")
    (version "1.4.538+0.20141022")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://debian/pool/main/w/w3m-el/w3m-el_"
                                 version ".orig.tar.gz"))
             (sha256
              (base32
               "0zfxmq86pwk64yv0426gnjrvhjrgrjqn08sdcdhmmjmfpmqvm79y"))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,autoconf)
                     ("emacs" ,emacs-minimal)))
    (inputs `(("w3m" ,w3m)
              ("imagemagick" ,imagemagick)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-utils))
       #:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "--with-lispdir="
                              out "/share/emacs/site-lisp")
               (string-append "--with-icondir="
                              out "/share/images/emacs-w3m")
               ;; Leave .el files uncompressed, otherwise GC can't
               ;; identify run-time dependencies.  See
               ;; <http://lists.gnu.org/archive/html/guix-devel/2015-12/msg00208.html>
               "--without-compress-install"))
       #:tests? #f  ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoconf
           (lambda _
             (zero? (system* "autoconf"))))
         (add-before 'build 'patch-exec-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (w3m (assoc-ref inputs "w3m"))
                   (imagemagick (assoc-ref inputs "imagemagick"))
                   (coreutils (assoc-ref inputs "coreutils")))
               (emacs-substitute-variables "w3m.el"
                 ("w3m-command" (string-append w3m "/bin/w3m"))
                 ("w3m-touch-command"
                  (string-append coreutils "/bin/touch"))
                 ("w3m-image-viewer"
                  (string-append imagemagick "/bin/display"))
                 ("w3m-icon-directory"
                  (string-append out "/share/images/emacs-w3m")))
               (emacs-substitute-variables "w3m-image.el"
                 ("w3m-imagick-convert-program"
                  (string-append imagemagick "/bin/convert"))
                 ("w3m-imagick-identify-program"
                  (string-append imagemagick "/bin/identify")))
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (and (zero? (system* "make" "install" "install-icons"))
                  (with-directory-excursion
                      (string-append (assoc-ref outputs "out")
                                     "/share/emacs/site-lisp")
                    (for-each delete-file '("ChangeLog" "ChangeLog.1"))
                    (symlink "w3m-load.el" "w3m-autoloads.el")
                    #t)))))))
    (home-page "http://emacs-w3m.namazu.org/")
    (synopsis "Simple Web browser for Emacs based on w3m")
    (description
     "Emacs-w3m is an emacs interface for the w3m web browser.")
    (license license:gpl2+)))

(define-public emacs-wget
  (package
    (name "emacs-wget")
    (version "0.5.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://debian/pool/main/w/wget-el/wget-el_"
                                 version ".orig.tar.gz"))
             (sha256
              (base32 "10byvyv9dk0ib55gfqm7bcpxmx2qbih1jd03gmihrppr2mn52nff"))))
    (build-system gnu-build-system)
    (inputs `(("wget" ,wget)))
    (native-inputs `(("emacs" ,emacs-minimal)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-utils))
       #:tests? #f  ; no check target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr/local") (assoc-ref outputs "out"))
               (("/site-lisp/emacs-wget") "/site-lisp"))
             #t))
         (add-before 'build 'patch-exec-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((wget (assoc-ref inputs "wget")))
               (emacs-substitute-variables "wget.el"
                 ("wget-command" (string-append wget "/bin/wget"))))
             #t))
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (emacs-generate-autoloads
              "wget" (string-append (assoc-ref outputs "out")
                                    "/share/emacs/site-lisp/"))
             #t)))))
    (home-page "http://www.emacswiki.org/emacs/EmacsWget")
    (synopsis "Simple file downloader for Emacs based on wget")
    (description
     "Emacs-wget is an emacs interface for the wget file downloader.")
    (license license:gpl2+)))


;;;
;;; Multimedia.
;;;

(define-public emms
  (package
    (name "emacs-emms")
    (version "4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/emms/emms-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1b0kalhn81dir26jgsma30i5bbly7d3s1ngqpf01zjjpr7lw5v0h"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "Makefile"
                  (("/usr/bin/install-info")
                   ;; No need to use 'install-info' since it would create a
                   ;; useless 'dir' file.
                   "true")
                  (("^INFODIR=.*")
                   ;; Install Info files to $out/share/info, not $out/info.
                   "INFODIR := $(PREFIX)/share/info\n")
                  (("/site-lisp/emms")
                   ;; Install directly in share/emacs/site-lisp, not in a
                   ;; sub-directory.
                   "/site-lisp")
                  (("^all: (.*)\n" _ rest)
                   ;; Build 'emms-print-metadata'.
                   (string-append "all: " rest " emms-print-metadata\n"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-utils))

       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out     (assoc-ref outputs "out"))
                   (flac    (assoc-ref inputs "flac"))
                   (vorbis  (assoc-ref inputs "vorbis-tools"))
                   (alsa    (assoc-ref inputs "alsa-utils"))
                   (mpg321  (assoc-ref inputs "mpg321"))
                   (mp3info (assoc-ref inputs "mp3info")))
               ;; Specify the installation directory.
               (substitute* "Makefile"
                 (("PREFIX=.*$")
                  (string-append "PREFIX := " out "\n")))

               (setenv "SHELL" (which "sh"))
               (setenv "CC" "gcc")

               ;; Specify the absolute file names of the various
               ;; programs so that everything works out-of-the-box.
               (with-directory-excursion "lisp"
                 (emacs-substitute-variables
                     "emms-player-mpg321-remote.el"
                   ("emms-player-mpg321-remote-command"
                    (string-append mpg321 "/bin/mpg321")))
                 (substitute* "emms-player-simple.el"
                   (("\"ogg123\"")
                    (string-append "\"" vorbis "/bin/ogg123\"")))
                 (substitute* "emms-player-simple.el"
                   (("\"mpg321\"")
                    (string-append "\"" mpg321 "/bin/mpg321\"")))
                 (emacs-substitute-variables "emms-info-ogginfo.el"
                   ("emms-info-ogginfo-program-name"
                    (string-append vorbis "/bin/ogginfo")))
                 (emacs-substitute-variables "emms-info-libtag.el"
                   ("emms-info-libtag-program-name"
                    (string-append out "/bin/emms-print-metadata")))
                 (emacs-substitute-variables "emms-info-mp3info.el"
                   ("emms-info-mp3info-program-name"
                    (string-append mp3info "/bin/mp3info")))
                 (emacs-substitute-variables "emms-info-metaflac.el"
                   ("emms-info-metaflac-program-name"
                    (string-append flac "/bin/metaflac")))
                 (emacs-substitute-variables "emms-source-file.el"
                   ("emms-source-file-gnu-find" (which "find")))
                 (substitute* "emms-volume-amixer.el"
                   (("\"amixer\"")
                    (string-append "\"" alsa "/bin/amixer\"")))
                 (substitute* "emms-tag-editor.el"
                   (("\"mp3info\"")
                    (string-append "\"" mp3info "/bin/mp3info\"")))))))
         (add-before 'install 'pre-install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The 'install' rule expects the target directories to exist.
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1")))
               (mkdir-p bin)
               (mkdir-p man1)
               #t)))
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (symlink "emms-auto.el"
                        (string-append out "/share/emacs/site-lisp/"
                                       "emms-autoloads.el"))))))
       #:tests? #f))
    (native-inputs `(("emacs" ,emacs-minimal)    ;for (guix build emacs-utils)
                     ("texinfo" ,texinfo)))
    (inputs `(("alsa-utils" ,alsa-utils)
              ("flac" ,flac)            ;for metaflac
              ("vorbis-tools" ,vorbis-tools)
              ("mpg321" ,mpg321)
              ("taglib" ,taglib)
              ("mp3info" ,mp3info)))
    (properties '((upstream-name . "emms")))
    (synopsis "Emacs Multimedia System")
    (description
     "EMMS is the Emacs Multimedia System.  It is a small front-end which
can control one of the supported external players.  Thus, it supports
whatever formats are supported by your music player.  It also
supports tagging and playlist management, all behind a clean and
light user interface.")
    (home-page "https://www.gnu.org/software/emms/")
    (license license:gpl3+)))

(define-public emacs-emms-player-mpv
  (package
    (name "emacs-emms-player-mpv")
    (version "0.0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/dochang/emms-player-mpv/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01i0bz7wdqzcnv5r63m9xgl07yf1nmn86dwy00rcfsn5za46y3r3"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emms" ,emms)))
    (home-page "https://github.com/dochang/emms-player-mpv/")
    (synopsis "Mpv support for EMMS")
    (description
     "This package provides an EMMS player that uses mpv.  It supports pause
and seeking.")
    (license license:gpl3+)))

(define-public emacs-emms-mode-line-cycle
  (package
    (name "emacs-emms-mode-line-cycle")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/momomo5717/emms-mode-line-cycle"
                           "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ifszi930pnaxk1x8pcydmvnp06868gc7nfx14q17zbajbx735k6"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emms" ,emms)))
    (home-page "https://github.com/momomo5717/emms-mode-line-cycle")
    (synopsis "Display the EMMS mode line as a ticker")
    (description
     "This is a minor mode for updating the EMMS mode-line string cyclically
within a specified width.  It is useful for displaying long track titles.")
    (license license:gpl3+)))


;;;
;;; Miscellaneous.
;;;

(define-public bbdb
  (package
    (name "bbdb")
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/bbdb/bbdb-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1gs16bbpiiy01w9pyg12868r57kx1v3hnw04gmqsmpc40l1hyy05"))
              (modules '((guix build utils)))
              (snippet
               ;; We don't want to build and install the PDF.
               '(substitute* "doc/Makefile.in"
                  (("^doc_DATA = .*$")
                   "doc_DATA =\n")))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Add an autoloads file with the right name for guix.el.
             (let* ((out  (assoc-ref outputs "out"))
                    (site (string-append out "/share/emacs/site-lisp")))
               (with-directory-excursion site
                 (symlink "bbdb-loaddefs.el" "bbdb-autoloads.el")))
             #t)))))
    (native-inputs `(("emacs" ,emacs-minimal)))
    (home-page "https://savannah.nongnu.org/projects/bbdb/")
    (synopsis "Contact management utility for Emacs")
    (description
     "BBDB is the Insidious Big Brother Database for GNU Emacs.  It provides
an address book for email and snail mail addresses, phone numbers and the
like.  It can be linked with various Emacs mail clients (Message and Mail
mode, Rmail, Gnus, MH-E, and VM).  BBDB is fully customizable.")
    (license license:gpl3+)))

(define-public emacs-aggressive-indent
  (package
    (name "emacs-aggressive-indent")
    (version "1.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/"
                                  "aggressive-indent-" version ".el"))
              (sha256
               (base32
                "0jnzccl50x0wapprgwxinp99pwwa6j43q6msn4gv437j7swy8wnj"))))
    (build-system emacs-build-system)
    (home-page "https://elpa.gnu.org/packages/aggressive-indent.html")
    (synopsis "Minor mode to aggressively keep your code always indented")
    (description
     "@code{aggressive-indent-mode} is a minor mode that keeps your code
always indented.  It reindents after every change, making it more reliable
than @code{electric-indent-mode}.")
    (license license:gpl2+)))

(define-public emacs-ag
  (package
    (name "emacs-ag")
    (version "0.47")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Wilfred/ag.el/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rlmp6wnyhqfg86dbz17r914msp58favn4kd4yrdwyia265a4lar"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-info
           (lambda _
             (with-directory-excursion "docs"
               (zero? (system* "make" "info")))))
         (add-after 'install 'install-info
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (info (string-append out "/share/info")))
               (install-file "docs/_build/texinfo/agel.info" info)
               #t))))))
    (native-inputs
     `(("python-sphinx" ,python-sphinx)
       ("texinfo" ,texinfo)))
    (propagated-inputs
     `(("dash" ,emacs-dash)
       ("s" ,emacs-s)
       ;; We need to use 'ag' as the executable on remote systems.
       ("the-silver-searcher" ,the-silver-searcher)))
    (home-page "https://github.com/Wilfred/ag.el")
    (synopsis "Front-end for ag (the-silver-searcher) for Emacs")
    (description "This package provides the ability to use the silver
searcher, a code searching tool, sometimes abbreviated to @code{ag}.  Features
include version control system awareness, use of Perl compatible regular
expressions, editing the search results directly and searching file names
rather than the contents of files.")
    (license license:gpl3+)))

(define-public emacs-async
  (package
    (name "emacs-async")
    (version "1.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/async-"
                                  version ".tar"))
              (sha256
               (base32
                "17fnvrj7jww29sav6a6jpizclg4w2962m6h37akpii71gf0vrffw"))))
    (build-system emacs-build-system)
    (home-page "https://elpa.gnu.org/packages/async.html")
    (synopsis "Asynchronous processing in Emacs")
    (description
     "This package provides the ability to call asynchronous functions and
processes.  For example, it can be used to run dired commands (for copying,
moving, etc.) asynchronously using @code{dired-async-mode}.  Also it is used
as a library for other Emacs packages.")
    (license license:gpl3+)))

(define-public emacs-auctex
  (package
    (name "emacs-auctex")
    (version "12.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://elpa.gnu.org/packages/auctex-"
             version
             ".tar"))
       (sha256
        (base32
         "0iy5x61xqkxaph2hq64sg50l1c6yp6qhzppwadayxkdz00b46sas"))))
    (build-system emacs-build-system)
    ;; We use 'emacs' because AUCTeX requires dbus at compile time
    ;; ('emacs-minimal' does not provide dbus).
    (arguments
     `(#:emacs ,emacs
       #:include '("\\.el$" "^images/" "^latex/" "\\.info$")
       #:exclude '("^tests/" "^latex/README")))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "https://www.gnu.org/software/auctex/")
    (synopsis "Integrated environment for TeX")
    (description
     "AUCTeX is a comprehensive customizable integrated environment for
writing input files for TeX, LaTeX, ConTeXt, Texinfo, and docTeX using Emacs
or XEmacs.")
    (license license:gpl3+)))

(define-public emacs-autothemer
  (package
    (name "emacs-autothemer")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/sebastiansturm/autothemer/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0rd28r9wfrbll212am4ih9hrvypx785aff76va2cbfxdwm9kixsa"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)))
    (home-page "https://github.com/sebastiansturm/autothemer")
    (synopsis "Conveniently create Emacs themes")
    (description
     "Autothemer provides a thin layer on top of @code{deftheme} and
@code{custom-theme-set-faces} that creates a new custom color theme, based on
a set of simplified face specifications and a user-supplied color palette")
    (license license:gpl3+)))

(define-public emacs-calfw
  (package
    (name "emacs-calfw")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/kiwanami/emacs-calfw/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17ssg8gx66yp63nhygjq2r6kgl4h45cacmrxsxs9f0lrfcx37k0l"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/kiwanami/emacs-calfw/")
    (synopsis "Calendar framework for Emacs")
    (description
     "This package displays a calendar view with various schedule data in the
Emacs buffer.")
    (license license:gpl3+)))

(define-public emacs-direnv
  (package
    (name "emacs-direnv")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/wbolster/emacs-direnv/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0m9nxawklhiiysyibzzhh2zkxgq1fskqvaqb06f7r8dnhabfy9fr"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("dash" ,emacs-dash)
       ("with-editor" ,emacs-with-editor)))
    (home-page "https://github.com/wbolster/emacs-direnv")
    (synopsis "Direnv integration for Emacs")
    (description
     "This package provides support for invoking direnv to get the environment
for the current file and updating the environment within Emacs to match.

Direnv can be invoked manually, and a global minor mode is included that will
update the environment when the active buffer changes.

Using emacs-direnv means that programs started from Emacs will use the
environment set through Direnv.")
    (license license:gpl3+)))

(define-public emacs-ggtags
  (package
    (name "emacs-ggtags")
    (version "0.8.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://elpa.gnu.org/packages/ggtags-"
                           version ".el"))
       (sha256
        (base32
         "0ny3llk021g6r0s75xdm4hzpbxv393ddm2r6f2xdk8kqnq4gnirp"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/leoliu/ggtags")
    (synopsis "Frontend to the GNU Global source code tagging system")
    (description "@code{ggtags} provides a frontend to the GNU Global source
code tagging system.

Features:

@itemize
@item Build on @code{compile.el} for asynchronicity and its large feature-set.
@item Automatically update Global's tag files when needed with tuning for
large source trees.
@item Intuitive navigation among multiple matches with mode-line display of
current match, total matches and exit status.
@item Read tag with completion.
@item Show definition at point.
@item Jump to #include files.
@item Support search history and saving a search to register/bookmark.
@item Query replace.
@item Manage Global's environment variables on a per-project basis.
@item Highlight (definition) tag at point.
@item Abbreviated display of file names.
@item Support all Global search backends: @code{grep}, @code{idutils}, etc.
@item Support exuberant ctags @url{http://ctags.sourceforge.net/} and
@code{pygments} backend.
@item Support all Global's output formats: @code{grep}, @code{ctags-x},
@code{cscope} etc.
@item Support projects on remote hosts (e.g. via @code{tramp}).
@item Support eldoc.
@item Search @code{GTAGSLIBPATH} for references and symbols.
@end itemize\n")
    (license license:gpl3+)))

(define-public emacs-go-mode
  (package
    (name "emacs-go-mode")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/dominikh/go-mode.el/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1adngbjyb8qnwg7n6r2y31djw9j6qf3b9fi63zd85035q7x4ljnm"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/dominikh/go-mode.el")
    (synopsis "Go mode for Emacs")
    (description
     "This package provides go-mode, an Emacs mode for working with software
written in the Go programming language.")
    (license license:bsd-3)))

(define-public emacs-google-maps
  (package
    (name "emacs-google-maps")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jd/google-maps.el/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "014bxapm4d8vjxbzrfjdpsavxyfx981mlcb10aq5rmigr6il8ybs"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/jd/google-maps.el")
    (synopsis "Access Google Maps from Emacs")
    (description "The @code{google-maps} package allows to display Google
Maps directly inside Emacs.")
    (license license:gpl3+)))

(define-public emacs-graphviz-dot-mode
  (let ((commit "fdaabbcc95d9156e3dadc84f81a4750c5b692580")
        (revision "1"))
    (package
      (name "emacs-graphviz-dot-mode")
      (version (string-append "0.3.10-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ppareit/graphviz-dot-mode.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1s1qh5r0xp6hs0rl5yz5mkmjhpg04bh449c7vgjbb1pjsl1dl714"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'make-info
             (lambda* (#:key inputs #:allow-other-keys)
               (with-directory-excursion "texinfo"
                 (substitute* "Makefile"
                   (("\\/usr\\/bin\\/gzip")
                    (string-append (assoc-ref inputs "gzip") "/bin/gzip")))
                 (zero?
                  (system* "make"
                           "clean"
                           "info"
                           (string-append "TEXINFODIR="
                                          (assoc-ref inputs "texinfo")
                                          "/bin"))))))
           (add-after 'install 'install-info
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out  (assoc-ref outputs "out"))
                      (info (string-append out "/share/info")))
                 (install-file "texinfo/graphviz-dot-mode.info.gz" info)
                 #t))))))
      (native-inputs
       `(("texinfo" ,texinfo)
         ("gzip" ,gzip)))
      (home-page "http://ppareit.github.com/graphviz-dot-mode")
      (synopsis "Major mode for editing Graphviz Dot files")
      (description
       "This Emacs packages helps you to create @file{.dot} or @file{.gv}
files using the dot syntax, and use Graphviz to convert these files to
diagrams.")
      (license license:gpl2+))))

(define-public emacs-mmm-mode
  (package
    (name "emacs-mmm-mode")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/purcell/mmm-mode/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "10kwslnflbjqm62wkrq420crqzdqalzfflp9pqk1i12zm6dm4mfv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _
             (zero? (system* "sh" "autogen.sh")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("emacs" ,emacs-minimal)
       ("texinfo" ,texinfo)))
    (home-page "https://github.com/purcell/mmm-mode")
    (synopsis "Allow multiple major modes in an Emacs buffer")
    (description
     "MMM Mode is a minor mode that allows multiple major modes to coexist in a
single buffer.")
    (license license:gpl3+)))

(define-public emacs-tablist
  (package
    (name "emacs-tablist")
    (version "0.70")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/politza/tablist/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "177d6s7ym1mwz1nhnl09r14z3n093g9a2szm97xsaig0c204xz9c"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/politza/tablist")
    (synopsis "Extension for @code{tabulated-list-mode}")
    (description "Tablist is the Emacs package that provides several
additional features to @code{tabulated-list-mode}: it adds marks,
filters, new key bindings and faces.  It can be enabled by
@code{tablist-mode} or @code{tablist-minor-mode} commands.")
    (license license:gpl3+)))

(define-public emacs-pdf-tools
  (package
    (name "emacs-pdf-tools")
    (version "0.80")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/politza/pdf-tools/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06imydn3a92vr57azpn1zhqc14kxyyslmyi9ldsyphan9b724gb6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:modules ((guix build gnu-build-system)
                  ((guix build emacs-build-system) #:prefix emacs:)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-build-system)
                           (guix build emacs-utils))
       #:phases
       (modify-phases %standard-phases
         ;; Build server side using 'gnu-build-system'.
         (add-after 'unpack 'enter-server-dir
           (lambda _ (chdir "server") #t))
         (add-after 'enter-server-dir 'autogen
           (lambda _
             (zero? (system* "bash" "autogen.sh"))))

         ;; Build emacs side using 'emacs-build-system'.
         (add-after 'compress-documentation 'enter-lisp-dir
           (lambda _ (chdir "../lisp") #t))
         (add-after 'enter-lisp-dir 'emacs-patch-variables
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Set path to epdfinfo program.
             (emacs-substitute-variables "pdf-info.el"
               ("pdf-info-epdfinfo-program"
                (string-append (assoc-ref outputs "out")
                               "/bin/epdfinfo")))
             ;; Set 'pdf-tools-handle-upgrades' to nil to avoid "auto
             ;; upgrading" that pdf-tools tries to perform.
             (emacs-substitute-variables "pdf-tools.el"
               ("pdf-tools-handle-upgrades" '()))))
         (add-after 'emacs-patch-variables 'emacs-install
           (assoc-ref emacs:%standard-phases 'install))
         (add-after 'emacs-install 'emacs-build
           (assoc-ref emacs:%standard-phases 'build))
         (add-after 'emacs-install 'emacs-make-autoloads
           (assoc-ref emacs:%standard-phases 'make-autoloads)))))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("pkg-config" ,pkg-config)
                     ("emacs" ,emacs-minimal)))
    (inputs `(("poppler" ,poppler)
              ("cairo" ,cairo)
              ("glib" ,glib)
              ("libpng" ,libpng)
              ("zlib" ,zlib)))
    (propagated-inputs `(("tablist" ,emacs-tablist)))
    (synopsis "Emacs support library for PDF files")
    (description
     "PDF Tools is, among other things, a replacement of DocView for PDF
files.  The key difference is that pages are not pre-rendered by
e.g. ghostscript and stored in the file-system, but rather created on-demand
and stored in memory.")
    (home-page "https://github.com/politza/pdf-tools")
    (license license:gpl3+)))

(define-public emacs-dash
  (package
    (name "emacs-dash")
    (version "2.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/magnars/dash.el/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pjlkrzr8n45bnp3xs3dybvy0nz3gwamrfc7vsi1nhpkkw99ihhb"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
                     (lambda _
                       (zero? (system* "./run-tests.sh")))))))
    (home-page "https://github.com/magnars/dash.el")
    (synopsis "Modern list library for Emacs")
    (description "This package provides a modern list API library for Emacs.")
    (license license:gpl3+)))

(define-public emacs-bui
  (package
    (name "emacs-bui")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/alezost/bui.el/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "112k0mq6xpy0r47vk66miw7rxbkv3d06pv3pd0vcmrhcnhnnk486"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("dash" ,emacs-dash)))
    (home-page "https://github.com/alezost/bui.el")
    (synopsis "Buffer interface library for Emacs")
    (description
     "BUI (Buffer User Interface) is a library for making @code{list} and
@code{info} interfaces to display an arbitrary data of the same
type, for example: packages, buffers, files, etc.")
    (license license:gpl3+)))

(define-public emacs-guix
  (package
    (name "emacs-guix")
    (version "0.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alezost/guix.el"
                                  "/releases/download/v" version
                                  "/emacs-guix-" version ".tar.gz"))
              (sha256
               (base32
                "152zf9vkafmnnf7by5armg165npznb961csgnvr8iwdj3smvivjf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (let ((guix        (assoc-ref %build-inputs "guix"))
             (geiser      (assoc-ref %build-inputs "geiser"))
             (dash        (assoc-ref %build-inputs "dash"))
             (bui         (assoc-ref %build-inputs "bui"))
             (magit-popup (assoc-ref %build-inputs "magit-popup"))
             (site-lisp   "/share/emacs/site-lisp"))
         (list (string-append "--with-guix-site-dir="
                              (car (find-files (string-append guix
                                                           "/share/guile/site")
                                               (lambda (file stat)
                                                 (string-prefix?
                                                  "2."
                                                  (basename file)))
                                               #:directories? #t)))
               (string-append "--with-guix-site-ccache-dir="
                              (car (find-files (string-append guix "/lib/guile")
                                               (lambda (file stat)
                                                 (string-prefix?
                                                  "2." (basename file)))
                                               #:directories? #t))
                              "/site-ccache")
               (string-append "--with-geiser-lispdir=" geiser site-lisp)
               (string-append "--with-dash-lispdir="
                              dash site-lisp "/guix.d/dash-"
                              ,(package-version emacs-dash))
               (string-append "--with-bui-lispdir="
                              bui site-lisp "/guix.d/bui-"
                              ,(package-version emacs-bui))
               (string-append "--with-popup-lispdir="
                              magit-popup site-lisp "/guix.d/magit-popup-"
                              ,(package-version emacs-magit-popup))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("emacs" ,emacs-minimal)))
    (inputs
     `(("guile" ,guile-2.2)
       ("guix" ,guix)))
    (propagated-inputs
     `(("geiser" ,geiser)
       ("dash" ,emacs-dash)
       ("bui" ,emacs-bui)
       ("magit-popup" ,emacs-magit-popup)))
    (home-page "https://alezost.github.io/guix.el/")
    (synopsis "Emacs interface for GNU Guix")
    (description
     "Emacs-Guix provides a visual interface, tools and features for the GNU
Guix package manager.  Particularly, it allows you to do various package
management tasks from Emacs.  To begin with, run @code{M-x guix-about} or
@code{M-x guix-help} command.")
    (license license:gpl3+)))

(define-public emacs-d-mode
  (package
    (name "emacs-d-mode")
    (version "2.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/"
                    "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0knpgi55jm09282aqf8pv55zillpnpzf9f4sgm6gwsmvxf17xaw0"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-undercover" ,emacs-undercover)))
    (home-page "https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode")
    (synopsis "Emacs major mode for editing D code")
    (description "This package provides an Emacs major mode for highlighting
code written in the D programming language.  This mode is currently known to
work with Emacs 24 and 25.")
    (license license:gpl2+)))

(define-public emacs-keyfreq
  (package
    (name "emacs-keyfreq")
    (version "20160516.716")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://melpa.org/packages/keyfreq-"
               version ".el"))
        (sha256
          (base32
            "008hd7d06qskc3mx0bbdgpgy2pwxr8185fzlyqf9qjg49y74p6g8"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/dacap/keyfreq")
    (synopsis "Track Emacs command frequencies")
    (description "@code{emacs-keyfeq} tracks and shows how many times you used
a command.")
    (license license:gpl3+)))

(define-public emacs-olivetti
  (package
    (name "emacs-olivetti")
    (version "1.5.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://stable.melpa.org/packages/olivetti-"
                    version ".el"))
              (sha256
               (base32
                "1yj2ylg46q0pw1xzlv2b0fv9x8p56x25284s9v2smwjr4vf0nwcj"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/rnkn/olivetti")
    (synopsis "Emacs minor mode for a nice writing environment")
    (description "This package provides an Emacs minor mode that puts writing
in the center.")
    (license license:gpl3+)))

(define-public emacs-undo-tree
  (package
    (name "emacs-undo-tree")
    (version "0.6.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "http://dr-qubit.org/git/undo-tree.git")
                    (commit (string-append "release/" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1hnh2mnmw179gr094r561w6cw1haid0lpvpqvkc24wpj82vphzpa"))))
    (build-system emacs-build-system)
    (home-page "http://www.dr-qubit.org/emacs.php")
    (synopsis "Treat undo history as a tree")
    (description "Tree-like interface to Emacs undo system, providing
graphical tree presentation of all previous states of buffer that
allows easily move between them.")
    (license license:gpl3+)))

(define-public emacs-s
  (package
    (name "emacs-s")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/magnars/s.el/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xbl75863pcm806zg0x1lw7qznzjq2c8320k8js7apyag8q4srvh"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
                     (lambda _
                       (zero? (system* "./run-tests.sh")))))))
    (home-page "https://github.com/magnars/s.el")
    (synopsis "Emacs string manipulation library")
    (description "This package provides an Emacs library for manipulating
strings.")
    (license license:gpl3+)))

(define-public emacs-symon
  (package
    (name "emacs-symon")
    (version "20160630")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/zk-phi/symon/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0h4jcgdnq98wc9rj72nwyazq8498yg55jfljiij5qwbn1xf1g5zz"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/zk-phi/symon")
    (synopsis "Tiny graphical system monitor")
    (description
     "Tiny graphical system monitor for the Emacs minibuffer when idle.")
    (license license:gpl2+)))

(define-public emacs-sx
  (package
    (name "emacs-sx")
    (version "0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/vermiculus/sx.el/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w0xghfljqg31axcnv8gzlrd8pw25nji6idnrhflq0af9qh1dw03"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-markdown-mode" ,emacs-markdown-mode)))
    (home-page "https://github.com/vermiculus/sx.el/")
    (synopsis "Emacs StackExchange client")
    (description
     "Emacs StackExchange client.  Ask and answer questions on
Stack Overflow, Super User, and other StackExchange sites.")
    (license license:gpl3+)))

(define-public emacs-f
  (package
    (name "emacs-f")
    (version "0.19.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rejeep/f.el/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05195n80ywa68qykxn7dza6qd59rhakvlzhaa9l6mcpmjf9l9grs"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-s" ,emacs-s)
       ("emacs-dash" ,emacs-dash)))
    (home-page "https://github.com/rejeep/f.el")
    (synopsis "Emacs API for working with files and directories")
    (description "This package provides an Emacs library for working with
files and directories.")
    (license license:gpl3+)))

(define-public emacs-git-gutter
  (package
    (name "emacs-git-gutter")
    (version "0.90")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/syohex/" name "/archive/"
                   version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "1nmhvhpq1l56mj2yq3ag23rw3x4xgnsy8szp30s26l0yjnkhc4qg"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/syohex/emacs-git-gutter")
    (synopsis "See and manage hunks of text in a version control system")
    (description
     "This package is an Emacs minor mode for displaying and interacting with
hunks of text managed in a version control system.  Added modified and deleted
areas can be indicated with symbols on the edge of the buffer, and commands
can be used to move between and perform actions on these hunks.

Git, Mercurial, Subversion and Bazaar are supported, and many parts of the
display and behaviour is easily customisable.")
    (license license:gpl3+)))

(define-public emacs-git-timemachine
  (package
    (name "emacs-git-timemachine")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pidu/git-timemachine/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1l4g0r69wfrnjsywv03v4bpdd53byg6zdx6mzabfxyymss3kvisa"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/pidu/git-timemachine")
    (synopsis "Step through historic versions of Git-controlled files")
    (description "This package enables you to step through historic versions
of files under Git version control from within Emacs.")
    (license license:gpl3+)))

(define-public emacs-minitest
  (package
    (name "emacs-minitest")
    (version "0.8.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/arthurnn/minitest-emacs/archive/v"
                   version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "1dsb7kzvs1x6g4sgqmq73jqacb7wzm0wfkiq5m9dqdzq8mppgiqs"))))
    (build-system emacs-build-system)
    (arguments
     '(#:include (cons "^snippets\\/minitest-mode\\/" %default-include)
       #:exclude (delete "^[^/]*tests?\\.el$" %default-exclude)))
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-f" ,emacs-f)))
    (home-page "https://github.com/arthurnn/minitest-emacs")
    (synopsis "Emacs minitest mode")
    (description
     "The minitest mode provides commands to run the tests for the current
file or line, as well as rerunning the previous tests, or all the tests for a
project.

This package also includes relevant snippets for yasnippet.")
    (license license:expat)))

(define-public emacs-el-mock
  (package
    (name "emacs-el-mock")
    (version "1.25.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rejeep/el-mock.el/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "16xw94n58xxn3zvgyj72bmzs0k5lkvswjmzs79ws9n7rzdivb38b"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/rejeep/el-mock.el")
    (synopsis "Tiny mock and stub framework in Emacs Lisp")
    (description
     "Emacs Lisp Mock is a library for mocking and stubbing using readable
syntax.  Most commonly Emacs Lisp Mock is used in conjunction with Emacs Lisp
Expectations, but it can be used in other contexts.")
    (license license:gpl3+)))

(define-public emacs-espuds
  (package
    (name "emacs-espuds")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ecukes/espuds/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0xv551376pbmh735a3zjwc9z4qdx6ngj1vpq3xqjpn0a1rwjyn4k"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-s" ,emacs-s)
       ("emacs-dash" ,emacs-dash)
       ("emacs-f" ,emacs-f)))
    (home-page "https://github.com/ecukes/espuds")
    (synopsis "Common step definitions for Ecukes")
    (description "Espuds is a collection of the most commonly used step
definitions for testing with the Ecukes framework.")
    (license license:gpl3+)))

(define-public emacs-es-mode
  (package
    (name "emacs-es-mode")
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/dakrone/es-mode/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02as82clm553yss7jfjac888308zr1h2229cch4z1yij70j25c8y"))))
    (build-system emacs-build-system)
    (propagated-inputs
     ;; The version of org in Emacs 24.5 is not sufficient, and causes tables
     ;; to be rendered incorrectly
     `(("emacs-org" ,emacs-org)))
    (home-page "https://github.com/dakrone/es-mode")
    (synopsis "Major mode for editing Elasticsearch queries")
    (description "@code{es-mode} includes highlighting, completion and
indentation support for Elasticsearch queries.  Also supported are
@code{es-mode} blocks in @code{org-mode}, for which the results of queries can
be processed through @code{jq}, or in the case of aggregations, can be
rendered in to a table.  In addition, there is an @code{es-command-center}
mode, which displays information about Elasticsearch clusters.")
    (license license:gpl3+)))

(define-public emacs-expand-region
  (package
    (name "emacs-expand-region")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/magnars/expand-region.el"
                           "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "08dy1f411sh9wwww53rjw80idcf3vpki6ba2arl4hl5jcw9651g0"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/magnars/expand-region.el")
    (synopsis "Increase selected region by semantic units")
    (description
     "Expand region increases the selected region by semantic units.  Just
keep pressing the key until it selects what you want.  There's also
@code{er/contract-region} if you expand too far.")
    (license license:gpl3+)))

(define-public emacs-fill-column-indicator
  (package
    (name "emacs-fill-column-indicator")
    (version "1.81")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/alpaker/Fill-Column-Indicator"
                           "/archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1xwyqbjbbicmvhlb85vg4j5snwy1vd7rfk89ws4viws5ljkhhyg8"))))
    (build-system emacs-build-system)
    (home-page "https://www.emacswiki.org/emacs/FillColumnIndicator")
    (synopsis "Graphically indicate the fill column")
    (description
     "Fill-column-indicator graphically indicates the location of the fill
column by drawing a thin line down the length of the editing window.")
    (license license:gpl3+)))

(define-public emacs-grep-a-lot
  (package
    (name "emacs-grep-a-lot")
    (version "1.0.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ZungBang/emacs-grep-a-lot.git")
                    (commit "9f9f645b9e308a0d887b66864ff97d0fca1ba4ad")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1f8262mrlinzgnn4m49hbj1hm3c1mvzza24py4b37sasn49546lw"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ZungBang/emacs-grep-a-lot")
    (synopsis "Enables multiple grep buffers in Emacs")
    (description
     "This Emacs package allows managing multiple grep buffers.")
    (license license:gpl3+)))

(define-public emacs-inf-ruby
  (package
    (name "emacs-inf-ruby")
    (version "2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/nonsequitur/inf-ruby/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0m7323k649ckxql1grsdnf71bjhys7l4qb8wbpphb1mr1q8i4066"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/nonsequitur/inf-ruby")
    (synopsis "Provides a REPL buffer connected to a Ruby subprocess in Emacs")
    (description
     "@code{inf-ruby} provides a Read Eval Print Loop (REPL) buffer, allowing
for easy interaction with a ruby subprocess.  Features include support for
detecting specific uses of Ruby, e.g. when using rails, and using a
appropriate console.")
    (license license:gpl3+)))

(define-public emacs-znc
  (package
    (name "emacs-znc")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://marmalade-repo.org/packages/znc-"
                           version ".el"))
       (sha256
        (base32
         "1d8lqvybgyazin5z0g1c4l3rg1vzrrvf0saqs53jr1zcdg0lianh"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/sshirokov/ZNC.el")
    (synopsis "Make ERC and ZNC get along better")
    (description
     "This is a thin wrapper around @code{erc} that enables one to use the ZNC
IRC bouncer with ERC.")
    (license license:expat)))

(define-public emacs-shut-up
  (package
    (name "emacs-shut-up")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cask/shut-up/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "09kzrjdkb569iviyg7ydwq44yh84m3f9hkl7jizfrlk0w4gz67d1"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/cask/shut-up")
    (synopsis "Silence Emacs")
    (description "This package silences most output of Emacs when running an
Emacs shell script.")
    (license license:expat)))

(define-public emacs-undercover
  (package
    (name "emacs-undercover")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/sviridov/undercover.el/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0f48fi0xnbsqs382rgh85m9mq1wdnr0yib7as9xhwzvq0hsr5m0a"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-shut-up" ,emacs-shut-up)))
    (home-page "https://github.com/sviridov/undercover.el")
    (synopsis "Test coverage library for Emacs Lisp")
    (description
     "Undercover is a test coverage library for software written in Emacs
Lisp.")
    (license license:expat)))

(define-public emacs-paren-face
  (package
    (name "emacs-paren-face")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/tarsius/paren-face/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0y4qrhxa9332vsvr999jg7qj1ymnfgwpf591yi4a4jgg90pm7qnn"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/tarsius/paren-face")
    (synopsis "Face for parentheses in lisp modes")
    (description
     "This library defines a face named @code{parenthesis} used just for
parentheses.  The intended purpose of this face is to make parentheses less
visible in Lisp code by dimming them.  Lispers probably don't need to be
constantly made aware of the existence of the parentheses.  Dimming them might
be even more useful for people new to lisp who have not yet learned to
subconsciously blend out the parentheses.")
    (license license:gpl3+)))

(define-public emacs-page-break-lines
  (package
    (name "emacs-page-break-lines")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/purcell/page-break-lines/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zzhziq5kbrm9rxk30kx2glz455fp1blqxg8cpcf6l8xl3w8z4pg"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/purcell/page-break-lines")
    (synopsis "Display page breaks as tidy horizontal lines")
    (description
     "This library provides a global mode which displays form feed characters
as horizontal rules.")
    (license license:gpl3+)))

(define-public emacs-simple-httpd
  (package
    (name "emacs-simple-httpd")
    (version "1.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/skeeto/emacs-web-server/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01r7h3imnj4qx1m53a2wjafvbylcyz5f9r2rg2cs7ky3chlg220r"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/skeeto/emacs-http-server")
    (synopsis "HTTP server in pure Emacs Lisp")
    (description
     "This package provides a simple HTTP server written in Emacs Lisp to
serve files and directory listings.")
    (license license:unlicense)))

(define-public emacs-skewer-mode
  (package
    (name "emacs-skewer-mode")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/skeeto/skewer-mode/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07jpz374j0j964szy3zznrkyja2kpdl3xa87wh7349mzxivqxdx0"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-simple-httpd" ,emacs-simple-httpd)
       ("emacs-js2-mode" ,emacs-js2-mode)))
    (arguments '(#:include '("\\.el$" "\\.js$" "\\.html$")))
    (home-page "https://github.com/skeeto/skewer-mode")
    (synopsis "Live web development in Emacs")
    (description
     "Skewer-mode provides live interaction with JavaScript, CSS, and HTML in
a web browser.  Expressions are sent on-the-fly from an editing buffer to be
evaluated in the browser, just like Emacs does with an inferior Lisp process
in Lisp modes.")
    (license license:unlicense)))

(define-public emacs-string-inflection
  (package
    (name "emacs-string-inflection")
    (version "1.0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/akicho8/string-inflection")
                    (commit "a150e7bdda60b7824d3a936750ce23f73b0e4edd")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1k0sm552iawi49v4zis6dbb81d1rzgky9v0dpv7nj31gnb7bmy7k"))))
    (build-system emacs-build-system)
    (native-inputs
     `(("ert-runner" ,ert-runner)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda _
             (zero? (system* "ert-runner")))))))
    (home-page "https://github.com/akicho8/string-inflection")
    (synopsis "Convert symbol names between different naming conventions")
    (description
     "This Emacs package provides convenient methods for manipulating the
naming style of a symbol.  It supports different naming conventions such as:

@enumerate
@item camel case
@item Pascal case
@item all upper case
@item lower case separated by underscore
@item etc...
@end enumerate\n")
    (license license:gpl2+)))

(define-public emacs-stripe-buffer
  (package
    (name "emacs-stripe-buffer")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/sabof/stripe-buffer/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1p515dq7raly5hw94kiwm3vzsfih0d8af622q4ipvvljsm98aiik"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/sabof/stripe-buffer/")
    (synopsis "Add stripes to list buffers")
    (description
     "This Emacs package adds faces to add stripes to list buffers and org
tables.")
    (license license:gpl2+)))

(define-public emacs-rich-minority
  (package
    (name "emacs-rich-minority")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Malabarba/rich-minority/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1l0cb0q7kyi88nwfqd542psnkgwnjklpzc5rx32gzd3lkwkrbr8v"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Malabarba/rich-minority")
    (synopsis "Clean-up and beautify the list of minor modes")
    (description
     "This Emacs package hides and/or highlights minor modes in the
mode-line.")
    (license license:gpl2+)))

(define-public emacs-robe
  (package
    (name "emacs-robe")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/dgutov/robe/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1vp45y99fwj88z04ah4yppz4z568qcib646az6m9az5ar0f203br"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-inf-ruby" ,emacs-inf-ruby)))
    (home-page "https://github.com/dgutov/robe")
    (synopsis "Ruby code assistance tool for Emacs")
    (description
     "Robe can provide information on loaded classes and modules in Ruby code,
as well as where methods are defined.  This allows the user to jump to method
definitions, modules and classes, display method documentation and provide
method and constant name completion.")
    (license license:gpl3+)))

(define-public emacs-rspec
  (package
    (name "emacs-rspec")
    (version "1.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pezra/rspec-mode/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1j0a7ms5516nlg60qfyn730pfxys6acm0rgyxh5xfkpi6jafgpvw"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/pezra/rspec-mode")
    (synopsis "Provides a rspec mode for working with RSpec")
    (description
     "The Emacs RSpec mode provides keybindings for Ruby source files, e.g. to
verify the spec associated with the current buffer, or entire project, as well
as moving between the spec files, and coresponding code files.

Also included are keybindings for spec files and Dired buffers, as well as
snippets for yasnippet.")
    (license license:gpl3+)))

(define-public emacs-smart-mode-line
  (package
    (name "emacs-smart-mode-line")
    (version "2.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Malabarba/smart-mode-line/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0i9wajabrrsjzwd842q0m2611kf0q31p9hg1pdj81177gynkw8l8"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-rich-minority" ,emacs-rich-minority)))
    (home-page "https://github.com/Malabarba/smart-mode-line")
    (synopsis "Color-coded smart mode-line")
    (description
     "Smart Mode Line is a mode-line theme for Emacs.  It aims to be easy to
read from small to large monitors by using colors, a prefix feature, and smart
truncation.")
    (license license:gpl2+)))

(define-public emacs-sr-speedbar
  (let ((commit "77a83fb50f763a465c021eca7343243f465b4a47")
        (revision "0"))
    (package
      (name "emacs-sr-speedbar")
      (version (git-version "20161025" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacsorphanage/sr-speedbar.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0sd12555hk7z721y00kv3crdybvcn1i08wmd148z5imayzibj153"))))
      (build-system emacs-build-system)
      (home-page "https://www.emacswiki.org/emacs/SrSpeedbar")
      (synopsis "Same frame Emacs @code{speedbar}")
      (description
       "This Emacs package allows you to show @code{M-x speedbar} in the
same frame (in an extra window).  You can customize the initial width of
the speedbar window.")
      (license license:gpl3+))))

(define-public emacs-shell-switcher
  (package
    (name "emacs-shell-switcher")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/DamienCassou/shell-switcher"
                           "/archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1c23mfkdqz2g9rixd9smm323vzlvhzz3ng34ambcqjfq309qb2nz"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/DamienCassou/shell-switcher")
    (synopsis "Provide fast switching between shell buffers")
    (description
     "This package provides commands to quickly switch between shell buffers.")
    (license license:gpl3+)))

(define-public emacs-ob-ipython
  (package
    (name "emacs-ob-ipython")
    (version "20150704.8807064693")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (commit "880706469338ab59b5bb7dbe8460016f89755364")
                    (url "https://github.com/gregsexton/ob-ipython.git")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1scf25snbds9ymagpny30ijbsg479r3nm0ih01dy4m9d0g7qryb7"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-f" ,emacs-f)))
    (home-page "http://www.gregsexton.org")
    (synopsis "Org-Babel functions for IPython evaluation")
    (description "This package adds support to Org-Babel for evaluating Python
source code using IPython.")
    (license license:gpl3+)))

(define-public emacs-debbugs
  (package
    (name "emacs-debbugs")
    (version "0.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/debbugs-"
                                  version ".tar"))
              (sha256
               (base32
                "1x7jw2ldgkknyxg7x9fhnqkary691icnysmi3xw0g2fjrvllzhqw"))))
    (build-system emacs-build-system)
    (arguments '(#:include '("\\.el$" "\\.wsdl$" "\\.info$")))
    (propagated-inputs
     `(("emacs-async" ,emacs-async)))
    (home-page "https://elpa.gnu.org/packages/debbugs.html")
    (synopsis "Access the Debbugs bug tracker in Emacs")
    (description
     "This package lets you access the @uref{http://bugs.gnu.org,GNU Bug
Tracker} from within Emacs.

For instance, it defines the command @code{M-x debbugs-gnu} for listing bugs,
and the command @code{M-x debbugs-gnu-search} for bug searching.  If you
prefer the listing of bugs as TODO items of @code{org-mode}, you could use
@code{M-x debbugs-org} and related commands.

A minor mode @code{debbugs-browse-mode} let you browse URLs to the GNU Bug
Tracker as well as bug identifiers prepared for @code{bug-reference-mode}.")
    (license license:gpl3+)))

(define-public emacs-deferred
  (package
    (name "emacs-deferred")
    (version "0.3.2")
    (home-page "https://github.com/kiwanami/emacs-deferred")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0059jy01ni5irpgrj9fa81ayd9j25nvmjjm79ms3210ysx4pgqdr"))
              (file-name (string-append name "-" version))))
    (build-system emacs-build-system)
    ;; FIXME: Would need 'el-expectations' to actually run tests.
    (synopsis "Simple asynchronous functions for Emacs Lisp")
    (description
     "The @code{deferred.el} library provides support for asynchronous tasks.
The API is almost the same as that of
@uref{https://github.com/cho45/jsdeferred, JSDeferred}, a JavaScript library
for asynchronous tasks.")
    (license license:gpl3+)))

(define-public butler
  (package
    (name "emacs-butler")
    (version "0.2.4")
    (home-page "https://github.com/AshtonKem/Butler")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (sha256
               (base32
                "1pii9dw4skq7nr4na6qxqasl36av8cwjp71bf1fgppqpcd9z8skj"))
              (file-name (string-append name "-" version))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-deferred" ,emacs-deferred)))
    (synopsis "Emacs client for Jenkins")
    (description
     "Butler provides an interface to connect to Jenkins continuous
integration servers.  Users can specify a list of server in the
@code{butler-server-list} variable and then use @code{M-x butler-status} to
view the build status of those servers' build jobs, and possibly to trigger
build jobs.")
    (license license:gpl3+)))

(define-public emacs-company
  (package
    (name "emacs-company")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/company-mode/company-mode/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fyrpchpdmvszssy1qmsw41aqpv6q5rybvs1bw00nv9xdhiaq4vh"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda _
             ;; The company-files-candidates-normal-root test looks
             ;; for the /bin directory, but the build environment has
             ;; no /bin directory. Modify the test to look for the
             ;; /tmp directory.
             (substitute* "test/files-tests.el"
               (("/bin/") "/tmp/"))
             (zero? (system* "make" "test-batch")))))))
    (home-page "http://company-mode.github.io/")
    (synopsis "Modular text completion framework")
    (description
     "Company is a modular completion mechanism.  Modules for retrieving
completion candidates are called back-ends, modules for displaying them are
front-ends.  Company comes with many back-ends, e.g. @code{company-elisp}.
These are distributed in separate files and can be used individually.")
    (license license:gpl3+)))

(define-public emacs-company-quickhelp
  (package
    (name "emacs-company-quickhelp")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/expez/company-quickhelp/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0xrn2z1dgk5gmkmp2jkn9g83ckk39lqp5pyyv8rl7f6gqvib3qh0"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-pos-tip" ,emacs-pos-tip)
       ("emacs-company" ,emacs-company)))
    (home-page "https://github.com/expez/company-quickhelp")
    (synopsis "Popup documentation for completion candidates")
    (description "@code{company-quickhelp} shows documentation for the
completion candidate when using the Company text completion framework.")
    (license license:gpl3+)))

(define-public emacs-multiple-cursors
  (package
    (name "emacs-multiple-cursors")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/magnars/multiple-cursors.el/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0hihihlvcvzayg5fnqzcg45fhvlmq6xlq58syy00rjwbry9w389k"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/magnars/multiple-cursors.el")
    (synopsis "Multiple cursors for Emacs")
    (description
     "This package adds support to Emacs for editing text with multiple
simultaneous cursors.")
    (license license:gpl3+)))

(define-public typo
  (package
    (name "emacs-typo")
    (version "1.1")
    (home-page "https://github.com/jorgenschaefer/typoel")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1jhd4grch5iz12gyxwfbsgh4dmz5hj4bg4gnvphccg8dsnni05k2"))
              (file-name (string-append name "-" version))))
    (build-system emacs-build-system)
    (synopsis "Minor mode for typographic editing")
    (description
     "This package provides two Emacs modes, @code{typo-mode} and
@code{typo-global-mode}.  These modes automatically insert Unicode characters
for quotation marks, dashes, and ellipses.  For example, typing @kbd{\"}
automatically inserts a Unicode opening or closing quotation mark, depending
on context.")
    (license license:gpl3+)))

(define-public emacs-scheme-complete
  (let ((commit "9b5cf224bf2a5994bc6d5b152ff487517f1a9bb5"))
    (package
      (name "emacs-scheme-complete")
      (version (string-append "20151223." (string-take commit 8)))
      (source
       (origin
         (file-name (string-append name "-" version))
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ashinn/scheme-complete.git")
               (commit commit)))
         (sha256
          (base32
           "141wn9l0m33w0g3dqmx8nxbfdny1r5xbr6ak61rsz21bk0qafs7x"))
         (patches
          (search-patches "emacs-scheme-complete-scheme-r5rs-info.patch"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/ashinn/scheme-complete")
      (synopsis "Smart tab completion for Scheme in Emacs")
      (description
       "This file provides a single function, @code{scheme-smart-complete},
which you can use for intelligent, context-sensitive completion for any Scheme
implementation in Emacs.  To use it just load this file and bind that function
to a key in your preferred mode.")
      (license license:public-domain))))

(define-public emacs-mit-scheme-doc
  (package
    (name "emacs-mit-scheme-doc")
    (version "20140203")
    (source
     (origin
       (modules '((guix build utils)))
       (snippet
        ;; keep only file of interest
        '(begin
           (for-each delete-file '("dot-emacs.el" "Makefile"))
           (install-file "6.945-config/mit-scheme-doc.el" ".")
           (delete-file-recursively "6.945-config")))
       (file-name (string-append name "-" version ".tar.bz2"))
       (method url-fetch)
       (uri (string-append "http://groups.csail.mit.edu/mac/users/gjs/"
                           "6.945/dont-panic/emacs-basic-config.tar.bz2"))
       (sha256
        (base32
         "0dqidg2bd66pawqfarvwca93w5gqf9mikn1k2a2rmd9ymfjpziq1"))))
    (build-system emacs-build-system)
    (inputs `(("mit-scheme" ,mit-scheme)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure-doc
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((mit-scheme-dir (assoc-ref inputs "mit-scheme"))
                    (doc-dir (string-append mit-scheme-dir "/share/doc/"
                                            "mit-scheme-"
                                            ,(package-version mit-scheme))))
               (substitute* "mit-scheme-doc.el"
                 (("http://www\\.gnu\\.org/software/mit-scheme/documentation/mit-scheme-ref/")
                  (string-append "file:" doc-dir "/mit-scheme-ref/")))))))))
    (home-page "http://groups.csail.mit.edu/mac/users/gjs/6.945/dont-panic/")
    (synopsis "MIT-Scheme documentation lookup for Emacs")
    (description
     "This package provides a set of Emacs functions to search definitions of
identifiers in the MIT-Scheme documentation.")
    (license license:gpl2+)))

(define-public emacs-constants
  (package
    (name "emacs-constants")
    (version "2.6")
    (home-page "https://staff.fnwi.uva.nl/c.dominik/Tools/constants")
    (source
     (origin
       (file-name (string-append name "-" version ".tar.gz"))
       (method url-fetch)
       (uri (string-append "https://github.com/fedeinthemix/emacs-constants"
                           "/archive/v" version ".tar.gz"))
       (sha256
        (base32
         "0pnrpmmxq8mh5h2hbrp5vcym0j0fh6dv3s7c5ccn18wllhzg9g7n"))))
    (build-system emacs-build-system)
    (synopsis "Enter definition of constants into an Emacs buffer")
    (description
     "This package provides functions for inserting the definition of natural
constants and units into an Emacs buffer.")
    (license license:gpl2+)))

(define-public emacs-tagedit
  (package
    (name "emacs-tagedit")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/magnars/tagedit/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1apfnann4qklfdsmdi7icjsj18x7gwx8d83iqr4z25clszz95xfq"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-s" ,emacs-s)
       ("emacs-dash" ,emacs-dash)))
    (home-page "https://github.com/magnars/tagedit")
    (synopsis "Some paredit-like features for html-mode")
    (description
     "This package provides a collection of paredit-like functions for editing
in @code{html-mode}.")
    (license license:gpl3+)))

(define-public emacs-slime
  (package
    (name "emacs-slime")
    (version "2.20")
    (source
     (origin
       (file-name (string-append name "-" version ".tar.gz"))
       (method url-fetch)
       (uri (string-append
             "https://github.com/slime/slime/archive/v"
             version ".tar.gz"))
       (sha256
        (base32
         "086lq5y4pvj9wihy0si02xxvyzpzz8mcg3hz1cvy9zxlyjwzr1gk"))))
    (build-system emacs-build-system)
    (native-inputs
     `(("texinfo" ,texinfo)))
    (arguments
     `(#:include '("\\.el$" "\\.lisp$" "\\.asd$" "contrib")
       #:exclude '("^slime-tests.el" "^contrib/test/"
                   "^contrib/Makefile$" "^contrib/README.md$")
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'configure
           (lambda* _
             (emacs-substitute-variables "slime.el"
               ("inferior-lisp-program" "sbcl"))
             #t))
         (add-before 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (info-dir (string-append out "/share/info"))
                    (doc-dir (string-append out "/share/doc/"
                                            ,name "-" ,version))
                    (doc-files '("doc/slime-refcard.pdf"
                                 "README.md" "NEWS" "PROBLEMS"
                                 "CONTRIBUTING.md")))
               (with-directory-excursion "doc"
                 (substitute* "Makefile"
                   (("infodir=/usr/local/info")
                    (string-append "infodir=" info-dir)))
                 (system* "make" "html/index.html")
                 (system* "make" "slime.info")
                 (install-file "slime.info" info-dir)
                 (copy-recursively "html" (string-append doc-dir "/html")))
               (for-each (lambda (f)
                           (install-file f doc-dir)
                           (delete-file f))
                         doc-files)
               (delete-file-recursively "doc")
               #t))))))
    (home-page "https://github.com/slime/slime")
    (synopsis "Superior Lisp Interaction Mode for Emacs")
    (description
     "SLIME extends Emacs with support for interactive programming in
Common Lisp.  The features are centered around @command{slime-mode},
an Emacs minor mode that complements the standard @command{lisp-mode}.
While lisp-mode supports editing Lisp source files, @command{slime-mode}
adds support for interacting with a running Common Lisp process
for compilation, debugging, documentation lookup, and so on.")
    (license license:gpl2+)))

(define-public emacs-popup
  (package
    (name "emacs-popup")
    (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/auto-complete/popup-el/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yrgfj8y69xmcb6kwgplhq68ndm9410qwh7sd2knnd1gchpphdc0"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/auto-complete/popup-el")
    (synopsis "Visual Popup User Interface for Emacs")
    (description
     "Popup.el is a visual popup user interface library for Emacs.
This provides a basic API and common UI widgets such as popup tooltips
and popup menus.")
    (license license:gpl3+)))

(define-public emacs-god-mode
  (let ((commit "6cf0807b6555eb6fcf8387a4e3b667071ef38964")
        (revision "1"))
    (package
      (name "emacs-god-mode")
      (version (string-append "20151005.925."
                              revision "-" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/chrisdone/god-mode.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1am415k4xxcva6y3vbvyvknzc6bma49pq3p85zmpjsdmsp18qdix"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/chrisdone/god-mode")
      (synopsis "Minor mode for entering commands without modifier keys")
      (description
       "This package provides a global minor mode for entering Emacs commands
without modifier keys.  It's similar to Vim's separation of commands and
insertion mode.  When enabled all keys are implicitly prefixed with
@samp{C-} (among other helpful shortcuts).")
      (license license:gpl3+))))

(define-public emacs-jinja2-mode
  (package
    (name "emacs-jinja2-mode")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/paradoxxxzero/jinja2-mode/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0cgxjab1kla2zc2fj7bzib6i7snp08zshandmp9kqcm85l262xpn"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/paradoxxxzero/jinja2-mode")
    (synopsis "Major mode for jinja2")
    (description
     "Emacs major mode for jinja2 with: syntax highlighting,
sgml/html integration, and indentation (working with sgml).")
    (license license:gpl3+)))

(define-public emacs-rfcview
  (package
    (name "emacs-rfcview")
    (version "0.13")
    (home-page "http://www.loveshack.ukfsn.org/emacs")
    (source (origin
              (method url-fetch)
              (uri "http://www.loveshack.ukfsn.org/emacs/rfcview.el")
              (sha256
               (base32
                "0ympj5rxig383zl2jf0pzdsa80nnq0dpvjiriq0ivfi98fj7kxbz"))))
    (build-system emacs-build-system)
    (synopsis "Prettify Request for Comments (RFC) documents")
    (description "The Internet Engineering Task Force (IETF) and the Internet
Society (ISOC) publish various Internet-related protocols and specifications
as \"Request for Comments\" (RFC) documents and Internet Standard (STD)
documents.  RFCs and STDs are published in a simple text form.  This package
provides an Emacs major mode, rfcview-mode, which makes it more pleasant to
read these documents in Emacs.  It prettifies the text and adds
hyperlinks/menus for easier navigation.  It also provides functions for
browsing the index of RFC documents and fetching them from remote servers or
local directories.")
    (license license:gpl3+)))

(define-public emacs-ffap-rfc-space
  (package
    (name "emacs-ffap-rfc-space")
    (version "12")
    (home-page "http://user42.tuxfamily.org/ffap-rfc-space/index.html")
    (source (origin
              (method url-fetch)
              (uri "http://download.tuxfamily.org/user42/ffap-rfc-space.el")
              (sha256
               (base32
                "1iv61dv57a73mdps7rn6zmgz7nqh14v0ninidyrasy45b1nv6gck"))))
    (build-system emacs-build-system)
    (synopsis "Make ffap recognize an RFC with a space before its number")
    (description "The Internet Engineering Task Force (IETF) and the
Internet Society (ISOC) publish various Internet-related protocols and
specifications as \"Request for Comments\" (RFC) documents.  The
built-in Emacs module \"ffap\" (Find File at Point) has the ability to
recognize names at point which look like \"RFC1234\" and \"RFC-1234\"
and load the appropriate RFC from a remote server.  However, it fails
to recognize a name like \"RFC 1234\".  This package enhances ffap so
that it correctly finds RFCs even when a space appears before the
number.")
    (license license:gpl3+)))

(define-public emacs-org-bullets
  (package
    (name "emacs-org-bullets")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/sabof/org-bullets/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1dyxvpb73vj80v8br2q9rf255hfphrgaw91fbvwdcd735np9pcnh"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/sabof/org-bullets")
    (synopsis "Show bullets in org-mode as UTF-8 characters")
    (description
     "This package provides an Emacs minor mode causing bullets in
@code{org-mode} to be rendered as UTF-8 characters.")
    (license license:gpl3+)))

(define-public emacs-org-pomodoro
  (package
    (name "emacs-org-pomodoro")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lolownia/org-pomodoro/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jalsggw3q5kvj353f84x4nl04a5vmq07h75ggppy1627lb31zm4"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-alert" ,emacs-alert)))
    (home-page "https://github.com/lolownia/org-pomodoro")
    (synopsis "Pomodoro technique for org-mode")
    (description "@code{emacs-org-pomodoro} adds very basic support for
Pomodoro technique in Emacs org-mode.

Run @code{M-x org-pomodoro} for the task at point or select one of the
last tasks that you clocked time for.  Each clocked-in pomodoro starts
a timer of 25 minutes and after each pomodoro a break timer of 5
minutes is started automatically.  Every 4 breaks a long break is
started with 20 minutes.  All values are customizable.")
    (license license:gpl3+)))

(define-public emacs-org-trello
  (package
    (name "emacs-org-trello")
    (version "0.7.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/org-trello/org-trello/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "074dka8g673bj1ck5vavbjaij5jyniygdlw51mdds005wd2br9wf"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-deferred" ,emacs-deferred)
       ("emacs-request" ,emacs-request)
       ("emacs-dash" ,emacs-dash)
       ("emacs-s" ,emacs-s)))
    (home-page "https://org-trello.github.io")
    (synopsis "Emacs minor mode for interacting with Trello")
    (description "This package provides an Emacs minor mode to extend
@code{org-mode} with Trello abilities.  Trello is an online project
organizer.")
    (license license:gpl3+)))

(define-public emacs-zenburn-theme
  (package
    (name "emacs-zenburn-theme")
    (version "2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/bbatsov/zenburn-emacs/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03kfhzgbbbl8ivpzzky6qxw4j9mmp452m1sk7wikxmcalfnix0gn"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/bbatsov/zenburn-emacs")
    (synopsis "Low contrast color theme for Emacs")
    (description
     "Zenburn theme is a port of the popular Vim Zenburn theme for Emacs.
It is built on top of the custom theme support in Emacs 24 or later.")
    (license license:gpl3+)))

(define-public emacs-solarized-theme
  (package
    (name "emacs-solarized-theme")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bbatsov/solarized-emacs/"
                                  "archive/v"  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ha3slc6d9wi9ilkhmwrzkvf308n6ph7b0k69pk369s9304awxzx"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)))
    (home-page "https://github.com/bbatsov/solarized-emacs")
    (synopsis "Port of the Solarized theme for Emacs")
    (description
     "Solarized for Emacs is a port of the Solarized theme for Vim.  This
package provides a light and a dark variant.")
    (license license:gpl3+)))

(define-public emacs-ahungry-theme
  (package
    (name "emacs-ahungry-theme")
    (version "1.10.0")
    (source
     (origin (method url-fetch)
             (uri (string-append "https://elpa.gnu.org/packages/ahungry-theme-"
                                 version ".tar"))
             (sha256
              (base32
               "14q5yw56n82qph09bk7wmj5b1snhh9w0nk5s1l7yn9ldg71xq6pm"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ahungry/color-theme-ahungry")
    (synopsis "Ahungry color theme for Emacs")
    (description "Ahungry theme for Emacs provides bright and bold colors.
If you load it from a terminal, you will be able to make use of the
transparent background.  If you load it from a GUI, it will default to a
dark background.")
    (license license:gpl3+)))

(define-public emacs-2048-game
  (package
    (name "emacs-2048-game")
    (version "20151026.1233")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://melpa.org/packages/2048-game-"
                           version ".el"))
       (sha256
        (base32
         "0gy2pvz79whpavp4jmz8h9krzn7brmvv3diixi1d4w51pcdvaldd"))))
    (build-system emacs-build-system)
    (home-page "https://bitbucket.org/zck/2048.el")
    (synopsis "Implementation of the game 2048 in Emacs Lisp")
    (description
     "This program is an implementation of 2048 for Emacs.
The goal of this game is to create a tile with value 2048.  The size of the
board and goal value can be customized.")
  (license license:gpl3+)))

(define-public emacs-base16-theme
  (package
    (name "emacs-base16-theme")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://stable.melpa.org/packages/base16-theme-"
                           version ".tar"))
       (sha256
        (base32
         "0z6hrwz2jlz6jbr381rcqcqvx6hss5cad352klx07rark7zccacj"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/belak/base16-emacs")
    (synopsis "Base16 color themes for Emacs")
    (description
     "Base16 provides carefully chosen syntax highlighting and a default set
of sixteen colors suitable for a wide range of applications.  Base16 is not a
single theme but a set of guidelines with numerous implementations.")
    (license license:expat)))

(define-public emacs-smartparens
  (package
    (name "emacs-smartparens")
    (version "1.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Fuco1/smartparens/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q5as813xs8y29i3v2rm97phd6m7xsmmw6hwbvx57gwmi8i1c409"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-dash" ,emacs-dash)))
    (home-page "https://github.com/Fuco1/smartparens")
    (synopsis "Paredit-like insertion, wrapping and navigation with user
defined pairs")
    (description
     "Smartparens is a minor mode for Emacs that deals with parens pairs
and tries to be smart about it.  It started as a unification effort to
combine functionality of several existing packages in a single,
compatible and extensible way to deal with parentheses, delimiters, tags
and the like.  Some of these packages include autopair, textmate,
wrap-region, electric-pair-mode, paredit and others.  With the basic
features found in other packages it also brings many improvements as
well as completely new features.")
    (license license:gpl3+)))

(define-public emacs-highlight-symbol
  (package
    (name "emacs-highlight-symbol")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/nschum/highlight-symbol.el/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1n7k1qns0fn0jsyc0hrjac5nzk21xw48yc30vyrhwvc51h0b9g90"))))
    (build-system emacs-build-system)
    (home-page "https://nschum.de/src/emacs/highlight-symbol")
    (synopsis "Automatic and manual symbol highlighting for Emacs")
    (description
     "Use @code{highlight-symbol} to toggle highlighting of the symbol at
point throughout the current buffer.  Use @code{highlight-symbol-mode} to keep
the symbol at point highlighted.

The functions @code{highlight-symbol-next}, @code{highlight-symbol-prev},
@code{highlight-symbol-next-in-defun} and
@code{highlight-symbol-prev-in-defun} allow for cycling through the locations
of any symbol at point.  Use @code{highlight-symbol-nav-mode} to enable key
bindings @code{M-p} and @code{M-p} for navigation.  When
@code{highlight-symbol-on-navigation-p} is set, highlighting is triggered
regardless of @code{highlight-symbol-idle-delay}.

@code{highlight-symbol-query-replace} can be used to replace the symbol. ")
    (license license:gpl2+)))

(define-public emacs-hl-todo
  (package
    (name "emacs-hl-todo")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://raw.githubusercontent.com/tarsius/hl-todo/"
                    version "/hl-todo.el"))
              (file-name (string-append "hl-todo-" version ".el"))
              (sha256
               (base32
                "0g0h9v4572p7mcird8wsj1c41haf60krslm6mlpi4mdbh248kv6z"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/tarsius/hl-todo")
    (synopsis "Emacs mode to highlight TODO and similar keywords")
    (description
     "This package provides an Emacs mode to highlight TODO and similar
keywords in comments and strings.  This package also provides commands for
moving to the next or previous keyword and to invoke @code{occur} with a
regexp that matches all known keywords.")
    (license license:gpl3+)))

(define-public emacs-perspective
  (package
    (name "emacs-perspective")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/nex3/perspective-el/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "078ahh0kmhdylq5ib9c81c76kz1n02xwc83pm729d00i84ibviic"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/nex3/perspective-el")
    (synopsis "Switch between named \"perspectives\"")
    (description
     "This package provides tagged workspaces in Emacs, similar to workspaces in
windows managers such as Awesome and XMonad.  @code{perspective.el} provides
multiple workspaces (or \"perspectives\") for each Emacs frame.  Each
perspective is composed of a window configuration and a set of buffers.
Switching to a perspective activates its window configuration, and when in a
perspective only its buffers are available by default.")
    ;; This package is released under the same license as Emacs (GPLv3+) or
    ;; the Expat license.
    (license license:gpl3+)))

(define-public emacs-test-simple
  (package
    (name "emacs-test-simple")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/test-simple-"
                           version ".el"))
       (sha256
        (base32
         "1yd61jc9ds95a5n09052kwc5gasy57g4lxr0jsff040brlyi9czz"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/rocky/emacs-test-simple")
    (synopsis "Simple unit test framework for Emacs Lisp")
    (description
     "Test Simple is a simple unit test framework for Emacs Lisp.  It
alleviates the need for context macros, enclosing specifications or required
test tags.  It supports both interactive and non-interactive use.")
    (license license:gpl3+)))

(define-public emacs-load-relative
  (package
    (name "emacs-load-relative")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/load-relative-"
                           version ".el"))
       (sha256
        (base32
         "1hfxb2436jdsi9wfmsv47lkkpa5galjf5q81bqabbsv79rv59dps"))))
    (build-system emacs-build-system)
    (home-page "http://github.com/rocky/emacs-load-relative")
    (synopsis "Emacs Lisp relative file loading related functions")
    (description
     "Provides functions which facilitate writing multi-file Emacs packages
and running from the source tree without having to \"install\" code or fiddle
with @{load-path}.

The main function, @code{load-relative}, loads an Emacs Lisp file relative to
another (presumably currently running) Emacs Lisp file.")
    (license license:gpl3+)))

(define-public emacs-loc-changes
  (package
    (name "emacs-loc-changes")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/loc-changes-"
                           version ".el"))
       (sha256
        (base32
         "1x8fn8vqasayf1rb8a6nma9n6nbvkx60krmiahyb05vl5rrsw6r3"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/rocky/emacs-loc-changes")
    (synopsis "Keeps track of positions even after buffer changes")
    (description
     "This Emacs package provides a mean to track important buffer positions
after buffer changes.")
    (license license:gpl3+)))

(define-public emacs-realgud
  (package
    (name "emacs-realgud")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/realgud-"
                           version ".tar"))
       (sha256
        (base32
         "1nc8km339ip90h1j55ahfga03v7x7rh4iycmw6yrxyzir68vwn7c"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-autogen-script
           (lambda _
             (substitute* "autogen.sh"
               (("./configure") "sh configure"))))
         (add-after 'fix-autogen-script 'autogen
           (lambda _
             (setenv "CONFIG_SHELL" "sh")
             (invoke "sh" "autogen.sh")))
         (add-after 'fix-autogen-script 'set-home
           (lambda _
             (setenv "HOME" (getenv "TMPDIR"))))
         (add-before 'patch-el-files 'remove-realgud-pkg.el
           (lambda _
             ;; XXX: This file is auto-generated at some point and causes
             ;; substitute* to crash during the `patch-el-files' phase with:
             ;; ERROR: In procedure stat: No such file or directory:
             ;; "./realgud-pkg.el"
             (delete-file "./realgud-pkg.el")
             ;; FIXME: `patch-el-files' crashes on this file with error:
             ;; unable to locate "bashdb".
             (delete-file "./test/test-regexp-bashdb.el"))))
       #:include (cons* ".*\\.el$" %default-include)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("emacs-test-simple" ,emacs-test-simple)))
    (propagated-inputs
     `(("emacs-load-relative" ,emacs-load-relative)
       ("emacs-loc-changes" ,emacs-loc-changes)))
    (home-page "https://github.com/realgud/realgud/")
    (synopsis
     "Modular front-end for interacting with external debuggers")
    (description
     "RealGUD is a modular, extensible GNU Emacs front-end for interacting
with external debuggers.  It integrates various debuggers such as gdb, pdb,
ipdb, jdb, lldb, bashdb, zshdb, etc. and allows to visually step code in the
sources.  Unlike GUD, it also supports running multiple debug sessions in
parallel.")
    (license license:gpl3+)))

(define-public emacs-request
  (package
    (name "emacs-request")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tkf/emacs-request.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0wyxqbb35yqf6ci47531lk32d6fppamx9d8826kdz983vm87him7"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/tkf/emacs-request")
    (synopsis "Package for speaking HTTP in Emacs Lisp")
    (description "This package provides a HTTP request library with multiple
backends.  It supports url.el which is shipped with Emacs and the curl command
line program.")
    (license license:gpl3+)))

(define-public emacs-rudel
  (package
    (name "emacs-rudel")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://elpa.gnu.org/packages/rudel-"
                           version ".tar"))
       (sha256
        (base32
         "0glqa68g509p0s2vcc0i8kzlddnc9brd9jqhnm5rzxz4i050cvnz"))))
    (build-system emacs-build-system)
    (home-page "http://rudel.sourceforge.net/")
    (synopsis "Collaborative editing framework")
    (description
     "Rudel is a collaborative editing environment for GNU Emacs.  Its purpose
is to share buffers with other users in order to edit the contents of those
buffers collaboratively.  Rudel supports multiple backends to enable
communication with other collaborative editors using different protocols,
though currently Obby (for use with the Gobby editor) is the only
fully-functional one.")
    (license license:gpl3+)))

(define-public emacs-hydra
  (package
    (name "emacs-hydra")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/abo-abo/hydra/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0884k3ffwzhh6krbd8l7vvm184dkagb2jf4q8xzg72plln34qrm8"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/abo-abo/hydra")
    (synopsis "Make Emacs bindings that stick around")
    (description
     "This package can be used to tie related commands into a family of short
bindings with a common prefix---a Hydra.  Once you summon the Hydra (through
the prefixed binding), all the heads can be called in succession with only a
short extension.  Any binding that isn't the Hydra's head vanquishes the
Hydra.  Note that the final binding, besides vanquishing the Hydra, will still
serve its original purpose, calling the command assigned to it.  This makes
the Hydra very seamless; it's like a minor mode that disables itself
automatically.")
    (license license:gpl3+)))

(define-public emacs-ivy
  (package
    (name "emacs-ivy")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abo-abo/swiper.git")
             (commit version)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "14vnigqb5c3yi4q9ysw1fiwdqyqwyklqpb9wnjf81chm7s2mshnr"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (info (string-append out "/share/info")))
               (with-directory-excursion "doc"
                 (unless (zero? (system* "makeinfo" "ivy.texi"))
                   (error "makeinfo failed"))
                 (install-file "ivy.info" info))))))))
    (propagated-inputs
     `(("emacs-hydra" ,emacs-hydra)))
    (native-inputs
     `(("texinfo" ,texinfo)))
    (home-page "http://oremacs.com/swiper/")
    (synopsis "Incremental vertical completion for Emacs")
    (description
     "This package provides @code{ivy-read} as an alternative to
@code{completing-read} and similar functions.  No attempt is made to determine
the best candidate.  Instead, the user can navigate candidates with
@code{ivy-next-line} and @code{ivy-previous-line}.  The matching is done by
splitting the input text by spaces and re-building it into a regular
expression.")
    (license license:gpl3+)))

(define-public emacs-avy
  (package
    (name "emacs-avy")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/abo-abo/avy/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wdrq512h25ymzjbf2kbsdymvd2ryfwzb6bh5bc3yv7q203im796"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/abo-abo/avy")
    (synopsis "Tree-based completion for Emacs")
    (description
     "This package provides a generic completion method based on building a
balanced decision tree with each candidate being a leaf.  To traverse the tree
from the root to a desired leaf, typically a sequence of @code{read-key} can
be used.

In order for @code{read-key} to make sense, the tree needs to be visualized
appropriately, with a character at each branch node.  So this completion
method works only for things that you can see on your screen, all at once,
such as the positions of characters, words, line beginnings, links, or
windows.")
    (license license:gpl3+)))

(define-public emacs-ace-window
  (package
    (name "emacs-ace-window")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/abo-abo/ace-window/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1p2sgfl5dml4zbd6ldql6lm2m9vmd236ah996ni32x254s48j5pn"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-avy" ,emacs-avy)))
    (home-page "https://github.com/abo-abo/ace-window")
    (synopsis "Quickly switch windows in Emacs")
    (description
     "@code{ace-window} is meant to replace @code{other-window}.
In fact, when there are only two windows present, @code{other-window} is
called.  If there are more, each window will have its first character
highlighted.  Pressing that character will switch to that window.")
    (license license:gpl3+)))

(define-public emacs-iedit
  (package
    (name "emacs-iedit")
    (version "0.9.9.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/victorhge/iedit/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1hv8q6pr85ss9g3158l1fqv3m62vsq8rslsi86jicr2dcxyascr0"))))
    (build-system emacs-build-system)
    (home-page "http://www.emacswiki.org/emacs/Iedit")
    (synopsis "Edit multiple regions in the same way simultaneously")
    (description
     "This package is an Emacs minor mode and allows you to edit one
occurrence of some text in a buffer (possibly narrowed) or region, and
simultaneously have other occurrences edited in the same way.

You can also use Iedit mode as a quick way to temporarily show only the buffer
lines that match the current text being edited.  This gives you the effect of
a temporary @code{keep-lines} or @code{occur}.")
    (license license:gpl3+)))

(define-public emacs-lispy
  (package
    (name "emacs-lispy")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/abo-abo/lispy/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "15gig95cvamw5zlw99cxggd27c18b9scznjj97gvjn2zbljcaqzl"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-ace-window" ,emacs-ace-window)
       ("emacs-iedit" ,emacs-iedit)
       ("emacs-ivy" ,emacs-ivy)
       ("emacs-hydra" ,emacs-hydra)))
    (home-page "https://github.com/abo-abo/lispy")
    (synopsis "Modal S-expression editing")
    (description
     "Due to the structure of Lisp syntax it's very rare for the programmer to
want to insert characters right before \"(\" or right after \")\".  Thus
unprefixed printable characters can be used to call commands when the point is
at one of these special locations.  Lispy provides unprefixed keybindings for
S-expression editing when point is at the beginning or end of an
S-expression.")
    (license license:gpl3+)))

(define-public emacs-clojure-mode
  (package
    (name "emacs-clojure-mode")
    (version "5.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/clojure-emacs/clojure-mode/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "117mvjqh4nm8mvmwmmvy4qmkdg23ldlzk08y91g8b8ac8kxwqg81"))))
    (build-system emacs-build-system)
    (native-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-s" ,emacs-s)
       ("ert-runner" ,ert-runner)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'check
           (lambda _
             (zero? (system* "ert-runner")))))))
    (home-page "https://github.com/clojure-emacs/clojure-mode")
    (synopsis "Major mode for Clojure code")
    (description
     "This Emacs package provides font-lock, indentation, navigation and basic
refactoring for the @uref{http://clojure.org, Clojure programming language}.
It is recommended to use @code{clojure-mode} with paredit or smartparens.")
    (license license:gpl3+)))

(define-public emacs-epl
  (package
    (name "emacs-epl")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cask/epl/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1511n3a3f5gvaf2b4nh018by61ciyzi3y3603fzqma7p9hrckarc"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/cask/epl")
    (synopsis "Emacs Package Library")
    (description
     "A package management library for Emacs, based on @code{package.el}.

The purpose of this library is to wrap all the quirks and hassle of
@code{package.el} into a sane API.")
    (license license:gpl3+)))

(define-public emacs-queue
  (package
    (name "emacs-queue")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/queue-"
                                  version ".el"))
              (sha256
               (base32
                "0cx2848sqnnkkr4zisvqadzxngjyhmb36mh0q3if7q19yjjhmrkb"))))
    (build-system emacs-build-system)
    (home-page "http://www.dr-qubit.org/tags/computing-code-emacs.html")
    (synopsis "Queue data structure for Emacs")
    (description
     "This Emacs library provides queue data structure.  These queues can be
used both as a first-in last-out (FILO) and as a first-in first-out (FIFO)
stack, i.e. elements can be added to the front or back of the queue, and can
be removed from the front.  This type of data structure is sometimes called an
\"output-restricted deque\".")
    (license license:gpl3+)))

(define-public emacs-pkg-info
  (package
    (name "emacs-pkg-info")
    (version "0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/lunaryorn/pkg-info.el/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gy1jks5mmm02gg1c8gcyr4f8a9s5ggzhk56gv33b9mzjqzi5rd5"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-epl" ,emacs-epl)))
    (home-page "https://github.com/lunaryorn/pkg-info.el")
    (synopsis "Information about Emacs packages")
    (description
     "This library extracts information from the installed Emacs packages.")
    (license license:gpl3+)))

(define-public emacs-spinner
  (package
    (name "emacs-spinner")
    (version "1.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/spinner-"
                                  version ".el"))
              (sha256
               (base32
                "19kp1mmndbmw11sgvv2ggfjl4pyf5zrsbh3871f0965pw9z8vahd"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Malabarba/spinner.el")
    (synopsis "Emacs mode-line spinner for operations in progress")
    (description
     "This Emacs package adds spinners and progress-bars to the mode-line for
ongoing operations.")
    (license license:gpl3+)))

(define-public emacs-sparql-mode
  (package
    (name "emacs-sparql-mode")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ljos/sparql-mode/archive/"
                                  "v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s93mkllxnhy7fw616cnnc2danacdlarys0g3cn89drh0llh53cv"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ljos/sparql-mode")
    (synopsis "SPARQL mode for Emacs")
    (description "This package provides a major mode for Emacs that provides
syntax highlighting for SPARQL.  It also provides a way to execute queries
against a SPARQL HTTP endpoint, such as is provided by Fuseki.  It is also
possible to query other endpoints like DBPedia.")
    (license license:gpl3+)))

(define-public emacs-better-defaults
  (package
    (name "emacs-better-defaults")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/technomancy/better-defaults"
                           "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "08fg4zslzlxbvyil5g4gwvwd22fh4zsgqprs5wh9hv1rgc6757m2"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/technomancy/better-defaults")
    (synopsis "Better defaults for Emacs")
    (description
     "Better defaults attempts to address the most obvious deficiencies of the
Emacs default configuration in uncontroversial ways that nearly everyone can
agree upon.")
    (license license:gpl3+)))

(define-public emacs-eprime
  (let ((commit "17a481af26496be91c07139a9bfc05cfe722506f"))
    (package
      (name "emacs-eprime")
      (version (string-append "20140513-" (string-take commit 7)))
      (source (origin
                (method url-fetch)
                (uri (string-append "https://raw.githubusercontent.com"
                                    "/AndrewHynes/eprime-mode/"
                                    commit "/eprime-mode.el"))
                (file-name (string-append "eprime-" version ".el"))
                (sha256
                 (base32
                  "0v68lggkyq7kbcr9zyi573m2g2x251xy3jadlaw8kx02l8krwq8d"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/AndrewHynes/eprime-mode")
      (synopsis "E-prime checking mode for Emacs")
      (description "This package provides an E-prime checking mode for Emacs
that highlights non-conforming text.  The subset of the English language called
E-Prime forbids the use of the \"to be\" form to strengthen your writing.")
      (license license:gpl3+))))

(define-public emacs-julia-mode
  ;; XXX: Upstream version remained stuck at 0.3.  See
  ;; <https://github.com/JuliaEditorSupport/julia-emacs/issues/46>.
  (let ((commit "115d4dc8a07445301772da8376b232fa8c7168f4")
        (revision "1"))
    (package
      (name "emacs-julia-mode")
      (version (string-append "0.3-" revision "." (string-take commit 8)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/JuliaEditorSupport/julia-emacs.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1is4dcv6blslpzbjcg8l2jpxi8xj96q4cm0nxjxsyswpm8bw8ki0"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'check
             (lambda _
               (zero? (system* "emacs" "-batch"
                               "-l" "julia-mode.el"
                               "-l" "julia-mode-tests.el"
                               "-f" "ert-run-tests-batch-and-exit")))))))
      (home-page "https://github.com/JuliaEditorSupport/julia-emacs")
      (synopsis "Major mode for Julia")
      (description "This Emacs package provides a mode for the Julia
programming language.")
      (license license:expat))))

(define-public emacs-ess
  (package
    (name "emacs-ess")
    (version "16.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ess.r-project.org/downloads/ess/ess-"
                                  version ".tgz"))
              (sha256
               (base32
                "04m8lwp3ylh2vl7k2bjjs7mxbm64j4sdckqpvnm9k0qhaqf02pjk"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Stop ESS from trying to bundle an external julia-mode.el.
                  (substitute* "lisp/Makefile"
                    (("^\tjulia-mode.elc\\\\\n") "")
                    (("^all: \\$\\(ELC\\) ess-custom.el julia-mode.el")
                     "all: $(ELC) ess-custom.el"))))))
    (build-system gnu-build-system)
    (arguments
     (let ((base-directory "/share/emacs/site-lisp/guix.d/ess"))
       `(#:make-flags (list (string-append "PREFIX=" %output)
                            (string-append "ETCDIR=" %output "/"
                                           ,base-directory "/etc")
                            (string-append "LISPDIR=" %output "/"
                                           ,base-directory))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'build 'more-shebang-patching
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "Makeconf"
                 (("SHELL = /bin/sh")
                  (string-append "SHELL = " (which "sh"))))))
           ;; FIXME: the texlive-union insists on regenerating fonts.  It stores
           ;; them in HOME, so it needs to be writeable.
           (add-before 'build 'set-HOME
             (lambda _ (setenv "HOME" "/tmp") #t))
           (replace 'check
             (lambda _
               (zero? (system* "make" "test"))))))))
    (inputs
     `(("emacs" ,emacs-minimal)
       ("r-minimal" ,r-minimal)))
    (native-inputs
     `(("perl" ,perl)
       ("texinfo" ,texinfo)
       ("texlive" ,(texlive-union (list texlive-latex-natbib
                                        texlive-latex-seminar
                                        texlive-latex-hyperref
                                        texlive-tex-texinfo)))))
    (home-page "http://ess.r-project.org/")
    (synopsis "Emacs mode for statistical analysis programs")
    (description "Emacs Speaks Statistics (ESS) is an add-on package for GNU
Emacs.  It is designed to support editing of scripts and interaction with
various statistical analysis programs such as R and OpenBUGS.")
    (license license:gpl2+)))

(define-public emacs-smex
  (package
    (name "emacs-smex")
    (version "3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com"
                                  "/nonsequitur/smex/" version "/smex.el"))
              (file-name (string-append "smex-" version ".el"))
              (sha256
               (base32
                "0ar310zx9k5y4i1vl2rawvi712xj9gx77160860jbs691p77cxqp"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/nonsequitur/smex/")
    (synopsis "M-x interface with Ido-style fuzzy matching")
    (description
     "Smex is a M-x enhancement for Emacs.  Built on top of Ido, it provides a
convenient interface to your recently and most frequently used commands.  And
to all the other commands, too.")
    (license license:gpl3+)))

(define-public emacs-js2-mode
  (package
    (name "emacs-js2-mode")
    (version "20150909")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mooz/js2-mode/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nsm36c4kwb473p13i58fgrnlk8fbn3rdhj47d9xz70az4ra44q0"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/mooz/js2-mode/")
    (synopsis "Improved JavaScript editing mode for Emacs")
    (description
     "Js2-mode provides a JavaScript major mode for Emacs that is more
advanced than the built-in javascript-mode.  Features include accurate syntax
highlighting using a recursive-descent parser, on-the-fly reporting of syntax
errors and strict-mode warnings, smart line-wrapping within comments and
strings, and code folding.")
    (license license:gpl3+)))

(define-public emacs-markdown-mode
  (package
    (name "emacs-markdown-mode")
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com/jrblevin"
                                  "/markdown-mode/v" version
                                  "/markdown-mode.el"))
              (file-name (string-append "markdown-mode-" version ".el"))
              (sha256
               (base32
                "152whyrq3dqlqy5wv4mdd94kmal19hs5kwaxjcp2gp2r97lsmdmi"))))
    (build-system emacs-build-system)
    (home-page "http://jblevins.org/projects/markdown-mode/")
    (synopsis "Emacs Major mode for Markdown files")
    (description
     "Markdown-mode is a major mode for editing Markdown-formatted text files
in Emacs.")
    (license license:gpl3+)))

(define-public emacs-edit-indirect
  (package
    (name "emacs-edit-indirect")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Fanael/edit-indirect/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07kr58rd1p5j764wminsssazr73hy51yw8iqcsv5z2dwgj7msv71"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/edit-indirect")
    (synopsis "Edit regions in separate buffers")
    (description "This package allows you to edit regions in separate buffers,
like @code{org-edit-src-code} but for arbitrary regions.")
    (license license:gpl3+)))

(define-public emacs-projectile
  (package
    (name "emacs-projectile")
    (version "0.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com/bbatsov"
                                  "/projectile/v" version "/projectile.el"))
              (file-name (string-append "projectile-" version ".el"))
              (sha256
               (base32
                "1ql1wnzhblbwnv66hf2y0wq45g71hh6s9inc090lmhm1vgylbd1f"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-pkg-info" ,emacs-pkg-info)))
    (home-page "https://github.com/bbatsov/projectile")
    (synopsis "Manage and navigate projects in Emacs easily")
    (description
     "This library provides easy project management and navigation.  The
concept of a project is pretty basic - just a folder containing special file.
Currently git, mercurial and bazaar repos are considered projects by default.
If you want to mark a folder manually as a project just create an empty
.projectile file in it.")
    (license license:gpl3+)))

(define-public emacs-elfeed
  (package
    (name "emacs-elfeed")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/skeeto/elfeed/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0d7i93l3b0ck3iad9ddqp7sqa8w16hnamrby8bwvl316rqk4lzlf"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda _
             (zero? (system* "make" "test")))))))
    (home-page "https://github.com/skeeto/elfeed")
    (synopsis "Atom/RSS feed reader for Emacs")
    (description
     "Elfeed is an extensible web feed reader for Emacs, supporting both Atom
and RSS, with a user interface inspired by notmuch.")
    (license license:gpl3+)))

(define-public emacs-rainbow-delimiters
  (package
    (name "emacs-rainbow-delimiters")
    (version "2.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com/Fanael"
                                  "/rainbow-delimiters/" version
                                  "/rainbow-delimiters.el"))
              (file-name (string-append "rainbow-delimiters-" version ".el"))
              (sha256
               (base32
                "1b3kampwsjabhcqdp0khgff13wc5jqhy3rbvaa12vnv7qy22l9ck"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/rainbow-delimiters")
    (synopsis "Highlight brackets according to their depth")
    (description
     "Rainbow-delimiters is a \"rainbow parentheses\"-like mode for Emacs which
highlights parentheses, brackets, and braces according to their depth.  Each
successive level is highlighted in a different color, making it easy to spot
matching delimiters, orient yourself in the code, and tell which statements
are at a given level.")
    (license license:gpl3+)))

(define-public emacs-rainbow-identifiers
  (package
    (name "emacs-rainbow-identifiers")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com/Fanael"
                                  "/rainbow-identifiers/" version
                                  "/rainbow-identifiers.el"))
              (file-name (string-append "rainbow-identifiers-" version ".el"))
              (sha256
               (base32
                "0325abxj47k0g1i8nqrq70w2wr6060ckhhf92krv1s072b3jzm31"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/rainbow-identifiers")
    (synopsis "Highlight identifiers in source code")
    (description
     "Rainbow identifiers mode is an Emacs minor mode providing highlighting of
identifiers based on their names.  Each identifier gets a color based on a hash
of its name.")
    (license license:bsd-2)))

(define-public emacs-rainbow-mode
  (package
    (name "emacs-rainbow-mode")
    (version "0.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://elpa.gnu.org/packages/rainbow-mode-" version ".el"))
              (sha256
               (base32
                "1d3aamx6qgqqpqijwsr02ggwrh67gfink1bir0692alfkm3zdddl"))))
    (build-system emacs-build-system)
    (home-page "http://elpa.gnu.org/packages/rainbow-mode.html")
    (synopsis "Colorize color names in buffers")
    (description
     "This minor mode sets background color to strings that match color
names, e.g. #0000ff is displayed in white with a blue background.")
    (license license:gpl3+)))

(define-public emacs-visual-fill-column
  (package
    (name "emacs-visual-fill-column")
    (version "1.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://codeload.github.com/joostkremers/"
                                  "visual-fill-column/tar.gz/" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13jnviakp607zcms7f8ams56mr8wffnq1pghlc6fvqs39663pgwh"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/joostkremers/visual-fill-column")
    (synopsis "Fill-column for visual-line-mode")
    (description
     "@code{visual-fill-column-mode} is a small Emacs minor mode that mimics
the effect of @code{fill-column} in @code{visual-line-mode}.  Instead of
wrapping lines at the window edge, which is the standard behaviour of
@code{visual-line-mode}, it wraps lines at @code{fill-column}.  If
@code{fill-column} is too large for the window, the text is wrapped at the
window edge.")
    (license license:gpl3+)))

(define-public emacs-writeroom
  (package
    (name "emacs-writeroom")
    (version "3.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/joostkremers/writeroom-mode/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0yqgp5h3kvvpgva4azakb2wnjl7gsyh45glf75crspv3xyq57f2r"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-visual-fill-column" ,emacs-visual-fill-column)))
    (home-page "https://github.com/joostkremers/writeroom-mode")
    (synopsis "Distraction-free writing for Emacs")
    (description
     "This package defines a minor mode for distraction-free writing.  Some of
the default effects include entering fullscreen, deleting other windows of the
current frame, disabling the mode line, and adding margins to the buffer that
restrict the text width to 80 characters.")
    (license license:bsd-3)))

(define-public emacs-ido-completing-read+
  (package
    (name "emacs-ido-completing-read+")
    (version "3.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com"
                                  "/DarwinAwardWinner/ido-ubiquitous/v"
                                  version "/ido-completing-read+.el"))
              (file-name (string-append "ido-completing-read+-" version ".el"))
              (sha256
               (base32
                "1cyalb0p7nfsm4n6n9q6rjmvn6adqc0fq8ybnlj3n41n289dkfjf"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/DarwinAwardWinner/ido-ubiquitous")
    (synopsis "Replacement for completing-read using ido")
    (description
     "The ido-completing-read+ function is a wrapper for ido-completing-read.
Importantly, it detects edge cases that ordinary ido cannot handle and either
adjusts them so ido can handle them, or else simply falls back to the standard
Emacs completion function instead.")
    (license license:gpl3+)))

(define-public emacs-ido-ubiquitous
  (package
    (name "emacs-ido-ubiquitous")
    (version "3.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com"
                                  "/DarwinAwardWinner/ido-ubiquitous/v"
                                  version "/ido-ubiquitous.el"))
              (file-name (string-append "ido-ubiquitous-" version ".el"))
              (sha256
               (base32
                "197ypji0fb6jsdcq40rpnknwlh3imas6s6jbsvkfm0pz9988c3q2"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-ido-completing-read+" ,emacs-ido-completing-read+)))
    (home-page "https://github.com/DarwinAwardWinner/ido-ubiquitous")
    (synopsis "Use ido (nearly) everywhere")
    (description
     "Ido-ubiquitous enables ido-style completion for almost every function
that uses the standard completion function completing-read.")
  (license license:gpl3+)))

(define-public emacs-yaml-mode
  (package
    (name "emacs-yaml-mode")
    (version "0.0.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com/yoshiki"
                                  "/yaml-mode/v" version "/yaml-mode.el"))
              (file-name (string-append "yaml-mode-" version ".el"))
              (sha256
               (base32
                "0im88sk9dqw03x6d6zaspgvg9i0pfpgb8f2zygrmbifh2w4pwmvj"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/yoshiki/yaml-mode")
    (synopsis "Major mode for editing YAML files")
    (description
     "Yaml-mode is an Emacs major mode for editing files in the YAML data
serialization format.  It was initially developed by Yoshiki Kurihara and many
features were added by Marshall Vandegrift.  As YAML and Python share the fact
that indentation determines structure, this mode provides indentation and
indentation command behavior very similar to that of python-mode.")
    (license license:gpl3+)))

(define-public emacs-web-mode
  (package
    (name "emacs-web-mode")
    (version "14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com/fxbois"
                                  "/web-mode/v" version "/web-mode.el"))
              (file-name (string-append "web-mode-" version ".el"))
              (sha256
               (base32
                "086hik5fmxg3kx74qmransx9cz961qd22d4m6ah2dw6cwaj1s3s5"))))
    (build-system emacs-build-system)
    (synopsis "Major mode for editing web templates")
    (description "Web-mode is an Emacs major mode for editing web templates
aka HTML files embedding parts (CSS/JavaScript) and blocks (pre rendered by
client/server side engines).  Web-mode is compatible with many template
engines: PHP, JSP, ASP, Django, Twig, Jinja, Mustache, ERB, FreeMarker,
Velocity, Cheetah, Smarty, CTemplate, Mustache, Blade, ErlyDTL, Go Template,
Dust.js, React/JSX, Angularjs, ejs, etc.")
    (home-page "http://web-mode.org/")
    (license license:gpl3+)))

(define-public emacs-wgrep
  (package
    (name "emacs-wgrep")
    (version "2.1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/mhayashi1120/Emacs-wgrep/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r2bpypar70xg6dsx12x1k74f39ww930rday7rgqpyknzsx1k4l1"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/mhayashi1120/Emacs-wgrep")
    (synopsis "Edit a grep buffer and apply those changes to the files")
    (description
     "Emacs wgrep allows you to edit a grep buffer and apply those changes to
the file buffer.")
    (license license:gpl3+)))

(define-public emacs-helm
  (package
    (name "emacs-helm")
    (version "2.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/" name "/helm/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15xlnjm9rsbn0xq7xc09y52h2kn41zwn7ldryammf6i46wl02kq3"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-async" ,emacs-async)
       ("emacs-popup" ,emacs-popup)))
    (home-page "https://emacs-helm.github.io/helm/")
    (synopsis "Incremental completion and selection narrowing
framework for Emacs")
    (description "Helm is incremental completion and selection narrowing
framework for Emacs.  It will help steer you in the right direction when
you're looking for stuff in Emacs (like buffers, files, etc).  Helm is a fork
of @code{anything.el} originally written by Tamas Patrovic and can be
considered to be its successor.  Helm sets out to clean up the legacy code in
@code{anything.el} and provide a cleaner, leaner and more modular tool, that's
not tied in the trap of backward compatibility.")
    (license license:gpl3+)))

(define-public emacs-helm-swoop
  (package
    (name "emacs-helm-swoop")
    (version "1.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ShingoFukuyama/helm-swoop/archive/"
                    version
                    ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1z34pfi0gsk054pxr906ilaalaw0xz3s536163gf9ykkwmc2356d"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-helm" ,emacs-helm)))
    (home-page "https://github.com/ShingoFukuyama/helm-swoop")
    (synopsis "Filter and jump to lines in an Emacs buffer using Helm")
    (description
     "This package builds on the Helm interface to provide several commands
for search-based navigation of buffers.")
    (license license:gpl2+)))

(define-public emacs-helm-projectile
  (package
    (name "emacs-helm-projectile")
    (version "0.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/bbatsov/helm-projectile/archive/v"
                    version
                    ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19cfmilqh8kbab3b2hmx6lyrj73q6vfmn3p730x95g23iz16mnd5"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-helm" ,emacs-helm)
       ("emacs-projectile" ,emacs-projectile)))
    (home-page "https://github.com/bbatsov/helm-projectile")
    (synopsis "Helm integration for Projectile")
    (description
     "This Emacs library provides a Helm interface for Projectile.")
    (license license:gpl3+)))

(define-public emacs-helm-make
  (let ((commit "feae8df22bc4b20705ea08ac9adfc2b43bb348d0")
        (revision "1"))
    (package
      (name "emacs-helm-make")
      (version (string-append "0.1.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/abo-abo/helm-make.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1y2v77mmd1bfkkz51cnk1l0dg3lvvxc39wlamnm7wjns66dbvlam"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)
         ("emacs-projectile" ,emacs-projectile)))
      (home-page "https://github.com/abo-abo/helm-make")
      (synopsis "Select a Makefile target with helm")
      (description "@code{helm-make} or @code{helm-make-projectile} will give
you a @code{helm} selection of directory Makefile's targets.  Selecting a
target will call @code{compile} on it.")
      (license license:gpl3+))))

(define-public emacs-cider
  (package
    (name "emacs-cider")
    (version "0.15.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/clojure-emacs/cider/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1j5hlmi14ypszv1f9nvq0jjlz7i742flg0ny3055l7i4x089xx6g"))))
    (build-system emacs-build-system)
    (arguments
     '(#:exclude                        ; Don't exclude 'cider-test.el'.
       '("^\\.dir-locals\\.el$" "^test/")))
    (propagated-inputs
     `(("emacs-clojure-mode" ,emacs-clojure-mode)
       ("emacs-spinner" ,emacs-spinner)
       ("emacs-pkg-info" ,emacs-pkg-info)
       ("emacs-queue" ,emacs-queue)))
    (home-page "https://cider.readthedocs.org/")
    (synopsis "Clojure development environment for Emacs")
    (description
     "CIDER (Clojure Interactive Development Environment that Rocks) aims to
provide an interactive development experience similar to the one you'd get
when programming in Emacs Lisp, Common Lisp (with SLIME or Sly), Scheme (with
Geiser) and Smalltalk.

CIDER is the successor to the now deprecated combination of using SLIME +
swank-clojure for Clojure development.

There are plenty of differences between CIDER and SLIME, but the core ideas
are pretty much the same (and SLIME served as the principle inspiration for
CIDER).")
    (license license:gpl3+)))

;; There hasn't been a tag or release since 2015, so we take the latest
;; commit.
(define-public emacs-sly
  (let ((commit "486bfbe95612bcdc0960c490207970a188e0fbb9")
        (revision "1"))
    (package
      (name "emacs-sly")
      (version (string-append "1.0.0-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/joaotavora/sly.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0ib4q4k3h3qn88pymyjjmlmnpizdn1mfg5gpk5a715nqsgxlg09l"))))
      (build-system emacs-build-system)
      (arguments
       `(#:include (cons "^lib\\/" %default-include)
         #:phases
         ;; The package provides autoloads.
         (modify-phases %standard-phases
           (delete 'make-autoloads))))
      (home-page "https://github.com/joaotavora/sly")
      (synopsis "Sylvester the Cat's Common Lisp IDE")
      (description
       "SLY is Sylvester the Cat's Common Lisp IDE.  SLY is a fork of SLIME, and
contains the following improvements over it:

@enumerate
@item Completely redesigned REPL based on Emacs's own full-featured
  @code{comint.el}
@item Live code annotations via a new @code{sly-stickers} contrib
@item Consistent interactive button interface.  Everything can be copied to
  the REPL.
@item Multiple inspectors with independent history
@item Regexp-capable @code{M-x sly-apropos}
@item Contribs are first class SLY citizens and enabled by default
@item Use ASDF to loads contribs on demand.
@end enumerate

SLY tracks SLIME's bugfixes and all its familar features (debugger, inspector,
xref, etc...) are still available, but with better integration.")
      (license license:gpl3+))))

(define-public emacs-lua-mode
  (let ((commit "652e299cb967fccca827dda381d61a9c144d97de")
        (revision "1"))
    (package
      (name "emacs-lua-mode")
      (version (string-append "20151025." revision "-" (string-take commit 9)))
      (home-page "https://github.com/immerrr/lua-mode/")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (file-name (string-append name "-" version ".checkout"))
                (sha256
                 (base32
                  "053025k930wh0lak6rc1973ynfrmm8zsyzfqhhd39x7abkl41hc9"))))
      (build-system emacs-build-system)
      (synopsis "Major mode for lua")
      (description
       "This Emacs package provides a mode for @uref{https://www.lua.org/,
Lua programing language}.")
      (license license:gpl2+))))

(define-public emacs-ebuild-mode
  (package
    (name "emacs-ebuild-mode")
    (version "1.37")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dev.gentoo.org/~ulm/emacs/ebuild-mode"
                    "-" version ".tar.xz"))
              (file-name (string-append name "-" version ".tar.xz"))
              (sha256
               (base32
                "07dzrdjjczkxdfdgi60h4jjkvzi4p0k9rij2wpfp8s03ay3qldpp"))))
    (build-system emacs-build-system)
    (home-page "https://devmanual.gentoo.org")
    (synopsis "Major modes for Gentoo package files")
    (description
     "This Emacs package provides modes for ebuild, eclass, eblit, GLEP42
news items, openrc and runscripts.")
    (license license:gpl2+)))

(define-public emacs-evil
  (package
    (name "emacs-evil")
    (version "1.2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/emacs-evil/evil/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1z63zsxmsc6mh74wv6065carwqmgs7b7lz5044s12xvgsjfbwi8h"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-undo-tree" ,emacs-undo-tree)
       ("emacs-goto-chg" ,emacs-goto-chg)))
    (home-page "https://github.com/emacs-evil/evil")
    (synopsis "Extensible Vi layer for Emacs")
    (description
     "Evil is an extensible vi layer for Emacs.  It emulates the
main features of Vim, and provides facilities for writing custom
extensions.")
    (license license:gpl3+)))

(define-public emacs-goto-chg
  (package
    (name "emacs-goto-chg")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       ;; There is no versioned source.
       (uri "https://www.emacswiki.org/emacs/download/goto-chg.el")
       (file-name (string-append "goto-chg-" version ".el"))
       (sha256
        (base32
         "078d6p4br5vips7b9x4v6cy0wxf6m5ij9gpqd4g33bryn22gnpij"))))
    (build-system emacs-build-system)
    ;; There is no other home page.
    (home-page "https://www.emacswiki.org/emacs/goto-chg.el")
    (synopsis "Go to the last change in the Emacs buffer")
    (description
     "This package provides @code{M-x goto-last-change} command that goes to
the point of the most recent edit in the current Emacs buffer.  When repeated,
go to the second most recent edit, etc.  Negative argument, @kbd{C-u -}, is
used for reverse direction.")
    (license license:gpl2+)))

(define-public emacs-monroe
  (package
    (name "emacs-monroe")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/sanel/monroe/archive/"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0icdx8shkd951phlnmcq1vqaxp1l667q5rjscskc5r22aylakh4w"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/sanel/monroe")
    (synopsis "Clojure nREPL client for Emacs")
    (description
      "Monroe is a nREPL client for Emacs, focused on simplicity and easy
distribution, primarily targeting Clojure users")
    (license license:gpl3+)))

(define-public emacs-writegood-mode
  (package
    (name "emacs-writegood-mode")
    (version "2.0.2")
    (home-page "https://github.com/bnbeckwith/writegood-mode")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1nnjn1r669hvvzfycllwap4w04m8rfsk4nzcg8057m1f263kj31b"))
              (file-name (string-append name "-checkout"))))
    (build-system emacs-build-system)
    (synopsis "Polish up poor writing on the fly")
    (description
     "This minor mode tries to find and highlight problems with your writing
in English as you type.  It primarily detects \"weasel words\" and abuse of
passive voice.")
    (license license:gpl3+)))

(define-public emacs-neotree
  (package
    (name "emacs-neotree")
    (version "0.5.2")
    (home-page "https://github.com/jaypei/emacs-neotree")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/jaypei/" name
                    "/archive/" version ".tar.gz"))
              (sha256
               (base32
                "1zd6dchwyijnf7kgchfcp51gs938l204dk9z6mljrfqf2zy0gp12"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system emacs-build-system)
    (synopsis "Folder tree view for Emacs")
    (description "This Emacs package provides a folder tree view.")
    (license license:gpl3+)))

(define-public emacs-org
  (package
    (name "emacs-org")
    ;; emacs-org-contrib inherits from this package.  Please update its sha256
    ;; checksum as well.
    (version "20180226")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://orgmode.org/elpa/org-"
                                  version ".tar"))
              (sha256
               (base32
                "0jqvry6gah1bwnryha4asynj13jyds3qim0xcy7s01rxk99m2ziy"))))
    (build-system emacs-build-system)
    (home-page "https://orgmode.org/")
    (synopsis "Outline-based notes management and organizer")
    (description "Org is an Emacs mode for keeping notes, maintaining TODO
lists, and project planning with a fast and effective plain-text system.  It
also is an authoring system with unique support for literate programming and
reproducible research.")
    (license license:gpl3+)))

(define-public emacs-org-contrib
  (package
    (inherit emacs-org)
    (name "emacs-org-contrib")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://orgmode.org/elpa/org-plus-contrib-"
                                  (package-version emacs-org) ".tar"))
              (sha256
               (base32
                "034wp70hcqnpidji5k1k80mj35iyyy098nbvc2sl7i2aca4m03zc"))))
    (arguments
     `(#:modules ((guix build emacs-build-system)
                  (guix build utils)
                  (guix build emacs-utils)
                  (ice-9 ftw)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'delete-org-files
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (org (assoc-ref inputs "emacs-org"))
                    (contrib-files
                     (map basename (find-files out)))
                    (org+contrib-files
                     (map basename (find-files org)))
                    (duplicates (lset-intersection
                                 string=? contrib-files org+contrib-files)))
               (with-directory-excursion
                   (string-append
                    out "/share/emacs/site-lisp/guix.d/org-contrib-"
                    ,(package-version emacs-org))
                 (for-each delete-file duplicates))
               #t))))))
    (propagated-inputs
     `(("emacs-org" ,emacs-org)))
    (synopsis "Contributed packages to Org mode")
    (description "Org is an Emacs mode for keeping notes, maintaining TODO
lists, and project planning with a fast and effective plain-text system.

This package is equivalent to org-plus-contrib, but only includes additional
files that you would find in @file{contrib/} from the git repository.")))

(define-public emacs-flx
  (package
    (name "emacs-flx")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lewang/"
                           "flx/archive/v" version ".tar.gz"))
       (sha256
        (base32
         "0bkcpnf1j4i2fcc2rllwbz62l00sw2mcia6rm5amgwvlkqavmkv6"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/lewang/flx")
    (synopsis "Fuzzy matching for Emacs")
    (description
     "Flx provides fuzzy matching for emacs a la sublime text.
The sorting algorithm is a balance between word beginnings (abbreviation)
and contiguous matches (substring).  The longer the substring match,
the higher it scores. This maps well to how we think about matching.
Flx has support for ido (interactively do things) through flx-ido.")
    (license license:gpl3+)))

(define-public emacs-cyberpunk-theme
  (package
    (name "emacs-cyberpunk-theme")
    (version "1.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/n3mo/cyberpunk-theme.el/"
                           "archive/" version ".tar.gz"))
       (sha256
        (base32
         "05l5fxw1mn5py6mfhxrzyqjq0d8m5m1akfi46vrgh13r414jffvv"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/n3mo/cyberpunk-theme.el")
    (synopsis "Cyberpunk theme for emacs built-in color theme support")
    (description
     "Cyberpunk color theme for the emacs 24+ built-in color theme support
known loosely as deftheme.  Many mode-specific customizations are included.")
    (license license:gpl3+)))

(define-public emacs-danneskjold-theme
  (let* ((commit "8733d2fe8743e8a01826ea6d4430ef376c727e57")
         (revision "1"))
    (package
      (name "emacs-danneskjold-theme")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (home-page "https://github.com/rails-to-cosmos/danneskjold-theme")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0s6rbsb0y8i8m5b9xm4gw1p1cxsxdqnqxqqb638pygz9f76mbir1"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'delete-screenshots
             (lambda _
               (delete-file-recursively "screenshots") #t)))))
      (synopsis "High-contrast Emacs theme")
      (description
       "@code{danneskjold-theme} is a high-contrast theme for Emacs.")
      (license license:gpl3+))))

(define-public emacs-dream-theme
  (let* ((commit "107a11d74365046f28a1802a2bdb5e69e4a7488b")
         (revision "1"))
    (package
      (name "emacs-dream-theme")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/djcb/dream-theme")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0za18nfkq4xqm35k6006vsixcbmvmxqgma4iw5sw37h8vmcsdylk"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/djcb/dream-theme")
      (synopsis "High-contrast Emacs theme")
      (description
       "@code{dream-theme} is a dark, clean theme for Emacs.  It is inspired
by zenburn, sinburn and similar themes, but slowly diverging from them.")
      (license license:gpl3+))))

(define-public emacs-auto-complete
  (package
    (name "emacs-auto-complete")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/auto-complete/"
                           "auto-complete/archive/v" version ".tar.gz"))
       (sha256
        (base32
         "1jvq4lj00hwml75lpmlciazy8f3bbg13gffsfnl835p4qd8l7yqv"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-popup" ,emacs-popup)))
    (home-page "https://github.com/auto-complete/auto-complete")
    (synopsis "Intelligent auto-completion extension for Emacs")
    (description
     "Auto-Complete is an intelligent auto-completion extension for Emacs.
It extends the standard Emacs completion interface and provides an environment
that allows users to concentrate more on their own work.  Its features are:
a visual interface, reduce overhead of completion by using statistic method,
extensibility.")
    (license license:gpl3+)))

(define-public m17n-db
  (package
    (name "m17n-db")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/m17n/m17n-db-"
                           version ".tar.gz"))
       (sha256
        (base32 "1w08hnsbknrcjlzp42c99bgwc9hzsnf5m4apdv0dacql2s09zfm2"))))
    (build-system gnu-build-system)
    (inputs
     `(("gettext" ,gettext-minimal)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-charmaps="
                            (assoc-ref %build-inputs "libc")
                            "/share/i18n/charmaps"))))
    ;; With `guix lint' the home-page URI returns a small page saying
    ;; that your browser does not handle frames. This triggers the "URI
    ;; returns suspiciously small file" warning.
    (home-page "https://www.nongnu.org/m17n/")
    (synopsis "Multilingual text processing library (database)")
    (description "The m17n library realizes multilingualization of
many aspects of applications.  The m17n library represents
multilingual text as an object named M-text.  M-text is a string with
attributes called text properties, and designed to substitute for
string in C.  Text properties carry any information required to input,
display and edit the text.

This package contains the library database.")
    (license license:lgpl2.1+)))

(define-public m17n-lib
  (package
    (name "m17n-lib")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/m17n/m17n-lib-"
                           version ".tar.gz"))
       (sha256
        (base32 "10yv730i25g1rpzv6q49m6xn4p8fjm7jdwvik2h70sn8w3hm7f4f"))))
    (build-system gnu-build-system)
    (inputs
     `(("fribidi" ,fribidi)
       ("gd" ,gd)
       ("libotf" ,libotf)
       ("libxft" ,libxft)
       ("libxml2" ,libxml2)
       ("m17n-db" ,m17n-db)))
    (arguments
     `(#:parallel-build? #f))
    ;; With `guix lint' the home-page URI returns a small page saying
    ;; that your browser does not handle frames. This triggers the "URI
    ;; returns suspiciously small file" warning.
    (home-page "https://www.nongnu.org/m17n/")
    (synopsis "Multilingual text processing library (runtime)")
    (description "The m17n library realizes multilingualization of
many aspects of applications.  The m17n library represents
multilingual text as an object named M-text.  M-text is a string with
attributes called text properties, and designed to substitute for
string in C.  Text properties carry any information required to input,
display and edit the text.

This package contains the library runtime.")
    (license license:lgpl2.1+)))

(define-public emacs-nginx-mode
  (package
    (name "emacs-nginx-mode")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ajc/nginx-mode/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1lvkj07kq0jkskr2f61vqb5rlrbnaz9a76ikq40w6925i2r970rr"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ajc/nginx-mode")
    (synopsis "Emacs major mode for editing nginx config files")
    (description "This package provides an Emacs major mode for
editing nginx config files.")
    (license license:gpl2+)))

(define-public emacs-stream
  (package
    (name "emacs-stream")
    (version "2.2.0")
    (home-page "https://github.com/NicolasPetton/stream")
    (source
     (origin
       (method url-fetch)
       (file-name (string-append name "-" version ".tar.gz"))
       (uri (string-append home-page "/archive/"version ".tar.gz"))
       (sha256
        (base32 "03ql4nqfz5pn55mjly6clhvc3g7x2d28kj7mrlqmigvjbql39xxc"))))
    (build-system emacs-build-system)
    (synopsis "Implementation of streams for Emacs")
    (description "This library provides an implementation of streams for Emacs.
Streams are implemented as delayed evaluation of cons cells.")
    (license license:gpl3+)))

(define-public emacs-el-search
  (let ((commit "f26277bfbb3fc3fc74beea6592f294c439796bd4")
        (revision "1"))
    (package
      (name "emacs-el-search")
      ;; No ufficial release.
      (version (string-append "0.0-" revision "." (string-take commit 7)))
      (home-page "https://github.com/emacsmirror/el-search")
      (source
       (origin
         (method git-fetch)
         (file-name (string-append name "-" version ".tar.gz"))
         (uri (git-reference
               (commit commit)
               (url (string-append home-page ".git"))))
         (sha256
          (base32 "12xf40h9sb7xxg2r97gsia94q02543mgiiiw46fzh1ac7b7993g6"))))
      (build-system emacs-build-system)
      (inputs `(("emacs-stream" ,emacs-stream)))
      (synopsis "Expression based interactive search for emacs-lisp-mode")
      (description "This package provides expression based interactive search
procedures for emacs-lisp-mode.")
      (license license:gpl3+))))

(define-public emacs-ht
  (package
    (name "emacs-ht")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Wilfred/ht.el/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1lpba36kzxcc966fvsbrfpy8ah9gnvay0yk26gbyjil0rggrbqzj"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-dash" ,emacs-dash)))
    (home-page "https://github.com/Wilfred/ht.el")
    (synopsis "Hash table library for Emacs")
    (description
     "This package simplifies the use of hash tables in elisp.  It also
provides functions to convert hash tables from and to alists and plists.")
    (license license:gpl3+)))

(define-public emacs-log4e
  (package
    (name "emacs-log4e")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/aki2o/log4e/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nbdpbw353snda3v19l9hsm6gimppwnpxj18amm350bm81lyim2g"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-tests
           ;; Guile builder complains about null characters in some
           ;; strings of test files.  Remove "test" directory (it is not
           ;; needed anyway).
           (lambda _
             (delete-file-recursively "test"))))))
    (home-page "https://github.com/aki2o/log4e")
    (synopsis "Logging framework for elisp")
    (description
     "This package provides a logging framework for elisp.  It allows
you to deal with multiple log levels.")
    (license license:gpl3+)))

(define-public emacs-gntp
  (package
    (name "emacs-gntp")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/tekai/gntp.el/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "16c1dfkia9yhl206bdhjr3b8kfvqcqr38jl5lq8qsyrrzsnmghny"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/tekai/gntp.el")
    (synopsis "Growl Notification Protocol for Emacs")
    (description
     "This package implements the Growl Notification Protocol GNTP
described at @uref{http://www.growlforwindows.com/gfw/help/gntp.aspx}.
It is incomplete as it only lets you send but not receive
notifications.")
    (license license:bsd-3)))

(define-public emacs-alert
  (package
    (name "emacs-alert")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jwiegley/alert/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1693kck3k2iz5zhpmxwqyafxm68hr6gzs60lkxd3j1wlp2c9fwyr"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-gntp" ,emacs-gntp)
       ("emacs-log4e" ,emacs-log4e)))
    (home-page "https://github.com/jwiegley/alert")
    (synopsis "Growl-style notification system for Emacs")
    (description
     "Alert is a Growl-workalike for Emacs which uses a common notification
interface and multiple, selectable \"styles\", whose use is fully
customizable by the user.")
    (license license:gpl2+)))

(define-public emacs-mu4e-alert
  (package
    (name "emacs-mu4e-alert")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/iqbalansari/mu4e-alert/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07qc834qnxn8xi4bw5nawj8g91bmkzw0r0vahkgysp7r9xrf57gj"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-alert" ,emacs-alert)
       ("emacs-s" ,emacs-s)
       ("emacs-ht" ,emacs-ht)))
    (home-page "https://github.com/iqbalansari/mu4e-alert")
    (synopsis "Desktop notification for mu4e")
    (description
     "This package provides desktop notifications for mu4e.
Additionally it can display the number of unread emails in the
mode-line.")
    (license license:gpl3+)))

(define-public emacs-pretty-mode
  (package
    (name "emacs-pretty-mode")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/akatov/pretty-mode/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fan7m4vnqs8kpg7r54kx3g7faadkpkf9kzarfv8n57kq8w157pl"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/akatov/pretty-mode")
    (synopsis "Redisplay parts of the buffer as Unicode symbols")
    (description
     "Emacs minor mode for redisplaying parts of the buffer as pretty symbols.")
    (license license:gpl3+)))

(define-public emacs-yasnippet
  (package
    (name "emacs-yasnippet")
    (version "0.12.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/joaotavora/yasnippet/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "01jabaz0g67bsziayrxgv74px55fx4nlrcl0csl8f1by2102pwc5"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; YASnippet expects a "snippets" subdirectory in the same
                  ;; directory as yasnippet.el, but we don't install it
                  ;; because it's a git submodule pointing to an external
                  ;; repository.  Adjust `yas-snippet-dirs' to prevent
                  ;; warnings about a missing directory.
                  (substitute* "yasnippet.el"
                    (("^ +'yas-installed-snippets-dir\\)\\)\n")
                     "))\n"))))))
    (build-system emacs-build-system)
    (home-page "https://github.com/joaotavora/yasnippet")
    (synopsis "Yet another snippet extension for Emacs")
    (description
     "YASnippet is a template system for Emacs.  It allows you to type an
abbreviation and automatically expand it into function templates.")
    (license license:gpl3+)))

(define-public emacs-yasnippet-snippets
  (let ((commit "885050d34737e2fb36a3e7759d60c09347bd4ce0")
        (revision "1"))
    (package
      (name "emacs-yasnippet-snippets")
      (version (string-append "1-" revision "." (string-take commit 8)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AndreaCrotti/yasnippet-snippets")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1m935zgglw0iakzrixld5rcjz3wnj84f8wy2mvc3pggjri9l0qr9"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((ice-9 ftw)
                    (ice-9 regex)
                    (guix build utils))
         #:builder
         (begin
           (use-modules (ice-9 ftw)
                        (ice-9 regex)
                        (guix build utils))
           (with-directory-excursion (assoc-ref %build-inputs "source")
             (for-each (lambda (dir)
                         (copy-recursively
                          dir
                          (string-append %output
                                         "/share/emacs/yasnippet-snippets/"
                                         dir)))
                       (scandir "." (lambda (fname)
                                      (and (string-match "-mode$" fname)
                                           (directory-exists? fname)))))))))
      (home-page "https://github.com/AndreaCrotti/yasnippet-snippets")
      (synopsis "Collection of YASnippet snippets for many languages")
      (description
       "Provides Andrea Crotti's collection of YASnippet snippets.  After installation,
the snippets will be in \"~/.guix-profile/share/emacs/yasnippet-snippets/\".
To make YASnippet aware of these snippets, add the above directory to
@code{yas-snippet-dirs}.")
      (license license:expat))))

(define-public emacs-helm-c-yasnippet
  (let ((commit "65ca732b510bfc31636708aebcfe4d2d845b59b0")
        (revision "1"))
    (package
      (name "emacs-helm-c-yasnippet")
      (version (string-append "0.6.7" "-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/emacs-jp/helm-c-yasnippet")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1cbafjqlzxbg19xfdqsinsh7afq58gkf44rsg1qxfgm8g6zhr7f8"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)
         ("emacs-yasnippet" ,emacs-yasnippet)))
      (home-page "https://github.com/emacs-jp/helm-c-yasnippet")
      (synopsis "Helm integration for Yasnippet")
      (description "This Emacs library provides Helm interface for
Yasnippet.")
      (license license:gpl2+))))

(define-public emacs-memoize
  (package
   (name "emacs-memoize")
   (version "20130421.b55eab0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/skeeto/emacs-memoize")
           (commit "b55eab0cb6ab05d941e07b8c01f1655c0cf1dd75")))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "0fjwlrdm270qcrqffvarw5yhijk656q4lam79ybhaznzj0dq3xpw"))))
   (build-system emacs-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-before 'install 'check
                    (lambda _
                      (zero? (system* "emacs" "-batch" "-l" "memoize.el"
                                      "-l" "memoize-test.el"
                                      "-f" "ert-run-tests-batch-and-exit")))))))
   (home-page "https://github.com/skeeto/emacs-memoize")
   (synopsis "Emacs lisp memoization library")
   (description "@code{emacs-memoize} is an Emacs library for
memoizing functions.")
   (license license:unlicense)))

(define-public emacs-linum-relative
  (package
    (name "emacs-linum-relative")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/coldnew/linum-relative/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0s4frvr27866lw1rn3jal9wj5rkz9fx4yiszqv7w06azsdgsqksv"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/coldnew/linum-relative")
    (synopsis "Relative line numbering for Emacs")
    (description "@code{emacs-linum-relative} displays the relative line
number on the left margin in Emacs.")
    (license license:gpl2+)))

(define-public emacs-idle-highlight
  (package
    (name "emacs-idle-highlight")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/nonsequitur/idle-highlight-mode/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0kdv10hrgqpskjh0zvpnzwlkn5bccnqxas62gkws6njln57bf8nl"))))
    (build-system emacs-build-system)
    (home-page "https://www.emacswiki.org/emacs/IdleHighlight")
    (synopsis "Highlights all occurrences of the word the point is on")
    (description
     "This Emacs package provides @code{idle-highlight-mode} that sets
 an idle timer to highlight all occurrences in the buffer of the word under
 the point.")
    (license license:gpl3+)))

(define-public emacs-ox-twbs
  (package
    (name "emacs-ox-twbs")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/marsmining/ox-twbs/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zaq8dczq5wijjk36114k2x3hfrqig3lyx6djril6wyk67vczyqs"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/marsmining/ox-twbs")
    (synopsis "Export org-mode docs as HTML compatible with Twitter Bootstrap")
    (description
     "This Emacs package outputs your org-mode docs with a simple, clean and
modern look.  It implements a new HTML back-end for exporting org-mode docs as
HTML compatible with Twitter Bootstrap.  By default, HTML is exported with
jQuery and Bootstrap resources included via osscdn.")
    (license license:gpl3+)))

(define-public emacs-highlight-sexp
  (package
    (name "emacs-highlight-sexp")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/daimrod/highlight-sexp/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jwx87qkln1rg9wmv4qkgkml935fh2pkgrg5x4ca6n5dgb4q6rj1"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/daimrod/highlight-sexp")
    (synopsis "Minor mode that highlights the s-exp at the current position")
    (description
     "This Emacs package highlights the s-exp at the current position.")
    (license license:gpl3+)))

(define-public emacs-highlight-stages
  (let ((commit "29cbc5b78261916da042ddb107420083da49b271")
        (revision "1"))
    (package
      (name "emacs-highlight-stages")
      (version (string-append "1.1.0" "-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zk-phi/highlight-stages.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0r6nbcrr0dqpgm8dir8ahzjy7rw4nrac48byamzrq96r7ajlxlv0"))
         (patches
          (search-patches "emacs-highlight-stages-add-gexp.patch"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/wigust/highlight-stages")
      (synopsis "Minor mode that highlights (quasi-quoted) expressions")
      (description "@code{highlight-stages} provides an Emacs minor mode that
highlights quasi-quoted expressions.")
      (license license:gpl3+))))

(define-public emacspeak
  (package
    (name "emacspeak")
    (version "47.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/tvraman/emacspeak/releases/download/"
             version "/emacspeak-" version ".tar.bz2"))
       (sha256
        (base32
         "0xbcc266x752y68s3g096m161irzvsqym3axzqn8rb276a8x55n7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list (string-append "prefix="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             ;; Configure Emacspeak according to etc/install.org.
             (setenv "SHELL" (which "sh"))
             (zero? (system* "make" "config"))))
         (add-after 'build 'build-espeak
           (lambda _
             (zero? (system* "make" "espeak"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lisp (string-append out "/share/emacs/site-lisp/emacspeak"))
                    (info (string-append out "/share/info")))
               ;; According to etc/install.org, the Emacspeak directory should
               ;; be copied to its installation destination.
               (for-each
                (lambda (file)
                  (copy-recursively file (string-append lisp "/" file)))
                '("etc" "info" "lisp" "media" "servers" "sounds" "stumpwm"
                  "xsl"))
               ;; Make sure emacspeak is loaded from the correct directory.
               (substitute* "etc/emacspeak.sh"
                 (("exec emacs.*$")
                  (string-append "exec emacs -l " lisp
                                 "/lisp/emacspeak-setup.el $CL_ALL")))
               ;; Install the convenient startup script.
               (mkdir-p bin)
               (copy-file "etc/emacspeak.sh" (string-append bin "/emacspeak")))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (emacspeak (string-append out "/bin/emacspeak"))
                    (espeak (string-append (assoc-ref inputs "espeak")
                                           "/bin/espeak")))
               ;; The environment variable DTK_PROGRAM tells emacspeak what
               ;; program to use for speech.
               (wrap-program emacspeak
                 `("DTK_PROGRAM" ":" prefix (,espeak)))
               #t))))
       #:tests? #f)) ; no check target
    (inputs
     `(("espeak" ,espeak)
       ("tcl" ,tcl)
       ("tclx" ,tclx)))
    (native-inputs `(("emacs" ,emacs-minimal)))
    (home-page "http://emacspeak.sourceforge.net")
    (synopsis "Audio desktop interface for Emacs")
    (description
     "Emacspeak is a speech interface that allows visually impaired users to
interact independently and efficiently with the computer.  Audio formatting
--a technique pioneered by AsTeR-- and full support for W3C's Aural CSS (ACSS)
allows Emacspeak to produce rich aural presentations of electronic information.
By seamlessly blending all aspects of the Internet such as Web-surfing and
messaging, Emacspeak speech-enables local and remote information via a
consistent and well-integrated user interface.")
    (license license:gpl2+)))

(define-public emacs-adaptive-wrap
  (package
    (name "emacs-adaptive-wrap")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://elpa.gnu.org/packages/adaptive-wrap-"
                    version ".el"))
              (sha256
               (base32
                "0qi7gjprcpywk2daivnlavwsx53hl5wcqvpxbwinvigg42vxh3ll"))))
    (build-system emacs-build-system)
    (home-page "http://elpa.gnu.org/packages/adaptive-wrap.html")
    (synopsis "Smart line-wrapping with wrap-prefix")
    (description
     "This Emacs package provides the @code{adaptive-wrap-prefix-mode}
minor mode which sets the wrap-prefix property on the fly so that
single-long-line paragraphs get word-wrapped in a way similar to what
you'd get with @kbd{M-q} using @code{adaptive-fill-mode}, but without
actually changing the buffer's text.")
    (license license:gpl3+)))

(define-public emacs-diff-hl
 (package
  (name "emacs-diff-hl")
  (version "1.8.4")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "http://elpa.gnu.org/packages/diff-hl-"
                          version ".tar"))
      (sha256
        (base32
          "0axhidc3cym7a2x4rpxf4745qss9s9ajyg4s9h5b4zn7v7fyp71n"))))
  (build-system emacs-build-system)
  (home-page "https://github.com/dgutov/diff-hl")
  (synopsis
    "Highlight uncommitted changes using VC")
  (description
    "@code{diff-hl-mode} highlights uncommitted changes on the side of the
window (using the fringe, by default), allows you to jump between
the hunks and revert them selectively.")
  (license license:gpl3+)))

(define-public emacs-diminish
  (package
    (name "emacs-diminish")
    (version "0.45")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/myrjola/diminish.el/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0i3629sv5cfrrb00hcnmaqzgs8mk36yasc1ax3ry1ga09nr6rkj9"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/myrjola/diminish.el")
    (synopsis "Diminish minor modes with no modeline display")
    (description "@code{emacs-diminish} implements hiding or
abbreviation of the mode line displays (lighters) of minor modes.")
    (license license:gpl2+)))

(define-public emacs-use-package
  (package
    (name "emacs-use-package")
    (version "2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jwiegley/use-package/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0x4h136jb3imyli6zsh7dyzjrra6pv0v6b0yk94jdng3rdfcmsf5"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-diminish" ,emacs-diminish)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda _
             (zero? (system* "emacs" "--batch" "-L" "."
                             "-l" "use-package-tests.el"
                             "-f" "ert-run-tests-batch-and-exit"))
             ;; Tests fail in this release, but have been fixed in
             ;; upstream commit 7956d40eed57d6c06bef36ebc174cf57d934e30d
             #t)))))
    (home-page "https://github.com/jwiegley/use-package")
    (synopsis "Declaration for simplifying your .emacs")
    (description "The use-package macro allows you to isolate package
configuration in your @file{.emacs} file in a way that is both
performance-oriented and tidy.")
    (license license:gpl2+)))

(define-public emacs-strace-mode
  (let* ((commit "6a69b4b06db6797af56f33eda5cb28af94e59f11")
         (revision "1"))
    (package
      (name "emacs-strace-mode")
      (version (string-append "0.0.2-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pkmoore/strace-mode")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1lbk2kzdznf2bkfazizfbimaxxzfzv00lrz1ran9dc2zqbc0bj9f"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/pkmoore/strace-mode")
      (synopsis "Emacs major mode to highlight strace outputs")
      (description "@code{emacs-strace-mode} provides an Emacs major mode
 highlighting strace outputs.")
      (license license:gpl3+))))

(define-public emacs-default-encrypt
  (package
    (name "emacs-default-encrypt")
    (version "4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.informationelle-selbstbestimmung-im-internet.de"
             "/emacs/jl-encrypt" version "/jl-encrypt.el"))
       (file-name (string-append "jl-encrypt-" version ".el"))
       (sha256
        (base32
         "16i3rlfp3jxlqvndn8idylhmczync3gwmy8a019v29vyr48rnnr0"))))
    (build-system emacs-build-system)
    (home-page "https://www.informationelle-selbstbestimmung-im-internet.de/Emacs.html")
    (synopsis "Automatically encrypt or sign Gnus messages in Emacs")
    (description
     "DefaultEncrypt is designed to be used with Gnus in Emacs.  It
automatically encrypts messages that you send (e.g., email) when public keys
for all recipients are available, and it protects you from accidentally
sending un-encrypted messages.  It can also be configured to automatically
sign messages that you send.  For details and instructions on how to use
DefaultEncrypt, please refer to the home page or read the comments in the
source file, @file{jl-encrypt.el}.")
    (license license:gpl3+)))

(define-public emacs-htmlize
  (package
    (name "emacs-htmlize")
    (version "1.51")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/hniksic/emacs-htmlize/archive/release/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fy1lybzrxl8a8r88f6p19nz8ygmvcxhxbnymkxh7jqaz25viwld"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/hniksic/emacs-htmlize")
    (synopsis "Convert buffer text and decorations to HTML")
    (description "@code{emacs-htmlize} converts the buffer text and
the associated decorations to HTML.  Output to CSS, inline CSS and
fonts is supported.")
    (license license:gpl2+)))

(define-public emacs-xmlgen
  (package
    (name "emacs-xmlgen")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/philjackson/xmlgen/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0zay490vjby3f7455r0vydmjg7q1gwc78hilpfb0rg4gwz224z8r"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda _
             (zero? (system* "emacs" "--batch" "-L" "."
                             "-l" "xmlgen-test.el"
                             "-f" "ert-run-tests-batch-and-exit")))))))
    (home-page "https://github.com/philjackson/xmlgen")
    (synopsis "S-expression to XML domain specific language (DSL) in
Emacs Lisp")
    (description "@code{emacs-xmlgen} provides S-expression to XML
conversion for Emacs Lisp.")
    (license license:gpl2+)))

(define-public emacs-cdlatex
  (package
    (name "emacs-cdlatex")
    (version "4.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/cdominik/cdlatex/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0pivapphmykc6vhvpx7hdyl55ls37vc4jcrxpvs4yk7jzcmwa9xp"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/cdominik/cdlatex")
    (synopsis "Fast Emacs input methods for LaTeX environments and
math")
    (description "CDLaTeX is an Emacs minor mode supporting fast
insertion of environment templates and math in LaTeX.  Similar
commands are also offered as part of the AUCTeX package, but it is not
the same - CDLaTeX focuses on speediness for inserting LaTeX
constructs.")
    (license license:gpl3+)))

(define-public emacs-cnfonts
  (package
    (name "emacs-cnfonts")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/tumashu/cnfonts/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l6cgcvc6md1zq97ccczankpyi0k4vjx6apflny6kjq3p33lyhf4"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/tumashu/cnfonts")
    (synopsis "Emacs Chinese fonts setup tool")
    (description "cnfonts is a Chinese fonts setup tool, allowing for easy
configuration of Chinese fonts.")
    (license license:gpl2+)))

(define-public emacs-php-mode
  (package
    (name "emacs-php-mode")
    (version "20171225.342")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://melpa.org/packages/php-mode-"
                    version ".tar"))
              (sha256
               (base32
                "1zz682f34v4wsm2dyj1gnrnvsrqdq1cy7j8p6cvc398w2fziyg3q"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ejmr/php-mode")
    (synopsis "Major mode for editing PHP code")
    (description "@code{php-mode} is a major mode for editing PHP source
code.  It's an extension of C mode; thus it inherits all C mode's navigation
functionality.  But it colors according to the PHP grammar and indents
according to the PEAR coding guidelines.  It also includes a couple handy
IDE-type features such as documentation search and a source and class
browser.")
    (license license:gpl3+)))

(define-public emacs-pos-tip
  (package
    (name "emacs-pos-tip")
    (version "0.4.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/pitkali/pos-tip/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12jqfy26vjk7lq0aa8yn8zqj8c85fkvx7y9prj0pcn4wqiz2ad2r"))))
    (build-system emacs-build-system)
    ;; The following functions and variables needed by emacs-pos-tip are
    ;; not included in emacs-minimal:
    ;; x-display-pixel-width, x-display-pixel-height, x-show-tip
    (arguments `(#:emacs ,emacs))
    (home-page "https://github.com/pitkali/pos-tip")
    (synopsis "Show tooltip at point")
    (description "The standard library tooltip.el provides a function for
displaying a tooltip at the mouse position.  However, locating a tooltip at an
arbitrary buffer position in a window is not easy.  Pos-tip provides such a
function to be used by other frontend programs.")
    (license license:gpl2+)))

(define-public emacs-pyim-basedict
  (package
    (name "emacs-pyim-basedict")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/tumashu/pyim-basedict/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0nfgxviavkgrpyfsw60xsws4fk51fcmgl8fp6zf4ibqjjbp53n3n"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/tumashu/pyim-basedict")
    (synopsis "Input method dictionary of pyim")
    (description "Pyim-basedict is the default pinyin input method dictionary,
containing words from the rime project.")
    (license license:gpl2+)))

(define-public emacs-pyim
  (package
    (name "emacs-pyim")
    (version "1.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/tumashu/pyim/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hfg8q9hcjifvnlghw2g94dfxfirms2psq2ghqb28fhkf0lks13r"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-async" ,emacs-async)
       ("emacs-pyim-basedict" ,emacs-pyim-basedict)
       ("emacs-popup" ,emacs-popup)
       ("emacs-pos-tip" ,emacs-pos-tip)))
    (home-page "https://github.com/tumashu/pyim")
    (synopsis "Chinese input method")
    (description "Chinese input method which supports quanpin, shuangpin, wubi
and cangjie.")
    (license license:gpl2+)))

(define-public emacs-el2org
  (package
    (name "emacs-el2org")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/tumashu/el2org/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gd3km1swwvg2w0kdi7370f54wgrflxn63gjgssfjc1iyc9sbqwq"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/tumashu/el2org")
    (synopsis "Convert Emacs-lisp file to org file")
    (description "El2org is a simple tool, which can convert Emacs-lisp file
to org file, you can use this tool to write orgify commentary.")
    (license license:gpl2+)))

(define-public emacs-mustache
  (package
    (name "emacs-mustache")
    (version "0.23")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Wilfred/mustache.el/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k9lcgil7kykkv1ylrgwy1g13ldjjmgi2cwmysgyb2vlj3jbwpdj"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-ht" ,emacs-ht)
       ("emacs-s" ,emacs-s)))
    (home-page "https://github.com/Wilfred/mustache.el")
    (synopsis "Mustache templating library for Emacs")
    (description "Mustache templating library for Emacs, mustache is
a simple web template system, which is described as a logic-less system
because it lacks any explicit control flow statements, both looping and
conditional evaluation can be achieved using section tags processing lists
and lambdas.")
    (license license:gpl3+)))

(define-public emacs-org2web
  (package
    (name "emacs-org2web")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/tumashu/org2web/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c0ixcphlhp4c4qdiwq40bc3yp1gp1llp8pxrk4s7ny9n68s52zp"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-el2org" ,emacs-el2org)
       ("emacs-ht" ,emacs-ht)
       ("emacs-mustache" ,emacs-mustache)
       ("emacs-simple-httpd" ,emacs-simple-httpd)))
    (home-page "https://github.com/tumashu/org2web")
    (synopsis "Static site generator based on org-mode ")
    (description "Org2web is a static site generator based on org-mode,
which code derived from Kelvin H's org-page.")
    (license license:gpl2+)))

(define-public emacs-xelb
  (package
    (name "emacs-xelb")
    (version "0.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/xelb-"
                                  version ".tar"))
              (sha256
               (base32
                "0i9n0f3ibj4a5pwcsvwrah9m0fz32m0x6a9wsmjn3li20v8pcb81"))))
    (build-system emacs-build-system)
    ;; The following functions and variables needed by emacs-xelb are
    ;; not included in emacs-minimal:
    ;; x-display-screens, x-keysym-table, x-alt-keysym, x-meta-keysym
    ;; x-hyper-keysym, x-super-keysym, libxml-parse-xml-region
    ;; x-display-pixel-width, x-display-pixel-height
    (arguments
     `(#:emacs ,emacs
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'regenerate-el-files
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system* "make"
                             (string-append "PROTO_PATH="
                                            (assoc-ref inputs "xcb-proto")
                                            "/share/xcb")
                             (string-append "EMACS_BIN="
                                            (assoc-ref inputs "emacs")
                                            "/bin/emacs -Q"))))))))
    (native-inputs `(("xcb-proto" ,xcb-proto)))
    (home-page "https://github.com/ch11ng/xelb")
    (synopsis "X protocol Emacs Lisp binding")
    (description "@code{emacs-xelb} is a pure Emacs Lisp implementation of the
X11 protocol based on the XML description files from the XCB project.  It
features an object-oriented API and permits a certain degree of concurrency.
It should enable you to implement low-level X11 applications.")
    (license license:gpl3+)))

(define-public emacs-exwm
  (package
    (name "emacs-exwm")
    (version "0.16")
    (synopsis "Emacs X window manager")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/exwm-"
                                  version ".tar"))
              (sha256
               (base32
                "0c4w5k9lzqj8yzhdqipdb4fs7ld2qklc6s137104jnfdvmrwcv2i"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-xelb" ,emacs-xelb)))
    (inputs
     `(("xhost" ,xhost)
       ("dbus" ,dbus)))
    ;; The following functions and variables needed by emacs-exwm are
    ;; not included in emacs-minimal:
    ;; scroll-bar-mode, fringe-mode
    ;; x-display-pixel-width, x-display-pixel-height
    (arguments
     `(#:emacs ,emacs
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'install-xsession
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions"))
                    (bin (string-append out "/bin"))
                    (exwm-executable (string-append bin "/exwm")))
               ;; Add a .desktop file to xsessions
               (mkdir-p xsessions)
               (mkdir-p bin)
               (with-output-to-file
                   (string-append xsessions "/exwm.desktop")
                 (lambda _
                   (format #t "[Desktop Entry]~@
                     Name=~a~@
                     Comment=~a~@
                     Exec=~a~@
                     TryExec=~@*~a~@
                     Type=Application~%" ,name ,synopsis exwm-executable)))
               ;; Add a shell wrapper to bin
               (with-output-to-file exwm-executable
                 (lambda _
                   (format #t "#!~a ~@
                     ~a +SI:localuser:$USER ~@
                     exec ~a --exit-with-session ~a \"$@\" --eval '~s' ~%"
                           (string-append (assoc-ref inputs "bash") "/bin/sh")
                           (string-append (assoc-ref inputs "xhost") "/bin/xhost")
                           (string-append (assoc-ref inputs "dbus") "/bin/dbus-launch")
                           (string-append (assoc-ref inputs "emacs") "/bin/emacs")
                           '(cond
                             ((file-exists-p "~/.exwm")
                              (load-file "~/.exwm"))
                             ((not (featurep 'exwm))
                              (require 'exwm)
                              (require 'exwm-config)
                              (exwm-config-default)
                              (message (concat "exwm configuration not found. "
                                               "Falling back to default configuration...")))))))
               (chmod exwm-executable #o555)
               #t))))))
    (home-page "https://github.com/ch11ng/exwm")
    (description "EXWM is a full-featured tiling X window manager for Emacs
built on top of XELB.")
    (license license:gpl3+)))

(define-public emacs-switch-window
  (package
    (name "emacs-switch-window")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/dimitri/switch-window/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07f99apxscwvsp2bjxsbi462c433kcglrjh6xl0gyafs1nvvvnd8"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/dimitri/switch-window")
    (synopsis "Emacs window switch tool")
    (description "Switch-window is an emacs window switch tool, which
offer a visual way to choose a window to switch to, delete, split or
other operations.")
    (license license:wtfpl2)))

(define-public emacs-exwm-x
  (package
    (name "emacs-exwm-x")
    (version "1.7.2")
    (synopsis "Derivative window manager based on EXWM")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/tumashu/exwm-x/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ny13i82fb72917jgl0ndwjg1x6l9f8gfhcx7cwlwhh5saq23mvy"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-exwm" ,emacs-exwm)
       ("emacs-switch-window" ,emacs-switch-window)
       ("emacs-ivy" ,emacs-ivy)
       ("emacs-use-package" ,emacs-use-package)))
    (inputs
     `(("xhost" ,xhost)
       ("dbus" ,dbus)))
    ;; Need emacs instead of emacs-minimal,
    ;; for emacs's bin path will be inserted into bin/exwm-x file.
    (arguments
     `(#:emacs ,emacs
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'install-xsession
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions"))
                    (bin (string-append out "/bin"))
                    (exwm-executable (string-append bin "/exwm-x")))
               ;; Add a .desktop file to xsessions
               (mkdir-p xsessions)
               (mkdir-p bin)
               (with-output-to-file
                   (string-append xsessions "/exwm-x.desktop")
                 (lambda _
                   (format #t "[Desktop Entry]~@
                     Name=~a~@
                     Comment=~a~@
                     Exec=~a~@
                     TryExec=~@*~a~@
                     Type=Application~%" ,name ,synopsis exwm-executable)))
               ;; Add a shell wrapper to bin
               (with-output-to-file exwm-executable
                 (lambda _
                   (format #t "#!~a ~@
                     ~a +SI:localuser:$USER ~@
                     exec ~a --exit-with-session ~a \"$@\" --eval '~s' ~%"
                           (string-append (assoc-ref inputs "bash") "/bin/sh")
                           (string-append (assoc-ref inputs "xhost") "/bin/xhost")
                           (string-append (assoc-ref inputs "dbus") "/bin/dbus-launch")
                           (string-append (assoc-ref inputs "emacs") "/bin/emacs")
                           '(require 'exwmx-loader))))
               (chmod exwm-executable #o555)
               #t))))))
    (home-page "https://github.com/tumashu/exwm-x")
    (description "EXWM-X is a derivative window manager based on EXWM, with focus
on mouse-control.")
    (license license:gpl3+)))

(define-public emacs-gnuplot
  (package
    (name "emacs-gnuplot")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/bruceravel/gnuplot-mode/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0glzymrn138lwig7p4cj17x4if5jisr6l4g6wcbxisqkqgc1h01i"))))
    (build-system gnu-build-system)
    (native-inputs `(("emacs" ,emacs-minimal)))
    (arguments
     (let ((elisp-dir (string-append "/share/emacs/site-lisp/guix.d"
                                     "/gnuplot-" version)))
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (guix build emacs-utils))
         #:imported-modules (,@%gnu-build-system-modules
                             (guix build emacs-utils))
         #:configure-flags
         (list (string-append "EMACS=" (assoc-ref %build-inputs "emacs")
                              "/bin/emacs")
               (string-append "--with-lispdir=" %output ,elisp-dir))
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'generate-autoloads
             (lambda* (#:key outputs #:allow-other-keys)
               (emacs-generate-autoloads
                "gnuplot"
                (string-append (assoc-ref outputs "out") ,elisp-dir))
               #t))))))
    (home-page "https://github.com/bruceravel/gnuplot-mode")
    (synopsis "Emacs major mode for interacting with gnuplot")
    (description "@code{emacs-gnuplot} is an emacs major mode for interacting
with gnuplot.")
    (license license:gpl2+)))

(define-public emacs-transpose-frame
  (package
    (name "emacs-transpose-frame")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "http://www.emacswiki.org/emacs/download/transpose-frame.el")
       (file-name (string-append "transpose-frame-" version ".el"))
       (sha256
        (base32
         "1f67yksgw9s6j0033hmqzaxx2a93jm11sd5ys7cc3li5gfh680m4"))))
    (build-system emacs-build-system)
    (home-page "https://www.emacswiki.org/emacs/TransposeFrame")
    (synopsis "Transpose window arrangement in current frame")
    (description "@code{emacs-transpose-frame} provides some interactive
functions which allows users to transpose windows arrangement in currently
selected frame.")
    (license license:bsd-2)))

(define-public emacs-key-chord
  (package
    (name "emacs-key-chord")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://www.emacswiki.org/emacs/download/key-chord.el")
       (file-name (string-append "key-chord-" version ".el"))
       (sha256
        (base32
         "03m44pqggfrd53nh9dvpdjgm0rvca34qxmd30hr33hzprzjambxg"))))
    (build-system emacs-build-system)
    (home-page "https://www.emacswiki.org/emacs/key-chord.el")
    (synopsis "Map pairs of simultaneously pressed keys to Emacs commands")
    (description "@code{emacs-key-chord} provides @code{key-chord-mode}, a
mode for binding key chords to commands.  A key chord is defined as two keys
pressed simultaneously or a single key quickly pressed twice.")
    (license license:gpl2+)))

(define-public emacs-evil-surround
  (package
    (name "emacs-evil-surround")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/timcharper/evil-surround/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0p572jgic3q1ia1nz37kclir729ay6i2f4sa7wnaapyxly2lwb3r"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-evil" ,emacs-evil)))
    (home-page "https://github.com/timcharper/evil-surround")
    (synopsis "Easily modify surrounding parantheses and quotes")
    (description "@code{emacs-evil-surround} allows easy deletion, change and
addition of surrounding pairs, such as parantheses and quotes, in evil mode.")
    (license license:gpl3+)))

(define-public emacs-evil-commentary
  (package
    (name "emacs-evil-commentary")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/linktohack/evil-commentary/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jdya0i921nwskwrzdsj0vrr3m7gm49dy6f6pk9p5nxaarfxk230"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-evil" ,emacs-evil)))
    (home-page "https://github.com/linktohack/evil-commentary")
    (synopsis "Comment out code in evil mode")
    (description "@code{emacs-evil-commentary} adds keybindings to easily
comment out lines of code in evil mode.  It provides @code{gcc} to comment out
lines, and @code{gc} to comment out the target of a motion.")
    (license license:gpl3+)))

;; Tests for emacs-ansi have a circular dependency with ert-runner, and
;; therefore cannot be run
(define-public emacs-ansi
  (package
    (name "emacs-ansi")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rejeep/ansi.el/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "13jj4vbi98j3p17hs99bmy7g21jd5h4v3wpxk4pkvhylm3bfwjw8"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-s" ,emacs-s)))
    (home-page "https://github.com/rejeep/ansi.el")
    (synopsis "Convert strings to ANSI")
    (description "@code{emacs-ansi} defines functions that turns simple
strings to ANSI strings.  Turning a string into an ANSI string can be to add
color to a text, add color in the background of a text or adding a style, such
as bold, underscore or italic.")
    (license license:gpl3+)))

;; Tests for emacs-commander have a circular dependency with ert-runner, and
;; therefore cannot be run
(define-public emacs-commander
  (package
    (name "emacs-commander")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rejeep/commander.el/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "196s2i15z7gwxa97l1wkxvjnfmj5n38wwm6d3g4zz15l2vqggc2y"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-f" ,emacs-f)
       ("emacs-s" ,emacs-s)))
    (home-page "https://github.com/rejeep/commander.el")
    (synopsis "Emacs command line parser")
    (description "@code{emacs-commander} provides command line parsing for
Emacs.")
    (license license:gpl3+)))

;; Tests for ert-runner have a circular dependency with ecukes, and therefore
;; cannot be run
(define-public ert-runner
  (let ((dependencies
         `(("emacs-ansi" ,emacs-ansi)
           ("emacs-commander" ,emacs-commander)
           ("emacs-dash" ,emacs-dash)
           ("emacs-f" ,emacs-f)
           ("emacs-s" ,emacs-s)
           ("emacs-shut-up" ,emacs-shut-up))))
    (package
      (name "ert-runner")
      (version "0.7.0")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/rejeep/ert-runner.el/archive/v"
                             version ".tar.gz"))
         (file-name (string-append name "-" version ".tar.gz"))
         (sha256
          (base32
           "1657nck9i96a4xgl8crfqq0s8gflzp21pkkzwg6m3z5npjxklgwp"))))
      (build-system emacs-build-system)
      (inputs dependencies)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'install-executable
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "bin/ert-runner"
                   (("ERT_RUNNER=\"\\$\\(dirname \\$\\(dirname \\$0\\)\\)")
                    (string-append "ERT_RUNNER=\"" out
                                   "/share/emacs/site-lisp/guix.d/"
                                   ,name "-" ,version)))
                 (install-file "bin/ert-runner" (string-append out "/bin"))
                 (wrap-program (string-append out "/bin/ert-runner")
                   (list "EMACSLOADPATH" ":" '=
                         (append
                          ,(match dependencies
                             (((labels packages) ...)
                              `(map (lambda (label package version)
                                      (string-append (assoc-ref inputs label)
                                                     "/share/emacs/site-lisp/guix.d/"
                                                     (string-drop package 6)
                                                     "-" version))
                                    ',labels
                                    ',(map package-name packages)
                                    ',(map package-version packages))))
                          ;; empty element to include the default load path as
                          ;; determined by emacs' standard initialization
                          ;; procedure
                          (list ""))))
                 #t))))
         #:include (cons* "^reporters/.*\\.el$" %default-include)))
      (home-page "https://github.com/rejeep/ert-runner.el")
      (synopsis "Opinionated Ert testing workflow")
      (description "@code{ert-runner} is a tool for Emacs projects tested
using ERT.  It assumes a certain test structure setup and can therefore make
running tests easier.")
      (license license:gpl3+))))

(define-public emacs-disable-mouse
  (package
    (name "emacs-disable-mouse")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/purcell/disable-mouse/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0haqpq23r1wx04lsqrrg3p5visg9hx5i36dg55ab003wfsrlrzbc"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/purcell/disable-mouse")
    (synopsis "Disable mouse commands globally")
    (description
     "Provides @code{disable-mouse-mode} and @code{global-disable-mouse-mode},
pair of minor modes which suppress all mouse events by intercepting them and
running a customisable handler command (@code{ignore} by default). ")
    (license license:gpl3+)))

(define-public emacs-json-reformat
  (package
    (name "emacs-json-reformat")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/gongo/json-reformat/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "11fbq4scrgr7m0iwnzcrn2g7xvqwm2gf82sa7zy1l0nil7265p28"))
       (patches (search-patches "emacs-json-reformat-fix-tests.patch"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-undercover" ,emacs-undercover)))
    (inputs
     `(("emacs-dash" ,emacs-dash)         ; for tests
       ("emacs-shut-up" ,emacs-shut-up))) ; for tests
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system* "emacs" "--batch" "-L" "."
                             "-L" (string-append
                                   (assoc-ref inputs "emacs-undercover")
                                   "/share/emacs/site-lisp/guix.d/undercover-"
                                   ,(package-version emacs-undercover))
                             "-L" (string-append
                                   (assoc-ref inputs "emacs-dash")
                                   "/share/emacs/site-lisp/guix.d/dash-"
                                   ,(package-version emacs-dash))
                             "-L" (string-append
                                   (assoc-ref inputs "emacs-shut-up")
                                   "/share/emacs/site-lisp/guix.d/shut-up-"
                                   ,(package-version emacs-shut-up))
                             "-l" "test/test-helper.el"
                             "-l" "test/json-reformat-test.el"
                             "-f" "ert-run-tests-batch-and-exit"))
             #t)))))
    (home-page "https://github.com/gongo/json-reformat")
    (synopsis "Reformatting tool for JSON")
    (description "@code{json-reformat} provides a reformatting tool for
@url{http://json.org/, JSON}.")
    (license license:gpl3+)))

(define-public emacs-json-snatcher
  (package
    (name "emacs-json-snatcher")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Sterlingg/json-snatcher/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1nfiwsifpdiz0lbrqa77nl0crnfrv5h85ans9b0g5rggnmyshcfb"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/sterlingg/json-snatcher")
    (synopsis "Grabs the path to JSON values in a JSON file")
    (description "@code{emacs-json-snatcher} grabs the path to JSON values in
a @url{http://json.org/, JSON} file.")
    (license license:gpl3+)))

(define-public emacs-json-mode
  (package
    (name "emacs-json-mode")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/joshwnj/json-mode/archive/"
                           "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "06h45p4cn767pk9sqi2zb1c65wy5gyyijqxzpglp80zwxhvajdz5"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-json-reformat" ,emacs-json-reformat)
       ("emacs-json-snatcher" ,emacs-json-snatcher)))
    (home-page "https://github.com/joshwnj/json-mode")
    (synopsis "Major mode for editing JSON files")
    (description "@code{json-mode} extends the builtin js-mode syntax
highlighting.")
    (license license:gpl3+)))

(define-public emacs-restclient
  (let ((commit "07a3888bb36d0e29608142ebe743b4362b800f40")
        (revision "1"))                 ;Guix package revision,
                                        ;upstream doesn't have official releases
    (package
      (name "emacs-restclient")
      (version (string-append revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pashky/restclient.el.git")
                      (commit commit)))
                (sha256
                 (base32
                  "00lmjhb5im1kgrp54yipf1h9pshxzgjlg71yf2rq5n973gvb0w0q"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)))
      (home-page "https://github.com/pashky/restclient.el")
      (synopsis "Explore and test HTTP REST webservices")
      (description
       "This tool allows for testing and exploration of HTTP REST Web services
from within Emacs.  Restclient runs queries from a plan-text query sheet,
displays results pretty-printed in XML or JSON with @code{restclient-mode}")
      (license license:public-domain))))

(define-public emacs-dired-hacks
  (let ((commit "eda68006ce73bbf6b9b995bfd70d08bec8cade36")
        (revision "1"))
    (package
      (name "emacs-dired-hacks")
      (version (string-append "0.0.1-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Fuco1/dired-hacks.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1w7ssl9zssn5rcha6apf4h8drkd02k4xgvs203bdbqyqp9wz9brx"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-f" ,emacs-f)
         ("emacs-s" ,emacs-s)))
      (home-page "https://github.com/Fuco1/dired-hacks")
      (synopsis
       "Collection of useful dired additions")
      (description
       "Collection of Emacs dired mode additions:
@itemize
@item dired-avfs
@item dired-columns
@item dired-filter
@item dired-hacks-utils
@item dired-images
@item dired-list
@item dired-narrow
@item dired-open
@item dired-rainbow
@item dired-ranger
@item dired-subtree
@item dired-tagsistant
@end itemize\n")
      (license license:gpl3+))))

(define-public emacs-which-key
  (package
    (name "emacs-which-key")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/justbur/emacs-which-key/archive/v"
             version ".tar.gz"))
       (sha256
        (base32
         "1xvd70cwq9n31f28viyjxmr3nn8l153gsy6scpszvgvjxkiikv24"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda _
             (zero? (system* "emacs" "--batch" "-L" "."
                             "-l" "which-key-tests.el"
                             "-f" "ert-run-tests-batch-and-exit")))))))
    (home-page "https://github.com/justbur/emacs-which-key")
    (synopsis "Display available key bindings in popup")
    (description
     "@code{emacs-which-key} is a minor mode for Emacs that displays the key
bindings following your currently entered incomplete command (a prefix) in a
popup.  For example, after enabling the minor mode if you enter C-x and wait
for the default of 1 second, the minibuffer will expand with all of the
available key bindings that follow C-x (or as many as space allows given your
settings).")
    (license license:gpl3+)))

(define-public emacs-ws-butler
  (package
    (name "emacs-ws-butler")
    (version "0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lewang/ws-butler.git")
                    (commit "323b651dd70ee40a25accc940b8f80c3a3185205")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1a4b0lsmwq84qfx51c5xy4fryhb1ysld4fhgw2vr37izf53379sb"))))
    (build-system emacs-build-system)
    (native-inputs
     `(("ert-runner" ,ert-runner)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda _
             (zero? (system* "ert-runner" "tests")))))))
    (home-page "https://github.com/lewang/ws-butler")
    (synopsis "Trim spaces from end of lines")
    (description
     "This Emacs package automatically and unobtrusively trims whitespace
characters from end of lines.")
    (license license:gpl3+)))

(define-public emacs-org-edit-latex
  (package
    (name "emacs-org-edit-latex")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/et2010/org-edit-latex/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1y4h6wrs8286h9pbsv4d8fr67a885vz8b2k80qgv5qddipi2i78p"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-auctex" ,emacs-auctex)
       ;; The version of org in Emacs 25.2 is not sufficient, because the
       ;; `org-latex-make-preamble' function is required.
       ("emacs-org" ,emacs-org)))
    (home-page "https://github.com/et2010/org-edit-latex")
    (synopsis "Edit a latex fragment just like editing a src block")
    (description "@code{emacs-org-edit-latex} is an extension for org-mode.
It lets you edit a latex fragment in a dedicated buffer just like editing a
src block.")
    (license license:gpl3+)))

(define-public emacs-emamux
  (package
    (name "emacs-emamux")
    (version "0.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/syohex/emacs-emamux/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wlqg4icy037bj70b0qmhvwvmiwhagpnx6pnxhq6gzy1hvwlilkx"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/syohex/emacs-emamux")
    (synopsis "Manipulate Tmux from Emacs")
    (description
     "@code{emacs-emamux} lets Emacs interact with the @code{tmux} terminal
multiplexer.")
    (license license:gpl3+)))

(define-public emacs-rpm-spec-mode
  (package
    (name "emacs-rpm-spec-mode")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       ;; URI has the Fedora release number instead of the version
       ;; number. This will have to updated manually every new release.
       (uri (string-append
             "https://src.fedoraproject.org/cgit/rpms"
             "/emacs-rpm-spec-mode.git/snapshot"
             "/emacs-rpm-spec-mode-f26.tar.gz"))
       (sha256
        (base32
         "17dz80lhjrc89fj17pysl8slahzrqdkxgcjdk55zls6jizkr6kz3"))))
    (build-system emacs-build-system)
    (home-page "http://pkgs.fedoraproject.org/cgit/rpms/emacs-rpm-spec-mode.git")
    (synopsis "Emacs major mode for editing RPM spec files")
    (description "@code{emacs-rpm-spec-mode} provides an Emacs major mode for
editing RPM spec files.")
    (license license:gpl2+)))

(define-public emacs-git-messenger
  (package
    (name "emacs-git-messenger")
    (version "0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/syohex/emacs-git-messenger/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17mqki6g0wx46fn7dcbcc2pjxik7vvrcb1j9jzxim8b9psbsbnp9"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-popup" ,emacs-popup)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system* "emacs" "--batch" "-L" "."
                             "-L" (string-append
                                   (assoc-ref inputs "emacs-popup")
                                   "/share/emacs/site-lisp/guix.d/popup-"
                                   ,(package-version emacs-popup))
                             "-l" "test/test.el"
                             "-f" "ert-run-tests-batch-and-exit")))))))
    (home-page "https://github.com/syohex/emacs-git-messenger")
    (synopsis "Popup commit message at current line")
    (description "@code{emacs-git-messenger} provides
@code{git-messenger:popup-message}, a function that when called, will popup
the last git commit message for the current line.  This uses git-blame
internally.")
    (license license:gpl3+)))

(define-public emacs-gitpatch
  (package
    (name "emacs-gitpatch")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/tumashu/gitpatch/archive/"
                           "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1yj6pmic541lcnscjin300k380qp9xdfprs55xg1q57jrkq6f6k7"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/tumashu/gitpatch")
    (synopsis "Mail git patch from Emacs")
    (description "@code{emacs-gitpatch} lets users easily send git patches,
created by @code{git format-patch}, from @code{magit}, @code{dired} and
@code{ibuffer} buffers.")
    (license license:gpl3+)))

(define-public emacs-erc-hl-nicks
  (package
    (name "emacs-erc-hl-nicks")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/leathekd/erc-hl-nicks"
                           "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01svpl9bps5kx4y1wnymakxya2cznqmlynvqv2r500wpnbxczrbs"))))
    (build-system emacs-build-system)
    (synopsis "Nickname highlighting for Emacs ERC")
    (description "@code{erc-hl-nicks} highlights nicknames in ERC, an IRC
client for Emacs.  The main features are:
@itemize
@item Auto-colorizes nicknames without having to specify colors
@item Ignores certain characters that IRC clients add to nicknames to avoid
duplicates (nickname, nickname’, nickname\", etc.)
@item Attempts to produce colors with a sufficient amount of contrast between
the nick color and the background color
@end itemize\n")
    (home-page "https://github.com/leathekd/erc-hl-nicks")
    (license license:gpl3+)))

(define-public emacs-engine-mode
  (package
    (name "emacs-engine-mode")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/hrs/engine-mode/archive/"
                           "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1vm4p7pcp1vnwwxvps1bhm7i7hkabqqxl898knxf2hqvxys76684"))))
    (build-system emacs-build-system)
    (synopsis "Minor mode for defining and querying search engines")
    (description "@code{engine-mode} is a global minor mode for Emacs.  It
enables you to easily define search engines, bind them to keybindings, and
query them from the comfort of your editor.")
    (home-page "https://github.com/hrs/engine-mode")
    (license license:gpl3+)))

(define-public emacs-prop-menu
  (package
    (name "emacs-prop-menu")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://stable.melpa.org/packages/prop-menu-"
             version ".el"))
       (sha256
        (base32
         "01bk4sjafzz7gqrkv9jg0pa85qr34vbk3q8ga2b0m61bndywzgpr"))))
    (build-system emacs-build-system)
    (home-page
     "https://github.com/david-christiansen/prop-menu-el")
    (synopsis
     "Create and display a context menu based on text and overlay properties")
    (description
     "This is a library for computing context menus based on text
properties and overlays.  The intended use is to have tools that
annotate source code and others that use these annotations, without
requiring a direct coupling between them, but maintaining
discoverability.

Major modes that wish to use this library should first define an
appropriate value for @code{prop-menu-item-functions}.  Then, they should
bind @code{prop-menu-by-completing-read} to an appropriate
key.  Optionally, a mouse pop-up can be added by binding
@code{prop-menu-show-menu} to a mouse event.")
    (license license:gpl3+)))

(define-public emacs-idris-mode
  (package
    (name "emacs-idris-mode")
    (version "0.9.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://stable.melpa.org/packages/idris-mode-"
             version ".tar"))
       (sha256
        (base32
         "0ld4kfwnyyhlsnj5f6cbn4is4mpxdqalk2aifkw02r00mbr9n294"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-prop-menu" ,emacs-prop-menu)))
    (home-page
     "https://github.com/idris-hackers/idris-mode")
    (synopsis "Major mode for editing Idris code")
    (description
     "This is an Emacs mode for editing Idris code.  It requires the latest
version of Idris, and some features may rely on the latest Git version of
Idris.")
    (license license:gpl3+)))

(define-public emacs-browse-at-remote
  (let ((commit "31dcf77d7c89a12f230e2b2332585db2c44530ef")
        (revision "1"))
    (package
      (name "emacs-browse-at-remote")
      (version (string-append "0.9.0-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rmuslimov/browse-at-remote.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (patches
                 (search-patches "emacs-browse-at-remote-cgit-gnu.patch"))
                (sha256
                 (base32
                  "017cb8lf7zbg0jmr7zxzd7d5kz2jy35cvw5vcpdmq1fdr3wqwkgj"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-f" ,emacs-f)
         ("emacs-s" ,emacs-s)))
      (native-inputs
       `(("ert-runner" ,ert-runner)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'check
             (lambda _
               (zero? (system* "ert-runner")))))))
      (home-page "https://github.com/rmuslimov/browse-at-remote")
      (synopsis "Open github/gitlab/bitbucket/stash page from Emacs")
      (description
       "This Emacs package allows you to open a target page on
github/gitlab (or bitbucket) by calling @code{browse-at-remote} command.
It supports dired buffers and opens them in tree mode at destination.")
      (license license:gpl3+))))

(define-public emacs-tiny
  (package
    (name "emacs-tiny")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://elpa.gnu.org/packages/tiny-" version ".tar"))
       (sha256
        (base32
         "1cr73a8gba549ja55x0c2s554f3zywf69zbnd7v82jz5q1k9wd2v"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/abo-abo/tiny")
    (synopsis "Quickly generate linear ranges in Emacs")
    (description
     "The main command of the @code{tiny} extension for Emacs is @code{tiny-expand}.
It is meant to quickly generate linear ranges, e.g. 5, 6, 7, 8.  Some elisp
proficiency is an advantage, since you can transform your numeric range with
an elisp expression.")
    (license license:gpl3+)))

(define-public emacs-emojify
  (package
    (name "emacs-emojify")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/iqbalansari/emacs-emojify/"
                           "releases/download/v" version "/emojify-"
                           version ".tar"))
       (sha256
        (base32
         "0k84v2d2bkiwcky9fi1yyprgkj46g7wh6pyl9gzmcd7sqv051d5n"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-data
           (lambda* (#:key  outputs #:allow-other-keys)
             (copy-recursively "data"
                               (string-append (assoc-ref outputs "out")
                                              "/share/emacs/site-lisp/guix.d/"
                                              "emojify-" ,version "/data"))
             #t)))))
    (propagated-inputs
     `(("emacs-ht" ,emacs-ht)))
    (home-page "https://github.com/iqbalansari/emacs-emojify")
    (synopsis "Display emojis in Emacs")
    (description "This package displays emojis in Emacs similar to how Github,
Slack, and other websites do.  It can display plain ASCII like @code{:)} as
well as Github-style emojis like @code{:smile:}.  It provides a minor mode
@code{emojify-mode} to enable the display of emojis in a buffer.")
    (license license:gpl3+)))

(define-public emacs-websocket
  (package
    (name "emacs-websocket")
    (version "1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/websocket-"
                           version ".tar"))
       (sha256
        (base32
         "0dcxmnnm8z7cvsc7nkb822a1g6w03klp7cijjnfq0pz84p3w9cd9"))))
    (build-system emacs-build-system)
    (home-page "http://elpa.gnu.org/packages/websocket.html")
    (synopsis "Emacs WebSocket client and server")
    (description "This is an Elisp library for WebSocket clients to talk to
WebSocket servers, and for WebSocket servers to accept connections from
WebSocket clients.  This library is designed to be used by other library
writers, to write applications that use WebSockets, and is not useful by
itself.")
    (license license:gpl3+)))

(define-public emacs-oauth2
  (package
    (name "emacs-oauth2")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/oauth2-"
                           version ".el"))
       (sha256
        (base32
         "0ydkc9jazsnbbvfhd47mql52y7k06n3z7r0naqxkwb99j9blqsmp"))))
    (build-system emacs-build-system)
    (home-page "http://elpa.gnu.org/packages/oauth2.html")
    (synopsis "OAuth 2.0 authorization protocol implementation")
    (description
     "This package provides an Elisp implementation of the OAuth 2.0 draft.
The main entry point is @code{oauth2-auth-and-store} which will return a token
structure.  This token structure can be then used with
@code{oauth2-url-retrieve-synchronously} or @code{oauth2-url-retrieve} to
retrieve any data that need OAuth authentication to be accessed.  If the token
needs to be refreshed, the code handles it automatically and stores the new
value of the access token.")
    (license license:gpl3+)))

(define-public emacs-circe
  (package
    (name "emacs-circe")
    (version "2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jorgenschaefer/circe.git")
             (commit (string-append "v" version))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "19h3983zy3f15cgs86irvbdzz55qyjm48qd7gjlzcxplr7vnnh0j"))))
    (build-system emacs-build-system)
    ;; In order to securely connect to an IRC server using TLS, Circe requires
    ;; the GnuTLS binary.
    (propagated-inputs
     `(("gnutls" ,gnutls)))
    (home-page "https://github.com/jorgenschaefer/circe")
    (synopsis "Client for IRC in Emacs")
    (description "Circe is a Client for IRC in Emacs.  It integrates well with
the rest of the editor, using standard Emacs key bindings and indicating
activity in channels in the status bar so it stays out of your way unless you
want to use it.")
    (license license:gpl3+)))

(define-public emacs-slack
  (let ((commit "92724604879149cf331fa8778d089813a9d4ce1a")
        (revision "3"))
    (package
      (name "emacs-slack")
      (version (string-append "0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yuya373/emacs-slack.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1fdf8s3ca356k7m7f5kqzfamfl2nrqjj2pynjv3kkrr0ad15nxmw"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-alert" ,emacs-alert)
         ("emacs-emojify" ,emacs-emojify)
         ("emacs-request" ,emacs-request)
         ("emacs-websocket" ,emacs-websocket)
         ("emacs-oauth2" ,emacs-oauth2)
         ("emacs-circe" ,emacs-circe)))
      (home-page "https://github.com/yuya373/emacs-slack")
      (synopsis "Slack client for Emacs")
      (description "This package provides an Emacs client for the Slack
messaging service.")
      (license license:gpl3+))))

(define-public emacs-bash-completion
  (package
   (name "emacs-bash-completion")
   (version "2.0.0")
   (source
    (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/szermatt/emacs-bash-completion/archive/v"
            version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "0mkci4a1fy8z4cmry8mx5vsx4f16a8r454slnh7lqzidnhfi63hj"))))
   (inputs `(("bash" ,bash)))
   (build-system emacs-build-system)
   (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bash (assoc-ref inputs "bash")))
               (emacs-substitute-variables "bash-completion.el"
                 ("bash-completion-prog" (string-append bash "/bin/bash"))))
             #t)))))
   (home-page "https://github.com/szermatt/emacs-bash-completion")
   (synopsis "BASH completion for the shell buffer")
   (description
    "@code{bash-completion} defines dynamic completion hooks for shell-mode
and shell-command prompts that are based on bash completion.")
   (license license:gpl2+)))

(define-public emacs-easy-kill
  (package
    (name "emacs-easy-kill")
    (version "0.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/easy-kill-"
                                  version ".tar"))
              (sha256
               (base32
                "17nw0mglmg877axwg1d0gs03yc0p04lzmd3pl0nsnqbh3303fnqb"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/leoliu/easy-kill")
    (synopsis "Kill and mark things easily in Emacs")
    (description
     "This package provides commands @code{easy-kill} and @code{easy-mark} to
let users kill or mark things easily.")
    (license license:gpl3+)))

(define-public emacs-csv-mode
  (package
    (name "emacs-csv-mode")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://elpa.gnu.org/packages/csv-mode-"
                           version ".el"))
       (sha256
        (base32
         "0r4bip0w3h55i8h6sxh06czf294mrhavybz0zypzrjw91m1bi7z6"))))
    (build-system emacs-build-system)
    (home-page
     "http://elpa.gnu.org/packages/csv-mode.html")
    (synopsis
     "Major mode for editing comma/char separated values")
    (description
     "This Emacs package implements CSV mode, a major mode for editing records
in a generalized CSV (character-separated values) format.")
    (license license:gpl3+)))

(define-public emacs-transmission
  (package
    (name "emacs-transmission")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/holomorph/transmission/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rrlgn96gi1ljfwbwvlyyxbq75xzamlbdhq1bpyadxxmxcvlmk3n"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/holomorph/transmission")
    (synopsis "Emacs interface to a Transmission session")
    (description "This package provides an Emacs interface to interact with a
running session of the Transmission Bittorrent client.

Features:

@itemize
@item List, add, start/stop, verify, remove torrents.
@item Set speed limits, ratio limits, bandwidth priorities, trackers.
@item Navigate to the corresponding file list, torrent info, peer info
contexts.
@item Toggle downloading and set priorities for individual files.
@end itemize\n")
    (license license:gpl3+)))

(define-public emacs-polymode
  ;; There hasn't been a proper release.
  (let ((commit "0340f5e7e55235832e59673f027cc79a23cbdcd6")
        (revision "1"))
    (package
      (name "emacs-polymode")
      (version (string-append "1.0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/vspinu/polymode.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "057cybkq3cy07n5s332k071sjiky3mziy003lza4rh75mgqkwhmh"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/vspinu/polymode")
      (synopsis "Framework for multiple Emacs modes based on indirect buffers")
      (description "Polymode is an Emacs package that offers generic support
for multiple major modes inside a single Emacs buffer.  It is lightweight,
object oriented and highly extensible.  Creating a new polymode typically
takes only a few lines of code.  Polymode also provides extensible facilities
for external literate programming tools for exporting, weaving and tangling.")
      (license license:gpl3+))))

(define-public eless
  (package
    (name "eless")
    (version "0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/kaushalmodi/eless/archive/"
                    "v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gjnnhgw5xs1w3qfnkvwa2nv44gnxr8pkhx3c7qig45p8nh1461h"))))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (setenv "PATH" (string-append
                         (assoc-ref %build-inputs "tar") "/bin" ":"
                         (assoc-ref %build-inputs "gzip") "/bin"))
         (system* "tar" "xvf" (assoc-ref %build-inputs "source"))
         (chdir (string-append "eless" "-" ,version))
         (substitute* "eless" (("/usr/bin/env bash")
                               (string-append (assoc-ref %build-inputs "bash")
                                              "/bin/bash")))
         (install-file "eless" (string-append %output "/bin"))
         (install-file "doc/eless.info" (string-append %output "/share/info"))
         #t)))
    (home-page "https://github.com/kaushalmodi/eless")
    (synopsis "Use Emacs as a paginator")
    (description "@code{eless} provides a combination of Bash script
and a minimal Emacs view-mode.

Feautures:

@itemize
@item Independent of a user’s Emacs config.
@item Customizable via the @code{(locate-user-emacs-file \"elesscfg\")} config.
@item Not require an Emacs server to be already running.
@item Syntax highlighting.
@item Org-mode file rendering.
@item @code{man} page viewer.
@item Info viewer.
@item Dired, wdired, (batch edit symbolic links).
@item Colored diffs, git diff, git log, ls with auto ANSI detection.
@item Filter log files lines matching a regexp.
@item Auto-revert log files similar to @code{tail -f}.
@item Quickly change frame and font sizes.
@end itemize\n")
    (license license:expat)))

(define-public emacs-evil-matchit
  (package
    (name "emacs-evil-matchit")
    (version "2.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/redguardtoo/evil-matchit/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1hm0k53m7d8zv2pk4p93k5mmilsv1mz7y2z6dqf7r6f0zmncs31a"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/redguardtoo/evil-matchit")
    (synopsis "Vim matchit ported into Emacs")
    (description
     "@code{evil-matchit} is a minor mode for jumping between matching tags in
evil mode using @kbd{%}.  It is a port of @code{matchit} for Vim.")
    (license license:gpl3+)))

(define-public emacs-evil-smartparens
  (package
    (name "emacs-evil-smartparens")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/expez/evil-smartparens/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bwzdd3054d407d5j4m3njsbvmc9r8zzp33m32pj3b3irxrl68q0"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-evil" ,emacs-evil)
       ("emacs-smartparens" ,emacs-smartparens)))
    (home-page "https://github.com/expez/evil-smartparens")
    (synopsis "Emacs Evil integration for Smartparens")
    (description "@code{emacs-evil-smartparens} is an Emacs minor mode which
makes Evil play nice with Smartparens.  Evil is an Emacs minor mode that
emulates Vim features and provides Vim-like key bindings.")
    (license license:gpl3+)))

(define-public emacs-evil-quickscope
  (package
    (name "emacs-evil-quickscope")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/blorbx/evil-quickscope/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1r26a412mmar7vbf89zcifswiwpdg30mjzj32xdyqss57aqi83ma"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-evil" ,emacs-evil)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "emacs" "--batch" "-L"
                     (string-append (assoc-ref inputs "emacs-evil")
                                    "/share/emacs/site-lisp/guix.d/evil-"
                                    ,(package-version emacs-evil))
                     "-l" "evil-quickscope-tests.el"
                     "-f" "ert-run-tests-batch-and-exit"))))))
    (home-page "https://github.com/blorbx/evil-quickscope")
    (synopsis "Target highlighting for emacs evil-mode f,F,t and T commands")
    (description "@code{emacs-evil-quickscope} highlights targets for Evil
mode’s f,F,t,T keys, allowing for quick navigation within a line.  It is a
port of quick-scope for Vim.  Evil is an Emacs minor mode that emulates Vim
features and provides Vim-like key bindings.")
    (license license:gpl3+)))

(define-public emacs-bongo
  (package
    (name "emacs-bongo")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/dbrock/bongo/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1pcsyyrvj7djjjwpaswd1i782hvqvlvs39cy9ns0k795si6xd64d"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/dbrock/bongo")
    (synopsis "Media player for Emacs")
    (description
     "This package provides a flexible media player for Emacs.  @code{Bongo}
supports multiple backends such as @code{vlc}, @code{mpg123},
@code{ogg123}, @code{speexdec}, @code{timidity}, @code{mikmod} and
@code{afplay}.")
    (license license:gpl2+)))

(define-public groovy-emacs-modes
  (package
    (name "groovy-emacs-modes")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Groovy-Emacs-Modes/" name
                    "/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15j0hnkx9nppjzda5cqsxxz5f3bq9hc4xfyjcdypzqiypcvmpa39"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-s" ,emacs-s)))
    (home-page "https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes")
    (synopsis "Groovy related modes for Emacs")
    (description
     "This package provides @code{groovy-mode} for syntax highlighing in
Groovy source files, REPL integration with run-groovy and Grails project
navigation with the grails mode.")
    (license license:gpl3+)))

(define-public org-tree-slide
  (let ((commit "dff8f1a4a64c8dd0a1fde0b0131e2fe186747134")
        (revision "0"))
    (package
      (name "emacs-org-tree-slide")
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/takaxp/org-tree-slide")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (sha256
                 (base32
                  "153bg0x7ypla11pq51jmsgzfjklwwnrq56xgpbfhk1j16xwz9hyf"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (synopsis "Presentation tool for org-mode")
      (description
       "Org-tree-slide provides a slideshow mode to view org-mode files.  Use
@code{org-tree-slide-mode} to enter the slideshow mode, and then @kbd{C->} and
@kbd{C-<} to jump to the next and previous slide.")
      (license license:gpl3+))))

(define-public emacs-scratch-el
  (let ((commit "2cdf2b841ce7a0987093f65b0cc431947549f897")
        (revision "1"))
    (package
      (name "emacs-scratch-el")
      (version (git-version "1.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ieure/scratch-el.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0wscsndynjmnliajqaz28r1ww81j8wh84zwaaswx51abhwgl0idf"))))
      (build-system emacs-build-system)
      (native-inputs
       `(("texinfo" ,texinfo)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'install-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (unless (invoke "makeinfo" "scratch.texi")
                 (error "makeinfo failed"))
               (install-file "scratch.info"
                             (string-append (assoc-ref outputs "out")
                                            "/share/info"))
               #t)))))
      (home-page "https://github.com/ieure/scratch-el/")
      (synopsis "Create scratch buffers with the same mode as current buffer")
      (description "Scratch is an extension to Emacs that enables one to create
scratch buffers that are in the same mode as the current buffer.  This is
notably useful when working on code in some language; you may grab code into a
scratch buffer, and, by virtue of this extension, do so using the Emacs
formatting rules for that language.")
      (license license:bsd-2))))

(define-public emacs-esxml
  (package
    (name "emacs-esxml")
    (version "0.3.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tali713/esxml.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00vv8a75wdklygdyr4km9mc2ismxak69c45jmcny41xl44rp9x8m"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/tali713/esxml/")
    (synopsis "SXML for EmacsLisp")
    (description "This is XML/XHTML done with S-Expressions in EmacsLisp.
Simply, this is the easiest way to write HTML or XML in Lisp.  This library
uses the native form of XML representation as used by many libraries already
included within Emacs.  See @code{esxml-to-xml} for a concise description of
the format.")
    (license license:gpl3+)))

(define-public emacs-nov-el
  (package
    (name "emacs-nov-el")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wasamasa/nov.el.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03s0qjvwk1f7y3i4wh2p5y3z4hdv00adgz8za3vphzc0q8i1kjzb"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'embed-path-to-unzip
           (lambda _
             (substitute* "nov.el"
               (("\\(executable-find \"unzip\"\\)")
                (string-append "\"" (which "unzip") "\"")))
             #t)))))
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-esxml" ,emacs-esxml)))
    (inputs
     `(("unzip" ,unzip)))
    (home-page "https://github.com/wasamasa/nov.el/")
    (synopsis "Major mode for reading EPUBs in Emacs")
    (description "@code{nov.el} provides a major mode for reading EPUB
documents.

Features:

@itemize
@item Basic navigation (jump to TOC, previous/next chapter)
@item Remembering and restoring the last read position
@item Jump to next chapter when scrolling beyond end
@item Renders EPUB2 (@code{.ncx}) and EPUB3 (@code{<nav>}) TOCs
@item Hyperlinks to internal and external targets
@item Supports textual and image documents
@item View source of document files
@item Metadata display
@item Image rescaling
@end itemize
")
    (license license:gpl3+)))

(define-public epipe
  (package
    (name "epipe")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cute-jumper/epipe/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "05a036852g4j63k1mhvyfrcsgkl9lczayi7x61570ysw3cli5wp5"))))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)
       ("perl" ,perl)))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (arguments
     `(#:modules
       ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         ;; Extract source
         (setenv "PATH" (string-append
                         (assoc-ref %build-inputs "tar") "/bin" ":"
                         (assoc-ref %build-inputs "gzip") "/bin"))
         (system* "tar" "xvf" (assoc-ref %build-inputs "source"))
         (chdir (string-append ,name "-" ,version))
         ;; Patch shebangs
         (substitute* "epipe"
           (("/usr/bin/env bash")
            (string-append (assoc-ref %build-inputs "bash") "/bin/bash")))
         (patch-shebang "epipe.pl"
                        (list (string-append (assoc-ref %build-inputs "perl")
                                             "/bin")))
         ;; Installation
         (for-each (lambda (file)
                     (install-file file (string-append %output "/bin")))
                   '("epipe" "epipe.pl"))
         #t)))
    (home-page "https://github.com/cute-jumper/epipe")
    (synopsis "Pipe to the @code{emacsclient}")
    (description "@code{epipe} provides an utility to use your editor in
the pipeline, featuring the support for running @code{emacsclient}.")
    (license license:gpl3+)))

(define-public emacs-hcl-mode
  (package
    (name "emacs-hcl-mode")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/syohex/emacs-hcl-mode/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0pvw74qpwh0znqzp6syp4wxjqs7dp1hbn5h7xfk97mff9l5d8k6x"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/syohex/emacs-hcl-mode")
    (synopsis "Major mode for the Hashicorp Configuration Language")
    (description
     "@code{emacs-hcl-mode} provides an Emacs major mode for working with
@acronym{HCL, Hashicorp Configuration Language}.  It provides syntax
highlighting and indentation support.")
    (license license:gpl3+)))

(define-public emacs-terraform-mode
  (package
    (name "emacs-terraform-mode")
    (version "0.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/syohex/emacs-terraform-mode/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0h9267ifdjmcin4sj8slxydbacx4bqicbvg8pa1qq2l72h9m5381"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-hcl-mode" ,emacs-hcl-mode)))
    (home-page "https://github.com/syohex/emacs-terraform-mode")
    (synopsis "Major mode for Terraform")
    (description
     "@code{emacs-terraform-mode} provides a major mode for working with
@uref{https://www.terraform.io/, Terraform} configuration files.  Most of the
functionality is inherited from @code{hcl-mode}.")
    (license license:gpl3+)))

(define-public emacs-exec-path-from-shell
  (package
    (name "emacs-exec-path-from-shell")
    (version "1.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://stable.melpa.org/packages/exec-path-from-shell-"
             version ".el"))
       (sha256
        (base32
         "03qjgb81cq1l3j54lvlf98r75vmmgd06mj6qh5wa6mz4xzp4w26r"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/purcell/exec-path-from-shell")
    (synopsis "Get environment variables such as @var{PATH} from the shell")
    (description
     "This library allows the user to set Emacs @var{exec-path} and @var{PATH}
from the shell @var{PATH}, so that @code{shell-command}, @code{compile} and
the like work as expected on systems on which Emacs is not guaranteed to
inherit a login shell's environment variables.  It also allows other
environment variables to be retrieved from the shell, so that Emacs will see
the same values you get in a terminal.")
    (license license:gpl3+)))

(define-public emacs-deft
  (package
    (name "emacs-deft")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://stable.melpa.org/packages/deft-"
                           version ".el"))
       (sha256
        (base32
         "1vb9cjxskc7c0yyf9pvxy1fzypg1vrcgwnjz0m3hslinsgdyig58"))))
    (build-system emacs-build-system)
    (home-page "https://jblevins.org/projects/deft/")
    (synopsis "Quickly browse, filter, and edit plain text notes")
    (description
     "Deft is an Emacs mode for quickly browsing, filtering, and editing
directories of plain text notes, inspired by Notational Velocity.")
    (license license:bsd-3)))
