;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Nils Gillmann <niasterisk@grrlz.net>
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
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages texinfo)
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
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages mp3)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public emacs
  (package
    (name "emacs")
    (version "24.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/emacs/emacs-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0kn3rzm91qiswi0cql89kbv6mqn27rwsyjfb8xmwy9m5s8fxfiyx"))
             (patches (list (search-patch "emacs-exec-path.patch")
                            (search-patch "emacs-source-date-epoch.patch")))))
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
         (add-after 'install 'remove-info.info
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Remove 'info.info', which is provided by Texinfo <= 6.0.
             ;; TODO: Remove this phase when we switch to Texinfo 6.1.
             (let ((out (assoc-ref outputs "out")))
               (delete-file
                (string-append out "/share/info/info.info.gz"))
               #t)))
         (add-after 'install 'install-site-start
           ;; Copy guix-emacs.el from Guix and add it to site-start.el.  This
           ;; way, Emacs packages provided by Guix and installed in
           ;; ~/.guix-profile/share/emacs/site-lisp/guix.d/PACKAGE-VERSION are
           ;; automatically found.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((guix-src (assoc-ref inputs "guix-src"))
                    (out      (assoc-ref outputs "out"))
                    (lisp-dir (string-append out "/share/emacs/"
                                             ,(version-major+minor version)
                                             "/site-lisp"))
                    (unpack   (assoc-ref %standard-phases 'unpack)))
               (mkdir "guix")
               (with-directory-excursion "guix"
                 (apply unpack (list #:source guix-src))
                 (install-file "emacs/guix-emacs.el" lisp-dir))
               (with-output-to-file (string-append lisp-dir "/site-start.el")
                 (lambda ()
                   (display "(require 'guix-emacs nil t)")))
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
       ("guix-src" ,(package-source guix))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))

    (native-search-paths
     (list (search-path-specification
            (variable "INFOPATH")
            (files '("share/info")))))

    (home-page "http://www.gnu.org/software/emacs/")
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

(define-public emacs-no-x
  ;; This is the version that you should use as an input to packages that just
  ;; need to byte-compile .el files.
  (package (inherit emacs)
    (name "emacs-no-x")
    (synopsis "The extensible, customizable, self-documenting text
editor (console only)")
    (build-system gnu-build-system)
    (inputs (fold alist-delete
                  (package-inputs emacs)
                  '("libx11" "gtk+" "libxft" "libtiff" "giflib" "libjpeg"
                    "libpng" "librsvg" "libxpm" "libice" "libsm"

                    ;; D-Bus depends on libx11, so remove it as well.
                    "dbus")))))

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
                        (zero? (system* "sh" "autogen.sh"))))))))))


;;;
;;; Emacs hacking.
;;;

(define-public geiser
  (package
    (name "geiser")
    (version "0.8.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/geiser/" version
                                 "/geiser-" version ".tar.gz"))
             (sha256
              (base32
               "163zh8qf1q8l485d94a51a9xixirj8r2xvrbgxyw06vkaqrz5qvc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-after
                 'install 'post-install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (symlink "geiser-install.el"
                            (string-append (assoc-ref outputs "out")
                                           "/share/emacs/site-lisp/"
                                           "geiser-autoloads.el")))
                 %standard-phases)))
    (inputs `(("guile" ,guile-2.0)
              ("emacs" ,emacs-no-x)))
    (home-page "http://nongnu.org/geiser/")
    (synopsis "Collection of Emacs modes for Guile and Racket hacking")
    (description
     "Geiser is a collection of Emacs major and minor modes that conspire with
one or more Scheme implementations to keep the Lisp Machine Spirit alive.  The
continuously running Scheme interpreter takes the center of the stage in
Geiser.  A bundle of Elisp shims orchestrates the dialog between the Scheme
implementation, Emacs and, ultimately, the schemer, giving them access to live
metadata.")
    (license license:bsd-3)))

(define-public paredit
  (package
    (name "paredit")
    (version "24")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://mumble.net/~campbell/emacs/paredit-"
                                  version ".el"))
              (sha256
               (base32
                "0pp3n8q6kc70blqsaw0zlzp6bc327dpgdrjr0cnh7hqg1lras7ka"))))
    (build-system trivial-build-system)
    (inputs `(("emacs" ,emacs-no-x)))
    (arguments
     `(#:modules ((guix build utils)
                  (guix build emacs-utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (use-modules (guix build emacs-utils))

         (let* ((emacs    (string-append (assoc-ref %build-inputs "emacs")
                                         "/bin/emacs"))
                (source   (assoc-ref %build-inputs "source"))
                (lisp-dir (string-append %output
                                         "/share/emacs/site-lisp"))
                (target   (string-append lisp-dir "/paredit.el")))
           (mkdir-p lisp-dir)
           (copy-file source target)
           (with-directory-excursion lisp-dir
             (parameterize ((%emacs emacs))
               (emacs-generate-autoloads ,name lisp-dir)
               (emacs-batch-eval '(byte-compile-file "paredit.el"))))))))
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

(define-public git-modes
  (package
    (name "git-modes")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/magit/git-modes/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "088wyddh8y0yw77i0hx449n9zg4wzyc90h63wlmxba1ijg4dzm0p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build emacs-utils)
                  (guix build utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-utils))

       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out"))
                          ;; Don't put .el files in a 'git-modes'
                          ;; sub-directory.
                          (string-append "LISPDIR="
                                         (assoc-ref %outputs "out")
                                         "/share/emacs/site-lisp"))
       #:tests? #f  ; no check target
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'install 'emacs-autoloads
                             (lambda* (#:key outputs #:allow-other-keys)
                               (let* ((out  (assoc-ref outputs "out"))
                                      (lisp (string-append
                                             out "/share/emacs/site-lisp/")))
                                 (emacs-generate-autoloads ,name lisp)))))))
    (native-inputs `(("emacs" ,emacs-no-x)))
    (home-page "https://github.com/magit/git-modes")
    (synopsis "Emacs major modes for Git configuration files")
    (description
     "This package provides Emacs major modes for editing various Git
configuration files, such as .gitattributes, .gitignore, and .git/config.")
    (license license:gpl3+)))

(define-public emacs-with-editor
  (package
    (name "emacs-with-editor")
    (version "2.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/magit/with-editor/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19gb381z61l2icg5v5pymgi1a11g3zdp5aysl2j5fh7fxxg4d4c0"))))
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
    (version "2.5.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/magit/magit/releases/download/"
                   version "/" name "-" version ".tar.gz"))
             (sha256
              (base32
               "0i6qpx5szzc4kyfcdhaic8gif0sqdqcya1niyj93lpvw66jcxsxa"))))
    (build-system gnu-build-system)
    (native-inputs `(("texinfo" ,texinfo)
                     ("emacs" ,emacs-no-x)))
    (inputs `(("git" ,git)))
    (propagated-inputs
     `(("dash" ,emacs-dash)
       ("with-editor" ,emacs-with-editor)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-utils))

       #:test-target "test"
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
            (let ((git (assoc-ref inputs "git")))
              (emacs-substitute-variables "lisp/magit-git.el"
                ("magit-git-executable" (string-append git "/bin/git")))
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
    (native-inputs `(("emacs" ,emacs-no-x)
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

(define-public haskell-mode
  (package
    (name "haskell-mode")
    (version "13.14.2")
    (source (origin
              (method url-fetch)
              (file-name (string-append name "-" version ".tar.gz"))
              (uri (string-append
                    "https://github.com/haskell/haskell-mode/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32 "1kxc2yj8vb122dv91r68h7c5ladcryx963fr16plfhg71fv7f9av"))))
    (inputs `(("emacs" ,emacs-no-x)))
    (native-inputs
     `(("texinfo" ,texinfo)))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "EMACS="
                                         (assoc-ref %build-inputs "emacs")
                                         "/bin/emacs"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before
          'build 'pre-build
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((sh (string-append (assoc-ref inputs "bash") "/bin/sh")))
              (setenv "SHELL" "sh")
              (substitute* (find-files "." "\\.el") (("/bin/sh") sh))
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
               ;; these are now distributed with emacs
               (with-directory-excursion el-dir
                 (for-each delete-file '("cl-lib.el" "ert.el")))
               #t))))))
    (home-page "https://github.com/haskell/haskell-mode")
    (synopsis "Haskell mode for Emacs")
    (description
     "This is an Emacs mode for editing, debugging and developing Haskell
programs.")
    (license license:gpl3+)))

(define-public let-alist
  (package
    (name "emacs-let-alist")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://elpa.gnu.org/packages/let-alist-"
                                  version ".el"))
              (sha256
               (base32
                "07312bvvyz86lf64vdkxg2l1wgfjl25ljdjwlf1bdzj01c4hm88x"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build emacs-utils))

       #:builder (begin
                   (use-modules (guix build emacs-utils)
                                (guix build utils))

                   (let* ((out     (assoc-ref %outputs "out"))
                          (lispdir (string-append out
                                                  "/share/emacs/site-lisp/"
                                                  "guix.d/let-alist-"
                                                  ,version))
                          (emacs   (assoc-ref %build-inputs "emacs")))

                     (mkdir-p lispdir)
                     (copy-file (assoc-ref %build-inputs "source")
                                (string-append lispdir "/let-alist.el"))

                     (setenv "PATH" (string-append emacs "/bin"))
                     (emacs-byte-compile-directory lispdir)
                     #t))))
    (native-inputs `(("emacs" ,emacs-no-x)))
    (home-page "http://elpa.gnu.org/packages/let-alist.html")
    (synopsis "Easily let-bind values of an assoc-list by their names")
    (description
     "This package offers a single Emacs Lisp macro, @code{let-alist}.  This
macro takes a first argument, whose value must be an alist (association list),
and a body.

The macro expands to a let form containing the body, where each dotted symbol
inside body is let-bound to their cdrs in the alist.  Only those present in
the body are let-bound and this search is done at compile time.")
    (license license:gpl3+)))

(define-public flycheck
  (package
    (name "emacs-flycheck")
    (version "0.23")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/flycheck/flycheck/releases/download/"
                    version "/flycheck-" version ".tar"))
              (sha256
               (base32
                "1n2cifzsl5dbv42l82bi3y1vk6q33msi8dd4bj7b9nvnl9jfjj5b"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-let-alist" ,let-alist)))
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
    (native-inputs `(("autoconf" ,autoconf)))
    (inputs `(("w3m" ,w3m)
              ("imagemagick" ,imagemagick)
              ("emacs" ,emacs-no-x)))
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
    (inputs `(("wget" ,wget)
              ("emacs" ,emacs-no-x)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-utils))
       #:tests? #f  ; no check target
       #:phases
       (alist-replace
        'configure
        (lambda* (#:key outputs #:allow-other-keys)
          (substitute* "Makefile"
            (("/usr/local") (assoc-ref outputs "out"))
            (("/site-lisp/emacs-wget") "/site-lisp")))
        (alist-cons-before
         'build 'patch-exec-paths
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((wget (assoc-ref inputs "wget")))
             (emacs-substitute-variables "wget.el"
               ("wget-command" (string-append wget "/bin/wget")))))
         (alist-cons-after
          'install 'post-install
          (lambda* (#:key outputs #:allow-other-keys)
            (emacs-generate-autoloads
             "wget" (string-append (assoc-ref outputs "out")
                                   "/share/emacs/site-lisp/")))
          %standard-phases)))))
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
    (name "emms")
    (version "4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/emms/emms-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1q0n3iwva8bvai2rl9sm49sdjmk0wi7vajz4knz01l7g67nrp87l"))
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

       #:phases (alist-replace
                 'configure
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let ((out     (assoc-ref outputs "out"))
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
                       (emacs-substitute-variables "emms-info-ogginfo.el"
                         ("emms-info-ogginfo-program-name"
                          (string-append vorbis "/bin/ogginfo")))
                       (emacs-substitute-variables "emms-info-libtag.el"
                         ("emms-info-libtag-program-name"
                          (string-append out "/bin/emms-print-metadata")))
                       (emacs-substitute-variables "emms-info-mp3info.el"
                         ("emms-info-mp3info-program-name"
                          (string-append mp3info "/bin/mp3info")))
                       (substitute* "emms-volume-amixer.el"
                         (("\"amixer\"")
                          (string-append "\"" alsa "/bin/amixer\"")))
                       (substitute* "emms-tag-editor.el"
                         (("\"mp3info\"")
                          (string-append "\"" mp3info "/bin/mp3info\""))))))
                 (alist-cons-before
                  'install 'pre-install
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; The 'install' rule expects the target directory to
                    ;; exist.
                    (let* ((out  (assoc-ref outputs "out"))
                           (man1 (string-append out "/share/man/man1")))
                      (mkdir-p man1)
                      #t))
                  (alist-cons-after
                   'install 'post-install
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out    (assoc-ref outputs "out"))
                            (target (string-append
                                     out "/bin/emms-print-metadata")))
                       (symlink "emms-auto.el"
                                (string-append out "/share/emacs/site-lisp/"
                                               "emms-autoloads.el"))
                       (mkdir-p (dirname target))
                       (copy-file "src/emms-print-metadata" target)
                       (chmod target #o555)))
                   %standard-phases)))
       #:tests? #f))
    (native-inputs `(("emacs" ,emacs-no-x)       ;for (guix build emacs-utils)
                     ("texinfo" ,texinfo)))
    (inputs `(("alsa-utils" ,alsa-utils)
              ("vorbis-tools" ,vorbis-tools)
              ("mpg321" ,mpg321)
              ("taglib" ,taglib)
              ("mp3info" ,mp3info)))
    (synopsis "Emacs Multimedia System")
    (description
     "EMMS is the Emacs Multimedia System.  It is a small front-end which
can control one of the supported external players.  Thus, it supports
whatever formats are supported by your music player.  It also
supports tagging and playlist management, all behind a clean and
light user interface.")
    (home-page "http://www.gnu.org/software/emms/")
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
     '(#:phases (alist-cons-after
                 'install 'post-install
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Add an autoloads file with the right name for guix.el.
                   (let* ((out  (assoc-ref outputs "out"))
                          (site (string-append out "/share/emacs/site-lisp")))
                     (with-directory-excursion site
                       (symlink "bbdb-loaddefs.el" "bbdb-autoloads.el"))))
                 %standard-phases)))
    (native-inputs `(("emacs" ,emacs-no-x)))
    (home-page "http://savannah.nongnu.org/projects/bbdb/")
    (synopsis "Contact management utility for Emacs")
    (description
     "BBDB is the Insidious Big Brother Database for GNU Emacs.  It provides
an address book for email and snail mail addresses, phone numbers and the
like.  It can be linked with various Emacs mail clients (Message and Mail
mode, Rmail, Gnus, MH-E, and VM).  BBDB is fully customizable.")
    (license license:gpl3+)))

(define-public emacs-async
  (package
    (name "emacs-async")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://elpa.gnu.org/packages/async-"
                                  version ".tar"))
              (sha256
               (base32
                "17psvz75n42x33my967wkgi7r0blx46n3jdv510j0z5jswv66039"))))
    (build-system emacs-build-system)
    (home-page "http://elpa.gnu.org/packages/async.html")
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
    (version "11.88.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://elpa.gnu.org/packages/auctex-"
             version
             ".tar"))
       (sha256
        (base32
         "1pmki8hdjjikxlvip3pzi350bln3gcimr27yjf0xfwjvnp5hh9nc"))))
    (build-system emacs-build-system)
    (native-inputs
     `(("perl" ,perl)))
    (home-page "http://www.gnu.org/software/auctex/")
    (synopsis "Integrated environment for TeX")
    (description
     "AUCTeX is a comprehensive customizable integrated environment for
writing input files for TeX, LaTeX, ConTeXt, Texinfo, and docTeX using Emacs
or XEmacs.")
    (license license:gpl3+)))

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
       ("emacs" ,emacs-no-x)
       ("texinfo" ,texinfo)))
    (home-page "https://github.com/purcell/mmm-mode")
    (synopsis "Allow multiple major modes in an Emacs buffer")
    (description
     "MMM Mode is a minor mode that allows multiple major modes to coexist in a
single buffer.")
    (license license:gpl3+)))

(define-public emacs-pdf-tools
  (package
    (name "emacs-pdf-tools")
    (version "0.70")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/politza/pdf-tools/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m0api6wiawswyk46bdsyk6r5rg3b86a4paar6nassm6x6c6vr77"))))
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
         (add-before 'configure 'autogen
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
                     ("emacs" ,emacs-no-x)))
    (propagated-inputs
     `(("let-alist" ,let-alist)))
    (inputs `(("poppler" ,poppler)
              ("cairo" ,cairo)
              ("glib" ,glib)
              ("libpng" ,libpng)
              ("zlib" ,zlib)))
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
    (version "2.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/magnars/dash.el/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "082jl7mp4x063bpj5ad2pc5125k0d6p7rb89gcj7ny3lma9h2ij1"))))
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

(define-public emacs-undo-tree
  (package
    (name "emacs-undo-tree")
    (version "0.6.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "http://dr-qubit.org/git/undo-tree.git")
                    (commit "release/0.6.4")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
                (base32
                  "0b6hnv6bq1g5np5q2yw9r9aj1cxpp14akm21br7vpb7wp01fv4b3"))))
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
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/magnars/s.el/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gah2k577gvnmxlpw7zrz0jr571vghzhdv2hbgchlgah07czd091"))))
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

(define-public emacs-f
  (package
    (name "emacs-f")
    (version "0.17.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rejeep/f.el/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1n5gcldf43wmkr7jxgs519v21zavwr0yn8048iv6gvgfwicnyjlx"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-s" ,emacs-s)
       ("emacs-dash" ,emacs-dash)))
    (home-page "http://github.com/rejeep/f.el")
    (synopsis "Emacs API for working with files and directories")
    (description "This package provides an Emacs library for working with
files and directories.")
    (license license:gpl3+)))

(define-public emacs-ob-ipython
  (package
    (name "emacs-ob-ipython")
    (version "20150704.8807064693")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (commit "8807064693")
                    (url "https://github.com/gregsexton/ob-ipython.git")))
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
    (version "0.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://elpa.gnu.org/packages/debbugs-"
                                  version ".tar"))
              (sha256
               (base32
                "1wc6kw7hihqqdx8qyl01akygycnan44x400hwrcf54m3hb4isa0k"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-async" ,emacs-async)))
    (home-page "http://elpa.gnu.org/packages/debbugs.html")
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
          (list (search-patch "emacs-scheme-complete-scheme-r5rs-info.patch")))))
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
           (copy-file "6.945-config/mit-scheme-doc.el" "mit-scheme-doc.el")
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

;;; XXX: move this procedure to an utility module
(define* (uncompressed-file-fetch url hash-algo hash
                                  #:optional name
                                  #:key (system (%current-system))
                                  (guile (default-guile)))
  (mlet %store-monad ((drv (url-fetch url hash-algo hash name
                                      #:system system
                                      #:guile guile)))
    (gexp->derivation (or name (basename url))
                      #~(begin
                          (mkdir #$output)
                          (setenv "PATH"
                                  (string-append #$gzip "/bin"))
                          (chdir #$output)
                          (copy-file #$drv (basename #$url))))))

(define-public emacs-constants
  (package
    (name "emacs-constants")
    (version "2.2")
    (source
     (origin
       (file-name (string-append name "-" version ".el"))
       (method uncompressed-file-fetch)
       (uri "https://staff.fnwi.uva.nl/c.dominik/Tools/constants/constants.el")
       (patches
        (list (search-patch "emacs-constants-lisp-like.patch")))
       (sha256
        (base32
         "14q094aphsjhq8gklv7i5a7byl0ygz63cv3n6b5p8ji2jy0mnnw3"))))
    (build-system emacs-build-system)
    (home-page "https://staff.fnwi.uva.nl/c.dominik/Tools/constants")
    (synopsis "Enter definition of constants into an Emacs buffer")
    (description
     "This package provides functions for inserting the definition of natural
constants and units into an Emacs buffer.")
    (license license:gpl2+)))

(define-public emacs-slime
  (package
    (name "emacs-slime")
    (version "2.15")
    (source
     (origin
       (file-name (string-append name "-" version ".tar.gz"))
       (method url-fetch)
       (uri (string-append
             "https://github.com/slime/slime/archive/v"
             version ".tar.gz"))
       (sha256
        (base32
         "0l2z6l2xm78mhh0nczkrmzh2ddb1n911ij9xb6q40zwvx4f8blds"))))
    (build-system emacs-build-system)
    (native-inputs
     `(("texinfo" ,texinfo)))
    (arguments
     `(#:phases
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
Common Lisp.  The features are centered around @{slime-mode}, an Emacs
minor mode that complements the standard @{lisp-mode}.  While lisp-mode
supports editing Lisp source files, @{slime-mode} adds support for
interacting with a running Common Lisp process for compilation,
debugging, documentation lookup, and so on.")
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
    (native-inputs
     `(("emacs" ,emacs-no-x)))
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
