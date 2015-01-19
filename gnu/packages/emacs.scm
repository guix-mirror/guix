;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
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
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages autotools)
  #:use-module ((gnu packages compression) #:prefix compression:)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages linux)               ;alsa
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages mp3)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public emacs
  (package
    (name "emacs")
    (version "24.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/emacs/emacs-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1zflm6ac34s6v166p58ilxrxbxjm0q2wfc25f8y0mjml1lbr3qs7"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'fix-/bin/pwd
                 (lambda _
                   ;; Use `pwd', not `/bin/pwd'.
                   (substitute* (find-files "." "^Makefile\\.in$")
                     (("/bin/pwd")
                      "pwd")))
                 %standard-phases)))
    (inputs
     `(("gnutls" ,gnutls)
       ("ncurses" ,ncurses)

       ;; TODO: Add the optional dependencies.
       ("xlibs" ,libx11)
       ("gtk+" ,gtk+)
       ("libXft" ,libxft)
       ("libtiff" ,libtiff)
       ("giflib" ,giflib)
       ("libjpeg" ,libjpeg-8)
       ("acl" ,acl)

       ;; When looking for libpng `configure' links with `-lpng -lz', so we
       ;; must also provide zlib as an input.
       ("libpng" ,libpng)
       ("zlib" ,compression:zlib)

       ("libXpm" ,libxpm)
       ("libxml2" ,libxml2)
       ("libice" ,libice)
       ("libsm" ,libsm)
       ("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))
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
    (license gpl3+)))

(define-public emacs-no-x-toolkit
  (package (inherit emacs)
    (location (source-properties->location (current-source-location)))
    (name "emacs-no-x-toolkit")
    (synopsis "The extensible, customizable, self-documenting text
editor (without an X toolkit)" )
    (build-system gnu-build-system)
    (inputs (append `(("inotify-tools" ,inotify-tools))
                    (alist-delete "gtk+" (package-inputs emacs))))
    (arguments (append '(#:configure-flags '("--with-x-toolkit=no"))
                       (package-arguments emacs)))))


;;;
;;; Emacs hacking.
;;;

(define-public geiser
  (package
    (name "geiser")
    (version "0.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/geiser/" version
                                 "/geiser-" version ".tar.gz"))
             (sha256
              (base32 "1mrk0bzqcpfhsw6635qznn47nzfy9ps7wrhkpymswdfpw5mdsry5"))))
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
              ("emacs" ,emacs)))
    (home-page "http://nongnu.org/geiser/")
    (synopsis "Collection of Emacs modes for Guile and Racket hacking")
    (description
     "Geiser is a collection of Emacs major and minor modes that
conspire with one or more Scheme interpreters to keep the Lisp Machine
Spirit alive.  It draws inspiration (and a bit more) from environments
such as Common Lisp’s Slime, Factor’s FUEL, Squeak or Emacs itself, and
does its best to make Scheme hacking inside Emacs (even more) fun.

Or, to be precise, what i consider fun.  Geiser is thus my humble
contribution to the dynamic school of expression, and a reaction against
what i perceive as a derailment, in modern times, of standard Scheme
towards the static camp.  Because i prefer growing and healing to poking
at corpses, the continuously running Scheme interpreter takes the center
of the stage in Geiser.  A bundle of Elisp shims orchestrates the dialog
between the Scheme interpreter, Emacs and, ultimately, the schemer,
giving her access to live metadata.")
    (license bsd-3)))

(define-public paredit
  (package
    (name "paredit")
    (version "23")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://mumble.net/~campbell/emacs/paredit-"
                                 version ".el"))
             (sha256
              (base32 "1np882jzvxckljx3cjz4absyzmc5hw65cs21sjmbic82163m9lf8"))))
    (build-system trivial-build-system)
    (inputs `(("emacs" ,emacs)))
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
    (license gpl3+)))

(define-public magit
  (package
    (name "magit")
    (version "1.2.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/magit/magit/releases/download/"
                   version "/" name "-" version ".tar.gz"))
             (sha256
              (base32 "1in48g5l5xdc9cf2apnpgx73mqlz2njrpi1w52dgql4qxv3kg6gr"))))
    (build-system gnu-build-system)
    (native-inputs `(("texinfo" ,texinfo)))
    (inputs `(("emacs" ,emacs)
              ("git" ,git)
              ("git:gui" ,git "gui")))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules ((guix build gnu-build-system)
                           (guix build utils)
                           (guix build emacs-utils))
       #:tests? #f  ; no check target
       #:phases
       (alist-replace
        'configure
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out")))
            (substitute* "Makefile"
              (("/usr/local") out)
              (("/etc") (string-append out "/etc")))))
        (alist-cons-before
         'build 'patch-exec-paths
         (lambda* (#:key inputs #:allow-other-keys)
           (let ((git (assoc-ref inputs "git"))
                 (git:gui (assoc-ref inputs "git:gui")))
             (emacs-substitute-variables "magit.el"
               ("magit-git-executable" (string-append git "/bin/git"))
               ("magit-gitk-executable" (string-append git:gui "/bin/gitk")))))
         (alist-cons-after
          'install 'post-install
          (lambda* (#:key outputs #:allow-other-keys)
            (emacs-generate-autoloads
             ,name (string-append (assoc-ref outputs "out")
                                  "/share/emacs/site-lisp/")))
          %standard-phases)))))
    (home-page "http://magit.github.io/")
    (synopsis "Emacs interface for the Git version control system")
    (description
     "With Magit, you can inspect and modify your Git repositories with Emacs.
You can review and commit the changes you have made to the tracked files, for
example, and you can browse the history of past changes.  There is support for
cherry picking, reverting, merging, rebasing, and other common Git
operations.")
    (license gpl3+)))


;;;
;;; Web browsing.
;;;

(define-public emacs-w3m
  (package
    (name "emacs-w3m")
    (version "1.4.483+0.20120614")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://debian/pool/main/w/w3m-el/w3m-el_"
                                 version ".orig.tar.gz"))
             (sha256
              (base32 "0ms181gjavnfk79hhv5xl9llik4c6kj0w3c04kgyif8lcy2sxljx"))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,autoconf)))
    (inputs `(("w3m" ,w3m)
              ("imagemagick" ,imagemagick)
              ("emacs" ,emacs)))
    (arguments
     '(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules ((guix build gnu-build-system)
                           (guix build utils)
                           (guix build emacs-utils))
       #:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "--with-lispdir="
                              out "/share/emacs/site-lisp")
               (string-append "--with-icondir="
                              out "/share/images/emacs-w3m")))
       #:tests? #f  ; no check target
       #:phases
       (alist-cons-before
        'configure 'pre-configure
        (lambda _
          (zero? (system* "autoconf")))
        (alist-cons-before
         'build 'patch-exec-paths
         (lambda* (#:key inputs outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out"))
                (w3m (assoc-ref inputs "w3m"))
                (imagemagick (assoc-ref inputs "imagemagick"))
                (coreutils (assoc-ref inputs "coreutils")))
            (emacs-substitute-variables "w3m.el"
              ("w3m-command" (string-append w3m "/bin/w3m"))
              ("w3m-touch-command" (string-append coreutils "/bin/touch"))
              ("w3m-image-viewer" (string-append imagemagick "/bin/display"))
              ("w3m-icon-directory" (string-append out
                                                   "/share/images/emacs-w3m")))
            (emacs-substitute-variables "w3m-image.el"
              ("w3m-imagick-convert-program" (string-append imagemagick
                                                            "/bin/convert"))
              ("w3m-imagick-identify-program" (string-append imagemagick
                                                             "/bin/identify")))
            #t))
         (alist-replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (and (zero? (system* "make" "install" "install-icons"))
                 (with-directory-excursion
                     (string-append (assoc-ref outputs "out")
                                    "/share/emacs/site-lisp")
                   (for-each delete-file '("ChangeLog" "ChangeLog.1"))
                   (symlink "w3m-load.el" "w3m-autoloads.el")
                   #t)))
          %standard-phases)))))
    (home-page "http://emacs-w3m.namazu.org/")
    (synopsis "Simple Web browser for Emacs based on w3m")
    (description
     "Emacs-w3m is an emacs interface for the w3m web browser.")
    (license gpl2+)))

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
              ("emacs" ,emacs)))
    (arguments
     '(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules ((guix build gnu-build-system)
                           (guix build utils)
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
    (license gpl2+)))


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
     '(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules ((guix build gnu-build-system)
                           (guix build utils)
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
                          (string-append mp3info "/bin/mp3info"))))))
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
    (native-inputs `(("emacs" ,emacs)            ;for (guix build emacs-utils)
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
    (license gpl3+)))


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
    (native-inputs `(("emacs" ,emacs)))
    (home-page "http://savannah.nongnu.org/projects/bbdb/")
    (synopsis "Contact management utility for Emacs")
    (description
     "BBDB is the Insidious Big Brother Database for GNU Emacs.  It provides
an address book for email and snail mail addresses, phone numbers and the
like.  It can be linked with various Emacs mail clients (Message and Mail
mode, Rmail, Gnus, MH-E, and VM).  BBDB is fully customizable.")
    (license gpl3+)))
