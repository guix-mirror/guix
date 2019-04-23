;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2015, 2016, 2018 Christopher Lemmer Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2017, 2018, 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016, 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017, 2018, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2018, 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017, 2018 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 George Clemmer <myglc2@gmail.com>
;;; Copyright © 2017, 2018 Feng Shu <tumashu@163.com>
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017, 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2017, 2018, 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018 Sohom Bhattacharjee <soham.bhattacharjee15@gmail.com>
;;; Copyright © 2018 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2018, 2019 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Alex Branham <alex.branham@gmail.com>
;;; Copyright © 2018 Thorsten Wilms <t_w_@freenet.de>
;;; Copyright © 2018 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018, 2019 Brett Gilio <brettg@posteo.net>
;;; Copyright © 2019 Dimakakos Dimos <bendersteed@teknik.io>
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019 mikadoZero <mikadozero@yandex.com>
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

(define-module (gnu packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix cvs-download)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages code)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages music)
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
  #:use-module (gnu packages mail)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages wordnet)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;;;
;;; Emacs hacking.
;;;

(define-public emacs-geiser
  (package
    (name "emacs-geiser")
    (version "0.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/geiser/" version
                                 "/geiser-" version ".tar.gz"))
             (sha256
              (base32
               "0pj3l7p8d60c9b4vfprnv6g5l61d74pls4b5dvd84cn4ky9mzwjv"))))
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
    (inputs `(("guile" ,guile-2.2)))
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

(define-public geiser
  (deprecated-package "geiser" emacs-geiser))

(define-public emacs-paredit
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

(define-public paredit
  (deprecated-package "paredit" emacs-paredit))

(define-public git-modes
  (package
    (name "emacs-git-modes")
    (version "1.2.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/magit/git-modes")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08hy7rbfazs6grkpk54i82bz0i0c74zcjk96cip8970h6jn3mj72"))))
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
    (version "2.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/magit/with-editor.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bbzvxnjpxqyvi808isld025b3pcidn4r2xf8hnk9bmzcfdvdr6q"))))
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

(define-public emacs-magit
  ;; Version 2.90.1 has trouble loading the transient library,
  ;; so we use a more recent commit that fixes it.
  (let ((commit "b4aec016b5577afa8d889f258b499814d1bb1d94"))
    (package
      (name "emacs-magit")
      (version (git-version "2.90.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/magit/magit")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0zl7v6z0y50pcgqsf2r8c1k3r5nwjad9ba7r6sgrnf4rc62br7jv"))))
      (build-system gnu-build-system)
      (native-inputs `(("texinfo" ,texinfo)
                       ("emacs" ,emacs-minimal)))
      (inputs
       `(("git" ,git)
         ("perl" ,perl)))
      (propagated-inputs
       `(("dash" ,emacs-dash)
         ("with-editor" ,emacs-with-editor)
         ("transient" ,emacs-transient)))
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (guix build emacs-utils))
         #:imported-modules (,@%gnu-build-system-modules
                             (guix build emacs-utils))
         #:test-target "test"
         #:tests? #f                   ; tests are not included in the release

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
                              ,(package-version emacs-with-editor))
               (string-append "TRANSIENT_DIR="
                              (assoc-ref %build-inputs "transient")
                              "/share/emacs/site-lisp/guix.d/transient-"
                              ,(package-version emacs-transient)))

         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before
               'build 'patch-exec-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((perl (assoc-ref inputs "perl")))
                 (make-file-writable "lisp/magit-sequence.el")
                 (emacs-substitute-variables "lisp/magit-sequence.el"
                   ("magit-perl-executable" (string-append perl "/bin/perl")))
                 #t))))))
      (home-page "https://magit.vc/")
      (synopsis "Emacs interface for the Git version control system")
      (description
       "With Magit, you can inspect and modify your Git repositories with Emacs.
You can review and commit the changes you have made to the tracked files, for
example, and you can browse the history of past changes.  There is support for
cherry picking, reverting, merging, rebasing, and other common Git
operations.")
      (license license:gpl3+))))

(define-public magit
  (deprecated-package "magit" emacs-magit))

(define-public emacs-magit-svn
  (let ((commit "9e33ceee32f665db59909e1c00a667ccdd04178f"))
    (package
      (name "emacs-magit-svn")
      (version (git-version "2.2.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/magit/magit-svn")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1mlqz8dh6jy5rv72lgkxv253dgh73fmbaidskicypapvbl3lr6xy"))))
      (build-system trivial-build-system)
      (native-inputs `(("emacs" ,emacs-minimal)))
      (propagated-inputs `(("dash" ,emacs-dash)
                           ("with-editor" ,emacs-with-editor)
                           ("magit" ,emacs-magit)
                           ("transient" ,emacs-transient)))
      (arguments
       `(#:modules ((guix build utils)
                    (guix build emacs-utils))

         #:builder
         (begin
           (use-modules (guix build utils)
                        (guix build emacs-utils))

           (let ((emacs    (string-append (assoc-ref %build-inputs "emacs")
                                          "/bin/emacs"))
                 (magit    (string-append (assoc-ref %build-inputs "magit")
                                          "/share/emacs/site-lisp"))
                 (transient (string-append (assoc-ref %build-inputs "transient")
                                           "/share/emacs/site-lisp/guix.d/transient-"
                                           ,(package-version emacs-transient)))
                 (dash     (string-append (assoc-ref %build-inputs "dash")
                                          "/share/emacs/site-lisp/guix.d/dash-"
                                          ,(package-version emacs-dash)))
                 (with-editor (string-append (assoc-ref %build-inputs "with-editor")
                                             "/share/emacs/site-lisp/guix.d/with-editor-"
                                             ,(package-version emacs-with-editor)))
                 (source   (assoc-ref %build-inputs "source"))
                 (lisp-dir (string-append %output "/share/emacs/site-lisp")))

             (install-file (string-append source "/magit-svn.el")
                           lisp-dir)

             (with-directory-excursion lisp-dir
               (parameterize ((%emacs emacs))
                 (emacs-generate-autoloads ,name lisp-dir)
                 (setenv "EMACSLOADPATH"
                         (string-append ":" magit ":" transient
                                        ":" dash ":" with-editor))
                 (emacs-batch-eval '(byte-compile-file "magit-svn.el"))))
             #t))))
      (home-page "https://github.com/magit/magit-svn")
      (synopsis "Git-SVN extension to Magit")
      (description
       "This package is an extension to Magit, the Git Emacs mode, providing
support for Git-SVN.")
      (license license:gpl3+))))

(define-public magit-svn
  (deprecated-package "magit-svn" emacs-magit-svn))

(define-public emacs-magit-popup
  (package
    (name "emacs-magit-popup")
    (version "2.12.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/magit/magit-popup.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13riknyqr6vxqll80sfhvz165flvdz367rbd0pr5slb01bnfsi2i"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-info
           (lambda _
             (invoke "make" "info"))))))
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

(define-public emacs-treepy
  (package
    (name "emacs-treepy")
    (version "0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/volrath/treepy.el.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04zwm6gx9pxfvgfkizx6pvb1ql8pqxjyzqp8flz0432x0gq5nlxk"))))
    (build-system emacs-build-system)
    (home-page
     "https://github.com/volrath/treepy.el")
    (synopsis "Tree traversal tools")
    (description
     "Generic tools for recursive and iterative tree traversal based on
clojure.walk and clojure.zip respectively.")
    (license license:gpl3+)))

(define-public emacs-graphql
  (package
   (name "emacs-graphql")
   (version "0.1.1")
   (source (origin
             (modules '((guix build utils)))
             ;; Remove examples file with references to external packages as
             ;; they do not exist at compilation time.
             (snippet
              '(begin (delete-file "examples.el")
                 #t))
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/vermiculus/graphql.el.git")
                   (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sp0skc1rnhi39szfbq1i99pdgd3bhn4c15cff05iqhjy2d4hniw"))))
   (build-system emacs-build-system)
   (home-page
    "https://github.com/vermiculus/graphql.el")
   (synopsis "GraphQL utilities")
   (description
    "GraphQL.el provides a generally-applicable domain-specific language for
creating and executing GraphQL queries against your favorite web services.
GraphQL is a data query language and runtime designed and used to request and
deliver data to mobile and web apps.")
   (license license:gpl3+)))

(define-public emacs-ghub
  (package
    (name "emacs-ghub")
    (version "3.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/magit/ghub")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lp52qygyavddl1lrgsyb6mq7hcf9h89dy2pzya3mb2va49f0vvl"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-info
           (lambda _
             (invoke "make" "info"))))))
    (native-inputs
     `(("texinfo" ,texinfo)))
    (propagated-inputs
     `(("dash" ,emacs-dash)
       ("graphql" ,emacs-graphql)
       ("treepy" ,emacs-treepy)))
    (home-page "https://github.com/magit/ghub")
    (synopsis "Emacs client libraries for the APIs of various Git forges")
    (description
     "Ghub provides basic support for using the APIs of various Git forges from
Emacs packages.  It supports the REST APIs of Github, Github GraphQL, Gitlab,
Gitea, Gogs and Bitbucket.  It abstracts access to API resources using only a
handful of functions that are not resource-specific.")
    (license license:gpl3+)))

(define-public emacs-scribble-mode
  (let ((commit "34e9e5edb921813b6483e0fefa848efb6ee4b314")
        (version "0.0")
        (revision 0))
    (package
      (name "emacs-scribble-mode")
      (version (if (zero? revision)
                   version
                   (string-append version "-"
                                  (number->string revision)
                                  "." (string-take commit 7))))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/emacs-pe/scribble-mode.git")
                      (commit commit)))
                (sha256
                 (base32
                  "0598byqpz2q6yi2q4dwd77jj9z3n99z34d3an51s9m2za0nh1qvp"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/emacs-pe/scribble-mode")
      (synopsis "Emacs mode for editing the Scribble documentation syntax.")
      (description
       "This package provides basic syntax highlighting and editing support
for editing Racket's Scribble documentation syntax in Emacs.")
      (license license:gpl3+))))

(define-public emacs-unpackaged-el
  (let ((commit "f4df7f8dfea715e893b2223adda32545803f5cce")
        (revision "1"))
    (package
      (name "emacs-unpackaged-el")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alphapapa/unpackaged.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1yf3zrgqfhnr0az8gn1kqqwnhfi3nc0vbjkcagwcqwk3sp1jda86"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-org" ,emacs-org)
         ("emacs-s" ,emacs-s)
         ("emacs-use-package" ,emacs-use-package)))
      (home-page "https://github.com/alphapapa/unpackaged.el")
      (synopsis "Useful snippets of Emacs Lisp code")
      (description "This package provides Emacs Lisp utilities for a variety
of tasks, including version control, task management, and regex-based
replacement.")
      (license license:gpl3+))))

(define-public emacs-haskell-mode
  (package
    (name "emacs-haskell-mode")
    (version "16.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/haskell/haskell-mode")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1qk36y0v9fzass6785il65c6wb5cfj4ihhwkvgnzmbafpa8p4dvq"))
              (patches
               (search-patches ; backport test failure fixes
                "haskell-mode-unused-variables.patch"
                "haskell-mode-make-check.patch"))))
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
              ;; embed filename to fix test failure
              (let ((file "tests/haskell-cabal-tests.el"))
                (substitute* file
                  (("\\(buffer-file-name\\)")
                   (format #f "(or (buffer-file-name) ~s)" file))))
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
                (invoke "makeinfo" "haskell-mode.texi")
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

(define-public haskell-mode
  (deprecated-package "haskell-mode" emacs-haskell-mode))

(define-public emacs-flycheck
  (package
    (name "emacs-flycheck")
    (version "31")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/flycheck/flycheck/releases/download/"
                    version "/flycheck-" version ".tar"))
              (sha256
               (base32
                "01rnwan16m7cyyrfca3c5c60mbj2r3knkpzbhji2fczsf0wns240"))
              (modules '((guix build utils)))
              (snippet `(begin
                          ;; Change 'flycheck-version' so that it does not
                          ;; attempt to get its version from pkg-info.el.
                          (substitute* "flycheck.el"
                            (("\\(pkg-info-version-info 'flycheck\\)")
                             (string-append "\"" ,version "\"")))
                          #t))))
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

(define-public emacs-a
  (package
    (name "emacs-a")
    (version "0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/plexus/a.el.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00v9w6qg3bkwdhypq0ssf0phdh0f4bcq59c20lngd6vhk0204dqi"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/plexus/a.el/")
    (synopsis
     "Emacs library for dealing with association lists and hash tables")
    (description "@code{emacs-a} provides Emacs Lisp functions for dealing
with associative structures in a uniform and functional way.  These functions
can take association lists, hash tables, and in some cases vectors (where the
index is considered the key).")
    (license license:gpl3+)))

(define-public emacs-ace-jump-mode
  (package
    (name "emacs-ace-jump-mode")
    (version "2.0")
    (home-page "https://github.com/winterTTr/ace-jump-mode")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1bwvzh056ls2v7y26a0s4j5mj582dmds04lx4x6iqihs04ss74bb"))
              (file-name (git-file-name name version))))
    (build-system emacs-build-system)
    (synopsis "Cursor location minor mode for fast navigation")
    (description "Ace-jump-mode is a fast/direct cursor location minor
mode.  It creates an N-Branch search tree internally and marks all
the possible position with predefined keys in within the whole Emacs
view.  This allows you to move to the character, word, or line almost
directly.")
    (license license:gpl3+)))

(define-public emacs-anaphora
  (package
    (name "emacs-anaphora")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rolandwalker/anaphora.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11fgiy029sqz7nvdm7dcal95lacryz9zql0x5h05z48nrrcl4bib"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/rolandwalker/anaphora/")
    (synopsis "Anaphoric expressions for Emacs Lisp")
    (description "@code{emacs-anaphora} implements anaphoric expressions for
Emacs Lisp.

Anaphoric expressions implicitly create one or more temporary variables which
can be referred to during the expression.  This technique can improve clarity
in certain cases.  It also enables recursion for anonymous functions.")
    (license license:public-domain)))

(define-public emacs-xr
  (package
    (name "emacs-xr")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://elpa.gnu.org/packages/xr-" version ".tar"))
       (sha256
        (base32
         "16p68rj5h609pnbp7y0jiiq1zkbksh4qqi1q8yby3ldj5x9sfpwc"))))
    (build-system emacs-build-system)
    (home-page "http://elpa.gnu.org/packages/xr.html")
    (synopsis "Convert string regexp to rx notation")
    (description
     "This is an inverse companion to the @code{rx} package for translating
regexps in string form to the @code{rx} notation.  Its chief uses are:

@itemize
@item Migrating existing code to @code{rx} form, for better readability and
maintainability
@item Understanding complex regexp strings and finding errors in them
@end itemize

In addition to Emacs regexps, this package can also parse and troubleshoot
skip set strings, which are arguments to @code{skip-chars-forward} and
@code{skip-chars-backward}.")
    (license license:gpl3+)))

(define-public emacs-relint
  (package
    (name "emacs-relint")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://elpa.gnu.org/packages/relint-" version ".el"))
       (sha256
        (base32
         "17nyy3zqpqgs22lrrpdm0k56xnhj5l3y2y3lnmhcqw63xksgbsyk"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-xr" ,emacs-xr)))
    (home-page "https://github.com/mattiase/relint")
    (synopsis "Elisp regexp mistake finder")
    (description
     "Relint (regular expression lint) scans Elisp files for mistakes in
regexps, including deprecated syntax and bad practice.  It also checks the
regexp-like arguments to @code{skip-chars-forward} and
@code{skip-chars-backward}.")
    (license license:gpl3+)))


;;;
;;; Web browsing.
;;;

(define-public emacs-w3m
  ;; Emacs-w3m follows a "rolling release" model.
  (package
    (name "emacs-w3m")
    (version "2018-11-11")
    (source (origin
              (method cvs-fetch)
              (uri (cvs-reference
                    (root-directory
                     ":pserver:anonymous@cvs.namazu.org:/storage/cvsroot")
                    (module "emacs-w3m")
                    (revision version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0nvahdbjs12zg7zsk4gql02mvnv56cf1rwj2f5p42lwp3xvswiwp"))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,autoconf)
                     ("texinfo" ,texinfo)
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
       #:tests? #f                              ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoconf
           (lambda _
             (invoke "autoconf")))
         (add-before 'configure 'support-emacs!
           (lambda _
             ;; For some reason 'AC_PATH_EMACS' thinks that 'Emacs 26' is
             ;; unsupported.
             (substitute* "configure"
               (("EMACS_FLAVOR=unsupported")
                "EMACS_FLAVOR=emacs"))
             #t))
         (add-before 'build 'patch-exec-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (w3m (assoc-ref inputs "w3m"))
                   (imagemagick (assoc-ref inputs "imagemagick"))
                   (coreutils (assoc-ref inputs "coreutils")))
               (make-file-writable "w3m.el")
               (emacs-substitute-variables "w3m.el"
                 ("w3m-command" (string-append w3m "/bin/w3m"))
                 ("w3m-touch-command"
                  (string-append coreutils "/bin/touch"))
                 ("w3m-icon-directory"
                  (string-append out "/share/images/emacs-w3m")))
               (make-file-writable "w3m-image.el")
               (emacs-substitute-variables "w3m-image.el"
                 ("w3m-imagick-convert-program"
                  (string-append imagemagick "/bin/convert"))
                 ("w3m-imagick-identify-program"
                  (string-append imagemagick "/bin/identify")))
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "install" "install-icons")
             (with-directory-excursion
                 (string-append (assoc-ref outputs "out")
                                "/share/emacs/site-lisp")
               (for-each delete-file '("ChangeLog" "ChangeLog.1"))
               (symlink "w3m-load.el" "w3m-autoloads.el")
               #t))))))
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

(define-public emacs-emms
  (package
    (name "emacs-emms")
    (version "5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/emms/emms-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "149ddczyx6x10zn4mn8g0rll1rwf4yciv8x6j0qdnlbwszblx2x6"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Makefile"
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
                     (string-append "all: " rest " emms-print-metadata\n")))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils)
                  (ice-9 ftw))
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
                   (mp3info (assoc-ref inputs "mp3info"))
                   (mutagen (assoc-ref inputs "mutagen"))
                   (opus    (assoc-ref inputs "opus-tools")))
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
                 (emacs-substitute-variables "emms-info-opusinfo.el"
                   ("emms-info-opusinfo-program-name"
                    (string-append opus "/bin/opusinfo")))
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
                   (("\"mid3v2\"")
                    (string-append "\"" mutagen "/bin/mid3v2\"")))
                 #t))))
         (add-before 'install 'pre-install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The 'install' rule expects the target directories to exist.
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1")))
               (mkdir-p bin)
               (mkdir-p man1)

               ;; Ensure that files are not rejected by gzip
               (let ((early-1980 315619200)) ; 1980-01-02 UTC
                 (ftw "." (lambda (file stat flag)
                            (unless (<= early-1980 (stat:mtime stat))
                              (utime file early-1980 early-1980))
                            #t)))
               #t)))
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (symlink "emms-auto.el"
                        (string-append out "/share/emacs/site-lisp/"
                                       "emms-autoloads.el")))
             #t)))
       #:tests? #f))
    (native-inputs `(("emacs" ,emacs-minimal)    ;for (guix build emacs-utils)
                     ("texinfo" ,texinfo)))
    (inputs `(("alsa-utils" ,alsa-utils)
              ("flac" ,flac)            ;for metaflac
              ("vorbis-tools" ,vorbis-tools)
              ("mpg321" ,mpg321)
              ("taglib" ,taglib)
              ("mp3info" ,mp3info)
              ("mutagen" ,python-mutagen)
              ("opus-tools" ,opus-tools)))
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
  ;; A new mpv backend is included in Emms from 5.0.
  (deprecated-package "emacs-emms-player-mpv" emacs-emms))

(define-public emacs-emms-mode-line-cycle
  (package
    (name "emacs-emms-mode-line-cycle")
    (version "0.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/momomo5717/emms-mode-line-cycle")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0q80f0plch6k4lhs8c9qm3mfycfbp3kn5sjrk9zxgxwnn901y9mp"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emms" ,emacs-emms)))
    (home-page "https://github.com/momomo5717/emms-mode-line-cycle")
    (synopsis "Display the EMMS mode line as a ticker")
    (description
     "This is a minor mode for updating the EMMS mode-line string cyclically
within a specified width.  It is useful for displaying long track titles.")
    (license license:gpl3+)))


;;;
;;; Miscellaneous.
;;;

(define-public emacs-ace-link
  (package
    (name "emacs-ace-link")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abo-abo/ace-link.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "147dz79vg4ym5wg3d544bw2khdb2j3hr73rw4qfm64wf0q2dj0vk"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-avy" ,emacs-avy)))
    (home-page "https://github.com/abo-abo/ace-link")
    (synopsis "Quickly follow links in Emacs")
    (description
     "Currently, to jump to a link in a @code{Info-mode}, @code{help-mode},
@code{woman-mode}, @code{org-mode}, @code{eww-mode}, @code{compilation-mode},
@code{goto-address-mode} buffer, you can tab through the links to select the
one you want.  This is an O(N) operation, where the N is the amount of links.
This package turns this into an O(1) operation.  It does so by assigning a
letter to each link using avy.")
    (license license:gpl3+)))

(define-public emacs-bbdb
  (package
    (name "emacs-bbdb")
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
               '(begin
                  (substitute* "doc/Makefile.in"
                    (("^doc_DATA = .*$")
                     "doc_DATA =\n"))
                  #t))))
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

(define-public bbdb
  (deprecated-package "bbdb" emacs-bbdb))

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

(define-public emacs-link-hint
  ;; Last release was in 2015.
  (let ((commit "d74a483652486260c052941fedeadddb1ea71f88")
        (revision "1"))
    (package
      (name "emacs-link-hint")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/noctuid/link-hint.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0dghxd165fbds6czy9bfwpid3i4irgp3q08n9mg57sfifi0cmij0"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-avy" ,emacs-avy)))
      (home-page "https://github.com/noctuid/link-hint.el")
      (synopsis "Vimperator-style link-hinting in Emacs")
      (description "This package provides commands for visiting and acting on
links.")
      (license license:gpl3+))))

(define-public emacs-ag
  (package
    (name "emacs-ag")
    (version "0.47")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/Wilfred/ag.el.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15kp99vwyi7hb1jkq3lwvqzw3v62ycixsq6y4pd1x0nn2v5p5m5r"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-info
           (lambda _
             (with-directory-excursion "docs"
               (invoke "make" "info"))))
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
    (home-page "https://github.com/jwiegley/emacs-async")
    (version "1.9.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zsnb6dy8p6y68xgidv3dfxaga4biramfw8fq7wac0sc50vc98vq"))))
    (build-system emacs-build-system)
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
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sebastiansturm/autothemer.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0cd2pqh6k32sjidkcd8682y4l6mx52xw4a05f38kk8nsrk28m74k"))))
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

(define-public emacs-howm
  (package
    (name "emacs-howm")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://howm.sourceforge.jp/a/howm-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0ddm91l6z58j7x59fa966j6q1rg4cinyza4r8ibg80hprn5h31qk"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("emacs" ,emacs-minimal)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-howmdir=" %output
                            "/share/emacs/site-lisp/guix.d/howm-" ,version))
       #:modules ((guix build gnu-build-system)
                  ((guix build emacs-build-system) #:prefix emacs:)
                  (guix build utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-build-system)
                           (guix build emacs-utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'make-autoloads
           (assoc-ref emacs:%standard-phases 'make-autoloads)))))
    (home-page "http://howm.osdn.jp/")
    (synopsis "Note-taking tool for Emacs")
    (description "Howm is a note-taking tool for Emacs.  Like
code@{emacs-wiki.el}, it facilitates using hyperlinks and doing full-text
searches.  Unlike code@{emacs-wiki.el}, it can be combined with any format.")
    (license license:gpl1+)))

(define-public emacs-calfw
  (package
    (name "emacs-calfw")
    (version "1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/kiwanami/emacs-calfw.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0r42cagvmvvib76kd15nd9ix55ys6i549vxnls4z16s864695zpa"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-howm" ,emacs-howm)))
    (home-page "https://github.com/kiwanami/emacs-calfw/")
    (synopsis "Calendar framework for Emacs")
    (description
     "This package displays a calendar view with various schedule data in the
Emacs buffer.")
    (license license:gpl3+)))

(define-public emacs-direnv
  (package
    (name "emacs-direnv")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/wbolster/emacs-direnv.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "02blhinkkfh3iai6j1k5swplf5mkwijy3p7wy38rnd1gnyj2z4la"))))
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
    (version "0.8.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://elpa.gnu.org/packages/ggtags-"
                           version ".el"))
       (sha256
        (base32
         "1qa7lcrcmf76sf6dy8sxbg4adq7rg59fm0n5848w3qxgsr0h45fg"))))
    (build-system emacs-build-system)
    (inputs
     `(("global" ,global)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (chmod "ggtags.el" #o644)
             (emacs-substitute-variables "ggtags.el"
               ("ggtags-executable-directory"
                (string-append (assoc-ref inputs "global") "/bin")))
             #t)))))
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
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dominikh/go-mode.el.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nd2h50yb0493wvf1h7fzplq45rmqn2w7kxpgnlxzhkvq99v8vzf"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-writable
           (lambda _
             (for-each make-file-writable (find-files "." "\\.el$"))
             #t)))))
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
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/jd/google-maps.el.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "183igr5lp20zcqi7rc01fk76sfxdhksd74i11v16gdsifdkjimd0"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/jd/google-maps.el")
    (synopsis "Access Google Maps from Emacs")
    (description "The @code{google-maps} package allows to display Google
Maps directly inside Emacs.")
    (license license:gpl3+)))

(define-public emacs-graphviz-dot-mode
  (let ((commit "c456a2b65c734089e6c44e87209a5a432a741b1a")
        (revision "1"))
    (package
      (name "emacs-graphviz-dot-mode")
      (version (string-append "0.3.11-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ppareit/graphviz-dot-mode.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0j1r2rspaakw37b0mx7pwpvdsvixq9sw3xjbww5piihzpdxz58z1"))))
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
                 (invoke "make"
                         "clean"
                         "info"
                         (string-append "TEXINFODIR="
                                        (assoc-ref inputs "texinfo")
                                        "/bin")))))
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
    (version "0.5.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/purcell/mmm-mode.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lxd55yhz0ag7v1ydff55bg4h8snq5lbk8cjwxqpyq6gh4v7md1h"))))
    (build-system gnu-build-system)
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
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/politza/tablist.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gy9hxm7bca0l1hfy2pzn86avpifrz3bs8xzpicj4kxw5wi4ygns"))))
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
    (version "0.90")
    (home-page "https://github.com/politza/pdf-tools")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0iv2g5kd14zk3r5dzdw7b7hk4b5w7qpbilcqkja46jgxbb6xnpl9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
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
             (invoke "bash" "autogen.sh")))

         ;; Build emacs side using 'emacs-build-system'.
         (add-after 'compress-documentation 'enter-lisp-dir
           (lambda _ (chdir "../lisp") #t))
         (add-after 'enter-lisp-dir 'emacs-patch-variables
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each make-file-writable (find-files "."))

             ;; Set path to epdfinfo program.
             (emacs-substitute-variables "pdf-info.el"
               ("pdf-info-epdfinfo-program"
                (string-append (assoc-ref outputs "out")
                               "/bin/epdfinfo")))
             ;; Set 'pdf-tools-handle-upgrades' to nil to avoid "auto
             ;; upgrading" that pdf-tools tries to perform.
             (emacs-substitute-variables "pdf-tools.el"
               ("pdf-tools-handle-upgrades" '()))))
         (add-after 'emacs-patch-variables 'emacs-set-emacs-load-path
           (assoc-ref emacs:%standard-phases 'set-emacs-load-path))
         (add-after 'emacs-set-emacs-load-path 'emacs-install
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
    (license license:gpl3+)))

(define-public emacs-dash
  (package
    (name "emacs-dash")
    (version "2.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/magnars/dash.el.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dx8q3jr8fh73cfl7mqi5dq0012ambpvc74d2c71fsv9rfw85693"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:test-command '("./run-tests.sh")))
    (home-page "https://github.com/magnars/dash.el")
    (synopsis "Modern list library for Emacs")
    (description "This package provides a modern list API library for Emacs.")
    (license license:gpl3+)))

(define-public emacs-bui
  (package
    (name "emacs-bui")
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://notabug.org/alezost/emacs-bui.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sszdl4kvqbihdh8d7mybpp0d8yw2p3gyiipjcxz9xhvvmw3ww4x"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("dash" ,emacs-dash)))
    (home-page "https://notabug.org/alezost/emacs-bui")
    (synopsis "Buffer interface library for Emacs")
    (description
     "BUI (Buffer User Interface) is a library for making @code{list} and
@code{info} interfaces to display an arbitrary data of the same
type, for example: packages, buffers, files, etc.")
    (license license:gpl3+)))

(define-public emacs-guix
  (package
    (name "emacs-guix")
    (version "0.5.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://emacs-guix.gitlab.io/website/"
                                  "releases/emacs-guix-" version ".tar.gz"))
              (sha256
               (base32
                "1gxg7lan3njc2yg2d02b2zij0d2cm2pv2q08nqz86s85jk3b6m03"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (let ((guix        (assoc-ref %build-inputs "guix"))
             (gcrypt      (assoc-ref %build-inputs "guile-gcrypt"))
             (geiser      (assoc-ref %build-inputs "geiser"))
             (dash        (assoc-ref %build-inputs "dash"))
             (bui         (assoc-ref %build-inputs "bui"))
             (magit-popup (assoc-ref %build-inputs "magit-popup"))
             (edit-indirect (assoc-ref %build-inputs "edit-indirect"))
             (site-lisp   "/share/emacs/site-lisp")
             (site-scm    "/share/guile/site")
             (site-go     "/lib/guile")
             (guile-dir (lambda (dir)
                          (car (find-files dir
                                           (lambda (file stat)
                                             (string-prefix?
                                              "2." (basename file)))
                                           #:directories? #t)))))
         (list (string-append "--with-guix-site-dir="
                              (guile-dir (string-append guix site-scm)))
               (string-append "--with-guix-site-ccache-dir="
                              (guile-dir (string-append guix site-go))
                              "/site-ccache")
               (string-append "--with-guile-gcrypt-site-dir="
                              (guile-dir (string-append gcrypt site-scm)))
               (string-append "--with-guile-gcrypt-site-ccache-dir="
                              (guile-dir (string-append gcrypt site-go))
                              "/site-ccache")
               (string-append "--with-geiser-lispdir=" geiser site-lisp)
               (string-append "--with-dash-lispdir="
                              dash site-lisp "/guix.d/dash-"
                              ,(package-version emacs-dash))
               (string-append "--with-bui-lispdir="
                              bui site-lisp "/guix.d/bui-"
                              ,(package-version emacs-bui))
               (string-append "--with-editindirect-lispdir="
                              edit-indirect site-lisp "/guix.d/edit-indirect-"
                              ,(package-version emacs-edit-indirect))
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
     `(("geiser" ,emacs-geiser)
       ("guile-gcrypt" ,guile-gcrypt)
       ("dash" ,emacs-dash)
       ("bui" ,emacs-bui)
       ("edit-indirect" ,emacs-edit-indirect)
       ("magit-popup" ,emacs-magit-popup)))
    (home-page "https://emacs-guix.gitlab.io/website/")
    (synopsis "Emacs interface for GNU Guix")
    (description
     "Emacs-Guix provides a visual interface, tools and features for the GNU
Guix package manager.  Particularly, it allows you to do various package
management tasks from Emacs.  To begin with, run @code{M-x guix-about} or
@code{M-x guix-help} command.")
    (license license:gpl3+)))

(define-public emacs-build-farm
  (package
    (name "emacs-build-farm")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://notabug.org/alezost/emacs-build-farm.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i0bwbav5861j2y15j9nd5m9rdqg9q97zgcbld8pivr9nyxy63lz"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("bui" ,emacs-bui)
       ("magit-popup" ,emacs-magit-popup)))
    (home-page "https://notabug.org/alezost/emacs-build-farm")
    (synopsis "Emacs interface for Hydra and Cuirass build farms")
    (description
     "This Emacs package provides an interface for Hydra and
Cuirass (build farms used by Nix and Guix).  It allows you to look at
various data related to the build farm projects, jobsets, builds and
evaluations.  The entry point is @code{M-x build-farm} command.")
    (license license:gpl3+)))

(define-public emacs-d-mode
  (package
    (name "emacs-d-mode")
    (version "2.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fzplvi1sm8k2sabfdvrd7j2xypwqh0g9v1mxa75dajdmcd85zpj"))))
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
              (file-name (git-file-name name version))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/magnars/s.el.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g8mqd13llj007al4nlxxx4z2lcsg3wk970mgjn0avwrhjjgdmmv"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:emacs ,emacs                   ; FIXME: tests fail with emacs-minimal
       #:test-command '("./run-tests.sh")))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zk-phi/symon.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q7di9s8k710nx98wnqnbkkhdimrn0jf6z4xkm4c78l6s5idjwlz"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/zk-phi/symon")
    (synopsis "Tiny graphical system monitor")
    (description
     "Tiny graphical system monitor for the Emacs minibuffer when idle.")
    (license license:gpl2+)))

(define-public emacs-sx
  (let ((version "20180212")
        (revision "1")
        (commit "833435fbf90d1c9e927d165b155f3b1ef39271de"))
    (package
      (name "emacs-sx")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/vermiculus/sx.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1369xaxq1vy3d9yh862ddnhddikdpg2d0wv1ly00pnvdp9v4cqgd"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-markdown-mode" ,emacs-markdown-mode)))
      (home-page "https://github.com/vermiculus/sx.el")
      (synopsis "Emacs StackExchange client")
      (description
       "Emacs StackExchange client.  Ask and answer questions on
Stack Overflow, Super User, and other StackExchange sites.")
      (license license:gpl3+))))

(define-public emacs-f
  (package
    (name "emacs-f")
    (version "0.20.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rejeep/f.el.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a47xk3yp1rp17fqg7ldl3d3fb888h0fz3sysqfdz1bfdgs8a9bk"))))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/syohex/emacs-git-gutter.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1abagq0psip7cgsqbfjv72qy60ywsny0ibsfcn74ldj6a9v17mz5"))))
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
    (version "4.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.com/pidu/git-timemachine"
                           "/-/archive/" version
                           "/git-timemachine-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ii40qcincasg7s1yrvqcxkqcqzb4sfs7gcxscn6m4x4ans165zy"))))
    (build-system emacs-build-system)
    (home-page "https://gitlab.com/pidu/git-timemachine")
    (synopsis "Step through historic versions of Git-controlled files")
    (description "This package enables you to step through historic versions
of files under Git version control from within Emacs.")
    (license license:gpl3+)))

(define-public emacs-minitest
  (let ((commit "1aadb7865c1dc69c201cecee275751ecec33a182")
        (revision "1"))
    (package
      (name "emacs-minitest")
      (version (git-version "0.8.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/arthurnn/minitest-emacs")
                      (commit commit)))
                (file-name (git-file-name name commit))
                (sha256
                 (base32
                  "1l18zqpdzbnqj2qawq8hj7z7pl8hr8z9d8ihy8jaiqma915hmhj1"))))
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
      (license license:expat))))

(define-public emacs-el-mock
  (package
    (name "emacs-el-mock")
    (version "1.25.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rejeep/el-mock.el.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13mv1rhgkwiww2wh5w926jz7idppp492wir1vdl245c5x50dh4f7"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ecukes/espuds.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16r4j27j9yfdiy841w9q5ykkc6n3wrm7hvfacagb32mydk821ijg"))))
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

(define-public emacs-spark
  (let ((version "20160503")  ; no proper tag, use date of commit
        (commit "0bf148c3ede3b31d56fd75f347cdd0b0eae60025")
        (revision "1"))
    (package
      (name "emacs-spark")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alvinfrancis/spark.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1ykqr86j17mi95s08d9fp02d7ych1331b04dcqxzxnmpkhwngyj1"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/alvinfrancis/spark")
      (synopsis "Sparkline generation library for Emacs Lisp")
      (description "@code{emacs-spark} is a sparkline generation library for
Emacs Lisp.  It generates a sparkline string given a list of numbers.  It is a
port of @code{cl-spark} to Emacs Lisp.")
      (license license:expat))))

(define-public emacs-es-mode
  (package
    (name "emacs-es-mode")
    (version "4.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dakrone/es-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qhfnd5anp5qrmravv7ks5ix763xnki2f5jwcyj70qyxwr0l60cg"))))
    (build-system emacs-build-system)
    (propagated-inputs
     ;; The version of org in Emacs 24.5 is not sufficient, and causes tables
     ;; to be rendered incorrectly
     `(("emacs-dash" ,emacs-dash)
       ("emacs-org" ,emacs-org)
       ("emacs-spark" ,emacs-spark)))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/magnars/expand-region.el.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h40dhc3kn8fq86xnwi5lz7ql8my8737y7wkqr897p15y90swr35"))))
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
    (version "1.89")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alpaker/Fill-Column-Indicator.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "010kf8jsly74y7m6mmkn1h6y205kz23zphs50zgy2nag2p88rz9y"))))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ZungBang/emacs-grep-a-lot.git")
             (commit "9f9f645b9e308a0d887b66864ff97d0fca1ba4ad")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f8262mrlinzgnn4m49hbj1hm3c1mvzza24py4b37sasn49546lw"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nonsequitur/inf-ruby.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r452h6cyypqlc59q8dx5smkwhck4qjcg1pf9qdw539cpva5q77z"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cask/shut-up.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "103yvfgkj78i4bnv1fwk76izsa8h4wyj3vwj1vq7xggj607hkxzq"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sviridov/undercover.el.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06qcvbp5rd0kh3ibrxj5p6r578lwsrgd7yj5c6slwmkdmna2fj33"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tarsius/paren-face.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f128gqn170s6hl62n44i9asais75ns1mpvb4l8vzy1sc0v16c0k"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/purcell/page-break-lines.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wp974716ih2cz9kdmdz7xwjy1qnnfzdzlfr9kchknagw8d9nn12"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skeeto/emacs-web-server.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qmkc0w28l53zzf5yd2grrk1sq222g5qnsm35ph25s1cfvc1qb2g"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skeeto/skewer-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05jndz0c26q60s416vqgvr66axdmxb7qsr2g70fvl5iqavnayhpv"))))
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
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k0sm552iawi49v4zis6dbb81d1rzgky9v0dpv7nj31gnb7bmy7k"))))
    (build-system emacs-build-system)
    (native-inputs
     `(("ert-runner" ,emacs-ert-runner)))
    (arguments
     `(#:tests? #t
       #:test-command '("ert-runner")))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sabof/stripe-buffer.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "035ym1c1vzg6hjsnd258z4dkrfc11lj4c0y4gpgybhk54dq3w9dk"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Malabarba/rich-minority.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11hwf9y5ax207w6rwrsmi3pmn7pn7ap6iys0z8hni2f5zzxjrmx3"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgutov/robe.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ll7ivxqnglfb0i70ly6qq2yfw9cyi3vq3lmj4s6h6c1c7rm3gcq"))))
    (build-system emacs-build-system)
    (arguments
     '(#:include (cons "^lib\\/" %default-include)))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pezra/rspec-mode.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hrn5n7aaymwimk511kjij44vqaxbmhly1gwmlmsrnbvvma7f2mp"))))
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
    (version "2.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Malabarba/smart-mode-line.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gs4ay9hdg8gmia4ir74qawk80pqwv99hp4yhy108kpfry5mrq6z"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DamienCassou/shell-switcher.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ia7sdip4hl27avckv3qpqgm3k4ynvp3xxq1cy53bqfzzx0gcria"))))
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
              (file-name (git-file-name name version))
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
    (version "0.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/debbugs-"
                                  version ".tar"))
              (sha256
               (base32
                "0zclh2nxj5p10n214sxyxy3ca07l5s9p5spk0xp1smr6nzn0w7gb"))))
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

(define-public emacs-ert-expectations
  (package
    (name "emacs-ert-expectations")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://www.emacswiki.org/emacs/download/ert-expectations.el")
       (sha256
        (base32
         "0cwy3ilsid90abzzjb7ha2blq9kmv3gfp3icwwfcz6qczgirq6g7"))))
    (build-system emacs-build-system)
    (home-page "https://www.emacswiki.org/emacs/ert-expectations.el")
    (synopsis "Simple unit test framework for Emacs Lisp")
    (description "@code{emacs-ert-expectations} is a simple unit test
framework for Emacs Lisp to be used with @code{ert}.")
    (license license:gpl3+)))

(define-public emacs-deferred
  (package
    (name "emacs-deferred")
    (version "0.5.1")
    (home-page "https://github.com/kiwanami/emacs-deferred")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xy9zb6wwkgwhcxdnslqk52bq3z24chgk6prqi4ks0qcf2bwyh5h"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-shell
           ;; Setting the SHELL environment variable is required for the tests
           ;; to find sh.
           (lambda _
             (setenv "SHELL" (which "sh"))
             #t))
         (add-before 'check 'fix-makefile
           (lambda _
             (substitute* "Makefile"
               (("\\$\\(CASK\\) exec ") ""))
             #t)))
       #:tests? #t
       #:test-command '("make" "test")))
    (native-inputs
     `(("emacs-ert-expectations" ,emacs-ert-expectations)
       ("emacs-undercover" ,emacs-undercover)
       ("ert-runner" ,emacs-ert-runner)))
    (synopsis "Simple asynchronous functions for Emacs Lisp")
    (description
     "The @code{deferred.el} library provides support for asynchronous tasks.
The API is almost the same as that of
@uref{https://github.com/cho45/jsdeferred, JSDeferred}, a JavaScript library
for asynchronous tasks.")
    (license license:gpl3+)))

(define-public emacs-butler
  (package
    (name "emacs-butler")
    (version "0.2.4")
    (home-page "https://github.com/AshtonKem/Butler")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pii9dw4skq7nr4na6qxqasl36av8cwjp71bf1fgppqpcd9z8skj"))))
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
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/company-mode/company-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gpapjxs4l6fmmj22q0q1pyhj1yd9j5iqfqnjf1abskkj69lqkpj"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-bin-dir
           (lambda _
             ;; The company-files-candidates-normal-root test looks
             ;; for the /bin directory, but the build environment has
             ;; no /bin directory. Modify the test to look for the
             ;; /tmp directory.
             (substitute* "test/files-tests.el"
               (("/bin/") "/tmp/"))
             #t)))
       #:tests? #t
       #:test-command '("make" "test-batch")))
    (home-page "http://company-mode.github.io/")
    (synopsis "Modular text completion framework")
    (description
     "Company is a modular completion mechanism.  Modules for retrieving
completion candidates are called back-ends, modules for displaying them are
front-ends.  Company comes with many back-ends, e.g. @code{company-elisp}.
These are distributed in separate files and can be used individually.")
    (license license:gpl3+)))

(define-public emacs-irony-mode
  (package
    (name "emacs-irony-mode")
    (version "1.2.0")
    (home-page "https://github.com/Sarcasm/irony-mode")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append home-page ".git"))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nhjrnlmss535jbshjjd30vydbr8py21vkx4p294w6d8vg2rssf8"))))
    (build-system emacs-build-system)
    (inputs `(("server" ,emacs-irony-mode-server)))
    (arguments `(#:phases
                 (modify-phases %standard-phases
                   (add-after 'unpack 'configure
                        (lambda* (#:key inputs #:allow-other-keys)
                          (chmod "irony.el" #o644)
                          (emacs-substitute-variables "irony.el"
                            ("irony-server-install-prefix"
                             (assoc-ref inputs "server")))
                          #t)))))
    (synopsis "C/C++/ObjC Code completion and syntax checks for Emacs")
    (description "Irony-mode provides Clang-assisted syntax checking and
completion for C, C++, and ObjC in GNU Emacs.  Using @code{libclang} it can
provide syntax checking and autocompletion on compiler level which is very
resistant against false positives.  It also integrates well with other
packages like @code{eldoc-mode} and especially @code{company-mode} as
described on the homepage.")
    (license license:gpl3+)))

(define-public emacs-irony-mode-server
  (package (inherit emacs-irony-mode)
    (name "emacs-irony-mode-server")
    (inputs
     `(("clang" ,clang)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "cmake"
                       "server"
                       (string-append "-DCMAKE_INSTALL_PREFIX=" out)) #t))))))
    (build-system cmake-build-system)
    (synopsis "Server for the Emacs @dfn{irony mode}")))

(define-public emacs-company-irony
  (package
    (name "emacs-company-irony")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Sarcasm/company-irony.git")
                    (commit (string-append "v" version))))
              (sha256 (base32
                       "1qgyam2vyjw90kpxns5cd6bq3qiqjhzpwrlvmi18vyb69qcgqd8a"))
              (file-name (git-file-name name version))))
    (build-system emacs-build-system)
    (inputs
     `(("emacs-irony-mode" ,emacs-irony-mode)
       ("emacs-company" ,emacs-company)))
    (synopsis "C++ completion backend for Company using irony-mode")
    (description "This backend for company-mode allows for C++ code completion
with irony-mode using clang-tooling.")
    (home-page "https://github.com/Sarcasm/company-irony")
    (license license:gpl3+)))

(define-public emacs-flycheck-irony
  (package
    (name "emacs-flycheck-irony")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Sarcasm/flycheck-irony.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0qa5a8wzvzxwqql92ibc9s43k8sj3vwn7skz9hfr8av0skkhx996"))))
    (build-system emacs-build-system)
    (inputs
     `(("irony-mode" ,emacs-irony-mode)
       ("flycheck-mode" ,emacs-flycheck)
       ("emacs-company" ,emacs-company)))
    (synopsis "Live syntax checking frontend for Flycheck using irony-mode")
    (description "This package provides a frontend for Flycheck that lets
irony-mode do the syntax checking.")
    (home-page "https://github.com/Sarcasm/flycheck-irony")
    (license license:gpl3+)))

(define-public emacs-irony-eldoc
  (package
    (name "emacs-irony-eldoc")
    (version (package-version emacs-irony-mode))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ikirill/irony-eldoc.git")
             (commit "0df5831eaae264a25422b061eb2792aadde8b3f2")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l5qpr66v1l12fb50yh73grb2rr85xxmbj19mm33b5rdrq2bqmmd"))))
    (build-system emacs-build-system)
    (inputs
     `(("irony-mode" ,emacs-irony-mode)))
    (synopsis "Eldoc integration for irony-mode")
    (description "Irony-eldoc is an eldoc extension that shows documentation
for the current function or variable in the minibuffer.")
    (home-page "https://github.com/ikirill/irony-eldoc")
    (license license:gpl3+)))

(define-public emacs-company-quickhelp
  (package
    (name "emacs-company-quickhelp")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/expez/company-quickhelp.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08ccsfvwdpzpj0gai3xrdb2bv1nl6myjkxsc5774pbvlq9nkfdvr"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/magnars/multiple-cursors.el.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ijgvzv5r44xqvz751fd5drbvrspapw6xwv47582w255j363r6ss"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/magnars/multiple-cursors.el")
    (synopsis "Multiple cursors for Emacs")
    (description
     "This package adds support to Emacs for editing text with multiple
simultaneous cursors.")
    (license license:gpl3+)))

(define-public emacs-typo
  (package
    (name "emacs-typo")
    (version "1.1")
    (home-page "https://github.com/jorgenschaefer/typoel")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jhd4grch5iz12gyxwfbsgh4dmz5hj4bg4gnvphccg8dsnni05k2"))))
    (build-system emacs-build-system)
    (synopsis "Minor mode for typographic editing")
    (description
     "This package provides two Emacs modes, @code{typo-mode} and
@code{typo-global-mode}.  These modes automatically insert Unicode characters
for quotation marks, dashes, and ellipses.  For example, typing @kbd{\"}
automatically inserts a Unicode opening or closing quotation mark, depending
on context.")
    (license license:gpl3+)))

(define-public emacs-company-lsp
  (package
    (name "emacs-company-lsp")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tigersoldier/company-lsp.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hy1x2w0yp5brm7714d1hziz3rpkywb5jp3yj78ibmi9ifny9vri"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-lsp-mode" ,emacs-lsp-mode)
       ("emacs-company" ,emacs-company)
       ("emacs-s" ,emacs-s)
       ("emacs-dash" ,emacs-dash)))
    (home-page "https://github.com/tigersoldier/company-lsp")
    (synopsis "Completion for @code{lsp-mode}")
    (description
     "This package provides completion features that are not possible with
@code{lsp-mode} and @code{company-capf} alone, including support for trigger
characters and asynchronous fetching of completion candidates.")
    (license license:gpl3+)))

(define-public emacs-scheme-complete
  (let ((commit "9b5cf224bf2a5994bc6d5b152ff487517f1a9bb5"))
    (package
      (name "emacs-scheme-complete")
      (version (string-append "20151223." (string-take commit 8)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ashinn/scheme-complete.git")
               (commit commit)))
         (file-name (git-file-name name version))
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

(define-public emacs-scel
  (let ((version "20170629")
        (revision "1")
        (commit "aeea3ad4be9306d14c3a734a4ff54fee10ac135b"))
    (package
      (name "emacs-scel")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/supercollider/scel.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0jvmzs1lsjyndqshhii2y4mnr3wghai26i3p75453zrpxpg0zvvw"))))
      (build-system emacs-build-system)
      (arguments
       `(#:modules ((guix build emacs-build-system)
                    ((guix build cmake-build-system) #:prefix cmake:)
                    (guix build utils))
         #:imported-modules (,@%emacs-build-system-modules
                             (guix build cmake-build-system))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "el/CMakeLists.txt"
                 (("share/emacs/site-lisp/SuperCollider")
                  (string-append
                   "share/emacs/site-lisp/guix.d/scel-" ,version)))
               ((assoc-ref cmake:%standard-phases 'configure)
                #:outputs outputs
                #:configure-flags '("-DSC_EL_BYTECOMPILE=OFF"))))
           (add-after 'set-emacs-load-path 'add-el-dir-to-emacs-load-path
             (lambda _
               (setenv "EMACSLOADPATH"
                       (string-append (getcwd) "/el:" (getenv "EMACSLOADPATH")))
               #t))
           (replace 'install (assoc-ref cmake:%standard-phases 'install)))))
      (inputs
       `(("supercollider" ,supercollider)))
      (native-inputs
       `(("cmake" ,cmake)))
      (home-page "https://github.com/supercollider/scel")
      (synopsis "SuperCollider Emacs interface")
      (description "@code{emacs-scel} is an Emacs interface to SuperCollider.
SuperCollider is a platform for audio synthesis and algorithmic composition.")
      (license license:gpl2+))))

(define-public emacs-company-auctex
  (let ((commit "48c42c58ce2f0e693301b0cb2d085055410c1b25")
        (revision "1"))
    (package
      (name "emacs-company-auctex")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alexeyr/company-auctex")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "10qn7frn5wcmrlci3v6iliqzj7r9dls87h9zp3xkgrgn4bqprfp8"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-yasnippet" ,emacs-yasnippet)
         ("emacs-auctex" ,emacs-auctex)
         ("emacs-company" ,emacs-company)))
      (home-page "https://github.com/alexeyr/company-auctex/")
      (synopsis "Completion for @code{AUCTeX}")
      (description
       "This package provides a group of backends permitting auto-completion
for @code{AUCTeX}.")
      (license license:gpl3+))))

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
           (delete-file-recursively "6.945-config")
           #t))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fedeinthemix/emacs-constants.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f5sh6b45a8p9kancjp9jk44ws8ww6x50a0i6zgyyvr7dbvii1a0"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/magnars/tagedit.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kq40g46s8kgiafrhdq99h79rz9h5fvgz59k7ralmf86bl4sdmdb"))))
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
    (version "2.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/slime/slime.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i637n0ragpbj39hqx65nx5k99xf0464c4w6w1qpzykm6z42grky"))))
    (build-system emacs-build-system)
    (native-inputs
     `(("texinfo" ,texinfo)))
    (arguments
     `(#:include '("\\.el$" "\\.lisp$" "\\.asd$" "contrib")
       #:exclude '("^slime-tests.el" "^contrib/test/"
                   "^contrib/Makefile$" "^contrib/README.md$")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
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
                 (invoke "make" "html/index.html")
                 (invoke "make" "slime.info")
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
    (license (list license:gpl2+ license:public-domain))))

(define-public emacs-popup
  (package
    (name "emacs-popup")
    (version "0.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/auto-complete/popup-el.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y538siabcf1n00wr4iz5gbxfndw661kx2mn9w1g4lg7yi4n0h0h"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/auto-complete/popup-el")
    (synopsis "Visual Popup User Interface for Emacs")
    (description
     "Popup.el is a visual popup user interface library for Emacs.
This provides a basic API and common UI widgets such as popup tooltips
and popup menus.")
    (license license:gpl3+)))

(define-public emacs-python-environment
  (package
    (name "emacs-python-environment")
    (version "0.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tkf/emacs-python-environment/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q6bib9nr6xiq6npzbngyfcjk87yyvwzq1zirr3z1h5wadm34lsk"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-deferred" ,emacs-deferred)))
    (home-page "https://github.com/tkf/emacs-python-environment")
    (synopsis "Provides a @code{virtualenv} API in Emacs Lisp")
    (description
     "This package permits automated installation of tools written in Python.")
    (license license:gpl3+)))

(define-public emacs-jedi
  (package
    (name "emacs-jedi")
    (version "0.2.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tkf/emacs-jedi/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xbp9fcxgbf298w05hvf52z41kk7r52975ailgdn8sg60xc98fa7"))))
    (build-system emacs-build-system)
    (native-inputs
     `(("emacs-mocker" ,emacs-mocker)))
    (propagated-inputs
     `(("emacs-auto-complete" ,emacs-auto-complete)
       ("emacs-python-environment" ,emacs-python-environment)
       ("emacs-epc" ,emacs-epc)))
    (home-page "https://github.com/tkf/emacs-jedi")
    (synopsis "Provides Python completion in Emacs")
    (description
     "This package provides completion in Python buffers and also helps find
the locations of docstrings, arguments, and functions.")
    (license license:gpl3+)))

(define-public emacs-company-jedi
  (package
    (name "emacs-company-jedi")
    (version "0.04")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/syohex/emacs-company-jedi")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ihqapp4dv92794rsgyq0rmhwika60cmradqd4bn9b72ss6plxs1"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-jedi" ,emacs-jedi)
       ("emacs-company" ,emacs-company)))
    (home-page "https://github.com/syohex/emacs-company-jedi")
    (synopsis "Provides Python completion in @code{company-mode}")
    (description
     "This package provides a Company backend for Python.")
    (license license:gpl3+)))

(define-public emacs-puppet-mode
  (let ((commit "b3ed5057166a4f49dfa9be638523a348b55a2fd2")
        (revision "1"))
    (package
      (name "emacs-puppet-mode")
      ;; The last release, 0.3 was several years ago, and there have been many
      ;; commits since
      (version (git-version "0.3" revision commit))
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://raw.githubusercontent.com/voxpupuli/puppet-mode/"
               commit "/puppet-mode.el"))
         (sha256
          (base32
           "1indycxawsl0p2aqqg754f6735q3cmah9vd886rpn0ncc3ipi1xm"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/voxpupuli/puppet-mode")
      (synopsis "Emacs major mode for the Puppet configuration language")
      (description
       "This package provides support for the Puppet configuration language,
including syntax highlighting, indentation of expressions and statements,
linting of manifests and integration with Puppet Debugger.")
      ;; Also incorporates work covered by the Apache License, Version 2.0
      (license license:gpl3+))))

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
         (file-name (git-file-name name version))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/paradoxxxzero/jinja2-mode.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l26wcy496k6xk7q5sf905xir0p73ziy6c44is77854lv3y0z381"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/paradoxxxzero/jinja2-mode")
    (synopsis "Major mode for jinja2")
    (description
     "Emacs major mode for jinja2 with: syntax highlighting,
sgml/html integration, and indentation (working with sgml).")
    (license license:gpl3+)))

(define-public emacs-company-cabal
  ;; The latest version is 0.3.0, but no release has been provided after 0.2.1.
  (let ((commit "62112a7259e24bd6c08885629a185afe512b7d3d")
        (revision "1"))
    (package
      (name "emacs-company-cabal")
      (version (git-version "0.3.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/iquiw/company-cabal/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1gf45xwjzdm8i4q6c6khk4dbg1mmp2r0awz2sjr4dcr2dbd1n7mg"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-company" ,emacs-company)))
      (home-page "https://github.com/iquiw/company-cabal/")
      (synopsis "Company completion for Haskell Cabal files")
      (description
       "This package allows for completion of field names, section names,
field values, and more within @code{haskell-cabal-mode}.")
      (license license:gpl3+))))

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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sabof/org-bullets.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10nr4sjffnqbllv6gmak6pviyynrb7pi5nvrq331h5alm3xcpq0w"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lolownia/org-pomodoro.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r5shgikm34d66i2hblyknbblpg92lb2zc9x4bcb28xkh7m9d0xv"))))
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
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/org-trello/org-trello.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m5hyhb6211hdmyp1bq6f3fklfgw3957knd96bfdafj727vdnlzm"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-deferred" ,emacs-deferred)
       ("emacs-f" ,emacs-f)
       ("emacs-helm" ,emacs-helm)
       ("emacs-request" ,emacs-request)
       ("emacs-s" ,emacs-s)))
    (home-page "https://org-trello.github.io")
    (synopsis "Emacs minor mode for interacting with Trello")
    (description "This package provides an Emacs minor mode to extend
@code{org-mode} with Trello abilities.  Trello is an online project
organizer.")
    (license license:gpl3+)))

(define-public emacs-company-flow
  (let ((commit "76ef585c70d2a3206c2eadf24ba61e59124c3a16")
        (revision "1"))
    (package
      (name "emacs-company-flow")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/aaronjensen/company-flow/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0zs9cblnbkxa0dxw4lyllmybqizxcdx96gv8jlhx20nrjpi78piw"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-company" ,emacs-company)
         ("emacs-dash" ,emacs-dash)))
      (home-page "https://github.com/aaronjensen/company-flow/")
      (synopsis "Flow backend for @code{company-mode}")
      (description
       "This package provides completion for JavaScript files utilizing Flow.")
      (license license:gpl3+))))

(define-public emacs-atom-one-dark-theme
  (let ((commit "1f1185bf667a38d3d0d180ce85fd4c131818aae2")
        (revision "0"))
    (package
     (name "emacs-atom-one-dark-theme")
     (version (git-version "0.4.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jonathanchu/atom-one-dark-theme.git")
                    (commit commit)))
              (sha256
               (base32
                "1alma16hg3mfjly8a9s3mrswkjjx4lrpdnf43869hn2ibkn7zx9z"))
              (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (home-page "https://github.com/jonathanchu/atom-one-dark-theme")
     (synopsis "Atom One Dark color theme for Emacs")
     (description "An Emacs port of the Atom One Dark theme from Atom.io.")
     (license license:gpl3+))))

(define-public emacs-zenburn-theme
  (package
    (name "emacs-zenburn-theme")
    (version "2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bbatsov/zenburn-emacs.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n87r5hs7h5r8dgfid66skpzcypl9hssr9m3npp916g6jfsi782f"))))
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

(define-public emacs-prescient
  (package
    (name "emacs-prescient")
    (version "2.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/raxod502/prescient.el/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ncplx5p3cffyzg9ygzqqxj0vpvwrz9rp2n4z6c375a78fyydrk0"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-company" ,emacs-company)
       ("emacs-ivy" ,emacs-ivy)))
    (home-page "https://github.com/raxod502/prescient.el/")
    (synopsis "Library that sorts and filters lists of candidates")
    (description
     "This package provides a library for sorting and filtering, as well as
extensions for @code{ivy-mode} and @code{company-mode} that make use of the
library.")
    (license license:gpl3+)))

(define-public emacs-smartparens
  (package
    (name "emacs-smartparens")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Fuco1/smartparens.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zij2f2rjjym98w68jkp10n1ckpfprlkk217c3fg16hz5nq4vnm6"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-markdown-mode" ,emacs-markdown-mode)))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nschum/highlight-symbol.el.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09z13kv2g21kjjkkm3iyaz93sdjmdy2d563r8n7r7ng94acrn7f6"))))
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
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://raw.githubusercontent.com/tarsius/hl-todo/"
                    version "/hl-todo.el"))
              (file-name (string-append "hl-todo-" version ".el"))
              (sha256
               (base32
                "0728givzh7xv5i88ac9if8byj1p8bilrj1fnizca10s0rv100hdr"))))
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
    (version "1.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/realgud-"
                           version ".tar"))
       (sha256
        (base32
         "108wgxg7fb4byaiasgvbxv2hq7b00biq9f0mh9hy6vw4160y5w24"))
       (patches
        ;; Patch awaiting inclusion upstream (see:
        ;; https://github.com/realgud/realgud/pull/226).
        (search-patches "emacs-realgud-fix-configure-ac.patch"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-emacs-load-path 'fix-autogen-script
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
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wyxqbb35yqf6ci47531lk32d6fppamx9d8826kdz983vm87him7"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-deferred" ,emacs-deferred)))
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
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/abo-abo/hydra")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ln4z2796ycy33g5jcxkqvm7638qxy4sipsab7d2864hh700cikg"))))
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
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abo-abo/swiper.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "009n8zjycs62cv4i1k9adbb284wz2w3r13xki2740sj34k683v13"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (info (string-append out "/share/info")))
               (with-directory-excursion "doc"
                 (invoke "makeinfo" "ivy.texi")
                 (install-file "ivy.info" info)
                 #t)))))))
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

(define-public emacs-ivy-yasnippet
  (let ((commit "32580b4fd23ebf9ca7dde96704f7d53df6e253cd")
        (revision "2"))
    (package
      (name "emacs-ivy-yasnippet")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mkcms/ivy-yasnippet.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1wfg6mmd5gl1qgvayyzpxlkh9s7jgn20y8l1vh1zbj1czvv51xp8"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-ivy" ,emacs-ivy)
         ("emacs-yasnippet" ,emacs-yasnippet)
         ("emacs-dash" ,emacs-dash)))
      (home-page "https://github.com/mkcms/ivy-yasnippet")
      (synopsis "Preview @code{yasnippets} with @code{ivy}")
      (description "This package allows you to select @code{yasnippet}
snippets using @code{ivy} completion.  When current selection changes in the
minibuffer, the snippet contents are temporarily expanded in the buffer.  To
use it, call @code{M-x ivy-yasnippet} (but make sure you have enabled
@code{yas-minor-mode} first).")
      (license license:gpl3+))))

(define-public emacs-ivy-rich
  (package
    (name "emacs-ivy-rich")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Yevgnen/ivy-rich.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ayf3dwfhafcbqnckm65zy8nc1rv9ji939qfn53wbhxkrgqdicgz"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-ivy" ,emacs-ivy)))
    (home-page "https://github.com/Yevgnen/ivy-rich")
    (synopsis "More friendly interface for @code{ivy}")
    (description
     "This package extends @code{ivy} by showing more information in the
minibuffer for each candidate.  It adds columns showing buffer modes, file
sizes, docstrings, etc.  If @code{emacs-all-the-icons} is installed, it can
show icons as well.")
    (license license:gpl3+)))

(define-public emacs-avy
  (package
    (name "emacs-avy")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abo-abo/avy.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rq9ab264565z83cly743nbhrd9m967apmnlhqr1gy8dm4hcy7nm"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abo-abo/ace-window.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07mcdzjmgrqdvjs94f2n5bkrf5vrq2fwzz256wbm3wzqxqkfy1q6"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/victorhge/iedit.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pwkrm98vlpzsy5iwwfksdaz3zzyi7bvdf5fglhsn4ssf47p787g"))))
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

(define-public emacs-zoutline
  (let ((commit "b3ee0f0e0b916838c2d2c249beba74ffdb8d5699")
        (revision "0"))
    (package
      (name "emacs-zoutline")
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/abo-abo/zoutline")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (sha256
                 (base32
                  "0sd0017piw0dis6dhpq5dkqd3acisxqgipl7dj8gmc1vnswhdwr8"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (synopsis "Simple outline library")
      (description
       "This library provides helpers for outlines.  Outlines allow users to
navigate code in a tree-like fashion.")
      (license license:gpl3+))))

(define-public emacs-lispy
  ;; Release 0.26.0 was almost 3 years ago, and there have been ~772 commits
  ;; since.
  (let ((commit "f94cfc6b8f9c3afe7d028c366928049c011023de")
        (revision "1"))
    (package
      (name "emacs-lispy")
      (version (git-version "0.26.0" revision commit))
      (home-page "https://github.com/abo-abo/lispy")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (sha256
                 (base32
                  "1bm2cpwizg1qfpm377gpx1af1hm5maw69if1csnk5vwaphmv8c4g"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-ace-window" ,emacs-ace-window)
         ("emacs-iedit" ,emacs-iedit)
         ("emacs-ivy" ,emacs-ivy)
         ("emacs-hydra" ,emacs-hydra)
         ("emacs-zoutline" ,emacs-zoutline)))
      (synopsis "Modal S-expression editing")
      (description
       "Due to the structure of Lisp syntax it's very rare for the programmer
to want to insert characters right before \"(\" or right after \")\".  Thus
unprefixed printable characters can be used to call commands when the point is
at one of these special locations.  Lispy provides unprefixed keybindings for
S-expression editing when point is at the beginning or end of an
S-expression.")
      (license license:gpl3+))))

(define-public emacs-lispyville
  (let ((commit "d28b937f0cabd8ce61e2020fe9a733ca80d82c74")
        (revision "1"))
    (package
      (name "emacs-lispyville")
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/noctuid/lispyville")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (sha256
                 (base32
                  "0f6srwj1qqkfkbmp5n5pjvi6gm7b7xav05p5hrs2i83rjrakzzqx"))
                (file-name (git-file-name name version))))
      (propagated-inputs
       `(("emacs-evil" ,emacs-evil)
         ("emacs-lispy" ,emacs-lispy)))
      (build-system emacs-build-system)
      (synopsis "Minor mode for integrating Evil with lispy")
      (description
       "LispyVille's main purpose is to provide a Lisp editing environment
suited towards Evil users.  It can serve as a minimal layer on top of lispy
for better integration with Evil, but it does not require the use of lispy’s
keybinding style.  The provided commands allow for editing Lisp in normal
state and will work even without lispy being enabled.")
      (license license:gpl3+))))

(define-public emacs-clojure-mode
  (package
    (name "emacs-clojure-mode")
    (version "5.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure-emacs/clojure-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qadymqzs5fn1sb347xzfw8lqq6s85vvkh4kzm35m61g5i2lm86y"))))
    (build-system emacs-build-system)
    (native-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-s" ,emacs-s)
       ("ert-runner" ,emacs-ert-runner)))
    (arguments
     `(#:tests? #t
       #:test-command '("ert-runner")))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cask/epl.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sjxd5y5hxhrbgfkpwx6m724r3841b53hgc61a0g5zwispw5pmrr"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/cask/epl")
    (synopsis "Emacs Package Library")
    (description
     "A package management library for Emacs, based on @code{package.el}.

The purpose of this library is to wrap all the quirks and hassle of
@code{package.el} into a sane API.")
    (license license:gpl3+)))

(define-public emacs-counsel-projectile
  (package
    (name "emacs-counsel-projectile")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ericdanan/counsel-projectile")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1inc4ndl0ysfwvxk4avbgpj4qi9rc93da6476a5c81xmwpsv8wmq"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-ivy" ,emacs-ivy)
       ("emacs-projectile" ,emacs-projectile)))
    (home-page "https://github.com/ericdanan/counsel-projectile")
    (synopsis "Enhance Projectile with Ivy")
    (description
     "This package uses Ivy to provide additional actions for Projectile
commands and replacements for existing functions.")
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lunaryorn/pkg-info.el.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nk12dcppdyhav6m6yf7abpywyd7amxd4237zsfd32w4zxsx39k1"))))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ljos/sparql-mode.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ywhqk6n5k0l85zjwbnrivnvxjzqipqrggv06lify6yv18qmyl6s"))))
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

(define-public emacs-undohist-el
  (let ((commit "d2239a5f736724ceb9e3b6bcaa86f4064805cda0")
        (revision "1"))
    (package
      (name "emacs-undohist-el")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/m2ym/undohist-el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1c0daw246ky7b1x5b8h55x79pl1pjqk1k348l487bdd8zdj4w9wx"))
         (patches
          (search-patches "emacs-undohist-ignored.patch"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/m2ym/undohist-el")
      (synopsis "Save undo history between sessions")
      (description "This package allows persistent use of undo history for
individual file buffers.")
      (license license:gpl3+))))

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
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1is4dcv6blslpzbjcg8l2jpxi8xj96q4cm0nxjxsyswpm8bw8ki0"))))
      (build-system emacs-build-system)
      (arguments
       `(#:tests? #t
         #:test-command '("emacs" "--batch"
                          "-l" "julia-mode-tests.el"
                          "-f" "ert-run-tests-batch-and-exit")))
      (home-page "https://github.com/JuliaEditorSupport/julia-emacs")
      (synopsis "Major mode for Julia")
      (description "This Emacs package provides a mode for the Julia
programming language.")
      (license license:expat))))

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
    (version "20190219")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mooz/js2-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0766bbr4piia9vfr4ivd2gwi8dxah654adv6h28ylz4q8xmfzm1b"))))
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

(define-public emacs-nodejs-repl
  (package
    (name "emacs-nodejs-repl")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/abicky/nodejs-repl.el"
                                  "/archive/" version ".tar.gz"))
              (sha256
               (base32
                "0hq2cqdq2668yf48g7qnkci90nhih1gnhacsgz355jnib56lhmkz"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/abicky/nodejs-repl.el")
    (synopsis "Node.js REPL inside Emacs")
    (description
     "This program is derived from comint-mode and provides the following
features:

@itemize
@item TAB completion same as Node.js REPL
@item file name completion in string
@item incremental history search
@end itemize")
    (license license:gpl3+)))

(define-public emacs-typescript-mode
  (package
    (name "emacs-typescript-mode")
    (version "0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ananthakumaran/typescript.el"
                    "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1gqjirm8scf0wysm7x97zdfbs4qa5nqdl64jfbkd18iskv5mg3rj"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ananthakumaran/typescript.el")
    (synopsis "Emacs major mode for editing Typescript code")
    (description
     "This is based on Karl Landstrom's barebones @code{typescript-mode}.
This is much more robust and works with @code{cc-mode}'s comment
filling (mostly).  The modifications to the original @code{javascript.el} mode
mainly consisted in replacing \"javascript\" with \"typescript\"

The main features of this Typescript mode are syntactic highlighting (enabled
with @code{font-lock-mode} or @code{global-font-lock-mode}), automatic
indentation and filling of comments and C preprocessor fontification.")
    (license license:gpl3+)))

(define-public emacs-tide
  (package
    (name "emacs-tide")
    (version "3.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ananthakumaran/tide"
                                  "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1c600myr2yqbkmy9lify38lz0zzjdqk1733db5n7vsay16vn6fzi"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-s" ,emacs-s)
       ("emacs-flycheck" ,emacs-flycheck)
       ("emacs-typescript-mode" ,emacs-typescript-mode)))
    (home-page "https://github.com/ananthakumaran/tide")
    (synopsis "Typescript IDE for Emacs")
    (description
     "Tide is an Interactive Development Environment (IDE) for Emacs which
provides the following features:

@itemize
@item ElDoc
@item Auto complete
@item Flycheck
@item Jump to definition, Jump to type definition
@item Find occurrences
@item Rename symbol
@item Imenu
@item Compile On Save
@item Highlight Identifiers
@item Code Fixes
@item Code Refactor
@item Organize Imports
@end itemize")
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
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Fanael/edit-indirect")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0by1x53pji39fjrj5bd446kz831nv0vdgw2jqasbym4pc1p2947r"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/edit-indirect")
    (synopsis "Edit regions in separate buffers")
    (description "This package allows you to edit regions in separate buffers,
like @code{org-edit-src-code} but for arbitrary regions.")
    (license license:gpl3+)))

(define-public emacs-projectile
  (package
    (name "emacs-projectile")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com/bbatsov"
                                  "/projectile/v" version "/projectile.el"))
              (file-name (string-append "projectile-" version ".el"))
              (sha256
               (base32
                "149hablms6n9b4wp78dz4pjz1rzyylfw9ayghq5p9d7k661mvyby"))))
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
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skeeto/elfeed.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01x4ww63lvn04c7f3ab5vx2s20xqisvv8213qwswz7vr9nxja5yi"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:test-command '("make" "test")))
    (home-page "https://github.com/skeeto/elfeed")
    (synopsis "Atom/RSS feed reader for Emacs")
    (description
     "Elfeed is an extensible web feed reader for Emacs, supporting both Atom
and RSS, with a user interface inspired by notmuch.")
    (license license:gpl3+)))

(define-public emacs-el-x
  (package
    (name "emacs-el-x")
    (version "0.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sigma/el-x.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i6j44ssxm1xdg0mf91nh1lnprwsaxsx8vsrf720nan7mfr283h5"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Move the source files to the top level, which is included in
         ;; the EMACSLOADPATH.
         (add-after 'unpack 'move-source-files
           (lambda _
             (let ((el-files (find-files "./lisp" ".*\\.el$")))
               (for-each (lambda (f)
                           (rename-file f (basename f)))
                         el-files))
             #t)))))
    (home-page "https://github.com/sigma/el-x")
    (synopsis "Emacs Lisp extensions")
    (description "command@{emacs-el-x} defines the @code{dflet} macro to
provide the historic behavior of @code{flet}, as well as
@code{declare-function} stub for older Emacs.")
    (license license:gpl2+)))

(define-public emacs-mocker
  (package
    (name "emacs-mocker")
    (version "0.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sigma/mocker.el.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lav7am41v63xgavq8pr88y828jmd1cxd4prjq7jlbxm6nvrwxh2"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:test-command '("ert-runner")))
    (native-inputs
     `(("ert-runner" ,emacs-ert-runner)))
    (propagated-inputs
     `(("emacs-el-x" ,emacs-el-x)))
    (home-page "https://github.com/sigma/mocker.el")
    (synopsis "Mocking framework for Emacs Lisp")
    (description "Mocker.el is a framework for writing tests in Emacs Lisp.
It uses regular Lisp rather than a domain specific language (DSL), which
maximizes flexibility (at the expense of conciseness).")
    (license license:gpl2+)))

(define-public emacs-find-file-in-project
  (package
    (name "emacs-find-file-in-project")
    (version "5.4.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/technomancy/find-file-in-project.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sdnyqv69mipbgs9yax88m9b6crsa59rjhwrih197pifl4089awr"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-shell
           ;; Otherwise Emacs shell-file-name is set to "/bin/sh", which doesn't
           ;; work.
           (lambda _
             (setenv "SHELL" (which "sh"))
             #t)))
       #:tests? #t
       #:test-command '("./tests/test.sh")))
    (home-page "https://github.com/technomancy/find-file-in-project")
    (synopsis "File/directory finder for Emacs")
    (description "@code{find-file-in-project} allows to find files or
directories quickly in the current project.  The project root is detected
automatically when Git, Subversion or Mercurial are used.  It also provides
functions to assist in reviewing changes on files.")
    (license license:gpl3+)))

(define-public emacs-pyvenv
  (package
    (name "emacs-pyvenv")
    (version "1.20")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jorgenschaefer/pyvenv.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x052fsavb94x3scpqd6n9spqgzaahzbdxhg4qa5sy6hqsabn6zh"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This phase incorrectly attempts to substitute "activate" and fails
         ;; doing so.
         (delete 'patch-el-files))
       #:tests? #t
       #:test-command '("ert-runner")))
    (native-inputs
     `(("ert-runner" ,emacs-ert-runner)
       ("emacs-mocker" ,emacs-mocker)))
    (home-page "https://github.com/jorgenschaefer/pyvenv")
    (synopsis "Python virtual environment interface for Emacs")
    (description "pyvenv.el is a minor mode to support using Python virtual
environments (virtualenv) inside Emacs.")
    (license license:gpl3+)))

(define-public emacs-highlight-indentation
  (package
    (name "emacs-highlight-indentation")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/antonj/Highlight-Indentation-for-Emacs.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00l54k75qk24a0znzl4ij3s3nrnr2wy9ha3za8apphzlm98m907k"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/antonj/Highlight-Indentation-for-Emacs/")
    (synopsis "Highlighting indentation for Emacs")
    (description "Provides two minor modes to highlight indentation guides in Emacs:
@enumerate
@item @code{highlight-indentation-mode}, which displays guidelines
indentation (space indentation only).
@item @code{highlight-indentation-current-column-mode}, which displays guidelines for the current-point indentation (space indentation only).
@end enumerate")
    (license license:gpl2+)))

(define-public emacs-elpy
  (package
    (name "emacs-elpy")
    (version "1.28.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jorgenschaefer/elpy.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "073bwxwjzcbmvpcz9q2xjwzx9x7hkvjni6fwvikh6yawzjp56jis"))))
    (build-system emacs-build-system)
    (arguments
     `(#:include (cons* "^elpy/[^/]+\\.py$" "^snippets\\/" %default-include)
       #:phases
       ;; TODO: Make `elpy-config' display Guix commands :)
       (modify-phases %standard-phases
         ;; One elpy test depends on being run inside a Python virtual
         ;; environment to pass. We have nothing to gain from doing so here,
         ;; so we just trick Elpy into thinking we are (see:
         ;; https://github.com/jorgenschaefer/elpy/pull/1293).
         (add-before 'check 'fake-virtualenv
           (lambda _
             (setenv "VIRTUAL_ENV" "/tmp")
             #t))
         (add-before 'check 'build-doc
           (lambda _
             (with-directory-excursion "docs"
               (invoke "make" "info" "man"))
             ;; Move .info file at the root so that it can installed by the
             ;; 'move-doc phase.
             (rename-file "docs/_build/texinfo/Elpy.info" "Elpy.info")
              #t))
         (add-after 'build-doc 'install-manpage
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (man1 (string-append out "/share/man/man1")))
               (mkdir-p man1)
               (copy-file "docs/_build/man/elpy.1"
                          (string-append man1 "/elpy.1")))
             #t)))
       #:tests? #t
       #:test-command '("ert-runner")))
    (propagated-inputs
     `(("emacs-company" ,emacs-company)
       ("emacs-find-file-in-project" ,emacs-find-file-in-project)
       ("emacs-highlight-indentation" ,emacs-highlight-indentation)
       ("emacs-yasnippet" ,emacs-yasnippet)
       ("pyvenv" ,emacs-pyvenv)
       ("s" ,emacs-s)))
    (native-inputs
     `(("ert-runner" ,emacs-ert-runner)
       ("emacs-f" ,emacs-f)
       ("python" ,python-wrapper)
       ("python-autopep8" ,python-autopep8)
       ("python-black" ,python-black)
       ("python-flake8" ,python-flake8)
       ("python-jedi" ,python-jedi)
       ("python-yapf" ,python-yapf)
       ;; For documentation.
       ("python-sphinx" ,python-sphinx)
       ("texinfo" ,texinfo)))
    (home-page "https://github.com/jorgenschaefer/elpy")
    (synopsis "Python development environment for Emacs")
    (description "Elpy brings powerful Python editing to Emacs.  It combines
and configures a number of other packages written in Emacs Lisp as well as
Python, together offering features such as navigation, documentation,
completion, interactive development and more.")
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
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://elpa.gnu.org/packages/rainbow-mode-" version ".el"))
              (sha256
               (base32
                "0cpga4ax635rfpj7y2vmh7ank0yw00dcy20gjg1mj74r97by8csf"))))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joostkremers/writeroom-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13nbls5qxz5z8firjxaip8m9vzfbbpxmwrmr01njbk4axpwrpj0z"))))
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
    (version "16")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://raw.githubusercontent.com/fxbois"
                                  "/web-mode/v" version "/web-mode.el"))
              (file-name (string-append "web-mode-" version ".el"))
              (sha256
               (base32
                "1hs5w7kdvcyn4ihyw1kfjg48djn5p7lz4rlbhzzdqv1g56xqx3gw"))))
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
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/mhayashi1120/Emacs-wgrep/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cssqbg03fjb6xwf7idv1l3jjsh9r5r232ryi11czqlxfiv658bj"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/mhayashi1120/Emacs-wgrep")
    (synopsis "Edit a grep buffer and apply those changes to the files")
    (description
     "Emacs wgrep allows you to edit a grep buffer and apply those changes
to the file buffer.  Several backends are supported beside the classic grep:
ack, ag, helm and pt.")
    (license license:gpl3+)))

(define-public emacs-helm
  (package
    (name "emacs-helm")
    (version "3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-helm/helm.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x3nv8zvp8vvl30bm2d83hd7zxb0ca64pc8kwb81ml9al6r3mm01"))))
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
    (version "1.7.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ShingoFukuyama/helm-swoop.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b23j1bkpg4pm310hqdhgnl4mxsj05gpl08b6kb2ja4fzrg6adsk"))))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bbatsov/helm-projectile.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lph38p112fridighqcizpsyzjbv7qr3d8prbfj6w6q6gfl6cna4"))))
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
         (file-name (git-file-name name version))
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
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure-emacs/cider.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lbrwj67fnvynkdkzvnzp3p8vqlz0ldrcs317vg60bqfhx7hvqkj"))))
    (build-system emacs-build-system)
    (arguments
     '(#:exclude                        ; Don't exclude 'cider-test.el'.
       '("^\\.dir-locals\\.el$" "^test/")))
    (propagated-inputs
     `(("emacs-clojure-mode" ,emacs-clojure-mode)
       ("emacs-sesman" ,emacs-sesman)
       ("emacs-spinner" ,emacs-spinner)
       ("emacs-pkg-info" ,emacs-pkg-info)
       ("emacs-queue" ,emacs-queue)))
    (home-page "https://cider.readthedocs.io/")
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
    (version "1.2.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-evil/evil")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1833w397xhac5g3pp25szr2gyvclxy91aw27azvbmsx94pyk2a3q"))))
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

(define-public emacs-evil-collection
  (let ((commit "baa9c6880779d416a91bf5bde3589673a7403612")
        (revision "8"))
    (package
      (name "emacs-evil-collection")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/emacs-evil/evil-collection")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09v97nk7qawphfijzqxzi2y63ydmfq7hsgpljjxwkykrc5h0kdlj"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-evil" ,emacs-evil)))
      (home-page "https://github.com/emacs-evil/evil-collection")
      (synopsis "Collection of Evil bindings for many major and minor modes")
      (description "This is a collection of Evil bindings for the parts of
Emacs that Evil does not cover properly by default, such as @code{help-mode},
@code{M-x calendar}, Eshell and more.")
      (license license:gpl3+))))

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

(define-public emacs-janpath-evil-numbers
  (let ((commit "d988041c1fe6e941dc8d591390750b237f71f524")
        (version "0.5")
        (revision "1"))
    (package
      (name "emacs-janpath-evil-numbers")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/janpath/evil-numbers")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "16zixhxxcb83m82f2cfiardfn99j3w41v6vh1qk6j9pplrlfw70v"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-evil" ,emacs-evil)))
      (home-page "https://github.com/janpath/evil-numbers")
      (synopsis "Increment and decrement numeric literals")
      (description
       "This package provides functionality to search for a number up to the
end of a line and increment or decrement it.")
      (license license:gpl3+))))

(define-public emacs-evil-exchange
  (let ((commit "47691537815150715e64e6f6ec79be7746c96120")
        (version "0.41")
        (revision "1"))
    (package
      (name "emacs-evil-exchange")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Dewdrops/evil-exchange")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0bjpn4yqig17ddym6wqq5fm1b294q74hzcbj9a6gs97fqiwf88xa"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-evil" ,emacs-evil)))
      (home-page "https://github.com/Dewdrops/evil-exchange")
      (synopsis "Exchange text easily within Evil")
      (description
       "This package, a port of @code{vim-exchange}, provides an Evil operator
for exchanging text.")
      (license license:gpl3+))))

(define-public emacs-evil-replace-with-register
  (let ((commit "91cc7bf21a94703c441cc9212214075b226b7f67")
        (version "0.1")
        (revision "1"))
    (package
      (name "emacs-evil-replace-with-register")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Dewdrops/evil-ReplaceWithRegister")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "14rpn76qrf287s3y2agmddcxi27r226i53ixjvd694ss039g0r11"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-evil" ,emacs-evil)))
      (home-page "https://github.com/Dewdrops/evil-ReplaceWithRegister")
      (synopsis "Quickly replace text with the contents of a register")
      (description
       "This package provides an Evil operator for replacing text with the
contents of a register.")
      (license license:gpl3+))))

(define-public emacs-evil-indent-plus
  (let ((commit "0c7501e6efed661242c3a20e0a6c79a6455c2c40")
        (version "1.0.0")
        (revision "1"))
    (package
      (name "emacs-evil-indent-plus")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/TheBB/evil-indent-plus")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1g6r1ydscwjvmhh1zg4q3nap4avk8lb9msdqrh7dff6pla0r2qs6"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-evil" ,emacs-evil)))
      (home-page "https://github.com/TheBB/evil-indent-plus")
      (synopsis "Text objects based on indentation")
      (description
       "This package adds new text objects for @code{evil-mode} based on
blocks having the same or higher indentation, including or excluding
surrounding lines.")
      (license license:gpl2+))))

(define-public emacs-evil-text-object-python
  (let ((commit "9a064fe6475429145cbcc3b270fcc963b67adb15")
        (version "1.0.1")
        (revision "1"))
    (package
      (name "emacs-evil-text-object-python")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/wbolster/evil-text-object-python")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "074zpm6mmr1wfl6d5xdf8jk1fs4ccpbzf4ahhkwga9g71xiplszv"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-evil" ,emacs-evil)))
      (home-page "https://github.com/wbolster/evil-text-object-python")
      (synopsis "Text objects for Python")
      (description
       "This package provides @code{evil-mode} text objects for Python.")
      (license license:gpl3+))))

(define-public emacs-evil-lion
  (let ((commit "6b03593f5dd6e7c9ca02207f9a73615cf94c93ab")
        (version "0.0.2")
        (revision "1"))
    (package
      (name "emacs-evil-lion")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/edkolev/evil-lion")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1a162hynp0jcsn50c1w5a02mrw9w3q05c7lkqzqd25px3d0p772q"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-evil" ,emacs-evil)))
      (home-page "https://github.com/edkolev/evil-lion")
      (synopsis "Align operator for @code{evil-mode}")
      (description
       "This package, a port of @code{vim-lion}, provides an operator for
aligning text objects based on separators.")
      (license license:gpl3+))))

(define-public emacs-evil-expat
  (let ((commit "4c344ea19b789002d759a202ffbf594730d2c59a")
        (version "0.0.1")
        (revision "1"))
    (package
      (name "emacs-evil-expat")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/edkolev/evil-expat")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "16v7fnldxag6l1lsnrnhdjkga9qi78lbdfbb82k6pmv04991mbkr"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-evil" ,emacs-evil)))
      (home-page "https://github.com/edkolev/evil-expat")
      (synopsis "Extra @code{ex} commands for @code{evil-mode}")
      (description
       "This package provides additional commands for the @code{ex} command
line, including functions for version control with Git and for copying and
pasting into and from @code{tmux} paste buffers.")
      (license license:gpl3+))))

(define-public emacs-evil-nerd-commenter
  (package
    (name "emacs-evil-nerd-commenter")
    (version "3.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/redguardtoo/evil-nerd-commenter")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0074i9lvfs5hkbf63g1xh3n7dbmkax30bppkx2cd394c7zlsvzzk"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-evil" ,emacs-evil)))
    (home-page "https://github.com/redguardtoo/evil-nerd-commenter")
    (synopsis "Comment and uncomment lines efficiently")
    (description
     "This package provides text objects and operators for comments within
@code{evil-mode}.")
    (license license:gpl3+)))

(define-public emacs-evil-visual-replace
  (package
    (name "emacs-evil-visual-replace")
    (version "0.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/troyp/evil-visual-replace")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1gfyrq7xfzmzh3x8k5f08n027dlbwi0pkkxf9c39fkxp4jngibsz"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-evil" ,emacs-evil)))
    (home-page "https://github.com/troyp/evil-visual-replace")
    (synopsis "Replacement commands for @code{evil-mode} visual blocks")
    (description
     "This package provides versions of @code{query-replace} and
@code{replace-regexp} that work for @code{evil-mode} visual blocks.")
    (license license:gpl2+)))

(define-public emacs-evil-visualstar
  (let ((commit "06c053d8f7381f91c53311b1234872ca96ced752")
        (version "0.0.2")
        (revision "1"))
    (package
      (name "emacs-evil-visualstar")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/bling/evil-visualstar")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0mkbzw12fav945icibc2293m5haxqr3hzkyli2cf4ssk6yvn0x4c"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-evil" ,emacs-evil)))
      (home-page "https://github.com/bling/evil-visualstar")
      (synopsis "Instantly search using the visual selection")
      (description
       "This package provides @code{evil-mode} functions for using selected text
to search.")
      (license license:gpl3+))))

(define-public emacs-monroe
  (package
    (name "emacs-monroe")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sanel/monroe.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "101lfrykdbv37spkbw7zihhx26bc1lhjyxbanrcp9880bxj04jiy"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/sanel/monroe")
    (synopsis "Clojure nREPL client for Emacs")
    (description
     "Monroe is a nREPL client for Emacs, focused on simplicity and easy
distribution, primarily targeting Clojure users")
    (license license:gpl3+)))

(define-public emacs-orgalist
  (package
    (name "emacs-orgalist")
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/"
                           "orgalist-" version ".el"))
       (sha256
        (base32
         "1rmmcyiiqkq54hn74nhzxzl4nvd902hv6gq341jwhrm7yiagffi6"))))
    (build-system emacs-build-system)
    (home-page "http://elpa.gnu.org/packages/orgalist.html")
    (synopsis "Manage Org-like lists in non-Org buffers")
    (description "Write Org mode's plain lists in non-Org buffers.  More
specifically, Orgalist supports the syntax of Org mode for numbered,
unnumbered, description items, checkboxes, and counter cookies.

The library also implements radio lists, i.e., lists written in Org
syntax later translated into the host format, e.g., LaTeX or HTML.")
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
    (version "9.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://elpa.gnu.org/packages/org-"
                                  version ".tar"))
              (sha256
               (base32
                "0hqy4lns9q5p0l1ylgmlckqprn9sbasszhznanmv0rsh0gzhsbyw"))))
    (build-system emacs-build-system)
    (home-page "https://orgmode.org/")
    (synopsis "Outline-based notes management and organizer")
    (description "Org is an Emacs mode for keeping notes, maintaining TODO
lists, and project planning with a fast and effective lightweight markup
language.  It also is an authoring system with unique support for literate
programming and reproducible research.")
    (license license:gpl3+)))

(define-public emacs-org-contrib
  (package
    (inherit emacs-org)
    (name "emacs-org-contrib")
    (version "20190402")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://orgmode.org/elpa/org-plus-contrib-"
                                  version ".tar"))
              (sha256
               (base32
                "0hbrhhq6sa0gz4ylzr54raxandqs4y953894b0hlr553b8sfl1wk"))))
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
                    (org (assoc-ref inputs "org"))
                    (contrib-files
                     (map basename (find-files out)))
                    (org+contrib-files
                     (map basename (find-files org)))
                    (duplicates (lset-intersection
                                 string=? contrib-files org+contrib-files)))
               (with-directory-excursion
                   (string-append
                    out "/share/emacs/site-lisp/guix.d/org-contrib-" ,version)
                 (for-each delete-file duplicates))
               #t))))))
    (propagated-inputs
     `(("arduino-mode" ,emacs-arduino-mode)
       ("cider" ,emacs-cider)
       ("org" ,emacs-org)
       ("scel" ,emacs-scel)))
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
         (file-name (git-file-name name version))
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
         (file-name (git-file-name name version))
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

(define-public emacs-nginx-mode
  (package
    (name "emacs-nginx-mode")
    (version "1.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ajc/nginx-mode.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17dh5pr3gh6adrbqx588gimxbb2fr7iv2qrxv6r48w2727l344xs"))))
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
    (version "2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Wilfred/ht.el.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p3qa7g0wa0wbviv2f8bda39cjys3naayk5xjm3nxxmqsyy8papx"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aki2o/log4e.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l28n7a0v2zkknc70i1wn6qb5i21dkhfizzk8wcj28v44cgzk022"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tekai/gntp.el.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nvyjjjydrimpxy4cpg90si7sr8lmldbhlcm2mx8npklp9pn5y3a"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jwiegley/alert.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vpc3q40m6dcrslki4bg725j4kv6c6xfxwjjl1ilg7la49fwwf26"))))
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

(define-public emacs-zones
  (let ((commit "353fc38a6544eb59887bee045e373406f1d038a5")
        (revision "1"))
    (package
      (name "emacs-zones")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacsmirror/zones.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0gwnw2giii2a14nlh62xp45f47cw6ikqphhzpmcw6c7mn9x5z2ar"))
         (patches
          (search-patches
           "emacs-zones-called-interactively.patch"))))
      (build-system emacs-build-system)
      (home-page "https://www.emacswiki.org/emacs/Zones")
      (synopsis "Define and act on multiple zones of buffer text")
      (description "Library @file{zones.el} lets you easily define and
subsequently act on multiple zones of buffer text.  You can think of this as
enlarging the notion of region.  In effect, it can remove the requirement of
target text being a contiguous sequence of characters.  A set of buffer zones
is, in effect, a (typically) noncontiguous set of text.")
      (license license:gpl3+))))

(define-public emacs-mu4e-alert
  (package
    (name "emacs-mu4e-alert")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/iqbalansari/mu4e-alert.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nvsfbfsma59ilf7c3vjngnmx3aapwvvvaafdy5szm5r6lkicqvg"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-alert" ,emacs-alert)
       ("emacs-s" ,emacs-s)
       ("emacs-ht" ,emacs-ht)
       ("mu" ,mu)))
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
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/joaotavora/yasnippet/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12ls2x17agzbrj1xynjbmfa11igqxia4hj4fv6fpr66yci2r1plc"))
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
                     "))\n"))
                  #t))))
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
         (file-name (git-file-name name version))
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
                                           (directory-exists? fname))))))
           #t)))
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
                (file-name (git-file-name name version))
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

(define-public emacs-helm-system-packages
  ;; There won't be a new release after 1.10.1 until
  ;; https://github.com/emacs-helm/helm-system-packages/issues/25 is fixed,
  ;; and latest commits fix import issues with Guix.
  (let ((commit "6572340f41611ef1991e9612d34d59130957ee4a"))
    (package
      (name "emacs-helm-system-packages")
      (version (git-version "1.10.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/emacs-helm/helm-system-packages")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0mcz6vkpk12vsyd37xv1rbg4v442sxc3lj8yxskqg294xbdaclz4"))))
      (build-system emacs-build-system)
      (inputs
       `(("recutils" ,recutils)))
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((recutils (assoc-ref inputs "recutils")))
                 ;; Specify the absolute file names of the various
                 ;; programs so that everything works out-of-the-box.
                 (substitute* "helm-system-packages-guix.el"
                   (("recsel") (string-append recutils "/bin/recsel")))))))))
      (home-page "https://github.com/emacs-helm/helm-system-packages")
      (synopsis "Helm System Packages is an interface to your package manager")
      (description "List all available packages in Helm (with installed
packages displayed in their own respective face).  Fuzzy-search, mark and
execute the desired action over any selections of packages: Install,
uninstall, display packages details (in Org Mode) or insert details at point,
find files owned by packages...  And much more, including performing all the
above over the network.")
      (license license:gpl3+))))

(define-public emacs-memoize
  (package
    (name "emacs-memoize")
    (version "1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skeeto/emacs-memoize.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04qgnlg4x6va7x364dhj1wbjmz8p5iq2vk36mn9198k2vxmijwzk"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:test-command '("emacs" "--batch"
                        "-l" "memoize-test.el"
                        "-f" "ert-run-tests-batch-and-exit")))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coldnew/linum-relative.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11bjnqqwvr9zrvz5dlm8a0yw4zg9ysh3jdiq5a6iw09d3f0h1v2s"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nonsequitur/idle-highlight-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x4w1ksrw7dicl84zpf4d4scg672dyan9g95jkn6zvri0lr8xciv"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/marsmining/ox-twbs.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kd45p8y7ykadmai4jn1x1pgpafyqggwb1ccbjzalxw4k9wmd45f"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/daimrod/highlight-sexp.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12cyk2q5g6p4ac2hykw1cag7dp1prjjnck5f7nalwwaklmy62y79"))))
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
         (file-name (git-file-name name version))
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
    (version "49.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/tvraman/emacspeak/releases/download/"
             version "/emacspeak-" version ".tar.bz2"))
       (sha256
        (base32
         "1smf26m7201z0bk49lzbw9zhbjfi6wylidfjixb8ylp6g0wnh8dx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list (string-append "prefix="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lisp (string-append out
                                         "/share/emacs/site-lisp/emacspeak")))
               (setenv "SHELL" (which "sh"))
               ;; Configure Emacspeak according to etc/install.org.
               (invoke "make" "config"))))
         (add-after 'build 'build-espeak
           (lambda _
             (invoke "make" "espeak")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lisp (string-append out "/share/emacs/site-lisp/emacspeak"))
                    (info (string-append out "/share/info"))
                    (emacs (string-append (assoc-ref inputs "emacs")
                                          "/bin/emacs")))
               ;; According to etc/install.org, the Emacspeak directory should
               ;; be copied to its installation destination.
               (for-each
                (lambda (file)
                  (copy-recursively file (string-append lisp "/" file)))
                '("etc" "info" "js" "lisp" "media" "scapes" "servers" "sounds"
                  "stumpwm" "xsl"))
               ;; Make sure emacspeak is loaded from the correct directory.
               (substitute* "etc/emacspeak.sh"
                 (("/lisp/emacspeak-setup.el")
                  (string-append lisp "/lisp/emacspeak-setup.el")))
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
     `(("emacs" ,emacs)
       ("espeak" ,espeak)
       ("perl" ,perl)
       ("tcl" ,tcl)
       ("tclx" ,tclx)))
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
  (version "1.8.5")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "https://elpa.gnu.org/packages/diff-hl-"
                          version ".tar"))
      (sha256
        (base32
          "1vxc7z7c2qs0mx7l5sa4sybi5qbzv0s79flj74p1ynw8dl3qxg3d"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/myrjola/diminish.el.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qpgfgp8hrzz4vdifxq8h25n0a0jlzgf7aa1fpy6r0080v5rqbb6"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/myrjola/diminish.el")
    (synopsis "Diminish minor modes with no modeline display")
    (description "@code{emacs-diminish} implements hiding or
abbreviation of the mode line displays (lighters) of minor modes.")
    (license license:gpl2+)))

(define-public emacs-use-package
  (let ((commit "da8c9e2840343906e732f9699e43d35a1f06481d")
        (revision "1"))
    (package
     (name "emacs-use-package")
     (version (git-version "2.3" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jwiegley/use-package")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jz38pbq1p9h85i6qcsh3sfzkd103y6mw3rg5zd14dxigp8ir3xz"))))
     (build-system emacs-build-system)
     (propagated-inputs
      `(("emacs-diminish" ,emacs-diminish)))
     (arguments
      `(#:tests? #t
        #:test-command '("emacs" "--batch"
                         "-l" "use-package-tests.el"
                         "-f" "ert-run-tests-batch-and-exit")))
     (home-page "https://github.com/jwiegley/use-package")
     (synopsis "Declaration for simplifying your .emacs")
     (description "The use-package macro allows you to isolate package
configuration in your @file{.emacs} file in a way that is both
performance-oriented and tidy.")
     (license license:gpl2+))))

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
                (file-name (git-file-name name version))
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
    (version "1.53")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hniksic/emacs-htmlize.git")
             (commit (string-append "release/" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dr235c0z8is3pi5xdgqyqljg6px0b2aya6qb79zkyi477bmz4ip"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/philjackson/xmlgen.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "096i29v0badx0a6339h9ckdz78zj59gbjdp7vj7vhkq9d830392s"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:test-command '("emacs" "--batch"
                        "-l" "xmlgen-test.el"
                        "-f" "ert-run-tests-batch-and-exit")))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cdominik/cdlatex.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jj9vmhc4s3ych08bjm1c2xwi81z1p20rj7bvxrgvb5aga2ghi9d"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-auctex" ,emacs-auctex)))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tumashu/cnfonts.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11d44lf0m0kbzq1mvyqkl4aprys0xqaarp08nij57xnynin1rynx"))))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pitkali/pos-tip.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w8bnspnk871qndp18hs0wk4x9x31xr9rwbvf5dc8mcbnj29ch33"))))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tumashu/pyim-basedict.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0576r8ap9gp91ycjf1d47pn13kxp0f9fysn09zlq44hr0s1y2y5d"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/tumashu/pyim-basedict")
    (synopsis "Input method dictionary of pyim")
    (description "Pyim-basedict is the default pinyin input method dictionary,
containing words from the rime project.")
    (license license:gpl2+)))

(define-public emacs-pyim
  (package
    (name "emacs-pyim")
    (version "1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tumashu/pyim")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "16rma4cv7xgky0g3x4an27v30jdi6i1sqw43cl99zhkqvp43l3f9"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-async" ,emacs-async)
       ("emacs-pyim-basedict" ,emacs-pyim-basedict)
       ("emacs-popup" ,emacs-popup)
       ("emacs-posframe" ,emacs-posframe)))
    (home-page "https://github.com/tumashu/pyim")
    (synopsis "Chinese input method")
    (description "Chinese input method which supports quanpin, shuangpin, wubi
and cangjie.")
    (license license:gpl2+)))

(define-public emacs-posframe
  (package
    (name "emacs-posframe")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://elpa.gnu.org/packages/posframe-" version ".el"))
       (sha256
        (base32
         "1h8vvxvsg41vc1nnglqjs2q0k1yzfsn72skga9s76qa3zxmx6kds"))))
    (build-system emacs-build-system)
    ;; emacs-minimal does not include the function font-info
    (arguments `(#:emacs ,emacs))
    (home-page "https://github.com/tumashu/posframe")
    (synopsis "Pop a posframe (a child frame) at point")
    (description "@code{emacs-posframe} can pop a posframe at point.  A
posframe is a child frame displayed within its root window's buffer.
@code{emacs-posframe} is fast and works well with CJK languages.")
    (license license:gpl3+)))

(define-public emacs-el2org
  (package
    (name "emacs-el2org")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tumashu/el2org.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mzddqny6wpg1fv99xrvlv7rxmaifvmy5bvj4in4pldhm4cx4q1b"))))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Wilfred/mustache.el.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n2ymd92qpvsby6ms0l3kjhdzzc47rri2aiscc6bs07hm4mjpr9q"))))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tumashu/org2web.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wsvfn409a2ivbich8b8zqza78sprirg4bl7igx536ydqclmi0n7"))))
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
    (version "0.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/xelb-"
                                  version ".tar"))
              (sha256
               (base32
                "0k98580vq253fjdgklgqlwl450saninfw39fbq8lv3xsnp3dcgml"))))
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
             (invoke "make"
                     (string-append "PROTO_PATH="
                                    (assoc-ref inputs "xcb-proto")
                                    "/share/xcb")
                     (string-append "EMACS_BIN="
                                    (assoc-ref inputs "emacs")
                                    "/bin/emacs -Q")))))))
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
    (version "0.22")
    (synopsis "Emacs X window manager")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/exwm-"
                                  version ".tar"))
              (sha256
               (base32
                "0lppm8ng37i5s4x7xdrxhjbdcnpl6pyvn4g7w52zbckjsn8qnqh0"))))
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
                     TryExec=~:*~a~@
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
    (version "1.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dimitri/switch-window")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0rci96asgamr6qp6nkyr5vwrnslswjxcjd96yccy4aivh0g66yfg"))))
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
    (version "1.9.0")
    (synopsis "Derivative window manager based on EXWM")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tumashu/exwm-x")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03l3dl7s1qys1kkh40rm1sfx7axy1b8sf5f6nyksj9ps6d30p5i4"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bruceravel/gnuplot-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bwri3cvm2vr27kyqkrddm28fs08axnd4nm9amfgp54xp20bn4yn"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/timcharper/evil-surround.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1smv7sqhm1l2bi9fmispnlmjssidblwkmiiycj1n3ag54q27z031"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/linktohack/evil-commentary.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zjs9zyqfygnpxapvf0ymmiid40i06cxbhjzd81zw33nafgkf6r4"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rejeep/ansi.el.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hbddxarr40ygvaw4pwaivq2l4f0brszw73w1r50lkjlggb7bl3g"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rejeep/commander.el.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j6hhyzww7wfwk6bllbb5mk4hw4qs8hsgfbfdifsam9c6i4spm45"))))
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
(define-public emacs-ert-runner
  (let ((version "0.7.0")
        (revision "1")
        (commit "90b8fdd5970ef76a4649be60003b37f82cdc1a65"))
    (package
      (name "emacs-ert-runner")
      (version (git-version "0.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rejeep/ert-runner.el.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "04nxmyzncacj2wmzd84vv9wkkr2dk9lcb10dvygqmg3p1gadnwzz"))))
      (build-system emacs-build-system)
      (inputs
       `(("emacs-ansi" ,emacs-ansi)
         ("emacs-commander" ,emacs-commander)
         ("emacs-dash" ,emacs-dash)
         ("emacs-f" ,emacs-f)
         ("emacs-s" ,emacs-s)
         ("emacs-shut-up" ,emacs-shut-up)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'install-executable
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (source-directory (string-append
                                  (getenv "TMPDIR") "/source")))
                 (substitute* "bin/ert-runner"
                   (("ERT_RUNNER=\"\\$\\(dirname \\$\\(dirname \\$0\\)\\)")
                    (string-append "ERT_RUNNER=\"" out
                                   "/share/emacs/site-lisp/guix.d/ert-runner-"
                                   ,version)))
                 (install-file "bin/ert-runner" (string-append out "/bin"))
                 (wrap-program (string-append out "/bin/ert-runner")
                   (list "EMACSLOADPATH" ":" 'prefix
                         ;; Do not capture the transient source directory in
                         ;; the wrapper.
                         (delete source-directory
                                 (string-split (getenv "EMACSLOADPATH") #\:))))
                 #t))))
         #:include (cons* "^reporters/.*\\.el$" %default-include)))
      (home-page "https://github.com/rejeep/ert-runner.el")
      (synopsis "Opinionated Ert testing workflow")
      (description "@code{ert-runner} is a tool for Emacs projects tested
using ERT.  It assumes a certain test structure setup and can therefore make
running tests easier.")
      (license license:gpl3+))))

(define-public ert-runner
  (deprecated-package "ert-runner" emacs-ert-runner))

(define-public emacs-disable-mouse
  (package
    (name "emacs-disable-mouse")
    (version "0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/purcell/disable-mouse.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v1y5hf6k6ng7xsvgb27nh740d14m6l4krr0paccda8zgm4mw357"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gongo/json-reformat.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qp4n2k6s69jj4gwwimkpadjv245y54wk3bxb1x96f034gkp81vs"))
       (patches (search-patches "emacs-json-reformat-fix-tests.patch"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-undercover" ,emacs-undercover)))
    (native-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-shut-up" ,emacs-shut-up)
       ("ert-runner" ,emacs-ert-runner)))
    (arguments
     `(#:tests? #t
       #:test-command '("ert-runner")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'make-tests-writable
           (lambda _
             (for-each make-file-writable (find-files "test"))
             #t))
         (add-before 'check 'delete-json-objects-order-test
           (lambda _
             (emacs-batch-edit-file "test/json-reformat-test.el"
               `(progn (progn (goto-char (point-min))
                              (re-search-forward
                               "ert-deftest json-reformat-test:json-reformat-region")
                              (beginning-of-line)
                              (kill-sexp))
                       (basic-save-buffer)))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Sterlingg/json-snatcher.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05zsgnk7grgw9jzwl80h5sxfpifxlr37b4mkbvx7mjq4z14xc2jw"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joshwnj/json-mode.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i79lqzdg59vkqwjd3q092xxn9vhxspb1vn4pkis0vfvn46g01jy"))))
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

(define-public emacs-eimp
  (let ((version "1.4.0")
        (commit "2e7536fe6d8f7faf1bad7a8ae37faba0162c3b4f")
        (revision "1"))
    (package
      (name "emacs-eimp")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nicferrier/eimp.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "154d57yafxbcf39r89n5j43c86rp2fki3lw3gwy7ww2g6qkclcra"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'configure
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((imagemagick (assoc-ref inputs "imagemagick")))
                 ;; eimp.el is read-only in git.
                 (chmod "eimp.el" #o644)
                 (emacs-substitute-variables "eimp.el"
                   ("eimp-mogrify-program"
                    (string-append imagemagick "/bin/mogrify"))))
               #t)))))
      (inputs
       `(("imagemagick" ,imagemagick)))
      (home-page "https://github.com/nicferrier/eimp")
      (synopsis "Interactive image manipulation utility for Emacs")
      (description "@code{emacs-eimp} allows interactive image manipulation
from within Emacs.  It uses the code@{mogrify} utility from ImageMagick to do
the actual transformations.")
      (license license:gpl2+))))

(define-public emacs-dired-hacks
  (let ((commit "2c1234592aee91dcd9401bcd67213e6a4a464fd9")
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
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1g7mky41cahpryzj6frdgzdymknpqq7pidzfjj9304887kijmhj3"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-eimp" ,emacs-eimp)
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

(define-public emacs-dired-sidebar
  (let ((commit "06bd0d40bab812c61a668129daf29ba359424454")
        (revision "0"))
    (package
      (name "emacs-dired-sidebar")
      (home-page "https://github.com/jojojames/dired-sidebar")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (sha256
                 (base32
                  "0lvwvq6sl80sha9fq5m4568sg534dhmifyjqw75bqddcbf3by84x"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dired-subtree" ,emacs-dired-hacks)))
      (synopsis "Sidebar for Emacs using Dired")
      (description
       "This package provides a sidebar for Emacs similar to @code{NeoTree}
or @code{treemacs}, but leveraging @code{Dired} to do the job of display.")
      (license license:gpl3+))))

(define-public emacs-which-key
  (package
    (name "emacs-which-key")
    (version "3.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/justbur/emacs-which-key.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dh6kr00wmql46whjkvnl953zngiv5j99ypvr1b3cb2174623afb"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:test-command '("emacs" "--batch"
                        "-l" "which-key-tests.el"
                        "-f" "ert-run-tests-batch-and-exit")))
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
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a4b0lsmwq84qfx51c5xy4fryhb1ysld4fhgw2vr37izf53379sb"))))
    (build-system emacs-build-system)
    (native-inputs
     `(("ert-runner" ,emacs-ert-runner)))
    (arguments
     `(#:tests? #t
       #:test-command '("ert-runner" "tests")))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/et2010/org-edit-latex.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zcllyhx9n9vcr5w87h0hfz25v52lvh5fi717cb7mf3jh89zh842"))))
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/syohex/emacs-emamux.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19y69qw79miim9cz5ji54gwspjkcp9g2c1xr5s7jj2fiabnxax6b"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/syohex/emacs-git-messenger.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04fnby2nblk8l70gv09asxkmnn53fh1pdfs77ix44npp99fyw8ix"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-popup" ,emacs-popup)))
    (arguments
     `(#:tests? #t
       #:test-command '("emacs" "--batch" "-l" "test/test.el"
                        "-f" "ert-run-tests-batch-and-exit")))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tumashu/gitpatch.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jj12pjwza6cq8a3kr8nqnmm3vxs0wam8h983irry4xr4ifywsn4"))))
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
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/leathekd/erc-hl-nicks"
                           "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1a1r2kc3688g8c2ybkpwh88kgmnqhg3h3032g2yn4zr9m0n3vpkr"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hrs/engine-mode.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02xas46nl28mascqsyr1zcd4hn15bh0fjv2xlxv1kmrj0pis94ml"))))
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
         "02r1qqsxi6qk7q4cj6a6pygbj856dcw9vcmhfh0ib92j41v77q6y"))))
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
  (package
    (name "emacs-browse-at-remote")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rmuslimov/browse-at-remote.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vhia7xmszcb3lxrb8wh93a3knjfzj48h8nhj4fh8zj1pjz6args"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-f" ,emacs-f)
       ("emacs-s" ,emacs-s)))
    (native-inputs
     `(("ert-runner" ,emacs-ert-runner)))
    (arguments
     `(#:tests? #t
       #:test-command '("ert-runner")))
    (home-page "https://github.com/rmuslimov/browse-at-remote")
    (synopsis "Open github/gitlab/bitbucket/stash page from Emacs")
    (description
     "This Emacs package allows you to open a target page on
github/gitlab (or bitbucket) by calling @code{browse-at-remote} command.
It supports dired buffers and opens them in tree mode at destination.")
    (license license:gpl3+)))

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
    (version "1.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ahyatt/emacs-websocket.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1dgrf7na6r6mmkknphzshlbd5fnzisg0qn0j7vfpa38wgsymaq52"))))
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
    (version "2.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jorgenschaefer/circe.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10gi14kwxd81blddpvqh95lgmpbfgp0m955naxix3bs3r6a75n4s"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:test-command '("buttercup" "-L" ".")
       #:phases
       (modify-phases %standard-phases
         ;; The HOME environment variable should be set to an existing
         ;; directory for the tests to succeed.
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" "/tmp")
             #t)))))
    (native-inputs
     `(("emacs-buttercup" ,emacs-buttercup)))
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

(define-public emacs-tracking
  (package
    (inherit emacs-circe)
    (name "emacs-tracking")
    (arguments
     ;; "tracking.el" is a library extracted from Circe package.  It requires
     ;; "shorten.el".
     `(#:include '("^shorten.el$" "^tracking.el$")
       ,@(package-arguments emacs-circe)))
    (home-page "https://github.com/jorgenschaefer/circe/wiki/Tracking")
    (synopsis "Buffer tracking library")
    (description "@code{tracking.el} provides a way for different modes to
notify the user that a buffer needs attention.  The user then can cycle
through them using @key{C-c C-SPC}.")
    (license license:gpl3+)))

(define-public emacs-slack
  (let ((commit "99a57501629a0329a9ca090c1ea1296462eda02d")
        (revision "5"))
    (package
      (name "emacs-slack")
      (version (git-version "0.0.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yuya373/emacs-slack.git")
                      (commit commit)))
                (file-name (git-file-name name commit))
                (sha256
                 (base32
                  "0jw1diypfw8pmzkq0napgxmfc0gqka7zcccgnw359604lr30k2z2"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-alert" ,emacs-alert)
         ("emacs-emojify" ,emacs-emojify)
         ("emacs-helm" ,emacs-helm)
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
   (version "2.1.0")
   (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/szermatt/emacs-bash-completion.git")
            (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "1a1wxcqzh0javjmxwi3lng5i99xiylm8lm04kv4q1lh9bli6vmv0"))))
   (inputs `(("bash" ,bash)))
   (build-system emacs-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'make-git-checkout-writable
          (λ _
            (for-each make-file-writable (find-files "."))
            #t))
        (add-before 'install 'configure
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((bash (assoc-ref inputs "bash")))
              (emacs-substitute-variables "bash-completion.el"
                ("bash-completion-prog" (string-append bash "/bin/bash"))))
            #t)))))
   (home-page "https://github.com/szermatt/emacs-bash-completion")
   (synopsis "Bash completion for the shell buffer")
   (description
    "@code{bash-completion} defines dynamic completion hooks for shell-mode
and shell-command prompts that are based on Bash completion.")
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/holomorph/transmission.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kvg2gawsgy440x1fsl2c4pkxwp3zirq9rzixanklk0ryijhd3ry"))))
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
  (package
    (name "emacs-polymode")
    (version "0.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vspinu/polymode.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wwphs54jx48a3ca6x1qaz56j3j9bg4mv8g2akkffrzbdcb8sbc7"))))
    (build-system emacs-build-system)
    (arguments
     `(#:include (cons* "^modes/.*\\.el$" %default-include)
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-emacs-load-path 'add-modes-subdir-to-load-path
           (lambda _
             (setenv "EMACSLOADPATH"
                     (string-append (getenv "EMACSLOADPATH")
                                    ":" (getcwd) "/modes" ":")))))))
    (home-page "https://github.com/vspinu/polymode")
    (synopsis "Framework for multiple Emacs modes based on indirect buffers")
    (description "Polymode is an Emacs package that offers generic support
for multiple major modes inside a single Emacs buffer.  It is lightweight,
object oriented and highly extensible.  Creating a new polymode typically
takes only a few lines of code.  Polymode also provides extensible facilities
for external literate programming tools for exporting, weaving and tangling.")
    (license license:gpl3+)))

(define-public emacs-polymode-ansible
  (let ((commit "b26094d029e25dc797b94254f797e7807a57e4c8"))
    (package
      (name "emacs-polymode-ansible")
      ;; No upstream version release yet.
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/mavit/poly-ansible")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "055shddqibib3hx2ykwdz910nrqws40cd407mq946l2bf6v87gj6"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-ansible-doc" ,emacs-ansible-doc)
         ("emacs-jinja2-mode" ,emacs-jinja2-mode)
         ("emacs-polymode" ,emacs-polymode)
         ("emacs-yaml-mode" ,emacs-yaml-mode)))
      (properties '((upstream-name . "poly-ansible")))
      (home-page "https://gitlab.com/mavit/poly-ansible/")
      (synopsis "Polymode for Ansible - Jinja2 in YAML")
      (description
       "Edit YAML files for Ansible containing embedded Jinja2 templating.")
      (license license:gpl3+))))

(define-public eless
  (package
    (name "eless")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kaushalmodi/eless.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jr7vhh4vw69llhi0fh9ljscljkszkj0acdxl04da5hvqv6pnqbb"))))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (copy-recursively (assoc-ref %build-inputs "source") "source")
         (chdir "source")
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
    (version "2.2.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/redguardtoo/evil-matchit.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12if45pxfndy3d7r4gd3zx4d3jk4d64fdmwkhc3y5zhqq9h9iy4c"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-evil" ,emacs-evil)))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/expez/evil-smartparens.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1di4qz5fbrlwbg16c2j0m7y8zqfxw027qd7zqmc3rwk9znbhg7wl"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blorbx/evil-quickscope.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ja9ggj70wf0nmma4xnc1zdzg2crq9h1cv3cj7cgwjmllflgkfq7"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-evil" ,emacs-evil)))
    (arguments
     `(#:tests? #t
       #:test-command '("emacs" "--batch"
                        "-l" "evil-quickscope-tests.el"
                        "-f" "ert-run-tests-batch-and-exit")))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dbrock/bongo.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q3ws2vn062dh7ci6jn2k2bcn7szh3ap64sgwkzdd6f1pas37fnr"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/dbrock/bongo")
    (synopsis "Media player for Emacs")
    (description
     "This package provides a flexible media player for Emacs.  @code{Bongo}
supports multiple backends such as @code{vlc}, @code{mpg123},
@code{ogg123}, @code{speexdec}, @code{timidity}, @code{mikmod} and
@code{afplay}.")
    (license license:gpl2+)))

(define-public emacs-groovy-modes
  (package
    (name "emacs-groovy-modes")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes"
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

(define-public groovy-emacs-modes
  (deprecated-package "groovy-emacs-modes" emacs-groovy-modes))

(define-public emacs-org-tree-slide
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

(define-public emacs-kv
  (package
    (name "emacs-kv")
    (version "0.0.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nicferrier/emacs-kv.git")
             (commit "721148475bce38a70e0b678ba8aa923652e8900e")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0r0lz2s6gvy04fwnafai668jsf4546h4k6zd6isx5wpk0n33pj5m"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:test-command '("emacs" "--batch" "-l" "kv-tests.el"
                        "-f" "ert-run-tests-batch-and-exit")))
    (home-page "https://github.com/nicferrier/emacs-kv")
    (synopsis "Key/Value data structures library for Emacs Lisp")
    (description "@code{emacs-kv} is a collection of tools for dealing with
key/value data structures such as plists, alists and hash-tables in Emacs
Lisp.")
    (license license:gpl3+)))

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
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-sources
           (lambda _
             ;; See: https://github.com/tali713/esxml/pull/28.
             (substitute* "css-lite.el"
               ((";;; main interface")
                (string-append ";;; main interface\n"
                               "(require 'cl-lib)"))
               (("mapcan")
                "cl-mapcan")
               (("',\\(cl-mapcan #'process-css-rule rules\\)")
                "(cl-mapcan #'process-css-rule ',rules)"))
             (substitute* "esxml-form.el"
               ((",esxml-form-field-defn")
                "#'esxml-form-field-defn"))
             ;; See: https://github.com/tali713/esxml/issues/25
             (delete-file "esxpath.el")
             #t)))))
    (propagated-inputs
     `(("emacs-kv" ,emacs-kv)))
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
    (version "0.2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wasamasa/nov.el.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "188h5gzn1zf443g0b7q5bpmvvpr6ds5h8aci8vxc92py56rhyrvc"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cute-jumper/epipe.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lkisi1s7sn12nx8zh58qmsxwnk1rjwryj18wcbr148xqz3swg57"))))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)
       ("perl" ,perl)))
    (arguments
     `(#:modules
       ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         ;; Extract source.
         (copy-recursively (assoc-ref %build-inputs "source") "source")
         (chdir "source")
         ;; Patch shebangs.
         (substitute* "epipe"
           (("/usr/bin/env bash")
            (string-append (assoc-ref %build-inputs "bash") "/bin/bash")))
         (patch-shebang "epipe.pl"
                        (list (string-append (assoc-ref %build-inputs "perl")
                                             "/bin")))
         ;; Install.
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/syohex/emacs-hcl-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jqrgq15jz6pvx38pnwkizzfiih0d3nxqphyrc92nqpcyimg8b6g"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/syohex/emacs-terraform-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05hn8kskx9lcgn7bzgam99c629zlryir2pickwrqndacjrqpdykx"))))
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

(define-public emacs-anzu
  (package
    (name "emacs-anzu")
    (version "0.62")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/syohex/emacs-anzu.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lzvc0ihcbplir4hqfyxfqpsd78arz15gk92kmq4f8ggbkl37fan"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/syohex/emacs-anzu")
    (synopsis "Show number of matches in mode-line while searching")
    (description
     "Anzu provides a minor mode which displays \"current match/total
matches\" in the mode line in various search modes.  This is an Emacs port of
Anzu.zim.")
    (license license:gpl3+)))

(define-public emacs-emmet-mode
  (package
    (name "emacs-emmet-mode")
    (version "1.0.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/smihica/emmet-mode.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dsa85bk33j90h1ypaz1ylqh9yp2xvlga237h3kwa5y3sb0d5ydi"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/smihica/emmet-mode")
    (synopsis "Unofficial Emmet's support for Emacs")
    (description
     "Unfold CSS-selector-like expressions to markup.  It is intended to be
used with SGML-like languages: XML, HTML, XHTML, XSL, etc.")
    (license license:gpl3+)))

(define-public emacs-ergoemacs-mode
  (let ((commit "3ce23bba3cb50562693860f87f3528c471d603ba")
        (revision "1"))
    (package
      (name "emacs-ergoemacs-mode")
      (version (git-version "5.16.10.12" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ergoemacs/ergoemacs-mode.git")
               (commit commit)))
         (sha256
          (base32
           "1s3b9bridl78hh1mxmdk9nqlmqhibbaxk0a1cixmsf23s06w8w6l"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-undo-tree" ,emacs-undo-tree)))
      (home-page "https://ergoemacs.github.io/")
      (synopsis "Emacs mode based on common modern interface and ergonomics")
      (description
       "This package provides an efficient Emacs keybinding set based on
statistics of command frequency, and supports common shortcuts for open,
close, copy, cut, paste, undo, redo.")
      (license license:gpl3+))))

(define-public emacs-password-store
  (package
    (name "emacs-password-store")
    (version "1.7.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://git.zx2c4.com/password-store/snapshot/"
                              "password-store-" version ".tar.xz"))
              (sha256
               (base32
                "1x53k5dn3cdmvy8m4fqdld4hji5n676ksl0ql4armkmsds26av1b"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'extract-el-file
           (lambda _
             (copy-file "contrib/emacs/password-store.el" "password-store.el")
             (delete-file-recursively "contrib")
             (delete-file-recursively "man")
             (delete-file-recursively "src")
             (delete-file-recursively "tests"))))))
    (propagated-inputs
     `(("emacs-f" ,emacs-f)
       ("emacs-s" ,emacs-s)
       ("emacs-with-editor" ,emacs-with-editor)
       ("password-store" ,password-store)))
    (home-page "https://git.zx2c4.com/password-store/tree/contrib/emacs")
    (synopsis "Password store (pass) support for Emacs")
    (description
     "This package provides functions for working with pass (\"the
standard Unix password manager\").")
    (license license:gpl2+)))

(define-public emacs-pass
  (package
    (name "emacs-pass")
    (version "1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NicolasPetton/pass.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05h4hacv3yygyjcjj004qbyqjpkl4pyhwgp25gsz8mw5c66l70cx"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-password-store" ,emacs-password-store)
       ("emacs-f" ,emacs-f)))
    (home-page "https://github.com/NicolasPetton/pass")
    (synopsis "Major mode for @file{password-store.el}")
    (description "This is a major mode for managing password-store (pass)
keychains.  The keychain entries are displayed in a directory-like structure
and can be consulted and modified.")
    (license license:gpl3+)))

(define-public emacs-evil-anzu
  (package
    (name "emacs-evil-anzu")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/syohex/emacs-evil-anzu"
                           "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "032hh2946z529cizqsg8pm6cpn5qdj8lfk3qskmx6xv3g2ra56ns"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-evil" ,emacs-evil)
       ("emacs-anzu" ,emacs-anzu)))
    (home-page "https://github.com/syohex/emacs-evil-anzu")
    (synopsis "Anzu for evil-mode")
    (description "@code{anzu} provides a minor mode that displays the current
match and total match information in the mode-line in various search modes.")
    (license license:gpl3+)))

(define-public emacs-pg
  (let ((commit "4f6516ec3946d95dcef49abb6703cc89ecb5183d"))
    (package
      (name "emacs-pg")
      (version (git-version "0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference (url "https://github.com/cbbrowne/pg.el")
                                    (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1zh7v4nnpzvbi8yj1ynlqlawk5bmlxi6s80b5f2y7hkdqb5q26k0"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/cbbrowne/pg.el")
      (synopsis "Emacs Lisp interface for PostgreSQL")
      (description
       "This package provides an Emacs Lisp interface for PostgreSQL.")
      (license license:gpl3+))))

(define-public emacs-cl-generic
  (package
    (name "emacs-cl-generic")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/cl-generic-"
                           version ".el"))
       (sha256
        (base32
         "0vb338bhjpsnrf60qgxny4z5rjrnifahnrv9axd4shay89d894zq"))))
    (build-system emacs-build-system)
    (home-page "https://elpa.gnu.org/packages/seq.html")
    (synopsis
     "Forward @code{cl-generic} compatibility for Emacs before version 25")
    (description "This package provides a subset of the features of the
@code{cl-generic} package introduced in Emacs-25, for use on previous
@code{emacsen}.")
    (license license:gpl3+)))

(define-public emacs-finalize
  (package
  (name "emacs-finalize")
  (version "2.0.0")
  (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/skeeto/elisp-finalize.git")
            (commit version)))
      (file-name (git-file-name name version))
      (sha256
        (base32 "1gvlm4i62af5jscwz0jccc8ra0grprxpg2rlq91d5nn8dn5lpy79"))))
  (build-system emacs-build-system)
  (propagated-inputs
    `(("emacs-cl-generic" ,emacs-cl-generic)))
  (home-page "https://github.com/skeeto/elisp-finalize")
  (synopsis "Finalizers for Emacs Lisp")
  (description
    "This package will allows to immediately run a callback (a finalizer)
after its registered lisp object has been garbage collected.  This allows for
extra resources, such as buffers and processes, to be cleaned up after the
object has been freed.")
  (license license:unlicense)))

(define-public emacs-emacsql
  (package
    (name "emacs-emacsql")
    (version "3.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/skeeto/emacsql.git")
                    (commit (string-append version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c84gxr1majqj4b59wgdy3lzm3ap66w9qsrnkx8hdbk9895ak81g"))))
    (build-system emacs-build-system)
    (arguments
     `(#:modules ((guix build emacs-build-system)
                  (guix build utils)
                  (guix build emacs-utils)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (delete 'build) ;‘build-emacsql-sqlite’ compiles ‘*.el’ files.
         (add-before 'install 'patch-elisp-shell-shebangs
           (lambda _
             (substitute* (find-files "." "\\.el")
               (("/bin/sh") (which "sh")))
             #t))
         (add-after 'patch-elisp-shell-shebangs 'setenv-shell
           (lambda _
             (setenv "SHELL" "sh")))
         (add-after 'setenv-shell 'build-emacsql-sqlite
           (lambda _
             (invoke "make" "binary" "CC=gcc")))
         (add-after 'build-emacsql-sqlite 'install-emacsql-sqlite
           ;; This build phase installs emacs-emacsql binary.
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "sqlite/emacsql-sqlite"
                           (string-append (assoc-ref outputs "out")
                                          "/bin"))
             #t))
         (add-after 'install-emacsql-sqlite 'patch-emacsql-sqlite.el
           ;; This build phase removes interactive prompts
           ;; and makes sure Emacs look for binaries in the right places.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((file "emacsql-sqlite.el"))
               (chmod file #o644)
               (emacs-substitute-sexps file
                 ;; Make sure Emacs looks for ‘GCC’ binary in the right place.
                 ("(executable-find" (which "gcc"))
                 ;; Make sure Emacs looks for ‘emacsql-sqlite’ binary
                 ;; in the right place.
                 ("(defvar emacsql-sqlite-executable"
                  (string-append (assoc-ref outputs "out")
                                 "/bin/emacsql-sqlite"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (install-file "sqlite/emacsql-sqlite"
                             (string-append out "/bin"))
               (for-each (cut install-file <>
                              (string-append out "/share/emacs/site-lisp/guix.d/"
                                             "emacsql" "-" ,version))
                         (find-files "." "\\.elc*$")))
             #t)))))
    (inputs
     `(("emacs-minimal" ,emacs-minimal)
       ("mariadb" ,mariadb)
       ("postgresql" ,postgresql)))
    (propagated-inputs
     `(("emacs-finalize" ,emacs-finalize)
       ("emacs-pg" ,emacs-pg)))
    (home-page "https://github.com/skeeto/emacsql")
    (synopsis "Emacs high-level SQL database front-end")
    (description "Any readable Lisp value can be stored as a value in EmacSQL,
including numbers, strings, symbols, lists, vectors, and closures.  EmacSQL
has no concept of @code{TEXT} values; it's all just Lisp objects.  The Lisp
object @code{nil} corresponds 1:1 with @code{NULL} in the database.")
    (license license:gpl3+)))

(define-public emacs-closql
  (package
    (name "emacs-closql")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacscollective/closql.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1xhpfjjkjqfc1k2rj77cscclz5r7gpvv3hi202x178vdcpipjwar"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-emacsql" ,emacs-emacsql)))
    (home-page "https://github.com/emacscollective/closql")
    (synopsis "Store EIEIO objects using EmacSQL")
    (description
     "This package allows to store uniform EIEIO objects in an EmacSQL
database.  SQLite is used as backend.  This library imposes some restrictions
on what kind of objects can be stored; it isn't intended to store arbitrary
objects.  All objects have to share a common superclass and subclasses cannot
add any additional instance slots.")
    (license license:gpl3)))

(define-public emacs-epkg
  ;; The release version is to old for the current database scheme.
  (let ((commit "432312b9583ed7b88ad9644fd1bf2183765a892e"))
    (package
      (name "emacs-epkg")
      (version (git-version "3.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacscollective/epkg.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0d882kahn7a0vri7a9r15lvmfx1zn2hsga6jfcc6jv0hqbswlb2k"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-closql" ,emacs-closql)
         ("emacs-dash" ,emacs-dash)))
      (home-page "https://emacsmirror.net")
      (synopsis "Browse the Emacsmirror package database")
      (description "This package provides access to a local copy of the
Emacsmirror package database.  It provides low-level functions for querying
the database and a @file{package.el} user interface for browsing the database.
Epkg itself is not a package manager.

Getting a local copy:

@example
git clone https://github.com/emacsmirror/epkgs.git ~/.emacs.d/epkgs
cd ~/.emacs.d/epkgs
git submodule init
git config --global url.https://github.com/.insteadOf git@@github.com:
git submodule update
@end example

Some submodule may be missing.  In this case Git will prompt for a GitHub user
name and password.  To skip it press a @key{Return} key.

You could get a Epkg package list by invoking @code{epkg-list-packages} in
Emacs.")
      (license license:gpl3+))))

(define-public emacs-elisp-slime-nav
  (package
    (name "emacs-elisp-slime-nav")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/purcell/elisp-slime-nav.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11vyy0bvzbs1h1kggikrvhd658j7c730w0pdp6qkm60rigvfi1ih"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/purcell/elisp-slime-nav")
    (synopsis "Make @code{M-.} and @code{M-,} work for elisp like they do in SLIME")
    (description
     "This package provides SLIME's convenient @code{M-.}and @code{M-,} navigation
in @code{emacs-lisp-mode}, together with an elisp equivalent of
@code{slime-describe-symbol}.")
    (license license:gpl3+)))

(define-public emacs-dedicated
  (package
    (name "emacs-dedicated")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacsorphanage/dedicated.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pba9s0h37sxyqh733vi6k5raa4cs7aradipf3826inw36jcw414"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/emacsorphanage/dedicated")
    (synopsis "Emacs minor mode for toggling a windows's \"dedicated\" flag")
    (description
     "This simple Emacs minor mode allows you to toggle a window's
\"dedicated\" flag.  When a window is \"dedicated\", Emacs will not select
files into that window.  This can be quite handy since many commands will use
another window to show results (compilation mode, starting info, and so on).
A dedicated window won't be used for such a purpose.  For details, please read
the source file.")
    (license license:gpl2+)))

(define-public emacs-nnreddit
  (let ((commit "9843f99d01fd8f1eea2fc685965a7c7f4eeb187a")
        (revision "1"))
    (package
      (name "emacs-nnreddit")
      (version (string-append "0.0.1-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/paul-issartel/nnreddit.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0j4h3bnga640250jdq8bwyja49r41ssrsjd6lba4gzzllqk02nbn"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/paul-issartel/nnreddit")
      (synopsis "Reddit backend for the Gnus newsreader")
      (description "@url{https://www.reddit.com} backend for the Gnus
newsreader.")
      (license license:gpl3+))))

(define-public emacs-makey
  (package
    (name "emacs-makey")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mickeynp/makey.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rr7vpm3xxzcaam3m8xni3ajy8ycyljix07n2jzczayri9sd8csy"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/mickeynp/makey")
    (synopsis "Emacs interactive command-line mode")
    (description
     "This package provides an Emacs interactive command-line mode.")
    (license license:gpl3+)))

(define-public emacs-outorg
  (let ((commit "78b0695121fb974bc4e971eb4ef7f8afd6d89d64"))
    (package
      (name "emacs-outorg")
      (version (git-version "2.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alphapapa/outorg")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "03aclh4m3f7rb821gr9pwvnqkkl91px3qxdcarpf3ypa1x4fxvlj"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/alphapapa/outorg")
      (synopsis "Org-style comment editing")
      (description "Outorg is for editing comment-sections of source-code
files in temporary Org-mode buffers.  It turns conventional
literate-programming upside-down in that the default mode is the
programming-mode, and special action has to be taken to switch to the
text-mode (i.e. Org-mode).")
      (license license:gpl3+))))

(define-public emacs-outshine
  (let ((commit "5f1a6b70231d2811c522e4e5e8c89ff461b311d6"))
    (package
      (name "emacs-outshine")
      (version (git-version "2.0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alphapapa/outshine.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1l9v1dfhgg7il11ifbhvcvrg3acfjk9sdxlc3lja1k54d7dp60jv"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-outorg" ,emacs-outorg)))
      (home-page "https://github.com/alphapapa/outshine")
      (synopsis "Emacs outline with outshine")
      (description "Outshine attempts to bring the look and feel of
@code{org-mode} to an Emacs outside of the Org major-mode.  It is an extension
of @code{outline-minor-mode} (@code{org-mode} itself derives from
outline-mode), so there is no such thing like an outshine mode, only
@code{outline-minor-mode} with outshine extensions loaded.")
      (license license:gpl3+))))

(define-public emacs-parsebib
  (package
    (name "emacs-parsebib")
    (version "2.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joostkremers/parsebib.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bnqnxkb9dnl0fjrrjx0xn9jsqki2h8ygw3d5dm4bl79smah3qkh"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/joostkremers/parsebib")
    (synopsis "Library for parsing bib files")
    (description
     "This package provides an Emacs library for parsing bib files.")
    (license license:gpl3+)))

(define-public emacs-biblio
  (package
    (name "emacs-biblio")
    (version "0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cpitclaudel/biblio.el.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gxjind6r235az59dr8liv03d8994mqb8a7m28j3c12q7p70aziz"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-seq" ,emacs-seq)
       ("emacs-dash" ,emacs-dash)
       ("emacs-let-alist" ,emacs-let-alist)))
    (home-page "https://github.com/cpitclaudel/biblio.el")
    (synopsis "Browse and import bibliographic references")
    (description "This package provides an extensible Emacs package for
browsing and fetching references.

@file{biblio.el} makes it easy to browse and gather bibliographic references
and publications from various sources, by keywords or by DOI.  References are
automatically fetched from well-curated sources, and formatted as BibTeX.")
    (license license:gpl3+)))

(define-public emacs-helm-bibtex
  (let ((commit "8ed898fb5a68f18e9bb9973832a5c1f8abcfc463")
        (revision "1"))
    (package
      (name "emacs-helm-bibtex")
      (version (string-append "2.0.0" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tmalsburg/helm-bibtex.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "14lyx0vbqr97p3anzrsp7m3q0kqclyjcdwplpraim403fcklzbnz"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)
         ("emacs-parsebib" ,emacs-parsebib)
         ("emacs-s" ,emacs-s)
         ("emacs-dash" ,emacs-dash)
         ("emacs-f" ,emacs-f)
         ("emacs-biblio" ,emacs-biblio)
         ("emacs-ivy" ,emacs-ivy)))
      (home-page "https://github.com/tmalsburg/helm-bibtex")
      (synopsis "Bibliography manager based on Helm")
      (description "This package provides bibliography manager for Emacs,
based on Helm and the bibtex-completion backend.

Key features:

@itemize
@item Quick access to your bibliography from within Emacs
@item Powerful search capabilities
@item Provides instant search results as you type
@item Tightly integrated with LaTeX authoring, emails, Org mode, etc.
@item Open the PDFs, URLs, or DOIs associated with an entry
@item Insert LaTeX cite commands, Ebib links, or Pandoc citations,
BibTeX entries, or plain text references at point, attach PDFs to emails
@item Support for note taking
@item Quick access to online bibliographic databases such as Pubmed,
arXiv, Google Scholar, Library of Congress, etc.
@item Imports BibTeX entries from CrossRef and other sources.
@end itemize\n")
      (license license:gpl3+))))

(define-public emacs-ewmctrl
  (let ((commit "3d0217c4d6cdb5c308b6cb4293574f470d4faacf"))
    (package
      (name "emacs-ewmctrl")
      (version (git-version "0.0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/flexibeast/ewmctrl.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0ilwvx0qryv3v6xf0gxqwnfm6pf96gxap8h9g3f6z6lk9ff4n1wi"))))
      (build-system emacs-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-ewmctrl
             ;; This build phase makes sure ‘ewmctrl’ looks
             ;; for ‘wmctrl’ in the right place.
             (lambda _
               (let ((file "ewmctrl.el"))
                 (chmod file #o644)
                 (emacs-substitute-sexps file
                   ("(defcustom ewmctrl-wmctrl-path" (which "wmctrl")))))))))
      (inputs
       `(("wmctrl" ,wmctrl)))
      (home-page "https://github.com/flexibeast/ewmctrl")
      (synopsis "Emacs interface to @code{wmctrl}")
      (description "@code{ewmctrl} provides an Emacs interface to
@code{wmctrl} command-line window-management program.")
      (license license:gpl3+))))

(define-public emacs-erc-image
  (let ((commit "82fb3871f02e24b1e880770b9a3d187aab43d0f0")
        (revision "1"))
    (package
      (name "emacs-erc-image")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kidd/erc-image.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1q8mkf612fb4fjp8h4kbr107wn083iqfdgv8f80pcmil8y33dw9i"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/kidd/erc-image.el")
      (synopsis "Show inlined images (png/jpg/gif/svg) in ERC buffers")
      (description "This plugin subscribes to hooks @code{erc-insert-modify-hook}
and @code{erc-send-modify-hook} to download and show images.")
      (license license:gpl3+))))

(define-public emacs-helm-gtags
  (package
    (name "emacs-helm-gtags")
    (version "1.5.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/syohex/emacs-helm-gtags.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zyspn9rqfs3hkq8qx0q1w5qiv30ignbmycyv0vn3a6q7a5fsnhx"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-helm" ,emacs-helm)))
    (home-page "https://github.com/syohex/emacs-helm-gtags")
    (synopsis "Emacs Helm interface to GNU Global")
    (description
     "@code{emacs-helm-gtags} provides a Emacs Helm interface to GNU Global.")
    (license license:gpl3+)))

(define-public emacs-list-utils
  (package
    (name "emacs-list-utils")
    (version "0.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rolandwalker/list-utils.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ql159v7sxs33yh2l080kchrj52vk34knz50cvqi3ykpb7djg3sz"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/rolandwalker/list-utils")
    (synopsis "List-manipulation utility functions")
    (description "This package provides a list manipulation library for Emacs.")
    (license license:gpl3+)))

(define-public emacs-move-text
  (package
    (name "emacs-move-text")
    (version "2.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacsfodder/move-text.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06jxk5g23822gfmwrxhc34zand3dr8p2wjh1zs3j61ibz6n0nmz1"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/emacsfodder/move-text")
    (synopsis "Move current line or region with M-up or M-down")
    (description "This package provide functions to move the current line
using @kbd{M-up} or @kbd{M-down} if a region is marked, it will move the
region instead.")
    (license license:gpl3+)))

(define-public emacs-validate
  (package
    (name "emacs-validate")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/Malabarba/validate.el"
                                  "/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "125mbd111f1h1baw0z3fzm48y1bvaigljyzvvnqgrn0shxbj0khg"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Malabarba/validate.el")
    (synopsis "Emacs library for scheme validation")
    (description "This Emacs library provides two functions that perform
schema validation.")
    (license license:gpl3+)))

(define-public emacs-rainbow-blocks
  (let ((commit "dd435d7bb34ff6f162a5f315df308b90b7e9f842"))
    (package
      (name "emacs-rainbow-blocks")
      (version (git-version "1.0.0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/istib/rainbow-blocks.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "06yfb3i7wzvqrhkb61zib9xvpb5i00s4frizkzff66im05k0n795"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/istib/rainbow-blocks")
      (synopsis "Highlight sexp blocks")
      (description "Rainbow-blocks is an Emacs mode that highlights blocks
made of parentheses, brackets, and braces according to their depth.  Each
successive level is highlighted in a different color.  This makes it easy to
orient yourself in the code, and tell which statements are at a given level.")
      (license license:gpl3+))))

(define-public emacs-hierarchy
  (package
    (name "emacs-hierarchy")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DamienCassou/hierarchy.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kykbb1sil5cycfa5aj8dhsxc5yrx1641i2np5kwdjid6ahdlz5r"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/DamienCassou/hierarchy")
    (synopsis "Library to create and display hierarchy structures")
    (description "This package provides an Emacs library to create, query,
navigate and display hierarchy structures.")
    (license license:gpl3+)))

(define-public emacs-tree-mode
  (let ((commit "b06078826d5875d74b0e7b7ac47b0d0917610534")
        (revision "1"))
    (package
      (name "emacs-tree-mode")
      (version (string-append "0.0.1" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacsorphanage/tree-mode.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "13bbdhdmqg4x9yghanhr8fsbsxbnypzxdxgicz31sjjm675kpnix"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/emacsorphanage/tree-mode")
      (synopsis "Emacs mode to manage tree widgets")
      (description
       "This package provides an Emacs library to manage tree widgets.")
      (license license:gpl3+))))

(define-public emacs-md4rd
  (let ((commit "c55512c2f7680db2a1e73db6bdf93adecaf40fec")
        (revision "1"))
    (package
      (name "emacs-md4rd")
      (version (string-append "0.0.2" "-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ahungry/md4rd.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0mvv1mvsrpkrmikcpfqf2zbawnzgq33j6zjdrlv48mcw57xb2ak9"))))
      (propagated-inputs
       `(("emacs-hierarchy" ,emacs-hierarchy)
         ("emacs-request" ,emacs-request)
         ("emacs-dash" ,emacs-dash)
         ("emacs-s" ,emacs-s)
         ("emacs-tree-mode" ,emacs-tree-mode)))
      (build-system emacs-build-system)
      (home-page "https://github.com/ahungry/md4rd")
      (synopsis "Emacs Mode for Reddit")
      (description
       "This package allows to read Reddit from within Emacs interactively.")
      (license license:gpl3+))))

(define-public emacs-pulseaudio-control
  (let ((commit "7e1a87068379075a5e9ce36c64c686c03d20d379")
        (revision "3"))
    (package
      (name "emacs-pulseaudio-control")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/flexibeast/pulseaudio-control.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0wcaqyh15x56255rrj350089z15pnwixa2vf0ly6dv0hjmzmh3mr"))))
      (build-system emacs-build-system)
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (add-after 'unpack 'patch-file-name
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let ((pulseaudio (assoc-ref inputs "pulseaudio")))
                          (chmod "pulseaudio-control.el" #o600)
                          (emacs-substitute-variables "pulseaudio-control.el"
                            ("pulseaudio-control-pactl-path"
                             (string-append pulseaudio "/bin/pactl")))
                          #t))))))
      (inputs `(("pulseaudio" ,pulseaudio)))
      (home-page "https://github.com/flexibeast/pulseaudio-control")
      (synopsis "Control @code{pulseaudio} from Emacs")
      (description
       "This package allows to control @code{pulseaudio} from Emacs.")
      (license license:gpl3+))))

(define-public emacs-datetime
  (package
    (name "emacs-datetime")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/doublep/datetime.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fdswqi53qx924lib7nd9dazn0916xf1ybrh3bcn3f8cn6b8ikg5"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/doublep/datetime/")
    (synopsis "Library to work with dates in Emacs")
    (description "Parsing, formatting, matching and recoding
timestamps and date-time format strings library for Emacs.")
    (license license:gpl3+)))

(define-public emacs-org-mind-map
  (let ((commit "9d6e262bedd94daf9de269f4d56de277275677cb")
        (revision "1"))
    (package
      (name "emacs-org-mind-map")
      (version (string-append "0.0.1" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/theodorewiles/org-mind-map.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0jgkkgq7g64zckrmjib0hvz0qy3ynz5vz13qbmlpf096l3bb65wn"))))
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)))
      (build-system emacs-build-system)
      (home-page "https://github.com/theodorewiles/org-mind-map")
      (synopsis "Create Graphviz directed graphs from Org files")
      (description
       "This package creates Graphviz directed graphs from Org files.")
      (license license:gpl3+))))

(define-public emacs-npm-mode
  (package
    (name "emacs-npm-mode")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mojochao/npm-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mh6nbffciw4yhv049kdhh796ysj1x21ndm3fwymhskb3dy0w1ss"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/mojochao/npm-mode")
    (synopsis "Minor mode for working with @code{npm} projects")
    (description
     "@code{npm-mode} provides a minor mode to work with @code{npm} projects.")
    (license license:gpl3+)))

(define-public emacs-seq
  (package
    (name "emacs-seq")
    (version "2.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://elpa.gnu.org/packages/seq-" version ".tar"))
       (sha256
        (base32
         "0vrpx6nnyjb0gsypknzagimlhvcvi5y1rcdkpxyqr42415zr8d0n"))))
    (build-system emacs-build-system)
    (home-page "http://elpa.gnu.org/packages/seq.html")
    (synopsis "Sequence manipulation functions")
    (description "Sequence-manipulation functions that complement basic
functions provided by @file{subr.el}.")
    (license license:gpl3+)))

(define-public emacs-itail
  (let ((commit "6e43c20da03be3b9c6ece93b7dc3495975ec1888")
        (revision "1"))
    (package
      (name "emacs-itail")
      (version (string-append "0.0.1" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/re5et/itail.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "044nzxh1hq41faxw3lix0wy78vfz304pjcaa5a11dqfz7q3gx5cv"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/re5et/itail")
      (synopsis "Interactive @code{tail} Emacs mode")
      (description "@code{itail} provides interactive @code{tail} mode
that allows you to filter the tail with unix pipes and highlight the
contents of the tailed file.  Works locally or on remote files using
tramp.")
      (license license:gpl3+))))

(define-public emacs-loop
  (package
    (name "emacs-loop")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Wilfred/loop.el.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gs95xnmnn8aa4794k7h8mw1sz1nfdh9v0caqj6yvnsdnwy74n5x"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Wilfred/loop.el")
    (synopsis "Imperative loop structures for Emacs")
    (description "Loop structures familiar to users of other languages.  This
library adds a selection of popular loop structures as well as break and
continue.")
    (license license:gpl3+)))

(define-public emacs-elisp-refs
  (package
    (name "emacs-elisp-refs")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Wilfred/elisp-refs.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w7k91xf69zc0zkjrw8h8sjdjf9xq9qs5zhvajhd718lzf93564b"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-f" ,emacs-f)
       ("emacs-list-utils" ,emacs-list-utils)
       ("emacs-loop" ,emacs-loop)
       ("emacs-s" ,emacs-s)
       ("emacs-shut-up" ,emacs-shut-up)))
    (home-page "https://github.com/Wilfred/elisp-refs")
    (synopsis "Find callers of elisp functions or macros")
    (description "Find references to functions, macros or variables.  Unlike a
dumb text search, @code{elisp-refs} actually parses the code, so it's never
confused by comments or @code{foo-bar} matching @code{foo}.")
    (license license:gpl3+)))

(define-public emacs-crux
  (let ((commit "4f5c8fefd5a6aa52e128c4a0401cc86410d6ac8f")
        (revision "1"))
    (package
      (name "emacs-crux")
      (version (string-append "0.3.0" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/bbatsov/crux.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1fdxvv25cs01sg6fmvmzxpzvs50i6v8n2jya60lbavxqqhi0sbxd"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/bbatsov/crux")
      (synopsis "Collection of useful functions for Emacs")
      (description
       "@code{crux} provides a collection of useful functions for Emacs.")
      (license license:gpl3+))))

(define-public emacs-edit-server
  (package
    (name "emacs-edit-server")
    (version "1.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stsquad/emacs_chrome.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12dp1xj09jrp0kxp9xb6cak9dn6zkyis1wfn4fnhzmxxnrd8c5rn"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-elisp
           ;; Elisp directory is not in root of the source.
           (lambda _
             (chdir "servers"))))))
    (home-page "https://github.com/stsquad/emacs_chrome")
    (synopsis "Server that responds to edit requests from Chromium")
    (description
     "This package provides an edit server to respond to requests from Emacs.")
    (license license:gpl3+)))

(define-public emacs-m-buffer-el
  (package
    (name "emacs-m-buffer-el")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/phillord/m-buffer-el"
                           "/archive/" "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17vdcc8q37q9db98jyww1c0ivinmwfcw4l04zccfacalra63a214"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "emacs" "--batch" "-L" "."
                     "-l" "test/m-buffer-test.el"
                     "-l" "test/m-buffer-at-test.el"
                     "-f" "ert-run-tests-batch-and-exit"))))))
    (build-system emacs-build-system)
    (home-page "https://github.com/phillord/m-buffer-el")
    (synopsis "List oriented buffer operations for Emacs")
    (description "@code{m-buffer} provides a set of list-orientated functions
for operating over the contents of Emacs buffers.")
    (license license:gpl3+)))

(define-public emacs-let-alist
  (package
    (name "emacs-let-alist")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://elpa.gnu.org/packages/let-alist-" version ".el"))
       (sha256
        (base32
         "0r7b9jni50la1m79kklml11syg8d2fmdlr83pv005sv1wh02jszw"))))
    (build-system emacs-build-system)
    (home-page "https://elpa.gnu.org/packages/let-alist.html")
    (synopsis "Easily let-bind values of an assoc-list by their names")
    (description "This package offers a single macro, @code{let-alist}.  This
macro takes a first argument (whose value must be an alist) and a body.")
    (license license:gpl3+)))

(define-public emacs-esup
  (let ((commit "a589005a9a888537deef94d6fe38a9b8790c97c7")
        (revision "1"))
    (package
      (name "emacs-esup")
      (version (string-append "0.6" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jschaf/esup.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "04lxmd0h7mfjjl0qghrycgff0vcv950j1wqv0dbkr61jxp64n5fv"))))
      ;; TODO: Add tests
      (build-system emacs-build-system)
      (home-page "https://github.com/jschaf/esup")
      (synopsis "Emacs start up profiler")
      (description "Benchmark Emacs Startup time without ever leaving
your Emacs.")
      (license license:gpl2+))))

(define-public emacs-sourcemap
  (package
    (name "emacs-sourcemap")
    (version "0.03")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/syohex/emacs-sourcemap.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "115g2mfpbfywp8xnag4gsb50klfvplqfh928a5mabb5s8v4a3582"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/syohex/emacs-sourcemap")
    (synopsis "Sourcemap parser")
    (description "Sourcemap parser")
    (license license:gpl3+)))

(define-public emacs-macrostep
  (let ((commit "424e3734a1ee526a1bd7b5c3cd1d3ef19d184267"))
    (package
      (name "emacs-macrostep")
      (version (git-version "0.9" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/joddie/macrostep.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1fm40mxdn289cyzgw992223dgrjmwxn4q8svyyxfaxjrpb38jhjz"))))
      (build-system emacs-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-before 'check 'remove-test
             ;; Fails because of requirement ‘/bin/sh’.
             (lambda _
               (let ((file "macrostep-test.el"))
                 (chmod file #o644)
                 (emacs-batch-edit-file file
                   `(progn (progn (goto-char (point-min))
                                  (re-search-forward
                                   "(ert-deftest macrostep-expand-c-macros")
                                  (beginning-of-line)
                                  (kill-sexp))
                           (basic-save-buffer))))))
           (add-before 'install 'check
             (lambda _
               (invoke "emacs" "--batch" "-L" "."
                       "-l" "macrostep-test.el"
                       "-f" "ert-run-tests-batch-and-exit"))))))
      (home-page "https://github.com/joddie/macrostep")
      (synopsis "Interactive macro-expander for Emacs")
      (description "@code{macrostep} is an Emacs minor mode for interactively
stepping through the expansion of macros in Emacs Lisp source code.  It lets
you see exactly what happens at each step of the expansion process by
pretty-printing the expanded forms inline in the source buffer, which is
temporarily read-only while macro expansions are visible.  You can expand and
collapse macro forms one step at a time, and evaluate or instrument the
expansions for debugging with Edebug as normal (but see “Bugs and known
limitations”, below).  Single-stepping through the expansion is particularly
useful for debugging macros that expand into another macro form.  These can be
difficult to debug with Emacs’ built-in macroexpand, which continues expansion
until the top-level form is no longer a macro call.")
      (license license:gpl3+))))

(define-public emacs-parent-mode
  (package
    (name "emacs-parent-mode")
    (version "2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Fanael/parent-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i5bc7lyyrx6swqlrp9l5x72yzwi53qn6ldrfs99gh08b3yvsnni"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/parent-mode")
    (synopsis "Get major mode's parent modes")
    (description "Get major mode's parent modes")
    (license license:gpl3+)))

(define-public emacs-lacarte
  (package
    (name "emacs-lacarte")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri "https://www.emacswiki.org/emacs/download/lacarte.el")
              (sha256
               (base32
                "1sbmk37ljq5j7dsw5c37sbxvlfgdqswh7bi4dknyjzfxlq50f4am"))))
    (build-system emacs-build-system)
    (home-page "https://www.emacswiki.org/emacs/lacarte.el")
    (synopsis "Execute menu items as commands, with completion")
    (description "Execute menu items as commands, with completion.")
    (license license:gpl3)))

(define-public emacs-isearch-prop
  (let ((commit "5e9dc44cf582d998aaa73ccd4d07cbda01248156")
        (revision "1"))
    (package
      (name "emacs-isearch-prop")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacsmirror/isearch-prop.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1xawppy705ybsgx566zvj8zk2aajq0sqvhvfs7nijqq32lx0a2zl"))))
      (build-system emacs-build-system)
      (home-page "https://www.emacswiki.org/emacs/isearch-prop.el")
      (synopsis "Extensions to @code{isearch.el}")
      (description "The Emacs library @code{isearch-prop.el} lets you search within
  contexts.  You can limit incremental search to a set of zones of
  buffer text, search contexts that in effect constitute a multi-region.
  These zones can be defined in various ways, including some ways
  provided specially by this library.")
      (license license:gpl3+))))

(define-public emacs-company-lua
  (let ((commit "0be8122f3adf57ad27953bf4b03545d6298d3da4"))
    (package
      (name "emacs-company-lua")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ptrv/company-lua.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1d9i165apgmwns7b2fd5wcpjpkah3dyj20v5sb8ynvz6qhhr5r9c"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-company" ,emacs-company)
         ("emacs-s" ,emacs-s)
         ("emacs-f" ,emacs-f)
         ("emacs-lua-mode" ,emacs-lua-mode)))
      (home-page "https://github.com/ptrv/company-lua")
      (synopsis "Company backend for Lua")
      (description
       "This package provides Company backend for Lua programming language.")
      (license license:gpl3+))))

(define-public emacs-beginend
  (package
    (name "emacs-beginend")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DamienCassou/beginend.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jbhg73g1rrkbwql5vi2b0ys9avfazmwzwgd90gkzwavw0ch9cvl"))))
    ;; TODO: Run tests.
    (build-system emacs-build-system)
    (inputs
     `(("emacs-undercover" ,emacs-undercover))) ; For tests.
    (home-page "https://github.com/DamienCassou/beginend")
    (synopsis "Redefine @code{M-<} and @code{M->} for Emacs modes")
    (description "@code{beginend} redefines @code{M-<} and @code{M->}
keybindings for Emacs modes so that point moves to meaningful
locations.  Redefined keys are still accessible by pressing the same
key again.")
    (license license:gpl3+)))

(define-public emacs-mbsync
  (let ((commit "42077e83ae2db778ce0f8e22f8357b40355526b3")
        (revision "1"))
    (package
      (name "emacs-mbsync")
      (version (string-append "0.0.1" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dimitri/mbsync-el.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0yj93y2mpxlir8x73znlg1slxlv4blm1vjv5h2w3j8lxg8bxvmn6"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/dimitri/mbsync-el")
      (synopsis "Interface to mbsync for Emacs")
      (description "This package allows to call the @code{mbsync} from
within Emacs.")
      (license license:gpl3+))))

(define-public emacs-ibuffer-projectile
  (let ((commit "c18ac540ee46cb759fc5df18747f6e8d23563011")
        (revision "1"))
    (package
      (name "emacs-ibuffer-projectile")
      (version (string-append "0.2" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/purcell/ibuffer-projectile.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1nd26cwwdpnwj0g4w393rd59klpyr6wqrnyr6scmwb5d06bsm44n"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-projectile" ,emacs-projectile)))
      (home-page "https://github.com/purcell/ibuffer-projectile")
      (synopsis "Group ibuffer's list by projectile root")
      (description "Adds functionality to Emacs @code{ibuffer} for
grouping buffers by their projectile root directory.")
      (license license:gpl3+))))

(define-public emacs-helm-mode-manager
  (package
    (name "emacs-helm-mode-manager")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/istib/helm-mode-manager/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0wllj321z16hgrx0ddwzk5wz4mnnx5am7w5nclqclfc5dfdn92wm"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-helm" ,emacs-helm)))
    (home-page "https://github.com/istib/helm-mode-manager/")
    (synopsis "Switch and toggle Emacs major and minor modes using Helm")
    (description "This package provides a Helm interface for toggling Emacs
major or minor mode.

@itemize
@item @code{helm-switch-major-mode} list of all major modes
@item @code{helm-enable-minor-mode} list of all inactive minor modes
@item @code{helm-disable-minor-mode} list of all ACTIVE minor modes
@end itemize\n

Hitting @code{RET} enables the mode, @code{C-z} shows the mode
documentation.")
    (license license:gpl3+)))

(define-public emacs-hy-mode
  (package
    (name "emacs-hy-mode")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hylang/hy-mode.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jxximiznz7fw9ys5k6plw85zrbzvxidql7py1fdi425fdp4058z"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-s" ,emacs-s)))
    (home-page "https://github.com/hylang/hy-mode")
    (synopsis "Major mode for Hylang")
    (description "This package provides a major mode for Hylang.")
    (license license:gpl3+)))

(define-public emacs-web-beautify
  (package
    (name "emacs-web-beautify")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yasuyk/web-beautify.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vms7zz3ym53wf1zdrkbf2ky2xjr1v134ngsd0jr8azyi8siw84d"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/yasuyk/web-beautify")
    (synopsis "Format HTML, CSS and JavaScript, JSON")
    (description "This package provides an Emacs functions to format HTML,
CSS, JavaScript, JSON.")
    (license license:gpl3+)))

(define-public emacs-helm-shell-history
  (let ((commit "110d3c35c52fe4b89b29e79ea4c8626bce7266a1"))
    (package
      (name "emacs-helm-shell-history")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yuutayamada/helm-shell-history.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "18fkjcz69g4dyaxhf9j8svr5x6dhsdnglddwisis8hdn504scpfj"))))
      (build-system emacs-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-before 'check 'patch-helm-shell-history-file
             (lambda _
               (let ((file "helm-shell-history.el"))
                 (chmod file #o644)
                 (emacs-substitute-sexps file
                   ("(defvar helm-shell-history-file"
                    `(expand-file-name "~/.bash_history"))))
               #t)))))
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)))
      (home-page "https://github.com/yuutayamada/helm-shell-history")
      (synopsis "Find shell history with Emacs Helm")
      (description "This package provides an Emacs Helm interface to search
throw a shell history.")
      (license license:gpl3+))))

(define-public emacs-discover-my-major
  (package
    (name "emacs-discover-my-major")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/steckerhalter/discover-my-major"
                       "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nah41f92rrl2l405kpqr6iaks11jyclgl4z7ilfymbr4ifmsiyl"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-makey" ,emacs-makey)))
    (home-page "https://github.com/steckerhalter/discover-my-major")
    (synopsis "Discover key bindings for the current Emacs major mode")
    (description "This package provides allows to discover key bindings and
their meaning for the current Emacs major-mode.")
    (license license:gpl3+)))

(define-public emacs-org-ref
  (let ((commit "8c9b5d7efb9f0c1ad5186b8203bdd017f4249129")
        (revision "1"))
    (package
      (name "emacs-org-ref")
      (version (string-append "1.1.1" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jkitchin/org-ref.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rxz0bjdsayk0slv23i07d9xhj2m7s4hsc81wc2d1cs52dkr5zmz"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-helm" ,emacs-helm)
         ("emacs-helm-bibtex" ,emacs-helm-bibtex)
         ("emacs-ivy" ,emacs-ivy)
         ("emacs-hydra" ,emacs-hydra)
         ("emacs-key-chord" ,emacs-key-chord)
         ("emacs-s" ,emacs-s)
         ("emacs-f" ,emacs-f)
         ("emacs-pdf-tools" ,emacs-pdf-tools)))
      (home-page "https://github.com/jkitchin/org-ref")
      (synopsis "Citations, cross-references and bibliographies in org-mode")
      (description
       "Lisp code to setup bibliography, cite, ref and label org-mode links.
Also sets up reftex and helm for org-mode citations.  The links are
clickable and do things that are useful.

The default setup uses helm-bibtex.

You should really read org-ref.org in this package for details.")
      (license license:gpl3+))))

;; This project is unmaintained.  Please use emacs-org-re-reveal instead.
(define-public emacs-org-reveal
  (let ((commit "1cdd088ec5fab631c564dca7f9f74fd3e9b7d4d4"))
    (package
      (name "emacs-org-reveal")
      (version (git-version "0.1" "2" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yjwen/org-reveal.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1vjxjadq2i74p96y9jxnqj1yb86fsgxzmn7bjgnb88ay6nvc1l72"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/yjwen/org-reveal")
      (synopsis "Org and Reveal.js powered HTML presentation tool")
      (description "Org-Reveal is a command@{org-mode} extension that allows
to create beautiful presentations (slides) with 3D effects from simple but
powerful Org contents.")
      (license license:gpl3+))))

(define-public emacs-org-re-reveal
  (package
    (name "emacs-org-re-reveal")
    (version "1.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/oer/org-re-reveal.git")
                    (commit "50cc6574c77f12d423f6cd096d8f76feb3673abc")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v3z30gpimg4spf6zzqwp9b597zxk89h0vpq6xp58js4rjg4ixk8"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-htmlize" ,emacs-htmlize)
       ("emacs-org" ,emacs-org)))
    (home-page "https://gitlab.com/oer/org-re-reveal")
    (synopsis "Build HTML presentations with reveal.js from Org source files")
    (description "This project started as fork of org-reveal.  It provides an
export back-end for HTML presentations with reveal.js from Org mode source
files.")
    (license license:gpl3+)))

(define-public emacs-add-hooks
  (package
    (name "emacs-add-hooks")
    (version "3.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nickmccurdy/add-hooks.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jv9fpcsm572zg0j1mbpbfkqgdlqapy89xhhj19pswkhjns1y2wl"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/nickmccurdy/add-hooks/")
    (synopsis "Emacs function for setting multiple hooks")
    (description "This package provides a @code{add-hooks} function tidies up
duplicate hook and function names further into a single declarative call.")
    (license license:gpl3+)))

(define-public emacs-fancy-narrow
  (package
    (name "emacs-fancy-narrow")
    (version "0.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Malabarba/fancy-narrow.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vcr1gnqawvc1yclqs23jvxm6bqix8kpflf1c7znb0wzxmz9kx7y"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Malabarba/fancy-narrow/releases")
    (synopsis "Imitate @code{narrow-to-region} with more eye candy")
    (description
     "Unlike @code{narrow-to-region}, which completely hides text outside
the narrowed region, this package simply de-emphasizes the text, makes it
read-only, and makes it unreachable.  This leads to a much more natural
feeling where the region stays static (instead of being brutally moved to a
blank slate) and is clearly highlighted with respect to the rest of the
buffer.")
    (license license:gpl2+)))

(define-public emacs-know-your-http-well
  (package
    (name "emacs-know-your-http-well")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/for-GET/know-your-http-well.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lppggnii2r9fvlhh33gbdrwb50za8lnalavlq9s86ngndn4n94k"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'install-json-files
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each (lambda (directory)
                         (copy-recursively directory
                                           (string-append
                                            (assoc-ref outputs "out")
                                            directory)))
                       '("js" "json"))))
         (add-after 'unpack 'chdir-elisp
           ;; Elisp directory is not in root of the source.
           (lambda _
             (chdir "emacs"))))))
    (build-system emacs-build-system)
    (home-page "https://github.com/for-GET/know-your-http-well")
    (synopsis "Meaning of HTTP headers codes")
    (description "Meaning of HTTP headers codes.")
    (license license:gpl3+)))

(define-public emacs-navi-mode
  (let ((commit "c1d38e8237f4e14af020a0b7d4f118ea198ab674"))
    (package
      (name "emacs-navi-mode")
      (version (git-version "2.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alphapapa/navi.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0jj5spk14hgb7zb1cd2n8whcw4k1kd5zb6llwj96v178yaws7l8k"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-outshine" ,emacs-outshine)
         ("emacs-outorg" ,emacs-outorg)))
      (home-page "https://github.com/alphapapa/navi")
      (synopsis "Emacs major-mode for easy buffer-navigation")
      (description
       "This package provides an Emacs major-mode for easy buffer-navigation")
      (license license:gpl3+))))

(define-public emacs-download-region
  (let ((commit "eb9e557529a73b4cfc8281c70dd0d95db333fffa")
        (revision "1"))
    (package
      (name "emacs-download-region")
      (version (string-append "0.0.1" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zk-phi/download-region.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0v52djg39b6k2snizd9x0qc009ws5y0ywqsfwhqgcbs5ymzh7dsc"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/zk-phi/download-region")
      (synopsis "In buffer download manager for Emacs")
      (description "@code{download-region} provides in buffer
downloading manager for Emacs.")
      (license license:gpl3+))))

(define-public emacs-helpful
  (package
    (name "emacs-helpful")
    (version "0.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Wilfred/helpful.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rqnx7672175288yqaslw0d9vw04j6psw7mys8j9zcp2i72hlvkn"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-elisp-refs" ,emacs-elisp-refs)))
    (home-page "https://github.com/Wilfred/helpful")
    (synopsis "More contextual information in Emacs help")
    (description "@code{helpful} is an alternative to the built-in Emacs help
that provides much more contextual information.

@itemize
@item Show the source code for interactively defined functions (unlike the
built-in Help).
@item Fall back to the raw sexp if no source is available.
@item Show where a function is being called.
@item Docstrings will Highlight the summary (the first sentence), include
cross-references, hide superfluous puncuation.
@item Show you the properties that have been applied to the current
symbol.  This provides visibility of features like edebug or byte-code
optimisation.
@item Provide a separate @code{helpful-command} function to view interactive
functions.
@item Display any keybindings that apply to interactive functions.
@item Trace, disassemble functions from inside Helpful.  This is discoverable
and doesn't require memorisation of commands.
@end itemize\n")
    (license license:gpl3+)))

(define-public emacs-logview
  (package
    (name "emacs-logview")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/doublep/logview.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qhzs75pxnaksbhczmpdcy2zmrqavlzkzss7ik5nv2wf9vs0sn02"))))
    (propagated-inputs
     `(("emacs-datetime" ,emacs-datetime)))
    (build-system emacs-build-system)
    (home-page "https://github.com/doublep/logview/")
    (synopsis "Emacs mode for viewing log files")
    (description "@code{logview} provides an Emacs mode to view log files.")
    (license license:gpl3+)))

(define-public emacs-suggest
  (package
    (name "emacs-suggest")
    (version "0.7")
    (home-page "https://github.com/Wilfred/suggest.el")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01v8plska5d3g19sb1m4ph1i3ayprfzk8mi6mpabjy6zad397xjl"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-loop" ,emacs-loop)
       ("emacs-dash" ,emacs-dash)
       ("emacs-s" ,emacs-s)
       ("emacs-f" ,emacs-f)
       ("emacs-spinner" ,emacs-spinner)
       ("emacs-shut-up" ,emacs-shut-up)))
    (synopsis "Suggest Elisp functions that give the output requested")
    (description "Suggest.el will find functions that give the output
requested.  It's a great way of exploring list, string and arithmetic
functions.")
    (license license:gpl3+)))

(define-public emacs-benchmark-init
  (package
    (name "emacs-benchmark-init")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dholm/benchmark-init-el.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kyn1izm5sbqbp9whnhk9dn3yc7zy8bz5san5w3ivi3rpx15fh94"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/dholm/benchmark-init-el")
    (synopsis "Benchmark Emacs @code{require} and @code{load} calls")
    (description "@code{benchmark-init} provides a way to keep track of where
time is being spent during Emacs startup in order to optimize startup time.")
    (license license:gpl3+)))

(define-public emacs-emms-player-simple-mpv
  ;; A new mpv backend is included in Emms from 5.0.
  (deprecated-package "emacs-emms-player-simple-mpv" emacs-emms))

(define-public emacs-magit-gerrit
  (let ((version "0.3")
        (revision "1")
        (commit "ece6f369694aca17f3ac166ed2801b432acfe20d"))
    (package
      (name "emacs-magit-gerrit")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/terranpro/magit-gerrit.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0mms0gxv9a3ns8lk5k2wjibm3088y1cmpr3axjdh6ppv7r5wdvii"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-magit" ,emacs-magit)))
      (home-page "https://github.com/terranpro/magit-gerrit")
      (synopsis "Magit extension for Gerrit")
      (description "This Magit extension provides integration with Gerrit,
which makes it possible to conduct Gerrit code reviews directly from within
Emacs.")
      (license license:gpl3+))))

(define-public emacs-magit-org-todos-el
  (let ((commit "df206287737b9671f2e36ae7b1474ebbe9940d2a"))
    (package
      (name "emacs-magit-org-todos-el")
      (version (git-version "0.1.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/danielma/magit-org-todos.el.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0kdp7k7jnnrkhsg0xh1c3h7iz0vgi120gf5xwl1hxy61avivnxrn"))))
      (propagated-inputs
       `(("magit" ,emacs-magit)))
      (build-system emacs-build-system)
      (home-page "https://github.com/danielma/magit-org-todos.el")
      (synopsis "Get todo.org into Emacs Magit status")
      (description "This package allows you to get @file{todo.org} into your
magit status.

If you have a @file{todo.org} file with @code{TODO} items in the root of your
repository, @code{magit-org-todos} will create a section in your Magit status
buffer with each of your todos.")
      (license license:gpl3+))))

(define-public emacs-f3
  (package
    (name "emacs-f3")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cosmicexplorer/f3.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qg48zbjdjqimw4516ymrsilz41zkib9321q0caf9474s9xyp2bi"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-helm" ,emacs-helm)))
    (home-page "https://github.com/cosmicexplorer/f3")
    (synopsis "Fantastic File Finder for Emacs")
    (description
     "The Fantastic File Finder for Emacs.  Find files fast, using helm.")
    (license license:gpl3+)))

(define-public emacs-lice-el
  (let ((commit "4339929927c62bd636f89bb39ea999d18d269250"))
    (package
      (name "emacs-lice-el")
      (version (git-version "0.2" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/buzztaiki/lice-el.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0879z761b7gajkhq176ps745xpdrivch349crransv8fnsc759yb"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/buzztaiki/lice-el")
      (synopsis "License and header template for Emacs")
      (description "@code{lice.el} provides following features:

@itemize
@item License template management.
@item File header insertion.
@end itemize\n")
      (license license:gpl3+))))

(define-public emacs-academic-phrases
  (let ((commit "0823ed8c24b26c32f909b896a469833ec4d7b656"))
    (package
      (name "emacs-academic-phrases")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nashamri/academic-phrases.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0qfzsq8jh05w4zkr0cvq3i1hdn97bq344vcqjg46sib26x3wpz6r"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-s" ,emacs-s)
         ("emacs-ht" ,emacs-ht)))
      (home-page "https://github.com/nashamri/academic-phrases")
      (synopsis "Bypass that mental block when writing your papers")
      (description
       "When writing your academic paper, you might get stuck trying to find
the right phrase that captures your intention.  This package tries to
alleviate that problem by presenting you with a list of phrases organized by
the topic or by the paper section that you are writing.  This package has
around 600 phrases so far.

Using this package is easy, just call @code{academic-phrases} to get a list of
phrases organized by topic, or call @code{academic-phrases-by-section} to
browse the phrases by the paper section and fill-in the blanks if required.")
      (license license:gpl3+))))

(define-public emacs-auto-yasnippet
  (let ((commit "d1ccfea87312c6dd8cf8501ab5b71b1d3d44d95b"))
    (package
      (name "emacs-auto-yasnippet")
      (version (git-version "0.3.0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/abo-abo/auto-yasnippet.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1i8k2qiyzd5rq0zplk4xb5nfa5mp0ibxbzwqj6c7877waq7244xk"))))
      (build-system emacs-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'check
             (lambda _
               (invoke "emacs" "--batch"
                       "-l" "auto-yasnippet.el"
                       "-l" "auto-yasnippet-test.el"
                       "-f" "ert-run-tests-batch-and-exit"))))))
      (propagated-inputs
       `(("emacs-yasnippet" ,emacs-yasnippet)))
      (home-page "https://github.com/abo-abo/auto-yasnippet/")
      (synopsis "Quickly create disposable yasnippets")
      (description "This package provides a hybrid of keyboard macros and
yasnippet.  You create the snippet on the go, usually to be used just in the
one place.  It's fast, because you're not leaving the current buffer, and all
you do is enter the code you'd enter anyway, just placing ~ where you'd like
yasnippet fields and mirrors to be.")
      (license license:gpl3+))))

(define-public emacs-highlight-numbers
  (package
    (name "emacs-highlight-numbers")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Fanael/highlight-numbers.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r07mpyr7rhd7bkg778hx6vbhb4n9ixgzkpszhgks7ri6ia38pj8"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-parent-mode" ,emacs-parent-mode)))
    (home-page "https://github.com/Fanael/highlight-numbers")
    (synopsis "Highlight numbers in source code")
    (description "@code{highlight-numbers-mode} provides a minor mode for
syntax highlighting of numeric literals in source code.

It s customizable: it's easy to add or redefine what exactly consitutes a
\"number\" in given major mode.  See @code{highlight-numbers-modelist}.")
    (license license:gpl3+)))

(define-public emacs-darkroom
  (package
    (name "emacs-darkroom")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/darkroom-"
                                  version ".el"))
              (sha256
               (base32
                "0fif8fm1h7x7g16949shfnaik5f5488clsvkf8bi5izpqp3vi6ak"))))
    (build-system emacs-build-system)
    (home-page "https://elpa.gnu.org/packages/darkroom.html")
    (synopsis "Remove visual distractions and focus on writing")
    (description "@code{darkroom-mode} makes visual distractions disappear.
The mode-line is temporarily elided, text is enlarged and margins are adjusted
so that it's centered on the window.

@code{darkroom-tentative-mode} is similar, but it doesn't immediately turn-on
@code{darkroom-mode}, unless the current buffer lives in the sole window of
the Emacs frame (i.e. all other windows are deleted).  Whenever the frame is
split to display more windows and more buffers, the buffer exits
@code{darkroom-mode}.  Whenever they are deleted, the buffer re-enters
@code{darkroom-mode}.")
    (license license:gpl3+)))

(define-public emacs-rsw-elisp
  (package
    (name "emacs-rsw-elisp")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rswgnu/rsw-elisp"
                                  "/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jnn7xfwl3wxc87v44ccsf1wwp80par3xgcvfb1icd6zchjmlcps"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/rswgnu/rsw-elisp")
    (synopsis "Improved expressions that interactively evaluate Emacs Lisp")
    (description "This package improves and replaces the GNU Emacs commands
that interactively evaluate Emacs Lisp expressions.  The new commands replace
standard key bindings and are all prefixed with @code{rsw-elisp-}.  They work
the same way as the old commands when called non-interactively; only the
interactive behavior should be different.")
    (license license:gpl3+)))

(define-public emacs-default-text-scale
  (let ((commit "968e985e219235f3e744d6d967e592acbaf6e0a8")
        (revision "1"))
    (package
      (name "emacs-default-text-scale")
      (version (string-append "0.1" "-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/purcell/default-text-scale")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0zds01c3q5yny6ab1fxfkzzgn1kgl3q23lxxap905f4qd70v922h"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/purcell/default-text-scale")
      (synopsis "Adjust the font size in all Emacs frames")
      (description "This package provides commands for increasing or
decreasing the default font size in all GUI Emacs frames.")
      (license license:gpl3+))))

(define-public emacs-visual-regexp
  (package
    (name "emacs-visual-regexp")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/benma/visual-regexp.el.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12zpmzwyp85dzsjpxd3279kpfi9yz3jwc1k9fnb3xv3pjiil5svg"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/benma/visual-regexp.el/")
    (synopsis "Regexp command with interactive visual feedback")
    (description "This package provides an Emacs regexp command with
interactive visual feedback.")
    (license license:gpl3+)))

(define-public emacs-faceup
  (let ((commit "6c92dad56a133e14e7b27831e1bcf9b3a71ff154")
        (revision "1"))
    (package
      (name "emacs-faceup")
      (version (string-append "0.0.1" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Lindydancer/faceup.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1yzmy7flrhrh0i10bdszx8idx6r8h6czm4vm4q0z6fp5fw94zwrx"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/Lindydancer/faceup")
      (synopsis "Markup language for faces and font-lock regression testing")
      (description "Emacs is capable of highlighting buffers based on
language-specific @code{font-lock} rules.  This package makes it possible to
perform regression test for packages that provide font-lock rules.")
      (license license:gpl3+))))

(define-public emacs-racket-mode
  (let ((commit "b977873e6128f8399432dcd60cc39f6a6f803d9c")
        (revision "2"))
    (package
      (name "emacs-racket-mode")
      (version (string-append "0.0.2" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/greghendershott/racket-mode")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0vp4bbbplqvmnhjpl6ajrlydmrhqzil56cfbs18m5c5fddx0zlh7"))))
      (build-system emacs-build-system)
      (arguments
       `(#:include '("\\.el$" "\\.rkt$")))
      (propagated-inputs
       `(("emacs-faceup" ,emacs-faceup)
         ("emacs-s" ,emacs-s)))
      (home-page "https://github.com/greghendershott/racket-mode")
      (synopsis "Major mode for Racket language")
      (description "@code{racket-mode} provides:

@itemize
@item Focus on Racket (not various Schemes).
@item Follow DrRacket concepts where applicable.
@item Thorough font-lock and indent.
@end itemize\n")
      (license license:gpl3+))))

(define-public emacs-grep-context
  (let ((commit "a17c57e66687a54e195e08afe776bdd60cb6c0a7"))
    (package
      (name "emacs-grep-context")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mkcms/grep-context.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1nqfa6kjzjshww4hnwg1c0vcr90bdjihy3kmixq3c3jkvxg99b62"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)))
      (home-page "https://github.com/nashamri/academic-phrases")
      (synopsis "Increase context in compilation and grep buffers")
      (description
       "This package provides an Emacs package for more context in
compilation/grep buffers.  Works with @code{wgrep}, @code{ack}, @code{ag},
@code{ivy}.")
      (license license:gpl3+))))

(define-public emacs-helm-firefox
  (let ((commit "0ad34b7b5abc485a86cae6920c14de861cbeb085")
        (revision "1"))
    (package
      (name "emacs-helm-firefox")
      (version (string-append "0.0.1" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-helm/helm-firefox.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "08mjsi2f9s29fkk35cj1rrparjnkm836qmbfdwdz7y51f9varjbs"))))
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)))
      (build-system emacs-build-system)
      (home-page "https://github.com/emacs-helm/helm-firefox")
      (synopsis "Display firefox bookmarks with Emacs Helm interface")
      (description "Display firefox bookmarks with Emacs Helm interface")
      (license license:gpl3+))))

(define-public emacs-interactive-align
  (package
    (name "emacs-interactive-align")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mkcms/interactive-align/"
                           "archive/" "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0sibpgb4lp6yy3pziak8f3hz4b28yj0dqy2nzh51z3d0b63h528m"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/mkcms/interactive-align/")
    (synopsis "Interactive align-regexp command in Emacs")
    (description "Interactive align-regexp command in Emacs")
    (license license:gpl3+)))

(define-public emacs-shift-number
  (package
    (name "emacs-shift-number")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/alezost/shift-number.el"
                           "/archive/" "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1g79m0hqn9jgpm565vvh8pdfzndc4vw7xisnh5qysj55qfg8cb1x"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/alezost/shift-number.el")
    (synopsis "Increase or decrease the number at point")
    (description "@code{emacs-shift-number} provides commands
@code{shift-number-up} to increase and @code{shift-number-down} to
decrease the number at point.")
    (license license:gpl3+)))

(define-public emacs-highlight-defined
  (package
    (name "emacs-highlight-defined")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Fanael/highlight-defined.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08czwa165rnd5z0dwwdddn7zi5w63sdk31l47bj0598kbly01n7r"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/highlight-defined")
    (synopsis "Syntax highlighting of known Elisp symbols")
    (description "Minor mode providing syntax highlighting of known Emacs Lisp
symbols.  Currently the code distinguishes Lisp functions, built-in functions,
macros, faces and variables.  To enable call @code{highlight-defined-mode}. ")
    (license license:gpl3+)))

(define-public emacs-parinfer-mode
  (package
    (name "emacs-parinfer-mode")
    (version "0.4.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DogLooksGood/parinfer-mode.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v97ncb0w1slb0x8861l3yr1kqz6fgw1fwl1z9lz6hh8p2ih34sk"))))
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-rainbow-delimiters" ,emacs-rainbow-delimiters)
       ("emacs-company" ,emacs-company)))
    (build-system emacs-build-system)
    (home-page "https://github.com/DogLooksGood/parinfer-mode/")
    (synopsis "Lisp structure editing mode")
    (description "@code{parinfer-mode} is a proof-of-concept editor
mode for Lisp programming languages.  It will infer some changes to
keep Parens and Indentation inline with one another.")
    (license license:gpl3+)))

(define-public emacs-helm-eww
  (package
    (name "emacs-helm-eww")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emacs-helm/helm-eww.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hpq1h0p69c9k6hkd2mjpprx213sc5475q7pr2zpmwrjdzgcv70z"))))
    (propagated-inputs
     `(("emacs-helm" ,emacs-helm)))
    (build-system emacs-build-system)
    (home-page "https://github.com/emacs-helm/helm-eww/")
    (synopsis "Helm interface to EWW")
    (description "This package provides a Helm interface for EWW buffers,
bookmarks and history.")
    (license license:gpl3+)))

(define-public emacs-stumpwm-mode
  (let ((commit "8fbe071d2c6c040794060a354eb377218dc10b35")
        (revision "1"))
    (package
      (name "emacs-stumpwm-mode")
      (version (string-append "0.0.1-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/stumpwm/stumpwm-contrib.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dfwsvz1c8w6j4jp0kzaz78ml3f5dp0a5pvf090kwpbpg176r7iq"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir-elisp
             ;; Elisp directory is not in root of the source.
             (lambda _
               (chdir "util/swm-emacs"))))))
      (home-page "https://github.com/stumpwm/stumpwm-contrib")
      (synopsis "Emacs minor-mode for Stumpwm")
      (description "Emacs minor-mode for Stumpwm")
      (license license:gpl3+))))

(define-public emacs-irfc
  (package
    (name "emacs-irfc")
    (version "20130824.507")
    (source
     (origin
       (method url-fetch)
       (uri "https://www.emacswiki.org/emacs/download/irfc.el")
       (file-name (string-append "irfc-" version ".el"))
       (sha256
        (base32
         "197ybqwbj8qjh2p9pkf5mvqnrkpcgmv8c5s2gvl6msyrabk0mnca"))))
    (build-system emacs-build-system)
    (home-page "https://www.emacswiki.org/emacs/download/irfc.el")
    (synopsis "Interface for IETF RFC document")
    (description
     "This package provides an Emacs interface for IETF RFC document.")
    (license license:gpl3+)))

(define-public emacs-ido-vertical-mode
  (package
    (name "emacs-ido-vertical-mode")
    (version "0.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/creichert/ido-vertical-mode.el.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lv82q639xjnmvby56nwqn23ijh6f163bk675s33dkingm8csj8k"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/creichert/ido-vertical-mode.el")
    (synopsis "Makes ido-mode display vertically")
    (description "Makes ido-mode display prospects vertically.")
    (license license:gpl3+)))

(define-public emacs-wordgen
  (package
    (name "emacs-wordgen")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Fanael/wordgen.el.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06vbc9ycz1nbjwjkg99y3lj6jwb6lnwnmkqf09yr00jjrrfhfash"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/wordgen.el")
    (synopsis "Random word generator")
    (description "This package provides functions to generate random words
using user-provided rules.")
    (license license:gpl3+)))

(define-public emacs-on-screen
  (package
    (name "emacs-on-screen")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://elpa.gnu.org/packages/on-screen-" version ".el"))
       (file-name (string-append name "-" version ".el"))
       (sha256
        (base32
         "15d18mjgv1pnwl6kf3pr5w64q1322p1l1qlfvnckglwmzy5sl2qv"))))
    (build-system emacs-build-system)
    (home-page
     "https://github.com/michael-heerdegen/on-screen.el")
    (synopsis "Guide your eyes while scrolling")
    (description
     "Scrolling can be distracting because your eyes may lose
orientation.  This library implements a minor mode that highlights
the previously visible buffer part after each scroll.")
    (license license:gpl3+)))

(define-public emacs-highlight-escape-sequences
  (let ((commit "08d846a7aa748209d65fecead2b6a766c3e5cb41")
        (revision "1"))
    (package
      (name "emacs-highlight-escape-sequences")
      (version (string-append "0.0.1" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dgutov/highlight-escape-sequences.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "05mc3w1f8ykf80914a1yddw6j8cmh0h57llm07xh89s53821v2is"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/dgutov/highlight-escape-sequences")
      (synopsis "Highlight escape sequences in Emacs")
      (description "@code{highlight-escape-sequences} provides an
Emacs minor mode to escape sequences in code.")
      (license license:gpl3+))))

(define-public emacs-dashboard
  (package
    (name "emacs-dashboard")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rakanalh/emacs-dashboard.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hhh1kfsz87qfmh45wjf2r93rz79rq0vbyxlfrsl02092zjbl1zr"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-page-break-lines" ,emacs-page-break-lines)))
    (arguments '(#:include '("\\.el$" "\\.txt$" "\\.png$")))
    (home-page "https://github.com/rakanalh/emacs-dashboard")
    (synopsis "Startup screen extracted from Spacemacs")
    (description "This package provides an extensible Emacs dashboard, with
sections for bookmarks, projectil projects, org-agenda and more. ")
    (license license:gpl3+)))

(define-public emacs-slime-company
  (package
    (name "emacs-slime-company")
    (version "1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anwyn/slime-company.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hl1hqkc1pxga9k2k8k15d7dip7sfsmwf4wm4sh346m6nj606q8g"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-slime" ,emacs-slime)
       ("emacs-company" ,emacs-company)))
    (home-page "https://company-mode.github.io")
    (synopsis "SLIME completion backend for @code{company-mode}")
    (description
     "This is a backend implementation for the completion package
@code{company-mode} which supports the normal and the fuzzy completion
modes of SLIME.")
    (license license:gpl3+)))

(define-public emacs-sml-mode
  (package
    (name "emacs-sml-mode")
    (version "6.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://elpa.gnu.org/packages/sml-mode-"
                           version ".el"))
       (sha256
        (base32
         "105fcrz5qp95f2n3fdm3awr6z58sbrjihjss6qnrg4lz2ggbc328"))))
    (build-system emacs-build-system)
    (home-page "http://elpa.gnu.org/packages/sml-mode.html")
    (synopsis "Major mode for editing (Standard) ML")
    (description "SML-MODE is a major Emacs mode for editing Standard ML.
It provides syntax highlighting and automatic indentation and
comes with sml-proc which allows interaction with an inferior SML
interactive loop.")
    (license license:gpl3+)))

(define-public emacs-eros
  (let ((commit "a42e45c9b2397156c684330b0fc90ee0eba773f5")
        (revision "1"))
    (package
      (name "emacs-eros")
      (version (string-append "0.0.1" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/xiongtx/eros.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0whlsq90v13fz69k3wjrwcwb9gkpfxqjd75mg3nrp85j9nwhb5i4"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/xiongtx/eros")
      (synopsis "Evaluation result overlays")
      (description "@code{eros} provides evaluation result overlays.")
      (license license:gpl3+))))

(define-public emacs-stickyfunc-enhance
  (let ((commit "13bdba51fcd83ccbc3267959d23afc94d458dcb0")
        (revision "1"))
    (package
      (name "emacs-stickyfunc-enhance")
      (version "0.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tuhdo/semantic-stickyfunc-enhance.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "16dxjsr5nj20blww4xpd4jzgjprzzh1nwvb810ggdmp9paf4iy0g"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/tuhdo/semantic-stickyfunc-enhance")
      (synopsis "Enhancement to stock @code{semantic-stickyfunc-mode}")
      (description
       "@code{semantic-stickyfunc-mode} shows the function point is currently
in at the first line of the current buffer.  This is useful when you have a
very long function that spreads more than a screen, and you don't have to
scroll up to read the function name and then scroll down to original position.")
      (license license:gpl3+))))

(define-public emacs-git-auto-commit-mode
  (package
    (name "emacs-git-auto-commit-mode")
    (version "4.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ryuslash/git-auto-commit-mode.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0psmr7749nzxln4b500sl3vrf24x3qijp12ir0i5z4x25k72hrlh"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ryuslash/git-auto-commit-mode")
    (synopsis "Emacs Minor mode to automatically commit and push")
    (description "@code{git-auto-commit-mode} is an Emacs minor mode that
tries to commit changes to a file after every save.

When @code{gac-automatically-push-p} is non-nil, it also tries to push to
the current upstream.")
    (license license:gpl3+)))

(define-public emacs-company-restclient
  (package
    (name "emacs-company-restclient")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/iquiw/company-restclient.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i1fh5lvqwlgn3g3fzh0xacxyljx6gkryipn133vfkv4jbns51n4"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-company" ,emacs-company)
       ("emacs-know-your-http-well" ,emacs-know-your-http-well)
       ("emacs-restclient" ,emacs-restclient)))
    (home-page "https://github.com/iquiw/company-restclient")
    (synopsis "Company-mode completion back-end for restclient-mode")
    (description "@code{company-mode} back-end for
@code{restclient-mode}.

It provides auto-completion for HTTP methods and headers in
@code{restclient-mode}.  Completion source is given by
@code{know-your-http-well}.")
    (license license:gpl3+)))

(define-public emacs-noflet
  (let ((version "20170629")
        (revision "1")
        (commit "7ae84dc3257637af7334101456dafe1759c6b68a"))
    (package
      (name "emacs-noflet")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nicferrier/emacs-noflet")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0g70gnmfi8n24jzfci9nrj0n9bn1qig7b8f9f325rin8h7x32ypf"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'require-dash
             ;; noflet.el uses -map from dash, but (require 'dash) is
             ;; missing. So, add it.
             (lambda _
               (substitute* "noflet.el"
                 ((";;; Code:") ";;; Code:\n(require 'dash)"))
               #t)))))
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)))
      (home-page "https://github.com/nicferrier/emacs-noflet")
      (synopsis "Locally override functions")
      (description "@code{emacs-noflet} let's you locally override functions,
in the manner of @command{flet}, but with access to the original function
through the symbol: @command{this-fn}.")
      (license license:gpl3+))))

(define-public emacs-dumb-jump
  (package
    (name "emacs-dumb-jump")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jacktasia/dumb-jump.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00ph85vp8sa3k99qrdxfz4l8zx121q9xf47vvspzg26bk9l4nwin"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #f ; FIXME: Tests freeze when run.
       #:test-command '("ert-runner")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-shell
           (lambda _
             ;; Setting the SHELL environment variable is required for the
             ;; tests to find sh.
             (setenv "SHELL" (which "sh"))
             #t)))))
    (native-inputs
     `(("emacs-el-mock" ,emacs-el-mock)
       ("emacs-noflet" ,emacs-noflet)
       ("emacs-undercover" ,emacs-undercover)
       ("ert-runner" ,emacs-ert-runner)))
    (propagated-inputs
     `(("emacs-f" ,emacs-f)
       ("emacs-popup" ,emacs-popup)))
    (home-page "https://github.com/jacktasia/dumb-jump")
    (synopsis "Jump to definition for multiple languages without configuration")
    (description "Dumb Jump is an Emacs \"jump to definition\" package with
support for multiple programming languages that favors \"just working\" over
speed or accuracy.  This means minimal --- and ideally zero --- configuration
with absolutely no stored indexes (tags) or persistent background processes.
Dumb Jump performs best with The Silver Searcher @command{ag} or ripgrep
@command{rg} installed.")
    (license license:gpl3+)))

(define-public emacs-dts-mode
  (let ((commit "9ee0854446dcc6c53d2b8d2941051768dba50344")
        (revision "1"))
    (package
      (name "emacs-dts-mode")
      (version (string-append "0.1.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/bgamari/dts-mode.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1k8lljdbc90nd29xrhdrsscxavzdq532wq2mg7ljc94krj7538b1"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/bgamari/dts-mode.git")
      (synopsis "Emacs minor mode for editing device tree files")
      (description
       "This package provides an Emacs minor mode for highlighting
device tree files.")
      (license license:gpl3+))))

(define-public emacs-daemons
  (package
    (name "emacs-daemons")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cbowdon/daemons.el")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00bkzfaw3bqykcks610vk9wlpa2z360xn32bpsrycacwfv29j7g4"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/cbowdon/daemons.el")
    (synopsis "Emacs UI for managing init system services")
    (description
     "This is an Emacs mode to give you a UI for managing init system
daemons (services) for those getting tired of typing out @code{sudo service
my_thing reload} all the time.  It offers a consistent UI over different init
systems.")
    (license license:gpl3+)))

(define-public emacs-isearch+
  (let ((commit "95e49af9dbf0254e095a11f115e101b52659520a")
        (revision "1"))
    (package
      (name "emacs-isearch+")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacsmirror/isearch-plus.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xhd9zgknf4lvzyf4apirpd7spb1hbpzkvys00a7pkmd0vvahk0v"))))
      (build-system emacs-build-system)
      (home-page "https://www.emacswiki.org/emacs/isearch+.el")
      (synopsis "Extensions to @code{isearch.el}")
      (description "This package extends @code{isearch} with advice, dynamic
filters, highlighting of regexp group levels, and more.")
      (license license:gpl2+))))

(define-public emacs-eshell-bookmark
  (package
    (name "emacs-eshell-bookmark")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Fuco1/eshell-bookmark")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "14dmsnixf9vqdhsixw693sml0fn80zcf0b37z049fb40cmppqxdw"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fuco1/eshell-bookmark")
    (synopsis "Provide @file{bookmark.el} integration for @code{eshell}")
    (description
     "This package allows for bookmarking @code{eshell} buffers.  Upon
visiting the bookmark, a new @code{eshell} session will be opened in the
appropriate directory if no @code{eshell} session is active.")
    (license license:gpl3+)))

(define-public emacs-eshell-z
  (package
    (name "emacs-eshell-z")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xuchunyang/eshell-z")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1aac4m814jgxwpz7lbyx5r4z5dmawp4sk7pwbx0zqpnbcsaq5wwc"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/xuchunyang/eshell-z")
    (synopsis "Quick navigation to frequently visited directories")
    (description
     "This package is a port of @code{z}, and keeps track of visited directories
and commands invoked within them in order to enable navigation via input of
matching regexps.")
    (license license:gpl3+)))

(define-public emacs-esh-autosuggest
  (package
    (name "emacs-esh-autosuggest")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dieggsy/esh-autosuggest")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "116pdjgpjy9b0psm5kzwkwy7dq8vn0p6dy75dl1zsy2xrjf1iqdw"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-company" ,emacs-company)))
    (home-page "https://github.com/dieggsy/esh-autosuggest")
    (synopsis "Fish-like autosuggestions in Eshell")
    (description
     "This package assumes you use something other than company for eshell
completion (e.g. @code{eshell-pcomplete}, @code{completion-at-point},
@code{helm-esh-pcomplete}).  @code{company-mode} is used solely as a mechanism
for history autosuggestions.

Unless you're using @code{use-package}'s hook keyword, you can enable the
autosuggestions with:
@code{(add-hook 'eshell-mode-hook #'esh-autosuggest-mode)}")
    (license license:gpl3+)))

(define-public emacs-desktop-environment
  (let ((commit "a9eba14f2b5b8070f78dca839ca8259736e346a6"))
    (package
      (name "emacs-desktop-environment")
      (version (git-version "0.2.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.petton.fr/DamienCassou/desktop-environment")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0x73x5hy7w55jrzy3xvqhk90rrsznmxjqvsvwhh21qznv5w269xz"))))
      (build-system emacs-build-system)
      (home-page "https://gitlab.petton.fr/DamienCassou/desktop-environment")
      (synopsis "Control your GNU/Linux desktop environment from Emacs")
      (description
       "This package helps you control your GNU/Linux desktop from Emacs.
With @code{desktop-environment}, you can control the brightness and volume as
well as take screenshots and lock your screen.  The package depends on the
availability of shell commands to do the hard work for us.  These commands can
be changed by customizing the appropriate variables.")
      (license license:gpl3+))))

(define-public emacs-org-caldav
  (package
    (name "emacs-org-caldav")
    (version "20180403")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/dengste/org-caldav/raw/"
             "8d3492c27a09f437d2d94f2736c56d7652e87aa0"
             "/org-caldav.el"))
       (sha256
        (base32
         "1fh4gh68ddj0is99z2ccyh97v6psnyda61n2dsadzqhcxn51amlc"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-org" ,emacs-org)))
    (home-page "https://github.com/dengste/org-caldav")
    (synopsis
     "Sync Org files with external calendars via the CalDAV protocol")
    (description
     "Synchronize between events in Org-mode files and a CalDAV calendar.
This code is still alpha.")
    (license license:gpl3+)))

(define-public emacs-zotxt
  (package
    (name "emacs-zotxt")
    (version "20180518")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/egh/zotxt-emacs.git")
             (commit "23a4a9f74a658222027d53a9a83cd4bcc583ca8b")))
       (sha256
        (base32 "09fq3w9yk9kn6bz7y9kgpiw612dvj3yzsdk734js6zgb0p8lfd2c"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-deferred" ,emacs-deferred)
       ("emacs-request" ,emacs-request)))
    (home-page "https://github.com/egh/zotxt-emacs")
    (synopsis "Integrate Emacs with Zotero")
    (description "This package provides two integration features between Emacs
and the Zotero research assistant: Insertion of links to Zotero items into an
Org-mode file, and citations of Zotero items in Pandoc Markdown files.")
    (license license:gpl3+)))

(define-public emacs-evil-ediff
  ;; Evil-Ediff is included in Evil Collection from 20180617.
  (deprecated-package "emacs-evil-ediff" emacs-evil-collection))

(define-public emacs-evil-magit
  (let ((commit "e2fec5877994c0c19f0c25fa01f3d22cb0ab38ba"))
    (package
      (name "emacs-evil-magit")
      (version (git-version "0.4.2" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-evil/evil-magit")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "134v7s03jvbhm70mavwab85r09i68g2a5bvriirh0chz1av2y16v"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-evil" ,emacs-evil)
         ("magit" ,emacs-magit)))
      (home-page
       "https://github.com/emacs-evil/evil-magit")
      (synopsis "Evil-based key bindings for Magit")
      (description
       "This Emacs library configures Magit and Evil to play well with each other.
For some background see @url{https://github.com/magit/evil-magit/issues/1}.
See the README at @url{https://github.com/justbur/evil-magit} for a table
describing the key binding changes.")
      (license license:gpl3+))))

(define-public emacs-evil-mu4e
  ;; Evil-mu4e is included in Evil Collection from 20180617.
  (deprecated-package "emacs-evil-mu4e" emacs-evil-collection))

(define-public emacs-evil-multiedit
  (package
    (name "emacs-evil-multiedit")
    (version "1.3.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hlissner/evil-multiedit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "19h3kqylqzbjv4297wkzzxdmn9yxbg6z4ga4ssrqri90xs7m3rw3"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-evil" ,emacs-evil)
       ("emacs-iedit" ,emacs-iedit)))
    (home-page
     "https://github.com/hlissner/evil-multiedit")
    (synopsis "Multiple cursors for Evil mode")
    (description
     "This plugin was an answer to the lack of proper multiple cursor support
in Emacs+Evil.  It allows you to select and edit matches interactively,
integrating @code{iedit-mode} into Evil mode with an attempt at sensible
defaults.")
    (license license:gpl3+)))

(define-public emacs-evil-org
  (let ((commit "b6d652a9163d3430a9e0933a554bdbee5244bbf6"))
    (package
      (name "emacs-evil-org")
      (version (git-version "0.1.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Somelauw/evil-org-mode")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "176hrw7y7nczffbyhsa167b8rvfacsmcafm2gpkrdjqlrikbmrhl"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-evil" ,emacs-evil)))
      (home-page
       "https://github.com/Somelauw/evil-org-mode")
      (synopsis "Evil keybindings for Org mode")
      (description
       "This package adds supplemental Evil mode key-bindings to Emacs
Org-mode.  It features:
@itemize
@item normal, visual and insert mode bindings;
@item key bindings organised in key themes;
@item operators like > and < to work on headings;
@item text objects;
@item table support;
@item calendar (date selection) support;
@item agenda support.
@end itemize\n")
      (license license:gpl3+))))

(define-public emacs-debpaste
  (package
    (name "emacs-debpaste")
    (version "0.1.5")
    (home-page "https://github.com/alezost/debpaste.el")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page)
                           (commit (string-append "v" version))))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wi70r56pd5z0x4dp4m58p9asq03j74kdm4fi9vai83vsl2z9amq"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-xml-rpc" ,emacs-xml-rpc)))
    (synopsis "Manipulate pastes from the Debian Pastezone")
    (description "Debpaste is an Emacs interface for the Debian Pastezone,
allowing you to receive, post, and delete pastes.  It communicates with the
server using XML-RPC.")
    (license license:gpl3+)))

(define-public emacs-xml-rpc
  (let ((commit "8f624f8b964e9145acb504e4457c9510e87dd93c")
        (revision "1"))
    (package
      (name "emacs-xml-rpc")
      (version (git-version "1.6.12" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hexmode/xml-rpc-el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xa54z52rsfl3n0xgmbycj4zazp8ksgdwcq56swzs6wp72zlalmj"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/hexmode/xml-rpc-el")
      (synopsis "XML-RPC client for Emacs")
      (description "This package provides an XML-RPC client for Emacs capable
of both synchronous and asynchronous method calls using the @code{url}
package's async retrieval functionality.  @file{xml-rpc.el} represents XML-RPC
datatypes as Lisp values, automatically converting to and from the XML
datastructures as needed, both for method parameters and return values, making
using XML-RPC methods fairly transparent to the Lisp code.")
      (license license:gpl3+))))

(define-public emacs-fish-completion
  (package
    (name "emacs-fish-completion")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gitlab.com/Ambrevar/emacs-fish-completion/repository/"
             "archive.tar.gz?ref="
             version))
       (sha256
        (base32
         "1hpma1c5j50ja03ibr7h1xmyv7k8j3rbvqivad47kwqhlsgw0jk0"))))
    (build-system emacs-build-system)
    (inputs `(("fish" ,fish)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((fish (assoc-ref inputs "fish")))
               ;; Specify the absolute file names of the various
               ;; programs so that everything works out-of-the-box.
               (emacs-substitute-variables
                   "fish-completion.el"
                 ("fish-completion-command"
                  (string-append fish "/bin/fish")))))))))
    (home-page
     "https://gitlab.com/Ambrevar/emacs-fish-completion")
    (synopsis "Fish completion for Emacs pcomplete")
    (description
     "This package provides completion for the Fish shell to pcomplete (used
by shell and Eshell).  You can set it up globally with:

@example
(when (and (executable-find \"fish\")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))
@end example

Alternatively, you can call the @code{fish-completion-mode} manually or in
shell/Eshell mode hook.

The package @code{emacs-bash-completion} is an optional dependency: if available,
@code{fish-completion-complete} can be configured to fall back on bash to further
try completing.  See @code{fish-completion-fallback-on-bash-p}.")
    (license license:gpl3+)))

(define-public emacs-gif-screencast
  (let ((commit "12b25442b97b84abae74ecb5190a9d14ff7cfe5a"))
    (package
      (name "emacs-gif-screencast")
      (version (git-version "20180616" "1" commit))
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://gitlab.com/Ambrevar/emacs-gif-screencast/"
               "repository/archive.tar.gz?ref="
               commit))
         (sha256
          (base32
           "0lc457i78xjkn5idr2aaiadkh76zcsksj84z0qh80a9y775syrgh"))))
      (build-system emacs-build-system)
      (inputs
       `(("scrot" ,scrot)
         ("imagemagick" ,imagemagick)
         ("gifsicle" ,gifsicle)))
     (arguments
      `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((scrot (assoc-ref inputs "scrot"))
                   (imagemagick (assoc-ref inputs "imagemagick"))
                   (gifsicle (assoc-ref inputs "gifsicle")))
               ;; Specify the absolute file names of the various
               ;; programs so that everything works out-of-the-box.
               (emacs-substitute-variables
                   "gif-screencast.el"
                 ("gif-screencast-program"
                  (string-append scrot "/bin/scrot"))
                 ("gif-screencast-convert-program"
                  (string-append imagemagick "/bin/convert"))
                 ("gif-screencast-cropping-program"
                  (string-append imagemagick "/bin/mogrify"))
                 ("gif-screencast-optimize-program"
                  (string-append imagemagick "/bin/gifsicle")))))))))
      (home-page
       "https://gitlab.com/Ambrevar/emacs-gif-screencast")
      (synopsis "One-frame-per-action GIF recording")
      (description
       "Call @code{gif-screencast} to start a recording.
A screenshot is taken for every user action.  Call
@code{gif-screencast-stop} (<f9> by default) to finish recording and create
the GIF result.")
      (license license:gpl3+))))

(define-public emacs-google-translate
  (package
    (name "emacs-google-translate")
    (version "0.11.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/atykhonov/google-translate/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01n9spj1d0gjfj39x526rl3m9c28wnx9afipmf5s8y77cx3mfwhl"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/atykhonov/google-translate")
    (synopsis "Emacs interface to Google Translate")
    (description
     "This packages provides an Emacs interface to the Google Translate
on-line service.")
    (license license:gpl3+)))

(define-public emacs-helm-company
  (let ((commit "d3fc093a0e833b4dee6561c00d6df3d62aa50f3f"))
    (package
      (name "emacs-helm-company")
      (version (git-version "0.2.3" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Sodel-the-Vociferous/helm-company")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1ciirsanhajdqm5iwl8k9ywf4jha1wdv4sc4d9kslyrfr9zn4q6k"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)
         ("emacs-company" ,emacs-company)))
      (home-page "https://github.com/Sodel-the-Vociferous/helm-company")
      (synopsis "Helm interface for company-mode")
      (description
       "This is a Helm interface to company-mode, a text completion
framework.")
      (license license:gpl3+))))

(define-public emacs-helm-descbinds
  (let ((commit "033be73f21778633813264ce1634a6e1ad873d8e"))
    (package
      (name "emacs-helm-descbinds")
      (version (git-version "1.13" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-helm/helm-descbinds")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1n89p56qwa243w1c85i5awnaf7piwjsvfi7nmnsrwm33hix5dknk"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-helm" ,emacs-helm)))
      (home-page "https://github.com/emacs-helm/helm-descbinds")
      (synopsis "Convenient @code{describe-bindings} with Helm")
      (description
       "This package is a replacement of @code{describe-bindings} for Helm.
@code{describe-bindings} is replaced with @code{helm-descbinds}.  As usual,
type @code{C-h b}, or any incomplete key sequence plus @code{C-h}, to run
@code{helm-descbinds}.  The bindings are presented in a similar way as
@code{describe-bindings} does, but you can use completion to find the command
you searched for and execute it, or view its documentation.")
    (license license:gpl3+))))

(define-public emacs-helm-emms
  (let ((commit "b785cb845a98a643eba9d5d53c9c0b4e6810a3cd"))
    (package
      (name "emacs-helm-emms")
      (version (git-version "1.3" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-helm/helm-emms")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1595r09y3rmwd46nnhvjja3hb8j2ila295ijxv61cg52ws4wginh"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)
         ("emacs-emms" ,emacs-emms)))
      (home-page
       "https://github.com/emacs-helm/helm-emms")
      (synopsis "Emms for Helm")
      (description "Helm interface for Emms to browse all tracks and all folders
from @code{emms-source-file-default-directory}.")
      (license license:gpl3+))))

(define-public emacs-helm-exwm
  (let ((commit "56266f261ba3b3d2753b374b50da20eb768c06f5"))
    (package
      (name "emacs-helm-exwm")
      (version (git-version "20180703" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-helm/helm-exwm.git")
               (commit commit)))
         (sha256
          (base32 "064ziinqa5sdv7rfjn0y278l12kld176fr88k4h78pgf2f2n7cd8"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)
         ("emacs-exwm" ,emacs-exwm)))
      (home-page
       "https://github.com/emacs-helm/helm-exwm")
      (synopsis "Helm for EXWM buffers")
      (description
       "@code{helm-exwm} runs a Helm session over the list of EXWM buffers.
@code{helm-exwm-switch} is a convenience X application launcher using Helm to
switch between the various windows of one or several specific applications.
See @code{helm-exwm-switch-browser} for an example.")
      (license license:gpl3+))))

(define-public emacs-helm-flycheck
  (let ((commit "3cf7d3bb194acacc6395f88360588013d92675d6"))
    (package
      (name "emacs-helm-flycheck")
      (version (git-version "0.4" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yasuyk/helm-flycheck")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1a2yfxhz04klwhcandqvfzysxp6b7bxyxx1xk1x3kds5hif5fkl4"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-flycheck" ,emacs-flycheck)
         ("emacs-helm" ,emacs-helm)))
      (home-page "https://github.com/yasuyk/helm-flycheck")
      (synopsis "Show Flycheck errors with Helm")
      (description
       "This integrates Flycheck with Helm.")
      (license license:gpl3+))))

(define-public emacs-helm-ls-git
  (let ((commit "76654c776a7f6e2e5290645e748aac2a746f7daa"))
    (package
      (name "emacs-helm-ls-git")
      (version (git-version "1.9.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-helm/helm-ls-git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0vsq1n3xl3ghy5zik2scm7jrs501n4kybdqd6yw6j0cv4jxdqbr0"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-helm" ,emacs-helm)))
      (home-page "https://github.com/emacs-helm/helm-ls-git")
      (synopsis "Helm interface for listing the files in a Git repository")
      (description
       "This package provides a Helm interface for Git files.
@itemize
@item Display the open buffers in project.
@item Display a status source showing state of project (modified files etc.).
@item Display a list of all files in project under git control.
@item Quickly look at diffs of modified files.
@item Allow switching to @code{git status} with your preferred frontend
(vc-dir, Magit,etc.).
@item Full integration of git-grep, also allow usage of @code{helm-grep} (you
can use ack-grep instead of grep).
@item Integrate usage of gid from id-utils.
@item Full integration with @code{helm-find-files}, allow you to browse
projects unrelated to current-buffer.
@item In addition, all actions of type files and buffers are provided.
@end itemize\n")
      (license license:gpl3+))))

(define-public emacs-helm-mu
  (let ((commit "77e6fea24e01481418738421dbcfe28ef1bd63cf"))
    (package
      (name "emacs-helm-mu")
      (version (git-version  "20180513" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-helm/helm-mu.git")
               (commit commit)))
         (sha256
          (base32 "1lh0ahxdc5b2z18m9p30gwg8sbg33sjwkjr38p7h5xsm5fm7i0fz"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)
         ("mu" ,mu)))
      (home-page
       "https://github.com/emacs-helm/helm-mu")
      (synopsis
       "Helm sources for searching emails and contacts")
      (description
       "Helm sources for searching emails and contacts using @code{mu} and
@code{mu4e}.  Mu is an indexer for maildirs and mu4e is a mutt-like MUA for
Emacs build on top of mu.  Mu is highly efficient making it possible to get
instant results even for huge maildirs.  It also provides search operators,
e.g: @code{from:Peter to:Anne flag:attach search term}.")
      (license license:gpl3+))))

(define-public emacs-helm-pass
  (package
    (name "emacs-helm-pass")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-helm/helm-pass")
             (commit version)))
       (sha256
        (base32
         "0vglaknmir3yv4iwibwn8r40ran8d04gcyp99hx73ldmf3zqpnxv"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-helm" ,emacs-helm)
       ("emacs-password-store" ,emacs-password-store)))
    (home-page "https://github.com/emacs-helm/helm-pass")
    (synopsis "Helm interface to pass, the standard Unix password manager")
    (description
     "Users of @code{helm-pass} may also be interested in functionality
provided by other Emacs packages dealing with pass:
@itemize
@item @code{emacs-password-store}, which @code{helm-pass} relies on.
@item @code{emacs-pass}, a major mode for @code{pass}.
@item @code{auth-source-pass.el}: integration of Emacs' auth-source with
@code{pass}, included in Emacs 26+).
@end itemize\n")
    (license license:gpl3+)))

(define-public emacs-image+
  (let ((commit "6834d0c09bb4df9ecc0d7a559bd7827fed48fffc"))
    (package
      (name "emacs-image+")
      (version (git-version "0.6.2" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mhayashi1120/Emacs-imagex")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0v66wk9nh0raih4jhrzmmyi5lbysjnmbv791vm2230ffi2hmwxnd"))))
      (build-system emacs-build-system)
      (inputs `(("imagemagick" ,imagemagick)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((imagemagick (assoc-ref inputs "imagemagick")))
                 ;; Specify the absolute file names of the various
                 ;; programs so that everything works out-of-the-box.
                 (chmod "image+.el" #o666)
                 (emacs-substitute-variables
                     "image+.el"
                   ("imagex-convert-command"
                    (string-append imagemagick "/bin/convert"))
                   ("imagex-identify-command"
                    (string-append imagemagick "/bin/identify")))))))))
      (home-page "https://github.com/mhayashi1120/Emacs-imagex")
      (synopsis "Image manipulation extensions for Emacs")
      (description
       "Image+ provides keybindings allowing you to zoom in or zoom out of an
image, rotate it, save modified images, and more.")
      (license license:gpl3+))))

(define-public emacs-package-lint
  (let ((commit "69bb89d00ba212b734c676ad056aa793c450b288"))
    (package
      (name "emacs-package-lint")
      (version (git-version "0.5" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/purcell/package-lint")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1hfricsgmy3x9snnd2p4xq6vnnv94qdsxxnxp07b3hqc9bhw31rq"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/purcell/package-lint")
      (synopsis "Linting library for elisp package authors")
      (description
       "This provides a list of issues with the Emacs package metadata of a file,
e.g. the package dependencies it requires.  See function
@code{package-lint-buffer}.  Checks will currently be enabled only if a
\"Package-Requires:\" or \"Package-Version:\" header is present in the
file.")
      (license license:gpl3+))))

(define-public emacs-picpocket
  (let ((version "40")
        (commit "6fd88b8711c4370662c0f9c462170187d092a046"))
    (package
      (name "emacs-picpocket")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/johanclaesson/picpocket")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1mdzzxf7xm7zwrpnqqxa27d1cr31pd72d7ilbwljv13qp177a3yw"))))
      (build-system emacs-build-system)
      (arguments ; needed for running tests
       `(#:tests? #t
         #:emacs ,emacs
         #:test-command '("emacs" "--batch"
                          "-l" "picpocket-test.el"
                          "-f" "ert-run-tests-batch-and-exit")))
      (home-page "https://github.com/johanclaesson/picpocket")
      (synopsis "Image viewer for Emacs")
      (description
       "Picpocket is an image viewer for GNU Emacs.  It has commands for:

@itemize
@item File operations on the picture files (delete, move, copy, hardlink).
@item Scale and rotate the picture.
@item Associate pictures with tags which are saved to disk.
@item Filter pictures according to tags.
@item Customizing keystrokes for quick tagging and file operations.
@item Undo and browse history of undoable commands.
@end itemize")
      (license license:gpl3+))))

(define-public emacs-wgrep-helm
  ;; `emacs-wgrep-helm' was mistakenly added.
  (deprecated-package "emacs-wgrep-helm" emacs-wgrep))

(define-public emacs-mu4e-conversation
  (let ((commit "e7d4bfcb0d392b0aed1f705ccac2419a168d1f5e"))
    (package
      (name "emacs-mu4e-conversation")
      (version (git-version "20181126" "4" commit))
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://gitlab.com/Ambrevar/mu4e-conversation/"
               "repository/archive.tar.gz?ref="
               commit))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0b52hf9rm2afba9pvgink9bwqm705sk0y5qikp0ff5sk53wqvy29"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("mu" ,mu)))
      (home-page
       "https://gitlab.com/Ambrevar/mu4e-conversation")
      (synopsis
       "Show a complete thread in a single buffer")
      (description
       "This package offers an alternate view to mu4e's e-mail display.  It
shows all e-mails of a thread in a single view, where each correspondent has
their own face.  Threads can be displayed linearly (in which case e-mails are
displayed in chronological order) or as an Org document where the node tree
maps the thread tree.")
      (license license:gpl3+))))

(define-public emacs-pinentry
  (let ((commit "dcc9ba03252ee5d39e03bba31b420e0708c3ba0c")
        (revision "1"))
    (package
      (name "emacs-pinentry")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain"
               "/packages/pinentry/pinentry.el?id=" commit))
         (file-name (string-append "pinentry.el"))
         (sha256
          (base32
           "1lf30q6r8nz5cjzclbb9bbymsk2y75nskvb55hnjdv93gr3j0sik"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("gnupg" ,gnupg)))
      (home-page "https://elpa.gnu.org/packages/pinentry.html")
      (synopsis "GnuPG Pinentry server implementation")
      (description
       "This package allows GnuPG passphrase to be prompted through the
minibuffer instead of graphical dialog.

To use, add @code{allow-emacs-pinentry} to @code{~/.gnupg/gpg-agent.conf},
reload the configuration with @code{gpgconf --reload gpg-agent}, and start the
server with @code{M-x pinentry-start}.")
      (license license:gpl3+))))

(define-public emacs-org-brain
  (package
    (name "emacs-org-brain")
    (version "0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Kungsgeten/org-brain.git")
             (commit "3faf9303af3f2356e3444e69c22dc6c5774047d1")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ad681zk6kckw2zbk0r4iaj4bw8cfqrbd1s3gdwgdjlzq81q9mmj"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Kungsgeten/org-brain")
    (synopsis "Org-mode wiki and concept-mapping for Emacs")
    (description "@code{emacs-org-brain} implements a variant of concept
mapping in Emacs, using @code{org-mode}.  An org-brain is a network of
org-mode entries, where each entry is a file or a headline, and you can get a
visual overview of the relationships between the entries: parents, children,
siblings and friends.  This visual overview can also be used to browse your
entries.  You can think of entries as nodes in a mind map, or pages in a
wiki.")
    (license license:expat)))

(define-public emacs-recent-addresses
  (let ((commit "afbbfdc43b81e620acf827ca20d297e0c517b6eb")
        (revision "1"))
    (package
      (name "emacs-recent-addresses")
      (home-page "http://nschum.de/src/emacs/recent-addresses/")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      ;; Note: Use a branch that works with Helm.  Submitted
                      ;; at <https://github.com/nschum/recent-addresses.el/pull/1>.
                      (url "https://github.com/civodul/recent-addresses.el")
                      (commit commit)))
                (sha256
                 (base32
                  "0ajrq0galjmdyjdjyxazykjyax3gh6hvfk4s7l657pi11g0q5zax"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (synopsis "Record recently-used email addressed and auto-complete them")
      (description
       "@code{recent-addresses} is an Emacs package that allows you to quickly
look up previously used email addresses.  It can be used alongside the Gnus
email client.")
      (license license:gpl2+))))

(define-public emacs-fold-dwim
  (let ((commit "c46f4bb2ce91b4e307136320e72c28dd50b6cd8b")
        (revision "0"))
    (package
      (name "emacs-fold-dwim")
      (version (git-version "1.2" revision commit))
      (home-page "https://github.com/emacsattic/fold-dwim")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (sha256
                 (base32
                  "1yz1wis31asw6xa5maliyd1ck2q02xnnh7dc6swgj9cb4wi7k6i1"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (synopsis "Unified user interface for Emacs folding modes")
      (description
       "DWIM stands for \"do what I mean\", as in the idea that one keystroke
can do different things depending on the context.  In this package, it means
that, if the cursor is in a currently hidden folded construction, we want to
show it; if it's not, we want to hide whatever fold the cursor is in.")
      (license license:gpl2+))))

(define-public emacs-markup-faces
  (package
    (name "emacs-markup-faces")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://stable.melpa.org/packages/markup-faces-"
                           version ".el"))
       (sha256
        (base32
         "124dxbaa25fwxnpwsygpz7pw6da6dnnw7y2lic3jf8rgz7lw4v32"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/sensorflo/markup-faces")
    (synopsis "Collection of Emacs faces for markup language modes")
    (description "emacs-markup-faces is like font-lock-faces, but tailored for
markup languages instead programming languages.  The sub group markup-faces-text
is also intended for 'text viewing modes' such as info or (wo)man.  This gives a
common look and feel, or let's say theme, across different markup language modes
and 'text viewing modes' respectively.")
    (license license:gpl3+)))

(define-public emacs-adoc-mode
  (package
    (name "emacs-adoc-mode")
    (version "0.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://stable.melpa.org/packages/adoc-mode-"
                           version ".el"))
       (sha256
        (base32
         "1c6hrgxxsnl2c19rgjykpm7r4xg9lp6bmk5z6bi7g8pqlrgwffcy"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-markup-faces" ,emacs-markup-faces)))
    (home-page "https://github.com/sensorflo/adoc-mode/wiki")
    (synopsis "AsciiDoc mode for Emacs")
    (description "This package provides an Emacs major mode for editing AsciiDoc
files.  It focuses on highlighting the document to improve readability.")
    (license license:gpl2+)))

(define-public emacs-rust-mode
  (let ((commit
         ;; Last release is old (2016), use more recent commit to get bug
         ;; fixes.
         "64b4a2450e4d4c47f6307851c9b2598cd2254d68")
        (revision "0"))
    (package
      (name "emacs-rust-mode")
      (version (git-version "0.3.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri
                 (git-reference
                  (url "https://github.com/rust-lang/rust-mode")
                  (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0pbz36lljgb7bdgx3h3g0pq1nss1kvn8mhk1l3mknsmynd6w4nd8"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda _
               (invoke "sh" "run_rust_emacs_tests.sh"))))))
      (home-page "https://github.com/rust-lang/rust-mode")
      (synopsis "Major Emacs mode for editing Rust source code")
      (description "This package provides a major Emacs mode for editing Rust
source code.")
      (license (list license:expat
                     license:asl2.0)))))

(define-public emacs-ztree
  (let ((commit "c54425a094353ec40a8179f9eab3596f76c6cf94"))
    (package
      (name "emacs-ztree")
      (version (git-version "1.0.5" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fourier/ztree")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0j8fpxds8m1zi04nrs8vv21091abvh4n8ab76f1sgdxnp4l5cfb0"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/fourier/ztree")
      (synopsis "Directory tree comparison mode for Emacs")
      (description "Ztree is a project dedicated to implementation of several
text-tree applications inside GNU Emacs.  It consists of 2 subprojects:
@command{ztree-diff} and @command{ztree-dir} (the basis of
@command{ztree-diff}).")
      (license license:gpl3))))

(define-public emacs-helm-org-contacts
  (let ((commit "0af703bd9a43032b89fdf5559673151d1ac2fffc"))
    (package
      (name "emacs-helm-org-contacts")
      (version (git-version "20180707" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tmalsburg/helm-org-contacts")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1cl7cm2ic9pg4vc9cdh84vzjj1x2lpd5ymimiva8h4l17kiphk4s"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-helm" ,emacs-helm)
         ("emacs-s" ,emacs-s)))
      (home-page "https://github.com/tmalsburg/helm-org-contacts")
      (synopsis "Helm source for org-contacts")
      (description "This Helm source can be used to search contacts stored in
org-contacts format.  There are actions for inserting postal addresses, email
addresses, and phone numbers in the buffer where @command{helm-org-contacts}
was called.")
      (license license:gpl3))))

(define-public emacs-dired-du
  (package
    (name "emacs-dired-du")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://elpa.gnu.org/packages/dired-du-"
             version ".tar"))
       (sha256
        (base32
         "1091scnrjh0a4gja4z6jxic6ghy1yryv46qk9c76pmh50cpw6766"))))
    (build-system emacs-build-system)
    (home-page "http://elpa.gnu.org/packages/dired-du.html")
    (synopsis "Dired with recursive directory sizes")
    (description
     "Display the recursive size of directories in Dired.
This file defines a minor mode @command{dired-du-mode} to show the recursive
size of directories in Dired buffers.  If @command{du} program is available,
then the directory sizes are obtained with it.  Otherwise, the directory sizes
are obtained with Lisp.  The former is faster and provide a more precise
value.  For directories where the user doesn't have read permission, the
recursive size is not obtained.  Once this mode is enabled, every new Dired
buffer displays recursive dir sizes.")
    (license license:gpl3+)))

(define-public emacs-pcre2el
  ;; Last release is very old so we get the latest commit.
  (let ((commit "0b5b2a2c173aab3fd14aac6cf5e90ad3bf58fa7d"))
    (package
      (name "emacs-pcre2el")
      (version (git-version "1.8" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/joddie/pcre2el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "14br6ad138qx1z822wqssswqiihxiynz1k69p6mcdisr2q8yyi1z"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/joddie/pcre2el")
      (synopsis "Convert between PCRE, Emacs and rx regexp syntax")
      (description "@code{pcre2el} or @code{rxt} (RegeXp Translator or RegeXp
Tools) is a utility for working with regular expressions in Emacs, based on a
recursive-descent parser for regexp syntax.  In addition to converting (a
subset of) PCRE syntax into its Emacs equivalent, it can do the following:

@itemize
@item convert Emacs syntax to PCRE
@item convert either syntax to @code{rx}, an S-expression based regexp syntax
@item untangle complex regexps by showing the parse tree in @code{rx} form and
highlighting the corresponding chunks of code
@item show the complete list of strings (productions) matching a regexp,
provided the list is finite
@item provide live font-locking of regexp syntax (so far only for Elisp
buffers – other modes on the TODO list).
@end itemize\n")
      (license license:gpl3))))

(define-public emacs-magit-todos
  (package
    (name "emacs-magit-todos")
    (version "1.1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alphapapa/magit-todos")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0qagdxpik64n4rw9scy451ws5sw00v64ri9g2dcw7b66bx2c6c6w"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-async" ,emacs-async)
       ("emacs-dash" ,emacs-dash)
       ("emacs-f" ,emacs-f)
       ("emacs-hl-todo" ,emacs-hl-todo)
       ("magit" ,emacs-magit)
       ("emacs-pcre2el" ,emacs-pcre2el)
       ("emacs-s" ,emacs-s)))
    (home-page "https://github.com/alphapapa/magit-todos")
    (synopsis "Show source files' TODOs (and FIXMEs, etc) in Magit status buffer")
    (description "This package displays keyword entries from source code
comments and Org files in the Magit status buffer.  Activating an item jumps
to it in its file.  By default, it uses keywords from @code{hl-todo}, minus a
few (like NOTE).")
    (license license:gpl3)))

(define-public emacs-git-annex
  ;; Unreleased version has a fontification fix.
  (let ((commit "ebdb44aef1883f1b2b8058e05d30fb9315b03707")
        (revision "1"))
    (package
      (name "emacs-git-annex")
      (version (string-append "1.1-" revision "." (string-take commit 8)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jwiegley/git-annex-el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1mzv40gj7k10h7h5s43my8msgzjpj680qprqa9pp8nbyhl49v3wh"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/jwiegley/git-annex-el")
      (synopsis "Emacs integration for git-annex")
      (description "Enhances Dired and buffers visiting annex files with
git-annex functionality.  In Dired, the names of annex files are shortened by
hiding the symbolic links and fontified based on whether content is present.
Commands for performing some common operations (e.g., unlocking and adding
files) are provided.")
      (license license:gpl2+))))

(define-public emacs-hackernews
  (let ((commit "916c3da8da45c757f5ec2faeed57fa370513d4ac"))
    (package
      (name "emacs-hackernews")
      (version (git-version "0.5.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/clarete/hackernews.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "09bxaaczana1cfvxyk9aagjvdszkj0j1yldl5r4xa60b59lxihsg"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/clarete/hackernews.el")
      (synopsis "Hacker News client for Emacs")
      (description "The @command{hackernews} package is able to fetch stories
from six different Hacker News feeds, namely top, new, best, ask, show and job
stories.  The default feed is top stories, which corresponds to the Hacker
News homepage.")
      (license license:gpl3))))

(define-public emacs-youtube-dl
  (let ((commit "7c9d7a7d05b72a7d1b1257a36c5e2b2567b185dd"))
    (package
      (name "emacs-youtube-dl")
      (version (git-version "1.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/skeeto/youtube-dl-emacs/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0mh4s089a4x8s380agzb2306kdp1hl204px1n5rrrrdcls7imnh6"))))
      (build-system emacs-build-system)
      (inputs
       `(("youtube-dl" ,youtube-dl)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'configure
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((youtube-dl (assoc-ref inputs "youtube-dl")))
                 ;; .el is read-only in git.
                 (chmod "youtube-dl.el" #o644)
                 ;; Specify the absolute file names of the various
                 ;; programs so that everything works out-of-the-box.
                 (emacs-substitute-variables
                     "youtube-dl.el"
                   ("youtube-dl-program"
                    (string-append youtube-dl "/bin/youtube-dl")))))))))
      (home-page "https://github.com/skeeto/youtube-dl-emacs/")
      (synopsis "Emacs youtube-dl download manager")
      (description "This package manages a video download queue for
@command{youtube-dl}, which serves as the back end.  It manages a single
@command{youtube-dl} subprocess, downloading one video at a time.  New videos
can be queued at any time.")
      (license license:unlicense))))

(define-public emacs-org-web-tools
  (package
    (name "emacs-org-web-tools")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alphapapa/org-web-tools")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0kak9h5ny00d39gnwspv53nadnag01brw2fq9zk5wpfc91h9bjng"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-esxml" ,emacs-esxml)
       ("emacs-s" ,emacs-s)))
    (inputs
     `(("pandoc" ,ghc-pandoc)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-exec-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((pandoc (assoc-ref inputs "pandoc")))
               (substitute* "org-web-tools.el"
                 (("\"pandoc\"") (string-append "\"" pandoc "/bin/pandoc\"")))
               #t))))))
    (home-page "https://github.com/alphapapa/org-web-tools")
    (synopsis "Display/Process web page as Org-mode content")
    (description "This package contains library functions and commands useful
for retrieving web page content and processing it into Org-mode content.

For example, you can copy a URL to the clipboard or kill-ring, then run a
command that downloads the page, isolates the “readable” content with
@command{eww-readable}, converts it to Org-mode content with Pandoc, and
displays it in an Org-mode buffer.  Another command does all of that but
inserts it as an Org entry instead of displaying it in a new buffer.")
    (license license:gpl3+)))

(define-public emacs-blimp
  (let ((commit "e420763d6d18b5d1be552cdbc47f91418343db03"))
    (package
      (name "emacs-blimp")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/walseb/blimp")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "09wmhpym516b81dfq8smdmysh1fn610dzlyvyl2rkx8600f0fizd"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-eimp" ,emacs-eimp)))
      (home-page "https://github.com/walseb/blimp")
      (synopsis "Emacs wrapper around all Imagemagick commands")
      (description "Blimp (Bustling Image Manipulation Package) is a complete
wrapper around all Imagemagick commands with descriptions, autocompletion (for
some commands) and hints displayed in prompt using @command{eimp.el} to
execute its commands and resize images.")
      (license license:gpl3+))))

(define-public emacs-synosaurus
  (let ((commit "8bf95b935976ec0a1964cf175ed57cc5f6f93bdb"))
    (package
      (name "emacs-synosaurus")
      (version (git-version "0.1.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hpdeifel/synosaurus")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "15by9jccab6kyplxa6k0glzaivxkqdigl33gl2qi2cvy6f2q7gva"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("wordnet" ,wordnet)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((wn (assoc-ref inputs "wordnet")))
                 ;; .el is read-only in git.
                 (chmod "synosaurus-wordnet.el" #o644)
                 ;; Specify the absolute file names of the various
                 ;; programs so that everything works out-of-the-box.
                 (emacs-substitute-variables
                     "synosaurus-wordnet.el"
                   ("wordnet-command"
                    (string-append wn "/bin/wn")))))))))
      (home-page "https://github.com/hpdeifel/synosaurus")
      (synopsis "Extensible thesaurus mode for Emacs")
      (description "Synosaurus is a thesaurus fontend for Emacs with pluggable
backends, including the @command{wordnet} offline backend.")
      (license license:gpl3+))))

(define-public emacs-editorconfig
  (package
    (name "emacs-editorconfig")
    (version "0.7.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/editorconfig/editorconfig-emacs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "19j2428ij7sqvrqs7rqg1mcnv9109y6drqba40dkv3vrkk5d2yia"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/editorconfig/editorconfig-emacs")
    (synopsis "Define and maintain consistent coding styles between different
editors and IDEs")
    (description "The EditorConfig project consists of a file format for
defining coding styles and a collection of text editor plugins that enable
editors to read the file format and adhere to defined styles.  EditorConfig
files are easily readable and they work nicely with version control systems.")
    (license license:gpl3+)))

(define-public emacs-all-the-icons
  (package
    (name "emacs-all-the-icons")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/domtronn/all-the-icons.el.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1sdl33117lccznj38021lwcdnpi9nxmym295q6y460y4dm4lx0jn"))))
    (build-system emacs-build-system)
    (arguments
     `(#:include '("\\.el$" "^data/" "^fonts/")
       ;; Compiling "test/" fails with "Symbol’s value as variable is void:
       ;; all-the-icons--root-code".  Ignoring tests.
       #:exclude '("^test/")
       #:tests? #f))
    (propagated-inputs
     `(("f" ,emacs-f)
       ("memoize" ,emacs-memoize)))
    (home-page "https://github.com/domtronn/all-the-icons.el")
    (synopsis "Collect icon fonts and propertize them within Emacs")
    (description "All-the-icons is a utility package to collect various icon
fonts and propertize them within Emacs.  Icon fonts allow you to propertize
and format icons the same way you would normal text.  This enables things such
as better scaling of and anti aliasing of the icons.")
    ;; Package is released under Expat license.  Elisp files are licensed
    ;; under GPL3+.  Fonts come with various licenses: Expat for
    ;; "all-the-icons.ttf" and "file-icons.ttf", Apache License 2.0 for
    ;; "material-design-icons.ttf", and SIL OFL 1.1 for "fontawesome.ttf",
    ;; "ocitcons.ttf" and "weathericons.ttf".
    (license
     (list license:expat license:gpl3+ license:silofl1.1 license:asl2.0))))

(define-public emacs-powerline
  (package
    (name "emacs-powerline")
    (version "2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/milkypostman/powerline.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hp3xp18943n0rlggz55150020ivw8gvi1vyxkr4z8xhpwq4gaar"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/milkypostman/powerline/")
    (synopsis "Mode-line plugin for Emacs")
    (description "Powerline is a utility plugin which allows you to create
a better-looking, more functional Emacs mode-line.  A collection of predefined
themes comes with the package.")
    (license license:gpl3+)))

(define-public emacs-spaceline
  (package
    (name "emacs-spaceline")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TheBB/spaceline.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1q8r95zfrh0vxna5ml2pq9b9f66clfqcl4d2qy2aizkvzyxg6skl"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("dash" ,emacs-dash)
       ("powerline" ,emacs-powerline)
       ("s" ,emacs-s)))
    (home-page "https://github.com/TheBB/spaceline")
    (synopsis "Powerline theme from Spacemacs")
    (description "Spaceline provides Spacemacs' mode-line theme.
This package provides features for three kinds of users.

@itemize
@item You just want to use the Spacemacs mode-line theme and forget about it.
@item You want to use something similar to the Spacemacs mode-line theme, but
with a handful of easy tweaks.
@item You want an easy-to-use library for building your own mode-line from
scratch, and you think the Spacemacs theme looks good.
@end itemize")
    (license license:gpl3+)))

(define-public emacs-column-marker
  (package
    (name "emacs-column-marker")
    (version "9")
    (source
     (origin
       (method url-fetch)
       (uri "https://www.emacswiki.org/emacs/download/column-marker.el")
       (sha256 (base32 "05bv198zhqw5hqq6cr11mhz02dpca74hhp1ycwq369m0yb2naxy9"))))
    (build-system emacs-build-system)
    (home-page "https://www.emacswiki.org/emacs/ColumnMarker")
    (synopsis "Emacs mode for highlighting columns")
    (description
     "With @code{column-marker.el} you can highlight any number of text columns.
Three such highlight column markers are provided by default.  This is
especially useful for languages like COBOL or Fortran where certain columns
have special meaning.  It is also handy for aligning text across long vertical
distances.  Multi-column characters, such as @kbd{TAB} are treated
correctly.")
    (license license:gpl2+)))

(define-public emacs-slime-repl-ansi-color
  (let ((commit "ad03263f5d4de473bc173b64a6fc3dc1106393d7"))
    (package
      (name "emacs-slime-repl-ansi-color")
      (version (git-version "0.0.0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/deadtrickster/slime-repl-ansi-color")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0bpg7gxz310x7bnlg324c507sxc5gxwwz6h64h6kdq141r73vbi4"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/deadtrickster/slime-repl-ansi-color")
      (synopsis "Color ANSI codes in the REPL of SLIME")
      (description "Color ANSI codes in the REPL of SLIME")
      (license license:gpl2+))))

(define-public emacs-helm-slime
  (package
    (name "emacs-helm-slime")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emacs-helm/helm-slime")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qhb9446rpj17pm0hi3miy5gs5k3ld43bq29kzy0y26bf7ivfcjv"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-helm" ,emacs-helm)
       ("emacs-slime" ,emacs-slime)))
    (home-page "https://github.com/emacs-helm/helm-slime")
    (synopsis "Helm for SLIME, the Superior Lisp Interaction Mode for Emacs")
    (description "Helm-SLIME defines a few new commands:

@itemize
@item helm-slime-complete: Select a symbol from the SLIME completion systems.
@item helm-slime-list-connections: Yet another slime-list-connections with Helm.
@item: helm-slime-apropos: Yet another slime-apropos with Helm.
@item helm-slime-repl-history: Select an input from the SLIME REPL history and insert it.
@end itemize\n")
    (license license:gpl3+)))

(define-public emacs-gtk-look
  (package
    (name "emacs-gtk-look")
    (version "29")
    (source (origin
              (method url-fetch)
              (uri "https://download.tuxfamily.org/user42/gtk-look.el")
              (sha256
               (base32
                "14p2nwrd51cr1v06fxbjjn6jdrkf9d6vcxhmscm0kl677s25ypsp"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda _
             ;; File is read-only.
             (chmod "gtk-look.el" #o644)
             (emacs-substitute-variables "gtk-look.el"
               ("gtk-lookup-devhelp-indices"
                '(list (expand-file-name "~/.guix-profile/share/gtk-doc/html/*/*.devhelp*"))))
             #t)))))
    (home-page "http://user42.tuxfamily.org/gtk-look/index.html")
    (synopsis "Find and display HTML documentation for GTK, GNOME and Glib")
    (description "@command{gtk-look} finds and displays HTML documentation for
GTK, GNOME and Glib functions and variables in Emacs, similar to what
info-lookup-symbol does for info files (C-h S).  The documentation is expected
to be devhelp indexes with HTML files.  The location of the indexes can be
customized.  In addition to C code development @command{gtk-look} is good for

@itemize
@item @command{perl-gtk2}, recognising class funcs like
@command{Gtk2::Label->new} and bare method names like @command{set_text}.
@item @command{guile-gnome}, recognising methods like @command{set-text} and
classes like @command{<gtk-window>}.
@end itemize\n")
    (license license:gpl3+)))

(define-public emacs-ov
  (package
    (name "emacs-ov")
    (version "1.0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ShingoFukuyama/ov.el.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qxk2rf84j86syxi8xknsq252irwg7sz396v3bb4wqz4prpj0kzc"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ShingoFukuyama/ov.el")
    (synopsis "Overlay library for Emacs Lisp")
    (description "@code{ov.el} provides a simple way to manipulate overlays in
Emacs.")
    (license license:gpl3+)))

(define-public emacs-matrix-client
  (let ((commit "a0623667b07a4bf60980c97b078e9faed97ace79"))
    (package
      (name "emacs-matrix-client")
      (version (git-version "0.0.0" "4" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jgkamat/matrix-client-el.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1zya8id3y9wzjaj7nplq7br6nhm3lsskv0fkn1xr1y77fzcfgcdb"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'add-missing-require
             ;; Fix a filter error at runtime due to a missing require.
             ;; Reported upstream:
             ;; <https://github.com/alphapapa/matrix-client.el/issues/76>
             (lambda _
               (substitute* "matrix-client-room.el"
                 (("\\(require 'dash-functional\\)" all)
                  (string-append all "\n" "(require 'anaphora)")))
               #t)))))
      (propagated-inputs
       `(("a" ,emacs-a)
         ("anaphora" ,emacs-anaphora)
         ("dash" ,emacs-dash)
         ("esxml" ,emacs-esxml)
         ("f" ,emacs-f)
         ("frame-purpose" ,emacs-frame-purpose)
         ("ht" ,emacs-ht)
         ("ov" ,emacs-ov)
         ("rainbow-identifiers" ,emacs-rainbow-identifiers)
         ("request" ,emacs-request)
         ("s" ,emacs-s)
         ("tracking" ,emacs-tracking)))
      (home-page "https://github.com/jgkamat/matrix-client-el")
      (synopsis "Matrix client for Emacs")
      (description "@code{matrix-client} is a simple chat UI to Matrix.org
rooms.  It also provides an API which allows Emacs to seamlessly create
RPC channels with users and other software.")
      (license license:gpl3+))))

(define-public emacs-sesman
  (package
    (name "emacs-sesman")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vspinu/sesman.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0r32f8ma9ddczxrrdz0nadp14j3zmk10q1ch02gb82synkx3xdra"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:test-command '("make" "test")))
    (home-page "https://github.com/vspinu/sesman")
    (synopsis "Session manager for Emacs based IDEs")
    (description "Sesman provides facilities for session management and
interactive session association with the current contexts (project, directory,
buffers).  While sesman can be used to manage arbitrary sessions, it primary
targets the Emacs based IDEs (CIDER, ESS, Geiser, Robe, SLIME etc.)")
    (license license:gpl3+)))

(define-public emacs-buttercup
  (package
    (name "emacs-buttercup")
    (version "1.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jorgenschaefer/emacs-buttercup.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0dckgcyzsav6ld78bcyrrygy1cz1jvqgav6vy8f6klpmk3r8xrl1"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #t
       #:test-command '("make" "test")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-bin
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "bin/buttercup"
                           (string-append (assoc-ref outputs "out") "/bin"))
             #t)))))
    (home-page "https://github.com/jorgenschaefer/emacs-buttercup")
    (synopsis "Behavior driven emacs lisp testing framework")
    (description "Buttercup is a behavior-driven development framework for
testing Emacs Lisp code.  It allows to group related tests so they can share
common set-up and tear-down code, and allows the programmer to \"spy\" on
functions to ensure they are called with the right arguments during testing.")
    (license license:gpl3+)))

(define-public emacs-wordnut
  (let ((commit "feac531404041855312c1a046bde7ea18c674915")
        (revision "0"))
    (package
      (name "emacs-wordnut")
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/gromnitsky/wordnut")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (sha256
                 (base32
                  "1jl0b6g64a9w0q7bfvwha67vgws5xd15b7mkfyb5gkz3pymqhfxn"))
                (patches
                 (search-patches "emacs-wordnut-require-adaptive-wrap.patch"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("wordnet" ,wordnet)
         ("emacs-adaptive-wrap" ,emacs-adaptive-wrap)))
      (synopsis "Major mode for WordNet")
      (description "This Emacs package provides an interface for
@code{wordnet}.  Features include completion, if the query is not found
too ambiguous and navigation in the result buffer.")
      (license license:gpl3+))))

(define-public emacs-frame-purpose
  (package
    (name "emacs-frame-purpose")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alphapapa/frame-purpose.el.git")
                    (commit version)))
              (sha256
               (base32
                "0jq2aam1yvccw887ighd1wm2xkvk5bv53ffiz3crcl16a255aj4q"))
              (file-name (git-file-name name version))))
    (build-system emacs-build-system)
    (inputs
     `(("dash" ,emacs-dash)))
    (synopsis "Purpose-specific frames for Emacs")
    (description "@code{frame-purpose} makes it easy to open purpose-specific
frames that only show certain buffers, e.g. by buffers’ major mode, their
filename or directory, etc, with custom frame/X-window titles, icons, and
other frame parameters.")
    (home-page "https://github.com/alphapapa/frame-purpose.el")
    (license license:gpl3+)))

(define-public emacs-arduino-mode
  (let ((commit "3e2bad4569ad26e929e6db2cbcff0d6d36812698")) ;no release yet
    (package
      (name "emacs-arduino-mode")
      (version (git-version "0" "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/bookest/arduino-mode.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1yvaqjc9hadbnnay5fprnh890xsp53kidad1zpb4a5z4a5z61n3c"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (synopsis "Emacs major mode for editing Arduino sketches")
      (description "Emacs major mode for editing Arduino sketches.")
      (home-page "https://github.com/bookest/arduino-mode")
      (license license:gpl3+))))

(define-public emacs-general
  (let ((commit "675050199b5a30d54a24b58a367db32c0bdc47f5"))
    (package
      (name "emacs-general")
      (version (git-version "0" "0" commit))
      (home-page "https://github.com/noctuid/general.el")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url (string-append home-page ".git"))
                      (commit commit)))
                (sha256
                 (base32
                  "175yyhzk57yk1sskxh3d2jzhrh2waiibbcfsll167qxr117yji5h"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (synopsis "More convenient key definitions in emacs")
      (description "@code{general.el} provides a more convenient method for
binding keys in emacs (for both evil and non-evil users).  Like
@code{use-package}, which provides a convenient, unified interface for
managing packages, @code{general.el} is intended to provide a convenient,
unified interface for key definitions.  While this package does implement some
completely new functionality (such as the ability to make vim-style
keybindings under non-prefix keys with an optional timeout), its primary
purpose is to build on existing functionality to make key definition more
clear and concise.  @code{general-define-key} is user-extensible and supports
defining multiple keys in multiple keymaps at once, implicitly wrapping key
strings with (@code{kbd ...}), using named prefix key sequences (like the
leader key in vim), and much more.")
      (license license:gpl3+))))

(define-public emacs-tldr
  (let ((commit "398b197c8d2238628b07e1b32d0f373876279f4c"))
    (package
      (name "emacs-tldr")
      (version (git-version "0" "0" commit))
      (home-page "https://github.com/kuanyui/tldr.el")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url (string-append home-page ".git"))
                      (commit commit)))
                (sha256
                 (base32
                  "0iq7qlis6c6r2qkdpncrhh5vsihkhvy5x4y1y8cjb7zxkh62w33f"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (synopsis "Simplified and community-driven man pages for Emacs")
      (description "@code{emacs-tldr} allows the user to access tldr pages
from within emacs.  The @code{tldr} pages are a community effort to simplify
the man pages with practical examples.")
      (license license:wtfpl2))))

(define-public emacs-window-layout
  (package
    (name "emacs-window-layout")
    (version "1.4")
    (home-page "https://github.com/kiwanami/emacs-window-layout")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0wgqi8r844lbx52fn6az8c1n8m681rp6dkfzd54wmdk1ka7zmvv6"))
              (file-name (git-file-name name version))))
    (build-system emacs-build-system)
    (synopsis "Simple window layout management framework for emacs")
    (description "A window-layout management library that can split a frame
or a window into some windows according to a layout recipe.")
    (license license:gpl3+)))

(define-public emacs-e2wm
  (package
      (name "emacs-e2wm")
      (version "1.4")
      (home-page "https://github.com/kiwanami/emacs-window-manager")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit (string-append "v" version))))
                (sha256
                 (base32
                  "12midsrx07pdrsr1qbl2rpi7xyhxqx08bkz7n7gf8vsmqkpfp56s"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-window-layout" ,emacs-window-layout)))
      (synopsis "Equilibrium Emacs Window Manager")
      (description "E2WM is a window manager for Emacs.  It enables to
customize the place of pop-up window, how the windows are split, how the
buffers are located in the windows, keybinds to manipulate windows and
buffers, etc.  It also has plug-ins to help your Emacs life.")
      (license license:gpl3+)))

(define-public emacs-ctable
  (let ((commit "b8830d1ca95abb100a81bc32011bd17d5ecba000"))
    (package
      (name "emacs-ctable")
      (version (git-version "0.1.2" "1" commit))
      (home-page "https://github.com/kiwanami/emacs-ctable")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "0pg303pnqscrsbx9579hc815angszsgf9vpd2z2f8p4f4ka6a00h"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (synopsis "Table component for Emacs Lisp")
      (description "This program is a table component for Emacs Lisp.  Other
programs can use this table component for the application UI.")
      (license license:gpl3+))))

(define-public emacs-epc
  (let ((commit "e1bfa5ca163273859336e3cc89b4b6460f7f8cda"))
    (package
      (name "emacs-epc")
      (version (git-version "0.1.1" "1" commit))
      (home-page "https://github.com/kiwanami/emacs-epc")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "15nkrjgi64f829isfd6xrhl4zw8jirr8pkas7nisrbk1av868hx0"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-deferred" ,emacs-deferred)
         ("emacs-ctable" ,emacs-ctable)))
      (synopsis "RPC stack for Emacs Lisp")
      (description "This program is an asynchronous RPC stack for Emacs.
Using this RPC stack, Emacs can communicate with the peer process
smoothly.  Because the protocol employs S-expression encoding and consists of
asynchronous communications, the RPC response is fairly good.")
      (license license:gpl3+))))

(define-public emacs-edbi
  (let ((commit "6f50aaf4bde75255221f2292c7a4ad3fa9d918c0"))
    (package
      (name "emacs-edbi")
      (version (git-version "0.1.3" "1" commit))
      (home-page "https://github.com/kiwanami/emacs-edbi")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "0x0igyvdcm4863n7zndvcv6wgzwgn7324cbfjja6xd7r0k936zdy"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (inputs
       `(("perl" ,perl)
         ("perl-rpc-epc-service" ,perl-rpc-epc-service)
         ("perl-dbi" ,perl-dbi)
         ;; TODO: Adding support for perl-dbd-mysql and others would
         ;; dramatically increase the closure size.  Make several packages?
         ("perl-dbd-sqlite" ,perl-dbd-sqlite)))
      (propagated-inputs
       `(("emacs-e2wm" ,emacs-e2wm)
         ("emacs-epc" ,emacs-epc)))
      (arguments
       `(#:include '("\\.el$" "\\.pl$")
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'patch-path
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((perl (assoc-ref inputs "perl"))
                     (dir (string-append  (assoc-ref outputs "out")
                                          "/share/emacs/site-lisp/guix.d/edbi-"
                                          ,version)))
                 (substitute* (string-append dir  "/edbi.el")
                   (("\"perl\"") (string-append "\"" perl "/bin/perl\"")))
                 (chmod (string-append dir "/edbi-bridge.pl") #o555)
                 (wrap-program (string-append dir "/edbi-bridge.pl")
                   `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB"))))
                 #t))))))
      (synopsis "Database Interface for Emacs Lisp")
      (description "This program connects the database server through Perl's
DBI, and provides DB-accessing API and the simple management UI.")
      (license license:gpl3+))))

(define-public emacs-edbi-sqlite
  (let ((commit "52cb9ca1af7691b592f2cfd2f007847e7a4ccd5f"))
    (package
      (name "emacs-edbi-sqlite")
      (version (git-version "0.1.1" "1" commit))
      (home-page "https://github.com/proofit404/edbi-sqlite")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "1vll81386fx90lq5sy4rlxcik6mvw7zx5cc51f0yaca9bkcckp51"))
                (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-edbi" ,emacs-edbi)))
      (synopsis "Open SQLite files in Emacs")
      (description "This package is a convenience wrapper for @command{edbi}
to open SQLite databases.")
      (license license:gpl3+))))

(define-public emacs-nix-mode
  (package
    (name "emacs-nix-mode")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NixOS/nix-mode.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vz3s2jx14nzy53f04d821n4f2s22ys5h9s7af6cnpynkwawyhhq"))))
    (build-system emacs-build-system)
    (inputs
     `(("emacs-company" ,emacs-company)
       ("emacs-mmm-mode" ,emacs-mmm-mode)))
    (home-page "https://github.com/NixOS/nix-mode")
    (synopsis "Emacs major mode for editing Nix expressions")
    (description "@code{nixos-mode} provides an Emacs major mode for editing
Nix expressions.  It supports syntax highlighting, indenting and refilling of
comments.")
    (license license:lgpl2.1+)))

(define-public emacs-simple-mpc
  ;; There have been no releases.
  (let ((commit "bee8520e81292b4c7353e45b193f9a13b482f5b2")
        (revision "1"))
    (package
      (name "emacs-simple-mpc")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jorenvo/simple-mpc.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1ja06pv007cmzjjgka95jlg31k7d29jrih1yxyblsxv85s9sg21q"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-s" ,emacs-s)))
      (home-page "https://github.com/jorenvo/simple-mpc")
      (synopsis "Simple Emacs frontend to mpc")
      (description "This package provides an Emacs major mode which acts as a
front end to mpc, a client for the @dfn{Music Player Daemon} (MPD).")
      (license license:gpl3+))))

(define-public emacs-mkmcc-gnuplot-mode
  (package
    (name "emacs-mkmcc-gnuplot-mode")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mkmcc/gnuplot-mode")
             (commit "601f6392986f0cba332c87678d31ae0d0a496ce7")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "14f0yh1rjqc3337j4sbqzfb7pjim2c8a7wk1a73xkrdkmjn82vgb"))))
    (build-system emacs-build-system)
    (home-page "https://mkmcc.github.io/software/gnuplot-mode.html")
    (synopsis "Minimal emacs major mode for editing gnuplot scripts")
    (description "@code{emacs-mkmcc-gnuplot-mode} is a minimal emacs major
mode for editing gnuplot scripts.  It provides syntax highlighting,
indentation and a command to plot the file.")
    (license license:gpl3+)))

(define-public emacs-dtrt-indent
  (package
    (name "emacs-dtrt-indent")
    (version "0.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jscheid/dtrt-indent")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pgf0pvqd8k4yzhdn2df9lp0y8hmlm2ccrh07jivwlccs95pcz7z"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/jscheid/dtrt-indent")
    (synopsis "Minor mode that guesses the indentation offset")
    (description "This package provides a minor mode that guesses the
indentation offset originally used for creating source code files and
transparently adjusts the corresponding settings in Emacs, making it more
convenient to edit foreign files.")
    (license license:gpl2+)))

(define-public emacs-repo
  (package
    (name "emacs-repo")
    (version "0.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/canatella/repo-el")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rbvcvm7bfr6ncji7cllfxyyr6x7n9fx863byp243phsj3n93adz"))))
    (build-system emacs-build-system)
    (native-inputs
     `(("emacs-el-mock" ,emacs-el-mock)
       ("ert-runner" ,emacs-ert-runner)))
    (propagated-inputs
     `(("emacs-f" ,emacs-f)
       ("magit" ,emacs-magit)))
    (home-page "https://github.com/canatella/repo-el")
    (synopsis "Emacs interface for the Google Repo tool")
    (description "This package provides integration of the Google Repo tool
with emacs.  It displays the output of the @code{repo status} command in a
buffer and launches Magit from the status buffer for the project at point.")
    (license license:gpl3+)))

(define-public emacs-alect-themes
  (package
    (name "emacs-alect-themes")
    (version "0.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alezost/alect-themes")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nffxpdm0sa7bynwi0rmlwpc4qmvbda5ankhzz7fmk4ap9fkjxv9"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/alezost/alect-themes")
    (synopsis "Low contrast, light, dark and black color theme for Emacs")
    (description "@code{emacs-alect-themes} provides configurable light, dark
and black color themes for Emacs.  The themes are intended to be used with
GUI.")
    (license license:gpl3+)))

(define-public emacs-google-c-style
  (let ((commit "6271f3f473ceb3a7fef99388a3040903b1a145f1")
        (revision "0"))
    (package
      (name "emacs-google-c-style")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/google/styleguide")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jghyyasdl15c4gaqaxmdn43am63k6bykn5ab83f1ahv9zi1krxk"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/google/styleguide")
      (synopsis "Emacs settings file for Google C/C++ style")
      (description "@code{emacs-google-c-style} provides an Emacs settings
file for Google C and C++ style.")
      (license license:gpl1+))))

(define-public emacs-redshank
  (let ((commit "f98e68f532e622bcd464292ca4a9cf5fbea14ebb")
        (revision "1"))
    (package
      (name "emacs-redshank")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "http://www.foldr.org/~michaelw/projects/redshank.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jdkgvd5xy9hl5q611jwah2n05abjp7qcy9sj4k1z11x0ii62b6p"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-paredit" ,emacs-paredit)))
      (home-page "http://www.foldr.org/~michaelw/emacs/redshank/")
      (synopsis "Common Lisp Editing Extensions (for Emacs)")
      (description "Redshank is a collection of code-wrangling Emacs macros
mostly geared towards Common Lisp, but some are useful for other Lisp
dialects, too.  Redshank's code transformations aim to be expression-based (as
opposed to character-based).")
      (license license:gpl1+))))

(define-public emacs-disk-usage
  (package
    (name "emacs-disk-usage")
    (version "1.3.3")
    (home-page "https://gitlab.com/Ambrevar/emacs-disk-usage")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://elpa.gnu.org/packages/disk-usage-"
             version
             ".el"))
       (sha256
        (base32
         "0h1jwznd41gi0vg830ilfgm01q05zknikzahwasm9cizwm2wyizj"))))
    (build-system emacs-build-system)
    (synopsis "Sort and browse disk usage listings with Emacs")
    (description "Disk Usage is a file system analyzer: it offers a tabulated
view of file listings sorted by size.  Directory sizes are computed
recursively.  The results are cached for speed.")
    (license license:gpl3+)))

(define-public emacs-orgit
  (let ((commit "2456436a7e64d26bcf455b3890a586acaa3e7f93"))
    (package
      (name "emacs-orgit")
      (version (git-version "1.5.1" "2" commit))
      (home-page "https://github.com/magit/orgit")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1i52dq2ynddb1irgigr5mdwfbfd3bvm1f29jnzd7nlc0nk186nvh"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-magit" ,emacs-magit)))
      (synopsis "Support for Org links to Magit buffers")
      (description "This package defines several Org link types, which can be
used to link to certain Magit buffers.  Use the command
@command{org-store-link} while such a buffer is current to store a link.
Later you can insert it into an Org buffer using the command
@code{org-insert-link}.")
      (license license:gpl3+))))

(define-public emacs-amx
  (package
    (name "emacs-amx")
    (version "3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DarwinAwardWinner/amx")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bb8y1dmzyqkrb4mg6zndcsxppby3glridv2aap2pv05gv8kx7mj"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-s" ,emacs-s)))
    (home-page "https://github.com/DarwinAwardWinner/amx")
    (synopsis "Alternative interface for M-x")
    (description "Amx is an alternative interface for M-x in Emacs.  It
provides several enhancements over the ordinary
@code{execute-extended-command}, such as prioritizing your most-used commands
in the completion list and showing keyboard shortcuts, and it supports several
completion systems for selecting commands, such as ido and ivy.")
    (license license:gpl3+)))

(define-public emacs-lorem-ipsum
  (let ((commit "4b39f6fed455d67f635b3837cf5668bf74d0f6cd"))
    (package
      (name "emacs-lorem-ipsum")
      (version (git-version "0.2" "1" commit))
      (home-page "https://github.com/jschaf/emacs-lorem-ipsum/")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0a3b18p3vdjci89prsgdzjnfxsl8p67vjhf8ai4qdng7zvh50lir"))))
      (build-system emacs-build-system)
      (synopsis "Insert dummy pseudo Latin text in Emacs")
      (description "This package provides convenience functions to insert
dummy Latin text into a buffer.  This can be useful if you need to produce
paragraphs or pages of text for testing purposes.")
      (license license:gpl3+))))

(define-public emacs-lisp-extra-font-lock
  (let ((commit "4605eccbe1a7fcbd3cacf5b71249435413b4db4f"))
    (package
      (name "emacs-lisp-extra-font-lock")
      (version (git-version "0.0.6" "1" commit))
      (home-page "https://github.com/Lindydancer/lisp-extra-font-lock")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "152vcp3mdlv33jf5va4rinl1d0k960gnfhbrqqrafazgx9j3ya8w"))))
      (build-system emacs-build-system)
      (synopsis "Highlight bound variables and quoted expressions in Emacs")
      (description "This package highlight the location where local variables
is created (bound, for example, by let) as well as quoted and backquoted
constant expressions.")
      (license license:gpl3+))))

(define-public emacs-docker-tramp
  (package
    (name "emacs-docker-tramp")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-pe/docker-tramp.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lxvzmfg52fhxrhbvp92zwp7cv4i1rlxnkyyzgngj3sjm7y60yvg"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/emacs-pe/docker-tramp.el")
    (synopsis "TRAMP integration for docker containers")
    (description
     "This package provides a TRAMP method for Docker containers.")
    (license license:gpl3+)))

(define-public emacs-docker
  (package
    (name "emacs-docker")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Silex/docker.el")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15kd86kaq1x6giz855q9w6zvnyc742j309j0pmm86rwx398g4rq1"))))
    (inputs
     `(("emacs-undercover" ,emacs-undercover)))
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-docker-tramp" ,emacs-docker-tramp)
       ("emacs-magit-popup" ,emacs-magit-popup)
       ("emacs-s" ,emacs-s)
       ("emacs-tablist" ,emacs-tablist)
       ("emacs-json-mode" ,emacs-json-mode)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)))) ;no tests
    (build-system emacs-build-system)
    (home-page "https://github.com/Silex/docker.el")
    (synopsis "Manage docker from Emacs")
    (description "This package provides an Emacs interface for Docker.")
    (license license:gpl3+)))

(define-public emacs-dockerfile-mode
  ;; Latest upstream release is too old.
  (let ((commit "7223d92718f78fa3ab15667cdb2ed90cfeb579e7"))
    (package
      (name "emacs-dockerfile-mode")
      (version (git-version "1.2" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/spotify/dockerfile-mode.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0hmipgl4rk6aih11i8mnspwdijjiwk2y0wns6lzs8bgkvy3c064r"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-s" ,emacs-s)))
      (home-page "https://github.com/spotify/dockerfile-mode")
      (synopsis "Major mode for editing Dockerfile")
      (description
       "This package provides a major mode @code{dockerfile-mode} for use with
the standard @code{Dockerfile} file format.")
      (license license:asl2.0))))

(define-public emacs-lsp-mode
  (package
    (name "emacs-lsp-mode")
    (version "6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emacs-lsp/lsp-mode.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v1mq6ixzlgiazj8fmg4xaqhsqn3l89iqy74yndhvzh2rdf0pbkl"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-f" ,emacs-f)
       ("emacs-ht" ,emacs-ht)
       ("emacs-spinner" ,emacs-spinner)))
    (home-page "https://github.com/emacs-lsp/lsp-mode")
    (synopsis "Emacs client and library for the Language Server Protocol")
    (description "@code{LSP-mode} is a client and library implmentation for
the Language Server Protocol.  This mode aims to provide an IDE-like
experience by providing optional integration with other popular Emacs packages
like @code{company}, @code{flycheck}, and @code{projectile}.")
    (license license:gpl3+)))

(define-public emacs-lsp-ui
  (package
    (name "emacs-lsp-ui")
    (version "6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emacs-lsp/lsp-ui.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r4327fd8cvjxfwkddp5c4bdskyncbs4sx9m3z2w4d773y2jrakc"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-lsp-mode" ,emacs-lsp-mode)
       ("emacs-markdown-mode" ,emacs-markdown-mode)
       ("emacs-flycheck" ,emacs-flycheck)))
    (home-page "https://github.com/emacs-lsp/lsp-ui")
    (synopsis "User interface extensions for @code{lsp-mode}")
    (description
     "@code{LSP-ui} contains several enhancements and integrations for
@code{lsp-mode}, such as visual flychecking, displaying references in-line,
and code peeking.")
    (license license:gpl3+)))

(define-public emacs-helm-notmuch
  (package
    (name "emacs-helm-notmuch")
    (version "1.2")
    (home-page "https://github.com/emacs-helm/helm-notmuch/")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k038dbdpaa411gl4071x19fklhnizhr346plxw23lsnxir9dhqc"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-helm" ,emacs-helm)
       ("notmuch" ,notmuch)))
    (synopsis "Search emails with Emacs Notmuch and Helm")
    (description
     "This package can be used to search emails in Emacs, searching result
displays as you type thanks to Helm, though @command{notmuch-search} does the
real search.")
    (license license:gpl3+)))

(define-public emacs-elmacro
  (let ((commit "89b9b0feabafd01fee48111d67131c4c9b5fed9a"))
    (package
      (name "emacs-elmacro")
      (version (git-version "1.1.0" "1" commit))
      (home-page "https://github.com/Silex/elmacro")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "06wkzafh6vbcjf0m3sl253mgcq32p8cdv7vsfmyx51baa36938ha"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-s" ,emacs-s)
         ("emacs-dash" ,emacs-dash)))
      (synopsis "Convert keyboard macros to Emacs Lisp")
      (description
       "This package displays keyboard macros or latest interactive commands
as Emacs Lisp.")
      (license license:gpl3+))))

(define-public emacs-transient
  ;; 0.1.0 depends on lv.el but not later versions.
  (let ((commit "7e45a57ec81185631fe763733f64c99021df2a06"))
    (package
      (name "emacs-transient")
      (version (git-version "0.1.0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/magit/transient")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0r6d4c1lga3bk0s7q7y4v4hbpxnd9h40cjxybqvax2z902931fz1"))))
      (build-system gnu-build-system)
      (native-inputs `(("texinfo" ,texinfo)
                       ("emacs" ,emacs-minimal)))
      (propagated-inputs
       `(("dash" ,emacs-dash)))
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-26)
                    (guix build emacs-utils))
         #:imported-modules (,@%gnu-build-system-modules
                             (guix build emacs-utils))
         #:tests? #f                   ; tests are not included in the release
         #:make-flags (list "lisp" "info"
                            (string-append "LOAD_PATH=-L . -L "
                                           (assoc-ref %build-inputs "dash")
                                           "/share/emacs/site-lisp/guix.d/dash-"
                                           ,(package-version emacs-dash)))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lisp (string-append out "/share/emacs/site-lisp/guix.d/"
                                           "transient" "-" ,version))
                      (info (string-append out "/share/info")))
                 (for-each (cut install-file <> lisp)
                           (find-files "." "\\.elc*$"))
                 (install-file "docs/transient.info" (string-append info)))
               #t)))))
      (home-page "https://magit.vc/manual/transient")
      (synopsis "Transient commands in Emacs")
      (description
       "Taking inspiration from prefix keys and prefix arguments in Emacs,
Transient implements a similar abstraction involving a prefix command, infix
arguments and suffix commands.  We could call this abstraction a \"transient
command\", but because it always involves at least two commands (a prefix and
a suffix) we prefer to call it just a \"transient\".")
      (license license:gpl3+))))

(define-public emacs-undo-propose-el
  (let ((commit "5f1fa99a04369a959aad01b476fe4f34229f28cd")
        (version "1.0.0")
        (revision "1"))
    (package
      (name "emacs-undo-propose-el")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jackkamm/undo-propose-el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1p9h1fqmva07mcs46rqrg9vqn537b615as84s9b7xh76k1r8h1c0"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/jackkamm/undo-propose-el")
      (synopsis "Simple and safe navigation of @code{undo} history")
      (description "This package permits navigation of @code{undo} history in a
temporary buffer, which is read-only save for @code{undo} commands, and
subsequent committal of a chain of @code{undo} commands as a single edit in
the @code{undo} history.")
      (license license:gpl3+))))

(define-public emacs-elisp-docstring-mode
  (let ((commit "f512e509dd690f65133e55563ebbfd2dede5034f")
        (version "0.0.1")
        (revision "1"))
    (package
      (name "emacs-elisp-docstring-mode")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Fuco1/elisp-docstring-mode")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0al8m75p359h4n82rf0dsl22qfdg9cwwywn5pn7x6gb96c7qrqaa"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/Fuco1/elisp-docstring-mode")
      (synopsis "Major mode for editing Emacs Lisp docstrings")
      (description "This package provides font lock and automatic escaping and
unescaping of quotes.")
      (license license:gpl3+))))

(define-public emacs-vimrc-mode
  (let ((commit "13bc150a870d5d4a95f1111e4740e2b22813c30e")
        (version "0.3.1")
        (revision "1"))
    (package
      (name "emacs-vimrc-mode")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mcandre/vimrc-mode")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0026dqs3hwygk2k2xfra90w5sfnxrfj7l69jz7sq5glavbf340pk"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/mcandre/vimrc-mode")
      (synopsis "Major mode for Vimscript")
      (description "This package provides font lock and @code{beginning-} and
@code{end-of-defun} functions for Vimscript files.")
      (license license:gpl3+))))

(define-public emacs-flycheck-haskell
  (let ((commit "32ddff87165a7d3a35e7318bee997b5b4bd41278")
        (version "0.8")
        (revision "79"))
    (package
      (name "emacs-flycheck-haskell")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/flycheck/flycheck-haskell")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "10pgsbagq6qj4mshq5sypv0q0khck92b30sc793b4g1pfpsxvgjn"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-seq" ,emacs-seq)
         ("emacs-flycheck" ,emacs-flycheck)
         ("emacs-haskell-mode" ,emacs-haskell-mode)
         ("emacs-let-alist" ,emacs-let-alist)))
      (home-page "https://github.com/flycheck/flycheck-haskell")
      (synopsis "Flycheck for Haskell")
      (description "This package configures syntax-checking for Haskell
buffers.")
      (license license:gpl3+))))

(define-public emacs-js2-refactor-el
  (let ((commit "79124b3274c43ad1f9ec6205fa362576552db02f")
        (version "0.9.0")
        (revision "27"))
    (package
      (name "emacs-js2-refactor-el")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/magnars/js2-refactor.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1wswhlpbd3airrhyncb9vblqigwnqg9n96z0iis8jnz37q2whica"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-s" ,emacs-s)
         ("emacs-js2-mode" ,emacs-js2-mode)
         ("emacs-yasnippet" ,emacs-yasnippet)
         ("emacs-multiple-cursors" ,emacs-multiple-cursors)))
      (native-inputs
       `(("emacs-buttercup" ,emacs-buttercup)
         ("emacs-espuds" ,emacs-espuds)))
      (home-page "https://github.com/magnars/js2-refactor.el")
      (synopsis "JavaScript refactoring in Emacs")
      (description "This package provides various refactoring functions for
JavaScript.")
      (license license:gpl3+))))

(define-public emacs-prettier
  (let ((commit "e9b73e81d3e1642aec682195f127a42dfb0b5774")
        (version "0.1.0")
        (revision "1"))
    (package
      (name "emacs-prettier")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/prettier/prettier-emacs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0hj4fv3fgc60i6jrsx4a81s5c9fsxxafhhs3q2q1dypsscjci9ph"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/prettier/prettier-emacs")
      (synopsis "Automatic formatting of JavaScript code")
      (description "This package integrates Prettier with Emacs, and
provides a minor mode that autoformats the buffer upon saving.")
      (license license:expat))))

(define-public emacs-fish-mode
  (package
    (name "emacs-fish-mode")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wwwjfy/emacs-fish")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0a74ghmjjrxfdhk4mvq6lar4w6l6lc4iilabs99smqr2fn5rsslq"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/wwwjfy/emacs-fish")
    (synopsis "Major mode for Fish shell scripts")
    (description "This package provides syntax highlighting and indentation
functions for Fish shell scripts.")
    (license license:gpl3+)))

(define-public emacs-eshell-up
  (let ((commit "9c100bae5c3020e8d9307e4332d3b64e7dc28519")
        (version "0.0.3")
        (revision "12"))
    (package
      (name "emacs-eshell-up")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/peterwvj/eshell-up")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00zdbcncjabgj5mp47l1chymx5na18v2g4kj730dgmj3rnl3iz2q"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/peterwvj/eshell-up")
      (synopsis "Quickly go to a parent directory in @code{Eshell}")
      (description "This package provides quick navigation to a specific
parent directory using @code{Eshell}.")
      (license license:gpl3+))))

(define-public emacs-tco-el
  (let ((commit "482db5313f090b17ed22ccd856f0e141dc75afe6")
        (version "0.3")
        (revision "1"))
    (package
      (name "emacs-tco-el")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Wilfred/tco.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1z7xkbrqznk6ni687qqknp8labcyhl8y6576hjfri89fn21385y9"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)))
      (home-page "https://github.com/Wilfred/tco.el")
      (synopsis "Tail-call optimization for Emacs Lisp")
      (description "This package provides tail-call optimization for Emacs
Lisp functions that call themselves in tail position.")
      (license license:gpl3+))))

(define-public emacs-equake
  (let ((commit "ed15fd55cd4f2276161a6f712ed0b83cd10a8cdc")
        (version "0.85")
        (revision "1"))
    (package
      (name "emacs-equake")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/emacsomancer/equake/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "04kj88rlnn22gwmmv2gly2ibi6jka6l2cd4979pi6lhlvqqgjdnj"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-tco-el" ,emacs-tco-el)))
      (home-page "https://gitlab.com/emacsomancer/equake/")
      (synopsis "Drop-down console for @code{Eshell} and terminal emulators")
      (description "This package provides a Quake-style drop-down console
compatible with Emacs' shell modes.")
      (license license:gpl3+))))

(define-public emacs-vdiff
  (let ((commit "09e15fc932bfd2febe1d4a65780a532394562b07")
        (version "0.2.3")
        (revision "1"))
    (package
      (name "emacs-vdiff")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/justbur/emacs-vdiff/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1gvqi5l4zs872nn4pmj603aza09d81qad2rgijzv268lif8z34db"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-hydra" ,emacs-hydra)))
      (home-page "https://github.com/justbur/emacs-vdiff/")
      (synopsis "Frontend for diffing based on vimdiff")
      (description "This package permits comparisons of two or three buffers
based on diff output.")
      (license license:gpl3+))))

(define-public emacs-vdiff-magit
  ;; Need to use a more recent commit than the latest release version because
  ;; of Magit and Transient
  (let ((commit "b100d126c69e5c26a61ae05aa1778bcc4302b597")
        (version "0.3.2")
        (revision "8"))
    (package
      (name "emacs-vdiff-magit")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/justbur/emacs-vdiff-magit/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "16cjmrzflf2i1w01973sl944xrfanakba8sb4dpwi79d92xp03xy"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-vdiff" ,emacs-vdiff)
         ("emacs-magit" ,emacs-magit)
         ("emacs-transient" ,emacs-transient)))
      (home-page "https://github.com/justbur/emacs-vdiff-magit/")
      (synopsis "Frontend for diffing based on vimdiff")
      (description "This package permits comparisons of two or three buffers
based on diff output.")
      (license license:gpl3+))))

(define-public emacs-all-the-icons-dired
  (let ((commit "980b7747d6c4a7992a1ec56afad908956db0a519")
        (version "1.0")
        (revision "1"))
    (package
      (name "emacs-all-the-icons-dired")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jtbm37/all-the-icons-dired/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1pvbgyxfj4j205nj1r02045f1y4wgavdsk7f45hxkkhms1rj8jyy"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-all-the-icons" ,emacs-all-the-icons)))
      (home-page "https://github.com/jtbm37/all-the-icons-dired/")
      (synopsis "Show icons for each file in @code{dired-mode}")
      (description "This package allows icons from @file{all-the-icons.el} to
be used in @code{dired-mode}.")
      (license license:gpl3+))))

(define-public emacs-exwm-edit
  (let ((commit "961c0f3ea45766b888c73d7353da13d329538034")
        (version "0.0.1")
        (revision "1"))
    (package
      (name "emacs-exwm-edit")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/agzam/exwm-edit/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "087pk5ckx753qrn6xpka9khhlp7iqlz76w7861x90av2f5cgy6fw"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-exwm" ,emacs-exwm)))
      (home-page "https://github.com/agzam/exwm-edit/")
      (synopsis "Open temp buffers for editing text from EXWM buffers")
      (description "This package facilitates editing text from EXWM buffers by
generating a temp buffer in which any useful Emacs utilities and modes can be
invoked.")
      (license license:gpl3+))))

(define-public emacs-ert-async
  (package
    (name "emacs-ert-async")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rejeep/ert-async.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hn9i405nfhjd1h9vnwj43nxbbz00khrwkjq0acfyxjaz1shfac9"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/rejeep/ert-async.el")
    (synopsis "Async support for ERT")
    (description "This package allows ERT to work with asynchronous tests.")
    (license license:gpl3+)))

(define-public emacs-prodigy-el
  (let ((commit "701dccaa56de9e6a330c05bde33bce4f3b3d6a97")
        (version "0.7.0")
        (revision "28"))
    (package
      (name "emacs-prodigy-el")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rejeep/prodigy.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1vyvxawlayp2nra0q83146q2nzv8qwn5a4nj0sx1jc90a0a83vgj"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-s" ,emacs-s)
         ("emacs-f" ,emacs-f)))
      (native-inputs
       `(("emacs-el-mock" ,emacs-el-mock)
         ("emacs-ert-async" ,emacs-ert-async)))
      (home-page "https://github.com/rejeep/prodigy.el")
      (synopsis "Manage external services from within Emacs")
      (description "This package provides a GUI for defining and monitoring services.")
      (license license:gpl3+))))

(define-public emacs-web-server
  (let ((commit "cafa5b7582c57252a0884b2c33da9b18fb678713")
        (version "0.1.1")
        (revision "1"))
    (package
      (name "emacs-web-server")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eschulte/emacs-web-server/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1c0lfqmbs5hvz3fh3c8wgp6ipwmxrwx9xj264bjpj3phixd5419y"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/eschulte/emacs-web-server/")
      (synopsis "Web server with handlers in Emacs Lisp")
      (description "This package supports HTTP GET and POST requests with
url-encoded parameters, as well as web sockets.")
      (license license:gpl3+))))

(define-public emacs-markdown-preview-mode
  (package
    (name "emacs-markdown-preview-mode")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ancane/markdown-preview-mode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1d1id99gagymvzdfa1mwqh8y3szm8ii47rpijkfi1qnifjg5jaq9"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-markdown-mode" ,emacs-markdown-mode)
       ("emacs-websocket" ,emacs-websocket)
       ("emacs-web-server" ,emacs-web-server)))
    (arguments '(#:include '("\\.el$" "\\.html$")))
    (home-page "https://github.com/ancane/markdown-preview-mode")
    (synopsis "Live web development in Emacs")
    (description "This package provides a minor mode for preview of Markdown
files, and sends rendered Markdown to a web browser.")
    (license license:gpl3+)))

(define-public emacs-dotenv-mode
  (package
    (name "emacs-dotenv-mode")
    (version "0.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/preetpalS/emacs-dotenv-mode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1fplkhxnsgdrg10iqsmw162zny2idz4vvv35spsb9j0hsk8imclc"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/preetpalS/emacs-dotenv-mode")
    (synopsis "Major mode for @file{.env} files")
    (description "This package provides syntax highlighting for @file{.env}
files.")
    (license license:gpl3+)))

(define-public emacs-add-node-modules-path
  (let ((commit "f31e69ccb681f882aebb806ce6e9478e3ac39708")
        (version "1.2.0")
        (revision "10"))
    (package
      (name "emacs-add-node-modules-path")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/codesuki/add-node-modules-path")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0p106bqmvdr8by5iv02bshm339qbrjcch2d15mrm4h3nav03v306"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/codesuki/add-node-modules-path")
      (synopsis "Add @file{node_modules} to the buffer-local @file{exec-path}")
      (description "This package searches the parent directories of the
current file for the project's @file{node_modules/.bin/} directory, allowing
Emacs to find project-specific installations of packages.")
      (license license:expat))))

(define-public emacs-flow-minor-mode
  (let ((commit "d1b32a7dd0d33c6a00a106da5f4b2323602cbd3e")
        (version "0.3")
        (revision "4"))
    (package
      (name "emacs-flow-minor-mode")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/an-sh/flow-minor-mode")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "169r4ib9qg9q6fm3p0p23qs1qx4pa9pg1qvyq4ysr85i7kwygppl"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/an-sh/flow-minor-mode")
      (synopsis "Minor mode for JavaScript Flow files")
      (description "This package integrates Flow with Emacs, allowing for
definition-jumping and type-checking on demand.")
      (license license:bsd-3))))

(define-public emacs-rjsx-mode
  (let ((commit "03dd8d1683501e81b58674d64c3032b7b718402c")
        (version "0.4.0")
        (revision "35"))
    (package
      (name "emacs-rjsx-mode")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/felipeochoa/rjsx-mode")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1kc44g9f38klpjklmz9n50a28nqv7prz6ck6ghdr6bnj1s98pb8a"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-js2-mode" ,emacs-js2-mode)))
      (home-page "https://github.com/felipeochoa/rjsx-mode")
      (synopsis "Major mode for JSX files")
      (description "This package extends the parser of @code{js2-mode} to
support JSX syntax.")
      (license license:expat))))

(define-public emacs-origami-el
  (let ((commit "1f38085c8f9af7842765ed63f7d6dfe4dab59366")
        (version "1.0")
        (revision "1"))
    (package
      (name "emacs-origami-el")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gregsexton/origami.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0ha1qsz2p36pqa0sa2sp83lspbgx5lr7930qxnwd585liajzdd9x"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-s" ,emacs-s)))
      (home-page "https://github.com/gregsexton/origami.el")
      (synopsis "Flexible text-folding")
      (description "This package provides a minor mode for collapsing and
expanding regions of text without modifying the actual contents.")
      (license license:expat))))

(define-public emacs-peep-dired
  (let ((commit "c88a9a3050197840edfe145f11e0bb9488de32f4")
        (version "0")
        (revision "1"))
    (package
      (name "emacs-peep-dired")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/asok/peep-dired")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1wy5qpnfri1gha2cnl6q20qar8dbl2mimpb43bnhmm2g3wgjyad6"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/asok/peep-dired")
      (synopsis "Preview files in another window")
      (description "This package provides a minor mode that allows files to be
previewed by scrolling up and down within a @code{dired} buffer.")
      (license license:gpl3+))))

(define-public emacs-counsel-etags
  (package
    (name "emacs-counsel-etags")
    (version "1.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/redguardtoo/counsel-etags")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1d8nlrbsyza6q7yqm9248bxxsf49qf6hchg3zwv0l11acn3w8np5"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-ivy" ,emacs-ivy)))
    (home-page "https://github.com/redguardtoo/counsel-etags")
    (synopsis "Fast @code{Ctags}/@code{Etags} solution with @code{ivy-mode}")
    (description "This package uses @code{ivy-mode} to facilitate navigating
and searching through @code{Ctags} files.")
    (license license:gpl3+)))

(define-public emacs-helm-dash
  (let ((commit "192b862185df661439a06de644791171e899348a")
        (version "1.3.0")
        (revision "18"))
    (package
      (name "emacs-helm-dash")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/areina/helm-dash")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "06am5vnr4hsxkvh2b8q8kb80y5x1h3qyv7gwggswwhfa7w2vba3w"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)))
      (home-page "https://github.com/areina/helm-dash")
      (synopsis "Offline documentation browser for APIs using Dash docsets")
      (description "This package uses Helm to install and navigate through
Dash docsets.")
      (license license:gpl3+))))

(define-public emacs-counsel-dash
  (let ((commit "07fa74a94ff4da5b6c8c4810f5e143e701b480d2")
        (version "0.1.3")
        (revision "3"))
    (package
      (name "emacs-counsel-dash")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nathankot/counsel-dash")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "17h2m9zsadq270mkq12kmdzmpbfjiwjbg8n1rg2apqnm1ndgcwf8"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm-dash" ,emacs-helm-dash)
         ("emacs-dash" ,emacs-dash)
         ("emacs-ivy" ,emacs-ivy)))
      (home-page "https://github.com/nathankot/counsel-dash")
      (synopsis "Offline documentation browser for APIs using Dash docsets")
      (description "This package uses @code{ivy-mode} to install and navigate
through Dash docsets.")
      (license license:expat))))

(define-public emacs-el-patch
  (package
    (name "emacs-el-patch")
    (version "2.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/raxod502/el-patch")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "18djslz177q6q33y82zmg2v6n9236a76kiqfvxdk4vbqzjbq82f7"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/raxod502/el-patch")
    (synopsis "Future-proof your Emacs customizations")
    (description "This package allows for an alternate definition of an Elisp
function to be specified and for any differences from the original definition
to be examined using Ediff.")
    (license license:expat)))

(define-public emacs-info-plus
  (let ((commit "b837d710f7d58db586116cf6f75e75a9a074bc4b")
        (version "5101")
        (revision "55"))
    (package
      (name "emacs-info-plus")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacsmirror/info-plus")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1knyjkdm4rcs3qrc51jllw46ph7ycq5zxnvl70ydchzfwava43h6"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/emacsmirror/info-plus")
      (synopsis "Extensions to @file{info.el}")
      (description "This package extends Emacs' @file{info.el} by allowing
outline-enabled table of contents, additional metadata association for Info
nodes, and more.")
      (license license:gpl2+))))

(define-public emacs-eval-sexp-fu-el
  (package
    (name "emacs-eval-sexp-fu-el")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hchbaw/eval-sexp-fu.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01mpnpgmlnfbi2yw9dxz5iw72mw3lk223bj172i4fnx3xdrrxbij"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/hchbaw/eval-sexp-fu.el")
    (synopsis "Enhancements for evaluating s-expressions")
    (description "This package provides provides variants of
@code{eval-last-sexp} that work on the containing list or s-expression, as
well as an option for visually flashing evaluated s-expressions.")
    (license license:gpl3+)))

(define-public emacs-counsel-tramp
  (package
    (name "emacs-counsel-tramp")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/masasam/emacs-counsel-tramp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0nz0733x2b9b5nkwivvhv5c8747dng451na1sdfbkx5x9fjs5gc7"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-ivy" ,emacs-ivy)))
    (home-page "https://github.com/masasam/emacs-counsel-tramp")
    (synopsis "Ivy interface for TRAMP")
    (description "This package allows @code{ivy-mode} to display and filter
SSH servers.")
    (license license:gpl3+)))

(define-public emacs-eacl
  (package
    (name "emacs-eacl")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/redguardtoo/eacl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ksn11sm3g1ja5lpjz3hrzzw8b480mfcb3q589m52qjgvvn5iyfv"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-ivy" ,emacs-ivy)))
    (home-page "https://github.com/redguardtoo/eacl")
    (synopsis "Auto-complete lines by using @code{grep} on a project")
    (description "This package provides auto-completion cammands for single
and multiple lines of code in a project.")
    (license license:gpl3+)))

(define-public emacs-semantic-refactor
  ;; The last release, 0.5, was made on 2015-07-26 and there have been 47
  ;; commits since then.
  (let ((commit "6f2c97d17fb70f4ca2112f5a2b99a8ec162004f5"))
    (package
      (name "emacs-semantic-refactor")
      (version (git-version "0.5" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tuhdo/semantic-refactor")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1lyz3zjkx2csh0xdy1zpx8s32qp1p3sig57mwi9xhgpqjyf0axmb"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/tuhdo/semantic-refactor")
      (synopsis "Refactoring tool for C/C++ and Lisp dialects")
      (description "This package provides a refactoring tool based on the
Emacs Semantic parser framework.  For C and C++ it supports operations such as:

@enumerate
@item Generating class implementations
@item Generating function prototypes
@item Converting functions to function pointers
@item Moving semantic units
@item etc...
@end enumerate

For Lisp dialects like Clojure, ELisp, and Scheme, it supports operations such
as:

@enumerate
@item Formatting the whole buffer
@item Converting sexpressions to one or multiple lines
@item etc...
@end enumerate\n")
      (license license:gpl3+))))

(define-public emacs-nhexl-mode
  (package
    (name "emacs-nhexl-mode")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://elpa.gnu.org/packages/nhexl-mode-"
             version ".el"))
       (sha256
        (base32
         "031h22p564qdvr9khs05qcba06pmsk68cr7zyc7c04hfr3y3ziaf"))))
    (build-system emacs-build-system)
    (home-page "http://elpa.gnu.org/packages/nhexl-mode.html")
    (synopsis "Minor mode to edit files via hex-dump format")
    (description
     "This package implements NHexl mode, a minor mode for editing files
in hex dump format.  The mode command is called @command{nhexl-mode}.

This minor mode implements similar functionality to @command{hexl-mode},
but using a different implementation technique, which makes it
usable as a \"plain\" minor mode.  It works on any buffer, and does
not mess with the undo log or with the major mode.

It also comes with:

@itemize
@item @command{nhexl-nibble-edit-mode}: a \"nibble editor\" minor mode, where
the cursor pretends to advance by nibbles (4-bit) and the self-insertion keys
(which only work for hex-digits) will only modify the nibble under point.
@item @command{nhexl-overwrite-only-mode}: a minor mode to try and avoid
moving text.  In this minor mode, not only self-inserting keys overwrite
existing text, but commands like `yank' and @command{kill-region} as well.
@item It overrides @code{C-u} to use hexadecimal, so you can do @code{C-u a 4
C-f} to advance by #xa4 characters.
@end itemize\n")
    (license license:gpl3+)))

(define-public emacs-helm-wikipedia
  (let ((commit "126f044e0a0e1432e0791c33ce2a41875d704a9b"))
    (package
      (name "emacs-helm-wikipedia")
      (version (git-version "0.0.0" "1" commit))
      (home-page "https://github.com/emacs-helm/helm-wikipedia/")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "148a5xsnbsiddhf9cl7yxdk41lrv38h0pip91kcflw9d7l0dp7pr"))))
      (build-system emacs-build-system)
      (inputs
       `(("helm" ,emacs-helm)))
      (synopsis "Search suggestions and article extracts from Wikipedia for Emacs")
      (description
       "This package provides an Emacs Helm interface for search suggestions
and article extracts for Wikipedia.")
      (license license:gpl3+))))

(define-public emacs-webfeeder
  (package
  (name "emacs-webfeeder")
  (version "1.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://elpa.gnu.org/packages/webfeeder-"
             version
             ".tar"))
      (sha256
        (base32
          "06y5vxw9m6pmbrzb8v2i3w9dnhgqxz06vyx1knmgi9cczlrj4a64"))))
  (build-system emacs-build-system)
  (home-page "https://gitlab.com/Ambrevar/emacs-webfeeder")
  (synopsis "Build RSS and Atom webfeeds from HTML files")
  (description
    "Webfeeder is an Emacs library to generate RSS and Atom feeds from HTML
files.  The various elements of the HTML input are parsed with customizable
functions (e.g. @code{webfeeder-title-function}).")
  (license license:gpl3+)))

(define-public emacs-evil-numbers
  (let ((commit "6ea1c8c3a9b37bed63d48f1128e9a4910e68187e"))
    (package
      (name "emacs-evil-numbers")
      (version (git-version "0.4" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dieggsy/evil-numbers")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1aq95hj8x13py0pwsnc6wvd8cc5yv5qin8ym9js42y5966vwj4np"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/dieggsy/evil-numbers")
      (synopsis "Increment and decrement numbers in Emacs")
      (description "This package increments and decrements binary, octal,
decimal and hex literals.  It works like C-a/C-x in Vim, i.e. searches for
number up to EOL and then increments or decrements and keeps zero padding up
(unlike in Vim).  When a region is active, as in Evil's visual mode, all the
numbers within that region will be incremented/decremented (unlike in Vim).

It is a fork of https://github.com/cofi/evil-numbers, which is unmaintained.")
      (license license:gpl3+))))
