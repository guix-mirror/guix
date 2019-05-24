;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2016, 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
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

(define-module (gnu packages shellutils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python))

(define-public envstore
  (package
    (name "envstore")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://finalrewind.org/projects/"
                           name "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "1x97lxad80m5blhdfanl5v2qzjwcgbij2i23701bn8mpyxsrqszi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://finalrewind.org/projects/envstore/")
    (synopsis "Save and restore environment variables")
    (description "Envstore is a program for sharing environment variables
between various shells or commands.")
    (license license:wtfpl2)))

(define-public trash-cli
  (package
    (name "trash-cli")
    (version "0.17.1.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trash-cli" version))
       (sha256
        (base32
         "01q0cl04ljf214z6s3g256gsxx3pqsgaf6ac1zh0vrq5bnhnr85h"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-path-constants
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libc (assoc-ref inputs "libc"))
                   (coreutils (assoc-ref inputs "coreutils")))
               (substitute* "trashcli/list_mount_points.py"
                 (("\"/lib/libc.so.6\".*")
                  (string-append "\"" libc "/lib/libc.so.6\"\n"))
                 (("\"df\"")
                  (string-append "\"" coreutils "/bin/df\"")))))))))
    (inputs `(("coreutils" ,coreutils)))
    (home-page "https://github.com/andreafrancia/trash-cli")
    (synopsis "Trash can management tool")
    (description
     "trash-cli is a command line utility for interacting with the
FreeDesktop.org trash can used by GNOME, KDE, XFCE, and other common desktop
environments.  It can move files to the trash, and remove or list files that
are already there.")
    (license license:gpl2+)))

(define-public direnv
  (package
    (name "direnv")
    (version "2.15.2")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/direnv/direnv.git")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1y18619pmhfl0vrf4w0h75ybkkwgi9wcb7d9kv4n8drg1xp4aw4w"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/direnv/direnv"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-vendor
           (lambda _
             ;; Using a snippet causes issues with the name of the directory,
             ;; so delete the extra source code here.
             (delete-file-recursively "src/github.com/direnv/direnv/vendor")
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" "/tmp")
               (with-directory-excursion "src/github.com/direnv/direnv"
                 ;; The following file needs to be writable so it can be
                 ;; modified by the testsuite.
                 (make-file-writable "test/scenarios/base/.envrc")
                 (invoke "make" "test")
                 ;; Clean up from the tests, especially so that the extra
                 ;; direnv executable that's generated is removed.
                 (invoke "make" "clean")))
             #t)))))
    (native-inputs
     `(("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
       ("go-github-com-direnv-go-dotenv" ,go-github-com-direnv-go-dotenv)
       ("which" ,which)))
    (home-page "https://direnv.net/")
    (synopsis "Environment switcher for the shell")
    (description
     "direnv can hook into the bash, zsh, tcsh, and fish shells to load
or unload environment variables depending on the current directory.  This
allows project-specific environment variables without using @file{~/.profile}.

Before each prompt, direnv checks for the existence of a @file{.envrc} file in
the current and parent directories.  This file is then used to alter the
environment variables of the current shell.")
    (license license:expat)))

(define-public fzy
  (package
    (name "fzy")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jhawthorn/fzy.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1gkzdvj73f71388jvym47075l9zw61v6l8wdv2lnc0mns6dxig0k"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/jhawthorn/fzy")
    (synopsis "Fast fuzzy text selector for the terminal with an advanced
scoring algorithm")
    (description
     "Most other fuzzy matchers sort based on the length of a match.  fzy tries
to find the result the user intended.  It does this by favouring matches on
consecutive letters and starts of words.  This allows matching using acronyms
or different parts of the path.

fzy is designed to be used both as an editor plugin and on the command
line.  Rather than clearing the screen, fzy displays its interface directly
below the current cursor position, scrolling the screen if necessary.")
    (license license:expat)))

(define-public hstr
  (package
    (name "hstr")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/dvorka/" name "/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0yk2008bl48hv0v3c90ngq4y45h3nxif2ik6s3l7kag1zs5yv4wd"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'adjust-ncurses-includes
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "src/include/hstr_curses.h"
                 (("ncursesw\\/curses.h") "ncurses.h"))
               (substitute* "src/include/hstr.h"
                 (("ncursesw\\/curses.h") "ncurses.h")))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("ncurses" ,ncurses)
       ("readline" ,readline)))
    (synopsis "Navigate and search command history with shell history suggest box")
    (description "HSTR (HiSToRy) is a command-line utility that brings
improved Bash and Zsh command completion from the history.  It aims to make
completion easier and more efficient than with @kbd{Ctrl-R}.  It allows you to
easily view, navigate, and search your command history with suggestion boxes.
HSTR can also manage your command history (for instance you can remove
commands that are obsolete or contain a piece of sensitive information) or
bookmark your favourite commands.")
    (home-page "http://me.mindforger.com/projects/hh.html")
    (license license:asl2.0)))
