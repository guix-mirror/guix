;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
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

(define-module (gnu packages tmux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages sphinx))

(define-public tmux
  (package
    (name "tmux")
    (version "3.1c")
    (source (origin
             (method url-fetch)
             (uri (string-append
                    "https://github.com/tmux/tmux/releases/download/"
                    version "/tmux-" version ".tar.gz"))
             (sha256
              (base32
               "11l3r337ly2wxwdrgjg19bdxkbqpagqaykrdj2hk7vvv8hh753wi"))))
    (build-system gnu-build-system)
    (inputs
     `(("libevent" ,libevent)
       ("ncurses" ,ncurses)))
    (home-page "https://tmux.github.io/")
    (synopsis "Terminal multiplexer")
    (description
     "tmux is a terminal multiplexer: it enables a number of terminals (or
windows), each running a separate program, to be created, accessed, and
controlled from a single screen.  tmux may be detached from a screen and
continue running in the background, then later reattached.")
    (license license:isc)))

(define-public tmux-themepack
  (let ((commit "03a372866f7677f7fe63bcee140b48b9fd372c48")
        (revision "1"))
    (package
      (name "tmux-themepack")
      (version (git-version "0.0.0" revision commit)) ; no version tags
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jimeh/tmux-themepack")
                      (commit commit)))
                (sha256
                 (base32
                  "1d3k87mq5lca042jbap5kxskjy3kg79wjhhpnm6jacbn3anc67zl"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no test suite
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    (delete 'build)
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let* ((out (string-append
                                     (assoc-ref outputs "out")
                                     "/share/" ,name "-" ,version)))
                          (copy-recursively "." out)))))))
      (home-page "https://github.com/jimeh/tmux-themepack")
      (synopsis "Collection of themes for Tmux")
      (description "A collection of various themes for Tmux.")
      (license license:wtfpl2))))

(define-public tmuxifier
  (package
    (name "tmuxifier")
    (version "0.13.0")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/jimeh/tmuxifier")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1b6a1cw2mnml84k5vhbcp58kvp94xlnlpp4kwdhqw4jrzfgcjfzd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'build)
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out    (assoc-ref %outputs "out"))
                             (bindir (string-append out "/bin"))
                             (share  (string-append out "/share/" ,name)))
                        (install-file "bin/tmuxifier" bindir)
                        (substitute* (string-append bindir "/tmuxifier")
                          (("set -e")
                           (string-append "TMUXIFIER=" share "\nset -e")))
                        (for-each (lambda (init-script)
                                    (install-file init-script (string-append
                                                               share "/init")))
                                  '("init.sh" "init.tcsh" "init.fish"))
                        (for-each (lambda (dir)
                                    (copy-recursively dir (string-append
                                                           share "/" dir)))
                                  '("completion" "lib" "libexec"
                                    "templates"))))))))
    (home-page "https://github.com/jimeh/tmuxifier")
    (synopsis "Powerful session, window & pane management for Tmux")
    (description "Tmuxifier allows you to easily create, edit, and load
@code{layout} files, which are simple shell scripts where you use the tmux
command and helper commands provided by tmuxifier to manage Tmux sessions and
windows.")
    (license license:expat)))

(define-public python-libtmux
  (package
    (name "python-libtmux")
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       ;; PyPI source tarball does not include tests.
       (uri (git-reference
             (url "https://github.com/tmux-python/libtmux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vrd99kl2gsk49mvbp6k7l1k7r96vf1fczsqclb62yd4hdpp7zaa"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("procps" ,procps)))             ;tests need top
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("tmux" ,tmux)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Extend PYTHONPATH so the built package will be found.
             (setenv "PYTHONPATH"
                     (string-append (getcwd) "/build/lib:"
                                    (getenv "PYTHONPATH")))
             ;; Fix <https://github.com/tmux-python/libtmux/issues/265>.
             (setenv "LANG" "en_US.utf8")
             ;; Skip tests that I suspect fail because of a change
             ;; in behavior in tmux 3 from tmux 2
             ;; https://github.com/tmux-python/libtmux/issues/281
             (invoke "pytest" "-vv" "-k"
                     (string-append "not test_show_option_unknown "
                                    "and not test_show_window_option_unknown"))
             #t)))))
    (home-page "https://github.com/tmux-python/libtmux")
    (synopsis "Python API for tmux")
    (description "Libtmux is the tool behind @command{tmuxp}, a tmux workspace
manager in Python.  It creates object mappings to traverse, inspect and interact
with live tmux sessions.")
    (license license:expat)))

(define-public python-daemux
  (package
    (name "python-daemux")
    (version "0.1.0")
    (source
     ;; We fetch from the Git repo because there are no tests in the PyPI
     ;; archive.
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edouardklein/daemux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cb8v552f2hkwz6d3hwsmrz3gd28jikga3lcc3r1zlw8ra7804ph"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (mkdir-p "tmptmux")
                      (setenv "TMUX_TMPDIR" (string-append (getcwd) "/tmptmux"))
                      (invoke "tmux" "new-session" "-d")
                      (invoke "make" "test"))))))
    (propagated-inputs
     `(("python-libtmux" ,python-libtmux)))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-sphinx" ,python-sphinx)
       ("tmux" ,tmux)))
    (home-page "https://github.com/edouardklein/daemux")
    (synopsis "Start, stop, restart and check daemons via tmux")
    (description
     "Daemux lets you run daemons in a @command{tmux} pane.  Users can launch
long-running background tasks, and check these tasks' health by hand, relaunch
them, etc., by attaching to the corresponding pane in tmux.")
    (license license:agpl3+)))

(define-public tmux-xpanes
  (package
    (name "tmux-xpanes")
    (version "4.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/greymd/tmux-xpanes")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09fmnn1q76r1l4cv7clmfr3j9cjmd053kq238d0qj2i486948ivv"))))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (setenv "PATH" (string-append (assoc-ref %build-inputs "bash") "/bin"))
         (copy-recursively (assoc-ref %build-inputs "source") ".")
         (substitute* "bin/xpanes"
           (("/bin/bash") (which "bash")))
         (install-file "bin/xpanes" (string-append %output "/bin"))
         (install-file "man/xpanes.1" (string-append %output "/man/man1"))
         #t)))
    (home-page "https://github.com/greymd/tmux-xpanes")
    (synopsis "Tmux based terminal divider")
    (description "This package provides tmux-based terminal divider.

@code{xpanes} or @code{tmux-xpanes} (alias of @code{xpanes}) commands have
following features:

@itemize
@item Split tmux window into multiple panes.
@item Build command lines & execute them on the panes.
@item Runnable from outside of tmux session.
@item Runnable from inside of tmux session.
@item Record operation log.
@item Flexible layout arrangement for panes.
@item Display pane title on each pane.
@item Generate command lines from standard input (Pipe mode).
@end itemize")
    (license license:expat)))
