;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages ncurses))


(define-public tmux
  (package
    (name "tmux")
    (version "2.8")
    (source (origin
             (method url-fetch)
             (uri (string-append
                    "https://github.com/tmux/tmux/releases/download/"
                    version "/tmux-" version ".tar.gz"))
             (sha256
              (base32
               "1fm22wva2znkcs5jczrnlxrpzsk2jlwdwy4dhzzyrbsgccsz6svz"))))
    (build-system gnu-build-system)
    (inputs
     `(("libevent" ,libevent)
       ("ncurses" ,ncurses)))
    (home-page "http://tmux.github.io/")
    (synopsis "Terminal multiplexer")
    (description
     "tmux is a terminal multiplexer: it enables a number of terminals (or
windows), each running a separate program, to be created, accessed, and
controlled from a single screen.  tmux may be detached from a screen and
continue running in the background, then later reattached.")
    (license isc)))

(define-public tmux-themepack
  (let ((commit "03a372866f7677f7fe63bcee140b48b9fd372c48")
        (revision "1"))
    (package
      (name "tmux-themepack")
      (version (git-version "0.0.0" revision commit)) ; no version tags
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jimeh/tmux-themepack.git")
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
      (license
       (non-copyleft "http://www.wtfpl.net/txt/copying/")))))

(define-public tmuxifier
  (package
    (name "tmuxifier")
    (version "0.13.0")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/jimeh/tmuxifier.git")
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
    (license expat)))
