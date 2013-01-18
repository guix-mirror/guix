;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages bash)
  #:use-module (guix licenses)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public bash
  (let ((cppflags (string-join '("-DSYS_BASHRC='\"/etc/bashrc\"'"
                                 "-DSYS_BASH_LOGOUT='\"/etc/bash_logout\"'"
                                 "-DDEFAULT_PATH_VALUE='\"/no-such-path\"'"
                                 "-DSTANDARD_UTILS_PATH='\"/no-such-path\"'"
                                 "-DNON_INTERACTIVE_LOGIN_SHELLS"
                                 "-DSSH_SOURCE_BASHRC")
                               " "))
        (post-install-phase
         '(lambda* (#:key outputs #:allow-other-keys)
            ;; Add a `bash' -> `sh' link.
            (let ((out (assoc-ref outputs "out")))
              (with-directory-excursion (string-append out "/bin")
                (symlink "bash" "sh"))))))
    (package
     (name "bash")
     (version "4.2")
     (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/bash/bash-" version ".tar.gz"))
              (sha256
               (base32
                "1n5kbblp5ykbz5q8aq88lsif2z0gnvddg9babk33024wxiwi2ym2"))))
     (build-system gnu-build-system)
     (inputs `(("readline" ,readline)
               ("ncurses" ,ncurses)))             ; TODO: add texinfo
     (arguments
      `(#:configure-flags `("--with-installed-readline"
                            ,,(string-append "CPPFLAGS=" cppflags)
                            ,(string-append
                              "LDFLAGS=-Wl,-rpath -Wl,"
                              (assoc-ref %build-inputs "readline")
                              "/lib"
                              " -Wl,-rpath -Wl,"
                              (assoc-ref %build-inputs "ncurses")
                              "/lib"))

        ;; Bash is reportedly not parallel-safe.  See, for instance,
        ;; <http://patches.openembedded.org/patch/32745/> and
        ;; <http://git.buildroot.net/buildroot/commit/?h=79e2d802ae7e376a413c02097790493e1f65c3a4>.
        #:parallel-build? #f
        #:parallel-tests? #f

        ;; XXX: The tests have a lot of hard-coded paths, so disable them
        ;; for now.
        #:tests? #f

        #:phases (alist-cons-after 'install 'post-install
                                   ,post-install-phase
                                   %standard-phases)))
     (synopsis "GNU Bourne-Again Shell")
     (description
      "Bash is the shell, or command language interpreter, that will appear in
the GNU operating system.  Bash is an sh-compatible shell that incorporates
useful features from the Korn shell (ksh) and C shell (csh).  It is intended
to conform to the IEEE POSIX P1003.2/ISO 9945.2 Shell and Tools standard.  It
offers functional improvements over sh for both programming and interactive
use.  In addition, most sh scripts can be run by Bash without
modification.")
     (license gpl3+)
     (home-page "http://www.gnu.org/software/bash/"))))

(define-public bash-light
  ;; A stripped-down Bash for non-interactive use.
  (package (inherit bash)
    (name "bash-light")
    (inputs '())                                ; no readline, no curses
    (arguments
     (let ((args `(#:modules ((guix build gnu-build-system)
                              (guix build utils)
                              (srfi srfi-1)
                              (srfi srfi-26))
                             ,@(package-arguments bash))))
       (substitute-keyword-arguments args
         ((#:configure-flags flags)
          `(list "--without-bash-malloc"
                 "--disable-readline"
                 "--disable-history"
                 "--disable-help-builtin"
                 "--disable-progcomp"
                 "--disable-net-redirections"
                 "--disable-nls")))))))
