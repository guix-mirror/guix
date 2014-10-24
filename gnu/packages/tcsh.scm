;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
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

(define-module (gnu packages tcsh)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages ncurses))

(define-public tcsh
  (package
    (name "tcsh")
    (version "6.18.01")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp.astron.com/pub/tcsh/tcsh-"
                                 version ".tar.gz"))
             (sha256
              (base32 "1a4z9kwgx1iqqzvv64si34m60gj34p7lp6rrcrb59s7ka5wa476q"))
             (patches (list (search-patch "tcsh-fix-autotest.patch")))
             (patch-flags '("-p0"))))
    (build-system gnu-build-system)
    (inputs
     `(("autoconf" ,autoconf)
       ("coreutils" ,coreutils)
       ("ncurses" ,ncurses)))
    (arguments
     `(#:phases
       (alist-cons-before
        'check 'patch-test-scripts
        (lambda _
          ;; Take care of pwd
          (substitute* '("tests/commands.at" "tests/variables.at")
            (("/bin/pwd") (which "pwd")))
          ;; The .at files create shell scripts without shebangs. Erk.
          (substitute* "tests/commands.at"
            (("./output.sh") "/bin/sh output.sh"))
          (substitute* "tests/syntax.at"
            (("; other_script.csh") "; /bin/sh other_script.csh"))
          ;; Now, let's generate the test suite and patch it
          (system* "make" "tests/testsuite")
          (substitute* "tests/testsuite" (("/bin/sh") (which "sh"))))
        (alist-cons-after
         'install 'post-install
         (lambda* (#:key inputs outputs #:allow-other-keys)
          (let* ((out (assoc-ref %outputs "out"))
                 (bin (string-append out "/bin")))
           (with-directory-excursion bin
             (symlink "tcsh" "csh"))))
         %standard-phases))))
    (home-page "http://www.tcsh.org/")
    (synopsis "Unix shell based on csh")
    (description
     "Tcsh is an enhanced, but completely compatible version of the Berkeley
UNIX C shell (csh).  It is a command language interpreter usable both as an
interactive login shell and a shell script command processor.  It includes a
command-line editor, programmable word completion, spelling correction, a
history mechanism, job control and a C-like syntax.")
    (license bsd-4)))
