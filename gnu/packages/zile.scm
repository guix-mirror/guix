;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages zile)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control))

(define-public zile
  (package
    (name "zile")
    (version "2.4.15")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/zile/zile-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0ph3wd0cz3ysdyka6ds2w5l5b89mb5l79kwkfyk7phvq9yih1hrr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-/bin/sh
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bash (assoc-ref inputs "bash")))
               ;; Refer to the actual shell.
               (substitute* '("lib/spawni.c" "src/funcs.c")
                 (("/bin/sh")
                  (string-append bash "/bin/sh")))
               #t)))
         ;; Zile generates its manual pages by calling the built Zile
         ;; with the --help argument.  That does not work when cross-
         ;; compiling; use the native Zile added below in that case.
         ,@(if (%current-target-system)
               '((add-before 'build 'use-native-zile-for-documentation
                   (lambda _
                     (substitute* "build-aux/zile-help2man-wrapper"
                       (("src/zile")
                        (which "zile")))
                     #t)))
               '()))))
    (inputs
     `(("boehm-gc" ,libgc)
       ("ncurses" ,ncurses)
       ("bash" ,bash)))
    (native-inputs
     `(("perl" ,perl)
       ("help2man" ,help2man)
       ;; When cross-compiling, Zile needs a native version of itself to
       ;; generate the manual pages (see the related phase above).
       ,@(if (%current-target-system)
             `(("self" ,this-package))
             '())
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.gnu.org/software/zile/")
    (synopsis "Lightweight Emacs clone")
    (description
     "GNU Zile is a lightweight Emacs clone.  It usage is similar to the
default Emacs configuration, but it carries a much lighter feature set.")
    (license gpl3+)))

(define-public zile-on-guile
  ;; This is a fork of Zile that uses Guile, announced here:
  ;; <http://lists.gnu.org/archive/html/guile-user/2012-02/msg00033.html>.
  (let ((commit "fd097811a60e58696d734c35b0eb7da3afc1adb7")
        (revision "0"))
    (package
      (inherit zile)
      (name "zile-on-guile")
      (version (string-append (package-version zile)
                              "-" revision "."
                              (string-take commit 7)))
      (home-page "https://github.com/spk121/zile")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)
                      (recursive? #t)))           ;for the Gnulib sub-module
                (sha256
                 (base32
                  "0wlli8hqal9ikmbl3a49kyhzyf164jk6mdbir3bclq2gxszs532d"))
                (file-name (string-append name "-" version "-checkout"))))
      (inputs
       `(("guile" ,guile-2.0)
         ,@(package-inputs zile)))
      (native-inputs
       `(("m4" ,m4)                               ;for 'bootstrap'
         ("autoconf" ,autoconf)
         ("automake" ,automake)

         ;; For some reason, 'bootstrap' insists on having these.
         ("git" ,git)
         ("gpg" ,gnupg)

         ,@(package-native-inputs zile)))
      (arguments
       (substitute-keyword-arguments (package-arguments zile)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'bootstrap
               (lambda _
                 ;; Make sure all the files are writable so that ./bootstrap
                 ;; can proceed.
                 (for-each (lambda (file)
                             (chmod file #o755))
                           (find-files "."))
                 (patch-shebang "gnulib/gnulib-tool")
                 (invoke "sh" "./bootstrap"
                         "--gnulib-srcdir=gnulib"
                         "--skip-git" "--skip-po"
                         "--verbose")))
             (add-after 'install 'wrap-command
               (lambda* (#:key outputs #:allow-other-keys)
                 ;; Add zile.scm to the search path.
                 (let* ((out    (assoc-ref outputs "out"))
                        (scheme (dirname
                                 (car (find-files out "^zile\\.scm$")))))
                   (wrap-program (string-append out "/bin/zile-on-guile")
                     `("GUILE_LOAD_PATH" ":" prefix (,scheme)))
                   #t)))))))
      (synopsis "Lightweight clone of the Emacs editor using Guile")
      (description
       "GNU Zile is a lightweight clone of the Emacs editor, and Zile-on-Guile
is a variant of Zile that can be extended in Guile Scheme.  Hitting
@kbd{M-C} (or: @kbd{Alt} and @kbd{C}) brings up a Guile REPL from which
interactive functions akin to those of Emacs can be invoked."))))
