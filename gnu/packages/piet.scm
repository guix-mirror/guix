;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Jesse Gibbons <jgibbons2357+guix@gmail.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages piet)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages image)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl))

(define-public npiet
  (package
    (name "npiet")
    (version "1.3f")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.bertnase.de/npiet/npiet-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0nl59fhdqqr7nslxdirdn8nvlq5wws67c7jyx2ckbmxbc9h8bv9d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-binaries
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/npietedit")
                 `("PATH" ":" prefix (,(dirname (which "wish")))))
               #t))))))
    (inputs
     (list gd giflib libpng tk))
    (native-inputs (list groff))
    (synopsis "Piet interpreter")
    (description
     "Npiet is an interpreter for the Piet programming language.  Instead of
text, Piet programs are pictures.  Commands are determined based on changes in
color.

This package includes:
@enumerate
@item @command{npiet}, a Piet interpreter with debugging capabilities
@item @command{npiet-foogol}, a program that builds a Piet program from Foogol,
an Algol-like language
@item @command{npietedit}, an editor for Piet programs.
@end enumerate\n")
    (home-page "https://www.bertnase.de/npiet/")
    (license license:gpl2+)))

(define-public piet-toolchain
  (let ((commit "f002ff6a924a6bbace5eef94f3be06f425e7f590")
        (revision "0"))
    (package
      (name "piet-toolchain")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sl236/Piet")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0xwbhwizfbn080fmrgavaz3b939brycmlar3m5px9avl2b68c816"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Remove a bundled fork of Marc Majcher's Piet interpreter.
             (delete-file-recursively "interpreter")
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-26))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (delete 'build)              ; nothing to build
           (delete 'check)              ; run our own tests below
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (doc (string-append out "/share/doc/"
                                          ,name "-" ,version)))
                 (for-each (lambda (script)
                             (install-file script bin)
                             (wrap-program (string-append bin "/" script)
                               `("PERL5LIB" ":" = (,(getenv "PERL5LIB")))))
                           (list "piet-assembler"
                                 "piet-compiler"))

                 ;; Fix an odd mode.
                 (chmod "compiler-samples/test-binary-ops.script" #o644)
                 (for-each (lambda (file)    ; INSTALL-FILE is not recursive
                             (copy-recursively file
                                               (string-append doc "/" file)))
                           (list "assembler-samples"
                                 "compiler-samples"
                                 "README.md")) ; includes the licence grant
                 #t)))
           (add-after 'install 'check
             (lambda* (#:key outputs tests? #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (when tests?
                   (unsetenv "PERL5LIB") ; test the wrapping
                   ;; Compile all scripts assemble all Piets.
                   (for-each (lambda (file)
                               (system (string-append bin "/piet-compiler "
                                                      file ">"
                                                      file ".piet")))
                             (find-files "." "\\.script$"))
                   (for-each (lambda (file)
                               (system (string-append bin "/piet-assembler "
                                                      file "|pnmtopng>"
                                                      file ".png")))
                             (find-files "." "\\.piet$"))

                   ;; Don't run the interactive one.
                   (delete-file "assembler-samples/quest.piet.png")
                   (for-each (cut invoke "npiet" <>)
                             (find-files "." "\\.png$"))
                   #t)))))))
      (native-inputs
       ;; For our tests.
       (list netpbm npiet))
      (inputs
       (list perl perl-parse-recdescent))
      (home-page "https://www.toothycat.net/wiki/wiki.pl?MoonShadow/Piet")
      (synopsis "Piet compiler and assembler")
      (description
       "This package provides a compiler and assembler that target the Piet
graphical programming language.

@command{piet-assembler} converts Piet assembler instructions (e.g.,
@code{push}, @code{add}, @code{switch}, @code{outn}) and directives into an
executable @code{netpbm} image of the corresponding Piet program.

@command{piet-compiler} compiles a C-like high-level language into assembly
source understood by @command{piet-assembler}.  It supports common arithmetic
and boolean logic operators (though not bitwise manipulation), flow control
(@code{if}, @code{for}, @code{while}), recursive functions, in-line assembler,
and input/output intrinsics.  The only supported data type is the integer.

The language is documented only by the compiler's Perl source code and the
included samples.")
      (license license:cc-by-sa4.0))))
