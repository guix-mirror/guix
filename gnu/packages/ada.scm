;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages ada)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (ice-9 match))

(define-public ada/ed
  (package
    (name "ada-ed")
    (version "1.11.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             ;; The HOME-PAGE sources, mirrored by one of the original authors.
             (url "https://github.com/daveshields/AdaEd")
             (commit "57daecfb7ccadfd9aaf13b4d54f51065affbe599")))
       (sha256
        (base32 "1k97a8nqsvbsadizrmhhypcx758sxqkai8wq3ckk853qxvzaasd8"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (supported-systems (list "i686-linux" "x86_64-linux"
                             "armhf-linux" "aarch64-linux"
                             "powerpc-linux"))
    (outputs (list "out" "debug"))
    (arguments
     `(#:system
       ,@(match (%current-system)
           ;; This package predates 64-bit PCs: a ‘64-bit’ adaexec segfaults.
           ;; Force a 32-bit build targeting a similar architecture.
           ("aarch64-linux"
            `("armhf-linux"))
           ("x86_64-linux"
            `("i686-linux"))
           (_
            (list (%current-system))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append
                "CFLAGS=-g"             ; compile with :debug symbols
                " -DOP_SYS='\"GNU\"'"   ; sic; quoting gets mangled somewhere
                " -DSYSTEM_V"           ; closest to modern GNU
                " -DWORDSIZE32"
                " -DALIGN4")            ; suffices on both x86 and ARM
               "LFLAGS="                ; don't link against -lg
               (string-append "BINDIR=" out "/bin")
               (string-append "LIBDIR=" out "/lib")
               (string-append "MANDIR=" out "/share/man")))
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sources
           (lambda _
             ;; Rename the custom (and incompatible) getline() implementation.
             (substitute* "adalex.c"
               (("getline") "adaed_getline"))
             ;; Work around ‘error: initializer element is not constant’ by not
             ;; initialising MSGFILE.
             (substitute* "vars.ch"
               (("INIT\\(stdout\\)") ""))
             #t))
         (delete 'configure)            ; no configure script
         (add-before 'build 'find-build-scripts
           (lambda _
             (setenv "PATH" (string-append ".:" (getenv "PATH")))
             #t))
         (add-after 'build 'build-predef
           (lambda* (#:key make-flags #:allow-other-keys)
             ;; These aren't otherwise compiled until the ‘install’ phase.
             (apply invoke "make" "predef" make-flags)
             #t))
         (delete 'check)                ; no test suite; run our own below
         (add-before 'install 'create-output-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/share/man/manl"))
               #t)))
         (add-after 'install 'check
           ;; Run most of the included demos as our own ‘test suite’.
           (lambda* (#:key outputs tests? #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (when tests?
                 (setenv "ADAED" (string-append out "/lib"))
                 (setenv "PATH" (string-append out "/bin:" (getenv "PATH")))
                 (with-directory-excursion "demos" ; won't run outside of it
                   (for-each
                    delete-file
                    '("runc"   ; ‘invalid data. Please make it a positive no.’
                      "rund"   ; deadlocks by design
                      "rune"   ; ‘dining2.ada: No such file or directory’
                      "rung")) ; ‘mathlib cannot be used as a library’ (!)
                   (for-each (lambda (script)
                               (format #t "\n=== Invoking ~a ===\n" script)
                               (invoke script))
                             (find-files "." "^run")))))))
         (add-after 'install 'clean-up-output
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion out
                 ;; These needn't be executable.
                 (for-each (cut chmod <> #o644)
                           (append (find-files "lib" "\\....$")
                                   (find-files "share" ".")))
                 #t)))))))
    (native-inputs
     (list sed))
    (home-page (string-append "https://web.archive.org/web/20140902150609/"
                              "http://www2.informatik.uni-stuttgart.de/iste/ps/"
                              "ada-software/html/dos_ada.html"))
    (synopsis "Ada 83 interpreter")
    (description "Ada/Ed is a translator-interpreter for Ada 83.  It's intended
primarily as a teaching tool and lacks the capacity, performance, and robustness
of other contemporary or modern-day Ada compilers.

Ada/Ed was the first Ada compiler to pass the @acronym{ACVC, Ada Compiler
Validation Suite} version 1.7 but fails many newer tests and is not a validated
Ada system.  Being an interpreter, it does not implement most representation
clauses, and thus does not support systems programming close to the machine
level.")
    (license license:gpl2+)))

(define-public python2-langkit
  (let ((commit "fe0bc8bf60dbd2937759810df76ac420d99fc15f")
        (revision "0"))
    (package
      (name "python2-langkit")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/AdaCore/langkit")
                      (commit commit)))
                (sha256
                 (base32
                  "1abqgw2p8pb1pm54my5kkbbixfhc6l0bwajdv1xlzyrh31xki3wx"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system python-build-system)
      (propagated-inputs
       (list python2-docutils python2-enum34 python2-funcy python2-mako))
      (arguments
       `(#:python ,python-2
         #:tests? #f))           ; Tests would requite gprbuild (Ada).
      (synopsis "Semantic analysis tool generator in Python")
      (description "@code{Langkit} is a tool whose purpose is to make it easy
to create syntactic and semantic analysis engines.  Write a language
specification in our Python DSL and Langkit will generate for you an
Ada library with bindings for the C and Python programming languages.")
      (home-page "https://github.com/AdaCore/langkit/")
      (license license:gpl3+))))   ; and gcc runtime library exception
