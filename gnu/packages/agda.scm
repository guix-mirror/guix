;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Alex ter Weele <alex.ter.weele@gmail.com>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages agda)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public agda
  (package
    (name "agda")
    (version "2.6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/Agda/Agda-"
             version ".tar.gz"))
       (sha256
        (base32
         "1s600ry1qwizr3ynyj05rvlx7jdcw9a1viyc0ycjamm5sjf8mf3v"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-alex" ,ghc-alex)
       ("ghc-async" ,ghc-async)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-boxes" ,ghc-boxes)
       ("ghc-data-hash" ,ghc-data-hash)
       ("ghc-edisoncore" ,ghc-edisoncore)
       ("ghc-edit-distance" ,ghc-edit-distance)
       ("ghc-equivalence" ,ghc-equivalence)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-filemanip" ,ghc-filemanip)
       ("ghc-geniplate-mirror" ,ghc-geniplate-mirror)
       ("ghc-gitrev" ,ghc-gitrev)
       ("ghc-happy" ,ghc-happy)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-hashtables" ,ghc-hashtables)
       ("ghc-ieee754" ,ghc-ieee754)
       ("ghc-murmur-hash" ,ghc-murmur-hash)
       ("ghc-uri-encode" ,ghc-uri-encode)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-strict" ,ghc-strict)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-zlib" ,ghc-zlib)))
    (arguments
     `(#:modules ((guix build haskell-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         ;; FIXME: This is a copy of the standard configure phase with a tiny
         ;; difference: this package needs the -package-db flag to be passed
         ;; to "runhaskell" in addition to the "configure" action, because
         ;; Setup.hs depends on filemanip.  Without this option the Setup.hs
         ;; file cannot be evaluated.  The haskell-build-system should be
         ;; changed to pass "-package-db" to "runhaskell" in any case.
         (replace 'configure
           (lambda* (#:key outputs inputs tests? (configure-flags '())
                     #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (name-version (strip-store-file-name out))
                    (input-dirs (match inputs
                                  (((_ . dir) ...)
                                   dir)
                                  (_ '())))
                    (ghc-path (getenv "GHC_PACKAGE_PATH"))
                    (params (append `(,(string-append "--prefix=" out))
                                    `(,(string-append "--libdir=" out "/lib"))
                                    `(,(string-append "--bindir=" out "/bin"))
                                    `(,(string-append
                                        "--docdir=" out
                                        "/share/doc/" name-version))
                                    '("--libsubdir=$compiler/$pkg-$version")
                                    '("--package-db=../package.conf.d")
                                    '("--global")
                                    `(,@(map
                                         (cut string-append "--extra-include-dirs=" <>)
                                         (search-path-as-list '("include") input-dirs)))
                                    `(,@(map
                                         (cut string-append "--extra-lib-dirs=" <>)
                                         (search-path-as-list '("lib") input-dirs)))
                                    (if tests?
                                        '("--enable-tests")
                                        '())
                                    configure-flags)))
               (unsetenv "GHC_PACKAGE_PATH")
               (apply invoke "runhaskell" "-package-db=../package.conf.d"
                      "Setup.hs" "configure" params)
               (setenv "GHC_PACKAGE_PATH" ghc-path)
               #t)))
         (add-after 'compile 'agda-compile
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (agda-compiler (string-append out "/bin/agda")))
               (for-each (cut invoke agda-compiler <>)
                         (find-files (string-append out "/share") "\\.agda$"))
               #t))))))
    (home-page "https://wiki.portal.chalmers.se/agda/")
    (synopsis
     "Dependently typed functional programming language and proof assistant")
    (description
     "Agda is a dependently typed functional programming language: it has
inductive families, which are similar to Haskell's GADTs, but they can be
indexed by values and not just types.  It also has parameterised modules,
mixfix operators, Unicode characters, and an interactive Emacs interface (the
type checker can assist in the development of your code).  Agda is also a
proof assistant: it is an interactive system for writing and checking proofs.
Agda is based on intuitionistic type theory, a foundational system for
constructive mathematics developed by the Swedish logician Per Martin-Löf.  It
has many similarities with other proof assistants based on dependent types,
such as Coq, Epigram and NuPRL.")
    ;; Agda is distributed under the MIT license, and a couple of
    ;; source files are BSD-3.  See LICENSE for details.
    (license (list license:expat license:bsd-3))))

(define-public emacs-agda2-mode
  (package
    (inherit agda)
    (name "emacs-agda2-mode")
    (build-system emacs-build-system)
    (inputs '())
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-elisp-dir
           (lambda _ (chdir "src/data/emacs-mode") #t)))))
    (home-page "https://agda.readthedocs.io/en/latest/tools/emacs-mode.html")
    (synopsis "Emacs mode for Agda")
    (description "This Emacs mode enables interactive development with
Agda.  It also aids the input of Unicode characters.")))

(define-public agda-ial
  (package
    (name "agda-ial")
    (version "1.5.0")
    (home-page "https://github.com/cedille/ial")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dlis6v6nzbscf713cmwlx8h9n2gxghci8y21qak3hp18gkxdp0g"))))
    (build-system gnu-build-system)
    (inputs
     `(("agda" ,agda)))
    (arguments
     `(#:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-dependencies
           (lambda _ (patch-shebang "find-deps.sh") #t))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (include (string-append out "/include/agda/ial")))
               (for-each (lambda (file)
                           (make-file-writable file)
                           (install-file file include))
                         (find-files "." "\\.agdai?(-lib)?$"))
               #t))))))
    (synopsis "The Iowa Agda Library")
    (description
     "The goal is to provide a concrete library focused on verification
examples, as opposed to mathematics.  The library has a good number
of theorems for booleans, natural numbers, and lists.  It also has
trees, tries, vectors, and rudimentary IO.  A number of good ideas
come from Agda's standard library.")
    (license license:expat)))
