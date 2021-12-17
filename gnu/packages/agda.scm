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
    (version "2.6.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/Agda/Agda-"
             version ".tar.gz"))
       (sha256
        (base32 "03dw7jfqr3ffik6avigm525djqh2gn5c3qwnb2h6298zkr9lch9w"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-aeson
           ghc-alex
           ghc-async
           ghc-blaze-html
           ghc-boxes
           ghc-case-insensitive
           ghc-data-hash
           ghc-edit-distance
           ghc-equivalence
           ghc-gitrev
           ghc-happy
           ghc-hashable
           ghc-hashtables
           ghc-monad-control
           ghc-murmur-hash
           ghc-parallel
           ghc-regex-tdfa
           ghc-split
           ghc-strict
           ghc-unordered-containers
           ghc-uri-encode
           ghc-zlib))
    (arguments
     `(#:modules ((guix build haskell-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         ;; This allows us to call the 'agda' binary before installing.
         (add-after 'unpack 'set-ld-library-path
           (lambda _
             (setenv "LD_LIBRARY_PATH" (string-append (getcwd) "/dist/build"))))
         (add-after 'compile 'agda-compile
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (agda-compiler (string-append out "/bin/agda")))
               (for-each (cut invoke agda-compiler <>)
                         (find-files (string-append out "/share") "\\.agda$"))))))))
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
     (list agda))
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
