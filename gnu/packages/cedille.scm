;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
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

(define-module (gnu packages cedille)
  #:use-module (gnu packages)
  #:use-module (gnu packages agda)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix build-system emacs)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public cedille
  (package
    (name "cedille")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cedille/cedille")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1h5s6ayh3s76z184jai3jidcs4cjk8s4nvkkv2am8dg4gfsybq22"))))
    (inputs
     (list agda agda-ial ghc ghc-alex ghc-happy))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-cedille-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "cedille-mode.el"
                 (("/usr/share/emacs/site-lisp/cedille-mode")
                  (string-append
                   out "/share/emacs/site-lisp/cedille")))
               (substitute* "cedille-mode/cedille-mode-info.el"
                 (("\\(concat cedille-path-el \"cedille-info-main.info\"\\)")
                  (string-append
                   "\"" out "/share/info/cedille-info-main.info.gz\"")))
               #t)))
         (add-after 'patch-cedille-paths 'copy-cedille-mode
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lisp
                     (string-append
                      out "/share/emacs/site-lisp/cedille/")))
               (mkdir-p (string-append lisp "cedille-mode"))
               (copy-recursively
                "cedille-mode"
                (string-append lisp "cedille-mode"))
               (mkdir-p (string-append lisp "se-mode"))
               (copy-recursively
                "se-mode"
                (string-append lisp "se-mode"))
               #t)))
         ;; FIXME: Byte compilation fails
         (delete 'build)
         (replace 'check
           (lambda _
             (with-directory-excursion "cedille-tests"
               (invoke "sh" "run-tests.sh"))))
         (add-after 'unpack 'patch-libraries
           (lambda _ (patch-shebang "create-libraries.sh") #t))
         (add-after 'unpack 'copy-ial
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively
              (search-input-directory inputs "/include/agda/ial")
              "ial")
             ;; Ambiguous module if main is included from ial
             (delete-file "ial/main.agda")
             #t))
         (add-after 'check 'build-cedille
           ;; Agda has a hard time with parallel compilation
           (lambda _
             (invoke "touch" "src/Templates.hs")
             (make-file-writable  "src/Templates.hs")
             (invoke "touch" "src/templates.agda")
             (make-file-writable  "src/templates.agda")
             (invoke "make" "--jobs=1")))
         (add-after 'install 'install-cedille
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively
                "lib" (string-append out "/lib/cedille"))
               (install-file "cedille" (string-append out "/bin"))
               (install-file "core/cedille-core"
                             (string-append out "/bin"))
               (install-file "docs/info/cedille-info-main.info"
                             (string-append out "/share/info"))
               #t))))))
    (home-page "https://cedille.github.io/")
    (synopsis
     "Language based on Calculus of Dependent Lambda Eliminations")
    (description
     "Cedille is an interactive theorem-prover and dependently typed
programming language, based on extrinsic (aka Curry-style) type theory.  This
makes it rather different from type theories like Coq and Agda, which are
intrinsic (aka Church-style).  In Cedille, terms are nothing more than
annotated versions of terms of pure untyped lambda calculus.  In contrast, in
Coq or Agda, the typing annotations are intrinsic parts of terms.  The typing
annotations can only be erased as an optimization under certain conditions,
not by virtue of the definition of the type theory.")
    (license license:expat)))
