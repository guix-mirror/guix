;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
;;; Copyright © 2018 Alex ter Weele <alex.ter.weele@gmail.com>
;;; Copyright © 2019 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages idris)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public idris
  (package
    (name "idris")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/"
                    "idris-" version "/idris-" version ".tar.gz"))
              (sha256
               (base32
                "0fn9h58l592j72njwma1ia48h8h87wi2rjqfxs7j2lfmvgfv18fi"))))
    (build-system haskell-build-system)
    (inputs
     `(("gmp" ,gmp)
       ("ncurses" ,ncurses)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-annotated-wl-pprint" ,ghc-annotated-wl-pprint)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-async" ,ghc-async)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-cheapskate" ,ghc-cheapskate)
       ("ghc-code-page" ,ghc-code-page)
       ("ghc-fingertree" ,ghc-fingertree)
       ("ghc-fsnotify" ,ghc-fsnotify)
       ("ghc-ieee754" ,ghc-ieee754)
       ("ghc-libffi" ,ghc-libffi)
       ("ghc-megaparsec" ,ghc-megaparsec)
       ("ghc-network" ,ghc-network)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-safe" ,ghc-safe)
       ("ghc-split" ,ghc-split)
       ("ghc-terminal-size" ,ghc-terminal-size)
       ("ghc-text" ,ghc-text)
       ("ghc-uniplate" ,ghc-uniplate)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-vector" ,ghc-vector)
       ("ghc-vector-binary-instances" ,ghc-vector-binary-instances)
       ("ghc-zip-archive" ,ghc-zip-archive)))
    (arguments
     `(#:tests? #f ; FIXME: Test suite doesn't run in a sandbox.
       #:configure-flags
       (list (string-append "--datasubdir="
                            (assoc-ref %outputs "out") "/lib/idris")
             "-fFFI" "-fGMP")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-cc-command
           (lambda _
             (setenv "CC" "gcc")
             #t))
         (add-after 'install 'fix-libs-install-location
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/idris"))
                    (modules (string-append lib "/libs")))
               (for-each
                (lambda (module)
                  (symlink (string-append modules "/" module)
                           (string-append lib "/" module)))
                '("prelude" "base" "contrib" "effects" "pruviloj"))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "IDRIS_LIBRARY_PATH")
            (files '("lib/idris")))))
    (home-page "http://www.idris-lang.org")
    (synopsis "General purpose language with full dependent types")
    (description "Idris is a general purpose language with full dependent
types.  It is compiled, with eager evaluation.  Dependent types allow types to
be predicated on values, meaning that some aspects of a program's behaviour
can be specified precisely in the type.  The language is closely related to
Epigram and Agda.")
    (license license:bsd-3)))

;; Idris modules use the gnu-build-system so that the IDRIS_LIBRARY_PATH is set.
(define (idris-default-arguments name)
  `(#:modules ((guix build gnu-build-system)
               (guix build utils)
               (ice-9 ftw)
               (ice-9 match))
    #:phases
    (modify-phases %standard-phases
      (delete 'configure)
      (delete 'build)
      (delete 'check)
      (replace 'install
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (idris (assoc-ref inputs "idris"))
                 (idris-bin (string-append idris "/bin/idris"))
                 (idris-libs (string-append idris "/lib/idris/libs"))
                 (module-name (and (string-prefix? "idris-" ,name)
                                   (substring ,name 6)))
                 (ibcsubdir (string-append out "/lib/idris/" module-name))
                 (ipkg (string-append module-name ".ipkg"))
                 (idris-library-path (getenv "IDRIS_LIBRARY_PATH"))
                 (idris-path (string-split idris-library-path #\:))
                 (idris-path-files (apply append
                                          (map (lambda (path)
                                                 (map (lambda (dir)
                                                        (string-append path "/" dir))
                                                      (scandir path))) idris-path)))
                 (idris-path-subdirs (filter (lambda (path)
                                               (and path (match (stat:type (stat path))
                                                           ('directory #t)
                                                           (_ #f))))
                                                    idris-path-files))
                 (install-cmd (cons* idris-bin
                                     "--ibcsubdir" ibcsubdir
                                     "--build" ipkg
                                     ;; only trigger a build, as --ibcsubdir
                                     ;; already installs .ibc files.

                                     (apply append (map (lambda (path)
                                                          (list "--idrispath"
                                                                path))
                                                        idris-path-subdirs)))))
            ;; FIXME: Seems to be a bug in idris that causes a dubious failure.
            (apply system* install-cmd)
            #t))))))

(define-public idris-lightyear
  (let ((commit "6d65ad111b4bed2bc131396f8385528fc6b3678a"))
    (package
      (name "idris-lightyear")
      (version (git-version "0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ziman/lightyear")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1pkxnn3ryr0v0cin4nasw7kgkc9dnnpja1nfbj466mf3qv5s98af"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("idris" ,idris)))
      (arguments (idris-default-arguments name))
      (home-page "https://github.com/ziman/lightyear")
      (synopsis "Lightweight parser combinator library for Idris")
      (description "Lightweight parser combinator library for Idris, inspired
by Parsec.  This package is used (almost) the same way as Parsec, except for one
difference: backtracking.")
      (license license:bsd-2))))

(define-public idris-wl-pprint
  (let ((commit "1d365fcf4ba075859844dbc5eb96a90f57b9f338"))
    (package
      (name "idris-wl-pprint")
      (version (git-version "0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/shayan-najd/wl-pprint")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0g7c3y9smifdz4sivi3qmvymhdr7v9kfq45fmfmmvkqcrix0spzn"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("idris" ,idris)))
      (arguments (idris-default-arguments name))
      (home-page "https://github.com/shayan-najd/wl-pprint")
      (synopsis "Pretty printing library")
      (description "A pretty printing library for Idris based on Phil Wadler's
paper A Prettier Printer and on Daan Leijen's extensions in the Haskell
wl-pprint library.")
      (license license:bsd-2))))

(define-public idris-bifunctors
  (let ((commit "53d06a6ccfe70c49c9ae8c8a4135981dd2173202"))
    (package
      (name "idris-bifunctors")
      (version (git-version "0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/HuwCampbell/Idris-Bifunctors")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "02vbsd3rmgnj0l1qq787709qcxjbr9890cbad4ykn27f77jk81h4"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("idris" ,idris)))
      (arguments (idris-default-arguments name))
      (home-page "https://github.com/HuwCampbell/Idris-Bifunctors")
      (synopsis "Bifunctor library")
      (description "This is a bifunctor library for Idris based off the
excellent Haskell Bifunctors package from Edward Kmett.")
      (license license:bsd-3))))

(define-public idris-lens
  (let ((commit "26f012005f6849806cea630afe317e42cae97f29"))
    (package
      (name "idris-lens")
      (version (git-version "0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/HuwCampbell/idris-lens")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "06jzfj6rad08rk92w8jk5byi79svmyg0mrcqhibgx8rkjjy6vmai"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("idris" ,idris)))
      (propagated-inputs
       `(("idris-bifunctors" ,idris-bifunctors)))
      (arguments (idris-default-arguments name))
      (home-page "https://github.com/HuwCampbell/idris-lens")
      (synopsis "Van Laarhoven lenses for Idris")
      (description "Lenses are composable functional references.  They allow
accessing and modifying data within a structure.")
      (license license:bsd-3))))
