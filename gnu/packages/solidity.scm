;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
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

(define-module (gnu packages solidity)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public solidity
  (let ((commit "3f05b770bdbf60eca866382049ea191dd701409a"))
    (package
      (name "solidity")
      (version "0.7.4")
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/ethereum/solidity")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1mswhjymiwnd3n7h3sjvjx5x8223yih0yvfcr0zpqr4aizpfx5z8"))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'create-commit_hash.txt
             (lambda _
               (with-output-to-file "commit_hash.txt"
                 (lambda _
                   (display
                    (substring ,commit 0 8))))))
           (delete 'configure)
           (delete 'install)
           (replace 'build
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Unbundle jsoncpp
               (delete-file "./cmake/jsoncpp.cmake")
               (substitute* "CMakeLists.txt"
                 (("include\\(jsoncpp\\)") ""))
               ;; Bug list is always sorted since we only build releases
               (substitute* "./test/cmdlineTests.sh"
                 (("\"\\$REPO_ROOT\"/scripts/update_bugs_by_version\\.py") ""))
               (substitute* "./scripts/build.sh"
                 (("sudo\\ make\\ install") "make install")
                 (("cmake\\ ..")
                  (string-append "cmake .. -DCMAKE_INSTALL_PREFIX="
                                 (assoc-ref outputs "out"))))
               (setenv "CIRCLECI" "1")
               (invoke "./scripts/build.sh")
               #t))
           (replace 'check
             (lambda _
               (invoke "./scripts/tests.sh")
               #t)))))
      (inputs
       (list boost-static jsoncpp z3))
      (native-inputs
       `(("python" ,python)
         ("tput" ,ncurses)
         ("xargs" ,findutils)))
      (home-page "https://solidity.readthedocs.io")
      (synopsis "Contract-Oriented Programming Language")
      (description
       "Solidity is a statically-typed curly-braces programming language
designed for developing smart contracts that run on the Ethereum Virtual
Machine.")
      (license license:gpl3+))))
