;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (gnu packages hyperledger)
  #:use-module (ice-9 match)
  #:use-module (guix build-system go)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages version-control))

(define-public hyperledger-fabric
  (package
    (name "hyperledger-fabric")
    (version "1.4")
    ;; While the GitHub repository is supposed to be "just a mirror," the Go
    ;; imports refer to it explicitly.
    (home-page "https://github.com/hyperledger/fabric")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "release-" version))))
              (sha256
               (base32
                "1g003wf6439f2c9i2vphf4sh463yyasq1vpqmkpw9lj170a6kl8k"))
              (file-name (git-file-name name version))))
    (build-system go-build-system)
    (native-inputs
     `(("which" ,which)
       ("docker-cli" ,docker-cli)
       ("git" ,git)
       ("curl" ,curl)))
    (arguments
     `(#:import-path "github.com/hyperledger/fabric"
       #:unpack-path "github.com/hyperledger/fabric"
       ;; We don't need to install the source code for end-user applications.
       #:install-source? #f
       ;; TODO: Tests require a running Docker daemon.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             ;; Only linux-amd64 and linux-ppc64le seem to be supported at the moment.
             (invoke "make" "-C" "src/github.com/hyperledger/fabric"
                     "release/linux-amd64")))
         (add-after 'install 'install-commands
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (src "src/github.com/hyperledger/fabric/"))
               (with-directory-excursion src
                 (copy-recursively
                  "release/linux-amd64/bin"
                  (string-append out "/bin"))
                 (install-file "LICENSE"
                               (string-append out "/share/licenses"))
                 (install-file "README.md"
                               (string-append out "/share/doc"))
                 (copy-recursively "sampleconfig"
                                   (string-append out "/etc/hyperledger/fabric"))))
             #t)))))
    (synopsis "Platform for distributed ledger solutions")
    (description "A platform for distributed ledger solutions, underpinned by
a modular architecture delivering high degrees of confidentiality, resiliency,
flexibility and scalability.")
    (license asl2.0)))
