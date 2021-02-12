;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
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

(define-module (gnu packages task-runners)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system go))

(define-public run
  (package
    (name "run")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TekWizely/run")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17n11lqhywq4z62w2rakdq80v7mxf83rgln19vj4v4nxpwd2hjjw"))))
    (build-system go-build-system)
    (propagated-inputs
     `(("go-github-com-tekwizely-go-parsing" ,go-github-com-tekwizely-go-parsing)))
    (arguments
     `(#:import-path "github.com/tekwizely/run"))
    (synopsis "Easily manage and invoke small scripts and wrappers")
    (description
     "Run is a tool to easily manage and invoke small scripts and wrappers by
using a Runfile.")
    (home-page "https://github.com/TekWizely/run")
    (license license:expat)))
