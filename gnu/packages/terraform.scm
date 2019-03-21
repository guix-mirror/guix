;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages terraform)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system go))

(define-public terraform-docs
  (package
    (name "terraform-docs")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/segmentio/terraform-docs")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p6prhjf82qnhf1zwl9h92j4ds5g383a6g9pwwnqbc3wdwy5zx7d"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/segmentio/terraform-docs"))
    (synopsis "Generate documentation from Terraform modules")
    (description
     "The @code{terraform-docs} utility can generate documentation describing
the inputs and outputs for modules of the Terraform infrastructure management
tool.  These can be shown, or written to a file in JSON or Markdown formats.")
    (home-page "https://github.com/segmentio/terraform-docs")
    (license license:expat)))
