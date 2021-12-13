;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Josh Holland <josh@inv.alid.pw>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages rcm)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages perl))

(define-public rcm
  (package
    (name "rcm")
    (version "1.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://thoughtbot.github.io/rcm/dist/rcm-"
                                  version ".tar.gz"))
              (sha256
               (base32 "0jn2crdqcna0fyg7w7x1mcyjblzykd3lh4vdxhsd5x4w8hvsw4cv"))))
    (build-system gnu-build-system)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (add-after 'patch-source-shebangs 'patch-tests
                     (lambda _
                       (substitute* '("test/rcrc-tilde.t"
                                      "test/rcdn-hooks-run-in-order.t"
                                      "test/rcup-hooks-run-in-order.t")
                         (("/bin/sh") (which "sh")))
                       (substitute* "test/rcup-hooks.t"
                         (("/usr/bin/env") (which "env")))
                       #t)))
                  #:parallel-tests? #f))
    (native-inputs (list perl python-cram))
    (synopsis "Management suite for dotfiles")
    (description "The rcm suite of tools is for managing dotfiles directories.  This is
a directory containing all the @code{.*rc} files in your home directory
(@code{.zshrc}, @code{.vimrc}, and so on). These files have gone by many
names in history, such as “rc files” because they typically end in rc
or “dotfiles” because they begin with a period.  This suite is useful
for committing your rc files to a central repository to share, but it also
scales to a more complex situation such as multiple source directories
shared between computers with some host-specific or task-specific files.")
    (license bsd-3)
    (home-page "https://github.com/thoughtbot/rcm")))
