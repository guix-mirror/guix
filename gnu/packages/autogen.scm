;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages autogen)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages which)
  #:use-module (gnu packages guile))

(define-public autogen
  (package
    (name "autogen")
    (version "5.18.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/autogen/rel"
                          version "/autogen-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0k0gkr5inr9wb3ws30q6bbiqg3qm3ryvl9cznym2xis4lm216d53"))))
    (build-system gnu-build-system)
    (inputs `(("which" ,which)
              ("guile" ,guile-2.0)))
    (arguments
     '(#:phases (alist-cons-before
                 'patch-source-shebangs 'patch-test-scripts
                 (lambda _
                   (let ((sh (which "sh")))
                     (substitute*
                         (append (find-files "agen5/test" "\\.test$")
                                 (find-files "autoopts/test" "\\.(test|in)$"))
                       (("/bin/sh") sh)
                       (("/usr/bin/tr") "tr"))))
                 %standard-phases)))
    (home-page "http://www.gnu.org/software/autogen/")
    (synopsis "Automated program generator")
    (description
     "AutoGen is a program to ease the maintenance of programs that contain
large amounts of repetitive text.  It automates the construction of these
sections of the code, simplifying the task of keeping the text in sync.  It
also includes an add-on package called AutoOpts, which is specialized for the
maintenance and documentation of program options.")
    (license gpl3+)))
