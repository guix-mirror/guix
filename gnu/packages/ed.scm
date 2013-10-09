;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages ed)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public ed
  (package
    (name "ed")
    (version "1.9")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/ed/ed-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "122syihsx2hwzj75mkf5a9ssiky2xby748kp4cc00wzhmp7p5cym"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("CC=gcc")
       #:phases (alist-cons-before 'patch-source-shebangs 'patch-test-suite
                                   (lambda _
                                     (substitute* "testsuite/check.sh"
                                       (("/bin/sh") (which "sh"))))
                                   %standard-phases)))
    (home-page "http://www.gnu.org/software/ed/")
    (synopsis "Line-oriented text editor")
    (description
     "Ed is a line-oriented text editor: rather than offering an overview of
a document, ed performs editing one line at a time.  It can be operated both
interactively and via shell scripts.  Its method of command input allows
complex tasks to be performed in an automated way, however it has largely
been superceded by full-screen editors like Emacs or Moe.")
    (license gpl3+)))
