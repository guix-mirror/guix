;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages gettext)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public gettext
  (package
    (name "gettext")
    (version "0.18.1.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gettext/gettext-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1sa3ch12qxa4h3ya6hkz119yclcccmincl9j20dhrdx5mykp3b4k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:patches (list (assoc-ref %build-inputs "patch/gets"))
       #:phases ,(if (%current-target-system)
                     '%standard-phases
                     '(alist-cons-before
                       'check 'patch-tests
                       (lambda* (#:key inputs #:allow-other-keys)
                         ;; TODO: Use (which "sh").
                         (let ((bash (assoc-ref inputs "bash")))
                           (substitute* (find-files "gettext-tools/tests"
                                                    "^msgexec-[0-9]")
                             (("#![[:blank:]]/bin/sh")
                              (format #f "#!~a/bin/sh" bash)))
                           (substitute* (find-files "gettext-tools/gnulib-tests"
                                                    "posix_spawn")
                             (("/bin/sh")
                              (format #f "~a/bin/bash" bash)))))
                       %standard-phases))))
    (inputs
     `(("patch/gets"
        ,(search-patch "gettext-gets-undeclared.patch"))))
    (home-page
     "http://www.gnu.org/software/gettext/")
    (synopsis "Tools and documentation for translation")
    (description
     "Usually, programs are written and documented in English, and use
English at execution time for interacting with users.  Using a common
language is quite handy for communication between developers,
maintainers and users from all countries.  On the other hand, most
people are less comfortable with English than with their own native
language, and would rather be using their mother tongue for day to
day's work, as far as possible.  Many would simply love seeing their
computer screen showing a lot less of English, and far more of their
own language.

GNU `gettext' is an important step for the GNU Translation Project, as
bit is an asset on which we may build many other steps. This package
offers to programmers, translators, and even users, a well integrated
set of tools and documentation. Specifically, the GNU `gettext'
utilities are a set of tools that provides a framework to help other
GNU packages produce multi-lingual messages.")
    (license gpl3))) ; some files are under GPLv2+
