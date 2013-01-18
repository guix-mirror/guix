;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages m4)
  #:use-module (guix licenses)
  #:use-module (distro)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public m4
  (package
   (name "m4")
   (version "1.4.16")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/m4/m4-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "035r7ma272j2cwni2961jp22k6bn3n9xwn3b3qbcn2yrvlghql22"))))
   (build-system gnu-build-system)
   (arguments (case-lambda
                ((system)
                 ;; XXX: Disable tests on those platforms with know issues.
                 `(#:tests? ,(not (member system
                                          '("x86_64-darwin"
                                            "i686-cygwin"
                                            "i686-sunos")))
                   #:patches (list (assoc-ref %build-inputs "patch/s_isdir")
                                   (assoc-ref %build-inputs
                                              "patch/readlink-EINVAL")
                                   (assoc-ref %build-inputs "patch/gets"))
                   #:phases (alist-cons-before
                             'check 'pre-check
                             (lambda* (#:key inputs #:allow-other-keys)
                               ;; Fix references to /bin/sh.
                               (let ((bash (assoc-ref inputs "bash")))
                                 (for-each patch-shebang
                                           (find-files "tests" "\\.sh$"))
                                 (substitute* (find-files "tests"
                                                          "posix_spawn")
                                   (("/bin/sh")
                                    (format #f "~a/bin/bash" bash)))))
                             %standard-phases)))
                ((system cross-system)
                 `(#:patches (list (assoc-ref %build-inputs "patch/s_isdir")
                                   (assoc-ref %build-inputs
                                              "patch/readlink-EINVAL")
                                   (assoc-ref %build-inputs "patch/gets"))))))
   (inputs `(("patch/s_isdir" ,(search-patch "m4-s_isdir.patch"))
             ("patch/readlink-EINVAL"
              ,(search-patch "m4-readlink-EINVAL.patch"))
             ("patch/gets" ,(search-patch "m4-gets-undeclared.patch"))))
   (synopsis "GNU M4, a macro processor")
   (description
    "GNU M4 is an implementation of the traditional Unix macro processor.  It
is mostly SVR4 compatible although it has some extensions (for example,
handling more than 9 positional parameters to macros).  GNU M4 also has
built-in functions for including files, running shell commands, doing
arithmetic, etc.

GNU M4 is a macro processor in the sense that it copies its input to the
output expanding macros as it goes.  Macros are either builtin or
user-defined and can take any number of arguments.  Besides just doing macro
expansion, m4 has builtin functions for including named files, running UNIX
commands, doing integer arithmetic, manipulating text in various ways,
recursion etc...  m4 can be used either as a front-end to a compiler or as a
macro processor in its own right.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/m4/")))
