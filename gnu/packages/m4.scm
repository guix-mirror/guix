;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public m4
  (package
   (name "m4")
   (version "1.4.17")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/m4/m4-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "0w0da1chh12mczxa5lnwzjk9czi3dq6gnnndbpa6w4rj76b1yklf"))))
   (build-system gnu-build-system)
   (arguments
    ;; XXX: Disable tests on those platforms with know issues.
    `(#:tests? ,(not (member (%current-system)
                             '("x86_64-darwin"
                               "i686-cygwin"
                               "i686-sunos")))
      #:patches (list (assoc-ref %build-inputs
                                 "patch/readlink-EINVAL"))
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
   (inputs `(("patch/readlink-EINVAL"
              ,(search-patch "m4-readlink-EINVAL.patch"))))
   (synopsis "Macro processor")
   (description
    "GNU M4 is an implementation of the M4 macro language, which features
some extensions over other implementations.  It is used as a macro processor,
which means it processes text, expanding macros as it encounters them.  It
also has some built-in functionns, for example to run shell commands or to do
arithmetic.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/m4/")))
