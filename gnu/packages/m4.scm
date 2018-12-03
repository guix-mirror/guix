;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
   (version "1.4.18")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/m4/m4-"
                                version ".tar.xz"))
            (patches (search-patches "m4-gnulib-libio.patch"))
            (sha256
             (base32
              "01sfjd5a4waqw83bibvmn522g69qfqvwig9i2qlgy154l1nfihgj"))))
   (build-system gnu-build-system)
   (arguments
    `(;; Explicitly disable tests when cross-compiling, otherwise 'make check'
      ;; proceeds and fails, unsurprisingly.
      #:tests? ,(not (%current-target-system))

      #:phases
      (modify-phases %standard-phases
        (add-before 'check 'pre-check
          (lambda* (#:key inputs #:allow-other-keys)
            ;; Fix references to /bin/sh.
            (let ((bash (assoc-ref inputs "bash")))
              (for-each patch-shebang
                        (find-files "tests" "\\.sh$"))
              (substitute* (find-files "tests"
                                       "posix_spawn")
                (("/bin/sh")
                 (format #f "~a/bin/sh" bash)))
              #t))))))
   (synopsis "Macro processor")
   (description
    "GNU M4 is an implementation of the M4 macro language, which features
some extensions over other implementations, some of which are required by GNU
Autoconf.  It is used as a macro processor, which means it processes text,
expanding macros as it encounters them.  It also has some built-in functions,
for example to run shell commands or to do arithmetic.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/m4/")))
