;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages libsigsegv)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libsigsegv
  (package
   (name "libsigsegv")
   (version "2.13")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://gnu/libsigsegv/libsigsegv-"
                  version ".tar.gz"))
            (sha256
             (base32 "06c1qjiv69hjvbq06mcxdi5hzfadhy28sa83zxspqpxhfr0ywy5y"))))
   (build-system gnu-build-system)
   (home-page "https://www.gnu.org/software/libsigsegv/")
   (synopsis "Library for handling page faults")
   (arguments
    `(;; The shared library isn't built by default but some packages need it.
      #:configure-flags '("--enable-shared")

      ;; On MIPS, work around this error:
      ;;
      ;; In file included from fault-linux-mips-old.h:18:0,
      ;;    [...]
      ;; linux-libre-headers-cross-mips64el-linux-gnu-3.3.8/include/asm/sigcontext.h:57:8: error: redefinition of 'struct sigcontext'
      ,@(if (string-contains (or (%current-target-system) (%current-system))
                             "mips64el")
            `(#:phases (modify-phases %standard-phases
                         (add-before 'configure 'patch-mips-old-h
                           (lambda _
                             (substitute* "src/fault-linux-mips-old.h"
                               (("#include <asm/sigcontext\\.h>") ""))
                             #t))))
            '())))
   (description
    "GNU libsigsegv is a library to handle page faults, which occur when a
program tries to access an unavailable region of memory, in user mode.  By
catching and handling page faults, the program can implement pageable virtual
memory, stack overflow handlers, and so on.")
   (license gpl2+)))
