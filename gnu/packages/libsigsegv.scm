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

(define-module (gnu packages libsigsegv)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libsigsegv
  (package
   (name "libsigsegv")
   (version "2.10")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://gnu/libsigsegv/libsigsegv-"
                  version ".tar.gz"))
            (sha256
             (base32 "16hrs8k3nmc7a8jam5j1fpspd6sdpkamskvsdpcw6m29vnis8q44"))))
   (build-system gnu-build-system)
   (home-page "http://www.gnu.org/software/libsigsegv/")
   (synopsis "Library for handling page faults")
   (arguments
    ;; On MIPS, work around this error:
    ;;
    ;; In file included from fault-linux-mips-old.h:18:0,
    ;;    [...]
    ;; linux-libre-headers-cross-mips64el-linux-gnu-3.3.8/include/asm/sigcontext.h:57:8: error: redefinition of 'struct sigcontext'
    (if (string-contains (or (%current-target-system) (%current-system))
                         "mips64el")
        `(#:phases (alist-cons-before
                    'configure 'patch-mips-old-h
                    (lambda _
                      (substitute* "src/fault-linux-mips-old.h"
                        (("#include <asm/sigcontext\\.h>") "")))
                    ,(if (%current-target-system)
                         '%standard-cross-phases
                         '%standard-phases)))
        '()))
   (description
"GNU libsigsegv is a library for handling page faults in user mode. A page
fault occurs when a program tries to access to a region of memory that is
currently not available. Catching and handling a page fault is a useful
technique for implementing pageable virtual memory, memory-mapped access to
persistent databases, generational garbage collectors, stack overflow
handlers, distributed shared memory, and more.")
   (license gpl2+)))
