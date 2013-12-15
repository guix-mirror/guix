;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu packages elf)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:select (lgpl3+))
  #:use-module (gnu packages m4)
  #:use-module (gnu packages compression))

(define-public elfutils
  (package
    (name "elfutils")
    (version "0.157")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://fedorahosted.org/releases/e/l/elfutils/"
                    version "/elfutils-" version ".tar.bz2"))
              (sha256
               (base32
                "11ffbihw9zs2dhmlww9zilwvmv6v1pr6bvnz5dnzn0lxq70ckbag"))))
    (build-system gnu-build-system)

    ;; Separate programs because that's usually not what elfutils users want,
    ;; and because they duplicate what Binutils provides.
    (outputs '("out"                           ; libelf.so, elfutils/*.h, etc.
               "bin"))                         ; ld, nm, objdump, etc.

    (native-inputs `(("m4" ,m4)))
    (inputs `(("zlib" ,zlib)))
    (home-page "https://fedorahosted.org/elfutils/")
    (synopsis #f)
    (description #f)

    ;; Libraries are dual-licensed LGPLv3.0+ | GPLv2, and programs are GPLv3+.
    (license lgpl3+)))
