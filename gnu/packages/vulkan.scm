;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
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

(define-module (gnu packages vulkan)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages))

(define-public spirv-headers
  (let ((commit "98b01515724c428d0f0a5d01deffcce0f5f5e61c")
        (revision "1"))
    (package
      (name "spirv-headers")
      (version (string-append "0.0-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/KhronosGroup/SPIRV-Headers")
               (commit commit)))
         (sha256
          (base32
           "15bknwkv3xwmjs3lmkp282a1wrp0da1b4lp45i4yiav04zmqygj2"))
         (file-name (string-append name "-" version "-checkout"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ;; No tests
         #:phases (modify-phases %standard-phases
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (zero? (system* "cmake" "-E" "copy_directory"
                                        "../source/include/spirv"
                                        (string-append (assoc-ref outputs "out")
                                                       "/include/spirv"))))))))
      (home-page "https://github.com/KhronosGroup/SPIRV-Headers")
      (synopsis "Machine-readable files from the SPIR-V Registry")
      (description
       "SPIRV-Headers is a repository containing machine-readable files from
the SPIR-V Registry.  This includes:
@itemize
@item Header files for various languages.
@item JSON files describing the grammar for the SPIR-V core instruction set,
and for the GLSL.std.450 extended instruction set.
@item The XML registry file.
@end itemize\n")
      (license (license:x11-style
                (string-append "https://github.com/KhronosGroup/SPIRV-Headers/blob/"
                               commit "/LICENSE"))))))
