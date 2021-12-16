;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Olivier Dion <olivier.dion@polymtl.ca>
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

(define-module (gnu packages instrumentation)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages tbb)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public dyninst
  (package
    (name "dyninst")
    ;; Newer versions are not promoted on main home page.
    ;; Upgrade to 12.0.1 if anyone require a newer version.
    (version "10.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dyninst/dyninst")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1m04pg824rqx647wvk9xl33ri8i6mm0vmrz9924li25dxbr4zqd5"))))

    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       ;; STERILE_BUILD: Do not download/build third-party dependencies from
       ;; source.
       #:configure-flags
       (list "-DSTERILE_BUILD=ON")
       ;; NOTE: dyninst needs to search for shared libraries that are linked
       ;; against the instrumented binary in order to rebuild the entire
       ;; program.  For this purpose, one can use LD_LIBRARY_PATH or
       ;; DYNISNT_REWRITER_PATHS environment variables to add paths for dyinst
       ;; to search.  However, dyninst also tries to be smart by executing
       ;; ldconfig, which is not portable.  If ldconfig is not available on
       ;; the system, dyinst wrongly assumes that the shared libraries can not
       ;; be found, even though it can.  This bad logic is still there with
       ;; newer versions of dyinst.  Thus, this substitution makes the bad
       ;; code path unreachable.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-bad-logic
           (lambda _
             (substitute* "dyninstAPI/src/linux.C"
               (("if\\(\\!fgets\\(buffer, 512, ldconfig\\)\\)")
                "fgets(buffer, 512, ldconfig); if (false)")))))))
    (propagated-inputs
     (list elfutils boost tbb-2020))
    (home-page "https://dyninst.org/")
    (synopsis "Dynamic instrumentation")
    (description "Dyninst is a collection of libraries for instrumenting,
analyzing and editing binaries.  It can attach to an existing program or
create a new one out of an ELF file for analysis or modification.  It come
with a handful of C++ libraries.")
    (license license:lgpl2.1+)))
