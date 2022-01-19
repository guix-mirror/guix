;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Calum Irwin <calumirwin1@gmail.com>
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

(define-module (gnu packages zig)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages llvm))

(define-public zig
  (package
    (name "zig")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ziglang/zig.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zdz5s434c48z3y0c8l9wm2z1gxa7yyzd53zmr68lwj6mcl7by8x"))
       (patches
        (search-patches
         "zig-disable-libc-note-test.patch"
         "zig-use-system-paths.patch"))))
    (build-system cmake-build-system)
    (inputs
     (list clang-13 ; Clang propagates llvm.
           lld))
    ;; Zig compiles fine with GCC, but also needs native LLVM libraries.
    (native-inputs
     (list llvm-13))
    (arguments
     `(#:configure-flags
       (list ,@(if (%current-target-system)
                   (string-append "-DZIG_TARGET_TRIPLE="
                                  (%current-target-system))
                   '()))
       #:out-of-source? #f ; for tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'set-cache-dir
           (lambda _
             ;; Set cache dir, otherwise Zig looks for `$HOME/.cache'.
             (setenv "ZIG_GLOBAL_CACHE_DIR"
                     (string-append (getcwd) "/zig-cache"))))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs tests? #:allow-other-keys)
             (when tests?
               (invoke (string-append (assoc-ref outputs "out") "/bin/zig")
                       ;; Testing the standard library takes >7.5GB RAM, and
                       ;; will fail if it is OOM-killed.  The 'test-toolchain'
                       ;; target skips standard library and doc tests.
                       "build" "test-toolchain"
                       ;; Stage 2 is experimental, not what we run with `zig',
                       ;; and stage 2 tests require a lot of RAM.
                       "-Dskip-stage2-tests"
                       ;; Non-native tests try to link and execute non-native
                       ;; binaries.
                       "-Dskip-non-native")))))))
    (native-search-paths
     (list
      (search-path-specification
       (variable "C_INCLUDE_PATH")
       (files '("include")))
      (search-path-specification
       (variable "CPLUS_INCLUDE_PATH")
       (files '("include/c++" "include")))
      (search-path-specification
       (variable "LIBRARY_PATH")
       (files '("lib" "lib64")))))
    (synopsis "General purpose programming language and toolchain")
    (description "Zig is a general-purpose programming language and
toolchain.  Among other features it provides
@itemize
@item an Optional type instead of null pointers,
@item manual memory management,
@item generic data structures and functions,
@item compile-time reflection and compile-time code execution,
@item integration with C using zig as a C compiler, and
@item concurrency via async functions.
@end itemize")
    (home-page "https://github.com/ziglang/zig")
    (license license:expat)))
