;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2019 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2016, 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2019, 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 John Doe <dftxbs3e@free.fr>
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

(define-module (gnu packages libffi)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sphinx)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby))

(define-public libffi
  (package
    (name "libffi")
    (version "3.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "ftp://sourceware.org/pub/libffi/"
                              name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mi0cpf8aa40ljjmzxb7im6dbj45bb0kllcd09xgmp834y9agyvj"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Prevent the build system from passing -march and -mtune to the
       ;; compiler.  See "ax_cc_maxopt.m4" and "ax_gcc_archflag.m4".
       #:configure-flags '("--enable-portable-binary" "--without-gcc-arch")

       ;; TODO: Inline patches on next rebuild cycle.
       ,@(if (string-prefix? "powerpc-" (or (%current-target-system)
                                            (%current-system)))
             '(#:phases (modify-phases %standard-phases
                          (add-after 'unpack 'apply-patch
                            (lambda* (#:key inputs #:allow-other-keys)
                              (let ((patch (assoc-ref inputs
                                                      "powerpc-patch")))
                                (invoke "patch" "--force" "-p1"
                                        "-i" patch))))))
             '())
       ,@(if (string-prefix? "powerpc64le-" (or (%current-target-system)
                                                (%current-system)))
             '(#:phases (modify-phases %standard-phases
                          (add-after 'unpack 'apply-patch2
                            (lambda* (#:key inputs #:allow-other-keys)
                              (let ((patch (assoc-ref inputs
                                                      "powerpc64le-patch")))
                                (invoke "patch" "--force" "-p1"
                                        "-i" patch))))))
             '())))
    (inputs
     (cond
      ((string-prefix? "powerpc-" (or (%current-target-system)
                                        (%current-system)))
       `(("powerpc-patch" ,@(search-patches
                             "libffi-3.3-powerpc-fixes.patch"))))
      ((string-prefix? "powerpc64le-" (or (%current-target-system)
                                          (%current-system)))
       `(("powerpc64le-patch" ,@(search-patches
                                 "libffi-float128-powerpc64le.patch"))))
      (else '())))
    (outputs '("out" "debug"))
    (synopsis "Foreign function call interface library")
    (description
     "The libffi library provides a portable, high level programming interface
to various calling conventions.  This allows a programmer to call any
function specified by a call interface description at run-time.

FFI stands for Foreign Function Interface.  A foreign function interface is
the popular name for the interface that allows code written in one language
to call code written in another language.  The libffi library really only
provides the lowest, machine dependent layer of a fully featured foreign
function interface.  A layer must exist above libffi that handles type
conversions for values passed between the two languages.")
    (home-page "http://www.sourceware.org/libffi/")
    (properties `((release-monitoring-url . ,home-page)))

    ;; See <https://github.com/atgreen/libffi/blob/master/LICENSE>.
    (license expat)))

(define-public python-cffi
  (package
    (name "python-cffi")
    (version "1.14.4")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "cffi" version))
      (sha256
       (base32 "0v080s7vlrjz9z823x2yh36yc8drwpvvir6w8wfkkzd7k2z5qihs"))))
    (build-system python-build-system)
    (inputs
     `(("libffi" ,libffi)))
    (propagated-inputs ; required at run-time
     `(("python-pycparser" ,python-pycparser)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-pytest" ,python-pytest)))
    (arguments
     `(#:modules ((ice-9 ftw)
                  (srfi srfi-26)
                  (guix build utils)
                  (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PYTHONPATH"
                     (string-append
                      (getenv "PYTHONPATH")
                      ":" (getcwd) "/build/"
                      (car (scandir "build" (cut string-prefix? "lib." <>)))))

             ;; XXX The "normal" approach of setting CC and friends does
             ;; not work here.  Is this the correct way of doing things?
             (substitute* "testing/embedding/test_basic.py"
               (("c = distutils\\.ccompiler\\.new_compiler\\(\\)")
                (string-append "c = distutils.ccompiler.new_compiler();"
                               "c.set_executables(compiler='gcc',"
                               "compiler_so='gcc',linker_exe='gcc',"
                               "linker_so='gcc -shared')")))
             (substitute* "testing/cffi0/test_ownlib.py"
               (("\"cc testownlib") "\"gcc testownlib"))
             (invoke "py.test" "-v" "c/" "testing/")
             #t))
         (add-before 'check 'patch-paths-of-dynamically-loaded-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Shared libraries should be referred by their absolute path as
             ;; using find_library or the like with their name fail when the
             ;; resolved .so object is a linker script rather than an ELF
             ;; binary (this is a limitation of the ctype library of Python).
             (let* ((glibc (assoc-ref inputs "libc"))
                    (libm (string-append glibc "/lib/libm.so.6"))
                    (libc (string-append glibc "/lib/libc.so.6")))
               (substitute* '("testing/cffi0/test_function.py"
                              "testing/cffi0/test_parsing.py"
                              "testing/cffi0/test_unicode_literals.py"
                              "testing/cffi0/test_zdistutils.py"
                              "testing/cffi1/test_recompiler.py")
                 (("lib_m = ['\"]{1}m['\"]{1}")
                  (format #f "lib_m = '~a'" libm)))
               (substitute* '("testing/cffi0/test_verify.py"
                              "testing/cffi1/test_verify1.py")
                 (("lib_m = \\[['\"]{1}m['\"]{1}\\]")
                  (format #f "lib_m = ['~a']" libm)))
               (substitute* "c/test_c.py"
                 (("find_and_load_library\\(['\"]{1}c['\"]{1}")
                  (format #f "find_and_load_library('~a'" libc)))
               #t))))))
    (home-page "https://cffi.readthedocs.io/")
    (synopsis "Foreign function interface for Python")
    (description "Foreign Function Interface for Python calling C code.")
    (license expat)))

(define-public python2-cffi
  (package-with-python2 python-cffi))

(define-public python-cffi-documentation
  (package
    (name "python-cffi-documentation")
    (version (package-version python-cffi))
    (source (package-source python-cffi))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'chdir
                    (lambda _ (chdir "doc") #t))
                  (delete 'configure)
                  (replace 'build
                    (lambda* (#:key (make-flags '()) #:allow-other-keys)
                      (apply invoke "make" "html" make-flags)))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (copy-recursively "build/html" (string-append out "/html"))
                        #t))))))
    (native-inputs
     `(("sphinx-build" ,python-sphinx)))
    (home-page (package-home-page python-cffi))
    (synopsis "Documentation for the Python CFFI interface")
    (description
     "This package contains HTML documentation for the @code{python-cffi}
project.")
    (license (package-license python-cffi))))

(define-public ruby-ffi
  (package
    (name "ruby-ffi")
    (version "1.12.2")
    (source (origin
              ;; Pull from git because the RubyGems release bundles LibFFI,
              ;; and comes with a gemspec that makes it difficult to unbundle.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ffi/ffi")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cvqsbjr2gfjgqggq9kdx90qhhzr7qkyr9wmxdsfsik6cnxnnpmd"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-depend-on-ccache
           (lambda _
             (substitute* "spec/ffi/fixtures/GNUmakefile"
               (("^CCACHE := .*")
                ""))
             #t))
         (replace 'replace-git-ls-files
           (lambda _
             ;; Do not try to execute git, or include the (un)bundled LibFFI.
             (substitute* "ffi.gemspec"
               (("git ls-files -z")
                "find * -type f -print0 | sort -z")
               (("lfs \\+?= .*")
                "lfs = []\n"))
             (substitute* "Rakefile"
               (("LIBFFI_GIT_FILES = .*")
                "LIBFFI_GIT_FILES = []\n"))
             #t))
         (replace 'build
          (lambda _
            ;; Tests depend on the native extensions, so we build it
            ;; beforehand without going through the gem machinery.
             (invoke "rake" "compile")

             ;; XXX: Ideally we'd use "rake native gem" here to prevent the
             ;; install phase from needlessly rebuilding everything, but that
             ;; requires the bundled LibFFI, and the install phase can not
             ;; deal with such gems anyway.
             (invoke "gem" "build" "ffi.gemspec")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (begin
                   (setenv "MAKE" "make")
                   (setenv "CC" "gcc")
                   (invoke "rspec" "spec"))
                 (format #t "test suite not run~%"))
             #t)))))
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubygems-tasks" ,ruby-rubygems-tasks)))
    (inputs
     `(("libffi" ,libffi)))
    (synopsis "Ruby foreign function interface library")
    (description "Ruby-FFI is a Ruby extension for programmatically loading
dynamic libraries, binding functions within them, and calling those functions
from Ruby code.  Moreover, a Ruby-FFI extension works without changes on Ruby
and JRuby.")
    (home-page "https://wiki.github.com/ffi/ffi")
    (license bsd-3)))
