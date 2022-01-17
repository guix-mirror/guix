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
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (srfi srfi-26)
  #:use-module (guix utils))

(define-public babeltrace
  (package
    (name "babeltrace")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.efficios.com/files/babeltrace/babeltrace2-"
                                  version ".tar.bz2"))
              (sha256
               (base32 "1jlv925pr7hykc48mdvbmqm4ipy1r11xwzapa6fdpdfshmk12kvp"))))

    (build-system gnu-build-system)

    (arguments
     `(#:tests? #f  ; FIXME - When Python's bindings are enabled, tests do not
                    ; pass.
       #:make-flags
       ,#~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
       #:configure-flags
       '("--enable-debug-info"
         "--enable-man-pages"
         "--enable-python-bindings"
         "--enable-python-plugins")
       #:phases
       (modify-phases %standard-phases
         ;; These are recommended in the project's README for a development
         ;; build configuration.
         (add-before 'configure 'set-environment-variables
           (lambda _
             (setenv "BABELTRACE_DEV_MODE" "1")
             (setenv "BABELTRACE_MINIMAL_LOG_LEVEL" "TRACE"))))))
    (inputs
     (list glib))
    ;; NOTE - elfutils is used for the LTTng debug information filter
    ;; component class.  This can be moved to `native-inputs` if
    ;; `--enable-debug-info` is replaced by `--disable-debug-info` in
    ;; `#:configure-flags`.
    (propagated-inputs
     (list elfutils))
    ;; NOTE - python-3 is set here for generating the bindings.  Users need to
    ;; install python-3 in their profile in order to use these bindings.
    (native-inputs
     (list asciidoc
           bison
           flex
           pkg-config
           python-3
           python-sphinx
           swig
           xmltoman))
    (home-page "https://babeltrace.org/")
    (synopsis "Trace manipulation toolkit")
    (description "Babeltrace 2 is a framework for viewing, converting,
transforming, and analyzing traces.  It is also the reference parser
implementation of the Common Trace Format (CTF), produced by tools such as
LTTng and barectf.  This package provides a library with a C API, Python 3
bindings, and the command-line tool @command{babeltrace2}.")
    (license license:expat)))

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

(define-public flamegraph
  ;; No new version since 2019, but there's still some new important commits.
  (let ((commit "810687f180f3c4929b5d965f54817a5218c9d89b")
        (revision "1"))
    (package
      (name "flamegraph")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/brendangregg/FlameGraph")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1lg02mxzdsm9szn4vcmx76c1bw9gqmxqk8n6v63v03036sc83s22"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         ',(map (cut list <> "bin/")
                '("flamegraph.pl"
                  "stackcollapse.pl"
                  "stackcollapse-perf.pl"
                  "stackcollapse-pmc.pl"
                  "stackcollapse-stap.pl"
                  "stackcollapse-instruments.pl"
                  "stackcollapse-vtune.pl"
                  "stackcollapse-jstack.pl"
                  "stackcollapse-gdb.pl"
                  "stackcollapse-go.pl"
                  "stackcollapse-vsprof.pl"
                  "stackcollapse-wcp.pl"))))
      (inputs (list perl))
      (home-page "http://www.brendangregg.com/flamegraphs.html")
      (synopsis "Stack trace visualizer")
      (description "Flamegraph is a collection of scripts that generate
interactive SVGs out of traces genated from various tracing tools.  It comes
with the script @command{flamegraph.pl} and many stackcollapse scripts.")
      (license license:cddl1.0))))

(define-public lttng-ust
  (package
    (name "lttng-ust")
    (version "2.13.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://lttng.org/files/lttng-ust/"
                                  "lttng-ust-" version ".tar.bz2"))
              (sha256
               (base32
                "1p7d94r275yvby6zqfxaswdl1q46zxbc8x5rkhnjxrp1d41byrsn"))))
    (build-system gnu-build-system)
    (inputs
     (list liburcu numactl))
    (native-inputs
     (list python-3 pkg-config))
    (home-page "https://lttng.org/")
    (synopsis "LTTng userspace tracer libraries")
    (description "The user space tracing library, liblttng-ust, is the LTTng
user space tracer.  It receives commands from a session daemon, for example to
enable and disable specific instrumentation points, and writes event records
to ring buffers shared with a consumer daemon.")
    (license license:lgpl2.1+)))

(define-public lttng-tools
  (package
    (name "lttng-tools")
    (version "2.13.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://lttng.org/files/lttng-tools/"
                                  "lttng-tools-" version ".tar.bz2"))
              (sha256
               (base32
                "1gfp9y24lpaiz4lcmbp30yd400jmh99mlay9gb8pz9qd080bmlnf"))))
    (build-system gnu-build-system)
    (arguments
     `( ;; FIXME - Currently there's a segmentation fault by swig when enabling
       ;; Python's bindings.  Thus, bindings are disable here.  Replace
       ;; `disable` by `enable` in #:configure-flags when this is fixed.
       #:configure-flags '("--disable-python-bindings")
       ;; FIXME - Tests are disabled for now because one test hangs
       ;; indefinetely.  Also, parallel testing is not possible because of how
       ;; the lttng-daemon handles sessions.  Thus, keep parallel testing
       ;; disabled even after tests are enabled!
       #:tests? #f
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-environment-variables
           (lambda _
             (setenv "HOME" "/tmp")
             (setenv "LTTNG_HOME" "/tmp")))
         ;; We don't put (which "man") here because LTTng uses execlp.
         (add-after 'unpack 'patch-default-man-path
           (lambda _
             (substitute* "src/common/defaults.h"
               (("/usr/bin/man") "man")))))))
    ;; NOTE - Users have to install python-3 in their profile to use the
    ;; bindings.  We don't put it in the inputs, because the rest of the tools
    ;; can work without it.
    (inputs
     (list liburcu popt numactl))
    (propagated-inputs
     (list kmod module-init-tools))
    (native-inputs
     (list pkg-config
           perl
           libpfm4
           python-3
           swig
           procps
           which
           flex
           bison
           asciidoc
           libxml2
           lttng-ust))
    (home-page "https://lttng.org/")
    (synopsis "LTTng userspace tracer libraries")
    (description "The lttng-tools project provides a session
daemon @code{lttng-sessiond} that acts as a tracing registry, the @command{lttng} command
line for tracing control, a @code{lttng-ctl} library for tracing control and a
@code{lttng-relayd} for network streaming.")
    (license (list  license:gpl2 license:lgpl2.1))))

(define-public uftrace
  (package
    (name "uftrace")
    (version "0.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/namhyung/uftrace")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0gk0hv3rnf5czvazz1prg21rf9qlniz42g5b389n8a29hqj4q6xr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list
        (string-append "CC=" ,(cc-for-target)))
       ;; runtest hang at some point -- probably dues to
       ;; failed socket connection -- but we want to keep the
       ;; unit tests.  Change the target to "test" when fixed.
       #:test-target "unittest"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs target #:allow-other-keys)
             (let ((arch ,(system->linux-architecture
                           (or (%current-target-system)
                               (%current-system)))))
               (setenv "ARCH"
                       (cond
                        ((string=? arch "arm64") "aarch64")
                        (else arch)))
               (when target
                 (setenv "CROSS_COMPILE" (string-append target "-"))))
             (setenv "SHELL" (which "sh"))
             (invoke "./configure"
                     (string-append "--prefix="
                                    (assoc-ref outputs "out"))))))))
    (inputs
     (list capstone
           elfutils
           libunwind
           ncurses))
    (native-inputs
     (list luajit
           pandoc
           pkg-config
           python-wrapper))
    (home-page "https://github.com/namhyung/uftrace")
    (synopsis "Function graph tracer for C/C++/Rust")
    (description "uftrace is a tool for tracing and analyzing the execution of
programs written in C/C++.  It is heavily inspired by the ftrace framework of
the Linux kernel, while supporting userspace programs.  It supports various
kind of commands and filters to help analysis of the program execution and
performance.  It provides the command @command{uftrace}.  User that want to do
scripting need to install python-3 or luajit in their profile.")
    (license license:gpl2)))
