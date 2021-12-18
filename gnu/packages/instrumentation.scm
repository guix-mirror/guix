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
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages man)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

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
     `(;; FIXME - When Python's bindings are enabled, tests do not pass.
       #:configure-flags '("--enable-debug-info"
                           "--enable-man-pages"
                           "--disable-python-bindings"
                           "--disable-python-plugins")
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

(define-public lttng-ust
  (package
    (name "lttng-ust")
    (version "2.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://lttng.org/files/lttng-ust/"
                                  "lttng-ust-" version ".tar.bz2"))
              (sha256
               (base32
                "0l0p6y2zrd9hgd015dhafjmpcj7waz762n6wf5ws1xlwcwrwkr2l"))))
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
    (version "2.13.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://lttng.org/files/lttng-tools/"
                                  "lttng-tools-" version ".tar.bz2"))
              (sha256
               (base32
                "1df8ag2a1yyjn6hz6wxgcz0p847cq91b8inf0zyhgz1im1yxzrng"))))
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
