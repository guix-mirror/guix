;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2016 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Sergei Trofimovich <slyfox@inbox.ru>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright @ 2018 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (gnu packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mp3)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

;; According to https://golang.org/doc/install/gccgo, gccgo-4.8.2 includes a
;; complete go-1.1.2 implementation, gccgo-4.9 includes a complete go-1.2
;; implementation, and gccgo-5 a complete implementation of go-1.4.  Ultimately
;; we hope to build go-1.5+ with a bootstrap process using gccgo-5.  As of
;; go-1.5, go cannot be bootstrapped without go-1.4, so we need to use go-1.4 or
;; gccgo-5.  Mips is not officially supported, but it should work if it is
;; bootstrapped.

(define-public go-1.4
  (package
    (name "go")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://storage.googleapis.com/golang/"
                                  name version ".src.tar.gz"))
              (sha256
               (base32
                "0na9yqilzpvq0bjndbibfp07wr796gf252y471cip10bbdqgqiwr"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"
               "tests"))
    (arguments
     `(#:modules ((ice-9 match)
                  (guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:tests? #f ; Tests are run by the all.bash script.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'patch-generated-file-shebangs 'chdir
           (lambda _
             (chdir "src")
             #t))
         (add-before 'build 'prebuild
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((gcclib (string-append (assoc-ref inputs "gcc:lib") "/lib"))
                    (ld (string-append (assoc-ref inputs "libc") "/lib"))
                    (loader (car (find-files ld "^ld-linux.+")))
                    (net-base (assoc-ref inputs "net-base"))
                    (tzdata-path
                     (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo"))
                    (output (assoc-ref outputs "out")))

               ;; Removing net/ tests, which fail when attempting to access
               ;; network resources not present in the build container.
               (for-each delete-file
                         '("net/multicast_test.go" "net/parse_test.go"
                           "net/port_test.go"))

               ;; Add libgcc to the RUNPATH.
               (substitute* "cmd/go/build.go"
                 (("cgoldflags := \\[\\]string\\{\\}")
                  (string-append "cgoldflags := []string{"
                                 "\"-rpath=" gcclib "\"}"))
                 (("ldflags := buildLdflags")
                  (string-append
                   "ldflags := buildLdflags\n"
                   "ldflags = append(ldflags, \"-r\")\n"
                   "ldflags = append(ldflags, \"" gcclib "\")\n")))

               (substitute* "os/os_test.go"
                 (("/usr/bin") (getcwd))
                 (("/bin/pwd") (which "pwd")))

               ;; Disable failing tests: these tests attempt to access
               ;; commands or network resources which are neither available or
               ;; necessary for the build to succeed.
               (for-each
                (match-lambda
                  ((file regex)
                   (substitute* file
                     ((regex all before test_name)
                      (string-append before "Disabled" test_name)))))
                '(("net/net_test.go" "(.+)(TestShutdownUnix.+)")
                  ("net/dial_test.go" "(.+)(TestDialTimeout.+)")
                  ("os/os_test.go" "(.+)(TestHostname.+)")
                  ("time/format_test.go" "(.+)(TestParseInSydney.+)")

                  ;; Tzdata 2016g changed the name of the time zone used in this
                  ;; test, and the patch for Go 1.7 does not work for 1.4.3:
                  ;; https://github.com/golang/go/issues/17545
                  ;; https://github.com/golang/go/issues/17276
                  ("time/time_test.go" "(.+)(TestLoadFixed.+)")
                  ("time/format_test.go" "(.+)(TestParseInLocation.+)")

                  ("os/exec/exec_test.go" "(.+)(TestEcho.+)")
                  ("os/exec/exec_test.go" "(.+)(TestCommandRelativeName.+)")
                  ("os/exec/exec_test.go" "(.+)(TestCatStdin.+)")
                  ("os/exec/exec_test.go" "(.+)(TestCatGoodAndBadFile.+)")
                  ("os/exec/exec_test.go" "(.+)(TestExitStatus.+)")
                  ("os/exec/exec_test.go" "(.+)(TestPipes.+)")
                  ("os/exec/exec_test.go" "(.+)(TestStdinClose.+)")
                  ("syscall/syscall_unix_test.go" "(.+)(TestPassFD\\(.+)")
                  ("os/exec/exec_test.go" "(.+)(TestExtraFiles.+)")))

               (substitute* "net/lookup_unix.go"
                 (("/etc/protocols") (string-append net-base "/etc/protocols")))
               (substitute* "time/zoneinfo_unix.go"
                 (("/usr/share/zoneinfo/") tzdata-path))
               (substitute* (find-files "cmd" "asm.c")
                 (("/lib/ld-linux.*\\.so\\.[0-9]") loader))
               #t)))

         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; FIXME: Some of the .a files are not bit-reproducible.
             (let* ((output (assoc-ref outputs "out")))
               (setenv "CC" (which "gcc"))
               (setenv "GOOS" "linux")
               (setenv "GOROOT" (dirname (getcwd)))
               (setenv "GOROOT_FINAL" output)
               ;; Go 1.4's cgo will not work with binutils >= 2.27:
               ;; https://github.com/golang/go/issues/16906
               (setenv "CGO_ENABLED" "0")
               (invoke "sh" "all.bash"))))

         (replace 'install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((output (assoc-ref outputs "out"))
                    (doc_out (assoc-ref outputs "doc"))
                    (bash (string-append (assoc-ref inputs "bash") "bin/bash"))
                    (docs (string-append doc_out "/share/doc/" ,name "-" ,version))
                    (tests (string-append
                            (assoc-ref outputs "tests") "/share/" ,name "-" ,version)))
               (mkdir-p tests)
               (copy-recursively "../test" (string-append tests "/test"))
               (delete-file-recursively "../test")
               (mkdir-p docs)
               (copy-recursively "../api" (string-append docs "/api"))
               (delete-file-recursively "../api")
               (copy-recursively "../doc" (string-append docs "/doc"))
               (delete-file-recursively "../doc")

               (for-each (lambda (file)
                           (let ((file (string-append "../" file)))
                             (install-file file docs)
                             (delete-file file)))
                         '("README" "CONTRIBUTORS" "AUTHORS" "PATENTS"
                           "LICENSE" "VERSION" "robots.txt"))
               (copy-recursively "../" output)
               #t))))))
    (inputs
     `(("tzdata" ,tzdata)
       ("pcre" ,pcre)
       ;; Building Go 1.10 with the Go 1.4 bootstrap, Thread Sanitizer from GCC
       ;; 5 finds a data race during the the test suite of Go 1.10. With GCC 6,
       ;; the race doesn't seem to be present:
       ;; https://github.com/golang/go/issues/24046
       ("gcc:lib" ,gcc-6 "lib")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)
       ("net-base" ,net-base)
       ("perl" ,perl)))

    (home-page "https://golang.org/")
    (synopsis "Compiler and libraries for Go, a statically-typed language")
    (description "Go, also commonly referred to as golang, is an imperative
programming language designed primarily for systems programming.  Go is a
compiled, statically typed language in the tradition of C and C++, but adds
garbage collection, various safety features, and concurrent programming features
in the style of communicating sequential processes (@dfn{CSP}).")
    (supported-systems '("x86_64-linux" "i686-linux" "armhf-linux"))
    (license license:bsd-3)))

(define-public go-1.9
  (package
    (inherit go-1.4)
    (name "go")
    (version "1.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://storage.googleapis.com/golang/"
                           name version ".src.tar.gz"))
       (sha256
        (base32
         "08kpy874x0rx43zpyv5kwd8xj2ma91xm33i0ka2v1v788px18a2q"))))
    (arguments
     (substitute-keyword-arguments (package-arguments go-1.4)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'prebuild
             ;; TODO: Most of this could be factorized with Go 1.4.
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((gcclib (string-append (assoc-ref inputs "gcc:lib") "/lib"))
                      (ld (string-append (assoc-ref inputs "libc") "/lib"))
                      (loader (car (find-files ld "^ld-linux.+")))
                      (net-base (assoc-ref inputs "net-base"))
                      (tzdata-path
                       (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo"))
                      (output (assoc-ref outputs "out")))

                 ;; Removing net/ tests, which fail when attempting to access
                 ;; network resources not present in the build container.
                 (for-each delete-file
                           '("net/listen_test.go"
                             "net/parse_test.go"
                             "net/cgo_unix_test.go"))

                 (substitute* "os/os_test.go"
                   (("/usr/bin") (getcwd))
                   (("/bin/pwd") (which "pwd"))
                   (("/bin/sh") (which "sh")))

                 ;; Add libgcc to runpath
                 (substitute* "cmd/link/internal/ld/lib.go"
                   (("!rpath.set") "true"))
                 (substitute* "cmd/go/internal/work/build.go"
                   (("cgoldflags := \\[\\]string\\{\\}")
                    (string-append "cgoldflags := []string{"
                                   "\"-rpath=" gcclib "\""
                                   "}"))
                   (("ldflags = setextld\\(ldflags, compiler\\)")
                    (string-append
                     "ldflags = setextld(ldflags, compiler)\n"
                     "ldflags = append(ldflags, \"-r\")\n"
                     "ldflags = append(ldflags, \"" gcclib "\")\n"))
                   (("\"-lgcc_s\", ")
                    (string-append
                     "\"-Wl,-rpath=" gcclib "\", \"-lgcc_s\", ")))

                 ;; XXX Commit 65fa2b615b72c1fa61a7 in the Go repo.  We do this
                 ;; without a patch because ((guix packages) patch-and-repack)
                 ;; resets file mtimes, and parts of Go's test suite rely on
                 ;; those timestamps.
                 ;; <https://github.com/golang/go/issues/26369>
                 (substitute* "cmd/internal/objfile/elf.go"
                   (("PT_LOAD")
                     "PT_LOAD && p.Flags&elf.PF_X != 0"))

                 ;; Disable failing tests: these tests attempt to access
                 ;; commands or network resources which are neither available
                 ;; nor necessary for the build to succeed.
                 (for-each
                  (match-lambda
                    ((file regex)
                     (substitute* file
                       ((regex all before test_name)
                        (string-append before "Disabled" test_name)))))
                  '(("net/net_test.go" "(.+)(TestShutdownUnix.+)")
                    ("net/dial_test.go" "(.+)(TestDialTimeout.+)")
                    ("os/os_test.go" "(.+)(TestHostname.+)")
                    ("time/format_test.go" "(.+)(TestParseInSydney.+)")
                    ("time/format_test.go" "(.+)(TestParseInLocation.+)")
                    ("os/exec/exec_test.go" "(.+)(TestEcho.+)")
                    ("os/exec/exec_test.go" "(.+)(TestCommandRelativeName.+)")
                    ("os/exec/exec_test.go" "(.+)(TestCatStdin.+)")
                    ("os/exec/exec_test.go" "(.+)(TestCatGoodAndBadFile.+)")
                    ("os/exec/exec_test.go" "(.+)(TestExitStatus.+)")
                    ("os/exec/exec_test.go" "(.+)(TestPipes.+)")
                    ("os/exec/exec_test.go" "(.+)(TestStdinClose.+)")
                    ("os/exec/exec_test.go" "(.+)(TestIgnorePipeErrorOnSuccess.+)")
                    ("syscall/syscall_unix_test.go" "(.+)(TestPassFD\\(.+)")
                    ("os/exec/exec_test.go" "(.+)(TestExtraFiles/areturn.+)")
                    ("cmd/go/go_test.go" "(.+)(TestCoverageWithCgo.+)")
                    ("cmd/go/go_test.go" "(.+)(TestTwoPkgConfigs.+)")
                    ("os/exec/exec_test.go" "(.+)(TestOutputStderrCapture.+)")
                    ("os/exec/exec_test.go" "(.+)(TestExtraFiles.+)")
                    ("os/exec/exec_test.go" "(.+)(TestExtraFilesRace.+)")
                    ("net/lookup_test.go" "(.+)(TestLookupPort.+)")
                    ("syscall/exec_linux_test.go"
                     "(.+)(TestCloneNEWUSERAndRemapNoRootDisableSetgroups.+)")))

                 (substitute* "../misc/cgo/testsanitizers/test.bash"
                   (("(CC=)cc" all var) (string-append var "gcc")))

                 ;; fix shebang for testar script
                 ;; note the target script is generated at build time.
                 (substitute* "../misc/cgo/testcarchive/carchive_test.go"
                   (("#!/usr/bin/env") (string-append "#!" (which "env"))))

                 (substitute* "net/lookup_unix.go"
                   (("/etc/protocols") (string-append net-base "/etc/protocols")))
                 (substitute* "net/port_unix.go"
                   (("/etc/services") (string-append net-base "/etc/services")))
                 (substitute* "time/zoneinfo_unix.go"
                   (("/usr/share/zoneinfo/") tzdata-path))
                 (substitute* (find-files "cmd" "\\.go")
                   (("/lib(64)?/ld-linux.*\\.so\\.[0-9]") loader))
                 #t)))
           (add-before 'build 'set-bootstrap-variables
             (lambda* (#:key outputs inputs #:allow-other-keys)
               ;; Tell the build system where to find the bootstrap Go.
               (let ((go  (assoc-ref inputs "go"))
                     (out (assoc-ref outputs "out")))
                 (setenv "GOROOT_BOOTSTRAP" go)
                 (setenv "PATH"
                         (string-append out "/bin:"
                                        (dirname (getcwd)) "/bin:"
                                        (getenv "PATH")))

                 ;; XXX: The following variables seem unrelated.
                 (setenv "GOGC" "400")
                 (setenv "GO_TEST_TIMEOUT_SCALE" "9999")
                 #t)))

           (replace 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; FIXME: Some of the .a files are not bit-reproducible.
               (let* ((output (assoc-ref outputs "out")))
                 (setenv "CC" (which "gcc"))
                 (setenv "GOOS" "linux")
                 (setenv "GOROOT" (dirname (getcwd)))
                 (setenv "GOROOT_FINAL" output)
                 (setenv "CGO_ENABLED" "1")
                 (invoke "sh" "all.bash"))))

           (replace 'install
             ;; TODO: Most of this could be factorized with Go 1.4.
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((output (assoc-ref outputs "out"))
                      (doc_out (assoc-ref outputs "doc"))
                      (docs (string-append doc_out "/share/doc/" ,name "-" ,version))
                      (src (string-append
                            (assoc-ref outputs "tests") "/share/" ,name "-" ,version)))
                 (delete-file-recursively "../pkg/bootstrap")

                 (mkdir-p src)
                 (copy-recursively "../test" (string-append src "/test"))
                 (delete-file-recursively "../test")
                 (mkdir-p docs)
                 (copy-recursively "../api" (string-append docs "/api"))
                 (delete-file-recursively "../api")
                 (copy-recursively "../doc" (string-append docs "/doc"))
                 (delete-file-recursively "../doc")

                 (for-each
                  (lambda (file)
                    (let* ((filein (string-append "../" file))
                           (fileout (string-append docs "/" file)))
                      (copy-file filein fileout)
                      (delete-file filein)))
                  ;; Note the slightly different file names compared to 1.4.
                  '("README.md" "CONTRIBUTORS" "AUTHORS" "PATENTS"
                    "LICENSE" "VERSION" "CONTRIBUTING.md" "robots.txt"))

                 (copy-recursively "../" output)
                 #t)))))))
    (native-inputs
     `(("go" ,go-1.4)
       ,@(package-native-inputs go-1.4)))
    (supported-systems %supported-systems)))

(define-public go-1.11
  (package
    (inherit go-1.9)
    (name "go")
    (version "1.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://storage.googleapis.com/golang/"
                           name version ".src.tar.gz"))
       (sha256
        (base32
         "05qivf2f59pv4bfrmdr4m0xvswkmvvl9c5a2h5dy45g2k8b8r3sm"))))
    (arguments
     (substitute-keyword-arguments (package-arguments go-1.9)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'prebuild
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((gcclib (string-append (assoc-ref inputs "gcc:lib") "/lib"))
                      (ld (string-append (assoc-ref inputs "libc") "/lib"))
                      (loader (car (find-files ld "^ld-linux.+")))
                      (net-base (assoc-ref inputs "net-base"))
                      (tzdata-path
                       (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo"))
                      (output (assoc-ref outputs "out")))

                 (for-each delete-file
                           ;; Removing net/ tests, which fail when attempting to access
                           ;; network resources not present in the build container.
                           '("net/listen_test.go"
                             "net/parse_test.go"
                             "net/cgo_unix_test.go"
                             ;; A side effect of these test scripts is testing
                             ;; cgo. Attempts at using cgo flags and
                             ;; directives with these scripts as specified
                             ;; here (https://golang.org/cmd/cgo/) have not
                             ;; worked. The tests continue to state that they
                             ;; can not find crt1.o despite being present.
                             "cmd/go/testdata/script/list_compiled_imports.txt"
                             "cmd/go/testdata/script/mod_case_cgo.txt"
                             ;; https://github.com/golang/go/issues/24884
                             "os/user/user_test.go"))

                 (substitute* "os/os_test.go"
                   (("/usr/bin") (getcwd))
                   (("/bin/pwd") (which "pwd"))
                   (("/bin/sh") (which "sh")))

                 (substitute* "cmd/vendor/golang.org/x/sys/unix/syscall_unix_test.go"
                   (("/usr/bin") "/tmp"))

                 ;; Add libgcc to runpath
                 (substitute* "cmd/link/internal/ld/lib.go"
                   (("!rpath.set") "true"))
                 (substitute* "cmd/go/internal/work/gccgo.go"
                   (("cgoldflags := \\[\\]string\\{\\}")
                    (string-append "cgoldflags := []string{"
                                   "\"-rpath=" gcclib "\""
                                   "}"))
                   (("\"-lgcc_s\", ")
                    (string-append
                     "\"-Wl,-rpath=" gcclib "\", \"-lgcc_s\", ")))
                 (substitute* "cmd/go/internal/work/gc.go"
                   (("ldflags = setextld\\(ldflags, compiler\\)")
                    (string-append
                     "ldflags = setextld(ldflags, compiler)\n"
                     "ldflags = append(ldflags, \"-r\")\n"
                     "ldflags = append(ldflags, \"" gcclib "\")\n")))

                 ;; Disable failing tests: these tests attempt to access
                 ;; commands or network resources which are neither available
                 ;; nor necessary for the build to succeed.
                 (for-each
                  (match-lambda
                    ((file regex)
                     (substitute* file
                       ((regex all before test_name)
                        (string-append before "Disabled" test_name)))))
                  '(("net/net_test.go" "(.+)(TestShutdownUnix.+)")
                    ("net/dial_test.go" "(.+)(TestDialTimeout.+)")
                    ("os/os_test.go" "(.+)(TestHostname.+)")
                    ("time/format_test.go" "(.+)(TestParseInSydney.+)")
                    ("time/format_test.go" "(.+)(TestParseInLocation.+)")
                    ("os/exec/exec_test.go" "(.+)(TestEcho.+)")
                    ("os/exec/exec_test.go" "(.+)(TestCommandRelativeName.+)")
                    ("os/exec/exec_test.go" "(.+)(TestCatStdin.+)")
                    ("os/exec/exec_test.go" "(.+)(TestCatGoodAndBadFile.+)")
                    ("os/exec/exec_test.go" "(.+)(TestExitStatus.+)")
                    ("os/exec/exec_test.go" "(.+)(TestPipes.+)")
                    ("os/exec/exec_test.go" "(.+)(TestStdinClose.+)")
                    ("os/exec/exec_test.go" "(.+)(TestIgnorePipeErrorOnSuccess.+)")
                    ("syscall/syscall_unix_test.go" "(.+)(TestPassFD\\(.+)")
                    ("os/exec/exec_test.go" "(.+)(TestExtraFiles/areturn.+)")
                    ("cmd/go/go_test.go" "(.+)(TestCoverageWithCgo.+)")
                    ("cmd/go/go_test.go" "(.+)(TestTwoPkgConfigs.+)")
                    ("os/exec/exec_test.go" "(.+)(TestOutputStderrCapture.+)")
                    ("os/exec/exec_test.go" "(.+)(TestExtraFiles.+)")
                    ("os/exec/exec_test.go" "(.+)(TestExtraFilesRace.+)")
                    ("net/lookup_test.go" "(.+)(TestLookupPort.+)")
                    ("syscall/exec_linux_test.go"
                     "(.+)(TestCloneNEWUSERAndRemapNoRootDisableSetgroups.+)")))

                 ;; fix shebang for testar script
                 ;; note the target script is generated at build time.
                 (substitute* "../misc/cgo/testcarchive/carchive_test.go"
                   (("#!/usr/bin/env") (string-append "#!" (which "env"))))

                 (substitute* "net/lookup_unix.go"
                   (("/etc/protocols") (string-append net-base "/etc/protocols")))
                 (substitute* "net/port_unix.go"
                   (("/etc/services") (string-append net-base "/etc/services")))
                 (substitute* "time/zoneinfo_unix.go"
                   (("/usr/share/zoneinfo/") tzdata-path))
                 (substitute* (find-files "cmd" "\\.go")
                   (("/lib(64)?/ld-linux.*\\.so\\.[0-9]") loader))
                 #t)))
           (replace 'set-bootstrap-variables
             (lambda* (#:key outputs inputs #:allow-other-keys)
               ;; Tell the build system where to find the bootstrap Go.
               (let ((go  (assoc-ref inputs "go")))
                 (setenv "GOROOT_BOOTSTRAP" go)
                 (setenv "GOGC" "400"))))))))))

(define-public go go-1.9)

(define-public go-github-com-alsm-ioprogress
  (let ((commit "063c3725f436e7fba0c8f588547bee21ffec7ac5")
        (revision "0"))
    (package
      (name "go-github-com-alsm-ioprogress")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/alsm/ioprogress.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "10ym5qlq77nynmkxbk767f2hfwyxg2k7hrzph05hvgzv833dhivh"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/alsm/ioprogress"))
      (synopsis "Textual progress bars in Go")
      (description "@code{ioprogress} is a Go library with implementations of
@code{io.Reader} and @code{io.Writer} that draws progress bars.  The primary use
case for these are for command-line applications but alternate progress bar
writers can be supplied for alternate environments.")
      (home-page "https://github.com/alsm/ioprogress")
      (license license:expat))))

(define-public go-github-com-aki237-nscjar
  (let ((commit "e2df936ddd6050d30dd90c7214c02b5019c42f06")
        (revision "0"))
    (package
      (name "go-github-com-aki237-nscjar")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/aki237/nscjar.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "03y7zzq12qvhsq86lb06sgns8xrkblbn7i7wd886wk3zr5574b96"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/aki237/nscjar"))
      (synopsis "Handle Netscape / Mozilla cookies")
      (description "@code{nscjar} is a Go library used to parse and output
Netscape/Mozilla's old-style cookie files.  It also implements a simple cookie
jar struct to manage the cookies added to the cookie jar.")
      (home-page "https://github.com/aki237/nscjar")
      (license license:expat))))

(define-public go-github.com-jessevdk-go-flags
  (package
    (name "go-github.com-jessevdk-go-flags")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jessevdk/go-flags")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jk2k2l10lwrn1r3nxdvbs0yz656830j4khzirw8p4ahs7c5zz36"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jessevdk/go-flags"))
    (synopsis "Go library for parsing command line arguments")
    (description
     "The @code{flags} package provides a command line option parser.  The
functionality is similar to the go builtin @code{flag} package, but
@code{flags} provides more options and uses reflection to provide a succinct
way of specifying command line options.")
    (home-page "https://github.com/jessevdk/go-flags")
    (license license:bsd-3)))

(define-public go-gopkg.in-tomb.v2
  (let ((commit "d5d1b5820637886def9eef33e03a27a9f166942c")
        (revision "0"))
    (package
      (name "go-gopkg.in-tomb.v2")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/go-tomb/tomb.git")
                      (commit commit)))
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32
                  "1sv15sri99szkdz1bkh0ir46w9n8prrwx5hfai13nrhkawfyfy10"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "gopkg.in/tomb.v2"))
      (synopsis "@code{tomb} handles clean goroutine tracking and termination")
      (description
       "The @code{tomb} package handles clean goroutine tracking and
termination.")
      (home-page "https://gopkg.in/tomb.v2")
      (license license:bsd-3))))

(define-public go-github.com-jtolds-gls
  (package
    (name "go-github.com-jtolds-gls")
    (version "4.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jtolds/gls")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vm37pvn0k4r6d3m620swwgama63laz8hhj3pyisdhxwam4m2g1h"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jtolds/gls"))
    (synopsis "@code{gls} provides Goroutine local storage")
    (description
     "The @code{gls} package provides a way to store a retrieve values
per-goroutine.")
    (home-page "https://github.com/jtolds/gls")
    (license license:expat)))

(define-public go-github-com-tj-docopt
  (package
    (name "go-github-com-tj-docopt")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tj/docopt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06h8hdg1mh3s78zqlr01g4si7k0f0g6pr7fj7lnvfg446hgc7080"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tj/docopt"))
    (synopsis "Go implementation of docopt")
    (description
     "This library allows the user to define a command-line interface from a
program's help message rather than specifying it programatically with
command-line parsers.")
    (home-page "https://github.com/tj/docopt")
    (license license:expat)))

(define-public go-github-com-hashicorp-hcl
  (let ((commit "23c074d0eceb2b8a5bfdbb271ab780cde70f05a8")
        (revision "0"))
    (package
      (name "go-github-com-hashicorp-hcl")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/hashicorp/hcl")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                  (base32
                    "0db4lpqb5m130rmfy3s3gjjf4dxllypmyrzxv6ggqhkmwmc7w4mc"))))
      (build-system go-build-system)
      (arguments
       '(#:tests? #f
         #:import-path "github.com/hashicorp/hcl"))
      (synopsis "Go implementation of HashiCorp Configuration Language")
      (description
       "This package contains the main implementation of the @acronym{HCL,
HashiCorp Configuration Language}.  HCL is designed to be a language for
expressing configuration which is easy for both humans and machines to read.")
      (home-page "https://github.com/hashicorp/hcl")
      (license license:mpl2.0))))

(define-public go-golang-org-x-crypto-bcrypt
  (let ((commit "95a4943f35d008beabde8c11e5075a1b714e6419")
        (revision "1"))
    (package
      (name "go-golang-org-x-crypto-bcrypt")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0bkm0jx9mxmi1liabb9c04kf765n7d0062zdp3zmvzyamsq00lcx"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/bcrypt"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Bcrypt in Go")
      (description "This package provides a Go implementation of the bcrypt
password hashing function.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-blowfish
  (let ((commit "95a4943f35d008beabde8c11e5075a1b714e6419")
        (revision "1"))
    (package
      (name "go-golang-org-x-crypto-blowfish")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0bkm0jx9mxmi1liabb9c04kf765n7d0062zdp3zmvzyamsq00lcx"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/blowfish"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Blowfish in Go")
      (description "This package provides a Go implementation of the Blowfish
symmetric-key block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-pbkdf2
  (let ((commit "95a4943f35d008beabde8c11e5075a1b714e6419")
        (revision "1"))
    (package
      (name "go-golang-org-x-crypto-pbkdf2")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0bkm0jx9mxmi1liabb9c04kf765n7d0062zdp3zmvzyamsq00lcx"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/pbkdf2"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "PBKDF2 in Go")
      (description "This package provides a Go implementation of the PBKDF2 key
derivation function.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-tea
  (let ((commit "95a4943f35d008beabde8c11e5075a1b714e6419")
        (revision "1"))
    (package
      (name "go-golang-org-x-crypto-tea")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0bkm0jx9mxmi1liabb9c04kf765n7d0062zdp3zmvzyamsq00lcx"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/tea"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Tiny Encryption Algorithm (TEA) in Go")
      (description "This packages a Go implementation of the Tiny Encryption
Algorithm (TEA) block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-salsa20
  (let ((commit "95a4943f35d008beabde8c11e5075a1b714e6419")
        (revision "1"))
    (package
      (name "go-golang-org-x-crypto-salsa20")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0bkm0jx9mxmi1liabb9c04kf765n7d0062zdp3zmvzyamsq00lcx"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/salsa20"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Salsa20 in Go")
      (description "This packages provides a Go implementation of the Salsa20
stream cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-cast5
  (let ((commit "95a4943f35d008beabde8c11e5075a1b714e6419")
        (revision "1"))
    (package
      (name "go-golang-org-x-crypto-cast5")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0bkm0jx9mxmi1liabb9c04kf765n7d0062zdp3zmvzyamsq00lcx"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/cast5"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Cast5 in Go")
      (description "This packages provides a Go implementation of the Cast5
symmetric-key block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-twofish
  (let ((commit "95a4943f35d008beabde8c11e5075a1b714e6419")
        (revision "1"))
    (package
      (name "go-golang-org-x-crypto-twofish")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0bkm0jx9mxmi1liabb9c04kf765n7d0062zdp3zmvzyamsq00lcx"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/twofish"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Twofish in Go")
      (description "This packages provides a Go implementation of the Twofish
symmetric-key block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-xtea
  (let ((commit "95a4943f35d008beabde8c11e5075a1b714e6419")
        (revision "1"))
    (package
      (name "go-golang-org-x-crypto-xtea")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0bkm0jx9mxmi1liabb9c04kf765n7d0062zdp3zmvzyamsq00lcx"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/xtea"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "eXtended Tiny Encryption Algorithm (XTEA) in Go")
      (description "This package provides a Go implementation of the eXtended
Tiny Encryption Algorithm (XTEA) block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-net-ipv4
  (let ((commit "d866cfc389cec985d6fda2859936a575a55a3ab6")
        (revision "1"))
    (package
      (name "go-golang-org-x-net-ipv4")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "10iahqcsiih5hgmqw8yfgv5b3fimfwl1skxg5062avcjjks59f03"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net/ipv4"
         #:unpack-path "golang.org/x/net"))
      (synopsis "Go IPv4 support")
      (description "This package provides @code{ipv4}, which implements IP-level
socket options for the Internet Protocol version 4.")
      (home-page "https://go.googlesource.com/net")
      (license license:bsd-3))))

(define-public go-golang-org-x-net-bpf
  (let ((commit "d866cfc389cec985d6fda2859936a575a55a3ab6")
        (revision "1"))
    (package
      (name "go-golang-org-x-net-bpf")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-net-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "10iahqcsiih5hgmqw8yfgv5b3fimfwl1skxg5062avcjjks59f03"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net/bpf"
         #:unpack-path "golang.org/x/net"))
      (synopsis "Berkeley Packet Filters (BPF) in Go")
      (description "This packages provides a Go implementation of the Berkeley
Packet Filter (BPF) virtual machine.")
      (home-page "https://go.googlesource.com/net/")
      (license license:bsd-3))))

(define-public go-golang-org-x-net-context
  (let ((commit "d866cfc389cec985d6fda2859936a575a55a3ab6")
        (revision "1"))
    (package
      (name "go-golang-org-x-net-context")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-net-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "10iahqcsiih5hgmqw8yfgv5b3fimfwl1skxg5062avcjjks59f03"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net/context"
         #:unpack-path "golang.org/x/net"))
      (synopsis "Golang Context type")
      (description "This packages provides @code{context}, which defines the
Context type, which carries deadlines, cancelation signals, and other
request-scoped values across API boundaries and between processes.")
      (home-page "https://go.googlesource.com/net/")
      (license license:bsd-3))))

(define-public go-golang-org-x-net-internal-iana
  (let ((commit "d866cfc389cec985d6fda2859936a575a55a3ab6")
        (revision "1"))
    (package
      (name "go-golang-org-x-net-internal-iana")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-net-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "10iahqcsiih5hgmqw8yfgv5b3fimfwl1skxg5062avcjjks59f03"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net/internal/iana"
         #:unpack-path "golang.org/x/net"))
      (synopsis "Go support for assigned numbers (IANA)")
      (description "This packages provides @code{iana}, which provides protocol
number resources managed by the Internet Assigned Numbers Authority (IANA).")
      (home-page "https://go.googlesource.com/net/")
      (license license:bsd-3))))

(define-public go-golang-org-x-net-ipv6
  (let ((commit "d866cfc389cec985d6fda2859936a575a55a3ab6")
        (revision "1"))
    (package
      (name "go-golang-org-x-net-ipv6")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-net-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "10iahqcsiih5hgmqw8yfgv5b3fimfwl1skxg5062avcjjks59f03"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net/ipv6"
         #:unpack-path "golang.org/x/net"))
      (synopsis "Go IPv6 support")
      (description "This packages provides @code{ipv6}, which implements
IP-level socket options for the Internet Protocol version 6.")
      (home-page "https://go.googlesource.com/net")
      (license license:bsd-3))))

(define-public go-golang-org-x-net-proxy
  (let ((commit "d866cfc389cec985d6fda2859936a575a55a3ab6")
        (revision "1"))
    (package
      (name "go-golang-org-x-net-proxy")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-net-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "10iahqcsiih5hgmqw8yfgv5b3fimfwl1skxg5062avcjjks59f03"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net/proxy"
         #:unpack-path "golang.org/x/net/"))
      (synopsis "Go support for network proxies")
      (description "This packages provides @code{proxy}, which provides support
for a variety of protocols to proxy network data.")
      (home-page "https://go.googlesource.com/net")
      (license license:bsd-3))))

(define-public go-golang-org-x-sys-unix
  (let ((commit "83801418e1b59fb1880e363299581ee543af32ca")
        (revision "1"))
    (package
      (name "go-golang-org-x-sys-unix")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/sys")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ilykaanvnzb27d42kmbr4i37hcn7hgqbx98z945gy63aa8dskji"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/sys/unix"
         #:unpack-path "golang.org/x/sys"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-tests
             (lambda _
               (pk (getcwd))
               (substitute* "src/golang.org/x/sys/unix/syscall_unix_test.go"
                 (("/usr/bin") "/tmp"))
               #t)))))
      (synopsis "Go support for low-level system interaction")
      (description "This package provides @code{unix}, which offers Go support
for low-level interaction with the operating system.")
      (home-page "https://go.googlesource.com/sys")
      (license license:bsd-3))))

(define-public go-golang-org-x-text-transform
  (let ((commit "e19ae1496984b1c655b8044a65c0300a3c878dd3")
        (revision "1"))
    (package
      (name "go-golang-org-x-text-transform")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/text")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-text-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1cvnnx8nwx5c7gr6ajs7sldhbqh52n7h6fsa3i21l2lhx6xrsh4w"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/text/transform"
         #:unpack-path "golang.org/x/text"))
      (synopsis "Go text transformation")
      (description "This package provides @code{transform}, which provides
reader and writer wrappers that transform the bytes passing through.  Example
transformations provided by other packages include normalization and conversion
between character sets.")
      (home-page "https://go.googlesource.com/text")
      (license license:bsd-3))))

(define-public go-golang-org-x-text-unicode-norm
  (let ((commit "e19ae1496984b1c655b8044a65c0300a3c878dd3")
        (revision "1"))
    (package
      (name "go-golang-org-x-text-unicode-norm")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/text")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-text-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1cvnnx8nwx5c7gr6ajs7sldhbqh52n7h6fsa3i21l2lhx6xrsh4w"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/text/unicode/norm"
         #:unpack-path "golang.org/x/text"))
      (synopsis "Unicode normalization in Go")
      (description "This package provides @code{norm}, which contains types and
functions for normalizing Unicode strings.")
      (home-page "https://go.googlesource.com/text")
      (license license:bsd-3))))

(define-public go-golang-org-x-time-rate
  (let ((commit "6dc17368e09b0e8634d71cac8168d853e869a0c7")
        (revision "1"))
    (package
      (name "go-golang-org-x-time-rate")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/time")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1fx4cf5fpdz00g3c7vxzy92hdcg0vh4yqw00qp5s52j72qixynbk"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/time/rate"
         #:unpack-path "golang.org/x/time"))
      (propagated-inputs
       `(("go-golang-org-x-net-context" ,go-golang-org-x-net-context)))
      (synopsis "Rate limiting in Go")
      (description "This package provides @{rate}, which implements rate
limiting in Go.")
      (home-page "https://godoc.org/golang.org/x/time/rate")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-ssh-terminal
  (let ((commit "95a4943f35d008beabde8c11e5075a1b714e6419")
        (revision "1"))
    (package
      (name "go-golang-org-x-crypto-ssh-terminal")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0bkm0jx9mxmi1liabb9c04kf765n7d0062zdp3zmvzyamsq00lcx"))))
      (build-system go-build-system)
      (inputs
       `(("go-golang-org-x-sys-unix" ,go-golang-org-x-sys-unix)))
      (arguments
       `(#:import-path "golang.org/x/crypto/ssh/terminal"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Terminal functions for Go")
      (description "This package provides @{terminal}, which implements
support functions for dealing with terminals, as commonly found on UNIX
systems.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-github-com-burntsushi-toml
  (let ((commit
         "a368813c5e648fee92e5f6c30e3944ff9d5e8895")
        (revision "0"))
    (package
      (name "go-github-com-burntsushi-toml")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/BurntSushi/toml.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1sjxs2lwc8jpln80s4rlzp7nprbcljhy5mz4rf9995gq93wqnym5"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/BurntSushi/toml"))
      (home-page "https://github.com/BurntSushi/toml")
      (synopsis "Toml parser and encoder for Go")
      (description "This package is toml parser and encoder for Go.  The
interface is similar to Go's standard library @code{json} and @code{xml}
package.")
      (license license:expat))))

(define-public go-github-com-getsentry-raven-go
  (let ((commit
         "dffeb57df75d6a911f00232155194e43d79d38d7")
        (revision "0"))
    (package
      (name "go-github-com-getsentry-raven-go")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/getsentry/raven-go.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "13sb9rvl3369m7fah3ss9g0hwky259snqfn8gmbr0h5zvp651lja"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/getsentry/raven-go"))
      (home-page
       "https://github.com/getsentry/raven-go")
      (synopsis "Sentry client in Go")
      (description "This package is Go client API for the Sentry event/error
logging system.")
      (license license:bsd-3))))

(define-public go-github-com-hashicorp-go-version
  (let ((commit
         "03c5bf6be031b6dd45afec16b1cf94fc8938bc77")
        (revision "0"))
    (package
      (name "go-github-com-hashicorp-go-version")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hashicorp/go-version.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0sjq57gpfznaqdrbyb2p0bn90g9h661cvr0jrk6ngags4pbw14ik"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/hashicorp/go-version"))
      (home-page
       "https://github.com/hashicorp/go-version")
      (synopsis "Go library for parsing and verifying versions and version
constraints")
      (description "This package is a library for parsing versions and version
constraints, and verifying versions against a set of constraints.  It can sort
a collection of versions properly, handles prerelease/beta versions, can
increment versions.")
      (license license:mpl2.0))))

(define-public go-github-com-jpillora-backoff
  (let ((commit
         "06c7a16c845dc8e0bf575fafeeca0f5462f5eb4d")
        (revision "0"))
    (package
      (name "go-github-com-jpillora-backoff")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jpillora/backoff.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xhvxr7bm47czdc5hy3kl508z3y4j91i2jm7vg774i52zych6k4l"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/jpillora/backoff"))
      (home-page "https://github.com/jpillora/backoff")
      (synopsis "Simple exponential backoff counter in Go")
      (description "This package is a simple exponential backoff counter in
Go.")
      (license license:expat))))

(define-public go-github-com-stretchr-testify
  (let ((commit
          "b1f989447a57594c728884458a39abf3a73447f7")
        (revision "0"))
    (package
      (name "go-github-com-stretchr-testify")
      (version (git-version "1.1.4" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/stretchr/testify.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "0p0gkqzh2p8r5g0rxm885ljl7ghih7h7hx9w562imx5ka0vdgixv"))))
      (build-system go-build-system)
      (arguments
        '(#:import-path "github.com/stretchr/testify"))
      (home-page "https://github.com/stretchr/testify")
      (synopsis "Go helper library for tests and invariant checking")
      (description "This package provide many tools for testifying that your
code will behave as you intend.

Features include:
@itemize
@item Easy assertions
@item Mocking
@item HTTP response trapping
@item Testing suite interfaces and functions.
@end itemize")
      (license license:expat))))

(define-public go-github-com-tevino-abool
  (let ((commit
          "3c25f2fe7cd0ef3eabefce1d90efd69a65d35b12")
        (revision "0"))
    (package
      (name "go-github-com-tevino-abool")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/tevino/abool.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "1wxqrclxk93q0aj15z596dx2y57x9nkhi64nbrr5cxnhxn8vwixm"))))
      (build-system go-build-system)
      (arguments
        '(#:import-path "github.com/tevino/abool"))
      (home-page "https://github.com/tevino/abool")
      (synopsis "Atomic boolean library for Go code")
      (description "This package is atomic boolean library for Go code,
optimized for performance yet simple to use.")
      (license license:expat))))

(define-public go-github-com-urfave-cli
  (let ((commit "cfb38830724cc34fedffe9a2a29fb54fa9169cd1")
        (revision "0"))
    (package
      (name "go-github-com-urfave-cli")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/urfave/cli.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0y6f4sbzkiiwrxbl15biivj8c7qwxnvm3zl2dd3mw4wzg4x10ygj"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/urfave/cli"))
      (home-page "https://github.com/urfave/cli")
      (synopsis "Library for building command-line interfaces in Go")
      (description "This package provides a library for building command-line
interfaces in Go.")
      (license license:expat))))

(define-public go-github-com-blang-semver
  (let ((commit "60ec3488bfea7cca02b021d106d9911120d25fe9")
        (revision "0"))
    (package
      (name "go-github-com-blang-semver")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/blang/semver.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "19pli07y5592g4dyjyj0jq5rn548vc3fz0qg3624vm1j5828p1c2"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/blang/semver"))
      (home-page "https://github.com/blang/semver")
      (synopsis "Semantic versioning library written in Go")
      (description "Semver is a library for Semantic versioning written in Go.")
      (license license:expat))))

(define-public go-github-com-emicklei-go-restful
  (let ((commit "89ef8af493ab468a45a42bb0d89a06fccdd2fb22")
        (revision "0"))
    (package
      (name "go-github-com-emicklei-go-restful")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emicklei/go-restful.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0rrlfcfq80fkxifpih6bq31vavb5mf4530xz51pp9pq1mn2fzjfh"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/emicklei/go-restful"))
      (home-page "https://github.com/emicklei/go-restful")
      (synopsis "Build REST-style web services using Go")
      (description "This package provides @code{go-restful}, which helps
developers to use @code{http} methods explicitly and in a way that's consistent
with the HTTP protocol definition.")
      (license license:expat))))

(define-public go-github-com-google-cadvisor
  (let ((commit "2ed7198f77395ee9a172878a0a7ab92ab59a2cfd")
        (revision "0"))
    (package
      (name "go-github-com-google-cadvisor")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/cadvisor.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1w8p345z5j0gk3yiq5ah0znd5lfh348p2s624k5r10drz04p3f55"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/google/cadvisor"))
      (home-page "https://github.com/google/cadvisor")
      (synopsis "Analyze resource usage of running containers")
      (description "The package provides @code{cadvisor}, which provides
information about the resource usage and preformance characteristics of running
containers.")
      (license license:asl2.0))))

(define-public go-github-com-google-gofuzz
  (let ((commit "fd52762d25a41827db7ef64c43756fd4b9f7e382")
        (revision "0"))
    (package
      (name "go-github-com-google-gofuzz")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/gofuzz.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1yxmmr73h0lq7ryf3q9a7pcm2x5xrg4d5bxkq8n5pxwxwyq26kw8"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/google/gofuzz"))
      (home-page "https://github.com/google/gofuzz")
      (synopsis "Fuzz testing library for Go")
      (description "Gofuzz is a library for populationg Go objects with random
values for the purpose of fuzz testing.")
      (license license:asl2.0))))

(define-public go-github-com-gorilla-context
  (let ((commit "08b5f424b9271eedf6f9f0ce86cb9396ed337a42")
        (revision "0"))
    (package
      (name "go-github-com-gorilla-context")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gorilla/context.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "03p4hn87vcmfih0p9w663qbx9lpsf7i7j3lc7yl7n84la3yz63m4"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/gorilla/context"))
      (home-page "https://github.com/gorilla/context")
      (synopsis "Go registry for request variables")
      (description "This package provides @code{gorilla/context}, which is a general purpose registry for global request variables in the Go programming language.")
      (license license:bsd-3))))

(define-public go-github-com-gorilla-mux
  (let ((commit "599cba5e7b6137d46ddf58fb1765f5d928e69604")
        (revision "0"))
    (package
      (name "go-github-com-gorilla-mux")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gorilla/mux.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0wd6jjii1kg5s0nk3ri6gqriz6hbd6bbcn6x4jf8n7ncrb8qsxyz"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/gorilla/mux"))
      (home-page "https://github.com/gorilla/mux")
      (synopsis "URL router and dispatcher for Go")
      (description
       "Gorilla/Mux implements a request router and dispatcher for matching
incoming requests with their respective handler.")
      (license license:bsd-3))))

(define-public go-github-com-jonboulle-clockwork
  (let ((commit "e3653ace2d63753697e0e5b07b9393971c0bba9d")
        (revision "0"))
    (package
      (name "go-github-com-jonboulle-clockwork")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/jonboulle/clockwork.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "1avzqhks12a8x2yzpvjsf3k0gv9cy7zx2z88hn0scacnxkphisvc"))))
      (build-system go-build-system)
      (arguments
        '(#:import-path "github.com/jonboulle/clockwork"))
      (home-page "https://github.com/jonboulle/clockwork")
      (synopsis "Fake clock library for Go")
      (description
       "Replace uses of the @code{time} package with the
@code{clockwork.Clock} interface instead.")
      (license license:asl2.0))))

(define-public go-github-com-spf13-pflag
  (let ((commit "4f9190456aed1c2113ca51ea9b89219747458dc1")
        (revision "0"))
    (package
      (name "go-github-com-spf13-pflag")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/spf13/pflag.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "12vrlcsbwjqlfc49rwky45mbcj74c0kb6z54354pzas6fwzyi1kc"))))
      (build-system go-build-system)
      (arguments
        '(#:import-path "github.com/spf13/pflag"))
      (home-page "https://github.com/spf13/pflag")
      (synopsis "Replacement for Go's @code{flag} package")
      (description
       "Pflag is library to replace Go's @code{flag} package.  It implements
POSIX/GNU-style command-line options with double hyphens.  It is is compatible
with the
@uref{https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html,
GNU extensions} to the POSIX recommendations for command-line options.")
      (license license:bsd-3))))

(define-public go-github-com-sirupsen-logrus
  (package
    (name "go-github-com-sirupsen-logrus")
    (version "1.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sirupsen/logrus.git")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0g5z7al7kky11ai2dhac6gkp3b5pxsvx72yj3xg4wg3265gbn7yz"))))
    (build-system go-build-system)
    (native-inputs
     `(("go-golang-org-x-crypto-ssh-terminal"
        ,go-golang-org-x-crypto-ssh-terminal)
       ("go-github-com-stretchr-testify"
        ,go-github-com-stretchr-testify)
       ("go-golang-org-x-sys-unix"
        ,go-golang-org-x-sys-unix)))
    (arguments
     '(#:tests? #f                    ;FIXME missing dependencies
       #:import-path "github.com/sirupsen/logrus"))
    (home-page "https://github.com/sirupsen/logrus")
    (synopsis "Structured, pluggable logging for Go")
    (description "Logrus is a structured logger for Go, completely API
compatible with the standard library logger.")
    (license license:expat)))

(define-public go-github-com-kardianos-osext
  (let ((commit "ae77be60afb1dcacde03767a8c37337fad28ac14")
        (revision "1"))
    (package
      (name "go-github-com-kardianos-osext")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/kardianos/osext")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "056dkgxrqjj5r18bnc3knlpgdz5p3yvp12y4y978hnsfhwaqvbjz"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/kardianos/osext"
         ;; The tests are flaky:
         ;; <https://github.com/kardianos/osext/issues/21>
         #:tests? #f))
      (synopsis "Find the running executable")
      (description "Osext provides a method for finding the current executable
file that is running.  This can be used for upgrading the current executable or
finding resources located relative to the executable file.")
      (home-page "https://github.com/kardianos/osext")
      (license license:bsd-3))))

(define-public go-github-com-ayufan-golang-kardianos-service
  (let ((commit "0c8eb6d8fff2e2fb884a7bfd23e183fb63c0eff3")
        (revision "0"))
    (package
      (name "go-github-com-ayufan-golang-kardianos-service")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/ayufan/golang-kardianos-service.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0x0cn7l5gda2khsfypix7adxd5yqighzn04mxjw6hc4ayrh7his5"))))
      (build-system go-build-system)
      (native-inputs
       `(("go-github-com-kardianos-osext"
          ,go-github-com-kardianos-osext)))
      (arguments
       '(#:tests? #f                ;FIXME tests fail: Service is not running.
         #:import-path "github.com/ayufan/golang-kardianos-service"))
      (home-page "https://github.com/ayufan/golang-kardianos-service")
      (synopsis "Go interface to a variety of service supervisors")
      (description "This package provides @code{service}, a Go module that can
run programs as a service using a variety of supervisors, including systemd,
SysVinit, and more.")
      (license license:zlib))))

(define-public go-github-com-docker-distribution
  (let ((commit "325b0804fef3a66309d962357aac3c2ce3f4d329")
        (revision "0"))
    (package
      (name "go-github-com-docker-distribution")
      (version (git-version "0.0.0" revision commit))
      (source
       ;; FIXME: This bundles many things, see
       ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31881#41>.
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/docker/distribution")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1yg2zrikn3vkvkx5mn51p6bfjk840qdkn7ahhhvvcsc8mpigrjc6"))))
      (build-system go-build-system)
      (native-inputs
       `(("go-golang-org-x-sys-unix"
          ,go-golang-org-x-sys-unix)
         ("go-github-com-sirupsen-logrus"
          ,go-github-com-sirupsen-logrus)
         ("go-golang-org-x-crypto-ssh-terminal"
          ,go-golang-org-x-crypto-ssh-terminal)))
      (arguments
       '(#:import-path "github.com/docker/distribution"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                     (assoc-ref outputs "out")
                     ".*\\.gz$"))
               #t)))))
      (home-page
       "https://github.com/docker/distribution")
      (synopsis "This package is Docker toolset to pack, ship, store, and
deliver content")
      (description "Docker Distribution is Docker toolset to pack, ship,
store, and deliver content.  It's containe Docker Registry 2.0 and libraries
to interacting with distribution components.")
      (license license:asl2.0))))

(define-public go-github-com-docker-go-connections
  (let ((commit "3ede32e2033de7505e6500d6c868c2b9ed9f169d")
        (revision "0"))
    (package
      (name "go-github-com-docker-go-connections")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/docker/go-connections.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "0v1pkr8apwmhyzbjfriwdrs1ihlk6pw7izm57r24mf9jdmg3fyb0"))))
      (build-system go-build-system)
      (arguments
        '(#:import-path "github.com/docker/go-connections"))
      (home-page "https://github.com/docker/go-connections")
      (synopsis "Networking library for Go")
      (description
       "This packages provides a library to work with network connections in
the Go language.  In particular it provides tools to deal with network address
translation (NAT), proxies, sockets, and transport layer security (TLS).")
      (license license:asl2.0))))

(define-public go-github-com-docker-machine
  (let ((commit "7b7a141da84480342357c51838be142bf183b095")
        (revision "0"))
    (package
      (name "go-github-com-docker-machine")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/docker/machine.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0bavk0lvs462yh0lnmnxi9psi5qv1x3nvzmd2b0drsahlp1gxi8s"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/docker/machine"))
      (home-page "https://github.com/docker/machine")
      (synopsis "Machine management for a container-centric world")
      (description
       "@dfn{Machine} lets you create Docker hosts on your computer, on
hosting providers, and inside your data center.  It creates servers, installs
Docker on them, then configures the Docker client to talk to them.")
      (license license:asl2.0))))

(define-public go-github-com-gorhill-cronexpr
  (let ((commit "f0984319b44273e83de132089ae42b1810f4933b")
        (revision "0"))
    (package
      (name "go-github-com-gorhill-cronexpr")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gorhill/cronexpr.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0dphhhqy3i7265znv3m8n57l80dmaq6z4hsj5kgd87qd19z8x0l2"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/gorhill/cronexpr"))
      (home-page "https://github.com/gorhill/cronexpr")
      (synopsis "Cron expression parser in the Go language")
      (description
       "This package provides a cron expression parser in the Go language.
Given a cron expression and a time stamp, you can get the next time stamp
which satisfies the cron expression.")
      (license (list license:gpl3+
                     license:asl2.0)))))

(define-public go-gopkg-in-check-v1
  (let ((commit "20d25e2804050c1cd24a7eea1e7a6447dd0e74ec")
        (revision "0"))
    (package
      (name "go-gopkg-in-check-v1")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/go-check/check")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0k1m83ji9l1a7ng8a7v40psbymxasmssbrrhpdv2wl4rhs0nc3np"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "gopkg.in/check.v1"))
      (home-page "https://gopkg.in/check.v1")
      (synopsis "Test framework for the Go language")
      (description
       "This package provides a test library for the Go language.")
      (license license:asl2.0))))

(define-public go-gopkg-in-yaml-v2
  (let ((commit "14227de293ca979cf205cd88769fe71ed96a97e2")
        (revision "0"))
    (package
      (name "go-gopkg-in-yaml-v2")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://gopkg.in/yaml.v2.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "038hnrjcnjygyi3qidfrkpkakis82qg381sr495d2s40g2dwlzah"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "gopkg.in/yaml.v2"))
      (native-inputs
       `(("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)))
      (home-page "https://gopkg.in/yaml.v2")
      (synopsis "YAML reader and writer for the Go language")
      (description
       "This package provides a Go library for encode and decode YAML
values.")
      (license license:asl2.0))))

(define-public go-github-com-mattn-go-isatty
  (let ((commit "6ca4dbf54d38eea1a992b3c722a76a5d1c4cb25c")
        (revision "0"))
    (package
      (name "go-github-com-mattn-go-isatty")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mattn/go-isatty")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0zs92j2cqaw9j8qx1sdxpv3ap0rgbs0vrvi72m40mg8aa36gd39w"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/mattn/go-isatty"))
      (home-page "https://github.com/mattn/go-isatty")
      (synopsis "Provide @code{isatty} for Golang")
      (description "This package provides @code{isatty}, a Go module that can
tell you whether a file descriptor points to a terminal and the type of the
terminal.")
      (license license:expat))))

(define-public go-github-com-mattn-go-colorable
  (let ((commit "efa589957cd060542a26d2dd7832fd6a6c6c3ade")
        (revision "0"))
    (package
      (name "go-github-com-mattn-go-colorable")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mattn/go-colorable")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0kshi4hvm0ayrsxqxy0599iv81kryhd2fn9lwjyczpj593cq069r"))))
      (build-system go-build-system)
      (native-inputs
       `(("go-github-com-mattn-go-isatty"
          ,go-github-com-mattn-go-isatty)))
      (arguments
       '(#:import-path "github.com/mattn/go-colorable"))
      (home-page "https://github.com/mattn/go-colorable")
      (synopsis "Handle ANSI color escapes on Windows")
      (description "This package provides @code{colorable}, a module that
makes it possible to handle ANSI color escapes on Windows.")
      (license license:expat))))

(define-public go-github-com-mgutz-ansi
  (let ((commit "9520e82c474b0a04dd04f8a40959027271bab992")
        (revision "0"))
    (package
      (name "go-github-com-mgutz-ansi")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/mgutz/ansi")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00bz22314j26736w1f0q4jy9d9dfaml17vn890n5zqy3cmvmww1j"))))
      (build-system go-build-system)
      (native-inputs
       `(("go-github-com-mattn-go-isatty"
          ,go-github-com-mattn-go-isatty)
         ("go-github-com-mattn-go-colorable"
          ,go-github-com-mattn-go-colorable)))
      (arguments
       '(#:import-path "github.com/mgutz/ansi"))
      (home-page "https://github.com/mgutz/ansi")
      (synopsis "Small, fast library to create ANSI colored strings and codes")
      (description "This package provides @code{ansi}, a Go module that can
generate ANSI colored strings.")
      (license license:expat))))

(define-public go-github-com-aarzilli-golua
  (let ((commit "03fc4642d792b1f2bc5e7343b403cf490f8c501d")
        (revision "0"))
    (package
      (name "go-github-com-aarzilli-golua")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/aarzilli/golua")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1d9hr29i36cza98afj3g6rs3l7xbkprwzz0blcxsr9dd7nak20di"))))
      (build-system go-build-system)
      (native-inputs
       `(("lua" ,lua)))
      (arguments
       `(#:unpack-path "github.com/aarzilli/golua"
         #:import-path "github.com/aarzilli/golua/lua"
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key import-path #:allow-other-keys)
               (invoke "go" "install"
                       "-v"  ; print the name of packages as they are compiled
                       "-x"  ; print each command as it is invoked
                       "-ldflags=-s -w" ; strip the symbol table and debug
                       "-tags" "llua" ; Latest Lua on Guix does not have a version number.
                       import-path)))
           (replace 'check
             (lambda* (#:key import-path #:allow-other-keys)
               (invoke "go" "test"
                       "-tags" "llua" ; Latest Lua on Guix does not have a version number.
                       import-path))))))
      (home-page "https://github.com/aarzilli/golua")
      (synopsis "Go Bindings for the Lua C API")
      (description "This package provides @code{lua}, a Go module that can
run a Lua virtual machine.")
      (license license:expat))))

(define-public go-gitlab-com-ambrevar-golua-unicode
  (let ((commit "97ce517e7a1fe2407a90c317a9c74b173d396144")
        (revision "0"))
    (package
      (name "go-gitlab-com-ambrevar-golua-unicode")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://gitlab.com/ambrevar/golua")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1izcp7p8nagjwqd13shb0020w7xhppib1a3glw2d1468bflhksnm"))))
      (build-system go-build-system)
      (native-inputs
       `(("lua" ,lua)
         ("go-github-com-aarzilli-golua" ,go-github-com-aarzilli-golua)))
      (arguments
       `(#:unpack-path "gitlab.com/ambrevar/golua"
         #:import-path "gitlab.com/ambrevar/golua/unicode"
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key import-path #:allow-other-keys)
               (setenv "USER" "homeless-dude")
               (invoke "go" "test" import-path))))))
      (home-page "https://gitlab.com/ambrevar/golua")
      (synopsis "Add Unicode support to Golua")
      (description "This extension to Arzilli's Golua adds Unicode support to
all functions from the Lua string library.  Lua patterns are replaced by Go
regexps.  This breaks compatibility with Lua, but Unicode support breaks it
anyways and Go regexps are more powerful.")
      (license license:expat))))

(define-public go-github-com-yookoala-realpath
  (let ((commit "d19ef9c409d9817c1e685775e53d361b03eabbc8")
        (revision "0"))
    (package
      (name "go-github-com-yookoala-realpath")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/yookoala/realpath")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0qvz1dcdldf53rq69fli76z5k1vr7prx9ds1d5rpzgs68kwn40nw"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/yookoala/realpath"))
      (home-page "https://github.com/yookoala/realpath")
      (synopsis "@code{realpath} for Golang")
      (description "This package provides @code{realpath}, a Go module that
when provided with a valid relative path / alias path, it will return you with
a string of its real absolute path in the system.")
      (license license:expat))))

(define-public go-gitlab-com-ambrevar-damerau
  (let ((commit "883829e1f25fad54015772ea663e69017cf22352")
        (revision "0"))
    (package
      (name "go-gitlab-com-ambrevar-damerau")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://gitlab.com/ambrevar/damerau")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1b9p8fypc914ij1afn6ir346zsgfqrc5mqc1k3d53n4snypq27qv"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "gitlab.com/ambrevar/damerau"))
      (home-page "https://gitlab.com/ambrevar/damerau")
      (synopsis "Damerau-Levenshtein distance for Golang")
      (description "This is a spelling corrector implementing the
Damerau-Levenshtein distance.  Takes a string value input from the user.
Looks for an identical word on a list of words, if none is found, look for a
similar word.")
      (license license:expat))))

(define-public go-github-com-stevedonovan-luar
  (let ((commit "22d247e5366095f491cd83edf779ee99a78f5ead")
        (revision "0"))
    (package
      (name "go-github-com-stevedonovan-luar")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/stevedonovan/luar")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1acjgw9cz1l0l9mzkyk7irz6cfk31wnxgbwa805fvm1rqcjzin2c"))))
      (build-system go-build-system)
      (native-inputs
       `(("go-github-com-aarzilli-golua" ,go-github-com-aarzilli-golua)))
      (arguments
       `(#:tests? #f                    ; Upstream tests are broken.
         #:import-path "github.com/stevedonovan/luar"))
      (home-page "https://github.com/stevedonovan/luar")
      (synopsis "Lua reflection bindings for Go")
      (description "Luar is designed to make using Lua from Go more
convenient.  Go structs, slices and maps can be automatically converted to Lua
tables and vice-versa.  The resulting conversion can either be a copy or a
proxy.  In the latter case, any change made to the result will reflect on the
source.

Any Go function can be made available to Lua scripts, without having to write
C-style wrappers.

Luar support cyclic structures (lists, etc.).

User-defined types can be made available to Lua as well: their exported
methods can be called and usual operations such as indexing or arithmetic can
be performed.")
      (license license:expat))))

(define-public go-github-com-kr-text
  (let ((commit "e2ffdb16a802fe2bb95e2e35ff34f0e53aeef34f")
        (revision "0"))
    (package
      (name "go-github-com-kr-text")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/kr/text")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1gm5bsl01apvc84bw06hasawyqm4q84vx1pm32wr9jnd7a8vjgj1"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/kr/text"))
      (home-page "https://github.com/kr/text")
      (synopsis "Go package for manipulating paragraphs of text")
      (description "Package @code{text} provides manipulation Go functions for
paragraphs of text.")
      (license license:expat))))

(define-public go-github-com-michiwend-golang-pretty
  (let ((commit "8ac61812ea3fa540f3f141a444fcb0dd713cdca4")
        (revision "0"))
    (package
      (name "go-github-com-michiwend-golang-pretty")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/michiwend/golang-pretty")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0rjfms0csjqi91xnddzx3rcrcaikc7xc027617px3kdwdap80ir4"))))
      (build-system go-build-system)
      (native-inputs
       `(("go-github-com-kr-text" ,go-github-com-kr-text)))
      (arguments
       `(#:tests? #f                    ; Upstream tests seem to be broken.
         #:import-path "github.com/michiwend/golang-pretty"))
      (home-page "https://github.com/michiwend/golang-pretty")
      (synopsis "Pretty printing for Go values")
      (description "Package @code{pretty} provides pretty-printing for Go
values.  This is useful during debugging, to avoid wrapping long output lines
in the terminal.

It provides a function, @code{Formatter}, that can be used with any function
that accepts a format string.  It also provides convenience wrappers for
functions in packages @code{fmt} and @code{log}.")
      (license license:expat))))

(define-public go-github-com-michiwend-gomusicbrainz
  (let ((commit "0cdeb13f9b24d2c714feb7e3c63d595cf7121d7d")
        (revision "0"))
    (package
      (name "go-github-com-michiwend-gomusicbrainz")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/michiwend/gomusicbrainz")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1li9daw0kghb80rdmxbh7g72qhxcvx3rvhwq5gs0jrr9hb8pjvcn"))))
      (build-system go-build-system)
      (native-inputs
       `(("go-github-com-michiwend-golang-pretty" ,go-github-com-michiwend-golang-pretty)
         ("go-github-com-kr-text" ,go-github-com-kr-text)))
      (arguments
       `(#:import-path "github.com/michiwend/gomusicbrainz"))
      (home-page "https://github.com/michiwend/gomusicbrainz")
      (synopsis "MusicBrainz WS2 client library for Golang")
      (description "Currently GoMusicBrainz provides methods to perform search
and lookup requests.  Browse requests are not supported yet.")
      (license license:expat))))

(define-public go-github-com-wtolson-go-taglib
  (let ((commit "6e68349ff94ecea412de7e748cb5eaa26f472777")
        (revision "0"))
    (package
      (name "go-github-com-wtolson-go-taglib")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/wtolson/go-taglib")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1cpjqnrviwflz150g78iir5ndrp3hh7a93zbp4dwbg6sb2q141p2"))))
      (build-system go-build-system)
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("taglib" ,taglib)))
      (arguments
       `(#:import-path "github.com/wtolson/go-taglib"))
      (home-page "https://github.com/wtolson/go-taglib")
      (synopsis "Go wrapper for taglib")
      (description "Go wrapper for taglib")
      (license license:unlicense))))
