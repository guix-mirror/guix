;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix build-system trivial)
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
       ,@(if (string-prefix? "aarch64-linux" (or (%current-system)
                                                 (%current-target-system)))
             '(#:system "armhf-linux")
             '())
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
    (supported-systems '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux"))
    (license license:bsd-3)))

(define-public go-1.11
  (package
    (inherit go-1.4)
    (name "go")
    (version "1.11.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://storage.googleapis.com/golang/"
                           name version ".src.tar.gz"))
       (sha256
        (base32
         "0gllmbjvp12iszwils8id78mvjxwviwf98lh2gdkb236n4mz07mw"))))
    (arguments
     (substitute-keyword-arguments (package-arguments go-1.4)
       ((#:phases phases)
        `(modify-phases ,phases
           ;; XXX Work around the Go 1.11.5 tarbomb.
           ;; <https://github.com/golang/go/issues/29906>
           (add-after 'unpack 'tarbomb-workaround
             (lambda _
               (chdir "..")
               (delete-file-recursively "gocache")
               (delete-file-recursively "tmp")
               #t))
           (replace 'chdir
             (lambda _
               (chdir "go/src")
               #t))
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
           (add-before 'build 'set-bootstrap-variables
             (lambda* (#:key outputs inputs #:allow-other-keys)
               ;; Tell the build system where to find the bootstrap Go.
               (let ((go  (assoc-ref inputs "go")))
                 (setenv "GOROOT_BOOTSTRAP" go)
                 (setenv "GOGC" "400")
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
                 ;; Prevent installation of the build cache, which contains
                 ;; store references to most of the tools used to build Go and
                 ;; would unnecessarily increase the size of Go's closure if it
                 ;; was installed.
                 (delete-file-recursively "../pkg/obj")

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
       ,@(match (%current-system)
           ((or "armhf-linux" "aarch64-linux")
            `(("gold" ,binutils-gold)))
           (_ `()))
       ,@(package-native-inputs go-1.4)))
    (supported-systems %supported-systems)))

(define-public go go-1.11)

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
       '(#:import-path "gopkg.in/tomb.v2"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-source
             (lambda _
               ;; Add a missing % to fix the compilation of this test
               (substitute* "src/gopkg.in/tomb.v2/tomb_test.go"
                 (("t.Fatalf\\(`Killf\\(\"BO%s")
                  "t.Fatalf(`Killf(\"BO%%s"))
               #t)))))
      (synopsis "@code{tomb} handles clean goroutine tracking and termination")
      (description
       "The @code{tomb} package handles clean goroutine tracking and
termination.")
      (home-page "https://gopkg.in/tomb.v2")
      (license license:bsd-3))))

(define-public go-github.com-jtolds-gls
  (package
    (name "go-github.com-jtolds-gls")
    (version "4.20")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jtolds/gls")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k7xd2q2ysv2xsh373qs801v6f359240kx0vrl0ydh7731lngvk6"))))
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
  (let ((commit "0fcca4842a8d74bfddc2c96a073bd2a4d2a7a2e8")
        (revision "2"))
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
                  "033ghifvrxmqr54nm8gmgxz7qxlqgw9z7z976kp88yf1rmxm2kjr"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/bcrypt"
         #:unpack-path "golang.org/x/crypto"))
      (synopsis "Bcrypt in Go")
      (description "This package provides a Go implementation of the bcrypt
password hashing function.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-blowfish
  (let ((commit "0fcca4842a8d74bfddc2c96a073bd2a4d2a7a2e8")
        (revision "2"))
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
                  "033ghifvrxmqr54nm8gmgxz7qxlqgw9z7z976kp88yf1rmxm2kjr"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/blowfish"
         #:unpack-path "golang.org/x/crypto"))
      (synopsis "Blowfish in Go")
      (description "This package provides a Go implementation of the Blowfish
symmetric-key block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-pbkdf2
  (let ((commit "0fcca4842a8d74bfddc2c96a073bd2a4d2a7a2e8")
        (revision "2"))
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
                  "033ghifvrxmqr54nm8gmgxz7qxlqgw9z7z976kp88yf1rmxm2kjr"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/pbkdf2"
         #:unpack-path "golang.org/x/crypto"))
      (synopsis "PBKDF2 in Go")
      (description "This package provides a Go implementation of the PBKDF2 key
derivation function.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-tea
  (let ((commit "0fcca4842a8d74bfddc2c96a073bd2a4d2a7a2e8")
        (revision "2"))
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
                  "033ghifvrxmqr54nm8gmgxz7qxlqgw9z7z976kp88yf1rmxm2kjr"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/tea"
         #:unpack-path "golang.org/x/crypto"))
      (synopsis "Tiny Encryption Algorithm (TEA) in Go")
      (description "This packages a Go implementation of the Tiny Encryption
Algorithm (TEA) block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-salsa20
  (let ((commit "0fcca4842a8d74bfddc2c96a073bd2a4d2a7a2e8")
        (revision "2"))
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
                  "033ghifvrxmqr54nm8gmgxz7qxlqgw9z7z976kp88yf1rmxm2kjr"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/salsa20"
         #:unpack-path "golang.org/x/crypto"))
      (synopsis "Salsa20 in Go")
      (description "This packages provides a Go implementation of the Salsa20
stream cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-cast5
  (let ((commit "0fcca4842a8d74bfddc2c96a073bd2a4d2a7a2e8")
        (revision "2"))
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
                  "033ghifvrxmqr54nm8gmgxz7qxlqgw9z7z976kp88yf1rmxm2kjr"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/cast5"
         #:unpack-path "golang.org/x/crypto"))
      (synopsis "Cast5 in Go")
      (description "This packages provides a Go implementation of the Cast5
symmetric-key block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-twofish
  (let ((commit "0fcca4842a8d74bfddc2c96a073bd2a4d2a7a2e8")
        (revision "2"))
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
                  "033ghifvrxmqr54nm8gmgxz7qxlqgw9z7z976kp88yf1rmxm2kjr"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/twofish"
         #:unpack-path "golang.org/x/crypto"))
      (synopsis "Twofish in Go")
      (description "This packages provides a Go implementation of the Twofish
symmetric-key block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-xtea
  (let ((commit "0fcca4842a8d74bfddc2c96a073bd2a4d2a7a2e8")
        (revision "2"))
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
                  "033ghifvrxmqr54nm8gmgxz7qxlqgw9z7z976kp88yf1rmxm2kjr"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/xtea"
         #:unpack-path "golang.org/x/crypto"))
      (synopsis "eXtended Tiny Encryption Algorithm (XTEA) in Go")
      (description "This package provides a Go implementation of the eXtended
Tiny Encryption Algorithm (XTEA) block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto-ed25519
  (package
    (inherit go-golang-org-x-crypto-bcrypt)
    (name "go-golang-org-x-crypto-ed25519")
    (arguments
     `(#:import-path "golang.org/x/crypto/ed25519"
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
    (synopsis "ED25519 in Go")
    (description "This package provides a Go implementation of the ED25519
signature algorithm.")))

(define-public go-golang-org-x-crypto-ripemd160
  (package
    (inherit go-golang-org-x-crypto-bcrypt)
    (name "go-golang-org-x-crypto-ripemd160")
    (arguments
     (substitute-keyword-arguments (package-arguments go-golang-org-x-crypto-bcrypt)
       ((#:import-path _)
        "golang.org/x/crypto/ripemd160")))
    (synopsis "RIPEMD-160 in Go")
    (description "This package provides a Go implementation of the RIPEMD-160
hash algorithm.")))

(define-public go-golang-org-x-crypto-blake2s
  (package
    (inherit go-golang-org-x-crypto-bcrypt)
    (name "go-golang-org-x-crypto-blake2s")
    (arguments
     (substitute-keyword-arguments (package-arguments go-golang-org-x-crypto-bcrypt)
       ((#:import-path _)
        "golang.org/x/crypto/blake2s")))
    (synopsis "BLAKE2s in Go")
    (description "This package provides a Go implementation of the BLAKE2s
hash algorithm.")))

(define-public go-golang-org-x-crypto-sha3
  (package
    (inherit go-golang-org-x-crypto-bcrypt)
    (name "go-golang-org-x-crypto-sha3")
    (arguments
     (substitute-keyword-arguments (package-arguments go-golang-org-x-crypto-bcrypt)
       ((#:import-path _)
        "golang.org/x/crypto/sha3")))
    (synopsis "SHA-3 in Go")
    (description "This package provides a Go implementation of the SHA-3
fixed-output-length hash functions and the SHAKE variable-output-length hash
functions defined by FIPS-202.")))

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
         #:unpack-path "golang.org/x/crypto"))
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
      ;; From go-1.10 onward, "pkg" compiled libraries are not re-used, so
      ;; when this package required as input for another one, it will have to
      ;; be built again.  Thus its CGO requirements must be made available in
      ;; the environment, that is, they must be propagated.
      (propagated-inputs
       `(("lua" ,lua)))
      (arguments
       `(#:unpack-path "github.com/aarzilli/golua"
         #:import-path "github.com/aarzilli/golua/lua"
         #:phases
         (modify-phases %standard-phases
           ;; While it's possible to fix the CGO_LDFLAGS with the "-tags"
           ;; command line argument, go-1.10+ does not re-use the produced pkg
           ;; for dependencies, which means we would need to propagate the
           ;; same "-tags" argument to all golua referrers.  A substitution is
           ;; more convenient here.  We also need to propagate the lua
           ;; dependency to make it available to referrers.
           (add-after 'unpack 'fix-lua-ldflags
             (lambda _
               (substitute* "src/github.com/aarzilli/golua/lua/lua.go"
                 (("#cgo linux,!llua,!luaa LDFLAGS: -llua5.3")
                  "#cgo linux,!llua,!luaa LDFLAGS: -llua")))))))
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
      ;; From go-1.10 onward, "pkg" compiled libraries are not re-used, so
      ;; when this package required as input for another one, it will have to
      ;; be built again.  Thus its CGO requirements must be made available in
      ;; the environment, that is, they must be propagated.
      (propagated-inputs
       `(("pkg-config" ,pkg-config)
         ("taglib" ,taglib)))
      (arguments
       `(#:import-path "github.com/wtolson/go-taglib"
         ;; Tests don't pass "vet" on go-1.11.  See
         ;; https://github.com/wtolson/go-taglib/issues/12.
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key import-path #:allow-other-keys)
               (invoke "go" "test"
                       "-vet=off"
                       import-path))))))
      (home-page "https://github.com/wtolson/go-taglib")
      (synopsis "Go wrapper for taglib")
      (description "Go wrapper for taglib")
      (license license:unlicense))))

(define-public go-github-com-gogo-protobuf
  (let ((commit "160de10b2537169b5ae3e7e221d28269ef40d311")
        (revision "2"))
    (package
      (name "go-github-com-gogo-protobuf")
      (version (git-version "0.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gogo/protobuf")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0hxq28sgxym04rv0q40gpwkh4ni359q21hq3g78wwxwx4qfd4zwm"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/gogo/protobuf/proto"
         #:unpack-path "github.com/gogo/protobuf"))
      (propagated-inputs
       `(("go-github-com-gogo-protobuf-protoc-gen-gogo"
          ,go-github-com-gogo-protobuf-protoc-gen-gogo)))
      (synopsis "Protocol Buffers for Go with Gadgets")
      (description "Gogoprotobuf is a fork of golang/protobuf with extra code
generation features.  This code generation is used to achieve:
@itemize
@item fast marshalling and unmarshalling
@item more canonical Go structures
@item goprotobuf compatibility
@item less typing by optionally generating extra helper code
@item peace of mind by optionally generating test and benchmark code
@item other serialization formats
@end itemize")
      (home-page "https://github.com/gogo/protobuf")
      (license license:bsd-3))))

(define-public go-github-com-gogo-protobuf-protoc-gen-gogo
  (let ((commit "efccd33a0c20aa078705571d5ddbfa14c8395a63")
        (revision "0"))
    (package
      (name "go-github-com-gogo-protobuf-protoc-gen-gogo")
      (version (git-version "0.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gogo/protobuf")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09kfa3aqmhh7p0rc6wd4fw5cjccidsk9vgcy13albv0g8vnbmmgw"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/gogo/protobuf/protoc-gen-gogo"
         #:unpack-path "github.com/gogo/protobuf"))
      (synopsis "Protocol Buffers for Go with Gadgets")
      (description "Gogoprotobuf is a fork of golang/protobuf with extra code
generation features.  This code generation is used to achieve:
@itemize
@item fast marshalling and unmarshalling
@item more canonical Go structures
@item goprotobuf compatibility
@item less typing by optionally generating extra helper code
@item peace of mind by optionally generating test and benchmark code
@item other serialization formats
@end itemize")
      (home-page "https://github.com/gogo/protobuf")
      (license license:bsd-3))))

(define-public go-github-com-gogo-protobuf-proto
  (let ((commit
          "fd322a3c49630fe6d05737e2b7d9426e6680e28d")
        (revision "0"))
    (package
      (name "go-github-com-gogo-protobuf-proto")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/gogo/protobuf.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "1zi85584dy91hyrwpanygz1pppi0chn3hzzv128i83i6j45a5fp9"))))
      (build-system go-build-system)
      (arguments
       '(#:unpack-path "github.com/gogo/protobuf"
         #:import-path "github.com/gogo/protobuf/proto"))
      (native-inputs `())
      (home-page "https://github.com/gogo/protobuf")
      (synopsis "XXX")
      (description "XXX")
      (license license:expat))))

(define-public go-github-com-libp2p-go-flow-metrics
  (let ((commit "7e5a55af485341567f98d6847a373eb5ddcdcd43")
        (revision "0"))
    (package
      (name "go-github-com-libp2p-go-flow-metrics")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libp2p/go-flow-metrics.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1p87iyk6q6f3g3xkncssx400qlld8f2z93qiz8m1f97grfyhjif1"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/libp2p/go-flow-metrics"
         ;; TODO: Tests hang.
         #:tests? #f))
      (home-page
       "https://github.com/libp2p/go-flow-metrics")
      (synopsis "Simple library for tracking flow metrics")
      (description "A simple alternative to rcrowley's @command{go-metrics}
that's a lot faster (and only does simple bandwidth metrics).")
      (license license:expat))))

(define-public go-github-com-davecgh-go-spew
  (let ((commit "d8f796af33cc11cb798c1aaeb27a4ebc5099927d")
        (revision "0"))
    (package
      (name "go-github-com-davecgh-go-spew")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/davecgh/go-spew.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "19z27f306fpsrjdvkzd61w1bdazcdbczjyjck177g33iklinhpvx"))))
      (build-system go-build-system)
      (arguments
       '(#:unpack-path "github.com/davecgh/go-spew"
         #:import-path "github.com/davecgh/go-spew/spew"))
      (home-page "https://github.com/davecgh/go-spew")
      (synopsis "Deep pretty printer for Go data structures to aid in debugging")
      (description "Package @command{spew} implements a deep pretty printer
for Go data structures to aid in debugging.

A quick overview of the additional features spew provides over the built-in printing facilities for Go data types are as follows:

@itemize
@item Pointers are dereferenced and followed.
@item Circular data structures are detected and handled properly.
@item Custom Stringer/error interfaces are optionally invoked, including on
unexported types.
@item Custom types which only implement the Stringer/error interfaces via a
pointer receiver are optionally invoked when passing non-pointer variables.
@item Byte arrays and slices are dumped like the hexdump -C command which
includes offsets, byte values in hex, and ASCII output (only when using Dump
style).
@end itemize\n")
      (license license:isc))))

(define-public go-github-com-btcsuite-btclog
  (let ((commit "84c8d2346e9fc8c7b947e243b9c24e6df9fd206a")
        (revision "0"))
    (package
      (name "go-github-com-btcsuite-btclog")
      (version (git-version "0.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/btcsuite/btclog.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "02dl46wcnfpg9sqvg0ipipkpnd7lrf4fnvb9zy56jqa7mfcwc7wk"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/btcsuite/btclog"))
      (home-page "https://github.com/btcsuite/btclog")
      (synopsis "Subsystem aware logger for Go")
      (description "Package @command{btclog} defines a logger interface and
provides a default implementation of a subsystem-aware leveled logger
implementing the same interface.")
      (license license:isc))))

(define-public go-github-com-btcsuite-btcd-btcec
  (let ((commit "67e573d211ace594f1366b4ce9d39726c4b19bd0")
        (revision "0"))
    (package
      (name "go-github-com-btcsuite-btcd-btcec")
      (version (git-version "0.12.0-beta" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/btcsuite/btcd.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "04s92gsy71w1jirlr5lkk9y6r5cparbas7nmf6ywbp7kq7fn8ajn"))))
      (build-system go-build-system)
      (arguments
       '(#:unpack-path "github.com/btcsuite/btcd"
         #:import-path "github.com/btcsuite/btcd/btcec"))
      (native-inputs
       `(("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)))
      (home-page "https://github.com/btcsuite/btcd")
      (synopsis "Elliptic curve cryptography to work with Bitcoin")
      (description "Package @command{btcec} implements elliptic curve
cryptography needed for working with Bitcoin (secp256k1 only for now).  It is
designed so that it may be used with the standard crypto/ecdsa packages
provided with Go.  A comprehensive suite of test is provided to ensure proper
functionality.  Package @command{btcec} was originally based on work from
ThePiachu which is licensed under the same terms as Go, but it has
signficantly diverged since then.  The @command{btcsuite} developers original
is licensed under the liberal ISC license.

Although this package was primarily written for btcd, it has intentionally
been designed so it can be used as a standalone package for any projects
needing to use secp256k1 elliptic curve cryptography.")
      (license license:isc))))

(define-public go-github-com-minio-sha256-simd
  (let ((commit "cc1980cb03383b1d46f518232672584432d7532d")
        (revision "3"))
    (package
      (name "go-github-com-minio-sha256-simd")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/minio/sha256-simd.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "04fp98nal0wsb26zwhw82spn5camxslc68g3xp8g4af9w6k9g31j"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/minio/sha256-simd"))
      (home-page "https://github.com/minio/sha256-simd")
      (synopsis "Accelerate SHA256 computations in pure Go")
      (description "Accelerate SHA256 computations in pure Go using AVX512 and
AVX2 for Intel and ARM64 for ARM.  On AVX512 it provides an up to 8x
improvement (over 3 GB/s per core) in comparison to AVX2.

This package is designed as a replacement for @command{crypto/sha256}.  For
Intel CPUs it has two flavors for AVX512 and AVX2 (AVX/SSE are also
supported).  For ARM CPUs with the Cryptography Extensions, advantage is taken
of the SHA2 instructions resulting in a massive performance improvement.

This package uses Golang assembly.  The AVX512 version is based on the Intel's
\"multi-buffer crypto library for IPSec\" whereas the other Intel
implementations are described in \"Fast SHA-256 Implementations on Intel
Architecture Processors\" by J. Guilford et al.")
      (license license:asl2.0))))

(define-public go-github-com-libp2p-go-libp2p-crypto
  (let ((commit "7240b40a3ddc47c4d17c15baabcbe45e5219171b")
        (revision "0"))
    (package
      (name "go-github-com-libp2p-go-libp2p-crypto")
      (version (git-version "2.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libp2p/go-libp2p-crypto.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0qwpy57qv5143l9dlfwfvpqsxdd2i4zwnawx1w4pmgxxim3nw1wb"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/libp2p/go-libp2p-crypto"))
      (native-inputs
       `(("go-golang-org-x-crypto-ed25519" ,go-golang-org-x-crypto-ed25519)
         ("go-github-com-btcsuite-btcd-btcec" ,go-github-com-btcsuite-btcd-btcec)
         ("go-github-com-gogo-protobuf-proto" ,go-github-com-gogo-protobuf-proto)
         ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)))
      (home-page
       "https://github.com/libp2p/go-libp2p-crypto")
      (synopsis "Various cryptographic utilities used by IPFS")
      (description "Various cryptographic utilities used by IPFS")
      (license license:expat))))

(define-public go-github-com-mr-tron-base58
  (let ((commit "d724c80ecac7b49e4e562d58b2b4f4ee4ed8c312")
        (revision "0"))
    (package
      (name "go-github-com-mr-tron-base58")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mr-tron/base58.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "12qhgnn9wf3c1ang16r4i778whk4wsrj7d90h2xgmz4fi1469rqa"))))
      (build-system go-build-system)
      (arguments
       `(#:unpack-path "github.com/mr-tron/base58"
         #:import-path "github.com/mr-tron/base58/base58"))
      (home-page "https://github.com/mr-tron/base58")
      (synopsis "Fast implementation of base58 encoding on Golang")
      (description "Fast implementation of base58 encoding on Golang.  A
trivial @command{big.Int} encoding benchmark results in 6 times faster
encoding and 8 times faster decoding.")
      (license license:expat))))

(define-public go-github-com-gxed-hashland-keccakpg
  (let ((commit "d9f6b97f8db22dd1e090fd0bbbe98f09cc7dd0a8")
        (revision "0"))
    (package
      (name "go-github-com-gxed-hashland-keccakpg")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gxed/hashland.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1q23y4lacsz46k9gmgfw4iwwydw36j2601rbidmmswl94grpc386"))))
      (build-system go-build-system)
      (arguments
       '(#:unpack-path "github.com/gxed/hashland"
         #:import-path "github.com/gxed/hashland/keccakpg"))
      (home-page "https://github.com/gxed/hashland")
      (synopsis "Implements the Keccak (SHA-3) hash algorithm in Go")
      (description "Package @command{keccak} implements the Keccak (SHA-3)
hash algorithm.  See http://keccak.noekeon.org.")
      (license license:expat))))

(define-public go-github-com-minio-blake2b-simd
  (let ((commit "3f5f724cb5b182a5c278d6d3d55b40e7f8c2efb4")
        (revision "0"))
    (package
      (name "go-github-com-minio-blake2b-simd")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/minio/blake2b-simd.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0b6jbnj62c0gmmfd4zdmh8xbg01p80f13yygir9xprqkzk6fikmd"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/minio/blake2b-simd"))
      (home-page "https://github.com/minio/blake2b-simd")
      (synopsis "Fast hashing in pure Go of BLAKE2b with SIMD instructions")
      (description "This package was initially based on the pure go BLAKE2b
implementation of Dmitry Chestnykh and merged with the (cgo dependent) AVX
optimized BLAKE2 implementation (which in turn is based on the official
implementation.  It does so by using Go's Assembler for amd64 architectures
with a golang only fallback for other architectures.

In addition to AVX there is also support for AVX2 as well as SSE.  Best
performance is obtained with AVX2 which gives roughly a 4X performance
increase approaching hashing speeds of 1GB/sec on a single core.")
      (license license:asl2.0))))

(define-public go-github-com-spaolacci-murmur3
  (let ((commit "f09979ecbc725b9e6d41a297405f65e7e8804acc")
        (revision "0"))
    (package
      (name "go-github-com-spaolacci-murmur3")
      (version (git-version "1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/spaolacci/murmur3.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1lv3zyz3jy2d76bhvvs8svygx66606iygdvwy5cwc0p5z8yghq25"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/spaolacci/murmur3"))
      (home-page "https://github.com/spaolacci/murmur3")
      (synopsis "Native MurmurHash3 Go implementation")
      (description "Native Go implementation of Austin Appleby's third
MurmurHash revision (aka MurmurHash3).

Reference algorithm has been slightly hacked as to support the streaming mode
required by Go's standard Hash interface.")
      (license license:bsd-3))))

(define-public go-github-com-multiformats-go-multihash
  (let ((commit "97cdb562a04c6ef66d8ed40cd62f8fbcddd396d6")
        (revision "0"))
    (package
      (name "go-github-com-multiformats-go-multihash")
      (version (git-version "1.0.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/multiformats/go-multihash.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "02wd9akrwy4y5m0nig9m24p14bjjgb4n1djydrq8cm4yhbvjrrk0"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/multiformats/go-multihash"))
      (native-inputs
       `(("go-github-com-mr-tron-base58" ,go-github-com-mr-tron-base58)
         ("go-github-com-gxed-hashland-keccakpg" ,go-github-com-gxed-hashland-keccakpg)
         ("go-github-com-minio-blake2b-simd" ,go-github-com-minio-blake2b-simd)
         ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)
         ("go-github-com-spaolacci-murmur3" ,go-github-com-spaolacci-murmur3)
         ("go-golang-org-x-crypto-blake2s" ,go-golang-org-x-crypto-blake2s)
         ("go-golang-org-x-crypto-sha3" ,go-golang-org-x-crypto-sha3)))
      (home-page "https://github.com/multiformats/go-multihash")
      (synopsis "Multihash implementation in Go")
      (description "Multihash implementation in Go.")
      (license license:expat))))

(define-public go-github-com-libp2p-go-libp2p-peer
  (let ((commit "993d742bc29dcf4894b7730ba610fd78900be76c")
        (revision "0"))
    (package
      (name "go-github-com-libp2p-go-libp2p-peer")
      (version (git-version "2.3.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libp2p/go-libp2p-peer.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1h96qjdi0i1wbr0jliap2903mycphas3ny0zdrm77yca9plcnphh"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/libp2p/go-libp2p-peer"))
      (native-inputs
       `(("go-github-com-libp2p-go-libp2p-crypto" ,go-github-com-libp2p-go-libp2p-crypto)
         ("go-github-com-gogo-protobuf-proto" ,go-github-com-gogo-protobuf-proto)
         ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)
         ("go-github-com-minio-blake2b-simd" ,go-github-com-minio-blake2b-simd)
         ("go-github-com-btcsuite-btcd-btcec" ,go-github-com-btcsuite-btcd-btcec)
         ("go-github-com-mr-tron-base58" ,go-github-com-mr-tron-base58)
         ("go-github-com-multiformats-go-multihash" ,go-github-com-multiformats-go-multihash)
         ("go-github-com-gxed-hashland-keccakpg" ,go-github-com-gxed-hashland-keccakpg)
         ("go-github-com-spaolacci-murmur3" ,go-github-com-spaolacci-murmur3)
         ("go-golang-org-x-crypto-blake2s" ,go-golang-org-x-crypto-blake2s)
         ("go-golang-org-x-crypto-ed25519" ,go-golang-org-x-crypto-ed25519)
         ("go-golang-org-x-crypto-sha3" ,go-golang-org-x-crypto-sha3)))
      (home-page "https://github.com/libp2p/go-libp2p-peer")
      (synopsis "PKI based identities for use in go-libp2p")
      (description "PKI based identities for use in @command{go-libp2p}.")
      (license license:expat))))

(define-public go-github-com-libp2p-go-libp2p-protocol
  (let ((commit "b29f3d97e3a2fb8b29c5d04290e6cb5c5018004b")
        (revision "0"))
    (package
      (name "go-github-com-libp2p-go-libp2p-protocol")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libp2p/go-libp2p-protocol.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1xgjfnx9zcqglg9li29wdqywsp8hz22wx6phns9zscni2jsfidld"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path
         "github.com/libp2p/go-libp2p-protocol"))
      (home-page "https://github.com/libp2p/go-libp2p-protocol")
      (synopsis "Type for protocol strings in Golang")
      (description "Just a type for protocol strings.  Nothing more.")
      (license license:expat))))

(define-public go-github-com-libp2p-go-libp2p-metrics
  (let ((commit "a10ff6e75dae3c868023867e8caa534a04bdc624")
        (revision "0"))
    (package
      (name "go-github-com-libp2p-go-libp2p-metrics")
      (version (git-version "2.1.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libp2p/go-libp2p-metrics.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "05wy0cq4h6yg9bzgapcvm2criwriicbswx80ma82gyn4a9fdrk8m"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/libp2p/go-libp2p-metrics"))
      (native-inputs
       `(("go-github-com-libp2p-go-flow-metrics" ,go-github-com-libp2p-go-flow-metrics)
         ("go-github-com-libp2p-go-libp2p-peer" ,go-github-com-libp2p-go-libp2p-peer)
         ("go-github-com-libp2p-go-libp2p-protocol" ,go-github-com-libp2p-go-libp2p-protocol)
         ("go-github-com-libp2p-go-libp2p-crypto" ,go-github-com-libp2p-go-libp2p-crypto)
         ("go-github-com-mr-tron-base58" ,go-github-com-mr-tron-base58)
         ("go-github-com-multiformats-go-multihash" ,go-github-com-multiformats-go-multihash)
         ("go-github-com-btcsuite-btcd-btcec" ,go-github-com-btcsuite-btcd-btcec)
         ("go-github-com-gogo-protobuf-proto" ,go-github-com-gogo-protobuf-proto)
         ("go-github-com-gxed-hashland-keccakpg" ,go-github-com-gxed-hashland-keccakpg)
         ("go-github-com-minio-blake2b-simd" ,go-github-com-minio-blake2b-simd)
         ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)
         ("go-github-com-spaolacci-murmur3" ,go-github-com-spaolacci-murmur3)
         ("go-golang-org-x-crypto-sha3" ,go-golang-org-x-crypto-sha3)
         ("go-golang-org-x-crypto-ed25519" ,go-golang-org-x-crypto-ed25519)
         ("go-golang-org-x-crypto-blake2s" ,go-golang-org-x-crypto-blake2s)))
      (home-page "https://github.com/libp2p/go-libp2p-metrics")
      (synopsis "Connection wrapper for go-libp2p that provides bandwidth metrics")
      (description "A connection wrapper for @command{go-libp2p} that provides bandwidth
statistics for wrapped connections.")
      (license license:expat))))

(define-public go-github-com-mitchellh-go-homedir
  (let ((commit "ae18d6b8b3205b561c79e8e5f69bff09736185f4")
        (revision "0"))
    (package
      (name "go-github-com-mitchellh-go-homedir")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mitchellh/go-homedir.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0f0z0aa4wivk4z1y503dmnw0k0g0g403dly8i4q263gfshs82sbq"))))
      (build-system go-build-system)
      (arguments
       (quote (#:import-path "github.com/mitchellh/go-homedir"
               ;; TODO: Tests fail because it tries to access home.
               #:tests? #f)))
      (home-page "https://github.com/mitchellh/go-homedir")
      (synopsis "Go library for detecting and expanding the user's home directory without cgo")
      (description "This is a Go library for detecting the user's home
directory without the use of @command{cgo}, so the library can be used in
cross-compilation environments.

Usage is simple, just call homedir.Dir() to get the home directory for a user,
and homedir.Expand() to expand the @command{~} in a path to the home
directory.

Why not just use @command{os/user}?  The built-in @command{os/user} package
requires cgo on Darwin systems.  This means that any Go code that uses that
package cannot cross compile.  But 99% of the time the use for
@command{os/user} is just to retrieve the home directory, which we can do for
the current user without cgo.  This library does that, enabling
cross-compilation.")
      (license license:expat))))

(define-public go-github-com-multiformats-go-multiaddr
  (let ((commit "fe1c46f8be5af4aff4db286e08839295bd922efb")
        (revision "0"))
    (package
      (name "go-github-com-multiformats-go-multiaddr")
      (version (git-version "1.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/multiformats/go-multiaddr.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0p5f8h098a4yjjmzsgqs7vhx1iqifb8izwg3559cr4h7clkpzznh"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path
         "github.com/multiformats/go-multiaddr"))
      (native-inputs
       `(("go-github-com-multiformats-go-multihash" ,go-github-com-multiformats-go-multihash)
         ("go-github-com-gxed-hashland-keccakpg" ,go-github-com-gxed-hashland-keccakpg)
         ("go-github-com-minio-blake2b-simd" ,go-github-com-minio-blake2b-simd)
         ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)
         ("go-github-com-mr-tron-base58" ,go-github-com-mr-tron-base58)
         ("go-github-com-spaolacci-murmur3" ,go-github-com-spaolacci-murmur3)
         ("go-golang-org-x-crypto-sha3" ,go-golang-org-x-crypto-sha3)
         ("go-golang-org-x-crypto-blake2s" ,go-golang-org-x-crypto-blake2s)))
      (home-page "https://github.com/multiformats/go-multiaddr")
      (synopsis "Composable and future-proof network addresses")
      (description "Multiaddr is a standard way to represent addresses that
does the following:

@itemize
@item Support any standard network protocols.
@item Self-describe (include protocols).
@item Have a binary packed format.
@item Have a nice string representation.
@item Encapsulate well.
@end itemize\n")
      (license license:expat))))

(define-public go-github-com-multiformats-go-multiaddr-net
  (let ((commit "1cb9a0e8a6de3c8a10f6cee60d01d793603c4f7e")
        (revision "0"))
    (package
      (name "go-github-com-multiformats-go-multiaddr-net")
      (version (git-version "1.6.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/multiformats/go-multiaddr-net.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1ypgi47xdz3bh8lh7f8cmk7w3ql9g4izx5l3kzdg9gda1xn5zxq3"))))
      (build-system go-build-system)
      (arguments
       (quote (#:import-path "github.com/multiformats/go-multiaddr-net"
               ;; TODO: Tests fail because they try to access the network.
               #:tests? #f)))
      (native-inputs
       `(("go-github-com-multiformats-go-multiaddr" ,go-github-com-multiformats-go-multiaddr)
         ("go-github-com-multiformats-go-multihash" ,go-github-com-multiformats-go-multihash)
         ("go-github-com-gxed-hashland-keccakpg" ,go-github-com-gxed-hashland-keccakpg)
         ("go-github-com-minio-blake2b-simd" ,go-github-com-minio-blake2b-simd)
         ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)
         ("go-github-com-mr-tron-base58" ,go-github-com-mr-tron-base58)
         ("go-github-com-spaolacci-murmur3" ,go-github-com-spaolacci-murmur3)
         ("go-golang-org-x-crypto-sha3" ,go-golang-org-x-crypto-sha3)
         ("go-golang-org-x-crypto-blake2s" ,go-golang-org-x-crypto-blake2s)))
      (home-page "https://github.com/multiformats/go-multiaddr-net")
      (synopsis "Multiaddress net tools")
      (description "This package provides Multiaddr specific versions of
common functions in stdlib's @command{net} package.  This means wrappers of
standard net symbols like @command{net.Dial} and @command{net.Listen}, as well
as conversion to and from @command{net.Addr}.")
      (license license:expat))))

(define-public go-github-com-whyrusleeping-tar-utils
  (let ((commit "8c6c8ba81d5c71fd69c0f48dbde4b2fb422b6dfc")
        (revision "0"))
    (package
      (name "go-github-com-whyrusleeping-tar-utils")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/whyrusleeping/tar-utils.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "14jjdw3yics0k467xsyk388684wdpi0bbx8nqj0y4pqxa0s0in6s"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path
         "github.com/whyrusleeping/tar-utils"))
      (home-page "https://github.com/whyrusleeping/tar-utils")
      (synopsis "Tar utilities extracted from go-ipfs codebase")
      (description "Tar utilities extracted from @command{go-ipfs} codebase.")
      (license license:expat))))

(define-public go-github-com-cheekybits-is
  (let ((commit "68e9c0620927fb5427fda3708222d0edee89eae9")
        (revision "0"))
    (package
      (name "go-github-com-cheekybits-is")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cheekybits/is.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1mkbyzhwq3rby832ikq00nxv3jnckxsm3949wkxd8ya9js2jmg4d"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/cheekybits/is"))
      (home-page "https://github.com/cheekybits/is")
      (synopsis "Mini testing helper for Go")
      (description "A mini testing helper for Go.

@itemize
@item It has a simple interface (@command{is.OK} and @command{is.Equal}).
@item It plugs into existing Go toolchain (uses @command{testing.T}).
@item It's obvious for newcomers.
@item It also gives you @command{is.Panic} and @command{is.PanicWith} helpers
- because testing panics is ugly.
@end itemize\n")
      (license license:expat))))

(define-public go-github-com-sabhiram-go-gitignore
  (let ((commit "d3107576ba9425fc1c85f4b3569c4631b805a02e")
        (revision "0"))
    (package
      (name "go-github-com-sabhiram-go-gitignore")
      (version (git-version "1.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sabhiram/go-gitignore.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rdwyxgcsiwgmlqnc3k6h300mzlvjc3j21np4yh1h476wc8dvl0l"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path
         "github.com/sabhiram/go-gitignore"))
      (native-inputs
       `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
      (home-page "https://github.com/sabhiram/go-gitignore")
      (synopsis "Gitignore parser for Go")
      (description "A @command{.gitignore} parser for Go.")
      (license license:expat))))

(define-public go-github-com-urfave-cli
  (let ((commit "934abfb2f102315b5794e15ebc7949e4ca253920")
        (revision "0"))
    (package
      (name "go-github-com-urfave-cli")
      (version (git-version "1.19.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/urfave/cli.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0c5r8pgj3k48dfcwj8lw3cxkwkl8vh0fhvz5snfdwd0bcxdqx1yq"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/urfave/cli"))
      (home-page "https://github.com/urfave/cli")
      (synopsis "Simple, fast, and fun package for building command line apps in Go")
      (description "@command{cli} is a simple, fast, and fun package for
building command line apps in Go.  The goal is to enable developers to write
fast and distributable command line applications in an expressive way.")
      (license license:expat))))

(define-public go-github-com-whyrusleeping-json-filter
  (let ((commit "ff25329a9528f01c5175414f16cc0a6a162a5b8b")
        (revision "0"))
    (package
      (name "go-github-com-whyrusleeping-json-filter")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/whyrusleeping/json-filter.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0cai0drvx4c8j686l908vpcsz3mw3vxi3ziz94b0f3c5ylpj07j7"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path
         "github.com/whyrusleeping/json-filter"))
      (home-page "https://github.com/whyrusleeping/json-filter")
      (synopsis "Library to query JSON objects marshalled into map[string]interface")
      (description "A library to query JSON objects marshalled into
@command{map[string]interface{}}.")
      (license license:expat))))

(define-public go-github-com-whyrusleeping-progmeter
  (let ((commit "f3e57218a75b913eff88d49a52c1debf9684ea04")
        (revision "0"))
    (package
      (name "go-github-com-whyrusleeping-progmeter")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/whyrusleeping/progmeter.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xs8rz6yhpvj9512c5v3b8dwr2kivywnyyfxzdfbr6fy1xc8zskb"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path
         "github.com/whyrusleeping/progmeter"))
      (home-page "https://github.com/whyrusleeping/progmeter")
      (synopsis "Progress meter for Go")
      (description "Progress meter for Go.")
      (license license:expat))))

(define-public go-github-com-whyrusleeping-stump
  (let ((commit "206f8f13aae1697a6fc1f4a55799faf955971fc5")
        (revision "0"))
    (package
      (name "go-github-com-whyrusleeping-stump")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/whyrusleeping/stump.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1s40qdppjnk8gijk7x6kbviiqz62nz3h6gic2q9cwcmq8r5isw7n"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/whyrusleeping/stump"))
      (home-page "https://github.com/whyrusleeping/stump")
      (synopsis "Very basic logging package for Go")
      (description "A simple log library, for when you don't really care to
have super fancy logs.")
      (license license:expat))))

(define-public go-github-com-kr-fs
  (let ((commit "1455def202f6e05b95cc7bfc7e8ae67ae5141eba")
        (revision "0"))
    (package
      (name "go-github-com-kr-fs")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kr/fs.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "11zg176x9hr9q7fsk95r6q0wf214gg4czy02slax4x56n79g6a7q"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/kr/fs"))
      (home-page "https://github.com/kr/fs")
      (synopsis "Filesystem-related functions for Go")
      (description "Package fs provides filesystem-related functions.")
      (license license:bsd-3))))

(define-public go-github-com-direnv-go-dotenv
  (let ((commit "4cce6d1a66f7bc8dc730eab85cab6af1b801abed")
        (revision "0"))
    (package
      (name "go-github-com-direnv-go-dotenv")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/direnv/go-dotenv")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00wn4fc2lma0csf6ryvlc6k9jbpbifm4n7i3kkd2xrfw5qlm29b6"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/direnv/go-dotenv"))
      (home-page "https://github.com/direnv/go-dotenv")
      (synopsis "Go dotenv parsing library")
      (description "This package provides a library for parsing the dotenv
format in Go.")
      (license license:expat))))

(define-public go-github-com-kr-pretty
  (package
    (name "go-github-com-kr-pretty")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kr/pretty.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18m4pwg2abd0j9cn5v3k2ksk9ig4vlwxmlw9rrglanziv9l967qp"))))
    (build-system go-build-system)
    (propagated-inputs
     `(("go-github-com-kr-text" ,go-github-com-kr-text)))
    (arguments
     '(#:import-path "github.com/kr/pretty"))
    (synopsis "A pretty printer for Go values")
    (description "This package provides a pretty printer for Go values.")
    (home-page "https://github.com/kr/pretty")
    (license license:expat)))

(define-public go-github-com-kr-text
  (package
    (name "go-github-com-kr-text")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kr/text.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gm5bsl01apvc84bw06hasawyqm4q84vx1pm32wr9jnd7a8vjgj1"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kr/text"))
    (synopsis "Text formatting in Go")
    (description "This package provides a text formatting functions in Go.")
    (home-page "https://github.com/kr/text")
    (license license:expat)))
