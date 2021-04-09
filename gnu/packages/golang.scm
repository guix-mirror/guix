;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2016 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2016, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Sergei Trofimovich <slyfox@inbox.ru>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019, 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2019 Giovanni Biscuolo <g@xelera.eu>
;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 raingloom <raingloom@riseup.net>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
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
    ;; The C-language bootstrap of Go:
    ;; https://golang.org/doc/install/source#go14
    (version "1.4-bootstrap-20171003")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://storage.googleapis.com/golang/"
                                  name version ".tar.gz"))
              (sha256
               (base32
                "0liybk5z00hizsb5ypkbhqcawnwwa6mkwgvjjg4y3jm3ndg5pzzl"))))
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

                  ;; XXX: This test fails with tzdata 2020b and newer.  Later
                  ;; Go releases work fine, so just disable this for the
                  ;; bootstrap Go.
                  ("time/example_test.go" "(.+)(ExampleParseInLocation.+)")

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
               (setenv "GO14TESTS" "1")
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
       ("gcc:lib" ,(canonical-package gcc) "lib")))
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

(define-public go-1.14
  (package
    (inherit go-1.4)
    (name "go")
    (version "1.14.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1crh90qkvhlx23hwsi4wxy3l3h8973lr18135y6h1nnzzwr3n3ps"))))
    (arguments
     (substitute-keyword-arguments (package-arguments go-1.4)
       ((#:system system)
        (if (string-prefix? "aarch64-linux" (or (%current-system)
                                                (%current-target-system)))
          "aarch64-linux"
          system))
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

                 ;; Having the patch in the 'patches' field of <origin> breaks
                 ;; the 'TestServeContent' test due to the fact that
                 ;; timestamps are reset.  Thus, apply it from here.
                 (invoke "patch" "-p2" "--force" "-i"
                         (assoc-ref inputs "go-skip-gc-test.patch"))

                 ;; A side effect of these test scripts is testing
                 ;; cgo. Attempts at using cgo flags and directives with these
                 ;; scripts as specified here (https://golang.org/cmd/cgo/)
                 ;; have not worked. The tests continue to state that they can
                 ;; not find object files/headers despite being present.
                 (for-each
                  delete-file
                  '("cmd/go/testdata/script/mod_case_cgo.txt"
                    "cmd/go/testdata/script/list_find.txt"
                    "cmd/go/testdata/script/list_compiled_imports.txt"
                    "cmd/go/testdata/script/cgo_syso_issue29253.txt"
                    "cmd/go/testdata/script/cover_cgo.txt"
                    "cmd/go/testdata/script/cover_cgo_xtest.txt"
                    "cmd/go/testdata/script/cover_cgo_extra_test.txt"
                    "cmd/go/testdata/script/cover_cgo_extra_file.txt"
                    "cmd/go/testdata/script/cgo_path_space.txt"
                    "cmd/go/testdata/script/ldflag.txt"
                    "cmd/go/testdata/script/cgo_path.txt"))

                 (for-each make-file-writable (find-files "."))

                 (substitute* "os/os_test.go"
                   (("/usr/bin") (getcwd))
                   (("/bin/pwd") (which "pwd"))
                   (("/bin/sh") (which "sh")))

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
                    ("net/cgo_unix_test.go" "(.+)(TestCgoLookupPort.+)")
                    ("net/cgo_unix_test.go" "(.+)(TestCgoLookupPortWithCancel.+)")
                    ;; 127.0.0.1 doesn't exist
                    ("net/cgo_unix_test.go" "(.+)(TestCgoLookupPTR.+)")
                    ;; 127.0.0.1 doesn't exist
                    ("net/cgo_unix_test.go" "(.+)(TestCgoLookupPTRWithCancel.+)")
                    ;; /etc/services doesn't exist
                    ("net/parse_test.go" "(.+)(TestReadLine.+)")
                    ("os/os_test.go" "(.+)(TestHostname.+)")
                    ;; The user's directory doesn't exist
                    ("os/os_test.go" "(.+)(TestUserHomeDir.+)")
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

                 ;; These tests fail on aarch64-linux
                 (substitute* "cmd/dist/test.go"
                   (("t.registerHostTest\\(\"testsanitizers/msan.*") ""))

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
       ("go-skip-gc-test.patch" ,(search-patch "go-skip-gc-test.patch"))
       ,@(match (%current-system)
           ((or "armhf-linux" "aarch64-linux")
            `(("gold" ,binutils-gold)))
           (_ `()))
       ,@(package-native-inputs go-1.4)))
    (supported-systems %supported-systems)))

(define-public go go-1.14)

(define-public go-github-com-alsm-ioprogress
  (let ((commit "063c3725f436e7fba0c8f588547bee21ffec7ac5")
        (revision "0"))
    (package
      (name "go-github-com-alsm-ioprogress")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/alsm/ioprogress")
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
                       (url "https://github.com/aki237/nscjar")
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

(define-public go-github-com-golangplus-fmt
  (package
    (name "go-github-com-golangplus-fmt")
    (version "1.0.0")
    (home-page "https://github.com/golangplus/fmt")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "07d5kxz0f8ss3v46y0c8jg02sagi0wlaaijhjzzp0r462jyzqii7"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/golangplus/fmt"))
    (synopsis "Additions to Go's standard @code{fmt} package")
    (description "This package provides additions to Go's stdlib @code{fmt}.")
    (license license:bsd-3)))

(define-public go-github-com-motemen-go-colorine
  (let ((commit "45d19169413a019e4e2be69629dde5c7d92f8706")
        (revision "0"))
    (package
      (name "go-github-com-motemen-go-colorine")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/motemen/go-colorine")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1mdy6q0926s1frj027nlzlvm2qssmkpjis7ic3l2smajkzh07118"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/motemen/go-colorine"))
      (propagated-inputs
       `(("github.com/daviddengcn/go-colortext" ,go-github-com-daviddengcn-go-colortext)))
      (synopsis "Simple colorized console logger for golang")
      (description
       "This package provides simple colorized console logger for golang.")
      (license license:expat))))

(define-public go-github-com-daviddengcn-go-colortext
  (package
    (name "go-github-com-daviddengcn-go-colortext")
    (version "1.0.0")
    (home-page "https://github.com/daviddengcn/go-colortext")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0j5ldwg3a768d3nniiglghr9axj4p87k7f7asqxa1a688xvcms48"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/daviddengcn/go-colortext"))
    (native-inputs
     `(("go-github-com-golangplus-testing" ,go-github-com-golangplus-testing)))
    (synopsis "Change the color of console text and background")
    (description
     "This is a package to change the color of the text and background in the
console, working both under Windows and other systems.

Under Windows, the console APIs are used.  Otherwise, ANSI texts are output.")
    ;; dual-licensed
    (license (list license:bsd-3 license:expat))))

(define-public go-github-com-golangplus-testing
  (package
    (name "go-github-com-golangplus-testing")
    (version "1.0.0")
    (home-page "https://github.com/golangplus/testing")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1a29m4zplf9m14k74lrb55dids2l17vx28sv0g3y3qcv1xygksiv"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/golangplus/testing"))
    (propagated-inputs
     `(("go-github-com-golangplus-fmt" ,go-github-com-golangplus-fmt)))
    (synopsis "Additions to Go's standard testing package")
    (description "This package provides additions to Go's stdlib testing.")
    (license license:bsd-3)))

(define-public go-github-com-leodido-go-urn
  (package
    (name "go-github-com-leodido-go-urn")
    (version "1.2.0")
    (home-page "https://github.com/leodido/go-urn")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d4g1vkhc1180l1n7q48vl84b27c7cziywml78cyijbcdz2f8vim"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/leodido/go-urn"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (synopsis "Parser for uniform resource names as seen on RFC 2141")
    (description
     "This package implements a parser for uniform resource names (URN) as
specified by @uref{https://tools.ietf.org/html/rfc2141, IETF RFC 2141}.")
    (license license:expat)))

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

(define-public go-gopkg-in-go-playground-assert-v1
  (package
    (name "go-gopkg-in-go-playground-assert-v1")
    (version "1.2.1")
    (home-page "https://github.com/go-playground/assert")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h4amgykpa0djwi619llr3g55p75ia0mi184h9s5zdl8l4rhn9pm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/go-playground/assert.v1"))
    (synopsis "Basic assertion library used alongside native Go testing")
    (description
     "This package provides bassic assertions along with building blocks for
custom assertions to be used alongside native Go testing.")
    (license license:expat)))

(define-public go-github-com-go-playground-locales
  (package
    (name "go-github-com-go-playground-locales")
    (version "0.13.0")
    (home-page "https://github.com/go-playground/locales")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qydcpkvss3mf8mk3xzg6a34n8i69aydrigcl2apifrkx72jw7pf"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-playground/locales"))
    (synopsis "Set of locales generated from the CLDR Unicode Project")
    (description
     "This package provides a set of locales generated from the
@uref{http://cldr.unicode.org/, Unicode CLDR Project} which can be used
independently or within an internalization (i18n) package.  Its currently
implemented features include

@itemize
@item Rules generated from the CLDR data, v31.0.3
@item Contains Cardinal, Ordinal and Range Plural Rules
@item Contains Month, Weekday and Timezone translations built in
@item Contains Date & Time formatting functions
@item Contains Number, Currency, Accounting and Percent formatting functions
@item Supports the \"Gregorian\" calendar only
@end itemize")
    (license license:expat)))

(define-public go-github-com-go-playground-universal-translator
  (package
    (name "go-github-com-go-playground-universal-translator")
    (version "0.17.0")
    (home-page "https://github.com/go-playground/universal-translator")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zdiaisb32iv4x93cpbqrgx8ll7sxh4hcd2iibpswy4bwvjbjlz6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-playground/universal-translator"))
    (propagated-inputs
     `(("go-github-com-go-playground-locales" ,go-github-com-go-playground-locales)))
    (synopsis "Translator using Unicode CLDR data and pluralization rules")
    (description
     "This package offers an Internalization Translator for Go using
@uref{http://cldr.unicode.org/, Unicode CLDR Project} data and pluralization
rules.  Its currently implemented features include

@itemize
@item Rules generated from the CLDR data, v30.0.3
@item Contains Cardinal, Ordinal and Range Plural Rules
@item Contains Month, Weekday and Timezone translations built in
@item Contains Date & Time formatting functions
@item Contains Number, Currency, Accounting and Percent formatting functions
@item Supports the \"Gregorian\" calendar only
@item Support loading translations from files
@item Exporting translations to file(s), mainly for getting them
professionally translated
@end itemize")
    (license license:expat)))

(define-public go-gopkg-in-go-playground-validator-v9
  (package
    (name "go-gopkg-in-go-playground-validator-v9")
    (version "9.31.0")
    (home-page "https://gopkg.in/go-playground/validator.v9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-playground/validator")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f8c77s8kx9rip2jarv27x5s4xkcmanh4ndyhbcwvrhncs5rq061"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/go-playground/validator.v9"))
    (native-inputs
     `(("go-gopkg-in-go-playground-assert-v1"
        ,go-gopkg-in-go-playground-assert-v1)))
    (propagated-inputs
     `(("go-github-com-go-playground-universal-translator"
        ,go-github-com-go-playground-universal-translator)
       ("go-github-com-leodido-go-urn" ,go-github-com-leodido-go-urn)))
    (synopsis "Validator for structs and individual fields based on tags")
    (description
     "This package implements value validations for structs and individual
fields based on tags.  It has the following unique features:

@itemize
@item Cross Field and Cross Struct validations by using validation tags or
custom validators
@item Slice, Array and Map diving, which allows any or all levels of a
multidimensional field to be validated
@item Ability to dive into both map keys and values for validation
@item Handles type interface by determining it's underlying type prior to validation
@item Handles custom field types such as sql driver
@uref{https://golang.org/src/database/sql/driver/types.go?s=1210:1293#L29,
Valuer}
@item Alias validation tags, which allows for mapping of several validations
to a single tag for easier defining of validations on structs
@item Extraction of custom defined Field Name e.g. can specify to extract the
JSON name while validating and have it available in the resulting FieldError
@item Customizable i18n aware error messages.
@item Default validator for the @uref{https://github.com/gin-gonic/gin, gin}
web framework
@end itemize")
    (license license:expat)))

(define-public go-github-com-aws-sdk
  (package
    (name "go-github-com-aws-sdk")
    (version "1.35.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ky5lw2s2zpslnnqcs6hgsrwvwbxwgflb5jwf16dd4aga3vrg10c"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go/aws"
       #:unpack-path "github.com/aws/aws-sdk-go"))
    (propagated-inputs
     `(("go-github-com-go-sql-driver-mysql" ,go-github-com-go-sql-driver-mysql)
       ("go-github-com-jmespath-go-jmespath" ,go-github-com-jmespath-go-jmespath)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://github.com/aws/aws-sdk-go")
    (synopsis "Library to access Amazon Web Services (AWS)")
    (description
     "This is the official AWS SDK for the Go programming language.")
    (license license:asl2.0)))

(define-public go-gopkg.in-tomb.v2
  (let ((commit "d5d1b5820637886def9eef33e03a27a9f166942c")
        (revision "0"))
    (package
      (name "go-gopkg.in-tomb.v2")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/go-tomb/tomb")
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

(define-public go-gopkg-in-natefinch-lumberjack.v2
  (package
    (name "go-gopkg-in-natefinch-lumberjack.v2")
    (version "2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/natefinch/lumberjack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1l3vlv72b7rfkpy1164kwd3qzrqmmjnb67akzxqp2mlvc66k6p3d"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/natefinch/lumberjack.v2"))
    (propagated-inputs
     `(("github.com/burntsush/toml" ,go-github-com-burntsushi-toml)
       ("gopkg.in/yaml.v2" ,go-gopkg-in-yaml-v2)))
    (home-page "https://github.com/natefinch/lumberjack")
    (synopsis "Rolling logger for Go")
    (description
     "Lumberjack is a Go package for writing logs to rolling files.")
    (license license:expat)))

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

(define-public go-github-com-saracen-walker
  (package
    (name "go-github-com-saracen-walker")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/saracen/walker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rq1lrp99lx7k1ysbfznn4c1iagnxdhb4lnnklsadnnzi3gvygqz"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/saracen/walker"))
    (inputs
     `(("go-golang-org-x-sync" ,go-golang-org-x-sync)))
    (home-page "https://github.com/saracen/walker")
    (synopsis "Faster, parallel version of Go's filepath.Walk")
    (license license:expat)
    (description "The @code{walker} function is a faster, parallel version, of
@code{filepath.Walk}")))

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
program's help message rather than specifying it programmatically with
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

(define-public go-golang-org-x-tools
  (let ((commit "8b927904ee0dec805c89aaf9172f4459296ed6e8")
        (revision "0"))
    (package
      (name "go-golang-org-x-tools")
      (version (git-version "0.1.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/tools")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-tools-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0iinb70xhcjsddgi42ia1n745lx2ibnjdm6m2v666qrk3876vpck"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/tools"
         ;; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           ;; Source-only package
           (delete 'build))))
      (synopsis "Tools that support the Go programming language")
      (description "This package provides miscellaneous tools that support the
Go programming language.")
      (home-page "https://go.googlesource.com/tools/")
      (license license:bsd-3))))

(define-public go-golang-org-x-crypto
  (let ((commit "2aa609cf4a9d7d1126360de73b55b6002f9e052a")
        (revision "5"))
    (package
      (name "go-golang-org-x-crypto")
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
                  "1yvis6fqbsd7f356aqyi18f76vnwj3bry6mxqnkvshq4cwrf92il"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/crypto"
         ;; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           ;; Source-only package
           (delete 'build)
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (propagated-inputs
       `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
      (synopsis "Supplementary cryptographic libraries in Go")
      (description "This package provides supplementary cryptographic libraries
for the Go language.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-golang-org-x-net
  (let ((commit "ba9fcec4b297b415637633c5a6e8fa592e4a16c3")
        (revision "4"))
    (package
      (name "go-golang-org-x-net")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1hbqvy6r0s5h0dpdqw8fynl3cq0acin3iyqki9xvl5r8h33yb9bx"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net"
         ; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'build))))
      (synopsis "Go supplemental networking libraries")
      (description "This package provides supplemental Go networking libraries.")
      (home-page "https://go.googlesource.com/net")
      (license license:bsd-3))))

(define-public go-golang-org-x-net-html
  (package
    (inherit go-golang-org-x-net)
    (name "go-golang.org-x-net-html")
    (arguments
     '(#:import-path "golang.org/x/net/html"
       #:unpack-path "golang.org/x/net"))
    (synopsis "HTML5-compliant tokenizer and parser")
    (description
     "This package provides an HTML5-compliant tokenizer and parser.")
    (home-page "https://godoc.org/golang.org/x/net/html")))

(define-public go-golang-org-x-image
  (let ((commit "58c23975cae11f062d4b3b0c143fe248faac195d")
        (revision "1"))
    (package
      (name "go-golang-org-x-image")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://go.googlesource.com/image")
                       (commit commit)))
                (file-name (string-append "go.googlesource.com-image-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0i2p2girc1sfcic6xs6vrq0fp3szfx057xppksb67kliywjjrm5x"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/image"
         ; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'build))))
      (home-page "https://go.googlesource.com/image")
      (synopsis "Supplemental Go image libraries")
      (description "This package provides supplemental Go libraries for image
processing.")
      (license license:bsd-3))))

(define-public go-golang-org-x-sync
  (let ((commit "6e8e738ad208923de99951fe0b48239bfd864f28")
        (revision "1"))
    (package
      (name "go-golang-org-x-sync")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/sync")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1avk27pszd5l5df6ff7j78wgla46ir1hhy2jwfl9a3c0ys602yx9"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/sync"
         #:tests? #f
         ;; Source-only package
         #:phases
         (modify-phases %standard-phases
           (delete 'build))))
      (synopsis "Additional Go concurrency primitives")
      (description "This package provides Go concurrency primitives in addition
to the ones provided by the language and “sync” and “sync/atomic”
packages.")
      (home-page "https://go.googlesource.com/sync/")
      (license license:bsd-3))))

(define-public go-golang-org-x-sys
  (let ((commit "05986578812163b26672dabd9b425240ae2bb0ad")
        (revision "7"))
    (package
      (name "go-golang-org-x-sys")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/sys")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1q2rxb6z5l6pmlckjsz2l0b8lw7bqgk6frhzbmi1dv0y5irb2ka7"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/sys"
         ;; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'build))))
      (synopsis "Go support for low-level system interaction")
      (description "This package provides supplemental libraries offering Go
support for low-level interaction with the operating system.")
      (home-page "https://go.googlesource.com/sys")
      (license license:bsd-3))))

(define-public go-golang-org-x-text
  (package
    (name "go-golang-org-x-text")
    (version "0.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/text")
                    (commit (string-append "v" version))))
              (file-name (string-append "go.googlesource.com-text-"
                                        version "-checkout"))
              (sha256
               (base32
                "0flv9idw0jm5nm8lx25xqanbkqgfiym6619w575p7nrdh0riqwqh"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "golang.org/x/text"
       ; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (synopsis "Supplemental Go text processing libraries")
    (description "This package provides supplemental Go libraries for text
    processing.")
    (home-page "https://go.googlesource.com/text")
    (license license:bsd-3)))

(define-public go-golang-org-x-time
  (let ((commit "9d24e82272b4f38b78bc8cff74fa936d31ccd8ef")
        (revision "2"))
    (package
      (name "go-golang-org-x-time")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/time")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1f5nkr4vys2vbd8wrwyiq2f5wcaahhpxmia85d1gshcbqjqf8dkb"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/time"
         ; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'build))))
;      (propagated-inputs
;       `(("go-golang-org-x-net" ,go-golang-org-x-net)))
      (synopsis "Supplemental Go time libraries")
      (description "This package provides supplemental Go libraries related to
time.")
      (home-page "https://godoc.org/golang.org/x/time/rate")
      (license license:bsd-3))))

(define-public go-golang-org-x-oauth2
  (let ((commit "0f29369cfe4552d0e4bcddc57cc75f4d7e672a33")
        (revision "1"))
    (package
      (name "go-golang-org-x-oauth2")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/oauth2")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-oauth2-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "06jwpvx0x2gjn2y959drbcir5kd7vg87k0r1216abk6rrdzzrzi2"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/oauth2"))
      (propagated-inputs
       `(("go-golang-org-x-net" ,go-golang-org-x-net)))
      (home-page "https://go.googlesource.com/oauth2")
      (synopsis "Client implementation of the OAuth 2.0 spec")
      (description "This package contains a client implementation for OAuth 2.0
 spec in Go.")
      (license license:bsd-3))))

(define-public go-golang-org-x-xerrors
  (let ((commit "5ec99f83aff198f5fbd629d6c8d8eb38a04218ca")
        (revision "0"))
    (package
      (name "go-golang-org-x-xerrors")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/xerrors")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dbzc3gmf2haazpv7cgmv97rq40g2xzwbglc17vas8dwhgwgwrzb"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/xerrors"))
      (synopsis "Go 1.13 error values")
      (description
       "This package holds the transition packages for the new Go 1.13 error values.")
      (home-page "https://godoc.org/golang.org/x/xerrors")
      (license license:bsd-3))))

(define-public go-github-com-burntsushi-toml
  (package
    (name "go-github-com-burntsushi-toml")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BurntSushi/toml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1fjdwwfzyzllgiwydknf1pwjvy49qxfsczqx5gz3y0izs7as99j6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/BurntSushi/toml"))
    (home-page "https://github.com/BurntSushi/toml")
    (synopsis "Toml parser and encoder for Go")
    (description "This package is toml parser and encoder for Go.  The interface
is similar to Go's standard library @code{json} and @code{xml} package.")
    (license license:expat)))

(define-public go-github-com-getsentry-raven-go
  (let ((commit "5c24d5110e0e198d9ae16f1f3465366085001d92")
        (revision "0"))
    (package
      (name "go-github-com-getsentry-raven-go")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/getsentry/raven-go")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0lvc376sq8r8jhy2v1m6rf1wyld61pvbk0x6j9xpg56ivqy69xs7"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/getsentry/raven-go"))
      (propagated-inputs
       `(("go-github-com-certifi-gocertifi" ,go-github-com-certifi-gocertifi)
         ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
      (home-page "https://github.com/getsentry/raven-go")
      (synopsis "Sentry client in Go")
      (description "This package is a Go client API for the Sentry event/error
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
               (url "https://github.com/hashicorp/go-version")
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
               (url "https://github.com/jpillora/backoff")
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

(define-public go-github-com-stretchr-objx
  (package
    (name "go-github-com-stretchr-objx")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stretchr/objx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0pcdvakxgddaiwcdj73ra4da05a3q4cgwbpm2w75ycq4kzv8ij8k"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/stretchr/objx"))
    (home-page "https://github.com/stretchr/objx")
    (synopsis "Go package for dealing with maps, slices, JSON and other data")
    (description "This package provides a Go library for dealing with maps,
slices, JSON and other data.")
    (license license:expat)))

(define-public go-github-com-stretchr-testify
  (package
    (name "go-github-com-stretchr-testify")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stretchr/testify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09r89m1wy4cjv2nps1ykp00qjpi0531r07q3s34hr7m6njk4srkl"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/stretchr/testify"))
    (propagated-inputs
     `(("github.com/davecgh/go-spew" ,go-github-com-davecgh-go-spew)
       ("github.com/pmezard/go-difflib" ,go-github-com-pmezard-go-difflib)
       ("github.com/stretchr/objx" ,go-github-com-stretchr-objx)
       ("gopkg.in/yaml.v2" ,go-gopkg-in-yaml-v2)))
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
    (license license:expat)))

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
                 (url "https://github.com/tevino/abool")
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

(define-public go-github-com-tomnomnom-gron
  (package
    (name "gron")
    (version "0.6.1")
    (home-page "https://github.com/tomnomnom/gron")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qmzawkhg0qn9kxxrssbdjni2khvamhrcklv3yxc0ljmh77mh61m"))))
    (build-system go-build-system)
    (arguments
     (let ((import-path "github.com/tomnomnom/gron"))
       `(#:import-path ,import-path
         #:phases
         (modify-phases %standard-phases
           (add-after 'check 'remove-non-source
             (lambda _
               (for-each (lambda (dir)
                           (delete-file-recursively
                            (string-append "src/" ,import-path dir)))
                         '("/docs" "/script" "/testdata"))
               #t))))))
    (inputs
     `(("github.com/fatih/color" ,go-github-com-fatih-color)
       ("github.com/mattn/go-colorable" ,go-github-com-mattn-go-colorable)
       ("github.com/mattn/go-isatty" ,go-github-com-mattn-go-isatty)
       ("github.com/nwidger/jsoncolor" ,go-github-com-nwidger-jsoncolor)
       ("github.com/pkg/errors" ,go-github-com-pkg-errors)))
    (synopsis "Transform JSON to make it easier to grep")
    (description
     "This package transforms JSON into discrete assignments to make it easier
to use line-based tools such as grep to search for what you want and see the
absolute \"path\" to it.")
    (license license:expat)))

(define-public go-github-com-tv42-httpunix
  (let ((commit "2ba4b9c3382c77e7b9ea89d00746e6111d142a22")
        (revision "0"))
    (package
      (name "go-github-com-tv42-httpunix")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tv42/httpunix")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0xbwpip2hsfhd2kd878jn5ndl8y1i9658lggha4x3xb5m1rsds9w"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/tv42/httpunix"))
      (home-page "https://github.com/tv42/httpunix")
      (synopsis "Go library to talk HTTP over Unix domain sockets")
      (description "This package is a Go library to talk HTTP over Unix domain
sockets.")
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
               (url "https://github.com/blang/semver")
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
  (package
    (name "go-github-com-emicklei-go-restful")
    (version "3.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emicklei/go-restful")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0m1y5a6xr6hmdj77afrvyh2llkbhn1166lcrgis654shl8zs9qhz"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/emicklei/go-restful"))
    (home-page "https://github.com/emicklei/go-restful")
    (synopsis "Build REST-style web services using Go")
    (description "This package provides @code{go-restful}, which helps
developers to use @code{http} methods explicitly and in a way that's consistent
with the HTTP protocol definition.")
    (license license:expat)))

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
               (url "https://github.com/google/cadvisor")
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
information about the resource usage and performance characteristics of running
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
               (url "https://github.com/google/gofuzz")
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

(define-public go-github-com-gorilla-css
  (package
    (name "go-github-com-gorilla-css")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gorilla/css")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "116fhy3n7bsq3psyn4pa0i4x9zy916kh1zxslmbbp0p9l4i7ysrj"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/css/scanner"
       #:unpack-path "github.com/gorilla/css"))
    (home-page "https://github.com/gorilla/css/")
    (synopsis "CSS3 tokenizer")
    (description "This package provides a CSS3 tokenizer.")
    (license license:bsd-3)))

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
               (url "https://github.com/gorilla/context")
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
  (package
    (name "go-github-com-gorilla-mux")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/mux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "18f0q9qxgq1yh4ji07mqhiydfcwvi56z9d775v7dc7yckj33kpdk"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gorilla/mux"))
    (home-page "https://github.com/gorilla/mux")
    (synopsis "URL router and dispatcher for Go")
    (description
     "Gorilla/Mux implements a request router and dispatcher for matching
incoming requests with their respective handler.")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-handlers
  (package
    (name "go-github-com-gorilla-handlers")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/handlers")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15gycdz9lkjnsvvichsbdf25vf6pi1sfn41khhz53iqf300l0w0s"))))
    (build-system go-build-system)
    (propagated-inputs
     `(("github.com/felixge/httpsnoop" ,go-github-com-felixge-httpsnoop)))
    (arguments
     '(#:tests? #f                      ; Tries to download from the internet
       #:import-path "github.com/gorilla/handlers"))
    (home-page "https://github.com/gorilla/handlers")
    (synopsis "Middleware for Go HTTP services and web applications")
    (description "A collection of useful middleware for Go HTTP services
and web applications.")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-securecookie
  (package
    (name "go-github-com-gorilla-securecookie")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/securecookie")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "16bqimpxs9vj5n59vm04y04v665l7jh0sddxn787pfafyxcmh410"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gorilla/securecookie"))
    (home-page "https://github.com/gorilla/securecookie")
    (synopsis "Encodes and decodes authenticated and optionally encrypted
cookie values")
    (description
     "Gorilla/securecookie encodes and decodes authenticated and optionally
encrypted cookie values for Go web applications.")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-csrf
  (package
    (name "go-github-com-gorilla-csrf")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/csrf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0iryq0z48yi7crfbd8jxyn7lh1gsglpiglvjgnf23bz6xfisssav"))))
    (build-system go-build-system)
    (propagated-inputs
     `(("github.com/gorilla/securecookie" ,go-github-com-gorilla-securecookie)
       ("github.com/pkg/errors" ,go-github-com-pkg-errors)))
    (arguments
     '(#:import-path "github.com/gorilla/csrf"))
    (home-page "https://github.com/gorilla/csrf")
    (synopsis "Cross Site Request Forgery (CSRF) prevention middleware")
    (description
     "Gorilla/csrf provides Cross Site Request Forgery (CSRF) prevention
middleware for Go web applications and services.")
    (license license:bsd-3)))

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
                 (url "https://github.com/jonboulle/clockwork")
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

(define-public go-github-com-spf13-afero
  (package
    (name "go-github-com-spf13-afero")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/afero")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j9r65qgd58324m85lkl49vk9dgwd62g7dwvkfcm3k6i9dc555a9"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/spf13/afero"))
    (propagated-inputs
     `(("golang.org/x/text" ,go-golang-org-x-text)))
    (home-page "https://github.com/spf13/afero")
    (synopsis "File system abstraction for Go")
    (description
     "This package provides a file system abstraction for Go.")
    (license license:asl2.0)))

(define-public go-github-com-spf13-cast
  (package
    (name "go-github-com-spf13-cast")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/cast")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lb84788glr0qzrq2ifi36rgvp96qrgywvxrr3ggq5hrbr38hgn1"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/spf13/cast"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/spf13/cast")
    (synopsis "Safe and easy casting from one type to another in Go")
    (description "Safe and easy casting from one type to another in Go")
    (license license:expat)))

(define-public go-github-com-spf13-cobra
  (package
    (name "go-github-com-spf13-cobra")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/cobra")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vbppqqhby302a5ayn0296jqr71qkcd4c9am7wzsk6z71fwdsa7h"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/spf13/cobra"))
    (propagated-inputs
     `(("github.com/spf13/pflag" ,go-github-com-spf13-pflag)))
    (home-page "https://github.com/spf13/cobra")
    (synopsis "Go library for creating CLI applications")
    (description "Cobra is both a library for creating powerful modern CLI
applications as well as a program to generate applications and command files.")
    (license license:asl2.0)))

(define-public go-github-com-spf13-jwalterweatherman
  (package
    (name "go-github-com-spf13-jwalterweatherman")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/jwalterweatherman")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ywmkwci5zyd88ijym6f30fj5c0k2yayxarkmnazf5ybljv50q7b"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/spf13/jwalterweatherman"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/spf13/jwalterweatherman")
    (synopsis "Go logging library")
    (description "Go logging library")
    (license license:expat)))

(define-public go-github-com-spf13-pflag
  (package
    (name "go-github-com-spf13-pflag")
    (version "1.0.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/spf13/pflag")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0gpmacngd0gpslnbkzi263f5ishigzgh6pbdv9hp092rnjl4nd31"))))
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
    (license license:bsd-3)))

(define-public go-github-com-spf13-viper
  (package
    (name "go-github-com-spf13-viper")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/viper")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "099n2g7fg6r8hqyszqw2axr775qyhyvwhsykvgw0f0s16ql48h5c"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/spf13/viper"))
    (propagated-inputs
     `(("github.com/spf13/afero" ,go-github-com-spf13-afero)
       ("github.com/spf13/cast" ,go-github-com-spf13-cast)
       ("github.com/spf13/pflag" ,go-github-com-spf13-pflag)
       ("github.com/spf13/jwalterweatherman" ,go-github-com-spf13-jwalterweatherman)
       ("github.com/fsnotify/fsnotify" ,go-github-com-fsnotify-fsnotify)
       ("github.com/hashicorp/hcl" ,go-github-com-hashicorp-hcl)
       ("github.com/magiconair/properties" ,go-github-com-magiconair-properties)
       ("github.com/mitchellh/mapstructure" ,go-github-com-mitchellh-mapstructure)
       ("github.com/pelletier/go-toml" ,go-github-com-pelletier-go-toml)
       ("github.com/subosito/gotenv" ,go-github-com-subosito-gotenv)

       ("gopkg.in/ini.v1" ,go-gopkg-in-ini-v1)
       ("gopkg.in/yaml.v2" ,go-gopkg-in-yaml-v2)))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/spf13/viper")
    (synopsis "Go configuration with fangs")
    (description
     "Viper is a complete configuration solution for Go applications including
12-Factor apps.  It is designed to work within an application, and can handle
all types of configuration needs and formats.")
    (license license:expat)))

(define-public go-github-com-felixge-httpsnoop
  (package
    (name "go-github-com-felixge-httpsnoop")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/felixge/httpsnoop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ncd8lar5zxiwjhsp315s4hsl4bhnm271h49jhyxc66r5yffgmac"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/felixge/httpsnoop"))
    (home-page "https://github.com/felixge/httpsnoop/")
    (synopsis "Capture http related metrics")
    (description
     "Httpsnoop provides an easy way to capture http related
metrics (i.e. response time, bytes written, and http status code) from your
application's http.Handlers.")
    (license license:expat)))

(define-public go-github-com-fsnotify-fsnotify
  (package
    (name "go-github-com-fsnotify-fsnotify")
    (version "1.4.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fsnotify/fsnotify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1i1r72knpbfwwql9frn9bqc3nhfc2ai5m6qllcyr6wban62lr40x"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/fsnotify/fsnotify"))
    (propagated-inputs
     `(("golang.org/x/sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/fsnotify/fsnotify")
    (synopsis "File system notifications for Go")
    (description "File system notifications for Go")
    (license license:bsd-3)))

(define-public go-github-com-magiconair-properties
  (package
    (name "go-github-com-magiconair-properties")
    (version "1.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/magiconair/properties")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0q7d55z0v8y55dyy8nhgdnswf5zkgj3i87irbk294nvzhx01bnxd"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/magiconair/properties"))
    (home-page "https://github.com/magiconair/properties")
    (synopsis "Java properties scanner for Go")
    (description "Java properties scanner for Go")
    (license license:bsd-2)))

(define-public go-github-com-pelletier-go-toml
  (package
    (name "go-github-com-pelletier-go-toml")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pelletier/go-toml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fxmjm85c9h43lvqz71wr93fcc63bhj82nwby80222xx8ja63g7y"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/pelletier/go-toml"))
    (native-inputs
     `(("github.com/BurntSushi/toml" ,go-github-com-burntsushi-toml)
       ("github.com/davecgh/go-spew" ,go-github-com-davecgh-go-spew)
       ("gopkg.in/yaml.v2" ,go-gopkg-in-yaml-v2)))
    (home-page "https://github.com/pelletier/go-toml")
    (synopsis "Go library for the TOML configuration language")
    (description "Go library for the TOML configuration language")
    (license license:expat)))

(define-public go-github-com-subosito-gotenv
  (package
    (name "go-github-com-subosito-gotenv")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/subosito/gotenv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mav91j7r4arjkpq5zcf9j74f6pww8ic53x43wy7kg3ibw31yjs5"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/subosito/gotenv"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/subosito/gotenv")
    (synopsis "Go library for loading environment variables from files")
    (description "Go library for loading environment variables from files")
    (license license:expat)))

(define-public go-github-com-sirupsen-logrus
  (package
    (name "go-github-com-sirupsen-logrus")
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sirupsen/logrus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0rvqzic2zz7fpxyizmqxwmhv1m52ii9bgxnqa6km8wsa0l08wh42"))))
    (build-system go-build-system)
    (propagated-inputs
     `(("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (arguments
     '(#:import-path "github.com/sirupsen/logrus"))
    (home-page "https://github.com/sirupsen/logrus")
    (synopsis "Structured, pluggable logging for Go")
    (description "Logrus is a structured logger for Go, completely API
compatible with the standard library logger.")
    (license license:expat)))

(define-public go-github-com-rifflock-lfshook
  (package
    (name "go-github-com-rifflock-lfshook")
    (version "2.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rifflock/lfshook")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wxqjcjfg8c0klmdgmbw3ckagby3wg9rkga9ihd4fsf05x5scxrc"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/rifflock/lfshook"))
    (propagated-inputs
     `(("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)))
    (home-page "https://github.com/rifflock/lfshook")
    (synopsis "Local File System hook for Logrus logger")
    (description "This package provides a hook for Logrus to write directly to
a file on the file system.  The log levels are dynamic at instantiation of the
hook, so it is capable of logging at some or all levels.")
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
                "https://github.com/ayufan/golang-kardianos-service")
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
       `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
         ("go-github-com-sirupsen-logrus"
          ,go-github-com-sirupsen-logrus)
         ("go-golang-org-x-crypto"
          ,go-golang-org-x-crypto)))
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
      (synopsis "This package is a Docker toolset to pack, ship, store, and
deliver content")
      (description "Docker Distribution is a Docker toolset to pack, ship,
store, and deliver content.  It contains Docker Registry 2.0 and libraries
to interact with distribution components.")
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
                 (url "https://github.com/docker/go-connections")
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
       "This package provides a library to work with network connections in
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
               (url "https://github.com/docker/machine")
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
               (url "https://github.com/gorhill/cronexpr")
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
  (let ((commit "788fd78401277ebd861206a03c884797c6ec5541")
        (revision "1"))
    (package
      (name "go-gopkg-in-check-v1")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/go-check/check")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0v3bim0j375z81zrpr5qv42knqs0y2qv2vkjiqi5axvb78slki1a"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "gopkg.in/check.v1"))
      (propagated-inputs
       `(("go-github-com-kr-pretty" ,go-github-com-kr-pretty)))
      (home-page "https://gopkg.in/check.v1")
      (synopsis "Test framework for the Go language")
      (description "This package provides a test library for the Go language.")
      (license license:asl2.0))))

(define-public go-gopkg-in-ini-v1
  (package
    (name "go-gopkg-in-ini-v1")
    (version "1.56.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ini/ini")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j5z0cngg6mq2f9id083jcdi7k6r2h35714pashv6sdv2q7bmfc5"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/ini.v1"
       ;; Requires large unpackaged test framework
       #:tests? #f))
    (home-page "https://gopkg.in/ini.v1")
    (synopsis "Go library for ini files")
    (description "Go library for ini files")
    (license license:asl2.0)))

(define-public go-gopkg-in-yaml-v2
  (package
    (name "go-gopkg-in-yaml-v2")
    (version "2.2.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/yaml.v2.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "01wj12jzsdqlnidpyjssmj0r4yavlqy7dwrg7adqd8dicjc4ncsa"))))
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
    (license license:asl2.0)))

(define-public go-github-com-mattn-go-isatty
  (package
    (name "go-github-com-mattn-go-isatty")
    (version "0.0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-isatty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0h671sv7hfprja495kavazkalkx7xzaqksjh13brcnwq67ijrali"))))
    (build-system go-build-system)
    (propagated-inputs
     `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (arguments
     '(#:import-path "github.com/mattn/go-isatty"))
    (home-page "https://github.com/mattn/go-isatty")
    (synopsis "Provide @code{isatty} for Golang")
    (description "This package provides @code{isatty}, a Go module that can
tell you whether a file descriptor points to a terminal and the type of the
terminal.")
    (license license:expat)))

(define-public go-github-com-mattn-go-colorable
  (package
    (name "go-github-com-mattn-go-colorable")
    (version "0.1.8")
    (home-page "https://github.com/mattn/go-colorable")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0q34zqwbnls72md8q8mhj368s7p3i4xspvs3rk8fs76s0pn7dr2l"))))
    (build-system go-build-system)
    (native-inputs
     `(("go-github-com-mattn-go-isatty"
        ,go-github-com-mattn-go-isatty)))
    (arguments
     '(#:import-path "github.com/mattn/go-colorable"))
    (synopsis "Handle ANSI color escapes on Windows")
    (description "This package provides @code{colorable}, a module that
makes it possible to handle ANSI color escapes on Windows.")
    (license license:expat)))

(define-public go-github-com-mattn-go-pointer
  (let ((commit "a0a44394634f41e4992b173b24f14fecd3318a67")
        (revision "1"))
    (package
      (name "go-github-com-mattn-go-pointer")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mattn/go-pointer")
               (commit commit)))
         (sha256
          (base32
           "09w7hcyc0zz2g23vld6jbcmq4ar27xakp1ldjvh549i5izf2anhz"))
         (file-name (git-file-name name version))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/mattn/go-pointer"))
      (home-page "https://github.com/mattn/go-pointer")
      (synopsis "Utility for cgo")
      (description
       "This package allows for a cgo argument to be passed a Go pointer.")
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
         ;; Tests don't pass "vet" on Go since 1.11.  See
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
  (package
    (name "go-github-com-gogo-protobuf")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gogo/protobuf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x77x64sxjgfhmbijqfzmj8h4ar25l2w97h01q3cqs1wk7zfnkhp"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gogo/protobuf"
       ; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
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
    (license license:bsd-3)))

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
               (url "https://github.com/libp2p/go-flow-metrics")
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
  (package
    (name "go-github-com-davecgh-go-spew")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/davecgh/go-spew")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hka6hmyvp701adzag2g26cxdj47g21x6jz4sc6jjz1mn59d474y"))))
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
    (license license:isc)))

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
               (url "https://github.com/btcsuite/btclog")
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
               (url "https://github.com/btcsuite/btcd")
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
significantly diverged since then.  The @command{btcsuite} developers original
is licensed under the liberal ISC license.

Although this package was primarily written for btcd, it has intentionally
been designed so it can be used as a standalone package for any projects
needing to use secp256k1 elliptic curve cryptography.")
      (license license:isc))))

(define-public go-github-com-minio-sha256-simd
  (package
    (name "go-github-com-minio-sha256-simd")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/sha256-simd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1j0iqsckm97g4l79vd4mc7apbmkdar23jpzqpnpdhwpfd834j8lp"))))
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
    (license license:asl2.0)))

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
               (url "https://github.com/libp2p/go-libp2p-crypto")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0qwpy57qv5143l9dlfwfvpqsxdd2i4zwnawx1w4pmgxxim3nw1wb"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/libp2p/go-libp2p-crypto"))
      (native-inputs
       `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
         ("go-github-com-btcsuite-btcd-btcec" ,go-github-com-btcsuite-btcd-btcec)
         ("go-github-com-gogo-protobuf" ,go-github-com-gogo-protobuf)
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
               (url "https://github.com/mr-tron/base58")
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
               (url "https://github.com/gxed/hashland")
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
               (url "https://github.com/minio/blake2b-simd")
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
  (package
    (name "go-github-com-spaolacci-murmur3")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spaolacci/murmur3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1lv3zyz3jy2d76bhvvs8svygx66606iygdvwy5cwc0p5z8yghq25"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/spaolacci/murmur3"))
    (home-page "https://github.com/spaolacci/murmur3")
    (synopsis "Native MurmurHash3 Go implementation")
    (description "Native Go implementation of Austin Appleby's third MurmurHash
revision (aka MurmurHash3).

Reference algorithm has been slightly hacked as to support the streaming mode
required by Go's standard Hash interface.")
    (license license:bsd-3)))

(define-public go-github-com-twmb-murmur3
  (package
    (name "go-github-com-twmb-murmur3")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twmb/murmur3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00riapwkyf23l5wyis47mbr8rwr4yrjw491jfc30wpzs111c1gyy"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/twmb/murmur3"))
    (home-page "https://github.com/twmb/murmur3")
    (synopsis "Native MurmurHash3 Go implementation")
    (description "Native Go implementation of Austin Appleby's third
MurmurHash revision (aka MurmurHash3).

Reference algorithm has been slightly hacked as to support the streaming mode
required by Go's standard Hash interface.")
    (license license:bsd-3)))

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
               (url "https://github.com/multiformats/go-multihash")
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
         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
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
               (url "https://github.com/libp2p/go-libp2p-peer")
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
         ("go-github-com-gogo-protobuf" ,go-github-com-gogo-protobuf)
         ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)
         ("go-github-com-minio-blake2b-simd" ,go-github-com-minio-blake2b-simd)
         ("go-github-com-btcsuite-btcd-btcec" ,go-github-com-btcsuite-btcd-btcec)
         ("go-github-com-mr-tron-base58" ,go-github-com-mr-tron-base58)
         ("go-github-com-multiformats-go-multihash" ,go-github-com-multiformats-go-multihash)
         ("go-github-com-gxed-hashland-keccakpg" ,go-github-com-gxed-hashland-keccakpg)
         ("go-github-com-spaolacci-murmur3" ,go-github-com-spaolacci-murmur3)
         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
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
               (url "https://github.com/libp2p/go-libp2p-protocol")
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
               (url "https://github.com/libp2p/go-libp2p-metrics")
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
         ("go-github-com-gogo-protobuf" ,go-github-com-gogo-protobuf)
         ("go-github-com-gxed-hashland-keccakpg" ,go-github-com-gxed-hashland-keccakpg)
         ("go-github-com-minio-blake2b-simd" ,go-github-com-minio-blake2b-simd)
         ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)
         ("go-github-com-spaolacci-murmur3" ,go-github-com-spaolacci-murmur3)
         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
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
               (url "https://github.com/mitchellh/go-homedir")
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

(define-public go-github-com-mitchellh-mapstructure
  (package
    (name "go-github-com-mitchellh-mapstructure")
    (version "1.1.2") ;; NOTE: Updating to 1.3.1 breaks tests on viper-1.7.0
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/mapstructure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03bpv28jz9zhn4947saqwi328ydj7f6g6pf1m2d4m5zdh5jlfkrr"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mitchellh/mapstructure"))
    (home-page "https://github.com/mitchellh/mapstructure")
    (synopsis "Go library for decoding generic map values")
    (description "Go library for decoding generic map values")
    (license license:expat)))

(define-public go-github-com-mitchellh-reflectwalk
  (package
    (name "go-github-com-mitchellh-reflectwalk")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mitchellh/reflectwalk")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pa6a3nhzwv5s5yqcmsmsfhdp5ggxsg2wa86f3akawxrhrkjarnx"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mitchellh/reflectwalk"))
    (home-page "https://github.com/mitchellh/reflectwalk/")
    (synopsis "Walk a value in Go using reflection")
    (description "reflectwalk is a Go library for \"walking\" a value in Go
using reflection, in the same way a directory tree can be \"walked\" on the
file system.  Walking a complex structure can allow you to do manipulations on
unknown structures such as those decoded from JSON.")
    (license license:expat)))

(define-public go-github-com-mitchellh-copystructure
  (package
    (name "go-github-com-mitchellh-copystructure")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/copystructure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "05njg92w1088v4yl0js0zdrpfq6k37i9j14mxkr3p90p5yd9rrrr"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mitchellh/copystructure"))
    (native-inputs
     `(("go-github-com-mitchellh-reflectwalk" ,go-github-com-mitchellh-reflectwalk)))
    (home-page "https://github.com/mitchellh/copystructure")
    (synopsis "Go library for decoding deep copying values")
    (description "@code{copystructure} is a Go library for deep copying values
in Go.

This allows you to copy Go values that may contain reference values such as
maps, slices, or pointers, and copy their data as well instead of just their
references.")
    (license license:expat)))

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
               (url "https://github.com/multiformats/go-multiaddr")
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
         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
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
               (url "https://github.com/multiformats/go-multiaddr-net")
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
         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
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
               (url "https://github.com/whyrusleeping/tar-utils")
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
               (url "https://github.com/cheekybits/is")
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
               (url "https://github.com/sabhiram/go-gitignore")
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
  (package
    (name "go-github-com-urfave-cli")
    (version "1.22.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/urfave/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10mcnvi5qmn00vpyk6si8gjka7p654wr9hac4zc9w5h3ickhvbdc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/urfave/cli"))
    (propagated-inputs
     `(("go-github-com-go-md2man" ,go-github-com-go-md2man)))
    (home-page "https://github.com/urfave/cli")
    (synopsis "Simple, fast, and fun package for building command line apps in Go")
    (description "@command{cli} is a simple, fast, and fun package for
building command line apps in Go.  The goal is to enable developers to write
fast and distributable command line applications in an expressive way.")
    (license license:expat)))

(define-public go-github-com-urfave-cli-v2
  (package
    (inherit go-github-com-urfave-cli)
    (name "go-github-com-urfave-cli-v2")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/urfave/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08pvn7gyfznni72xrxfh2x6xxa8ykr7l1ka278js8g8qkh71bj8l"))))
    (arguments
     '(#:import-path "github.com/urfave/cli/v2"))))

(define-public go-github-com-go-md2man
  (package
    (name "go-github-com-go-md2man")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cpuguy83/go-md2man")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0r1f7v475dxxgzqci1mxfliwadcrk86ippflx9n411325l4g3ghv"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "vendor")
                   #t))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/cpuguy83/go-md2man"))
    (propagated-inputs
     `(("go-github-com-russross-blackfriday" ,go-github-com-russross-blackfriday)))
    (home-page "https://github.com/cpuguy83/go-md2man")
    (synopsis "Convert markdown into roff")
    (description "Go-md2man is a Go program that converts markdown to roff for
the purpose of building man pages.")
    (license license:expat)))

(define-public go-github-com-russross-blackfriday
  (package
    (name "go-github-com-russross-blackfriday")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/russross/blackfriday")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0nlz7isdd4rgnwzs68499hlwicxz34j2k2a0b8jy0y7ycd2bcr5j"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/russross/blackfriday"))
    (propagated-inputs
     `(("go-github-com-shurcool-sanitized-anchor-name"
        ,go-github-com-shurcool-sanitized-anchor-name)))
    (native-inputs
     `(("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)))
    (home-page "https://github.com/russross/blackfriday")
    (synopsis "Markdown processor in Go")
    (description "Blackfriday is a Markdown processor in Go.")
    (license license:bsd-2)))

(define-public go-github-com-shurcool-sanitized-anchor-name
  (package
    (name "go-github-com-shurcool-sanitized-anchor-name")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shurcooL/sanitized_anchor_name")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1gv9p2nr46z80dnfjsklc6zxbgk96349sdsxjz05f3z6wb6m5l8f"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/shurcooL/sanitized_anchor_name"))
    (home-page "https://github.com/shurcooL/sanitized_anchor_name")
    (synopsis "Create sanitized anchor names")
    (description "This package provides a Go program for creating sanitized
anchor names.")
    (license license:expat)))

(define-public go-github-com-pmezard-go-difflib
  (package
    (name "go-github-com-pmezard-go-difflib")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pmezard/go-difflib")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c1cn55m4rypmscgf0rrb88pn58j3ysvc2d0432dp3c6fqg6cnzw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/pmezard/go-difflib/difflib"
       #:unpack-path "github.com/pmezard/go-difflib/"))
    (home-page "https://github.com/pmezard/go-difflib")
    (synopsis "Go diff implementation")
    (description "This package provides unified and context-aware diffs in Go.")
    (license license:bsd-3)))

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
               (url "https://github.com/whyrusleeping/json-filter")
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
               (url "https://github.com/whyrusleeping/progmeter")
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
               (url "https://github.com/whyrusleeping/stump")
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
               (url "https://github.com/kr/fs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "11zg176x9hr9q7fsk95r6q0wf214gg4czy02slax4x56n79g6a7q"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/kr/fs"))
      (home-page "https://github.com/kr/fs")
      (synopsis "File-system-related functions for Go")
      (description
       "The fs package provides file-system-related Go functions.")
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
    (version "0.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kr/pretty")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vzfz06y9q8gs2nxx0kys0591vzp78k0fvpb8digi5n15h3b25hy"))))
    (build-system go-build-system)
    (propagated-inputs
     `(("go-github-com-kr-text" ,go-github-com-kr-text)))
    (arguments
     '(#:import-path "github.com/kr/pretty"))
    (synopsis "Pretty printer for Go values")
    (description "This package provides a pretty printer for Go values.")
    (home-page "https://github.com/kr/pretty")
    (license license:expat)))

(define-public go-github-com-kylelemons-godebug
  (package
    (name "go-github-com-kylelemons-godebug")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kylelemons/godebug")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0dkk3friykg8p6wgqryx6745ahhb9z1j740k7px9dac6v5xjp78c"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kylelemons/godebug/diff"
       #:unpack-path "github.com/kylelemons/godebug"))
    (home-page "https://github.com/kylelemons/godebug")
    (synopsis "Pretty printer for Go values.")
    (description
     "This package will pretty print a compact representation of a Go data
structure.  It can also produce a much more verbose, one-item-per-line
representation suitable for computing diffs.")
    (license license:asl2.0)))

(define-public go-github-com-kr-text
  (package
    (name "go-github-com-kr-text")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kr/text")
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

(define-public go-golang-org-sql-mock
  (let ((commit "e98392b8111b45f8126e00af035a0dd95dc12e8b")
        (version "1.3.3")
        (revision "1"))
    (package
      (name "go-golang-org-sql-mock")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/DATA-DOG/go-sqlmock")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "033vv29g2wf6fd757ajfmha30bqin3b07377037zkl051mk6mghs"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/DATA-DOG/go-sqlmock"))
      (synopsis "Mock library implementing @code{sql/driver}")
      (description "This library simulates SQL-driver behavior in tests
without requiring a real database connection.")
      (home-page "https://github.com/DATA-DOG/go-sqlmock")
      (license license:expat))))

(define-public go-github-com-go-sql-driver-mysql
  (package
    (name "go-github-com-go-sql-driver-mysql")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-sql-driver/mysql")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11x0m9yf3kdnf6981182r824psgxwfaqhn3x3in4yiidp0w0hk3v"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f ;; tests require a network connection
       #:import-path "github.com/go-sql-driver/mysql"))
    (home-page "https://github.com/go-sql-driver/mysql")
    (synopsis "MySQL driver for golang")
    (description
     "This is a pure Go implementation of the MySQL API, compatible with
golang's database/sql package.")
    (license license:mpl2.0)))

(define-public go-golang-org-colorful
  (package
    (name "go-golang-org-colorful")
    (version "1.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lucasb-eyer/go-colorful")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fig06880bvk1l92j4127v4x9sar4ds7ga8959gxxghb2w70b7l2"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/lucasb-eyer/go-colorful"))
    (native-inputs
     `(("go-golang-org-sql-mock" ,go-golang-org-sql-mock)))
    (synopsis "Convert between colorspaces and generate colors")
    (description "This package implements Go's @code{color.Color} interface
and provides a means of converting colors stored as RGB to various
colorspaces.")
    (home-page "https://github.com/lucasb-eyer/go-colorful")
    (license license:expat)))

(define-public go-github-com-gdamore-encoding
  (package
    (name "go-github-com-gdamore-encoding")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gdamore/encoding")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1vmm5zll92i2fm4ajqx0gyx0p9j36496x5nabi3y0x7h0inv0pk9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gdamore/encoding"))
    (inputs
     `(("go-golang-org-x-text" ,go-golang-org-x-text)))
    (home-page "https://github.com/gdamore/encoding")
    (synopsis "Provide encodings missing from Go")
    (description "This package provides useful encodings not included in the
standard @code{Text} package, including some for dealing with I/O streams from
non-UTF-friendly sources.")
    (license license:expat)))

(define-public go-github-com-gdamore-tcell
  (let ((commit "aaadc574a6ed8dc3abe56036ca130dcee1ee6b6e")
        (version "1.1.2")
        (revision "1"))
    (package
      (name "go-github-com-gdamore-tcell")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gdamore/tcell")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0il2nnxp2cqiy73m49215dnf9in3vd25ji8qxbmq87c5qy7i1q9d"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/gdamore/tcell"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-files-writable
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Make sure .gz files are writable so that the
               ;; 'reset-gzip-timestamps' phase can do its work.
               (let ((out (assoc-ref outputs "out")))
                 (for-each make-file-writable
                           (find-files out "\\.gz$"))
                 #t))))))
      (inputs
       `(("go-github.com-mattn-go-runewidth" ,go-github.com-mattn-go-runewidth)
         ("go-golang-org-colorful" ,go-golang-org-colorful)
         ("go-golang-org-x-text" ,go-golang-org-x-text)
         ("go-github-com-gdamore-encoding" ,go-github-com-gdamore-encoding)))
      (home-page "https://github.com/gdamore/tcell")
      (synopsis "Provide a cell-based view for text terminals")
      (description "This package includes a full parser and expander for
terminfo capability strings to avoid hard-coding escape strings for
formatting.  It also favors portability, and includes support for all POSIX
systems.")
      (license license:expat))))

(define-public go-github-com-mattn-go-shellwords
  (let ((commit "2444a32a19f450fabaa0bb3e96a703f15d9a97d2")
        (version "1.0.5")
        (revision "1"))
    (package
      (name "go-github-com-mattn-go-shellwords")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mattn/go-shellwords")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "08zcgr1az1n8zaxzwdd205j86hczgyc52nxfnw5avpw7rrkf7v0d"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/mattn/go-shellwords"
         ;; TODO: can't make homeless-shelter:
         ;; go: disabling cache (/homeless-shelter/.cache/go-build) due to
         ;; initialization failure: mkdir /homeless-shelter: permission denied

         ;; This doesn't seem to work:

         ;; #:phases
         ;; (modify-phases %standard-phases
         ;;   (replace 'check
         ;;     (lambda* (#:key import-path #:allow-other-keys)
         ;;       (setenv "HOME" "/tmp")
         ;;       (invoke "go" "test" import-path))))

         ;; TODO: There are also a couple of tests that have stymied Debian in
         ;; the past.  They seem to work when run locally.

         #:tests? #f
         ))
      (home-page "https://github.com/mattn/go-shellwords")
      (synopsis "Parse lines into shell words")
      (description "This package parses text into shell arguments.  Based on
the @code{cpan} module @code{Parse::CommandLine}.")
      (license license:expat))))

(define-public go-github-com-burntsushi-locker
  (let ((commit "a6e239ea1c69bff1cfdb20c4b73dadf52f784b6a")
        (revision "0"))
    (package
      (name "go-github-com-burntsushi-locker")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/BurntSushi/locker")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1xak4aync4klswq5217qvw191asgla51jr42y94vp109lirm5dzg"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/BurntSushi/locker"))
      (home-page "https://github.com/BurntSushi/locker")
      (synopsis "Manage named ReadWrite mutexes in Go")
      (description "Golang package for conveniently using named read/write
locks.  These appear to be especially useful for synchronizing access to
session based information in web applications.

The common use case is to use the package level functions, which use a package
level set of locks (safe to use from multiple goroutines
simultaneously).  However, you may also create a new separate set of locks
test.

All locks are implemented with read-write mutexes.  To use them like a regular
mutex, simply ignore the RLock/RUnlock functions.")
      (license license:unlicense))))

(define-public go-github-com-marten-seemann-qtls
  (package
    (name "go-github-com-marten-seemann-qtls")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/marten-seemann/qtls")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dz60y98nm7l70hamq0v2vrs2dspyr5yqhnrds2dfh7hchxvq76j"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/marten-seemann/qtls"
       ;; The test suite requires networking.
       #:tests? #f))
    (propagated-inputs
     `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
    (synopsis "TLS 1.3 with QUIC in Go")
    (description "This package provides @code{qtls}, a QUIC-capable variant of
the Go standard library's TLS 1.3 implementation.")
    (home-page "https://github.com/marten-seemann/qtls")
    (license license:bsd-3)))

(define-public go-github-com-marten-seemann-chacha20
  (package
    (name "go-github-com-marten-seemann-chacha20")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/marten-seemann/chacha20")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x1j4cvbap45zk962qkjalc1h3axhzzdy9cdzhcjmprmm1ql4gjm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/marten-seemann/chacha20"))
    (synopsis "ChaCha20 in Go")
    (description "This package is an external copy of the Go standard library's
internal ChaCha20 package.")
    (home-page "https://github.com/marten-seemann/chacha20")
    (license license:bsd-3)))

(define-public go-github-com-cheekybits-genny
  (package
    (name "go-github-com-cheekybits-genny")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cheekybits/genny")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pcir5ic86713aqa51581rfb67rgc3m0c72ddjfcp3yakv9vyq87"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/cheekybits/genny"))
    (propagated-inputs
     `(("go-golang-org-x-tools" ,go-golang-org-x-tools)))
    (synopsis "Generics for Go")
    (description "This package provides @code{genny}, a Go language
implementation of generics.")
    (home-page "https://github.com/cheekybits/genny/")
    (license license:expat)))

(define-public go-github-com-lucas-clemente-quic-go
  (package
    (name "go-github-com-lucas-clemente-quic-go")
    (version "0.14.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/lucas-clemente/quic-go")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04l3gqbc3gh079n8vgnrsf8ypgv8sl63xjf28jqfrb45v2l73vyz"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/lucas-clemente/quic-go"
       ;; XXX More packages required...
       #:tests? #f))
    (propagated-inputs
     `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-github-com-cheekybits-genny" ,go-github-com-cheekybits-genny)
       ("go-github-com-marten-seemann-chacha20" ,go-github-com-marten-seemann-chacha20)
       ("go-github-com-marten-seemann-qtls" ,go-github-com-marten-seemann-qtls)
       ("go-github-com-golang-protobuf-proto" ,go-github-com-golang-protobuf-proto)))
    (synopsis "QUIC in Go")
    (description "This package provides a Go language implementation of the QUIC
network protocol.")
    (home-page "https://github.com/lucas-clemente/quic-go")
    (license license:expat)))

(define-public go-github-com-francoispqt-gojay
  (package
    (name "go-github-com-francoispqt-gojay")
    (version "1.2.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/francoispqt/gojay")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ix95qdyajfmxhf9y52vjrih63f181pjs4v5as8905s4d5vmkd06"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/francoispqt/gojay"))
    (propagated-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (synopsis "JSON encoder/decoder with powerful stream API for Golang")
    (description "GoJay is a performant JSON encoder/decoder for Golang.  It has
a simple API and doesn't use reflection.  It relies on small interfaces to
decode/encode structures and slices.")
    (home-page "https://github.com/francoispqt/gojay")
    (license license:expat)))

(define-public go-github-com-pkg-errors
  (package
    (name "go-github-com-pkg-errors")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pkg/errors")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1761pybhc2kqr6v5fm8faj08x9bql8427yqg6vnfv6nhrasx1mwq"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/pkg/errors"))
    (synopsis "Go error handling primitives")
    (description "This package provides @code{error}, which offers simple
error handling primitives in Go.")
    (home-page "https://github.com/pkg/errors")
    (license license:bsd-2)))

(define-public go-github-com-maruel-panicparse
  (package
    (name "go-github-com-maruel-panicparse")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/maruel/panicparse")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13qkn7f64yln8jdmma37h6ra4c7anxkp3vfgvfyb6lb07dpr1ibq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/maruel/panicparse"))
    (synopsis "Toolkit for parsing Go stack traces")
    (description "This package provides a toolkit for parsing Go language panic
stack traces.  It simplifies the traces to make salient information more visible
and aid debugging.")
    (home-page "https://github.com/maruel/panicparse")
    (license license:asl2.0)))

(define-public go-github-com-robfig-cron
  (package
    (name "go-github-com-robfig-cron")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/robfig/cron")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1agzbw2dfk2d1mpmddr85s5vh6ygm8kqrvfg87i9d2wqnlsnliqm"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/robfig/cron"))
    (home-page "https://godoc.org/github.com/robfig/cron")
    (synopsis "Cron library for Go")
    (description "This package provides a cron library for Go.  It implements
a cron spec parser and job runner.")
    (license license:expat)))

(define-public go-github-com-shirou-gopsutil
  (let ((commit "47ef3260b6bf6ead847e7c8fc4101b33c365e399")
        (revision "0"))
    (package
      (name "go-github-com-shirou-gopsutil")
      (version (git-version "v2.19.7" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/shirou/gopsutil")
                       (commit commit))) ; XXX
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0x1g4r32q4201nr2b754xnrrndmwsrhfr7zg37spya86qrmijnws"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/shirou/gopsutil"))
      (synopsis "Process and system monitoring in Go")
      (description "This package provides a library for retrieving information
on running processes and system utilization (CPU, memory, disks, network,
sensors).")
      (home-page "https://github.com/shirou/gopsutil")
      (license license:bsd-3))))

(define-public go-github-com-danwakefield-fnmatch
  (let ((commit "cbb64ac3d964b81592e64f957ad53df015803288")
        (revision "0"))
    (package
     (name "go-github-com-danwakefield-fnmatch")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/danwakefield/fnmatch")
             (commit commit)))
       (sha256
        (base32
         "0cbf511ppsa6hf59mdl7nbyn2b2n71y0bpkzbmfkdqjhanqh1lqz"))
       (file-name (git-file-name name version))))
     (build-system go-build-system)
     (arguments
      '(#:import-path "github.com/danwakefield/fnmatch"))
     (home-page "https://github.com/danwakefield/fnmatch")
     (synopsis "Updated clone of kballards golang fnmatch gist")
     (description "This package provides an updated clone of kballards golang
fnmatch gist (https://gist.github.com/kballard/272720).")
     (license license:bsd-2))))

(define-public go-github-com-ddevault-go-libvterm
  (let ((commit "b7d861da381071e5d3701e428528d1bfe276e78f")
        (revision "0"))
    (package
      (name "go-github-com-ddevault-go-libvterm")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/ddevault/go-libvterm")
                (commit commit)))
          (sha256
           (base32
            "06vv4pgx0i6hjdjcar4ch18hp9g6q6687mbgkvs8ymmbacyhp7s6"))
          (file-name (git-file-name name version))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/ddevault/go-libvterm"))
      (propagated-inputs
       `(("go-github-com-mattn-go-pointer" ,go-github-com-mattn-go-pointer)))
      (home-page "https://github.com/ddevault/go-libvterm")
      (synopsis "Go binding to libvterm")
      (description
       "This is a fork of another go-libvterm library for use with aerc.")
      (license license:expat))))

(define-public go-github-com-emersion-go-imap
  (package
    (name "go-github-com-emersion-go-imap")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/emersion/go-imap")
              (commit (string-append "v" version))))
        (sha256
         (base32
          "1id8j2d0rn9sj8y62xhyygqpk5ygrcl9jlfx92sm1jsvxsm3kywq"))
        (file-name (git-file-name name version))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/emersion/go-imap"))
    (native-inputs
     `(("go-golang-org-x-text" ,go-golang-org-x-text)))
    (home-page "https://github.com/emersion/go-imap")
    (synopsis "IMAP4rev1 library written in Go")
    (description "This package provides an IMAP4rev1 library written in Go.  It
can be used to build a client and/or a server.")
    (license license:expat)))

(define-public go-github-com-emersion-go-sasl
  (let ((commit "240c8404624e076f633766c16adbe96c7ac516b7")
        (revision "0"))
    (package
      (name "go-github-com-emersion-go-sasl")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/emersion/go-sasl")
                (commit commit)))
          (sha256
           (base32
            "1py18p3clp474xhx6ypyp0bgv6n1dfm24m95cyyqb0k3vibar6ih"))
          (file-name (git-file-name name version))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/emersion/go-sasl"))
      (home-page "https://github.com/emersion/go-sasl")
      (synopsis "SASL library written in Go")
      (description "This package provides a SASL library written in Go.")
      (license license:expat))))

(define-public go-github-com-emersion-go-imap-idle
  (let ((commit "2704abd7050ed7f2143753554ee23affdf847bd9")
        (revision "0"))
    (package
      (name "go-github-com-emersion-go-imap-idle")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/emersion/go-imap-idle")
                (commit commit)))
          (sha256
           (base32
            "0blwcadmxgqsdwgr9m4jqfbpfa2viw5ah19xbybpa1z1z4aj5cbc"))
          (file-name (git-file-name name version))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/emersion/go-imap-idle"))
      (native-inputs
       `(("go-github-com-emersion-go-imap" ,go-github-com-emersion-go-imap)
         ("go-github-com-emersion-go-sasl" ,go-github-com-emersion-go-sasl)
         ("go-golang-org-x-text" ,go-golang-org-x-text)))
      (home-page "https://github.com/emersion/go-imap-idle")
      (synopsis "IDLE extension for go-imap")
      (description "This package provides an IDLE extension for go-imap.")
      (license license:expat))))

(define-public go-github-com-fatih-color
  (package
    (name "go-github-com-fatih-color")
    (version "1.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/fatih/color")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zc0zlilf03h121f9jqq3ar0hfm7706547zysxp2qxbm920pz7h0"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/fatih/color"))
    (synopsis "Print colored text in Go")
    (description "This package provides an ANSI color package to output
colorized or SGR defined output to the standard output.")
    (home-page "https://godoc.org/github.com/fatih/color")
    (license license:expat)))

(define-public go-github-com-google-go-cmp-cmp
  (package
    (name "go-github-com-google-go-cmp-cmp")
    (version "0.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/google/go-cmp")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qchy411jm9q2l9mf7x3ry2ycaqp9xdhf2nx14qrpzcxfigv2705"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/go-cmp/cmp"
       #:unpack-path "github.com/google/go-cmp"))
    (propagated-inputs
     `(("go-golang-org-x-xerrors" ,go-golang-org-x-xerrors)))
    (synopsis "Determine equality of values in Go")
    (description "This package provides a more powerful and safer
alternative to @code{reflect.DeepEqual} for comparing whether two values
are semantically equal in Go (for writing tests).")
    (home-page "https://godoc.org/github.com/google/go-cmp/cmp")
    (license license:asl2.0)))

(define-public go-github-com-google-uuid
  (package
    (name "go-github-com-google-uuid")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/uuid")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hfxcf9frkb57k6q0rdkrmnfs78ms21r1qfk9fhlqga2yh5xg8zb"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/uuid"))
    (home-page "https://github.com/google/uuid/")
    (synopsis "Generate and inspect UUIDs based on RFC 4122 and DCE 1.1")
    (description "The uuid package generates and inspects UUIDs based on RFC
4122 and DCE 1.1: Authentication and Security Services.")
    (license license:bsd-3)))

(define-public go-github-com-google-goterm
  (let ((commit "fc88cf888a3fa99ecc23d1efc1a44284268457d3")
        (revision "1"))
    (package
      (name "go-github-com-google-goterm")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/google/goterm")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0809sf02dhg2bjhsz43pmlb5d7nbsnwxls3lw01zw5p7ri9bqwfb"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/google/goterm/term"
         #:unpack-path "github.com/google/goterm"))
      (home-page "https://github.com/google/goterm/")
      (synopsis "PTY creation and termios get/set attributes")
      (description "The term package implements PTY creation and termios get/set
attributes.  It also contains some convenience functions for colors, SSH to
and from termios translations, readCh, reading passwords, etc.")
      (license license:bsd-3))))

(define-public go-github-com-google-go-querystring
  (let ((commit "992e8021cf787c100d55520d5c906e01536c0a19") ;fix format in tests
        (revision "1"))
    (package
      (name "go-github-com-google-go-querystring")
      (version "1.0.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/google/go-querystring")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0mbx4jvf7nz4sk2fgqfq1llz4xb3vc4625b4x398mspr3a5077rs"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/google/go-querystring/query"
         #:unpack-path "github.com/google/go-querystring"))
      (home-page "https://github.com/google/go-querystring/")
      (synopsis "Library for encoding structs into URL query parameters")
      (description "@code{go-querystring} is Go library for encoding structs
into URL query parameters.")
      (license license:bsd-3))))

(define-public go-github-com-google-go-github
  (package
    (name "go-github-com-google-go-github")
    (version "26.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/go-github")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x0zz1vcmllp6r6l2qin9b2llm5cxbf6n84rf99h8wrmhvzs2ipi"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f ;application/octet-stream instead of text/plain
       #:import-path "github.com/google/go-github/v26/github"
       #:unpack-path "github.com/google/go-github/v26"))
    (native-inputs
     `(("go-github-com-google-go-querystring" ,go-github-com-google-go-querystring)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
    (home-page "https://github.com/google/go-github/")
    (synopsis "Client library for accessing the GitHub API v3")
    (description "@code{go-github} is a Go client library for accessing the
GitHub API v3.")
    (license license:bsd-3)))

(define-public go-github-com-google-renameio
  (package
    (name "go-github-com-google-renameio")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/renameio")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ki2x5a9nrj17sn092d6n4zr29lfg5ydv4xz5cp58z6cw8ip43jx"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/google/renameio"))
    (home-page "https://github.com/google/renameio/")
    (synopsis "Atomically create or replace a file or symbolic link")
    (description "@code{renameio} Go package provides a way to atomically
create or replace a file or symbolic link.")
    (license license:asl2.0)))

(define-public go-golang.org-x-sync-errgroup
  (let ((commit "cd5d95a43a6e21273425c7ae415d3df9ea832eeb")
        (revision "0"))
    (package
      (name "go-golang.org-x-sync-errgroup")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://go.googlesource.com/sync")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1nqkyz2y1qvqcma52ijh02s8aiqmkfb95j08f6zcjhbga3ds6hds"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/sync/errgroup"
         #:unpack-path "golang.org/x/sync"))
      (synopsis "Synchronization, error propagation, and Context cancellation
for groups of goroutines working on subtasks of a common task.")
      (description "This package provides synchronization, error propagation,
and Context cancellation for groups of goroutines working on subtasks of a
common task.")
      (home-page "https://godoc.org/golang.org/x/sync/errgroup")
      (license license:bsd-3))))

(define (go-gotest-tools-source version sha256-base32-hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/gotestyourself/gotest.tools")
          (commit (string-append "v" version))))
    (file-name (git-file-name "go-gotest-tools" version))
    (sha256
     (base32 sha256-base32-hash))))

;; Note that version 3.0.0 is incompatible to 2.3.0.
;; See also <https://github.com/gotestyourself/gotest.tools/issues/166>.
(define (go-gotest-tools-package suffix)
  (package
    (name (string-append "go-gotest-tools-"
                         (string-replace-substring suffix "/" "-")))
    (version "2.3.0")
    (source
     (go-gotest-tools-source version
      "0071rjxp4xzcr3vprkaj1hdk35a3v45bx8v0ipk16wwc5hx84i2i"))
    (build-system go-build-system)
    (arguments
     `(#:import-path ,(string-append "gotest.tools/" suffix)
       #:unpack-path "gotest.tools"))
    (synopsis "@code{gotest-tools} part")
    (description "This package provides a part of @code{gotest-tools}.")
    (home-page "https://github.com/gotestyourself/gotest.tools")
    (license license:asl2.0)))

(define-public go-gotest-tools-internal-format
  (package (inherit (go-gotest-tools-package "internal/format"))
    (native-inputs
     `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-google-go-cmp-cmp"
        ,go-github-com-google-go-cmp-cmp)))
    (synopsis "Formats messages for use with gotest-tools")
    (description "This package provides a way to format messages for use
with gotest-tools.")))

(define-public go-gotest-tools-internal-difflib
  (package (inherit (go-gotest-tools-package "internal/difflib"))
    (synopsis "Differences for use with gotest-tools")
    (description "This package computes differences for use
with gotest-tools.")))

(define-public go-gotest-tools-internal-source
  (package (inherit (go-gotest-tools-package "internal/source"))
    (native-inputs
     `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-google-go-cmp-cmp" ,go-github-com-google-go-cmp-cmp)))
    (synopsis "Source code AST formatters for gotest-tools")
    (description "This package provides source code AST formatters for
gotest-tools.")))

(define-public go-gotest-tools-assert
  (package (inherit (go-gotest-tools-package "assert"))
    (name "go-gotest-tools-assert")
    (arguments
     `(#:tests? #f ; Test failure concerning message formatting (FIXME)
       #:import-path "gotest.tools/assert"
       #:unpack-path "gotest.tools"))
    ;(propagated-inputs
    ; `(("go-gotest-tools-internal-format" ,go-gotest-tools-internal-format)))
    (native-inputs
     `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-google-go-cmp-cmp"
        ,go-github-com-google-go-cmp-cmp)))
    (synopsis "Compare values and fail a test when a comparison fails")
    (description "This package provides a way to compare values and fail a
test when a comparison fails.")
    (home-page "https://github.com/gotestyourself/gotest.tools")
    (license license:asl2.0)))

(define-public gotestsum
  (package
    (name "gotestsum")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/gotestyourself/gotestsum")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y71qr3ss3hgc8c7nmvpwk946xy1jc5d8whsv6y77wb24ncla7n0"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gotest.tools/gotestsum"))
    (native-inputs
     `(("go-github-com-fatih-color" ,go-github-com-fatih-color)
       ("go-golang.org-x-sync-errgroup" ,go-golang.org-x-sync-errgroup)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-sirupsen-logrus"
        ,go-github-com-sirupsen-logrus)
       ("go-github-com-spf13-pflag" ,go-github-com-spf13-pflag)
       ("go-github-com-jonboulle-clockwork"
        ,go-github-com-jonboulle-clockwork)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-gotest-tools-assert" ,go-gotest-tools-assert)
       ("go-github-com-google-go-cmp-cmp"
        ,go-github-com-google-go-cmp-cmp)
       ;; TODO: This would be better as a propagated-input of
       ;; go-gotest-tools-assert, but that does not work for
       ;; some reason.
       ("go-gotest-tools-internal-format"
        ,go-gotest-tools-internal-format)
       ("go-gotest-tools-internal-difflib"
        ,go-gotest-tools-internal-difflib)
       ("go-gotest-tools-internal-source"
        ,go-gotest-tools-internal-source)
       ("go-github-com-google-go-cmp-cmp"
        ,go-github-com-google-go-cmp-cmp)))
    (synopsis "Go test runner with output optimized for humans")
    (description "This package provides a @code{go test} runner with output
optimized for humans, JUnit XML for CI integration, and a summary of the
test results.")
    (home-page "https://github.com/gotestyourself/gotestsum")
    (license license:asl2.0)))

(define-public go-github-com-golang-protobuf-proto
  (package
    (name "go-github-com-golang-protobuf-proto")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/golang/protobuf")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15am4s4646qy6iv0g3kkqq52rzykqjhm4bf08dk0fy2r58knpsyl"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/golang/protobuf/proto"
       #:unpack-path "github.com/golang/protobuf"
       ;; Requires unpackaged golang.org/x/sync/errgroup
       #:tests? #f))
    (synopsis "Go support for Protocol Buffers")
    (description "This package provides Go support for the Protocol Buffers
data serialization format.")
    (home-page "https://github.com/golang/protobuf")
    (license license:bsd-3)))

(define-public go-github-com-mattn-go-zglob
  (package
    (name "go-github-com-mattn-go-zglob")
    (version "0.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mattn/go-zglob")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1923lvakm66mzy62jmngdvcmbmiqclinsvnghs3907rgygnx1qc1"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mattn/go-zglob"))
    (home-page "https://github.com/mattn/go-zglob")
    (synopsis "Glob library that descends into other directories")
    (description " A glob library that implements descending into other
directories.  It is optimized for filewalking. ")
    (license license:expat)))

(define-public go-github-com-mattn-go-sqlite3
  (package
    (name "go-github-com-mattn-go-sqlite3")
    (version "1.14.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mattn/go-sqlite3")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04anvqkc37mmc3z1dy4xfa6cas67zlxnnab0ywii7sylk864mhxz"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mattn/go-sqlite3"))
    (home-page "https://github.com/mattn/go-sqlite3")
    (synopsis "Sqlite3 driver for Go")
    (description "This package provides a Sqlite3 driver for Go using
@code{database/sql}.")
    (license license:expat)))

(define-public go-github-com-willf-bitset
  (package
    (name "go-github-com-willf-bitset")
    (version "1.1.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/willf/bitset")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wpaxg6va3qwd0hq0b8rpb1hswvzzbfm2h8sjmcsdpbkydjjx9zg"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/willf/bitset"))
    (synopsis "Bitsets in Go")
    (description "This package provides a Go implementation of bitsets, which
are a mapping between non-negative integers and boolean values focused on
efficient space usage.")
    (home-page "https://github.com/willf/bitset")
    (license license:bsd-3)))

(define-public go-github-com-willf-bloom
  (package
    (name "go-github-com-willf-bloom")
    (version "2.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/willf/bloom")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ygan8pgcay7wx3cs3ja8rdqj7nly7v3and97ddcc66020jxchzg"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/willf/bloom"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-import-path
           (lambda _
             ;; See 'go.mod' in the source distribution of Syncthing 1.5.0 for
             ;; more information.
             ;; <https://github.com/spaolacci/murmur3/issues/29>
             (substitute* "src/github.com/willf/bloom/bloom.go"
               (("spaolacci") "twmb"))
             #t)))))
    (propagated-inputs
     `(("go-github-com-twmb-murmur3" ,go-github-com-twmb-murmur3)
       ("go-github-com-willf-bitset" ,go-github-com-willf-bitset)))
    (synopsis "Bloom filters in Go")
    (description "This package provides a Go implementation of bloom filters,
based on murmurhash.")
    (home-page "https://github.com/willf/bloom")
    (license license:bsd-2)))

(define-public go-golang-org-rainycape-unidecode
  (let ((commit "cb7f23ec59bec0d61b19c56cd88cee3d0cc1870c")
        (revision "1"))
    (package
      (name "go-golang-org-rainycape-unidecode")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/rainycape/unidecode")
                       (commit commit)))
                (file-name (string-append "go-golang-org-rainycape-unidecode-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1wvzdijd640blwkgmw6h09frkfa04kcpdq87n2zh2ymj1dzla5v5"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/rainycape/unidecode"))
      (home-page "https://github.com/rainycape/unidecode")
      (synopsis "Unicode transliterator in Golang")
      (description "Unicode transliterator in Golang - Replaces non-ASCII
characters with their ASCII approximations.")
      (license license:asl2.0))))

(define-public go-github-com-golang-freetype
  (let ((commit "e2365dfdc4a05e4b8299a783240d4a7d5a65d4e4")
        (revision "1"))
    (package
      (name "go-github-com-golang-freetype")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/golang/freetype")
                       (commit commit)))
                (file-name (string-append "go-github-com-golang-freetype-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "194w3djc6fv1rgcjqds085b9fq074panc5vw582bcb8dbfzsrqxc"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/golang/freetype"))
      (propagated-inputs
       `(("go-golang-org-x-image" ,go-golang-org-x-image)))
      (home-page "https://github.com/golang/freetype")
      (synopsis "Freetype font rasterizer in the Go programming language")
      (description "The Freetype font rasterizer in the Go programming language.")
      (license (list license:freetype
                     license:gpl2+)))))

(define-public go-github-com-fogleman-gg
  (package
    (name "go-github-com-fogleman-gg")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/fogleman/gg")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nkldjghbqnzj2djfaxhiv35kk341xhcrj9m2dwq65v684iqkk8n"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f      ; Issue with test flags.
       #:import-path "github.com/fogleman/gg"))
    (propagated-inputs
     `(("go-github-com-golang-freetype" ,go-github-com-golang-freetype)))
    (home-page "https://github.com/fogleman/gg")
    (synopsis "2D rendering in Go")
    (description "@code{gg} is a library for rendering 2D graphics in pure Go.")
    (license license:expat)))

(define-public go-github-com-gedex-inflector
  (let ((commit "16278e9db8130ac7ec405dc174cfb94344f16325")
        (revision "1"))
    (package
      (name "go-github-com-gedex-inflector")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/gedex/inflector")
                       (commit commit)))
                (file-name (string-append "go-github-com-gedex-inflector-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "05hjqw1m71vww4914d9h6nqa9jw3lgjzwsy7qaffl02s2lh1amks"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/gedex/inflector"))
      (home-page "https://github.com/gedex/inflector")
      (synopsis "Go library that pluralizes and singularizes English nouns")
      (description "Go library that pluralizes and singularizes English nouns.")
      (license license:bsd-2))))

(define-public go-github-com-klauspost-cpuid
  (package
    (name "go-github-com-klauspost-cpuid")
    (version "1.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/klauspost/cpuid")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1s510210wdj5dkamii1qrk7v87k4qpdcrrjzflp5ha9iscw6b06l"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/klauspost/cpuid"))
    (home-page "https://github.com/klauspost/cpuid")
    (synopsis "CPU feature identification for Go")
    (description "@code{cpuid} provides information about the CPU running the
current program.  CPU features are detected on startup, and kept for fast access
through the life of the application.  Currently x86 / x64 (AMD64) is supported,
and no external C (cgo) code is used, which should make the library very eas
to use.")
    (license license:expat)))

(define-public go-github-com-pbnjay-memory
  (let ((commit "974d429e7ae40c89e7dcd41cfcc22a0bfbe42510")
        (revision "1"))
    (package
      (name "go-github-com-pbnjay-memory")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/pbnjay/memory")
                       (commit commit)))
                (file-name (string-append "go-github-com-pbnjay-memory-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0kazg5psdn90pqadrzma5chdwh0l2by9z31sspr47gx93fhjmkkq"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/pbnjay/memory"))
      (home-page "https://github.com/gedex/inflector")
      (synopsis "Go library to report total system memory")
      (description "@code{memory} provides a single method reporting total
physical system memory accessible to the kernel.  It does not account for memory
used by other processes.")
      (license license:bsd-3))))

(define-public go-github-com-surge-glog
  (let ((commit "2578deb2b95c665e6b1ebabf304ce2085c9e1985")
        (revision "1"))
    (package
      (name "go-github-com-surge-glog")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/surge/glog")
                       (commit commit)))
                (file-name (string-append "go-github-com-surge-glog-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1bxcwxvsvr2hfpjz9hrrn0wrgykwmrbyk567102k3vafw9xdcwk4"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/surge/glog"))
      (home-page "https://github.com/surge/glog")
      (synopsis "Leveled execution logs for Go")
      (description "Leveled execution logs for Go.")
      (license license:asl2.0))))

(define-public go-github-com-surgebase-porter2
  (let ((commit "56e4718818e8dc4ea5ba6348402fc7661863732a")
        (revision "1"))
    (package
      (name "go-github-com-surgebase-porter2")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/surgebase/porter2")
                       (commit commit)))
                (file-name (string-append "go-github-com-surgebase-porter2-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1ivcf83jlj9s7q5y9dfbpyl0br35cz8fcp0dm8sxxvqh54py06v2"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/surgebase/porter2"))
      (native-inputs
       `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
         ("go-github-com-surge-glog" ,go-github-com-surge-glog)))
      (home-page "https://github.com/surgebase/porter2")
      (synopsis "Go library implementing english Porter2 stemmer")
      (description "Porter2 implements the
@url{http://snowball.tartarus.org/algorithms/english/stemmer.html, english
Porter2 stemmer}.  It is written completely using finite state machines to do
suffix comparison, rather than the string-based or tree-based approaches.")
      (license license:asl2.0))))

(define-public go-github-com-masterminds-goutils
  (package
    (name "go-github-com-masterminds-goutils")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Masterminds/goutils")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "180px47gj936qyk5bkv5mbbgiil9abdjq6kwkf7sq70vyi9mcfiq"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/Masterminds/goutils"))
    (home-page "https://github.com/Masterminds/goutils/")
    (synopsis "Utility functions to manipulate strings")
    (description "GoUtils provides utility functions to manipulate strings in
various ways.  It is a Go implementation of some string manipulation libraries
of Java Apache Commons.")
    (license license:asl2.0)))

(define-public go-github-com-masterminds-semver
  (package
    (name "go-github-com-masterminds-semver")
    (version "3.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Masterminds/semver")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g1wizfdy29d02l9dh8gsb029yr4m4swp13swf0pnh9ryh5f1msz"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/Masterminds/semver"))
    (home-page "https://github.com/Masterminds/semver/")
    (synopsis "@code{semver} helps to work with semantic versions")
    (description "The semver package provides the ability to work with
semantic versions.  Specifically it provides the ability to:
@itemize
@item Parse semantic versions
@item Sort semantic versions
@item Check if a semantic version fits within a set of constraints
@item Optionally work with a @code{v} prefix
@end itemize\n")
    (license license:expat)))

(define-public go-github-com-huandu-xstrings
  (package
    (name "go-github-com-huandu-xstrings")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/huandu/xstrings")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pwar6rc0fqb6pll38a44s81g5kb65vbg71jg5lx8caphjnikq5r"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/huandu/xstrings"))
    (home-page "https://github.com/huandu/xstrings/")
    (synopsis "Collection of string functions")
    (description "Go package xstrings is a collection of string functions,
which are widely used in other languages but absent in Go package strings.")
    (license license:expat)))

(define-public go-github-com-imdario-mergo
  (package
    (name "go-github-com-imdario-mergo")
    (version "0.3.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/imdario/mergo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09h765p8yby9r8s0a3hv5kl8n2i382mda76wmvk48w1cc1w9s92p"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/imdario/mergo"))
    (native-inputs
     `(("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)))
    (home-page "https://github.com/imdario/mergo/")
    (synopsis "Helper to merge structs and maps in Golang")
    (description "Helper to merge structs and maps in Golang.  Useful for
configuration default values, avoiding messy if-statements.

Mergo merges same-type structs and maps by setting default values in
zero-value fields.  Mergo won't merge unexported (private) fields.  It will do
recursively any exported one.  It also won't merge structs inside
maps (because they are not addressable using Go reflection).")
    (license license:bsd-3)))

(define-public go-github-com-masterminds-sprig
  (package
    (name "go-github-com-masterminds-sprig")
    (version "3.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Masterminds/sprig")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wwi8n2adjc5jlga25lqq0hrz4jcgd5vpll68y2dfji034caaq18"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f ;network tests only
       #:import-path "github.com/Masterminds/sprig"))
    (native-inputs
     `(("go-github-com-masterminds-goutils" ,go-github-com-masterminds-goutils)
       ("go-github-com-masterminds-semver" ,go-github-com-masterminds-semver)
       ("go-github-com-google-uuid" ,go-github-com-google-uuid)
       ("go-github-com-huandu-xstrings" ,go-github-com-huandu-xstrings)
       ("go-github-com-imdario-mergo" ,go-github-com-imdario-mergo)
       ("go-github-com-mitchellh-reflectwalk" ,go-github-com-mitchellh-reflectwalk)
       ("go-github-com-mitchellh-copystructure" ,go-github-com-mitchellh-copystructure)
       ("go-github-com-spf13-cast" ,go-github-com-spf13-cast)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/Masterminds/sprig/")
    (synopsis "Template functions for Go templates")
    (description "Sprig is a library that provides more than 100 commonly used
template functions.")
    (license license:expat)))

(define-public go-github-com-bmatcuk-doublestar
  (package
    (name "go-github-com-bmatcuk-doublestar")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bmatcuk/doublestar")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bk5bixl6rqa8znxghyp6zndbccx9kdyrymjahgyp6qsrp7rk144"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/bmatcuk/doublestar"))
    (home-page "https://github.com/bmatcuk/doublestar/")
    (synopsis "Path pattern matching and globbing supporting doublestar")
    (description "@code{doublestar} is a Go implementation of path pattern
matching and globbing with support for \"doublestar\" patterns.")
    (license license:expat)))

(define-public go-github-com-dlclark-regexp2
  (package
    (name "go-github-com-dlclark-regexp2")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dlclark/regexp2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "011l1prsywvhhi0yc7qmpsca1cwavmawyyld5kjzi0ff9ghvj4ng"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/dlclark/regexp2"))
    (home-page "https://github.com/dlclark/regexp2/")
    (synopsis "Full featured regular expressions for Go")
    (description "Regexp2 is a feature-rich RegExp engine for Go.")
    (license license:expat)))

(define-public go-github-com-alecthomas-colour
  (package
    (name "go-github-com-alecthomas-colour")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alecthomas/colour")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10zbm12j40ppia4b5ql2blmsps5jhh5d7ffphxx843qk7wlbqnjb"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/alecthomas/colour"))
    (native-inputs
     `(("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)))
    (home-page "https://github.com/alecthomas/colour/")
    (synopsis "Colour terminal text for Go")
    (description "Package colour provides Quake-style colour formatting for
Unix terminals.

The package level functions can be used to write to stdout (or strings or
other files).  If stdout is not a terminal, colour formatting will be
stripped.")
    (license license:expat)))

(define-public go-github-com-alecthomas-repr
  (let ((commit "4184120f674c8860a5b48142509a2411a0a1766f")
        (revision "1"))
    (package
      (name "go-github-com-alecthomas-repr")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alecthomas/repr")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1z0gdkjryxg1ps5fh4ybzip27g9lzdldz4hxqp5j7s2frbzaa9s7"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/alecthomas/repr"))
      (native-inputs
       `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
      (home-page "https://github.com/alecthomas/repr/")
      (synopsis "Represent Go values in an almost direct form")
      (description "This package attempts to represent Go values in a form that
can be used almost directly in Go source code.")
      (license license:expat))))

(define-public go-github-com-sergi-go-diff
  (package
    (name "go-github-com-sergi-go-diff")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sergi/go-diff")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ir8ali2vx0j7pipmlfd6k8c973akyy2nmbjrf008fm800zcp7z2"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/sergi/go-diff/diffmatchpatch"
       #:unpack-path "github.com/sergi/go-diff"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/sergi/go-diff/")
    (synopsis "Algorithms to perform operations for synchronizing plain text")
    (description "@code{go-diff} offers algorithms to perform operations required for
synchronizing plain text:
@itemize
@item compare two texts and return their differences
@item perform fuzzy matching of text
@item apply patches onto text
@end itemize\n")
    (license license:expat)))

(define-public go-github-com-alecthomas-assert
  (let ((commit "405dbfeb8e38effee6e723317226e93fff912d06")
        (revision "1"))
    (package
      (name "go-github-com-alecthomas-assert")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alecthomas/assert")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1l567pi17k593nrd1qlbmiq8z9jy3qs60px2a16fdpzjsizwqx8l"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/alecthomas/assert"))
      (native-inputs
       `(("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
         ("go-github-com-alecthomas-colour" ,go-github-com-alecthomas-colour)
         ("go-github-com-alecthomas-repr" ,go-github-com-alecthomas-repr)
         ("go-github-com-sergi-go-diff" ,go-github-com-sergi-go-diff)))
      (home-page "https://github.com/alecthomas/assert/")
      (synopsis "Go assertion library")
      (description "Assertion library that:
@itemize
@item makes spotting differences in equality much easier
@item uses repr and diffmatchpatch to display structural differences in colour
@item aborts tests on first assertion failure
@end itemize\n")
      (license license:expat))))

(define-public go-github-com-alecthomas-chroma
  (package
    (name "go-github-com-alecthomas-chroma")
    (version "0.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alecthomas/chroma")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "066a6rdmf670d3v5sc7chbn7db09ldgxjympb03pcqwk644dixb1"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/alecthomas/chroma"))
    (native-inputs
     `(("go-github-com-dlclark-regexp2" ,go-github-com-dlclark-regexp2)
       ("go-github-com-alecthomas-assert" ,go-github-com-alecthomas-assert)
       ("go-github-com-alecthomas-colour" ,go-github-com-alecthomas-colour)
       ("go-github-com-alecthomas-repr" ,go-github-com-alecthomas-repr)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
       ("go-github-com-sergi-go-diff" ,go-github-com-sergi-go-diff)))
    (home-page "https://github.com/alecthomas/chroma/")
    (synopsis "General purpose syntax highlighter in pure Go")
    (description "Chroma takes source code and other structured text and
converts it into syntax highlighted HTML, ANSI-coloured text, etc.")
    (license license:expat)))

(define-public go-github-com-andybalholm-cascadia
  (package
    (name "go-github-com-andybalholm-cascadia")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/andybalholm/cascadia")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09j8cavbhqqdxjqrkwbc40g8p0i49zf3184rpjm5p2rjbprcghcc"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/andybalholm/cascadia"))
    (native-inputs
     `(("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://github.com/andybalholm/cascadia/")
    (synopsis "CSS selectors for HTML")
    (description "The Cascadia package implements CSS selectors for use with
the parse trees produced by the html package.")
    (license license:bsd-2)))

(define-public go-github-com-puerkitobio-goquery
  (package
    (name "go-github-com-puerkitobio-goquery")
    (version "1.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PuerkitoBio/goquery")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08nf88cg663slzqr51k2jxlm1krnh86nrzwbk6v41ccq5jkfm7fx"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/PuerkitoBio/goquery"))
    (propagated-inputs
     `(("go-github-com-andybalholm-cascadia" ,go-github-com-andybalholm-cascadia)
       ("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://github.com/PuerkitoBio/goquery")
    (synopsis "Features similar to jQuery to the Go language")
    (description "@code{goquery} brings a syntax and a set of features similar
to jQuery to the Go language.")
    (license license:bsd-3)))

(define-public go-github-com-jmespath-go-jmespath
  (package
    (name "go-github-com-jmespath-go-jmespath")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jmespath/go-jmespath")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "18zyr9nlywmwp3wpzcjxrgq9s9d2mmc6zg6xhsna00m663nkyc3n"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jmespath/go-jmespath"))
    (native-inputs
     `(("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)))
    (home-page "https://github.com/jmespath/go-jmespath")
    (synopsis "Golang implementation of JMESPath")
    (description
     "This package implements JMESPath, a query language for JSON.  It
transforms one JSON document into another through a JMESPath expression.")
    (license license:asl2.0)))

(define-public go-github-com-aymerick-douceur
  (package
    (name "go-github-com-aymerick-douceur")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aymerick/douceur/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hfysznib0fqbp8vqxpk0xiggpp0ayk2bsddi36vbg6f8zq5f81n"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/aymerick/douceur"))
    (native-inputs
     `(("go-github-com-puerkitobio-goquery" ,go-github-com-puerkitobio-goquery)
       ("go-github-com-andybalholm-cascadia" ,go-github-com-andybalholm-cascadia)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-github-com-gorilla-css" ,go-github-com-gorilla-css)))
    (home-page "https://github.com/aymerick/douceur/")
    (synopsis "CSS parser and inliner")
    (description "This package provides a CSS parser and inliner.")
    (license license:expat)))

(define-public go-github-com-chris-ramon-douceur
  (package
    (name "go-github-com-chris-ramon-douceur")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/chris-ramon/douceur")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hfysznib0fqbp8vqxpk0xiggpp0ayk2bsddi36vbg6f8zq5f81n"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/chris-ramon/douceur"))
    (native-inputs
     `(("go-github-com-aymerick-douceur" ,go-github-com-aymerick-douceur)
       ("go-github-com-puerkitobio-goquery" ,go-github-com-puerkitobio-goquery)
       ("go-github-com-andybalholm-cascadia" ,go-github-com-andybalholm-cascadia)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-github-com-gorilla-css" ,go-github-com-gorilla-css)))
    (home-page "https://github.com/chris-ramon/douceur/")
    (synopsis "CSS parser and inliner")
    (description "This package provides a CSS parser and inliner.")
    (license license:expat)))

(define-public go-github-com-microcosm-cc-bluemonday
  (package
    (name "go-github-com-microcosm-cc-bluemonday")
    (version "1.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/microcosm-cc/bluemonday")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "071ph097c1iwbcc33x6kblj9rxb1r4mp3qfkrj4qw5mg7qcqxydk"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/microcosm-cc/bluemonday"))
    (native-inputs
     `(("go-github-com-chris-ramon-douceur" ,go-github-com-chris-ramon-douceur)
       ("go-github-com-aymerick-douceur" ,go-github-com-aymerick-douceur)
       ("go-github-com-gorilla-css" ,go-github-com-gorilla-css)
       ("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://github.com/microcosm-cc/bluemonday/")
    (synopsis "HTML sanitizer")
    (description "@code{bluemonday} is a HTML sanitizer implemented in Go.")
    (license license:bsd-3)))

(define-public go-github-com-muesli-reflow-wordwrap
  (package
    (name "go-github-com-muesli-reflow-wordwrap")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/muesli/reflow")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "go-github-com-muesli-reflow" version))
              (sha256
               (base32
                "1vhynm2n1az13fn03lp0gi28p9mznq1mblglh8f2rb9y1vkd2dqr"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/muesli/reflow/wordwrap"
       #:unpack-path "github.com/muesli/reflow"))
    (native-inputs
     `(("go-github.com-mattn-go-runewidth" ,go-github.com-mattn-go-runewidth)))
    (home-page "https://github.com/muesli/reflow/")
    (synopsis "Collection of methods helping to transform blocks of text")
    (description "This package provides a collection of ANSI-aware methods and
io.Writers helping you to transform blocks of text.")
    (license license:expat)))

(define-public go-github-com-muesli-reflow-ansi
  (package
    (inherit go-github-com-muesli-reflow-wordwrap)
    (name "go-github-com-muesli-reflow-ansi")
    (arguments
     `(#:import-path "github.com/muesli/reflow/ansi"
       #:unpack-path "github.com/muesli/reflow"))))

(define-public go-github-com-muesli-reflow-indent
  (package
    (inherit go-github-com-muesli-reflow-wordwrap)
    (name "go-github-com-muesli-reflow-indent")
    (arguments
     `(#:import-path "github.com/muesli/reflow/indent"
       #:unpack-path "github.com/muesli/reflow"))))

(define-public go-github-com-muesli-reflow-padding
  (package
    (inherit go-github-com-muesli-reflow-wordwrap)
    (name "go-github-com-muesli-reflow-padding")
    (arguments
     `(#:import-path "github.com/muesli/reflow/padding"
       #:unpack-path "github.com/muesli/reflow"))))

(define-public go-github-com-muesli-termenv
  (package
    (name "go-github-com-muesli-termenv")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/muesli/termenv")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09fwrdhy7c9qlf70h97f5inh6xvkfq1vi8fwx9q7bwmjjbiykk8m"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/muesli/termenv"))
    (native-inputs
     `(("go-github-com-google-goterm" ,go-github-com-google-goterm)
       ("go-golang-org-colorful" ,go-golang-org-colorful)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
       ("go-github.com-mattn-go-runewidth" ,go-github.com-mattn-go-runewidth)))
    (home-page "https://github.com/muesli/termenv/")
    (synopsis "Advanced styling options on the terminal")
    (description "termenv lets you safely use advanced styling options on the
terminal.  It gathers information about the terminal environment in terms of
its ANSI and color support and offers you convenient methods to colorize and
style your output, without you having to deal with all kinds of weird ANSI
escape sequences and color conversions.")
    (license license:expat)))

(define-public go-github-com-nwidger-jsoncolor
  (package
    (name "go-github-com-nwidger-jsoncolor")
    (version "0.3.0")
    (home-page "https://github.com/nwidger/jsoncolor")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "13rd146pnj7qm70r1333gyd1f61x40nafxlpvdxlci9h7mx8c5p8"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/nwidger/jsoncolor"))
    (native-inputs
     `(("go-github-com-fatih-color" ,go-github-com-fatih-color)))
    (synopsis "Colorized JSON marshalling and encoding")
    (description
     "@code{jsoncolor} is a drop-in replacement for @code{encoding/json}'s
@code{Marshal} and @code{MarshalIndent} functions and @code{Encoder} type
which produce colorized output using github.com/fatih/color.")
    (license license:expat)))

(define-public go-github-com-olekukonko-tablewriter
  (package
    (name "go-github-com-olekukonko-tablewriter")
    (version "0.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/olekukonko/tablewriter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02r0n2b9yh3x8xyf48k17dxlwj234hlgjycylbjxi6qg08hfmz2x"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/olekukonko/tablewriter"))
    (native-inputs
     `(("go-github.com-mattn-go-runewidth" ,go-github.com-mattn-go-runewidth)))
    (home-page "https://github.com/olekukonko/tablewriter/")
    (synopsis "Generate ASCII table")
    (description "This package generates ASCII tables.  Features:
@itemize
@item automatic Padding
@item support Multiple Lines
@item supports Alignment
@item support Custom Separators
@item automatic Alignment of numbers and percentage
@item write directly to http , file etc via @code{io.Writer}
@item read directly from CSV file
@item optional row line via @code{SetRowLine}
@item normalise table header
@item make CSV Headers optional
@item enable or disable table border
@item set custom footer support
@item optional identical cells merging
@item set custom caption
@item optional reflowing of paragrpahs in multi-line cells
@end itemize\n")
    (license license:expat)))

(define-public go-github-com-yuin-goldmark
  (package
    (name "go-github-com-yuin-goldmark")
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/yuin/goldmark")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12rsnsf65drcp0jfw2jl9w589vsn3pxdk1zh3v9q908iigngrcmy"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/yuin/goldmark"))
    (home-page "https://github.com/yuin/goldmark/")
    (synopsis "Markdown parser")
    (description "This package provides a markdown parser.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-glamour
  (package
    (name "go-github-com-charmbracelet-glamour")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/charmbracelet/glamour")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1idq8d13rp1hx2a1xak31fwl9fmi09p2x4ymvzl7aj850saw5w0z"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/charmbracelet/glamour"))
    (native-inputs
     `(("go-github-com-alecthomas-chroma" ,go-github-com-alecthomas-chroma)
       ("go-github-com-danwakefield-fnmatch" ,go-github-com-danwakefield-fnmatch)
       ("go-github-com-dlclark-regexp2" ,go-github-com-dlclark-regexp2)
       ("go-github-com-microcosm-cc-bluemonday" ,go-github-com-microcosm-cc-bluemonday)
       ("go-github-com-chris-ramon-douceur" ,go-github-com-chris-ramon-douceur)
       ("go-github-com-aymerick-douceur" ,go-github-com-aymerick-douceur)
       ("go-github-com-gorilla-css" ,go-github-com-gorilla-css)
       ("go-github-com-muesli-reflow-ansi" ,go-github-com-muesli-reflow-ansi)
       ("go-github-com-muesli-reflow-wordwrap" ,go-github-com-muesli-reflow-wordwrap)
       ("go-github-com-muesli-reflow-indent" ,go-github-com-muesli-reflow-indent)
       ("go-github-com-muesli-reflow-padding" ,go-github-com-muesli-reflow-padding)
       ("go-github.com-mattn-go-runewidth" ,go-github.com-mattn-go-runewidth)
       ("go-github-com-muesli-termenv" ,go-github-com-muesli-termenv)
       ("go-github-com-google-goterm" ,go-github-com-google-goterm)
       ("go-golang-org-colorful" ,go-golang-org-colorful)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
       ("go-github-com-olekukonko-tablewriter" ,go-github-com-olekukonko-tablewriter)
       ("go-github-com-yuin-goldmark" ,go-github-com-yuin-goldmark)
       ("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://github.com/charmbracelet/glamour/")
    (synopsis "Write handsome command-line tools with glamour")
    (description "@code{glamour} lets you render markdown documents and
templates on ANSI compatible terminals.  You can create your own stylesheet or
use one of our glamorous default themes.")
    (license license:expat)))

(define-public go-github-com-coreos-go-semver
  (package
    (name "go-github-com-coreos-go-semver")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/coreos/go-semver")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0770h1mpig2j5sbiha3abnwaw8p6dg9i87r8pc7cf6m4kwml3sc9"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/coreos/go-semver"))
    (home-page "https://github.com/coreos/go-semver/")
    (synopsis "Semantic versioning library")
    (description "@code{go-semver} is a semantic versioning library for Go.
It lets you parse and compare two semantic version strings.")
    (license license:asl2.0)))

(define-public go-github-com-emirpasic-gods
  (package
    (name "go-github-com-emirpasic-gods")
    (version "1.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emirpasic/gods")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i5qqq7ajvw3mikr95zl9rsnfsjanzwpqqs6kzzplsfgsifybar1"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/emirpasic/gods"
       ; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (home-page "https://github.com/emirpasic/gods/")
    (synopsis "Implementation of various data structures and algorithms in Go")
    (description "This package provides implementation of various data
structures and algorithms in Go.")
    (license license:bsd-2)))

(define-public go-gopkg-in-warnings
  (package
    (name "go-gopkg-in-warnings")
    (version "0.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-warnings/warnings")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kzj50jn708cingn7a13c2wdlzs6qv89dr2h4zj8d09647vlnd81"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "gopkg.in/warnings.v0"))
    (home-page "https://gopkg.in/warnings.v0")
    (synopsis "Error handling with non-fatal errors")
    (description "Package warnings implements error handling with non-fatal
errors (warnings).")
    (license license:bsd-2)))

(define-public go-github-com-go-git-gcfg
  (package
    (name "go-github-com-go-git-gcfg")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-git/gcfg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lb14z4j35pwz2b2rbykkpsq515spwbndb00gwn2xlrzn949xb83"))))
    (arguments
     `(#:import-path "github.com/go-git/gcfg"))
    (native-inputs
     `(("go-gopkg-in-warnings" ,go-gopkg-in-warnings)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
    (build-system go-build-system)
    (home-page "https://github.com/go-git/gcfg/")
    (synopsis "Gcfg reads INI-style configuration files into Go structs")
    (description "Gcfg reads INI-style configuration files into Go structs.")
    (license license:bsd-3)))

(define-public go-github-com-go-git-go-billy
  (package
    (name "go-github-com-go-git-go-billy")
    (version "5.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-git/go-billy")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wdzczfk1n50dl2zpgf46m69b0sm8qkan5xyv82pk9x53zm1dmdx"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/go-git/go-billy/v5"))
    (native-inputs
     `(("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)))
    (home-page "https://github.com/go-git/go-billy/")
    (synopsis "File system abstraction for Go")
    (description "Billy implements an interface based on the OS's standard
library to develop applications without depending on the underlying storage.
This makes it virtually free to implement mocks and testing over
file system operations.")
    (license license:asl2.0)))

(define-public go-github-com-jbenet-go-context
  (let ((commit "d14ea06fba99483203c19d92cfcd13ebe73135f4")
        (revision "1"))
    (package
      (name "go-github-com-jbenet-go-context")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jbenet/go-context")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0q91f5549n81w3z5927n4a1mdh220bdmgl42zi3h992dcc4ls0sl"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/jbenet/go-context"
         ; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'build))))
      (home-page "https://github.com/jbenet/go-context/")
      (synopsis "@code{jbenet's} context extensions")
      (description "This package provides @code{jbenet's} context
extensions.")
      (license license:expat))))

(define-public go-github-com-kevinburke-ssh-config
  (package
    (name "go-github-com-kevinburke-ssh-config")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kevinburke/ssh_config")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05jvz5r58a057zxvic9dyr9v2wilha8l6366npwkqgxmnmk9sh5f"))))
    (arguments
     `(#:import-path "github.com/kevinburke/ssh_config"))
    (build-system go-build-system)
    (home-page "https://github.com/kevinburke/ssh_config/")
    (synopsis "Parser for @file{ssh_config} files")
    (description "This is a Go parser for @file{ssh_config} files.
Importantly, this parser attempts to preserve comments in a given file, so you
can manipulate a @file{ssh_config} file from a program.")
    (license license:expat)))

(define-public go-github-com-xanzy-ssh-agent
  (package
    (name "go-github-com-xanzy-ssh-agent")
    (version "0.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xanzy/ssh-agent")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1chjlnv5d6svpymxgsr62d992m2xi6jb5lybjc5zn1h3hv1m01av"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/xanzy/ssh-agent"))
    (native-inputs
     `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
    (home-page "https://github.com/xanzy/ssh-agent/")
    (synopsis "Control ssh-agent from Go")
    (description "Package agent implements the ssh-agent protocol, and
provides both a client and a server.  The client can talk to a standard
ssh-agent that uses UNIX sockets, and one could implement an alternative
ssh-agent process using the sample server. ")
    (license license:asl2.0)))

(define-public go-github-com-alcortesm-tgz
  (let ((commit "9c5fe88206d7765837fed3732a42ef88fc51f1a1")
        (revision "1"))
    (package
      (name "go-github-com-alcortesm-tgz")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alcortesm/tgz")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "04dcwnz2c2i4wbq2vx3g2wrdgqpncr2r1h6p1k08rdwk4bq1h8c5"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (substitute* "tgz_test.go"
                      ;; Fix format error
                      (("t.Fatalf\\(\"%s: unexpected error extracting: %s\", err\\)")
                       "t.Fatalf(\"%s: unexpected error extracting: %s\", com, err)"))
                    #t))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/alcortesm/tgz"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'make-git-checkout-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (for-each make-file-writable (find-files "."))
               (for-each make-file-writable (find-files (assoc-ref outputs "out")))
               #t)))))
      (home-page "https://github.com/alcortesm/tgz/")
      (synopsis "Go library to extract tgz files to temporal directories")
      (description "This package provides a Go library to extract tgz files to
temporal directories.")
      (license license:expat))))

(define-public go-github-com-go-git-go-git-fixtures
  (package
    (name "go-github-com-go-git-go-git-fixtures")
    (version "4.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-git/go-git-fixtures")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "002yb1s2mxq2xijkl39ip1iyc3l52k23ikyi9ijfl4bgqxy79ljg"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/go-git/go-git-fixtures/v4"
       #:phases
       (modify-phases %standard-phases
         (delete 'reset-gzip-timestamps))))
    (native-inputs
     `(("go-github-com-alcortesm-tgz" ,go-github-com-alcortesm-tgz)
       ("go-github-com-go-git-go-billy" ,go-github-com-go-git-go-billy)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)))
    (home-page "https://github.com/go-git/go-git-fixtures/")
    (synopsis "Fixtures used by @code{go-git}")
    (description "This package provides fixtures used by @code{go-git}.")
    (license license:asl2.0)))

(define-public go-github-com-pkg-diff
  (let ((commit "531926345625d489a6b56f860a569e68245ace36")
        (revision "1"))
    (package
      (name "go-github-com-pkg-diff")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pkg/diff")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1770m7qhww6lm0wj1v3mhv6hwa2v92p4w2fqxj1xyrg5dd58d944"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/pkg/diff"))
      (native-inputs
       `(("go-github-com-sergi-go-diff" ,go-github-com-sergi-go-diff)))
      (home-page "https://github.com/pkg/diff/")
      (synopsis "Create and print diffs")
      (description
       "This package provides a Go library to create and print diffs.")
      (license license:bsd-3))))

(define-public go-github-com-twpayne-go-shell
  (package
    (name "go-github-com-twpayne-go-shell")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/twpayne/go-shell")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hv0ggy3935iddjnmpp9vl0kqjknxpnbmm9w7xr3gds7fpbxz6yp"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/twpayne/go-shell"))
    (home-page "https://github.com/twpayne/go-shell/")
    (synopsis "Shell across multiple platforms")
    (description
     "Package @code{shell} returns a user's shell across multiple platforms.")
    (license license:expat)))

(define-public go-github-com-twpayne-go-vfs
  (package
    (name "go-github-com-twpayne-go-vfs")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/twpayne/go-vfs")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19dm3gi45znwaqbzxhwcgkiz8059bwa3ank80hc6qhdl579bpjnz"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/twpayne/go-vfs"))
    (native-inputs
     `(("go-github-com-bmatcuk-doublestar" ,go-github-com-bmatcuk-doublestar)))
    (home-page "https://github.com/twpayne/go-vfs/")
    (synopsis "Abstraction of the @code{os} and @code{ioutil} Go packages")
    (description "Package @code{vfs} provides an abstraction of the @code{os}
and @code{ioutil} packages that is easy to test.")
    (license license:expat)))

(define-public go-github-com-twpayne-go-vfsafero
  (package
    (name "go-github-com-twpayne-go-vfsafero")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/twpayne/go-vfsafero")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18jwxhlrjd06z8xzg9ij0irl4f79jfy5jpwiz6xqlhzb1fja19pw"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/twpayne/go-vfsafero"))
    (native-inputs
     `(("go-github-com-twpayne-go-vfs" ,go-github-com-twpayne-go-vfs)
       ("go-github-com-spf13-afero" ,go-github-com-spf13-afero)))
    (home-page "https://github.com/twpayne/go-vfsafero/")
    (synopsis "Compatibility later between @code{go-vfs} and @code{afero}")
    (description
     "Package @code{vfsafero} provides a compatibility later between
@code{go-github-com-twpayne-go-vfs} and @code{go-github-com-spf13-afero}.")
    (license license:expat)))

(define-public go-github-com-twpayne-go-xdg
  (package
    (name "go-github-com-twpayne-go-xdg")
    (version "3.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/twpayne/go-xdg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j8q7yzixs6jlaad0lpa8hs6b240gm2cmy0yxgnprrbpa0y2r7ln"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/twpayne/go-xdg/v3"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ("go-github-com-twpayne-go-vfs" ,go-github-com-twpayne-go-vfs)))
    (home-page "https://github.com/twpayne/go-xdg/")
    (synopsis "Functions related to freedesktop.org")
    (description "Package @code{xdg} provides functions related to
@uref{freedesktop.org}.")
    (license license:expat)))

(define-public go-github-com-godbus-dbus
  (package
    (name "go-github-com-godbus-dbus")
    (version "5.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/godbus/dbus")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bkc904073k807yxg6mvqaxrr6ammmhginr9p54jfb55mz3hfw3s"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f ;no /var/run/dbus/system_bus_socket
       #:import-path "github.com/godbus/dbus"))
    (native-inputs
     `(("dbus" ,dbus))) ;dbus-launch
    (home-page "https://github.com/godbus/dbus/")
    (synopsis "Native Go client bindings for the D-Bus")
    (description "@code{dbus} is a library that implements native Go client
bindings for the D-Bus message bus system.")
    (license license:bsd-2)))

(define-public go-github-com-zalando-go-keyring
  (package
    (name "go-github-com-zalando-go-keyring")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zalando/go-keyring")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kj54nkiyccy6m9iy9a53f6412a54xk96j88jaiq35yzdgfa4z3p"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f ;XXX: Fix dbus tests
       #:import-path "github.com/zalando/go-keyring"))
    (native-inputs
     `(("go-github-com-godbus-dbus" ,go-github-com-godbus-dbus)
       ("dbus" ,dbus)))
    (home-page "https://github.com/zalando/go-keyring/")
    (synopsis "Library for working with system keyring")
    (description "@code{go-keyring} is a library for setting, getting and
deleting secrets from the system keyring.")
    (license license:expat)))

(define-public go-etcd-io-bbolt
  (package
    (name "go-etcd-io-bbolt")
    (version "1.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/etcd-io/bbolt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h64gipvcg7060byv5wjlf524kqwj12p3v08kfh4ygv46vpm8p2r"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "go.etcd.io/bbolt"))
    (home-page "https://pkg.go.dev/go.etcd.io/bbolt/")
    (synopsis "Low-level key/value store in Go")
    (description "This package implements a low-level key/value store in Go.")
    (license license:expat)))

(define-public go-github-com-rogpeppe-go-internal
  (package
    (name "go-github-com-rogpeppe-go-internal")
    (version "1.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rogpeppe/go-internal")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00j2vpp1bsggdvw1winkz23mg0q6drjiir5q0k49pmqx1sh7106l"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/rogpeppe/go-internal"
       ; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (home-page "https://github.com/rogpeppe/go-internal/")
    (synopsis "Internal packages from the Go standard library")
    (description "This repository factors out an opinionated selection of
internal packages and functionality from the Go standard library.  Currently
this consists mostly of packages and testing code from within the Go tool
implementation.

Included are the following:
@itemize
@item dirhash: calculate hashes over directory trees the same way that the Go tool does.
@item goproxytest: a GOPROXY implementation designed for test use.
@item gotooltest: Use the Go tool inside test scripts (see testscript below)
@item imports: list of known architectures and OSs, and support for reading import import statements.
@item modfile: read and write go.mod files while preserving formatting and comments.
@item module: module paths and versions.
@item par: do work in parallel.
@item semver: semantic version parsing.
@item testenv: information on the current testing environment.
@item testscript: script-based testing based on txtar files
@item txtar: simple text-based file archives for testing.
@end itemize\n")
    (license license:bsd-3)))

(define-public gopkg-in-errgo-fmt-errors
  (package
    (name "gopkg-in-errgo-fmt-errors")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-errgo/errgo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "065mbihiy7q67wnql0bzl9y1kkvck5ivra68254zbih52jxwrgr2"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "gopkg.in/errgo.v2/fmt/errors"
       #:tests? #f
       ;; Source-only package
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (home-page "https://godoc.org/gopkg.in/errgo.v2")
    (synopsis "Functions that use the fmt package to format error messages")
    (description "This package is the same as @code{gopkg.in/errgo.v2/errors}
except that it adds convenience functions that use the fmt package to format
error messages.")
    (license license:bsd-3)))

(define-public go-github-com-arceliar-phony
  (let ((commit "d0c68492aca0bd4b5c5c8e0452c9b4c8af923eaf")
        (revision "0"))
    (package
      (name "go-github-com-arceliar-phony")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Arceliar/phony")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0876y0hlb1zh8hn0pxrb5zfdadvaqmqwlr66p19yl2a76galz992"))))
      (arguments
       '(#:import-path "github.com/Arceliar/phony"))
      (build-system go-build-system)
      (home-page "https://github.com/Arceliar/phony")
      (synopsis "Very minimal actor model library")
      (description "Phony is a very minimal actor model library for Go,
inspired by the causal messaging system in the Pony programming language.")
      (license license:expat))))

(define-public go-github-com-cheggaaa-pb
  (package
    (name "go-github-com-cheggaaa-pb")
    (version "3.0.4")
    (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cheggaaa/pb/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0xhsv9yf3fz918ay6w0d87jnb3hk9vxvi16jk172kqq26x7jixd0"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/cheggaaa/pb/"))
    (propagated-inputs
     `(("go-github-com-fatih-color" ,go-github-com-fatih-color)
       ("go-github-com-mattn-go-colorable" ,go-github-com-mattn-go-colorable)
       ("go-github.com-mattn-go-runewidth" ,go-github.com-mattn-go-runewidth)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (native-inputs
     `(("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)))
    (home-page "https://github.com/cheggaaa/pb/")
    (synopsis "Console progress bar for Go")
    (description "This package is a Go library that draws progress bars on
the terminal.")
    (license license:bsd-3)))

(define-public go-github-com-gologme-log
  ;; this is the same as v1.2.0, only the LICENSE file changed
  (let ((commit "720ba0b3ccf0a91bc6018c9967a2479f93f56a55"))
    (package
      (name "go-github-com-gologme-log")
      (version "1.2.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gologme/log")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0z3gs5ngv2jszp42ypp3ai0pn410v3b2m674g73ma7vsbn2yjk1n"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/gologme/log"))
      (home-page "https://github.com/gologme/log/")
      (synopsis
       "Fork of the golang built in log package to add support for levels")
      (description "This package is a drop in replacement for the built-in Go
log package.  All the functionality of the built-in package still exists and
is unchanged.  This package contains a series of small enhancements and
additions.")
      (license license:bsd-3))))

(define-public go-github-com-frankban-quicktest
  (package
    (name "go-github-com-frankban-quicktest")
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/frankban/quicktest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0b1b44b2hli2p969gqz30z8v9z6ahlklpqzi17nwk1lsjz9yv938"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/frankban/quicktest"))
    (propagated-inputs
     `(("go-github-com-google-go-cmp-cmp" ,go-github-com-google-go-cmp-cmp)
       ("go-github-com-kr-pretty" ,go-github-com-kr-pretty)))
    (home-page "https://github.com/frankban/quicktest")
    (synopsis "Quick helpers for testing Go applications")
    (description
     "Package quicktest provides a collection of Go helpers for writing
tests.")
    (license license:expat)))

(define-public go-github-com-bep-golibsass
  (package
    (name "go-github-com-bep-golibsass")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bep/golibsass")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0xk3m2ynbydzx87dz573ihwc4ryq0r545vz937szz175ivgfrhh3"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "libsass_src")
           #t))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/bep/golibsass/libsass"
       #:unpack-path "github.com/bep/golibsass"
       ;; The dev build tag modifies the build to link to system libsass
       ;; instead of including the bundled one (which we remove.)
       ;; https://github.com/bep/golibsass/blob/v0.7.0/internal/libsass/a__cgo_dev.go
       #:build-flags '("-tags" "dev")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-bindings
           ;; Generate bindings for system libsass, replacing the
           ;; pre-generated bindings.
           (lambda* (#:key inputs unpack-path #:allow-other-keys)
             (mkdir-p (string-append "src/" unpack-path "/internal/libsass"))
             (let ((libsass-src (string-append (assoc-ref inputs "libsass-src") "/src")))
               (substitute* (string-append "src/" unpack-path "/gen/main.go")
                 (("filepath.Join\\(rootDir, \"libsass_src\", \"src\"\\)")
                  (string-append "\"" libsass-src "\""))
                 (("../../libsass_src/src/")
                  libsass-src)))
             (invoke "go" "generate" (string-append unpack-path "/gen"))
             #t))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (if tests?
                 (invoke "go" "test" import-path "-tags" "dev"))
             #t)))))
    (propagated-inputs
     `(("libsass" ,libsass)))
    (native-inputs
     `(("go-github-com-frankban-quicktest" ,go-github-com-frankban-quicktest)
       ("libsass-src" ,(package-source libsass))))
    (home-page "https://github.com/bep/golibsass")
    (synopsis "Easy to use Go bindings for LibSass")
    (description
     "This package provides SCSS compiler support for Go applications.")
    (license license:expat)))

(define-public go-github-com-hashicorp-go-syslog
  (package
    (name "go-github-com-hashicorp-go-syslog")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-syslog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09vccqggz212cg0jir6vv708d6mx0f9w5bxrcdah3h6chgmal6v1"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/hashicorp/go-syslog"))
    (home-page "https://github.com/hashicorp/go-syslog")
    (synopsis "Golang syslog wrapper, cross-compile friendly")
    (description "This package is a very simple wrapper around log/syslog")
    (license license:expat)))

(define-public go-github-com-hjson-hjson-go
  (package
    (name "go-github-com-hjson-hjson-go")
    (version "3.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hjson/hjson-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1dfdiahimg6z9idg8jiqxwnlwjnmasbjccx8gnag49cz4yfqskaz"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/hjson/hjson-go"))
    (home-page "https://hjson.org/")
    (synopsis "Human JSON implementation for Go")
    (description "Hjson is a syntax extension to JSON.  It is intended to be
used like a user interface for humans, to read and edit before passing the
JSON data to the machine.")
    (license license:expat)))

(define-public go-golang-zx2c4-com-wireguard
  (package
    (name "go-golang-zx2c4-com-wireguard")
    (version "0.0.20200320")
    (source
     (origin
       (method git-fetch)
       ;; NOTE: module URL is a redirect
       ;; target: git.zx2c4.com/wireguard-go
       ;; source: golang.zx2c4.com/wireguard
       (uri (git-reference
             (url "https://git.zx2c4.com/wireguard-go/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fy4qsss3i3pkq1rpgjds4aipbwlh1dr9hbbf7jn2a1c63kfks0r"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.zx2c4.com/wireguard"))
    (propagated-inputs
     `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-golang-org-x-text" ,go-golang-org-x-text)))
    (home-page "https://git.zx2c4.com/wireguard")
    (synopsis "Implementation of WireGuard in Go")
    (description "This package is a Go Implementation of WireGuard.")
    (license license:expat)))

(define-public go-github-com-kardianos-minwinsvc
  (package
    (name "go-github-com-kardianos-minwinsvc")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kardianos/minwinsvc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0z941cxymkjcsj3p5l3g4wm2da3smz7iyqk2wbs5y8lmxd4kfzd8"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kardianos/minwinsvc"))
    (home-page "https://github.com/kardianos/minwinsvc/")
    ;; some packages (Yggdrasil) need it to compile
    ;; it's a tiny package and it's easier to bundle it than to patch it out
    (synopsis "Minimal windows only service stub for Go")
    (description "Go programs designed to run from most *nix style operating
systems can import this package to enable running programs as services without
modifying them.")
    (license license:zlib)))

(define-public go-github-com-goccy-yaml
  (package
    (name "go-github-com-goccy-yaml")
    (version "1.8.0")
    (home-page "https://github.com/goccy/go-yaml")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nps58dwkd915mx35h5f0dc05b880b4fdl6dcjxpfmmbzyinvg38"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/goccy/go-yaml"))
    (propagated-inputs
     `(("go-github-com-fatih-color" ,go-github-com-fatih-color)
       ("go-golang-org-x-xerrors" ,go-golang-org-x-xerrors)))
    (native-inputs
     `(("go-gopkg-in-go-playground-validator-v9"
        ,go-gopkg-in-go-playground-validator-v9)))
    (synopsis "YAML support for the Go language")
    (description
     "This package provides features beyond the
@uref{https://github.com/go-yaml/yaml, defacto YAML library} including:

@itemize
@item Pretty format for error notifications
@item Support Scanner or Lexer or Parser as public API
@item Support Anchor and Alias to Marshaler
@item Allow referencing elements declared in another file via anchors
@item Extract value or AST by YAMLPath (YAMLPath is like a JSONPath)
@end itemize")
    (license license:expat)))

(define-public go-github-com-tekwizely-go-parsing
  (let ((commit "1548cfb17df54d365ce9febed0677c06a40a8ceb")
        (revision "0"))
    (package
      (name "go-github-com-tekwizely-go-parsing")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tekwizely/go-parsing")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bv5amka8hb9crc7qvlzi2kbycqrnh9d46b9wgcs8wqzl0z7w609"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/tekwizely/go-parsing"))
      (home-page "https://github.com/tekwizely/go-parsing")
      (synopsis "Text parsing, with lexers, parsers, and related tools")
      (description
       "This package provides Go modules focused on text parsing, with lexers,
parsers, and related tools.")
      (license license:expat))))

(define-public go-github.com-ulikunitz-xz
  (package
    (name "go-github.com-ulikunitz-xz")
    (version "0.5.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ulikunitz/xz.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1xnsymi5fmmm734bi4c6z57p5cvnyxlpi29yxs4v21w5k763aypd"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/ulikunitz/xz"))
    (synopsis "Read and write xz compressed streams in Go")
    (description "This package provides a library to read and write xz
compressed streams in Go.")
    (home-page "https://github.com/ulikunitz/xz")
    (license license:bsd-3)))

(define-public go-github-com-songmu-gitconfig
  (package
    (name "go-github-com-songmu-gitconfig")
    (version "0.1.0")
    (home-page "https://github.com/songmu/gitconfig")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y01h496a7pfj1g2bclls5b0nl3vnj7nz610jj1dzq9kxrwxk7fk"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/Songmu/gitconfig"
       ;; Package's tests appear to be hardcoded to the author's gitconfig
       ;; and require network access.
       #:tests? #f))
    (propagated-inputs
     `(("go-github-com-goccy-yaml" ,go-github-com-goccy-yaml)))
    (synopsis "Go library to get configuration values from gitconfig")
    (description "@{gitconfig} is a package to get configuration values from gitconfig.")
    (license license:expat)))

(define-public go-github-com-operatorfoundation-ed25519
  (let ((commit "b22b4bd3ddef042eec45f3ee135cd40281fde2b4")
        (revision "0"))
    (package
      (name "go-github-com-operatorfoundation-ed25519")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/OperatorFoundation/ed25519")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0xrzqrjlghkgd1cy5rj4khryn4f59vas2vzrxc6d8jpj5ijf3xkv"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/OperatorFoundation/ed25519"
         #:phases
         (modify-phases %standard-phases
           (add-before 'install 'remove-test-data
             (lambda* (#:key import-path #:allow-other-keys)
               (delete-file-recursively
                 (string-append "src/" import-path "/testdata"))
               #t)))))
      (home-page "https://github.com/OperatorFoundation/ed25519")
      (synopsis "Ed25519 for go")
      (description "Package ed25519 implements the Ed25519 signature
algorithm.")
      (license license:bsd-3))))

(define-public go-github-com-akosmarton-papipes
  (let ((commit "3c63b4919c769c9c2b2d07e69a98abb0eb47fe64")
        (revision "0"))
    (package
      (name "go-github-com-akosmarton-papipes")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/akosmarton/papipes")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16p77p3d1v26qd3knxn087jqlad2qm23q8m796cdr66hrdc0gahq"))))
      (build-system go-build-system)
      (inputs
       `(("pulseaudio" ,pulseaudio)))
      (arguments
       `(#:import-path "github.com/akosmarton/papipes"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("src/github.com/akosmarton/papipes/common.go"
                              "src/github.com/akosmarton/papipes/sink.go"
                              "src/github.com/akosmarton/papipes/source.go")
                 (("exec.Command\\(\"pactl\"")
                  (string-append "exec.Command(\""
                                 (assoc-ref inputs "pulseaudio")
                                 "/bin/pactl\""))))))))
      (home-page "https://github.com/akosmarton/papipes")
      (synopsis "Pulseaudio client library for Go")
      (description
       "This is a Pulseaudio client library in Golang for creating virtual
sinks and sources.")
      (license license:expat))))

(define-public go-github-com-mesilliac-pulse-simple
  (let ((commit "75ac54e19fdff88f4fbd82f45125134b602230b0")
        (revision "0"))
    (package
      (name "go-github-com-mesilliac-pulse-simple")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mesilliac/pulse-simple")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1awwczsa9yy99p035ckajqfs8m6mab0lz82mzlj1c5cj9lnmwplj"))))
      (build-system go-build-system)
      (propagated-inputs
       `(("pkg-config" ,pkg-config)
         ("pulseaudio" ,pulseaudio)))
      (arguments
       '(#:import-path "github.com/mesilliac/pulse-simple"))
      (home-page "https://github.com/mesilliac/pulse-simple")
      (synopsis "Cgo bindings to PulseAudio's Simple API")
      (description
       "This packages provides Cgo bindings to PulseAudio's Simple API, to play
or capture raw audio.")
      (license license:expat))))

(define-public go-github-com-pborman-getopt
  (package
    (name "go-github-com-pborman-getopt")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pborman/getopt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sacv6g8cxfibxd3gnfjnzp7fynrnc4s2aaz5wbxivqqhvflc22l"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/pborman/getopt"))
    (home-page "https://github.com/pborman/getopt")
    (synopsis "Getopt style option parsing for Go")
    (description
     "This package provides traditional getopt processing for implementing
programs that use traditional command lines.")
    (license license:bsd-3)))

(define-public go-go-uber-org-atomic
  (package
    (name "go-go-uber-org-atomic")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/atomic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yxvb5sixh76cl9j8dpa97gznj0p8pmg2cdw0ypfwhd3ipx9wph1"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.uber.org/atomic"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)))
    (home-page "https://go.uber.org/atomic")
    (synopsis "Wrapper types for sync/atomic")
    (description
     "This package provides simple wrappers for primitive types to enforce
atomic access.")
    (license license:expat)))

(define-public go-go-uber-org-multierr
  (package
    (name "go-go-uber-org-multierr")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/multierr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "162941s8f6a9x2w04qm4qa3zz0zylwag9149hywrj9ibp2nzcsqz"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.uber.org/multierr"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (propagated-inputs
     `(("go-go-uber-org-atomic" ,go-go-uber-org-atomic)))
    (home-page "https://go.uber.org/multierr")
    (synopsis "Error combination for Go")
    (description
     "@code{multierr} allows combining one or more Go errors together.")
    (license license:expat)))

(define-public go-golang-org-x-lint
  (let ((commit "83fdc39ff7b56453e3793356bcff3070b9b96445")
        (revision "0"))
    (package
      (name "go-golang-org-x-lint")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/lint")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ms3rs5hvpnm9bxbr5f9743i7hn2bbmqdmvzxq6nmi0f24ypv1l3"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/lint"
         #:tests? #f)) ;; TODO: Fix tests
      (propagated-inputs
       `(("go-golang-org-x-tools" ,go-golang-org-x-tools)))
      (home-page "https://golang.org/x/lint")
      (synopsis "Linter for Go source code")
      (description
       "This is a linter for Go source code.  Unlike gofmt, it doesn't
reformat the source code, it only prints out style mistakes.")
      (license license:bsd-3))))

(define-public go-github-com-kisielk-gotool
  (package
    (name "go-github-com-kisielk-gotool")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kisielk/gotool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14af2pa0ssyp8bp2mvdw184s5wcysk6akil3wzxmr05wwy951iwn"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kisielk/gotool"))
    (home-page "https://github.com/kisielk/gotool")
    (synopsis "Go library of utility functions")
    (description
     "This package contains utility functions used to implement the standard
@code{cmd/go} tool, provided as a convenience to developers who want to write
tools with similar semantics.")
    (license license:expat)))

(define-public go-honnef-co-go-tools
  (package
    (name "go-honnef-co-go-tools")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dominikh/go-tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17li8jbw3cpn59kpcl3j3r2an4wkx3fc81xn0j4xgbjpkxh9493n"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "honnef.co/go/tools"
       #:tests? #f
       ;; Source-only package
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (propagated-inputs
     `(("go-golang-org-x-tools" ,go-golang-org-x-tools)
       ("go-github-com-kisielk-gotool",go-github-com-kisielk-gotool)
       ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)))
    (home-page "https://honnef.co/go/tools")
    (synopsis "Staticcheck advanced Go linter")
    (description
     "Staticcheck is a state of the art linter for the Go programming language.
Using static analysis, it finds bugs and performance issues, offers
simplifications, and enforces style rules.")
    (license license:expat)))

(define-public go-go-uber-org-zap
  (package
    (name "go-go-uber-org-zap")
    (version "1.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/zap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05ix5wg1r8pgi7fb6084lg4x7mrkvzkh1nxa7zj337w5b9xj0myr"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.uber.org/zap"
       #:tests? #f)) ; TODO: Fix tests
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ("go-golang-org-x-lint" ,go-golang-org-x-lint)
       ("go-honnef-co-go-tools" ,go-honnef-co-go-tools)))
    (propagated-inputs
     `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-go-uber-org-atomic" ,go-go-uber-org-atomic)
       ("go-go-uber-org-multierr" ,go-go-uber-org-multierr)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)))
    (home-page "https://go.uber.org/zap")
    (synopsis "Logging library for Go")
    (description
     "This package provides a library for fast, structured, leveled logging in
Go.")
    (license license:expat)))
