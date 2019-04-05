;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2017, 2019 Leo Famulari <leo@famulari.name>
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

(define-module (guix build go-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build union)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:export (%standard-phases
            go-build))

;; Commentary:
;;
;; Build procedures for Go packages.  This is the builder-side code.
;;
;; Software written in Go is either a 'package' (i.e. library) or 'command'
;; (i.e. executable).  Both types can be built with either the `go build` or `go
;; install` commands.  However, `go build` discards the result of the build
;; process for Go libraries, so we use `go install`, which preserves the
;; results. [0]

;; Go software is developed and built within a particular file system hierarchy
;; structure called a 'workspace' [1].  This workspace can be found by Go via
;; the GOPATH environment variable.  Typically, all Go source code and compiled
;; objects are kept in a single workspace, but GOPATH may be a list of
;; directories [2].  In this go-build-system we create a file system union of
;; the Go-language dependencies. Previously, we made GOPATH a list of store
;; directories, but stopped because Go programs started keeping references to
;; these directories in Go 1.11:
;; <https://bugs.gnu.org/33620>.
;;
;; Go software, whether a package or a command, is uniquely named using an
;; 'import path'.  The import path is based on the URL of the software's source.
;; Because most source code is provided over the internet, the import path is
;; typically a combination of the remote URL and the source repository's file
;; system structure. For example, the Go port of the common `du` command is
;; hosted on github.com, at <https://github.com/calmh/du>.  Thus, the import
;; path is <github.com/calmh/du>. [3]
;;
;; It may be possible to automatically guess a package's import path based on
;; the source URL, but we don't try that in this revision of the
;; go-build-system.
;;
;; Modules of modular Go libraries are named uniquely with their
;; file system paths.  For example, the supplemental but "standardized"
;; libraries developed by the Go upstream developers are available at
;; <https://golang.org/x/{net,text,crypto, et cetera}>.  The Go IPv4
;; library's import path is <golang.org/x/net/ipv4>.  The source of
;; such modular libraries must be unpacked at the top-level of the
;; file system structure of the library.  So the IPv4 library should be
;; unpacked to <golang.org/x/net>.  This is handled in the
;; go-build-system with the optional #:unpack-path key.
;;
;; In general, Go software is built using a standardized build mechanism
;; that does not require any build scripts like Makefiles.  This means
;; that all modules of modular libraries cannot be built with a single
;; command.  Each module must be built individually.  This complicates
;; certain cases, and these issues are currently resolved by creating a
;; file system union of the required modules of such libraries.  I think
;; this could be improved in future revisions of the go-build-system.
;;
;; TODO:
;; * Avoid copying dependencies into the build environment and / or avoid using
;; a tmpdir when creating the inputs union.
;; * Use Go modules [4]
;; * Re-use compiled packages [5]
;; * Avoid the go-inputs hack
;; * Stop needing remove-go-references (-trimpath ? )
;; * Remove module packages, only offering the full Git repos? This is
;; more idiomatic, I think, because Go downloads Git repos, not modules.
;; What are the trade-offs?
;;
;; [0] `go build`:
;; https://golang.org/cmd/go/#hdr-Compile_packages_and_dependencies
;; `go install`:
;; https://golang.org/cmd/go/#hdr-Compile_and_install_packages_and_dependencies
;; [1] Go workspace example, from <https://golang.org/doc/code.html#Workspaces>:
;; bin/
;;     hello                          # command executable
;;     outyet                         # command executable
;; pkg/
;;     linux_amd64/
;;         github.com/golang/example/
;;             stringutil.a           # package object
;; src/
;;     github.com/golang/example/
;;         .git/                      # Git repository metadata
;; 	   hello/
;; 	       hello.go               # command source
;; 	   outyet/
;; 	        main.go               # command source
;; 	        main_test.go          # test source
;; 	   stringutil/
;; 	       reverse.go             # package source
;; 	       reverse_test.go        # test source
;;         golang.org/x/image/
;;             .git/                  # Git repository metadata
;; 	       bmp/
;; 	           reader.go          # package source
;; 	           writer.go          # package source
;;     ... (many more repositories and packages omitted) ...
;;
;; [2] https://golang.org/doc/code.html#GOPATH
;; [3] https://golang.org/doc/code.html#ImportPaths
;; [4] https://golang.org/cmd/go/#hdr-Modules__module_versions__and_more
;; [5] https://bugs.gnu.org/32919
;;
;; Code:

(define* (setup-go-environment #:key inputs outputs #:allow-other-keys)
  "Prepare a Go build environment for INPUTS and OUTPUTS.  Build a file system
union of INPUTS.  Export GOPATH, which helps the compiler find the source code
of the package being built and its dependencies, and GOBIN, which determines
where executables (\"commands\") are installed to.  This phase is sometimes used
by packages that use (guix build-system gnu) but have a handful of Go
dependencies, so it should be self-contained."
  ;; Using the current working directory as GOPATH makes it easier for packagers
  ;; who need to manipulate the unpacked source code.
  (setenv "GOPATH" (getcwd))
  (setenv "GOBIN" (string-append (assoc-ref outputs "out") "/bin"))
  (let ((tmpdir (tmpnam)))
    (match (go-inputs inputs)
      (((names . directories) ...)
       (union-build tmpdir (filter directory-exists? directories)
                    #:create-all-directories? #t
                    #:log-port (%make-void-port "w"))))
    ;; XXX A little dance because (guix build union) doesn't use mkdir-p.
    (copy-recursively tmpdir
                      (string-append (getenv "GOPATH"))
                      #:keep-mtime? #t)
    (delete-file-recursively tmpdir))
  #t)

(define* (unpack #:key source import-path unpack-path #:allow-other-keys)
  "Relative to $GOPATH, unpack SOURCE in the UNPACK-PATH, or the IMPORT-PATH is
the UNPACK-PATH is unset.  When SOURCE is a directory, copy it instead of
unpacking."
  (if (string-null? import-path)
      ((display "WARNING: The Go import path is unset.\n")))
  (if (string-null? unpack-path)
      (set! unpack-path import-path))
  (let ((dest (string-append (getenv "GOPATH") "/src/" unpack-path)))
    (mkdir-p dest)
    (if (file-is-directory? source)
        (begin
          (copy-recursively source dest #:keep-mtime? #t)
          #t)
        (if (string-suffix? ".zip" source)
            (invoke "unzip" "-d" dest source)
            (invoke "tar" "-C" dest "-xvf" source)))))

(define (go-package? name)
  (string-prefix? "go-" name))

(define (go-inputs inputs)
  "Return the alist of INPUTS that are Go software."
  ;; XXX This should not check the file name of the store item. Instead we
  ;; should pass, from the host side, the list of inputs that are packages using
  ;; the go-build-system.
  (alist-delete "go" ; Exclude the Go compiler
    (alist-delete "source" ; Exclude the source code of the package being built
      (filter (match-lambda
                ((label . directory)
                 (go-package? ((compose package-name->name+version
                                        strip-store-file-name)
                               directory)))
                (_ #f))
              inputs))))

(define* (build #:key import-path #:allow-other-keys)
  "Build the package named by IMPORT-PATH."
  (with-throw-handler
    #t
    (lambda _
      (invoke "go" "install"
              "-v" ; print the name of packages as they are compiled
              "-x" ; print each command as it is invoked
              ;; Respectively, strip the symbol table and debug
              ;; information, and the DWARF symbol table.
              "-ldflags=-s -w"
              import-path))
    (lambda (key . args)
      (display (string-append "Building '" import-path "' failed.\n"
                              "Here are the results of `go env`:\n"))
      (invoke "go" "env"))))

;; Can this also install commands???
(define* (check #:key tests? import-path #:allow-other-keys)
  "Run the tests for the package named by IMPORT-PATH."
  (when tests?
    (invoke "go" "test" import-path))
  #t)

(define* (install #:key install-source? outputs import-path unpack-path #:allow-other-keys)
  "Install the source code of IMPORT-PATH to the primary output directory.
Compiled executable files (Go \"commands\") should have already been installed
to the store based on $GOBIN in the build phase.
XXX We can't make us of compiled libraries (Go \"packages\")."
  (when install-source?
    (if (string-null? import-path)
        ((display "WARNING: The Go import path is unset.\n")))
    (let* ((out (assoc-ref outputs "out"))
           (source (string-append (getenv "GOPATH") "/src/" import-path))
           (dest (string-append out "/src/" import-path)))
      (mkdir-p dest)
      (copy-recursively source dest #:keep-mtime? #t)))
  #t)

(define* (remove-store-reference file file-name
                                  #:optional (store (%store-directory)))
  "Remove from FILE occurrences of FILE-NAME in STORE; return #t when FILE-NAME
is encountered in FILE, #f otherwise. This implementation reads FILE one byte at
a time, which is slow. Instead, we should use the Boyer-Moore string search
algorithm; there is an example in (guix build grafts)."
  (define pattern
    (string-take file-name
                 (+ 34 (string-length (%store-directory)))))

  (with-fluids ((%default-port-encoding #f))
    (with-atomic-file-replacement file
      (lambda (in out)
        ;; We cannot use `regexp-exec' here because it cannot deal with
        ;; strings containing NUL characters.
        (format #t "removing references to `~a' from `~a'...~%" file-name file)
        (setvbuf in 'block 65536)
        (setvbuf out 'block 65536)
        (fold-port-matches (lambda (match result)
                             (put-bytevector out (string->utf8 store))
                             (put-u8 out (char->integer #\/))
                             (put-bytevector out
                                             (string->utf8
                                              "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-"))
                             #t)
                           #f
                           pattern
                           in
                           (lambda (char result)
                             (put-u8 out (char->integer char))
                             result))))))

(define* (remove-go-references #:key allow-go-reference?
                               inputs outputs #:allow-other-keys)
  "Remove any references to the Go compiler from the compiled Go executable
files in OUTPUTS."
;; We remove this spurious reference to save bandwidth when installing Go
;; executables. It would be better to not embed the reference in the first
;; place, but I'm not sure how to do that. The subject was discussed at:
;; <https://lists.gnu.org/archive/html/guix-devel/2017-10/msg00207.html>
  (if allow-go-reference?
    #t
    (let ((go (assoc-ref inputs "go"))
          (bin "/bin"))
      (for-each (lambda (output)
                  (when (file-exists? (string-append (cdr output)
                                                     bin))
                    (for-each (lambda (file)
                                (remove-store-reference file go))
                              (find-files (string-append (cdr output) bin)))))
                outputs)
      #t)))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'patch-generated-file-shebangs)
    (add-before 'unpack 'setup-go-environment setup-go-environment)
    (replace 'unpack unpack)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'install 'remove-go-references remove-go-references)))

(define* (go-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Go package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
