;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
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

;; Go software is developed and built within a particular filesystem hierarchy
;; structure called a 'workspace' [1].  This workspace is found by Go
;; via the GOPATH environment variable.  Typically, all Go source code
;; and compiled objects are kept in a single workspace, but it is
;; possible for GOPATH to contain a list of directories, and that is
;; what we do in this go-build-system. [2]
;;
;; Go software, whether a package or a command, is uniquely named using
;; an 'import path'.  The import path is based on the URL of the
;; software's source.  Since most source code is provided over the
;; internet, the import path is typically a combination of the remote
;; URL and the source repository's filesystem structure. For example,
;; the Go port of the common `du` command is hosted on github.com, at
;; <https://github.com/calmh/du>.  Thus, the import path is
;; <github.com/calmh/du>. [3]
;;
;; It may be possible to programatically guess a package's import path
;; based on the source URL, but we don't try that in this revision of
;; the go-build-system.
;;
;; Modules of modular Go libraries are named uniquely with their
;; filesystem paths.  For example, the supplemental but "standardized"
;; libraries developed by the Go upstream developers are available at
;; <https://golang.org/x/{net,text,crypto, et cetera}>.  The Go IPv4
;; library's import path is <golang.org/x/net/ipv4>.  The source of
;; such modular libraries must be unpacked at the top-level of the
;; filesystem structure of the library.  So the IPv4 library should be
;; unpacked to <golang.org/x/net>.  This is handled in the
;; go-build-system with the optional #:unpack-path key.
;;
;; In general, Go software is built using a standardized build mechanism
;; that does not require any build scripts like Makefiles.  This means
;; that all modules of modular libraries cannot be built with a single
;; command.  Each module must be built individually.  This complicates
;; certain cases, and these issues are currently resolved by creating a
;; filesystem union of the required modules of such libraries.  I think
;; this could be improved in future revisions of the go-build-system.
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
;;
;; Code:

(define* (unpack #:key source import-path unpack-path #:allow-other-keys)
  "Unpack SOURCE in the UNPACK-PATH, or the IMPORT-PATH is the UNPACK-PATH is
unset.  When SOURCE is a directory, copy it instead of unpacking."
  (if (string-null? import-path)
      ((display "WARNING: The Go import path is unset.\n")))
  (if (string-null? unpack-path)
      (set! unpack-path import-path))
  (mkdir "src")
  (let ((dest (string-append "src/" unpack-path)))
    (mkdir-p dest)
    (if (file-is-directory? source)
      (begin
        (copy-recursively source dest #:keep-mtime? #t)
        #t)
      (if (string-suffix? ".zip" source)
        (zero? (system* "unzip" "-d" dest source))
        (zero? (system* "tar" "-C" dest "-xvf" source))))))

(define* (install-source #:key install-source? outputs #:allow-other-keys)
  "Install the source code to the output directory."
  (let* ((out (assoc-ref outputs "out"))
         (source "src")
         (dest (string-append out "/" source)))
    (if install-source?
      (copy-recursively source dest #:keep-mtime? #t)
      #t)))

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

(define* (setup-environment #:key inputs outputs #:allow-other-keys)
  "Export the variables GOPATH and GOBIN, which are based on INPUTS and OUTPUTS,
respectively."
  (let ((out (assoc-ref outputs "out")))
    ;; GOPATH is where Go looks for the source code of the build's dependencies.
    (set-path-environment-variable "GOPATH"
                                   ;; XXX Matching "." hints that we could do
                                   ;; something simpler here...
                                   (list ".")
                                   (match (go-inputs inputs)
                                     (((_ . dir) ...)
                                      dir)))

    ;; Add the source code of the package being built to GOPATH.
    (if (getenv "GOPATH")
      (setenv "GOPATH" (string-append (getcwd) ":" (getenv "GOPATH")))
      (setenv "GOPATH" (getcwd)))
    ;; Where to install compiled executable files ('commands' in Go parlance').
    (setenv "GOBIN" (string-append out "/bin"))
    #t))

(define* (build #:key import-path #:allow-other-keys)
  "Build the package named by IMPORT-PATH."
  (or
    (zero? (system* "go" "install"
                    "-v" ; print the name of packages as they are compiled
                    "-x" ; print each command as it is invoked
                    ;; Respectively, strip the symbol table and debug
                    ;; information, and the DWARF symbol table.
                    "-ldflags=-s -w"
                    import-path))
    (begin
      (display (string-append "Building '" import-path "' failed.\n"
                              "Here are the results of `go env`:\n"))
      (system* "go" "env")
      #f)))

(define* (check #:key tests? import-path #:allow-other-keys)
  "Run the tests for the package named by IMPORT-PATH."
  (if tests?
    (zero? (system* "go" "test" import-path))))

(define* (install #:key outputs #:allow-other-keys)
  "Install the compiled libraries. `go install` installs these files to
$GOPATH/pkg, so we have to copy them into the output directory manually.
Compiled executable files should have already been installed to the store based
on $GOBIN in the build phase."
  (when (file-exists? "pkg")
    (copy-recursively "pkg" (string-append (assoc-ref outputs "out") "/pkg")))
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
    (delete 'configure)
    (delete 'patch-generated-file-shebangs)
    (replace 'unpack unpack)
    (add-after 'unpack 'install-source install-source)
    (add-before 'build 'setup-environment setup-environment)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'install 'remove-go-references remove-go-references)))

(define* (go-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Go package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
