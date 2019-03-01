;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (guix build cargo-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (guix build cargo-utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            cargo-build))

;; Commentary:
;;
;; Builder-side code of the standard Rust package build procedure.
;;
;; Code:

;; FIXME: Needs to be parsed from url not package name.
(define (package-name->crate-name name)
  "Return the crate name of NAME."
  (match (string-split name #\-)
    (("rust" rest ...)
     (string-join rest "-"))
    (_ #f)))

(define* (configure #:key inputs #:allow-other-keys)
  "Replace Cargo.toml [dependencies] section with guix inputs."
  ;; Make sure Cargo.toml is writeable when the crate uses git-fetch.
  (chmod "Cargo.toml" #o644)
  (chmod "." #o755)
  (if (not (file-exists? "vendor"))
    (if (not (file-exists? "Cargo.lock"))
      (begin
        (substitute* "Cargo.toml"
          ((".*32-sys.*") "
")
          ((".*winapi.*") "
")
          ((".*core-foundation.*") "
"))
        ;; Prepare one new directory with all the required dependencies.
        ;; It's necessary to do this (instead of just using /gnu/store as the
        ;; directory) because we want to hide the libraries in subdirectories
        ;;   share/rust-source/... instead of polluting the user's profile root.
        (mkdir "vendor")
        (for-each
          (match-lambda
            ((name . path)
             (let ((crate (package-name->crate-name name)))
               (when (and crate path)
                 (match (string-split (basename path) #\-)
                   ((_ ... version)
                    (symlink (string-append path "/share/rust-source")
                             (string-append "vendor/" (basename path)))))))))
          inputs)
        ;; Configure cargo to actually use this new directory.
        (mkdir-p ".cargo")
        (let ((port (open-file ".cargo/config" "w" #:encoding "utf-8")))
          (display "
[source.crates-io]
registry = 'https://github.com/rust-lang/crates.io-index'
replace-with = 'vendored-sources'

[source.vendored-sources]
directory = '" port)
          (display (getcwd) port)
          (display "/vendor" port)
          (display "'
" port)
          (close-port port)))))
    (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))

    ;(setenv "CARGO_HOME" "/gnu/store")
    ; (setenv "CMAKE_C_COMPILER" cc)
  #t)

(define* (build #:key (cargo-build-flags '("--release"))
                #:allow-other-keys)
  "Build a given Cargo package."
  (zero? (apply system* `("cargo" "build" ,@cargo-build-flags))))

(define* (check #:key tests? #:allow-other-keys)
  "Run tests for a given Cargo package."
  (if (and tests? (file-exists? "Cargo.lock"))
      (zero? (system* "cargo" "test"))
      #t))

(define (touch file-name)
  (call-with-output-file file-name (const #t)))

(define* (install #:key inputs outputs #:allow-other-keys)
  "Install a given Cargo package."
  (let* ((out (assoc-ref outputs "out"))
         (src (assoc-ref inputs "source"))
         (rsrc (string-append (assoc-ref outputs "src")
                              "/share/rust-source")))
    (mkdir-p rsrc)
    ;; Rust doesn't have a stable ABI yet. Because of this
    ;; Cargo doesn't have a search path for binaries yet.
    ;; Until this changes we are working around this by
    ;; distributing crates as source and replacing
    ;; references in Cargo.toml with store paths.
    (copy-recursively "src" (string-append rsrc "/src"))
    (touch (string-append rsrc "/.cargo-ok"))
    (generate-checksums rsrc src)
    (install-file "Cargo.toml" rsrc)
    ;; When the package includes executables we install
    ;; it using cargo install. This fails when the crate
    ;; doesn't contain an executable.
    (if (file-exists? "Cargo.lock")
        (zero? (system* "cargo" "install" "--root" out))
        (begin
          (mkdir out)
          #t))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))

(define* (cargo-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Cargo package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; cargo-build-system.scm ends here
