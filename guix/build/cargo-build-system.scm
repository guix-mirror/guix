;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
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
  #:use-module (json parser)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            cargo-build))

;; Commentary:
;;
;; Builder-side code of the standard Rust package build procedure.
;;
;; Code:

(define (manifest-targets)
  "Extract all targets from the Cargo.toml manifest"
  (let* ((port (open-input-pipe "cargo read-manifest"))
         (data (json->scm port))
         (targets (hash-ref data "targets" '())))
    (close-port port)
    targets))

(define (has-executable-target?)
  "Check if the current cargo project declares any binary targets."
  (let* ((bin? (lambda (kind) (string=? kind "bin")))
         (get-kinds (lambda (dep) (hash-ref dep "kind")))
         (bin-dep? (lambda (dep) (find bin? (get-kinds dep)))))
    (find bin-dep? (manifest-targets))))

(define* (configure #:key inputs
                    (vendor-dir "guix-vendor")
                    #:allow-other-keys)
  "Vendor Cargo.toml dependencies as guix inputs."
  (chmod "." #o755)
  ;; Prepare one new directory with all the required dependencies.
  ;; It's necessary to do this (instead of just using /gnu/store as the
  ;; directory) because we want to hide the libraries in subdirectories
  ;; share/rust-source/... instead of polluting the user's profile root.
  (mkdir-p vendor-dir)
  (for-each
    (match-lambda
      ((name . path)
       (let* ((rust-share (string-append path "/share/rust-source"))
              (basepath (basename path))
              (link-dir (string-append vendor-dir "/" basepath)))
         (and (file-exists? rust-share)
              ;; Gracefully handle duplicate inputs
              (not (file-exists? link-dir))
              (symlink rust-share link-dir)))))
    inputs)
  ;; Configure cargo to actually use this new directory.
  (mkdir-p ".cargo")
  (let ((port (open-file ".cargo/config" "w" #:encoding "utf-8")))
    (display "
[source.crates-io]
replace-with = 'vendored-sources'

[source.vendored-sources]
directory = '" port)
    (display (string-append (getcwd) "/" vendor-dir) port)
    (display "'
" port)
    (close-port port))

  ;; Lift restriction on any lints: a crate author may have decided to opt
  ;; into stricter lints (e.g. #![deny(warnings)]) during their own builds
  ;; but we don't want any build failures that could be caused later by
  ;; upgrading the compiler for example.
  (setenv "RUSTFLAGS" "--cap-lints allow")
  (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
  #t)

(define* (build #:key
                skip-build?
                (cargo-build-flags '("--release"))
                #:allow-other-keys)
  "Build a given Cargo package."
  (or skip-build?
      (zero? (apply system* `("cargo" "build" ,@cargo-build-flags)))))

(define* (check #:key
                tests?
                (cargo-test-flags '("--release"))
                #:allow-other-keys)
  "Run tests for a given Cargo package."
  (if tests?
      (zero? (apply system* `("cargo" "test" ,@cargo-test-flags)))
      #t))

(define (touch file-name)
  (call-with-output-file file-name (const #t)))

(define* (install-source #:key inputs outputs #:allow-other-keys)
  "Install the source for a given Cargo package."
  (let* ((out (assoc-ref outputs "out"))
         (src (assoc-ref inputs "source"))
         (rsrc (string-append (assoc-ref outputs "src")
                              "/share/rust-source")))
    (mkdir-p rsrc)
    ;; Rust doesn't have a stable ABI yet. Because of this
    ;; Cargo doesn't have a search path for binaries yet.
    ;; Until this changes we are working around this by
    ;; vendoring the crates' sources by symlinking them
    ;; to store paths.
    (copy-recursively "." rsrc)
    (touch (string-append rsrc "/.cargo-ok"))
    (generate-checksums rsrc "/dev/null")
    (install-file "Cargo.toml" rsrc)
    #t))

(define* (install #:key inputs outputs skip-build? #:allow-other-keys)
  "Install a given Cargo package."
  (let* ((out (assoc-ref outputs "out")))
    (mkdir-p out)

    ;; Make cargo reuse all the artifacts we just built instead
    ;; of defaulting to making a new temp directory
    (setenv "CARGO_TARGET_DIR" "./target")
    ;; Force cargo to honor our .cargo/config definitions
    ;; https://github.com/rust-lang/cargo/issues/6397
    (setenv "CARGO_HOME" ".")

    ;; Only install crates which include binary targets,
    ;; otherwise cargo will raise an error.
    (or skip-build?
        (not (has-executable-target?))
        (zero? (system* "cargo" "install" "--path" "." "--root" out)))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (add-before 'configure 'install-source install-source)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))

(define* (cargo-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Cargo package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; cargo-build-system.scm ends here
