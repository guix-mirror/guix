;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
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
  #:use-module (guix build json)
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

(define (manifest-targets)
  "Extract all targets from the Cargo.toml manifest"
  (let* ((port (open-input-pipe "cargo read-manifest"))
         (data (read-json port))
         (targets (or (assoc-ref data "targets") '())))
    (close-port port)
    targets))

(define (has-executable-target?)
  "Check if the current cargo project declares any binary targets."
  (let* ((bin? (lambda (kind) (string=? kind "bin")))
         (get-kinds (lambda (dep) (assoc-ref dep "kind")))
         (bin-dep? (lambda (dep) (find bin? (get-kinds dep)))))
    (find bin-dep? (manifest-targets))))

(define (crate-src? path)
  "Check if PATH refers to a crate source, namely a gzipped tarball with a
Cargo.toml file present at its root."
    (and (not (directory-exists? path)) ; not a tarball
         ;; First we print out all file names within the tarball to see if it
         ;; looks like the source of a crate. However, the tarball will include
         ;; an extra path component which we would like to ignore (since we're
         ;; interested in checking if a Cargo.toml exists at the root of the
         ;; archive, but not nested anywhere else). We do this by cutting up
         ;; each output line and only looking at the second component. We then
         ;; check if it matches Cargo.toml exactly and short circuit if it does.
         (apply invoke (list "sh" "-c"
                             (string-append "tar -tf " path
                                            " | cut -d/ -f2"
                                            " | grep -q '^Cargo.toml$'")))))

(define* (unpack-rust-crates #:key inputs vendor-dir #:allow-other-keys)
  (define (inputs->rust-inputs inputs)
    "Filter using the label part from INPUTS."
    (filter (lambda (input)
              (match input
                ((name . _) (rust-package? name))))
            inputs))
  (define (inputs->directories inputs)
    "Extract the directory part from INPUTS."
    (match inputs
      (((names . directories) ...)
       directories)))

  (let ((rust-inputs (inputs->directories (inputs->rust-inputs inputs))))
    (unless (null? rust-inputs)
      (mkdir-p "target/package")
      (mkdir-p vendor-dir)
      ;; TODO: copy only regular inputs to target/package, not native-inputs.
      (for-each
        (lambda (input-crate)
          (for-each
            (lambda (packaged-crate)
              (unless
                (file-exists?
                  (string-append "target/package/" (basename packaged-crate)))
                (install-file packaged-crate "target/package/")))
            (find-files
              (string-append input-crate "/share/cargo/registry") "\\.crate$")))
        (delete-duplicates rust-inputs))

      (for-each (lambda (crate)
                  (invoke "tar" "xzf" crate "-C" vendor-dir))
                (find-files "target/package" "\\.crate$"))))
  #t)

(define (rust-package? name)
  (string-prefix? "rust-" name))

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
       (let* ((basepath (strip-store-file-name path))
              (crate-dir (string-append vendor-dir "/" basepath)))
         (and (crate-src? path)
              ;; Gracefully handle duplicate inputs
              (not (file-exists? crate-dir))
              (mkdir-p crate-dir)
              ;; Cargo crates are simply gzipped tarballs but with a .crate
              ;; extension. We expand the source to a directory name we control
              ;; so that we can generate any cargo checksums.
              ;; The --strip-components argument is needed to prevent creating
              ;; an extra directory within `crate-dir`.
              (invoke "tar" "xvf" path "-C" crate-dir "--strip-components" "1")))))
    inputs)

  ;; Configure cargo to actually use this new directory.
  (setenv "CARGO_HOME" (string-append (getcwd) "/.cargo"))
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
  (setenv "LIBGIT2_SYS_USE_PKG_CONFIG" "1")
  (setenv "LIBSSH2_SYS_USE_PKG_CONFIG" "1")
  (when (assoc-ref inputs "openssl")
    (setenv "OPENSSL_DIR" (assoc-ref inputs "openssl")))
  (when (assoc-ref inputs "gettext")
    (setenv "GETTEXT_SYSTEM" (assoc-ref inputs "gettext")))
  (when (assoc-ref inputs "clang")
    (setenv "LIBCLANG_PATH"
            (string-append (assoc-ref inputs "clang") "/lib")))

  ;; We don't use the Cargo.lock file to determine the package versions we use
  ;; during building, and in any case if one is not present it is created
  ;; during the 'build phase by cargo.
  (when (file-exists? "Cargo.lock")
    (delete-file "Cargo.lock"))
  #t)

;; After the 'patch-generated-file-shebangs phase any vendored crates who have
;; their shebangs patched will have a mismatch on their checksum.
(define* (patch-cargo-checksums #:key
                                (vendor-dir "guix-vendor")
                                #:allow-other-keys)
  "Patch the checksums of the vendored crates after patching their shebangs."
  (generate-all-checksums vendor-dir)
  #t)

(define* (build #:key
                skip-build?
                (features '())
                (cargo-build-flags '("--release"))
                #:allow-other-keys)
  "Build a given Cargo package."
  (or skip-build?
      (apply invoke
             `("cargo" "build"
               ,@(if (null? features)
                     '()
                     `("--features" ,(string-join features)))
               ,@cargo-build-flags))))

(define* (check #:key
                tests?
                (cargo-test-flags '("--release"))
                #:allow-other-keys)
  "Run tests for a given Cargo package."
  (if tests?
      (apply invoke "cargo" "test" cargo-test-flags)
      #t))

(define* (package #:key
                  install-source?
                  (cargo-package-flags '("--no-metadata" "--no-verify"))
                  #:allow-other-keys)
  "Run 'cargo-package' for a given Cargo package."
  (if install-source?
    (apply invoke `("cargo" "package" ,@cargo-package-flags))
    (format #t "Not installing cargo sources, skipping `cargo package`.~%"))
  #t)

(define* (install #:key
                  inputs
                  outputs
                  skip-build?
                  install-source?
                  features
                  #:allow-other-keys)
  "Install a given Cargo package."
  (let* ((out      (assoc-ref outputs "out"))
         (registry (string-append out "/share/cargo/registry"))
         (sources  (string-append out "/share/cargo/src")))
    (mkdir-p out)

    ;; Make cargo reuse all the artifacts we just built instead
    ;; of defaulting to making a new temp directory
    (setenv "CARGO_TARGET_DIR" "./target")

    ;; Only install crates which include binary targets,
    ;; otherwise cargo will raise an error.
    (or skip-build?
        (not (has-executable-target?))
        (invoke "cargo" "install" "--no-track" "--path" "." "--root" out
                "--features" (string-join features)))

    (when install-source?
      ;; Install crate tarballs and unpacked sources for later use.
      ;; TODO: Is there a better format/directory for these files?
      (mkdir-p sources)
      (for-each (lambda (crate)
                  (install-file crate registry))
                (find-files "target/package" "\\.crate$"))

      (for-each (lambda (crate)
                  (invoke "tar" "xzf" crate "-C" sources))
                (find-files registry "\\.crate$")))

    #t))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'build 'package package)
    (add-after 'unpack 'unpack-rust-crates unpack-rust-crates)
    (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums patch-cargo-checksums)))

(define* (cargo-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Cargo package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; cargo-build-system.scm ends here
