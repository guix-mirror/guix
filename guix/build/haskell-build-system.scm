;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2021 John Kehayias <john.kehayias@protonmail.com>
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

(define-module (guix build haskell-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 ftw)
  #:export (%standard-phases
            haskell-build))

;; Commentary:
;;
;; Builder-side code of the standard Haskell package build procedure.
;;
;; The Haskell compiler, to find libraries, relies on a library database with
;; a binary cache. For GHC the cache has to be named 'package.cache'. If every
;; library would generate the cache at build time, then they would clash in
;; profiles. For this reason we do not generate the cache when we generate
;; libraries substitutes. Instead:
;;
;; - At build time we use the 'setup-compiler' phase to generate a temporary
;;   library database and its cache.
;;
;; - We generate the cache when a profile is created.
;;
;; Code:

;; Directory where we create the temporary libraries database with its cache
;; as required by the compiler.
(define %tmp-db-dir
  (string-append (or (getenv "TMP") "/tmp")
                 "/package.conf.d"))

(define (run-setuphs command params)
  (let ((setup-file (cond
                     ((file-exists? "Setup.hs")
                      "Setup.hs")
                     ((file-exists? "Setup.lhs")
                      "Setup.lhs")
                     (else
                      #f)))
        (pkgdb (string-append "-package-db=" %tmp-db-dir)))
    (if setup-file
        (begin
          (format #t "running \"runhaskell Setup.hs\" with command ~s \
and parameters ~s~%"
                  command params)
          (apply invoke "runhaskell" pkgdb setup-file command params))
        (error "no Setup.hs nor Setup.lhs found"))))

(define* (configure #:key outputs inputs tests? (configure-flags '())
                    (extra-directories '()) #:allow-other-keys)
  "Configure a given Haskell package."
  (let* ((out (assoc-ref outputs "out"))
         (doc (assoc-ref outputs "doc"))
         (lib (assoc-ref outputs "lib"))
         (name-version (strip-store-file-name out))
         (extra-dirs (filter-map (cut assoc-ref inputs <>) extra-directories))
         (ghc-path (getenv "GHC_PACKAGE_PATH"))
         (params `(,(string-append "--prefix=" out)
                   ,(string-append "--libdir=" (or lib out) "/lib")
                   ,(string-append "--docdir=" (or doc out)
                                   "/share/doc/" name-version)
                   "--libsubdir=$compiler/$pkg-$version"
                   ,(string-append "--package-db=" %tmp-db-dir)
                   "--global"
                   ,@(map (cut string-append "--extra-include-dirs=" <>)
                          (search-path-as-list '("include") extra-dirs))
                   ,@(map (cut string-append "--extra-lib-dirs=" <>)
                          (search-path-as-list '("lib") extra-dirs))
                   ,@(if tests?
                         '("--enable-tests")
                         '())
                   ;; Build and link with shared libraries
                   "--enable-shared"
                   "--enable-executable-dynamic"
                   "--ghc-option=-fPIC"
                   ,(string-append "--ghc-option=-optl=-Wl,-rpath=" (or lib out)
                                   "/lib/$compiler/$pkg-$version")
                   ,@configure-flags)))
    ;; Cabal errors if GHC_PACKAGE_PATH is set during 'configure', so unset
    ;; and restore it.
    (unsetenv "GHC_PACKAGE_PATH")

    ;; For packages where the Cabal build-type is set to "Configure",
    ;; ./configure will be executed.  In these cases, the following
    ;; environment variable is needed to be able to find the shell executable.
    ;; For other package types, the configure script isn't present.  For more
    ;; information, see the Build Information section of
    ;; <https://www.haskell.org/cabal/users-guide/developing-packages.html>.
    (when (file-exists? "configure")
      (setenv "CONFIG_SHELL" "sh"))
    (run-setuphs "configure" params)

    (setenv "GHC_PACKAGE_PATH" ghc-path)
    #t))

(define* (build #:key parallel-build? #:allow-other-keys)
  "Build a given Haskell package."
  (run-setuphs "build"
               (if parallel-build?
                   `(,(string-append "--ghc-option=-j" (number->string (parallel-job-count))))
                   '())))

(define* (install #:key outputs #:allow-other-keys)
  "Install a given Haskell package."
  (run-setuphs "copy" '())
  (when (assoc-ref outputs "static")
    (let ((static (assoc-ref outputs "static"))
          (lib (or (assoc-ref outputs "lib")
                   (assoc-ref outputs "out"))))
      (for-each (lambda (static-lib)
                  (let* ((subdir (string-drop static-lib (string-length lib)))
                         (new    (string-append static subdir)))
                    (mkdir-p (dirname new))
                    (rename-file static-lib new)))
                (find-files lib "\\.a$"))))
  #t)

(define* (setup-compiler #:key system inputs outputs #:allow-other-keys)
  "Setup the compiler environment."
  (let* ((haskell (assoc-ref inputs "haskell"))
         (name-version (strip-store-file-name haskell)))
    (cond
     ((string-match "ghc" name-version)
      (make-ghc-package-database system inputs outputs))
     (else
      (format #t
              "Compiler ~a not supported~%" name-version)))))

;;; TODO: Move this to (guix build utils)?
(define-syntax-rule (with-null-error-port exp)
  "Evaluate EXP with the error port pointing to the bit bucket."
  (with-error-to-port (%make-void-port "w")
    (lambda () exp)))

(define (make-ghc-package-database system inputs outputs)
  "Generate the GHC package database."
  (let* ((haskell  (assoc-ref inputs "haskell"))
         (name-version (strip-store-file-name haskell))
         ;; Silence 'find-files' (see 'evaluate-search-paths')
         (conf-dirs (search-path-as-string->list (getenv "GHC_PACKAGE_PATH")))
         (conf-files (append-map (cut find-files <> "\\.conf$") conf-dirs)))
    (mkdir-p %tmp-db-dir)
    (for-each (lambda (file)
                (let ((dest (string-append %tmp-db-dir "/" (basename file))))
                  (unless (file-exists? dest)
                    (copy-file file dest))))
              conf-files)
    (invoke "ghc-pkg"
            (string-append "--package-db=" %tmp-db-dir)
            "recache")
    #t))

(define* (register #:key name system inputs outputs #:allow-other-keys)
  "Generate the compiler registration and binary package database files for a
given Haskell package."

  (define (conf-depends conf-file)
    ;; Return a list of pkg-ids from the "depends" field in CONF-FILE
    (let ((port (open-input-file conf-file))
          (field-rx (make-regexp "^(.*):")))
      (let loop ((collecting #f)
                 (deps '()))
        (let* ((line (read-line port))
               (field (and=> (and (not (eof-object? line))
                                  (regexp-exec field-rx line))
                             (cut match:substring <> 1))))
          (cond
           ((and=> field (cut string=? <> "depends"))
            ;; The first dependency is listed on the same line as "depends:",
            ;; so drop those characters.  A line may list more than one .conf.
            (let ((d (string-tokenize (string-drop line 8))))
              (loop #t (append d deps))))
           ((or (eof-object? line) (and collecting field))
            (begin
              (close-port port)
              (reverse! deps)))
           (collecting
            (loop #t (append (string-tokenize line) deps)))
           (else (loop #f deps)))))))

  (define (install-transitive-deps conf-file src dest)
    ;; Copy .conf files from SRC to DEST for dependencies in CONF-FILE, and
    ;; their dependencies, etc.
    (let loop ((seen vlist-null)
               (lst (conf-depends conf-file)))
      (match lst
        (() #t)                         ;done
        ((id . tail)
         (if (not (vhash-assoc id seen))
             (let ((dep-conf  (string-append src  "/" id ".conf"))
                   (dep-conf* (string-append dest "/" id ".conf")))
               (when (not (file-exists? dep-conf))
                   (error (format #f "File ~a does not exist. This usually means the dependency ~a is missing. Was checking conf-file ~a." dep-conf id conf-file)))
               (copy-file dep-conf dep-conf*) ;XXX: maybe symlink instead?
               (loop (vhash-cons id #t seen)
                     (append lst (conf-depends dep-conf))))
             (loop seen tail))))))

  (let* ((out (assoc-ref outputs "out"))
         (doc (assoc-ref outputs "doc"))
         (haskell  (assoc-ref inputs "haskell"))
         (name-version (strip-store-file-name haskell))
         (version (last (string-split name-version #\-)))
         (lib (string-append (or (assoc-ref outputs "lib") out) "/lib"))
         (config-dir (string-append lib
                                    "/ghc-" version
                                    "/" name ".conf.d"))
         (id-rx (make-regexp "^id:[ \n\t]+([^ \t\n]+)$" regexp/newline))
         (config-file (string-append out "/" name ".conf"))
         (params
          (list (string-append "--gen-pkg-config=" config-file))))
    (run-setuphs "register" params)
    ;; The conf file is created only when there is a library to register.
    (when (file-exists? config-file)
      (mkdir-p config-dir)
      (let* ((contents (call-with-input-file config-file read-string))
             (config-file-name+id (match:substring (first (list-matches id-rx contents)) 1)))

        (when (or
                (and
                  (string? config-file-name+id)
                  (string-null? config-file-name+id))
                (not config-file-name+id))
          (error (format #f "The package id for ~a is empty. This is a bug." config-file)))

        ;; Remove reference to "doc" output from "lib" (or "out") by rewriting the
        ;; "haddock-interfaces" field and removing the optional "haddock-html"
        ;; field in the generated .conf file.
        (when doc
          (substitute* config-file
            (("^haddock-html: .*") "\n")
            (((format #f "^haddock-interfaces: ~a" doc))
             (string-append "haddock-interfaces: " lib)))
          ;; Move the referenced file to the "lib" (or "out") output.
          (match (find-files doc "\\.haddock$")
            ((haddock-file . rest)
             (let* ((subdir (string-drop haddock-file (string-length doc)))
                    (new    (string-append lib subdir)))
               (mkdir-p (dirname new))
               (rename-file haddock-file new)))
            (_ #f)))
        (install-transitive-deps config-file %tmp-db-dir config-dir)
        (rename-file config-file
                     (string-append config-dir "/"
                                    config-file-name+id ".conf"))
        (invoke "ghc-pkg"
                (string-append "--package-db=" config-dir)
                "recache")))
    #t))

(define* (check #:key tests? test-target #:allow-other-keys)
  "Run the test suite of a given Haskell package."
  (if tests?
      (run-setuphs test-target '())
      (format #t "test suite not run~%"))
  #t)

(define* (haddock #:key outputs haddock? haddock-flags #:allow-other-keys)
  "Generate the Haddock documentation of a given Haskell package."
  (when haddock?
    (run-setuphs "haddock" haddock-flags))
  #t)

(define* (patch-cabal-file #:key cabal-revision #:allow-other-keys)
  (when cabal-revision
    ;; Cabal requires there to be a single file with the suffix ".cabal".
    (match (scandir "." (cut string-suffix? ".cabal" <>))
      ((original)
       (format #t "replacing ~s with ~s~%" original cabal-revision)
       (copy-file cabal-revision original))
      (_ (error "Could not find a Cabal file to patch."))))
  #t)

(define* (generate-setuphs #:rest empty)
  "Generate a default Setup.hs if needed."
  (when (not (or (file-exists? "Setup.hs")
                 (file-exists? "Setup.lhs")))
    (format #t "generating missing Setup.hs~%")
    (with-output-to-file "Setup.hs"
      (lambda ()
        (format #t "import Distribution.Simple~%")
        (format #t "main = defaultMain~%"))))
  #t)

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (add-after 'unpack 'patch-cabal-file patch-cabal-file)
    (add-after 'unpack 'generate-setuphs generate-setuphs)
    (delete 'bootstrap)
    (add-before 'configure 'setup-compiler setup-compiler)
    (add-before 'install 'haddock haddock)
    (add-after 'install 'register register)
    (replace 'install install)
    (replace 'check check)
    (replace 'build build)
    (replace 'configure configure)))

(define* (haskell-build #:key inputs (phases %standard-phases)
                        #:allow-other-keys #:rest args)
  "Build the given Haskell package, applying all of PHASES in order."
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))

;;; haskell-build-system.scm ends here
