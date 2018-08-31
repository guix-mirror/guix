;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
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
                      #f))))
    (if setup-file
        (begin
          (format #t "running \"runhaskell Setup.hs\" with command ~s \
and parameters ~s~%"
                  command params)
          (apply invoke "runhaskell" setup-file command params))
        (error "no Setup.hs nor Setup.lhs found"))))

(define* (configure #:key outputs inputs tests? (configure-flags '())
                    #:allow-other-keys)
  "Configure a given Haskell package."
  (let* ((out (assoc-ref outputs "out"))
         (doc (assoc-ref outputs "doc"))
         (lib (assoc-ref outputs "lib"))
         (bin (assoc-ref outputs "bin"))
         (input-dirs (match inputs
                       (((_ . dir) ...)
                        dir)
                       (_ '())))
         (ghc-path (getenv "GHC_PACKAGE_PATH"))
         (params (append `(,(string-append "--prefix=" out))
                         `(,(string-append "--libdir=" (or lib out) "/lib"))
                         `(,(string-append "--bindir=" (or bin out) "/bin"))
                         `(,(string-append
                             "--docdir=" (or doc out)
                             "/share/doc/" (package-name-version out)))
                         '("--libsubdir=$compiler/$pkg-$version")
                         `(,(string-append "--package-db=" %tmp-db-dir))
                         '("--global")
                         `(,@(map
                              (cut string-append "--extra-include-dirs=" <>)
                              (search-path-as-list '("include") input-dirs)))
                         `(,@(map
                              (cut string-append "--extra-lib-dirs=" <>)
                              (search-path-as-list '("lib") input-dirs)))
                         (if tests?
                             '("--enable-tests")
                             '())
                         configure-flags)))
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

(define* (build #:rest empty)
  "Build a given Haskell package."
  (run-setuphs "build" '()))

(define* (install #:rest empty)
  "Install a given Haskell package."
  (run-setuphs "copy" '()))

(define (package-name-version store-dir)
  "Given a store directory STORE-DIR return 'name-version' of the package."
  (let* ((base (basename store-dir)))
    (string-drop base
                 (+ 1 (string-index base #\-)))))

(define (grep rx port)
  "Given a regular-expression RX including a group, read from PORT until the
first match and return the content of the group."
  (let ((line (read-line port)))
    (if (eof-object? line)
        #f
        (let ((rx-result (regexp-exec rx line)))
          (if rx-result
              (match:substring rx-result 1)
              (grep rx port))))))

(define* (setup-compiler #:key system inputs outputs #:allow-other-keys)
  "Setup the compiler environment."
  (let* ((haskell (assoc-ref inputs "haskell"))
         (name-version (package-name-version haskell)))
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
         (input-dirs (match inputs
                       (((_ . dir) ...)
                        dir)
                       (_ '())))
         ;; Silence 'find-files' (see 'evaluate-search-paths')
         (conf-dirs (with-null-error-port
                     (search-path-as-list
                      `(,(string-append "lib/" (package-name-version haskell)))
                      input-dirs #:pattern ".*\\.conf.d$")))
         (conf-files (append-map (cut find-files <> "\\.conf$") conf-dirs)))
    (mkdir-p %tmp-db-dir)
    (for-each (lambda (file)
                (let ((dest (string-append %tmp-db-dir "/" (basename file))))
                  (unless (file-exists? dest)
                    (copy-file file dest))))
              conf-files)
    (zero? (system* "ghc-pkg"
                    (string-append "--package-db=" %tmp-db-dir)
                    "recache"))))

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
               (copy-file dep-conf dep-conf*) ;XXX: maybe symlink instead?
               (loop (vhash-cons id #t seen)
                     (append lst (conf-depends dep-conf))))
             (loop seen tail))))))

  (let* ((out (assoc-ref outputs "out"))
         (haskell  (assoc-ref inputs "haskell"))
         (lib (string-append out "/lib"))
         (config-dir (string-append lib "/"
                                    (package-name-version haskell)
                                    "/" name ".conf.d"))
         (id-rx (make-regexp "^id: *(.*)$"))
         (config-file (string-append out "/" name ".conf"))
         (params
          (list (string-append "--gen-pkg-config=" config-file))))
    (run-setuphs "register" params)
    ;; The conf file is created only when there is a library to register.
    (or (not (file-exists? config-file))
        (begin
          (mkdir-p config-dir)
          (let* ((config-file-name+id
                  (call-with-ascii-input-file config-file (cut grep id-rx <>))))
            (install-transitive-deps config-file %tmp-db-dir config-dir)
            (rename-file config-file
                         (string-append config-dir "/"
                                        config-file-name+id ".conf"))
            (zero? (system* "ghc-pkg"
                            (string-append "--package-db=" config-dir)
                            "recache")))))))

(define* (check #:key tests? test-target #:allow-other-keys)
  "Run the test suite of a given Haskell package."
  (if tests?
      (run-setuphs test-target '())
      (begin
        (format #t "test suite not run~%")
        #t)))

(define* (haddock #:key outputs haddock? haddock-flags #:allow-other-keys)
  "Run the test suite of a given Haskell package."
  (if haddock?
      (run-setuphs "haddock" haddock-flags)
      #t))

(define* (patch-cabal-file #:key cabal-revision #:allow-other-keys)
  (when cabal-revision
    ;; Cabal requires there to be a single file with the suffix ".cabal".
    (match (scandir "." (cut string-suffix? ".cabal" <>))
      ((original)
       (format #t "replacing ~s with ~s~%" original cabal-revision)
       (copy-file cabal-revision original))
      (_ (error "Could not find a Cabal file to patch."))))
  #t)

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (add-after 'unpack 'patch-cabal-file patch-cabal-file)
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
