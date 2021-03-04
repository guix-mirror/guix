;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-transformations)
  #:use-module (guix tests)
  #:use-module (guix store)
  #:use-module ((guix gexp) #:select (lower-object))
  #:use-module ((guix profiles)
                #:select (package->manifest-entry
                          manifest-entry-properties))
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix transformations)
  #:use-module ((guix gexp) #:select (local-file? local-file-file))
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix git)
  #:use-module (guix upstream)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages busybox)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))


(test-begin "transformations")

(test-assert "options->transformation, no transformations"
  (let ((p (dummy-package "foo"))
        (t (options->transformation '())))
    (eq? (t p) p)))

(test-assert "options->transformation, with-source"
  ;; Our pseudo-package is called 'guix.scm' so the 'guix.scm' source should
  ;; be applicable.
  (let* ((p (dummy-package "guix.scm"))
         (s (search-path %load-path "guix.scm"))
         (t (options->transformation `((with-source . ,s)))))
    (with-store store
      (let* ((new (t p))
             (source (run-with-store store
                       (lower-object (package-source new)))))
        (and (not (eq? new p))
             (string=? source
                       (add-to-store store "guix.scm" #t
                                     "sha256" s)))))))

(test-assert "options->transformation, with-source, replacement"
  ;; Same, but this time the original package has a 'replacement' field.  We
  ;; expect that replacement to be set to #f in the new package.
  (let* ((p (dummy-package "guix.scm" (replacement coreutils)))
         (s (search-path %load-path "guix.scm"))
         (t (options->transformation `((with-source . ,s)))))
    (let ((new (t p)))
      (and (not (eq? new p))
           (not (package-replacement new))))))

(test-assert "options->transformation, with-source, with version"
  ;; Our pseudo-package is called 'guix.scm' so the 'guix.scm-2.0' source
  ;; should be applicable, and its version should be extracted.
  (let ((p (dummy-package "foo"))
        (s (search-path %load-path "guix.scm")))
    (call-with-temporary-directory
     (lambda (directory)
       (let* ((f (string-append directory "/foo-42.0.tar.gz"))
              (t (options->transformation `((with-source . ,f)))))
         (copy-file s f)
         (with-store store
           (let* ((new (t p))
                  (source (run-with-store store
                            (lower-object (package-source new)))))
             (and (not (eq? new p))
                  (string=? (package-name new) (package-name p))
                  (string=? (package-version new) "42.0")
                  (string=? source
                            (add-to-store store (basename f) #t
                                          "sha256" f))))))))))

(test-assert "options->transformation, with-source, no matches"
  ;; When a transformation in not applicable, a warning must be raised.
  (let* ((p (dummy-package "foobar"))
         (s (search-path %load-path "guix.scm"))
         (t (options->transformation `((with-source . ,s)))))
    (let* ((port (open-output-string))
           (new  (parameterize ((guix-warning-port port))
                   (t p))))
      (and (eq? new p)
           (string-contains (get-output-string port)
                            "had no effect")))))

(test-assert "options->transformation, with-source, PKG=URI"
  (let* ((p (dummy-package "foo"))
         (s (search-path %load-path "guix.scm"))
         (f (string-append "foo=" s))
         (t (options->transformation `((with-source . ,f)))))
    (with-store store
      (let* ((new (t p))
             (source (run-with-store store
                       (lower-object (package-source new)))))
        (and (not (eq? new p))
             (string=? (package-name new) (package-name p))
             (string=? (package-version new)
                       (package-version p))
             (string=? source
                       (add-to-store store (basename s) #t
                                     "sha256" s)))))))

(test-assert "options->transformation, with-source, PKG@VER=URI"
  (let* ((p (dummy-package "foo"))
         (s (search-path %load-path "guix.scm"))
         (f (string-append "foo@42.0=" s))
         (t (options->transformation `((with-source . ,f)))))
    (with-store store
      (let* ((new (t p))
             (source (run-with-store store
                       (lower-object (package-source new)))))
        (and (not (eq? new p))
             (string=? (package-name new) (package-name p))
             (string=? (package-version new) "42.0")
             (string=? source
                       (add-to-store store (basename s) #t
                                     "sha256" s)))))))

(test-assert "options->transformation, with-input"
  (let* ((p (dummy-package "guix.scm"
              (inputs `(("foo" ,(specification->package "coreutils"))
                        ("bar" ,(specification->package "grep"))
                        ("baz" ,(dummy-package "chbouib"
                                  (native-inputs `(("x" ,grep)))))))))
         (t (options->transformation '((with-input . "coreutils=busybox")
                                       (with-input . "grep=findutils")))))
    (let ((new (t p)))
      (and (not (eq? new p))
           (match (package-inputs new)
             ((("foo" dep1) ("bar" dep2) ("baz" dep3))
              (and (string=? (package-full-name dep1)
                             (package-full-name busybox))
                   (string=? (package-full-name dep2)
                             (package-full-name findutils))
                   (string=? (package-name dep3) "chbouib")
                   (match (package-native-inputs dep3)
                     ((("x" dep))
                      (string=? (package-full-name dep)
                                (package-full-name findutils)))))))))))

(test-assert "options->transformation, with-graft"
  (let* ((p (dummy-package "guix.scm"
              (inputs `(("foo" ,grep)
                        ("bar" ,(dummy-package "chbouib"
                                  (native-inputs `(("x" ,grep)))))))))
         (t (options->transformation '((with-graft . "grep=findutils")))))
    (let ((new (t p)))
      (and (not (eq? new p))
           (match (package-inputs new)
             ((("foo" dep1) ("bar" dep2))
              (and (string=? (package-full-name dep1)
                             (package-full-name grep))
                   (string=? (package-full-name (package-replacement dep1))
                             (package-full-name findutils))
                   (string=? (package-name dep2) "chbouib")
                   (match (package-native-inputs dep2)
                     ((("x" dep))
                      (with-store store
                        (string=? (derivation-file-name
                                   (package-derivation store findutils))
                                  (derivation-file-name
                                   (package-derivation store dep)))))))))))))

(test-equal "options->transformation, with-branch"
  (git-checkout (url "https://example.org")
                (branch "devel")
                (recursive? #t))
  (let* ((p (dummy-package "guix.scm"
              (inputs `(("foo" ,grep)
                        ("bar" ,(dummy-package "chbouib"
                                  (source (origin
                                            (method git-fetch)
                                            (uri (git-reference
                                                  (url "https://example.org")
                                                  (commit "cabba9e")))
                                            (sha256 #f)))))))))
         (t (options->transformation '((with-branch . "chbouib=devel")))))
    (let ((new (t p)))
      (and (not (eq? new p))
           (match (package-inputs new)
             ((("foo" dep1) ("bar" dep2))
              (and (string=? (package-full-name dep1)
                             (package-full-name grep))
                   (string=? (package-name dep2) "chbouib")
                   (package-source dep2))))))))

(test-equal "options->transformation, with-commit"
  (git-checkout (url "https://example.org")
                (commit "abcdef")
                (recursive? #t))
  (let* ((p (dummy-package "guix.scm"
              (inputs `(("foo" ,grep)
                        ("bar" ,(dummy-package "chbouib"
                                  (source (origin
                                            (method git-fetch)
                                            (uri (git-reference
                                                  (url "https://example.org")
                                                  (commit "cabba9e")))
                                            (sha256 #f)))))))))
         (t (options->transformation '((with-commit . "chbouib=abcdef")))))
    (let ((new (t p)))
      (and (not (eq? new p))
           (match (package-inputs new)
             ((("foo" dep1) ("bar" dep2))
              (and (string=? (package-full-name dep1)
                             (package-full-name grep))
                   (string=? (package-name dep2) "chbouib")
                   (package-source dep2))))))))

(test-equal "options->transformation, with-git-url"
  (let ((source (git-checkout (url "https://example.org")
                              (recursive? #t))))
    (list source source))
  (let* ((p (dummy-package "guix.scm"
              (inputs `(("foo" ,grep)
                        ("bar" ,(dummy-package "chbouib"
                                  (native-inputs `(("x" ,grep)))))))))
         (t (options->transformation '((with-git-url . "grep=https://example.org")))))
    (let ((new (t p)))
      (and (not (eq? new p))
           (match (package-inputs new)
             ((("foo" dep1) ("bar" dep2))
              (and (string=? (package-full-name dep1)
                             (package-full-name grep))
                   (string=? (package-name dep2) "chbouib")
                   (match (package-native-inputs dep2)
                     ((("x" dep3))
                      (map package-source (list dep1 dep3)))))))))))

(test-equal "options->transformation, with-git-url + with-branch"
  ;; Combine the two options and make sure the 'with-branch' transformation
  ;; comes after the 'with-git-url' transformation.
  (let ((source (git-checkout (url "https://example.org")
                              (branch "BRANCH")
                              (recursive? #t))))
    (list source source))
  (let* ((p (dummy-package "guix.scm"
              (inputs `(("foo" ,grep)
                        ("bar" ,(dummy-package "chbouib"
                                  (native-inputs `(("x" ,grep)))))))))
         (t (options->transformation
             (reverse '((with-git-url
                         . "grep=https://example.org")
                        (with-branch . "grep=BRANCH"))))))
    (let ((new (t p)))
      (and (not (eq? new p))
           (match (package-inputs new)
             ((("foo" dep1) ("bar" dep2))
              (and (string=? (package-name dep1) "grep")
                   (string=? (package-name dep2) "chbouib")
                   (match (package-native-inputs dep2)
                     ((("x" dep3))
                      (map package-source (list dep1 dep3)))))))))))

(define* (depends-on-toolchain? p #:optional (toolchain "gcc-toolchain"))
  "Return true if P depends on TOOLCHAIN instead of the default tool chain."
  (define toolchain-packages
    '("gcc" "binutils" "glibc" "ld-wrapper"))

  (define (package-name* obj)
    (and (package? obj) (package-name obj)))

  (match (bag-build-inputs (package->bag p))
    (((_ (= package-name* packages) . _) ...)
     (and (not (any (cut member <> packages) toolchain-packages))
          (member toolchain packages)))))

(test-assert "options->transformation, with-c-toolchain"
  (let* ((dep0 (dummy-package "chbouib"
                 (build-system gnu-build-system)
                 (native-inputs `(("y" ,grep)))))
         (dep1 (dummy-package "stuff"
                 (native-inputs `(("x" ,dep0)))))
         (p    (dummy-package "thingie"
                 (build-system gnu-build-system)
                 (inputs `(("foo" ,grep)
                           ("bar" ,dep1)))))
         (t    (options->transformation
                '((with-c-toolchain . "chbouib=gcc-toolchain")))))
    ;; Here we check that the transformation applies to DEP0 and all its
    ;; dependents: DEP0 must use GCC-TOOLCHAIN, DEP1 must use GCC-TOOLCHAIN
    ;; and the DEP0 that uses GCC-TOOLCHAIN, and so on.
    (let ((new (t p)))
      (and (depends-on-toolchain? new "gcc-toolchain")
           (match (bag-build-inputs (package->bag new))
             ((("foo" dep0) ("bar" dep1) _ ...)
              (and (depends-on-toolchain? dep1 "gcc-toolchain")
                   (not (depends-on-toolchain? dep0 "gcc-toolchain"))
                   (string=? (package-full-name dep0)
                             (package-full-name grep))
                   (match (bag-build-inputs (package->bag dep1))
                     ((("x" dep) _ ...)
                      (and (depends-on-toolchain? dep "gcc-toolchain")
                           (match (bag-build-inputs (package->bag dep))
                             ((("y" dep) _ ...)   ;this one is unchanged
                              (eq? dep grep)))))))))))))

(test-equal "options->transformation, with-c-toolchain twice"
  (package-full-name grep)
  (let* ((dep0 (dummy-package "chbouib"))
         (dep1 (dummy-package "stuff"))
         (p    (dummy-package "thingie"
                 (build-system gnu-build-system)
                 (inputs `(("foo" ,dep0)
                           ("bar" ,dep1)
                           ("baz" ,grep)))))
         (t    (options->transformation
                '((with-c-toolchain . "chbouib=clang-toolchain")
                  (with-c-toolchain . "stuff=clang-toolchain")))))
    (let ((new (t p)))
      (and (depends-on-toolchain? new "clang-toolchain")
           (match (bag-build-inputs (package->bag new))
             ((("foo" dep0) ("bar" dep1) ("baz" dep2) _ ...)
              (and (depends-on-toolchain? dep0 "clang-toolchain")
                   (depends-on-toolchain? dep1 "clang-toolchain")
                   (not (depends-on-toolchain? dep2 "clang-toolchain"))
                   (package-full-name dep2))))))))

(test-assert "options->transformation, with-c-toolchain, no effect"
  (let ((p (dummy-package "thingie"))
        (t (options->transformation
            '((with-c-toolchain . "does-not-exist=gcc-toolchain")))))
    ;; When it has no effect, '--with-c-toolchain' returns P.
    (eq? (t p) p)))

(test-equal "options->transformation, with-debug-info"
  '(#:strip-binaries? #f)
  (let* ((dep  (dummy-package "chbouib"))
         (p    (dummy-package "thingie"
                 (build-system gnu-build-system)
                 (inputs `(("foo" ,dep)
                           ("bar" ,grep)))))
         (t    (options->transformation
                '((with-debug-info . "chbouib")))))
    (let ((new (t p)))
      (match (package-inputs new)
        ((("foo" dep0) ("bar" dep1))
         (and (string=? (package-full-name dep1)
                        (package-full-name grep))
              (package-arguments (package-replacement dep0))))))))

(test-assert "options->transformation, without-tests"
  (let* ((dep (dummy-package "dep"))
         (p   (dummy-package "foo"
                (inputs `(("dep" ,dep)))))
         (t   (options->transformation '((without-tests . "dep")
                                         (without-tests . "tar")))))
    (let ((new (t p)))
      (match (bag-direct-inputs (package->bag new))
        ((("dep" dep) ("tar" tar) _ ...)
         (and (equal? (package-arguments dep) '(#:tests? #f))
              (match (memq #:tests? (package-arguments tar))
                ((#:tests? #f _ ...) #t))))))))

(test-equal "options->transformation, with-patch"
  (search-patches "glibc-locales.patch" "guile-relocatable.patch")
  (let* ((dep    (dummy-package "dep"
                   (source (dummy-origin))))
         (p      (dummy-package "foo"
                   (inputs `(("dep" ,dep)))))
         (patch1 (search-patch "glibc-locales.patch"))
         (patch2 (search-patch "guile-relocatable.patch"))
         (t      (options->transformation
                  `((with-patch . ,(string-append "dep=" patch1))
                    (with-patch . ,(string-append "dep=" patch2))
                    (with-patch . ,(string-append "tar=" patch1))))))
    (let ((new (t p)))
      (match (bag-direct-inputs (package->bag new))
        ((("dep" dep) ("tar" tar) _ ...)
         (and (member patch1
                      (filter-map (lambda (patch)
                                    (and (local-file? patch)
                                         (local-file-file patch)))
                                  (origin-patches (package-source tar))))
              (map local-file-file
                   (origin-patches (package-source dep)))))))))

(test-equal "options->transformation, with-latest"
  "42.0"
  (mock ((guix upstream) %updaters
         (delay (list (upstream-updater
                       (name 'dummy)
                       (pred (const #t))
                       (description "")
                       (latest (const (upstream-source
                                       (package "foo")
                                       (version "42.0")
                                       (urls '("http://example.org")))))))))
        (let* ((p (dummy-package "foo" (version "1.0")))
               (t (options->transformation
                   `((with-latest . "foo")))))
          (package-version (t p)))))

(test-equal "options->transformation + package->manifest-entry"
  '((transformations . ((without-tests . "foo"))))
  (let* ((p (dummy-package "foo"))
         (t (options->transformation '((without-tests . "foo"))))
         (e (package->manifest-entry (t p))))
    (manifest-entry-properties e)))

(test-end)

;;; Local Variables:
;;; eval: (put 'dummy-package 'scheme-indent-function 1)
;;; End:
