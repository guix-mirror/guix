;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-scripts-build)
  #:use-module (guix tests)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix scripts build)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix git)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages busybox)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))


(test-begin "scripts-build")

(test-assert "options->transformation, no transformations"
  (let ((p (dummy-package "foo"))
        (t (options->transformation '())))
    (with-store store
      (eq? (t store p) p))))

(test-assert "options->transformation, with-source"
  ;; Our pseudo-package is called 'guix.scm' so the 'guix.scm' source should
  ;; be applicable.
  (let* ((p (dummy-package "guix.scm"))
         (s (search-path %load-path "guix.scm"))
         (t (options->transformation `((with-source . ,s)))))
    (with-store store
      (let ((new (t store p)))
        (and (not (eq? new p))
             (string=? (package-source new)
                       (add-to-store store "guix.scm" #t
                                     "sha256" s)))))))

(test-assert "options->transformation, with-source, replacement"
  ;; Same, but this time the original package has a 'replacement' field.  We
  ;; expect that replacement to be set to #f in the new package.
  (let* ((p (dummy-package "guix.scm" (replacement coreutils)))
         (s (search-path %load-path "guix.scm"))
         (t (options->transformation `((with-source . ,s)))))
    (with-store store
      (let ((new (t store p)))
        (and (not (eq? new p))
             (string=? (package-source new)
                       (add-to-store store "guix.scm" #t "sha256" s))
             (not (package-replacement new)))))))

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
           (let ((new (t store p)))
             (and (not (eq? new p))
                  (string=? (package-name new) (package-name p))
                  (string=? (package-version new) "42.0")
                  (string=? (package-source new)
                            (add-to-store store (basename f) #t
                                          "sha256" f))))))))))

(test-assert "options->transformation, with-source, no matches"
  ;; When a transformation in not applicable, a warning must be raised.
  (let* ((p (dummy-package "foobar"))
         (s (search-path %load-path "guix.scm"))
         (t (options->transformation `((with-source . ,s)))))
    (with-store store
      (let* ((port (open-output-string))
             (new  (parameterize ((guix-warning-port port))
                     (t store p))))
        (and (eq? new p)
             (string-contains (get-output-string port)
                              "had no effect"))))))

(test-assert "options->transformation, with-source, PKG=URI"
  (let* ((p (dummy-package "foo"))
         (s (search-path %load-path "guix.scm"))
         (f (string-append "foo=" s))
         (t (options->transformation `((with-source . ,f)))))
    (with-store store
      (let ((new (t store p)))
        (and (not (eq? new p))
             (string=? (package-name new) (package-name p))
             (string=? (package-version new)
                       (package-version p))
             (string=? (package-source new)
                       (add-to-store store (basename s) #t
                                     "sha256" s)))))))

(test-assert "options->transformation, with-source, PKG@VER=URI"
  (let* ((p (dummy-package "foo"))
         (s (search-path %load-path "guix.scm"))
         (f (string-append "foo@42.0=" s))
         (t (options->transformation `((with-source . ,f)))))
    (with-store store
      (let ((new (t store p)))
        (and (not (eq? new p))
             (string=? (package-name new) (package-name p))
             (string=? (package-version new) "42.0")
             (string=? (package-source new)
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
    (with-store store
      (let ((new (t store p)))
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
                                  (package-full-name findutils))))))))))))

(test-assert "options->transformation, with-graft"
  (let* ((p (dummy-package "guix.scm"
              (inputs `(("foo" ,grep)
                        ("bar" ,(dummy-package "chbouib"
                                  (native-inputs `(("x" ,grep)))))))))
         (t (options->transformation '((with-graft . "grep=findutils")))))
    (with-store store
      (let ((new (t store p)))
        (and (not (eq? new p))
             (match (package-inputs new)
               ((("foo" dep1) ("bar" dep2))
                (and (string=? (package-full-name dep1)
                               (package-full-name grep))
                     (eq? (package-replacement dep1) findutils)
                     (string=? (package-name dep2) "chbouib")
                     (match (package-native-inputs dep2)
                       ((("x" dep))
                        (eq? (package-replacement dep) findutils)))))))))))

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
    (with-store store
      (let ((new (t store p)))
        (and (not (eq? new p))
             (match (package-inputs new)
               ((("foo" dep1) ("bar" dep2))
                (and (string=? (package-full-name dep1)
                               (package-full-name grep))
                     (string=? (package-name dep2) "chbouib")
                     (package-source dep2)))))))))

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
    (with-store store
      (let ((new (t store p)))
        (and (not (eq? new p))
             (match (package-inputs new)
               ((("foo" dep1) ("bar" dep2))
                (and (string=? (package-full-name dep1)
                               (package-full-name grep))
                     (string=? (package-name dep2) "chbouib")
                     (package-source dep2)))))))))

(test-equal "options->transformation, with-git-url"
  (let ((source (git-checkout (url "https://example.org")
                              (recursive? #t))))
    (list source source))
  (let* ((p (dummy-package "guix.scm"
              (inputs `(("foo" ,grep)
                        ("bar" ,(dummy-package "chbouib"
                                  (native-inputs `(("x" ,grep)))))))))
         (t (options->transformation '((with-git-url . "grep=https://example.org")))))
    (with-store store
      (let ((new (t store p)))
        (and (not (eq? new p))
             (match (package-inputs new)
               ((("foo" dep1) ("bar" dep2))
                (and (string=? (package-full-name dep1)
                               (package-full-name grep))
                     (string=? (package-name dep2) "chbouib")
                     (match (package-native-inputs dep2)
                       ((("x" dep3))
                        (map package-source (list dep1 dep3))))))))))))

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
    (with-store store
      (let ((new (t store p)))
        (and (not (eq? new p))
             (match (package-inputs new)
               ((("foo" dep1) ("bar" dep2))
                (and (string=? (package-name dep1) "grep")
                     (string=? (package-name dep2) "chbouib")
                     (match (package-native-inputs dep2)
                       ((("x" dep3))
                        (map package-source (list dep1 dep3))))))))))))


(test-end)
