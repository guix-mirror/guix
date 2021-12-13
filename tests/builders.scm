;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
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


(define-module (tests builders)
  #:use-module (guix download)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build gnu-build-system)
  #:use-module (guix build utils)
  #:use-module (guix build-system python)
  #:use-module (guix grafts)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix base32)
  #:use-module (guix derivations)
  #:use-module (gcrypt hash)
  #:use-module (guix tests)
  #:use-module (guix packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

;; Test the higher-level builders.

(define %store
  (open-connection-for-tests))

(define url-fetch*
  (store-lower url-fetch))

;; Globally disable grafts because they can trigger early builds.
(%graft? #f)


(test-begin "builders")

(unless (network-reachable?) (test-skip 1))
(test-assert "url-fetch"
  (let* ((url      '("http://ftp.gnu.org/gnu/hello/hello-2.8.tar.gz"
                     "ftp://ftp.gnu.org/gnu/hello/hello-2.8.tar.gz"))
         (hash     (nix-base32-string->bytevector
                    "0wqd8sjmxfskrflaxywc7gqw7sfawrfvdxd9skxawzfgyy0pzdz6"))
         (drv      (url-fetch* %store url 'sha256 hash
                               #:guile %bootstrap-guile))
         (out-path (derivation->output-path drv)))
    (and (build-derivations %store (list drv))
         (file-exists? out-path)
         (valid-path? %store out-path))))

(test-assert "url-fetch, file"
  (let* ((file (search-path %load-path "guix.scm"))
         (hash (call-with-input-file file port-sha256))
         (out  (url-fetch* %store file 'sha256 hash)))
    (and (file-exists? out)
         (valid-path? %store out))))

(test-assert "url-fetch, file URI"
  (let* ((file (search-path %load-path "guix.scm"))
         (hash (call-with-input-file file port-sha256))
         (out  (url-fetch* %store
                           (string-append "file://" (canonicalize-path file))
                           'sha256 hash)))
    (and (file-exists? out)
         (valid-path? %store out))))

(test-assert "gnu-build-system"
  (build-system? gnu-build-system))

(define unpack (assoc-ref %standard-phases 'unpack))

(define compressors '(("gzip"  . "gz")
                      ("xz"    . "xz")
                      ("bzip2" . "bz2")
                      (#f      . #f)))

(for-each
 (match-lambda
   ((comp . ext)

    (unless (network-reachable?) (test-skip 1)) ;for bootstrap binaries
    (test-equal (string-append "gnu-build-system unpack phase, "
                               "single file (compression: "
                               (if comp comp "None") ")")
      "expected text"
      (let*-values
          (((name) "test")
           ((compressed-name) (if ext
                                  (string-append name "." ext)
                                  name))
           ((file hash) (test-file %store compressed-name "expected text")))
        (call-with-temporary-directory
         (lambda (dir)
           (with-directory-excursion dir
             (unpack #:source file)
             (call-with-input-file name get-string-all))))))))
 compressors)


;;;
;;; Test the sanity-check phase of the Python build system.
;;;

(define* (make-python-dummy name #:key (setup-py-extra "")
                            (init-py "") (use-setuptools? #t))
  (dummy-package (string-append "python-dummy-" name)
    (version "0.1")
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:use-setuptools? ,use-setuptools?
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda _
             (mkdir-p "dummy")
             (with-output-to-file "dummy/__init__.py"
               (lambda _
                 (display ,init-py)))
             (with-output-to-file "setup.py"
               (lambda _
                 (format #t "\
~a
setup(
     name='dummy-~a',
     version='0.1',
     packages=['dummy'],
     ~a
     )"
                         (if ,use-setuptools?
                             "from setuptools import setup"
                             "from distutils.core import setup")
                         ,name ,setup-py-extra))))))))))

(define python-dummy-ok
  (make-python-dummy "ok"))

;; distutil won't install any metadata, so make sure our script does not fail
;; on a otherwise fine package.
(define python-dummy-no-setuptools
  (make-python-dummy
   "no-setuptools" #:use-setuptools? #f))

(define python-dummy-fail-requirements
  (make-python-dummy "fail-requirements"
                     #:setup-py-extra "install_requires=['nonexistent'],"))

(define python-dummy-fail-import
  (make-python-dummy "fail-import" #:init-py "import nonexistent"))

(define python-dummy-fail-console-script
  (make-python-dummy "fail-console-script"
                     #:setup-py-extra (string-append "entry_points={'console_scripts': "
                                                     "['broken = dummy:nonexistent']},")))

(define (check-build-success store p)
  (unless store (test-skip 1))
  (test-assert (string-append "python-build-system: " (package-name p))
    (let* ((drv (package-derivation store p)))
      (build-derivations store (list drv)))))

(define (check-build-failure store p)
  (unless store (test-skip 1))
  (test-assert (string-append "python-build-system: " (package-name p))
    (let ((drv (package-derivation store p)))
      (guard (c ((store-protocol-error? c)
                 (pk 'failure c #t)))             ;good!
        (build-derivations store (list drv))
        #f))))                                    ;bad: it should have failed

(with-external-store store
  (for-each (lambda (p) (check-build-success store p))
            (list
             python-dummy-ok
             python-dummy-no-setuptools))
  (for-each (lambda (p) (check-build-failure store p))
            (list
             python-dummy-fail-requirements
             python-dummy-fail-import
             python-dummy-fail-console-script)))

(test-end "builders")
