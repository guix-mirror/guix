;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2022 Arjan Adriaanse <arjan@adriaan.se>
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

(define-module (test-home-import)
  #:use-module (guix scripts home import)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module ((guix profiles) #:hide (manifest->code))
  #:use-module ((guix build syscalls) #:select (mkdtemp!))
  #:use-module ((guix scripts package)
                #:select (manifest-entry-version-prefix))
  #:use-module (gnu packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

;; Test the (guix scripts home import) tools.

(test-begin "home-import")

;; Example manifest entries.

(define guile-2.0.9
  (manifest-entry
    (name "guile")
    (version "2.0.9")
    (item "/gnu/store/...")))

(define glibc
  (manifest-entry
    (name "glibc")
    (version "2.19")
    (item "/gnu/store/...")))

(define gcc
  (manifest-entry
    (name "gcc")
    (version "")
    (output "lib")
    (item "/gnu/store/...")))

;; Helpers for checking and generating home environments.

(define %destination-directory "/tmp/guix-config")
(mkdir-p %destination-directory)

(define %temporary-home-directory (mkdtemp! "/tmp/guix-home-import.XXXXXX"))

(define-syntax-rule (define-home-environment-matcher name pattern)
  (define (name obj)
    (match obj
      (pattern #t)
      (x (pk 'fail x #f)))))

(define (create-temporary-home files-alist)
  "Create a temporary home directory in '%temporary-home-directory'.
FILES-ALIST is an association list of files and the content of the
corresponding file."
  (define (create-file file content)
    (let ((absolute-path (string-append %temporary-home-directory "/" file)))
      (unless (file-exists? absolute-path)
        (mkdir-p (dirname absolute-path)))
      (call-with-output-file absolute-path
        (cut display content <>))))

  (for-each (match-lambda
              ((file . content) (create-file file content)))
            files-alist))

(define (eval-test-with-home-environment files-alist manifest matcher)
  (create-temporary-home files-alist)
  (setenv "HOME" %temporary-home-directory)
  (mkdir-p %temporary-home-directory)
  (let* ((home-environment (manifest+configuration-files->code
                            manifest %destination-directory))
         (result (matcher home-environment)))
    (delete-file-recursively %temporary-home-directory)
    result))

(define-home-environment-matcher match-home-environment-no-services
  ('begin
    ('use-modules
     ('gnu 'home)
     ('gnu 'packages)
     ('gnu 'services))
    ('home-environment
     ('packages
      ('map ('compose 'list 'specification->package+output)
            ('list "guile@2.0.9" "gcc:lib" "glibc@2.19")))
     ('services
      ('list)))))

(define-home-environment-matcher match-home-environment-transformations
  ('begin
    ('use-modules
     ('gnu 'home)
     ('gnu 'packages)
     ('gnu 'services)
     ('guix 'transformations))

    ('define transform ('options->transformation _))
    ('home-environment
     ('packages
      ('list (transform ('specification->package "guile@2.0.9"))
             ('list ('specification->package "gcc") "lib")
             ('specification->package "glibc@2.19")))
     ('services ('list)))))

(define-home-environment-matcher match-home-environment-no-services-nor-packages
  ('begin
    ('use-modules
     ('gnu 'home)
     ('gnu 'packages)
     ('gnu 'services))
    ('home-environment
     ('packages
      ('map ('compose 'list 'specification->package+output)
            ('list)))
     ('services
      ('list)))))

(define-home-environment-matcher match-home-environment-bash-service
  ('begin
    ('use-modules
     ('gnu 'home)
     ('gnu 'packages)
     ('gnu 'services)
     ('guix 'gexp)
     ('gnu 'home 'services 'shells))
    ('home-environment
     ('packages
      ('map ('compose 'list 'specification->package+output)
            ('list)))
     ('services
      ('list ('service
              'home-bash-service-type
              ('home-bash-configuration
               ('aliases ('quote ()))
               ('bashrc
                ('list ('local-file "/tmp/guix-config/.bashrc"
                                    "bashrc"))))))))))


(test-assert "manifest->code: No services"
  (eval-test-with-home-environment
   '()
   (make-manifest (list guile-2.0.9 gcc glibc))
   match-home-environment-no-services))

(test-assert "manifest->code: No services, package transformations"
  (eval-test-with-home-environment
   '()
   (make-manifest (list (manifest-entry
                          (inherit guile-2.0.9)
                          (properties `((transformations
                                         . ((foo . "bar"))))))
                        gcc glibc))
   match-home-environment-transformations))

(test-assert "manifest->code: No packages nor services"
  (eval-test-with-home-environment
   '()
   (make-manifest '())
   match-home-environment-no-services-nor-packages))

(test-assert "manifest->code: Bash service"
  (eval-test-with-home-environment
   '((".bashrc" . "echo 'hello guix'"))
   (make-manifest '())
   match-home-environment-bash-service))

(test-end "home-import")
