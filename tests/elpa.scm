;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
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

(define-module (test-elpa)
  #:use-module (guix import elpa)
  #:use-module (guix tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define elpa-mock-archive
  '(1
    (ace-window .
                [(0 9 0)
                 ((avy
                   (0 2 0)))
                 "Quickly switch windows." single
                 ((:url . "https://github.com/abo-abo/ace-window")
                  (:keywords "window" "location"))])
    (auctex .
            [(11 88 6)
             nil "Integrated environment for *TeX*" tar
             ((:url . "http://www.gnu.org/software/auctex/"))])))

(define auctex-readme-mock "This is the AUCTeX description.")

(define* (elpa-package-info-mock name #:optional (repo "gnu"))
  "Simulate retrieval of 'archive-contents' file from REPO and extraction of
information about package NAME. (Function 'elpa-package-info'.)"
  (let* ((archive elpa-mock-archive)
         (info (filter (lambda (p) (eq? (first p) (string->symbol name)))
                       (cdr archive))))
    (if (pair? info) (first info) #f)))

(define elpa-version->string
  (@@ (guix import elpa) elpa-version->string))

(define package-source-url
  (@@ (guix import elpa) package-source-url))

(define ensure-list
  (@@ (guix import elpa) ensure-list))

(define package-home-page
  (@@ (guix import elpa) package-home-page))

(define make-elpa-package
  (@@ (guix import elpa) make-elpa-package))

(test-begin "elpa")

(define (eval-test-with-elpa pkg)
  (mock
   ;; replace the two fetching functions
   ((guix import elpa) fetch-elpa-package
    (lambda* (name #:optional (repo "gnu"))
      (let ((pkg (elpa-package-info-mock name repo)))
        (match pkg
          ((name version reqs synopsis kind . rest)
           (let* ((name (symbol->string name))
                  (ver (elpa-version->string version))
                  (url (package-source-url kind name ver repo)))
             (make-elpa-package name ver
                                (ensure-list reqs) synopsis kind
                                (package-home-page (first rest))
                                auctex-readme-mock
                                url)))
          (_ #f)))))
   (mock
    ((guix build download) url-fetch
     (lambda (url file . _)
       (call-with-output-file file
         (lambda (port)
           (display "fake tarball" port)))))

    (match (elpa->guix-package pkg)
      (('package
         ('name "emacs-auctex")
         ('version "11.88.6")
         ('source
          ('origin
            ('method 'url-fetch)
            ('uri ('string-append
                   "https://elpa.gnu.org/packages/auctex-" 'version ".tar"))
            ('sha256 ('base32 (? string? hash)))))
         ('build-system 'emacs-build-system)
         ('home-page "http://www.gnu.org/software/auctex/")
         ('synopsis "Integrated environment for *TeX*")
         ('description (? string?))
         ('license 'license:gpl3+))
       #t)
      (x
       (pk 'fail x #f))))))

(test-assert "elpa->guix-package test 1"
  (eval-test-with-elpa "auctex"))

(test-end "elpa")
