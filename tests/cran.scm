;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-cran)
  #:use-module (gnu packages statistics)
  #:use-module (guix import cran)
  #:use-module (guix tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define description "
Package: My-Example
Type: Package
Title: Example package
Version: 1.2.3
Date: 2015-12-10
Author: Ricardo Wurmus
Maintainer: Guix Schmeeks <guix@gnu.org>
URL: http://gnu.org/s/my-example
Description: This is a long description
spanning multiple lines: and it could confuse the parser that this line is very long or perhaps the fact that
there is a colon : on the lines.
  And: this line continues the description.
biocViews: 0
SystemRequirements: Cairo (>= 0)
Depends: A C++11 compiler. Version 4.6.* of g++ (as
	currently in Rtools) is insufficient; versions 4.8.*, 4.9.* or
	later will be fine.
License: GPL (>= 3)
Imports: Rcpp (>= 0.11.5), proto, Scales
LinkingTo: Rcpp, BH
NeedsCompilation: yes
Repository: CRAN
Date/Publication: 2015-07-14 14:15:16
")

(define description-alist
  (description->alist description))

(define simple-alist
  '(("Key"        . "Value")
    ("SimpleList" . "R, Rcpp, something, whatever")
    ("BadList"    . "This is not a real list, you know?")
    ("List"       . "R (>= 2.2), BH (for no reason), GenomicRanges")))

(test-begin "cran")

(test-assert "description->alist: contains all valid keys"
  (let ((keys '("Package" "Type" "Title" "Version" "Date"
                "Author" "Maintainer" "URL" "Description"
                "SystemRequirements" "Depends" "License"
                "Imports" "biocViews" "LinkingTo"
                "NeedsCompilation" "Repository"
                "Date/Publication")))
    (lset= string=? keys (map car description-alist))))

(test-equal "listifyx: return empty list if key cannot be found"
  '()
  ((@@ (guix import cran) listify) simple-alist "Letters"))

(test-equal "listify: split comma-separated value into elements"
  '("R" "Rcpp" "something" "whatever")
  ((@@ (guix import cran) listify) simple-alist "SimpleList"))

(test-equal "listify: strip off parentheses"
  '("R" "BH" "GenomicRanges")
  ((@@ (guix import cran) listify) simple-alist "List"))

(test-equal "listify: ignore values that are no lists"
  '()
  ((@@ (guix import cran) listify) simple-alist "BadList"))

(test-equal "r-mininal is not a cran package"
  #f
  ((@@ (guix import cran) cran-package?) r-minimal))

(test-assert "description->package"
  ;; Replace network resources with sample data.
  (mock ((guix build download) url-fetch
         (lambda* (url file-name
                       #:key
                       (mirrors '()) verify-certificate?)
           (with-output-to-file file-name
             (lambda ()
               (display
                (match url
                  ("mirror://cran/src/contrib/My-Example_1.2.3.tar.gz"
                   "source")
                  (_ (error "Unexpected URL: " url))))))))
    (match (description->package 'cran description-alist)
      (('package
         ('name "r-my-example")
         ('version "1.2.3")
         ('source ('origin
                    ('method 'url-fetch)
                    ('uri ('cran-uri "My-Example" 'version))
                    ('sha256
                     ('base32
                      (? string? hash)))))
         ('properties ('quasiquote (('upstream-name . "My-Example"))))
         ('build-system 'r-build-system)
         ('inputs ('list 'cairo))
         ('propagated-inputs
          ('list 'r-bh 'r-proto 'r-rcpp 'r-scales))
         ('home-page "http://gnu.org/s/my-example")
         ('synopsis "Example package")
         ('description
          "\
This is a long description spanning multiple lines: and it could confuse the
parser that this line is very long or perhaps the fact that there is a colon :
on the lines.  And: this line continues the description.")
         ('license 'gpl3+))
       #t)
      (x
       (begin
         (format #t "~s\n" x)
         (pk 'fail x #f))))))

(test-end "cran")
