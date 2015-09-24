;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix import cran)
  #:use-module (guix tests)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define sxml
  '(*TOP* (xhtml:html
           (xhtml:head
            (xhtml:title "CRAN - Package my-example-sxml"))
           (xhtml:body
            (xhtml:h2 "my-example-sxml: Short description")
            (xhtml:p "Long description")
            (xhtml:table
             (@ (summary "Package my-example-sxml summary"))
             (xhtml:tr
              (xhtml:td "Version:")
              (xhtml:td "1.2.3"))
             (xhtml:tr
              (xhtml:td "Depends:")
              (xhtml:td "R (>= 3.1.0)"))
             (xhtml:tr
              (xhtml:td "SystemRequirements:")
              (xhtml:td "cairo (>= 1.2 http://www.cairographics.org/)"))
             (xhtml:tr
              (xhtml:td "Imports:")
              (xhtml:td
               (xhtml:a (@ (href "../scales/index.html"))
                        "scales")
               " (>= 0.2.3), "
               (xhtml:a (@ (href "../proto/index.html"))
                        "proto")
               ", "
               (xhtml:a (@ (href "../Rcpp/index.html")) "Rcpp")
               " (>= 0.11.0)"))
             (xhtml:tr
              (xhtml:td "Suggests:")
              (xhtml:td
               (xhtml:a (@ (href "../some/index.html"))
                        "some")
               ", "
               (xhtml:a (@ (href "../suggestions/index.html"))
                        "suggestions")))
             (xhtml:tr
              (xhtml:td "License:")
              (xhtml:td
               (xhtml:a (@ (href "../../licenses/MIT")) "MIT")))
             (xhtml:tr
              (xhtml:td "URL:")
              (xhtml:td
               (xhtml:a (@ (href "http://gnu.org/s/my-example-sxml"))
                        "http://gnu.org/s/my-example-sxml")
               ", "
               (xhtml:a (@ (href "http://alternative/home/page"))
                        "http://alternative/home/page"))))
            (xhtml:h4 "Downloads:")
            (xhtml:table
             (@ (summary "Package my-example-sxml downloads"))
             (xhtml:tr
              (xhtml:td " Reference manual: ")
              (xhtml:td
               (xhtml:a (@ (href "my-example-sxml.pdf"))
                        " my-example-sxml.pdf ")))
             (xhtml:tr
              (xhtml:td " Package source: ")
              (xhtml:td
               (xhtml:a
                (@ (href "../../../src/contrib/my-example-sxml_1.2.3.tar.gz"))
                " my-example-sxml_1.2.3.tar.gz "))))
            (xhtml:h4 "Reverse dependencies:")
            (xhtml:table
             (@ (summary "Package my-example-sxml reverse dependencies"))
             (xhtml:tr
              (xhtml:td "Reverse depends:")
              (xhtml:td "Too many."))
             (xhtml:tr
              (xhtml:td "Reverse imports:")
              (xhtml:td "Likewise."))
             (xhtml:tr
              (xhtml:td "Reverse suggests:")
              (xhtml:td "Uncountable.")))))))

(define simple-table
  '(xhtml:table
    (xhtml:tr
     (xhtml:td "Numbers")
     (xhtml:td "123"))
    (xhtml:tr
     (@ (class "whatever"))
     (xhtml:td (@ (class "unimportant")) "Letters")
     (xhtml:td "abc"))
    (xhtml:tr
     (xhtml:td "Letters")
     (xhtml:td "xyz"))
    (xhtml:tr
     (xhtml:td "Single"))
    (xhtml:tr
     (xhtml:td "not a value")
     (xhtml:td "not a label")
     (xhtml:td "also not a label"))))

(test-begin "cran")

(test-equal "table-datum: return list of first table cell matching label"
  '((xhtml:td "abc"))
  ((@@ (guix import cran) table-datum) simple-table "Letters"))

(test-equal "table-datum: return empty list if no match"
  '()
  ((@@ (guix import cran) table-datum) simple-table "Astronauts"))

(test-equal "table-datum: only consider the first cell as a label cell"
  '()
  ((@@ (guix import cran) table-datum) simple-table "not a label"))


(test-assert "cran-sxml->sexp"
  ;; Replace network resources with sample data.
  (mock ((guix build download) url-fetch
         (lambda* (url file-name #:key (mirrors '()))
           (with-output-to-file file-name
             (lambda ()
               (display
                (match url
                  ("mirror://cran/src/contrib/my-example-sxml_1.2.3.tar.gz"
                   "source")
                  (_ (error "Unexpected URL: " url))))))))
    (match ((@@ (guix import cran) cran-sxml->sexp) sxml)
      (('package
         ('name "r-my-example-sxml")
         ('version "1.2.3")
         ('source ('origin
                    ('method 'url-fetch)
                    ('uri ('cran-uri "my-example-sxml" 'version))
                    ('sha256
                     ('base32
                      (? string? hash)))))
         ('build-system 'r-build-system)
         ('inputs
          ('quasiquote
           (("cairo" ('unquote 'cairo)))))
         ('propagated-inputs
          ('quasiquote
           (("r-proto" ('unquote 'r-proto))
            ("r-rcpp" ('unquote 'r-rcpp))
            ("r-scales" ('unquote 'r-scales)))))
         ('home-page "http://gnu.org/s/my-example-sxml")
         ('synopsis "Short description")
         ('description "Long description")
         ('license 'x11)))
      (x
       (begin
         (format #t "~s\n" x)
         (pk 'fail x #f))))))

(test-end "cran")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
