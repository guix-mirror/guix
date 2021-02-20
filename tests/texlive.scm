;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-texlive)
  #:use-module (gnu packages tex)
  #:use-module (guix import texlive)
  #:use-module (guix tests)
  #:use-module (guix tests http)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-26)
  #:use-module (web client)
  #:use-module (ice-9 match))

(test-begin "texlive")

(define xml
  "\
<entry id=\"foo\">
   <name>foo</name>
   <caption>Foomatic frobnication in LuaLaTeX</caption>
   <authorref id=\"rekado\"/>
   <license type=\"lppl1.3\"/>
   <version number=\"2.6a\"/>
   <description>
     <p>
          Foo is a package for LuaLaTeX. It provides an interface to frobnicate gimbals
          in a foomatic way with the LuaTeX engine.
     </p>
     <p>
          The package requires the bar and golly
          bundles for extremely special specialties.
     </p>
   </description>
   <ctan path=\"/macros/latex/contrib/foo\" file=\"true\"/>
   <texlive location=\"foo\"/>
   <keyval key=\"topic\" value=\"tests\"/>
  null
</entry>")

(define sxml
  '(*TOP* (entry (@ (id "foo"))
                 (name "foo")
                 (caption "Foomatic frobnication in LuaLaTeX")
                 (authorref (@ (id "rekado")))
                 (license (@ (type "lppl1.3")))
                 (version (@ (number "2.6a")))
                 (description
                  (p "\n          Foo is a package for LuaLaTeX. It provides an interface to frobnicate gimbals\n          in a foomatic way with the LuaTeX engine.\n     ")
                  (p "\n          The package requires the bar and golly\n          bundles for extremely special specialties.\n     "))
                 (ctan (@ (path "/macros/latex/contrib/foo") (file "true")))
                 (texlive (@ (location "foo")))
                 (keyval (@ (value "tests") (key "topic")))
                 "\n  null\n")))

(test-equal "fetch-sxml: returns SXML for valid XML"
  sxml
  (with-http-server `((200 ,xml))
    (parameterize ((current-http-proxy (%local-url)))
      (fetch-sxml "foo"))))

;; TODO:
(test-assert "sxml->package"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (sxml->package sxml)))
          (match result
            (('package
               ('name "texlive-latex-foo")
               ('version "2.6a")
               ('source ('origin
                          ('method 'svn-fetch)
                          ('uri ('texlive-ref "latex" "foo"))
                          ('sha256
                           ('base32
                            (? string? hash)))))
               ('build-system 'texlive-build-system)
               ('arguments ('quote (#:tex-directory "latex/foo")))
               ('home-page "http://www.ctan.org/pkg/foo")
               ('synopsis "Foomatic frobnication in LuaLaTeX")
               ('description
                "Foo is a package for LuaLaTeX.  It provides an interface to \
frobnicate gimbals in a foomatic way with the LuaTeX engine.  The package \
requires the bar and golly bundles for extremely special specialties.")
               ('license 'lppl1.3+))
             #t)
            (_
             (begin
               (format #t "~s\n" result)
               (pk 'fail result #f)))))))

(test-end "texlive")
