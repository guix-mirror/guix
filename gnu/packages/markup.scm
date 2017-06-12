;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 ng0 <ng0@no-reply.pragmatique.xyz>
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

(define-module (gnu packages markup)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages web))

(define-public hoedown
  (package
    (name "hoedown")
    (version "3.0.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/hoedown/hoedown/archive/"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "0859dc2xjasd6kgkshi8mb20kbyw5sql1ln0hw3bfaf33qdh5dh1"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc" (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)) ; no configure script
       #:test-target "test"))
    (native-inputs
     `(("python" ,python-2)
       ("tidy" ,tidy)))
    (synopsis "Markdown processing library")
    (description "Hoedown is a standards compliant, fast, secure markdown
processing library written in C.")
    (home-page "https://github.com/hoedown/hoedown")
    (license expat)))

(define-public markdown
  (package
    (name "markdown")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://daringfireball.net/projects/downloads/"
             (string-capitalize name) "_" version ".zip"))
       (sha256
        (base32 "0dq1pj91pvlwkv0jwcgdfpv6gvnxzrk3s8mnh7imamcclnvfj835"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source (assoc-ref %build-inputs "source"))
               (out    (assoc-ref %outputs "out"))
               (perlbd (string-append (assoc-ref %build-inputs "perl") "/bin"))
               (unzip  (string-append (assoc-ref %build-inputs "unzip")
                                      "/bin/unzip")))
           (mkdir-p out)
           (with-directory-excursion out
             (system* unzip source)
             (mkdir "bin")
             (mkdir-p "share/doc")
             (rename-file "Markdown_1.0.1/Markdown.pl" "bin/markdown")
             (rename-file "Markdown_1.0.1/Markdown Readme.text"
                          "share/doc/README")
             (patch-shebang "bin/markdown" (list perlbd))
             (delete-file-recursively "Markdown_1.0.1"))))))
    (native-inputs `(("unzip" ,unzip)))
    (inputs `(("perl" ,perl)))
    (home-page "http://daringfireball.net/projects/markdown")
    (synopsis "Text-to-HTML conversion tool")
    (description
     "Markdown is a text-to-HTML conversion tool for web writers.  It allows
you to write using an easy-to-read, easy-to-write plain text format, then
convert it to structurally valid XHTML (or HTML).")
    (license (non-copyleft "file://License.text"
                           "See License.text in the distribution."))))

(define-public cmark
  (package
    (name "cmark")
    (version "0.27.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/jgm/cmark/archive/"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "1da62ispca9aal2a36gaj87175rv5013pl7x740vk32y6lclr6v6"))))
    (build-system cmake-build-system)
    (arguments
     '(#:test-target "test"))
    (native-inputs `(("python" ,python)))
    (synopsis "CommonMark Markdown reference implementation")
    (description "CommonMark is a strongly defined, highly compatible
specification of Markdown.  cmark is the C reference implementation of
CommonMark.  It provides @code{libcmark} shared library for parsing
CommonMark to an abstract syntax tree (AST) and rendering the document
as HTML, groff man, LaTeX, CommonMark, or an XML representation of the
AST.  The package also provides the command-line program @command{cmark}
for parsing and rendering CommonMark.")
    (home-page "http://commonmark.org")
    ;; cmark is distributed with a BSD-2 license, but some components are Expat
    ;; licensed. The CommonMark specification is Creative Commons CC-BY-SA 4.0
    ;; licensed. See 'COPYING' in the source distribution for more information.
    (license (list bsd-2 expat cc-by-sa4.0))))

(define-public smu
  (package
    (name "smu")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Gottox/smu/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0iazl45rkz8ngsb5hpykl76w0ngvdvqqhym1qz5wykgmrzk293rp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:tests? #f ;No tests included
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/Gottox/smu")
    (synopsis "Simple markup")
    (description
     "Smu is a very simple and minimal markup language.  It is
designed for using in wiki-like environments.  Smu makes it very
easy to write your documents on the fly and convert them into HTML.
Smu is capable to parse very large documents.  As long as you avoid an huge
amount of indents it scales just great.

Smu was started as a rewrite of Markdown but became something more
lightweight and consistent.  The biggest difference between Markdown
and smu is that smu doesn't support reference style links.")
    (license x11)))
