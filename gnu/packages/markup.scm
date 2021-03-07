;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 EuAndreh <eu@euandre.org>
;;; Copyright © 2021 Noisytoot <noisytoot@disroot.org>
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
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages web))

(define-public hoedown
  (package
    (name "hoedown")
    (version "3.0.7")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/hoedown/hoedown")
                    (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1kr3hxjg2dgmwy9738qgj3sh3f5cygx0zxskkfhrg7x19bq9yd26"))))
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
             (invoke unzip source)
             (mkdir "bin")
             (mkdir-p "share/doc")
             (rename-file "Markdown_1.0.1/Markdown.pl" "bin/markdown")
             (rename-file "Markdown_1.0.1/Markdown Readme.text"
                          "share/doc/README")
             (patch-shebang "bin/markdown" (list perlbd))
             (delete-file-recursively "Markdown_1.0.1"))
           #t))))
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

(define-public discount
  (package
    (name "discount")
    (version "2.2.7")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.pell.portland.or.us/~orc/Code/"
                   "discount/discount-" version ".tar.bz2"))
             (sha256
              (base32
               "024mxv0gpvilyfczarcgy5m7h4lv6qvhjfpf5i73qkxhszjjn9mi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:parallel-build? #f             ; libmarkdown won't be built in time
       #:make-flags (list
                     (string-append "LFLAGS=-L. -Wl,-rpath="
                                    (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-AC_PATH
           (lambda _
             ;; The default value is not suitable, so override using an
             ;; environment variable. This just affects the build, and not the
             ;; resulting store item.
             (setenv "AC_PATH" (getenv "PATH"))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "CC" ,(cc-for-target))
               ;; The ‘validate-runpath’ phase fails otherwise.
               (setenv "LDFLAGS" (string-append "-Wl,-rpath=" out "/lib"))
               (invoke "./configure.sh"
                       (string-append "--prefix=" out)
                       "--shared")))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Markdown processing library, written in C")
    (description
     "Discount is a markdown implementation, written in C.  It provides a
@command{markdown} command, and a library.")
    (home-page "https://www.pell.portland.or.us/~orc/Code/discount/")
    (license bsd-3)))

(define-public perl-text-markdown-discount
  (package
    (name "perl-text-markdown-discount")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/S/SE/SEKIMURA/Text-Markdown-Discount-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1xx7v3wnla7m6wa3h33whxw3vvincaicg4yra1b9wbzf2aix9rnw"))
       (patches
        (search-patches "perl-text-markdown-discount-unbundle.patch"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-ldflags
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("OTHERLDFLAGS = ")
                (string-append
                      "OTHERLDFLAGS = -lmarkdown -Wl,-rpath="
                      (assoc-ref inputs "discount")
                      "/lib")))
             #t)))))
    (inputs
     `(("discount" ,discount)))
    (home-page
     "https://metacpan.org/release/Text-Markdown-Discount")
    (synopsis
     "Fast function for converting Markdown to HTML using Discount")
    (description
     "Text::Markdown::Discount is a Perl extension to the Discount markdown
implementation.

@example
  use Text::Markdown::Discount;
  my $html = markdown($text)
@end example")
    (license perl-license)))

(define-public cmark
  (package
    (name "cmark")
    (version "0.29.0")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/jgm/cmark")
                    (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0r7jpqhgnssq444i8pwji2g36058vfzwkl70wbiwj13h4w5rfc8f"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 ;; Mimic upstream commit 68c3a91166347 to fix a test failure
                 ;; when using Python 3.8.  Remove for versions > 0.29.
                 ;; See <https://github.com/commonmark/cmark/issues/313>.
                 (substitute* "test/normalize.py"
                   (("cgi") "html"))
                 #t))))
    (build-system cmake-build-system)
    (arguments
     '(#:test-target "test"))
    (native-inputs `(("python" ,python)))
    (synopsis "CommonMark Markdown reference implementation")
    (description "CommonMark is a strongly defined, highly compatible
specification of Markdown.  cmark is the C reference implementation of
CommonMark.  It provides @code{libcmark} shared library for parsing
CommonMark to an abstract syntax tree (@dfn{AST}) and rendering the document
as HTML, groff man, LaTeX, CommonMark, or an XML representation of the
AST.  The package also provides the command-line program @command{cmark}
for parsing and rendering CommonMark.")
    (home-page "https://commonmark.org")
    ;; cmark is distributed with a BSD-2 license, but some components are Expat
    ;; licensed. The CommonMark specification is Creative Commons CC-BY-SA 4.0
    ;; licensed. See 'COPYING' in the source distribution for more information.
    (license (list bsd-2 expat cc-by-sa4.0))))

(define-public smu
  (package
    (name "smu")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Gottox/smu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jm7lhnzjx4q7gcwlkvsbffcy0zppywyh50d71ami6dnq182vvcc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:tests? #f                      ; no tests included
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

(define-public md4c
  (package
    (name "md4c")
    (version "0.4.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mity/md4c/")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m3202zzjvw4k7jw66z1qi3cbisxzvplq5alkygpifvhzm81gwwx"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/mity/md4c/")
    (synopsis "C Markdown parser compliant to CommonMark")
    (description "MD4C is a C Markdown parser with a
SAX-like interface.  It is compliant to the CommonMark specification,
with a few extensions.")
    (license expat)))

(define-public python-mistletoe
  (package
    (name "python-mistletoe")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mistletoe" version))
       (sha256
        (base32 "18z6hqfnfjqnrcgfgl5pkj9ggf9yx0yyy94azcn1qf7hqn6g3l14"))))
    (build-system python-build-system)
    (home-page "https://github.com/miyuchina/mistletoe")
    (synopsis "Extensible Markdown parser in pure Python")
    (description
     "The @code{mistletoe} Markdown parser is a CommonMark-compliant Markdown
parser that supports definitions of custom tokens.

Parsing Markdown into an abstract syntax tree also allows @code{mistletoe} to
swap out renderers for different output formats, without touching any of the
core components.")
    (license expat)))
