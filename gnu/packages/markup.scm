;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 EuAndreh <eu@euandre.org>
;;; Copyright © 2021 Noisytoot <noisytoot@disroot.org>
;;; Copyright © 2021 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
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
    (license license:expat)))

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
               (unzip  (search-input-file %build-inputs "/bin/unzip")))
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
    (native-inputs (list unzip))
    (inputs (list perl))
    (home-page "http://daringfireball.net/projects/markdown")
    (synopsis "Text-to-HTML conversion tool")
    (description
     "Markdown is a text-to-HTML conversion tool for web writers.  It allows
you to write using an easy-to-read, easy-to-write plain text format, then
convert it to structurally valid XHTML (or HTML).")
    (license (license:non-copyleft "file://License.text"
                                   "See License.text in the distribution."))))

(define-public lowdown
  (let ((commit "1de10c1d71bfb4348ae0beaec8b1547d5e114969")
        (revision "1"))
    (package
      (name "lowdown")
      (version (git-version "0.10.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kristapsdz/lowdown")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1wh07nkiihvp1m79sj4qlnqklnn0rfp3hwls8sqcp0bfd96wpa1h"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:test-target "regress"
        #:phases
        #~(modify-phases %standard-phases
            (replace 'configure
              (lambda _
                (invoke "./configure"
                        (string-append "PREFIX=" #$output)
                        (string-append "MANDIR=" #$output "/share/man")))))
        #:make-flags #~(list "CFLAGS=-fPIC")))
      (native-inputs
       (list which))
      (home-page "https://kristaps.bsd.lv/lowdown/")
      (synopsis "Simple Markdown translator")
      (description "Lowdown is a Markdown translator producing HTML5,
roff documents in the ms and man formats, LaTeX, gemini, and terminal output.")
      (license license:isc))))

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
     (list pkg-config))
    (synopsis "Markdown processing library, written in C")
    (description
     "Discount is a markdown implementation, written in C.  It provides a
@command{markdown} command, and a library.")
    (home-page "https://www.pell.portland.or.us/~orc/Code/discount/")
    (license license:bsd-3)))

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
     (list discount))
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
    (license license:perl-license)))

(define-public python-cmarkgfm
  (package
    (name "python-cmarkgfm")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cmarkgfm" version))
              (sha256
               (base32
                "06cw49bzxl3k7m8993cyi5zqxvk817z8ghhr9xqq5gx8klpiap56"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled cmark and generated headers.
                  (for-each delete-file-recursively
                            '("third_party/cmark" "generated"))))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'copy-cmark-gfm
                 (lambda _
                   ;; This package needs the cmark-gfm source files
                   ;; to generate FFI bindings.
                   (copy-recursively #+(package-source (this-package-input
                                                        "cmark-gfm"))
                                     "third_party/cmark")))
               (add-after 'unpack 'install-cmark-headers
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; XXX: Loosely based on 'regenerate' from noxfile.py.
                   (let ((version.h (search-input-file
                                     inputs "/include/cmark-gfm_version.h")))
                     (for-each (lambda (file)
                                 (install-file file "generated/unix/"))
                               (cons version.h
                                     (find-files (dirname version.h)
                                                 "_export\\.h$"))))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests? (invoke "pytest" "-vv" "tests")))))))
    (native-inputs (list python-pytest))
    (inputs (list cmark-gfm))
    (propagated-inputs (list python-cffi-1.15))
    (home-page "https://github.com/theacodes/cmarkgfm")
    (synopsis "Python bindings for GitHub's fork of cmark")
    (description
     "This package provides a minimal set of Python bindings for the
GitHub cmark fork (@code{cmark-gfm}).")
    (license license:expat)))

(define-public python-markdownify
  (package
    (name "python-markdownify")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "markdownify" version))
       (sha256
        (base32
         "0msvrsgq9jigbgg7r7iq7ql5bgslmbxd8sq0nmpbxrjwqypgs7w2"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-flake8 python-beautifulsoup4 python-six))
    (home-page
     "https://github.com/matthewwithanm/python-markdownify")
    (synopsis "Converts HTML to Markdown")
    (description "This package provides @code{markdownify} a Python library to
convert HTML to Markdown.")
    (license license:expat)))

(define-public cmark
  (package
    (name "cmark")
    (version "0.30.2")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/jgm/cmark")
                    (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1426snw3mq8qmpdxznkhsyy75xd9v9nwlc7sph08qpdz8xnp4hr2"))))
    (build-system cmake-build-system)
    (arguments
     '(#:test-target "test"))
    (native-inputs (list python))
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
    (license (list license:bsd-2 license:expat license:cc-by-sa4.0))))

(define-public cmark-gfm
  (package
    (inherit cmark)
    (name "cmark-gfm")
    (version "0.29.0.gfm.2")
    (home-page "https://github.com/github/cmark-gfm")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vz6zs3m22k7jzfj4782lahciwfjlbi4m3qz5crsmssip3rwdy7h"))))
    (arguments
     '(#:test-target "test"
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-config
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        ;; XXX: cmark-gfm-core-extensions.h includes this file.
                        (install-file "src/config.h"
                                      (string-append out "/include"))))))))
    (synopsis "GitHub flavored CommonMark")
    (description
     "This package is a fork of @code{cmark}, with GitHub-specific Markdown
additions.")))

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
    (license license:x11)))

(define-public md4c
  (package
    (name "md4c")
    (version "0.4.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mity/md4c/")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12pdh4rfjc3b0cblj5nz3jksr2376lx8ay0vw5dwa1s97q09pczq"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/mity/md4c/")
    (synopsis "C Markdown parser compliant to CommonMark")
    (description "MD4C is a C Markdown parser with a
SAX-like interface.  It is compliant to the CommonMark specification,
with a few extensions.")
    (license license:expat)))

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
    (license license:expat)))
