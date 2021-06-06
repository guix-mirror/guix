;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
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

(define-module (gnu packages racket)
  #:use-module ((guix licenses)
                #:select (asl2.0 expat lgpl3+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg))


(define %installer-mirrors
  ;; Source:
  ;; https://github.com/racket/racket-lang-org/blob/master/download/data.rkt#L58
  ;; Matthew Flatt says: "note that many are commented out"
  ;; INVARIANT: End with a trailing "/"!
  '("https://mirror.racket-lang.org/installers/"
    "https://www.cs.utah.edu/plt/installers/"
    "https://plt.cs.northwestern.edu/racket-mirror/"
    "https://mirror.csclub.uwaterloo.ca/racket/racket-installers/"
    ;; Universität Tübingen is using a self-signed HTTPS certificate:
    "http://mirror.informatik.uni-tuebingen.de/mirror/racket/"
    "https://racket.infogroep.be/"
    ))


(define-public racket-minimal
  (package
    (name "racket-minimal")
    (version "8.1")            ; note: remember to also update racket!
    (source
     (origin
       (method url-fetch)
       (uri (map (lambda (base)
                   (string-append base version "/racket-minimal-src.tgz"))
                 %installer-mirrors))
       (sha256 "04zzqybpxss50n1jrwwq98539gw0y0ygpw9civl2sq3s4ww7m8l3")
       (patches (search-patches
                 "racket-sh-via-rktio.patch"))))
    (home-page "https://racket-lang.org")
    (synopsis "Racket without bundled packages such as DrRacket")
    (inputs
     `(("openssl" ,openssl)
       ("sqlite" ,sqlite)
       ("sh" ,bash-minimal)
       ("zlib" ,zlib)
       ("zlib:static" ,zlib "static")
       ("lz4" ,lz4)
       ("lz4:static" ,lz4 "static")))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `(,(string-append "CPPFLAGS=-DGUIX_RKTIO_PATCH_BIN_SH="
                         (assoc-ref %build-inputs "sh")
                         "/bin/sh")
         "--enable-libz"
         "--enable-liblz4")
       #:modules
       ((guix build gnu-build-system)
        (guix build utils)
        (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure-minimal
           (lambda* (#:key inputs #:allow-other-keys)
             (chdir "src")
             #t))
         (add-after 'build 'patch-config.rktd-lib-search-dirs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; We do this between the `build` and `install` phases
             ;; so that we have racket to read and write the hash table,
             ;; but it comes before `raco setup`, when foreign libraries
             ;; are needed to build the documentation.
             (define out (assoc-ref outputs "out"))
             (apply invoke
                    "./cs/c/racketcs"
                    "-e"
                    ,(format #f
                             "~s"
                             '(let* ((args
                                      (vector->list
                                       (current-command-line-arguments)))
                                     (file (car args))
                                     (extra-lib-search-dirs (cdr args)))
                                (write-to-file
                                 (hash-update
                                  (file->value file)
                                  'lib-search-dirs
                                  (lambda (dirs)
                                    (append dirs extra-lib-search-dirs))
                                  null)
                                 #:exists 'truncate/replace
                                 file)))
                    "--"
                    "../etc/config.rktd"
                    (filter-map (lambda (lib)
                                  (cond
                                   ((assoc-ref inputs lib)
                                    => (lambda (pth)
                                         (string-append pth "/lib")))
                                   (else
                                    #f)))
                                '("cairo"
                                  "fontconfig"
                                  "glib"
                                  "glu"
                                  "gmp"
                                  "gtk+"
                                  "libjpeg"
                                  "libpng"
                                  "libx11"
                                  "mesa"
                                  "mpfr"
                                  "openssl"
                                  "pango"
                                  "sqlite"
                                  "unixodbc"
                                  "libedit")))
             #t)))
       ;; Tests are in packages like racket-test-core and
       ;; main-distribution-test that aren't part of the main distribution.
       #:tests? #f))
    (description
     "Racket is a general-purpose programming language in the Scheme family,
with a large set of libraries and a compiler based on Chez Scheme.  Racket is
also a platform for language-oriented programming, from small domain-specific
languages to complete language implementations.

The ``minimal Racket'' distribution includes just enough of Racket for you to
use @command{raco pkg} to install more.  Bundled packages, such as the
DrRacket IDE, are not included.")
    ;; https://download.racket-lang.org/license.html
    (license (list lgpl3+ asl2.0 expat))))


(define-public racket
  (package
    (inherit racket-minimal)
    (name "racket")
    (version (package-version racket-minimal)) ; needed for origin uri to work
    (source
     (origin
       (inherit (package-source racket-minimal))
       (uri (map (lambda (base)
                   (string-append base version "/racket-src.tgz"))
                 %installer-mirrors))
       (sha256
        (base32
         "0xdqwrwm604bbnr97h75dps2ixxz2svlw0fn0f674bn04dcfd60f"))))
    (inputs
     `(;; sqlite and libraries for `racket/draw' are needed to build the doc.
       ("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("glib" ,glib)
       ("glu" ,glu)
       ("gmp" ,gmp)
       ("gtk+" ,gtk+)                   ; propagates gdk-pixbuf+svg
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("mesa" ,mesa)
       ("mpfr" ,mpfr)
       ("pango" ,pango)
       ("unixodbc" ,unixodbc)
       ("libedit" ,libedit)
       ,@(package-inputs racket-minimal)))
    (synopsis "A programmable programming language in the Scheme family")
    (description
     "Racket is a general-purpose programming language in the Scheme family,
with a large set of libraries and a compiler based on Chez Scheme.  Racket is
also a platform for language-oriented programming, from small domain-specific
languages to complete language implementations.

The main Racket distribution comes with many bundled packages, including the
DrRacket IDE, libraries for GUI and web programming, and implementations of
languages such as Typed Racket, R5RS and R6RS Scheme, Algol 60, and Datalog.")))
