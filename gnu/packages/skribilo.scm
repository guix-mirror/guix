;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages skribilo)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module ((guix utils) #:select (version-major+minor))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages lout)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages plotutils)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages ghostscript))

(define-public skribilo
  (package
    (name "skribilo")
    (version "0.9.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/skribilo/skribilo-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "02dzy2imqgfmwda4d1r51205si4c0r4fp2gf22sb0kv3qhhnm0h0"))))
    (build-system gnu-build-system)
    (arguments
     ;; Make the modules available under the usual location.
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Make sure the 'skribilo' command gets to see
             ;; Guile-Reader, even if Guile-Reader is not in the search
             ;; path.
             (let ((reader (assoc-ref inputs "guile-reader"))
                   (effective ,(version-major+minor
                                (package-version
                                 (car (assoc-ref (package-inputs this-package)
                                                 "guile"))))))
               (substitute* "src/skribilo.in"
                 (("^exec (.*) -c" _ things)
                  (string-append "exec " things
                                 " -L " reader "/share/guile/site/" effective
                                 " -C " reader
                                 "/lib/guile/" effective "/site-ccache"
                                 " -c"))))
             #t)))

       #:parallel-build? #f

       ;; XXX: Temporarily disable tests because they rely on
       ;; 'test-runner-current' *not* returning #f after 'test-end', which is
       ;; no longer the case in Guile >= 3.0.6.  This is fixed upstream.
       #:tests? #f))

    (native-inputs (list pkg-config))

    (inputs (list guile-3.0
                  imagemagick
                  ghostscript ; for 'convert'
                  ploticus
                  lout))

    ;; The 'skribilo' command needs them, and for people using Skribilo as a
    ;; library, these inputs are needed as well.
    (propagated-inputs (list guile-reader guile-lib))

    (home-page "https://www.nongnu.org/skribilo/")
    (synopsis "Document production tool written in Guile Scheme")
    (description
     "Skribilo is a free document production tool that takes a structured
document representation as its input and renders that document in a variety of
output formats: HTML and Info for on-line browsing, and Lout and LaTeX for
high-quality hard copies.

The input document can use Skribilo's markup language to provide information
about the document's structure, which is similar to HTML or LaTeX and does not
require expertise.  Alternatively, it can use a simpler, “markup-less” format
that borrows from Emacs' outline mode and from other conventions used in
emails, Usenet and text.

Lastly, Skribilo provides Guile Scheme APIs.")
    (license gpl3+)))
