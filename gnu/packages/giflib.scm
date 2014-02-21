;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages giflib)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages perl))

(define-public giflib
  (package
    (name "giflib")
    (version "4.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/giflib/giflib-"
                                  (first (string-split version #\.))
                                  ".x/giflib-" version ".tar.bz2"))
              (sha256
               (base32 "0rmp7ipzk42r841bggd7bfqk4p8qsssbp4wcck4qnz7p4rkxbj0a"))))
    (build-system gnu-build-system)
    (outputs '("bin"                    ; utility programs
               "out"))                  ; library
    (inputs `(("libx11" ,libx11)
              ("libice" ,libice)
              ("libsm" ,libsm)
              ("perl" ,perl)))
    (arguments
     `(#:phases (alist-cons-after
                 'unpack 'disable-html-doc-gen
                 (lambda _
                   (substitute* "doc/Makefile.in"
                     (("^all: allhtml manpages") "")))
                 (alist-cons-after
                  'install 'install-manpages
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((bin (assoc-ref outputs "bin"))
                           (man1dir (string-append bin "/share/man/man1")))
                      (mkdir-p man1dir)
                      (for-each (lambda (file)
                                  (let ((base (basename file)))
                                    (format #t "installing `~a' to `~a'~%"
                                            base man1dir)
                                    (copy-file file
                                               (string-append
                                                man1dir "/" base))))
                                (find-files "doc" "\\.1"))))
                  %standard-phases))))
    (synopsis "Tools and library for working with GIF images")
    (description
     "giflib is a library for reading and writing GIF images.  It is API and
ABI compatible with libungif which was in wide use while the LZW compression
algorithm was patented.  Tools are also included to convert, manipulate,
compose, and analyze GIF images.")
    (home-page "http://giflib.sourceforge.net/")
    (license x11)))

;;; giflib.scm ends here
