;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Marek Benc <merkur32@gmail.com>
;;; Copyright © 2016, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages fribidi)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages))

(define-public fribidi
  (package
    (name "fribidi")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri
         (string-append "https://github.com/fribidi/fribidi/releases"
                        "/download/v" version "/fribidi-" version
                         ".tar.xz"))
        (sha256
         (base32 "1iz06r6ha2nrgbzbn4141r58a60a9s5qiaadjjhhvdkg0alpxr65"))))
    (build-system gnu-build-system)
    (synopsis "Implementation of the Unicode bidirectional algorithm")
    (description
     "GNU FriBidi is an implementation of the Unicode Bidirectional
Algorithm.  This algorithm is used to properly display text in left-to-right
or right-to-left ordering as necessary.")
    (home-page "https://github.com/fribidi/fribidi")
    (license lgpl2.1+)))

(define-public bidiv
  (package
    (name "bidiv")
    (version "1.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://debian/pool/main/b/bidiv/bidiv_"
                            version ".orig.tar.gz"))
        (sha256
         (base32
          "05p5m2ihxbmc1qsgs8rjlww08fy9859fhl7xf196p8g5qygqd7cv"))
        (patches (search-patches "bidiv-update-fribidi.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure
         (add-after 'unpack 'misc-fixes
           (lambda _
             (substitute* "bidiv.c"
               (("FriBidiCharType") "FriBidiParType")
               (("&c") "(char *)&c"))
             #t))
         ;; We don't want to use the handwritten makefile
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((fribidi (assoc-ref inputs "fribidi")))
               (invoke "gcc" "-o" "bidiv" "bidiv.c"
                       ;; pkg-config --cflags fribidi
                       (string-append "-I" fribidi "/include/fribidi")
                       ;; pkg-config --libs fribidi
                       (string-append "-L" fribidi "/lib") "-lfribidi"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "bidiv" bin)
               (install-file "bidiv.1" man))
             #t)))
       #:tests? #f)) ; no tests
    (inputs
     (list fribidi))
    (home-page "https://tracker.debian.org/pkg/bidiv")
    (synopsis "BiDi viewer - command-line tool displaying logical Hebrew/Arabic")
    (description "bidiv is a simple utility for converting logical-Hebrew input
to visual-Hebrew output.  This is useful for reading Hebrew mail messages,
viewing Hebrew texts, etc.  It was written for Hebrew but Arabic (or other BiDi
languages) should work equally well.")
    (license gpl2+)))
