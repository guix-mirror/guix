;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Alex Vong <alexvong1995@gmail.com>
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

(define-module (gnu packages ocr)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages image))

(define-public ocrad
  (package
    (name "ocrad")
    (version "0.27")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/ocrad/ocrad-"
                                 version ".tar.lz"))
             (sha256
              (base32
               "0divffvcaim89g4pvqs8kslbcxi475bcl3b4ynphf284k9zfdgx9"))))
    (build-system gnu-build-system)
    (native-inputs `(("lzip" ,lzip)))
    (home-page "https://www.gnu.org/software/ocrad/")
    (synopsis "Optical character recognition based on feature extraction")
    (description
     "GNU Ocrad is an optical character recognition program based on a
feature extraction method.  It can read images in PBM, PGM or PPM formats and
it produces text in 8-bit or UTF-8 formats.")
    (license license:gpl3+)))

(define-public tesseract-ocr
  (package
    (name "tesseract-ocr")
    (version "3.04.01")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/tesseract-ocr/tesseract")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h1x4z1h86n2gwknd0wck6gykkp99bmm02lg4a47a698g4az6ybv"))))
    (build-system gnu-build-system)
    (inputs
     `(("leptonica" ,leptonica)))
    (arguments
     '(#:configure-flags
       (let ((leptonica (assoc-ref %build-inputs "leptonica")))
         (list (string-append "LIBLEPT_HEADERSDIR=" leptonica "/include")))))
    (home-page "https://github.com/tesseract-ocr/tesseract")
    (synopsis "Optical character recognition engine")
    (description
     "Tesseract is an optical character recognition (OCR) engine with very
high accuracy.  It supports many languages, output text formatting, hOCR
positional information and page layout analysis.  Several image formats are
supported through the Leptonica library.  It can also detect whether text is
monospaced or proportional.")
    (license license:asl2.0)))

(define-public zinnia
  (let* ((commit "581faa8f6f15e4a7b21964be3a5ec36265c80e5b")
         (revision "1")
         ;; version copied from 'configure.in'
         (version (git-version "0.07" revision commit)))
    (package
      (name "zinnia")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/taku910/zinnia.git")
               (commit commit)))
         (sha256
          (base32
           "1izjy5qw6swg0rs2ym2i72zndb90mwrfbd1iv8xbpwckbm4899lg"))
         (file-name (git-file-name name version))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (srfi srfi-26)))
         (snippet ; remove unnecessary files with potentially different license
          '(begin
             (for-each delete-file-recursively
                       (scandir "."
                                (negate (cut member <> '("zinnia"
                                                         "." "..")))))
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'bootstrap
             (lambda _
               (chdir "zinnia")
               (for-each make-file-writable
                         '("config.log" "config.status"))
               #t)))))
      (home-page "https://taku910.github.io/zinnia/")
      (synopsis "Online hand recognition system with machine learning")
      (description
       "Zinnia is a simple, customizable and portable online hand recognition
system based on Support Vector Machines.  Zinnia simply receives user pen
strokes as a sequence of coordinate data and outputs n-best characters sorted
by SVM confidence.  To keep portability, Zinnia doesn't have any rendering
functionality.  In addition to recognition, Zinnia provides training module
that allows us to create any hand-written recognition systems with low-cost.")
      (license (list license:bsd-3 ; all files except...
                     (license:non-copyleft ; some autotools related files
                      "file://zinnia/aclocal.m4")
                     license:x11 ; 'install-sh'
                     license:public-domain))))) ; 'install-sh'

;;; python 2 bindings, license under the same terms as zinnia
(define-public python2-zinnia
  (package
    (inherit zinnia)
    (name "python2-zinnia")
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; CObject API is used, it was removed in Python 3.2
       #:tests? #f ; avoid circular dependency on tegaki-zinnia-japanese
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "zinnia/python")
             #t)))))
    (inputs
     `(("zinnia" ,zinnia)))))
