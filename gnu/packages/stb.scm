;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages stb)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:select (expat public-domain)))

(define stb
  ;; stb is a collection of libraries developed within the same repository.
  ;; When updating this, remember to change versions below as appropriate.
  (let ((commit "b42009b3b9d4ca35bc703f5310eedc74f584be58")
        (revision "2"))
    (package
      (name "stb")
      (home-page "https://github.com/nothings/stb")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "1gmcjhmj62mfdscrsg2hv4j4j9v447y8zj3rbrm7mqn94cx73z1i"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((ice-9 ftw)
                    (ice-9 regex)
                    (srfi srfi-26)
                    ,@%gnu-build-system-modules)
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    (delete 'build)
                    (replace 'check
                      (lambda _
                        (invoke "make" "-C" "tests" "CC=gcc")))
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let ((out (assoc-ref outputs "out"))
                              (files (make-regexp "\\.(c|h|md)$")))
                          (for-each (lambda (file)
                                      (install-file file out))
                                    (scandir "." (cut regexp-exec files <>)))
                          #t))))))
      (synopsis "Single file libraries for C/C++")
      (description
       "This package contains a variety of small independent libraries for
the C programming language.")
      ;; The user can choose either license.
      (license (list expat public-domain)))))

(define (make-stb-header-package name version description)
  (package
    (inherit stb)
    (name name)
    (version version)
    (source #f)
    (inputs (list stb))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((stb (assoc-ref %build-inputs "stb"))
                         (lib (string-join (string-split ,name #\-) "_"))
                         (out (assoc-ref %outputs "out")))
                     (install-file (string-append stb "/" lib ".h")
                                   (string-append out "/include"))
                     #t))))
    (description description)))

;; TODO: These descriptions are not translatable!  They should be
;; converted to macros as outlined in <https://bugs.gnu.org/32155>.
(define-public stb-image
  (make-stb-header-package
   "stb-image" "2.26"
   "stb-image is a small and self-contained library for image loading or
decoding from file or memory.  A variety of formats are supported."))

(define-public stb-image-write
  (make-stb-header-package
   "stb-image-write" "1.15"
   "stb-image-write is a small library for writing image files to the
C@tie{}@code{stdio} interface."))

(define-public stb-rect-pack
  (make-stb-header-package
   "stb-rect-pack" "1.00"
   "stb-rect-pack is a small rectangle packing library useful for, e.g., packing
rectangular textures into an atlas.  It does not do rotation."))

(define-public stb-sprintf
  (make-stb-header-package
   "stb-sprintf" "1.09"
   "stb-sprintf implements fast @code{sprintf}, @code{snprintf} for C/C++."))

(define-public stb-truetype
  (make-stb-header-package
   "stb-truetype" "1.24"
   "stb-truetype is a library for parsing, decoding, and rasterizing
characters from TrueType fonts."))
