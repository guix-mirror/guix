;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
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
  (let ((commit "2c2908f50515dcd939f24be261c3ccbcd277bb49")
        (revision "1"))
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
                  "1z753rscqc4clp0rd57bw68i60kz694y1z52bwv6slzmkgds1cki"))
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
    (inputs `(("stb" ,stb)))
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
   "stb-image" "2.22"
   "stb-image is a small and self-contained library for image loading or
decoding from file or memory.  A variety of formats are supported."))

(define-public stb-image-write
  (make-stb-header-package
   "stb-image-write" "1.13"
   "stb-image-write is a small library for writing image files to the
C@tie{}@code{stdio} interface."))
