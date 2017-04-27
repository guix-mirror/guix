;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages mes)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public mes
  (let ((commit   "a437c173b9da1949ad966fd50dd4f26e522a910a")
        (revision "0")
        (triplet  "i686-unknown-linux-gnu"))
    (package
      (name "mes")
      (version (string-append "0.5-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/janneke/mes")
                      (commit commit)))
                (file-name (string-append name "-" version))
                ;; TODO: Unbundle nyacc.
                (sha256
                 (base32 "1ynr0hc0k15307sgzv09k3y5rvy46h0wbh7zcblx1f9v7y8k90zv"))))
      (build-system gnu-build-system)
      (supported-systems '("x86_64-linux"))
      (native-inputs
       `(("guile" ,guile-2.2)
         ;; Use cross-compiler rather than #:system "i686-linux" to get
         ;; MesCC 64 bit .go files installed ready for use with Guile.
         ("i686-linux-binutils" ,(cross-binutils triplet))
         ("i686-linux-gcc" ,(let ((triplet triplet)) (cross-gcc triplet)))
         ("perl" ,perl)))                       ;build-aux/gitlog-to-changelog
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'generate-changelog
             (lambda _
               (with-output-to-file "ChangeLog"
                 (lambda ()
                   (display "Please run
    build-aux/gitlog-to-changelog --srcdir=<git-checkout> > ChangeLog\n")))
               #t)))))
      (synopsis "Maxwell Equations of Software")
      (description
       "Mes aims to create full source bootstrapping for GuixSD.  It
consists of a mutual self-hosting [close to Guile-] Scheme interpreter
prototype in C and a Nyacc-based C compiler in [Guile] Scheme.")
      (home-page "https://gitlab.com/janneke/mes")
      (license gpl3+))))
