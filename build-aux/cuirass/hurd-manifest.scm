;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;;; Commentary:
;;;
;;; This file defines a manifest with a selection of packages for Cuirass to
;;; build for GNU/Hurd.
;;;
;;; Code:

(use-modules (gnu)
             (gnu system hurd)
             (guix packages)
             (guix utils)
             (ice-9 match)
             (srfi srfi-1))

(use-package-modules
 base commencement compression file gawk gdb gettext guile
 hurd less m4 package-management python ssh version-control)

(define (input->package input)
  "Return the INPUT as package, or #f."
  (match input
    ((label (and (? package?) package))
     package)
    ((label (and (? package?) package . output))
     (cons package output))
    (_ #f)))

(define guix-dependencies
  (filter-map input->package
              (fold alist-delete (package-direct-inputs guix)
                    '("glibc-utf8-locales" "graphviz" "po4a"))))

(define (package-without-tests p)
  (package/inherit p
    (arguments
     (substitute-keyword-arguments (package-arguments p)
       ((#:tests? _ #f) #f)))))

(packages->manifest
 (cons*
  ;; where it all starts
  hello

  ;; development utililities
  diffutils file findutils gawk grep gzip less m4 openssh-sans-x tar xz

  ;; development packages
  gcc-toolchain gdb-minimal git-minimal gnu-make

  ;; guix environment guix --without-tests=python-minimal --without-tests=gettext-minimal
  (package-without-tests gettext-minimal)
  (package-without-tests python-minimal)

  (append
   guix-dependencies
   (delete guile-3.0 %base-packages/hurd))))
