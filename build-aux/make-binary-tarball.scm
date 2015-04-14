;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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


;;;
;;; Build a self-contained tarball containing binaries for Guix and its
;;; dependencies.
;;;

(use-modules (guix)
             (guix ui)
             (gnu system install)
             (ice-9 match))

(define show-what-to-build*
  (store-lift show-what-to-build))

(define copy-file*
  (lift2 copy-file %store-monad))

(define rename-file*
  (lift2 rename-file %store-monad))

(match (command-line)
  ((_ system file)
   (with-store store
     (run-with-store store
       (mlet %store-monad ((tarball (self-contained-tarball)))
         (mbegin %store-monad
           (show-what-to-build* (list tarball))
           (built-derivations (list tarball))
           (copy-file* (derivation->output-path tarball)
                       (string-append file ".part"))
           (rename-file* (string-append file ".part") file)))
       #:system system))))
