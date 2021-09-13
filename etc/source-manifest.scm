;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

;;; This file returns a manifest containing origins of all the packages.  The
;;; main purpose is to allow continuous integration services to keep upstream
;;; source code around.  It can also be passed to 'guix weather -m'.

(use-modules (srfi srfi-1) (srfi srfi-26)
             (ice-9 match) (ice-9 vlist)
             (guix packages) (guix profiles)
             (gnu packages))

(define (all-packages)
  "Return the list of all the packages, public or private, omitting only
superseded packages."
  (fold-packages (lambda (package lst)
                   (match (package-replacement package)
                     (#f (cons package lst))
                     (replacement
                      (append (list replacement package) lst))))
                 '()
                 #:select? (negate package-superseded)))

(define (upstream-origin source)
  "Return SOURCE without any patches or snippet."
  (origin (inherit source)
          (snippet #f) (patches '())))

(define (all-origins)
  "Return the list of origins referred to by all the packages."
  (let loop ((packages (all-packages))
             (origins  '())
             (visited   vlist-null))
    (match packages
      ((head . tail)
       (let ((new (remove (cut vhash-assq <> visited)
                          (package-direct-sources head))))
         (loop tail (append new origins)
               (fold (cut vhash-consq <> #t <>)
                     visited new))))
      (()
       origins))))

;; Return a manifest containing all the origins.
(manifest (map (lambda (origin)
                 (manifest-entry
                   (name (or (origin-actual-file-name origin)
                             "origin"))
                   (version "0")
                   (item (upstream-origin origin))))
               (all-origins)))
