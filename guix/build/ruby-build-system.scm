;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
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

(define-module (guix build ruby-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            ruby-build))

;; Commentary:
;;
;; Builder-side code of the standard Ruby package build procedure.
;;
;; Code:

(define (first-matching-file pattern)
  "Return the first file name that matches PATTERN in the current working
directory."
  (match (find-files "." pattern)
    ((file-name . _) file-name)
    (() (error "No files matching pattern: " pattern))))

(define build
  (lambda _
    (zero? (system* "gem" "build" (first-matching-file "\\.gemspec$")))))

(define* (check #:key tests? test-target #:allow-other-keys)
  (if tests?
      (zero? (system* "rake" test-target))
      #t))

(define* (install #:key source inputs outputs #:allow-other-keys)
  (let* ((ruby-version
          (match:substring (string-match "ruby-(.*)$"
                                         (assoc-ref inputs "ruby"))
                           1))
         (gem-home (string-append (assoc-ref outputs "out")
                                  "/lib/ruby/gems/"
                                  ruby-version)))
    (setenv "GEM_HOME" gem-home)
    (mkdir-p gem-home)
    (zero? (system* "gem" "install" "--local"
                    (first-matching-file "\\.gem$")))))

(define %standard-phases
  (alist-replace
   'build build
   (alist-replace
    'install install
    (alist-replace
     'check check
     (alist-delete 'configure gnu:%standard-phases)))))

(define* (ruby-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
