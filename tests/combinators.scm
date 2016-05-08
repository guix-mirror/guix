;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (test-combinators)
  #:use-module (guix combinators)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 vlist))

(test-begin "combinators")

(test-equal "fold2, 1 list"
    (list (reverse (iota 5))
          (map - (reverse (iota 5))))
  (call-with-values
      (lambda ()
        (fold2 (lambda (i r1 r2)
                 (values (cons i r1)
                         (cons (- i) r2)))
               '() '()
               (iota 5)))
    list))

(test-equal "fold2, 2 lists"
    (list (reverse '((a . 0) (b . 1) (c . 2) (d . 3)))
          (reverse '((a . 0) (b . -1) (c . -2) (d . -3))))
  (call-with-values
      (lambda ()
        (fold2 (lambda (k v r1 r2)
                 (values (alist-cons k v r1)
                         (alist-cons k (- v) r2)))
               '() '()
               '(a b c d)
               '(0 1 2 3)))
    list))

(let* ((tree (alist->vhash
              '((0 2 3) (1 3 4) (2) (3 5 6) (4 6) (5) (6))
              hashq))
       (add-one (lambda (_ r) (1+ r)))
       (tree-lookup (lambda (n) (cdr (vhash-assq n tree)))))
  (test-equal "fold-tree, single root"
    5 (fold-tree add-one 0 tree-lookup '(0)))
  (test-equal "fold-tree, two roots"
    7 (fold-tree add-one 0 tree-lookup '(0 1)))
  (test-equal "fold-tree, sum"
    16 (fold-tree + 0 tree-lookup '(0)))
  (test-equal "fold-tree, internal"
    18 (fold-tree + 0 tree-lookup '(3 4)))
  (test-equal "fold-tree, cons"
    '(1 3 4 5 6)
    (sort (fold-tree cons '() tree-lookup '(1)) <))
  (test-equal "fold-tree, overlapping paths"
    '(1 3 4 5 6)
    (sort (fold-tree cons '() tree-lookup '(1 4)) <))
  (test-equal "fold-tree, cons, two roots"
    '(0 2 3 4 5 6)
    (sort (fold-tree cons '() tree-lookup '(0 4)) <))
  (test-equal "fold-tree-leaves, single root"
    2 (fold-tree-leaves add-one 0 tree-lookup '(1)))
  (test-equal "fold-tree-leaves, single root, sum"
    11 (fold-tree-leaves + 0 tree-lookup '(1)))
  (test-equal "fold-tree-leaves, two roots"
    3 (fold-tree-leaves add-one 0 tree-lookup '(0 1)))
  (test-equal "fold-tree-leaves, two roots, sum"
    13 (fold-tree-leaves + 0 tree-lookup '(0 1))))

(test-end)

