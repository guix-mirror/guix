;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.


(define-module (test-build-utils)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-64))

(test-begin "build-utils")

(test-equal "alist-cons-before"
  '((a . 1) (x . 42) (b . 2) (c . 3))
  (alist-cons-before 'b 'x 42 '((a . 1) (b . 2) (c . 3))))

(test-equal "alist-cons-before, reference not found"
  '((a . 1) (b . 2) (c . 3) (x . 42))
  (alist-cons-before 'z 'x 42 '((a . 1) (b . 2) (c . 3))))

(test-equal "alist-cons-after"
  '((a . 1) (b . 2) (x . 42) (c . 3))
  (alist-cons-after 'b 'x 42 '((a . 1) (b . 2) (c . 3))))

(test-equal "alist-cons-after, reference not found"
  '((a . 1) (b . 2) (c . 3) (x . 42))
  (alist-cons-after 'z 'x 42 '((a . 1) (b . 2) (c . 3))))

(test-equal "alist-replace"
  '((a . 1) (b . 77) (c . 3))
  (alist-replace 'b 77 '((a . 1) (b . 2) (c . 3))))

(test-assert "alist-replace, key not found"
  (not (false-if-exception
        (alist-replace 'z 77 '((a . 1) (b . 2) (c . 3))))))

(test-equal "fold-port-matches"
  (make-list 3 "Guix")
  (call-with-input-string "Guix is cool, Guix rocks, and it uses Guile, Guix!"
    (lambda (port)
      (fold-port-matches cons '() "Guix" port))))

(test-equal "fold-port-matches, trickier"
  (reverse '("Guix" "guix" "Guix" "guiX" "Guix"))
  (call-with-input-string "Guix, guix, GuiGuixguiX, Guix"
    (lambda (port)
      (fold-port-matches cons '()
                         (list (char-set #\G #\g)
                               (char-set #\u)
                               (char-set #\i)
                               (char-set #\x #\X))
                         port))))

(test-equal "fold-port-matches, with unmatched chars"
  '("Guix" #\, #\space
    "guix" #\, #\space
    #\G #\u #\i "Guix" "guiX" #\, #\space
    "Guix")
  (call-with-input-string "Guix, guix, GuiGuixguiX, Guix"
    (lambda (port)
      (reverse
       (fold-port-matches cons '()
                          (list (char-set #\G #\g)
                                (char-set #\u)
                                (char-set #\i)
                                (char-set #\x #\X))
                          port
                          cons)))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'test-assert 'scheme-indent-function 1)
;;; eval: (put 'test-equal 'scheme-indent-function 1)
;;; eval: (put 'call-with-input-string 'scheme-indent-function 1)
;;; End:
