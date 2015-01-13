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

(define-module (guix sets)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:export (set
            setq
            set?
            set-insert
            set-union
            set-contains?
            set->list
            list->set
            list->setq))

;;; Commentary:
;;;
;;; A simple (simplistic?) implementation of unordered persistent sets based
;;; on vhashes that seems to be good enough so far.
;;;
;;; Another option would be to use "bounded balance trees" (Adams 1992) as
;;; implemented by Ian Price in 'pfds', which has faster union etc. but needs
;;; an order on the objects of the set.
;;;
;;; Code:

(define-record-type <set>
  (%make-set vhash insert ref)
  set?
  (vhash  set-vhash)
  (insert set-insert-proc)
  (ref    set-ref))

(define %insert
  (cut vhash-cons <> #t <>))
(define %insertq
  (cut vhash-consq <> #t <>))

(define (set . args)
  "Return a set containing the ARGS, compared as per 'equal?'."
  (list->set args))

(define (setq . args)
  "Return a set containing the ARGS, compared as per 'eq?'."
  (list->setq args))

(define (list->set lst)
  "Return a set with the elements taken from LST.  Elements of the set will be
compared with 'equal?'."
  (%make-set (fold %insert vlist-null lst)
             %insert
             vhash-assoc))

(define (list->setq lst)
  "Return a set with the elements taken from LST.  Elements of the set will be
compared with 'eq?'."
  (%make-set (fold %insertq vlist-null lst)
             %insertq
             vhash-assq))

(define-inlinable (set-contains? set value)
  "Return #t if VALUE is a member of SET."
  (->bool ((set-ref set) value (set-vhash set))))

(define (set-insert value set)
  "Insert VALUE into SET."
  (if (set-contains? set value)
      set
      (let ((vhash ((set-insert-proc set) value (set-vhash set))))
        (%make-set vhash (set-insert-proc set) (set-ref set)))))

(define-inlinable (set-size set)
  "Return the number of elements in SET."
  (vlist-length (set-vhash set)))

(define (set-union set1 set2)
  "Return the union of SET1 and SET2.  Warning: this is linear in the number
of elements of the smallest."
  (unless (eq? (set-insert-proc set1) (set-insert-proc set2))
    (error "set-union: incompatible sets"))

  (let* ((small (if (> (set-size set1) (set-size set2))
                    set2 set1))
         (large (if (eq? small set1) set2 set1)))
    (vlist-fold (match-lambda*
                 (((item . _) result)
                  (set-insert item result)))
                large
                (set-vhash small))))

(define (set->list set)
  "Return the list of elements of SET."
  (map (match-lambda
        ((key . _) key))
       (vlist->list (set-vhash set))))

;;; sets.scm ends here
