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

;;;
;;; This file defines build jobs for the Cuirass continuation integration
;;; tool.
;;;

(include-from-path "build-aux/hydra/gnu-system.scm")

(use-modules ((guix licenses)
              #:select (license? license-name license-uri license-comment)))

(define (cuirass-jobs store arguments)
  "Return Cuirass jobs."
  (map hydra-job->cuirass-job (hydra-jobs store arguments)))

(define (hydra-job->cuirass-job hydra-job)
  (let ((name (car hydra-job))
        (job ((cdr hydra-job))))
    (lambda _ (acons #:job-name (symbol->string name)
                     (map symbol-alist-entry->keyword-alist-entry job)))))

(define (symbol-alist-entry->keyword-alist-entry entry)
  (cons (symbol->keyword (car entry)) (entry->sexp-entry (cdr entry))))

(define (entry->sexp-entry o)
  (match o
    ((? license?) `((name . (license-name o))
                    (uri . ,(license-uri o))
                    (comment . ,(license-comment o))))
    (_ o)))
