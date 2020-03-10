;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018, 2019 Clément Lassieur <clement@lassieur.org>
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
;;; This file defines build jobs for the Hydra continuation integration
;;; tool.
;;;

(use-modules (guix inferior) (guix channels)
             (guix)
             (guix ui)
             (srfi srfi-1)
             (ice-9 match))

;; XXX: Debugging hack: since `hydra-eval-guile-jobs' redirects the output
;; port to the bit bucket, let us write to the error port instead.
(setvbuf (current-error-port) 'line)
(set-current-output-port (current-error-port))

(define (find-current-checkout arguments)
  "Find the first checkout of ARGUMENTS that provided the current file.
Return #f if no such checkout is found."
  (let ((current-root
         (canonicalize-path
          (string-append (dirname (current-filename)) "/../.."))))
    (find (lambda (argument)
            (and=> (assq-ref argument 'file-name)
                   (lambda (name)
                     (string=? name current-root)))) arguments)))

(define (hydra-jobs store arguments)
  "Return a list of jobs where each job is a NAME/THUNK pair."

  (define checkout
    (find-current-checkout arguments))

  (define commit
    (assq-ref checkout 'revision))

  (define source
    (assq-ref checkout 'file-name))

  (define instance
    (checkout->channel-instance source #:commit commit))

  (define derivation
    ;; Compute the derivation of Guix for COMMIT.
    (run-with-store store
      (channel-instances->derivation (list instance))))

  (show-what-to-build store (list derivation))
  (build-derivations store (list derivation))

  ;; Open an inferior for the just-built Guix.
  (let ((inferior (open-inferior (derivation->output-path derivation))))
    (inferior-eval '(use-modules (gnu ci) (ice-9 match)) inferior)

    (map (match-lambda
           ((name . fields)
            ;; Hydra expects a thunk, so here it is.
            (cons name (lambda () fields))))
         (inferior-eval-with-store
          inferior store
          `(lambda (store)
             (map (match-lambda
                    ((name . thunk)
                     (cons name (thunk))))
                  (hydra-jobs store '((superior-guix-checkout . ,checkout)
                                      ,@arguments))))))))
