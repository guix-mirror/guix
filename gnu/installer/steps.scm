;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer steps)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (&installer-step-abort
            installer-step-abort?

            &installer-step-break
            installer-step-break?

            <installer-step>
            installer-step
            make-installer-step
            installer-step?
            installer-step-id
            installer-step-description
            installer-step-compute
            installer-step-configuration-proc

            run-installer-steps
            find-step-by-id
            result->step-ids
            result-step
            result-step-done?))

;; This condition may be raised to abort the current step.
(define-condition-type &installer-step-abort &condition
  installer-step-abort?)

;; This condition may be raised to break out from the steps execution.
(define-condition-type &installer-step-break &condition
  installer-step-break?)

;; An installer-step record is basically an id associated to a compute
;; procedure. The COMPUTE procedure takes exactly one argument, an association
;; list containing the results of previously executed installer-steps (see
;; RUN-INSTALLER-STEPS description). The value returned by the COMPUTE
;; procedure will be stored in the results list passed to the next
;; installer-step and so on.
(define-record-type* <installer-step>
  installer-step make-installer-step
  installer-step?
  (id                           installer-step-id) ;symbol
  (description                  installer-step-description ;string
                                (default #f))
  (compute                      installer-step-compute) ;procedure
  (configuration-format-proc    installer-step-configuration-proc ;procedure
                                (default #f)))

(define* (run-installer-steps #:key
                              steps
                              (rewind-strategy 'previous)
                              (menu-proc (const #f)))
  "Run the COMPUTE procedure of all <installer-step> records in STEPS
sequencially. If the &installer-step-abort condition is raised, fallback to a
previous install-step, accordingly to the specified REWIND-STRATEGY.

REWIND-STRATEGY possible values are 'previous, 'menu and 'start.  If 'previous
is selected, the execution will resume at the previous installer-step. If
'menu is selected, the MENU-PROC procedure will be called. Its return value
has to be an installer-step ID to jump to. The ID has to be the one of a
previously executed step. It is impossible to jump forward. Finally if 'start
is selected, the execution will resume at the first installer-step.

The result of every COMPUTE procedures is stored in an association list, under
the form:

		'((STEP-ID . COMPUTE-RESULT) ...)

where STEP-ID is the ID field of the installer-step and COMPUTE-RESULT the
result of the associated COMPUTE procedure. This result association list is
passed as argument of every COMPUTE procedure. It is finally returned when the
computation is over.

If the &installer-step-break condition is raised, stop the computation and
return the accumalated result so far."
  (define (pop-result list)
    (cdr list))

  (define (first-step? steps step)
    (match steps
      ((first-step . rest-steps)
       (equal? first-step step))))

  (define* (skip-to-step step result
                         #:key todo-steps done-steps)
    (match (list todo-steps done-steps)
      (((todo . rest-todo) (prev-done ... last-done))
       (if (eq? (installer-step-id todo)
                (installer-step-id step))
           (run result
                #:todo-steps todo-steps
                #:done-steps done-steps)
           (skip-to-step step (pop-result result)
                         #:todo-steps (cons last-done todo-steps)
                         #:done-steps prev-done)))))

  (define* (run result #:key todo-steps done-steps)
    (match todo-steps
      (() (reverse result))
      ((step . rest-steps)
       (guard (c ((installer-step-abort? c)
                  (case rewind-strategy
                    ((previous)
                     (match done-steps
                       (()
                        ;; We cannot go previous the first step. So re-raise
                        ;; the exception. It might be useful in the case of
                        ;; nested run-installer-steps. Abort to 'raise-above
                        ;; prompt to prevent the condition from being catched
                        ;; by one of the previously installed guard.
                        (abort-to-prompt 'raise-above c))
                       ((prev-done ... last-done)
                        (run (pop-result result)
                             #:todo-steps (cons last-done todo-steps)
                             #:done-steps prev-done))))
                    ((menu)
                     (let ((goto-step (menu-proc
                                       (append done-steps (list step)))))
                       (if (eq? goto-step step)
                           (run result
                                #:todo-steps todo-steps
                                #:done-steps done-steps)
                           (skip-to-step goto-step result
                                         #:todo-steps todo-steps
                                         #:done-steps done-steps))))
                    ((start)
                     (if (null? done-steps)
                         ;; Same as above, it makes no sense to jump to start
                         ;; when we are at the first installer-step. Abort to
                         ;; 'raise-above prompt to re-raise the condition.
                         (abort-to-prompt 'raise-above c)
                         (run '()
                              #:todo-steps steps
                              #:done-steps '())))))
                 ((installer-step-break? c)
                  (reverse result)))
         (let* ((id (installer-step-id step))
                (compute (installer-step-compute step))
                (res (compute result)))
           (run (alist-cons id res result)
                #:todo-steps rest-steps
                #:done-steps (append done-steps (list step))))))))

  (call-with-prompt 'raise-above
    (lambda ()
      (run '()
           #:todo-steps steps
           #:done-steps '()))
    (lambda (k condition)
      (raise condition))))

(define (find-step-by-id steps id)
  "Find and return the step in STEPS whose id is equal to ID."
  (find (lambda (step)
          (eq? (installer-step-id step) id))
        steps))

(define (result-step results step-id)
  "Return the result of the installer-step specified by STEP-ID in
RESULTS."
  (assoc-ref results step-id))

(define (result-step-done? results step-id)
  "Return #t if the installer-step specified by STEP-ID has a COMPUTE value
stored in RESULTS. Return #f otherwise."
  (and (assoc step-id results) #t))
