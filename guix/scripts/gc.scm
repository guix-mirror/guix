;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts gc)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:export (guix-gc))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((action . collect-garbage)))

(define (show-help)
  (display (_ "Usage: guix gc [OPTION]... PATHS...
Invoke the garbage collector.\n"))
  (display (_ "
  -C, --collect-garbage[=MIN]
                         collect at least MIN bytes of garbage"))
  (display (_ "
  -d, --delete           attempt to delete PATHS"))
  (display (_ "
      --list-dead        list dead paths"))
  (display (_ "
      --list-live        list live paths"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define (size->number str)
  "Convert STR, a storage measurement representation such as \"1024\" or
\"1MiB\", to a number of bytes.  Raise an error if STR could not be
interpreted."
  (define unit-pos
    (string-rindex str char-set:digit))

  (define unit
    (and unit-pos (substring str (+ 1 unit-pos))))

  (let* ((numstr (if unit-pos
                     (substring str 0 (+ 1 unit-pos))
                     str))
         (num    (string->number numstr)))
    (if num
        (* num
           (match unit
             ("KiB" (expt 2 10))
             ("MiB" (expt 2 20))
             ("GiB" (expt 2 30))
             ("TiB" (expt 2 40))
             ("KB"  (expt 10 3))
             ("MB"  (expt 10 6))
             ("GB"  (expt 10 9))
             ("TB"  (expt 10 12))
             (""    1)
             (_
              (format (current-error-port) (_ "error: unknown unit: ~a~%")
                      unit)
              (exit 1))))
        (begin
          (format (current-error-port)
                  (_ "error: invalid number: ~a") numstr)
          (exit 1)))))

(define %options
  ;; Specification of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix gc")))

        (option '(#\C "collect-garbage") #f #t
                (lambda (opt name arg result)
                  (let ((result (alist-cons 'action 'collect-garbage
                                            (alist-delete 'action result))))
                   (match arg
                     ((? string?)
                      (let ((amount (size->number arg)))
                        (if arg
                            (alist-cons 'min-freed amount result)
                            (begin
                              (format (current-error-port)
                                      (_ "error: invalid amount of storage: ~a~%")
                                      arg)
                              (exit 1)))))
                     (#f result)))))
        (option '(#\d "delete") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'delete
                              (alist-delete 'action result))))
        (option '("list-dead") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'list-dead
                              (alist-delete 'action result))))
        (option '("list-live") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'list-live
                              (alist-delete 'action result))))))


;;;
;;; Entry point.
;;;

(define (guix-gc . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold args %options
               (lambda (opt name arg result)
                 (leave (_ "~A: unrecognized option~%") name))
               (lambda (arg result)
                 (alist-cons 'argument arg result))
               %default-options))

  (with-error-handling
    (let ((opts  (parse-options))
          (store (open-connection)))
      (case (assoc-ref opts 'action)
        ((collect-garbage)
         (let ((min-freed (assoc-ref opts 'min-freed)))
           (if min-freed
               (collect-garbage store min-freed)
               (collect-garbage store))))
        ((delete)
         (let ((paths (filter-map (match-lambda
                                   (('argument . arg) arg)
                                   (_ #f))
                                  opts)))
           (delete-paths store paths)))
        ((list-dead)
         (for-each (cut simple-format #t "~a~%" <>)
                   (dead-paths store)))
        ((list-live)
         (for-each (cut simple-format #t "~a~%" <>)
                   (live-paths store)))))))
