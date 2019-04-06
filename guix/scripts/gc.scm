;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix scripts)
  #:use-module (guix store)
  #:use-module (guix store roots)
  #:autoload   (guix build syscalls) (free-disk-space)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
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
  (display (G_ "Usage: guix gc [OPTION]... PATHS...
Invoke the garbage collector.\n"))
  (display (G_ "
  -C, --collect-garbage[=MIN]
                         collect at least MIN bytes of garbage"))
  (display (G_ "
  -F, --free-space=FREE  attempt to reach FREE available space in the store"))
  (display (G_ "
  -d, --delete           attempt to delete PATHS"))
  (display (G_ "
      --list-roots       list the user's garbage collector roots"))
  (display (G_ "
      --optimize         optimize the store by deduplicating identical files"))
  (display (G_ "
      --list-dead        list dead paths"))
  (display (G_ "
      --list-live        list live paths"))
  (newline)
  (display (G_ "
      --references       list the references of PATHS"))
  (display (G_ "
  -R, --requisites       list the requisites of PATHS"))
  (display (G_ "
      --referrers        list the referrers of PATHS"))
  (display (G_ "
      --derivers         list the derivers of PATHS"))
  (newline)
  (display (G_ "
      --verify[=OPTS]    verify the integrity of the store; OPTS is a
                         comma-separated combination of 'repair' and
                         'contents'"))
  (display (G_ "
      --list-failures    list cached build failures"))
  (display (G_ "
      --clear-failures   remove PATHS from the set of cached failures"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define argument->verify-options
  (let ((not-comma (char-set-complement (char-set #\,)))
        (validate  (lambda (option)
                     (unless (memq option '(repair contents))
                       (leave (G_ "~a: invalid '--verify' option~%")
                              option)))))
    (lambda (arg)
      "Turn ARG into a list of symbols denoting '--verify' options."
      (if arg
          (let ((lst (map string->symbol
                          (string-tokenize arg not-comma))))
            (for-each validate lst)
            lst)
          '()))))

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
                            (leave (G_ "invalid amount of storage: ~a~%")
                                   arg))))
                     (#f result)))))
        (option '(#\F "free-space") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'free-space (size->number arg) result)))
        (option '(#\d "delete") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'delete
                              (alist-delete 'action result))))
        (option '("optimize") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'optimize
                              (alist-delete 'action result))))
        (option '("verify") #f #t
                (lambda (opt name arg result)
                  (let ((options (argument->verify-options arg)))
                    (alist-cons 'action 'verify
                                (alist-cons 'verify-options options
                                            (alist-delete 'action
                                                          result))))))
        (option '("list-roots") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'list-roots
                              (alist-delete 'action result))))
        (option '("list-dead") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'list-dead
                              (alist-delete 'action result))))
        (option '("list-live") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'list-live
                              (alist-delete 'action result))))
        (option '("references") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'list-references
                              (alist-delete 'action result))))
        (option '(#\R "requisites") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'list-requisites
                              (alist-delete 'action result))))
        (option '("referrers") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'list-referrers
                              (alist-delete 'action result))))
        (option '("derivers") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'list-derivers
                              (alist-delete 'action result))))
        (option '("list-failures") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'list-failures
                              (alist-delete 'action result))))
        (option '("clear-failures") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'action 'clear-failures
                              (alist-delete 'action result))))))


;;;
;;; Entry point.
;;;

(define (guix-gc . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (define (symlink-target file)
    (let ((s (false-if-exception (lstat file))))
      (if (and s (eq? 'symlink (stat:type s)))
          (symlink-target (readlink file))
          file)))

  (define (store-directory file)
    ;; Return the store directory that holds FILE if it's in the store,
    ;; otherwise return FILE.
    (or (and=> (string-match (string-append "^" (regexp-quote (%store-prefix))
                                            "/([^/]+)")
                             file)
               (compose (cut string-append (%store-prefix) "/" <>)
                        (cut match:substring <> 1)))
        file))

  (define (ensure-free-space store space)
    ;; Attempt to have at least SPACE bytes available in STORE.
    (let ((free (free-disk-space (%store-prefix))))
      (if (> free space)
          (info (G_ "already ~h MiBs available on ~a, nothing to do~%")
                (/ free 1024. 1024.) (%store-prefix))
          (let ((to-free (- space free)))
            (info (G_ "freeing ~h MiBs~%") (/ to-free 1024. 1024.))
            (collect-garbage store to-free)))))

  (define (list-roots)
    ;; List all the user-owned GC roots.
    (let ((roots (filter (if (zero? (getuid)) (const #t) user-owned?)
                         (gc-roots))))
      (for-each (lambda (root)
                  (display root)
                  (newline))
                roots)))

  (with-error-handling
    (let* ((opts  (parse-options))
           (store (open-connection))
           (paths (filter-map (match-lambda
                               (('argument . arg) arg)
                               (_ #f))
                              opts)))
      (define (assert-no-extra-arguments)
        (unless (null? paths)
          (leave (G_ "extraneous arguments: ~{~a ~}~%") paths)))

      (define (list-relatives relatives)
        (for-each (compose (lambda (path)
                             (for-each (cut simple-format #t "~a~%" <>)
                                       (relatives store path)))
                           store-directory
                           symlink-target)
                  paths))

      (case (assoc-ref opts 'action)
        ((collect-garbage)
         (assert-no-extra-arguments)
         (let ((min-freed  (assoc-ref opts 'min-freed))
               (free-space (assoc-ref opts 'free-space)))
           (cond
            (free-space
             (ensure-free-space store free-space))
            (min-freed
             (let-values (((paths freed) (collect-garbage store min-freed)))
              (info (G_ "freed ~h MiBs~%") (/ freed 1024. 1024.))))
            (else
             (let-values (((paths freed) (collect-garbage store)))
              (info (G_ "freed ~h MiBs~%") (/ freed 1024. 1024.)))))))
        ((list-roots)
         (assert-no-extra-arguments)
         (list-roots))
        ((delete)
         (delete-paths store (map direct-store-path paths)))
        ((list-references)
         (list-relatives references))
        ((list-requisites)
         (list-relatives (lambda (store item)
                           (requisites store (list item)))))
        ((list-referrers)
         (list-relatives referrers))
        ((list-derivers)
         (list-relatives valid-derivers))
        ((optimize)
         (assert-no-extra-arguments)
         (optimize-store store))
        ((verify)
         (assert-no-extra-arguments)
         (let ((options (assoc-ref opts 'verify-options)))
           (exit
            (verify-store store
                          #:check-contents? (memq 'contents options)
                          #:repair? (memq 'repair options)))))
        ((list-failures)
         (for-each (cut simple-format #t "~a~%" <>)
                   (query-failed-paths store)))
        ((clear-failures)
         (clear-failed-paths store (map direct-store-path paths)))
        ((list-dead)
         (for-each (cut simple-format #t "~a~%" <>)
                   (dead-paths store)))
        ((list-live)
         (for-each (cut simple-format #t "~a~%" <>)
                   (live-paths store)))))))
