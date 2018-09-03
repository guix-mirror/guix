;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts describe)
  #:use-module ((guix ui) #:hide (display-profile-content))
  #:use-module (guix scripts)
  #:use-module (guix describe)
  #:use-module (guix profiles)
  #:use-module ((guix scripts pull) #:select (display-profile-content))
  #:use-module (git)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:autoload   (ice-9 pretty-print) (pretty-print)
  #:export (guix-describe))


;;;
;;; Command-line options.
;;;

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\f "format") #t #f
                (lambda (opt name arg result)
                  (unless (member arg '("human" "channels"))
                    (leave (G_ "~a: unsupported output format~%") arg))
                  (alist-cons 'format 'channels result)))
        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix describe")))))

(define %default-options
  ;; Alist of default option values.
  '((format . human)))

(define (show-help)
  (display (G_ "Usage: guix describe [OPTION]...
Display information about the channels currently in use.\n"))
  (display (G_ "
  -f, --format=FORMAT    display information in the given FORMAT"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define (display-package-search-path fmt)
  "Display GUIX_PACKAGE_PATH, if it is set, according to FMT."
  (match (getenv "GUIX_PACKAGE_PATH")
    (#f #t)
    (string
     (match fmt
       ('human
        (format #t "~%GUIX_PACKAGE_PATH=\"~a\"~%" string))
       ('channels
        (format #t (G_ "~%;; warning: GUIX_PACKAGE_PATH=\"~a\"~%")
                string))))))

(define (display-checkout-info fmt)
  "Display information about the current checkout according to FMT, a symbol
denoting the requested format.  Exit if the current directory does not lie
within a Git checkout."
  (let* ((program    (car (command-line)))
         (directory  (catch 'git-error
                       (lambda ()
                         (repository-discover (dirname program)))
                       (lambda (key err)
                         (leave (G_ "failed to determine origin~%")))))
         (repository (repository-open directory))
         (head       (repository-head repository))
         (commit     (oid->string (reference-target head))))
    (match fmt
      ('human
       (format #t (G_ "Git checkout:~%"))
       (format #t (G_ "  repository: ~a~%") (dirname directory))
       (format #t (G_ "  branch: ~a~%") (reference-shorthand head))
       (format #t (G_ "  commit: ~a~%") commit))
      ('channels
       (pretty-print `(list (channel
                             (name 'guix)
                             (url ,(dirname directory))
                             (commit ,commit))))))
    (display-package-search-path fmt)))

(define (display-profile-info profile fmt)
  "Display information about PROFILE, a profile as created by (guix channels),
in the format specified by FMT."
  (define number
    (match (profile-generations profile)
      ((_ ... last) last)))

  (match fmt
    ('human
     (display-profile-content profile number))
    ('channels
     (pretty-print
      `(list ,@(map (lambda (entry)
                      (match (assq 'source (manifest-entry-properties entry))
                        (('source ('repository ('version 0)
                                               ('url url)
                                               ('branch branch)
                                               ('commit commit)
                                               _ ...))
                         `(channel (name ',(string->symbol
                                            (manifest-entry-name entry)))
                                   (url ,url)
                                   (commit ,commit)))

                        ;; Pre-0.15.0 Guix does not provide that information,
                        ;; so there's not much we can do in that case.
                        (_ '???)))

                    ;; Show most recently installed packages last.
                    (reverse
                     (manifest-entries
                      (profile-manifest (generation-file-name profile
                                                              number)))))))))
  (display-package-search-path fmt))


;;;
;;; Entry point.
;;;

(define (guix-describe . args)
  (let* ((opts   (args-fold* args %options
                             (lambda (opt name arg result)
                               (leave (G_ "~A: unrecognized option~%")
                                      name))
                             cons
                             %default-options))
         (format (assq-ref opts 'format)))
    (with-error-handling
      (match (current-profile)
        (#f
         (display-checkout-info format))
        (profile
         (display-profile-info profile format))))))
