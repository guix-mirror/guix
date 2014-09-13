;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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
;;; Report package synopses and descriptions that defer from those found in
;;; the GNU Womb.
;;;

(use-modules (guix gnu-maintenance)
             (guix packages)
             (guix utils)
             (guix ui)
             (gnu packages)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match))

(define official
  ;; GNU package descriptors from the Womb.
  (official-gnu-packages))

(define gnus
  ;; GNU packages available in the distro.
  (let ((lookup (lambda (p)
                  (find (lambda (descriptor)
                          (equal? (gnu-package-name descriptor)
                                  (package-name p)))
                        official))))
    (fold-packages (lambda (package result)
                     (or (and=> (lookup package)
                                (cut alist-cons package <> result))
                         result))
                   '())))

(define (escape-quotes str)
  "Replace any quote character in STR by an escaped quote character."
  (list->string
   (string-fold-right (lambda (chr result)
                        (match chr
                          (#\" (cons* #\\ #\"result))
                          (_   (cons chr result))))
                      '()
                      str)))

;; Iterate over GNU packages.  Report those whose synopsis defers from that
;; found upstream.
(for-each (match-lambda
           ((package . descriptor)
            (let ((upstream   (gnu-package-doc-summary descriptor))
                  (downstream (package-synopsis package))
                  (loc        (or (package-field-location package 'synopsis)
                                  (package-location package))))
              (unless (and upstream (string=? upstream downstream))
                (format (guix-warning-port)
                        "~a: ~a: proposed synopsis: ~s~%"
                        (location->string loc) (package-name package)
                        upstream)))

            (let ((upstream   (gnu-package-doc-description descriptor))
                  (downstream (package-description package))
                  (loc        (or (package-field-location package 'description)
                                  (package-location package))))
              (when (and upstream
                         (not (string=? (fill-paragraph upstream 100)
                                        (fill-paragraph downstream 100))))
                (format (guix-warning-port)
                        "~a: ~a: proposed description:~%     \"~a\"~%"
                        (location->string loc) (package-name package)
                        (fill-paragraph (escape-quotes upstream) 77 7))))))
          gnus)
