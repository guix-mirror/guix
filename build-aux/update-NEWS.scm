;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

;;; Commentary:
;;;
;;; This script updates the list of new and updated packages in 'NEWS'.
;;;
;;; Code:

(use-modules (gnu) (guix)
             (guix build utils)
             ((guix ui) #:select (fill-paragraph))
             (srfi srfi-1)
             (srfi srfi-11)
             (ice-9 match)
             (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 vlist)
             (ice-9 pretty-print))

(define %header-rx
  (make-regexp "^\\* Changes in (version )?([0-9.]+) \\(since ([0-9.]+)\\)"))

(define (NEWS->versions port)
  "Return two values: the previous version and the current version as read
from PORT, which is an input port on the 'NEWS' file."
  (let loop ()
    (let ((line (read-line port)))
      (cond ((eof-object? line)
             (error "failed to determine previous and current version"
                    port))
            ((regexp-exec %header-rx line)
             =>
             (lambda (match)
               (values (match:substring match 3)
                       (match:substring match 2))))
            (else
             (loop))))))

(define (skip-to-org-heading port)
  "Read from PORT until an Org heading is found."
  (let loop ()
    (let ((next (peek-char port)))
      (cond ((eqv? next #\*)
             #t)
            ((eof-object? next)
             (error "next heading could not be found"))
            (else
             (read-line port)
             (loop))))))

(define (rewrite-org-section input output heading-rx proc)
  "Write to OUTPUT the text read from INPUT, but with the first Org section
matching HEADING-RX replaced by NEW-HEADING and CONTENTS."
  (let loop ()
    (let ((line (read-line input)))
      (cond ((eof-object? line)
             (error "failed to match heading regexp" heading-rx))
            ((regexp-exec heading-rx line)
             =>
             (lambda (match)
               (proc match output)
               (skip-to-org-heading input)
               (dump-port input output)
               #t))
            (else
             (display line output)
             (newline output)
             (loop))))))

(define (enumeration->paragraph lst)
  "Turn LST, a list of strings, into a single string that is a ready-to-print
paragraph."
  (fill-paragraph (string-join (sort lst string<?) ", ")
                  75))

(define (write-packages-added news-file old new)
  "Write to NEWS-FILE the list of packages added between OLD and NEW."
  (let ((added (lset-difference string=? (map car new) (map car old))))
    (with-atomic-file-replacement news-file
      (lambda (input output)
        (rewrite-org-section input output
                             (make-regexp "^(\\*+) (.*) new packages")
                             (lambda (match port)
                               (let ((stars (match:substring match 1)))
                                 (format port
                                         "~a ~a new packages~%~%"
                                         stars (length added)))))))))

(define (write-packages-updates news-file old new)
  "Write to NEWS-FILE the list of packages upgraded between OLD and NEW."
  (define important
    '("gcc-toolchain" "glibc" "binutils" "gdb"         ;toolchain
      "shepherd" "linux-libre" "xorg-server" "cups"    ;OS
      "gnome" "xfce" "enlightenment" "lxde" "mate"     ;desktop env.
      "guile" "bash" "python" "python2" "perl"         ;languages
      "ghc" "rust" "go" "julia" "r" "ocaml"
      "icedtea" "openjdk" "clojure" "sbcl" "racket"
      "emacs" "gimp" "inkscape" "libreoffice"          ;applications
      "octave" "icecat" "gnupg"))

  (let* ((table    (fold (lambda (package table)
                           (match package
                             ((name . version)
                              (vhash-cons name version table))))
                         vlist-null
                         new))
         (latest   (lambda (name)
                     (let ((versions (vhash-fold* cons '() name table)))
                       (match (sort versions version>?)
                         ((latest . _) latest)))))
         (upgraded (filter-map (match-lambda
                                 ((package . new-version)
                                  (match (assoc package old)
                                    ((_ . old-version)
                                     (and (string=? new-version
                                                    (latest package))
                                          (version>? new-version old-version)
                                          (cons package new-version)))
                                    (_ #f))))
                               new))
         (noteworthy (filter (match-lambda
                               ((package . version)
                                (member package important)))
                             upgraded)))
    (with-atomic-file-replacement news-file
      (lambda (input output)
        (rewrite-org-section input output
                             (make-regexp "^(\\*+) (.*) package updates")
                             (lambda (match port)
                               (let ((stars (match:substring match 1))
                                     (lst   (map (match-lambda
                                                   ((package . version)
                                                    (string-append package " "
                                                                   version)))
                                                 noteworthy)))
                                 (format port
                                         "~a ~a package updates~%~%Noteworthy updates:~%~a~%~%"
                                         stars (length upgraded)
                                         (enumeration->paragraph lst)))))))))


(define (main . args)
  (match args
    ((news-file data-directory)
     ;; Don't browse things listed in the user's $GUIX_PACKAGE_PATH and
     ;; in external channels.
     (parameterize ((%package-module-path
                     %default-package-module-path))
       (define (package-file version)
         (string-append data-directory "/packages-"
                        version ".txt"))

       (let-values (((previous-version new-version)
                     (call-with-input-file news-file NEWS->versions)))
         (format (current-error-port) "Updating NEWS for ~a to ~a...~%"
                 previous-version new-version)
         (let* ((old (call-with-input-file (package-file previous-version)
                       read))
                (new (fold-packages (lambda (p r)
                                      (alist-cons (package-name p) (package-version p)
                                                  r))
                                    '())))
           (call-with-output-file (package-file new-version)
             (lambda (port)
               (pretty-print new port)))

           (write-packages-added news-file old new)
           (write-packages-updates news-file old new)))))
    (x
     (format (current-error-port) "Usage: update-NEWS NEWS-FILE DATA-DIRECTORY

Update the list of new and updated packages in NEWS-FILE using the
previous-version package list from DATA-DIRECTORY.\n")
     (exit 1))))

(apply main (cdr (command-line)))
