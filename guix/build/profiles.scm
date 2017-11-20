;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build profiles)
  #:use-module (guix build union)
  #:use-module (guix build utils)
  #:use-module (guix search-paths)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:export (ensure-writable-directory
            build-profile))

;;; Commentary:
;;;
;;; Build a user profile (essentially the union of all the installed packages)
;;; with its associated meta-data.
;;;
;;; Code:

(define (abstract-profile profile)
  "Return a procedure that replaces PROFILE in VALUE with a reference to the
'GUIX_PROFILE' environment variable.  This allows users to specify what the
user-friendly name of the profile is, for instance ~/.guix-profile rather than
/gnu/store/...-profile."
  (let ((replacement (string-append "${GUIX_PROFILE:-" profile "}"))
        (crop        (cute string-drop <> (string-length profile))))
    (match-lambda
      ((search-path . value)
       (match (search-path-specification-separator search-path)
         (#f
          (cons search-path
                (string-append replacement (crop value))))
         ((? string? separator)
          (let ((items (string-tokenize* value separator)))
            (cons search-path
                  (string-join (map (lambda (str)
                                      (string-append replacement (crop str)))
                                    items)
                               separator)))))))))

(define (write-environment-variable-definition port)
  "Write the given environment variable definition to PORT."
  (match-lambda
    ((search-path . value)
     (display (search-path-definition search-path value #:kind 'prefix)
              port)
     (newline port))))

(define (build-etc/profile output search-paths)
  "Build the 'OUTPUT/etc/profile' shell file containing environment variable
definitions for all the SEARCH-PATHS."
  (mkdir-p (string-append output "/etc"))
  (call-with-output-file (string-append output "/etc/profile")
    (lambda (port)
      ;; The use of $GUIX_PROFILE described below is not great.  Another
      ;; option would have been to use "$1" and have users run:
      ;;
      ;;   source ~/.guix-profile/etc/profile ~/.guix-profile
      ;;
      ;; However, when 'source' is used with no arguments, $1 refers to the
      ;; first positional parameter of the calling script, so we cannot rely
      ;; on it.
      (display "\
# Source this file to define all the relevant environment variables in Bash
# for this profile.  You may want to define the 'GUIX_PROFILE' environment
# variable to point to the \"visible\" name of the profile, like this:
#
#  GUIX_PROFILE=/path/to/profile ; \\
#  source /path/to/profile/etc/profile
#
# When GUIX_PROFILE is undefined, the various environment variables refer
# to this specific profile generation.
\n" port)
      (let ((variables (evaluate-search-paths (cons $PATH search-paths)
                                              (list output))))
        (for-each (write-environment-variable-definition port)
                  (map (abstract-profile output) variables))))))

(define (ensure-writable-directory directory)
  "Ensure DIRECTORY exists and is writable.  If DIRECTORY is currently a
symlink (to a read-only directory in the store), then delete the symlink and
instead make DIRECTORY a \"real\" directory containing symlinks."
  (define (unsymlink link)
    (let* ((target (readlink link))
           ;; TARGET might itself be a symlink, so append "/" to make sure
           ;; 'scandir' enters it.
           (files  (scandir (string-append target "/")
                            (negate (cut member <> '("." ".."))))))
      (delete-file link)
      (mkdir link)
      (for-each (lambda (file)
                  (symlink (string-append target "/" file)
                           (string-append link "/" file)))
                files)))

  (catch 'system-error
    (lambda ()
      (mkdir directory))
    (lambda args
      (let ((errno (system-error-errno args)))
        (if (= errno EEXIST)
            (let ((stat (lstat directory)))
              (case (stat:type stat)
                ((symlink)
                 ;; "Unsymlink" DIRECTORY so that it is writable.
                 (unsymlink directory))
                ((directory)
                 #t)
                (else
                 (error "cannot mkdir because a same-named file exists"
                        directory))))
            (apply throw args))))))

(define* (build-profile output inputs
                        #:key manifest search-paths)
  "Build a user profile from INPUTS in directory OUTPUT.  Write MANIFEST, an
sexp, to OUTPUT/manifest.  Create OUTPUT/etc/profile with Bash definitions for
-all the variables listed in SEARCH-PATHS."
  ;; Make the symlinks.
  (union-build output inputs
               #:log-port (%make-void-port "w"))

  ;; Store meta-data.
  (call-with-output-file (string-append output "/manifest")
    (lambda (p)
      (pretty-print manifest p)))

  ;; Make sure we can write to 'OUTPUT/etc'.  'union-build' above could have
  ;; made 'etc' a symlink to a read-only sub-directory in the store so we need
  ;; to work around that.
  (ensure-writable-directory (string-append output "/etc"))

  ;; Write 'OUTPUT/etc/profile'.
  (build-etc/profile output search-paths))

;;; profile.scm ends here
