;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Ludovic Courtès <ludo@gnu.org>
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
;;; This scripts updates the definition of the 'guix' package in Guix for the
;;; current commit.  It requires Git to be installed.
;;;
;;; Code:

(use-modules (guix)
             (guix git-download)
             (guix upstream)
             (guix utils)
             (guix base32)
             (guix build utils)
             (gnu packages package-management)
             (ice-9 match))

(define %top-srcdir
  (string-append (current-source-directory) "/.."))

(define version-controlled?
  (git-predicate %top-srcdir))

(define (package-definition-location)
  "Return the source properties of the definition of the 'guix' package."
  (call-with-input-file (location-file (package-location guix))
    (lambda (port)
      (let loop ()
        (match (read port)
          ((? eof-object?)
           (error "definition of 'guix' package could not be found"
                  (port-filename port)))
          (('define-public 'guix value)
           (source-properties value))
          (_
           (loop)))))))

(define* (update-definition commit hash
                            #:key version old-hash)
  "Return a one-argument procedure that takes a string, the definition of the
'guix' package, and returns a string, the update definition for VERSION,
COMMIT."
  (define (linear-offset str line column)
    ;; Return the offset in characters to reach LINE and COLUMN (both
    ;; zero-indexed) in STR.
    (call-with-input-string str
      (lambda (port)
        (let loop ((offset 0))
          (cond ((and (= (port-column port) column)
                      (= (port-line port) line))
                 offset)
                ((eof-object? (read-char port))
                 (error "line and column not reached!"
                        str))
                (else
                 (loop (+ 1 offset))))))))

  (define (update-hash str)
    ;; Replace OLD-HASH with HASH in STR.
    (string-replace-substring str
                              (bytevector->nix-base32-string old-hash)
                              (bytevector->nix-base32-string hash)))

  (lambda (str)
    (match (call-with-input-string str read)
      (('let (('version old-version)
              ('commit old-commit)
              ('revision old-revision))
         defn)
       (let* ((location (source-properties defn))
              (line     (assq-ref location 'line))
              (column   0)
              (offset   (linear-offset str line column)))
         (string-append (format #f "(let ((version \"~a\")
        (commit \"~a\")
        (revision ~a))\n"
                                (or version old-version)
                                commit
                                (if (and version
                                         (not (string=? version old-version)))
                                    0
                                    (+ 1 old-revision)))
                        (string-drop (update-hash str) offset))))
      (exp
       (error "'guix' package definition is not as expected" exp)))))


(define (main . args)
  (match args
    ((commit version)
     (with-store store
       (let* ((source   (add-to-store store
                                      "guix-checkout" ;dummy name
                                      #t "sha256" %top-srcdir
                                      #:select? version-controlled?))
              (hash     (query-path-hash store source))
              (location (package-definition-location))
              (old-hash (content-hash-value
                          (origin-hash (package-source guix)))))
         (edit-expression location
                          (update-definition commit hash
                                             #:old-hash old-hash
                                             #:version version))

         ;; Re-add SOURCE to the store, but this time under the real name used
         ;; in the 'origin'.  This allows us to build the package without
         ;; having to make a real checkout; thus, it also works when working
         ;; on a private branch.
         (reload-module
          (resolve-module '(gnu packages package-management)))

         (let* ((source (add-to-store store
                                      (origin-file-name (package-source guix))
                                      #t "sha256" source))
                (root   (store-path-package-name source)))

           ;; Add an indirect GC root for SOURCE in the current directory.
           (false-if-exception (delete-file root))
           (symlink source root)
           (add-indirect-root store
                              (string-append (getcwd) "/" root))

           (format #t "source code for commit ~a: ~a (GC root: ~a)~%"
                   commit source root)))))
    ((commit)
     ;; Automatically deduce the version and revision numbers.
     (main commit #f))))

(apply main (cdr (command-line)))
