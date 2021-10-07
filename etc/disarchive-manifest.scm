;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

;;; This file returns a manifest that builds a directory containing Disarchive
;;; metadata for all the tarballs packages refer to.

(use-modules (srfi srfi-1) (ice-9 match)
             (guix packages) (guix gexp) (guix profiles)
             (guix base16)
             (gnu packages))

(include "source-manifest.scm")

(define (tarball-origin? origin)
  (match (origin-actual-file-name origin)
    (#f #f)
    ((? string? file)
     ;; As of version 0.2.1, Disarchive can only deal with raw tarballs and
     ;; gzip-compressed tarballs.
     (and (origin-hash origin)
          (or (string-suffix? ".tar.gz" file)
              (string-suffix? ".tgz" file)
              (string-suffix? ".tar" file))))))

(define (origin->disarchive origin)
  "Return a directory containing Disarchive metadata for ORIGIN, a tarball, or
an empty directory if ORIGIN could not be disassembled."
  (define file-name
    (let ((hash (origin-hash origin)))
      (string-append (symbol->string (content-hash-algorithm hash))
                     "/"
                     (bytevector->base16-string
                      (content-hash-value hash)))))

  (define disarchive
    (specification->package "disarchive"))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-34))

          (define tarball
            #+(upstream-origin origin))

          (define file-name
            (string-append #$output "/" #$file-name))

          (define profile
            #+(profile (content (packages->manifest (list disarchive)))))

          (mkdir-p (dirname file-name))
          (setenv "PATH" (string-append profile "/bin"))
          (setenv "GUILE_LOAD_PATH"
                  (string-append profile "/share/guile/site/"
                                 (effective-version)))
          (setenv "GUILE_LOAD_COMPILED_PATH"
                  (string-append profile "/lib/guile/" (effective-version)
                                 "/site-ccache"))

          (guard (c ((invoke-error? c)
                     ;; Sometimes Disarchive fails with "could not find Gzip
                     ;; compressor".  When that happens, produce an empty
                     ;; directory instead of failing.
                     (report-invoke-error c)
                     (delete-file file-name)))
            (with-output-to-file file-name
              (lambda ()
                ;; Disarchive records the tarball name in its output.  Thus,
                ;; strip the hash from TARBALL.
                (let ((short-name (strip-store-file-name tarball)))
                  (symlink tarball short-name)
                  (invoke "disarchive" "disassemble" short-name))))))))

  (computed-file (match (origin-actual-file-name origin)
                   ((? string? str) (string-append str ".dis"))
                   (#f "anonymous-tarball.dis"))
                 build))

(define (disarchive-collection origins)
  "Return a directory containing all the Disarchive metadata for ORIGINS."
  (directory-union "disarchive-collection"
                   (filter-map (lambda (origin)
                                 (and (tarball-origin? origin)
                                      (origin->disarchive origin)))
                               origins)
                   #:copy? #t))


;; The manifest containing Disarchive data.
(let ((origins (all-origins)))
  (manifest
   (list (manifest-entry
           (name "disarchive-collection")
           (version (number->string (length origins)))
           (item (disarchive-collection origins))))))
