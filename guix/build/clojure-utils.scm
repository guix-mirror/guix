;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
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

(define-module (guix build clojure-utils)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%clojure-regex
            define-with-docs
            install-doc))

(define-syntax-rule (define-with-docs name docs val)
  "Create top-level variable named NAME with doc string DOCS and value VAL."
  (begin (define name val)
         (set-object-property! name 'documentation docs)))

(define-with-docs %doc-regex
  "Default regex for matching the base name of top-level documentation files."
  (format #f
          "(~a)|(\\.(html|markdown|md|txt)$)"
          (@@ (guix build guile-build-system)
              %documentation-file-regexp)))

(define* (install-doc #:key
                      doc-dirs
                      (doc-regex %doc-regex)
                      outputs
                      #:allow-other-keys)
  "Install the following to the default documentation directory:

1. Top-level files with base name matching DOC-REGEX.
2. All files (recursively) inside DOC-DIRS.

DOC-REGEX can be compiled or uncompiled."
  (let* ((out (assoc-ref outputs "out"))
         (doc (assoc-ref outputs "doc"))
         (name-ver (strip-store-file-name out))
         (dest-dir (string-append (or doc out) "/share/doc/" name-ver "/"))
         (doc-regex* (if (string? doc-regex)
                         (make-regexp doc-regex)
                         doc-regex)))
    (for-each (cut install-file <> dest-dir)
              (remove (compose file-exists?
                               (cut string-append dest-dir <>))
                      (scandir "./" (cut regexp-exec doc-regex* <>))))
    (for-each (cut copy-recursively <> dest-dir)
              doc-dirs)
    #t))
