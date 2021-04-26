;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (guix build copy-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            copy-build))

;; Commentary:
;;
;; System for building packages that don't require much compilation, mostly
;; only to copy files around.
;;
;; Code:

(define* (install #:key install-plan outputs #:allow-other-keys)
  "Copy files from the \"source\" build input to the \"out\" output according to INSTALL-PLAN.

An install plan is a list of plans in the form:

  (SOURCE TARGET [FILTERS])

In the above, FILTERS are optional.

- When SOURCE matches a file or directory without trailing slash, install it to
  TARGET.
  - If TARGET has a trailing slash, install SOURCE basename beneath TARGET.
  - Otherwise install SOURCE as TARGET.

- When SOURCE is a directory with a trailing slash, or when FILTERS are used,
  the trailing slash of TARGET is implied.
  - Without FILTERS, install the full SOURCE _content_ to TARGET.
    The paths relative to SOURCE are preserved within TARGET.
  - With FILTERS among `#:include`, `#:include-regexp`, `#:exclude`,
    `#:exclude-regexp`:
    - With `#:include`, install only the paths which suffix exactly matches
      one of the elements in the list.
    - With `#:include-regexp`, install subpaths matching the regexps in the list.
    - The `#:exclude*` FILTERS work similarly.  Without `#:include*` flags,
      install every subpath but the files matching the `#:exclude*` filters.
      If both `#:include*` and `#:exclude*` are specified, the exclusion is done
      on the inclusion list.

Examples:

- `(\"foo/bar\" \"share/my-app/\")`: Install bar to \"share/my-app/bar\".
- `(\"foo/bar\" \"share/my-app/baz\")`: Install bar to \"share/my-app/baz\".
- `(\"foo/\" \"share/my-app\")`: Install the content of foo inside \"share/my-app\",
  e.g. install \"foo/sub/file\" to \"share/my-app/sub/file\".
- `(\"foo/\" \"share/my-app\" #:include (\"sub/file\"))`: Install only \"foo/sub/file\" to
\"share/my-app/sub/file\".
- `(\"foo/sub\" \"share/my-app\" #:include (\"file\"))`: Install \"foo/sub/file\" to
\"share/my-app/file\"."
  (define (install-simple source target)
    "Install SOURCE to TARGET.
TARGET must point to a store location.
SOURCE may be a file or a directory.
If a directory, the directory itself is installed, not its content.
if TARGET ends with a '/', the source is installed underneath."
    (let ((target (if (string-suffix? "/" target)
                      (string-append target (basename source))
                      target)))
      (mkdir-p (dirname target))
      (copy-recursively source target)))

  (define (install-file file target)
    (let ((dest (string-append target
                               (if (string-suffix? "/" target)
                                   (string-append "/" file)
                                   file))))
      (format (current-output-port) "`~a' -> `~a'~%" file dest)
      (mkdir-p (dirname dest))
      (let ((stat (lstat file)))
        (case (stat:type stat)
          ((symlink)
           (let ((target (readlink file)))
             (symlink target dest)))
          (else
           (copy-file file dest))))))

  (define* (make-file-predicate suffixes matches-regexp #:optional (default-value #t))
    "Return a predicate that returns #t if its file argument matches the
SUFFIXES or the MATCHES-REGEXP.  If neither SUFFIXES nor MATCHES-REGEXP is
given, then the predicate always returns DEFAULT-VALUE."
    (if (or suffixes matches-regexp)
        (let* ((suffixes (or suffixes '()))
               (regexps (map make-regexp (or matches-regexp '())))
               (predicates (append
                            (map (lambda (str)
                                   (cut string-suffix? str <>))
                                 suffixes)
                            (map (lambda (regexp)
                                   (cut regexp-exec regexp <>))
                                 regexps))))
          (lambda (file)
            (any (cut <> file) predicates)))
        (const default-value)))

  (define* (install-file-list source target #:key include exclude include-regexp exclude-regexp)
    ;; We must use switch current directory to source so that `find-files'
    ;; returns file paths relative to source.
    (with-directory-excursion source
      (let* ((exclusion-pred (negate (make-file-predicate exclude exclude-regexp #f)))
             (inclusion-pred (make-file-predicate include include-regexp))
             (file-list
              (filter! exclusion-pred
                       (find-files "." (lambda (file _stat)
                                         (inclusion-pred file))))))
        (map (cut install-file <> (if (string-suffix? "/" target)
                                      target
                                      (string-append target "/")))
             file-list))))

  (define* (install source target #:key include exclude include-regexp exclude-regexp)
    (let ((final-target (string-append (assoc-ref outputs "out") "/" target))
          (filters? (or include exclude include-regexp exclude-regexp)))
      (when (and (not (file-is-directory? source))
                 filters?)
        (error "Cannot use filters when SOURCE is a file."))
      (let ((multi-files-in-source?
             (or (string-suffix? "/" source)
                 (and (file-is-directory? source)
                      filters?))))
        (if multi-files-in-source?
            (install-file-list source final-target
                               #:include include
                               #:exclude exclude
                               #:include-regexp include-regexp
                               #:exclude-regexp exclude-regexp)
            (install-simple source final-target)))))

  (for-each (lambda (plan) (apply install plan)) install-plan)
  #t)

(define %standard-phases
  ;; Everything is as with the GNU Build System except for the `configure'
  ;; , `build', `check' and `install' phases.
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'build)
    (delete 'check)
    (replace 'install install)))

(define* (copy-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; copy-build-system.scm ends here
