;;; guix-read.el --- Minibuffer readers

;; Copyright © 2015, 2016 Alex Kost <alezost@gmail.com>

;; This file is part of GNU Guix.

;; GNU Guix is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Guix is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides functions to prompt a user for packages, system
;; types, hash formats and other guix related stuff.

;;; Code:

(require 'guix-help-vars)
(require 'guix-utils)
(require 'guix-backend)
(require 'guix-guile)


;;; Receivable lists of packages, lint checkers, etc.

(guix-memoized-defun guix-graph-type-names ()
  "Return a list of names of available graph node types."
  (guix-eval-read (guix-make-guile-expression 'graph-type-names)))

(guix-memoized-defun guix-refresh-updater-names ()
  "Return a list of names of available refresh updater types."
  (guix-eval-read (guix-make-guile-expression 'refresh-updater-names)))

(guix-memoized-defun guix-lint-checker-names ()
  "Return a list of names of available lint checkers."
  (guix-eval-read (guix-make-guile-expression 'lint-checker-names)))

(guix-memoized-defun guix-package-names ()
  "Return a list of names of available packages."
  (sort
   ;; Work around <https://github.com/jaor/geiser/issues/64>:
   ;; list of strings is parsed much slower than list of lists,
   ;; so we use 'package-names-lists' instead of 'package-names'.

   ;; (guix-eval-read (guix-make-guile-expression 'package-names))

   (mapcar #'car
           (guix-eval-read (guix-make-guile-expression
                            'package-names-lists)))
   #'string<))

(guix-memoized-defun guix-license-names ()
  "Return a list of names of available licenses."
  (guix-eval-read (guix-make-guile-expression 'license-names)))

(guix-memoized-defun guix-package-locations ()
  "Return a list of available package locations."
  (sort (guix-eval-read (guix-make-guile-expression
                         'package-location-files))
        #'string<))


;;; Readers

(guix-define-readers
 :completions-var guix-help-system-types
 :single-reader guix-read-system-type
 :single-prompt "System type: ")

(guix-define-readers
 :completions-var guix-help-source-types
 :single-reader guix-read-source-type
 :single-prompt "Source type: ")

(guix-define-readers
 :completions-var guix-help-hash-formats
 :single-reader guix-read-hash-format
 :single-prompt "Hash format: ")

(guix-define-readers
 :completions-var guix-help-refresh-subsets
 :single-reader guix-read-refresh-subset
 :single-prompt "Refresh subset: ")

(guix-define-readers
 :completions-getter guix-refresh-updater-names
 :multiple-reader guix-read-refresh-updater-names
 :multiple-prompt "Refresh updater,s: "
 :multiple-separator ",")

(guix-define-readers
 :completions-var guix-help-key-policies
 :single-reader guix-read-key-policy
 :single-prompt "Key policy: ")

(guix-define-readers
 :completions-var guix-help-elpa-archives
 :single-reader guix-read-elpa-archive
 :single-prompt "ELPA archive: ")

(guix-define-readers
 :completions-var guix-help-verify-options
 :multiple-reader guix-read-verify-options
 :multiple-prompt "Verify option,s: "
 :multiple-separator ",")

(guix-define-readers
 :completions-getter guix-graph-type-names
 :single-reader guix-read-graph-type
 :single-prompt "Graph node type: ")

(guix-define-readers
 :completions-getter guix-lint-checker-names
 :multiple-reader guix-read-lint-checker-names
 :multiple-prompt "Linter,s: "
 :multiple-separator ",")

(guix-define-readers
 :completions-getter guix-package-names
 :single-reader guix-read-package-name
 :single-prompt "Package: "
 :multiple-reader guix-read-package-names
 :multiple-prompt "Package,s: "
 :multiple-separator " ")

(guix-define-readers
 :completions-getter guix-license-names
 :single-reader guix-read-license-name
 :single-prompt "License: ")

(guix-define-readers
 :completions-getter guix-package-locations
 :single-reader guix-read-package-location
 :single-prompt "Location: ")

(provide 'guix-read)

;;; guix-read.el ends here
