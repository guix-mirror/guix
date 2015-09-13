;;; guix-read.el --- Minibuffer readers

;; Copyright © 2015 Alex Kost <alezost@gmail.com>

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
(require 'guix-base)

(defun guix-read-file-name (prompt &optional dir default-filename
                                   mustmatch initial predicate)
  "Read file name.
This function is similar to `read-file-name' except it also
expands the file name."
  (expand-file-name (read-file-name prompt dir default-filename
                                    mustmatch initial predicate)))

(defmacro guix-define-reader (name read-fun completions prompt)
  "Define NAME function to read from minibuffer.
READ-FUN may be `completing-read', `completing-read-multiple' or
another function with the same arguments."
  `(defun ,name (&optional prompt initial-contents)
     (,read-fun ,(if prompt
                     `(or prompt ,prompt)
                   'prompt)
                ,completions nil nil initial-contents)))

(defmacro guix-define-readers (&rest args)
  "Define reader functions.

ARGS should have a form [KEYWORD VALUE] ...  The following
keywords are available:

  - `completions-var' - variable used to get completions.

  - `completions-getter' - function used to get completions.

  - `single-reader', `single-prompt' - name of a function to read
    a single value, and a prompt for it.

  - `multiple-reader', `multiple-prompt' - name of a function to
    read multiple values, and a prompt for it.

  - `multiple-separator' - if specified, another
    `<multiple-reader-name>-string' function returning a string
    of multiple values separated the specified separator will be
    defined."
  (let (completions-var
        completions-getter
        single-reader
        single-prompt
        multiple-reader
        multiple-prompt
        multiple-separator)

    ;; Process the keyword args.
    (while (keywordp (car args))
      (pcase (pop args)
        (`:completions-var    (setq completions-var    (pop args)))
        (`:completions-getter (setq completions-getter (pop args)))
        (`:single-reader      (setq single-reader      (pop args)))
        (`:single-prompt      (setq single-prompt      (pop args)))
        (`:multiple-reader    (setq multiple-reader    (pop args)))
        (`:multiple-prompt    (setq multiple-prompt    (pop args)))
        (`:multiple-separator (setq multiple-separator (pop args)))
	(_ (pop args))))

    (let ((completions
           (cond ((and completions-var completions-getter)
                  `(or ,completions-var
                       (setq ,completions-var
                             (funcall ',completions-getter))))
                 (completions-var
                  completions-var)
                 (completions-getter
                  `(funcall ',completions-getter)))))
      `(progn
         ,(when (and completions-var
                     (not (boundp completions-var)))
            `(defvar ,completions-var nil))

         ,(when single-reader
            `(guix-define-reader ,single-reader completing-read
                                 ,completions ,single-prompt))

         ,(when multiple-reader
            `(guix-define-reader ,multiple-reader completing-read-multiple
                                 ,completions ,multiple-prompt))

         ,(when (and multiple-reader multiple-separator)
            (let ((name (intern (concat (symbol-name multiple-reader)
                                        "-string"))))
              `(defun ,name (&optional prompt initial-contents)
                 (guix-concat-strings
                  (,multiple-reader prompt initial-contents)
                  ,multiple-separator))))))))

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

(provide 'guix-read)

;;; guix-read.el ends here
