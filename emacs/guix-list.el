;;; guix-list.el --- List buffers for displaying entries   -*- lexical-binding: t -*-

;; Copyright © 2014, 2015 Alex Kost <alezost@gmail.com>

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

;; This file provides a list-like buffer for displaying information
;; about Guix packages and generations.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'guix-info)
(require 'guix-base)
(require 'guix-entry)
(require 'guix-utils)

(defgroup guix-list nil
  "General settings for list buffers."
  :prefix "guix-list-"
  :group 'guix)

(defgroup guix-list-faces nil
  "Faces for list buffers."
  :group 'guix-list
  :group 'guix-faces)

(defface guix-list-file-path
  '((t :inherit guix-info-file-path))
  "Face used for file paths."
  :group 'guix-list-faces)

(defface guix-list-time
  '((t :inherit guix-info-time))
  "Face used for time stamps."
  :group 'guix-list-faces)

(defvar guix-list-format
  `((package
     (name guix-package-list-get-name 20 t)
     (version nil 10 nil)
     (outputs nil 13 t)
     (installed guix-package-list-get-installed-outputs 13 t)
     (synopsis guix-list-get-one-line 30 nil))
    (output
     (name guix-package-list-get-name 20 t)
     (version nil 10 nil)
     (output nil 9 t)
     (installed nil 12 t)
     (synopsis guix-list-get-one-line 30 nil))
    (generation
     (number nil 5 guix-list-sort-numerically-0 :right-align t)
     (current guix-generation-list-get-current 10 t)
     (time guix-list-get-time 20 t)
     (path guix-list-get-file-path 30 t)))
  "List of format values of the displayed columns.
Each element of the list has a form:

  (ENTRY-TYPE . ((PARAM VALUE-FUN WIDTH SORT . PROPS) ...))

PARAM is the name of an entry parameter of ENTRY-TYPE.

VALUE-FUN may be either nil or a function returning a value that
will be inserted.  The function is called with 2 arguments: the
first one is the value of the parameter; the second one is an
entry (alist of parameter names and values).

For the meaning of WIDTH, SORT and PROPS, see
`tabulated-list-format'.")

(defvar guix-list-column-titles
  '((generation
     (number . "N.")))
  "Column titles for list buffers.
Has the same structure as `guix-param-titles', but titles from
this list have a priority.")

(defun guix-list-describe (&optional mark-names)
  "Describe entries marked with a general mark.
'Describe' means display entries in 'info' buffer.
If no entries are marked, describe the current entry.
With prefix argument, describe entries marked with any mark."
  (interactive (list (unless current-prefix-arg '(general))))
  (let* ((ids        (or (apply #'guix-list-get-marked-id-list mark-names)
                         (list (guix-list-current-id))))
         (count      (length ids))
         (entry-type guix-entry-type))
    (when (or (<= count (guix-list-describe-warning-count entry-type))
              (y-or-n-p (format "Do you really want to describe %d entries? "
                                count)))
      (guix-list-describe-entries entry-type ids))))

(defun guix-list-describe-ids (ids)
  "Describe entries with IDS (list of identifiers)."
  (apply #'guix-get-show-entries
         guix-profile 'info guix-entry-type 'id ids))


;;; Wrappers for 'list' variables

(defvar guix-list-data nil
  "Alist with 'list' data.
This alist is filled by `guix-list-define-entry-type' macro.")

(defun guix-list-value (entry-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE from `guix-list-data'."
  (symbol-value (guix-assq-value guix-list-data entry-type symbol)))

(defun guix-list-param-title (entry-type param)
  "Return column title of an ENTRY-TYPE parameter PARAM."
  (or (guix-assq-value guix-list-column-titles
                       entry-type param)
      (guix-get-param-title entry-type param)))

(defun guix-list-format (entry-type)
  "Return column format for ENTRY-TYPE."
  (guix-assq-value guix-list-format entry-type))

(defun guix-list-displayed-params (entry-type)
  "Return a list of ENTRY-TYPE parameters that should be displayed."
  (mapcar #'car (guix-list-format entry-type)))

(defun guix-list-sort-key (entry-type)
  "Return sort key for ENTRY-TYPE."
  (guix-list-value entry-type 'sort-key))

(defun guix-list-additional-marks (entry-type)
  "Return alist of additional marks for ENTRY-TYPE."
  (guix-list-value entry-type 'marks))

(defun guix-list-describe-warning-count (entry-type)
  "Return the maximum number of ENTRY-TYPE entries to describe."
  (guix-list-value entry-type 'describe-count))

(defun guix-list-describe-entries (entry-type ids)
  "Describe ENTRY-TYPE entries with IDS in 'info' buffer"
  (funcall (guix-list-value entry-type 'describe)
           ids))


;;; Tabulated list internals

(defun guix-list-sort-numerically (column a b)
  "Compare COLUMN of tabulated entries A and B numerically.
This function is used for sort predicates for `tabulated-list-format'.
Return non-nil, if B is bigger than A."
  (cl-flet ((num (entry)
              (string-to-number (aref (cadr entry) column))))
    (> (num b) (num a))))

(defmacro guix-list-define-numerical-sorter (column)
  "Define numerical sort predicate for COLUMN.
See `guix-list-sort-numerically' for details."
  (let ((name (intern (format "guix-list-sort-numerically-%d" column)))
        (doc  (format "\
Predicate to sort tabulated list by column %d numerically.
See `guix-list-sort-numerically' for details."
                      column)))
    `(defun ,name (a b)
       ,doc
       (guix-list-sort-numerically ,column a b))))

(defmacro guix-list-define-numerical-sorters (n)
  "Define numerical sort predicates for columns from 0 to N.
See `guix-list-define-numerical-sorter' for details."
  `(progn
     ,@(mapcar (lambda (i)
                 `(guix-list-define-numerical-sorter ,i))
               (number-sequence 0 n))))

(guix-list-define-numerical-sorters 9)

(defun guix-list-tabulated-sort-key (entry-type)
  "Return ENTRY-TYPE sort key for `tabulated-list-sort-key'."
  (let ((sort-key (guix-list-sort-key entry-type)))
    (and sort-key
         (cons (guix-list-param-title entry-type (car sort-key))
               (cdr sort-key)))))

(defun guix-list-tabulated-vector (entry-type fun)
  "Call FUN on each column specification for ENTRY-TYPE.

FUN is applied to column specification as arguments (see
`guix-list-format').

Return a vector made of values of FUN calls."
  (apply #'vector
         (mapcar (lambda (col-spec)
                   (apply fun col-spec))
                 (guix-list-format entry-type))))

(defun guix-list-tabulated-format (entry-type)
  "Return ENTRY-TYPE list specification for `tabulated-list-format'."
  (guix-list-tabulated-vector
   entry-type
   (lambda (param _ &rest rest-spec)
     (cons (guix-list-param-title entry-type param)
           rest-spec))))

(defun guix-list-insert-entries (entries entry-type)
  "Display ENTRIES of ENTRY-TYPE in the current list buffer.
ENTRIES should have a form of `guix-entries'."
  (setq tabulated-list-entries
        (guix-list-tabulated-entries entries entry-type))
  (tabulated-list-print))

(defun guix-list-tabulated-entries (entries entry-type)
  "Return a list of ENTRY-TYPE values for `tabulated-list-entries'."
  (mapcar (lambda (entry)
            (list (guix-entry-id entry)
                  (guix-list-tabulated-entry entry entry-type)))
          entries))

(defun guix-list-tabulated-entry (entry entry-type)
  "Return array of values for `tabulated-list-entries'.
Parameters are taken from ENTRY-TYPE ENTRY."
  (guix-list-tabulated-vector
   entry-type
   (lambda (param fun &rest _)
     (let ((val (guix-entry-value entry param)))
       (if fun
           (funcall fun val entry)
         (guix-get-string val))))))

(defun guix-list-get-one-line (val &optional _)
  "Return one-line string from a multi-line string VAL.
VAL may be nil."
  (if val
      (guix-get-one-line val)
    (guix-get-string nil)))

(defun guix-list-get-time (seconds &optional _)
  "Return formatted time string from SECONDS."
  (guix-get-string (guix-get-time-string seconds)
                   'guix-list-time))

(defun guix-list-get-file-path (path &optional _)
  "Return PATH button specification for `tabulated-list-entries'."
  (list path
        'face 'guix-list-file-path
        'action (lambda (btn) (find-file (button-label btn)))
        'follow-link t
        'help-echo "Find file"))

(defun guix-list-current-id ()
  "Return ID of the current entry."
  (or (tabulated-list-get-id)
      (user-error "No entry here")))

(defun guix-list-current-entry ()
  "Return alist of the current entry info."
  (guix-entry-by-id (guix-list-current-id) guix-entries))

(defun guix-list-current-package-id ()
  "Return ID of the current package."
  (cl-ecase major-mode
    (guix-package-list-mode
     (guix-list-current-id))
    (guix-output-list-mode
     (guix-entry-value (guix-list-current-entry) 'package-id))))

(defun guix-list-for-each-line (fun &rest args)
  "Call FUN with ARGS for each entry line."
  (or (derived-mode-p 'guix-list-mode)
      (error "The current buffer is not in Guix List mode"))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (apply fun args)
      (forward-line))))

(defun guix-list-fold-lines (fun init)
  "Fold over entry lines in the current list buffer.
Call FUN with RESULT as argument for each line, using INIT as
the initial value of RESULT.  Return the final result."
  (let ((res init))
    (guix-list-for-each-line
     (lambda () (setq res (funcall fun res))))
    res))


;;; Marking and sorting

(defvar-local guix-list-marked nil
  "List of the marked entries.
Each element of the list has a form:

  (ID MARK-NAME . ARGS)

ID is an entry ID.
MARK-NAME is a symbol from `guix-list-marks'.
ARGS is a list of additional values.")

(defvar-local guix-list-marks nil
  "Alist of available mark names and mark characters.")

(defvar guix-list-default-marks
  '((empty   . ?\s)
    (general . ?*))
  "Alist of default mark names and mark characters.")

(defun guix-list-marks (entry-type)
  "Return alist of available marks for ENTRY-TYPE."
  (append guix-list-default-marks
          (guix-list-additional-marks entry-type)))

(defun guix-list-get-mark (name)
  "Return mark character by its NAME."
  (or (guix-assq-value guix-list-marks name)
      (error "Mark '%S' not found" name)))

(defun guix-list-get-mark-string (name)
  "Return mark string by its NAME."
  (string (guix-list-get-mark name)))

(defun guix-list-current-mark ()
  "Return mark character of the current line."
  (char-after (line-beginning-position)))

(defun guix-list-get-marked (&rest mark-names)
  "Return list of specs of entries marked with any mark from MARK-NAMES.
Entry specs are elements from `guix-list-marked' list.
If MARK-NAMES are not specified, use all marks from
`guix-list-marks' except the `empty' one."
  (or mark-names
      (setq mark-names
            (delq 'empty
                  (mapcar #'car guix-list-marks))))
  (cl-remove-if-not (lambda (assoc)
                      (memq (cadr assoc) mark-names))
                    guix-list-marked))

(defun guix-list-get-marked-args (mark-name)
  "Return list of (ID . ARGS) elements from lines marked with MARK-NAME.
See `guix-list-marked' for the meaning of ARGS."
  (mapcar (lambda (spec)
            (let ((id (car spec))
                  (args (cddr spec)))
              (cons id args)))
          (guix-list-get-marked mark-name)))

(defun guix-list-get-marked-id-list (&rest mark-names)
  "Return list of IDs of entries marked with any mark from MARK-NAMES.
See `guix-list-get-marked' for details."
  (mapcar #'car (apply #'guix-list-get-marked mark-names)))

(defun guix-list--mark (mark-name &optional advance &rest args)
  "Put a mark on the current line.
Also add the current entry to `guix-list-marked' using its ID and ARGS.
MARK-NAME is a symbol from `guix-list-marks'.
If ADVANCE is non-nil, move forward by one line after marking."
  (let ((id (guix-list-current-id)))
    (if (eq mark-name 'empty)
        (setq guix-list-marked (assq-delete-all id guix-list-marked))
      (let ((assoc (assq id guix-list-marked))
            (val (cons mark-name args)))
        (if assoc
            (setcdr assoc val)
          (push (cons id val) guix-list-marked)))))
  (tabulated-list-put-tag (guix-list-get-mark-string mark-name)
                          advance))

(defun guix-list-mark (&optional arg)
  "Mark the current line and move to the next line.
With ARG, mark all lines."
  (interactive "P")
  (if arg
      (guix-list-mark-all)
    (guix-list--mark 'general t)))

(defun guix-list-mark-all (&optional mark-name)
  "Mark all lines with MARK-NAME mark.
MARK-NAME is a symbol from `guix-list-marks'.
Interactively, put a general mark on all lines."
  (interactive)
  (or mark-name (setq mark-name 'general))
  (guix-list-for-each-line #'guix-list--mark mark-name))

(defun guix-list-unmark (&optional arg)
  "Unmark the current line and move to the next line.
With ARG, unmark all lines."
  (interactive "P")
  (if arg
      (guix-list-unmark-all)
    (guix-list--mark 'empty t)))

(defun guix-list-unmark-backward ()
  "Move up one line and unmark it."
  (interactive)
  (forward-line -1)
  (guix-list--mark 'empty))

(defun guix-list-unmark-all ()
  "Unmark all lines."
  (interactive)
  (guix-list-mark-all 'empty))

(defun guix-list-restore-marks ()
  "Put marks according to `guix-list-marked'."
  (guix-list-for-each-line
   (lambda ()
     (let ((mark-name (car (guix-assq-value guix-list-marked
                                            (guix-list-current-id)))))
       (tabulated-list-put-tag
        (guix-list-get-mark-string (or mark-name 'empty)))))))

(defun guix-list-sort (&optional n)
  "Sort guix list entries by the column at point.
With a numeric prefix argument N, sort the Nth column.
Same as `tabulated-list-sort', but also restore marks after sorting."
  (interactive "P")
  (tabulated-list-sort n)
  (guix-list-restore-marks))


(defvar guix-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap guix-root-map
                               tabulated-list-mode-map))
    (define-key map (kbd "RET") 'guix-list-describe)
    (define-key map (kbd "i")   'guix-list-describe)
    (define-key map (kbd "m")   'guix-list-mark)
    (define-key map (kbd "*")   'guix-list-mark)
    (define-key map (kbd "u")   'guix-list-unmark)
    (define-key map (kbd "DEL") 'guix-list-unmark-backward)
    (define-key map [remap tabulated-list-sort] 'guix-list-sort)
    map)
  "Parent keymap for list buffers.")

(define-derived-mode guix-list-mode tabulated-list-mode "Guix-List"
  "Parent mode for displaying information in list buffers."
  (setq tabulated-list-padding 2))

(defmacro guix-list-define-entry-type (entry-type &rest args)
  "Define common stuff for displaying ENTRY-TYPE entries in list buffers.

Remaining argument (ARGS) should have a form [KEYWORD VALUE] ...  The
following keywords are available:

  - `:sort-key' - default value of the generated
    `guix-ENTRY-TYPE-list-sort-key' variable.

  - `:describe-function' - default value of the generated
    `guix-ENTRY-TYPE-describe-function' variable.

  - `:marks' - default value of the generated
    `guix-ENTRY-TYPE-list-marks' variable.
"
  (let* ((entry-type-str     (symbol-name entry-type))
         (prefix             (concat "guix-" entry-type-str "-list"))
         (group              (intern prefix))
         (mode-str           (concat prefix "-mode"))
         (init-fun           (intern (concat prefix "-mode-initialize")))
         (describe-var       (intern (concat prefix "-describe-function")))
         (describe-count-var (intern (concat prefix
                                             "-describe-warning-count")))
         (sort-key-var       (intern (concat prefix "-sort-key")))
         (marks-var          (intern (concat prefix "-marks"))))
    (guix-keyword-args-let args
        ((describe-val       :describe-function)
         (describe-count-val :describe-count 10)
         (sort-key-val       :sort-key)
         (marks-val          :marks))
      `(progn
         (defcustom ,sort-key-var ,sort-key-val
           ,(format "\
Default sort key for 'list' buffer with '%s' entries.
Should be nil (no sort) or have a form:

  (PARAM . FLIP)

PARAM is the name of '%s' entry parameter.  For the meaning of
FLIP, see `tabulated-list-sort-key'."
                    entry-type-str entry-type-str)
           :type '(choice (const :tag "No sort" nil)
                          (cons symbol boolean))
           :group ',group)

         (defvar ,marks-var ,marks-val
           ,(format "\
Alist of additional marks for 'list' buffer with '%s' entries.
Marks from this list are used along with `guix-list-default-marks'."
                    entry-type-str))

         (defcustom ,describe-count-var ,describe-count-val
           ,(format "\
The maximum number of '%s' entries to describe without a warning.
If a user wants to describe more than this number of marked
entries, he will be prompted for confirmation.
See also `guix-list-describe'."
                    entry-type-str)
           :type 'integer
           :group ',group)

         (defvar ,describe-var ,describe-val
           ,(format "Function used to describe '%s' entries."
                    entry-type-str))

         (defun ,init-fun ()
           ,(concat "Initial settings for `" mode-str "'.")
           (setq tabulated-list-sort-key (guix-list-tabulated-sort-key
                                          ',entry-type)
                 tabulated-list-format (guix-list-tabulated-format
                                        ',entry-type))
           (setq-local guix-list-marks (guix-list-marks ',entry-type))
           (tabulated-list-init-header))

         (guix-alist-put!
          '((describe       . ,describe-var)
            (describe-count . ,describe-count-var)
            (sort-key       . ,sort-key-var)
            (marks          . ,marks-var))
          'guix-list-data ',entry-type)))))

(put 'guix-list-define-entry-type 'lisp-indent-function 'defun)

(defun guix-list-edit-package ()
  "Go to the location of the current package."
  (interactive)
  (guix-edit (guix-list-current-package-id)))


;;; Displaying packages

(guix-define-buffer-type list package)

(guix-list-define-entry-type package
  :describe-function 'guix-list-describe-ids
  :sort-key '(name)
  :marks '((install . ?I)
           (upgrade . ?U)
           (delete  . ?D)))

(defface guix-package-list-installed
  '((t :inherit guix-package-info-installed-outputs))
  "Face used if there are installed outputs for the current package."
  :group 'guix-package-list-faces)

(defface guix-package-list-obsolete
  '((t :inherit guix-package-info-obsolete))
  "Face used if a package is obsolete."
  :group 'guix-package-list-faces)

(defcustom guix-package-list-generation-marking-enabled nil
  "If non-nil, allow putting marks in a list with 'generation packages'.

By default this is disabled, because it may be confusing.  For
example a package is installed in some generation, so a user can
mark it for deletion in the list of packages from this
generation, but the package may not be installed in the latest
generation, so actually it cannot be deleted.

If you managed to understand the explanation above or if you
really know what you do or if you just don't care, you can set
this variable to t.  It should not do much harm anyway (most
likely)."
  :type 'boolean
  :group 'guix-package-list)

(let ((map guix-package-list-mode-map))
  (define-key map (kbd "e")   'guix-list-edit-package)
  (define-key map (kbd "x")   'guix-package-list-execute)
  (define-key map (kbd "i")   'guix-package-list-mark-install)
  (define-key map (kbd "d")   'guix-package-list-mark-delete)
  (define-key map (kbd "U")   'guix-package-list-mark-upgrade)
  (define-key map (kbd "^")   'guix-package-list-mark-upgrades))

(defun guix-package-list-get-name (name entry)
  "Return NAME of the package ENTRY.
Colorize it with `guix-package-list-installed' or
`guix-package-list-obsolete' if needed."
  (guix-get-string name
                   (cond ((guix-entry-value entry 'obsolete)
                          'guix-package-list-obsolete)
                         ((guix-entry-value entry 'installed)
                          'guix-package-list-installed))))

(defun guix-package-list-get-installed-outputs (installed &optional _)
  "Return string with outputs from INSTALLED entries."
  (guix-get-string
   (mapcar (lambda (entry)
             (guix-entry-value entry 'output))
           installed)))

(defun guix-package-list-marking-check ()
  "Signal an error if marking is disabled for the current buffer."
  (when (and (not guix-package-list-generation-marking-enabled)
             (or (derived-mode-p 'guix-package-list-mode)
                 (derived-mode-p 'guix-output-list-mode))
             (eq guix-search-type 'generation))
    (error "Action marks are disabled for lists of 'generation packages'")))

(defun guix-package-list-mark-outputs (mark default
                                       &optional prompt available)
  "Mark the current package with MARK and move to the next line.
If PROMPT is non-nil, use it to ask a user for outputs from
AVAILABLE list, otherwise mark all DEFAULT outputs."
  (let ((outputs (if prompt
                     (guix-completing-read-multiple
                      prompt available nil t)
                   default)))
    (apply #'guix-list--mark mark t outputs)))

(defun guix-package-list-mark-install (&optional arg)
  "Mark the current package for installation and move to the next line.
With ARG, prompt for the outputs to install (several outputs may
be separated with \",\")."
  (interactive "P")
  (guix-package-list-marking-check)
  (let* ((entry     (guix-list-current-entry))
         (all       (guix-entry-value entry 'outputs))
         (installed (guix-package-installed-outputs entry))
         (available (cl-set-difference all installed :test #'string=)))
    (or available
        (user-error "This package is already installed"))
    (guix-package-list-mark-outputs
     'install '("out")
     (and arg "Output(s) to install: ")
     available)))

(defun guix-package-list-mark-delete (&optional arg)
  "Mark the current package for deletion and move to the next line.
With ARG, prompt for the outputs to delete (several outputs may
be separated with \",\")."
  (interactive "P")
  (guix-package-list-marking-check)
  (let* ((entry (guix-list-current-entry))
         (installed (guix-package-installed-outputs entry)))
    (or installed
        (user-error "This package is not installed"))
    (guix-package-list-mark-outputs
     'delete installed
     (and arg "Output(s) to delete: ")
     installed)))

(defun guix-package-list-mark-upgrade (&optional arg)
  "Mark the current package for upgrading and move to the next line.
With ARG, prompt for the outputs to upgrade (several outputs may
be separated with \",\")."
  (interactive "P")
  (guix-package-list-marking-check)
  (let* ((entry (guix-list-current-entry))
         (installed (guix-package-installed-outputs entry)))
    (or installed
        (user-error "This package is not installed"))
    (when (or (guix-entry-value entry 'obsolete)
              (y-or-n-p "This package is not obsolete.  Try to upgrade it anyway? "))
      (guix-package-list-mark-outputs
       'upgrade installed
       (and arg "Output(s) to upgrade: ")
       installed))))

(defun guix-list-mark-package-upgrades (fun)
  "Mark all obsolete packages for upgrading.
Use FUN to perform marking of the current line.  FUN should
accept an entry as argument."
  (guix-package-list-marking-check)
  (let ((obsolete (cl-remove-if-not
                   (lambda (entry)
                     (guix-entry-value entry 'obsolete))
                   guix-entries)))
    (guix-list-for-each-line
     (lambda ()
       (let* ((id (guix-list-current-id))
              (entry (cl-find-if
                      (lambda (entry)
                        (equal id (guix-entry-id entry)))
                      obsolete)))
         (when entry
           (funcall fun entry)))))))

(defun guix-package-list-mark-upgrades ()
  "Mark all obsolete packages for upgrading."
  (interactive)
  (guix-list-mark-package-upgrades
   (lambda (entry)
     (apply #'guix-list--mark
            'upgrade nil
            (guix-package-installed-outputs entry)))))

(defun guix-list-execute-package-actions (fun)
  "Perform actions on the marked packages.
Use FUN to define actions suitable for `guix-process-package-actions'.
FUN should accept action-type as argument."
  (let ((actions (delq nil
                       (mapcar fun '(install delete upgrade)))))
    (if actions
        (guix-process-package-actions
         guix-profile actions (current-buffer))
      (user-error "No operations specified"))))

(defun guix-package-list-execute ()
  "Perform actions on the marked packages."
  (interactive)
  (guix-list-execute-package-actions #'guix-package-list-make-action))

(defun guix-package-list-make-action (action-type)
  "Return action specification for the packages marked with ACTION-TYPE.
Return nil, if there are no packages marked with ACTION-TYPE.
The specification is suitable for `guix-process-package-actions'."
  (let ((specs (guix-list-get-marked-args action-type)))
    (and specs (cons action-type specs))))


;;; Displaying outputs

(guix-define-buffer-type list output
  :buffer-name "*Guix Package List*"
  :required (package-id))

(guix-list-define-entry-type output
  :describe-function 'guix-output-list-describe
  :sort-key '(name)
  :marks '((install . ?I)
           (upgrade . ?U)
           (delete  . ?D)))

(let ((map guix-output-list-mode-map))
  (define-key map (kbd "e")   'guix-list-edit-package)
  (define-key map (kbd "x")   'guix-output-list-execute)
  (define-key map (kbd "i")   'guix-output-list-mark-install)
  (define-key map (kbd "d")   'guix-output-list-mark-delete)
  (define-key map (kbd "U")   'guix-output-list-mark-upgrade)
  (define-key map (kbd "^")   'guix-output-list-mark-upgrades))

(defun guix-output-list-mark-install ()
  "Mark the current output for installation and move to the next line."
  (interactive)
  (guix-package-list-marking-check)
  (let* ((entry     (guix-list-current-entry))
         (installed (guix-entry-value entry 'installed)))
    (if installed
        (user-error "This output is already installed")
      (guix-list--mark 'install t))))

(defun guix-output-list-mark-delete ()
  "Mark the current output for deletion and move to the next line."
  (interactive)
  (guix-package-list-marking-check)
  (let* ((entry     (guix-list-current-entry))
         (installed (guix-entry-value entry 'installed)))
    (if installed
        (guix-list--mark 'delete t)
      (user-error "This output is not installed"))))

(defun guix-output-list-mark-upgrade ()
  "Mark the current output for deletion and move to the next line."
  (interactive)
  (guix-package-list-marking-check)
  (let* ((entry     (guix-list-current-entry))
         (installed (guix-entry-value entry 'installed)))
    (or installed
        (user-error "This output is not installed"))
    (when (or (guix-entry-value entry 'obsolete)
              (y-or-n-p "This output is not obsolete.  Try to upgrade it anyway? "))
      (guix-list--mark 'upgrade t))))

(defun guix-output-list-mark-upgrades ()
  "Mark all obsolete package outputs for upgrading."
  (interactive)
  (guix-list-mark-package-upgrades
   (lambda (_) (guix-list--mark 'upgrade))))

(defun guix-output-list-execute ()
  "Perform actions on the marked outputs."
  (interactive)
  (guix-list-execute-package-actions #'guix-output-list-make-action))

(defun guix-output-list-make-action (action-type)
  "Return action specification for the outputs marked with ACTION-TYPE.
Return nil, if there are no outputs marked with ACTION-TYPE.
The specification is suitable for `guix-process-output-actions'."
  (let ((ids (guix-list-get-marked-id-list action-type)))
    (and ids (cons action-type
                   (mapcar #'guix-package-id-and-output-by-output-id
                           ids)))))

(defun guix-output-list-describe (ids)
  "Describe outputs with IDS (list of output identifiers).
See `guix-package-info-type'."
  (if (eq guix-package-info-type 'output)
      (apply #'guix-get-show-entries
             guix-profile 'info 'output 'id ids)
    (let ((pids (mapcar (lambda (oid)
                          (car (guix-package-id-and-output-by-output-id
                                oid)))
                        ids)))
      (apply #'guix-get-show-entries
             guix-profile 'info 'package 'id
             (cl-remove-duplicates pids)))))


;;; Displaying generations

(guix-define-buffer-type list generation)

(guix-list-define-entry-type generation
  :describe-function 'guix-list-describe-ids
  :sort-key '(number . t)
  :marks '((delete . ?D)))

(let ((map guix-generation-list-mode-map))
  (define-key map (kbd "RET") 'guix-generation-list-show-packages)
  (define-key map (kbd "+")   'guix-generation-list-show-added-packages)
  (define-key map (kbd "-")   'guix-generation-list-show-removed-packages)
  (define-key map (kbd "=")   'guix-generation-list-diff)
  (define-key map (kbd "D")   'guix-generation-list-diff)
  (define-key map (kbd "e")   'guix-generation-list-ediff)
  (define-key map (kbd "x")   'guix-generation-list-execute)
  (define-key map (kbd "s")   'guix-generation-list-switch)
  (define-key map (kbd "d")   'guix-generation-list-mark-delete))

(defun guix-generation-list-get-current (val &optional _)
  "Return string from VAL showing whether this generation is current.
VAL is a boolean value."
  (if val "(current)" ""))

(defun guix-generation-list-switch ()
  "Switch current profile to the generation at point."
  (interactive)
  (let* ((entry   (guix-list-current-entry))
         (current (guix-entry-value entry 'current))
         (number  (guix-entry-value entry 'number)))
    (if current
        (user-error "This generation is already the current one")
      (guix-switch-to-generation guix-profile number (current-buffer)))))

(defun guix-generation-list-show-packages ()
  "List installed packages for the generation at point."
  (interactive)
  (guix-get-show-entries guix-profile 'list guix-package-list-type
                         'generation (guix-list-current-id)))

(defun guix-generation-list-generations-to-compare ()
  "Return a sorted list of 2 marked generations for comparing."
  (let ((numbers (guix-list-get-marked-id-list 'general)))
    (if (/= (length numbers) 2)
        (user-error "2 generations should be marked for comparing")
      (sort numbers #'<))))

(defun guix-generation-list-show-added-packages ()
  "List package outputs added to the latest marked generation.
If 2 generations are marked with \\[guix-list-mark], display
outputs installed in the latest marked generation that were not
installed in the other one."
  (interactive)
  (apply #'guix-get-show-entries
         guix-profile 'list 'output 'generation-diff
         (reverse (guix-generation-list-generations-to-compare))))

(defun guix-generation-list-show-removed-packages ()
  "List package outputs removed from the latest marked generation.
If 2 generations are marked with \\[guix-list-mark], display
outputs not installed in the latest marked generation that were
installed in the other one."
  (interactive)
  (apply #'guix-get-show-entries
         guix-profile 'list 'output 'generation-diff
         (guix-generation-list-generations-to-compare)))

(defun guix-generation-list-compare (diff-fun gen-fun)
  "Run GEN-FUN on the 2 marked generations and run DIFF-FUN on the results."
  (cl-multiple-value-bind (gen1 gen2)
      (guix-generation-list-generations-to-compare)
    (funcall diff-fun
             (funcall gen-fun gen1)
             (funcall gen-fun gen2))))

(defun guix-generation-list-ediff-manifests ()
  "Run Ediff on manifests of the 2 marked generations."
  (interactive)
  (guix-generation-list-compare
   #'ediff-files
   #'guix-profile-generation-manifest-file))

(defun guix-generation-list-diff-manifests ()
  "Run Diff on manifests of the 2 marked generations."
  (interactive)
  (guix-generation-list-compare
   #'guix-diff
   #'guix-profile-generation-manifest-file))

(defun guix-generation-list-ediff-packages ()
  "Run Ediff on package outputs installed in the 2 marked generations."
  (interactive)
  (guix-generation-list-compare
   #'ediff-buffers
   #'guix-profile-generation-packages-buffer))

(defun guix-generation-list-diff-packages ()
  "Run Diff on package outputs installed in the 2 marked generations."
  (interactive)
  (guix-generation-list-compare
   #'guix-diff
   #'guix-profile-generation-packages-buffer))

(defun guix-generation-list-ediff (arg)
  "Run Ediff on package outputs installed in the 2 marked generations.
With ARG, run Ediff on manifests of the marked generations."
  (interactive "P")
  (if arg
      (guix-generation-list-ediff-manifests)
    (guix-generation-list-ediff-packages)))

(defun guix-generation-list-diff (arg)
  "Run Diff on package outputs installed in the 2 marked generations.
With ARG, run Diff on manifests of the marked generations."
  (interactive "P")
  (if arg
      (guix-generation-list-diff-manifests)
    (guix-generation-list-diff-packages)))

(defun guix-generation-list-mark-delete (&optional arg)
  "Mark the current generation for deletion and move to the next line.
With ARG, mark all generations for deletion."
  (interactive "P")
  (if arg
      (guix-list-mark-all 'delete)
    (guix-list--mark 'delete t)))

(defun guix-generation-list-execute ()
  "Delete marked generations."
  (interactive)
  (let ((marked (guix-list-get-marked-id-list 'delete)))
    (or marked
        (user-error "No generations marked for deletion"))
    (guix-delete-generations guix-profile marked (current-buffer))))

(provide 'guix-list)

;;; guix-list.el ends here
