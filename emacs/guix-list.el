;;; guix-list.el --- 'List' buffer interface for displaying data  -*- lexical-binding: t -*-

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

;; This file provides 'list' buffer interface for displaying an arbitrary
;; data.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'guix-buffer)
(require 'guix-info)
(require 'guix-entry)
(require 'guix-utils)

(guix-define-buffer-type list)

(defface guix-list-file-path
  '((t :inherit guix-info-file-path))
  "Face used for file paths."
  :group 'guix-list-faces)

(defface guix-list-time
  '((t :inherit guix-info-time))
  "Face used for time stamps."
  :group 'guix-list-faces)

(defun guix-list-describe (&optional mark-names)
  "Describe entries marked with a general mark.
'Describe' means display entries in 'info' buffer.
If no entries are marked, describe the current entry.
With prefix argument, describe entries marked with any mark."
  (interactive (list (unless current-prefix-arg '(general))))
  (let* ((ids        (or (apply #'guix-list-get-marked-id-list mark-names)
                         (list (guix-list-current-id))))
         (count      (length ids))
         (entry-type (guix-buffer-current-entry-type)))
    (when (or (<= count (guix-list-describe-warning-count entry-type))
              (y-or-n-p (format "Do you really want to describe %d entries? "
                                count)))
      (guix-list-describe-entries entry-type ids))))


;;; Wrappers for 'list' variables

(defvar guix-list-data nil
  "Alist with 'list' data.
This alist is filled by `guix-list-define-interface' macro.")

(defun guix-list-value (entry-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE from `guix-list-data'."
  (symbol-value (guix-assq-value guix-list-data entry-type symbol)))

(defun guix-list-param-title (entry-type param)
  "Return column title of an ENTRY-TYPE parameter PARAM."
  (guix-buffer-param-title 'list entry-type param))

(defun guix-list-format (entry-type)
  "Return column format for ENTRY-TYPE."
  (guix-list-value entry-type 'format))

(defun guix-list-displayed-params (entry-type)
  "Return a list of ENTRY-TYPE parameters that should be displayed."
  (mapcar #'car (guix-list-format entry-type)))

(defun guix-list-sort-key (entry-type)
  "Return sort key for ENTRY-TYPE."
  (guix-list-value entry-type 'sort-key))

(defun guix-list-additional-marks (entry-type)
  "Return alist of additional marks for ENTRY-TYPE."
  (guix-list-value entry-type 'marks))

(defun guix-list-single-entry? (entry-type)
  "Return non-nil, if a single entry of ENTRY-TYPE should be listed."
  (guix-list-value entry-type 'list-single))

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


;;; Displaying entries

(defun guix-list-get-display-entries (entry-type &rest args)
  "Search for entries and show them in a 'list' buffer preferably."
  (let ((entries (guix-buffer-get-entries 'list entry-type args)))
    (if (or (null entries)      ; = 0
            (cdr entries)       ; > 1
            (guix-list-single-entry? entry-type)
            (null (guix-buffer-value 'info entry-type 'show-entries)))
        (guix-buffer-display-entries entries 'list entry-type args 'add)
      (if (equal (guix-buffer-value 'info entry-type 'get-entries)
                 (guix-buffer-value 'list entry-type 'get-entries))
          (guix-buffer-display-entries entries 'info entry-type args 'add)
        (guix-buffer-get-display-entries 'info entry-type args 'add)))))

(defun guix-list-insert-entries (entries entry-type)
  "Print ENTRY-TYPE ENTRIES in the current buffer."
  (setq tabulated-list-entries
        (guix-list-tabulated-entries entries entry-type))
  (tabulated-list-print))

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


;;; 'List' lines

(defun guix-list-current-id ()
  "Return ID of the entry at point."
  (or (tabulated-list-get-id)
      (user-error "No entry here")))

(defun guix-list-current-entry ()
  "Return entry at point."
  (guix-entry-by-id (guix-list-current-id)
                    (guix-buffer-current-entries)))

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


;;; Major mode and interface definer

(defvar guix-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap guix-buffer-map
                               tabulated-list-mode-map))
    (define-key map (kbd "RET") 'guix-list-describe)
    (define-key map (kbd "i")   'guix-list-describe)
    (define-key map (kbd "m")   'guix-list-mark)
    (define-key map (kbd "*")   'guix-list-mark)
    (define-key map (kbd "u")   'guix-list-unmark)
    (define-key map (kbd "DEL") 'guix-list-unmark-backward)
    (define-key map [remap tabulated-list-sort] 'guix-list-sort)
    map)
  "Keymap for `guix-list-mode' buffers.")

(define-derived-mode guix-list-mode tabulated-list-mode "Guix-List"
  "Parent mode for displaying data in 'list' form.")

(defun guix-list-mode-initialize (entry-type)
  "Set up the current 'list' buffer for displaying ENTRY-TYPE entries."
  (setq tabulated-list-padding  2
        tabulated-list-format   (guix-list-tabulated-format entry-type)
        tabulated-list-sort-key (guix-list-tabulated-sort-key entry-type))
  (setq-local guix-list-marks   (guix-list-marks entry-type))
  (tabulated-list-init-header))

(defmacro guix-list-define-interface (entry-type &rest args)
  "Define 'list' interface for displaying ENTRY-TYPE entries.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...

Required keywords:

  - `:format' - default value of the generated
    `guix-ENTRY-TYPE-list-format' variable.

Optional keywords:

  - `:sort-key' - default value of the generated
    `guix-ENTRY-TYPE-list-sort-key' variable.

  - `:describe-function' - default value of the generated
    `guix-ENTRY-TYPE-describe-function' variable.

  - `:list-single?' - default value of the generated
    `guix-ENTRY-TYPE-list-single' variable.

  - `:marks' - default value of the generated
    `guix-ENTRY-TYPE-list-marks' variable.

The rest keyword arguments are passed to
`guix-buffer-define-interface' macro."
  (declare (indent 1))
  (let* ((entry-type-str     (symbol-name entry-type))
         (prefix             (concat "guix-" entry-type-str "-list"))
         (group              (intern prefix))
         (describe-var       (intern (concat prefix "-describe-function")))
         (describe-count-var (intern (concat prefix
                                             "-describe-warning-count")))
         (format-var         (intern (concat prefix "-format")))
         (sort-key-var       (intern (concat prefix "-sort-key")))
         (list-single-var    (intern (concat prefix "-single")))
         (marks-var          (intern (concat prefix "-marks"))))
    (guix-keyword-args-let args
        ((show-entries-val   :show-entries-function)
         (describe-val       :describe-function)
         (describe-count-val :describe-count 10)
         (format-val         :format)
         (sort-key-val       :sort-key)
         (list-single-val    :list-single?)
         (marks-val          :marks))
      `(progn
         (defcustom ,format-var ,format-val
           ,(format "\
List of format values of the displayed columns.
Each element of the list has a form:

  (PARAM VALUE-FUN WIDTH SORT . PROPS)

PARAM is a name of '%s' entry parameter.

VALUE-FUN may be either nil or a function returning a value that
will be inserted.  The function is called with 2 arguments: the
first one is the value of the parameter; the second one is an
entry (alist of parameter names and values).

For the meaning of WIDTH, SORT and PROPS, see
`tabulated-list-format'."
                    entry-type-str)
           :type 'sexp
           :group ',group)

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

         (defcustom ,list-single-var ,list-single-val
           ,(format "\
If non-nil, list '%s' entry even if it is the only matching result.
If nil, show a single '%s' entry in the 'info' buffer."
                    entry-type-str entry-type-str)
           :type 'boolean
           :group ',group)

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

         (guix-alist-put!
          '((describe       . ,describe-var)
            (describe-count . ,describe-count-var)
            (format         . ,format-var)
            (sort-key       . ,sort-key-var)
            (list-single    . ,list-single-var)
            (marks          . ,marks-var))
          'guix-list-data ',entry-type)

         ,(if show-entries-val
              `(guix-buffer-define-interface list ,entry-type
                 :show-entries-function ,show-entries-val
                 ,@%foreign-args)

            (let ((insert-fun    (intern (concat prefix "-insert-entries")))
                  (mode-init-fun (intern (concat prefix "-mode-initialize"))))
              `(progn
                 (defun ,insert-fun (entries)
                   ,(format "\
Print '%s' ENTRIES in the current 'list' buffer."
                            entry-type-str)
                   (guix-list-insert-entries entries ',entry-type))

                 (defun ,mode-init-fun ()
                   ,(format "\
Set up the current 'list' buffer for displaying '%s' entries."
                            entry-type-str)
                   (guix-list-mode-initialize ',entry-type))

                 (guix-buffer-define-interface list ,entry-type
                   :insert-entries-function ',insert-fun
                   :mode-init-function ',mode-init-fun
                   ,@%foreign-args))))))))


(defvar guix-list-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group "guix-list-define-interface")
            symbol-end)
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode guix-list-font-lock-keywords)

(provide 'guix-list)

;;; guix-list.el ends here
