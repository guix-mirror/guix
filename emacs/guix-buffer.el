;;; guix-buffer.el --- Buffer interface for displaying data  -*- lexical-binding: t -*-

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

;; This file provides a general 'buffer' interface for displaying an
;; arbitrary data.

;;; Code:

(require 'cl-lib)
(require 'guix-history)
(require 'guix-utils)

(defvar guix-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'guix-history-back)
    (define-key map (kbd "r") 'guix-history-forward)
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "R") 'guix-buffer-redisplay)
    map)
  "Parent keymap for Guix buffer modes.")


;;; Buffer item

(cl-defstruct (guix-buffer-item
               (:constructor nil)
               (:constructor guix-buffer-make-item
                             (entries buffer-type entry-type args))
               (:copier      nil))
  entries buffer-type entry-type args)

(defvar-local guix-buffer-item nil
  "Data (structure) for the current Guix buffer.
The structure consists of the following elements:

- `entries': list of the currently displayed entries.

  Each element of the list is an alist with an entry data of the
  following form:

    ((PARAM . VAL) ...)

  PARAM is a name of the entry parameter.
  VAL is a value of this parameter.

- `entry-type': type of the currently displayed entries.

- `buffer-type': type of the current buffer.

- `args': search arguments used to get the current entries.")
(put 'guix-buffer-item 'permanent-local t)

(defmacro guix-buffer-with-item (item &rest body)
  "Evaluate BODY using buffer ITEM.
The following local variables are available inside BODY:
`%entries', `%buffer-type', `%entry-type', `%args'.
See `guix-buffer-item' for details."
  (declare (indent 1) (debug t))
  (let ((item-var (make-symbol "item")))
    `(let ((,item-var ,item))
       (let ((%entries     (guix-buffer-item-entries     ,item-var))
             (%buffer-type (guix-buffer-item-buffer-type ,item-var))
             (%entry-type  (guix-buffer-item-entry-type  ,item-var))
             (%args        (guix-buffer-item-args        ,item-var)))
         ,@body))))

(defmacro guix-buffer-with-current-item (&rest body)
  "Evaluate BODY using `guix-buffer-item'.
See `guix-buffer-with-item' for details."
  (declare (indent 0) (debug t))
  `(guix-buffer-with-item guix-buffer-item
     ,@body))

(defmacro guix-buffer-define-current-item-accessor (name)
  "Define `guix-buffer-current-NAME' function to access NAME
element of `guix-buffer-item' structure.
NAME should be a symbol."
  (let* ((name-str (symbol-name name))
         (accessor (intern (concat "guix-buffer-item-" name-str)))
         (fun-name (intern (concat "guix-buffer-current-" name-str)))
         (doc      (format "\
Return '%s' of the current Guix buffer.
See `guix-buffer-item' for details."
                           name-str)))
    `(defun ,fun-name ()
       ,doc
       (and guix-buffer-item
            (,accessor guix-buffer-item)))))

(defmacro guix-buffer-define-current-item-accessors (&rest names)
  "Define `guix-buffer-current-NAME' functions for NAMES.
See `guix-buffer-define-current-item-accessor' for details."
  `(progn
     ,@(mapcar (lambda (name)
                 `(guix-buffer-define-current-item-accessor ,name))
               names)))

(guix-buffer-define-current-item-accessors
 entries entry-type buffer-type args)

(defmacro guix-buffer-define-current-args-accessor (n prefix name)
  "Define `PREFIX-NAME' function to access Nth element of 'args'
field of `guix-buffer-item' structure.
PREFIX and NAME should be strings."
  (let ((fun-name (intern (concat prefix "-" name)))
        (doc      (format "\
Return '%s' of the current Guix buffer.
'%s' is the element number %d in 'args' of `guix-buffer-item'."
                          name name n)))
    `(defun ,fun-name ()
       ,doc
       (nth ,n (guix-buffer-current-args)))))

(defmacro guix-buffer-define-current-args-accessors (prefix &rest names)
  "Define `PREFIX-NAME' functions for NAMES.
See `guix-buffer-define-current-args-accessor' for details."
  `(progn
     ,@(cl-loop for name in names
                for i from 0
                collect `(guix-buffer-define-current-args-accessor
                          ,i ,prefix ,name))))


;;; Wrappers for defined variables

(defvar guix-buffer-data nil
  "Alist with 'buffer' data.
This alist is filled by `guix-buffer-define-interface' macro.")

(defun guix-buffer-value (buffer-type entry-type symbol)
  "Return SYMBOL's value for BUFFER-TYPE/ENTRY-TYPE from `guix-buffer-data'."
  (symbol-value
   (guix-assq-value guix-buffer-data buffer-type entry-type symbol)))

(defun guix-buffer-get-entries (buffer-type entry-type args)
  "Return ENTRY-TYPE entries.
Call an appropriate 'get-entries' function from `guix-buffer'
using ARGS as its arguments."
  (apply (guix-buffer-value buffer-type entry-type 'get-entries)
         args))

(defun guix-buffer-mode-enable (buffer-type entry-type)
  "Turn on major mode to display ENTRY-TYPE ENTRIES in BUFFER-TYPE buffer."
  (funcall (guix-buffer-value buffer-type entry-type 'mode)))

(defun guix-buffer-mode-initialize (buffer-type entry-type)
  "Set up the current BUFFER-TYPE buffer to display ENTRY-TYPE entries."
  (let ((fun (guix-buffer-value buffer-type entry-type 'mode-init)))
    (when fun
      (funcall fun))))

(defun guix-buffer-insert-entries (entries buffer-type entry-type)
  "Show ENTRY-TYPE ENTRIES in the current BUFFER-TYPE buffer."
  (funcall (guix-buffer-value buffer-type entry-type 'insert-entries)
           entries))

(defun guix-buffer-show-entries-default (entries buffer-type entry-type)
  "Show ENTRY-TYPE ENTRIES in the current BUFFER-TYPE buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (guix-buffer-mode-enable buffer-type entry-type)
    (guix-buffer-insert-entries entries buffer-type entry-type)
    (goto-char (point-min))))

(defun guix-buffer-show-entries (entries buffer-type entry-type)
  "Show ENTRY-TYPE ENTRIES in the current BUFFER-TYPE buffer."
  (funcall (guix-buffer-value buffer-type entry-type 'show-entries)
           entries))

(defun guix-buffer-message (entries buffer-type entry-type args)
  "Display a message for BUFFER-ITEM after showing entries."
  (let ((fun (guix-buffer-value buffer-type entry-type 'message)))
    (when fun
      (apply fun entries args))))

(defun guix-buffer-name (buffer-type entry-type args)
  "Return name of BUFFER-TYPE buffer for displaying ENTRY-TYPE entries."
  (let ((str-or-fun (guix-buffer-value buffer-type entry-type
                                       'buffer-name)))
    (if (stringp str-or-fun)
        str-or-fun
      (apply str-or-fun args))))

(defun guix-buffer-param-title (buffer-type entry-type param)
  "Return PARAM title for BUFFER-TYPE/ENTRY-TYPE."
  (or (guix-assq-value (guix-buffer-value buffer-type entry-type 'titles)
                       param)
      ;; Fallback to a title defined in 'info' interface.
      (unless (eq buffer-type 'info)
        (guix-assq-value (guix-buffer-value 'info entry-type 'titles)
                         param))
      (guix-symbol-title param)))

(defun guix-buffer-history-size (buffer-type entry-type)
  "Return history size for BUFFER-TYPE/ENTRY-TYPE."
  (guix-buffer-value buffer-type entry-type 'history-size))

(defun guix-buffer-revert-confirm? (buffer-type entry-type)
  "Return 'revert-confirm' value for BUFFER-TYPE/ENTRY-TYPE."
  (guix-buffer-value buffer-type entry-type 'revert-confirm))


;;; Displaying entries

(defun guix-buffer-display (buffer)
  "Switch to a Guix BUFFER."
  (pop-to-buffer buffer
                 '((display-buffer-reuse-window
                    display-buffer-same-window))))

(defun guix-buffer-history-item (buffer-item)
  "Make and return a history item for displaying BUFFER-ITEM."
  (list #'guix-buffer-set buffer-item))

(defun guix-buffer-set (buffer-item &optional history)
  "Set up the current buffer for displaying BUFFER-ITEM.
HISTORY should be one of the following:

  `nil' - do not save BUFFER-ITEM in history,

  `add' - add it to history,

  `replace' - replace the current history item."
  (guix-buffer-with-item buffer-item
    (when %entries
      ;; Set buffer item before showing entries, so that its value can
      ;; be used by the code for displaying entries.
      (setq guix-buffer-item buffer-item)
      (guix-buffer-show-entries %entries %buffer-type %entry-type)
      (when history
        (funcall (cl-ecase history
                   (add     #'guix-history-add)
                   (replace #'guix-history-replace))
                 (guix-buffer-history-item buffer-item))))
    (guix-buffer-message %entries %buffer-type %entry-type %args)))

(defun guix-buffer-display-entries-current
    (entries buffer-type entry-type args &optional history)
  "Show ENTRIES in the current Guix buffer.
See `guix-buffer-item' for the meaning of BUFFER-TYPE, ENTRY-TYPE
and ARGS, and `guix-buffer-set' for the meaning of HISTORY."
  (let ((item (guix-buffer-make-item entries buffer-type
                                     entry-type args)))
    (guix-buffer-set item history)))

(defun guix-buffer-get-display-entries-current
    (buffer-type entry-type args &optional history)
  "Search for entries and show them in the current Guix buffer.
See `guix-buffer-display-entries-current' for details."
  (guix-buffer-display-entries-current
   (guix-buffer-get-entries buffer-type entry-type args)
   buffer-type entry-type args history))

(defun guix-buffer-display-entries
    (entries buffer-type entry-type args &optional history)
  "Show ENTRIES in a BUFFER-TYPE buffer.
See `guix-buffer-display-entries-current' for details."
  (let ((buffer (get-buffer-create
                 (guix-buffer-name buffer-type entry-type args))))
    (with-current-buffer buffer
      (guix-buffer-display-entries-current
       entries buffer-type entry-type args history))
    (when entries
      (guix-buffer-display buffer))))

(defun guix-buffer-get-display-entries
    (buffer-type entry-type args &optional history)
  "Search for entries and show them in a BUFFER-TYPE buffer.
See `guix-buffer-display-entries-current' for details."
  (guix-buffer-display-entries
   (guix-buffer-get-entries buffer-type entry-type args)
   buffer-type entry-type args history))

(defun guix-buffer-revert (_ignore-auto noconfirm)
  "Update the data in the current Guix buffer.
This function is suitable for `revert-buffer-function'.
See `revert-buffer' for the meaning of NOCONFIRM."
  (guix-buffer-with-current-item
    (when (or noconfirm
              (not (guix-buffer-revert-confirm? %buffer-type %entry-type))
              (y-or-n-p "Update the current buffer? "))
      (guix-buffer-get-display-entries-current
       %buffer-type %entry-type %args 'replace))))

(defvar guix-buffer-after-redisplay-hook nil
  "Hook run by `guix-buffer-redisplay'.
This hook is called before seting up a window position.")

(defun guix-buffer-redisplay ()
  "Redisplay the current Guix buffer.
Restore the point and window positions after redisplaying.

This function does not update the buffer data, use
'\\[revert-buffer]' if you want the full update."
  (interactive)
  (let* ((old-point (point))
         ;; For simplicity, ignore an unlikely case when multiple
         ;; windows display the same buffer.
         (window (car (get-buffer-window-list (current-buffer) nil t)))
         (window-start (and window (window-start window))))
    (guix-buffer-set guix-buffer-item)
    (goto-char old-point)
    (run-hooks 'guix-buffer-after-redisplay-hook)
    (when window
      (set-window-point window (point))
      (set-window-start window window-start))))

(defun guix-buffer-redisplay-goto-button ()
  "Redisplay the current buffer and go to the next button, if needed."
  (let ((guix-buffer-after-redisplay-hook
         (cons (lambda ()
                 (unless (button-at (point))
                   (forward-button 1)))
               guix-buffer-after-redisplay-hook)))
    (guix-buffer-redisplay)))


;;; Interface definers

(defmacro guix-define-groups (type &rest args)
  "Define `guix-TYPE' and `guix-TYPE-faces' custom groups.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...

Optional keywords:

  - `:parent-group' - name of a parent custom group.

  - `:parent-faces-group' - name of a parent custom faces group.

  - `:group-doc' - docstring of a `guix-TYPE' group.

  - `:faces-group-doc' - docstring of a `guix-TYPE-faces' group."
  (declare (indent 1))
  (let* ((type-str           (symbol-name type))
         (prefix             (concat "guix-" type-str))
         (group              (intern prefix))
         (faces-group        (intern (concat prefix "-faces"))))
    (guix-keyword-args-let args
        ((parent-group       :parent-group 'guix)
         (parent-faces-group :parent-faces-group 'guix-faces)
         (group-doc          :group-doc
                             (format "Settings for '%s' buffers."
                                     type-str))
         (faces-group-doc    :faces-group-doc
                             (format "Faces for '%s' buffers."
                                     type-str)))
      `(progn
         (defgroup ,group nil
           ,group-doc
           :group ',parent-group)

         (defgroup ,faces-group nil
           ,faces-group-doc
           :group ',group
           :group ',parent-faces-group)))))

(defmacro guix-define-entry-type (entry-type &rest args)
  "Define general code for ENTRY-TYPE.
See `guix-define-groups'."
  (declare (indent 1))
  `(guix-define-groups ,entry-type
     ,@args))

(defmacro guix-define-buffer-type (buffer-type &rest args)
  "Define general code for BUFFER-TYPE.
See `guix-define-groups'."
  (declare (indent 1))
  `(guix-define-groups ,buffer-type
     ,@args))

(defmacro guix-buffer-define-interface (buffer-type entry-type &rest args)
  "Define BUFFER-TYPE interface for displaying ENTRY-TYPE entries.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...
In the following description TYPE means ENTRY-TYPE-BUFFER-TYPE.

Required keywords:

  - `:buffer-name' - default value of the generated
    `guix-TYPE-buffer-name' variable.

  - `:get-entries-function' - default value of the generated
    `guix-TYPE-get-function' variable.

  - `:show-entries-function' - default value of the generated
    `guix-TYPE-show-function' variable.

  Alternatively, if `:show-entries-function' is not specified, a
  default `guix-TYPE-show-entries' will be generated, and the
  following keyword should be specified instead:

  - `:insert-entries-function' - default value of the generated
    `guix-TYPE-insert-function' variable.

Optional keywords:

  - `:message-function' - default value of the generated
    `guix-TYPE-message-function' variable.

  - `:titles' - default value of the generated
    `guix-TYPE-titles' variable.

  - `:history-size' - default value of the generated
    `guix-TYPE-history-size' variable.

  - `:revert-confirm?' - default value of the generated
    `guix-TYPE-revert-confirm' variable.

  - `:mode-name' - name (a string appeared in the mode-line) of
     the generated `guix-TYPE-mode'.

  - `:mode-init-function' - default value of the generated
    `guix-TYPE-mode-initialize-function' variable.

  - `:reduced?' - if non-nil, generate only group, faces group
    and titles variable (if specified); all keywords become
    optional."
  (declare (indent 2))
  (let* ((entry-type-str     (symbol-name entry-type))
         (buffer-type-str    (symbol-name buffer-type))
         (prefix             (concat "guix-" entry-type-str "-"
                                     buffer-type-str))
         (group              (intern prefix))
         (faces-group        (intern (concat prefix "-faces")))
         (get-entries-var    (intern (concat prefix "-get-function")))
         (show-entries-var   (intern (concat prefix "-show-function")))
         (show-entries-fun   (intern (concat prefix "-show-entries")))
         (message-var        (intern (concat prefix "-message-function")))
         (buffer-name-var    (intern (concat prefix "-buffer-name")))
         (titles-var         (intern (concat prefix "-titles")))
         (history-size-var   (intern (concat prefix "-history-size")))
         (revert-confirm-var (intern (concat prefix "-revert-confirm"))))
    (guix-keyword-args-let args
        ((get-entries-val    :get-entries-function)
         (show-entries-val   :show-entries-function)
         (insert-entries-val :insert-entries-function)
         (mode-name          :mode-name (capitalize prefix))
         (mode-init-val      :mode-init-function)
         (message-val        :message-function)
         (buffer-name-val    :buffer-name)
         (titles-val         :titles)
         (history-size-val   :history-size 20)
         (revert-confirm-val :revert-confirm? t)
         (reduced?           :reduced?))
      `(progn
         (defgroup ,group nil
           ,(format "Displaying '%s' entries in '%s' buffer."
                    entry-type-str buffer-type-str)
           :group ',(intern (concat "guix-" entry-type-str))
           :group ',(intern (concat "guix-" buffer-type-str)))

         (defgroup ,faces-group nil
           ,(format "Faces for displaying '%s' entries in '%s' buffer."
                    entry-type-str buffer-type-str)
           :group ',group
           :group ',(intern (concat "guix-" entry-type-str "-faces"))
           :group ',(intern (concat "guix-" buffer-type-str "-faces")))

         (defcustom ,titles-var ,titles-val
           ,(format "Alist of titles of '%s' parameters."
                    entry-type-str)
           :type '(alist :key-type symbol :value-type string)
           :group ',group)

         ,(unless reduced?
            `(progn
               (defvar ,get-entries-var ,get-entries-val
                 ,(format "\
Function used to receive '%s' entries for '%s' buffer."
                          entry-type-str buffer-type-str))

               (defvar ,show-entries-var
                 ,(or show-entries-val `',show-entries-fun)
                 ,(format "\
Function used to show '%s' entries in '%s' buffer."
                          entry-type-str buffer-type-str))

               (defvar ,message-var ,message-val
                 ,(format "\
Function used to display a message after showing '%s' entries.
If nil, do not display messages."
                          entry-type-str))

               (defcustom ,buffer-name-var ,buffer-name-val
                 ,(format "\
Default name of '%s' buffer for displaying '%s' entries.
May be a string or a function returning a string.  The function
is called with the same arguments as `%S'."
                          buffer-type-str entry-type-str get-entries-var)
                 :type '(choice string function)
                 :group ',group)

               (defcustom ,history-size-var ,history-size-val
                 ,(format "\
Maximum number of items saved in history of `%S' buffer.
If 0, the history is disabled."
                          buffer-name-var)
                 :type 'integer
                 :group ',group)

               (defcustom ,revert-confirm-var ,revert-confirm-val
                 ,(format "\
If non-nil, ask to confirm for reverting `%S' buffer."
                          buffer-name-var)
                 :type 'boolean
                 :group ',group)

               (guix-alist-put!
                '((get-entries    . ,get-entries-var)
                  (show-entries   . ,show-entries-var)
                  (message        . ,message-var)
                  (buffer-name    . ,buffer-name-var)
                  (history-size   . ,history-size-var)
                  (revert-confirm . ,revert-confirm-var))
                'guix-buffer-data ',buffer-type ',entry-type)

               ,(unless show-entries-val
                  `(defun ,show-entries-fun (entries)
                     ,(format "\
Show '%s' ENTRIES in the current '%s' buffer."
                              entry-type-str buffer-type-str)
                     (guix-buffer-show-entries-default
                      entries ',buffer-type ',entry-type)))

               ,(when (or insert-entries-val
                          (null show-entries-val))
                  (let ((insert-entries-var
                         (intern (concat prefix "-insert-function"))))
                    `(progn
                       (defvar ,insert-entries-var ,insert-entries-val
                         ,(format "\
Function used to print '%s' entries in '%s' buffer."
                                  entry-type-str buffer-type-str))

                       (guix-alist-put!
                        ',insert-entries-var 'guix-buffer-data
                        ',buffer-type ',entry-type
                        'insert-entries))))

               ,(when (or mode-name
                          mode-init-val
                          (null show-entries-val))
                  (let* ((mode-str      (concat prefix "-mode"))
                         (mode-map-str  (concat mode-str "-map"))
                         (mode          (intern mode-str))
                         (parent-mode   (intern
                                         (concat "guix-" buffer-type-str
                                                 "-mode")))
                         (mode-var      (intern
                                         (concat mode-str "-function")))
                         (mode-init-var (intern
                                         (concat mode-str
                                                 "-initialize-function"))))
                    `(progn
                       (defvar ,mode-var ',mode
                         ,(format "\
Major mode for displaying '%s' entries in '%s' buffer."
                                  entry-type-str buffer-type-str))

                       (defvar ,mode-init-var ,mode-init-val
                         ,(format "\
Function used to set up '%s' buffer for displaying '%s' entries."
                                  buffer-type-str entry-type-str))

                       (define-derived-mode ,mode ,parent-mode ,mode-name
                         ,(format "\
Major mode for displaying '%s' entries in '%s' buffer.

\\{%s}"
                                  entry-type-str buffer-type-str mode-map-str)
                         (setq-local revert-buffer-function
                                     'guix-buffer-revert)
                         (setq-local guix-history-size
                                     (guix-buffer-history-size
                                      ',buffer-type ',entry-type))
                         (guix-buffer-mode-initialize
                          ',buffer-type ',entry-type))

                       (guix-alist-put!
                        ',mode-var 'guix-buffer-data
                        ',buffer-type ',entry-type 'mode)
                       (guix-alist-put!
                        ',mode-init-var 'guix-buffer-data
                        ',buffer-type ',entry-type
                        'mode-init))))))

         (guix-alist-put!
          ',titles-var 'guix-buffer-data
          ',buffer-type ',entry-type 'titles)))))


(defvar guix-buffer-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group (or "guix-buffer-with-item"
                           "guix-buffer-with-current-item"
                           "guix-buffer-define-interface"
                           "guix-define-groups"
                           "guix-define-entry-type"
                           "guix-define-buffer-type"))
            symbol-end)
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode guix-buffer-font-lock-keywords)

(provide 'guix-buffer)

;;; guix-buffer.el ends here
