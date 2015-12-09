;;; guix-info.el --- 'Info' buffer interface for displaying data  -*- lexical-binding: t -*-

;; Copyright © 2014, 2015 Alex Kost <alezost@gmail.com>
;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>

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

;; This file provides 'info' (help-like) buffer interface for displaying
;; an arbitrary data.

;;; Code:

(require 'guix-buffer)
(require 'guix-entry)
(require 'guix-utils)

(defgroup guix-info nil
  "General settings for info buffers."
  :prefix "guix-info-"
  :group 'guix)

(defgroup guix-info-faces nil
  "Faces for info buffers."
  :group 'guix-info
  :group 'guix-faces)

(defface guix-info-heading
  '((((type tty pc) (class color)) :weight bold)
    (t :height 1.6 :weight bold :inherit variable-pitch))
  "Face for headings."
  :group 'guix-info-faces)

(defface guix-info-param-title
  '((t :inherit font-lock-type-face))
  "Face used for titles of parameters."
  :group 'guix-info-faces)

(defface guix-info-file-path
  '((t :inherit link))
  "Face used for file paths."
  :group 'guix-info-faces)

(defface guix-info-url
  '((t :inherit link))
  "Face used for URLs."
  :group 'guix-info-faces)

(defface guix-info-time
  '((t :inherit font-lock-constant-face))
  "Face used for timestamps."
  :group 'guix-info-faces)

(defface guix-info-action-button
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black")
    (t :inherit button))
  "Face used for action buttons."
  :group 'guix-info-faces)

(defface guix-info-action-button-mouse
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "grey90" :foreground "black")
    (t :inherit highlight))
  "Mouse face used for action buttons."
  :group 'guix-info-faces)

(defcustom guix-info-ignore-empty-values nil
  "If non-nil, do not display parameters with nil values."
  :type 'boolean
  :group 'guix-info)

(defcustom guix-info-fill t
  "If non-nil, fill string parameters to fit the window.
If nil, insert text parameters (like synopsis or description) in
a raw form."
  :type 'boolean
  :group 'guix-info)

(defvar guix-info-param-title-format "%-18s: "
  "String used to format a title of a parameter.
It should be a '%s'-sequence.  After inserting a title formatted
with this string, a value of the parameter is inserted.
This string is used by `guix-info-insert-title-format'.")

(defvar guix-info-multiline-prefix
  (make-string (length (format guix-info-param-title-format " "))
               ?\s)
  "String used to format multi-line parameter values.
If a value occupies more than one line, this string is inserted
in the beginning of each line after the first one.
This string is used by `guix-info-insert-value-format'.")

(defvar guix-info-indent 2
  "Number of spaces used to indent various parts of inserted text.")

(defvar guix-info-delimiter "\n\f\n"
  "String used to separate entries.")


;;; Wrappers for 'info' variables

(defvar guix-info-data nil
  "Alist with 'info' data.
This alist is filled by `guix-info-define-interface' macro.")

(defun guix-info-value (entry-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE from `guix-info-data'."
  (symbol-value (guix-assq-value guix-info-data entry-type symbol)))

(defun guix-info-param-title (entry-type param)
  "Return a title of an ENTRY-TYPE parameter PARAM."
  (guix-buffer-param-title 'info entry-type param))

(defun guix-info-format (entry-type)
  "Return 'info' format for ENTRY-TYPE."
  (guix-info-value entry-type 'format))

(defun guix-info-displayed-params (entry-type)
  "Return a list of ENTRY-TYPE parameters that should be displayed."
  (delq nil
        (mapcar (lambda (spec)
                  (pcase spec
                    (`(,param . ,_) param)))
                (guix-info-format entry-type))))


;;; Inserting entries

(defvar guix-info-title-aliases
  '((format . guix-info-insert-title-format)
    (simple . guix-info-insert-title-simple))
  "Alist of aliases and functions to insert titles.")

(defvar guix-info-value-aliases
  '((format . guix-info-insert-value-format)
    (indent . guix-info-insert-value-indent)
    (simple . guix-info-insert-value-simple)
    (time   . guix-info-insert-time))
  "Alist of aliases and functions to insert values.")

(defun guix-info-title-function (fun-or-alias)
  "Convert FUN-OR-ALIAS into a function to insert a title."
  (or (guix-assq-value guix-info-title-aliases fun-or-alias)
      fun-or-alias))

(defun guix-info-value-function (fun-or-alias)
  "Convert FUN-OR-ALIAS into a function to insert a value."
  (or (guix-assq-value guix-info-value-aliases fun-or-alias)
      fun-or-alias))

(defun guix-info-title-method->function (method)
  "Convert title METHOD into a function to insert a title."
  (pcase method
    ((pred null) #'ignore)
    ((pred symbolp) (guix-info-title-function method))
    (`(,fun-or-alias . ,rest-args)
     (lambda (title)
       (apply (guix-info-title-function fun-or-alias)
              title rest-args)))
    (_ (error "Unknown title method '%S'" method))))

(defun guix-info-value-method->function (method)
  "Convert value METHOD into a function to insert a value."
  (pcase method
    ((pred null) #'ignore)
    ((pred functionp) method)
    (`(,fun-or-alias . ,rest-args)
     (lambda (value _)
       (apply (guix-info-value-function fun-or-alias)
              value rest-args)))
    (_ (error "Unknown value method '%S'" method))))

(defun guix-info-fill-column ()
  "Return fill column for the current window."
  (min (window-width) fill-column))

(defun guix-info-get-indent (&optional level)
  "Return `guix-info-indent' \"multiplied\" by LEVEL spaces.
LEVEL is 1 by default."
  (make-string (* guix-info-indent (or level 1)) ?\s))

(defun guix-info-insert-indent (&optional level)
  "Insert `guix-info-indent' spaces LEVEL times (1 by default)."
  (insert (guix-info-get-indent level)))

(defun guix-info-insert-entries (entries entry-type)
  "Display ENTRY-TYPE ENTRIES in the current info buffer."
  (guix-mapinsert (lambda (entry)
                    (guix-info-insert-entry entry entry-type))
                  entries
                  guix-info-delimiter))

(defun guix-info-insert-entry (entry entry-type &optional indent-level)
  "Insert ENTRY of ENTRY-TYPE into the current info buffer.
If INDENT-LEVEL is non-nil, indent displayed data by this number
of `guix-info-indent' spaces."
  (guix-with-indent (* (or indent-level 0)
                       guix-info-indent)
    (dolist (spec (guix-info-format entry-type))
      (guix-info-insert-entry-unit spec entry entry-type))))

(defun guix-info-insert-entry-unit (format-spec entry entry-type)
  "Insert title and value of a PARAM at point.
ENTRY is alist with parameters and their values.
ENTRY-TYPE is a type of ENTRY."
  (pcase format-spec
    ((pred functionp)
     (funcall format-spec entry)
     (insert "\n"))
    (`(,param ,title-method ,value-method)
     (let ((value (guix-entry-value entry param)))
       (unless (and guix-info-ignore-empty-values (null value))
         (let ((title        (guix-info-param-title entry-type param))
               (insert-title (guix-info-title-method->function title-method))
               (insert-value (guix-info-value-method->function value-method)))
           (funcall insert-title title)
           (funcall insert-value value entry)
           (insert "\n")))))
    (_ (error "Unknown format specification '%S'" format-spec))))

(defun guix-info-insert-title-simple (title &optional face)
  "Insert \"TITLE: \" string at point.
If FACE is nil, use `guix-info-param-title'."
  (guix-format-insert title
                      (or face 'guix-info-param-title)
                      "%s: "))

(defun guix-info-insert-title-format (title &optional face)
  "Insert TITLE using `guix-info-param-title-format' at point.
If FACE is nil, use `guix-info-param-title'."
  (guix-format-insert title
                      (or face 'guix-info-param-title)
                      guix-info-param-title-format))

(defun guix-info-insert-value-simple (value &optional button-or-face indent)
  "Format and insert parameter VALUE at point.

VALUE may be split into several short lines to fit the current
window, depending on `guix-info-fill', and each line is indented
with INDENT number of spaces.

If BUTTON-OR-FACE is a button type symbol, transform VALUE into
this (these) button(s) and insert each one on a new line.  If it
is a face symbol, propertize inserted line(s) with this face."
  (or indent (setq indent 0))
  (guix-with-indent indent
    (let* ((button?  (guix-button-type? button-or-face))
           (face     (unless button? button-or-face))
           (fill-col (unless (or button?
                                 (and (stringp value)
                                      (not guix-info-fill)))
                       (- (guix-info-fill-column) indent)))
           (value    (if (and value button?)
                         (guix-buttonize value button-or-face "\n")
                       value)))
      (guix-split-insert value face fill-col "\n"))))

(defun guix-info-insert-value-indent (value &optional button-or-face)
  "Format and insert parameter VALUE at point.

This function is intended to be called after inserting a title
with `guix-info-insert-title-simple'.

VALUE may be split into several short lines to fit the current
window, depending on `guix-info-fill', and each line is indented
with `guix-info-indent'.

For the meaning of BUTTON-OR-FACE, see `guix-info-insert-value-simple'."
  (when value (insert "\n"))
  (guix-info-insert-value-simple value button-or-face guix-info-indent))

(defun guix-info-insert-value-format (value &optional button-or-face
                                            &rest button-properties)
  "Format and insert parameter VALUE at point.

This function is intended to be called after inserting a title
with `guix-info-insert-title-format'.

VALUE may be split into several short lines to fit the current
window, depending on `guix-info-fill' and
`guix-info-multiline-prefix'.  If VALUE is a list, its elements
will be separated with `guix-list-separator'.

If BUTTON-OR-FACE is a button type symbol, transform VALUE into
this (these) button(s).  If it is a face symbol, propertize
inserted line(s) with this face.

BUTTON-PROPERTIES are passed to `guix-buttonize' (only if
BUTTON-OR-FACE is a button type)."
  (let* ((button?  (guix-button-type? button-or-face))
         (face     (unless button? button-or-face))
         (fill-col (when (or button?
                             guix-info-fill
                             (not (stringp value)))
                     (- (guix-info-fill-column)
                        (length guix-info-multiline-prefix))))
         (value    (if (and value button?)
                       (apply #'guix-buttonize
                              value button-or-face guix-list-separator
                              button-properties)
                     value)))
    (guix-split-insert value face fill-col
                       (concat "\n" guix-info-multiline-prefix))))

(defun guix-info-insert-time (seconds &optional face)
  "Insert formatted time string using SECONDS at point."
  (guix-format-insert (guix-get-time-string seconds)
                      (or face 'guix-info-time)))


;;; Buttons

(defvar guix-info-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "c") 'guix-info-button-copy-label)
    map)
  "Keymap for buttons in info buffers.")

(define-button-type 'guix
  'keymap guix-info-button-map
  'follow-link t)

(define-button-type 'guix-action
  :supertype 'guix
  'face 'guix-info-action-button
  'mouse-face 'guix-info-action-button-mouse)

(define-button-type 'guix-file
  :supertype 'guix
  'face 'guix-info-file-path
  'help-echo "Find file"
  'action (lambda (btn)
            (guix-find-file (button-label btn))))

(define-button-type 'guix-url
  :supertype 'guix
  'face 'guix-info-url
  'help-echo "Browse URL"
  'action (lambda (btn)
            (browse-url (button-label btn))))

(defun guix-info-button-copy-label (&optional pos)
  "Copy a label of the button at POS into kill ring.
If POS is nil, use the current point position."
  (interactive)
  (let ((button (button-at (or pos (point)))))
    (when button
      (guix-copy-as-kill (button-label button)))))

(defun guix-info-insert-action-button (label action &optional message
                                             &rest properties)
  "Make action button with LABEL and insert it at point.
ACTION is a function called when the button is pressed.  It
should accept button as the argument.
MESSAGE is a button message.
See `insert-text-button' for the meaning of PROPERTIES."
  (apply #'guix-insert-button
         label 'guix-action
         'action action
         'help-echo message
         properties))


;;; Major mode and interface definer

(defvar guix-info-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap (list guix-buffer-map button-buffer-map)
                               special-mode-map))
    map)
  "Keymap for `guix-info-mode' buffers.")

(define-derived-mode guix-info-mode special-mode "Guix-Info"
  "Parent mode for displaying data in 'info' form."
  (setq-local revert-buffer-function 'guix-buffer-revert))

(defun guix-info-mode-initialize ()
  "Set up the current 'info' buffer."
  ;; Without this, syntactic fontification is performed, and it may
  ;; break our highlighting.  For example, description of "emacs-typo"
  ;; package contains a single " (double-quote) character, so the
  ;; default syntactic fontification highlights the rest text after it
  ;; as a string.  See (info "(elisp) Font Lock Basics") for details.
  (setq font-lock-defaults '(nil t)))

(defmacro guix-info-define-interface (entry-type &rest args)
  "Define 'info' interface for displaying ENTRY-TYPE entries.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...

Required keywords:

  - `:format' - default value of the generated
    `guix-ENTRY-TYPE-info-format' variable.

The rest keyword arguments are passed to
`guix-buffer-define-interface' macro."
  (declare (indent 1))
  (let* ((entry-type-str     (symbol-name entry-type))
         (prefix             (concat "guix-" entry-type-str "-info"))
         (group              (intern prefix))
         (format-var         (intern (concat prefix "-format"))))
    (guix-keyword-args-let args
        ((show-entries-val   :show-entries-function)
         (format-val         :format))
      `(progn
         (defcustom ,format-var ,format-val
           ,(format "\
List of methods for inserting '%s' entry.
Each METHOD should be either a function or should have the
following form:

  (PARAM INSERT-TITLE INSERT-VALUE)

If METHOD is a function, it is called with an entry as argument.

PARAM is a name of '%s' entry parameter.

INSERT-TITLE may be either a symbol or a list.  If it is a
symbol, it should be a function or an alias from
`guix-info-title-aliases', in which case it is called with title
as argument.  If it is a list, it should have a
form (FUN-OR-ALIAS [ARGS ...]), in which case FUN-OR-ALIAS is
called with title and ARGS as arguments.

INSERT-VALUE may be either a symbol or a list.  If it is a
symbol, it should be a function or an alias from
`guix-info-value-aliases', in which case it is called with value
and entry as arguments.  If it is a list, it should have a
form (FUN-OR-ALIAS [ARGS ...]), in which case FUN-OR-ALIAS is
called with value and ARGS as arguments.

Parameters are inserted in the same order as defined by this list.
After calling each METHOD, a new line is inserted."
                    entry-type-str entry-type-str)
           :type 'sexp
           :group ',group)

         (guix-alist-put!
          '((format . ,format-var))
          'guix-info-data ',entry-type)

         ,(if show-entries-val
              `(guix-buffer-define-interface info ,entry-type
                 :show-entries-function ,show-entries-val
                 ,@%foreign-args)

            (let ((insert-fun (intern (concat prefix "-insert-entries"))))
              `(progn
                 (defun ,insert-fun (entries)
                   ,(format "\
Print '%s' ENTRIES in the current 'info' buffer."
                            entry-type-str)
                   (guix-info-insert-entries entries ',entry-type))

                 (guix-buffer-define-interface info ,entry-type
                   :insert-entries-function ',insert-fun
                   :mode-init-function 'guix-info-mode-initialize
                   ,@%foreign-args))))))))


(defvar guix-info-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group "guix-info-define-interface")
            symbol-end)
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode guix-info-font-lock-keywords)

(provide 'guix-info)

;;; guix-info.el ends here
