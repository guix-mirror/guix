;;; guix-info.el --- Info buffers for displaying entries   -*- lexical-binding: t -*-

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

;; This file provides a help-like buffer for displaying information
;; about Guix packages and generations.

;;; Code:

(require 'guix-base)
(require 'guix-entry)
(require 'guix-utils)
(require 'guix-ui)

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
  "Display ENTRIES of ENTRY-TYPE in the current info buffer.
ENTRIES should have a form of `guix-entries'."
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

(define-button-type 'guix-package-location
  :supertype 'guix
  'face 'guix-package-info-location
  'help-echo "Find location of this package"
  'action (lambda (btn)
            (guix-find-location (button-label btn))))

(define-button-type 'guix-package-name
  :supertype 'guix
  'face 'guix-package-info-name-button
  'help-echo "Describe this package"
  'action (lambda (btn)
            (guix-get-show-entries guix-profile 'info guix-package-info-type
                                   'name (button-label btn))))

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
     map (make-composed-keymap (list guix-root-map button-buffer-map)
                               special-mode-map))
    map)
  "Parent keymap for info buffers.")

(define-derived-mode guix-info-mode special-mode "Guix-Info"
  "Parent mode for displaying information in info buffers.")

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
        ((format-val         :format))
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

         (guix-buffer-define-interface info ,entry-type
           ,@%foreign-args)))))


;;; Displaying packages

(guix-ui-info-define-interface package
  :format '(guix-package-info-insert-heading
            ignore
            (synopsis ignore (simple guix-package-info-synopsis))
            ignore
            (description ignore (simple guix-package-info-description))
            ignore
            (outputs simple guix-package-info-insert-outputs)
            (source simple guix-package-info-insert-source)
            (location format (format guix-package-location))
            (home-url format (format guix-url))
            (license format (format guix-package-info-license))
            (inputs format (format guix-package-input))
            (native-inputs format (format guix-package-native-input))
            (propagated-inputs format
                               (format guix-package-propagated-input)))
  :titles '((home-url . "Home page"))
  :required '(id name version installed non-unique))

(guix-info-define-interface installed-output
  :format '((path simple (indent guix-file))
            (dependencies simple (indent guix-file)))
  :titles '((path . "Store directory"))
  :reduced? t)

(defface guix-package-info-heading
  '((t :inherit guix-info-heading))
  "Face for package name and version headings."
  :group 'guix-package-info-faces)

(defface guix-package-info-name
  '((t :inherit font-lock-keyword-face))
  "Face used for a name of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-name-button
  '((t :inherit button))
  "Face used for a full name that can be used to describe a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-version
  '((t :inherit font-lock-builtin-face))
  "Face used for a version of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-synopsis
  '((((type tty pc) (class color)) :weight bold)
    (t :height 1.1 :weight bold :inherit variable-pitch))
  "Face used for a synopsis of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-description
  '((t))
  "Face used for a description of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-license
  '((t :inherit font-lock-string-face))
  "Face used for a license of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-location
  '((t :inherit link))
  "Face used for a location of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-installed-outputs
  '((default :weight bold)
    (((class color) (min-colors 88) (background light))
     :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark))
     :foreground "PaleGreen")
    (((class color) (min-colors 8))
     :foreground "green")
    (t :underline t))
  "Face used for installed outputs of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-uninstalled-outputs
  '((t :weight bold))
  "Face used for uninstalled outputs of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-obsolete
  '((t :inherit error))
  "Face used if a package is obsolete."
  :group 'guix-package-info-faces)

(defun guix-package-info-insert-heading (entry)
  "Insert package ENTRY heading (name specification) at point."
  (guix-format-insert (concat (guix-entry-value entry 'name) " "
                              (guix-entry-value entry 'version))
                      'guix-package-info-heading))

(defmacro guix-package-info-define-insert-inputs (&optional type)
  "Define a face and a function for inserting package inputs.
TYPE is a type of inputs.
Function name is `guix-package-info-insert-TYPE-inputs'.
Face name is `guix-package-info-TYPE-inputs'."
  (let* ((type-str (symbol-name type))
         (type-name (and type (concat type-str "-")))
         (type-desc (and type (concat type-str " ")))
         (face (intern (concat "guix-package-info-" type-name "inputs")))
         (btn  (intern (concat "guix-package-" type-name "input"))))
    `(progn
       (defface ,face
         '((t :inherit guix-package-info-name-button))
         ,(concat "Face used for " type-desc "inputs of a package.")
         :group 'guix-package-info-faces)

       (define-button-type ',btn
         :supertype 'guix-package-name
         'face ',face))))

(guix-package-info-define-insert-inputs)
(guix-package-info-define-insert-inputs native)
(guix-package-info-define-insert-inputs propagated)


;;; Inserting outputs and installed parameters

(defvar guix-package-info-output-format "%-10s"
  "String used to format output names of the packages.
It should be a '%s'-sequence.  After inserting an output name
formatted with this string, an action button is inserted.")

(defvar guix-package-info-obsolete-string "(This package is obsolete)"
  "String used if a package is obsolete.")

(defun guix-package-info-insert-outputs (outputs entry)
  "Insert OUTPUTS from package ENTRY at point."
  (and (guix-entry-value entry 'obsolete)
       (guix-package-info-insert-obsolete-text))
  (and (guix-entry-value entry 'non-unique)
       (guix-entry-value entry 'installed)
       (guix-package-info-insert-non-unique-text
        (guix-package-entry->name-specification entry)))
  (insert "\n")
  (mapc (lambda (output)
          (guix-package-info-insert-output output entry))
        outputs))

(defun guix-package-info-insert-obsolete-text ()
  "Insert a message about obsolete package at point."
  (guix-info-insert-indent)
  (guix-format-insert guix-package-info-obsolete-string
                      'guix-package-info-obsolete))

(defun guix-package-info-insert-non-unique-text (full-name)
  "Insert a message about non-unique package with FULL-NAME at point."
  (insert "\n")
  (guix-info-insert-indent)
  (insert "Installed outputs are displayed for a non-unique ")
  (guix-insert-button full-name 'guix-package-name)
  (insert " package."))

(defun guix-package-info-insert-output (output entry)
  "Insert OUTPUT at point.
Make some fancy text with buttons and additional stuff if the
current OUTPUT is installed (if there is such output in
`installed' parameter of a package ENTRY)."
  (let* ((installed (guix-entry-value entry 'installed))
         (obsolete  (guix-entry-value entry 'obsolete))
         (installed-entry (cl-find-if
                           (lambda (entry)
                             (string= (guix-entry-value entry 'output)
                                      output))
                           installed))
         (action-type (if installed-entry 'delete 'install)))
    (guix-info-insert-indent)
    (guix-format-insert output
                        (if installed-entry
                            'guix-package-info-installed-outputs
                          'guix-package-info-uninstalled-outputs)
                        guix-package-info-output-format)
    (guix-package-info-insert-action-button action-type entry output)
    (when obsolete
      (guix-info-insert-indent)
      (guix-package-info-insert-action-button 'upgrade entry output))
    (insert "\n")
    (when installed-entry
      (guix-info-insert-entry installed-entry 'installed-output 2))))

(defun guix-package-info-insert-action-button (type entry output)
  "Insert button to process an action on a package OUTPUT at point.
TYPE is one of the following symbols: `install', `delete', `upgrade'.
ENTRY is an alist with package info."
  (let ((type-str (capitalize (symbol-name type)))
        (full-name (guix-package-entry->name-specification entry output)))
    (guix-info-insert-action-button
     type-str
     (lambda (btn)
       (guix-process-package-actions
        guix-profile
        `((,(button-get btn 'action-type) (,(button-get btn 'id)
                                           ,(button-get btn 'output))))
        (current-buffer)))
     (concat type-str " '" full-name "'")
     'action-type type
     'id (or (guix-entry-value entry 'package-id)
             (guix-entry-id entry))
     'output output)))


;;; Inserting a source

(defface guix-package-info-source
  '((t :inherit link :underline nil))
  "Face used for a source URL of a package."
  :group 'guix-package-info-faces)

(defcustom guix-package-info-auto-find-source nil
  "If non-nil, find a source file after pressing a \"Show\" button.
If nil, just display the source file path without finding."
  :type 'boolean
  :group 'guix-package-info)

(defcustom guix-package-info-auto-download-source t
  "If nil, do not automatically download a source file if it doesn't exist.
After pressing a \"Show\" button, a derivation of the package
source is calculated and a store file path is displayed.  If this
variable is non-nil and the source file does not exist in the
store, it will be automatically downloaded (with a possible
prompt depending on `guix-operation-confirm' variable)."
  :type 'boolean
  :group 'guix-package-info)

(defvar guix-package-info-download-buffer nil
  "Buffer from which a current download operation was performed.")

(define-button-type 'guix-package-source
  :supertype 'guix
  'face 'guix-package-info-source
  'help-echo ""
  'action (lambda (_)
            ;; As a source may not be a real URL (e.g., "mirror://..."),
            ;; no action is bound to a source button.
            (message "Yes, this is the source URL. What did you expect?")))

(defun guix-package-info-show-source (entry-id package-id)
  "Show file name of a package source in the current info buffer.
Find the file if needed (see `guix-package-info-auto-find-source').
ENTRY-ID is an ID of the current entry (package or output).
PACKAGE-ID is an ID of the package which source to show."
  (let* ((entry (guix-entry-by-id entry-id guix-entries))
         (file  (guix-package-source-path package-id)))
    (or file
        (error "Couldn't define file path of the package source"))
    (let* ((new-entry (cons (cons 'source-file file)
                            entry))
           (entries (guix-replace-entry entry-id new-entry guix-entries)))
      (guix-redisplay-buffer :entries entries)
      (if (file-exists-p file)
          (if guix-package-info-auto-find-source
              (guix-find-file file)
            (message "The source store path is displayed."))
        (if guix-package-info-auto-download-source
            (guix-package-info-download-source package-id)
          (message "The source does not exist in the store."))))))

(defun guix-package-info-download-source (package-id)
  "Download a source of the package PACKAGE-ID."
  (setq guix-package-info-download-buffer (current-buffer))
  (guix-package-source-build-derivation
   package-id
   "The source does not exist in the store. Download it?"))

(defun guix-package-info-insert-source (source entry)
  "Insert SOURCE from package ENTRY at point.
SOURCE is a list of URLs."
  (if (null source)
      (guix-format-insert nil)
    (let* ((source-file (guix-entry-value entry 'source-file))
           (entry-id    (guix-entry-id entry))
           (package-id  (or (guix-entry-value entry 'package-id)
                            entry-id)))
      (if (null source-file)
          (guix-info-insert-action-button
           "Show"
           (lambda (btn)
             (guix-package-info-show-source (button-get btn 'entry-id)
                                            (button-get btn 'package-id)))
           "Show the source store directory of the current package"
           'entry-id entry-id
           'package-id package-id)
        (unless (file-exists-p source-file)
          (guix-info-insert-action-button
           "Download"
           (lambda (btn)
             (guix-package-info-download-source
              (button-get btn 'package-id)))
           "Download the source into the store"
           'package-id package-id))
        (guix-info-insert-value-indent source-file 'guix-file))
      (guix-info-insert-value-indent source 'guix-package-source))))

(defun guix-package-info-redisplay-after-download ()
  "Redisplay an 'info' buffer after downloading the package source.
This function is used to hide a \"Download\" button if needed."
  (when (buffer-live-p guix-package-info-download-buffer)
    (guix-redisplay-buffer :buffer guix-package-info-download-buffer)
    (setq guix-package-info-download-buffer nil)))

(add-hook 'guix-after-source-download-hook
          'guix-package-info-redisplay-after-download)


;;; Displaying outputs

(guix-ui-info-define-interface output
  :buffer-name "*Guix Package Info*"
  :format '((name format (format guix-package-info-name))
            (version format guix-output-info-insert-version)
            (output format guix-output-info-insert-output)
            (synopsis simple (indent guix-package-info-synopsis))
            (source simple guix-package-info-insert-source)
            (path simple (indent guix-file))
            (dependencies simple (indent guix-file))
            (location format (format guix-package-location))
            (home-url format (format guix-url))
            (license format (format guix-package-info-license))
            (inputs format (format guix-package-input))
            (native-inputs format (format guix-package-native-input))
            (propagated-inputs format
                               (format guix-package-propagated-input))
            (description simple (indent guix-package-info-description)))
  :titles guix-package-info-titles
  :required '(id package-id installed non-unique))

(defun guix-output-info-insert-version (version entry)
  "Insert output VERSION and obsolete text if needed at point."
  (guix-info-insert-value-format version
                                 'guix-package-info-version)
  (and (guix-entry-value entry 'obsolete)
       (guix-package-info-insert-obsolete-text)))

(defun guix-output-info-insert-output (output entry)
  "Insert OUTPUT and action buttons at point."
  (let* ((installed (guix-entry-value entry 'installed))
         (obsolete  (guix-entry-value entry 'obsolete))
         (action-type (if installed 'delete 'install)))
    (guix-info-insert-value-format
     output
     (if installed
         'guix-package-info-installed-outputs
       'guix-package-info-uninstalled-outputs))
    (guix-info-insert-indent)
    (guix-package-info-insert-action-button action-type entry output)
    (when obsolete
      (guix-info-insert-indent)
      (guix-package-info-insert-action-button 'upgrade entry output))))


;;; Displaying generations

(guix-ui-info-define-interface generation
  :format '((number format guix-generation-info-insert-number)
            (prev-number format (format))
            (current format guix-generation-info-insert-current)
            (path simple (indent guix-file))
            (time format (time)))
  :titles '((path . "File name")
            (prev-number . "Previous number")))

(defface guix-generation-info-number
  '((t :inherit font-lock-keyword-face))
  "Face used for a number of a generation."
  :group 'guix-generation-info-faces)

(defface guix-generation-info-current
  '((t :inherit guix-package-info-installed-outputs))
  "Face used if a generation is the current one."
  :group 'guix-generation-info-faces)

(defface guix-generation-info-not-current
  '((t nil))
  "Face used if a generation is not the current one."
  :group 'guix-generation-info-faces)

(defun guix-generation-info-insert-number (number &optional _)
  "Insert generation NUMBER and action buttons."
  (guix-info-insert-value-format number 'guix-generation-info-number)
  (guix-info-insert-indent)
  (guix-info-insert-action-button
   "Packages"
   (lambda (btn)
     (guix-get-show-entries guix-profile 'list guix-package-list-type
                            'generation (button-get btn 'number)))
   "Show installed packages for this generation"
   'number number)
  (guix-info-insert-indent)
  (guix-info-insert-action-button
   "Delete"
   (lambda (btn)
     (guix-delete-generations guix-profile (list (button-get btn 'number))
                              (current-buffer)))
   "Delete this generation"
   'number number))

(defun guix-generation-info-insert-current (val entry)
  "Insert boolean value VAL showing whether this generation is current."
  (if val
      (guix-info-insert-value-format "Yes" 'guix-generation-info-current)
    (guix-info-insert-value-format "No" 'guix-generation-info-not-current)
    (guix-info-insert-indent)
    (guix-info-insert-action-button
     "Switch"
     (lambda (btn)
       (guix-switch-to-generation guix-profile (button-get btn 'number)
                                  (current-buffer)))
     "Switch to this generation (make it the current one)"
     'number (guix-entry-value entry 'number))))


(defvar guix-info-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group "guix-info-define-interface")
            symbol-end)
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode guix-info-font-lock-keywords)

(provide 'guix-info)

;;; guix-info.el ends here
