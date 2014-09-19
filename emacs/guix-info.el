;;; guix-info.el --- Info buffers for displaying entries

;; Copyright © 2014 Alex Kost <alezost@gmail.com>

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

(require 'guix-history)
(require 'guix-base)
(require 'guix-utils)

(defgroup guix-info nil
  "General settings for info buffers."
  :prefix "guix-info-"
  :group 'guix)

(defface guix-info-param-title
  '((t :inherit font-lock-type-face))
  "Face used for titles of parameters."
  :group 'guix-info)

(defface guix-info-file-path
  '((t :inherit link))
  "Face used for file paths."
  :group 'guix-info)

(defface guix-info-url
  '((t :inherit link))
  "Face used for URLs."
  :group 'guix-info)

(defface guix-info-time
  '((t :inherit font-lock-constant-face))
  "Face used for timestamps."
  :group 'guix-info)

(defface guix-info-action-button
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black")
    (t :inherit button))
  "Face used for action buttons."
  :group 'guix-info)

(defface guix-info-action-button-mouse
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "grey90" :foreground "black")
    (t :inherit highlight))
  "Mouse face used for action buttons."
  :group 'guix-info)

(defcustom guix-info-ignore-empty-vals nil
  "If non-nil, do not display parameters with nil values."
  :type 'boolean
  :group 'guix-info)

(defvar guix-info-param-title-format "%-18s: "
  "String used to format a title of a parameter.
It should be a '%s'-sequence.  After inserting a title formatted
with this string, a value of the parameter is inserted.
This string is used by `guix-info-insert-title-default'.")

(defvar guix-info-multiline-prefix (make-string 20 ?\s)
  "String used to format multi-line parameter values.
If a value occupies more than one line, this string is inserted
in the beginning of each line after the first one.
This string is used by `guix-info-insert-val-default'.")

(defvar guix-info-indent 2
  "Number of spaces used to indent various parts of inserted text.")

(defvar guix-info-fill-column 60
  "Column used for filling (word wrapping) parameters with long lines.
If a value is not multi-line and it occupies more than this
number of characters, it will be split into several lines.")

(defvar guix-info-delimiter "\n\f\n"
  "String used to separate entries.")

(defvar guix-info-insert-methods
  '((package
     (name              guix-package-info-name)
     (version           guix-package-info-version)
     (license           guix-package-info-license)
     (synopsis          guix-package-info-synopsis)
     (description       guix-package-info-insert-description
                        guix-info-insert-title-simple)
     (outputs           guix-package-info-insert-outputs
                        guix-info-insert-title-simple)
     (home-url          guix-info-insert-url)
     (inputs            guix-package-info-insert-inputs)
     (native-inputs     guix-package-info-insert-native-inputs)
     (propagated-inputs guix-package-info-insert-propagated-inputs)
     (location          guix-package-info-insert-location))
    (installed
     (path              guix-package-info-insert-output-path
                        guix-info-insert-title-simple)
     (dependencies      guix-package-info-insert-output-dependencies
                        guix-info-insert-title-simple))
    (output
     (name              guix-package-info-name)
     (version           guix-output-info-insert-version)
     (output            guix-output-info-insert-output)
     (path              guix-package-info-insert-output-path
                        guix-info-insert-title-simple)
     (dependencies      guix-package-info-insert-output-dependencies
                        guix-info-insert-title-simple)
     (license           guix-package-info-license)
     (synopsis          guix-package-info-synopsis)
     (description       guix-package-info-insert-description
                        guix-info-insert-title-simple)
     (home-url          guix-info-insert-url)
     (inputs            guix-package-info-insert-inputs)
     (native-inputs     guix-package-info-insert-native-inputs)
     (propagated-inputs guix-package-info-insert-propagated-inputs)
     (location          guix-package-info-insert-location))
    (generation
     (number            guix-generation-info-insert-number)
     (path              guix-info-insert-file-path)
     (time              guix-info-insert-time)))
  "Methods for inserting parameter values.
Each element of the list should have a form:

  (ENTRY-TYPE . ((PARAM INSERT-VALUE [INSERT-TITLE]) ...))

INSERT-VALUE may be either nil, a face name or a function.  If it
is nil or a face, `guix-info-insert-val-default' function is
called with parameter value and INSERT-VALUE as arguments.  If it
is a function, this function is called with parameter value and
entry info (alist of parameters and their values) as arguments.

INSERT-TITLE may be either nil, a face name or a function.  If it
is nil or a face, `guix-info-insert-title-default' function is
called with parameter title and INSERT-TITLE as arguments.  If it
is a function, this function is called with parameter title as
argument.")

(defvar guix-info-displayed-params
  '((package name version synopsis outputs location home-url
             license inputs native-inputs propagated-inputs description)
    (output name version output synopsis path dependencies location home-url
            license inputs native-inputs propagated-inputs description)
    (installed path dependencies)
    (generation number prev-number time path))
  "List of displayed entry parameters.
Each element of the list should have a form:

  (ENTRY-TYPE . (PARAM ...))

The order of displayed parameters is the same as in this list.")

(defun guix-info-get-insert-methods (entry-type param)
  "Return list of insert methods for parameter PARAM of ENTRY-TYPE.
See `guix-info-insert-methods' for details."
  (guix-get-key-val guix-info-insert-methods
                    entry-type param))

(defun guix-info-get-displayed-params (entry-type)
  "Return parameters of ENTRY-TYPE that should be displayed."
  (guix-get-key-val guix-info-displayed-params
                    entry-type))

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
If INDENT-LEVEL is non-nil, indent displayed information by this
number of `guix-info-indent' spaces."
  (let ((region-beg (point)))
    (mapc (lambda (param)
            (guix-info-insert-param param entry entry-type))
          (guix-info-get-displayed-params entry-type))
    (when indent-level
      (indent-rigidly region-beg (point)
                      (* indent-level guix-info-indent)))))

(defun guix-info-insert-param (param entry entry-type)
  "Insert title and value of a PARAM at point.
ENTRY is alist with parameters and their values.
ENTRY-TYPE is a type of ENTRY."
  (let ((val (guix-get-key-val entry param)))
    (unless (and guix-info-ignore-empty-vals (null val))
      (let* ((title          (guix-get-param-title entry-type param))
             (insert-methods (guix-info-get-insert-methods entry-type param))
             (val-method     (car insert-methods))
             (title-method   (cadr insert-methods)))
        (guix-info-method-funcall title title-method
                                  #'guix-info-insert-title-default)
        (guix-info-method-funcall val val-method
                                  #'guix-info-insert-val-default
                                  entry)
        (insert "\n")))))

(defun guix-info-method-funcall (val method default-fun &rest args)
  "Call METHOD or DEFAULT-FUN.

If METHOD is a function and VAL is non-nil, call this
function by applying it to VAL and ARGS.

If METHOD is a face, propertize inserted VAL with this face."
  (cond ((or (null method)
             (facep method))
         (funcall default-fun val method))
        ((functionp method)
         (apply method val args))
        (t (error "Unknown method '%S'" method))))

(defun guix-info-insert-title-default (title &optional face format)
  "Insert TITLE formatted with `guix-info-param-title-format' at point."
  (guix-format-insert title
                      (or face 'guix-info-param-title)
                      (or format guix-info-param-title-format)))

(defun guix-info-insert-title-simple (title &optional face)
  "Insert TITLE at point."
  (guix-info-insert-title-default title face "%s:"))

(defun guix-info-insert-val-default (val &optional face)
  "Format and insert parameter value VAL at point.

This function is intended to be called after
`guix-info-insert-title-default'.

If VAL is a one-line string longer than `guix-info-fill-column',
split it into several short lines.  See also
`guix-info-multiline-prefix'.

If FACE is non-nil, propertize inserted line(s) with this FACE."
  (guix-split-insert val face
                     guix-info-fill-column
                     (concat "\n" guix-info-multiline-prefix)))

(defun guix-info-insert-val-simple (val &optional face-or-fun)
  "Format and insert parameter value VAL at point.

This function is intended to be called after
`guix-info-insert-title-simple'.

If VAL is a one-line string longer than `guix-info-fill-column',
split it into several short lines and indent each line with
`guix-info-indent' spaces.

If FACE-OR-FUN is a face, propertize inserted line(s) with this FACE.

If FACE-OR-FUN is a function, call it with VAL as argument.  If
VAL is a list, call the function on each element of this list."
  (if (null val)
      (progn (guix-info-insert-indent)
             (guix-format-insert nil))
    (let ((prefix (concat "\n" (guix-info-get-indent))))
      (insert prefix)
      (if (functionp face-or-fun)
          (guix-mapinsert face-or-fun
                          (if (listp val) val (list val))
                          prefix)
        (guix-split-insert val face-or-fun
                           guix-info-fill-column prefix)))))

(defun guix-info-insert-action-button (label action &optional message
                                             &rest properties)
  "Make action button with LABEL and insert it at point.
For the meaning of ACTION, MESSAGE and PROPERTIES, see
`guix-insert-button'."
  (apply #'guix-insert-button
         label 'guix-info-action-button action message
         'mouse-face 'guix-info-action-button-mouse
         properties))

(defun guix-info-insert-file-path (path &optional _)
  "Make button from file PATH and insert it at point."
  (guix-insert-button
   path 'guix-info-file-path
   (lambda (btn) (find-file (button-label btn)))
   "Find file"))

(defun guix-info-insert-url (url &optional _)
  "Make button from URL and insert it at point."
  (guix-insert-button
   url 'guix-info-url
   (lambda (btn) (browse-url (button-label btn)))
   "Browse URL"))

(defun guix-info-insert-time (seconds &optional _)
  "Insert formatted time string using SECONDS at point."
  (guix-info-insert-val-default (guix-get-time-string seconds)
                                'guix-info-time))


(defvar guix-info-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap button-buffer-map
                               special-mode-map))
    map)
  "Parent keymap for info buffers.")

(define-derived-mode guix-info-mode special-mode "Guix-Info"
  "Parent mode for displaying information in info buffers.")


;;; Displaying packages

(guix-define-buffer-type info package
  :required (id installed non-unique))

(defface guix-package-info-name
  '((t :inherit font-lock-keyword-face))
  "Face used for a name of a package."
  :group 'guix-package-info)

(defface guix-package-info-version
  '((t :inherit font-lock-builtin-face))
  "Face used for a version of a package."
  :group 'guix-package-info)

(defface guix-package-info-synopsis
  '((t :inherit font-lock-doc-face))
  "Face used for a synopsis of a package."
  :group 'guix-package-info)

(defface guix-package-info-description
  '((t))
  "Face used for a description of a package."
  :group 'guix-package-info)

(defface guix-package-info-license
  '((t :inherit font-lock-string-face))
  "Face used for a license of a package."
  :group 'guix-package-info)

(defface guix-package-info-location
  '((t :inherit link))
  "Face used for a location of a package."
  :group 'guix-package-info)

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
  :group 'guix-package-info)

(defface guix-package-info-uninstalled-outputs
  '((t :weight bold))
  "Face used for uninstalled outputs of a package."
  :group 'guix-package-info)

(defface guix-package-info-obsolete
  '((t :inherit error))
  "Face used if a package is obsolete."
  :group 'guix-package-info)

(defun guix-package-info-insert-description (desc &optional _)
  "Insert description DESC at point."
  (guix-info-insert-val-simple desc 'guix-package-info-description))

(defun guix-package-info-insert-location (location &optional _)
  "Make button from file LOCATION and insert it at point."
  (guix-insert-button
   location 'guix-package-info-location
   (lambda (btn) (guix-find-location (button-label btn)))
   "Find location of this package"))

(defmacro guix-package-info-define-insert-inputs (&optional type)
  "Define a face and a function for inserting package inputs.
TYPE is a type of inputs.
Function name is `guix-package-info-insert-TYPE-inputs'.
Face name is `guix-package-info-TYPE-inputs'."
  (let* ((type-str (symbol-name type))
         (type-name (and type (concat type-str "-")))
         (type-desc (and type (concat type-str " ")))
         (face (intern (concat "guix-package-info-" type-name "inputs")))
         (fun  (intern (concat "guix-package-info-insert-" type-name "inputs"))))
    `(progn
       (defface ,face
         '((t :inherit button))
         ,(concat "Face used for " type-desc "inputs of a package.")
         :group 'guix-package-info)

       (defun ,fun (inputs &optional _)
         ,(concat "Make buttons from " type-desc "INPUTS and insert them at point.")
         (guix-package-info-insert-full-names inputs ',face)))))

(guix-package-info-define-insert-inputs)
(guix-package-info-define-insert-inputs native)
(guix-package-info-define-insert-inputs propagated)

(defun guix-package-info-insert-full-names (names face)
  "Make buttons from package NAMES and insert them at point.
NAMES is a list of strings.
Propertize buttons with FACE."
  (if names
      (guix-info-insert-val-default
       (with-temp-buffer
         (guix-mapinsert (lambda (name)
                           (guix-package-info-insert-full-name
                            name face))
                         names
                         guix-list-separator)
         (buffer-substring (point-min) (point-max))))
    (guix-format-insert nil)))

(defun guix-package-info-insert-full-name (name face)
  "Make button and insert package NAME at point.
Propertize package button with FACE."
  (guix-insert-button
   name face
   (lambda (btn)
     (guix-get-show-entries 'info 'package 'name
                            (button-label btn)))
   "Describe this package"))


;;; Inserting outputs and installed parameters

(defvar guix-package-info-output-format "%-10s"
  "String used to format output names of the packages.
It should be a '%s'-sequence.  After inserting an output name
formatted with this string, an action button is inserted.")

(defvar guix-package-info-obsolete-string "(This package is obsolete)"
  "String used if a package is obsolete.")

(defun guix-package-info-insert-outputs (outputs entry)
  "Insert OUTPUTS from package ENTRY at point."
  (and (guix-get-key-val entry 'obsolete)
       (guix-package-info-insert-obsolete-text))
  (and (guix-get-key-val entry 'non-unique)
       (guix-get-key-val entry 'installed)
       (guix-package-info-insert-non-unique-text
        (guix-get-full-name entry)))
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
  (guix-package-info-insert-full-name full-name
                                      'guix-package-info-inputs)
  (insert " package."))

(defun guix-package-info-insert-output (output entry)
  "Insert OUTPUT at point.
Make some fancy text with buttons and additional stuff if the
current OUTPUT is installed (if there is such output in
`installed' parameter of a package ENTRY)."
  (let* ((installed (guix-get-key-val entry 'installed))
         (obsolete  (guix-get-key-val entry 'obsolete))
         (installed-entry (cl-find-if
                           (lambda (entry)
                             (string= (guix-get-key-val entry 'output)
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
      (guix-info-insert-entry installed-entry 'installed 2))))

(defun guix-package-info-insert-action-button (type entry output)
  "Insert button to process an action on a package OUTPUT at point.
TYPE is one of the following symbols: `install', `delete', `upgrade'.
ENTRY is an alist with package info."
  (let ((type-str (capitalize (symbol-name type)))
        (full-name (guix-get-full-name entry output)))
    (guix-info-insert-action-button
     type-str
     (lambda (btn)
       (guix-process-package-actions
        (list (button-get btn 'action-type)
              (list (button-get btn 'id)
                    (button-get btn 'output)))))
     (concat type-str " '" full-name "'")
     'action-type type
     'id (or (guix-get-key-val entry 'package-id)
             (guix-get-key-val entry 'id))
     'output output)))

(defun guix-package-info-insert-output-path (path &optional _)
  "Insert PATH of the installed output."
  (guix-info-insert-val-simple path #'guix-info-insert-file-path))

(defalias 'guix-package-info-insert-output-dependencies
  'guix-package-info-insert-output-path)


;;; Displaying outputs

(guix-define-buffer-type info output
  :buffer-name "*Guix Package Info*"
  :required (id package-id installed non-unique))

(defun guix-output-info-insert-version (version entry)
  "Insert output VERSION and obsolete text if needed at point."
  (guix-info-insert-val-default version
                                'guix-package-info-version)
  (and (guix-get-key-val entry 'obsolete)
       (guix-package-info-insert-obsolete-text)))

(defun guix-output-info-insert-output (output entry)
  "Insert OUTPUT and action buttons at point."
  (let* ((installed (guix-get-key-val entry 'installed))
         (obsolete  (guix-get-key-val entry 'obsolete))
         (action-type (if installed 'delete 'install)))
    (guix-info-insert-val-default
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

(guix-define-buffer-type info generation)

(defface guix-generation-info-number
  '((t :inherit font-lock-keyword-face))
  "Face used for a number of a generation."
  :group 'guix-generation-info)

(defun guix-generation-info-insert-number (number &optional _)
  "Insert generation NUMBER and action buttons."
  (guix-info-insert-val-default number 'guix-generation-info-number)
  (guix-info-insert-indent)
  (guix-info-insert-action-button
   "Packages"
   (lambda (btn)
     (guix-get-show-entries 'list 'package 'generation
                            (button-get btn 'number)))
   "Show installed packages for this generation"
   'number number)
  (guix-info-insert-indent)
  (guix-info-insert-action-button
   "Delete"
   (lambda (btn) (error "Sorry, not implemented yet"))
   "Delete this generation"))

(provide 'guix-info)

;;; guix-info.el ends here
