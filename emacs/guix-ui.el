;;; guix-ui.el --- Common code for Guix package management interface  -*- lexical-binding: t -*-

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

;; This file provides some general code for 'list'/'info' interfaces for
;; packages and generations.

;;; Code:

(require 'cl-lib)
(require 'guix-backend)
(require 'guix-utils)

(defgroup guix-ui nil
  "Settings for Guix package management.
This group includes settings for displaying packages, outputs and
generations in 'list' and 'info' buffers."
  :group 'guix)

(defvar guix-ui-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M") 'guix-apply-manifest)
    (define-key map (kbd "C-c C-z") 'guix-switch-to-repl)
    map)
  "Parent keymap for Guix package/generation buffers.")

(defun guix-ui-list-describe (ids)
  "Describe 'ui' entries with IDS (list of identifiers)."
  (apply #'guix-get-show-entries
         guix-profile 'info guix-entry-type 'id ids))


;;; Buffers and auto updating

(defcustom guix-ui-update-after-operation 'current
  "Define what kind of data to update after executing an operation.

After successful executing an operation in the Guix REPL (for
example after installing a package), the data in Guix buffers
will or will not be automatically updated depending on a value of
this variable.

If nil, update nothing (do not revert any buffer).
If `current', update the buffer from which an operation was performed.
If `all', update all Guix buffers (not recommended)."
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Update operation buffer" current)
                 (const :tag "Update all Guix buffers" all))
  :group 'guix-ui)

(defcustom guix-ui-buffer-name-function
  #'guix-ui-buffer-name-default
  "Function used to define a name of a Guix buffer.
The function is called with 2 arguments: BASE-NAME and PROFILE."
  :type '(choice (function-item guix-ui-buffer-name-default)
                 (function-item guix-ui-buffer-name-simple)
                 (function :tag "Other function"))
  :group 'guix-ui)

(defun guix-ui-buffer-name-simple (base-name &rest _)
  "Return BASE-NAME."
  base-name)

;; TODO separate '*...*' logic from the real profile appending.  Also add
;; another function to return '*Guix ...: /full/path/to/profile*' name.
(defun guix-ui-buffer-name-default (base-name profile)
  "Return buffer name by appending BASE-NAME and PROFILE's base file name."
  (let ((profile-name (file-name-base (directory-file-name profile)))
        (re (rx string-start
                (group (? "*"))
                (group (*? any))
                (group (? "*"))
                string-end)))
    (or (string-match re base-name)
        (error "Unexpected error in defining guix buffer name"))
    (let ((first*    (match-string 1 base-name))
          (name-body (match-string 2 base-name))
          (last*     (match-string 3 base-name)))
      ;; Handle the case when buffer name is wrapped by '*'.
      (if (and (string= "*" first*)
               (string= "*" last*))
          (concat "*" name-body ": " profile-name "*")
        (concat base-name ": " profile-name)))))

(defun guix-ui-buffer-name (base-name profile)
  "Return Guix buffer name based on BASE-NAME and profile.
See `guix-ui-buffer-name-function' for details."
  (funcall guix-ui-buffer-name-function
           base-name profile))

(defun guix-ui-buffer? (&optional buffer modes)
  "Return non-nil if BUFFER mode is derived from any of the MODES.
If BUFFER is nil, check current buffer.
If MODES is nil, use `guix-list-mode' and `guix-info-mode'."
  (with-current-buffer (or buffer (current-buffer))
    (apply #'derived-mode-p
           (or modes '(guix-list-mode guix-info-mode)))))

(defun guix-ui-buffers (&optional modes)
  "Return a list of all buffers with major modes derived from MODES.
If MODES is nil, return list of all Guix 'list' and 'info' buffers."
  (cl-remove-if-not (lambda (buf)
                      (guix-ui-buffer? buf modes))
                    (buffer-list)))

(defun guix-ui-update-buffer (buffer)
  "Update data in a 'list' or 'info' BUFFER."
  (with-current-buffer buffer
    (guix-buffer-revert nil t)))

(defun guix-ui-update-buffers-after-operation ()
  "Update buffers after Guix operation if needed.
See `guix-ui-update-after-operation' for details."
  (let ((to-update
         (and guix-operation-buffer
              (cl-case guix-ui-update-after-operation
                (current (and (buffer-live-p guix-operation-buffer)
                              (guix-ui-buffer? guix-operation-buffer)
                              (list guix-operation-buffer)))
                (all     (guix-ui-buffers))))))
    (setq guix-operation-buffer nil)
    (mapc #'guix-ui-update-buffer to-update)))

(add-hook 'guix-after-repl-operation-hook
          'guix-ui-update-buffers-after-operation)


;;; Interface definers

(defmacro guix-ui-define-interface (buffer-type entry-type &rest args)
  "Define BUFFER-TYPE interface for displaying ENTRY-TYPE entries.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...
In the following description TYPE means ENTRY-TYPE-BUFFER-TYPE.

Required keywords:

  - `:buffer-name' - base part of a buffer name.  It is used in a
    generated `guix-TYPE-buffer-name' function; see
    `guix-ui-buffer-name' for details.

Optional keywords:

  - `:required' - default value of the generated
    `guix-TYPE-required-params' variable.

The rest keyword arguments are passed to
`guix-BUFFER-TYPE-define-interface' macro."
  (declare (indent 2))
  (let* ((entry-type-str  (symbol-name entry-type))
         (buffer-type-str (symbol-name buffer-type))
         (prefix          (concat "guix-" entry-type-str "-"
                                  buffer-type-str))
         (mode-str        (concat prefix "-mode"))
         (mode-map        (intern (concat mode-str "-map")))
         (parent-map      (intern (format "guix-%s-mode-map"
                                          buffer-type-str)))
         (required-var    (intern (concat prefix "-required-params")))
         (buffer-name-fun (intern (concat prefix "-buffer-name")))
         (definer         (intern (format "guix-%s-define-interface"
                                          buffer-type-str))))
    (guix-keyword-args-let args
        ((buffer-name-val :buffer-name)
         (required-val    :required ''(id)))
      `(progn
         (defvar ,mode-map
           (let ((map (make-sparse-keymap)))
             (set-keymap-parent
              map (make-composed-keymap ,parent-map guix-ui-map))
             map)
           ,(format "Keymap for `%s' buffers." mode-str))

         (defvar ,required-var ,required-val
           ,(format "\
List of the required '%s' parameters for '%s' buffer.
These parameters are received along with the displayed parameters."
                    entry-type-str buffer-type-str))

         (defun ,buffer-name-fun (profile &rest _)
           ,(format "\
Return a name of '%s' buffer for displaying '%s' entries.
See `guix-ui-buffer-name' for details."
                    buffer-type-str entry-type-str)
           (guix-ui-buffer-name ,buffer-name-val profile))

         (,definer ,entry-type
           :buffer-name ',buffer-name-fun
           ,@%foreign-args)))))

(defmacro guix-ui-info-define-interface (entry-type &rest args)
  "Define 'info' interface for displaying ENTRY-TYPE entries.
See `guix-ui-define-interface'."
  (declare (indent 1))
  `(guix-ui-define-interface info ,entry-type
     ,@args))

(defmacro guix-ui-list-define-interface (entry-type &rest args)
  "Define 'list' interface for displaying ENTRY-TYPE entries.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...

Optional keywords:

  - `:describe-function' - default value of the generated
    `guix-ENTRY-TYPE-list-describe-function' variable (if not
    specified, use `guix-ui-list-describe').

The rest keyword arguments are passed to
`guix-ui-define-interface' macro."
  (declare (indent 1))
  (guix-keyword-args-let args
      ((describe-val :describe-function))
    `(guix-ui-define-interface list ,entry-type
       :describe-function ,(or describe-val ''guix-ui-list-describe)
       ,@args)))


(defvar guix-ui-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group (or "guix-ui-define-interface"
                           "guix-ui-info-define-interface"
                           "guix-ui-list-define-interface"))
            symbol-end)
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode guix-ui-font-lock-keywords)

(provide 'guix-ui)

;;; guix-ui.el ends here
