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
(require 'guix-utils)

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


;;; Interface definers

(defmacro guix-ui-define-interface (buffer-type entry-type &rest args)
  "Define BUFFER-TYPE interface for displaying ENTRY-TYPE entries.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...
In the following description TYPE means ENTRY-TYPE-BUFFER-TYPE.

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
         (definer         (intern (format "guix-%s-define-interface"
                                          buffer-type-str))))
    (guix-keyword-args-let args
        ((required-val    :required ''(id)))
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

         (,definer ,entry-type
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
