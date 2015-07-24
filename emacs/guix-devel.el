;;; guix-devel.el --- Development tools   -*- lexical-binding: t -*-

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

;; This file provides commands useful for developing Guix (or even
;; arbitrary Guile code) with Geiser.

;;; Code:

(require 'guix-guile)
(require 'guix-geiser)
(require 'guix-utils)

(defgroup guix-devel nil
  "Settings for Guix development utils."
  :group 'guix)

(defcustom guix-devel-activate-mode t
  "If non-nil, then `guix-devel-mode' is automatically activated
in Scheme buffers."
  :type 'boolean
  :group 'guix-devel)

(defun guix-devel-use-modules (&rest modules)
  "Use guile MODULES."
  (apply #'guix-geiser-call "use-modules" modules))

(defun guix-devel-use-module (&optional module)
  "Use guile MODULE in the current Geiser REPL.
MODULE is a string with the module name - e.g., \"(ice-9 match)\".
Interactively, use the module defined by the current scheme file."
  (interactive (list (guix-guile-current-module)))
  (guix-devel-use-modules module)
  (message "Using %s module." module))

(defun guix-devel-copy-module-as-kill ()
  "Put the name of the current guile module into `kill-ring'."
  (interactive)
  (guix-copy-as-kill (guix-guile-current-module)))

(defvar guix-devel-keys-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "k") 'guix-devel-copy-module-as-kill)
    (define-key map (kbd "u") 'guix-devel-use-module)
    map)
  "Keymap with subkeys for `guix-devel-mode-map'.")

(defvar guix-devel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c .") guix-devel-keys-map)
    map)
  "Keymap for `guix-devel-mode'.")

;;;###autoload
(define-minor-mode guix-devel-mode
  "Minor mode for `scheme-mode' buffers.

With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

When Guix Devel mode is enabled, it provides the following key
bindings:

\\{guix-devel-mode-map}"
  :init-value nil
  :lighter " Guix"
  :keymap guix-devel-mode-map)

;;;###autoload
(defun guix-devel-activate-mode-maybe ()
  "Activate `guix-devel-mode' depending on
`guix-devel-activate-mode' variable."
  (when guix-devel-activate-mode
    (guix-devel-mode)))

(provide 'guix-devel)

;;; guix-devel.el ends here
