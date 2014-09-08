;;; guix-history.el --- History of buffer information

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

;; This file provides support for history of buffers similar to the
;; history of a `help-mode' buffer.

;;; Code:

(require 'cl-macs)

(defvar-local guix-history-stack-item nil
  "Current item of the history.
A list of the form (FUNCTION [ARGS ...]).
The item is used by calling (apply FUNCTION ARGS).")
(put 'guix-history-stack-item 'permanent-local t)

(defvar-local guix-history-back-stack nil
  "Stack (list) of visited items.
Each element of the list has a form of `guix-history-stack-item'.")
(put 'guix-history-back-stack 'permanent-local t)

(defvar-local guix-history-forward-stack nil
  "Stack (list) of items visited with `guix-history-back'.
Each element of the list has a form of `guix-history-stack-item'.")
(put 'guix-history-forward-stack 'permanent-local t)

(defvar guix-history-size 0
  "Maximum number of items saved in history.
If 0, the history is disabled.")

(defun guix-history-add (item)
  "Add ITEM to history."
  (and guix-history-stack-item
       (push guix-history-stack-item guix-history-back-stack))
  (setq guix-history-forward-stack nil
        guix-history-stack-item item)
  (when (>= (length guix-history-back-stack)
            guix-history-size)
    (setq guix-history-back-stack
          (cl-loop for elt in guix-history-back-stack
                   for i from 1 to guix-history-size
                   collect elt))))

(defun guix-history-replace (item)
  "Replace current item in history with ITEM."
  (setq guix-history-stack-item item))

(defun guix-history-goto (item)
  "Go to the ITEM of history.
ITEM should have the form of `guix-history-stack-item'."
  (or (listp item)
      (error "Wrong value of history element"))
  (setq guix-history-stack-item item)
  (apply (car item) (cdr item)))

(defun guix-history-back ()
  "Go back to the previous element of history in the current buffer."
  (interactive)
  (or guix-history-back-stack
      (user-error "No previous element in history"))
  (push guix-history-stack-item guix-history-forward-stack)
  (guix-history-goto (pop guix-history-back-stack)))

(defun guix-history-forward ()
  "Go forward to the next element of history in the current buffer."
  (interactive)
  (or guix-history-forward-stack
      (user-error "No next element in history"))
  (push guix-history-stack-item guix-history-back-stack)
  (guix-history-goto (pop guix-history-forward-stack)))

(provide 'guix-history)

;;; guix-history.el ends here
