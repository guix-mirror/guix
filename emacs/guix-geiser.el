;;; guix-geiser.el --- Interacting with Geiser   -*- lexical-binding: t -*-

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

;; This file provides functions to evaluate guile code using Geiser.

;;; Code:

(require 'geiser-mode)

(defun guix-geiser-repl ()
  "Return the current Geiser REPL."
  (or geiser-repl--repl
      (geiser-repl--repl/impl 'guile)
      (error "Geiser REPL not found")))

(defun guix-geiser-eval (str &optional repl)
  "Evaluate STR with guile expression using Geiser REPL.
If REPL is nil, use the current Geiser REPL.
Return a list of strings with result values of evaluation."
  (with-current-buffer (or repl (guix-geiser-repl))
    (let ((res (geiser-eval--send/wait `(:eval (:scm ,str)))))
      (if (geiser-eval--retort-error res)
          (error "Error in evaluating guile expression: %s"
                 (geiser-eval--retort-output res))
        (cdr (assq 'result res))))))

(defun guix-geiser-eval-read (str &optional repl)
  "Evaluate STR with guile expression using Geiser REPL.
Return elisp expression of the first result value of evaluation."
  ;; Parsing scheme code with elisp `read' is probably not the best idea.
  (read (replace-regexp-in-string
         "#f\\|#<unspecified>" "nil"
         (replace-regexp-in-string
          "#t" "t" (car (guix-geiser-eval str repl))))))

(defun guix-repl-send (cmd &optional save-history)
  "Send CMD input string to the current REPL buffer.
This is the same as `geiser-repl--send', but with SAVE-HISTORY
argument.  If SAVE-HISTORY is non-nil, save CMD in the REPL
history."
  (when (and cmd (eq major-mode 'geiser-repl-mode))
    (geiser-repl--prepare-send)
    (goto-char (point-max))
    (comint-kill-input)
    (insert cmd)
    (let ((comint-input-filter (if save-history
                                   comint-input-filter
                                 'ignore)))
      (comint-send-input nil t))))

(defun guix-geiser-eval-in-repl (str &optional repl no-history no-display)
  "Switch to Geiser REPL and evaluate STR with guile expression there.
If NO-HISTORY is non-nil, do not save STR in the REPL history.
If NO-DISPLAY is non-nil, do not switch to the REPL buffer."
  (let ((repl (or repl (guix-geiser-repl))))
    (with-current-buffer repl
      ;; XXX Since Geiser 0.8, `geiser-repl--send' has SAVE-HISTORY
      ;; argument, so use this function eventually and remove
      ;; `guix-repl-send'.
      (guix-repl-send str (not no-history)))
    (unless no-display
      (geiser-repl--switch-to-buffer repl))))

(provide 'guix-geiser)

;;; guix-geiser.el ends here
