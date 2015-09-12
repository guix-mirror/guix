;;; guix-build-log.el --- Major and minor modes for build logs   -*- lexical-binding: t -*-

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

;; This file provides a major mode (`guix-build-log-mode') and a minor mode
;; (`guix-build-log-minor-mode') for highlighting Guix build logs.

;;; Code:

(defgroup guix-build-log nil
  "Settings for `guix-build-log-mode'."
  :group 'guix)

(defgroup guix-build-log-faces nil
  "Faces for `guix-build-log-mode'."
  :group 'guix-build-log
  :group 'guix-faces)

(defface guix-build-log-title-head
  '((t :inherit font-lock-keyword-face))
  "Face for '@' symbol of a log title."
  :group 'guix-build-log-faces)

(defface guix-build-log-title-start
  '((t :inherit guix-build-log-title-head))
  "Face for a log title denoting a start of a process."
  :group 'guix-build-log-faces)

(defface guix-build-log-title-success
  '((t :inherit guix-build-log-title-head))
  "Face for a log title denoting a successful end of a process."
  :group 'guix-build-log-faces)

(defface guix-build-log-title-fail
  '((t :inherit error))
  "Face for a log title denoting a failed end of a process."
  :group 'guix-build-log-faces)

(defface guix-build-log-title-end
  '((t :inherit guix-build-log-title-head))
  "Face for a log title denoting an undefined end of a process."
  :group 'guix-build-log-faces)

(defface guix-build-log-phase-name
  '((t :inherit font-lock-function-name-face))
  "Face for a phase name."
  :group 'guix-build-log-faces)

(defface guix-build-log-phase-start
  '((default :weight bold)
    (((class grayscale) (background light)) :foreground "Gray90")
    (((class grayscale) (background dark))  :foreground "DimGray")
    (((class color) (min-colors 16) (background light))
     :foreground "DarkGreen")
    (((class color) (min-colors 16) (background dark))
     :foreground "LimeGreen")
    (((class color) (min-colors 8)) :foreground "green"))
  "Face for the start line of a phase."
  :group 'guix-build-log-faces)

(defface guix-build-log-phase-end
  '((((class grayscale) (background light)) :foreground "Gray90")
    (((class grayscale) (background dark))  :foreground "DimGray")
    (((class color) (min-colors 16) (background light))
     :foreground "ForestGreen")
    (((class color) (min-colors 16) (background dark))
     :foreground "LightGreen")
    (((class color) (min-colors 8)) :foreground "green")
    (t :weight bold))
  "Face for the end line of a phase."
  :group 'guix-build-log-faces)

(defface guix-build-log-phase-success
  '((t))
  "Face for the 'succeeded' word of a phase line."
  :group 'guix-build-log-faces)

(defface guix-build-log-phase-fail
  '((t :inherit error))
  "Face for the 'failed' word of a phase line."
  :group 'guix-build-log-faces)

(defface guix-build-log-phase-seconds
  '((t :inherit font-lock-constant-face))
  "Face for the number of seconds for a phase."
  :group 'guix-build-log-faces)

(defcustom guix-build-log-mode-hook
  ;; Not using `compilation-minor-mode' because it rebinds some standard
  ;; keys, including M-n/M-p.
  '(compilation-shell-minor-mode view-mode)
  "Hook run after `guix-build-log-mode' is entered."
  :type 'hook
  :group 'guix-build-log)

(defvar guix-build-log-phase-name-regexp "`\\([^']+\\)'"
  "Regexp for a phase name.")

(defvar guix-build-log-phase-start-regexp
  (concat "^starting phase " guix-build-log-phase-name-regexp)
  "Regexp for the start line of a 'build' phase.")

(defun guix-build-log-title-regexp (&optional state)
  "Return regexp for the log title.
STATE is a symbol denoting a state of the title.  It should be
`start', `fail', `success' or `nil' (for a regexp matching any
state)."
  (let* ((word-rx (rx (1+ (any word "-"))))
         (state-rx (cond ((eq state 'start)   (concat word-rx "started"))
                         ((eq state 'success) (concat word-rx "succeeded"))
                         ((eq state 'fail)    (concat word-rx "failed"))
                         (t word-rx))))
    (rx-to-string
     `(and bol (group "@") " " (group (regexp ,state-rx)))
     t)))

(defun guix-build-log-phase-end-regexp (&optional state)
  "Return regexp for the end line of a 'build' phase.
STATE is a symbol denoting how a build phase was ended.  It should be
`fail', `success' or `nil' (for a regexp matching any state)."
  (let ((state-rx (cond ((eq state 'success) "succeeded")
                        ((eq state 'fail)    "failed")
                        (t (regexp-opt '("succeeded" "failed"))))))
    (rx-to-string
     `(and bol "phase " (regexp ,guix-build-log-phase-name-regexp)
           " " (group (regexp ,state-rx)) " after "
           (group (1+ digit)) " seconds")
     t)))

(defvar guix-build-log-font-lock-keywords
  `((,(guix-build-log-title-regexp 'start)
     (1 'guix-build-log-title-head)
     (2 'guix-build-log-title-start))
    (,(guix-build-log-title-regexp 'success)
     (1 'guix-build-log-title-head)
     (2 'guix-build-log-title-success))
    (,(guix-build-log-title-regexp 'fail)
     (1 'guix-build-log-title-head)
     (2 'guix-build-log-title-fail))
    (,(guix-build-log-title-regexp)
     (1 'guix-build-log-title-head)
     (2 'guix-build-log-title-end))
    (,guix-build-log-phase-start-regexp
     (0 'guix-build-log-phase-start)
     (1 'guix-build-log-phase-name prepend))
    (,(guix-build-log-phase-end-regexp 'success)
     (0 'guix-build-log-phase-end)
     (1 'guix-build-log-phase-name prepend)
     (2 'guix-build-log-phase-success prepend)
     (3 'guix-build-log-phase-seconds prepend))
    (,(guix-build-log-phase-end-regexp 'fail)
     (0 'guix-build-log-phase-end)
     (1 'guix-build-log-phase-name prepend)
     (2 'guix-build-log-phase-fail prepend)
     (3 'guix-build-log-phase-seconds prepend)))
  "A list of `font-lock-keywords' for `guix-build-log-mode'.")

(defvar guix-build-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "M-n") 'guix-build-log-next-phase)
    (define-key map (kbd "M-p") 'guix-build-log-previous-phase)
    map)
  "Keymap for `guix-build-log-mode' buffers.")

(defun guix-build-log-next-phase (&optional arg)
  "Move to the next build phase.
With ARG, do it that many times.  Negative ARG means move
backward."
  (interactive "^p")
  (if arg
      (when (zerop arg) (user-error "Try again"))
    (setq arg 1))
  (let ((search-fun (if (> arg 0)
                        #'re-search-forward
                      #'re-search-backward))
        (n (abs arg))
        found last-found)
    (save-excursion
      (end-of-line (if (> arg 0) 1 0))  ; skip the current line
      (while (and (not (zerop n))
                  (setq found
                        (funcall search-fun
                                 guix-build-log-phase-start-regexp
                                 nil t)))
        (setq n (1- n)
              last-found found)))
    (when last-found
      (goto-char last-found)
      (forward-line 0))
    (or found
        (user-error (if (> arg 0)
                        "No next build phase"
                      "No previous build phase")))))

(defun guix-build-log-previous-phase (&optional arg)
  "Move to the previous build phase.
With ARG, do it that many times.  Negative ARG means move
forward."
  (interactive "^p")
  (guix-build-log-next-phase (- (or arg 1))))

;;;###autoload
(define-derived-mode guix-build-log-mode special-mode
  "Guix-Build-Log"
  "Major mode for viewing Guix build logs.

\\{guix-build-log-mode-map}"
  (setq font-lock-defaults '(guix-build-log-font-lock-keywords t)))

;;;###autoload
(define-minor-mode guix-build-log-minor-mode
  "Toggle Guix Build Log minor mode.

With a prefix argument ARG, enable Guix Build Log minor mode if
ARG is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

When Guix Build Log minor mode is enabled, it highlights build
log in the current buffer.  This mode can be enabled
programmatically using hooks:

  (add-hook 'shell-mode-hook 'guix-build-log-minor-mode)"
  :init-value nil
  :lighter " Guix-Build-Log"
  :group 'guix-build-log
  (if guix-build-log-minor-mode
      (font-lock-add-keywords nil guix-build-log-font-lock-keywords)
    (font-lock-remove-keywords nil guix-build-log-font-lock-keywords))
  (when font-lock-mode
    (font-lock-fontify-buffer)))

(provide 'guix-build-log)

;;; guix-build-log.el ends here
