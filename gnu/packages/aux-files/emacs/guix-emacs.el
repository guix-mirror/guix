;;; guix-emacs.el --- Emacs packages installed with Guix

;; Copyright © 2014, 2015, 2016, 2017 Alex Kost <alezost@gmail.com>
;; Copyright © 2017 Kyle Meyer <kyle@kyleam.com>
;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>

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

;; This file provides auxiliary code to autoload Emacs packages
;; installed with Guix.

;;; Code:
(require 'seq)
(declare-function package-load-descriptor "package" (pkg-dir))

(defvar guix-emacs-autoloads-regexp
  (rx (* any) "-autoloads.el" (zero-or-one "c") string-end)
  "Regexp to match Emacs 'autoloads' file.")

(defun guix-emacs-find-autoloads (directory)
  "Return a list of Emacs 'autoloads' files in DIRECTORY.
The files in the list do not have extensions (.el, .elc)."
  ;; `directory-files' doesn't honor group in regexp.
  (delete-dups (mapcar #'file-name-sans-extension
                       (directory-files directory 'full-name
                                        guix-emacs-autoloads-regexp))))

(defun guix-emacs--non-core-load-path ()
  ;; Filter out core Elisp directories, which are already handled by Emacs.
  (seq-filter (lambda (dir)
                (string-match-p "/share/emacs/site-lisp" dir))
              load-path))

;;;###autoload
(defun guix-emacs-autoload-packages ()
  "Autoload Emacs packages found in EMACSLOADPATH.

'Autoload' means to load the 'autoloads' files matching
`guix-emacs-autoloads-regexp'."
  (interactive)
  (let ((autoloads (mapcan #'guix-emacs-find-autoloads
                           (guix-emacs--non-core-load-path))))
    (mapc (lambda (f)
            (load f 'noerror))
          autoloads)))

;;;###autoload
(defun guix-emacs-load-package-descriptors ()
  "Load descriptors for packages found in EMACSLOADPATH via subdirs.el."
  (dolist (dir (guix-emacs--non-core-load-path))
    (let ((subdirs-file (expand-file-name "subdirs.el" dir)))
     (when (file-exists-p subdirs-file)
      (with-temp-buffer
        (insert-file-contents subdirs-file)
        (goto-char (point-min))
        (let ((subdirs (read (current-buffer))))
          (and (equal (car-safe subdirs) 'normal-top-level-add-to-load-path)
               (equal (car-safe (cadr subdirs)) 'list)
               (dolist (subdir (cdadr subdirs))
                 (let ((pkg-dir (expand-file-name subdir dir)))
                   (when (file-directory-p pkg-dir)
                     (package-load-descriptor pkg-dir)))))))))))

(provide 'guix-emacs)

;;; guix-emacs.el ends here
