;;; guix.el --- Interface for GNU Guix package manager

;; Copyright © 2014 Alex Kost <alezost@gmail.com>

;; Package-Requires: ((geiser "0.3"))
;; Keywords: tools

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

;; This package provides an interface for searching, listing and getting
;; information about Guix packages and generations; and for
;; installing/upgrading/removing packages.

;;; Code:

(require 'guix-base)
(require 'guix-list)
(require 'guix-info)

(defgroup guix nil
  "Interface for Guix package manager."
  :prefix "guix-"
  :group 'external)

(defcustom guix-list-single-package nil
  "If non-nil, list a package even if it is the only matching result.
If nil, show a single package in the info buffer."
  :type 'boolean
  :group 'guix)

(defvar guix-search-params '(name synopsis description)
  "Default list of package parameters for searching by regexp.")

(defvar guix-search-history nil
  "A history of minibuffer prompts.")

(defun guix-get-show-packages (search-type &rest search-vals)
  "Search for packages and show results.

See `guix-get-entries' for the meaning of SEARCH-TYPE and
SEARCH-VALS.

Results are displayed in the list buffer, unless a single package
is found and `guix-list-single-package' is nil."
  (let ((packages (guix-get-entries guix-package-list-type
                                    search-type search-vals
                                    (guix-get-params-for-receiving
                                     'list guix-package-list-type))))
    (if (or guix-list-single-package
            (cdr packages))
        (guix-set-buffer packages 'list guix-package-list-type
                         search-type search-vals)
      (let ((packages (guix-get-entries guix-package-info-type
                                        search-type search-vals
                                        (guix-get-params-for-receiving
                                         'info guix-package-info-type))))
        (guix-set-buffer packages 'info guix-package-info-type
                         search-type search-vals)))))

(defun guix-get-show-generations (search-type &rest search-vals)
  "Search for generations and show results."
  (apply #'guix-get-show-entries
         'list 'generation search-type search-vals))

;;;###autoload
(defun guix-search-by-name (name)
  "Search for Guix packages by NAME.
NAME is a string with name specification.  It may optionally contain
a version number.  Examples: \"guile\", \"guile-2.0.11\"."
  (interactive
   (list (read-string "Package name: " nil 'guix-search-history)))
  (guix-get-show-packages 'name name))

;;;###autoload
(defun guix-search-by-regexp (regexp &rest params)
  "Search for Guix packages by REGEXP.
PARAMS are package parameters that should be searched.
If PARAMS are not specified, use `guix-search-params'."
  (interactive
   (list (read-string "Regexp: " nil 'guix-search-history)))
  (or params (setq params guix-search-params))
  (guix-get-show-packages 'regexp regexp params))

;;;###autoload
(defun guix-installed-packages ()
  "Display information about installed Guix packages."
  (interactive)
  (guix-get-show-packages 'installed))

;;;###autoload
(defun guix-obsolete-packages ()
  "Display information about obsolete Guix packages."
  (interactive)
  (guix-get-show-packages 'obsolete))

;;;###autoload
(defun guix-all-available-packages ()
  "Display information about all available Guix packages."
  (interactive)
  (guix-get-show-packages 'all-available))

;;;###autoload
(defun guix-newest-available-packages ()
  "Display information about the newest available Guix packages."
  (interactive)
  (guix-get-show-packages 'newest-available))

;;;###autoload
(defun guix-generations (&optional number)
  "Display information about last NUMBER generations.
If NUMBER is nil, display all generations.

Generations can be displayed in a list or info buffers depending
on `guix-show-generations-function'.

Interactively, NUMBER is defined by a numeric prefix."
  (interactive "P")
  (if (numberp number)
      (guix-get-show-generations 'last number)
    (guix-get-show-generations 'all)))

(provide 'guix)

;;; guix.el ends here
