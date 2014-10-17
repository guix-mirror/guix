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
(require 'guix-utils)

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

(defun guix-get-show-packages (profile search-type &rest search-vals)
  "Search for packages and show results.

If PROFILE is nil, use `guix-current-profile'.

See `guix-get-entries' for the meaning of SEARCH-TYPE and
SEARCH-VALS.

Results are displayed in the list buffer, unless a single package
is found and `guix-list-single-package' is nil."
  (or profile (setq profile guix-current-profile))
  (let ((packages (guix-get-entries profile guix-package-list-type
                                    search-type search-vals
                                    (guix-get-params-for-receiving
                                     'list guix-package-list-type))))
    (if (or guix-list-single-package
            (cdr packages))
        (guix-set-buffer profile packages 'list guix-package-list-type
                         search-type search-vals)
      (let ((packages (guix-get-entries profile guix-package-info-type
                                        search-type search-vals
                                        (guix-get-params-for-receiving
                                         'info guix-package-info-type))))
        (guix-set-buffer profile packages 'info guix-package-info-type
                         search-type search-vals)))))

(defun guix-get-show-generations (profile search-type &rest search-vals)
  "Search for generations and show results.

If PROFILE is nil, use `guix-current-profile'.

See `guix-get-entries' for the meaning of SEARCH-TYPE and
SEARCH-VALS."
  (apply #'guix-get-show-entries
         (or profile guix-current-profile)
         'list 'generation search-type search-vals))

;;;###autoload
(defun guix-search-by-name (name &optional profile)
  "Search for Guix packages by NAME.
NAME is a string with name specification.  It may optionally contain
a version number.  Examples: \"guile\", \"guile-2.0.11\".

If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (read-string "Package name: " nil 'guix-search-history)
         (and current-prefix-arg
              (guix-profile-prompt))))
  (guix-get-show-packages profile 'name name))

;;;###autoload
(defun guix-search-by-regexp (regexp &optional params profile)
  "Search for Guix packages by REGEXP.
PARAMS are package parameters that should be searched.
If PARAMS are not specified, use `guix-search-params'.

If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (read-regexp "Regexp: " nil 'guix-search-history)
         nil
         (and current-prefix-arg
              (guix-profile-prompt))))
  (guix-get-show-packages profile 'regexp regexp
                          (or params guix-search-params)))

;;;###autoload
(defun guix-installed-packages (&optional profile)
  "Display information about installed Guix packages.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (and current-prefix-arg
              (guix-profile-prompt))))
  (guix-get-show-packages profile 'installed))

;;;###autoload
(defun guix-obsolete-packages (&optional profile)
  "Display information about obsolete Guix packages.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (and current-prefix-arg
              (guix-profile-prompt))))
  (guix-get-show-packages profile 'obsolete))

;;;###autoload
(defun guix-all-available-packages (&optional profile)
  "Display information about all available Guix packages.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (and current-prefix-arg
              (guix-profile-prompt))))
  (guix-get-show-packages profile 'all-available))

;;;###autoload
(defun guix-newest-available-packages (&optional profile)
  "Display information about the newest available Guix packages.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (and current-prefix-arg
              (guix-profile-prompt))))
  (guix-get-show-packages profile 'newest-available))

;;;###autoload
(defun guix-generations (&optional profile)
  "Display information about all generations.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (and current-prefix-arg
              (guix-profile-prompt))))
  (guix-get-show-generations profile 'all))

;;;###autoload
(defun guix-last-generations (number &optional profile)
  "Display information about last NUMBER generations.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (read-number "The number of last generations: ")
         (and current-prefix-arg
              (guix-profile-prompt))))
  (guix-get-show-generations profile 'last number))

;;;###autoload
(defun guix-generations-by-time (from to &optional profile)
  "Display information about generations created between FROM and TO.
FROM and TO should be time values.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (guix-read-date "Find generations (from): ")
         (guix-read-date "Find generations (to): ")
         (and current-prefix-arg
              (guix-profile-prompt))))
  (guix-get-show-generations profile 'time
                             (float-time from)
                             (float-time to)))

(provide 'guix)

;;; guix.el ends here
