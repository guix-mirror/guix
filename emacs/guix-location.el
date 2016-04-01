;;; guix-location.el --- Package locations

;; Copyright © 2016 Alex Kost <alezost@gmail.com>

;; This file is part of GNU Guix.

;; GNU Guix is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Location as published by
;; the Free Software Foundation, either version 3 of the Location, or
;; (at your option) any later version.

;; GNU Guix is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public Location for more details.

;; You should have received a copy of the GNU General Public Location
;; along with this program.  If not, see <http://www.gnu.org/locations/>.

;;; Commentary:

;; This file provides the code to work with locations of Guix packages.

;;; Code:

(require 'cl-lib)
(require 'guix-backend)
(require 'guix-read)
(require 'guix-guile)

(defun guix-package-location (id-or-name)
  "Return location of a package with ID-OR-NAME.
For the meaning of location, see `guix-find-location'."
  (guix-eval-read (guix-make-guile-expression
                   'package-location-string id-or-name)))

(defun guix-find-location (location &optional directory)
  "Go to LOCATION of a package.
LOCATION is a string of the form:

  \"PATH:LINE:COLUMN\"

If PATH is relative, it is considered to be relative to
DIRECTORY (`guix-directory' by default)."
  (cl-multiple-value-bind (path line col)
      (split-string location ":")
    (let ((file (expand-file-name path (or directory guix-directory)))
          (line (string-to-number line))
          (col  (string-to-number col)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (- line 1))
      (move-to-column col)
      (recenter 1))))

;;;###autoload
(defun guix-edit (id-or-name &optional directory)
  "Edit (go to location of) package with ID-OR-NAME.
See `guix-find-location' for the meaning of package location and
DIRECTORY.
Interactively, with prefix argument, prompt for DIRECTORY."
  (interactive
   (list (guix-read-package-name)
         (guix-read-directory)))
  (let ((loc (guix-package-location id-or-name)))
    (if loc
        (guix-find-location loc directory)
      (message "Couldn't find package location."))))

(provide 'guix-location)

;;; guix-location.el ends here
