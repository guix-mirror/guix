;;; guix-ui-location.el --- Interface for displaying package locations

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

;; This file provides a 'list' interface for displaying locations of Guix
;; packages.

;;; Code:

(require 'guix-buffer)
(require 'guix-list)
(require 'guix-location)
(require 'guix-backend)

(guix-define-entry-type location)

(defun guix-location-get-entries ()
  "Receive 'package location' entries."
  (guix-eval-read "(package-location-entries)"))


;;; Location 'list'

(guix-list-define-interface location
  :buffer-name "*Guix Package Locations*"
  :get-entries-function 'guix-location-get-entries
  :format '((location guix-location-list-file-name-specification 50 t)
            (number-of-packages nil 10 guix-list-sort-numerically-1
                                :right-align t))
  :sort-key '(location))

(let ((map guix-location-list-mode-map))
  (define-key map (kbd "RET") 'guix-location-list-show-packages)
  ;; "Location Info" buffer is not defined (it would be useless), so
  ;; unbind "i" key (by default, it is used to display Info buffer).
  (define-key map (kbd "i") nil))

(defun guix-location-list-file-name-specification (location &optional _)
  "Return LOCATION button specification for `tabulated-list-entries'."
  (list location
        'face 'guix-list-file-name
        'action (lambda (btn)
                  (guix-find-location (button-get btn 'location)))
        'follow-link t
        'help-echo (concat "Find location: " location)
        'location location))

(declare-function guix-packages-by-location "guix-ui-package")

(defun guix-location-list-show-packages ()
  "Display packages placed in the location at point."
  (interactive)
  (guix-packages-by-location (guix-list-current-id)))


;;; Interactive commands

;;;###autoload
(defun guix-locations ()
  "Display locations of the Guix packages."
  (interactive)
  (guix-list-get-display-entries 'location))

(provide 'guix-ui-location)

;;; guix-ui-location.el ends here
