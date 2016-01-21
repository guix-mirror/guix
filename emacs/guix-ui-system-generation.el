;;; guix-ui-system-generation.el --- Interface for displaying system generations  -*- lexical-binding: t -*-

;; Copyright © 2016 Alex Kost <alezost@gmail.com>

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

;; This file provides an interface for displaying system generations
;; in 'list' and 'info' buffers, and commands for working with them.

;;; Code:

(require 'cl-lib)
(require 'guix-list)
(require 'guix-ui)
(require 'guix-ui-generation)
(require 'guix-profiles)

(guix-ui-define-entry-type system-generation)

(defun guix-system-generation-get-display (search-type &rest search-values)
  "Search for system generations and show results.
See `guix-ui-get-entries' for the meaning of SEARCH-TYPE and
SEARCH-VALUES."
  (apply #'guix-list-get-display-entries
         'system-generation
         guix-system-profile
         search-type search-values))


;;; System generation 'info'

(guix-ui-info-define-interface system-generation
  :buffer-name "*Guix Generation Info*"
  :format '((number format guix-generation-info-insert-number)
            (label format (format))
            (prev-number format (format))
            (current format guix-generation-info-insert-current)
            (path format (format guix-file))
            (time format (time))
            (root-device format (format))
            (kernel format (format guix-file)))
  :titles guix-generation-info-titles)


;;; System generation 'list'

;; FIXME It is better to make `guix-generation-list-shared-map' with
;; common keys for both usual and system generations.
(defvar guix-system-generation-list-mode-map
  (copy-keymap guix-generation-list-mode-map)
  "Keymap for `guix-system-generation-list-mode' buffers.")

(guix-ui-list-define-interface system-generation
  :buffer-name "*Guix Generation List*"
  :format '((number nil 5 guix-list-sort-numerically-0 :right-align t)
            (current guix-generation-list-get-current 10 t)
            (label nil 40 t)
            (time guix-list-get-time 20 t)
            (path guix-list-get-file-name 30 t))
  :titles guix-generation-list-titles
  :sort-key '(number . t)
  :marks '((delete . ?D)))


;;; Interactive commands

;;;###autoload
(defun guix-system-generations ()
  "Display information about system generations."
  (interactive)
  (guix-system-generation-get-display 'all))

;;;###autoload
(defun guix-last-system-generations (number)
  "Display information about last NUMBER of system generations."
  (interactive "nThe number of last generations: ")
  (guix-system-generation-get-display 'last number))

;;;###autoload
(defun guix-system-generations-by-time (from to)
  "Display information about system generations created between FROM and TO."
  (interactive
   (list (guix-read-date "Find generations (from): ")
         (guix-read-date "Find generations (to): ")))
  (guix-system-generation-get-display
   'time (float-time from) (float-time to)))

(provide 'guix-ui-system-generation)

;;; guix-ui-system-generation.el ends here
