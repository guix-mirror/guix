;;; guix-hydra-jobset.el --- Interface for Hydra jobsets  -*- lexical-binding: t -*-

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

;; This file provides an interface for displaying Hydra jobsets in
;; 'list' and 'info' buffers.

;;; Code:

(require 'cl-lib)
(require 'guix-buffer)
(require 'guix-list)
(require 'guix-info)
(require 'guix-hydra)
(require 'guix-hydra-build)
(require 'guix-utils)

(guix-hydra-define-entry-type hydra-jobset
  :search-types '((project . guix-hydra-jobset-api-url))
  :filters '(guix-hydra-jobset-filter-id)
  :filter-names '((nrscheduled . scheduled)
                  (nrsucceeded . succeeded)
                  (nrfailed . failed)
                  (nrtotal . total)))

(defun guix-hydra-jobset-get-display (search-type &rest args)
  "Search for Hydra builds and show results."
  (apply #'guix-list-get-display-entries
         'hydra-jobset search-type args))


;;; Defining URLs

(defun guix-hydra-jobset-url (project jobset)
  "Return Hydra URL of a PROJECT's JOBSET."
  (guix-hydra-url "jobset/" project "/" jobset))

(defun guix-hydra-jobset-api-url (project)
  "Return Hydra API URL for jobsets by PROJECT."
  (guix-hydra-api-url "jobsets"
    `(("project" . ,project))))


;;; Filters for processing raw entries

(defun guix-hydra-jobset-filter-id (entry)
  "Add 'ID' parameter to 'hydra-jobset' ENTRY."
  (cons `(id . ,(guix-entry-value entry 'name))
        entry))


;;; Hydra jobset 'info'

(guix-hydra-info-define-interface hydra-jobset
  :mode-name "Hydra-Jobset-Info"
  :buffer-name "*Guix Hydra Jobset Info*"
  :format '((name ignore (simple guix-info-heading))
            ignore
            guix-hydra-jobset-info-insert-url
            (project   format guix-hydra-jobset-info-insert-project)
            (scheduled format (format guix-hydra-jobset-info-scheduled))
            (succeeded format (format guix-hydra-jobset-info-succeeded))
            (failed    format (format guix-hydra-jobset-info-failed))
            (total     format (format guix-hydra-jobset-info-total))))

(defface guix-hydra-jobset-info-scheduled
  '((t))
  "Face used for the number of scheduled builds."
  :group 'guix-hydra-jobset-info-faces)

(defface guix-hydra-jobset-info-succeeded
  '((t :inherit guix-hydra-build-status-succeeded))
  "Face used for the number of succeeded builds."
  :group 'guix-hydra-jobset-info-faces)

(defface guix-hydra-jobset-info-failed
  '((t :inherit guix-hydra-build-status-failed))
  "Face used for the number of failed builds."
  :group 'guix-hydra-jobset-info-faces)

(defface guix-hydra-jobset-info-total
  '((t))
  "Face used for the total number of builds."
  :group 'guix-hydra-jobset-info-faces)

(defun guix-hydra-jobset-info-insert-project (project entry)
  "Insert PROJECT button for the jobset ENTRY."
  (let ((jobset (guix-entry-value entry 'name)))
    (guix-insert-button
     project 'guix-hydra-build-project
     'action (lambda (btn)
               (let ((args (guix-hydra-build-latest-prompt-args
                            :project (button-get btn 'project)
                            :jobset  (button-get btn 'jobset))))
                 (apply #'guix-hydra-build-get-display
                        'latest args)))
     'project project
     'jobset jobset)))

(defun guix-hydra-jobset-info-insert-url (entry)
  "Insert Hydra URL for the jobset ENTRY."
  (guix-insert-button (guix-hydra-jobset-url
                       (guix-entry-value entry 'project)
                       (guix-entry-value entry 'name))
                      'guix-url))


;;; Hydra jobset 'list'

(guix-hydra-list-define-interface hydra-jobset
  :mode-name "Hydra-Jobset-List"
  :buffer-name "*Guix Hydra Jobset List*"
  :format '((name nil 25 t)
            (project nil 10 t)
            (scheduled nil 12 t)
            (succeeded nil 12 t)
            (failed nil 9 t)
            (total nil 10 t)))

(let ((map guix-hydra-jobset-list-mode-map))
  (define-key map (kbd "B") 'guix-hydra-jobset-list-latest-builds))

(defun guix-hydra-jobset-list-latest-builds (number &rest args)
  "Display latest NUMBER of Hydra builds of the current jobset.
Interactively, prompt for NUMBER.  With prefix argument, prompt
for all ARGS."
  (interactive
   (let ((entry (guix-list-current-entry)))
     (guix-hydra-build-latest-prompt-args
      :project (guix-entry-value entry 'project)
      :jobset  (guix-entry-value entry 'name))))
  (apply #'guix-hydra-latest-builds number args))


;;; Interactive commands

;;;###autoload
(defun guix-hydra-jobsets (project)
  "Display jobsets of PROJECT."
  (interactive (list (guix-hydra-read-project)))
  (guix-hydra-jobset-get-display 'project project))

(provide 'guix-hydra-jobset)

;;; guix-hydra-jobset.el ends here
