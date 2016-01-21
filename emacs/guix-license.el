;;; guix-license.el --- Licenses

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

;; This file provides the code to work with licenses of Guix packages.

;;; Code:

(require 'guix-buffer)
(require 'guix-list)
(require 'guix-info)
(require 'guix-read)
(require 'guix-backend)
(require 'guix-guile)

(guix-define-entry-type license)

(defun guix-lookup-license-url (license)
  "Return URL of a LICENSE."
  (or (guix-eval-read (guix-make-guile-expression
                       'lookup-license-uri license))
      (error "Hm, I don't know URL of '%s' license" license)))

(defun guix-license-get-entries (search-type &rest args)
  "Receive 'license' entries.
SEARCH-TYPE may be one of the following symbols: `all', `id', `name'."
  (guix-eval-read
   (apply #'guix-make-guile-expression
          'license-entries search-type args)))

(defun guix-license-get-display (search-type &rest args)
  "Search for licenses and show results."
  (apply #'guix-list-get-display-entries
         'license search-type args))


;;; License 'info'

(guix-info-define-interface license
  :buffer-name "*Guix License Info*"
  :get-entries-function 'guix-license-get-entries
  :format '((name ignore (simple guix-info-heading))
            ignore
            guix-license-insert-packages-button
            (url ignore (simple guix-url))
            guix-license-insert-comment)
  :titles '((url . "URL")))

(declare-function guix-packages-by-license "guix-ui-package")

(defun guix-license-insert-packages-button (entry)
  "Insert button to display packages by license ENTRY."
  (guix-info-insert-action-button
   "Packages"
   (lambda (btn)
     (guix-packages-by-license (button-get btn 'license)))
   "Show packages with this license"
   'license (guix-entry-value entry 'name)))

(defun guix-license-insert-comment (entry)
  "Insert 'comment' of a license ENTRY."
  (let ((comment (guix-entry-value entry 'comment)))
    (if (and comment
             (string-match-p "^http" comment))
        (guix-info-insert-value-simple comment 'guix-url)
      (guix-info-insert-title-simple
       (guix-info-param-title 'license 'comment))
      (guix-info-insert-value-indent comment))))


;;; License 'list'

(guix-list-define-interface license
  :buffer-name "*Guix Licenses*"
  :get-entries-function 'guix-license-get-entries
  :describe-function 'guix-license-list-describe
  :format '((name nil 40 t)
            (url guix-list-get-url 50 t))
  :titles '((name . "License"))
  :sort-key '(name))

(let ((map guix-license-list-mode-map))
  (define-key map (kbd "RET") 'guix-license-list-show-packages))

(defun guix-license-list-describe (ids)
  "Describe licenses with IDS (list of identifiers)."
  (guix-buffer-display-entries
   (guix-entries-by-ids ids (guix-buffer-current-entries))
   'info 'license (cl-list* 'id ids) 'add))

(defun guix-license-list-show-packages ()
  "Display packages with the license at point."
  (interactive)
  (guix-packages-by-license (guix-list-current-id)))


;;; Interactive commands

;;;###autoload
(defun guix-browse-license-url (license)
  "Browse URL of a LICENSE."
  (interactive (list (guix-read-license-name)))
  (browse-url (guix-lookup-license-url license)))

;;;###autoload
(defun guix-licenses ()
  "Display licenses of the Guix packages."
  (interactive)
  (guix-license-get-display 'all))

(provide 'guix-license)

;;; guix-license.el ends here
