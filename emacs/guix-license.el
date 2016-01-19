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

(require 'guix-read)
(require 'guix-backend)
(require 'guix-guile)

(defun guix-lookup-license-url (license)
  "Return URL of a LICENSE."
  (or (guix-eval-read (guix-make-guile-expression
                       'lookup-license-uri license))
      (error "Hm, I don't know URL of '%s' license" license)))

;;;###autoload
(defun guix-browse-license-url (license)
  "Browse URL of a LICENSE."
  (interactive (list (guix-read-license-name)))
  (browse-url (guix-lookup-license-url license)))

(provide 'guix-license)

;;; guix-license.el ends here
