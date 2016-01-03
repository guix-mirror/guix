;;; guix-entry.el --- 'Entry' type  -*- lexical-binding: t -*-

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

;; This file provides an API for 'entry' type which is just an alist of
;; KEY/VALUE pairs (KEY should be a symbol) with the required 'id' KEY.

;;; Code:

(require 'cl-lib)
(require 'guix-utils)

(defalias 'guix-entry-value #'guix-assq-value)

(defun guix-entry-id (entry)
  "Return ENTRY ID."
  (guix-entry-value entry 'id))

(defun guix-entry-by-id (id entries)
  "Return an entry from ENTRIES by its ID."
  (cl-find-if (lambda (entry)
                (equal (guix-entry-id entry) id))
              entries))

(defun guix-entries-by-ids (ids entries)
  "Return entries with IDS (a list of identifiers) from ENTRIES."
  (cl-remove-if-not (lambda (entry)
                      (member (guix-entry-id entry) ids))
                    entries))

(defun guix-replace-entry (id new-entry entries)
  "Replace an entry with ID from ENTRIES by NEW-ENTRY.
Return a list of entries with the replaced entry."
  (cl-substitute-if new-entry
                    (lambda (entry)
                      (equal id (guix-entry-id entry)))
                    entries
                    :count 1))

(provide 'guix-entry)

;;; guix-entry.el ends here
