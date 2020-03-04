;;; copyright.el --- Insert a Guix copyright.

;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>

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

;; This package provides skeleton to insert a copyright with `guix-copyright'.

;;; Code:

(define-skeleton guix-copyright
  "Insert a copyright by $USER notice at cursor."
  "FULL_NAME <MAIL_ADDRESS>: "
  comment-start
  ";; Copyright © " `(format-time-string "%Y") " "
  (or (format "%s <%s>" user-full-name user-mail-address) str)
  comment-end)

;;; copyright.el ends here
