;;; guix-guile.el --- Auxiliary tools for working with guile code   -*- lexical-binding: t -*-

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

;; This file provides functions for parsing guile code, making guile
;; expressions, etc.

;;; Code:

(defun guix-guile-make-call-expression (proc &rest args)
  "Return \"(PROC ARGS ...)\" string.
PROC and ARGS should be strings."
  (format "(%s %s)"
          proc
          (mapconcat #'identity args " ")))

(defun guix-make-guile-expression (fun &rest args)
  "Return string containing a guile expression for calling FUN with ARGS."
  (format "(%S %s)" fun
          (mapconcat
           (lambda (arg)
             (cond
              ((null arg) "'()")
              ((or (eq arg t)
                   ;; An ugly hack to separate 'false' from nil.
                   (equal arg 'f)
                   (keywordp arg))
               (concat "#" (prin1-to-string arg t)))
              ((or (symbolp arg) (listp arg))
               (concat "'" (prin1-to-string arg)))
              (t (prin1-to-string arg))))
           args
           " ")))

(provide 'guix-guile)

;;; guix-guile.el ends here
