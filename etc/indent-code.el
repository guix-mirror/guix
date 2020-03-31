:;exec emacs --batch --quick --load="$0" --funcall=main "$@"
;;; indent-code.el --- Run Emacs to indent a package definition.

;; Copyright © 2017 Alex Kost <alezost@gmail.com>
;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>

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

;; This scripts indents the given file or package definition in the specified
;; file using Emacs.

;;; Code:

;; Load Scheme indentation rules from ".dir-locals.el".
(with-temp-buffer
  (scheme-mode)
  (let ((default-directory (file-name-as-directory load-file-name))
        (enable-local-variables :all))
    (hack-dir-local-variables)
    (hack-local-variables-apply)))

;; Add indentation info for Scheme constructs that are not Guix-specific.
;; This is normally provided by Geiser but this file is for people who may not
;; be running Geiser, so we just copy it here (from 'geiser-syntax.el').
(defmacro guix-syntax--scheme-indent (&rest pairs)
  `(progn ,@(mapcar (lambda (p)
                      `(put ',(car p) 'scheme-indent-function ',(cadr p)))
                    pairs)))

(guix-syntax--scheme-indent
 (and-let* 1)
 (case-lambda 0)
 (catch defun)
 (class defun)
 (dynamic-wind 0)
 (guard 1)
 (let*-values 1)
 (let-values 1)
 (let/ec 1)
 (letrec* 1)
 (match 1)
 (match-lambda 0)
 (match-lambda* 0)
 (match-let scheme-let-indent)
 (match-let* 1)
 (match-letrec 1)
 (opt-lambda 1)
 (parameterize 1)
 (parameterize* 1)
 (receive 2)
 (require-extension 0)
 (syntax-case 2)
 (test-approximate 1)
 (test-assert 1)
 (test-eq 1)
 (test-equal 1)
 (test-eqv 1)
 (test-group-with-cleanup 1)
 (test-runner-on-bad-count! 1)
 (test-runner-on-bad-end-name! 1)
 (test-runner-on-final! 1)
 (test-runner-on-group-begin! 1)
 (test-runner-on-group-end! 1)
 (test-runner-on-test-begin! 1)
 (test-runner-on-test-end! 1)
 (test-with-runner 1)
 (unless 1)
 (when 1)
 (while 1)
 (with-exception-handler 1)
 (with-syntax 1))


(defun main ()
  (pcase command-line-args-left
    (`(,file-name ,package-name)
     ;; Indent the definition of PACKAGE-NAME in FILE-NAME.
     (find-file file-name)
     (goto-char (point-min))
     (if (re-search-forward (concat "^(define\\(-public\\) +"
                                    package-name)
                            nil t)
         (let ((indent-tabs-mode nil))
           (beginning-of-defun)
           (indent-sexp)
           (save-buffer)
           (message "Done!"))
       (error "Package '%s' not found in '%s'"
              package-name file-name)))
    (`(,file-name)
     ;; Indent all of FILE-NAME.
     (find-file file-name)
     (let ((indent-tabs-mode nil))
       (indent-region (point-min) (point-max))
       (save-buffer)
       (message "Done!")))
    (x
     (error "Usage: indent-code.el FILE [PACKAGE]"))))

;;; indent-code.el ends here
