;;; guix-emacs.el --- Emacs packages installed with Guix

;; Copyright © 2014, 2015, 2016 Alex Kost <alezost@gmail.com>

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

;; This file provides auxiliary code for working with Emacs packages
;; installed with Guix.

;;; Code:

(require 'cl-lib)
(unless (require 'guix-profiles nil t)
  (defvar guix-user-profile (expand-file-name "~/.guix-profile")))

(defcustom guix-package-enable-at-startup t
  "If non-nil, activate Emacs packages installed in a user profile.
Set this variable to nil before requiring `guix-emacs' file to
avoid loading autoloads of Emacs packages installed in
`guix-user-profile'."
  :type 'boolean
  :group 'guix)

(defvar guix-emacs-autoloads nil
  "List of the last loaded Emacs autoloads.")

(defvar guix-emacs-autoloads-regexp
  (rx (group (* any) "-autoloads")
      ".el" (zero-or-one "c") string-end)
  "Regexp to match Emacs 'autoloads' file.")

(defun guix-emacs-directory (&optional profile)
  "Return directory with Emacs packages installed in PROFILE.
If PROFILE is nil, use `guix-user-profile'."
  (expand-file-name "share/emacs/site-lisp"
                    (or profile guix-user-profile)))

(defun guix-emacs-find-autoloads (directory)
  "Return a list of Emacs 'autoloads' files in DIRECTORY.
The files in the list do not have extensions (.el, .elc)."
  (cl-remove-duplicates
   (delq nil
        (mapcar (lambda (file)
                  (when (string-match guix-emacs-autoloads-regexp file)
                    (match-string 1 file)))
                (directory-files directory 'full-name nil 'no-sort)))
   :test #'string=))

(defun guix-emacs-subdirs (directory)
  "Return list of DIRECTORY subdirectories."
  (cl-remove-if (lambda (file)
                  (or (string-match-p (rx "/." string-end) file)
                      (string-match-p (rx "/.." string-end) file)
                      (not (file-directory-p file))))
                (directory-files directory 'full-name nil 'no-sort)))

(defun guix-emacs-directories (&optional profile)
  "Return the list of directories under PROFILE that contain Emacs packages.
This includes both `share/emacs/site-lisp/guix.d/PACKAGE'
sub-directories and `share/emacs/site-lisp' itself.

If PROFILE is nil, use `guix-user-profile'.
Return nil, if Emacs packages are not installed in PROFILE."
  (let ((root-dir (guix-emacs-directory (or profile guix-user-profile))))
    (when (file-directory-p root-dir)
      (let* ((pkgs-dir  (expand-file-name "guix.d" root-dir))
             (pkgs-dirs (when (file-directory-p pkgs-dir)
                          (guix-emacs-subdirs pkgs-dir))))
        (cons root-dir pkgs-dirs)))))

;;;###autoload
(defun guix-emacs-autoload-packages (&rest profiles)
  "Autoload Emacs packages installed in PROFILES.
If PROFILES are not specified, use a default user and system
profiles.

'Autoload' means add directories with Emacs packages to
`load-path' and load 'autoloads' files matching
`guix-emacs-autoloads-regexp'."
  (interactive (list (if (fboundp 'guix-profile-prompt)
                         (funcall 'guix-profile-prompt)
                       guix-user-profile)))
  (let ((profiles (or profiles
                      (list "/run/current-system/profile"
                            guix-user-profile))))
    (dolist (profile profiles)
      (let ((dirs (guix-emacs-directories profile)))
        (when dirs
          (let* ((autoloads     (cl-mapcan #'guix-emacs-find-autoloads
                                           dirs))
                 (new-autoloads (cl-nset-difference autoloads
                                                    guix-emacs-autoloads
                                                    :test #'string=)))
            (dolist (dir dirs)
              (cl-pushnew (directory-file-name dir)
                          load-path
                          :test #'string=))
            (dolist (file new-autoloads)
              (load file 'noerror))
            (setq guix-emacs-autoloads
                  (append new-autoloads guix-emacs-autoloads))))))))

(when guix-package-enable-at-startup
  (guix-emacs-autoload-packages))

(provide 'guix-emacs)

;;; guix-emacs.el ends here
