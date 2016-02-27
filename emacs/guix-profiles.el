;;; guix-profiles.el --- Guix profiles

;; Copyright © 2014, 2015, 2016 Alex Kost <alezost@gmail.com>
;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>

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

;;; Code:

(require 'guix-config)

(defvar guix-user-profile
  (expand-file-name "~/.guix-profile")
  "User profile.")

(defvar guix-system-profile
  (concat guix-config-state-directory "/profiles/system")
  "System profile.")

(defvar guix-default-profile
  (concat guix-config-state-directory
          "/profiles/per-user/"
          (getenv "USER")
          "/guix-profile")
  "Default Guix profile.")

(defvar guix-current-profile guix-default-profile
  "Current profile.")

(defvar guix-system-profile-regexp
  (concat "\\`" (regexp-quote guix-system-profile))
  "Regexp matching system profiles.")

(defun guix-system-profile? (profile)
  "Return non-nil, if PROFILE is a system one."
  (string-match-p guix-system-profile-regexp profile))

(defun guix-profile-prompt (&optional default)
  "Prompt for profile and return it.
Use DEFAULT as a start directory.  If it is nil, use
`guix-current-profile'."
  (let* ((path (read-file-name "Profile: "
                               (file-name-directory
                                (or default guix-current-profile))))
         (path (directory-file-name (expand-file-name path))))
    (if (string= path guix-user-profile)
        guix-default-profile
      path)))

(defun guix-set-current-profile (path)
  "Set `guix-current-profile' to PATH.
Interactively, prompt for PATH.  With prefix, use
`guix-default-profile'."
  (interactive
   (list (if current-prefix-arg
             guix-default-profile
           (guix-profile-prompt))))
  (setq guix-current-profile path)
  (message "Current profile has been set to '%s'."
           guix-current-profile))

(provide 'guix-profiles)

;;; guix-profiles.el ends here
