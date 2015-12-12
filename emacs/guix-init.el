(require 'guix-autoloads)
(require 'guix-emacs)

(defcustom guix-package-enable-at-startup t
  "If non-nil, activate Emacs packages installed in a user profile.
Set this variable to nil before requiring `guix-init' file to
avoid loading autoloads of Emacs packages installed in
`guix-user-profile'."
  :type 'boolean
  :group 'guix)

(add-to-list 'load-path (guix-emacs-directory))

(when guix-package-enable-at-startup
  (guix-emacs-load-autoloads))

(add-hook 'scheme-mode-hook 'guix-devel-activate-mode-maybe)
(add-hook 'shell-mode-hook 'guix-build-log-minor-mode-activate-maybe)

(provide 'guix-init)
