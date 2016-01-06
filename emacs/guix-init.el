(require 'guix-autoloads)
(require 'guix-emacs)

(add-hook 'scheme-mode-hook 'guix-devel-activate-mode-maybe)
(add-hook 'shell-mode-hook 'guix-build-log-minor-mode-activate-maybe)

(provide 'guix-init)
