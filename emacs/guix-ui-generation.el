;;; guix-ui-generation.el --- Interface for displaying generations  -*- lexical-binding: t -*-

;; Copyright © 2014, 2015 Alex Kost <alezost@gmail.com>

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

;; This file provides an interface for displaying profile generations in
;; 'list' and 'info' buffers, and commands for working with them.

;;; Code:

(require 'cl-lib)
(require 'guix-buffer)
(require 'guix-list)
(require 'guix-info)
(require 'guix-ui)
(require 'guix-ui-package)
(require 'guix-base)
(require 'guix-backend)
(require 'guix-guile)
(require 'guix-entry)
(require 'guix-utils)

(defgroup guix-generation nil
  "Interface for displaying generations."
  :group 'guix-ui)

(defun guix-generation-get-display (profile search-type &rest search-values)
  "Search for generations and show results.

If PROFILE is nil, use `guix-current-profile'.

See `guix-ui-get-entries' for the meaning of SEARCH-TYPE and
SEARCH-VALUES."
  (let ((args (cl-list* (or profile guix-current-profile)
                        search-type search-values)))
    (guix-buffer-get-display-entries
     'list 'generation args 'add)))

(defun guix-delete-generations (profile generations
                                &optional operation-buffer)
  "Delete GENERATIONS from PROFILE.
Each element from GENERATIONS is a generation number."
  (when (or (not guix-operation-confirm)
            (y-or-n-p
             (let ((count (length generations)))
               (if (> count 1)
                   (format "Delete %d generations from profile '%s'? "
                           count profile)
                 (format "Delete generation %d from profile '%s'? "
                         (car generations) profile)))))
    (guix-eval-in-repl
     (guix-make-guile-expression
      'delete-generations* profile generations)
     operation-buffer)))

(defun guix-switch-to-generation (profile generation
                                  &optional operation-buffer)
  "Switch PROFILE to GENERATION."
  (when (or (not guix-operation-confirm)
            (y-or-n-p (format "Switch profile '%s' to generation %d? "
                              profile generation)))
    (guix-eval-in-repl
     (guix-make-guile-expression
      'switch-to-generation* profile generation)
     operation-buffer)))


;;; Generation 'info'

(guix-ui-info-define-interface generation
  :buffer-name "*Guix Generation Info*"
  :format '((number format guix-generation-info-insert-number)
            (prev-number format (format))
            (current format guix-generation-info-insert-current)
            (path simple (indent guix-file))
            (time format (time)))
  :titles '((path . "File name")
            (prev-number . "Previous number")))

(defface guix-generation-info-number
  '((t :inherit font-lock-keyword-face))
  "Face used for a number of a generation."
  :group 'guix-generation-info-faces)

(defface guix-generation-info-current
  '((t :inherit guix-package-info-installed-outputs))
  "Face used if a generation is the current one."
  :group 'guix-generation-info-faces)

(defface guix-generation-info-not-current
  '((t nil))
  "Face used if a generation is not the current one."
  :group 'guix-generation-info-faces)

(defun guix-generation-info-insert-number (number &optional _)
  "Insert generation NUMBER and action buttons."
  (guix-info-insert-value-format number 'guix-generation-info-number)
  (guix-info-insert-indent)
  (guix-info-insert-action-button
   "Packages"
   (lambda (btn)
     (guix-buffer-get-display-entries
      'list guix-package-list-type
      (list (guix-ui-current-profile)
            'generation (button-get btn 'number))
      'add))
   "Show installed packages for this generation"
   'number number)
  (guix-info-insert-indent)
  (guix-info-insert-action-button
   "Delete"
   (lambda (btn)
     (guix-delete-generations (guix-ui-current-profile)
                              (list (button-get btn 'number))
                              (current-buffer)))
   "Delete this generation"
   'number number))

(defun guix-generation-info-insert-current (val entry)
  "Insert boolean value VAL showing whether this generation is current."
  (if val
      (guix-info-insert-value-format "Yes" 'guix-generation-info-current)
    (guix-info-insert-value-format "No" 'guix-generation-info-not-current)
    (guix-info-insert-indent)
    (guix-info-insert-action-button
     "Switch"
     (lambda (btn)
       (guix-switch-to-generation (guix-ui-current-profile)
                                  (button-get btn 'number)
                                  (current-buffer)))
     "Switch to this generation (make it the current one)"
     'number (guix-entry-value entry 'number))))


;;; Generation 'list'

(guix-ui-list-define-interface generation
  :buffer-name "*Guix Generation List*"
  :format '((number nil 5 guix-list-sort-numerically-0 :right-align t)
            (current guix-generation-list-get-current 10 t)
            (time guix-list-get-time 20 t)
            (path guix-list-get-file-path 30 t))
  :titles '((number . "N."))
  :sort-key '(number . t)
  :marks '((delete . ?D)))

(let ((map guix-generation-list-mode-map))
  (define-key map (kbd "RET") 'guix-generation-list-show-packages)
  (define-key map (kbd "+")   'guix-generation-list-show-added-packages)
  (define-key map (kbd "-")   'guix-generation-list-show-removed-packages)
  (define-key map (kbd "=")   'guix-generation-list-diff)
  (define-key map (kbd "D")   'guix-generation-list-diff)
  (define-key map (kbd "e")   'guix-generation-list-ediff)
  (define-key map (kbd "x")   'guix-generation-list-execute)
  (define-key map (kbd "s")   'guix-generation-list-switch)
  (define-key map (kbd "c")   'guix-generation-list-switch)
  (define-key map (kbd "d")   'guix-generation-list-mark-delete))

(defun guix-generation-list-get-current (val &optional _)
  "Return string from VAL showing whether this generation is current.
VAL is a boolean value."
  (if val "(current)" ""))

(defun guix-generation-list-switch ()
  "Switch current profile to the generation at point."
  (interactive)
  (let* ((entry   (guix-list-current-entry))
         (current (guix-entry-value entry 'current))
         (number  (guix-entry-value entry 'number)))
    (if current
        (user-error "This generation is already the current one")
      (guix-switch-to-generation (guix-ui-current-profile)
                                 number (current-buffer)))))

(defun guix-generation-list-show-packages ()
  "List installed packages for the generation at point."
  (interactive)
  (guix-package-get-display
   (guix-ui-current-profile)
   'generation (guix-list-current-id)))

(defun guix-generation-list-generations-to-compare ()
  "Return a sorted list of 2 marked generations for comparing."
  (let ((numbers (guix-list-get-marked-id-list 'general)))
    (if (/= (length numbers) 2)
        (user-error "2 generations should be marked for comparing")
      (sort numbers #'<))))

(defun guix-generation-list-show-added-packages ()
  "List package outputs added to the latest marked generation.
If 2 generations are marked with \\[guix-list-mark], display
outputs installed in the latest marked generation that were not
installed in the other one."
  (interactive)
  (guix-buffer-get-display-entries
   'list 'output
   (cl-list* (guix-ui-current-profile)
             'generation-diff
             (reverse (guix-generation-list-generations-to-compare)))
   'add))

(defun guix-generation-list-show-removed-packages ()
  "List package outputs removed from the latest marked generation.
If 2 generations are marked with \\[guix-list-mark], display
outputs not installed in the latest marked generation that were
installed in the other one."
  (interactive)
  (guix-buffer-get-display-entries
   'list 'output
   (cl-list* (guix-ui-current-profile)
             'generation-diff
             (guix-generation-list-generations-to-compare))
   'add))

(defun guix-generation-list-compare (diff-fun gen-fun)
  "Run GEN-FUN on the 2 marked generations and run DIFF-FUN on the results."
  (cl-multiple-value-bind (gen1 gen2)
      (guix-generation-list-generations-to-compare)
    (funcall diff-fun
             (funcall gen-fun gen1)
             (funcall gen-fun gen2))))

(defun guix-generation-list-ediff-manifests ()
  "Run Ediff on manifests of the 2 marked generations."
  (interactive)
  (guix-generation-list-compare
   #'ediff-files
   #'guix-profile-generation-manifest-file))

(defun guix-generation-list-diff-manifests ()
  "Run Diff on manifests of the 2 marked generations."
  (interactive)
  (guix-generation-list-compare
   #'guix-diff
   #'guix-profile-generation-manifest-file))

(defun guix-generation-list-ediff-packages ()
  "Run Ediff on package outputs installed in the 2 marked generations."
  (interactive)
  (guix-generation-list-compare
   #'ediff-buffers
   #'guix-profile-generation-packages-buffer))

(defun guix-generation-list-diff-packages ()
  "Run Diff on package outputs installed in the 2 marked generations."
  (interactive)
  (guix-generation-list-compare
   #'guix-diff
   #'guix-profile-generation-packages-buffer))

(defun guix-generation-list-ediff (arg)
  "Run Ediff on package outputs installed in the 2 marked generations.
With ARG, run Ediff on manifests of the marked generations."
  (interactive "P")
  (if arg
      (guix-generation-list-ediff-manifests)
    (guix-generation-list-ediff-packages)))

(defun guix-generation-list-diff (arg)
  "Run Diff on package outputs installed in the 2 marked generations.
With ARG, run Diff on manifests of the marked generations."
  (interactive "P")
  (if arg
      (guix-generation-list-diff-manifests)
    (guix-generation-list-diff-packages)))

(defun guix-generation-list-mark-delete (&optional arg)
  "Mark the current generation for deletion and move to the next line.
With ARG, mark all generations for deletion."
  (interactive "P")
  (if arg
      (guix-list-mark-all 'delete)
    (guix-list--mark 'delete t)))

(defun guix-generation-list-execute ()
  "Delete marked generations."
  (interactive)
  (let ((marked (guix-list-get-marked-id-list 'delete)))
    (or marked
        (user-error "No generations marked for deletion"))
    (guix-delete-generations (guix-ui-current-profile)
                             marked (current-buffer))))


;;; Inserting packages to compare generations

(defcustom guix-generation-packages-buffer-name-function
  #'guix-generation-packages-buffer-name-default
  "Function used to define name of a buffer with generation packages.
This function is called with 2 arguments: PROFILE (string) and
GENERATION (number)."
  :type '(choice (function-item guix-generation-packages-buffer-name-default)
                 (function-item guix-generation-packages-buffer-name-long)
                 (function :tag "Other function"))
  :group 'guix-generation)

(defcustom guix-generation-packages-update-buffer t
  "If non-nil, always update list of packages during comparing generations.
If nil, generation packages are received only once.  So when you
compare generation 1 and generation 2, the packages for both
generations will be received.  Then if you compare generation 1
and generation 3, only the packages for generation 3 will be
received.  Thus if you use comparing of different generations a
lot, you may set this variable to nil to improve the
performance."
  :type 'boolean
  :group 'guix-generation)

(defvar guix-generation-output-name-width 30
  "Width of an output name \"column\".
This variable is used in auxiliary buffers for comparing generations.")

(defun guix-generation-packages (profile generation)
  "Return a list of sorted packages installed in PROFILE's GENERATION.
Each element of the list is a list of the package specification
and its store path."
  (let ((names+paths (guix-eval-read
                      (guix-make-guile-expression
                       'generation-package-specifications+paths
                       profile generation))))
    (sort names+paths
          (lambda (a b)
            (string< (car a) (car b))))))

(defun guix-generation-packages-buffer-name-default (profile generation)
  "Return name of a buffer for displaying GENERATION's package outputs.
Use base name of PROFILE file name."
  (let ((profile-name (file-name-base (directory-file-name profile))))
    (format "*Guix %s: generation %s*"
            profile-name generation)))

(defun guix-generation-packages-buffer-name-long (profile generation)
  "Return name of a buffer for displaying GENERATION's package outputs.
Use the full PROFILE file name."
  (format "*Guix generation %s (%s)*"
          generation profile))

(defun guix-generation-packages-buffer-name (profile generation)
  "Return name of a buffer for displaying GENERATION's package outputs."
  (funcall guix-generation-packages-buffer-name-function
           profile generation))

(defun guix-generation-insert-package (name path)
  "Insert package output NAME and store PATH at point."
  (insert name)
  (indent-to guix-generation-output-name-width 2)
  (insert path "\n"))

(defun guix-generation-insert-packages (buffer profile generation)
  "Insert package outputs installed in PROFILE's GENERATION in BUFFER."
  (with-current-buffer buffer
    (setq buffer-read-only nil
          indent-tabs-mode nil)
    (erase-buffer)
    (mapc (lambda (name+path)
            (guix-generation-insert-package
             (car name+path) (cadr name+path)))
          (guix-generation-packages profile generation))))

(defun guix-generation-packages-buffer (profile generation)
  "Return buffer with package outputs installed in PROFILE's GENERATION.
Create the buffer if needed."
  (let ((buf-name (guix-generation-packages-buffer-name
                   profile generation)))
    (or (and (null guix-generation-packages-update-buffer)
             (get-buffer buf-name))
        (let ((buf (get-buffer-create buf-name)))
          (guix-generation-insert-packages buf profile generation)
          buf))))

(defun guix-profile-generation-manifest-file (generation)
  "Return the file name of a GENERATION's manifest.
GENERATION is a generation number of the current profile."
  (guix-manifest-file (guix-ui-current-profile) generation))

(defun guix-profile-generation-packages-buffer (generation)
  "Insert GENERATION's package outputs in a buffer and return it.
GENERATION is a generation number of the current profile."
  (guix-generation-packages-buffer (guix-ui-current-profile)
                                   generation))


;;; Interactive commands

;;;###autoload
(defun guix-generations (&optional profile)
  "Display information about all generations.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (and current-prefix-arg
              (guix-profile-prompt))))
  (guix-generation-get-display profile 'all))

;;;###autoload
(defun guix-last-generations (number &optional profile)
  "Display information about last NUMBER generations.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (read-number "The number of last generations: ")
         (and current-prefix-arg
              (guix-profile-prompt))))
  (guix-generation-get-display profile 'last number))

;;;###autoload
(defun guix-generations-by-time (from to &optional profile)
  "Display information about generations created between FROM and TO.
FROM and TO should be time values.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (guix-read-date "Find generations (from): ")
         (guix-read-date "Find generations (to): ")
         (and current-prefix-arg
              (guix-profile-prompt))))
  (guix-generation-get-display profile 'time
                               (float-time from)
                               (float-time to)))

(provide 'guix-ui-generation)

;;; guix-ui-generation.el ends here
