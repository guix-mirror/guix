;;; guix-ui-package.el --- Interface for displaying packages  -*- lexical-binding: t -*-

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

;; This file provides an interface for displaying packages and outputs
;; in 'list' and 'info' buffers, and commands for working with them.

;;; Code:

(require 'cl-lib)
(require 'guix-buffer)
(require 'guix-list)
(require 'guix-info)
(require 'guix-ui)
(require 'guix-base)
(require 'guix-backend)
(require 'guix-guile)
(require 'guix-entry)
(require 'guix-utils)
(require 'guix-hydra)
(require 'guix-hydra-build)
(require 'guix-read)
(require 'guix-license)
(require 'guix-profiles)

(guix-ui-define-entry-type package)
(guix-ui-define-entry-type output)

(defcustom guix-package-list-type 'output
  "Define how to display packages in 'list' buffer.
Should be a symbol `package' or `output' (if `output', display each
output on a separate line; if `package', display each package on
a separate line)."
  :type '(choice (const :tag "List of packages" package)
                 (const :tag "List of outputs" output))
  :group 'guix-package)

(defcustom guix-package-info-type 'package
  "Define how to display packages in 'info' buffer.
Should be a symbol `package' or `output' (if `output', display
each output separately; if `package', display outputs inside
package data)."
  :type '(choice (const :tag "Display packages" package)
                 (const :tag "Display outputs" output))
  :group 'guix-package)

(defun guix-package-get-display (profile search-type &rest search-values)
  "Search for packages/outputs and show results.

If PROFILE is nil, use `guix-current-profile'.

See `guix-ui-get-entries' for the meaning of SEARCH-TYPE and
SEARCH-VALUES.

Results are displayed in the list buffer, unless a single package
is found and `guix-package-list-single' is nil."
  (let* ((args    (cl-list* (or profile guix-current-profile)
                            search-type search-values))
         (entries (guix-buffer-get-entries
                   'list guix-package-list-type args)))
    (if (or guix-package-list-single
            (null entries)
            (cdr entries))
        (guix-buffer-display-entries
         entries 'list guix-package-list-type args 'add)
      (guix-buffer-get-display-entries
       'info guix-package-info-type args 'add))))

(defun guix-package-entry->name-specification (entry &optional output)
  "Return name specification of the package ENTRY and OUTPUT."
  (guix-package-name-specification
   (guix-entry-value entry 'name)
   (guix-entry-value entry 'version)
   (or output (guix-entry-value entry 'output))))

(defun guix-package-entries->name-specifications (entries)
  "Return name specifications by the package or output ENTRIES."
  (cl-remove-duplicates (mapcar #'guix-package-entry->name-specification
                                entries)
                        :test #'string=))

(defun guix-package-installed-outputs (entry)
  "Return a list of installed outputs for the package ENTRY."
  (mapcar (lambda (installed-entry)
            (guix-entry-value installed-entry 'output))
          (guix-entry-value entry 'installed)))

(defun guix-package-id-and-output-by-output-id (output-id)
  "Return a list (PACKAGE-ID OUTPUT) by OUTPUT-ID."
  (cl-multiple-value-bind (package-id-str output)
      (split-string output-id ":")
    (let ((package-id (string-to-number package-id-str)))
      (list (if (= 0 package-id) package-id-str package-id)
            output))))


;;; Processing package actions

(defun guix-process-package-actions (profile actions
                                     &optional operation-buffer)
  "Process package ACTIONS on PROFILE.
Each action is a list of the form:

  (ACTION-TYPE PACKAGE-SPEC ...)

ACTION-TYPE is one of the following symbols: `install',
`upgrade', `remove'/`delete'.
PACKAGE-SPEC should have the following form: (ID [OUTPUT] ...)."
  (let (install upgrade remove)
    (mapc (lambda (action)
            (let ((action-type (car action))
                  (specs (cdr action)))
              (cl-case action-type
                (install (setq install (append install specs)))
                (upgrade (setq upgrade (append upgrade specs)))
                ((remove delete) (setq remove (append remove specs))))))
          actions)
    (when (guix-continue-package-operation-p
           profile
           :install install :upgrade upgrade :remove remove)
      (guix-eval-in-repl
       (guix-make-guile-expression
        'process-package-actions profile
        :install install :upgrade upgrade :remove remove
        :use-substitutes? (or guix-use-substitutes 'f)
        :dry-run? (or guix-dry-run 'f))
       (and (not guix-dry-run) operation-buffer)))))

(cl-defun guix-continue-package-operation-p (profile
                                             &key install upgrade remove)
  "Return non-nil if a package operation should be continued.
Ask a user if needed (see `guix-operation-confirm').
INSTALL, UPGRADE, REMOVE are 'package action specifications'.
See `guix-process-package-actions' for details."
  (or (null guix-operation-confirm)
      (let* ((entries (guix-ui-get-entries
                       profile 'package 'id
                       (append (mapcar #'car install)
                               (mapcar #'car upgrade)
                               (mapcar #'car remove))
                       '(id name version location)))
             (install-strings (guix-get-package-strings install entries))
             (upgrade-strings (guix-get-package-strings upgrade entries))
             (remove-strings  (guix-get-package-strings remove entries)))
        (if (or install-strings upgrade-strings remove-strings)
            (let ((buf (get-buffer-create guix-temp-buffer-name)))
              (with-current-buffer buf
                (setq-local cursor-type nil)
                (setq buffer-read-only nil)
                (erase-buffer)
                (insert "Profile: " profile "\n\n")
                (guix-insert-package-strings install-strings "install")
                (guix-insert-package-strings upgrade-strings "upgrade")
                (guix-insert-package-strings remove-strings "remove")
                (let ((win (temp-buffer-window-show
                            buf
                            '((display-buffer-reuse-window
                               display-buffer-at-bottom)
                              (window-height . fit-window-to-buffer)))))
                  (prog1 (guix-operation-prompt)
                    (quit-window nil win)))))
          (message "Nothing to be done.
If Guix REPL was restarted, the data is not up-to-date.")
          nil))))

(defun guix-get-package-strings (specs entries)
  "Return short package descriptions for performing package actions.
See `guix-process-package-actions' for the meaning of SPECS.
ENTRIES is a list of package entries to get info about packages."
  (delq nil
        (mapcar
         (lambda (spec)
           (let* ((id (car spec))
                  (outputs (cdr spec))
                  (entry (guix-entry-by-id id entries)))
             (when entry
               (let ((location (guix-entry-value entry 'location)))
                 (concat (guix-package-entry->name-specification entry)
                         (when outputs
                           (concat ":"
                                   (guix-concat-strings outputs ",")))
                         (when location
                           (concat "\t(" location ")")))))))
         specs)))

(defun guix-insert-package-strings (strings action)
  "Insert information STRINGS at point for performing package ACTION."
  (when strings
    (insert "Package(s) to " (propertize action 'face 'bold) ":\n")
    (mapc (lambda (str)
            (insert "  " str "\n"))
          strings)
    (insert "\n")))


;;; Package 'info'

(guix-ui-info-define-interface package
  :buffer-name "*Guix Package Info*"
  :format '(guix-package-info-insert-heading
            ignore
            (synopsis ignore (simple guix-package-info-synopsis))
            ignore
            (description ignore (simple guix-package-info-description))
            ignore
            (outputs simple guix-package-info-insert-outputs)
            (source simple guix-package-info-insert-source)
            (location format (format guix-package-location))
            (home-url format (format guix-url))
            (license format (format guix-package-license))
            (systems format guix-package-info-insert-systems)
            (inputs format (format guix-package-input))
            (native-inputs format (format guix-package-native-input))
            (propagated-inputs format
                               (format guix-package-propagated-input)))
  :titles '((home-url . "Home page")
            (systems . "Supported systems"))
  :required '(id name version installed non-unique))

(guix-info-define-interface installed-output
  :format '((path simple (indent guix-file))
            (dependencies simple (indent guix-file)))
  :titles '((path . "Store directory"))
  :reduced? t)

(defface guix-package-info-heading
  '((t :inherit guix-info-heading))
  "Face for package name and version headings."
  :group 'guix-package-info-faces)

(defface guix-package-info-name
  '((t :inherit font-lock-keyword-face))
  "Face used for a name of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-name-button
  '((t :inherit button))
  "Face used for a full name that can be used to describe a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-version
  '((t :inherit font-lock-builtin-face))
  "Face used for a version of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-synopsis
  '((((type tty pc) (class color)) :weight bold)
    (t :height 1.1 :weight bold :inherit variable-pitch))
  "Face used for a synopsis of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-description
  '((t))
  "Face used for a description of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-license
  '((t :inherit font-lock-string-face))
  "Face used for a license of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-location
  '((t :inherit link))
  "Face used for a location of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-source
  '((t :inherit link :underline nil))
  "Face used for a source URL of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-installed-outputs
  '((default :weight bold)
    (((class color) (min-colors 88) (background light))
     :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark))
     :foreground "PaleGreen")
    (((class color) (min-colors 8))
     :foreground "green")
    (t :underline t))
  "Face used for installed outputs of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-uninstalled-outputs
  '((t :weight bold))
  "Face used for uninstalled outputs of a package."
  :group 'guix-package-info-faces)

(defface guix-package-info-obsolete
  '((t :inherit error))
  "Face used if a package is obsolete."
  :group 'guix-package-info-faces)

(defcustom guix-package-info-auto-find-source nil
  "If non-nil, find a source file after pressing a \"Show\" button.
If nil, just display the source file path without finding."
  :type 'boolean
  :group 'guix-package-info)

(defcustom guix-package-info-auto-download-source t
  "If nil, do not automatically download a source file if it doesn't exist.
After pressing a \"Show\" button, a derivation of the package
source is calculated and a store file path is displayed.  If this
variable is non-nil and the source file does not exist in the
store, it will be automatically downloaded (with a possible
prompt depending on `guix-operation-confirm' variable)."
  :type 'boolean
  :group 'guix-package-info)

(defvar guix-package-info-download-buffer nil
  "Buffer from which a current download operation was performed.")

(defvar guix-package-info-output-format "%-10s"
  "String used to format output names of the packages.
It should be a '%s'-sequence.  After inserting an output name
formatted with this string, an action button is inserted.")

(defvar guix-package-info-obsolete-string "(This package is obsolete)"
  "String used if a package is obsolete.")

(define-button-type 'guix-package-location
  :supertype 'guix
  'face 'guix-package-info-location
  'help-echo "Find location of this package"
  'action (lambda (btn)
            (guix-find-location (button-label btn))))

(define-button-type 'guix-package-license
  :supertype 'guix
  'face 'guix-package-info-license
  'help-echo "Browse license URL"
  'action (lambda (btn)
            (guix-browse-license-url (button-label btn))))

(define-button-type 'guix-package-name
  :supertype 'guix
  'face 'guix-package-info-name-button
  'help-echo "Describe this package"
  'action (lambda (btn)
            (guix-buffer-get-display-entries-current
             'info guix-package-info-type
             (list (guix-ui-current-profile)
                   'name (or (button-get btn 'spec)
                             (button-label btn)))
             'add)))

(define-button-type 'guix-package-heading
  :supertype 'guix-package-name
  'face 'guix-package-info-heading)

(define-button-type 'guix-package-source
  :supertype 'guix
  'face 'guix-package-info-source
  'help-echo ""
  'action (lambda (_)
            ;; As a source may not be a real URL (e.g., "mirror://..."),
            ;; no action is bound to a source button.
            (message "Yes, this is the source URL. What did you expect?")))

(defun guix-package-info-insert-heading (entry)
  "Insert package ENTRY heading (name and version) at point."
  (guix-insert-button
   (concat (guix-entry-value entry 'name) " "
           (guix-entry-value entry 'version))
   'guix-package-heading
   'spec (guix-package-entry->name-specification entry)))

(defun guix-package-info-insert-systems (systems entry)
  "Insert supported package SYSTEMS at point."
  (guix-info-insert-value-format
   systems 'guix-hydra-build-system
   'action (lambda (btn)
             (let ((args (guix-hydra-build-latest-prompt-args
                          :job (button-get btn 'job-name)
                          :system (button-label btn))))
               (apply #'guix-hydra-build-get-display
                      'latest args)))
   'job-name (guix-hydra-job-name-specification
              (guix-entry-value entry 'name)
              (guix-entry-value entry 'version))))

(defmacro guix-package-info-define-insert-inputs (&optional type)
  "Define a face and a function for inserting package inputs.
TYPE is a type of inputs.
Function name is `guix-package-info-insert-TYPE-inputs'.
Face name is `guix-package-info-TYPE-inputs'."
  (let* ((type-str (symbol-name type))
         (type-name (and type (concat type-str "-")))
         (type-desc (and type (concat type-str " ")))
         (face (intern (concat "guix-package-info-" type-name "inputs")))
         (btn  (intern (concat "guix-package-" type-name "input"))))
    `(progn
       (defface ,face
         '((t :inherit guix-package-info-name-button))
         ,(concat "Face used for " type-desc "inputs of a package.")
         :group 'guix-package-info-faces)

       (define-button-type ',btn
         :supertype 'guix-package-name
         'face ',face))))

(guix-package-info-define-insert-inputs)
(guix-package-info-define-insert-inputs native)
(guix-package-info-define-insert-inputs propagated)

(defun guix-package-info-insert-outputs (outputs entry)
  "Insert OUTPUTS from package ENTRY at point."
  (and (guix-entry-value entry 'obsolete)
       (guix-package-info-insert-obsolete-text))
  (and (guix-entry-value entry 'non-unique)
       (guix-entry-value entry 'installed)
       (guix-package-info-insert-non-unique-text
        (guix-package-entry->name-specification entry)))
  (insert "\n")
  (dolist (output outputs)
    (guix-package-info-insert-output output entry)))

(defun guix-package-info-insert-obsolete-text ()
  "Insert a message about obsolete package at point."
  (guix-info-insert-indent)
  (guix-format-insert guix-package-info-obsolete-string
                      'guix-package-info-obsolete))

(defun guix-package-info-insert-non-unique-text (full-name)
  "Insert a message about non-unique package with FULL-NAME at point."
  (insert "\n")
  (guix-info-insert-indent)
  (insert "Installed outputs are displayed for a non-unique ")
  (guix-insert-button full-name 'guix-package-name)
  (insert " package."))

(defun guix-package-info-insert-output (output entry)
  "Insert OUTPUT at point.
Make some fancy text with buttons and additional stuff if the
current OUTPUT is installed (if there is such output in
`installed' parameter of a package ENTRY)."
  (let* ((installed (guix-entry-value entry 'installed))
         (obsolete  (guix-entry-value entry 'obsolete))
         (installed-entry (cl-find-if
                           (lambda (entry)
                             (string= (guix-entry-value entry 'output)
                                      output))
                           installed))
         (action-type (if installed-entry 'delete 'install))
         (profile (guix-ui-current-profile)))
    (guix-info-insert-indent)
    (guix-format-insert output
                        (if installed-entry
                            'guix-package-info-installed-outputs
                          'guix-package-info-uninstalled-outputs)
                        guix-package-info-output-format)
    ;; Do not allow a user to install/delete anything to/from a system
    ;; profile, so add action buttons only for non-system profiles.
    (when (and profile
               (not (guix-system-profile? profile)))
      (guix-package-info-insert-action-button action-type entry output)
      (when obsolete
        (guix-info-insert-indent)
        (guix-package-info-insert-action-button 'upgrade entry output)))
    (insert "\n")
    (when installed-entry
      (guix-info-insert-entry installed-entry 'installed-output 2))))

(defun guix-package-info-insert-action-button (type entry output)
  "Insert button to process an action on a package OUTPUT at point.
TYPE is one of the following symbols: `install', `delete', `upgrade'.
ENTRY is an alist with package info."
  (let ((type-str  (capitalize (symbol-name type)))
        (full-name (guix-package-entry->name-specification entry output)))
    (guix-info-insert-action-button
     type-str
     (lambda (btn)
       (guix-process-package-actions
        (guix-ui-current-profile)
        `((,(button-get btn 'action-type) (,(button-get btn 'id)
                                           ,(button-get btn 'output))))
        (current-buffer)))
     (concat type-str " '" full-name "'")
     'action-type type
     'id (or (guix-entry-value entry 'package-id)
             (guix-entry-id entry))
     'output output)))

(defun guix-package-info-show-source (entry-id package-id)
  "Show file name of a package source in the current info buffer.
Find the file if needed (see `guix-package-info-auto-find-source').
ENTRY-ID is an ID of the current entry (package or output).
PACKAGE-ID is an ID of the package which source to show."
  (let* ((entries (guix-buffer-current-entries))
         (entry   (guix-entry-by-id entry-id entries))
         (file    (guix-package-source-path package-id)))
    (or file
        (error "Couldn't define file name of the package source"))
    (let* ((new-entry (cons (cons 'source-file file)
                            entry))
           (new-entries (guix-replace-entry entry-id new-entry entries)))
      (setf (guix-buffer-item-entries guix-buffer-item)
            new-entries)
      (guix-buffer-redisplay-goto-button)
      (if (file-exists-p file)
          (if guix-package-info-auto-find-source
              (guix-find-file file)
            (message "The source store path is displayed."))
        (if guix-package-info-auto-download-source
            (guix-package-info-download-source package-id)
          (message "The source does not exist in the store."))))))

(defun guix-package-info-download-source (package-id)
  "Download a source of the package PACKAGE-ID."
  (setq guix-package-info-download-buffer (current-buffer))
  (guix-package-source-build-derivation
   package-id
   "The source does not exist in the store. Download it?"))

(defun guix-package-info-insert-source (source entry)
  "Insert SOURCE from package ENTRY at point.
SOURCE is a list of URLs."
  (if (null source)
      (guix-format-insert nil)
    (let* ((source-file (guix-entry-value entry 'source-file))
           (entry-id    (guix-entry-id entry))
           (package-id  (or (guix-entry-value entry 'package-id)
                            entry-id)))
      (if (null source-file)
          (guix-info-insert-action-button
           "Show"
           (lambda (btn)
             (guix-package-info-show-source (button-get btn 'entry-id)
                                            (button-get btn 'package-id)))
           "Show the source store directory of the current package"
           'entry-id entry-id
           'package-id package-id)
        (unless (file-exists-p source-file)
          (guix-info-insert-action-button
           "Download"
           (lambda (btn)
             (guix-package-info-download-source
              (button-get btn 'package-id)))
           "Download the source into the store"
           'package-id package-id))
        (guix-info-insert-value-indent source-file 'guix-file))
      (guix-info-insert-value-indent source 'guix-package-source))))

(defun guix-package-info-redisplay-after-download ()
  "Redisplay an 'info' buffer after downloading the package source.
This function is used to hide a \"Download\" button if needed."
  (when (buffer-live-p guix-package-info-download-buffer)
    (with-current-buffer guix-package-info-download-buffer
      (guix-buffer-redisplay-goto-button))
    (setq guix-package-info-download-buffer nil)))

(add-hook 'guix-after-source-download-hook
          'guix-package-info-redisplay-after-download)


;;; Package 'list'

(guix-ui-list-define-interface package
  :buffer-name "*Guix Package List*"
  :format '((name guix-package-list-get-name 20 t)
            (version nil 10 nil)
            (outputs nil 13 t)
            (installed guix-package-list-get-installed-outputs 13 t)
            (synopsis guix-list-get-one-line 30 nil))
  :sort-key '(name)
  :marks '((install . ?I)
           (upgrade . ?U)
           (delete  . ?D)))

(let ((map guix-package-list-mode-map))
  (define-key map (kbd "B")   'guix-package-list-latest-builds)
  (define-key map (kbd "e")   'guix-package-list-edit)
  (define-key map (kbd "x")   'guix-package-list-execute)
  (define-key map (kbd "i")   'guix-package-list-mark-install)
  (define-key map (kbd "d")   'guix-package-list-mark-delete)
  (define-key map (kbd "U")   'guix-package-list-mark-upgrade)
  (define-key map (kbd "^")   'guix-package-list-mark-upgrades))

(defface guix-package-list-installed
  '((t :inherit guix-package-info-installed-outputs))
  "Face used if there are installed outputs for the current package."
  :group 'guix-package-list-faces)

(defface guix-package-list-obsolete
  '((t :inherit guix-package-info-obsolete))
  "Face used if a package is obsolete."
  :group 'guix-package-list-faces)

(defcustom guix-package-list-generation-marking-enabled nil
  "If non-nil, allow putting marks in a list with 'generation packages'.

By default this is disabled, because it may be confusing.  For
example, a package is installed in some generation, so a user can
mark it for deletion in the list of packages from this
generation, but the package may not be installed in the latest
generation, so actually it cannot be deleted.

If you managed to understand the explanation above or if you
really know what you do or if you just don't care, you can set
this variable to t.  It should not do much harm anyway (most
likely)."
  :type 'boolean
  :group 'guix-package-list)

(defun guix-package-list-get-name (name entry)
  "Return NAME of the package ENTRY.
Colorize it with `guix-package-list-installed' or
`guix-package-list-obsolete' if needed."
  (guix-get-string name
                   (cond ((guix-entry-value entry 'obsolete)
                          'guix-package-list-obsolete)
                         ((guix-entry-value entry 'installed)
                          'guix-package-list-installed))))

(defun guix-package-list-get-installed-outputs (installed &optional _)
  "Return string with outputs from INSTALLED entries."
  (guix-get-string
   (mapcar (lambda (entry)
             (guix-entry-value entry 'output))
           installed)))

(defun guix-package-list-marking-check ()
  "Signal an error if marking is disabled for the current buffer."
  (when (and (not guix-package-list-generation-marking-enabled)
             (or (derived-mode-p 'guix-package-list-mode)
                 (derived-mode-p 'guix-output-list-mode))
             (eq (guix-ui-current-search-type) 'generation))
    (error "Action marks are disabled for lists of 'generation packages'")))

(defun guix-package-list-mark-outputs (mark default
                                       &optional prompt available)
  "Mark the current package with MARK and move to the next line.
If PROMPT is non-nil, use it to ask a user for outputs from
AVAILABLE list, otherwise mark all DEFAULT outputs."
  (let ((outputs (if prompt
                     (guix-completing-read-multiple
                      prompt available nil t)
                   default)))
    (apply #'guix-list--mark mark t outputs)))

(defun guix-package-list-mark-install (&optional arg)
  "Mark the current package for installation and move to the next line.
With ARG, prompt for the outputs to install (several outputs may
be separated with \",\")."
  (interactive "P")
  (guix-package-list-marking-check)
  (let* ((entry     (guix-list-current-entry))
         (all       (guix-entry-value entry 'outputs))
         (installed (guix-package-installed-outputs entry))
         (available (cl-set-difference all installed :test #'string=)))
    (or available
        (user-error "This package is already installed"))
    (guix-package-list-mark-outputs
     'install '("out")
     (and arg "Output(s) to install: ")
     available)))

(defun guix-package-list-mark-delete (&optional arg)
  "Mark the current package for deletion and move to the next line.
With ARG, prompt for the outputs to delete (several outputs may
be separated with \",\")."
  (interactive "P")
  (guix-package-list-marking-check)
  (let* ((entry (guix-list-current-entry))
         (installed (guix-package-installed-outputs entry)))
    (or installed
        (user-error "This package is not installed"))
    (guix-package-list-mark-outputs
     'delete installed
     (and arg "Output(s) to delete: ")
     installed)))

(defun guix-package-list-mark-upgrade (&optional arg)
  "Mark the current package for upgrading and move to the next line.
With ARG, prompt for the outputs to upgrade (several outputs may
be separated with \",\")."
  (interactive "P")
  (guix-package-list-marking-check)
  (let* ((entry (guix-list-current-entry))
         (installed (guix-package-installed-outputs entry)))
    (or installed
        (user-error "This package is not installed"))
    (when (or (guix-entry-value entry 'obsolete)
              (y-or-n-p "This package is not obsolete.  Try to upgrade it anyway? "))
      (guix-package-list-mark-outputs
       'upgrade installed
       (and arg "Output(s) to upgrade: ")
       installed))))

(defun guix-package-mark-upgrades (fun)
  "Mark all obsolete packages for upgrading.
Use FUN to perform marking of the current line.  FUN should
take an entry as argument."
  (guix-package-list-marking-check)
  (let ((obsolete (cl-remove-if-not
                   (lambda (entry)
                     (guix-entry-value entry 'obsolete))
                   (guix-buffer-current-entries))))
    (guix-list-for-each-line
     (lambda ()
       (let* ((id (guix-list-current-id))
              (entry (cl-find-if
                      (lambda (entry)
                        (equal id (guix-entry-id entry)))
                      obsolete)))
         (when entry
           (funcall fun entry)))))))

(defun guix-package-list-mark-upgrades ()
  "Mark all obsolete packages for upgrading."
  (interactive)
  (guix-package-mark-upgrades
   (lambda (entry)
     (apply #'guix-list--mark
            'upgrade nil
            (guix-package-installed-outputs entry)))))

(defun guix-package-assert-non-system-profile ()
  "Verify that the current profile is not a system one.
The current profile is the one used by the current buffer."
  (let ((profile (guix-ui-current-profile)))
    (and profile
         (guix-system-profile? profile)
         (user-error "Packages cannot be installed or removed to/from \
profile '%s'.
Use 'guix system reconfigure' shell command to modify a system profile."
                     profile))))

(defun guix-package-execute-actions (fun)
  "Perform actions on the marked packages.
Use FUN to define actions suitable for `guix-process-package-actions'.
FUN should take action-type as argument."
  (guix-package-assert-non-system-profile)
  (let ((actions (delq nil
                       (mapcar fun '(install delete upgrade)))))
    (if actions
        (guix-process-package-actions (guix-ui-current-profile)
                                      actions (current-buffer))
      (user-error "No operations specified"))))

(defun guix-package-list-execute ()
  "Perform actions on the marked packages."
  (interactive)
  (guix-package-execute-actions #'guix-package-list-make-action))

(defun guix-package-list-make-action (action-type)
  "Return action specification for the packages marked with ACTION-TYPE.
Return nil, if there are no packages marked with ACTION-TYPE.
The specification is suitable for `guix-process-package-actions'."
  (let ((specs (guix-list-get-marked-args action-type)))
    (and specs (cons action-type specs))))

(defun guix-package-list-edit (&optional directory)
  "Go to the location of the current package.
See `guix-find-location' for the meaning of DIRECTORY."
  (interactive (list (guix-read-directory)))
  (guix-edit (guix-list-current-id) directory))

(defun guix-package-list-latest-builds (number &rest args)
  "Display latest NUMBER of Hydra builds of the current package.
Interactively, prompt for NUMBER.  With prefix argument, prompt
for all ARGS."
  (interactive
   (let ((entry (guix-list-current-entry)))
     (guix-hydra-build-latest-prompt-args
      :job (guix-hydra-job-name-specification
            (guix-entry-value entry 'name)
            (guix-entry-value entry 'version)))))
  (apply #'guix-hydra-latest-builds number args))


;;; Output 'info'

(guix-ui-info-define-interface output
  :buffer-name "*Guix Package Info*"
  :format '((name format (format guix-package-info-name))
            (version format guix-output-info-insert-version)
            (output format guix-output-info-insert-output)
            (synopsis simple (indent guix-package-info-synopsis))
            (source simple guix-package-info-insert-source)
            (path simple (indent guix-file))
            (dependencies simple (indent guix-file))
            (location format (format guix-package-location))
            (home-url format (format guix-url))
            (license format (format guix-package-license))
            (systems format guix-package-info-insert-systems)
            (inputs format (format guix-package-input))
            (native-inputs format (format guix-package-native-input))
            (propagated-inputs format
                               (format guix-package-propagated-input))
            (description simple (indent guix-package-info-description)))
  :titles guix-package-info-titles
  :required '(id package-id installed non-unique))

(defun guix-output-info-insert-version (version entry)
  "Insert output VERSION and obsolete text if needed at point."
  (guix-info-insert-value-format version
                                 'guix-package-info-version)
  (and (guix-entry-value entry 'obsolete)
       (guix-package-info-insert-obsolete-text)))

(defun guix-output-info-insert-output (output entry)
  "Insert OUTPUT and action buttons at point."
  (let* ((installed   (guix-entry-value entry 'installed))
         (obsolete    (guix-entry-value entry 'obsolete))
         (action-type (if installed 'delete 'install)))
    (guix-info-insert-value-format
     output
     (if installed
         'guix-package-info-installed-outputs
       'guix-package-info-uninstalled-outputs))
    (guix-info-insert-indent)
    (guix-package-info-insert-action-button action-type entry output)
    (when obsolete
      (guix-info-insert-indent)
      (guix-package-info-insert-action-button 'upgrade entry output))))


;;; Output 'list'

(guix-ui-list-define-interface output
  :buffer-name "*Guix Package List*"
  :describe-function 'guix-output-list-describe
  :format '((name guix-package-list-get-name 20 t)
            (version nil 10 nil)
            (output nil 9 t)
            (installed nil 12 t)
            (synopsis guix-list-get-one-line 30 nil))
  :required '(id package-id)
  :sort-key '(name)
  :marks '((install . ?I)
           (upgrade . ?U)
           (delete  . ?D)))

(let ((map guix-output-list-mode-map))
  (define-key map (kbd "B")   'guix-package-list-latest-builds)
  (define-key map (kbd "e")   'guix-output-list-edit)
  (define-key map (kbd "x")   'guix-output-list-execute)
  (define-key map (kbd "i")   'guix-output-list-mark-install)
  (define-key map (kbd "d")   'guix-output-list-mark-delete)
  (define-key map (kbd "U")   'guix-output-list-mark-upgrade)
  (define-key map (kbd "^")   'guix-output-list-mark-upgrades))

(defun guix-output-list-mark-install ()
  "Mark the current output for installation and move to the next line."
  (interactive)
  (guix-package-list-marking-check)
  (let* ((entry     (guix-list-current-entry))
         (installed (guix-entry-value entry 'installed)))
    (if installed
        (user-error "This output is already installed")
      (guix-list--mark 'install t))))

(defun guix-output-list-mark-delete ()
  "Mark the current output for deletion and move to the next line."
  (interactive)
  (guix-package-list-marking-check)
  (let* ((entry     (guix-list-current-entry))
         (installed (guix-entry-value entry 'installed)))
    (if installed
        (guix-list--mark 'delete t)
      (user-error "This output is not installed"))))

(defun guix-output-list-mark-upgrade ()
  "Mark the current output for upgrading and move to the next line."
  (interactive)
  (guix-package-list-marking-check)
  (let* ((entry     (guix-list-current-entry))
         (installed (guix-entry-value entry 'installed)))
    (or installed
        (user-error "This output is not installed"))
    (when (or (guix-entry-value entry 'obsolete)
              (y-or-n-p "This output is not obsolete.  Try to upgrade it anyway? "))
      (guix-list--mark 'upgrade t))))

(defun guix-output-list-mark-upgrades ()
  "Mark all obsolete package outputs for upgrading."
  (interactive)
  (guix-package-mark-upgrades
   (lambda (_) (guix-list--mark 'upgrade))))

(defun guix-output-list-execute ()
  "Perform actions on the marked outputs."
  (interactive)
  (guix-package-execute-actions #'guix-output-list-make-action))

(defun guix-output-list-make-action (action-type)
  "Return action specification for the outputs marked with ACTION-TYPE.
Return nil, if there are no outputs marked with ACTION-TYPE.
The specification is suitable for `guix-process-output-actions'."
  (let ((ids (guix-list-get-marked-id-list action-type)))
    (and ids (cons action-type
                   (mapcar #'guix-package-id-and-output-by-output-id
                           ids)))))

(defun guix-output-list-describe (ids)
  "Describe outputs with IDS (list of output identifiers).
See `guix-package-info-type'."
  (if (eq guix-package-info-type 'output)
      (guix-buffer-get-display-entries
       'info 'output
       (cl-list* (guix-ui-current-profile) 'id ids)
       'add)
    (let ((pids (mapcar (lambda (oid)
                          (car (guix-package-id-and-output-by-output-id
                                oid)))
                        ids)))
      (guix-buffer-get-display-entries
       'info 'package
       (cl-list* (guix-ui-current-profile)
                 'id (cl-remove-duplicates pids))
       'add))))

(defun guix-output-list-edit (&optional directory)
  "Go to the location of the current package.
See `guix-find-location' for the meaning of DIRECTORY."
  (interactive (list (guix-read-directory)))
  (guix-edit (guix-entry-value (guix-list-current-entry)
                               'package-id)
             directory))


;;; Interactive commands

(defvar guix-package-search-params '(name synopsis description)
  "Default list of package parameters for searching by regexp.")

(defvar guix-package-search-history nil
  "A history of minibuffer prompts.")

;;;###autoload
(defun guix-packages-by-name (name &optional profile)
  "Display Guix packages with NAME.
NAME is a string with name specification.  It may optionally contain
a version number.  Examples: \"guile\", \"guile@2.0.11\".

If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (guix-read-package-name)
         (guix-ui-read-profile)))
  (guix-package-get-display profile 'name name))

;;;###autoload
(defun guix-packages-by-license (license &optional profile)
  "Display Guix packages with LICENSE.
LICENSE is a license name string.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (guix-read-license-name)
         (guix-ui-read-profile)))
  (guix-package-get-display profile 'license license))

;;;###autoload
(defun guix-search-by-regexp (regexp &optional params profile)
  "Search for Guix packages by REGEXP.
PARAMS are package parameters that should be searched.
If PARAMS are not specified, use `guix-package-search-params'.

If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (read-regexp "Regexp: " nil 'guix-package-search-history)
         nil (guix-ui-read-profile)))
  (guix-package-get-display profile 'regexp regexp
                            (or params guix-package-search-params)))

;;;###autoload
(defun guix-search-by-name (regexp &optional profile)
  "Search for Guix packages matching REGEXP in a package name.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive
   (list (read-string "Package name by regexp: "
                      nil 'guix-package-search-history)
         (guix-ui-read-profile)))
  (guix-search-by-regexp regexp '(name) profile))

;;;###autoload
(defun guix-installed-packages (&optional profile)
  "Display information about installed Guix packages.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive (list (guix-ui-read-profile)))
  (guix-package-get-display profile 'installed))

;;;###autoload
(defun guix-installed-user-packages ()
  "Display information about Guix packages installed in a user profile."
  (interactive)
  (guix-installed-packages guix-user-profile))

;;;###autoload
(defun guix-installed-system-packages ()
  "Display information about Guix packages installed in a system profile."
  (interactive)
  (guix-installed-packages
   (guix-packages-profile guix-system-profile nil t)))

;;;###autoload
(defun guix-obsolete-packages (&optional profile)
  "Display information about obsolete Guix packages.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive (list (guix-ui-read-profile)))
  (guix-package-get-display profile 'obsolete))

;;;###autoload
(defun guix-all-available-packages (&optional profile)
  "Display information about all available Guix packages.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive (list (guix-ui-read-profile)))
  (guix-package-get-display profile 'all-available))

;;;###autoload
(defun guix-newest-available-packages (&optional profile)
  "Display information about the newest available Guix packages.
If PROFILE is nil, use `guix-current-profile'.
Interactively with prefix, prompt for PROFILE."
  (interactive (list (guix-ui-read-profile)))
  (guix-package-get-display profile 'newest-available))

(provide 'guix-ui-package)

;;; guix-ui-package.el ends here
