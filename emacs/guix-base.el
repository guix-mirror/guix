;;; guix-base.el --- Common definitions   -*- lexical-binding: t -*-

;; Copyright © 2014 Alex Kost <alezost@gmail.com>

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

;; This file provides some base and common definitions for guix.el
;; package.

;; List and info buffers have many common patterns that are defined
;; using `guix-define-buffer-type' macro from this file.

;;; Code:

(require 'cl-lib)
(require 'guix-backend)
(require 'guix-utils)


;;; Profiles

(defvar guix-user-profile
  (expand-file-name "~/.guix-profile")
  "User profile.")

(defvar guix-default-profile
  (concat (or (getenv "NIX_STATE_DIR") "/var/guix")
          "/profiles/per-user/"
          (getenv "USER")
          "/guix-profile")
  "Default Guix profile.")

(defvar guix-current-profile guix-default-profile
  "Current profile.")

(defun guix-set-current-profile (path)
  "Set `guix-current-profile' to PATH.
Interactively, prompt for PATH.  With prefix, use
`guix-default-profile'."
  (interactive
   (list (if current-prefix-arg
             guix-default-profile
           (read-file-name "Set profile: "
                           (file-name-directory guix-current-profile)))))
  (let ((path (directory-file-name (expand-file-name path))))
    (setq guix-current-profile
          (if (string= path guix-user-profile)
              guix-default-profile
            path))
    (message "Current profile has been set to '%s'."
             guix-current-profile)))


;;; Parameters of the entries

(defvar guix-param-titles
  '((package
     (id                . "ID")
     (name              . "Name")
     (version           . "Version")
     (license           . "License")
     (synopsis          . "Synopsis")
     (description       . "Description")
     (home-url          . "Home page")
     (outputs           . "Outputs")
     (inputs            . "Inputs")
     (native-inputs     . "Native inputs")
     (propagated-inputs . "Propagated inputs")
     (location          . "Location")
     (installed         . "Installed"))
    (installed
     (path              . "Installed path")
     (dependencies      . "Dependencies")
     (output            . "Output"))
    (generation
     (id                . "ID")
     (number            . "Number")
     (prev-number       . "Previous number")
     (path              . "Path")
     (time              . "Time")))
  "List for defining titles of entry parameters.
Titles are used for displaying information about entries.
Each element of the list has a form:

  (ENTRY-TYPE . ((PARAM . TITLE) ...))")

(defun guix-get-param-title (entry-type param)
  "Return title of an ENTRY-TYPE entry parameter PARAM."
  (or (guix-get-key-val guix-param-titles
                        entry-type param)
      (prog1 (symbol-name param)
        (message "Couldn't find title for '%S %S'."
                 entry-type param))))

(defun guix-get-name-spec (name version &optional output)
  "Return Guix package specification by its NAME, VERSION and OUTPUT."
  (concat name "-" version
          (when output (concat ":" output))))

(defun guix-get-full-name (entry &optional output)
  "Return name specification of the package ENTRY and OUTPUT."
  (guix-get-name-spec (guix-get-key-val entry 'name)
                      (guix-get-key-val entry 'version)
                      output))

(defun guix-get-installed-outputs (entry)
  "Return list of installed outputs for the package ENTRY."
  (mapcar (lambda (installed-entry)
            (guix-get-key-val installed-entry 'output))
          (guix-get-key-val entry 'installed)))

(defun guix-get-entry-by-id (id entries)
  "Return entry from ENTRIES by entry ID."
  (cl-find-if (lambda (entry)
                (equal id (guix-get-key-val entry 'id)))
              entries))


;;; Location of the packages

(defvar guix-directory nil
  "Default Guix directory.
If it is not set by a user, it is set after starting Guile REPL.
This directory is used to define location of the packages.")

(defun guix-set-directory ()
  "Set `guix-directory' if needed."
  (or guix-directory
      (setq guix-directory
            (guix-eval-read "%guix-dir"))))

(add-hook 'guix-after-start-repl-hook 'guix-set-directory)

(defun guix-find-location (location)
  "Go to LOCATION of a package.
LOCATION is a string of the form:

  \"PATH:LINE:COLUMN\"

If PATH is relative, it is considered to be relative to
`guix-directory'."
  (cl-multiple-value-bind (path line col)
      (split-string location ":")
    (let ((file (expand-file-name path guix-directory))
          (line (string-to-number line))
          (col  (string-to-number col)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (- line 1))
      (move-to-column col)
      (recenter 1))))


;;; Common definitions for buffer types

(defvar-local guix-entries nil
  "List of the currently displayed entries.
Each element of the list is alist with entry info of the
following form:

  ((PARAM . VAL) ...)

PARAM is a name of the entry parameter.
VAL is a value of this parameter.")
(put 'guix-entries 'permanent-local t)

(defvar-local guix-buffer-type nil
  "Type of the current buffer.")
(put 'guix-buffer-type 'permanent-local t)

(defvar-local guix-entry-type nil
  "Type of the current entry.")
(put 'guix-entry-type 'permanent-local t)

(defvar-local guix-search-type nil
  "Type of the current search.")
(put 'guix-search-type 'permanent-local t)

(defvar-local guix-search-vals nil
  "Values of the current search.")
(put 'guix-search-vals 'permanent-local t)

(defsubst guix-set-vars (entries buffer-type entry-type
                         search-type search-vals)
  (setq guix-entries     entries
        guix-buffer-type buffer-type
        guix-entry-type  entry-type
        guix-search-type search-type
        guix-search-vals search-vals))

(defun guix-get-symbol (postfix buffer-type &optional entry-type)
  (intern (concat "guix-"
                  (when entry-type
                    (concat (symbol-name entry-type) "-"))
                  (symbol-name buffer-type) "-" postfix)))

(defmacro guix-define-buffer-type (buf-type entry-type &rest args)
  "Define common for BUF-TYPE buffers for displaying ENTRY-TYPE entries.

In the text below TYPE means ENTRY-TYPE-BUF-TYPE.

This macro defines `guix-TYPE-mode', a custom group and several
user variables.

The following stuff should be defined outside this macro:

  - `guix-BUF-TYPE-mode' - parent mode for the defined mode.

  - `guix-TYPE-mode-initialize' (optional) - function for
  additional mode settings; it is called without arguments.

Remaining argument (ARGS) should have a form [KEYWORD VALUE] ...  The
following keywords are available:

  - `:required' - default value for the defined
    `guix-TYPE-required-params' variable.

  - `:history-size' - default value for the defined
    `guix-TYPE-history-size' variable.

  - `:revert' - default value for the defined
    `guix-TYPE-revert-no-confirm' variable."
  (let* ((entry-type-str (symbol-name entry-type))
         (buf-type-str   (symbol-name buf-type))
         (Entry-type-str (capitalize entry-type-str))
         (Buf-type-str   (capitalize buf-type-str))
         (entry-str      (concat entry-type-str " entries"))
         (buf-str        (concat buf-type-str " buffer"))
         (prefix         (concat "guix-" entry-type-str "-" buf-type-str))
         (group          (intern prefix))
         (mode-map-str   (concat prefix "-mode-map"))
         (mode-map       (intern mode-map-str))
         (parent-mode    (intern (concat "guix-" buf-type-str "-mode")))
         (mode           (intern (concat prefix "-mode")))
         (mode-init-fun  (intern (concat prefix "-mode-initialize")))
         (buf-name-var   (intern (concat prefix "-buffer-name")))
         (revert-var     (intern (concat prefix "-revert-no-confirm")))
         (history-var    (intern (concat prefix "-history-size")))
         (params-var     (intern (concat prefix "-required-params")))
         (revert-val     nil)
         (history-val    20)
         (params-val     '(id)))

    ;; Process the keyword args.
    (while (keywordp (car args))
      (pcase (pop args)
	(`:required     (setq params-val (pop args)))
	(`:history-size (setq history-val (pop args)))
	(`:revert       (setq revert-val (pop args)))
	(_ (pop args))))

    `(progn
       (defgroup ,group nil
         ,(concat Buf-type-str " buffer with " entry-str ".")
         :prefix ,(concat prefix "-")
         :group ',(intern (concat "guix-" buf-type-str)))

       (defcustom ,buf-name-var ,(format "*Guix %s %s*"
                                         Entry-type-str Buf-type-str)
         ,(concat "Default name of the " buf-str " for displaying " entry-str ".")
         :type 'string
         :group ',group)

       (defcustom ,history-var ,history-val
         ,(concat "Maximum number of items saved in the history of the " buf-str ".\n"
                  "If 0, the history is disabled.")
         :type 'integer
         :group ',group)

       (defcustom ,revert-var ,revert-val
         ,(concat "If non-nil, do not ask to confirm for reverting the " buf-str ".")
         :type 'boolean
         :group ',group)

       (defvar ,params-var ',params-val
         ,(concat "List of required " entry-type-str " parameters.\n\n"
                  "Displayed parameters and parameters from this list are received\n"
                  "for each " entry-type-str ".\n\n"
                  "May be a special value `all', in which case all supported\n"
                  "parameters are received (this may be very slow for a big number\n"
                  "of entries).\n\n"
                  "Do not remove `id' from this list as it is required for\n"
                  "identifying an entry."))

       (define-derived-mode ,mode ,parent-mode ,(concat "Guix-" Buf-type-str)
         ,(concat "Major mode for displaying information about " entry-str ".\n\n"
                  "\\{" mode-map-str "}")
         (setq-local revert-buffer-function 'guix-revert-buffer)
         (setq-local guix-history-size ,history-var)
         (and (fboundp ',mode-init-fun) (,mode-init-fun)))

       (let ((map ,mode-map))
         (define-key map (kbd "l") 'guix-history-back)
         (define-key map (kbd "r") 'guix-history-forward)
         (define-key map (kbd "g") 'revert-buffer)
         (define-key map (kbd "R") 'guix-redisplay-buffer)
         (define-key map (kbd "C-c C-z") 'guix-switch-to-repl)))))

(put 'guix-define-buffer-type 'lisp-indent-function 'defun)


;;; Getting info about packages and generations

(defun guix-get-entries (entry-type search-type search-vals
                         &optional params)
  "Search for entries of ENTRY-TYPE.

Call an appropriate scheme function and return a list of the
form of `guix-entries'.

ENTRY-TYPE should be one of the following symbols: `package',
`output' or `generation'.

SEARCH-TYPE may be one of the following symbols:

- If ENTRY-TYPE is `package' or `output': `id', `name', `regexp',
  `all-available', `newest-available', `installed', `obsolete',
  `generation'.

- If ENTRY-TYPE is `generation': `id', `last', `all'.

PARAMS is a list of parameters for receiving.  If nil, get
information with all available parameters."
  (guix-eval-read (guix-make-guile-expression
                   'entries
                   guix-current-profile params
                   entry-type search-type search-vals)))

(defun guix-get-show-entries (buffer-type entry-type search-type
                                          &rest search-vals)
  "Search for ENTRY-TYPE entries and show results in BUFFER-TYPE buffer.
See `guix-get-entries' for the meaning of SEARCH-TYPE and SEARCH-VALS."
  (let ((entries (guix-get-entries entry-type search-type search-vals
                                   (guix-get-params-for-receiving
                                    buffer-type entry-type))))
    (guix-set-buffer entries buffer-type entry-type
                     search-type search-vals)))

(defun guix-set-buffer (entries buffer-type entry-type search-type
                        search-vals &optional history-replace)
  "Set up BUFFER-TYPE buffer for displaying ENTRY-TYPE ENTRIES.

Display ENTRIES, set variables and make history item.
ENTRIES should have a form of `guix-entries'.

See `guix-get-entries' for the meaning of SEARCH-TYPE and SEARCH-VALS.

If HISTORY-REPLACE is non-nil, replace current history item,
otherwise add the new one."
  (when entries
    (let ((buf (if (eq major-mode (guix-get-symbol
                                   "mode" buffer-type entry-type))
                   (current-buffer)
                 (get-buffer-create
                  (symbol-value
                   (guix-get-symbol "buffer-name"
                                    buffer-type entry-type))))))
      (with-current-buffer buf
        (guix-show-entries entries buffer-type entry-type)
        (guix-set-vars entries buffer-type entry-type
                       search-type search-vals)
        (funcall (if history-replace
                     #'guix-history-replace
                   #'guix-history-add)
                 (guix-make-history-item)))
      (pop-to-buffer buf
                     '((display-buffer-reuse-window
                        display-buffer-same-window)))))
  (guix-result-message entries entry-type search-type search-vals))

(defun guix-show-entries (entries buffer-type entry-type)
  "Display ENTRY-TYPE ENTRIES in the current BUFFER-TYPE buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (funcall (symbol-function (guix-get-symbol
                               "mode" buffer-type entry-type)))
    (funcall (guix-get-symbol "insert-entries" buffer-type)
             entries entry-type)
    (goto-char (point-min))))

(defun guix-history-call (entries buffer-type entry-type
                          search-type search-vals)
  "Function called for moving by history."
  (guix-show-entries entries buffer-type entry-type)
  (guix-set-vars entries buffer-type entry-type
                 search-type search-vals)
  (guix-result-message entries entry-type search-type search-vals))

(defun guix-make-history-item ()
  "Make and return a history item for the current buffer."
  (list #'guix-history-call
        guix-entries guix-buffer-type guix-entry-type
        guix-search-type guix-search-vals))

(defun guix-get-params-for-receiving (buffer-type entry-type)
  "Return parameters that should be received for BUFFER-TYPE, ENTRY-TYPE."
  (let* ((required-var (guix-get-symbol "required-params"
                                        buffer-type entry-type))
         (required (symbol-value required-var)))
    (unless (equal required 'all)
      (cl-union required
                (funcall (guix-get-symbol "get-displayed-params"
                                          buffer-type)
                         entry-type)))))

(defun guix-revert-buffer (_ignore-auto noconfirm)
  "Update information in the current buffer.
The function is suitable for `revert-buffer-function'.
See `revert-buffer' for the meaning of NOCONFIRM."
  (when (or noconfirm
            (symbol-value
             (guix-get-symbol "revert-no-confirm"
                              guix-buffer-type guix-entry-type))
            (y-or-n-p "Update current information? "))
    (let ((entries (guix-get-entries
                    guix-entry-type guix-search-type guix-search-vals
                    (guix-get-params-for-receiving guix-buffer-type
                                                   guix-entry-type))))
      (guix-set-buffer entries guix-buffer-type guix-entry-type
                       guix-search-type guix-search-vals t))))

(defun guix-redisplay-buffer ()
  "Redisplay current information.
This function will not update the information, use
\"\\[revert-buffer]\" if you want the full update."
  (interactive)
  (guix-show-entries guix-entries guix-buffer-type guix-entry-type)
  (guix-result-message guix-entries guix-entry-type
                       guix-search-type guix-search-vals))


;;; Messages

(defvar guix-messages
  '((package
     (id
      (0 "Packages not found.")
      (1 "")
      (many "%d packages." count))
     (name
      (0 "The package '%s' not found." val)
      (1 "A single package with name '%s'." val)
      (many "%d packages with '%s' name." count val))
     (regexp
      (0 "No packages matching '%s'." val)
      (1 "A single package matching '%s'." val)
      (many "%d packages matching '%s'." count val))
     (all-available
      (0 "No packages are available for some reason.")
      (1 "A single available package (that's strange).")
      (many "%d available packages." count))
     (newest-available
      (0 "No packages are available for some reason.")
      (1 "A single newest available package (that's strange).")
      (many "%d newest available packages." count))
     (installed
      (0 "No installed packages.")
      (1 "A single installed package.")
      (many "%d installed packages." count))
     (obsolete
      (0 "No obsolete packages.")
      (1 "A single obsolete package.")
      (many "%d obsolete packages." count))
     (generation
      (0 "No packages installed in generation %d." val)
      (1 "A single package installed in generation %d." val)
      (many "%d packages installed in generation %d." count val)))
    (generation
     (id
      (0 "Generations not found.")
      (1 "")
      (many "%d generations." count))
     (last
      (0 "No available generations.")
      (1 "The last generation.")
      (many "%d last generations." count))
     (all
      (0 "No available generations.")
      (1 "A single available generation.")
      (many "%d available generations." count)))))

(defun guix-result-message (entries entry-type search-type search-vals)
  "Display an appropriate message after displaying ENTRIES."
  (let* ((val (car search-vals))
         (count (length entries))
         (count-key (if (> count 1) 'many count))
         (msg-spec (guix-get-key-val guix-messages
                                     entry-type search-type count-key))
         (format (car msg-spec))
         (args (cdr msg-spec)))
    (mapc (lambda (subst)
            (setq args (cl-substitute (car subst) (cdr subst) args)))
          (list (cons count 'count)
                (cons val 'val)))
    (apply #'message format args)))


;;; Actions on packages and generations

(defcustom guix-operation-confirm t
  "If nil, do not prompt to confirm an operation."
  :type 'boolean
  :group 'guix)

(defcustom guix-use-substitutes t
  "If non-nil, use substitutes for the Guix packages."
  :type 'boolean
  :group 'guix)

(defvar guix-dry-run nil
  "If non-nil, do not perform the real actions, just simulate.")

(defvar guix-temp-buffer-name " *Guix temp*"
  "Name of a buffer used for displaying info before executing operation.")

(defun guix-process-package-actions (&rest actions)
  "Process package ACTIONS.
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
           :install install :upgrade upgrade :remove remove)
      (guix-eval-in-repl
       (guix-make-guile-expression
        'process-package-actions guix-current-profile
        :install install :upgrade upgrade :remove remove
        :use-substitutes? (or guix-use-substitutes 'f)
        :dry-run? (or guix-dry-run 'f))))))

(cl-defun guix-continue-package-operation-p (&key install upgrade remove)
  "Return non-nil if a package operation should be continued.
Ask a user if needed (see `guix-operation-confirm').
INSTALL, UPGRADE, REMOVE are 'package action specifications'.
See `guix-process-package-actions' for details."
  (or (null guix-operation-confirm)
      (let* ((entries (guix-get-entries
                       'package 'id
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
                (guix-insert-package-strings install-strings "install")
                (guix-insert-package-strings upgrade-strings "upgrade")
                (guix-insert-package-strings remove-strings "remove")
                (let ((win (temp-buffer-window-show
                            buf
                            '((display-buffer-reuse-window
                               display-buffer-at-bottom)
                              (window-height . fit-window-to-buffer)))))
                  (prog1 (y-or-n-p "Continue operation? ")
                    (quit-window nil win)))))
          (message "Nothing to be done.  If the REPL was restarted, information is not up-to-date.")
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
                  (entry (guix-get-entry-by-id id entries)))
             (when entry
               (let ((location (guix-get-key-val entry 'location)))
                 (concat (guix-get-full-name entry)
                         (when outputs
                           (concat ":"
                                   (mapconcat #'identity outputs ",")))
                         (when location
                           (concat "\t(" location ")")))))))
         specs)))

(defun guix-insert-package-strings (strings action)
  "Insert information STRINGS at point for performing package ACTION."
  (when strings
    (insert "Package(s) to " (guix-get-string action 'bold) ":\n")
    (mapc (lambda (str)
            (insert "  " str "\n"))
          strings)
    (insert "\n")))

(provide 'guix-base)

;;; guix-base.el ends here
