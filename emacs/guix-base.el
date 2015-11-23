;;; guix-base.el --- Common definitions   -*- lexical-binding: t -*-

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

;; This file provides some base and common definitions for guix.el
;; package.

;; List and info buffers have many common patterns that are defined
;; using `guix-buffer-define-interface' macro from this file.

;;; Code:

(require 'cl-lib)
(require 'guix-profiles)
(require 'guix-backend)
(require 'guix-entry)
(require 'guix-guile)
(require 'guix-utils)
(require 'guix-ui)
(require 'guix-history)
(require 'guix-messages)


;;; Parameters of the entries

(defun guix-package-name-specification (name version &optional output)
  "Return Guix package specification by its NAME, VERSION and OUTPUT."
  (concat name "-" version
          (when output (concat ":" output))))

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
  "Return list of installed outputs for the package ENTRY."
  (mapcar (lambda (installed-entry)
            (guix-entry-value installed-entry 'output))
          (guix-entry-value entry 'installed)))

(defun guix-package-id-and-output-by-output-id (oid)
  "Return list (PACKAGE-ID OUTPUT) by output id OID."
  (cl-multiple-value-bind (pid-str output)
      (split-string oid ":")
    (let ((pid (string-to-number pid-str)))
      (list (if (= 0 pid) pid-str pid)
            output))))


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

(defun guix-package-location (id-or-name)
  "Return location of a package with ID-OR-NAME.
For the meaning of location, see `guix-find-location'."
  (guix-eval-read (guix-make-guile-expression
                   'package-location-string id-or-name)))


;;; Receivable lists of packages, lint checkers, etc.

(guix-memoized-defun guix-graph-type-names ()
  "Return a list of names of available graph node types."
  (guix-eval-read (guix-make-guile-expression 'graph-type-names)))

(guix-memoized-defun guix-refresh-updater-names ()
  "Return a list of names of available refresh updater types."
  (guix-eval-read (guix-make-guile-expression 'refresh-updater-names)))

(guix-memoized-defun guix-lint-checker-names ()
  "Return a list of names of available lint checkers."
  (guix-eval-read (guix-make-guile-expression 'lint-checker-names)))

(guix-memoized-defun guix-package-names ()
  "Return a list of names of available packages."
  (sort
   ;; Work around <https://github.com/jaor/geiser/issues/64>:
   ;; list of strings is parsed much slower than list of lists,
   ;; so we use 'package-names-lists' instead of 'package-names'.

   ;; (guix-eval-read (guix-make-guile-expression 'package-names))

   (mapcar #'car
           (guix-eval-read (guix-make-guile-expression
                            'package-names-lists)))
   #'string<))


;;; Buffers

(defun guix-switch-to-buffer (buffer)
  "Switch to a 'list' or 'info' BUFFER."
  (pop-to-buffer buffer
                 '((display-buffer-reuse-window
                    display-buffer-same-window))))


;;; Common definitions for buffer types

(defvar guix-buffer-data nil
  "Alist with 'buffer' data.
This alist is filled by `guix-buffer-define-interface' macro.")

(defun guix-buffer-value (buffer-type entry-type symbol)
  "Return SYMBOL's value for BUFFER-TYPE/ENTRY-TYPE from `guix-buffer-data'."
  (symbol-value
   (guix-assq-value guix-buffer-data buffer-type entry-type symbol)))

(defun guix-buffer-param-title (buffer-type entry-type param)
  "Return PARAM title for BUFFER-TYPE/ENTRY-TYPE."
  (or (guix-assq-value (guix-buffer-value buffer-type entry-type 'titles)
                       param)
      ;; Fallback to a title defined in 'info' interface.
      (unless (eq buffer-type 'info)
        (guix-assq-value (guix-buffer-value 'info entry-type 'titles)
                         param))
      (guix-symbol-title param)))

(defun guix-buffer-name (buffer-type entry-type profile)
  "Return name of BUFFER-TYPE buffer for displaying ENTRY-TYPE entries."
  (let ((str-or-fun (guix-buffer-value buffer-type entry-type
                                       'buffer-name)))
    (if (stringp str-or-fun)
        str-or-fun
      (funcall str-or-fun profile))))

(defun guix-buffer-history-size (buffer-type entry-type)
  "Return history size for BUFFER-TYPE/ENTRY-TYPE."
  (guix-buffer-value buffer-type entry-type 'history-size))

(defun guix-buffer-revert-confirm? (buffer-type entry-type)
  "Return 'revert-confirm' value for BUFFER-TYPE/ENTRY-TYPE."
  (guix-buffer-value buffer-type entry-type 'revert-confirm))

(defvar guix-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'guix-history-back)
    (define-key map (kbd "r") 'guix-history-forward)
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "R") 'guix-buffer-redisplay)
    map)
  "Parent keymap for Guix buffer modes.")

(defvar-local guix-profile nil
  "Profile used for the current buffer.")
(put 'guix-profile 'permanent-local t)

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

(defsubst guix-set-vars (profile entries buffer-type entry-type
                         search-type search-vals)
  "Set local variables for the current Guix buffer."
  (setq default-directory profile
        guix-profile      profile
        guix-entries      entries
        guix-buffer-type  buffer-type
        guix-entry-type   entry-type
        guix-search-type  search-type
        guix-search-vals  search-vals))

(defun guix-get-symbol (postfix buffer-type &optional entry-type)
  (intern (concat "guix-"
                  (when entry-type
                    (concat (symbol-name entry-type) "-"))
                  (symbol-name buffer-type) "-" postfix)))

(defmacro guix-buffer-define-interface (buffer-type entry-type &rest args)
  "Define BUFFER-TYPE interface for displaying ENTRY-TYPE entries.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...
In the following description TYPE means ENTRY-TYPE-BUFFER-TYPE.

The following stuff should be defined outside this macro:

  - `guix-BUFFER-TYPE-mode' - parent mode of the generated mode.

  - `guix-TYPE-mode-initialize' (optional) - function for
  additional mode settings; it is called without arguments.

Required keywords:

  - `:buffer-name' - default value of the generated
    `guix-TYPE-buffer-name' variable.

Optional keywords:

  - `:titles' - default value of the generated
    `guix-TYPE-titles' variable.

  - `:history-size' - default value of the generated
    `guix-TYPE-history-size' variable.

  - `:revert-confirm?' - default value of the generated
    `guix-TYPE-revert-confirm' variable.

  - `:reduced?' - if non-nil, generate only group, faces group
    and titles variable."
  (declare (indent 2))
  (let* ((entry-type-str     (symbol-name entry-type))
         (buffer-type-str    (symbol-name buffer-type))
         (Entry-type-str     (capitalize entry-type-str))
         (Buffer-type-str    (capitalize buffer-type-str))
         (entry-str          (concat entry-type-str " entries"))
         (prefix             (concat "guix-" entry-type-str "-"
                                     buffer-type-str))
         (group              (intern prefix))
         (faces-group        (intern (concat prefix "-faces")))
         (mode-map-str       (concat prefix "-mode-map"))
         (parent-mode        (intern (concat "guix-" buffer-type-str "-mode")))
         (mode               (intern (concat prefix "-mode")))
         (mode-init-fun      (intern (concat prefix "-mode-initialize")))
         (buffer-name-var    (intern (concat prefix "-buffer-name")))
         (titles-var         (intern (concat prefix "-titles")))
         (history-size-var   (intern (concat prefix "-history-size")))
         (revert-confirm-var (intern (concat prefix "-revert-confirm"))))
    (guix-keyword-args-let args
        ((buffer-name-val    :buffer-name)
         (titles-val         :titles)
         (history-size-val   :history-size 20)
         (revert-confirm-val :revert-confirm? t)
         (reduced?           :reduced?))
      `(progn
         (defgroup ,group nil
           ,(format "Display '%s' entries in '%s' buffer."
                    entry-type-str buffer-type-str)
           :prefix ,(concat prefix "-")
           :group ',(intern (concat "guix-" buffer-type-str)))

         (defgroup ,faces-group nil
           ,(format "Faces for displaying '%s' entries in '%s' buffer."
                    entry-type-str buffer-type-str)
           :group ',(intern (concat "guix-" buffer-type-str "-faces")))

         (defcustom ,titles-var ,titles-val
           ,(format "Alist of titles of '%s' parameters."
                    entry-type-str)
           :type '(alist :key-type symbol :value-type string)
           :group ',group)

         ,(unless reduced?
            `(progn
               (defcustom ,buffer-name-var ,buffer-name-val
                 ,(format "\
Default name of '%s' buffer for displaying '%s' entries."
                          buffer-type-str entry-type-str)
                 :type 'string
                 :group ',group)

               (defcustom ,history-size-var ,history-size-val
                 ,(format "\
Maximum number of items saved in history of `%S' buffer.
If 0, the history is disabled."
                          buffer-name-var)
                 :type 'integer
                 :group ',group)

               (defcustom ,revert-confirm-var ,revert-confirm-val
                 ,(format "\
If non-nil, ask to confirm for reverting `%S' buffer."
                          buffer-name-var)
                 :type 'boolean
                 :group ',group)

               (guix-alist-put!
                '((buffer-name    . ,buffer-name-var)
                  (history-size   . ,history-size-var)
                  (revert-confirm . ,revert-confirm-var))
                'guix-buffer-data ',buffer-type ',entry-type)

               (define-derived-mode ,mode ,parent-mode
                 ,(concat "Guix-" Buffer-type-str)
                 ,(concat "Major mode for displaying information about "
                          entry-str ".\n\n"
                          "\\{" mode-map-str "}")
                 (setq-local revert-buffer-function 'guix-buffer-revert)
                 (setq-local guix-history-size
                             (guix-buffer-history-size
                              ',buffer-type ',entry-type))
                 (and (fboundp ',mode-init-fun) (,mode-init-fun)))))

         (guix-alist-put!
          ',titles-var 'guix-buffer-data
          ',buffer-type ',entry-type 'titles)))))


;;; Getting and displaying info about packages and generations

(defcustom guix-package-list-type 'output
  "Define how to display packages in a list buffer.
May be a symbol `package' or `output' (if `output', display each
output on a separate line; if `package', display each package on
a separate line)."
  :type '(choice (const :tag "List of packages" package)
                 (const :tag "List of outputs" output))
  :group 'guix)

(defcustom guix-package-info-type 'package
  "Define how to display packages in an info buffer.
May be a symbol `package' or `output' (if `output', display each
output separately; if `package', display outputs inside a package
information)."
  :type '(choice (const :tag "Display packages" package)
                 (const :tag "Display outputs" output))
  :group 'guix)

(defun guix-get-entries (profile entry-type search-type search-vals
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

- If ENTRY-TYPE is `generation': `id', `last', `all', `time'.

PARAMS is a list of parameters for receiving.  If nil, get
information with all available parameters."
  (guix-eval-read (guix-make-guile-expression
                   'entries
                   profile params entry-type search-type search-vals)))

(defun guix-get-show-entries (profile buffer-type entry-type search-type
                              &rest search-vals)
  "Search for ENTRY-TYPE entries and show results in BUFFER-TYPE buffer.
See `guix-get-entries' for the meaning of SEARCH-TYPE and SEARCH-VALS."
  (let ((entries (guix-get-entries profile entry-type search-type search-vals
                                   (guix-get-params-for-receiving
                                    buffer-type entry-type))))
    (guix-set-buffer profile entries buffer-type entry-type
                     search-type search-vals)))

(defun guix-set-buffer (profile entries buffer-type entry-type search-type
                        search-vals &optional history-replace no-display)
  "Set up BUFFER-TYPE buffer for displaying ENTRY-TYPE ENTRIES.

Insert ENTRIES in buffer, set variables and make history item.
ENTRIES should have a form of `guix-entries'.

See `guix-get-entries' for the meaning of SEARCH-TYPE and SEARCH-VALS.

If HISTORY-REPLACE is non-nil, replace current history item,
otherwise add the new one.

If NO-DISPLAY is non-nil, do not switch to the buffer."
  (when entries
    (let ((buf (if (and (eq major-mode
                            (guix-get-symbol "mode" buffer-type entry-type))
                        (equal guix-profile profile))
                   (current-buffer)
                 (get-buffer-create
                  (guix-buffer-name buffer-type entry-type profile)))))
      (with-current-buffer buf
        (guix-show-entries entries buffer-type entry-type)
        (guix-set-vars profile entries buffer-type entry-type
                       search-type search-vals)
        (funcall (if history-replace
                     #'guix-history-replace
                   #'guix-history-add)
                 (guix-make-history-item)))
      (or no-display
          (guix-switch-to-buffer buf))))
  (guix-result-message profile entries entry-type
                       search-type search-vals))

(defun guix-show-entries (entries buffer-type entry-type)
  "Display ENTRY-TYPE ENTRIES in the current BUFFER-TYPE buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (funcall (symbol-function (guix-get-symbol
                               "mode" buffer-type entry-type)))
    (funcall (guix-get-symbol "insert-entries" buffer-type)
             entries entry-type)
    (goto-char (point-min))))

(defun guix-history-call (profile entries buffer-type entry-type
                          search-type search-vals)
  "Function called for moving by history."
  (guix-show-entries entries buffer-type entry-type)
  (guix-set-vars profile entries buffer-type entry-type
                 search-type search-vals)
  (guix-result-message profile entries entry-type
                       search-type search-vals))

(defun guix-make-history-item ()
  "Make and return a history item for the current buffer."
  (list #'guix-history-call
        guix-profile guix-entries guix-buffer-type guix-entry-type
        guix-search-type guix-search-vals))

(defun guix-get-params-for-receiving (buffer-type entry-type)
  "Return parameters that should be received for BUFFER-TYPE, ENTRY-TYPE."
  (let* ((required-var (guix-get-symbol "required-params"
                                        buffer-type entry-type))
         (required (symbol-value required-var)))
    (unless (equal required 'all)
      (cl-union required
                (funcall (guix-get-symbol "displayed-params"
                                          buffer-type)
                         entry-type)))))

(defun guix-buffer-revert (_ignore-auto noconfirm)
  "Update information in the current buffer.
The function is suitable for `revert-buffer-function'.
See `revert-buffer' for the meaning of NOCONFIRM."
  (when (or noconfirm
            (guix-buffer-revert-confirm? guix-buffer-type
                                         guix-entry-type)
            (y-or-n-p "Update current information? "))
    (let* ((params (guix-get-params-for-receiving guix-buffer-type
                                                  guix-entry-type))
           (entries (guix-get-entries
                     guix-profile guix-entry-type
                     guix-search-type guix-search-vals params)))
      (guix-set-buffer guix-profile entries guix-buffer-type guix-entry-type
                       guix-search-type guix-search-vals t t))))

(defvar guix-buffer-after-redisplay-hook nil
  "Hook run by `guix-buffer-redisplay'.
This hook is called before seting up a window position.")

(defun guix-buffer-redisplay ()
  "Redisplay the current Guix buffer.
Restore the point and window positions after redisplaying.

This function does not update the buffer data, use
'\\[revert-buffer]' if you want the full update."
  (interactive)
  (let* ((old-point (point))
         ;; For simplicity, ignore an unlikely case when multiple
         ;; windows display the same buffer.
         (window (car (get-buffer-window-list (current-buffer) nil t)))
         (window-start (and window (window-start window))))
    (guix-set-buffer guix-profile guix-entries guix-buffer-type
                     guix-entry-type guix-search-type guix-search-vals
                     t t)
    (goto-char old-point)
    (run-hooks 'guix-buffer-after-redisplay-hook)
    (when window
      (set-window-point window (point))
      (set-window-start window window-start))))

(defun guix-buffer-redisplay-goto-button ()
  "Redisplay the current buffer and go to the next button, if needed."
  (let ((guix-buffer-after-redisplay-hook
         (cons (lambda ()
                 (unless (button-at (point))
                   (forward-button 1)))
               guix-buffer-after-redisplay-hook)))
    (guix-buffer-redisplay)))


;;; Generations

(defcustom guix-generation-packages-buffer-name-function
  #'guix-generation-packages-buffer-name-default
  "Function used to define name of a buffer with generation packages.
This function is called with 2 arguments: PROFILE (string) and
GENERATION (number)."
  :type '(choice (function-item guix-generation-packages-buffer-name-default)
                 (function-item guix-generation-packages-buffer-name-long)
                 (function :tag "Other function"))
  :group 'guix)

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
  :group 'guix)

(defvar guix-output-name-width 30
  "Width of an output name \"column\".
This variable is used in auxiliary buffers for comparing generations.")

(defun guix-generation-file (profile generation)
  "Return the file name of a PROFILE's GENERATION."
  (format "%s-%s-link" profile generation))

(defun guix-manifest-file (profile &optional generation)
  "Return the file name of a PROFILE's manifest.
If GENERATION number is specified, return manifest file name for
this generation."
  (expand-file-name "manifest"
                    (if generation
                        (guix-generation-file profile generation)
                      profile)))

(defun guix-generation-packages (profile generation)
  "Return a list of sorted packages installed in PROFILE's GENERATION.
Each element of the list is a list of the package specification and its path."
  (let ((names+paths (guix-eval-read
                      (guix-make-guile-expression
                       'generation-package-specifications+paths
                       profile generation))))
    (sort names+paths
          (lambda (a b)
            (string< (car a) (car b))))))

(defun guix-generation-packages-buffer-name-default (profile generation)
  "Return name of a buffer for displaying GENERATION's package outputs.
Use base name of PROFILE path."
  (let ((profile-name (file-name-base (directory-file-name profile))))
    (format "*Guix %s: generation %s*"
            profile-name generation)))

(defun guix-generation-packages-buffer-name-long (profile generation)
  "Return name of a buffer for displaying GENERATION's package outputs.
Use the full PROFILE path."
  (format "*Guix generation %s (%s)*"
          generation profile))

(defun guix-generation-packages-buffer-name (profile generation)
  "Return name of a buffer for displaying GENERATION's package outputs."
  (let ((fun (if (functionp guix-generation-packages-buffer-name-function)
                 guix-generation-packages-buffer-name-function
               #'guix-generation-packages-buffer-name-default)))
    (funcall fun profile generation)))

(defun guix-generation-insert-package (name path)
  "Insert package output NAME and PATH at point."
  (insert name)
  (indent-to guix-output-name-width 2)
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
GENERATION is a generation number of `guix-profile' profile."
  (guix-manifest-file guix-profile generation))

(defun guix-profile-generation-packages-buffer (generation)
  "Insert GENERATION's package outputs in a buffer and return it.
GENERATION is a generation number of `guix-profile' profile."
  (guix-generation-packages-buffer guix-profile generation))


;;; Actions on packages and generations

(defface guix-operation-option-key
  '((t :inherit font-lock-warning-face))
  "Face used for the keys of operation options."
  :group 'guix-faces)

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

(defvar guix-operation-option-true-string "yes"
  "String displayed in the mode-line when operation option is t.")

(defvar guix-operation-option-false-string "no "
  "String displayed in the mode-line when operation option is nil.")

(defvar guix-operation-option-separator "  |  "
  "String used in the mode-line to separate operation options.")

(defvar guix-operation-options
  '((?s "substitutes" guix-use-substitutes)
    (?d "dry-run"     guix-dry-run))
  "List of available operation options.
Each element of the list has a form:

  (KEY NAME VARIABLE)

KEY is a character that may be pressed during confirmation to
toggle the option.
NAME is a string displayed in the mode-line.
VARIABLE is a name of an option variable.")

(defun guix-operation-option-by-key (key)
  "Return operation option by KEY (character)."
  (assq key guix-operation-options))

(defun guix-operation-option-key (option)
  "Return key (character) of the operation OPTION."
  (car option))

(defun guix-operation-option-name (option)
  "Return name of the operation OPTION."
  (nth 1 option))

(defun guix-operation-option-variable (option)
  "Return name of the variable of the operation OPTION."
  (nth 2 option))

(defun guix-operation-option-value (option)
  "Return boolean value of the operation OPTION."
  (symbol-value (guix-operation-option-variable option)))

(defun guix-operation-option-string-value (option)
  "Convert boolean value of the operation OPTION to string and return it."
  (if (guix-operation-option-value option)
      guix-operation-option-true-string
    guix-operation-option-false-string))

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
      (let* ((entries (guix-get-entries
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

(defun guix-operation-prompt (&optional prompt)
  "Prompt a user for continuing the current operation.
Return non-nil, if the operation should be continued; nil otherwise.
Ask a user with PROMPT for continuing an operation."
  (let* ((option-keys (mapcar #'guix-operation-option-key
                              guix-operation-options))
         (keys (append '(?y ?n) option-keys))
         (prompt (concat (propertize (or prompt "Continue operation?")
                                     'face 'minibuffer-prompt)
                         " ("
                         (mapconcat
                          (lambda (key)
                            (propertize (string key)
                                        'face 'guix-operation-option-key))
                          keys
                          ", ")
                         ") ")))
    (let ((mode-line mode-line-format))
      (prog1 (guix-operation-prompt-1 prompt keys)
        (setq mode-line-format mode-line)
        ;; Clear the minibuffer after prompting.
        (message "")))))

(defun guix-operation-prompt-1 (prompt keys)
  "This function is internal for `guix-operation-prompt'."
  (guix-operation-set-mode-line)
  (let ((key (read-char-choice prompt (cons ?\C-g keys) t)))
    (cl-case key
      (?y t)
      ((?n ?\C-g) nil)
      (t (let* ((option (guix-operation-option-by-key key))
                (var (guix-operation-option-variable option)))
           (set var (not (symbol-value var)))
           (guix-operation-prompt-1 prompt keys))))))

(defun guix-operation-set-mode-line ()
  "Display operation options in the mode-line of the current buffer."
  (setq mode-line-format
        (concat (propertize " Options:   "
                            'face 'mode-line-buffer-id)
                (mapconcat
                 (lambda (option)
                   (let ((key  (guix-operation-option-key option))
                         (name (guix-operation-option-name option))
                         (val  (guix-operation-option-string-value option)))
                     (concat name
                             " ("
                             (propertize (string key)
                                         'face 'guix-operation-option-key)
                             "): " val)))
                 guix-operation-options
                 guix-operation-option-separator)))
  (force-mode-line-update))

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

(defun guix-package-source-path (package-id)
  "Return a store file path to a source of a package PACKAGE-ID."
  (message "Calculating the source derivation ...")
  (guix-eval-read
   (guix-make-guile-expression
    'package-source-path package-id)))

(defvar guix-after-source-download-hook nil
  "Hook run after successful performing a 'source-download' operation.")

(defun guix-package-source-build-derivation (package-id &optional prompt)
  "Build source derivation of a package PACKAGE-ID.
Ask a user with PROMPT for continuing an operation."
  (when (or (not guix-operation-confirm)
            (guix-operation-prompt (or prompt
                                       "Build the source derivation?")))
    (guix-eval-in-repl
     (guix-make-guile-expression
      'package-source-build-derivation
      package-id
      :use-substitutes? (or guix-use-substitutes 'f)
      :dry-run? (or guix-dry-run 'f))
     nil 'source-download)))

;;;###autoload
(defun guix-apply-manifest (profile file &optional operation-buffer)
  "Apply manifest from FILE to PROFILE.
This function has the same meaning as 'guix package --manifest' command.
See Info node `(guix) Invoking guix package' for details.

Interactively, use the current profile and prompt for manifest
FILE.  With a prefix argument, also prompt for PROFILE."
  (interactive
   (let* ((default-profile (or guix-profile guix-current-profile))
          (profile (if current-prefix-arg
                       (guix-profile-prompt)
                     default-profile))
          (file (read-file-name "File with manifest: "))
          (buffer (and guix-profile (current-buffer))))
     (list profile file buffer)))
  (when (or (not guix-operation-confirm)
            (y-or-n-p (format "Apply manifest from '%s' to profile '%s'? "
                              file profile)))
    (guix-eval-in-repl
     (guix-make-guile-expression
      'guix-command
      "package"
      (concat "--profile="  (expand-file-name profile))
      (concat "--manifest=" (expand-file-name file)))
     operation-buffer)))


;;; Executing guix commands

(defcustom guix-run-in-shell-function #'guix-run-in-shell
  "Function used to run guix command.
The function is called with a single argument - a command line string."
  :type '(choice (function-item guix-run-in-shell)
                 (function-item guix-run-in-eshell)
                 (function :tag "Other function"))
  :group 'guix)

(defcustom guix-shell-buffer-name "*shell*"
  "Default name of a shell buffer used for running guix commands."
  :type 'string
  :group 'guix)

(declare-function comint-send-input "comint" t)

(defun guix-run-in-shell (string)
  "Run command line STRING in `guix-shell-buffer-name' buffer."
  (shell guix-shell-buffer-name)
  (goto-char (point-max))
  (insert string)
  (comint-send-input))

(declare-function eshell-send-input "esh-mode" t)

(defun guix-run-in-eshell (string)
  "Run command line STRING in eshell buffer."
  (eshell)
  (goto-char (point-max))
  (insert string)
  (eshell-send-input))

(defun guix-run-command-in-shell (args)
  "Execute 'guix ARGS ...' command in a shell buffer."
  (funcall guix-run-in-shell-function
           (guix-command-string args)))

(defun guix-run-command-in-repl (args)
  "Execute 'guix ARGS ...' command in Guix REPL."
  (guix-eval-in-repl
   (apply #'guix-make-guile-expression
          'guix-command args)))

(defun guix-command-output (args)
  "Return string with 'guix ARGS ...' output."
  (cl-multiple-value-bind (output error)
      (guix-eval (apply #'guix-make-guile-expression
                        'guix-command-output args))
    ;; Remove trailing new space from the error string.
    (message (replace-regexp-in-string "\n\\'" "" (read error)))
    (read output)))

(defun guix-help-string (&optional commands)
  "Return string with 'guix COMMANDS ... --help' output."
  (guix-eval-read
   (apply #'guix-make-guile-expression
          'help-string commands)))


;;; Pull

(defcustom guix-update-after-pull t
  "If non-nil, update Guix buffers after performing \\[guix-pull]."
  :type 'boolean
  :group 'guix)

(defvar guix-after-pull-hook
  '(guix-restart-repl-after-pull guix-update-buffers-maybe-after-pull)
  "Hook run after successful performing `guix-pull' operation.")

(defun guix-restart-repl-after-pull ()
  "Restart Guix REPL after `guix-pull' operation."
  (guix-repl-exit)
  (guix-start-process-maybe
   "Restarting Guix REPL after pull operation ..."))

(defun guix-update-buffers-maybe-after-pull ()
  "Update buffers depending on `guix-update-after-pull'."
  (when guix-update-after-pull
    (mapc #'guix-ui-update-buffer
          ;; No need to update "generation" buffers.
          (guix-ui-buffers '(guix-package-list-mode
                             guix-package-info-mode
                             guix-output-list-mode
                             guix-output-info-mode)))
    (message "Guix buffers have been updated.")))

;;;###autoload
(defun guix-pull (&optional verbose)
  "Run Guix pull operation.
If VERBOSE is non-nil (with prefix argument), produce verbose output."
  (interactive "P")
  (let ((args (and verbose '("--verbose"))))
    (guix-eval-in-repl
     (apply #'guix-make-guile-expression
            'guix-command "pull" args)
     nil 'pull)))

(provide 'guix-base)

;;; guix-base.el ends here
