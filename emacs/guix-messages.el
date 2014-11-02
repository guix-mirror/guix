;;; guix-messages.el --- Minibuffer messages

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

;; This file provides `guix-result-message' function used to show a
;; minibuffer message after displaying packages/generations in a
;; list/info buffer.

;;; Code:

(require 'cl-lib)
(require 'guix-utils)

(defvar guix-messages
  `((package
     (id
      (0 "Packages not found.")
      (1 "")
      (many "%d packages." count))
     (name
      ,(lambda (_ entries names)
         (guix-message-packages-by-name entries 'package names)))
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
      (0 "No packages installed in profile '%s'." profile)
      (1 "A single package installed in profile '%s'." profile)
      (many "%d packages installed in profile '%s'." count profile))
     (obsolete
      (0 "No obsolete packages in profile '%s'." profile)
      (1 "A single obsolete package in profile '%s'." profile)
      (many "%d obsolete packages in profile '%s'." count profile))
     (generation
      (0 "No packages installed in generation %d of profile '%s'."
         val profile)
      (1 "A single package installed in generation %d of profile '%s'."
         val profile)
      (many "%d packages installed in generation %d of profile '%s'."
            count val profile)))

    (output
     (id
      (0 "Package outputs not found.")
      (1 "")
      (many "%d package outputs." count))
     (name
      ,(lambda (_ entries names)
         (guix-message-packages-by-name entries 'output names)))
     (regexp
      (0 "No package outputs matching '%s'." val)
      (1 "A single package output matching '%s'." val)
      (many "%d package outputs matching '%s'." count val))
     (all-available
      (0 "No package outputs are available for some reason.")
      (1 "A single available package output (that's strange).")
      (many "%d available package outputs." count))
     (newest-available
      (0 "No package outputs are available for some reason.")
      (1 "A single newest available package output (that's strange).")
      (many "%d newest available package outputs." count))
     (installed
      (0 "No package outputs installed in profile '%s'." profile)
      (1 "A single package output installed in profile '%s'." profile)
      (many "%d package outputs installed in profile '%s'." count profile))
     (obsolete
      (0 "No obsolete package outputs in profile '%s'." profile)
      (1 "A single obsolete package output in profile '%s'." profile)
      (many "%d obsolete package outputs in profile '%s'." count profile))
     (generation
      (0 "No package outputs installed in generation %d of profile '%s'."
         val profile)
      (1 "A single package output installed in generation %d of profile '%s'."
         val profile)
      (many "%d package outputs installed in generation %d of profile '%s'."
            count val profile))
     (generation-diff
      guix-message-outputs-by-diff))

    (generation
     (id
      (0 "Generations not found.")
      (1 "")
      (many "%d generations." count))
     (last
      (0 "No generations in profile '%s'." profile)
      (1 "The last generation of profile '%s'." profile)
      (many "%d last generations of profile '%s'." count profile))
     (all
      (0 "No generations in profile '%s'." profile)
      (1 "A single generation available in profile '%s'." profile)
      (many "%d generations available in profile '%s'." count profile))
     (time
      guix-message-generations-by-time))))

(defun guix-message-string-name (name)
  "Return a quoted name string."
  (concat "'" name "'"))

(defun guix-message-string-entry-type (entry-type &optional plural)
  "Return a string denoting an ENTRY-TYPE."
  (cl-ecase entry-type
    (package
     (if plural "packages" "package"))
    (output
     (if plural "package outputs" "package output"))
    (generation
     (if plural "generations" "generation"))))

(defun guix-message-string-entries (count entry-type)
  "Return a string denoting the COUNT of ENTRY-TYPE entries."
  (cl-case count
    (0 (concat "No "
               (guix-message-string-entry-type
                entry-type 'plural)))
    (1 (concat "A single "
               (guix-message-string-entry-type
                entry-type)))
    (t (format "%d %s"
               count
               (guix-message-string-entry-type
                entry-type 'plural)))))

(defun guix-message-packages-by-name (entries entry-type names)
  "Display a message for packages or outputs searched by NAMES."
  (let* ((count (length entries))
         (str-beg (guix-message-string-entries count entry-type))
         (str-end (if (cdr names)
                      (concat "matching the following names: "
                              (mapconcat #'guix-message-string-name
                                         names ", "))
                    (concat "with name "
                            (guix-message-string-name (car names))))))
    (message "%s %s." str-beg str-end)))

(defun guix-message-generations-by-time (profile entries times)
  "Display a message for generations searched by TIMES."
  (let* ((count (length entries))
         (str-beg (guix-message-string-entries count 'generation))
         (time-beg (guix-get-time-string (car  times)))
         (time-end (guix-get-time-string (cadr times))))
    (message (concat "%s of profile '%s'\n"
                     "matching time period '%s' - '%s'.")
             str-beg profile time-beg time-end)))

(defun guix-message-outputs-by-diff (profile entries generations)
  "Display a message for outputs searched by GENERATIONS difference."
  (let* ((count (length entries))
         (str-beg (guix-message-string-entries count 'output))
         (gen1 (car  generations))
         (gen2 (cadr generations)))
    (cl-multiple-value-bind (new old str-action)
        (if (> gen1 gen2)
            (list gen1 gen2 "added to")
          (list gen2 gen1 "removed from"))
      (message (concat "%s %s generation %d comparing with "
                       "generation %d of profile '%s'.")
               str-beg str-action new old profile))))

(defun guix-result-message (profile entries entry-type
                            search-type search-vals)
  "Display an appropriate message after displaying ENTRIES."
  (let* ((type-spec (guix-get-key-val guix-messages
                                      entry-type search-type))
         (fun-or-count-spec (car type-spec)))
    (if (functionp fun-or-count-spec)
        (funcall fun-or-count-spec profile entries search-vals)
      (let* ((count     (length entries))
             (count-key (if (> count 1) 'many count))
             (msg-spec  (guix-get-key-val type-spec count-key))
             (msg       (car msg-spec))
             (args      (cdr msg-spec)))
        (mapc (lambda (subst)
                (setq args (cl-substitute (cdr subst) (car subst) args)))
              `((count   . ,count)
                (val     . ,(car search-vals))
                (profile . ,profile)))
        (apply #'message msg args)))))

(provide 'guix-messages)

;;; guix-messages.el ends here
