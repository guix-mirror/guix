;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu installer newt final)
  #:use-module (gnu installer final)
  #:use-module (gnu installer parted)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer newt utils)
  #:use-module (guix i18n)
  #:use-module (guix colors)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module (newt)
  #:export (run-final-page))

(define* (strip-prefix file #:optional (prefix (%installer-target-dir)))
  "Strip PREFIX from FILE, if PREFIX actually is a prefix of FILE."
  (if (string-prefix? prefix file)
      (string-drop file (string-length prefix))
      file))

(define* (run-config-display-page #:key locale)
  (let ((width (max 70 (- (screen-columns) 20)))
        (height (default-listbox-height)))
    (run-file-textbox-page
     #:info-text (format #f (G_ "\
We're now ready to proceed with the installation! \
A system configuration file has been generated, it is displayed below.  \
This file will be available as '~a' on the installed system.  \
The new system will be created from this file once you've pressed OK.  \
This will take a few minutes.")
                         (strip-prefix (%installer-configuration-file)))
     #:title (G_ "Configuration file")
     #:file (%installer-configuration-file)
     #:edit-button? #t
     #:editor-locale locale
     #:info-textbox-width width
     #:file-textbox-width width
     #:file-textbox-height height
     #:exit-button-callback-procedure
     (lambda ()
       (abort-to-prompt 'installer-step 'abort)))))

(define (run-install-success-page)
  (match (current-clients)
    (()
     (message-window
      (G_ "Installation complete")
      (G_ "Reboot")
      (G_ "Congratulations!  Installation is now complete.  \
You may remove the device containing the installation image and \
press the button to reboot.")))
    (_
     ;; When there are clients connected, send them a message and keep going.
     (send-to-clients '(installation-complete))))

  ;; Return success so that the installer happily reboots.
  'success)

(define (run-install-failed-page)
  (match (current-clients)
    (()
     (match (choice-window
             (G_ "Installation failed")
             (G_ "Resume")
             (G_ "Restart the installer")
             (G_ "The final system installation step failed.  You can resume from \
a specific step, or restart the installer."))
       (1 (abort-to-prompt 'installer-step 'abort))
       (2
        ;; Keep going, the installer will be restarted later on.
        #t)))
    (_
     (send-to-clients '(installation-failure))
     #t)))

(define* (run-install-shell locale
                            #:key (users '()))
  (clear-screen)
  (newt-suspend)
  (let ((install-ok? (install-system locale #:users users)))
    (newt-resume)
    install-ok?))

(define (run-final-page result prev-steps)
  (define (wait-for-clients)
    (unless (null? (current-clients))
      (installer-log-line "waiting with clients before starting final step")
      (send-to-clients '(starting-final-step))
      (match (select (current-clients) '() '())
        (((port _ ...) _ _)
         (read-line port)))))

  ;; Before generating the configuration file, give clients a chance to do
  ;; things such as changing the swap partition label.
  (wait-for-clients)

  (installer-log-line "proceeding with final step")
  (let* ((configuration   (format-configuration prev-steps result))
         (user-partitions (result-step result 'partition))
         (locale          (result-step result 'locale))
         (users           (result-step result 'user))
         (install-ok?
          (with-mounted-partitions
           user-partitions
           (configuration->file configuration)
           (run-config-display-page #:locale locale)
           (run-install-shell locale #:users users))))
    (if install-ok?
        (run-install-success-page)
        (run-install-failed-page))))
