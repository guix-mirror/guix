;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (newt)
  #:export (run-final-page))

(define* (strip-prefix file #:optional (prefix (%installer-target-dir)))
  "Strip PREFIX from FILE, if PREFIX actually is a prefix of FILE."
  (if (string-prefix? prefix file)
      (string-drop file (string-length prefix))
      file))

(define (run-config-display-page)
  (let ((width (%configuration-file-width))
        (height (nearest-exact-integer
                 (/ (screen-rows) 2))))
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
     #:info-textbox-width width
     #:file-textbox-width width
     #:file-textbox-height height
     #:exit-button-callback-procedure
     (lambda ()
       (raise
        (condition
         (&installer-step-abort)))))))

(define (run-install-success-page)
  (message-window
   (G_ "Installation complete")
   (G_ "Reboot")
   (G_ "Congratulations!  Installation is now complete.  \
You may remove the device containing the installation image and \
press the button to reboot."))

  ;; Return success so that the installer happily reboots.
  'success)

(define (run-install-failed-page)
  (choice-window
   (G_ "Installation failed")
   (G_ "Restart installer")
   (G_ "Retry system install")
   (G_ "The final system installation step failed.  You can retry the \
last step, or restart the installer.")))

(define* (run-install-shell locale
                            #:key (users '()))
  (clear-screen)
  (newt-suspend)
  (let ((install-ok? (install-system locale #:users users)))
    (newt-resume)
    install-ok?))

(define (run-final-page result prev-steps)
  (let* ((configuration   (format-configuration prev-steps result))
         (user-partitions (result-step result 'partition))
         (locale          (result-step result 'locale))
         (users           (result-step result 'user))
         (install-ok?
          (with-mounted-partitions
           user-partitions
           (configuration->file configuration)
           (run-config-display-page)
           (run-install-shell locale #:users users))))
    (if install-ok?
        (run-install-success-page)
        (run-install-failed-page))))
