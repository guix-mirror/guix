;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build activation)
  #:use-module (ice-9 ftw)
  #:export (activate-etc))

;;; Commentary:
;;;
;;; This module provides "activation" helpers.  Activation is the process that
;;; consists in setting up system-wide files and directories so that an
;;; 'operating-system' configuration becomes active.
;;;
;;; Code:

(define (activate-etc etc)
  "Install ETC, a directory in the store, as the source of static files for
/etc."

  ;; /etc is a mixture of static and dynamic settings.  Here is where we
  ;; initialize it from the static part.

  (format #t "populating /etc from ~a...~%" etc)
  (let ((rm-f (lambda (f)
                (false-if-exception (delete-file f)))))
    (rm-f "/etc/static")
    (symlink etc "/etc/static")
    (for-each (lambda (file)
                ;; TODO: Handle 'shadow' specially so that changed
                ;; password aren't lost.
                (let ((target (string-append "/etc/" file))
                      (source (string-append "/etc/static/" file)))
                  (rm-f target)
                  (symlink source target)))
              (scandir etc
                       (lambda (file)
                         (not (member file '("." ".."))))

                       ;; The default is 'string-locale<?', but we don't have
                       ;; it when run from the initrd's statically-linked
                       ;; Guile.
                       string<?))

    ;; Prevent ETC from being GC'd.
    (rm-f "/var/guix/gcroots/etc-directory")
    (symlink etc "/var/guix/gcroots/etc-directory")))

;;; activation.scm ends here
