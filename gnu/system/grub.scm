;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system grub)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (menu-entry
            menu-entry?
            grub-configuration-file))

;;; Commentary:
;;;
;;; Configuration of GNU GRUB.
;;;
;;; Code:

(define-record-type* <menu-entry>
  menu-entry make-menu-entry
  menu-entry?
  (label           menu-entry-label)
  (linux           menu-entry-linux)
  (linux-arguments menu-entry-linux-arguments
                   (default '()))
  (initrd          menu-entry-initrd))

(define* (grub-configuration-file store entries
                                  #:key (default-entry 1) (timeout 5)
                                  (system (%current-system)))
  "Return the GRUB configuration file in STORE for ENTRIES, a list of
<menu-entry> objects, defaulting to DEFAULT-ENTRY and with the given TIMEOUT."
  (define prologue
    (format #f "
set default=~a
set timeout=~a
search.file ~a~%"
            default-entry timeout
            (any (match-lambda
                  (($ <menu-entry> _ linux)
                   (let* ((drv (package-derivation store linux system))
                          (out (derivation->output-path drv)))
                     (string-append out "/bzImage"))))
                 entries)))

  (define entry->text
    (match-lambda
     (($ <menu-entry> label linux arguments initrd)
      (let ((linux-drv  (package-derivation store linux system))
            (initrd-drv (package-derivation store initrd system)))
        ;; XXX: Assume that INITRD is a directory containing an 'initrd' file.
        (format #f "menuentry ~s {
  linux ~a/bzImage ~a
  initrd ~a/initrd
}~%"
                label
                (derivation->output-path linux-drv)
                (string-join arguments)
                (derivation->output-path initrd-drv))))))

  (add-text-to-store store "grub.cfg"
                     (string-append prologue
                                    (string-concatenate
                                     (map entry->text entries)))
                     '()))

;;; grub.scm ends here
