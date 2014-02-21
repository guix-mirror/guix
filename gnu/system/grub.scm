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

(define-module (gnu system grub)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix records)
  #:use-module (guix monads)
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
  (initrd          menu-entry-initrd))            ; file name of the initrd

(define* (grub-configuration-file entries
                                  #:key (default-entry 1) (timeout 5)
                                  (system (%current-system)))
  "Return the GRUB configuration file for ENTRIES, a list of
<menu-entry> objects, defaulting to DEFAULT-ENTRY and with the given TIMEOUT."
  (define (prologue kernel)
    (format #f "
set default=~a
set timeout=~a
search.file ~a~%"
            default-entry timeout kernel))

  (define (bzImage)
    (any (match-lambda
          (($ <menu-entry> _ linux)
           (package-file linux "bzImage"
                         #:system system)))
         entries))

  (define entry->text
    (match-lambda
     (($ <menu-entry> label linux arguments initrd)
      (mlet %store-monad ((linux  (package-file linux "bzImage"
                                                #:system system)))
        (return (format #f "menuentry ~s {
  linux ~a ~a
  initrd ~a
}~%"
                        label
                        linux (string-join arguments) initrd))))))

  (mlet %store-monad ((kernel (bzImage))
                      (body   (sequence %store-monad
                                        (map entry->text entries))))
    (text-file "grub.cfg"
               (string-append (prologue kernel)
                              (string-concatenate body)))))

;;; grub.scm ends here
