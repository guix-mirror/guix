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
  #:use-module (guix gexp)
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
                   (default '()))          ; list of string-valued gexps
  (initrd          menu-entry-initrd))     ; file name of the initrd as a gexp

(define* (grub-configuration-file entries
                                  #:key (default-entry 1) (timeout 5)
                                  (system (%current-system)))
  "Return the GRUB configuration file for ENTRIES, a list of
<menu-entry> objects, defaulting to DEFAULT-ENTRY and with the given TIMEOUT."
  (define entry->gexp
    (match-lambda
     (($ <menu-entry> label linux arguments initrd)
      #~(format port "menuentry ~s {
  linux ~a/bzImage ~a
  initrd ~a
}~%"
                #$label
                #$linux (string-join (list #$@arguments))
                #$initrd))))

  (define builder
    #~(call-with-output-file #$output
        (lambda (port)
          (format port "
set default=~a
set timeout=~a
search.file ~a/bzImage~%"
                  #$default-entry #$timeout
                  #$(any (match-lambda
                          (($ <menu-entry> _ linux)
                           linux))
                         entries))
          #$@(map entry->gexp entries))))

  (gexp->derivation "grub.cfg" builder))

;;; grub.scm ends here
