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
  #:export (grub-configuration
            grub-configuration?
            grub-configuration-device

            menu-entry
            menu-entry?

            grub-configuration-file))

;;; Commentary:
;;;
;;; Configuration of GNU GRUB.
;;;
;;; Code:

(define-record-type* <grub-configuration>
  grub-configuration make-grub-configuration
  grub-configuration?
  (grub            grub-configuration-grub           ; package
                   (default (@ (gnu packages grub) grub)))
  (device          grub-configuration-device)        ; string
  (menu-entries    grub-configuration-menu-entries   ; list
                   (default '()))
  (default-entry   grub-configuration-default-entry  ; integer
                   (default 0))
  (timeout         grub-configuration-timeout        ; integer
                   (default 5)))

(define-record-type* <menu-entry>
  menu-entry make-menu-entry
  menu-entry?
  (label           menu-entry-label)
  (linux           menu-entry-linux)
  (linux-arguments menu-entry-linux-arguments
                   (default '()))          ; list of string-valued gexps
  (initrd          menu-entry-initrd))     ; file name of the initrd as a gexp

(define* (grub-configuration-file config entries
                                  #:key
                                  (system (%current-system))
                                  (old-entries '()))
  "Return the GRUB configuration file corresponding to CONFIG, a
<grub-configuration> object.  OLD-ENTRIES is taken to be a list of menu
entries corresponding to old generations of the system."
  (define all-entries
    (append entries (grub-configuration-menu-entries config)))

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
                  #$(grub-configuration-default-entry config)
                  #$(grub-configuration-timeout config)
                  #$(any (match-lambda
                          (($ <menu-entry> _ linux)
                           linux))
                         all-entries))
          #$@(map entry->gexp all-entries)

          #$@(if (pair? old-entries)
                 #~((format port "
submenu \"GNU system, old configurations...\" {~%")
                    #$@(map entry->gexp old-entries)
                    (format port "}~%"))
                 #~()))))

  (gexp->derivation "grub.cfg" builder))

;;; grub.scm ends here
