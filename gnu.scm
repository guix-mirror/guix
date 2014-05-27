;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu))

;;; Commentary:
;;;
;;; This composite module re-exports core parts the (gnu …) public modules.
;;;
;;; Code:

(eval-when (eval load compile)
  (begin
    (define %public-modules
      '((gnu system)
        (gnu system file-systems)
        (gnu system grub)                         ; 'grub-configuration'
        (gnu system linux)                        ; 'base-pam-services'
        (gnu system shadow)                       ; 'user-account'
        (gnu system linux-initrd)
        (gnu services)
        (gnu services base)
        (gnu packages)
        (gnu packages base)))

    (for-each (let ((i (module-public-interface (current-module))))
                (lambda (m)
                  (module-use! i (resolve-interface m))))
              %public-modules)))

;;; gnu.scm ends here
