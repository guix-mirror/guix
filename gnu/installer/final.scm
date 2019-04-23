;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer final)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer utils)
  #:use-module (gnu services herd)
  #:use-module (guix build utils)
  #:export (install-system))

(define (install-system locale)
  "Start COW-STORE service on target directory and launch guix install command
in a subshell.  LOCALE must be the locale name under which that command will
run, or #f."
  (let ((install-command
         (format #f "guix system init ~a ~a"
                 (%installer-configuration-file)
                 (%installer-target-dir))))
    (mkdir-p (%installer-target-dir))
    (start-service 'cow-store (list (%installer-target-dir)))
    (false-if-exception (run-shell-command install-command
                                           #:locale locale))))
