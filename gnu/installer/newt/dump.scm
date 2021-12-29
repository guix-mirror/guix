;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (gnu installer newt dump)
  #:use-module (gnu installer dump)
  #:use-module (gnu installer newt page)
  #:use-module (guix i18n)
  #:use-module (newt)
  #:export (run-dump-page))

(define (run-dump-page dump)
  "Run a dump page, proposing the user to upload the crash dump to Guix
servers."
  (case (choice-window
         (G_ "Crash dump upload")
         (G_ "Yes")
         (G_ "No")
         (G_ "The installer failed.  Do you accept to upload the crash dump \
to Guix servers, so that we can investigate the issue?"))
    ((1) (send-dump-report dump))
    ((2) #f)))
