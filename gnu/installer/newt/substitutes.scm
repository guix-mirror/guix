;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (gnu installer newt substitutes)
  #:use-module (gnu installer substitutes)
  #:use-module (gnu installer utils)
  #:use-module (guix i18n)
  #:use-module (newt)
  #:use-module (ice-9 match)
  #:export (run-substitutes-page))

(define* (run-substitutes-page)
  (match (current-clients)
    (()
     (case (choice-window
            (G_ "Substitute server discovery.")
            (G_ "Enable") (G_ "Disable")
            (G_ " By turning this option on, you allow Guix to fetch \
substitutes (pre-built binaries) during installation from servers \
discovered on your local area network (LAN) in addition to the official \
server.  This can increase download throughput.

 There are no security risks: only genuine substitutes may be retrieved from \
those servers.  However, eavesdroppers on your LAN may be able to see what \
software you are installing."))
       ((1) (enable-discovery))
       ((2) (disable-discovery))))
    (_ #f)))
