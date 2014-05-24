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

(define-module (gnu services)
  #:use-module (guix records)
  #:export (service?
            service
            service-documentation
            service-provision
            service-requirement
            service-respawn?
            service-start
            service-stop
            service-activate
            service-user-accounts
            service-user-groups
            service-pam-services))

;;; Commentary:
;;;
;;; System services as cajoled by dmd.
;;;
;;; Code:

(define-record-type* <service>
  service make-service
  service?
  (documentation service-documentation            ; string
                 (default "[No documentation.]"))
  (provision     service-provision)               ; list of symbols
  (requirement   service-requirement              ; list of symbols
                 (default '()))
  (respawn?      service-respawn?                 ; Boolean
                 (default #t))
  (start         service-start)                   ; g-expression
  (stop          service-stop                     ; g-expression
                 (default #f))
  (user-accounts service-user-accounts            ; list of <user-account>
                 (default '()))
  (user-groups   service-user-groups              ; list of <user-groups>
                 (default '()))
  (pam-services  service-pam-services             ; list of <pam-service>
                 (default '()))
  (activate      service-activate                 ; gexp
                 (default #f)))

;;; services.scm ends here.
