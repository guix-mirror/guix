;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix store ssh)
  #:use-module (guix ssh)
  #:use-module (web uri)
  #:export (connect-to-daemon))

;;; Commentary:
;;;
;;; This modules provides the entry point for 'open-connection' in (guix
;;; store).  Passing an 'ssh://' URI to 'open-connection' triggers the use of
;;; the code in this module.
;;;
;;; End:

(define (connect-to-daemon uri)
  "Connect to the SSH daemon at URI, a URI object with the 'ssh' scheme."
  (remote-daemon-channel
   (open-ssh-session (uri-host uri)
                     #:port (or (uri-port uri) 22)
                     #:user (uri-userinfo uri))))

;;; ssh.scm ends here
