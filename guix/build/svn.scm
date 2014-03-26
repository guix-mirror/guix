;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
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

(define-module (guix build svn)
  #:use-module (guix build utils)
  #:export (svn-fetch))

;;; Commentary:
;;;
;;; This is the build-side support code of (guix svn-download).  It allows a
;;; Subversion repository to be cloned and checked out at a specific revision.
;;;
;;; Code:

(define* (svn-fetch url revision directory
                    #:key (svn-command "svn"))
  "Fetch REVISION from URL into DIRECTORY.  REVISION must be an integer, and a
valid Subversion revision.  Return #t on success, #f otherwise."
  (and (zero? (system* svn-command "checkout" "--non-interactive"
                       ;; Trust the server certificate.  This is OK as we
                       ;; verify the checksum later.  This can be removed when
                       ;; ca-certificates package is added.
                       "--trust-server-cert" "-r" (number->string revision)
                       url directory))
       (with-directory-excursion directory
         (begin
           ;; The contents of '.svn' vary as a function of the current status
           ;; of the repo.  Since we want a fixed output, this directory needs
           ;; to be taken out.
           (delete-file-recursively ".svn")
           #t))))

;;; svn.scm ends here
