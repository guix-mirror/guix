;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix build bzr)
  #:use-module (guix build utils)
  #:export (bzr-fetch))

;;; Commentary:
;;;
;;; This is the build-side support code of (guix bzr-download).  It allows a
;;; Bazaar repository to be branched at a specific revision.
;;;
;;; Code:

(define* (bzr-fetch url revision directory
                    #:key (bzr-command "bzr"))
  "Fetch REVISION from URL into DIRECTORY.  REVISION must be a valid Bazaar
revision identifier.  Return #t on success, else throw an exception."
  ;; Do not attempt to write .bzr.log to $HOME, which doesn't exist.
  (setenv "BZR_LOG" "/dev/null")
  ;; Disable SSL certificate verification; we rely on the hash instead.
  (invoke bzr-command "-Ossl.cert_reqs=none" "checkout"
          "--lightweight" "-r" revision url directory)
  (with-directory-excursion directory
    (begin
      (delete-file-recursively ".bzr")
      #t)))

;;; bzr.scm ends here
