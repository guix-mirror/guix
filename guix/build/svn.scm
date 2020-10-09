;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Simon Tournier <zimon.toutoune@gmail.com>
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
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 format)
  #:export (svn-fetch))

;;; Commentary:
;;;
;;; This is the build-side support code of (guix svn-download).  It allows a
;;; Subversion repository to be cloned and checked out at a specific revision.
;;;
;;; Code:

(define* (svn-fetch url revision directory
                    #:key (svn-command "svn")
                    (recursive? #t)
                    (user-name #f)
                    (password #f))
  "Fetch REVISION from URL into DIRECTORY.  REVISION must be an integer, and a
valid Subversion revision.  Return #t on success, #f otherwise."
  (guard (c ((invoke-error? c)
             (report-invoke-error c)
             #f))
    (apply invoke svn-command
           "export" "--non-interactive"
           ;; Trust the server certificate.  This is OK as we
           ;; verify the checksum later.  This can be removed when
           ;; ca-certificates package is added.
           "--trust-server-cert" "-r" (number->string revision)
           `(,@(if (and user-name password)
                   (list (string-append "--username=" user-name)
                         (string-append "--password=" password))
                   '())
             ,@(if recursive?
                   '()
                   (list "--ignore-externals"))
             ,url ,directory))
    #t))

;;; svn.scm ends here
