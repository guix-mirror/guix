;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages rush)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages))

(define-public rush
  (package
    (name "rush")
    (version "1.7")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://gnu/rush/rush-"
                   version
                   ".tar.gz"))
             (sha256
              (base32
               "0fh0gbbp0iiq3wbkf503xb40r8ljk42vyj9bnlflbz82d6ipy1rm"))
             (patches (list (search-patch "cpio-gets-undeclared.patch")))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/rush/")
    (synopsis "Restricted user (login) shell")
    (description
     "GNU Rush is a Restricted User Shell, designed for sites providing
limited remote access to their resources, such as svn or git repositories,
scp, or the like.  Using a sophisticated configuration file, Rush gives you
complete control over the command lines that users execute, as well as over
the usage of system resources, such as virtual memory, CPU time, etc.

In particular, it allows remote programs to be run in a chrooted environment,
which is important with such programs as sftp-server or scp, that lack this
ability.")
    (license gpl3+)))
