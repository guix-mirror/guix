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

(define-module (gnu packages cflow)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages emacs))

(define-public cflow
  (package
    (name "cflow")
    (version "1.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/cflow/cflow-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1jkbq97ajcf834z68hbn3xfhiz921zhn39gklml1racf0kb3jzh3"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/cflow/")
    (synopsis "Create a graph of control flow within a program")
    (description
     "cflow analyzes C source files and produces a graph charting the
control flow of the program.  It can output the graph in several styles and
in either the POSIX format or in an extended GNU format. cflow also includes
a major mode for Emacs for examining the flowcharts that it produces.")
   (license gpl3+)))
