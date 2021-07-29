;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
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

(define-module (gnu system setuid)
  #:use-module (guix records)
  #:export (setuid-program
            setuid-program?
            setuid-program-program
            setuid-program-setuid?
            setuid-program-setgid?
            setuid-program-user
            setuid-program-group

            file-like->setuid-program))

;;; Commentary:
;;;
;;; Data structures representing setuid/setgid programs.  This is meant to be
;;; used both on the host side and at run time--e.g., in activation snippets.
;;;
;;; Code:

(define-record-type* <setuid-program>
  setuid-program make-setuid-program
  setuid-program?
  ;; Path to program to link with setuid permissions
  (program       setuid-program-program) ;file-like
  ;; Whether to set user setuid bit
  (setuid?       setuid-program-setuid? ;boolean
                 (default #t))
  ;; Whether to set group setgid bit
  (setgid?       setuid-program-setgid? ;boolean
                 (default #f))
  ;; The user this should be set to (defaults to root)
  (user          setuid-program-user    ;integer or string
                 (default 0))
  ;; Group we want to set this to (defaults to root)
  (group         setuid-program-group   ;integer or string
                 (default 0)))

(define (file-like->setuid-program program)
  (setuid-program (program program)))
