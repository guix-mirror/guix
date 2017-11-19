;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix i18n)
  #:use-module (srfi srfi-26)
  #:export (G_
            N_
            P_
            %gettext-domain
            %package-text-domain))

;;; Commentary:
;;;
;;; Internationalization support.
;;;
;;; Code:

(define %gettext-domain
  ;; Text domain for strings used in the tools.
  "guix")

(define %package-text-domain
  ;; Text domain for package synopses and descriptions.
  "guix-packages")

(define G_ (cut gettext <> %gettext-domain))
(define N_ (cut ngettext <> <> <> %gettext-domain))

(define (P_ msgid)
  "Return the translation of the package description or synopsis MSGID."
  ;; Descriptions/synopses might occasionally be empty strings, even if that
  ;; is something we try to avoid.  Since (gettext "") can return a non-empty
  ;; string, explicitly check for that case.
  (if (string-null? msgid)
      msgid
      (gettext msgid %package-text-domain)))
