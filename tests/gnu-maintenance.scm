;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-gnu-maintenance)
  #:use-module (guix gnu-maintenance)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(test-begin "gnu-maintenance")

(test-assert "release-file?"
  (and (every (lambda (project+file)
                (apply release-file? project+file))
              '(("gcc" "gcc-5.3.0.tar.bz2")
                ("texmacs" "TeXmacs-1.0.7.9-src.tar.gz")
                ("icecat" "icecat-38.4.0-gnu1.tar.bz2")
                ("mit-scheme" "mit-scheme-9.2.tar.gz")))
       (every (lambda (project+file)
                (not (apply release-file? project+file)))
              '(("guile" "guile-www-1.1.1.tar.gz")
                ("guile" "guile-2.0.11.tar.gz.sig")
                ("mit-scheme" "mit-scheme-9.2-i386.tar.gz")
                ("mit-scheme" "mit-scheme-9.2-doc-pdf.tar.gz")
                ("gnutls" "gnutls-3.2.18-w32.zip")))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))
