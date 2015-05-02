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

(define-module (test-gremlin)
  #:use-module (guix elf)
  #:use-module (guix build utils)
  #:use-module (guix build gremlin)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match))

(define %guile-executable
  (match (command-line)
    ((program . _)
     (and (file-exists? program) (elf-file? program)
          program))
    (_
     #f)))

(define read-elf
  (compose parse-elf get-bytevector-all))


(test-begin "gremlin")

(unless %guile-executable (test-skip 1))
(test-assert "elf-dynamic-info-needed, executable"
  (let* ((elf     (call-with-input-file %guile-executable read-elf))
         (dyninfo (elf-dynamic-info elf)))
    (or (not dyninfo)                             ;static executable
        (lset<= string=?
                (list (string-append "libguile-" (effective-version))
                      "libgc" "libunistring" "libffi")
                (map (lambda (lib)
                       (string-take lib (string-contains lib ".so")))
                     (elf-dynamic-info-needed dyninfo))))))

(test-equal "expand-origin"
  '("OOO/../lib"
    "OOO"
    "../OOO/bar/OOO/baz"
    "ORIGIN/foo")
  (map (cut expand-origin <> "OOO")
       '("$ORIGIN/../lib"
         "${ORIGIN}"
         "../${ORIGIN}/bar/$ORIGIN/baz"
         "ORIGIN/foo")))

(test-end "gremlin")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
