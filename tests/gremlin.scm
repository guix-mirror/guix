;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2018 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module (guix build utils)
  #:use-module (guix build gremlin)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match))

(define %guile-executable
  (match (false-if-exception (readlink "/proc/self/exe"))
    ((? string? program)
     (and (file-exists? program) (elf-file? program)
          program))
    (_
     #f)))

(define read-elf
  (compose parse-elf get-bytevector-all))

(define c-compiler
  (or (which "gcc") (which "cc") (which "g++")))


(test-begin "gremlin")

(unless %guile-executable (test-skip 1))
(test-assert "elf-dynamic-info-needed, executable"
  (let* ((elf     (call-with-input-file %guile-executable read-elf))
         (dyninfo (elf-dynamic-info elf)))
    (or (not dyninfo)                             ;static executable
        (lset<= string=?
                (list (string-append "libguile-" (effective-version))
                      "libc")
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

(unless c-compiler
  (test-skip 1))
(test-equal "strip-runpath"
  "hello\n"
  (call-with-temporary-directory
   (lambda (directory)
     (with-directory-excursion directory
       (call-with-output-file "t.c"
         (lambda (port)
           (display "int main () { puts(\"hello\"); }" port)))
       (invoke c-compiler "t.c"
               "-Wl,--enable-new-dtags" "-Wl,-rpath=/foo" "-Wl,-rpath=/bar")
       (let* ((dyninfo (elf-dynamic-info
                        (parse-elf (call-with-input-file "a.out"
                                     get-bytevector-all))))
              (old     (elf-dynamic-info-runpath dyninfo))
              (new     (strip-runpath "a.out"))
              (new*    (strip-runpath "a.out")))
         (validate-needed-in-runpath "a.out")
         (and (member "/foo" old) (member "/bar" old)
              (not (member "/foo" new))
              (not (member "/bar" new))
              (equal? new* new)
              (let* ((pipe (open-input-pipe "./a.out"))
                     (str  (get-string-all pipe)))
                (close-pipe pipe)
                str)))))))

(test-end "gremlin")
