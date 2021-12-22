;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2018, 2020, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
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
  #:use-module (guix tests)
  #:use-module ((guix utils) #:select (call-with-temporary-directory
                                       target-aarch64?))
  #:use-module (guix build utils)
  #:use-module (guix build gremlin)
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
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

(unless (and %guile-executable (not (getenv "LD_LIBRARY_PATH"))
             (file-needed %guile-executable) ;statically linked?
             ;; When Guix has been built on a foreign distro, using a
             ;; toolchain and libraries from that foreign distro, it is not
             ;; unusual for the runpath to be empty.
             (pair? (file-runpath %guile-executable)))
  (test-skip 1))
(test-assert "file-needed/recursive"
  (let* ((needed (file-needed/recursive %guile-executable))
         (pipe   (dynamic-wind
                   (lambda ()
                     ;; Tell ld.so to list loaded objects, like 'ldd' does.
                     (setenv "LD_TRACE_LOADED_OBJECTS" "yup"))
                   (lambda ()
                     (open-pipe* OPEN_READ %guile-executable))
                   (lambda ()
                     (unsetenv "LD_TRACE_LOADED_OBJECTS")))))
    (define ldd-rx
      (make-regexp "^[[:blank:]]+([[:graph:]]+ => )?([[:graph:]]+) .*$"))

    (define (read-ldd-output port)
      ;; Read from PORT output in GNU ldd format.
      (let loop ((result '()))
        (match (read-line port)
          ((? eof-object?)
           (reverse result))
          ((= (cut regexp-exec ldd-rx <>) m)
           (if m
               (loop (cons (match:substring m 2) result))
               (loop result))))))
    (define ground-truth
      (remove (lambda (entry)
                ;; See vdso(7) for the list of vDSO names across
                ;; architectures.
                (or (string-prefix? "linux-vdso.so" entry)
                    (string-prefix? "linux-vdso32.so" entry) ;32-bit powerpc
                    (string-prefix? "linux-vdso64.so" entry) ;64-bit powerpc
                    (string-prefix? "linux-gate.so" entry)   ;i386
                    ;; FIXME: ELF files on aarch64 do not always include a
                    ;; NEEDED entry for the dynamic linker, and it is unclear
                    ;; if that is OK.  See: https://issues.guix.gnu.org/52943
                    (and (target-aarch64?)
                         (string-contains entry (glibc-dynamic-linker)))))
              (read-ldd-output pipe)))

    (and (zero? (close-pipe pipe))
         ;; It's OK if file-needed/recursive returns multiple entries that are
         ;; different strings referring to the same file.  This appears to be a
         ;; benign edge case.  See: https://issues.guix.gnu.org/52940
         (lset= file=? (pk 'truth ground-truth) (pk 'needed needed)))))

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

(unless c-compiler
  (test-skip 1))
(test-equal "set-file-runpath + file-runpath"
  "hello\n"
  (call-with-temporary-directory
   (lambda (directory)
     (with-directory-excursion directory
       (call-with-output-file "t.c"
         (lambda (port)
           (display "int main () { puts(\"hello\"); }" port)))

       (invoke c-compiler "t.c"
               "-Wl,--enable-new-dtags" "-Wl,-rpath=/xxxxxxxxx")

       (let ((original-runpath (file-runpath "a.out")))
         (and (member "/xxxxxxxxx" original-runpath)
              (guard (c ((runpath-too-long-error? c)
                         (string=? "a.out" (runpath-too-long-error-file c))))
                (set-file-runpath "a.out" (list (make-string 777 #\y))))
              (let ((runpath (delete "/xxxxxxxxx" original-runpath)))
                (set-file-runpath "a.out" runpath)
                (equal? runpath (file-runpath "a.out")))
              (let* ((pipe (open-input-pipe "./a.out"))
                     (str  (get-string-all pipe)))
                (close-pipe pipe)
                str)))))))

(unless c-compiler
  (test-skip 1))
(test-equal "elf-dynamic-info-soname"
  "libfoo.so.2"
  (call-with-temporary-directory
   (lambda (directory)
     (with-directory-excursion directory
       (call-with-output-file "t.c"
         (lambda (port)
           (display "// empty file" port)))
       (invoke c-compiler "t.c"
               "-shared" "-Wl,-soname,libfoo.so.2")
       (let* ((dyninfo (elf-dynamic-info
                       (parse-elf (call-with-input-file "a.out"
                                    get-bytevector-all))))
              (soname  (elf-dynamic-info-soname dyninfo)))
	 soname)))))

(test-end "gremlin")
