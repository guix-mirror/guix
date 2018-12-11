;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-debug-link)
  #:use-module (guix elf)
  #:use-module (guix build utils)
  #:use-module (guix build debug-link)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix tests)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
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


(test-begin "debug-link")

(unless %guile-executable (test-skip 1))
(test-assert "elf-debuglink"
  (let ((elf (call-with-input-file %guile-executable read-elf)))
    (match (call-with-values (lambda () (elf-debuglink elf)) list)
      ((#f #f)                                    ;no '.gnu_debuglink' section
       (pk 'no-debuglink #t))
      (((? string? file) (? integer? crc))
       (string-suffix? ".debug" file)))))

;; Since we need %BOOTSTRAP-GCC and co., we have to skip the following tests
;; when networking is unreachable because we'd fail to download it.
(unless (network-reachable?) (test-skip 1))
(test-assertm "elf-debuglink"
  ;; Check whether we can compute the CRC just like objcopy, and whether we
  ;; can retrieve it.
  (let* ((code (plain-file "test.c" "int main () { return 42; }"))
         (exp  (with-imported-modules '((guix build utils)
                                        (guix build debug-link)
                                        (guix elf))
                 #~(begin
                     (use-modules (guix build utils)
                                  (guix build debug-link)
                                  (guix elf)
                                  (rnrs io ports))

                     (define read-elf
                       (compose parse-elf get-bytevector-all))

                     (setenv "PATH" (string-join '(#$%bootstrap-gcc
                                                   #$%bootstrap-binutils)
                                                 "/bin:" 'suffix))
                     (invoke "gcc" "-O0" "-g" #$code "-o" "exe")
                     (copy-file "exe" "exe.debug")
                     (invoke "strip" "--only-keep-debug" "exe.debug")
                     (invoke "strip" "--strip-debug" "exe")
                     (invoke "objcopy" "--add-gnu-debuglink=exe.debug"
                             "exe")
                     (call-with-values (lambda ()
                                         (elf-debuglink
                                          (call-with-input-file "exe"
                                            read-elf)))
                       (lambda (file crc)
                         (call-with-output-file #$output
                           (lambda (port)
                             (let ((expected (call-with-input-file "exe.debug"
                                               debuglink-crc32)))
                               (write (list file (= crc expected))
                                      port))))))))))
    (mlet* %store-monad ((drv (gexp->derivation "debuglink" exp))
                         (x   (built-derivations (list drv))))
      (call-with-input-file (derivation->output-path drv)
        (lambda (port)
          (return (match (read port)
                    (("exe.debug" #t) #t)
                    (x                (pk 'fail x #f)))))))))

(unless (network-reachable?) (test-skip 1))
(test-assertm "set-debuglink-crc"
  ;; Check whether 'set-debuglink-crc' successfully updates the CRC.
  (let* ((code  (plain-file "test.c" "int main () { return 42; }"))
         (debug (plain-file "exe.debug" "a"))
         (exp   (with-imported-modules '((guix build utils)
                                         (guix build debug-link)
                                         (guix elf))
                  #~(begin
                      (use-modules (guix build utils)
                                   (guix build debug-link)
                                   (guix elf)
                                   (rnrs io ports))

                      (define read-elf
                        (compose parse-elf get-bytevector-all))

                      (setenv "PATH" (string-join '(#$%bootstrap-gcc
                                                    #$%bootstrap-binutils)
                                                  "/bin:" 'suffix))
                      (invoke "gcc" "-O0" "-g" #$code "-o" "exe")
                      (copy-file "exe" "exe.debug")
                      (invoke "strip" "--only-keep-debug" "exe.debug")
                      (invoke "strip" "--strip-debug" "exe")
                      (invoke "objcopy" "--add-gnu-debuglink=exe.debug"
                              "exe")
                      (set-debuglink-crc "exe" #$debug)
                      (call-with-values (lambda ()
                                          (elf-debuglink
                                           (call-with-input-file "exe"
                                             read-elf)))
                        (lambda (file crc)
                          (call-with-output-file #$output
                            (lambda (port)
                              (write (list file crc) port)))))))))
    (mlet* %store-monad ((drv (gexp->derivation "debuglink" exp))
                         (x   (built-derivations (list drv))))
      (call-with-input-file (derivation->output-path drv)
        (lambda (port)
          (return (match (read port)
                    (("exe.debug" crc)
                     (= crc (debuglink-crc32 (open-input-string "a"))))
                    (x
                     (pk 'fail x #f)))))))))

(test-end "debug-link")
