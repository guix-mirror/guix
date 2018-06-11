;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
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

(define-module (guix build perl-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            perl-build))

;; Commentary:
;;
;; Builder-side code of the standard Perl package build procedure.
;;
;; Code:

(define* (configure #:key outputs make-maker?
                    (make-maker-flags '()) (module-build-flags '())
                    #:allow-other-keys)
  "Configure the given Perl package."
  (let* ((out (assoc-ref outputs "out"))
         (args (cond
                ;; Prefer to use Module::Build unless otherwise told
                ((and (file-exists? "Build.PL")
                      (not make-maker?))
                 `("Build.PL" ,(string-append "--prefix=" out)
                   "--installdirs=site" ,@module-build-flags))
                ((file-exists? "Makefile.PL")
                 `("Makefile.PL" ,(string-append "PREFIX=" out)
                   ;; Prevent installation of 'perllocal.pod' files for
                   ;; determinism.  These are typically used to build a
                   ;; catalogue of installed packages, but does not provide
                   ;; any useful information when installed with a module.
                   "INSTALLDIRS=site" "NO_PERLLOCAL=1" ,@make-maker-flags))
                (else (error "no Build.PL or Makefile.PL found")))))
    (format #t "running `perl' with arguments ~s~%" args)
    (apply invoke "perl" args)))

(define-syntax-rule (define-w/gnu-fallback* (name args ...) body ...)
  (define* (name args ... #:rest rest)
    (if (access? "Build" X_OK)
        (begin body ...)
        (apply (assoc-ref gnu:%standard-phases 'name) rest))))

(define-w/gnu-fallback* (build)
  (invoke "./Build"))

(define-w/gnu-fallback* (check #:key target
                               (tests? (not target)) (test-flags '())
                               #:allow-other-keys)
  (if tests?
      (apply invoke "./Build" "test" test-flags)
      (format #t "test suite not run~%"))
  #t)

(define-w/gnu-fallback* (install)
  (invoke "./Build" "install"))

(define %standard-phases
  ;; Everything is as with the GNU Build System except for the `configure',
  ;; `build', `check', and `install' phases.
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (replace 'install install)
    (replace 'check check)
    (replace 'build build)
    (replace 'configure configure)))

(define* (perl-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  "Build the given Perl package, applying all of PHASES in order."
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))

;;; perl-build-system.scm ends here
