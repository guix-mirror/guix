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

(define-module (guix build perl-build-system)
  #:use-module ((guix build gnu-build-system)
                #:renamer (symbol-prefix-proc 'gnu:))
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            perl-build))

;; Commentary:
;;
;; Builder-side code of the standard Perl package build procedure.
;;
;; Code:

(define* (configure #:key outputs (make-maker-flags '())
                    #:allow-other-keys)
  "Configure the given Perl package."
  (let ((out (assoc-ref outputs "out")))
    (if (file-exists? "Makefile.PL")
        (let ((args `("Makefile.PL" ,(string-append "PREFIX=" out)
                      "INSTALLDIRS=site" ,@make-maker-flags)))
          (format #t "running `perl' with arguments ~s~%" args)
          (zero? (apply system* "perl" args)))
        (error "no Makefile.PL found"))))

(define %standard-phases
  ;; Everything is as with the GNU Build System except for the `configure'
  ;; phase.
  (alist-replace 'configure configure
                 gnu:%standard-phases))

(define* (perl-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  "Build the given Perl package, applying all of PHASES in order."
  (set-path-environment-variable "PERL5LIB" '("lib/perl5/site_perl")
                                 (match inputs
                                   (((_ . path) ...)
                                    path)))
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))

;;; perl-build-system.scm ends here
