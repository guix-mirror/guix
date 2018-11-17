;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Julien Lepiller <julien@lepiller.eu>
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

(define-module (guix build ocaml-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            ocaml-build))

;; Commentary:
;;
;; Builder-side code of the standard ocaml build procedure.
;;
;; Code:

(define* (ocaml-findlib-environment #:key outputs #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out")))
    (setenv "OCAMLFIND_DESTDIR" (string-append out "/lib/ocaml/site-lib"))
    (setenv "OCAMLFIND_LDCONF" "ignore"))
  #t)

(define* (configure #:key outputs (configure-flags '())
                    (test-flags '("--enable-tests")) tests?
                    #:allow-other-keys)
  "Configure the given package."
  (let* ((out        (assoc-ref outputs "out")))
    (format #t "build directory: ~s~%" (getcwd))
    (if (file-exists? "setup.ml")
      (let ((args `("-configure"
                    "--prefix" ,out
                    ,@(if tests?
                          test-flags
                          '())
                    ,@configure-flags)))
        (format #t "running 'setup.ml' with arguments ~s~%" args)
        (apply invoke "ocaml" "setup.ml" args))
       (let ((args `("-prefix" ,out ,@configure-flags)))
        (format #t "running 'configure' with arguments ~s~%" args)
        (apply invoke "./configure" args))))
    #t)

(define* (build #:key inputs outputs (build-flags '()) (make-flags '())
                (use-make? #f) #:allow-other-keys)
  "Build the given package."
  (if (and (file-exists? "setup.ml") (not use-make?))
    (apply invoke "ocaml" "setup.ml" "-build" build-flags)
    (if (file-exists? "Makefile")
      (apply invoke "make" make-flags)
      (let ((file (if (file-exists? "pkg/pkg.ml") "pkg/pkg.ml" "pkg/build.ml")))
        (apply invoke "ocaml" "-I"
                      (string-append (assoc-ref inputs "findlib")
                                     "/lib/ocaml/site-lib")
                      file build-flags))))
  #t)

(define* (check #:key inputs outputs (make-flags '()) (test-target "test") tests?
                  (use-make? #f) #:allow-other-keys)
  "Install the given package."
  (when tests?
    (if (and (file-exists? "setup.ml") (not use-make?))
      (invoke "ocaml" "setup.ml" (string-append "-" test-target))
      (if (file-exists? "Makefile")
        (apply invoke "make" test-target make-flags)
        (let ((file (if (file-exists? "pkg/pkg.ml") "pkg/pkg.ml" "pkg/build.ml")))
          (invoke "ocaml" "-I"
                  (string-append (assoc-ref inputs "findlib")
                                 "/lib/ocaml/site-lib")
                  file test-target)))))
  #t)

(define* (install #:key outputs (build-flags '()) (make-flags '()) (use-make? #f)
                  (install-target "install")
                  #:allow-other-keys)
  "Install the given package."
  (let ((out (assoc-ref outputs "out")))
    (if (and (file-exists? "setup.ml") (not use-make?))
      (apply invoke "ocaml" "setup.ml"
             (string-append "-" install-target) build-flags)
      (if (file-exists? "Makefile")
        (apply invoke "make" install-target make-flags)
        (invoke "opam-installer" "-i" (string-append "--prefix=" out)
                (string-append "--libdir=" out "/lib/ocaml/site-lib")))))
  #t)

(define* (prepare-install #:key outputs #:allow-other-keys)
  "Prepare for building the given package."
  (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ocaml/site-lib"))
  (mkdir-p (string-append (assoc-ref outputs "out") "/bin"))
  #t)

(define %standard-phases
  ;; Everything is as with the GNU Build System except for the `configure'
  ;; , `build', `check' and `install' phases.
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (add-before 'configure 'ocaml-findlib-environment
                ocaml-findlib-environment)
    (add-before 'install 'prepare-install prepare-install)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))

(define* (ocaml-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; ocaml-build-system.scm ends here
