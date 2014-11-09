;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (build-self)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (srfi srfi-1)
  #:export (build))

;;; Commentary:
;;;
;;; When loaded, this module returns a monadic procedure of at least one
;;; argument: the source tree to build.  It returns a derivation that
;;; builds it.
;;;
;;; This file uses modules provided by the already-installed Guix.  Those
;;; modules may be arbitrarily old compared to the version we want to
;;; build.  Because of that, it must rely on the smallest set of features
;;; that are likely to be provided by the (guix) and (gnu) modules, and by
;;; Guile itself, forever and ever.
;;;
;;; Code:


;; The dependencies.  Don't refer explicitly to the variables because they
;; could be renamed or shuffled around in modules over time.  Conversely,
;; 'find-best-packages-by-name' is expected to always have the same semantics.

(define libgcrypt
  (first (find-best-packages-by-name "libgcrypt" #f)))

(define guile-json
  (first (find-best-packages-by-name "guile-json" #f)))



;; The actual build procedure.

(define (top-source-directory)
  "Return the name of the top-level directory of this source tree."
  (and=> (assoc-ref (current-source-location) 'filename)
         (lambda (file)
           (string-append (dirname file) "/.."))))

;; The procedure below is our return value.
(define* (build source #:key verbose?
                #:allow-other-keys
                #:rest rest)
  "Return a derivation that unpacks SOURCE into STORE and compiles Scheme
files."
  (define builder
    #~(begin
        (use-modules (guix build pull))

        (let ((json (string-append #$guile-json "/share/guile/site/2.0")))
          (set! %load-path (cons json %load-path))
          (set! %load-compiled-path (cons json %load-compiled-path)))

        (build-guix #$output #$source

                    ;; XXX: This is not perfect, enabling VERBOSE? means
                    ;; building a different derivation.
                    #:debug-port (if #$verbose?
                                     (current-error-port)
                                     (%make-void-port "w"))
                    #:gcrypt #$libgcrypt)))

  (gexp->derivation "guix-latest" builder
                    #:modules '((guix build pull)
                                (guix build utils))

                    ;; Arrange so that our own (guix build …) modules are
                    ;; used.
                    #:module-path (list (top-source-directory))))

;; This file is loaded by 'guix pull'; return it the build procedure.
build

;; Local Variables:
;; eval: (put 'with-load-path 'scheme-indent-function 1)
;; End:

;;; build-self.scm ends here
