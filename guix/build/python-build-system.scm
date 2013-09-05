;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (guix build python-build-system)
  #:use-module ((guix build gnu-build-system)
                #:renamer (symbol-prefix-proc 'gnu:))
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            python-build))

;; Commentary:
;;
;; Builder-side code of the standard Python package build procedure.
;;
;; Code:


(define (call-setuppy command params)
  (if (file-exists? "setup.py")
      (begin
         (format #t "running \"python setup.py\" with command ~s and parameters ~s~%"
                command params)
         (zero? (apply system* "python" "setup.py" command params)))
      (error "no setup.py found")))

(define* (build #:rest empty)
  "Build a given Python package."
  (call-setuppy "build" '()))

(define* (check #:key tests? test-target #:allow-other-keys)
  "Run the test suite of a given Python package."
  (if tests?
    (call-setuppy test-target '())
    #t))

(define* (install #:key outputs (configure-flags '())
                  #:allow-other-keys)
  "Install a given Python package."
  (let* ((out (assoc-ref outputs "out"))
         (params (append (list (string-append "--prefix=" out))
                         configure-flags)))
        (call-setuppy "install" params)))

(define* (wrap #:key inputs outputs #:allow-other-keys)
  (define (list-of-files dir)
    (map (cut string-append dir "/" <>)
         (or (scandir dir (lambda (f)
                            (let ((s (stat (string-append dir "/" f))))
                              (eq? 'regular (stat:type s)))))
             '())))

  (define bindirs
    (append-map (match-lambda
                 ((_ . dir)
                  (list (string-append dir "/bin")
                        (string-append dir "/sbin"))))
                outputs))

  (let* ((out  (assoc-ref outputs "out"))
         (python (assoc-ref inputs "python"))
         (python-version (string-take (string-take-right python 5) 3))
         (var `("PYTHONPATH" prefix
                ,(cons (string-append out "/lib/python"
                                      python-version "/site-packages")
                       (search-path-as-string->list
                        (or (getenv "PYTHONPATH") ""))))))
    (for-each (lambda (dir)
                (let ((files (list-of-files dir)))
                  (for-each (cut wrap-program <> var)
                            files)))
              bindirs)))

(define %standard-phases
  ;; 'configure' and 'build' phases are not needed.  Everything is done during
  ;; 'install'.
  (alist-cons-after
   'install 'wrap
   wrap
   (alist-replace
    'build build
    (alist-replace
     'check check
     (alist-replace 'install install
                    (alist-delete 'configure
                                               gnu:%standard-phases))))))

(define* (python-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given Python package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; python-build-system.scm ends here
