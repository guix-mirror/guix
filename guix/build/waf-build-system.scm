;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix build waf-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            waf-build))

;; Commentary:
;;
;; Builder-side code of the standard waf build procedure.
;;
;; Code:


(define (call-waf command params)
  (if (file-exists? "waf")
      (begin
         (format #t "running \"python waf\" with command ~s and parameters ~s~%"
                command params)
         (apply invoke "python" "waf" command params)
         #t)
      (error "no waf found")))

(define* (configure #:key target native-inputs inputs outputs
                    (configure-flags '())
                    #:allow-other-keys)
  "Build a given waf application."
  (let* ((prefix (assoc-ref outputs "out"))
         (flags  `(,(string-append "--prefix=" prefix)
                   ,@configure-flags)))
    (call-waf "configure" flags)))

(define* (build #:rest empty)
  "Build a given waf application."
  (call-waf "build" '()))

(define* (check #:key tests? test-target #:allow-other-keys)
  "Run the test suite of a given waf application."
  (if tests?
    (call-waf test-target '())
    #t))

(define* (install #:key outputs inputs (configure-flags '())
                  #:allow-other-keys)
  "Install a given waf application."
  (let* ((out (assoc-ref outputs "out"))
         (params (append (list (string-append "--prefix=" out))
                         configure-flags)))
        (call-waf "install" params)))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))

(define* (waf-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given waf application, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; waf-build-system.scm ends here
