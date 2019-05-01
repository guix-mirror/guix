;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Joshua S. Grant <jgrant@parenthetical.io>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu)
  #:use-module (guix i18n)
  #:use-module (guix utils)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:export (use-package-modules
            use-service-modules
            use-system-modules))

;;; Commentary:
;;;
;;; This composite module re-exports core parts the (gnu …) public modules.
;;;
;;; Code:

(eval-when (eval load compile)
  (begin
    (define %public-modules
      '((gnu system)
        (gnu system mapped-devices)
        (gnu system file-systems)
        (gnu bootloader)
        (gnu bootloader grub)
        (gnu system keyboard)
        (gnu system pam)
        (gnu system shadow)                       ; 'user-account'
        (gnu system linux-initrd)
        (gnu system nss)
        (gnu services)
        (gnu services base)
        (gnu packages)
        (gnu packages base)
        (guix gexp)))                             ; so gexps can be used

    (for-each (let ((i (module-public-interface (current-module))))
                (lambda (m)
                  (module-use! i (resolve-interface m))))
              %public-modules)))

(define (%try-use-modules modules location make-hint)
  "Attempt to load all of MODULES.  Report errors as coming from LOCATION, a
<location> record, and use MAKE-HINT to produce a fix hint."
  (define (location->string loc)
    (match loc
      (#f "")
      (($ <location> file line column)
       (format #f "~a:~a:~a: " file line column))))

  (for-each (lambda (module)
              (catch 'misc-error
                (lambda ()
                  (process-use-modules `((,module))))
                (lambda _
                  (raise
                   (apply
                    make-compound-condition
                    (condition
                     (&message
                      (message (format #f (G_ "module ~a not found")
                                       module))))
                    (condition
                     (&error-location (location location)))
                    (or (and=> (make-hint module) list)
                        '()))))))
            modules))

(define (package-module-hint module)
  (define last-name
    (match module
      ((_ ... last)
       (symbol->string last))))

  (match (find-packages-by-name last-name)
    (()
     (condition
      (&fix-hint
       (hint (G_ "\
You may use @command{guix package --show=foo | grep location} to search
for the location of package @code{foo}.
If you get the line @code{location: gnu/packages/bar.scm:174:2},
add @code{bar} to the @code{use-package-modules} form.")))))
    ((package _ ...)
     (condition
      (&fix-hint
       (hint (format #f (G_ "\
Try adding @code{(use-package-modules ~a)}.")
                     (basename (location-file (package-location package))
                               ".scm"))))))))

(define (service-module-hint module)
  (define last-name
    (match module
      ((_ ... last)
       last)))

  (match (lookup-service-types last-name)
    (()
     (condition
      (&fix-hint
       (hint (format #f (G_ "\
You may use @command{guix system search ~a} to search for a service
matching @code{~a}.
If you get the line @code{location: gnu/services/foo.scm:188:2},
add @code{foo} to the @code{use-service-modules} form.")
                     last-name last-name)))))
    ((package _ ...)
     (condition
      (&fix-hint
       (hint (format #f (G_ "\
Try adding @code{(use-service-modules ~a)}.")
                     (basename (location-file (service-type-location package))
                               ".scm"))))))))

(define-syntax-rule (try-use-modules hint modules ...)
  (eval-when (expand load eval)
    (%try-use-modules '(modules ...)
                      (source-properties->location
                       (current-source-location))
                      hint)))

(define-syntax-rule (use-package-modules module ...)
  (try-use-modules package-module-hint
                   (gnu packages module) ...))

(define-syntax-rule (use-service-modules module ...)
  (try-use-modules service-module-hint
                   (gnu services module) ...))

(define-syntax-rule (use-system-modules module ...)
  (try-use-modules (const #f)                     ;no hint
                   (gnu system module) ...))

;;; gnu.scm ends here
