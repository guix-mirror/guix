;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
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

;;;
;;; This file defines build jobs of Guix itself for the Hydra continuation
;;; integration tool.
;;;

;; Attempt to use our very own Guix modules.
(eval-when (expand load eval)

  ;; Ignore any available .go, and force recompilation.  This is because our
  ;; checkout in the store has mtime set to the epoch, and thus .go files look
  ;; newer, even though they may not correspond.
  (set! %fresh-auto-compile #t)

  ;; Display which files are loaded.
  (set! %load-verbosely #t)

  (and=> (assoc-ref (current-source-location) 'filename)
         (lambda (file)
           (let ((dir (string-append (dirname file) "/../..")))
             (format (current-error-port) "prepending ~s to the load path~%"
                     dir)
             (set! %load-path (cons dir %load-path))))))


(use-modules (guix store)
             (guix packages)
             (guix utils)
             (guix grafts)
             (guix derivations)
             (guix build-system gnu)
             (gnu packages package-management)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match))

;; XXX: Debugging hack: since `hydra-eval-guile-jobs' redirects the output
;; port to the bit bucket, let us write to the error port instead.
(setvbuf (current-error-port) _IOLBF)
(set-current-output-port (current-error-port))

(define* (package->alist store package system
                         #:optional (package-derivation package-derivation))
  "Convert PACKAGE to an alist suitable for Hydra."
  `((derivation . ,(derivation-file-name
                    (parameterize ((%graft? #f))
                      (package-derivation store package system
                                          #:graft? #f))))
    (description . ,(package-synopsis package))
    (long-description . ,(package-description package))
    (license . ,(package-license package))
    (home-page . ,(package-home-page package))
    (maintainers . ("bug-guix@gnu.org"))))

(define (hydra-jobs store arguments)
  "Return Hydra jobs."
  (define systems
    (match (filter-map (match-lambda
                        (('system . value)
                         value)
                        (_ #f))
                       arguments)
      ((lst ..1)
       lst)
      (_
       (list (%current-system)))))

  (define guix-checkout
    (assq-ref arguments 'guix))

  (let ((file (assq-ref guix-checkout 'file-name)))
    (format (current-error-port) "using checkout ~s (~s)~%"
            guix-checkout file)

    `((tarball . ,(cute package->alist store
                        (dist-package guix file)
                        (%current-system)))

      ,@(map (lambda (system)
               (let ((name (string->symbol
                            (string-append "guix." system))))
                 `(,name
                   . ,(cute package->alist store
                            (package
                              (inherit guix)
                              (version "latest")
                              (source file))
                            system))))
             %hydra-supported-systems))))
