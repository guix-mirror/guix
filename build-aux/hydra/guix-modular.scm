;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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
;;; This file defines a continuous integration job to build the same modular
;;; Guix as 'guix pull', which is defined in (guix self).
;;;

;; Attempt to use our very own Guix modules.
(eval-when (compile load eval)

  ;; Ignore any available .go, and force recompilation.  This is because our
  ;; checkout in the store has mtime set to the epoch, and thus .go files look
  ;; newer, even though they may not correspond.
  (set! %fresh-auto-compile #t)

  (and=> (assoc-ref (current-source-location) 'filename)
         (lambda (file)
           (let ((dir (canonicalize-path
                       (string-append (dirname file) "/../.."))))
             (format (current-error-port) "prepending ~s to the load path~%"
                     dir)
             (set! %load-path (cons dir %load-path))))))


(use-modules (guix store)
             (guix config)
             (guix utils)
             (guix grafts)
             ((guix packages) #:select (%hydra-supported-systems))
             (guix derivations)
             (guix monads)
             (guix gexp)
             (guix self)
             ((guix licenses) #:prefix license:)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match))

;; XXX: Debugging hack: since `hydra-eval-guile-jobs' redirects the output
;; port to the bit bucket, let us write to the error port instead.
(setvbuf (current-error-port) _IOLBF)
(set-current-output-port (current-error-port))

(define* (build-job store source version system)
  "Return a Hydra job a list building the modular Guix derivation from SOURCE
for SYSTEM.  Use VERSION as the version identifier."
  (lambda ()
    `((derivation . ,(derivation-file-name
                      (parameterize ((%graft? #f))
                        (run-with-store store
                          (lower-object (compiled-guix source
                                                       #:version version))))))
      (description . "Modular Guix")
      (long-description
       . "This is the modular Guix package as produced by 'guix pull'.")
      (license . ,license:gpl3+)
      (home-page . ,%guix-home-page-url)
      (maintainers . (,%guix-bug-report-address)))))

(define (hydra-jobs store arguments)
  "Return Hydra jobs."
  (define systems
    (match (filter-map (match-lambda
                         (('system . value) value)
                         (_ #f))
                       arguments)
      ((lst ..1)
       lst)
      (_
       (list (%current-system)))))

  (define guix-checkout
    (assq-ref arguments 'guix))

  (define version
    (or (assq-ref guix-checkout 'revision)
        "0.unknown"))

  (let ((file (assq-ref guix-checkout 'file-name)))
    (format (current-error-port) "using checkout ~s (~s)~%"
            guix-checkout file)

    (map (lambda (system)
           (let ((name (string->symbol
                        (string-append "guix." system))))
             `(,name
               . ,(build-job store file version system))))
         %hydra-supported-systems)))
