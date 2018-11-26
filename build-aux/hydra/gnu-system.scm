;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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
;;; This file defines build jobs for the Hydra continuation integration
;;; tool.
;;;

(use-modules (system base compile))

(eval-when (expand load eval)

  ;; Pre-load the compiler so we don't end up auto-compiling it.
  (compile #t)

  ;; Use our very own Guix modules.
  (set! %fresh-auto-compile #t)

  ;; Ignore .go files except for Guile's.  This is because our checkout in the
  ;; store has mtime set to the epoch, and thus .go files look newer, even
  ;; though they may not correspond.  Use 'reverse' so that /gnu/store/…-guile
  ;; comes before /run/current-system/profile.
  (set! %load-compiled-path
    (list
     (dirname (dirname (search-path (reverse %load-compiled-path)
                                    "ice-9/boot-9.go")))))

  (and=> (assoc-ref (current-source-location) 'filename)
         (lambda (file)
           (let ((dir (canonicalize-path
                       (string-append (dirname file) "/../.."))))
             (format (current-error-port) "prepending ~s to the load path~%"
                     dir)
             (set! %load-path (cons dir %load-path))))))

(use-modules (gnu ci))

;; XXX: Debugging hack: since `hydra-eval-guile-jobs' redirects the output
;; port to the bit bucket, let us write to the error port instead.
(setvbuf (current-error-port) _IOLBF)
(set-current-output-port (current-error-port))

;; Return the procedure from (gnu ci).
hydra-jobs
