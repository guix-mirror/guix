;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

;; Build Guix using Guix.

(use-modules (srfi srfi-26))

;; Add ~/.config/guix/current to the search path.
(eval-when (expand load eval)
  (and=> (or (getenv "XDG_CONFIG_HOME")
             (and=> (getenv "HOME")
                    (cut string-append <> "/.config/guix/current")))
         (lambda (current)
           (set! %load-path
             (cons (string-append current "/share/guile/site/"
                                  (effective-version))
                   %load-path))
           (set! %load-compiled-path
             (cons (string-append current "/lib/guile/" (effective-version)
                                  "/site-ccache")
                   %load-compiled-path)))))

(use-modules (guix) (guix ui)
             (guix git-download)
             (ice-9 match))

(match (command-line)
  ((program source)
   (with-error-handling
     (with-store store
       (let* ((script (string-append source "/build-aux/build-self.scm"))
              (build  (primitive-load script))
              (git?   (git-predicate source)))
         (run-with-store store
           ;; TODO: Extract #:version and #:commit using Guile-Git.
           (mlet* %store-monad ((source (interned-file source "guix-source"
                                                       #:select? git?
                                                       #:recursive? #t))
                                (drv    (build source #:pull-version 1)))
             (mbegin %store-monad
               (show-what-to-build* (list drv))
               (built-derivations (list drv))
               (with-monad %store-monad
                 (display (derivation->output-path drv))
                 (newline)
                 (return drv))))))))))
