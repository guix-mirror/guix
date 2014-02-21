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

(define-module (guix git-download)
  #:use-module (guix records)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (git-reference
            git-reference?
            git-reference-url
            git-reference-commit

            git-fetch))

;;; Commentary:
;;;
;;; An <origin> method that fetches a specific commit from a Git repository.
;;; The repository URL and commit hash are specified with a <git-reference>
;;; object.
;;;
;;; Code:

(define-record-type* <git-reference>
  git-reference make-git-reference
  git-reference?
  (url    git-reference-url)
  (commit git-reference-commit))

(define* (git-fetch store ref hash-algo hash
                    #:optional name
                    #:key (system (%current-system)) guile git)
  "Return a fixed-output derivation in STORE that fetches REF, a
<git-reference> object.  The output is expected to have recursive hash HASH of
type HASH-ALGO (a symbol).  Use NAME as the file name, or a generic name if
#f."
  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages base)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system)))))

  (define git-for-build
    (match git
      ((? package?)
       (package-derivation store git system))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages version-control)))
              (git    (module-ref distro 'git)))
         (package-derivation store git system)))))

  (let* ((command (string-append (derivation->output-path git-for-build)
                                 "/bin/git"))
         (builder `(begin
                     (use-modules (guix build git))
                     (git-fetch ',(git-reference-url ref)
                                ',(git-reference-commit ref)
                                %output
                                #:git-command ',command))))
    (build-expression->derivation store (or name "git-checkout") builder
                                  #:system system
                                  #:local-build? #t
                                  #:inputs `(("git" ,git-for-build))
                                  #:hash-algo hash-algo
                                  #:hash hash
                                  #:recursive? #t
                                  #:modules '((guix build git)
                                              (guix build utils))
                                  #:guile-for-build guile-for-build)))

;;; git-download.scm ends here
