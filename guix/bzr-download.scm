;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix bzr-download)
  #:use-module (guix gexp)
  #:use-module (guix modules)   ;for 'source-module-closure'
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)

  #:export (bzr-reference
            bzr-reference?
            bzr-reference-url
            bzr-reference-revision

            bzr-fetch))

;;; Commentary:
;;;
;;; An <origin> method that fetches a specific revision from a Bazaar
;;; repository.  The repository URL and revision identifier are specified with
;;; a <bzr-reference> object.
;;;
;;; Code:

(define-record-type* <bzr-reference>
  bzr-reference make-bzr-reference
  bzr-reference?
  (url bzr-reference-url)
  (revision bzr-reference-revision))

(define (bzr-package)
  "Return the default Bazaar package."
  (let ((distro (resolve-interface '(gnu packages version-control))))
    (module-ref distro 'bazaar)))

(define* (bzr-fetch ref hash-algo hash
                       #:optional name
                       #:key (system (%current-system)) (guile (default-guile))
                       (bzr (bzr-package)))
  "Return a fixed-output derivation that fetches REF, a <bzr-reference>
object.  The output is expected to have recursive hash HASH of type
HASH-ALGO (a symbol).  Use NAME as the file name, or a generic name if #f."
  (define build
    (with-imported-modules (source-module-closure
                            '((guix build bzr)))
      #~(begin
          (use-modules (guix build bzr))
          (bzr-fetch
           (getenv "bzr url") (getenv "bzr reference") #$output
           #:bzr-command (string-append #+bzr "/bin/bzr")))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "bzr-branch") build
                      ;; Use environment variables and a fixed script name so
                      ;; there's only one script in store for all the
                      ;; downloads.
                      #:script-name "bzr-download"
                      #:env-vars
                      `(("bzr url" . ,(bzr-reference-url ref))
                        ("bzr reference" . ,(bzr-reference-revision ref)))
                      #:system system
                      #:local-build? #t  ;don't offload repo branching
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile)))

;;; bzr-download.scm ends here
