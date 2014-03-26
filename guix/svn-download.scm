;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
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

(define-module (guix svn-download)
  #:use-module (guix records)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (svn-reference
            svn-reference?
            svn-reference-url
            svn-reference-revision
            svn-fetch))

;;; Commentary:
;;;
;;; An <origin> method that fetches a specific revision from a Subversion
;;; repository.  The repository URL and REVISION are specified with a
;;; <svn-reference> object.  REVISION should be specified as a number.
;;;
;;; Code:

(define-record-type* <svn-reference>
  svn-reference make-svn-reference
  svn-reference?
  (url      svn-reference-url)                    ; string
  (revision svn-reference-revision))              ; number

(define* (svn-fetch store ref hash-algo hash
                    #:optional name
                    #:key (system (%current-system)) guile svn)
  "Return a fixed-output derivation in STORE that fetches REF, a
<svn-reference> object.  The output is expected to have recursive hash HASH of
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

  (define svn-for-build
    (match svn
      ((? package?)
       (package-derivation store svn system))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages version-control)))
              (svn    (module-ref distro 'subversion)))
         (package-derivation store svn system)))))

  (let* ((command (string-append (derivation->output-path svn-for-build)
                                 "/bin/svn"))
         (builder `(begin
                     (use-modules (guix build svn))
                     (svn-fetch ',(svn-reference-url ref)
                                ',(svn-reference-revision ref)
                                %output
                                #:svn-command ',command))))
    (build-expression->derivation store (or name "svn-checkout") builder
                                  #:system system
                                  #:local-build? #t
                                  #:inputs `(("svn" ,svn-for-build))
                                  #:hash-algo hash-algo
                                  #:hash hash
                                  #:recursive? #t
                                  #:modules '((guix build svn)
                                              (guix build utils))
                                  #:guile-for-build guile-for-build
                                  #:local-build? #t)))

;;; svn-download.scm ends here
