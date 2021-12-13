;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2017, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix build svn) #:prefix build:)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (svn-reference
            svn-reference?
            svn-reference-url
            svn-reference-revision
            svn-reference-recursive?
            svn-fetch
            download-svn-to-store

            svn-multi-reference
            svn-multi-reference?
            svn-multi-reference-url
            svn-multi-reference-revision
            svn-multi-reference-locations
            svn-multi-reference-recursive?
            svn-multi-fetch
            download-multi-svn-to-store))

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
  (url        svn-reference-url)                    ; string
  (revision   svn-reference-revision)               ; number
  (recursive? svn-reference-recursive? (default #t))
  (user-name  svn-reference-user-name (default #f))
  (password   svn-reference-password (default #f)))

(define (subversion-package)
  "Return the default Subversion package."
  (let ((distro (resolve-interface '(gnu packages version-control))))
    (module-ref distro 'subversion)))

(define* (svn-fetch ref hash-algo hash
                    #:optional name
                    #:key (system (%current-system)) (guile (default-guile))
                    (svn (subversion-package)))
  "Return a fixed-output derivation that fetches REF, a <svn-reference>
object.  The output is expected to have recursive hash HASH of type
HASH-ALGO (a symbol).  Use NAME as the file name, or a generic name if #f."
  (define build
    (with-imported-modules '((guix build svn)
                             (guix build utils))
      #~(begin
          (use-modules (guix build svn))
          (svn-fetch '#$(svn-reference-url ref)
                     '#$(svn-reference-revision ref)
                     #$output
                     #:svn-command (string-append #+svn "/bin/svn")
                     #:recursive? #$(svn-reference-recursive? ref)
                     #:user-name #$(svn-reference-user-name ref)
                     #:password #$(svn-reference-password ref)))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "svn-checkout") build
                      #:system system
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile
                      #:local-build? #t)))

(define-record-type* <svn-multi-reference>
  svn-multi-reference make-svn-multi-reference
  svn-multi-reference?
  (url        svn-multi-reference-url)                 ; string
  (revision   svn-multi-reference-revision)            ; number
  (locations  svn-multi-reference-locations)           ; list of strings
  (recursive? svn-multi-reference-recursive? (default #t))
  (user-name  svn-multi-reference-user-name (default #f))
  (password   svn-multi-reference-password (default #f)))

(define* (svn-multi-fetch ref hash-algo hash
                          #:optional name
                          #:key (system (%current-system)) (guile (default-guile))
                          (svn (subversion-package)))
  "Return a fixed-output derivation that fetches REF, a <svn-multi-reference>
object.  The output is expected to have recursive hash HASH of type
HASH-ALGO (a symbol).  Use NAME as the file name, or a generic name if #f."
  (define build
    (with-imported-modules '((guix build svn)
                             (guix build utils))
      #~(begin
          (use-modules (guix build svn)
                       (guix build utils)
                       (srfi srfi-1))
          (every (lambda (location)
                   ;; The directory must exist if we are to fetch only a
                   ;; single file.
                   (unless (string-suffix? "/" location)
                     (mkdir-p (string-append #$output "/" (dirname location))))
                   (svn-fetch (string-append '#$(svn-multi-reference-url ref)
                                             "/" location)
                              '#$(svn-multi-reference-revision ref)
                              (if (string-suffix? "/" location)
                                  (string-append #$output "/" location)
                                  (string-append #$output "/" (dirname location)))
                              #:svn-command (string-append #+svn "/bin/svn")
                              #:recursive?
                              #$(svn-multi-reference-recursive? ref)
                              #:user-name #$(svn-multi-reference-user-name ref)
                              #:password #$(svn-multi-reference-password ref)))
                 '#$(sexp->gexp (svn-multi-reference-locations ref))))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "svn-checkout") build
                      #:leaked-env-vars '("http_proxy" "https_proxy"
                                          "LC_ALL" "LC_MESSAGES" "LANG"
                                          "COLUMNS")
                      #:system system
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile
                      #:local-build? #t)))

(define* (download-svn-to-store store ref
                                #:optional (name (basename (svn-reference-url ref)))
                                #:key (log (current-error-port)))
  "Download from REF, a <svn-reference> object to STORE.  Write progress
reports to LOG."
  (call-with-temporary-directory
   (lambda (temp)
     (let ((result
            (parameterize ((current-output-port log))
              (build:svn-fetch (svn-reference-url ref)
                               (svn-reference-revision ref)
                               (string-append temp "/svn")
                               #:user-name (svn-reference-user-name ref)
                               #:password (svn-reference-password ref)))))
       (and result
            (add-to-store store name #t "sha256"
                          (string-append temp "/svn")))))))

(define* (download-multi-svn-to-store store ref
                                      #:optional (name (basename (svn-multi-reference-url ref)))
                                      #:key (log (current-error-port)))
  "Download from REF, a <svn-multi-reference> object to STORE.  Write progress
reports to LOG."
  (call-with-temporary-directory
   (lambda (temp)
     (and (every (lambda (location)
                   (let ((dir (string-append temp "/" (dirname location))))
                     (mkdir-p dir))
                   (parameterize ((current-output-port log))
                     (build:svn-fetch (string-append (svn-multi-reference-url ref)
                                                     "/" location)
                                      (svn-multi-reference-revision ref)
                                      (if (string-suffix? "/" location)
                                          (string-append temp "/" location)
                                          (string-append temp "/" (dirname location)))
                                      #:recursive?
                                      (svn-multi-reference-recursive? ref)
                                      #:user-name (svn-multi-reference-user-name ref)
                                      #:password (svn-multi-reference-password ref))))
                 (svn-multi-reference-locations ref))
          (add-to-store store name #t "sha256" temp)))))

;;; svn-download.scm ends here
