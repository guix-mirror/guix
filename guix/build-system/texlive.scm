;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix build-system texlive)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix svn-download)
  #:use-module (ice-9 match)
  #:export (%texlive-build-system-modules
            texlive-build
            texlive-build-system
            texlive-ref
            texlive-origin
            %texlive-tag
            %texlive-revision))

;; Commentary:
;;
;; Standard build procedure for Texlive packages.
;;
;; Code:

;; These variables specify the SVN tag and the matching SVN revision.  They
;; are taken from https://www.tug.org/svn/texlive/tags/
(define %texlive-tag "texlive-2019.3")
(define %texlive-revision 51265)

(define (texlive-origin name version locations hash)
  "Return an <origin> object for a TeX Live package consisting of multiple
LOCATIONS with a provided HASH.  Use NAME and VERSION to compute a prettier
name for the checkout directory."
  (origin
    (method svn-multi-fetch)
    (uri (svn-multi-reference
          (url (string-append "svn://www.tug.org/texlive/tags/"
                              %texlive-tag "/Master/texmf-dist/"))
          (locations locations)
          (revision %texlive-revision)))
    (file-name (string-append name "-" version "-checkout"))
    (sha256 hash)))

(define (texlive-ref component id)
  "Return a <svn-reference> object for the package ID, which is part of the
given Texlive COMPONENT."
  (svn-reference
   (url (string-append "svn://www.tug.org/texlive/tags/"
                       %texlive-tag "/Master/texmf-dist/"
                       "source/" component "/" id))
   (revision %texlive-revision)))

(define %texlive-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build texlive-build-system)
    (guix build union)
    ,@%gnu-build-system-modules))

(define (default-texlive-bin)
  "Return the default texlive-bin package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((tex-mod (resolve-interface '(gnu packages tex))))
    (module-ref tex-mod 'texlive-bin)))

(define (default-texlive-latex-base)
  "Return the default texlive-latex-base package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((tex-mod (resolve-interface '(gnu packages tex))))
    (module-ref tex-mod 'texlive-latex-base)))

(define* (lower name
                #:key
                source inputs native-inputs outputs
                system target
                (texlive-latex-base (default-texlive-latex-base))
                (texlive-bin (default-texlive-bin))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:inputs #:native-inputs
      #:texlive-latex-base #:texlive-bin))

  (bag
    (name name)
    (system system)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@inputs

                   ;; Keep the standard inputs of 'gnu-build-system'.
                   ,@(standard-packages)))
    (build-inputs `(("texlive-bin" ,texlive-bin)
                    ("texlive-latex-base" ,texlive-latex-base)
                    ,@native-inputs))
    (outputs outputs)
    (build texlive-build)
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (texlive-build store name inputs
                        #:key
                        (tests? #f)
                        tex-directory
                        (build-targets #f)
                        (tex-format "luatex")
                        (phases '(@ (guix build texlive-build-system)
                                    %standard-phases))
                        (outputs '("out"))
                        (search-paths '())
                        (system (%current-system))
                        (guile #f)
                        (substitutable? #t)
                        (imported-modules %texlive-build-system-modules)
                        (modules '((guix build texlive-build-system)
                                   (guix build union)
                                   (guix build utils))))
  "Build SOURCE with INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (texlive-build #:name ,name
                      #:source ,(match (assoc-ref inputs "source")
                                       (((? derivation? source))
                                        (derivation->output-path source))
                                       ((source)
                                        source)
                                       (source
                                        source))
                      #:tex-directory ,tex-directory
                      #:build-targets ,build-targets
                      #:tex-format ,tex-format
                      #:system ,system
                      #:tests? ,tests?
                      #:phases ,phases
                      #:outputs %outputs
                      #:search-paths ',(map search-path-specification->sexp
                                            search-paths)
                      #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                               ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build
                                #:substitutable? substitutable?))

(define texlive-build-system
  (build-system
    (name 'texlive)
    (description "The build system for TeX Live packages")
    (lower lower)))

;;; texlive.scm ends here
