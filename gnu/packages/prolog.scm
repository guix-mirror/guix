;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
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

(define-module (gnu packages prolog)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1))

(define-public gprolog
  (package
    (name "gprolog")
    (version "1.4.5")
    (source
      (origin
        (method url-fetch)
        ;; Recent versions are not hosted on the GNU mirrors.
        (uri (list (string-append "http://gprolog.org/gprolog-" version
                                  ".tar.gz")
                   (string-append "mirror://gnu/gprolog/gprolog-" version
                                  ".tar.gz")))
        (sha256
         (base32
          "0z4cc42n3k6i35b8mr816iwsvrpxshw6d7dgz6s2h1hy0l7g1p5z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append
              "--with-install-dir=" %output "/share/gprolog"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'change-dir-n-fix-shells
           (lambda _
             (chdir "src")
             (substitute* "configure"
               (("-/bin/sh")  (string-append "-"  (which "sh")))
               (("= /bin/sh") (string-append "= " (which "sh"))))
             #t)))))
    (home-page "https://www.gnu.org/software/gprolog/")
    (synopsis "Prolog compiler")
    (description
     "GNU Prolog is a standards-compliant Prolog compiler with constraint
solving over finite domains.  It accepts Prolog+ constraint programs and
produces a compiled, native binary which can function in a stand-alone
manner.  It also features an interactive interpreter.")
    (license (list gpl2+ lgpl3+))

    ;; See 'configure' for the list of supported architectures.
    (supported-systems (fold delete
                             %supported-systems
                             '("armhf-linux" "mips64el-linux")))))

;; TODO: This current version of SWI-PROLOG using the
;; tarball from the official website is completely self-contained
;; such that all submodules are provided. Eventually replace this
;; and manually patch those submodules.
(define-public swi-prolog
  (package
    (name "swi-prolog")
    (version "8.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.swi-prolog.org/download/stable/src/swipl-"
			    version ".tar.gz"))
        (sha256
         (base32
          "1bnnwbdxnifb1qj42prsqhx8y0k2z596wgvh4bbnd0bw8w59rrff"))))
    (build-system cmake-build-system)
    (arguments
     `(#:parallel-build? #f
       #:tests? #t)) ; FIXME: 
       ;#:configure-flags
	; (list "-DINSTALL_DOCUMENTATION=OFF"))) ; FIXME:
    (native-inputs
     `(("zlib" ,zlib)
       ("gmp" ,gmp)
       ("readline" ,readline)
					;("texlive" ,texlive)
       ("texinfo" ,texinfo)
       ("libunwind" ,libunwind)
       ("libjpeg", libjpeg)
       ("libxft" ,libxft)
       ("fontconfig" ,fontconfig)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("openssl" ,openssl)))
    (home-page "https://www.swi-prolog.org/")
    (synopsis "ISO/Edinburgh-style Prolog interpreter")
    (description "SWI-Prolog is a fast and powerful ISO/Edinburgh-style Prolog 
compiler with a rich set of built-in predicates. It offers a fast, robust and 
small environment which enables substantial applications to be developed with 
it.")
    (license #f))) ; TODO:
