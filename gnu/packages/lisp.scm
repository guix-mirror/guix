;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
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

(define-module (gnu packages lisp)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages m4)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public gcl
  (package
    (name "gcl")
    (version "2.6.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/" name "/" name "-" version ".tar.gz"))
      (sha256
       (base32 "1vsicv81ml7d92c87bckgkpvcshi6hzdnj44k0j6zs5mj8pzp8br"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f  ; The build system seems not to be thread safe.
       #:tests? #f  ; There does not seem to be make check or anything similar.
       #:configure-flags '("--enable-ansi") ; required for use by the maxima package
       #:phases (alist-cons-before
                'configure 'pre-conf
                (lambda _ 
                  ;; Patch bug when building readline support.  This bug was
                  ;; also observed by Debian
                  ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=741819
                  (substitute* "o/gcl_readline.d"
                    (("rl_attempted_completion_function = \\(CPPFunction \\*\\)rl_completion;")
                      "rl_attempted_completion_function = rl_completion;"))
                  (substitute* 
                      (append 
                       '("pcl/impl/kcl/makefile.akcl"
                         "add-defs"
                         "unixport/makefile.dos"
                         "add-defs.bat"
                         "gcl-tk/makefile.prev"
                         "add-defs1")
                       (find-files "h" "\\.defs"))
                    (("SHELL=/bin/(ba)?sh")
                     (string-append "SHELL=" (which "bash")))))
                %standard-phases)))
    (native-inputs
     `(("m4" ,m4)
       ("readline" ,readline)
       ("texinfo" ,texinfo)
       ("texlive" ,texlive)))
    (home-page "http://www.gnu.org/software/gcl")
    (synopsis "A Common Lisp implementation")
    (description "GCL is an implementation of the Common Lisp language.  It
features the ability to compile to native object code and to load native
object code modules directly into its lisp core.  It also features a
stratified garbage collection strategy, a source-level debugger and a built-in
interface to the Tk widget system.")
    (license license:lgpl2.0+)))

