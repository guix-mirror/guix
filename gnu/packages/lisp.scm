;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages m4)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages which)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages libffi))

(define-public gcl
  (package
    (name "gcl")
    (version "2.6.12")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/" name "/" name "-" version ".tar.gz"))
      (sha256
       (base32 "1s4hs2qbjqmn9h88l4xvsifq5c3dlc5s74lyb61rdi5grhdlkf4f"))))
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
                ;; drop strip phase to make maxima build, see
                ;; https://www.ma.utexas.edu/pipermail/maxima/2008/009769.html
                (alist-delete 'strip
                 %standard-phases))))
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

(define-public ecl
  (package
    (name "ecl")
    (version "13.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/ecls/ecls/"
                           (version-major+minor version)
                           "/ecl-" version ".tgz"))
       (sha256
        (base32 "18ic8w9sdl0dh3kmyc9lsrafikrd9cg1jkhhr25p9saz0v75f77r"))))
    (build-system gnu-build-system)
    (native-inputs `(("which" ,which)))
    (inputs `(("gmp" ,gmp)
              ("libatomic-ops" ,libatomic-ops)
              ("libgc" ,libgc)
              ("libffi" ,libffi)))
    (arguments
     '(#:phases
       ;; The test-suite seems to assume that ECL is installed.  So re-order
       ;; the phases, then reference the installed executable.
       (let* ((check-phase (assq-ref %standard-phases 'check))
              (rearranged-phases
               (alist-cons-after 'install 'check check-phase
                                 (alist-delete 'check %standard-phases))))
         (alist-cons-before
          'check 'pre-check
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* '("build/tests/Makefile")
              (("ECL=ecl")
               (string-append
                "ECL=" (assoc-ref outputs "out") "/bin/ecl"))))
          rearranged-phases))
       ;; Parallel builds explicitly not supported:
       ;; http://sourceforge.net/p/ecls/bugs/98/
       #:parallel-build? #f
       #:parallel-tests? #f))
    (home-page "http://ecls.sourceforge.net/")
    (synopsis "Embeddable Common Lisp")
    (description "ECL is an implementation of the Common Lisp language as
defined by the ANSI X3J13 specification.  Its most relevant features are: a
bytecode compiler and interpreter, being able to compile Common Lisp with any
C/C++ compiler, being able to build standalone executables and libraries, and
supporting ASDF, Sockets, Gray streams, MOP, and other useful components.")
    ;; Note that the file "Copyright" points to some files and directories
    ;; which aren't under the lgpl2.0+ and instead contain many different,
    ;; non-copyleft licenses.
    (license license:lgpl2.0+)))
