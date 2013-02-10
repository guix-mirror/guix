;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages autotools)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages bash)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial))

(define-public autoconf
  (package
    (name "autoconf")
    (version "2.69")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/autoconf/autoconf-"
                          version ".tar.xz"))
      (sha256
       (base32
        "113nlmidxy9kjr45kg9x3ngar4951mvag1js2a3j8nxcz34wxsv4"))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl)
       ("m4" ,m4)))
    ;; XXX: testsuite: 209 and 279 failed. The latter is an impurity. It
    ;; should use our own "cpp" instead of "/lib/cpp".
    (arguments `(#:tests? #f))
    (home-page
     "http://www.gnu.org/software/autoconf/")
    (synopsis
     "GNU Autoconf, a part of the GNU Build System")
    (description
     "GNU Autoconf is an extensible package of M4 macros that produce
shell scripts to automatically configure software source code
packages.  These scripts can adapt the packages to many kinds of
UNIX-like systems without manual user intervention.  Autoconf
creates a configuration script for a package from a template
file that lists the operating system features that the package
can use, in the form of M4 macro calls.")
    (license gpl3+))) ; some files are under GPLv2+

(define-public autoconf-wrapper
  ;; An Autoconf wrapper that generates `configure' scripts that use our
  ;; own Bash instead of /bin/sh in shebangs.  For that reason, it
  ;; should only be used internally---users should not end up
  ;; distributing `configure' files with a system-specific shebang.
  (package (inherit autoconf)
    (location (source-properties->location (current-source-location)))
    (name (string-append (package-name autoconf) "-wrapper"))
    (build-system trivial-build-system)
    (inputs `(("guile"
               ;; XXX: Kludge to hide the circular dependency.
               ,(module-ref (resolve-interface '(gnu packages guile))
                            'guile-2.0))
              ("autoconf" ,autoconf)
              ("bash" ,bash)))
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out      (assoc-ref %outputs "out"))
                (bin      (string-append out "/bin"))
                (autoconf (string-append
                           (assoc-ref %build-inputs "autoconf")
                           "/bin/autoconf"))
                (guile    (string-append
                           (assoc-ref %build-inputs "guile")
                           "/bin/guile"))
                (sh       (string-append
                           (assoc-ref %build-inputs "bash")
                           "/bin/sh"))
                (modules  ((compose dirname dirname dirname)
                           (search-path %load-path
                                        "guix/build/utils.scm"))))
           (mkdir-p bin)

           ;; Symlink all the binaries but `autoconf'.
           (with-directory-excursion bin
             (for-each (lambda (file)
                         (unless (string=? (basename file) "autoconf")
                           (symlink file (basename file))))
                       (find-files (dirname autoconf) ".*")))

           ;; Add an `autoconf' binary that wraps the real one.
           (call-with-output-file (string-append bin "/autoconf")
             (lambda (port)
               ;; Shamefully, Guile can be used in shebangs only if a
               ;; single argument is passed (-ds); otherwise it gets
               ;; them all as a single argument and fails to parse them.
               (format port "#!~a
export GUILE_LOAD_PATH=\"~a\"
export GUILE_LOAD_COMPILED_PATH=\"~a\"
exec ~a --no-auto-compile \"$0\" \"$@\"
!#~%"
                       sh modules modules guile)
               (write
                `(begin
                   (use-modules (guix build utils))
                   (let ((result (apply system* ,autoconf
                                        (cdr (command-line)))))
                     (if (and (zero? result)
                              (file-exists? "configure")
                              (not (file-exists? "/bin/sh")))
                         (begin
                           (patch-shebang "configure")
                           #t)
                         (exit (status:exit-val result)))))
                port)))
           (chmod (string-append bin "/autoconf") #o555)))))))

(define-public automake
  (package
    (name "automake")
    (version "1.13.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/automake/automake-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "12yi1bzkipi7qdmkdy77pazljsa9z7q66hi6c4rq73p7hbv6rkbf"))))
    (build-system gnu-build-system)
    (inputs
     `(("autoconf" ,autoconf-wrapper)
       ("perl" ,perl)
       ("patch/skip-amhello"
        ,(search-patch "automake-skip-amhello-tests.patch"))))
    (arguments
     '(#:patches (list (assoc-ref %build-inputs "patch/skip-amhello"))
       #:phases (alist-cons-before
                 'patch-source-shebangs 'patch-tests-shebangs
                 (lambda _
                   (let ((sh (which "sh")))
                     (substitute* (find-files "t" "\\.(sh|tap)$")
                       (("#![[:blank:]]?/bin/sh")
                        (string-append "#!" sh)))

                     ;; Set these variables for all the `configure' runs
                     ;; that occur during the test suite.
                     (setenv "SHELL" sh)
                     (setenv "CONFIG_SHELL" sh)))
                 %standard-phases)))
    (home-page "http://www.gnu.org/software/automake/")
    (synopsis
     "GNU Automake, a GNU standard-compliant makefile generator")
    (description
     "GNU Automake is a tool for automatically generating
`Makefile.in' files compliant with the GNU Coding
Standards.  Automake requires the use of Autoconf.")
    (license gpl2+)))                      ; some files are under GPLv3+

(define-public libtool
  (package
    (name "libtool")
    (version "2.4.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/libtool/libtool-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0649qfpzkswgcj9vqkkr9rn4nlcx80faxpyqscy2k1x9c94f93dk"))))
    (build-system gnu-build-system)
    (native-inputs `(("m4" ,m4)
                     ("perl" ,perl)))

    ;; Separate binaries from the rest.  During bootstrap, only ltdl is
    ;; used; not depending on the binaries allows us to avoid retaining
    ;; a reference to the bootstrap bash.
    (outputs '("bin"                         ; libtoolize, libtool, etc.
               "out"))                       ; libltdl.so, ltdl.h, etc.

    (arguments
     `(#:patches (list (assoc-ref %build-inputs "patch/skip-tests"))
       #:phases (alist-cons-before
                 'check 'pre-check
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Run the test suite in parallel, if possible.
                   (let ((ncores
                          (cond
                           ((getenv "NIX_BUILD_CORES")
                            =>
                            (lambda (n)
                              (if (zero? (string->number n))
                                  (number->string (current-processor-count))
                                  n)))
                           (else "1"))))
                    (setenv "TESTSUITEFLAGS"
                            (string-append "-j" ncores)))

                   ;; Path references to /bin/sh.
                   (let ((bash (assoc-ref inputs "bash")))
                     (substitute* "tests/testsuite"
                       (("/bin/sh")
                        (string-append bash "/bin/bash")))))
                 %standard-phases)))
    (inputs `(("patch/skip-tests"
               ,(search-patch "libtool-skip-tests.patch"))))
    (synopsis "GNU Libtool, a generic library support script")
    (description
     "GNU libtool is a generic library support script.  Libtool hides the
complexity of using shared libraries behind a consistent, portable interface.

To use libtool, add the new generic library building commands to your
Makefile, Makefile.in, or Makefile.am.  See the documentation for
details.")
    (license gpl3+)
    (home-page "http://www.gnu.org/software/libtool/")))
