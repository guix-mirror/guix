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
    (synopsis "Create source code configuration scripts")
    (description
     "Autoconf offers the developer a robust set of M4 macros which expand
into shell code to test the features of Unix-like systems and to adapt
automatically their software package to these systems.  The resulting shell
scripts are self-contained and portable, freeing the user from needing to
know anything about Autoconf or M4.")
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
                     (when (and (file-exists? "configure")
                                (not (file-exists? "/bin/sh")))
                       ;; Patch regardless of RESULT, because `autoconf
                       ;; -Werror' can both create a `configure' file and
                       ;; return a non-zero exit code.
                       (patch-shebang "configure"))
                     (exit (status:exit-val result))))
                port)))
           (chmod (string-append bin "/autoconf") #o555)))))))

(define-public automake
  (package
    (name "automake")
    (version "1.14")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/automake/automake-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0nc0zqq8j336kamizzd86wb19vhbwywv5avcjh3cyx230xfqy671"))
             (patches
              (list (search-patch "automake-skip-amhello-tests.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("autoconf" ,autoconf-wrapper)
       ("perl" ,perl)))
    (native-search-paths
     (list (search-path-specification
            (variable "ACLOCAL_PATH")
            (directories '("share/aclocal")))))
    (arguments
     '(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (rnrs io ports))
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

                 ;; Files like `install-sh', `mdate.sh', etc. must use
                 ;; #!/bin/sh, otherwise users could leak erroneous shebangs
                 ;; in the wild.  See <http://bugs.gnu.org/14201> for an
                 ;; example.
                 (alist-cons-after
                  'install 'unpatch-shebangs
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (dir (string-append out "/share")))
                      (define (starts-with-shebang? file)
                        (equal? (call-with-input-file file
                                  (lambda (p)
                                    (list (get-u8 p) (get-u8 p))))
                                (map char->integer '(#\# #\!))))

                      (for-each (lambda (file)
                                  (when (and (starts-with-shebang? file)
                                             (executable-file? file))
                                    (format #t "restoring shebang on `~a'~%"
                                            file)
                                    (substitute* file
                                      (("^#!.*/bin/sh")
                                       "#!/bin/sh")
                                      (("^#!.*/bin/env(.*)$" _ args)
                                       (string-append "#!/usr/bin/env"
                                                      args)))))
                                (find-files dir ".*"))))
                  %standard-phases))))
    (home-page "http://www.gnu.org/software/automake/")
    (synopsis "Making GNU standards-compliant Makefiles")
    (description
     "Automake the part of the GNU build system for producing
standards-compliant Makefiles.  Build requirements are entered in an
intuitive format and then Automake works with Autoconf to produce a robust
Makefile, simplifying the entire process for the developer.")
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
               "0649qfpzkswgcj9vqkkr9rn4nlcx80faxpyqscy2k1x9c94f93dk"))
             (patches
              (list (search-patch "libtool-skip-tests.patch")
                    (search-patch "libtool-skip-tests-for-mips.patch")))))
    (build-system gnu-build-system)
    (native-inputs `(("m4" ,m4)
                     ("perl" ,perl)))

    ;; Separate binaries from the rest.  During bootstrap, only ltdl is
    ;; used; not depending on the binaries allows us to avoid retaining
    ;; a reference to the bootstrap bash.
    (outputs '("bin"                         ; libtoolize, libtool, etc.
               "out"))                       ; libltdl.so, ltdl.h, etc.

    (arguments
     (if (%current-target-system)
         '()                            ; no `check' phase when cross-building
         '(#:phases (alist-cons-before
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
                     %standard-phases))))
    (synopsis "Generic shared library support tools")
    (description
     "GNU Libtool helps in the creation and use of shared libraries, by
presenting a single consistent, portable interface that hides the usual
complexity of working with shared libraries across platforms.")
    (license gpl3+)
    (home-page "http://www.gnu.org/software/libtool/")))
