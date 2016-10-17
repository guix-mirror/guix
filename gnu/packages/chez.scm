;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
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

(define-module (gnu packages chez)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:select (gpl2+ lgpl2.0+ lgpl2.1+ asl2.0 bsd-3 expat))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define nanopass
  (let ((version "1.9"))
    (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/nanopass/nanopass-framework-scheme/archive"
            "/v" version ".tar.gz"))
      (sha256 (base32 "11pwyy4jiwhcl2am3a4ciczacjbjkyvdizqzdglb3l1hj2gj6nv2"))
      (file-name (string-append "nanopass-" version ".tar.gz")))))

(define stex
  (let ((version "1.2.1"))
    (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/dybvig/stex/archive"
            "/v" version ".tar.gz"))
      (sha256 (base32 "03pl3f668h24dn51vccr1sj5lsba9zq3j37bnxjvdadcdaj4qy5z"))
      (file-name (string-append "stex-" version ".tar.gz")))))

(define-public chez-scheme
  (package
    (name "chez-scheme")
    (version "9.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cisco/ChezScheme/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32 "0lprmpsjg2plc6ykgkz482zyvhkzv6gd0vnar71ph21h6zknyklz"))
       (file-name (string-append "chez-scheme-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)
       ("libx11" ,libx11)
       ("xorg-rgb" ,xorg-rgb)
       ("nanopass" ,nanopass)
       ("zlib" ,zlib)
       ("stex" ,stex)))
    (native-inputs
     `(("texlive" ,texlive)
       ("ghostscript" ,ghostscript-gs)
       ("netpbm" ,netpbm)))
    (native-search-paths
     (list (search-path-specification
            (variable "CHEZSCHEMELIBDIRS")
            (files (list (string-append "lib/csv" version "-site"))))))
    (outputs '("out" "doc"))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:test-target "test"
       #:configure-flags
       (list ,(match (or (%current-target-system) (%current-system))
                ("x86_64-linux" '(list "--machine=ta6le"))
                ("i686-linux" '(list "--machine=ti3le"))
                ;; FIXME: Some people succeeded in cross-compiling to
                ;; ARM. https://github.com/cisco/ChezScheme/issues/13
                (_
                 '())))
       #:phases
       (modify-phases %standard-phases
         ;; Adapt the custom 'configure' script.
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (nanopass (assoc-ref inputs "nanopass"))
                   (stex (assoc-ref inputs "stex"))
                   (zlib (assoc-ref inputs "zlib"))
                   (unpack (assoc-ref %standard-phases 'unpack))
                   (patch-source-shebangs
                    (assoc-ref %standard-phases 'patch-source-shebangs)))
               (map (match-lambda
                      ((src orig-name new-name)
                       (with-directory-excursion "."
                         (apply unpack (list #:source src))
                         (apply patch-source-shebangs (list #:source src)))
                       (delete-file-recursively new-name)
                       (system* "mv" orig-name new-name)))
                    `((,nanopass "nanopass-framework-scheme-1.9" "nanopass")
                      (,stex "stex-1.2.1" "stex")))
               ;; The Makefile wants to download and compile "zlib".  We patch
               ;; it to use the one from our 'zlib' package.
               (substitute* "configure"
                 (("rmdir zlib .*$") "echo \"using system zlib\"\n"))
               (substitute* (find-files "./c" "Mf-[a-zA-Z0-9.]+")
                 (("\\$\\{Kernel\\}: \\$\\{kernelobj\\} \\.\\./zlib/libz\\.a")
                  "${Kernel}: ${kernelobj}")
                 (("ld ([-a-zA-Z0-9_${} ]+) \\.\\./zlib/libz\\.a" all args)
                  (string-append "ld " args " " zlib "/lib/libz.a"))
                 (("\\(cd \\.\\./zlib; ([-a-zA-Z0-9=./ ]+))")
                  (which "true")))
               (substitute* (find-files "mats" "Mf-.*")
                 (("^[[:space:]]+(cc ) *") "\tgcc "))
               (substitute*
                   (find-files "." (string-append
                                    "("
                                    "Mf-[a-zA-Z0-9.]+"
                                    "|Makefile[a-zA-Z0-9.]*"
                                    "|checkin"
                                    "|stex\\.stex"
                                    "|newrelease"
                                    "|workarea"
                                    ;;"|[a-zA-Z0-9.]+\\.ms" ; guile can't read
                                    ")"))
                 (("/bin/rm") (which "rm"))
                 (("/bin/ln") (which "ln"))
                 (("/bin/cp") (which "cp")))
               (substitute* "makefiles/installsh"
                 (("/bin/true") (which "true")))
               (substitute* "stex/Makefile"
                 (("PREFIX=/usr") (string-append "PREFIX=" out)))
               (zero? (system* "./configure" "--threads"
                               (string-append "--installprefix=" out))))))
         ;; Installation of the documentation requires a running "chez".
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   (doc (string-append (assoc-ref outputs "doc")
                                       "/share/doc/" ,name "-" ,version)))
               (setenv "HOME" (getcwd))
               (setenv "PATH" (string-append (getenv "PATH") ":" bin))
               (with-directory-excursion "stex"
                 (system* "make" (string-append "BIN=" bin)))
               (system* "make" "docs")
               (with-directory-excursion "csug"
                 (substitute* "Makefile"
                   (("/tmp/csug9") doc))
                 (system* "make" "install")
                 (install-file "csug.pdf" doc))
               (with-directory-excursion "release_notes"
                 (install-file "release_notes.pdf" doc))
               #t)))
         ;; The binary file name is called "scheme" as the one from MIT/GNU
         ;; Scheme.  We add a symlink to use in case both are installed.
         (add-after 'install 'install-symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (name "chez-scheme"))
               (symlink (string-append bin "/scheme")
                        (string-append bin "/" name))
               (map (lambda (file)
                      (symlink file (string-append (dirname file)
                                                   "/" name ".boot")))
                    (find-files lib "scheme.boot"))
               #t))))))
    ;; According to the documentation MIPS is not supported.
    (supported-systems (delete "mips64el-linux" %supported-systems))
    (home-page "http://www.scheme.com")
    (synopsis "R6RS Scheme compiler and run-time")
    (description
     "Chez Scheme is a compiler and run-time system for the language of the
Revised^6 Report on Scheme (R6RS), with numerous extensions.  The compiler
generates native code for each target processor, with support for x86, x86_64,
and 32-bit PowerPC architectures.")
    (license asl2.0)))
