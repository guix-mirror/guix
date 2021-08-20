;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages datamash)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public datamash
  (package
    (name "datamash")
    (version "1.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/datamash/datamash-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1cxdlhgz3wzjqlq8bgwad93fgqymk2abbldfzw1ffnhcp4mmjjjp"))))
    (native-inputs
     (list which ;for tests
           perl))                 ;for help2man
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/datamash/")
    (synopsis "Scriptable statistics and data calculation")
    (description
     "Perform basic numeric, textual and statistical operations on plain text
files.  Designed to work within standard pipelines without additional code.")
    (license gpl3+)))

(define-public vnlog
  (package
    (name "vnlog")
    (version "1.32")
    (home-page "https://github.com/dkogan/vnlog")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g4insm6gkw3c82fq8q9swkdha3cncbz1nib15yg9b2s4yl123hm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list python-numpy))               ;for tests
    (inputs
     (list mawk
           perl
           perl-ipc-run
           perl-list-moreutils
           perl-string-shellquote
           perl-text-diff
           perl-text-table
           python-wrapper))
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "DESTDIR=" #$output)
                   "USRLIB=lib"
                   "MANDIR=share/man"
                   (string-append "PY3_MODULE_PATH=lib/python"
                                  #$(version-major+minor
                                     (package-version
                                      (this-package-input "python-wrapper")))
                                  "/site-packages")
                   ;; Do not install the Python 2 modules.
                   "DIST_PY2_MODULES=")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'adjust-Makefile
                 (lambda _
                   (substitute* "GNUmakefile"
                     ;; Install Perl modules to the usual location.
                     (("/usr/share/perl5")
                      (string-append "/lib/perl5/site_perl/"
                                     #$(package-version
                                        (this-package-input "perl"))))
                     ;; Do not add a '/usr' suffix to DESTDIR.
                     (("\\$\\(DESTDIR\\)/usr")
                      "$(DESTDIR)")
                     ;; Do not strip RUNPATH from the installed C library.
                     ((".*find.*chrpath.*")
                      ""))))
               (add-after 'unpack 'use-absolute-mawk
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((mawk (search-input-file inputs "bin/mawk")))
                     (substitute* '("vnl-filter" "lib/Vnlog/Util.pm")
                       (("'mawk'")
                        (string-append "'" mawk "'"))))))
               (delete 'configure)
               (add-before 'check 'disable-python2-test
                 (lambda _
                   (delete-file "test/test_python2_parser.sh")
                   (substitute* "GNUmakefile"
                     ((".*test/test_python2_parser\\.sh\\.RUN.*")
                      ""))))
               (add-after 'install 'wrap
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (PERL5LIB (string-append
                                     out "/lib/perl5/site_perl/"
                                     #$(package-version
                                        (this-package-input "perl"))
                                     ":" (getenv "PERL5LIB"))))
                     (for-each (lambda (script)
                                 (wrap-program script
                                   `("PERL5LIB" ":" prefix (,PERL5LIB))))
                               (find-files (string-append out "/bin"))))))
               (add-after 'wrap 'check-wrapped-executables
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     ;; Assert that the installed Perl scripts can find
                     ;; their dependencies even when PERL5LIB is unset.
                     (unsetenv "PERL5LIB")
                     (for-each (lambda (script)
                                 (invoke/quiet script "--help"))
                               (find-files (string-append out "/bin")
                                           "^vnl-[[:lower:]]+$"))))))))
    (synopsis "Process labelled tabular ASCII data on the command line")
    (description
     "Vnlog (pronounced @dfn{vanillog}) is a toolkit for manipulating
tabular ASCII data with labelled fields using regular command-line tools.")
    (license lgpl2.1+)))
