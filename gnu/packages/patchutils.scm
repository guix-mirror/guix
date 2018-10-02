;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages patchutils)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages less)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml))

(define-public patchutils
  (package
    (name "patchutils")
    (version "0.3.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://cyberelk.net/tim/data/patchutils/stable/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "0g5df00cj4nczrmr4k791l7la0sq2wnf8rn981fsrz1f3d2yix4i"))
      (patches (search-patches "patchutils-xfail-gendiff-tests.patch"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (arguments
     '(#:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-test-scripts
           (lambda _
             (let ((echo (which "echo")))
               (substitute*
                   (find-files "tests" "^run-test$")
                 (("/bin/echo") echo)))
             #t))
         (add-after 'install 'wrap-program
           ;; Point installed scripts to the utilities they need.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (diffutils (assoc-ref inputs "diffutils"))
                    (sed       (assoc-ref inputs "sed"))
                    (gawk      (assoc-ref inputs "gawk")))
               (for-each
                (lambda (prog)
                  (wrap-program (string-append out "/bin/" prog)
                    `("PATH" ":" prefix
                      ,(map (lambda (dir)
                              (string-append dir "/bin"))
                            (list diffutils sed gawk)))))
                '("dehtmldiff" "editdiff" "espdiff")))
             #t)))))
    (home-page "http://cyberelk.net/tim/software/patchutils")
    (synopsis "Collection of tools for manipulating patch files")
    (description
     "Patchutils is a collection of programs that can manipulate patch files
in useful ways such as interpolating between two pre-patches, combining two
incremental patches, fixing line numbers in hand-edited patches, and simply
listing the files modified by a patch.")
    (license gpl2+)))

(define-public quilt
  (package
    (name "quilt")
    (version "0.65")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://savannah/quilt/"
                          name "-" version ".tar.gz"))
      (sha256
       (base32
        "06b816m2gz9jfif7k9v2hrm7fz76zjg5pavf7hd3ifybwn4cgjzn"))
      (patches (search-patches "quilt-test-fix-regex.patch"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)
              ("less" ,less)
              ("file" ,file)
              ("ed" ,ed)))
    (arguments
     '(#:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-tests
           (lambda _
             (substitute*
                 '("test/run"
                   "test/edit.test") 
               (("/bin/sh") (which "sh")))
             (substitute* "test/create-delete.test"
               ;; We'd rather use quilt's compat/getopt than declare a
               ;; dependency on util-linux, but this test fails because of
               ;; compat/getopt's handling of "---" in this test, so remove it
               ;; for now.
               ((" ---") ""))
             (substitute* '("test/empty-files.test" "test/faildiff.test")
               ;; compat/getopt seems not to handle splitting of short opts
               ;; from its arguments.
               (("-pab") "-p ab"))
             #t))
         (add-after 'install 'wrap-program
           ;; quilt's configure checks for the absolute path to the utilities it
           ;; needs, but uses only the name when invoking them, so we need to
           ;; make sure the quilt script can find those utilities when run.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (coreutils (assoc-ref inputs "coreutils"))
                    (diffutils (assoc-ref inputs "diffutils"))
                    (findutils (assoc-ref inputs "findutils"))
                    (less      (assoc-ref inputs "less"))
                    (file      (assoc-ref inputs "file"))
                    (ed        (assoc-ref inputs "ed"))
                    (sed       (assoc-ref inputs "sed"))
                    (bash      (assoc-ref inputs "bash"))
                    (grep      (assoc-ref inputs "grep")))
               (wrap-program (string-append out "/bin/quilt")
                 `("PATH" ":" prefix
                   ,(map (lambda (dir)
                           (string-append dir "/bin"))
                         (list coreutils diffutils findutils
                               less file ed sed bash grep)))))
             #t)))))
    (home-page "https://savannah.nongnu.org/projects/quilt/")
    (synopsis "Script for managing patches to software")
    (description
     "Quilt allows you to easily manage large numbers of patches by keeping
track of the changes each patch makes.  Patches can be applied, un-applied,
refreshed, and more.")
    (license gpl2)))

(define-public colordiff
  (package
    (name "colordiff")
    (version "1.0.18")
    (source
      (origin
        (method url-fetch)
        (uri (list (string-append "https://www.colordiff.org/colordiff-"
                                  version ".tar.gz")
                   (string-append "http://www.colordiff.org/archive/colordiff-"
                                  version ".tar.gz")))
      (sha256
       (base32
        "1q6n60n4b9fnzccxyxv04mxjsql4ddq17vl2c74ijvjdhpcfrkr9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                          "INSTALL_DIR=/bin" "MAN_DIR=/share/man/man1")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (delete 'build))))             ; nothing to build
    (inputs
     `(("perl" ,perl)
       ("xmlto" ,xmlto)))
    (home-page "https://www.colordiff.org")
    (synopsis "Display diff output with colors")
    (description
     "Colordiff is Perl script wrapper on top of diff command which provides
'syntax highlighting' for various patch formats.")
    (license gpl2+)))

(define-public patches
  (let ((commit "ef1b8a7d954b82ed4af3a08fd63d2085d19090ef"))
    (package
      (name "patches")
      (home-page "https://github.com/stefanha/patches")
      (version (string-append "0.0-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "11rdmhv0l1s8nqb20ywmw2zqizczch2p62qf9apyx5wqgxlnjshk"))
                (file-name (string-append name "-"version "-checkout"))))
      (build-system python-build-system)
      (inputs `(("python-notmuch" ,python2-notmuch)))
      (arguments
       `(#:tests? #f                             ;no "test" target
         #:python ,python-2))                    ;not compatible with Python 3
      (synopsis "Patch tracking tool")
      (description
       "'Patches' is a patch-tracking tool initially written for the QEMU
project.  It provides commands that build a database of patches from a mailing
list, and commands that can search that database.  It allows users to track
the status of a patch, apply patches, and search for patches---all that from
the command-line or from Emacs via its Notmuch integration.")
      (license gpl2+))))

(define-public vbindiff
  (package
    (name "vbindiff")
    (version "3.0_beta5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.cjmweb.net/vbindiff/vbindiff-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1f1kj4jki08bnrwpzi663mjfkrx4wnfpzdfwd2qgijlkx5ysjkgh"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)))
    (home-page "https://www.cjmweb.net/vbindiff/")
    (synopsis "Console-based tool for comparing binary data")
    (description "Visual Binary Diff (@command{vbindiff}) displays files in
hexadecimal and ASCII (or EBCDIC).  It can also display two files at once, and
highlight the differences between them.  It works well with large files (up to 4
GiB).")
    (license gpl2+)))
