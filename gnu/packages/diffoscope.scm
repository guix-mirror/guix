;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@reproducible-builds.org>
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

(define-module (gnu packages diffoscope)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages android)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages file)      ;for 'file-next'
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mono)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pascal)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

(define-public diffoscope
  (package
    (name "diffoscope")
    (version "199")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://salsa.debian.org/reproducible-builds/diffoscope.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wy1nd52a4rzqhhdcdi3pfgrixz3w3q7qfc9mpc9zraq5cj94l7n"))
       (patches
        (search-patches "diffoscope-fix-llvm-test.patch"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  ;; These tests are broken because our `file` package has a
                  ;; bug in berkeley-db and wasm file type detection.
                  (add-after 'unpack 'remove-broken-file-type-detection-test
                    (lambda _
                      (delete-file "tests/comparators/test_berkeley_db.py")
                      (delete-file "tests/comparators/test_wasm.py")))
                  (add-after 'unpack 'embed-tool-references
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "diffoscope/comparators/utils/compare.py"
                        (("\\['xxd',")
                         (string-append "['" (which "xxd") "',")))
                      (substitute* "diffoscope/comparators/elf.py"
                        (("@tool_required\\('readelf'\\)") "")
                        (("get_tool_name\\('readelf'\\)")
                         (string-append "'" (which "readelf") "'")))
                      (substitute* "diffoscope/comparators/directory.py"
                        (("@tool_required\\('stat'\\)") "")
                        (("@tool_required\\('getfacl'\\)") "")
                        (("\\['stat',")
                         (string-append "['" (which "stat") "',"))
                        (("\\['getfacl',")
                         (string-append "['" (which "getfacl") "',")))))
                  (add-after 'build 'build-man-page
                    (lambda* (#:key (make-flags '()) #:allow-other-keys)
                      (apply invoke "make" "-C" "doc" make-flags)))
                  (add-before 'check 'writable-test-data
                    (lambda _
                      ;; Tests may need write access to tests directory.
                      (for-each make-file-writable (find-files "tests"))))
                  (add-before 'check 'fix-failing-test
                    (lambda _
                      ;; There is no user name mapping in the build environment.
                      ;; Pytest made it so much harder than should be necessary,
                      ;; so I'm leaving… this here in case I ever need it again:
                      ;; (substitute* "tests/comparators/test_squashfs.py"
                      ;;   (("^def test_symlink_root.*" match)     ; no, I don't
                      ;;    (string-append                         ; know Python
                      ;;     match "\n    raise ValueError("       ; why do you
                      ;;     "differences_root[1].unified_diff)\n"))) ; ask
                      (substitute* "tests/data/squashfs_root_expected_diff"
                        (("root/root")
                         '"0/0      "))))
                  (add-before 'check 'delete-failing-test
                    ;; Please add new tests to fix-failing-test and not here ;-)
                    (lambda _
                      ;; This requires /sbin to be in $PATH.
                      (delete-file "tests/test_tools.py")))
                  (add-after 'install 'install-man-page
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (man (string-append out "/share/man/man1")))
                        (install-file "doc/diffoscope.1" man)))))))
    (inputs (list rpm ;for rpm-python
                  python-debian
                  python-libarchive-c
                  python-magic
                  python-tlsh
                  acl ;for getfacl
                  colordiff
                  xxd))
    (native-inputs
     (append
       (list help2man

             ;; Below are packages used for tests.
             binwalk
             python-pytest
             python-chardet
             python-h5py
             python-pypdf2
             python-progressbar33

             abootimg
             bdb
             binutils
             bzip2
             cdrtools
             colord
             cpio
             docx2txt
             dtc
             e2fsprogs
             ffmpeg

             ;; XXX: Must be the same version as python-magic uses;
             ;; remove when 'file' is updated.
             file-next

             fpc
             gettext-minimal
             ghostscript
             `(,giflib "bin")
             gnumeric
             gnupg
             hdf5
             imagemagick
             libarchive
             llvm
             lz4
             mono
             ocaml
             odt2txt
             openssh
             openssl
             pgpdump
             poppler
             python-jsbeautifier
             r-minimal
             rpm
             sng
             sqlite
             squashfs-tools
             tcpdump
             unzip
             wabt
             xxd
             xz
             zip
             zstd)

       ;; Also for tests.  The test suite skips tests when these are missing.
       (match (%current-system)
         ;; ghc is only available on x86 currently.
         ((or "x86_64-linux" "i686-linux")
          (list ghc))
         (_ '()))
       (match (%current-system)
         ;; openjdk and dependent packages are only
         ;; available on x86_64 currently.
         ((or "x86_64-linux")
          (list enjarify)
          ;; No unversioned openjdk available.
          (list `(,openjdk12 "jdk")))
         (_ '()))))
    (home-page "https://diffoscope.org/")
    (synopsis "Compare files, archives, and directories in depth")
    (description
     "Diffoscope tries to get to the bottom of what makes files or directories
different.  It recursively unpacks archives of many kinds and transforms
various binary formats into more human readable forms to compare them.  It can
compare two tarballs, ISO images, or PDFs just as easily.

Diffoscope has many optional dependencies; @code{diffoscope
--list-missing-tools guix} will display optional packages to
install.")
    (license license:gpl3+)))

(define-public reprotest
  (package
    (name "reprotest")
    (version "0.7.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://salsa.debian.org/reproducible-builds/reprotest.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "19lwsxq53isgfkvlxvxqqmbjfcim3lhcxwk7m9ddfjiynhq74949"))))
    (inputs
     (list python-debian python-distro python-libarchive-c python-rstr))
    (native-inputs
     `(("diffoscope" ,diffoscope)
       ("help2man" ,help2man)
       ("libfaketime" ,libfaketime)
       ("python-coverage" ,python-coverage)
       ("python-docutils" ,python-docutils)
       ("python-magic " ,python-magic)
       ("python-pytest " ,python-pytest)
       ("python-tlsh" ,python-tlsh)
       ("python-tox" ,python-tox)
       ("unzip" ,unzip)
       ("xxd" ,xxd)))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Neither glibc-locales nor glibc-utf8-locales have the C.UTF-8
         ;; locale or several other locales used in reprotest.
         (add-after 'unpack 'adjust-locales
           (lambda _
             (substitute* "reprotest/build.py"
               (("'C.UTF-8'") "'en_US.UTF-8'")
               (("'ru_RU.CP1251'") "'ru_RU.KOI8-R'")
               (("'kk_KZ.RK1048'") "'kk_KZ'"))
             (substitute* "reprotest/lib/adt_testbed.py"
               (("export LANG=C.UTF-8") "export LANG=en_US.UTF-8"))
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((mandir1 (string-append
                              (assoc-ref outputs "out") "/share/man/man1"))
                    (docdir (string-append
                             (assoc-ref outputs "out") "/share/doc/" ,name "-" ,version)))
               (invoke "make" "-C" "doc")
               (mkdir-p mandir1)
               (install-file "doc/reprotest.1" mandir1)
               (mkdir-p docdir)
               (install-file "./README.rst" docdir)
               (install-file "./README-dev.rst" docdir))
             #t)))))
    (home-page "https://salsa.debian.org/reproducible-builds/reprotest")
    (synopsis "Build software and check it for reproducibility")
    (description "Reprotest builds the same source code twice in different
environments, and then checks the binaries produced by each build for
differences.  If any are found, then diffoscope or diff is used to display
them in detail for later analysis.")
    (license (list license:gpl3+ license:gpl2+))))

(define-public trydiffoscope
 (package
   (name "trydiffoscope")
   (version "67.0.1")
   (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://salsa.debian.org/reproducible-builds/trydiffoscope.git")
            (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "03b66cjii7l2yiwffj6ym6mycd5drx7prfp4j2550281pias6mjh"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((share (string-append (assoc-ref outputs "out") "/share/")))
               (mkdir-p (string-append share "/man/man1/" ))
               (invoke "rst2man.py"
                       "trydiffoscope.1.rst"
                       (string-append share "/man/man1/trydiffoscope.1"))
               (mkdir-p (string-append share "/doc/" ,name "-" ,version))
               (install-file "./README.rst"
                          (string-append share "/doc/" ,name "-" ,version)))
             #t)))))
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list gzip python-docutils))
    (build-system python-build-system)
    (home-page "https://try.diffoscope.org")
    (synopsis "Client for remote diffoscope service")
    (description "This is a client for the @url{https://try.diffoscope.org,
remote diffoscope service}.

Diffoscope tries to get to the bottom of what makes files or directories
different.  It recursively unpacks archives of many kinds and transforms
various binary formats into more human readable forms to compare them.  It can
compare two tarballs, ISO images, or PDFs just as easily.

Results are displayed by default, stored as local text or html files, or made
available via a URL on @url{https://try.diffoscope.org}.  Results stored on the
server are purged after 30 days.")
    (license license:gpl3+)))
