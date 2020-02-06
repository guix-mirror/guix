;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages mono)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages textutils)
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
  (let ((version "136"))
    (package
      (name "diffoscope")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://salsa.debian.org/reproducible-builds/diffoscope.git")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1wp4fnmwcsgv17dmvk9xr3h63gp4nmmpysr248qvxs8s5qy5xlyk"))))
      (build-system python-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    ;; setup.py mistakenly requires python-magic from PyPi, even
                    ;; though the Python bindings of `file` are sufficient.
                    ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=815844
                    (add-after 'unpack 'dependency-on-python-magic
                      (lambda _
                        (substitute* "setup.py"
                          (("'python-magic',") ""))))
                    ;; This test is broken because our `file` package has a
                    ;; bug in berkeley-db file type detection.
                    (add-after 'unpack 'remove-berkeley-test
                      (lambda _
                        (delete-file "tests/comparators/test_berkeley_db.py")
                        #t))
                    ;; Test is dynamically generated and may have false
                    ;; negatives with different ocaml versions.  Further
                    ;; background in: https://bugs.debian.org/939386
                    (add-after 'unpack 'remove-ocaml-test
                      (lambda _
                        (substitute* "tests/comparators/test_ocaml.py"
                          (("def test_diff.differences.:")
                           "def skip_test_diff(differences):"))
                        #t))
                    (add-after 'unpack 'skip-elf-tests
                      ;; FIXME: libmix_differences test added in 125, and is
                      ;; failing, need to explore why...
                      (lambda _
                        (substitute* "tests/comparators/test_elf.py"
                          (("def test_libmix_differences.libmix_differences.:")
                           "def skip_test_libmix_differences(libmix_differences):"))
                        #t))
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
                           (string-append "['" (which "getfacl") "',")))
                        #t))
                    (add-before 'check 'writable-test-data
                      (lambda _
                        ;; tests may need needs write access to tests
                        ;; directory
                        (for-each make-file-writable (find-files "tests"))
                        #t))
                    (add-before 'check 'delete-failing-test
                      (lambda _
                        ;; this requires /sbin to be on the path
                        (delete-file "tests/test_tools.py")
                        #t)))))
      (inputs `(("rpm" ,rpm)                        ;for rpm-python
                ("python-file" ,python-file)
                ("python-debian" ,python-debian)
                ("python-libarchive-c" ,python-libarchive-c)
                ("python-tlsh" ,python-tlsh)
                ("acl" ,acl)                        ;for getfacl
                ("colordiff" ,colordiff)
                ("xxd" ,xxd)))
      ;; Below are modules used for tests.
      (native-inputs `(("python-pytest" ,python-pytest)
                       ("python-chardet" ,python-chardet)
                       ("python-binwalk" ,python-binwalk)
                       ("python-pypdf2" ,python-pypdf2)
                       ("python-progressbar33" ,python-progressbar33)
                       ;; test suite skips tests when tool is missing
                       ,@(match (%current-system)
                                ;; ghc is only available on x86 currently.
                                ((or "x86_64-linux" "i686-linux")
                                 `(("ghc" ,ghc)))
                                (_
                                 `()))
                       ,@(match (%current-system)
                                ;; openjdk and dependent packages are only
                                ;; available on x86_64 currently.
                                ((or "x86_64-linux")
                                 `(("enjarify" ,enjarify)
                                   ;; no unversioned openjdk available
                                   ("openjdk:jdk" ,openjdk12 "jdk")
                                   ))
                                (_
                                 `()))
                       ("abootimg" ,abootimg)
                       ("bdb" ,bdb)
                       ("binutils" ,binutils)
                       ("bzip2" ,bzip2)
                       ("cdrtools" ,cdrtools)
                       ("colord" ,colord)
                       ("cpio" ,cpio)
                       ("docx2txt" ,docx2txt)
                       ("dtc" ,dtc)
                       ("e2fsprogs" ,e2fsprogs)
                       ("ffmpeg" ,ffmpeg)
                       ("gettext" ,gettext-minimal)
                       ("ghostscript" ,ghostscript)
                       ("giflib:bin" ,giflib "bin")
                       ("gnumeric" ,gnumeric)
                       ("gnupg" ,gnupg)
                       ("imagemagick" ,imagemagick)
                       ("libarchive" ,libarchive)
                       ("llvm" ,llvm)
                       ("lz4" ,lz4)
                       ("mono" ,mono)
                       ("ocaml" ,ocaml)
                       ("odt2txt" ,odt2txt)
                       ("openssh" ,openssh)
                       ("pgpdump" ,pgpdump)
                       ("poppler" ,poppler)
                       ("python-jsbeautifier" ,python-jsbeautifier)
                       ("r-minimal" ,r-minimal)
                       ("rpm" ,rpm)
                       ("sng" ,sng)
                       ("sqlite" ,sqlite)
                       ("squashfs-tools" ,squashfs-tools)
                       ("tcpdump" ,tcpdump)
                       ("unzip" ,unzip)
                       ("wabt" ,wabt)
                       ("xxd" ,xxd)
                       ("xz" ,xz)
                       ("zip" ,zip)
                       ("zstd" ,zstd)))
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
      (license license:gpl3+))))

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
     `(("python-requests" ,python-requests)))
    (native-inputs
     `(("gzip" ,gzip)
       ("python-docutils" ,python-docutils)))
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
