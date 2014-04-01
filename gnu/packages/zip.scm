;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages zip)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public zip
  (package
    (name "zip")
    (version "3.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/infozip/Zip%203.x%20%28latest%29/3.0/zip30.tar.gz"))
      (sha256
       (base32
        "0sb3h3067pzf3a7mlxn1hikpcjrsvycjcnj9hl9b1c3ykcgvps7h"))))
    (build-system gnu-build-system)
    (inputs `(("bzip2" ,bzip2)))
    (arguments
     `(#:tests? #f ; no test target
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "-f" "unix/Makefile"
                            (string-append "prefix=" out)
                            (string-append "MANDIR=" out "/share/man/man1")))
       #:phases (alist-replace
                 'build
                 (lambda* (#:key (make-flags '()) #:allow-other-keys)
                   (zero? (apply system* "make" "generic_gcc" make-flags)))
                 (alist-delete 'configure %standard-phases))))
    (home-page "http://www.info-zip.org/Zip.html")
    (synopsis "Zip compression and file packing utility")
    (description
     "Zip is a compression and file packaging/archive utility. Zip is useful
for packaging a set of files for distribution, for archiving files, and for
saving disk space by temporarily compressing unused files or directories.
Zip puts one or more compressed files into a single ZIP archive, along with
information about the files (name, path, date, time of last modification,
protection, and check information to verify file integrity). An entire
directory structure can be packed into a ZIP archive with a single command.

Zip has one compression method (deflation) and can also store files without
compression. Zip automatically chooses the better of the two for each file.
Compression ratios of 2:1 to 3:1 are common for text files.")
  (license (license:bsd-style "file://LICENSE"
                               "See LICENSE in the distribution."))))

(define-public unzip
  (package (inherit zip)
    (name "unzip")
    (version "6.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/infozip/UnZip%206.x%20%28latest%29/UnZip%206.0/unzip60.tar.gz"))
      (sha256
       (base32
        "0dxx11knh3nk95p2gg2ak777dd11pr7jx5das2g49l262scrcv83"))))
    (build-system gnu-build-system)
    ;; no inputs; bzip2 is not supported, since not compiled with BZ_NO_STDIO
    (arguments
     `(#:make-flags '("generic_gcc")
       #:phases
        (alist-replace
         'configure
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out")))
             (copy-file "unix/Makefile" "Makefile")
             (substitute* "Makefile"
               (("/usr/local") out)
               (("/man/") "/share/man/"))))
        %standard-phases)))
    (home-page "http://www.info-zip.org/UnZip.html")
    (synopsis "Unzip decompression and file extraction utility")
    (description
     "UnZip is an extraction utility for archives compressed in .zip format
(also called \"zipfiles\").

UnZip lists, tests, or extracts files from a .zip archive. The default
behaviour (with no options) is to extract into the current directory
(and subdirectories below it) all files from the specified zipfile.
UnZip recreates the stored directory structure by default.")
  (license (license:bsd-style "file://LICENSE"
                               "See LICENSE in the distribution."))))

(define-public zziplib
  (package
    (name "zziplib")
    (version "0.13.62")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/zziplib/zziplib13/"
                          version "/zziplib-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "0nsjqxw017hiyp524p9316283jlf5piixc1091gkimhz38zh7f51"))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs `(("perl" ,perl)     ; for the documentation
                     ("pkg-config" ,pkg-config)
                     ("python" ,python-2) ; for the documentation; Python 3 not supported,
                                        ; http://forums.gentoo.org/viewtopic-t-863161-start-0.html
                     ("zip" ,zip))) ; to create test files
    (arguments
     `(#:parallel-tests? #f)) ; since test files are created on the fly
    (home-page "http://zziplib.sourceforge.net/")
    (synopsis "zziplib, a library for accessing zip files")
    (description
     "zziplib is a library based on zlib for accessing zip files.")
    (license license:lgpl2.0+)))
