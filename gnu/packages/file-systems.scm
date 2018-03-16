;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages file-systems)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public httpfs2
  (package
    (name "httpfs2")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/httpfs/" name "/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "1h8ggvhw30n2r6w11n1s458ypggdqx6ldwd61ma4yd7binrlpjq1"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("docbook-xml" ,docbook-xml)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("fuse" ,fuse)
       ("gnutls" ,gnutls)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'install
           ;; There's no ‘install’ target. Install all variants manually.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1")))
               (mkdir-p bin)
               (mkdir-p man1)
               (for-each
                (lambda (variant)
                  (let ((man1-page (string-append variant ".1")))
                    (install-file variant bin)
                    (install-file man1-page man1)))
                (list "httpfs2"
                      "httpfs2-mt"
                      "httpfs2-ssl"
                      "httpfs2-ssl-mt")))
             #t)))
       #:make-flags (list "CC=gcc")
       #:parallel-build? #f             ; can result in missing man pages
       #:tests? #f))                    ; no tests
    (home-page "https://sourceforge.net/projects/httpfs/")
    (synopsis "Mount remote files over HTTP")
    (description "httpfs2 is a @code{fuse} file system for mounting any
@dfn{HyperText} (HTTP or HTTPS) URL.  It uses HTTP/1.1 byte ranges to request
arbitrary bytes from the web server, without needing to download the entire
file.  This is particularly useful with large archives such as ZIP files and
ISO images when you only need to inspect their contents or extract specific
files.  Since the HTTP protocol itself has no notion of directories, only a
single file can be mounted.")
    (license license:gpl2+)))

(define-public disorderfs
  (package
    (name "disorderfs")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ReproducibleBuilds/disorderfs.git")
             (commit "0.5.2")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1j028dq3d4m64mn9xmfamcnnc7i2drmra4pdmxdmqdsi8p7yj4sv"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fuse" ,fuse)
       ("attr" ,attr)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)))
       #:test-target "test"
       ;; FIXME: Tests require 'run-parts' which is not in Guix yet.
       #:tests? #f))
    (home-page "https://github.com/ReproducibleBuilds/disorderfs")
    (synopsis "FUSE file system that introduces non-determinism")
    (description
     "An overlay FUSE file system that introduces non-determinism
into file system metadata.  For example, it can randomize the order
in which directory entries are read.  This is useful for detecting
non-determinism in the build process.")
    (license license:gpl3+)))

(define-public glusterfs
  (package
    (name "glusterfs")
    (version "3.10.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.gluster.org/pub/gluster/glusterfs/"
                           (version-major+minor version) "/" version
                           "/glusterfs-" version ".tar.gz"))
       (sha256
        (base32
         "02sn9s3jjva2i1l47y3in326n8jgp57rbykz5s8m87y4bzpw0ym1"))
       (patches
        (search-patches "glusterfs-use-PATH-instead-of-hardcodes.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "--with-initdir=" out "/etc/init.d")
               (string-append "--with-mountutildir=" out "/sbin")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'replace-config.sub
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The distributed config.sub is intentionally left empty and
             ;; must be replaced.
             (install-file (string-append (assoc-ref inputs "automake")
                                          "/share/automake-"
                                          ,(package-version automake) "/config.sub")
                           ".")
             #t))
         ;; Fix flex error.  This has already been fixed with upstream commit
         ;; db3fe245a9e8812829eae7d143e49d0bfdfef9a7.
         (add-before 'configure 'fix-lex
           (lambda _
             (substitute* "libglusterfs/src/Makefile.in"
               (("libglusterfs_la_LIBADD = @LEXLIB@")
                "libglusterfs_la_LIBADD ="))
             #t)))))
    (native-inputs
     `(("cmocka" ,cmocka)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2) ; must be version 2
       ("flex" ,flex)
       ("bison" ,bison)
       ("automake" ,automake)))
    (inputs
     `(("acl" ,acl)
       ;; GlusterFS fails to build with libressl because HMAC_CTX_new and
       ;; HMAC_CTX_free are undefined.
       ("openssl" ,openssl)
       ("liburcu" ,liburcu)
       ("libuuid" ,util-linux)
       ("libxml2" ,libxml2)
       ("lvm2" ,lvm2)
       ("readline" ,readline)
       ("sqlite" ,sqlite) ; for tiering
       ("zlib" ,zlib)))
    (home-page "https://www.gluster.org")
    (synopsis "Distributed file system")
    (description "GlusterFS is a distributed scalable network file system
suitable for data-intensive tasks such as cloud storage and media streaming.
It allows rapid provisioning of additional storage based on your storage
consumption needs.  It incorporates automatic failover as a primary feature.
All of this is accomplished without a centralized metadata server.")
    ;; The user may choose either LGPLv3+ or GPLv2 only.
    (license (list license:lgpl3+ license:gpl2+))))

(define-public curlftpfs
  (package
    (name "curlftpfs")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/curlftpfs/curlftpfs/" version
                           "/curlftpfs-" version ".tar.gz"))
       (sha256
        (base32
         "0n397hmv21jsr1j7zx3m21i7ryscdhkdsyqpvvns12q7qwwlgd2f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test
           (lambda _
             ;; One of the 512-Byte block counts is definitely wrong.
             ;; See <https://sourceforge.net/p/curlftpfs/bugs/73/>.
             (substitute* "tests/ftpfs-ls_unittest.c"
              (("4426192") "12814800"))
             #t)))))
    (inputs
     `(("curl" ,curl)
       ("glib" ,glib)
       ("fuse" ,fuse)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://curlftpfs.sourceforge.net/")
    (synopsis "Mount remote file systems over FTP")
    (description
     "This is a file system client based on the FTP File Transfer Protocol.")
    (license license:gpl2+)))
