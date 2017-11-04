;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
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
    (synopsis "FUSE filesystem that introduces non-determinism")
    (description
     "An overlay FUSE filesystem that introduces non-determinism
into filesystem metadata.  For example, it can randomize the order
in which directory entries are read.  This is useful for detecting
non-determinism in the build process.")
    (license license:gpl3+)))

