;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages cdrom)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (lgpl2.1+ gpl2 gpl2+ gpl3+))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages which))

(define-public libcddb
  (package
    (name "libcddb")
    (version "1.3.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/libcddb/libcddb-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1y8bfy12dwm41m1jahayn3v47dm34fmz7m9cjxyh7xcw6fp3lzaf"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))      ; tests rely on access to external servers
    (home-page "http://libcddb.sourceforge.net/")
    (synopsis "C library to access data on a CDDB server")
    (description
     "Libcddb is a C library to access data on a CDDB server (freedb.org).  It
allows you to:

 1. search the database for possible CD matches;

 2. retrieve detailed information about a specific CD;

 3. submit new CD entries to the database.

Libcddb supports both the custom CDDB protocol and tunnelling the query and
read operations over plain HTTP.  It is also possible to use an HTTP proxy
server.  If you want to speed things up, you can make use of the built-in
caching facility provided by the library.")
    (license lgpl2.1+)))

(define-public libcdio
  (package
    (name "libcdio")
    (version "0.92")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/libcdio/libcdio-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1b9zngn8nnxb1yyngi1kwi73nahp4lsx59j17q1bahzz58svydik"))))
    (build-system gnu-build-system)
    (inputs
       `(("ncurses" ,ncurses)
         ("libcddb" ,libcddb)))
    (native-inputs
     `(("help2man" ,help2man)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.gnu.org/software/libcdio/")
    (synopsis "CD Input and Control library")
    (description
     "The GNU Compact Disc Input and Control Library (libcdio) is a library
for CD-ROM and CD image file access.  It allows the developer to add CD
access to an application without having to worry about the OS- and
device-dependent properties of CD-ROM or the specific details of CD image
formats.  It includes pycdio, a Python interface to libcdio, and
libcdio-paranoia, a library providing jitter-free and error-free audio
extraction from CDs.")
    (license gpl3+)))

(define-public xorriso
  (package
    (name "xorriso")
    (version "1.3.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/xorriso/xorriso-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0zhhj9lr9z7hnb2alac54mc28w1l0mbanphhpmy3ylsi8rih84lh"))))
    (build-system gnu-build-system)
    (inputs
     `(("acl" ,acl)
       ("readline" ,readline)
       ("bzip2" ,bzip2)
       ("zlib" ,zlib)
       ("libcdio" ,libcdio)))
    (home-page "http://www.gnu.org/software/xorriso/")
    (synopsis "Create, manipulate, burn ISO-9660 filesystems")
    (description
     "GNU Xorriso is a tool for copying files to and from ISO 9660 Rock
Ridge, a.k.a. Compact Disc File System, filesystems and it allows
session-wise manipulation of them.  It features a formatter and burner for
CD, DVD and BD.  It can operate on existing ISO images or it can create new
ones.  xorriso can then be used to copy files directly into or out of ISO
files.")
    (license gpl3+)))

(define-public cdparanoia
  (package
    (name "cdparanoia")
    (version "10.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://downloads.xiph.org/releases/cdparanoia/cdparanoia-III-"
                                 version ".src.tgz"))
             (sha256
              (base32
               "1pv4zrajm46za0f6lv162iqffih57a8ly4pc69f7y0gfyigb8p80"))
             (patches (list (search-patch "cdparanoia-fpic.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("patchelf" ,patchelf)))
    (arguments
     `(#:tests? #f ; there is no check target
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build rpath)
                  (srfi srfi-26))
       #:imported-modules ((guix build gnu-build-system)
                           (guix build utils)
                           (guix build rpath))
       #:phases
        (alist-cons-after
         'strip 'add-lib-to-runpath
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (lib (string-append out "/lib")))
             ;; Add LIB to the RUNPATH of all the executables.
             (with-directory-excursion out
               (for-each (cut augment-rpath <> lib)
                         (find-files "bin" ".*")))))
         %standard-phases)))
    (home-page "http://www.xiph.org/paranoia/")
    (synopsis "Audio CD reading utility")
    (description "Cdparanoia retrieves audio tracks from CDDA capable CDROM
drives.  The data can be saved to a file or directed to standard output
in WAV, AIFF, AIFF-C or raw format.  Most ATAPI, SCSI and several
proprietary CDROM drive makes are supported; cdparanoia can determine if the
target drive is CDDA capable.  In addition to simple reading, cdparanoia adds
extra-robust data verification, synchronization, error handling and scratch
reconstruction capability.")
    (license gpl2))) ; libraries under lgpl2.1

(define-public dvdisaster
  (package
    (name "dvdisaster")
    (version "0.72.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://dvdisaster.net/downloads/dvdisaster-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0sqrprc5rh3shnfli25m2wy0i5f83db54iv04s5s7bxf77m7sy79"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtk+" ,gtk+-2)))
    (native-inputs
     `(("gettext" ,gnu-gettext)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (arguments
     `(;; Parallel builds appear to be unsafe, see
       ;; <http://hydra.gnu.org/build/49331/nixlog/1/raw>.
       #:parallel-build? #f
       #:tests? #f ; no check target
       #:phases
         (alist-cons-before
          'patch-source-shebangs 'sanitise
          (lambda _
            ;; delete dangling symlink
            (delete-file ".#GNUmakefile"))
          %standard-phases)))
    (home-page "http://dvdisaster.net/en/index.html")
    (synopsis "Error correcting codes for optical media images")
    (description "Optical media (CD,DVD,BD) keep their data only for a
finite time (typically for many years).  After that time, data loss develops
slowly with read errors growing from the outer media region towards the
inside.

Dvdisaster stores data on CD/DVD/BD (supported media) in a way that it is
fully recoverable even after some read errors have developed.  This enables
you to rescue the complete data to a new medium.

Data loss is prevented by using error correcting codes.  Error correction
data is either added to the medium or kept in separate error correction
files.  Dvdisaster works at the image level so that the recovery does not
depend on the file system of the medium.  The maximum error correction
capacity is user-selectable.")
    (license gpl2+)))
