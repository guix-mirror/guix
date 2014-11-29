;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
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

(define-module (gnu packages fonts)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((gnu packages base)
                #:select (tar))
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pkg-config))

(define-public font-dejavu
  (package
    (name "font-dejavu")
    (version "2.34")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/dejavu/"
                                 version "/dejavu-fonts-ttf-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0pgb0a3ngamidacmrvasg51ck3gp8gn93w6sf1s8snwzx4x2r9yh"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))

                   (let ((tar      (string-append (assoc-ref %build-inputs
                                                             "tar")
                                                  "/bin/tar"))
                         (PATH     (string-append (assoc-ref %build-inputs
                                                             "bzip2")
                                                  "/bin"))
                         (font-dir (string-append
                                    %output "/share/fonts/truetype"))
                         (conf-dir (string-append
                                    %output "/share/fontconfig/conf.avail"))
                         (doc-dir  (string-append
                                    %output "/share/doc/" ,name "-" ,version)))
                     (setenv "PATH" PATH)
                     (system* tar "xvf" (assoc-ref %build-inputs "source"))

                     (mkdir-p font-dir)
                     (mkdir-p conf-dir)
                     (mkdir-p doc-dir)
                     (chdir (string-append "dejavu-fonts-ttf-" ,version))
                     (for-each (lambda (ttf)
                                 (copy-file ttf
                                            (string-append font-dir "/"
                                                           (basename ttf))))
                               (find-files "ttf" "\\.ttf$"))
                     (for-each (lambda (conf)
                                 (copy-file conf
                                            (string-append conf-dir "/"
                                                           (basename conf))))
                               (find-files "fontconfig" "\\.conf$"))
                     (for-each (lambda (doc)
                                 (copy-file doc
                                            (string-append doc-dir "/"
                                                           (basename doc))))
                               (find-files "." "\\.txt$|^[A-Z][A-Z]*$"))))))
    (native-inputs `(("source" ,source)
                     ("tar" ,tar)
                     ("bzip2" ,bzip2)))
    (home-page "http://dejavu-fonts.org/")
    (synopsis "Vera font family derivate with additional characters")
    (description "DejaVu provides an expanded version of the Vera font family
aiming for quality and broader Unicode coverage while retaining the original
Vera style.  DejaVu currently works towards conformance with the Multilingual
European Standards (MES-1 and MES-2) for Unicode coverage.  The DejaVu fonts
provide serif, sans and monospaced variants.")
    (license
     (license:x11-style
      "http://dejavu-fonts.org/"))))

(define-public font-bitstream-vera
  (package
    (name "font-bitstream-vera")
    (version "1.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/ttf-bitstream-vera/"
                                 version "/ttf-bitstream-vera-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1p3qs51x5327gnk71yq8cvmxc6wgx79sqxfvxcv80cdvgggjfnyv"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let ((tar      (string-append (assoc-ref %build-inputs
                                                             "tar")
                                                  "/bin/tar"))
                         (PATH     (string-append (assoc-ref %build-inputs
                                                             "bzip2")
                                                  "/bin"))
                         (font-dir (string-append %output
                                                  "/share/fonts/truetype"))
                         (doc-dir  (string-append %output "/share/doc/"
                                                  ,name "-" ,version)))
                     (setenv "PATH" PATH)
                     (system* tar "xvf" (assoc-ref %build-inputs "source"))

                     (mkdir-p font-dir)
                     (mkdir-p doc-dir)
                     (chdir (string-append "ttf-bitstream-vera-" ,version))
                     (for-each (lambda (ttf)
                                 (copy-file ttf
                                            (string-append font-dir "/" ttf)))
                               (find-files "." "\\.ttf$"))
                     (for-each (lambda (doc)
                                 (copy-file doc
                                            (string-append doc-dir "/" doc)))
                               (find-files "." "\\.TXT$"))))))
    (native-inputs `(("source" ,source)
                     ("tar" ,tar)
                     ("bzip2" ,bzip2)))
    (home-page "https://www-old.gnome.org/fonts/")
    (synopsis "Bitstream Vera sans-serif typeface")
    (description "Vera is a sans-serif typeface from Bitstream, Inc.  This
package provides the TrueType (TTF) files.")
    (license
     (license:x11-style
      "https://www-old.gnome.org/fonts/#Final_Bitstream_Vera_Fonts"))))

(define-public font-gnu-freefont-ttf
  (package
    (name "font-gnu-freefont-ttf")
    (version "20100919")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/freefont/freefont-ttf-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1q3h5jp1mbdkinkwxy0lfd0a1q7azlbagraydlzaa2ng82836wg4"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let ((tar      (string-append (assoc-ref %build-inputs
                                                             "tar")
                                                  "/bin/tar"))
                         (PATH     (string-append (assoc-ref %build-inputs
                                                             "gzip")
                                                  "/bin"))
                         (font-dir (string-append %output
                                                  "/share/fonts/truetype"))
                         (doc-dir  (string-append %output "/share/doc/"
                                                  ,name "-" ,version)))
                     (setenv "PATH" PATH)
                     (system* tar "xvf" (assoc-ref %build-inputs "source"))

                     (mkdir-p font-dir)
                     (mkdir-p doc-dir)
                     (chdir (string-append "freefont-" ,version))
                     (for-each (lambda (file)
                                 (let ((dir (if (string-suffix? "ttf" file)
                                                font-dir
                                                doc-dir)))
                                   (copy-file file
                                              (string-append dir "/" file))))
                               (find-files "." ""))))))
    (native-inputs `(("source" ,source)
                     ("tar" ,tar)
                     ("gzip" ,gzip)))
    (home-page "http://www.gnu.org/software/freefont/")
    (synopsis "Unicode-encoded outline fonts")
    (description
     "The GNU Freefont project aims to provide a set of free outline
 (PostScript Type0, TrueType, OpenType...) fonts covering the ISO
10646/Unicode UCS (Universal Character Set).")
   (license license:gpl3+)))

(define-public font-liberation
  (package
    (name "font-liberation")
    (version "2.00.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://fedorahosted.org/releases/l/i/"
                                  "liberation-fonts/liberation-fonts-ttf-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "010m4zfqan4w04b6bs9pm3gapn9hsb18bmwwgp2p6y6idj52g43q"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (let ((tar      (string-append (assoc-ref %build-inputs "tar")
                                        "/bin/tar"))
               (PATH     (string-append (assoc-ref %build-inputs "gzip")
                                        "/bin"))
               (font-dir (string-append %output "/share/fonts/truetype"))
               (doc-dir  (string-append %output "/share/doc/" ,name)))
           (setenv "PATH" PATH)
           (system* tar "xvf" (assoc-ref %build-inputs "source"))
           (mkdir-p font-dir)
           (mkdir-p doc-dir)
           (chdir (string-append "liberation-fonts-ttf-" ,version))
           (for-each (lambda (ttf)
                       (copy-file ttf
                                  (string-append font-dir "/"
                                                 (basename ttf))))
                     (find-files "." "\\.ttf$"))
           (for-each (lambda (doc)
                       (copy-file doc
                                  (string-append doc-dir "/"
                                                 (basename doc))))
                     '("AUTHORS" "ChangeLog" "LICENSE" "README" "TODO"))))))
    (native-inputs
     `(("source" ,source)
       ("tar" ,tar)
       ("gzip" ,gzip)))
    (home-page "https://fedorahosted.org/liberation-fonts/")
    (synopsis
     "Fonts compatible with Arial, Times New Roman, and Courier New")
    (description
     "The Liberation font family aims at metric compatibility with
Arial, Times New Roman, and Courier New.

There are three sets:

- Sans (a substitute for Arial, Albany, Helvetica, Nimbus Sans L, and
Bitstream Vera Sans);

- Serif (a substitute for Times New Roman, Thorndale, Nimbus Roman, and
Bitstream Vera Serif);

- Mono (a substitute for Courier New, Cumberland, Courier, Nimbus Mono L,
and Bitstream Vera Sans Mono).

The Liberation Fonts are sponsored by Red Hat.")
    (license license:silofl1.1)))

(define-public font-terminus
  (package
    (name "font-terminus")
    (version "4.39")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://sourceforge/project/terminus-font/terminus-font-"
               version
               "/terminus-font-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1gzmn7zakvy6yrvmswyjfklnsvqrjm0imhq8rjws8rdkhqwkh21i"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ("bdftopcf" ,bdftopcf)
       ("font-util", font-util)
       ("mkfontdir" ,mkfontdir)))
    (arguments
     `(#:configure-flags (list
                          ;; install fonts into subdirectory of package output
                          ;; instead of font-util-?.?.?/share/fonts/X11
                          (string-append "--with-fontrootdir="
                                         %output "/share/fonts/X11"))
       #:tests? #f)) ;; No test target in tarball
    (home-page "http://terminus-font.sourceforge.net/")
    (synopsis "Simple bitmap programming font")
    (description "Terminus Font is a clean, fixed width bitmap font, designed
for long (8 and more hours per day) work with computers.")
    (license license:silofl1.1)))
