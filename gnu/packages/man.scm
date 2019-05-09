;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2014, 2015, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2015, 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages man)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages less)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux))

(define-public libpipeline
  (package
    (name "libpipeline")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://savannah/libpipeline/libpipeline-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0bwh5xz5f2czwb7f564jz1mp4znm8pldnvf65fs0hpw4gmmp0cyn"))))
    (build-system gnu-build-system)
    (home-page "http://libpipeline.nongnu.org/")
    (synopsis "C library for manipulating pipelines of subprocesses")
    (description
     "libpipeline is a C library for manipulating pipelines of subprocesses in
a flexible and convenient way.")
    (license gpl3+)))

(define-public man-db
  (package
    (name "man-db")
    (version "2.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/man-db/man-db-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1b641kcgjvyc41pj67dn4p0zvwlj1vx3l6nf7qdcc7kf6v5a2cjr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-test-shebangs
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Patch shebangs in test scripts.
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (file)
                           (substitute* file
                             (("#! /bin/sh")
                              (string-append "#!" (which "sh")))))
                         (remove file-is-directory?
                                 (find-files "src/tests" ".*")))
               #t)))
         (add-after 'unpack 'patch-absolute-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/man.c"
               (("\"iconv\"")
                (string-append "\"" (which "iconv") "\"")))
             ;; Embed an absolute reference to "preconv", otherwise it
             ;; falls back to searching in PATH and ultimately fails
             ;; to render unicode data (see <https://bugs.gnu.org/30785>).
             (substitute* "lib/encodings.c"
               (("groff_preconv = NULL")
                (string-append "groff_preconv = \""
                               (assoc-ref inputs "groff-minimal")
                               "/bin/preconv\"")))
             #t)))
       #:configure-flags
       (let ((groff (assoc-ref %build-inputs "groff"))
             (groff-minimal (assoc-ref %build-inputs "groff-minimal"))
             (less  (assoc-ref %build-inputs "less"))
             (gzip  (assoc-ref %build-inputs "gzip"))
             (bzip2 (assoc-ref %build-inputs "bzip2"))
             (xz    (assoc-ref %build-inputs "xz"))
             (util  (assoc-ref %build-inputs "util-linux")))
         ;; Invoke groff, less, gzip, bzip2, and xz directly from the store.
         (append (list ;; Disable setuid man user.
                       "--disable-setuid"
                       ;; Don't constrain ownership of system-wide cache files.
                       ;; Otherwise creating the manpage database fails with
                       ;; man-db > 2.7.5.
                       "--disable-cache-owner"
                       (string-append "--with-pager=" less "/bin/less")
                       (string-append "--with-gzip=" gzip "/bin/gzip")
                       (string-append "--with-bzip2=" bzip2 "/bin/gzip")
                       (string-append "--with-xz=" xz "/bin/xz")
                       (string-append "--with-col=" util "/bin/col")
                       ;; Default value is "/usr/lib/tmpfiles.d" (not
                       ;; prefix-sensitive).
                       (string-append "--with-systemdtmpfilesdir="
                                      %output "/lib/tmpfiles.d"))
                 (map (lambda (prog)
                        (string-append "--with-" prog "=" groff-minimal
                                       "/bin/" prog))
                      '("nroff" "eqn" "neqn" "tbl" "refer" "pic"))))

       ;; At run time we should refer to GROFF-MINIMAL, not GROFF (the latter
       ;; pulls in Perl.)
       #:disallowed-references (,groff)

       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("groff" ,groff)))   ;needed at build time (troff, grops, soelim, etc.)
    (inputs
     `(("flex" ,flex)
       ("gdbm" ,gdbm)
       ("groff-minimal" ,groff-minimal)
       ("less" ,less)
       ("libpipeline" ,libpipeline)
       ;; FIXME: 4.8 and later can use libseccomp, but it causes test
       ;; failures in the build chroot.
       ;;("libseccomp" ,libseccomp)
       ("util-linux" ,util-linux)))
    (native-search-paths
     (list (search-path-specification
            (variable "MANPATH")
            (files '("share/man")))))
    (home-page "http://man-db.nongnu.org/")
    (synopsis "Standard Unix documentation system")
    (description
     "Man-db is an implementation of the standard Unix documentation system
accessed using the man command.  It uses a Berkeley DB database in place of
the traditional flat-text whatis databases.")
    (license gpl2+)))

(define-public man-pages
  (package
    (name "man-pages")
    (version "5.01")
    (source
     (origin
       (method url-fetch)
       (uri
        (list (string-append "mirror://kernel.org/linux/docs/man-pages/"
                             "man-pages-" version ".tar.xz")
              (string-append "mirror://kernel.org/linux/docs/man-pages/Archive/"
                             "man-pages-" version ".tar.xz")))
       (sha256
        (base32 "09xn8d8xxwgms6h1bvjlgn3mxz51vxf3ra0ry9f5dqi29qry3z3x"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases (delete 'configure))

       ;; The 'all' target depends on three targets that directly populate
       ;; $(MANDIR) based on its current contents.  Doing that in parallel
       ;; leads to undefined behavior (see <http://bugs.gnu.org/18701>.)
       #:parallel-build? #f

       #:tests? #f
       #:make-flags (list (string-append "MANDIR="
                                         (assoc-ref %outputs "out")
                                         "/share/man"))))
    (home-page "https://www.kernel.org/doc/man-pages/")
    (synopsis "Development manual pages from the Linux project")
    (description
     "This package provides traditional Unix \"man pages\" documenting the
Linux kernel and C library interfaces employed by user-space programs.")

    ;; Each man page has its own license; some are GPLv2+, some are MIT/X11.
    (license gpl2+)))

(define-public help2man
  (package
    (name "help2man")
    (version "1.47.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/help2man/help2man-"
                          version ".tar.xz"))
      (sha256
       (base32
        "0vz4dlrvy4vc6l7w0a7n668pfa0rdm73wr2gar58wqranyah46yr"))))
    (build-system gnu-build-system)
    (arguments `(;; There's no `check' target.
                 #:tests? #f))
    (inputs
     `(("perl" ,perl)
       ;; TODO: Add these optional dependencies.
       ;; ("perl-LocaleGettext" ,perl-LocaleGettext)
       ;; ("gettext" ,gettext-minimal)
       ))
    (home-page "https://www.gnu.org/software/help2man/")
    (synopsis "Automatically generate man pages from program --help")
    (description
     "GNU help2man is a program that converts the output of standard
\"--help\" and \"--version\" command-line arguments into a manual page
automatically.")
    (license gpl3+)))

(define-public help2man/latest
  (package
    (inherit help2man)
    (version "1.47.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/help2man/help2man-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1p5830h88cx0zn0snwaj0vpph81xicpsirfwlxmcgjrlmn0nm3sj"))))))

(define-public scdoc
  (package
   (name "scdoc")
   (version "1.9.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://git.sr.ht/%7Esircmpwn/scdoc/archive/" version
                         ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "00zc3rzj97gscby31djlqyczvqpyhrl66i44czwzmmn7rc5j03m1"))))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags '("CC=gcc")
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'install 'hardcode-paths
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "Makefile"
                         (("/usr/local") (assoc-ref outputs "out")))
            #t)))))
   (home-page "https://git.sr.ht/~sircmpwn/scdoc")
   (synopsis "Simple man page generator")
   (description "scdoc is a simple man page generator written for POSIX systems
in C99.")
   ;; MIT license, see /share/doc/scdoc-1.6.0/COPYING.
   (license expat)))

(define-public txt2man
  (package
    (name "txt2man")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mvertes/txt2man/archive/txt2man-"
             version ".tar.gz"))
       (sha256
        (base32
         "168cj96974n2z0igin6j1ic1m45zyic7nm5ark7frq8j78rrx4zn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (inputs
     `(("gawk" ,gawk)))
    (home-page "https://github.com/mvertes/txt2man")
    (synopsis "Convert text to man page")
    (description "Txt2man converts flat ASCII text to man page format.")
    (license gpl2+)))
