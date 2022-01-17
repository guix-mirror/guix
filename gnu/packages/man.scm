;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2014, 2015, 2017, 2018, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2015, 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2016, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ruby)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages less)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xml))

(define-public xmltoman
  (package
    (name "xmltoman")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/xmltoman/xmltoman/"
                           "xmltoman-" version ".tar.gz/"
                           "xmltoman-" version ".tar.gz"))
       (sha256
        (base32 "1c0lvzr7kdy63wbn1jv6s126ds7add3pxqb0vlxd3v5a2sir91wl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No tests
       #:make-flags
       (list
        (string-append "PREFIX="
                       (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (propagated-inputs
     (list perl perl-xml-parser))
    (synopsis "XML to Man converter")
    (description "XMLtoMan and XMLMantoHTML are two small scripts to convert xml
to man pages in groff format or html.  It features the usual man page items such
as description, options, see also, etc.")
    (home-page "http://xmltoman.sourceforge.net/")
    (license license:gpl2+)))

(define-public ronn
  (package
    (name "ronn")
    (version "0.7.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/rtomayko/ronn")
         (commit version)))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "0fkniz7j1jp8v3i05m6hks3nsh6rzvjfi0ichpi7h4gwk5byxb94"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))                   ; Library hpricot not available
    (synopsis "Manual authoring tool")
    (description "Ronn builds manuals.  It converts simple, human readable
textfiles to roff for terminal display, and also to HTML for the web.")
    (home-page "https://rtomayko.github.io/ronn/")
    (license license:expat)))

(define-public libpipeline
  (package
    (name "libpipeline")
    (version "1.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://savannah/libpipeline/libpipeline-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1c5dl017xil2ssb6a5vg927bnsbc9vymfgi9ahvqbb8gypx0igsx"))))
    (build-system gnu-build-system)
    (home-page "http://libpipeline.nongnu.org/")
    (synopsis "C library for manipulating pipelines of subprocesses")
    (description
     "libpipeline is a C library for manipulating pipelines of subprocesses in
a flexible and convenient way.")
    (license license:gpl3+)))

(define-public man-db
  (package
    (name "man-db")
    (version "2.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/man-db/man-db-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0mk7n7yn6scy785jhg1j14b3q9l0cgvpry49r0ldjsnizbnrjv5n"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
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
           #~(let ((groff (assoc-ref %build-inputs "groff"))
                   (groff-minimal (assoc-ref %build-inputs "groff-minimal"))
                   (less  (assoc-ref %build-inputs "less"))
                   (gzip  (assoc-ref %build-inputs "gzip"))
                   (bzip2 (assoc-ref %build-inputs "bzip2"))
                   (xz    (assoc-ref %build-inputs "xz"))
                   (util  (assoc-ref %build-inputs "util-linux")))
               ;; Invoke groff, less, gzip, bzip2, & xz directly from the store.
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
                        ;; The default systemd directories ignore --prefix.
                        ;; XXX TODO: Replace with simply #$OUTPUT on staging.
                        (string-append "--with-systemdsystemunitdir="
                                       #$(if (%current-target-system)
                                             #~#$output
                                             #~%output)
                                       "/lib/systemd/system")
                        (string-append "--with-systemdtmpfilesdir="
                                       #$(if (%current-target-system)
                                             #~#$output
                                             #~%output)
                                       "/lib/tmpfiles.d"))
                   (map (lambda (prog)
                          (string-append "--with-" prog "=" groff-minimal
                                         "/bin/" prog))
                        '("nroff" "eqn" "neqn" "tbl" "refer" "pic"))))

           ;; At run time we should refer to GROFF-MINIMAL, not GROFF (the latter
           ;; pulls in Perl.)
           #:disallowed-references
           (list groff)

           #:modules '((guix build gnu-build-system)
                       (guix build utils)
                       (srfi srfi-1))))
    (native-inputs
     (list pkg-config flex groff))   ;needed at build time (troff, grops, soelim, etc.)
    (inputs
     (list gdbm
           groff-minimal
           less
           libpipeline
           ;; FIXME: 4.8 and later can use libseccomp, but it causes test
           ;; failures in the build chroot.
           ;;("libseccomp" ,libseccomp)
           util-linux))
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
    (license license:gpl2+)))

(define-public mandoc
  (package
    (name "mandoc")
    (version "1.14.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://mandoc.bsd.lv/snapshots/mandoc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "174x2x9ws47b14lm339j6rzm7mxy1j3qhh484khscw0yy1qdbw4b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "regress"
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-prefix
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "configure"
                        (("^CC=.*")
                         (string-append "CC=" ,(cc-for-target) "\n"))
                        (("^DEFCFLAGS=\\\\\"")
                         "DEFCFLAGS=\"-O2 ")
                        (("^UTF8_LOCALE=.*")      ;used for tests
                         "UTF8_LOCALE=en_US.UTF-8\n")
                        (("^MANPATH_(BASE|DEFAULT)=.*" _ which)
                         (string-append "MANPATH_" which "="
                                        "/run/current-system/profile/share/man\n"))
                        (("^PREFIX=.*")
                         (string-append "PREFIX=" (assoc-ref outputs "out")
                                        "\n"))))))))
    (native-inputs (list perl))             ;used to run tests
    (inputs (list zlib))
    (native-search-paths
     (list (search-path-specification
            (variable "MANPATH")
            (files '("share/man")))))
    (synopsis "Tools for BSD mdoc and man pages")
    (description
     "mandoc is a suite of tools compiling mdoc, the roff macro language of
choice for BSD manual pages, and man, the predominant historical language for
UNIX manuals.  It is small and quite fast.  The main component of the toolset
is the @command{mandoc} utility program, based on the libmandoc validating
compiler, to format output for UTF-8 and ASCII UNIX terminals, HTML 5,
PostScript, and PDF.  Additional tools include the @command{man} viewer, and
@command{apropos} and @command{whatis}.")
    (home-page "https://mandoc.bsd.lv/")
    (license license:isc)))

(define-public man-pages
  (package
    (name "man-pages")
    (version "5.13")
    (source
     (origin
       (method url-fetch)
       (uri
        (list (string-append "mirror://kernel.org/linux/docs/man-pages/"
                             "man-pages-" version ".tar.xz")
              (string-append "mirror://kernel.org/linux/docs/man-pages/Archive/"
                             "man-pages-" version ".tar.xz")))
       (sha256
        (base32 "12vb15gs56g8wl5nqlm4llr508brh4m2lfknhq4lizbxzqzawkb1"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases (delete 'configure))

       ;; The 'all' target depends on three targets that directly populate
       ;; $(MANDIR) based on its current contents.  Doing that in parallel
       ;; leads to undefined behavior (see <http://bugs.gnu.org/18701>.)
       #:parallel-build? #f

       #:tests? #f
       #:make-flags (list (string-append "mandir="
                                         (assoc-ref %outputs "out")
                                         "/share/man"))))
    (home-page "https://www.kernel.org/doc/man-pages/")
    (synopsis "Development manual pages from the Linux project")
    (description
     "This package provides traditional Unix \"man pages\" documenting the
Linux kernel and C library interfaces employed by user-space programs.")

    ;; Each man page has its own license; some are GPLv2+, some are MIT/X11.
    (license license:gpl2+)))

(define-public help2man
  ;; TODO: Manual pages for languages not available from the implicit
  ;; input "locales" contain the original (English) text.
  (package
    (name "help2man")
    (version "1.48.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/help2man/help2man-"
                          version ".tar.xz"))
      (sha256
       (base32
        "1gl24n9am3ivhql1gs9lffb415irg758fhxyk4ryssiflk5f8fb7"))))
    (build-system gnu-build-system)
    (arguments `(;; There's no `check' target.
                 #:tests? #f
                 #:phases
                 (modify-phases %standard-phases
                   (add-after 'unpack 'patch-help2man-with-perl-gettext
                     (lambda* (#:key inputs #:allow-other-keys)
                       (let ((lib (assoc-ref inputs "perl-gettext"))
                             (fmt "use lib '~a/lib/perl5/site_perl';~%~a"))
                         (substitute* "help2man.PL"
                           (("^use Locale::gettext.*$" load)
                            (format #f fmt lib load))))
                       #t)))))
    (inputs
     `(("perl" ,perl)
       ,@(if (%current-target-system)
             '()
             `(("perl-gettext" ,perl-gettext)))))
    (native-inputs
     `(("perl" ,perl)
       ("gettext" ,gettext-minimal)))
    (home-page "https://www.gnu.org/software/help2man/")
    (synopsis "Automatically generate man pages from program --help")
    (description
     "GNU help2man is a program that converts the output of standard
\"--help\" and \"--version\" command-line arguments into a manual page
automatically.")
    (license license:gpl3+)))

(define-public scdoc
  (package
   (name "scdoc")
   (version "1.10.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://git.sr.ht/~sircmpwn/scdoc")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1xmh6fnp378xmiycslg4migs1vx7yly4i1cf2vbbnwim9c9g0aw7"))))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags
      (list (string-append "CC=" ,(cc-for-target))
            (string-append "PREFIX=" (assoc-ref %outputs "out")))
      #:phases
      (modify-phases %standard-phases
        (delete 'configure))))
   (home-page "https://git.sr.ht/~sircmpwn/scdoc")
   (synopsis "Simple man page generator")
   (description "scdoc is a simple man page generator written for POSIX systems
in C99.")
   ;; MIT license, see /share/doc/scdoc-1.6.0/COPYING.
   (license license:expat)))

(define-public txt2man
  (package
    (name "txt2man")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/mvertes/txt2man")
              (commit (string-append "txt2man-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1razjpvlcp85hqli77mwr9nmn5jnv3lm1fxbbqjpx1brv3h1lvm5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (inputs
     (list gawk))
    (home-page "https://github.com/mvertes/txt2man")
    (synopsis "Convert text to man page")
    (description "Txt2man converts flat ASCII text to man page format.")
    (license license:gpl2+)))
