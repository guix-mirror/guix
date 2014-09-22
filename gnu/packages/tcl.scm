;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages tcl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages image)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (guix licenses))

(define-public tcl
  (package
    (name "tcl")
    (version "8.6.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/tcl/Tcl/"
                          version "/tcl" version "-src.tar.gz"))
      (sha256
       (base32
        "1pnabp3xsja4rc8c01l9q1avb65a3zhdzci3j54qa5krqjwj4i1m"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'pre-configure
                 (lambda _
                   (chdir "unix"))
                 (alist-cons-after
                  'install 'install-private-headers
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; Private headers are needed by Expect.
                    (and (zero? (system* "make"
                                         "install-private-headers"))
                         (let ((bin (string-append (assoc-ref outputs "out")
                                                   "/bin")))
                           ;; Create a tclsh -> tclsh8.6 symlink.
                           ;; Programs such as Ghostscript rely on it.
                           (with-directory-excursion bin
                             (symlink (car (find-files "." "tclsh"))
                                      "tclsh")))))
                  %standard-phases))

       ;; By default, man pages are put in PREFIX/man,
       ;; but we want them in PREFIX/share/man.
       #:configure-flags (list (string-append "--mandir="
                                              (assoc-ref %outputs "out")
                                              "/share/man"))

       ;; XXX: There are a few test failures (related to HTTP, most
       ;; likely related to name resolution), but that doesn't cause
       ;; `make' to fail.
       #:test-target "test"))
    (home-page "http://www.tcl.tk/")
    (synopsis "The Tcl scripting language")
    (description "The Tcl (Tool Command Language) scripting language.")
    (license (bsd-style "http://www.tcl.tk/software/tcltk/license.html"
                        "Tcl/Tk license"))))


(define-public expect
  (package
    (name "expect")
    (version "5.45")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/expect/Expect/"
                          version "/expect" version ".tar.gz"))
      (sha256
       (base32
        "0h60bifxj876afz4im35rmnbnxjx4lbdqp2ja3k30fwa8a8cm3dj"))))
    (build-system gnu-build-system)
    (inputs
     `(;; TODO: Add these optional dependencies.
       ;; ("libX11" ,libX11)
       ;; ("xproto" ,xproto)
       ;; ("tk" ,tk)
       ("tcl" ,tcl)))
    (arguments
     '(#:configure-flags
       (let ((out (assoc-ref %outputs "out"))
             (tcl (assoc-ref %build-inputs "tcl")))
         (list (string-append "--with-tcl=" tcl "/lib")
               (string-append "--with-tclinclude=" tcl "/include")
               (string-append "--exec-prefix=" out)
               (string-append "--mandir=" out "/share/man")))

       #:phases (alist-cons-before
                 'configure 'set-path-to-stty
                 (lambda _
                   (substitute* "configure"
                     (("STTY_BIN=/bin/stty")
                      (string-append "STTY_BIN=" (which "stty")))))
                 %standard-phases)

       #:test-target "test"))
    (home-page "http://expect.nist.gov/")
    (synopsis "Tool for automating interactive applications")
    (description
     "Expect is a tool for automating interactive applications such as
telnet, ftp, passwd, fsck, rlogin, tip, etc.  Expect really makes this
stuff trivial. Expect is also useful for testing these same
applications. And by adding Tk, you can wrap interactive applications in
X11 GUIs.")
    (license public-domain)))            ; as written in `license.terms'

(define-public tk
  (package
    (name "tk")
    (version "8.6.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/tcl/Tcl/"
                                 version "/tk" version "-src.tar.gz"))
             (sha256
              (base32
               "1rld0l7p1h31z488w44j170jpsm11xsjf2qrb7gid2b5dwmqnw2w"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'pre-configure
                 (lambda _
                   (chdir "unix"))
                 %standard-phases)

       #:configure-flags (list (string-append "--with-tcl="
                                              (assoc-ref %build-inputs "tcl")
                                              "/lib"))

       ;; The tests require a running X server, so we just skip them.
       #:tests? #f))
    (inputs `(("tcl" ,tcl)))

    ;; tk.h refers to X11 headers, hence the propagation.
    (propagated-inputs `(("libx11" ,libx11)
                         ("libxext" ,libxext)))

    (home-page "http://www.tcl.tk/")
    (synopsis "Graphical user interface toolkit for Tcl")
    (description
     "Tk is a graphical toolkit for building graphical user
interfaces (GUIs) in the Tcl language.")
    (license (package-license tcl))))

(define-public perl-tk
  (package
    (name "perl-tk")
    (version "804.032")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/S/SR/SREZIC/Tk-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0jarvplhfvnm0shhdm2a5zczlnk9mkf8jvfjiwyhjrr3cy1gl0w0"))
             (patches (list (search-patch "perl-tk-x11-discover.patch")))))
    (build-system perl-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libx11" ,libx11)
              ("libpng" ,libpng)
              ("libjpeg" ,libjpeg)))
    (arguments
     `(#:make-maker-flags `(,(string-append
                              "X11=" (assoc-ref %build-inputs "libx11")))

       ;; Fails to build in parallel: <http://bugs.gnu.org/18262>.
       #:parallel-build? #f))
    (synopsis "Graphical user interface toolkit for Perl")
    (description
     "Tk is a Graphical User Interface ToolKit.")
    (home-page (string-append "http://search.cpan.org/~srezic/Tk-" version))
    ;; From the package README: "... you can redistribute it and/or modify it
    ;; under the same terms as Perl itself, with the exception of all the
    ;; files in the pTk sub-directory which have separate terms derived from
    ;; those of the orignal Tix4.1.3 or Tk8.4.* sources. See the files
    ;; pTk/license.terms, pTk/license.html_lib, and pTk/Tix.license for
    ;; details of this license."
    (license (package-license perl))))
