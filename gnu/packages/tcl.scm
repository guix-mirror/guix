;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
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
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages image)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:))

(define-public tcl
  (package
    (name "tcl")
    (version "8.6.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/tcl/Tcl/"
                                  version "/tcl" version "-src.tar.gz"))
              (sha256
               (base32
                "1vc7imilx6kcb5319r7hnrp4jn5pqb41an3vr3azhgcfcgvdp5ji"))
              (patches (search-patches "tcl-fix-cross-compilation.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'pre-configure
                    (lambda _ (chdir "unix") #t))
                 (add-after 'install 'install-private-headers
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; Private headers are needed by Expect.
                     (invoke "make" "install-private-headers")
                     (let ((bin (string-append (assoc-ref outputs "out")
                                               "/bin")))
                       ;; Create a tclsh -> tclsh8.6 symlink.
                       ;; Programs such as Ghostscript rely on it.
                       (with-directory-excursion bin
                         (symlink (car (find-files "." "tclsh"))
                                  "tclsh"))
                       #t))))

       ;; By default, man pages are put in PREFIX/man, but we want them in
       ;; PREFIX/share/man.  The 'validate-documentation-location' phase is
       ;; not able to fix this up because the default install populates both
       ;; PREFIX/man and PREFIX/share/man.
       #:configure-flags
       (list (string-append "--mandir="
                            (assoc-ref %outputs "out")
                            "/share/man")
             ;; This is needed when cross-compiling, see:
             ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=719247
             ,@(if (%current-target-system)
                   '("tcl_cv_strtod_buggy=1"
                     "ac_cv_func_strtod=yes")
                   '()))

       ;; XXX: There are a few test failures (related to HTTP, most
       ;; likely related to name resolution), but that doesn't cause
       ;; `make' to fail.
       #:test-target "test"))
    (home-page "https://www.tcl.tk/")
    (synopsis "The Tcl scripting language")
    (description "The Tcl (Tool Command Language) scripting language.")
    (license license:tcl/tk)))

(define-public itcl
  (package
    (name "itcl")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/incrtcl/%5Bincr%20Tcl_Tk%5D-4-source/itcl%20"
             version "/itcl" version ".tar.gz"))
       (file-name (string-append "incrtcl-" version ".tar.gz"))
       (sha256
        (base32 "0w28v0zaraxcq1s9pa6cihqqwqvvwfgz275lks7w4gl7hxjxmasw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        (string-append
         "--exec-prefix=" (assoc-ref %outputs "out"))
        (string-append
         "--with-tclinclude=" (assoc-ref %build-inputs "tcl") "/include")
        (string-append
         "--with-tcl=" (assoc-ref %build-inputs "tcl") "/lib"))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'cleanup-bin-and-lib
           (lambda* (#:key outputs #:allow-other-keys)
             ;; NOTE: (Sharlatan-20210213T204336+0000): libraries appearer in
             ;; "out/lib/itcl{{version}}" and there are no binaries, some extra
             ;; rename and remove spells are to be applied.
             (let ((out (assoc-ref outputs "out")))
               (rmdir
                (string-append out "/bin"))
               (rename-file
                (string-append out "/lib/itcl" ,version) (string-append out "/libtmp"))
               (rename-file
                (string-append out "/libtmp") (string-append out "/lib")))
             #t)))))
    (native-inputs
     `(("tcl" ,tcl)))
    (inputs
     `(("tcllib" ,tcllib)))
    (home-page "http://incrtcl.sourceforge.net/")
    (synopsis "Object Oriented programming (OOP) extension for Tcl")
    (description
     "[incr Tcl] is a widely used object-oriented system for Tcl.  The name is
a play on C++, and [incr Tcl] provides a similar object model, including
multiple inheritence and public and private classes and variables.")
    (license license:public-domain)))

(define-public expect
  (package
    (name "expect")
    (version "5.45.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/expect/Expect/"
                          version "/expect" version ".tar.gz"))
      (sha256
       (base32
        "0d1cp5hggjl93xwc8h1y6adbnrvpkk0ywkd00inz9ndxn21xm9s9"))))
    (build-system gnu-build-system)
    (inputs
     `(;; TODO: Add these optional dependencies.
       ;; ("libX11" ,libX11)
       ;; ("xorgproto" ,xorgproto)
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

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-path-to-stty
           (lambda _
             (substitute* "configure"
               (("STTY_BIN=/bin/stty")
                (string-append "STTY_BIN=" (which "stty"))))
             #t)))

       #:test-target "test"))
    (home-page "http://expect.sourceforge.net/")
    (synopsis "Tool for automating interactive applications")
    (description
     "Expect is a tool for automating interactive applications such as
telnet, ftp, passwd, fsck, rlogin, tip, etc.  Expect really makes this
stuff trivial.  Expect is also useful for testing these same
applications.  And by adding Tk, you can wrap interactive applications in
X11 GUIs.")
    (license license:public-domain))) ; as written in `license.terms'

(define-public tk
  (package
    (name "tk")
    (version "8.6.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/tcl/Tcl/"
                                 (version-prefix version 3) "/tk"
                                 version "-src.tar.gz"))
             (sha256
              (base32
               "11p3ycqbr5116vpaxv6fl6md6gcav1ffspgr8wrlc2lxhn543pv3"))
             (patches (search-patches "tk-find-library.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before
                   'configure 'pre-configure
                   (lambda _
                     (chdir "unix")
                     #t))
                  (add-after
                   'install 'create-wish-symlink
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out")))
                       (symlink (string-append out "/bin/wish"
                                               ,(version-major+minor
                                                  (package-version tk)))
                                (string-append out "/bin/wish")))
                     #t))
                  (add-after
                   'install 'add-fontconfig-flag
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     ;; Add the missing -L flag for Fontconfig in 'tk.pc' and
                     ;; 'tkConfig.sh'.
                     (let ((out        (assoc-ref outputs "out"))
                           (fontconfig (assoc-ref inputs "fontconfig")))
                       (substitute* (find-files out
                                                "^(tkConfig\\.sh|tk\\.pc)$")
                         (("-lfontconfig")
                          (string-append "-L" fontconfig
                                         "/lib -lfontconfig")))
                       #t))))

       #:configure-flags
       (list (string-append "--with-tcl="
                            (assoc-ref %build-inputs "tcl")
                            "/lib")
             ;; This is needed when cross-compiling, see:
             ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=719247
             ,@(if (%current-target-system)
                   '("tcl_cv_strtod_buggy=1"
                     "ac_cv_func_strtod=yes")
                   '()))

       ;; The tests require a running X server, so we just skip them.
       #:tests? #f))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libxft" ,libxft)
              ("fontconfig" ,fontconfig)
              ("tcl" ,tcl)))
    ;; tk.h refers to X11 headers, hence the propagation.
    (propagated-inputs `(("libx11" ,libx11)
                         ("libxext" ,libxext)))

    (home-page "https://www.tcl.tk/")
    (synopsis "Graphical user interface toolkit for Tcl")
    (description
     "Tk is a graphical toolkit for building graphical user
interfaces (GUIs) in the Tcl language.")
    (license (package-license tcl))))

(define-public perl-tk
  (package
    (name "perl-tk")
    (version "804.034")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/S/SR/SREZIC/Tk-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1qiz55dmw7hm1wgpjdzf2jffwcj0hisr3kf80qi8lli3qx2b39py"))))
    (build-system perl-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libx11" ,libx11)
              ("libpng" ,libpng)
              ("libjpeg" ,libjpeg-turbo)))
    (arguments
     `(#:make-maker-flags `(,(string-append
                              "X11=" (assoc-ref %build-inputs "libx11")))

       ;; Fails to build in parallel: <http://bugs.gnu.org/18262>.
       #:parallel-build? #f))
    (synopsis "Graphical user interface toolkit for Perl")
    (description
     "Tk is a Graphical User Interface ToolKit.")
    (home-page "https://metacpan.org/release/Tk")
    ;; From the package README: "... you can redistribute it and/or modify it
    ;; under the same terms as Perl itself, with the exception of all the
    ;; files in the pTk sub-directory which have separate terms derived from
    ;; those of the orignal Tix4.1.3 or Tk8.4.* sources. See the files
    ;; pTk/license.terms, pTk/license.html_lib, and pTk/Tix.license for
    ;; details of this license."
    (license license:perl-license)))

(define-public tcllib
  (package
    (name "tcllib")
    (version "1.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/tcllib/tcllib/"
                                  version "/tcllib-" version ".tar.gz"))
              (sha256
               (base32
                "173abxaazdmf210v651708ab6h7xhskvd52krxk6ifam337qgzh1"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("tcl" ,tcl)))
    (native-search-paths
     (list (search-path-specification
            (variable "TCLLIBPATH")
            (separator " ")
            (files (list (string-append "lib/tcllib" version))))))
    (home-page "https://core.tcl.tk/tcllib/home")
    (synopsis "Standard Tcl Library")
    (description "Tcllib, the standard Tcl library, is a collection of common
utility functions and modules all written in high-level Tcl.")
    (license (package-license tcl))))

(define-public tklib
  (package
    (name "tklib")
    (version "0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://core.tcl.tk/tklib/tarball/tklib-"
                                  version ".tar.gz?uuid=tklib-0-6"))
              (sha256
               (base32
                "03y0bzgwbh7nnyqkh8n00bbkq2fyblq39s3bdb6mawna0bbn0wwg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("tcl" ,tcl)))
    (propagated-inputs
     `(("tcllib" ,tcllib)
       ("tk" ,tk))) ; for "wish"
    (native-search-paths
     (list (search-path-specification
            (variable "TCLLIBPATH")
            (separator " ")
            (files (list (string-append "lib/tklib" version))))))
    (home-page "https://www.tcl.tk/software/tklib/")
    (synopsis "Tk utility modules for Tcl")
    (description "Tklib is a collection of common utility functions and
modules for Tk, all written in high-level Tcl.  Examples of provided widgets:
@enumerate
@item @code{chatwidget}
@item @code{datefield}
@item @code{tooltip}
@item @code{cursor}
@item @code{ipentry}
@item @code{tablelist}
@item @code{history}
@item @code{tkpiechart}
@item @code{ico}
@item @code{crosshair}
@item @code{ntext}
@item @code{plotchart}
@item @code{ctext}
@item @code{autosscroll}
@item @code{canvas}
@end enumerate")
    (license (package-license tcl))))

(define-public tclxml
  (package
    (name "tclxml")
    (version "3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name "/TclXML/"
                                  version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ffb4aw63inig3aql33g4pk0kjk14dv238anp1scwjdjh1k6n4gl"))
              (patches (search-patches "tclxml-3.2-install.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("tcl" ,tcl)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)))
    (propagated-inputs
     `(("tcllib" ,tcllib))) ; uri
    (native-search-paths
     (list (search-path-specification
            (variable "TCLLIBPATH")
            (separator " ")
            (files (list (string-append "lib/Tclxml" version))))))
    (arguments
     `(#:configure-flags
       (list (string-append "--exec-prefix=" (assoc-ref %outputs "out"))
             (string-append "--with-tclconfig="
                            (assoc-ref %build-inputs "tcl") "/lib")
             (string-append "--with-xml2-config="
                            (assoc-ref %build-inputs "libxml2")
                            "/bin/xml2-config")
             (string-append "--with-xslt-config="
                            (assoc-ref %build-inputs "libxslt")
                            "/bin/xslt-config"))
       #:test-target "test"))
    (home-page "http://tclxml.sourceforge.net/")
    (synopsis "Tcl library for XML parsing")
    (description "TclXML provides event-based parsing of XML documents.  The
application may register callback scripts for certain document features, and
when the parser encounters those features while parsing the document the
callback is evaluated.")
    (license (license:non-copyleft
              "file://LICENCE"
              "See LICENCE in the distribution."))))

(define-public tclx
  (package
    (name "tclx")
    (version "8.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/tclx/TclX/"
                                  version "/tclx" version ".tar.bz2"))
              (sha256
               (base32
                "1v2qwzzidz0is58fd1p7wfdbscxm3ip2wlbqkj5jdhf6drh1zd59"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; a test named profile.test segfaults
       #:configure-flags (list (string-append "--with-tcl="
                                              (assoc-ref %build-inputs "tcl")
                                              "/lib")
                               (string-append "--libdir="
                                              (assoc-ref %outputs "out")
                                              "/lib"))))
    (inputs
     `(("tcl" ,tcl)
       ("tk" ,tk)))
    (home-page "http://tclx.sourceforge.net/")
    (synopsis "System programming extensions for Tcl")
    (description
     "Extended Tcl is oriented towards system programming tasks and large
application development.  TclX provides additional interfaces to the operating
system, and adds many new programming constructs, text manipulation tools, and
debugging tools.")
    (license license:tcl/tk)))

(define-public go-github.com-nsf-gothic
  (let ((commit "97dfcc195b9de36c911a69a6ec2b5b2659c05652")
        (revision "0"))
    (package
      (name "go-github.com-nsf-gothic")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/nsf/gothic")
                       (commit commit)))
                (sha256
                 (base32
                  "1lrhbml6r4sh22rrn3m9bck70pv0g0c1diprg7cil90x0jidxczr"))
                (file-name (git-file-name name version))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/nsf/gothic"))
    (propagated-inputs
     `(("tk" ,tk)
       ("tcl" ,tcl)))
    (home-page "https://github.com/nsf/gothic")
    (synopsis "Tcl/Tk Go bindings")
    (description "Gothic contains Go bindings for Tcl/Tk.  The package contains
only one type and one function that can be used to create a Tk interpreter.")
    (license license:expat))))
