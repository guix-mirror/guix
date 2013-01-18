;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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
                  (lambda _
                    ;; Private headers are needed by Expect.
                    (zero? (system* "make" "install-private-headers")))
                  %standard-phases))

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
       (list (string-append "--with-tcl="
                            (assoc-ref %build-inputs "tcl")
                            "/lib")
             (string-append "--with-tclinclude="
                            (assoc-ref %build-inputs "tcl")
                            "/include")
             (string-append "--exec-prefix="
                            (assoc-ref %outputs "out")))

       #:phases (alist-cons-before
                 'configure 'set-path-to-stty
                 (lambda _
                   (substitute* "configure"
                     (("STTY_BIN=/bin/stty")
                      (string-append "STTY_BIN=" (which "stty")))))
                 %standard-phases)

       #:test-target "test"))
    (home-page "http://expect.nist.gov/")
    (synopsis
     "A tool for automating interactive applications")
    (description
     "Expect is a tool for automating interactive applications such as
telnet, ftp, passwd, fsck, rlogin, tip, etc.  Expect really makes this
stuff trivial. Expect is also useful for testing these same
applications. And by adding Tk, you can wrap interactive applications in
X11 GUIs.")
    (license public-domain)))            ; as written in `license.terms'
