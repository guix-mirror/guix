;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
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

(define-module (gnu packages ftp)
  #:use-module ((guix licenses) #:select (gpl2+ gpl3+ clarified-artistic))
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml))

(define-public lftp
  (package
    (name "lftp")
    (version "4.8.3")
    (source (origin
              (method url-fetch)
              ;; See https://lftp.tech/get.html for mirrors.
              (uri (list (string-append "https://lftp.tech/ftp/lftp-"
                                        version ".tar.xz")
                         (string-append "https://lftp.tech/ftp/old/lftp-"
                                        version ".tar.xz")
                         (string-append "ftp://ftp.st.ryukoku.ac.jp/pub/network/"
                                        "ftp/lftp/lftp-" version ".tar.xz")))
              (sha256
               (base32
                "12y77jlfs4x4zvcah92mw2h2sb4j0bvbaxkh3wwsm8gs392ywyny"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("zlib" ,zlib)
       ("readline" ,readline)
       ("gnutls" ,gnutls)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Disable tests that require network access, which is most of them.
         (add-before 'check 'disable-impure-tests
                     (lambda _
                       (substitute* "tests/Makefile"
                         (("(ftp-cls-l|ftp-list|http-get)\\$\\(EXEEXT\\)") "")
                         (("lftp-https-get ") "")))))
       #:configure-flags
       (list (string-append "--with-readline="
                            (assoc-ref %build-inputs "readline")))))
    (home-page "https://lftp.tech/")
    (synopsis "Command-line file transfer program")
    (description
     "LFTP is a sophisticated FTP/HTTP client, and a file transfer program
supporting a number of network protocols.  Like Bash, it has job control and
uses the Readline library for input.  It has bookmarks, a built-in mirror
command, and can transfer several files in parallel.  It was designed with
reliability in mind.")
    (license gpl3+)))

(define-public ncftp
  (package
    (name "ncftp")
    (version "3.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.ncftp.com/ncftp/ncftp-"
                                  version "-src.tar.bz2"))
              (sha256
               (base32
                "0hlx12i0lwi99qsrx7nccf4nvwjj2gych4yks5y179b1ax0y5sxl"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Use the right 'rm' and 'ls'.
                  (substitute* (cons "configure"
                                     (find-files "."
                                                 "^(Makefile\\.in|.*\\.sh)$"))
                    (("/bin/(rm|ls)" _ command)
                     command))

                  ;; This is free software, avoid any confusion.
                  (substitute* (find-files "." "\\.c$")
                    (("a freeware program")
                     "free software"))))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; This is an old 'configure' script that doesn't
                    ;; understand variables passed as arguments.
                    (let ((out (assoc-ref outputs "out")))
                      (setenv "CONFIG_SHELL" (which "sh"))
                      (setenv "SHELL" (which "sh"))
                      (zero? (system* "./configure"
                                      (string-append "--prefix=" out)))))))
                #:tests? #f))           ;there are no tests
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://www.ncftp.com/ncftp/")
    (synopsis "Command-line File Transfer Protocol (FTP) client")
    (description
     "NcFTP Client (or just NcFTP) is a set of command-line programs to access
File Transfer Protocol (FTP) servers.  This includes 'ncftp', an interactive
FTP browser, as well as non-interactive commands such as 'ncftpput' and
'ncftpget'.")
    (license clarified-artistic)))


(define-public weex
  (package
    (name "weex")
    (version "2.8.2")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "mirror://sourceforge/weex/weex/"
                         "/weex_" version ".tar.gz"))
        (sha256
          (base32
            "1ir761hjncr1bamaqcw9j7x57xi3s9jax3223bxwbq30a0vsw1pd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("automake" ,automake)
       ("autoconf" ,autoconf)
       ("gettext" ,gettext-minimal)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           (lambda _ (zero? (system* "autoreconf" "-vfi")))))))
    (home-page "http://weex.sourceforge.net/")
    (synopsis "Non-interactive client for FTP synchronization")
    (description
     "Weex is a utility designed to automate the task of remotely
maintaining a web page or other FTP archive.  It synchronizes a set of
local files to a remote server by performing uploads and remote deletes
as required.")
    (license gpl2+)))

(define-public libfilezilla
  (package
    (name "libfilezilla")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.filezilla-project.org/"
                           name "/" name "-" version ".tar.bz2"))
       (sha256
        (base32
         "1xv4is3zaz66h6iblj9pikapsjasjcbxx31bhkgn62xdq1sadfpc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("cppunit" ,cppunit)
       ("pkg-config" ,pkg-config)))
    (home-page "https://lib.filezilla-project.org")
    (synopsis "Cross-platform C++ library used by Filezilla client")
    (description
     "This package provides some basic functionality to build high-performing,
platform-independent programs.")
    (license gpl2+)))

(define-public filezilla
  (package
    (name "filezilla")
    (version "3.27.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/" name
                           "/files/FileZilla_Client/" version
                           "/FileZilla_" version "_src" ".tar.bz2"))
       (sha256
        (base32
         "14lsplbp9fy7lk6cpwi3aj6jskz4j82h67x0fik82z1bns0zm2a3"))))
    (build-system gnu-build-system)
    (arguments
      ;; Don't let filezilla phone home to check for updates.
     '(#:configure-flags '("--disable-autoupdatecheck")))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("pugixml" ,pugixml)
       ("xdg-utils" ,xdg-utils)))
    (inputs
     `(("dbus" ,dbus)
       ("gnutls" ,gnutls)
       ("gtk+" ,gtk+)
       ("libfilezilla" ,libfilezilla)
       ("libidn" ,libidn)
       ("nettle" ,nettle)
       ("sqlite" ,sqlite)
       ("wxwidgets" ,wxwidgets)))
    (home-page "https://filezilla-project.org")
    (synopsis "Full-featured graphical FTP/FTPS/SFTP client")
    (description
     "Filezilla client supports FTP, FTP over SSL/TLS (FTPS),
SSH File Transfer Protocol (SFTP), HTTP/1.1, SOCKS5, FTP-Proxy, IPv6
and others features such as bookmarks, drag and drop, filename filters,
directory comparison and more.")
    (license gpl2+)
    (properties '((upstream-name . "FileZilla")))))
