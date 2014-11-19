;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((guix licenses) #:select (gpl3+ clarified-artistic))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages compression))

(define-public lftp
  (package
    (name "lftp")
    (version "4.4.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://lftp.yar.ru/ftp/lftp-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0s38vc2ij869dwx3i1c7sk96mqv0hknf3cqf86av59rqnix0px3m"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("zlib" ,zlib)
       ("readline" ,readline)
       ("gnutls" ,gnutls)))
    (home-page "http://lftp.yar.ru/")
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
     '(#:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; This is an old 'configure' script that doesn't
                   ;; understand variables passed as arguments.
                   (let ((out (assoc-ref outputs "out")))
                     (setenv "CONFIG_SHELL" (which "sh"))
                     (setenv "SHELL" (which "sh"))
                     (zero? (system* "./configure"
                                     (string-append "--prefix=" out)))))
                 %standard-phases)
       #:tests? #f))                              ;there are no tests
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://www.ncftp.com/ncftp/")
    (synopsis "Command-line File Transfer Protocol (FTP) client")
    (description
     "NcFTP Client (or just NcFTP) is a set of command-line programs to access
File Transfer Protocol (FTP) servers.  This includes 'ncftp', an interactive
FTP browser, as well as non-interactive commands such as 'ncftpput' and
'ncftpget'.")
    (license clarified-artistic)))
