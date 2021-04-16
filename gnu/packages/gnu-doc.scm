;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages gnu-doc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages texinfo)

  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix cvs-download)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public miscfiles
  (package
    (name "miscfiles")
    (version "1.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/" name "/" name "-"
                          version ".tar.gz"))
      (sha256
       (base32
        "005588vfrwx8ghsdv9p7zczj9lbc9a3r4m5aphcaqv8gif4siaka"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/miscfiles/")
    (synopsis
     "Data files for airport codes, zip codes, a dictionary, and more")
    (description
     "GNU Miscfiles is a collection of common data files.  They include, for
example, country abbreviations, names and capital cities; currency
abbreviations and names; a Best Current Practices index; a map of the ASCII
character set; a list of three-letter airport codes; and an English word
list.")
    (license gpl2+)))

(define-public gnu-standards
  (package
    (name "gnu-standards")
    (version "2020-11-25")
    (source
     (origin
       (method cvs-fetch)
       (uri (cvs-reference
             (root-directory
              ":pserver:anonymous@cvs.savannah.gnu.org:/sources/gnustandards")
             (module "gnustandards")
             (revision version)))
       (sha256
        (base32
         "1xlwmgcnvp81ipgfir4ckpgl922mbckvxy1x758r0lksq5vrpglj"))
       (file-name (string-append name "-" version "-checkout"))))
    (build-system trivial-build-system)
    (native-inputs `(("gzip" ,gzip)
                     ("source" ,source)
                     ("texinfo" ,texinfo)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
        (begin
          (use-modules (guix build utils))
          (let ((gzip (assoc-ref %build-inputs "gzip"))
                (source (assoc-ref %build-inputs "source"))
                (texinfo (assoc-ref %build-inputs "texinfo"))
                (info-dir (string-append %output "/share/info")))
            (setenv "PATH" (string-append gzip "/bin"
                                          ":" texinfo "/bin"))
            (mkdir-p info-dir)
            (invoke "makeinfo" "--output" info-dir
                    (string-append source "/maintain.texi"))
            (invoke "makeinfo" "--output" info-dir
                    (string-append source "/standards.texi"))
            (invoke "gzip" (string-append info-dir "/maintain.info"))
            (invoke "gzip" (string-append info-dir "/standards.info"))))))
    (home-page "https://www.gnu.org/prep/standards/")
    (synopsis "GNU coding standards and maintainer information")
    (description "The GNU Coding Standards were written by Richard Stallman
and other GNU Project volunteers.  Their purpose is to make the GNU system
clean, consistent, and easy to install.

The information for maintainers of GNU software has guidelines and advice for
someone who is the maintainer of a GNU program on behalf of the GNU Project.")
    (license fdl1.3+)))
