;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
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

(define-module (gnu packages moreutils)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages docbook))

(define-public moreutils
  (package
    (name "moreutils")
    (version "0.57")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://debian/pool/main/m/moreutils/moreutils_"
                    version ".orig.tar.gz"))
              (sha256
               (base32
                "078dpkwwwrv8hxnylbc901kib2d1rr3hsja37j6dlpjfcfq58z9s"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)
              ("libxml2" ,libxml2)
              ("libxslt" ,libxslt)
              ("docbook-xml" ,docbook-xml-4.4)
              ("docbook-xsl" ,docbook-xsl)))
    (arguments
     `(#:phases
       (alist-replace
        'configure
        (lambda* (#:key inputs #:allow-other-keys)
          (use-modules (srfi srfi-1))
          (substitute* "Makefile"
            (("/usr/share/xml/.*/docbook.xsl")
             (let* ((docbook-xsl (assoc-ref inputs "docbook-xsl"))
                    (files (find-files docbook-xsl "^docbook\\.xsl$")))
               (find (lambda (file)
                       (string-suffix? "/manpages/docbook.xsl" file))
                     files)))))
        %standard-phases)
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "CC=gcc")))
    (home-page "http://joeyh.name/code/moreutils/")
    (synopsis "Miscellaneous general-purpose command-line tools")
    (description
     "Moreutils is a collection of general-purpose command-line tools to
augment the traditional Unix toolbox.")
    (license l:gpl2+)))
