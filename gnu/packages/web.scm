;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Aljosha Papsch <misc@rpapsch.de>
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

(define-module (gnu packages web)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'l:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl))

(define-public httpd
  (package
    (name "httpd")
    (version "2.4.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://apache/httpd/httpd-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1sig08xxq1kqxr2a42ndwr9g4mm6zdqnxldhxraym2y0xcjkd7yw"))))
    (build-system gnu-build-system)
    (inputs `(("apr" ,apr)
              ("apr-util" ,apr-util)
              ("openssl" ,openssl)
              ("pcre" ,pcre)))
    (arguments
     `(#:test-target "test"
       #:configure-flags (list "--enable-rewrite"
                               "--enable-userdir"
                               "--enable-vhost-alias"
                               "--enable-ssl"
                               "--enable-mime-magic"
                               (string-append "--sysconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc/httpd"))))
    (synopsis "Featureful HTTP server")
    (description
     "The Apache HTTP Server Project is a collaborative software development
effort aimed at creating a robust, commercial-grade, featureful, and
freely-available source code implementation of an HTTP (Web) server. The project
is jointly managed by a group of volunteers located around the world, using the
Internet and the Web to communicate, plan, and develop the server and its
related documentation.")
    (license l:asl2.0)
    (home-page "https://httpd.apache.org/")))

(define-public perl-html-tagset
  (package
    (name "perl-html-tagset")
    (version "3.20")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/P/PE/PETDANCE/HTML-Tagset-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1qh8249wgr4v9vgghq77zh1d2zs176bir223a8gh3k9nksn7vcdd"))))
    (build-system perl-build-system)
    (license (package-license perl))
    (synopsis "Perl data tables useful in parsing HTML")
    (description
     "The HTML::Tagset module contains several data tables useful in various
kinds of HTML parsing operations.")
    (home-page "http://search.cpan.org/dist/HTML-Tagset/")))

