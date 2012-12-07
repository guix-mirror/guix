;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module ((guix store) #:select (derivation-path?))
  #:use-module (guix utils)
  #:use-module (srfi srfi-26)
  #:export (%mirrors
            url-fetch))

;;; Commentary:
;;;
;;; Produce fixed-output derivations with data fetched over HTTP or FTP.
;;;
;;; Code:

(define %mirrors
  ;; Mirror lists used when `mirror://' URLs are passed.
  (let* ((gnu-mirrors
          '(;; This one redirects to a (supposedly) nearby and (supposedly)
            ;; up-to-date mirror.
            "http://ftpmirror.gnu.org/"

            "ftp://ftp.cs.tu-berlin.de/pub/gnu/"
            "ftp://ftp.chg.ru/pub/gnu/"
            "ftp://ftp.funet.fi/pub/mirrors/ftp.gnu.org/gnu/"

            ;; This one is the master repository, and thus it's always
            ;; up-to-date.
            "http://ftp.gnu.org/pub/gnu/")))
    `((gnu ,@gnu-mirrors)
      (gcc
       "ftp://ftp.nluug.nl/mirror/languages/gcc/"
       "ftp://ftp.fu-berlin.de/unix/languages/gcc/"
       "ftp://ftp.irisa.fr/pub/mirrors/gcc.gnu.org/gcc/"
       "ftp://gcc.gnu.org/pub/gcc/"
       ,@(map (cut string-append <> "/gcc") gnu-mirrors))
      (gnupg
       "ftp://gd.tuwien.ac.at/privacy/gnupg/"
       "ftp://gnupg.x-zone.org/pub/gnupg/"
       "ftp://ftp.gnupg.cz/pub/gcrypt/"
       "ftp://sunsite.dk/pub/security/gcrypt/"
       "http://gnupg.wildyou.net/"
       "http://ftp.gnupg.zone-h.org/"
       "ftp://ftp.jyu.fi/pub/crypt/gcrypt/"
       "ftp://trumpetti.atm.tut.fi/gcrypt/"
       "ftp://mirror.cict.fr/gnupg/"
       "ftp://ftp.strasbourg.linuxfr.org/pub/gnupg/")
      (savannah
       "http://download.savannah.gnu.org/releases/"
       "ftp://ftp.twaren.net/Unix/NonGNU/"
       "ftp://mirror.csclub.uwaterloo.ca/nongnu/"
       "ftp://mirror.publicns.net/pub/nongnu/"
       "ftp://savannah.c3sl.ufpr.br/"
       "http://ftp.cc.uoc.gr/mirrors/nongnu.org/"
       "http://ftp.twaren.net/Unix/NonGNU/"
       "http://mirror.csclub.uwaterloo.ca/nongnu/"
       "http://nongnu.askapache.com/"
       "http://savannah.c3sl.ufpr.br/"
       "http://www.centervenus.com/mirrors/nongnu/")
      (sourceforge
       "http://prdownloads.sourceforge.net/"
       "http://heanet.dl.sourceforge.net/sourceforge/"
       "http://surfnet.dl.sourceforge.net/sourceforge/"
       "http://dfn.dl.sourceforge.net/sourceforge/"
       "http://mesh.dl.sourceforge.net/sourceforge/"
       "http://ovh.dl.sourceforge.net/sourceforge/"
       "http://osdn.dl.sourceforge.net/sourceforge/"
       "http://kent.dl.sourceforge.net/sourceforge/")
      (kernel.org
       "http://www.all.kernel.org/pub/"
       "http://ramses.wh2.tu-dresden.de/pub/mirrors/kernel.org/"
       "http://linux-kernel.uio.no/pub/"
       "http://kernel.osuosl.org/pub/"
       "ftp://ftp.funet.fi/pub/mirrors/ftp.kernel.org/pub/"))))


(define* (url-fetch store url hash-algo hash
                    #:optional name
                    #:key (system (%current-system)) guile
                    (mirrors %mirrors))
  "Return the path of a fixed-output derivation in STORE that fetches
URL (a string, or a list of strings denoting alternate URLs), which is
expected to have hash HASH of type HASH-ALGO (a symbol).  By default,
the file name is the base name of URL; optionally, NAME can specify a
different file name.

When one of the URL starts with mirror://, then its host part is
interpreted as the name of a mirror scheme, taken from MIRRORS; MIRRORS
must be a list of symbol/URL-list pairs."
  (define builder
    `(begin
       (use-modules (guix build download))
       (url-fetch ',url %output
                  #:mirrors ',mirrors)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system))
      ((and (? string?) (? derivation-path?))
       guile)
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(distro packages base)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system)))))

  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))

  (build-expression->derivation store (or name file-name) system
                                builder '()
                                #:hash-algo hash-algo
                                #:hash hash
                                #:modules '((guix build download)
                                            (guix build utils)
                                            (guix ftp-client))
                                #:guile-for-build guile-for-build))

;;; download.scm ends here
