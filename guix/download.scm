;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module ((guix store) #:select (derivation-path? add-to-store))
  #:use-module ((guix build download) #:renamer (symbol-prefix-proc 'build:))
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (%mirrors
            url-fetch
            download-to-store))

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
      (gnome
       "http://ftp.belnet.be/ftp.gnome.org/"
       "http://ftp.linux.org.uk/mirrors/ftp.gnome.org/"
       "http://ftp.gnome.org/pub/GNOME/"
       "http://mirror.yandex.ru/mirrors/ftp.gnome.org/")
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
       "http://osdn.dl.sourceforge.net/sourceforge/")
      (kernel.org
       "http://www.all.kernel.org/pub/"
       "http://ramses.wh2.tu-dresden.de/pub/mirrors/kernel.org/"
       "http://linux-kernel.uio.no/pub/"
       "http://kernel.osuosl.org/pub/"
       "ftp://ftp.funet.fi/pub/mirrors/ftp.kernel.org/pub/")
      (apache             ; from http://www.apache.org/mirrors/dist.html
       "http://www.eu.apache.org/dist/"
       "http://www.us.apache.org/dist/"
       "ftp://gd.tuwien.ac.at/pub/infosys/servers/http/apache/dist/"
       "http://apache.belnet.be/"
       "http://mirrors.ircam.fr/pub/apache/"
       "http://apache-mirror.rbc.ru/pub/apache/")
      (xorg               ; from http://www.x.org/wiki/Releases/Download
       "http://xorg.freedesktop.org/releases/" ; main mirrors
       "http://www.x.org/pub/"
       "ftp://mirror.csclub.uwaterloo.ca/x.org/" ; North America
       "ftp://xorg.mirrors.pair.com/"
       "http://mirror.csclub.uwaterloo.ca/x.org/"
       "http://xorg.mirrors.pair.com/"
       "http://mirror.us.leaseweb.net/xorg/"
       "ftp://artfiles.org/x.org/" ; Europe
       "ftp://ftp.chg.ru/pub/X11/x.org/"
       "ftp://ftp.fu-berlin.de/unix/X11/FTP.X.ORG/"
       "ftp://ftp.gwdg.de/pub/x11/x.org/"
       "ftp://ftp.mirrorservice.org/sites/ftp.x.org/"
       "ftp://ftp.ntua.gr/pub/X11/"
       "ftp://ftp.piotrkosoft.net/pub/mirrors/ftp.x.org/"
       "ftp://ftp.portal-to-web.de/pub/mirrors/x.org/"
       "ftp://ftp.solnet.ch/mirror/x.org/"
       "ftp://ftp.sunet.se/pub/X11/"
       "ftp://gd.tuwien.ac.at/X11/"
       "ftp://mi.mirror.garr.it/mirrors/x.org/"
       "ftp://mirror.cict.fr/x.org/"
       "ftp://mirror.switch.ch/mirror/X11/"
       "ftp://mirrors.ircam.fr/pub/x.org/"
       "ftp://x.mirrors.skynet.be/pub/ftp.x.org/"
       "ftp://ftp.cs.cuhk.edu.hk/pub/X11" ; East Asia
       "ftp://ftp.u-aizu.ac.jp/pub/x11/x.org/"
       "ftp://ftp.yz.yamagata-u.ac.jp/pub/X11/x.org/"
       "ftp://ftp.kaist.ac.kr/x.org/"
       "ftp://mirrors.go-part.com/xorg/"
       "http://x.cs.pu.edu.tw/"
       "ftp://ftp.is.co.za/pub/x.org")            ; South Africa
      (cpan                              ; from http://www.cpan.org/SITES.html
       "http://cpan.enstimac.fr/"
       "ftp://ftp.ciril.fr/pub/cpan/"
       "ftp://artfiles.org/cpan.org/"
       "http://www.cpan.org/"
       "ftp://cpan.rinet.ru/pub/mirror/CPAN/"
       "http://cpan.cu.be/"
       "ftp://cpan.inode.at/"
       "ftp://cpan.iht.co.il/"
       "ftp://ftp.osuosl.org/pub/CPAN/"
       "ftp://ftp.nara.wide.ad.jp/pub/CPAN/"
       "http://mirrors.163.com/cpan/"
       "ftp://cpan.mirror.ac.za/"))))

(define (gnutls-derivation store system)
  "Return the GnuTLS derivation for SYSTEM."
  (let* ((module (resolve-interface '(gnu packages gnutls)))
         (gnutls (module-ref module 'gnutls)))
    (package-derivation store gnutls system)))

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
       (let* ((distro (resolve-interface '(gnu packages base)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system)))))

  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))

  (define need-gnutls?
    ;; True if any of the URLs need TLS support.
    (let ((https? (cut string-prefix? "https://" <>)))
      (match url
        ((? string?)
         (https? url))
        ((url ...)
         (any https? url)))))

  (let*-values (((gnutls-drv-path gnutls-drv)
                 (if need-gnutls?
                     (gnutls-derivation store system)
                     (values #f #f)))
                ((gnutls)
                 (and gnutls-drv
                      (derivation-output-path
                       (assoc-ref (derivation-outputs gnutls-drv)
                                  "out"))))
                ((env-vars)
                 (if gnutls
                     (let ((dir (string-append gnutls "/share/guile/site")))
                       ;; XXX: `GUILE_LOAD_COMPILED_PATH' is overridden
                       ;; by `build-expression->derivation', so we can't
                       ;; set it here.
                       `(("GUILE_LOAD_PATH" . ,dir)))
                     '())))
    (build-expression->derivation store (or name file-name) system
                                  builder
                                  (if gnutls-drv
                                      `(("gnutls" ,gnutls-drv-path))
                                      '())
                                  #:hash-algo hash-algo
                                  #:hash hash
                                  #:modules '((guix build download)
                                              (guix build utils)
                                              (guix ftp-client))
                                  #:guile-for-build guile-for-build
                                  #:env-vars env-vars)))

(define* (download-to-store store url #:optional (name (basename url))
                            #:key (log (current-error-port)))
  "Download from URL to STORE, either under NAME or URL's basename if
omitted.  Write progress reports to LOG."
  (call-with-temporary-output-file
   (lambda (temp port)
     (let ((result
            (parameterize ((current-output-port log))
              (build:url-fetch url temp #:mirrors %mirrors))))
       (close port)
       (and result
            (add-to-store store name #f "sha256" temp))))))

;;; download.scm ends here
