;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Guy Fleury Iteriteka <hoonandon@gmail.com>
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
  #:use-module (guix store)
  #:use-module ((guix build download) #:prefix build:)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%mirrors
            url-fetch
            url-fetch/tarbomb
            url-fetch/zipbomb
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
            "https://ftpmirror.gnu.org/gnu/"

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
       "http://artfiles.org/gnupg.org"
       "http://www.crysys.hu/"
       "https://gnupg.org/ftp/gcrypt/"
       "ftp://mirrors.dotsrc.org/gcrypt/"
       "ftp://mirror.cict.fr/gnupg/"
       "ftp://ftp.franken.de/pub/crypt/mirror/ftp.gnupg.org/gcrypt/"
       "ftp://ftp.freenet.de/pub/ftp.gnupg.org/gcrypt/"
       "ftp://ftp.hi.is/pub/mirrors/gnupg/"
       "ftp://ftp.heanet.ie/mirrors/ftp.gnupg.org/gcrypt/"
       "ftp://ftp.bit.nl/mirror/gnupg/"
       "ftp://ftp.surfnet.nl/pub/security/gnupg/"
       "ftp://ftp.iasi.roedu.net/pub/mirrors/ftp.gnupg.org/"
       "ftp://ftp.sunet.se/pub/security/gnupg/"
       "ftp://mirror.switch.ch/mirror/gnupg/"
       "ftp://mirror.tje.me.uk/pub/mirrors/ftp.gnupg.org/"
       "ftp://ftp.mirrorservice.org/sites/ftp.gnupg.org/gcrypt/"
       "ftp://ftp.ring.gr.jp/pub/net/gnupg/"
       "ftp://ftp.gnupg.org/gcrypt/")
      (gnome
       "http://ftp.belnet.be/ftp.gnome.org/"
       "http://ftp.linux.org.uk/mirrors/ftp.gnome.org/"
       "http://ftp.gnome.org/pub/GNOME/"
       "https://download.gnome.org/"
       "http://mirror.yandex.ru/mirrors/ftp.gnome.org/")
      (hackage
       "http://hackage.haskell.org/")
      (savannah
       "http://download.savannah.gnu.org/releases/"
       "http://ftp.cc.uoc.gr/mirrors/nongnu.org/"
       "http://ftp.twaren.net/Unix/NonGNU/"
       "http://mirror.csclub.uwaterloo.ca/nongnu/"
       "http://nongnu.askapache.com/"
       "http://savannah.c3sl.ufpr.br/"
       "http://download.savannah.gnu.org/releases-noredirect/"
       "http://download-mirror.savannah.gnu.org/releases/"
       "ftp://ftp.twaren.net/Unix/NonGNU/"
       "ftp://mirror.csclub.uwaterloo.ca/nongnu/"
       "ftp://mirror.publicns.net/pub/nongnu/"
       "ftp://savannah.c3sl.ufpr.br/")
      (sourceforge ; https://sourceforge.net/p/forge/documentation/Mirrors/
       "http://downloads.sourceforge.net/project/"
       "http://ufpr.dl.sourceforge.net/project/"
       "http://heanet.dl.sourceforge.net/project/"
       "http://freefr.dl.sourceforge.net/project/"
       "http://internode.dl.sourceforge.net/project/"
       "http://jaist.dl.sourceforge.net/project/"
       "http://kent.dl.sourceforge.net/project/"
       "http://liquidtelecom.dl.sourceforge.net/project/"
       ;; "http://nbtelecom.dl.sourceforge.net/project/"  ;never returns 404s
       "http://nchc.dl.sourceforge.net/project/"
       "http://ncu.dl.sourceforge.net/project/"
       "http://netcologne.dl.sourceforge.net/project/"
       "http://netix.dl.sourceforge.net/project/"
       "http://pilotfiber.dl.sourceforge.net/project/"
       "http://superb-sea2.dl.sourceforge.net/project/"
       "http://tenet.dl.sourceforge.net/project/"
       "http://vorboss.dl.sourceforge.net/project/"
       "http://netassist.dl.sourceforge.net/project/")
      (netfilter.org ; https://www.netfilter.org/mirrors.html
       "http://ftp.netfilter.org/pub/"
       "ftp://ftp.es.netfilter.org/mirrors/netfilter/"
       "ftp://ftp.hu.netfilter.org/"
       "ftp://www.lt.netfilter.org/pub/")
      (kernel.org
       "http://ramses.wh2.tu-dresden.de/pub/mirrors/kernel.org/"
       "http://linux-kernel.uio.no/pub/"
       "http://kernel.osuosl.org/pub/"
       "http://ftp.be.debian.org/pub/"
       "http://mirror.linux.org.au/"
       "ftp://ftp.funet.fi/pub/mirrors/ftp.kernel.org/pub/")
      (apache             ; from http://www.apache.org/mirrors/dist.html
       "http://www.eu.apache.org/dist/"
       "http://www.us.apache.org/dist/"
       "http://apache.belnet.be/"
       "http://mirrors.ircam.fr/pub/apache/"
       "http://apache-mirror.rbc.ru/pub/apache/"

       ;; As a last resort, try the archive.
       "http://archive.apache.org/dist/")
      (xorg               ; from http://www.x.org/wiki/Releases/Download
       "http://www.x.org/releases/" ; main mirrors
       "http://mirror.csclub.uwaterloo.ca/x.org/" ; North America
       "http://xorg.mirrors.pair.com/"
       "http://mirror.us.leaseweb.net/xorg/"
       "ftp://mirror.csclub.uwaterloo.ca/x.org/"
       "ftp://xorg.mirrors.pair.com/"
       "ftp://artfiles.org/x.org/" ; Europe
       "ftp://ftp.chg.ru/pub/X11/x.org/"
       "ftp://ftp.fu-berlin.de/unix/X11/FTP.X.ORG/"
       "ftp://ftp.gwdg.de/pub/x11/x.org/"
       "ftp://ftp.mirrorservice.org/sites/ftp.x.org/"
       "ftp://ftp.ntua.gr/pub/X11/"
       "ftp://ftp.piotrkosoft.net/pub/mirrors/ftp.x.org/"
       "ftp://ftp.portal-to-web.de/pub/mirrors/x.org/"
       "ftp://ftp.solnet.ch/mirror/x.org/"
       "ftp://mi.mirror.garr.it/mirrors/x.org/"
       "ftp://mirror.cict.fr/x.org/"
       "ftp://mirror.switch.ch/mirror/X11/"
       "ftp://mirrors.ircam.fr/pub/x.org/"
       "ftp://x.mirrors.skynet.be/pub/ftp.x.org/"
       "http://x.cs.pu.edu.tw/" ; East Asia
       "ftp://ftp.cs.cuhk.edu.hk/pub/X11"
       "ftp://ftp.u-aizu.ac.jp/pub/x11/x.org/"
       "ftp://ftp.yz.yamagata-u.ac.jp/pub/X11/x.org/"
       "ftp://ftp.kaist.ac.kr/x.org/"
       "ftp://mirrors.go-part.com/xorg/"
       "ftp://ftp.is.co.za/pub/x.org")            ; South Africa
      (cpan
       "http://www.cpan.org/"
       "http://cpan.metacpan.org/"
       ;; A selection of HTTP mirrors from http://www.cpan.org/SITES.html.
       ;; Europe.
       "http://ftp.belnet.be/mirror/ftp.cpan.org/"
       "http://mirrors.nic.cz/CPAN/"
       "http://mirror.ibcp.fr/pub/CPAN/"
       "http://ftp.ntua.gr/pub/lang/perl/"
       "http://kvin.lv/pub/CPAN/"
       "http://mirror.as43289.net/pub/CPAN/"
       "http://cpan.cs.uu.nl/"
       "http://cpan.uib.no/"
       "http://cpan-mirror.rbc.ru/pub/CPAN/"
       "http://mirror.sbb.rs/CPAN/"
       "http://cpan.lnx.sk/"
       "http://ftp.rediris.es/mirror/CPAN/"
       "http://mirror.ox.ac.uk/sites/www.cpan.org/"
       ;; Africa.
       "http://mirror.liquidtelecom.com/CPAN/"
       "http://cpan.mirror.ac.za/"
       "http://mirror.is.co.za/pub/cpan/"
       "http://cpan.saix.net/"
       "http://mirror.ucu.ac.ug/cpan/"
       ;; North America.
       "http://mirrors.gossamer-threads.com/CPAN/"
       "http://mirror.csclub.uwaterloo.ca/CPAN/"
       "http://mirrors.ucr.ac.cr/CPAN/"
       "http://www.msg.com.mx/CPAN/"
       "http://mirrors.namecheap.com/CPAN/"
       "http://mirror.uic.edu/CPAN/"
       "http://mirror.datapipe.net/CPAN/"
       "http://mirror.cc.columbia.edu/pub/software/cpan/"
       "http://mirror.uta.edu/CPAN/"
       ;; South America.
       "http://cpan.mmgdesigns.com.ar/"
       "http://mirror.nbtelecom.com.br/CPAN/"
       "http://linorg.usp.br/CPAN/"
       "http://cpan.dcc.uchile.cl/"
       "http://mirror.cedia.org.ec/CPAN/"
       ;; Oceania.
       "http://cpan.mirror.serversaustralia.com.au/"
       "http://mirror.waia.asn.au/pub/cpan/"
       "http://mirror.as24220.net/pub/cpan/"
       "http://cpan.lagoon.nc/pub/CPAN/"
       "http://cpan.inspire.net.nz/"
       ;; Asia.
       "http://mirror.dhakacom.com/CPAN/"
       "http://mirrors.ustc.edu.cn/CPAN/"
       "http://ftp.cuhk.edu.hk/pub/packages/perl/CPAN/"
       "http://kambing.ui.ac.id/cpan/"
       "http://cpan.hostiran.ir/"
       "http://ftp.nara.wide.ad.jp/pub/CPAN/"
       "http://mirror.neolabs.kz/CPAN/"
       "http://cpan.nctu.edu.tw/"
       "http://cpan.ulak.net.tr/"
       "http://mirrors.vinahost.vn/CPAN/")
      (cran
       ;; Arbitrary mirrors from http://cran.r-project.org/mirrors.html
       ;; This one automatically redirects to servers worldwide
       "http://cran.r-project.org/"
       "http://cran.rstudio.com/"
       "http://cran.univ-lyon1.fr/"
       "http://cran.ism.ac.jp/"
       "http://cran.stat.auckland.ac.nz/"
       "http://cran.mirror.ac.za/"
       "http://cran.csie.ntu.edu.tw/")
      (imagemagick
       ;; from http://www.imagemagick.org/script/download.php
       ;; (without mirrors that are unavailable or not up to date)
       ;; mirrors keeping old versions at the top level
       "https://sunsite.icm.edu.pl/packages/ImageMagick/"
       ;; mirrors moving old versions to "legacy"
       "http://mirrors-usa.go-parts.com/mirrors/ImageMagick/"
       "http://mirror.checkdomain.de/imagemagick/"
       "http://ftp.surfnet.nl/pub/ImageMagick/"
       "http://mirror.searchdaimon.com/ImageMagick"
       "http://mirror.is.co.za/pub/imagemagick/"
       "http://www.imagemagick.org/download/"
       "ftp://mirror.aarnet.edu.au/pub/imagemagick/"
       "ftp://ftp.kddlabs.co.jp/graphics/ImageMagick/"
       "ftp://ftp.u-aizu.ac.jp/pub/graphics/image/ImageMagick/imagemagick.org/"
       "ftp://ftp.nluug.nl/pub/ImageMagick/"
       "ftp://ftp.tpnet.pl/pub/graphics/ImageMagick/"
       "ftp://ftp.fifi.org/pub/ImageMagick/"
       ;; one legacy location as a last resort
       "http://www.imagemagick.org/download/legacy/")
      (debian
       "http://ftp.de.debian.org/debian/"
       "http://ftp.fr.debian.org/debian/"
       "http://ftp.debian.org/debian/"
       "http://archive.debian.org/debian/")
      (kde
       "http://download.kde.org"
       "http://download.kde.org/Attic" ; for when it gets archived.
       ;; Mirrors from http://files.kde.org/extra/mirrors.html
       ;; Europe
       "http://mirror.easyname.at/kde"
       "http://mirror.karneval.cz/pub/kde"
       "http://ftp.fi.muni.cz/pub/kde/"
       "http://mirror.oss.maxcdn.com/kde/"
       "http://ftp5.gwdg.de/pub/linux/kde/"
       "http://ftp-stud.fht-esslingen.de/Mirrors/ftp.kde.org/pub/kde/"
       "http://mirror.klaus-uwe.me/kde/ftp/"
       "http://kde.beta.mirror.ga/"
       "http://kde.alpha.mirror.ga/"
       "http://mirror.netcologne.de/kde"
       "http://vesta.informatik.rwth-aachen.de/ftp/pub/mirror/kde/"
       "http://ftp.rz.uni-wuerzburg.de/pub/unix/kde/"
       "http://mirrors.dotsrc.org/kde/"
       "http://ftp.funet.fi/pub/mirrors/ftp.kde.org/pub/kde/"
       "http://kde-mirror.freenux.org/"
       "http://mirrors.ircam.fr/pub/KDE/"
       "http://www-ftp.lip6.fr/pub/X11/kde/"
       "http://fr2.rpmfind.net/linux/KDE/"
       "http://kde.mirror.anlx.net/"
       "http://www.mirrorservice.org/sites/ftp.kde.org/pub/kde/"
       "http://ftp.heanet.ie/mirrors/ftp.kde.org/"
       "http://ftp.nluug.nl/pub/windowing/kde/"
       "http://ftp.surfnet.nl/windowing/kde/"
       "http://ftp.icm.edu.pl/pub/unix/kde/"
       "http://ftp.pbone.net/pub/kde/"
       "http://piotrkosoft.net/pub/mirrors/ftp.kde.org/"
       "http://mirrors.fe.up.pt/pub/kde/"
       "http://ftp.iasi.roedu.net/pub/mirrors/ftp.kde.org/"
       "http://ftp.acc.umu.se/mirror/kde.org/ftp/"
       "http://kde.ip-connect.vn.ua/"
       ;; North America
       "http://mirror.its.dal.ca/kde/"
       "http://mirror.csclub.uwaterloo.ca/kde/"
       "http://mirror.cc.columbia.edu/pub/software/kde/"
       "http://mirrors-usa.go-parts.com/kde"
       "http://kde.mirrors.hoobly.com/"
       "http://ftp.ussg.iu.edu/kde/"
       "http://mirrors.mit.edu/kde/"
       "http://kde.mirrors.tds.net/pub/kde/"
       ;; Oceania
       "http://ftp.kddlabs.co.jp/pub/X11/kde/"
       "http://kde.mirror.uber.com.au/")
      (openbsd
       "https://ftp.openbsd.org/pub/OpenBSD/"
       ;; Anycast CDN redirecting to your friendly local mirror.
       "https://mirrors.evowise.com/pub/OpenBSD/"
       ;; Other HTTPS mirrors from https://www.openbsd.org/ftp.html
       "https://mirror.aarnet.edu.au/pub/OpenBSD/"
       "https://ftp2.eu.openbsd.org/pub/OpenBSD/"
       "https://openbsd.c3sl.ufpr.br/pub/OpenBSD/"
       "https://openbsd.ipacct.com/pub/OpenBSD/"
       "https://ftp.OpenBSD.org/pub/OpenBSD/"
       "https://openbsd.cs.toronto.edu/pub/OpenBSD/"
       "https://openbsd.delfic.org/pub/OpenBSD/"
       "https://openbsd.mirror.netelligent.ca/pub/OpenBSD/"
       "https://mirrors.ucr.ac.cr/pub/OpenBSD/"
       "https://mirrors.dotsrc.org/pub/OpenBSD/"
       "https://mirror.one.com/pub/OpenBSD/"
       "https://ftp.fr.openbsd.org/pub/OpenBSD/"
       "https://ftp2.fr.openbsd.org/pub/OpenBSD/"
       "https://mirrors.ircam.fr/pub/OpenBSD/"
       "https://ftp.spline.de/pub/OpenBSD/"
       "https://mirror.hs-esslingen.de/pub/OpenBSD/"
       "https://ftp.halifax.rwth-aachen.de/openbsd/"
       "https://ftp.hostserver.de/pub/OpenBSD/"
       "https://ftp.fau.de/pub/OpenBSD/"
       "https://ftp.cc.uoc.gr/pub/OpenBSD/"
       "https://openbsd.hk/pub/OpenBSD/"
       "https://ftp.heanet.ie/pub/OpenBSD/"
       "https://openbsd.mirror.garr.it/pub/OpenBSD/"
       "https://mirror.litnet.lt/pub/OpenBSD/"
       "https://mirror.meerval.net/pub/OpenBSD/"
       "https://ftp.nluug.nl/pub/OpenBSD/"
       "https://ftp.bit.nl/pub/OpenBSD/"
       "https://mirrors.dalenys.com/pub/OpenBSD/"
       "https://ftp.icm.edu.pl/pub/OpenBSD/"
       "https://ftp.rnl.tecnico.ulisboa.pt/pub/OpenBSD/"
       "https://mirrors.pidginhost.com/pub/OpenBSD/"
       "https://mirror.yandex.ru/pub/OpenBSD/"
       "https://ftp.eu.openbsd.org/pub/OpenBSD/"
       "https://ftp.yzu.edu.tw/pub/OpenBSD/"
       "https://www.mirrorservice.org/pub/OpenBSD/"
       "https://anorien.csc.warwick.ac.uk/pub/OpenBSD/"
       "https://mirror.bytemark.co.uk/pub/OpenBSD/"
       "https://mirrors.sonic.net/pub/OpenBSD/"
       "https://ftp3.usa.openbsd.org/pub/OpenBSD/"
       "https://mirrors.syringanetworks.net/pub/OpenBSD/"
       "https://openbsd.mirror.constant.com/pub/OpenBSD/"
       "https://ftp4.usa.openbsd.org/pub/OpenBSD/"
       "https://ftp5.usa.openbsd.org/pub/OpenBSD/"
       "https://mirror.esc7.net/pub/OpenBSD/")
      (mate
       "https://pub.mate-desktop.org/releases/"
       "http://pub.mate-desktop.org/releases/"))))

(define %mirror-file
  ;; Copy of the list of mirrors to a file.  This allows us to keep a single
  ;; copy in the store, and computing it here avoids repeated calls to
  ;; 'object->string'.
  (plain-file "mirrors" (object->string %mirrors)))

(define %content-addressed-mirrors
  ;; List of content-addressed mirrors.  Each mirror is represented as a
  ;; procedure that takes a file name, an algorithm (symbol) and a hash
  ;; (bytevector), and returns a URL or #f.
  '(begin
     (use-modules (guix base32))

     (define (guix-publish host)
       (lambda (file algo hash)
         ;; Files served by 'guix publish' are accessible under a single
         ;; hash algorithm.
         (string-append "https://" host "/file/"
                        file "/" (symbol->string algo) "/"
                        (bytevector->nix-base32-string hash))))

     ;; XXX: (guix base16) appeared in March 2017 (and thus 0.13.0) so old
     ;; installations of the daemon might lack it.  Thus, load it lazily to
     ;; avoid gratuitous errors.  See <https://bugs.gnu.org/33542>.
     (module-autoload! (current-module)
                       '(guix base16) '(bytevector->base16-string))

     (list (guix-publish "mirror.hydra.gnu.org")
           (guix-publish "berlin.guixsd.org")
           (lambda (file algo hash)
             ;; 'tarballs.nixos.org' supports several algorithms.
             (string-append "https://tarballs.nixos.org/"
                            (symbol->string algo) "/"
                            (bytevector->nix-base32-string hash)))
           (lambda (file algo hash)
             ;; Software Heritage usually archives VCS history rather than
             ;; tarballs, but tarballs are sometimes available (and can be
             ;; explicitly stored there.)  For example, see
             ;; <https://archive.softwareheritage.org/api/1/content/sha256:92d0fa1c311cacefa89853bdb53c62f4110cdfda3820346b59cbd098f40f955e/>.
             (string-append "https://archive.softwareheritage.org/api/1/content/"
                            (symbol->string algo) ":"
                            (bytevector->base16-string hash) "/raw/")))))

(define %content-addressed-mirror-file
  ;; Content-addressed mirrors stored in a file.
  (plain-file "content-addressed-mirrors"
              (object->string %content-addressed-mirrors)))

(define built-in-builders*
  (store-lift built-in-builders))

(define* (built-in-download file-name url
                            #:key system hash-algo hash
                            mirrors content-addressed-mirrors
                            (guile 'unused))
  "Download FILE-NAME from URL using the built-in 'download' builder.

This is an \"out-of-band\" download in that the returned derivation does not
explicitly depend on Guile, GnuTLS, etc.  Instead, the daemon performs the
download by itself using its own dependencies."
  (mlet %store-monad ((mirrors (lower-object mirrors))
                      (content-addressed-mirrors
                       (lower-object content-addressed-mirrors)))
    (raw-derivation file-name "builtin:download" '()
                    #:system system
                    #:hash-algo hash-algo
                    #:hash hash
                    #:inputs `((,mirrors)
                               (,content-addressed-mirrors))

                    ;; Honor the user's proxy and locale settings.
                    #:leaked-env-vars '("http_proxy" "https_proxy"
                                        "LC_ALL" "LC_MESSAGES" "LANG"
                                        "COLUMNS")

                    #:env-vars `(("url" . ,(object->string url))
                                 ("mirrors" . ,mirrors)
                                 ("content-addressed-mirrors"
                                  . ,content-addressed-mirrors))

                    ;; Do not offload this derivation because we cannot be
                    ;; sure that the remote daemon supports the 'download'
                    ;; built-in.  We may remove this limitation when support
                    ;; for that built-in is widespread.
                    #:local-build? #t)))

(define* (url-fetch url hash-algo hash
                    #:optional name
                    #:key (system (%current-system))
                    (guile (default-guile)))
  "Return a fixed-output derivation that fetches URL (a string, or a list of
strings denoting alternate URLs), which is expected to have hash HASH of type
HASH-ALGO (a symbol).  By default, the file name is the base name of URL;
optionally, NAME can specify a different file name.

When one of the URL starts with mirror://, then its host part is
interpreted as the name of a mirror scheme, taken from %MIRROR-FILE.

Alternately, when URL starts with file://, return the corresponding file name
in the store."
  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))

  (let ((uri (and (string? url) (string->uri url))))
    (if (or (and (string? url) (not uri))
            (and uri (memq (uri-scheme uri) '(#f file))))
        (interned-file (if uri (uri-path uri) url)
                       (or name file-name))
        (mlet %store-monad ((builtins (built-in-builders*)))
          ;; The "download" built-in builder was added in guix-daemon in
          ;; Nov. 2016 and made it in the 0.12.0 release of Dec. 2016.  We now
          ;; require it.
          (unless (member "download" builtins)
            (error "'guix-daemon' is too old, please upgrade" builtins))

          (built-in-download (or name file-name) url
                             #:guile guile
                             #:system system
                             #:hash-algo hash-algo
                             #:hash hash
                             #:mirrors %mirror-file
                             #:content-addressed-mirrors
                             %content-addressed-mirror-file)))))

(define* (url-fetch/tarbomb url hash-algo hash
                            #:optional name
                            #:key (system (%current-system))
                            (guile (default-guile)))
  "Similar to 'url-fetch' but unpack the file from URL in a directory of its
own.  This helper makes it easier to deal with \"tar bombs\"."
  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))
  (define gzip
    (module-ref (resolve-interface '(gnu packages compression)) 'gzip))
  (define tar
    (module-ref (resolve-interface '(gnu packages base)) 'tar))

  (mlet %store-monad ((drv (url-fetch url hash-algo hash
                                      (string-append "tarbomb-"
                                                     (or name file-name))
                                      #:system system
                                      #:guile guile)))
    ;; Take the tar bomb, and simply unpack it as a directory.
    ;; Use ungrafted tar/gzip so that the resulting tarball doesn't depend on
    ;; whether grafts are enabled.
    (gexp->derivation (or name file-name)
                      (with-imported-modules '((guix build utils))
                        #~(begin
                            (use-modules (guix build utils))
                            (mkdir #$output)
                            (setenv "PATH" (string-append #$gzip "/bin"))
                            (chdir #$output)
                            (invoke (string-append #$tar "/bin/tar")
                                    "xf" #$drv)))
                      #:graft? #f
                      #:local-build? #t)))

(define* (url-fetch/zipbomb url hash-algo hash
                            #:optional name
                            #:key (system (%current-system))
                            (guile (default-guile)))
  "Similar to 'url-fetch' but unpack the zip file at URL in a directory of its
own.  This helper makes it easier to deal with \"zip bombs\"."
  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))
  (define unzip
    (module-ref (resolve-interface '(gnu packages compression)) 'unzip))

  (mlet %store-monad ((drv (url-fetch url hash-algo hash
                                      (string-append "zipbomb-"
                                                     (or name file-name))
                                      #:system system
                                      #:guile guile)))
    ;; Take the zip bomb, and simply unpack it as a directory.
    ;; Use ungrafted unzip so that the resulting tarball doesn't depend on
    ;; whether grafts are enabled.
    (gexp->derivation (or name file-name)
                      (with-imported-modules '((guix build utils))
                        #~(begin
                            (use-modules (guix build utils))
                            (mkdir #$output)
                            (chdir #$output)
                            (invoke (string-append #$unzip "/bin/unzip")
                                    #$drv)))
                      #:graft? #f
                      #:local-build? #t)))

(define* (download-to-store store url #:optional (name (basename url))
                            #:key (log (current-error-port)) recursive?
                            (verify-certificate? #t))
  "Download from URL to STORE, either under NAME or URL's basename if
omitted.  Write progress reports to LOG.  RECURSIVE? has the same effect as
the same-named parameter of 'add-to-store'.  VERIFY-CERTIFICATE? determines
whether or not to validate HTTPS server certificates."
  (define uri
    (string->uri url))

  (if (or (not uri) (memq (uri-scheme uri) '(file #f)))
      (add-to-store store name recursive? "sha256"
                    (if uri (uri-path uri) url))
      (call-with-temporary-output-file
       (lambda (temp port)
         (let ((result
                (parameterize ((current-output-port log))
                  (build:url-fetch url temp
                                   #:mirrors %mirrors
                                   #:verify-certificate?
                                   verify-certificate?))))
           (close port)
           (and result
                (add-to-store store name recursive? "sha256" temp)))))))

;;; download.scm ends here
