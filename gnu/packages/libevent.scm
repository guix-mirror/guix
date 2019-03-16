;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (gnu packages libevent)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls))

(define-public libevent
  (package
    (name "libevent")
    (version "2.1.8")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/libevent/libevent/releases/download/release-"
                   version "-stable/libevent-" version "-stable.tar.gz"))
             (sha256
              (base32
               "1hhxnxlr0fsdv7bdmzsnhdz16fxf3jg2r6vyljcl3kj6pflcap4n"))
             (patches (search-patches "libevent-2.1-dns-tests.patch"
                                      ;; XXX: Try removing this for > 2.1.8.
                                      ;; https://github.com/libevent/libevent/issues/452
                                      "libevent-2.1-skip-failing-test.patch"))))
    (build-system gnu-build-system)
    (arguments
      ;; This skips some of the tests which fail on armhf and aarch64.
     '(#:configure-flags '("--disable-libevent-regress")))
    (inputs
     `(("python" ,python-2)))           ; for 'event_rpcgen.py'
    (native-inputs
     `(("which" ,which)))
    (home-page "http://libevent.org/")
    (synopsis "Event notification library")
    (description
     "The libevent API provides a mechanism to execute a callback
function when a specific event occurs on a file descriptor or after a
timeout has been reached.  Furthermore, libevent also support callbacks
due to signals or regular timeouts.

libevent is meant to replace the event loop found in event driven
network servers.  An application just needs to call event_dispatch() and
then add or remove events dynamically without having to change the event
loop.")
    (license bsd-3)))

(define-public libevent-2.0
  (package
    (inherit libevent)
    (version "2.0.22")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/libevent/libevent/releases/download/release-"
                    version "-stable/libevent-" version "-stable.tar.gz"))
              (sha256
               (base32
                "18qz9qfwrkakmazdlwxvjmw8p76g70n3faikwvdwznns1agw9hki"))
              (patches
               (search-patches
                "libevent-dns-tests.patch"
                "libevent-2.0-CVE-2016-10195.patch"
                "libevent-2.0-CVE-2016-10196.patch"
                "libevent-2.0-CVE-2016-10197.patch"
                "libevent-2.0-evbuffer-add-use-last-with-datap.patch"))))
    (arguments '())))

(define-public libev
  (package
    (name "libev")
    (version "4.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dist.schmorp.de/libev/Attic/libev-"
                                  version
                                  ".tar.gz"))
              (sha256
               (base32
                "08gqsza1czx0nf62nkk183jb0946yzjsymaacxbzdgcs8z9r6dcp"))))
    (build-system gnu-build-system)
    (home-page "http://software.schmorp.de/pkg/libev.html")
    (synopsis "Event loop loosely modelled after libevent")
    (description
     "libev provides a full-featured and high-performance event loop that is
loosely modelled after libevent.  It includes relative timers, absolute timers
with customized rescheduling, synchronous signals, process status change
events, event watchers dealing with the event loop itself, file watchers, and
limited support for fork events.")
    (license
     (list bsd-2 gpl2+))))

(define-public libuv
  (package
    (name "libuv")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dist.libuv.org/dist/v" version
                                  "/libuv-v" version ".tar.gz"))
              (sha256
               (base32
                "01pg0zsfr8mxlpipkbpw0dpsl26x5s966f5br7dx9ac29abk419q"))))
    (build-system gnu-build-system)
    (arguments
     '(;; XXX: Some tests want /dev/tty, attempt to make connections, etc.
       #:tests? #f))
    (native-inputs `(("autoconf" ,autoconf-wrapper)
                     ("automake" ,automake)
                     ("libtool" ,libtool)

                     ;; libuv.pc is installed only when pkg-config is found.
                     ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/libuv/libuv")
    (synopsis "Library for asynchronous I/O")
    (description
     "libuv is a multi-platform support library with a focus on asynchronous
I/O.  Among other things, it supports event loops via epoll, kqueue, and
similar IOCP, and event ports, asynchronous TCP/UDP sockets, asynchronous DNS
resolution, asynchronous file system operations, and threading primitives.")

    ;; A few files fall under other non-copyleft licenses; see 'LICENSE' for
    ;; details.  Documentation is CC-BY 4.0 as of 1.12.0; see 'LICENSE-docs'.
    (license (list expat cc-by4.0))))

;; This version is required for Node versions < 10.
(define-public libuv-1.19
  (package
    (inherit libuv)
    (version "1.19.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dist.libuv.org/dist/v" version
                                  "/libuv-v" version ".tar.gz"))
              (sha256
               (base32
                "1msk9ac1z69whww88ibrwjqkd1apdla6l77cm2fwy5kigq0z5g3w"))))))

(define-public perl-anyevent
  (package
    (name "perl-anyevent")
    (version "7.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/"
                                  "AnyEvent-" version ".tar.gz"))
              (sha256
               (base32
                "0m73r67ah9xmcwzxs50jxf8ncd8h71mi4wf2mvnqkxvibhrv478i"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-canary-stability" ,perl-canary-stability)))
    (propagated-inputs
     `(("perl-async-interrupt" ,perl-async-interrupt)
       ("perl-ev" ,perl-ev)
       ("perl-guard" ,perl-guard)
       ("perl-json" ,perl-json)
       ("perl-json-xs" ,perl-json-xs)
       ("perl-net-ssleay" ,perl-net-ssleay)
       ("perl-task-weaken" ,perl-task-weaken)))
    (home-page "https://metacpan.org/release/AnyEvent")
    (synopsis
     "API for I/O, timer, signal, child process and completion events")
    (description
     "This module allows using a variety of events without forcing module
authors to pick a specific event loop, and without noticeable overhead.
Currently supported event loops are EV, Event, Glib/Gtk2, Tk, Qt,
@code{Event::Lib}, Irssi, @code{IO::Async} and POE (and thus also WxWidgets
and Prima).  It also comes with a very fast Pure Perl event loop that does
not rely on XS.")
    (license perl-license)))

(define-public perl-ev
  (package
    (name "perl-ev")
    (version "4.25")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/EV-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0slw68zxrkfribf6lhggdhpay3mdng0nqxlglkwrk19myblchr9f"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Drop bundled libev.
                  (delete-file-recursively "libev")
                  #t))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-libev
           ;; This package requires the libev *sources* in order
           ;; to build. Unpack system libev here...
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir "./libev")
             (invoke "tar" "-xf" (assoc-ref inputs "libev-source")
                     "-C" "./libev" "--strip-components=1"))))))
    (native-inputs
     `(("libev-source" ,(package-source libev))
       ("perl-canary-stability" ,perl-canary-stability)))
    (propagated-inputs
     `(("perl-common-sense" ,perl-common-sense)))
    (home-page "https://metacpan.org/release/EV")
    (synopsis "Perl interface to libev")
    (description
     "This module provides an interface to @code{libev}, a high performance
full-featured event loop.  It can be used through the @code{AnyEvent} module
and still be faster than other event loops currently supported in Perl.")
    (license perl-license)))

(define-public perl-rpc-epc-service
  (package
    (name "perl-rpc-epc-service")
    (version "0.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/K/KI/KIWANAMI/RPC-EPC-Service-"
             "v" version ".tar.gz"))
       (sha256
        (base32
         "1qwb284z4ig3xzy21m1b3w8bkb8k6l2ij6cjz93znn2j6qs42pwp"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-simple" ,perl-test-simple)))
    (propagated-inputs
     `(("perl-anyevent" ,perl-anyevent)
       ("perl-data-sexpression" ,perl-data-sexpression)))
    (arguments
     ;; Tests seem to fail because they try to start a server.
     `(#:tests? #f))
    (home-page "https://metacpan.org/release/RPC-EPC-Service")
    (synopsis "Asynchronous remote procedure stack")
    (description "RPC::EPC::Service enables to connect the other process with
the S-expression protocol, like the Swank protocol of the SLIME.")
    (license perl-license)))
