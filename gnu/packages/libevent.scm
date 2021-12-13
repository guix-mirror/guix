;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2017, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
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
  #:use-module (guix git-download)
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
    (version "2.1.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/libevent/libevent/releases/download/release-"
                    version "-stable/libevent-" version "-stable.tar.gz"))
              (sha256
               (base32
                "1fq30imk8zd26x8066di3kpc5zyfc5z6frr3zll685zcx4dxxrlj"))))
    (build-system gnu-build-system)
    (outputs '("out" "bin"))
    (arguments
     ;; This skips some of the tests which fail on armhf and aarch64.
     '(#:configure-flags '("--disable-libevent-regress"
                           "--disable-openssl")))
    (inputs
     `(("python" ,python-wrapper)))     ;for 'event_rpcgen.py'
    (native-inputs
     (list which))
    (home-page "https://libevent.org/")
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

(define-public libev
  (package
    (name "libev")
    (version "4.33")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dist.schmorp.de/libev/Attic/libev-"
                                  version
                                  ".tar.gz"))
              (sha256
               (base32
                "1sjs4324is7fp21an4aas2z4dwsvs6z4xwrmp72vwpq1s6wbfzjh"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
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
    (version "1.41.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dist.libuv.org/dist/v" version
                                  "/libuv-v" version ".tar.gz"))
              (sha256
               (base32
                "0zb818sjwnxn5yv3qvkxaprjf037yqmjipk5i3a8rg1q4izhrnv5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")
       ;; XXX: Some tests want /dev/tty, attempt to make connections, etc.
       #:tests? #f))
    (native-inputs (list autoconf automake libtool
                         ;; libuv.pc is installed only when pkg-config is found.
                         pkg-config))
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

(define-public libuv-for-node
  ;; When upgrading Node, also upgrade this. Get the version from
  ;; https://github.com/nodejs/node/blob/master/deps/uv/include/uv/version.h
  (package
    (inherit libuv)
    (name "libuv")
    (version "1.42.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dist.libuv.org/dist/v" version
                                  "/libuv-v" version ".tar.gz"))
              (sha256
               (base32
                "0wpb9pz3r8nksnrf4zbixj2kk9whr7abi45ydrwyv2js2ljrc4j3"))))
    (properties '((hidden? . #t)))))

(define-public libuv-julia
  (let ((commit "fb3e3364c33ae48c827f6b103e05c3f0e78b79a9")
        (revision "3"))
    ;; When upgrading Julia, also upgrade this.  Get the commit from
    ;; https://github.com/JuliaLang/julia/blob/v1.6.1/deps/libuv.version
    (package
      (inherit libuv)
      (name "libuv-julia")
      (version (git-version "2.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/JuliaLang/libuv")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1kqpn19d20aka30h6q5h8lnzyp0vw0xzgx0wm4w2r5j6yf76m2hr"))))
      (home-page "https://github.com/JuliaLang/libuv")
      (properties '((hidden? . #t))))))

(define-public perl-anyevent
  (package
    (name "perl-anyevent")
    (version "7.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/"
                                  "AnyEvent-" version ".tar.gz"))
              (sha256
               (base32
                "11drlj8r02czhjgzkb39axnr8zzyp506r043xfmf93q9kilfmgjh"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-canary-stability))
    (propagated-inputs
     (list perl-async-interrupt
           perl-ev
           perl-guard
           perl-json
           perl-json-xs
           perl-net-ssleay
           perl-task-weaken))
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
    (version "4.31")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/EV-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1jxlhnvrqim39977zwavjrcbdf9bifb46pwaxvm0s8klq121kjwb"))
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
     (list perl-common-sense))
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
     (list perl-module-build perl-test-simple))
    (propagated-inputs
     (list perl-anyevent perl-data-sexpression))
    (arguments
     ;; Tests seem to fail because they try to start a server.
     `(#:tests? #f))
    (home-page "https://metacpan.org/release/RPC-EPC-Service")
    (synopsis "Asynchronous remote procedure stack")
    (description "RPC::EPC::Service enables to connect the other process with
the S-expression protocol, like the Swank protocol of the SLIME.")
    (license perl-license)))
