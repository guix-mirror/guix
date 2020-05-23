;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Nikita <nikita@n0.is>
;;; Copyright © 2020, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Oskar Köök <oskar@maatriks.ee>
;;; Copyright © 2021 Cees de Groot <cg@evrl.com>
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

(define-module (gnu packages erlang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system rebar3)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hexpm-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages wxwidgets))

(define-public erlang
  (package
    (name "erlang")
    (version "24.0.5")
    (source (origin
              (method git-fetch)
              ;; The tarball from http://erlang.org/download contains many
              ;; pre-compiled files, so we use this snapshot of the source
              ;; repository.
              (uri (git-reference
                    (url "https://github.com/erlang/otp")
                    (commit (string-append "OTP-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f8zr2jxr0v4zcd98zqx99zxdn768vjpzwxsbsd6ss3if405sq2a"))
              (patches (search-patches "erlang-man-path.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)

       ;; Erlang's documentation is distributed in a separate tarball.
       ("erlang-manpages"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://erlang.org/download/otp_doc_man_"
                               (version-major+minor version) ".tar.gz"))
           (sha256
            (base32
             "1c9ccp93pmm54mmvpiyrmj8v00pq11a60c4xv220k97i965zkwsg"))))))
    (inputs
     `(("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("wxwidgets" ,wxwidgets)))
    (propagated-inputs
     `(("fontconfig" ,fontconfig)
       ("glu" ,glu)
       ("mesa" ,mesa)))
    (arguments
     `(#:test-target "release_tests"
       #:configure-flags
       (list "--disable-saved-compile-time"
             "--enable-dynamic-ssl-lib"
             "--enable-native-libs"
             "--enable-shared-zlib"
             "--enable-smp-support"
             "--enable-threads"
             "--enable-wx"
             (string-append "--with-ssl=" (assoc-ref %build-inputs "openssl")))
       #:modules ((srfi srfi-19) ; make-time, et cetera.
                  (guix build utils)
                  (guix build gnu-build-system))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         ;; The are several code fragments that embed timestamps into the
         ;; output. Here, we alter those fragments to use the value of
         ;; SOURCE_DATE_EPOCH instead.
         (add-after 'unpack 'remove-timestamps
           (lambda _
             (let ((source-date-epoch
                    (time-utc->date
                     (make-time time-utc 0 (string->number
                                            (getenv "SOURCE_DATE_EPOCH"))))))
               (substitute* "lib/reltool/src/reltool_target.erl"
                 (("Date = date\\(\\),")
                  (string-append "Date = "
                                 (date->string source-date-epoch
                                               "'{~Y,~m,~d}',"))))
               (substitute* "lib/reltool/src/reltool_target.erl"
                 (("Time = time\\(\\),")
                  (string-append "Time = "
                                 (date->string source-date-epoch
                                               "'{~H,~M,~S}',"))))
               (substitute* '("lib/reltool/src/reltool_target.erl"
                              "lib/sasl/src/systools_make.erl")
                 (("date\\(\\), time\\(\\),")
                  (date->string source-date-epoch
                                "{~Y,~m,~d}, {~H,~M,~S},")))
               (substitute* "lib/dialyzer/test/small_SUITE_data/src/gs_make.erl"
                 (("tuple_to_list\\(date\\(\\)\\),tuple_to_list\\(time\\(\\)\\)")
                  (date->string
                   source-date-epoch
                   "tuple_to_list({~Y,~m,~d}), tuple_to_list({~H,~M,~S})")))
               (substitute* "lib/snmp/src/compile/snmpc_mib_to_hrl.erl"
                 (("\\{Y,Mo,D\\} = date\\(\\),")
                  (date->string source-date-epoch
                                "{Y,Mo,D} = {~Y,~m,~d},")))
               (substitute* "lib/snmp/src/compile/snmpc_mib_to_hrl.erl"
                 (("\\{H,Mi,S\\} = time\\(\\),")
                  (date->string source-date-epoch
                                "{H,Mi,S} = {~H,~M,~S},")))
               #t)))
         (add-after 'unpack 'patch-/bin/sh
           (lambda _
             (substitute* "erts/etc/unix/run_erl.c"
               (("sh = \"/bin/sh\";")
                (string-append "sh = \""
                               (which "sh")
                               "\";")))

             (substitute* "erts/emulator/sys/unix/sys_drivers.c"
               (("SHELL \"/bin/sh\"")
                (string-append "SHELL \""
                               (which "sh")
                               "\"")))
             (substitute* "erts/emulator/sys/unix/erl_child_setup.c"
               (("SHELL \"/bin/sh\"")
                (string-append "SHELL \""
                               (which "sh")
                               "\"")))

             (substitute* "lib/kernel/src/os.erl"
               (("/bin/sh") (which "sh")))

             #t))
         (add-after 'patch-source-shebangs 'patch-source-env
           (lambda _
             (let ((escripts
                    (append
                     (find-files "." "\\.escript")
                     (find-files "lib/stdlib/test/escript_SUITE_data/")
                     '("erts/lib_src/utils/make_atomics_api"
                       "erts/preloaded/src/add_abstract_code"
                       "lib/diameter/bin/diameterc"
                       "lib/reltool/examples/display_args"
                       "lib/reltool/examples/mnesia_core_dump_viewer"
                       "lib/snmp/src/compile/snmpc.src"
                       "make/verify_runtime_dependencies"
                       "make/emd2exml.in"))))
               (substitute* escripts
                 (("/usr/bin/env") (which "env")))
               #t)))
         (add-before 'configure 'set-erl-top
           (lambda _
             (setenv "ERL_TOP" (getcwd))
             #t))
         (add-after 'install 'patch-erl
           ;; This only works after install.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (substitute* (string-append out "/bin/erl")
                 (("sed") (which "sed")))
               #t)))
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (manpages (assoc-ref inputs "erlang-manpages"))
                    (share (string-append out "/share/")))
               (mkdir-p share)
               (with-directory-excursion share
                 (invoke "tar" "xvf" manpages))
               #t))))))
    (home-page "https://www.erlang.org/")
    (synopsis "The Erlang programming language")
    (description
     "Erlang is a programming language used to build massively
scalable soft real-time systems with requirements on high
availability.  Some of its uses are in telecoms, banking, e-commerce,
computer telephony and instant messaging.  Erlang's runtime system has
built-in support for concurrency, distribution and fault tolerance.")
    ;; Erlang is distributed under the Apache License 2.0, but some components
    ;; have other licenses. See 'system/COPYRIGHT' in the source distribution.
    (license (list license:asl2.0 license:bsd-2 license:bsd-3 license:expat
                   license:lgpl2.0+ license:tcl/tk license:zlib))))

(define-public emacs-erlang
  (package
    (name "emacs-erlang")
    (version (package-version erlang))
    (source (package-source erlang))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'expand-load-path 'change-working-directory
           (lambda _ (chdir "lib/tools/emacs") #t)))))
    (home-page "https://www.erlang.org/")
    (synopsis "Erlang major mode for Emacs")
    (description
     "This package provides an Emacs major mode for editing Erlang source
files.")
    (license license:asl2.0)))

(define-public erlang-bbmustache
  (package
    (name "erlang-bbmustache")
    (version "1.12.1")
    (source
     (origin
       (method hexpm-fetch)
       (uri (hexpm-uri "bbmustache" version))
       (sha256
        (base32 "0wbfayx6940zf57bpwg1m9sk3cpgam2q8n0w74alkrc4gc7hn47w"))))
    (build-system rebar3-build-system)
    (inputs
     `(("erlang-edown" ,erlang-edown)
       ("erlang-getopt" ,erlang-getopt)
       ("erlang-rebar3-git-vsn" ,erlang-rebar3-git-vsn)))
    (arguments
     `(#:tests? #f ;; requires mustache specification file
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-escript
           (lambda _
             (invoke "rebar3" "as" "dev" "escriptize")))
         (add-after 'install 'install-escript
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (install-file "_build/dev/bin/bbmustache"
                             (string-append out "/bin")))
             #t)))))
    (home-page "https://github.com/soranoba/bbmustache/")
    (synopsis "Binary pattern match Based Mustache template engine for Erlang")
    (description "This Erlang library provides a Binary pattern match Based
Mustache template engine")
    (license license:expat)))

(define-public erlang-certifi
  (package
    (name "erlang-certifi")
    (version "2.7.0")
    (source
     (origin
       (method hexpm-fetch)
       (uri (hexpm-uri "certifi" version))
       (sha256
        (base32 "1ssiajvll5nilrnsg23ja3qz2fmvnbhy176c8i0gqj0h1alismn9"))))
    (build-system rebar3-build-system)
    (inputs
     `(("parse-trans" ,erlang-parse-trans)))
    (home-page "https://github.com/certifi/erlang-certifi/")
    (synopsis "CA bundle adapted from Mozilla for Erlang")
    (description "This Erlang library contains a CA bundle that you can
reference in your Erlang application.  This is useful for systems that do not
have CA bundles that Erlang can find itself, or where a uniform set of CAs is
valuable.

This an Erlang specific port of certifi.  The CA bundle is derived from
Mozilla's canonical set.")
    (license license:bsd-3)))

(define-public erlang-cf
  (package
    (name "erlang-cf")
    (version "0.3.1")
    (source
     (origin
       (method hexpm-fetch)
       (uri (hexpm-uri "cf" version))
       (sha256
        (base32 "0vnmbb1n899xw2p4x6c3clpzxcqqdsfbfhh1dfy530i3201vr2h4"))))
    (build-system rebar3-build-system)
    (home-page "https://github.com/project-fifo/cf")
    (synopsis "Terminal colour helper for Erlang io and io_lib")
    (description "This package provides a helper library for termial colour
printing extending the io:format syntax to add colours.")
    (license license:expat)))

(define-public erlang-cth-readable
  (package
    (name "erlang-cth-readable")
    (version "1.5.1")
    (source
     (origin
       (method hexpm-fetch)
       (uri (hexpm-uri "cth_readable" version))
       (sha256
        (base32 "0hqzgd8fvs4d1bhpm6dkm3bm2jik4qbl78s514r5ivwjxw1dzrds"))))
    (build-system rebar3-build-system)
    (propagated-inputs
     `(("erlang-cf" ,erlang-cf)))
    (arguments
     `(#:tests? #f)) ;; no test-suite
    (home-page "https://github.com/ferd/cth_readable")
    (synopsis "Common Test hooks for more readable logs for Erlang")
    (description "This package provides an OTP library to be used for CT log
outputs you want to be readable around all that noise they contain.")
    (license license:bsd-3)))

(define-public erlang-erlware-commons
  (package
    (name "erlang-erlware-commons")
    (version "1.6.0")
    (source
     (origin
       (method hexpm-fetch)
       (uri (hexpm-uri "erlware_commons" version))
       (sha256
        (base32 "0xc3kiylingqrrnzhxm2j2n5gr3hxqgpibhi9nva9bwjs4n155fm"))))
    (build-system rebar3-build-system)
    (propagated-inputs
     `(("erlang-cf" ,erlang-cf)))
    (native-inputs
     `(("git" ,git)))  ;; Required for tests
    (arguments
     `(#:tests? #f)) ;; TODO: 1/219 tests fail -  very simple one, though
    (home-page "http://erlware.github.io/erlware_commons/")
    (synopsis "Additional standard library for Erlang")
    (description "Erlware Commons is an Erlware project focused on all aspects
of reusable Erlang components.")
    (license license:expat)))

(define-public erlang-eunit-formatters
  (package
    (name "erlang-eunit-formatters")
    (version "0.5.0")
    (source
     (origin
       (method hexpm-fetch)
       (uri (hexpm-uri "eunit_formatters" version))
       (sha256
        (base32 "18q3vb12799584kdb998298b6bfh686mzi5s7pkb7djrf93vgf5f"))))
    (build-system rebar3-build-system)
    (home-page "https://github.com/seancribbs/eunit_formatters")
    (synopsis "Better output for eunit suites")
    (description "This package provides a better output for Erlang eunits.")
    (license license:asl2.0)))

(define-public erlang-getopt
  (package
    (name "erlang-getopt")
    (version "1.0.2")
    (source
     (origin
       (method hexpm-fetch)
       (uri (hexpm-uri "getopt" version))
       (sha256
        (base32 "1yxs36l1ll56zrxn81kw5qd8fv1q14myhjylk7dk31palg7jl725"))))
    (build-system rebar3-build-system)
    (home-page "https://github.com/jcomellas/getopt")
    (synopsis "Command-line options parser for Erlang")
    (description "This package provides an Erlang module to parse command line
arguments using the GNU getopt syntax.")
    (license license:bsd-3)))

(define-public erlang-hex-core
  (package
    (name "erlang-hex-core")
    (version "0.8.2")
    (source
     (origin
       (method hexpm-fetch)
       (uri (hexpm-uri "hex_core" version))
       (sha256
        (base32 "15fybnqxl5lzkpd8fjj1fxmj8cxcdpkxn0cvwc41cv0vxv3pw797"))))
    (build-system rebar3-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rebar3" "as" "test" "proper")))))))
    (inputs
     `(("erlang-proper" ,erlang-proper)
       ("erlang-rebar3-proper" ,erlang-rebar3-proper)))
    (propagated-inputs
     `(("erlang-getopt" ,erlang-getopt)))
    (home-page "https://github.com/hexpm/hex_core")
    (synopsis "Reference implementation of Hex specifications")
    (description "This package provides the reference implementation of Hex
specifications.")
    (license license:asl2.0)))

(define-public erlang-parse-trans
  (package
    (name "erlang-parse-trans")
    (version "3.4.1")
    (source
     (origin
       (method hexpm-fetch)
       (uri (hexpm-uri "parse_trans" version))
       (sha256
        (base32 "1g3ablipihi8z64j9195pmrlf7gymyi21j2da9y509igs3q1sxfc"))))
    (build-system rebar3-build-system)
    (inputs
     `(("erlang-getopt" ,erlang-getopt)))
    (home-page "https://github.com/uwiger/parse_trans")
    (synopsis "Parse transform utilities for Erlang")
    (description "This package provides parse transform utilities for
Erlang.")
    (license license:asl2.0)))

(define-public erlang-providers
  (package
    (name "erlang-providers")
    (version "1.9.0")
    (source
     (origin
       (method hexpm-fetch)
       (uri (hexpm-uri "providers" version))
       (sha256
        (base32 "0rq5zrqrsv2zgg84yfgh1faahnl4hkn92lja43iqihyiy181813z"))))
    (propagated-inputs
     `(("erlang-cf" ,erlang-cf)
       ("erlang-erlware-commons" ,erlang-erlware-commons)
       ("erlang-getopt" ,erlang-getopt)))
    (build-system rebar3-build-system)
    (home-page "https://github.com/tsloughter/providers")
    (synopsis "Erlang providers library")
    (description "This package provides an Erlang providers library.")
    (license license:asl2.0)))

(define-public erlang-relx
  (package
    (name "erlang-relx")
    (version "4.5.0")
    (source
     (origin
       (method hexpm-fetch)
       (uri (hexpm-uri "relx" version))
       (sha256
        (base32 "12fjcb5b992ixxkc7v7v55ln1i5qak7dzmzqvf6hx50l1ip3hh58"))))
    (build-system rebar3-build-system)
    (propagated-inputs
     `(("erlang-bbmustache" ,erlang-bbmustache)
       ("erlang-cf" ,erlang-cf)
       ("erlang-erlware-commons" ,erlang-erlware-commons)
       ("erlang-getopt" ,erlang-getopt)
       ("erlang-providers" ,erlang-providers)))
    (home-page "https://erlware.github.io/relx/")
    (synopsis "Release assembler for Erlang/OTP Releases")
    (description "Relx assembles releases for an Erlang/OTP release.  Given a
release specification and a list of directories in which to search for OTP
applications it will generate a release output.  That output depends heavily on
what plugins available and what options are defined, but usually it is simply
a well configured release directory.")
    (license license:asl2.0)))

(define-public erlang-ssl-verify-fun
  (package
    (name "erlang-ssl-verify-fun")
    (version "1.1.6")
    (source
     (origin
       (method hexpm-fetch)
       (uri (hexpm-uri "ssl_verify_fun" version))
       (sha256
        (base32 "0bwdqhnmlv0jfs5mrws2a75zngiihnvcni2hj4l65r5abnw050vx"))))
    (build-system rebar3-build-system)
    (home-page "https://github.com/deadtrickster/ssl_verify_fun.erl")
    (synopsis "SSL verification functions for Erlang")
    (description "This package provides SSL verification functions for
Erlang.")
    (license license:expat)))
