;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 ng0 <ng0@n0.is>
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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages wxwidgets))

(define-public erlang
  (package
    (name "erlang")
    (version "21.0.5")
    (source (origin
              (method git-fetch)
              ;; The tarball from http://erlang.org/download contains many
              ;; pre-compiled files, so we use this snapshot of the source
              ;; repository.
              (uri (git-reference
                    (url "https://github.com/erlang/otp.git")
                    (commit (string-append "OTP-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gv83i5ybj1z3ykbbldjzf7dbfjszp84c0yzrpshj611b9wp0176"))
              (patches (search-patches "erlang-man-path.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("autoconf" ,autoconf)
       ("automake" ,automake)

       ;; Erlang's documentation is distributed in a separate tarball.
       ("erlang-manpages"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://erlang.org/download/otp_doc_man_"
                               (version-major+minor version) ".tar.gz"))
           (sha256
            (base32
             "0cgv23q0215f6lvmhj4w9skx5m29khjs9mb890635s3yp520xgqh"))))))
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
         (add-after 'patch-source-env 'autoconf
           (lambda _
             (invoke "./otp_build" "autoconf")
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
               (mkdir-p (string-append share "/misc/erlang"))
               (with-directory-excursion share
                 (invoke "tar" "xvf" manpages)
                 (rename-file "COPYRIGHT"
                              (string-append share "/misc/erlang/COPYRIGHT"))
                 ;; Delete superfluous file.
                 (delete-file "PR.template"))
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
         (add-before 'set-emacs-load-path 'change-working-directory
           (lambda _ (chdir "lib/tools/emacs") #t)))))
    (home-page "https://www.erlang.org/")
    (synopsis "Erlang major mode for Emacs")
    (description
     "This package provides an Emacs major mode for editing Erlang source
files.")
    (license license:asl2.0)))
