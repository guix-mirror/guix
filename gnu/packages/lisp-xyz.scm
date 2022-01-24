;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018, 2020, 2021 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019, 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2019 Jesse Gildersleve <jessejohngildersleve@protonmail.com>
;;; Copyright © 2019, 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2020 Dimakis Dimakakos <me@bendersteed.tech>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020, 2021 Adam Kandur <rndd@tuta.io>
;;; Copyright © 2020, 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Aurora <rind38@disroot.org>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 André A. Gomes <andremegafone@gmail.com>
;;; Copyright © 2021 Cage <cage-dev@twistfold.it>
;;; Copyright © 2021 Cameron Chaparro <cameron@cameronchaparro.com>
;;; Copyright © 2021 Charles Jackson <charles.b.jackson@protonmail.com>
;;; Copyright © 2021, 2022 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2021 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2021 Jacob MacDonald <jaccarmac@gmail.com>
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

;;; This file only contains Common Lisp libraries.
;;; Common Lisp compilers and tooling go to lisp.scm.
;;; Common Lisp applications should go to the most appropriate file,
;;; e.g. StumpWM is in wm.scm.

(define-module (gnu packages lisp-xyz)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19))

(define-public sbcl-alexandria
  (package
   (name "sbcl-alexandria")
   (version "1.4")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://gitlab.common-lisp.net/alexandria/alexandria.git")
           (commit (string-append "v" version))))
     (sha256
      (base32
       "0r1adhvf98h0104vq14q7y99h0hsa8wqwqw92h7ghrjxmsvz2z6l"))
     (file-name (git-file-name name version))))
   (build-system asdf-build-system/sbcl)
   (native-inputs
    (list sbcl-rt))
   (synopsis "Collection of portable utilities for Common Lisp")
   (description
    "Alexandria is a collection of portable utilities.  It does not contain
conceptual extensions to Common Lisp.  It is conservative in scope, and
portable between implementations.")
   (home-page "https://common-lisp.net/project/alexandria/")
   (license license:public-domain)))

(define-public cl-alexandria
  (sbcl-package->cl-source-package sbcl-alexandria))

(define-public ecl-alexandria
  (sbcl-package->ecl-package sbcl-alexandria))

(define-public sbcl-bodge-utilities
  (let ((commit "6304bac4abe06d53579e2c0fc4437d14ff077d9f")
        (revision "1"))
    (package
     (name "sbcl-bodge-utilities")
     (version (git-version "1.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/borodust/bodge-utilities")
             (commit commit)))
       (file-name (git-file-name "bodge-utilities" version))
       (sha256
        (base32 "1z1blj05q71vzh323qwyn9p3xs7v0mq2yhwfyzza5libp37wqm3c"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      (list sbcl-alexandria
            sbcl-cffi
            sbcl-claw
            sbcl-dissect
            sbcl-local-time
            sbcl-log4cl
            sbcl-split-sequence
            sbcl-static-vectors
            sbcl-trivial-gray-streams))
     (home-page "https://github.com/borodust/bodge-utilities")
     (synopsis "Common Lisp utilities library for CL-BODGE")
     (description
      "This Common Lisp library provides utilities for the @emph{Bodge} library
collection.")
     (license license:expat))))

(define-public ecl-bodge-utilities
  (sbcl-package->ecl-package sbcl-bodge-utilities))

(define-public cl-bodge-utilities
  (sbcl-package->cl-source-package sbcl-bodge-utilities))

(define-public sbcl-bodge-queue
  (let ((commit "948c9a501dcd412689952d09eb7453ec2722336a")
        (revision "0"))
    (package
      (name "sbcl-bodge-queue")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/borodust/bodge-queue")
               (commit commit)))
         (file-name (git-file-name "bodge-queue" version))
         (sha256
          (base32 "148hjikqk8v2m30mj15xh89zni6szf9z3prav580qk9dqr8djjdr"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/borodust/bodge-queue")
      (synopsis "Simple queue for Common Lisp")
      (description "This Common Lisp library provides a simple FIFO
implementation with no external dependencies.")
      (license license:expat))))

(define-public cl-bodge-queue
  (sbcl-package->cl-source-package sbcl-bodge-queue))

(define-public ecl-bodge-queue
  (sbcl-package->ecl-package sbcl-bodge-queue))

(define-public sbcl-golden-utils
  (let ((commit "fe1898f9abbd302b0359f017637c063173cf73e1")
        (revision "3"))
    (package
      (name "sbcl-golden-utils")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/mfiano/mfiano-utils")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name "golden-utils" version))
         (sha256
          (base32 "1ljc8yj32lmd1d60446rzl9m0r1ar15gdzacsf6blw1kny8xlrsr"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria))
      (synopsis "Common Lisp utility library")
      (description
       "This is a Common Lisp library providing various utilities.")
      (license license:expat))))

(define-public ecl-golden-utils
  (sbcl-package->ecl-package sbcl-golden-utils))

(define-public cl-golden-utils
  (sbcl-package->cl-source-package sbcl-golden-utils))

(define-public sbcl-asdf-finalizers
  (let ((commit "7f537f6c598b662ae987c6acc268dd27c25977e0")
        (revision "1"))
    (package
      (name "sbcl-asdf-finalizers")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/asdf/asdf-finalizers")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1w56c9yjjydjshsgqxz57qlp2v3r4ilbisnsgiqphvxnhvd41y0v"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("fare-utils" ,sbcl-fare-utils)
         ("hu.dwim.stefil" ,sbcl-hu.dwim.stefil)))
      (arguments
       `(#:asd-files '("asdf-finalizers.asd"
                       "list-of.asd"
                       "asdf-finalizers-test.asd")
         #:asd-systems '("asdf-finalizers"
                         "list-of")))
      (home-page "https://gitlab.common-lisp.net/asdf/asdf-finalizers")
      (synopsis "Enforced calling of finalizers for Lisp code")
      (description "This library allows you to implement and enforce proper
finalization of compile-time constructs while building Lisp source files.

It produces two systems: asdf-finalizers and list-of.")
      (license license:expat))))

(define-public ecl-asdf-finalizers
  (sbcl-package->ecl-package sbcl-asdf-finalizers))

(define-public cl-asdf-finalizers
  (sbcl-package->cl-source-package sbcl-asdf-finalizers))

(define-public sbcl-net.didierverna.asdf-flv
  (package
    (name "sbcl-net.didierverna.asdf-flv")
    (version "2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/didierverna/asdf-flv")
             (commit (string-append "version-" version))))
       (file-name (git-file-name "asdf-flv" version))
       (sha256
        (base32 "1fi2y4baxan103jbg4idjddzihy03kwnj2mzbwrknw4d4x7xlgwj"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Common Lisp ASDF extension to provide support for file-local variables")
    (description "ASDF-FLV provides support for file-local variables through
ASDF.  A file-local variable behaves like @code{*PACKAGE*} and
@code{*READTABLE*} with respect to @code{LOAD} and @code{COMPILE-FILE}: a new
dynamic binding is created before processing the file, so that any
modification to the variable becomes essentially file-local.

In order to make one or several variables file-local, use the macros
@code{SET-FILE-LOCAL-VARIABLE(S)}.")
    (home-page "https://www.lrde.epita.fr/~didier/software/lisp/misc.php#asdf-flv")
    (license (license:non-copyleft
              "https://www.gnu.org/prep/maintain/html_node/License-Notices-for-Other-Files.html"
              "GNU All-Permissive License"))))

(define-public cl-net.didierverna.asdf-flv
  (sbcl-package->cl-source-package sbcl-net.didierverna.asdf-flv))

(define-public ecl-net.didierverna.asdf-flv
  (sbcl-package->ecl-package sbcl-net.didierverna.asdf-flv))

(define-public sbcl-command-line-arguments
  (let ((commit "fbac862fb01c0e368141204f3f639920462c23fe")
        (revision "1"))
    (package
      (name "sbcl-command-line-arguments")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fare/command-line-arguments")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "054m1ikndzqf72mb9ajaa64136cwr3bgag4yfbi1574a9vq75mjq"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/fare/command-line-arguments")
      (synopsis "Trivial command-line argument parsing library for Common Lisp")
      (description "This is a library to abstract away the parsing of
Unix-style command-line arguments.  Use it in conjunction with asdf:program-op
or cl-launch for portable processing of command-line arguments.")
      (license license:expat))))

(define-public ecl-command-line-arguments
  (sbcl-package->ecl-package sbcl-command-line-arguments))

(define-public cl-command-line-arguments
  (sbcl-package->cl-source-package sbcl-command-line-arguments))

(define-public sbcl-cl-irc
  (let ((commit "963823537c7bfcda2edd4c44d172192da6722175")
        (revision "0"))
    (package
      (name "sbcl-cl-irc")
      (version (git-version "0.9.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://salsa.debian.org/common-lisp-team/cl-irc.git")
               (commit commit)))
         (file-name (git-file-name "cl-irc" version))
         (sha256
          (base32 "1b3nqbb4pj377lxl47rfgrs82pidadnrc65l48bk553c2f59b52w"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       ;; Tests only.
       (list sbcl-rt))
      (inputs
       `(("cl+ssl" ,sbcl-cl+ssl)
         ("flexi-streams" ,sbcl-flexi-streams)
         ("split-sequence" ,sbcl-split-sequence)
         ("usocket" ,sbcl-usocket)))
      (arguments
       `(#:asd-systems '("cl-irc") ;; Some inexisting "c" system is
                                   ;; found by guix otherwise.
         #:asd-files '("cl-irc.asd")
         #:test-asd-file "test/cl-irc-test.asd"))
      (synopsis "IRC client library for Common Lisp")
      (description "@code{cl-irc} is a Common Lisp IRC client library that
features (partial) DCC, CTCP and all relevant commands from the IRC
RFCs (RFC2810, RFC2811 and RFC2812).

Features:
@itemize
@item implements all commands in the RFCs
@item extra convenience commands such as op/deop, ban, ignore, etc.
@item partial DCC SEND/CHAT support
@item event driven model with hooks makes interfacing easy
@item the user can keep multiple connections
@item all CTCP commands
@end itemize\n")
      (home-page "https://common-lisp.net/project/cl-irc/")
      (license license:bsd-2))))

(define-public cl-irc
  (sbcl-package->cl-source-package sbcl-cl-irc))

(define-public ecl-cl-irc
  (sbcl-package->ecl-package sbcl-cl-irc))

(define-public sbcl-trivial-timeout
  (let ((commit "feb869357f40f5e109570fb40abad215fb370c6c")
        (revision "1"))
    (package
      (name "sbcl-trivial-timeout")
      (version (git-version "0.1.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gwkkwg/trivial-timeout/")
               (commit commit)))
         (file-name (git-file-name "trivial-timeout" version))
         (sha256
          (base32 "1kninxwvvih9nhh7a9y8lfgi7pdr76675y1clw4ss17vz8fbim5p"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-lift))
      (arguments
       ;; NOTE: (Sharlatan-20210202T231437+0000): Due to the age of this library
       ;; tests use some deprecated functionality and keep failing.
       `(#:tests? #f))
      (home-page "https://github.com/gwkkwg/trivial-timeout/")
      (synopsis "Timeout library for Common Lisp")
      (description
       "This library provides an OS and implementation independent access to
timeouts.")
      (license license:expat))))

(define-public ecl-trivial-timeout
  (sbcl-package->ecl-package sbcl-trivial-timeout))

(define-public cl-trivial-timeout
  (sbcl-package->cl-source-package sbcl-trivial-timeout))

(define-public sbcl-bordeaux-threads
  (package
    (name "sbcl-bordeaux-threads")
    (version "0.8.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sionescu/bordeaux-threads")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "19i443fz3488v1pbbr9x24y8h8vlyhny9vj6c9jk5prm702awrp6"))
              (file-name
               (git-file-name "bordeaux-threads" version))))
    (inputs (list sbcl-alexandria))
    (native-inputs (list sbcl-fiveam))
    (build-system asdf-build-system/sbcl)
    (synopsis "Portable shared-state concurrency library for Common Lisp")
    (description "BORDEAUX-THREADS is a proposed standard for a minimal
MP/Threading interface.  It is similar to the CLIM-SYS threading and lock
support.")
    (home-page "https://common-lisp.net/project/bordeaux-threads/")
    (license license:x11)))

(define-public cl-bordeaux-threads
  (sbcl-package->cl-source-package sbcl-bordeaux-threads))

(define-public ecl-bordeaux-threads
  (sbcl-package->ecl-package sbcl-bordeaux-threads))

(define-public sbcl-trivial-gray-streams
  (let ((revision "1")
        (commit "ebd59b1afed03b9dc8544320f8f432fdf92ab010"))
    (package
      (name "sbcl-trivial-gray-streams")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/trivial-gray-streams/trivial-gray-streams")
           (commit commit)))
         (sha256
          (base32 "0b1pxlccmnagk9cbh4cy8s5k66g3x0gwib5shjwr24xvrji6lp94"))
         (file-name
          (string-append "trivial-gray-streams-" version "-checkout"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Compatibility layer for Gray streams implementations")
      (description "Gray streams is an interface proposed for inclusion with
ANSI CL by David N. Gray.  The proposal did not make it into ANSI CL, but most
popular CL implementations implement it.  This package provides an extremely
thin compatibility layer for gray streams.")
      (home-page "https://www.cliki.net/trivial-gray-streams")
      (license license:x11))))

(define-public cl-trivial-gray-streams
  (sbcl-package->cl-source-package sbcl-trivial-gray-streams))

(define-public ecl-trivial-gray-streams
  (sbcl-package->ecl-package sbcl-trivial-gray-streams))

(define-public sbcl-flexi-streams
  (package
    (name "sbcl-flexi-streams")
    (version "1.0.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edicl/flexi-streams")
             (commit (string-append "v" version))))
       (file-name (git-file-name "flexi-streams" version))
       (sha256
        (base32 "0v7lh4nrldzczd4mwylvmxfdxk7wfsli24iv1axd6mkb833llr70"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t)))))
    (inputs `(("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
    (synopsis "Implementation of virtual bivalent streams for Common Lisp")
    (description "Flexi-streams is an implementation of \"virtual\" bivalent
streams that can be layered atop real binary or bivalent streams and that can
be used to read and write character data in various single- or multi-octet
encodings which can be changed on the fly.  It also supplies in-memory binary
streams which are similar to string streams.")
    (home-page "http://weitz.de/flexi-streams/")
    (license license:bsd-3)))

(define-public cl-flexi-streams
  (sbcl-package->cl-source-package sbcl-flexi-streams))

(define-public ecl-flexi-streams
  (sbcl-package->ecl-package sbcl-flexi-streams))

(define-public sbcl-cl-abnf
  ;; There are no releases
  (let ((commit "ba1fbb104dedbdaddb1ef93d2e4da711bd96cd70")
        (revision "1"))
    (package
     (name "sbcl-cl-abnf")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/dimitri/cl-abnf")
              (commit commit)))
        (file-name (git-file-name "cl-abnf" version))
        (sha256
         (base32 "0f09nsndxa90acm71zd4qdnp40v705a4sqm04mnv9x76h6dlggmz"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      `(("cl-ppcre" ,sbcl-cl-ppcre)
        ("esrap" ,sbcl-esrap)))
     (arguments
      `(#:asd-systems '("abnf")))
     (home-page "https://github.com/dimitri/cl-abnf")
     (synopsis "ABNF parser generator for Common Lisp")
     (description "This Common Lisp library implements a parser generator for
the ABNF grammar format as described in RFC2234.  The generated parser is a
regular expression scanner provided by the cl-ppcre lib, which means that we
can't parse recursive grammar definition.  One such definition is the ABNF
definition as given by the RFC.  Fortunately, as you have this lib, you most
probably don't need to generate another parser to handle that particular ABNF
grammar.")
     (license license:expat))))

(define-public cl-abnf
  (sbcl-package->cl-source-package sbcl-cl-abnf))

(define-public ecl-cl-abnf
  (sbcl-package->ecl-package sbcl-cl-abnf))

(define-public sbcl-cl-ppcre
  (package
    (name "sbcl-cl-ppcre")
    (version "2.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edicl/cl-ppcre")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cl-ppcre" version))
       (sha256
        (base32 "0dwvr29diqzcg5n6jvbk2rnd90i05l7n828hhw99khmqd0kz7xsi"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-flexi-streams))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-ppcre-unicode
           ;; cl-ppcre and cl-ppcre-unicode are put in different packages
           ;; to work around the circular dependency between edicl/cl-ppcre
           ;; and edicl/cl-unicode.
           (lambda _
             (delete-file "cl-ppcre-unicode.asd")
             #t)))))
    (synopsis "Portable regular expression library for Common Lisp")
    (description "CL-PPCRE is a portable regular expression library for Common
Lisp, which is compatible with perl.  It is pretty fast, thread-safe, and
compatible with ANSI-compliant Common Lisp implementations.")
    (home-page "http://weitz.de/cl-ppcre/")
    (license license:bsd-2)))

(define-public cl-ppcre
  (sbcl-package->cl-source-package sbcl-cl-ppcre))

(define-public ecl-cl-ppcre
  (sbcl-package->ecl-package sbcl-cl-ppcre))

(define-public sbcl-parse
  (let ((commit "2351ee78acac065fcf10b8713d3f404e2e910786")
        (revision "1"))
    (package
     (name "sbcl-parse")
      (version (git-version "1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/massung/parse")
             (commit commit)))
       (file-name (git-file-name "parse" version))
       (sha256
        (base32 "0l18yabyh7jizm5lgvra0jxi8s1cfwghidi6ix1pyixjkdbjlmvy"))))
     (build-system asdf-build-system/sbcl)
     (home-page "https://github.com/massung/parse")
     (synopsis "Monadic parsing for Common Lisp")
     (description
      "PARSE is a simple token parsing library for Common Lisp.")
     (license license:asl2.0))))

(define-public ecl-parse
  (sbcl-package->ecl-package sbcl-parse))

(define-public cl-parse
  (sbcl-package->cl-source-package sbcl-parse))

(define-public sbcl-re
  (let ((commit "cfbc1f482970221e80d445080a188fd5c755cd2c")
        (revision "1"))
    (package
     (name "sbcl-re")
      (version (git-version "1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/massung/re")
             (commit commit)))
       (file-name (git-file-name "re" version))
       (sha256
        (base32 "1y2gq2sckspnq8118bix55p2j43dk9qn3p8a2rplp1ip2qxqbb1i"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      (list sbcl-parse))
     (home-page "https://github.com/massung/re")
     (synopsis "Lua-style Pattern Matching for Common Lisp")
     (description
      "RE is a small, portable, lightweight, and quick, regular
expression library for Common Lisp.  It is a non-recursive, backtracing VM.")
     (license license:asl2.0))))

(define-public ecl-re
  (sbcl-package->ecl-package sbcl-re))

(define-public cl-re
  (sbcl-package->cl-source-package sbcl-re))

(define-public sbcl-ubiquitous
  (let ((commit "35eb7bd9e1b3daee1705f6b41260775180cce8af")
        (revision "1"))
    (package
      (name "sbcl-ubiquitous")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/ubiquitous")
               (commit commit)))
         (file-name (git-file-name "ubiquitous" version))
         (sha256
          (base32 "1xlkaqmjcpkiv2xl2s2pvvrv976dlc846wm16s1lj62iy1315i49"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("bordeaux-threads" ,sbcl-bordeaux-threads)))
      (arguments
       '(#:asd-systems '("ubiquitous"
                         "ubiquitous-concurrent")))
      (home-page "https://shinmera.github.io/ubiquitous/")
      (synopsis "Application configuration mechanism for Common Lisp")
      (description
       "@code{UBIQUITOUS} is a very easy-to-use library for persistent
configuration storage.  It automatically takes care of finding a suitable place
to save your data, and provides simple functions to access and modify the data
within.")
      (license license:zlib))))

(define-public ecl-ubiquitous
  (sbcl-package->ecl-package sbcl-ubiquitous))

(define-public cl-ubiquitous
  (sbcl-package->cl-source-package sbcl-ubiquitous))

(define-public sbcl-uax-15
  (package
    (name "sbcl-uax-15")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sabracrolleton/uax-15")
             (commit (string-append "v" version))))
       (file-name (git-file-name "uax-15" version))
       (sha256
        (base32 "0p2ckw7mzxhwa9vbwj2q2dzayz9dl94d9yqd2ynp0pc5v8i0n2fr"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:asd-systems
       '("uax-15")))
    (native-inputs
     (list sbcl-fiveam))
    (inputs
     `(("cl-ppcre" ,sbcl-cl-ppcre)
       ("split-sequence" ,sbcl-split-sequence)))
    (home-page "https://github.com/sabracrolleton/uax-15")
    (synopsis "Common Lisp implementation of unicode normalization functions")
    (description
     "This package provides supports for unicode normalization, RFC8264 and
RFC7564.")
    (license license:expat)))

(define-public cl-uax-15
  (sbcl-package->cl-source-package sbcl-uax-15))

(define-public ecl-uax-15
  (sbcl-package->ecl-package sbcl-uax-15))

(define-public sbcl-cl-unicode
  (package
    (name "sbcl-cl-unicode")
    (version "0.1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/edicl/cl-unicode")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ykx2s9lqfl74p1px0ik3l2izd1fc9jd1b4ra68s5x34rvjy0hza"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-flexi-streams))
    (inputs
     (list sbcl-cl-ppcre))
    (home-page "http://weitz.de/cl-unicode/")
    (synopsis "Portable Unicode library for Common Lisp")
    (description "CL-UNICODE is a portable Unicode library Common Lisp, which
is compatible with perl.  It is pretty fast, thread-safe, and compatible with
ANSI-compliant Common Lisp implementations.")
    (license license:bsd-2)))

(define-public ecl-cl-unicode
  (sbcl-package->ecl-package sbcl-cl-unicode))

(define-public cl-unicode
  (sbcl-package->cl-source-package sbcl-cl-unicode))

(define-public sbcl-cl-ppcre-unicode
  (package (inherit sbcl-cl-ppcre)
    (name "sbcl-cl-ppcre-unicode")
    (inputs
     (list sbcl-cl-ppcre sbcl-cl-unicode))
    (arguments
     `(#:tests? #f ; tests fail with "Component :CL-PPCRE-TEST not found"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-ppcre
           ;; cl-ppcre and cl-ppcre-unicode are put in different packages
           ;; to work around the circular dependency between edicl/cl-ppcre
           ;; and edicl/cl-unicode.
           (lambda _
             (delete-file "cl-ppcre.asd")
             #t)))))))

(define-public cl-ppcre-unicode
  (sbcl-package->cl-source-package sbcl-cl-ppcre-unicode))

(define-public ecl-cl-ppcre-unicode
  (sbcl-package->ecl-package sbcl-cl-ppcre-unicode))

(define-public sbcl-zpb-ttf
  (package
    (name "sbcl-zpb-ttf")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xach/zpb-ttf")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wh66vjijzqlydnrihynpwp6796917xwrh0i9li93c17kyxa74ih"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/xach/zpb-ttf")
    (synopsis "TrueType font file access for Common Lisp")
    (description
     "ZPB-TTF is a TrueType font file parser that provides an interface for
reading typographic metrics, glyph outlines, and other information from the
file.")
    (license license:bsd-2)))

(define-public ecl-zpb-ttf
  (sbcl-package->ecl-package sbcl-zpb-ttf))

(define-public cl-zpb-ttf
  (sbcl-package->cl-source-package sbcl-zpb-ttf))

(define-public sbcl-cl-vectors
  (package
    (name "sbcl-cl-vectors")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://projects.tuxee.net/cl-vectors/"
                           "files/cl-vectors-" version ".tar.gz"))
       (sha256
        (base32
         "04lhwi0kq8pkwhgd885pk80m1cp9sfvjjn5zj70s1dnckibhdmqh"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("zpb-ttf" ,sbcl-zpb-ttf)))
    (arguments
     '(#:asd-systems '("cl-vectors"
                       "cl-paths-ttf")))
    (home-page "http://projects.tuxee.net/cl-vectors/")
    (synopsis "Create, transform and render anti-aliased vectorial paths")
    (description
     "This is a pure Common Lisp library to create, transform and render
anti-aliased vectorial paths.")
    (license license:expat)))

(define-public ecl-cl-vectors
  (sbcl-package->ecl-package sbcl-cl-vectors))

(define-public cl-vectors
  (sbcl-package->cl-source-package sbcl-cl-vectors))

(define-public sbcl-spatial-trees
  ;; There have been no releases.
  (let ((commit "81fdad0a0bf109c80a53cc96eca2e093823400ba")
        (revision "1"))
    (package
      (name "sbcl-spatial-trees")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rpav/spatial-trees")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "11rhc6h501dwcik2igkszz7b9n515cr99m5pjh4r2qfwgiri6ysa"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:tests? #f           ; spatial-trees.test requires spatial-trees.nns
         #:test-asd-file "spatial-trees.test.asd"))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/rpav/spatial-trees")
      (synopsis "Dynamic index data structures for spatially-extended data")
      (description
       "Spatial-trees is a set of dynamic index data structures for
spatially-extended data.")
      (license license:bsd-3))))

(define-public ecl-spatial-trees
  (sbcl-package->ecl-package sbcl-spatial-trees))

(define-public cl-spatial-trees
  (sbcl-package->cl-source-package sbcl-spatial-trees))

(define-public sbcl-flexichain
  ;; There are no releases.
  (let ((commit "13d2a6c505ed0abfcd4c4ec7d7145059b06855d6")
        (revision "1"))
    (package
      (name "sbcl-flexichain")
      (version "1.5.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/robert-strandh/Flexichain")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0pfyvhsfbjd2sjb30grfs52r51a428xglv7bwydvpg2lc117qimg"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/robert-strandh/Flexichain.git")
      (synopsis "Dynamically add elements to or remove them from sequences")
      (description
       "This package provides an implementation of the flexichain protocol,
allowing client code to dynamically add elements to, and delete elements from
a sequence (or chain) of such elements.")
      (license license:lgpl2.1+))))

(define-public ecl-flexichain
  (sbcl-package->ecl-package sbcl-flexichain))

(define-public cl-flexichain
  (sbcl-package->cl-source-package sbcl-flexichain))

(define-public sbcl-cl-pdf
  (let ((commit "dbafd62afcb2d2e9164054c72612763721297d59")
        (revision "1"))
    (package
      (name "sbcl-cl-pdf")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mbattyani/cl-pdf")
               (commit commit)))
         (file-name (git-file-name "cl-pdf" version))
         (sha256
          (base32 "0w6igiav35a65h6r4p1g6dw2i7mw0s06mviw31768r6z62l1ny1v"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-iterate sbcl-zpb-ttf))
      (home-page "https://github.com/mbattyani/cl-pdf")
      (synopsis "Common Lisp library for generating PDF files")
      (description
       "CL-PDF is a cross-platform Common Lisp library for generating PDF
files.")
      (license license:bsd-2))))

(define-public ecl-cl-pdf
  (sbcl-package->ecl-package sbcl-cl-pdf))

(define-public cl-pdf
  (sbcl-package->cl-source-package sbcl-cl-pdf))

(define-public sbcl-clx
  (package
    (name "sbcl-clx")
    (version "0.7.5")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/sharplispers/clx")
         (commit version)))
       (sha256
        (base32
         "1vi67z9hpj5rr4xcmfbfwzmlcc0ah7hzhrmfid6lqdkva238v2wf"))
       (file-name (string-append "clx-" version))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-fiasco))
    (home-page "https://www.cliki.net/portable-clx")
    (synopsis "X11 client library for Common Lisp")
    (description "CLX is an X11 client library for Common Lisp.  The code was
originally taken from a CMUCL distribution, was modified somewhat in order to
make it compile and run under SBCL, then a selection of patches were added
from other CLXes around the net.")
    (license license:x11)))

(define-public cl-clx
  (sbcl-package->cl-source-package sbcl-clx))

(define-public ecl-clx
  (sbcl-package->ecl-package sbcl-clx))

(define-public sbcl-clx-truetype
  (let ((commit "c6e10a918d46632324d5863a8ed067a83fc26de8")
        (revision "1"))
    (package
      (name "sbcl-clx-truetype")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/l04m33/clx-truetype")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "079hyp92cjkdfn6bhkxsrwnibiqbz4y4af6nl31lzw6nm91j5j37"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (substitute* "package.lisp"
               ((":export") ":export\n   :+font-cache-filename+"))
             #t))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-clx
             sbcl-zpb-ttf
             sbcl-cl-vectors
             sbcl-cl-fad
             sbcl-cl-store
             sbcl-trivial-features))
      (home-page "https://github.com/l04m33/clx-truetype")
      (synopsis "Antialiased TrueType font rendering using CLX and XRender")
      (description "CLX-TrueType is pure common lisp solution for
antialiased TrueType font rendering using CLX and XRender extension.")
      (license license:expat))))

(define-public cl-clx-truetype
  (sbcl-package->cl-source-package sbcl-clx-truetype))

(define-public ecl-clx-truetype
  (sbcl-package->ecl-package sbcl-clx-truetype))

(define-public sbcl-slynk
  (let ((commit "0470c0281498b9de072fcbf3718fc66720eeb3d0"))
    (package
      (name "sbcl-slynk")
      (version (git-version "1.0.43" "5" commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/joaotavora/sly")
           (commit commit)))
         (sha256
          (base32 "1ws2a9azmdkkg47xnd4jggna45nf0bh54gyp0799b44c4bgjp029"))
         (file-name (git-file-name "slynk" version))))
      (build-system asdf-build-system/sbcl)
      (outputs '("out" "image"))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'create-asdf-configuration 'build-image
             (lambda* (#:key outputs #:allow-other-keys)
               (build-image (string-append
                             (assoc-ref outputs "image")
                             "/bin/slynk")
                            outputs
                            #:dependencies '("slynk"
                                             "slynk/arglists"
                                             "slynk/fancy-inspector"
                                             "slynk/package-fu"
                                             "slynk/mrepl"
                                             "slynk/trace-dialog"
                                             "slynk/profiler"
                                             "slynk/stickers"
                                             "slynk/indentation"
                                             "slynk/retro"))
               #t)))))
      (synopsis "Common Lisp IDE for Emacs")
      (description "SLY is a fork of SLIME, an IDE backend for Common Lisp.
It also features a completely redesigned REPL based on Emacs's own
full-featured @code{comint-mode}, live code annotations, and a consistent interactive
button interface.  Everything can be copied to the REPL.  One can create
multiple inspectors with independent history.")
      (home-page "https://github.com/joaotavora/sly")
      (license license:public-domain)
      (properties `((cl-source-variant . ,(delay cl-slynk)))))))

(define-public cl-slynk
  (sbcl-package->cl-source-package sbcl-slynk))

(define-public ecl-slynk
  (let ((pkg (sbcl-package->ecl-package sbcl-slynk)))
    (package
      (inherit pkg)
      (outputs '("out"))
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'build-image))))))))

(define-public sbcl-parse-js
  (let ((commit "fbadc6029bec7039602abfc06c73bb52970998f6")
        (revision "1"))
    (package
      (name "sbcl-parse-js")
      (version (string-append "0.0.0-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://marijn.haverbeke.nl/git/parse-js")
               (commit commit)))
         (file-name (string-append name "-" commit "-checkout"))
         (sha256
          (base32
           "1wddrnr5kiya5s3gp4cdq6crbfy9fqcz7fr44p81502sj3bvdv39"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://marijnhaverbeke.nl/parse-js/")
      (synopsis "Parse JavaScript")
      (description "Parse-js is a Common Lisp package for parsing
JavaScript (ECMAScript 3).  It has basic support for ECMAScript 5.")
      (license license:zlib))))

(define-public cl-parse-js
  (sbcl-package->cl-source-package sbcl-parse-js))

(define-public ecl-parse-js
  (sbcl-package->ecl-package sbcl-parse-js))

(define-public sbcl-parse-number
  (package
    (name "sbcl-parse-number")
    (version "1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sharplispers/parse-number/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0sk06ib1bhqv9y39vwnnw44vmbc4b0kvqm37xxmkxd4dwchq82d7"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://www.cliki.net/PARSE-NUMBER")
    (synopsis "Parse numbers")
    (description "@code{parse-number} is a library of functions for parsing
strings into one of the standard Common Lisp number types without using the
reader.  @code{parse-number} accepts an arbitrary string and attempts to parse
the string into one of the standard Common Lisp number types, if possible, or
else @code{parse-number} signals an error of type @code{invalid-number}.")
    (license license:bsd-3)))

(define-public cl-parse-number
  (sbcl-package->cl-source-package sbcl-parse-number))

(define-public ecl-parse-number
  (sbcl-package->ecl-package sbcl-parse-number))

(define-public sbcl-iterate
  (package
    (name "sbcl-iterate")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://common-lisp.net/project/iterate/releases/"
                           "iterate-" version ".tar.gz"))
       (sha256
        (base32
         "1lqsbhrkfl0yif46aymvb7l3nb9wdcmj4jyw485blj32jb4famzn"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-rt))
    (home-page "https://common-lisp.net/project/iterate/")
    (synopsis "Iteration construct for Common Lisp")
    (description "@code{iterate} is an iteration construct for Common Lisp.
It is similar to the @code{CL:LOOP} macro, with these distinguishing marks:

@itemize
@item it is extensible,
@item it helps editors like Emacs indent iterate forms by having a more
  lisp-like syntax, and
@item it isn't part of the ANSI standard for Common Lisp.
@end itemize\n")
    (license license:expat)))

(define-public cl-iterate
  (sbcl-package->cl-source-package sbcl-iterate))

(define-public ecl-iterate
  (sbcl-package->ecl-package sbcl-iterate))

(define-public sbcl-cl-uglify-js
  ;; There have been many bug fixes since the 2010 release.
  (let ((commit "429c5e1d844e2f96b44db8fccc92d6e8e28afdd5")
        (revision "1"))
    (package
      (name "sbcl-cl-uglify-js")
      (version (string-append "0.1-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mishoo/cl-uglify-js")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0k39y3c93jgxpr7gwz7w0d8yknn1fdnxrjhd03057lvk5w8js27a"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-parse-js sbcl-cl-ppcre sbcl-cl-ppcre-unicode
             sbcl-parse-number sbcl-iterate))
      (home-page "https://github.com/mishoo/cl-uglify-js")
      (synopsis "JavaScript compressor library for Common Lisp")
      (description "This is a Common Lisp version of UglifyJS, a JavaScript
compressor.  It works on data produced by @code{parse-js} to generate a
@dfn{minified} version of the code.  Currently it can:

@itemize
@item reduce variable names (usually to single letters)
@item join consecutive @code{var} statements
@item resolve simple binary expressions
@item group most consecutive statements using the @code{sequence} operator (comma)
@item remove unnecessary blocks
@item convert @code{IF} expressions in various ways that result in smaller code
@item remove some unreachable code
@end itemize\n")
      (license license:zlib))))

(define-public cl-uglify-js
  (sbcl-package->cl-source-package sbcl-cl-uglify-js))

(define-public ecl-cl-uglify-js
  (sbcl-package->ecl-package sbcl-cl-uglify-js))

(define-public uglify-js
  (package
    (inherit sbcl-cl-uglify-js)
    (name "uglify-js")
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let* ((bin    (string-append (assoc-ref %outputs "out") "/bin/"))
              (script (string-append bin "uglify-js")))
         (use-modules (guix build utils))
         (mkdir-p bin)
         (with-output-to-file script
           (lambda _
             (format #t "#!~a/bin/sbcl --script

 (require :asdf)
 (asdf:initialize-source-registry
  #p\"~a/etc/common-lisp/source-registry.conf.d/\")
 (asdf:initialize-output-translations
  #p\"~a/etc/common-lisp/asdf-output-translations.conf.d/\")"
                     (assoc-ref %build-inputs "sbcl")
                     (assoc-ref %build-inputs "sbcl-cl-uglify-js")
                     (assoc-ref %build-inputs "sbcl-cl-uglify-js"))
             ;; FIXME: cannot use progn here because otherwise it fails to
             ;; find cl-uglify-js.
             (for-each
              write
              '(;; Quiet, please!
                (let ((*standard-output* (make-broadcast-stream))
                      (*error-output* (make-broadcast-stream)))
                  (asdf:load-system :cl-uglify-js))
                (let ((file (cadr *posix-argv*)))
                  (if file
                      (format t "~a"
                              (cl-uglify-js:ast-gen-code
                               (cl-uglify-js:ast-mangle
                                (cl-uglify-js:ast-squeeze
                                 (with-open-file (in file)
                                                 (parse-js:parse-js in))))
                               :beautify nil))
                      (progn
                       (format *error-output*
                               "Please provide a JavaScript file.~%")
                       (sb-ext:exit :code 1))))))))
         (chmod script #o755)
         #t)))
    (inputs
     (list sbcl sbcl-cl-uglify-js))
    (synopsis "JavaScript compressor")))

(define-public sbcl-cl-strings
  (let ((revision "1")
        (commit "c5c5cbafbf3e6181d03c354d66e41a4f063f00ae"))
    (package
      (name "sbcl-cl-strings")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/diogoalexandrefranco/cl-strings")
               (commit commit)))
         (sha256
          (base32
           "00754mfaqallj480lwd346nkfb6ra8pa8xcxcylf4baqn604zlmv"))
         (file-name (string-append "cl-strings-" version "-checkout"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Portable, dependency-free set of utilities to manipulate strings in Common Lisp")
      (description
       "@command{cl-strings} is a small, portable, dependency-free set of
utilities that make it even easier to manipulate text in Common Lisp.  It has
100% test coverage and works at least on sbcl, ecl, ccl, abcl and clisp.")
      (home-page "https://github.com/diogoalexandrefranco/cl-strings")
      (license license:expat))))

(define-public cl-strings
  (sbcl-package->cl-source-package sbcl-cl-strings))

(define-public ecl-cl-strings
  (sbcl-package->ecl-package sbcl-cl-strings))

(define-public sbcl-trivial-features
  (package
    (name "sbcl-trivial-features")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trivial-features/trivial-features")
             (commit (string-append "v" version))))
       (file-name (git-file-name "trivial-features" version))
       (sha256
        (base32 "0jsqah1znzqilxnw5vannb083ayk0d7phkackqzwwqkyg5hpn6pq"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     '(#:asd-files '("trivial-features.asd")
       ;; FIXME: Tests disabled because of a circular dependency between
       ;; trivial-features and cffi.
       #:tests? #f))
    ;; (native-inputs
    ;;  `(("cffi" ,sbcl-cffi)))
    (home-page "https://cliki.net/trivial-features")
    (synopsis "Ensures consistency of @code{*FEATURES*} in Common Lisp")
    (description "Trivial-features ensures that @code{*FEATURES*} is
consistent across multiple Common Lisp implementations.")
    (license license:expat)))

(define-public cl-trivial-features
  (sbcl-package->cl-source-package sbcl-trivial-features))

(define-public ecl-trivial-features
  (sbcl-package->ecl-package sbcl-trivial-features))

(define-public sbcl-hu.dwim.asdf
  (let ((commit "67cdf84390e530af4303cc4bc815fdf2a5e48f59"))
    (package
      (name "sbcl-hu.dwim.asdf")
      (version "20200724")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hu-dwim/hu.dwim.asdf")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0p81jalilkaqw832a12s35q0z6rrarxjasm1jy6h4fvyj9pf0zkx"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://hub.darcs.net/hu.dwim/hu.dwim.asdf")
      (synopsis "Extensions to ASDF")
      (description "Various ASDF extensions such as attached test and
documentation system, explicit development support, etc.")
      (license license:public-domain))))

(define-public cl-hu.dwim.asdf
  (sbcl-package->cl-source-package sbcl-hu.dwim.asdf))

(define-public ecl-hu.dwim.asdf
  (sbcl-package->ecl-package sbcl-hu.dwim.asdf))

(define-public sbcl-babel
  ;; No release since 2014.
  (let ((commit "aeed2d1b76358db48e6b70a64399c05678a6b9ea"))
    (package
      (name "sbcl-babel")
      (version (git-version "0.5.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cl-babel/babel")
               (commit commit)))
         (file-name (git-file-name "babel" version))
         (sha256
          (base32 "0lkvv4xdpv4cv1y2bqillmabx8sdb2y4l6pbinq6mjh33w2brpvb"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-hu.dwim.stefil))
      (inputs
       (list sbcl-alexandria sbcl-trivial-features))
      (home-page "https://common-lisp.net/project/babel/")
      (synopsis "Charset encoding and decoding library")
      (description "Babel is a charset encoding and decoding library, not unlike
GNU libiconv, but completely written in Common Lisp.")
      (license license:expat))))

(define-public cl-babel
  (sbcl-package->cl-source-package sbcl-babel))

(define-public ecl-babel
  (sbcl-package->ecl-package sbcl-babel))

(define-public sbcl-cl-yacc
  (package
    (name "sbcl-cl-yacc")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jech/cl-yacc")
             (commit (string-append "cl-yacc-" version))))
       (sha256
        (base32
         "16946pzf8vvadnyfayvj8rbh4zjzw90h0azz2qk1mxrvhh5wklib"))
       (file-name (string-append "cl-yacc-" version "-checkout"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:asd-systems '("yacc")))
    (synopsis "LALR(1) parser generator for Common Lisp, similar in spirit to Yacc")
    (description
     "CL-Yacc is a LALR(1) parser generator for Common Lisp, similar in spirit
to AT&T Yacc, Berkeley Yacc, GNU Bison, Zebu, lalr.cl or lalr.scm.

CL-Yacc uses the algorithm due to Aho and Ullman, which is the one also used
by AT&T Yacc, Berkeley Yacc and Zebu.  It does not use the faster algorithm due
to DeRemer and Pennello, which is used by Bison and lalr.scm (not lalr.cl).")
    (home-page "https://www.irif.fr/~jch//software/cl-yacc/")
    (license license:expat)))

(define-public cl-yacc
  (sbcl-package->cl-source-package sbcl-cl-yacc))

(define-public ecl-cl-yacc
  (sbcl-package->ecl-package sbcl-cl-yacc))

(define-public sbcl-eager-future2
  (let ((commit "54df8effd9d9eccac917509590286b5ac5f9cb30"))
    (package
      (name "sbcl-eager-future2")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/vsedach/eager-future2.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1qs1bv3m0ki8l5czhsflxcryh22r9d9g9a3a3b0cr0pl954q5rld"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-bordeaux-threads sbcl-trivial-garbage))
      (synopsis "Futures promises synchronization mechanism for Common Lisp")
      (description
       "Eager Future2 is a Common Lisp library that provides composable
concurrency primitives that unify parallel and lazy evaluation, are integrated
with the Common Lisp condition system, and have automatic resource
management.")
      (home-page "https://gitlab.common-lisp.net/vsedach/eager-future2")
      (license license:lgpl3+))))

(define-public cl-eager-future2
  (sbcl-package->cl-source-package sbcl-eager-future2))

(define-public ecl-eager-future2
  (sbcl-package->ecl-package sbcl-eager-future2))

(define-public sbcl-jpl-util
  (let ((commit "0311ed374e19a49d43318064d729fe3abd9a3b62"))
    (package
      (name "sbcl-jpl-util")
      (version "20151005")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               ;; Quicklisp uses this fork.
               (url "https://github.com/hawkir/cl-jpl-util")
               (commit commit)))
         (file-name
          (git-file-name "jpl-util" version))
         (sha256
          (base32
           "0nc0rk9n8grkg3045xsw34whmcmddn2sfrxki4268g7kpgz0d2yz"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Collection of Common Lisp utility functions and macros")
      (description
       "@command{cl-jpl-util} is a collection of Common Lisp utility functions
and macros, primarily for software projects written in CL by the author.")
      (home-page "https://www.thoughtcrime.us/software/cl-jpl-util/")
      (license license:isc))))

(define-public cl-jpl-util
  (sbcl-package->cl-source-package sbcl-jpl-util))

(define-public ecl-jpl-util
  (sbcl-package->ecl-package sbcl-jpl-util))

(define-public sbcl-piping
  (let ((commit "c7a4163c00dea7e72bf6ad33d6abac0d5826a656")
        (revision "1"))
    (package
      (name "sbcl-piping")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/piping/")
               (commit commit)))
         (file-name (git-file-name "piping" version))
         (sha256
          (base32 "0in84qnfkynm36d4n4d6v87vprpi27xrydnga462wfhplji6klv5"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://shinmera.github.io/piping/")
      (synopsis "Library to enable simple message pipelines")
      (description
       "This is a Common Lisp library to enable simple message pipelines.")
      (license license:zlib))))

(define-public ecl-piping
  (sbcl-package->ecl-package sbcl-piping))

(define-public cl-piping
  (sbcl-package->cl-source-package sbcl-piping))

(define-public sbcl-cl-pcg
  (let ((commit "8263d85ab0ca17fb05637a4430c2d564456bce8f")
        (revision "1"))
    (package
      (name "sbcl-cl-pcg")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sjl/cl-pcg")
               (commit commit)))
         (file-name (git-file-name "cl-pcg" version))
         (sha256
          (base32 "0s57wvvlvshp1gcp9i9d3qcmqhswnxps3i0y7wbb0v8i1a3p46m4"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-1am))
      (home-page "https://github.com/sjl/cl-pcg")
      (synopsis "Permuted congruential generators in Common Lisp")
      (description
       "This is a bare-bones Permuted Congruential Generator implementation in
pure Common Lisp.")
      (license license:expat))))

(define-public ecl-cl-pcg
  (sbcl-package->ecl-package sbcl-cl-pcg))

(define-public cl-pcg
  (sbcl-package->cl-source-package sbcl-cl-pcg))

(define-public sbcl-seedable-rng
  (let ((commit "aa1a1564b6e07e2698df37c7a98348c4f762cb15")
        (revision "1"))
    (package
      (name "sbcl-seedable-rng")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.mfiano.net/mfiano/seedable-rng")
               (commit commit)))
         (file-name (git-file-name "seedable-rng" version))
         (sha256
          (base32 "1ldpsbp3qrfzvknclsxj3sdyms1jf9ad20dvh4w0kw3zgahn2nr5"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-pcg sbcl-golden-utils sbcl-ironclad))
      (home-page "https://git.mfiano.net/mfiano/seedable-rng")
      (synopsis "Common Lisp random number generator")
      (description
       "SEEDABLE-RNG provides a convenient means of generating random numbers
that are seedable with deterministic results across hardware and Common Lisp
implementations.")
      (license license:expat))))

(define-public ecl-seedable-rng
  (sbcl-package->ecl-package sbcl-seedable-rng))

(define-public cl-seedable-rng
  (sbcl-package->cl-source-package sbcl-seedable-rng))

(define-public sbcl-jpl-queues
  (package
    (name "sbcl-jpl-queues")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.thoughtcrime.us/software/jpl-queues/jpl-queues-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1wvvv7j117h9a42qaj1g4fh4mji28xqs7s60rn6d11gk9jl76h96"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("jpl-util" ,sbcl-jpl-util)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)))
    (arguments
     ;; Tests seem to be broken.
     `(#:tests? #f))
    (synopsis "Common Lisp library implementing a few different kinds of queues")
    (description
     "A Common Lisp library implementing a few different kinds of queues:

@itemize
@item Bounded and unbounded FIFO queues.
@item Lossy bounded FIFO queues that drop elements when full.
@item Unbounded random-order queues that use less memory than unbounded FIFO queues.
@end itemize

Additionally, a synchronization wrapper is provided to make any queue
conforming to the @command{jpl-queues} API thread-safe for lightweight
multithreading applications.  (See Calispel for a more sophisticated CL
multithreaded message-passing library with timeouts and alternation among
several blockable channels.)")
    (home-page "https://www.thoughtcrime.us/software/jpl-queues/")
    (license license:isc)))

(define-public cl-jpl-queues
  (sbcl-package->cl-source-package sbcl-jpl-queues))

(define-public ecl-jpl-queues
  (sbcl-package->ecl-package sbcl-jpl-queues))

(define-public sbcl-calispel
  (let ((commit "e9f2f9c1af97f4d7bb4c8ac25fb2a8f3e8fada7a"))
    (package
      (name "sbcl-calispel")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               ;; This fork replaces the dependency on the obsolete
               ;; eager-future with eager-future2.
               (url "https://github.com/hawkir/calispel")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "08bmf3pi7n5hadpmqqkg65cxcj6kbvm997wcs1f53ml1nb79d9z8"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-jpl-queues sbcl-bordeaux-threads))
      (native-inputs
       (list sbcl-eager-future2))
      (synopsis "Thread-safe message-passing channels in Common Lisp")
      (description
       "Calispel is a Common Lisp library for thread-safe message-passing
channels, in the style of the occam programming language, also known as
communicating sequential processes (CSP).  See
@url{https://en.wikipedia.org/wiki/Communicating_sequential_processes}.

Calispel channels let one thread communicate with another, facilitating
unidirectional communication of any Lisp object.  Channels may be unbuffered,
where a sender waits for a receiver (or vice versa) before either operation can
continue, or channels may be buffered with flexible policy options.

Because sending and receiving on a channel may block, either operation can time
out after a specified amount of time.

A syntax for alternation is provided (like @code{ALT} in occam, or Unix
@code{select()}): given a sequence of operations, any or all of which may
block, alternation selects the first operation that doesn't block and executes
associated code.  Alternation can also time out, executing an \"otherwise\"
clause if no operation becomes available within a set amount of time.

Calispel is a message-passing library, and as such leaves the role of
threading abstractions and utilities left to be filled by complementary
libraries such as Bordeaux-Threads and Eager Future.")
      (home-page "https://www.thoughtcrime.us/software/jpl-queues/")
      (license license:isc))))

(define-public cl-calispel
  (sbcl-package->cl-source-package sbcl-calispel))

(define-public ecl-calispel
  (sbcl-package->ecl-package sbcl-calispel))

(define-public sbcl-esrap
  (let ((commit "866f28fa7a2c1d3fb6d0d0423850d1f9d955750f")
        (revision "2"))
    (package
      (name "sbcl-esrap")
      (version (git-version "0.18" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/scymtym/esrap")
               (commit commit)))
         (sha256
          (base32 "19rb3dmpw3csqqagmrf80dpk5d2qn0l7fpfmxx5zwdnjk367kbwg"))
         (file-name (git-file-name "esrap" version))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-alexandria sbcl-trivial-with-current-source-form))
      (synopsis "Common Lisp packrat parser")
      (description
       "This is a packrat parser for Common Lisp.
In addition to regular Packrat / Parsing Grammar / TDPL features ESRAP supports:

@itemize
@item dynamic redefinition of nonterminals
@item inline grammars
@item semantic predicates
@item introspective facilities (describing grammars, tracing, setting breaks)
@item left-recursive grammars
@item functions as terminals
@item accurate, customizable parse error reports
@end itemize\n")
      (home-page "https://scymtym.github.io/esrap/")
      (license license:expat))))

(define-public cl-esrap
  (sbcl-package->cl-source-package sbcl-esrap))

(define-public ecl-esrap
  (sbcl-package->ecl-package sbcl-esrap))

(define-public sbcl-split-sequence
  (package
    (name "sbcl-split-sequence")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sharplispers/split-sequence")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "172k7iv775kwism6304p6z7mqpjvipl57nq1bgvmbk445943fmhq"))
       (file-name (git-file-name "split-sequence" version))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-fiveam))
    (synopsis "Member of the Common Lisp Utilities family of programs")
    (description
     "Splits sequence into a list of subsequences delimited by objects
satisfying the test.")
    (home-page "https://cliki.net/split-sequence")
    (license license:expat)))

(define-public cl-split-sequence
  (sbcl-package->cl-source-package sbcl-split-sequence))

(define-public ecl-split-sequence
  (sbcl-package->ecl-package sbcl-split-sequence))

(define-public sbcl-html-encode
  (package
    (name "sbcl-html-encode")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://beta.quicklisp.org/archive/html-encode/2010-10-06/html-encode-"
             version ".tgz"))
       (sha256
        (base32
         "06mf8wn95yf5swhmzk4vp0xr4ylfl33dgfknkabbkd8n6jns8gcf"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Common Lisp library for encoding text in various web-savvy encodings")
    (description
     "A library for encoding text in various web-savvy encodings.")
    (home-page "http://quickdocs.org/html-encode/")
    (license license:expat)))

(define-public cl-html-encode
  (sbcl-package->cl-source-package sbcl-html-encode))

(define-public ecl-html-encode
  (sbcl-package->ecl-package sbcl-html-encode))

(define-public sbcl-colorize
  (let ((commit "ea676b584e0899cec82f21a9e6871172fe3c0eb5"))
    (package
      (name "sbcl-colorize")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kingcons/colorize")
               (commit commit)))
         (sha256
          (base32
           "1pdg4kiaczmr3ivffhirp7m3lbr1q27rn7dhaay0vwghmi31zcw9"))
         (file-name (git-file-name "colorize" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-split-sequence sbcl-html-encode))
      (synopsis "Common Lisp for syntax highlighting")
      (description
       "@command{colorize} is a Lisp library for syntax highlighting
supporting the following languages: Common Lisp, Emacs Lisp, Scheme, Clojure,
C, C++, Java, Python, Erlang, Haskell, Objective-C, Diff, Webkit.")
      (home-page "https://github.com/kingcons/colorize")
      ;; TODO: Missing license?
      (license license:expat))))

(define-public cl-colorize
  (sbcl-package->cl-source-package sbcl-colorize))

(define-public ecl-colorize
  (sbcl-package->ecl-package sbcl-colorize))

(define-public sbcl-3bmd
  (let ((commit "6fc5759448f6f6df6f6df556e020a289a2643288")
        (revision "2"))
    (package
      (name "sbcl-3bmd")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/3b/3bmd")
               (commit commit)))
         (sha256
          (base32 "1avmbp8xdjlbqpqk7p3vmj7abiw5p3vb5mrxp4wlvgql4sf6z3p4"))
         (file-name (git-file-name "3bmd" version))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems
         '("3bmd"
           "3bmd-ext-definition-lists"
           "3bmd-ext-math"
           "3bmd-ext-tables"
           "3bmd-ext-wiki-links"
           "3bmd-youtube"
           "3bmd-ext-code-blocks")))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("colorize" ,sbcl-colorize)
         ("esrap" ,sbcl-esrap)
         ("split-sequence" ,sbcl-split-sequence)))
      (home-page "https://github.com/3b/3bmd")
      (synopsis "Markdown processor in Command Lisp using esrap parser")
      (description
       "This is a Common Lisp Markdown to HTML converter, using @command{esrap}
for parsing, and grammar based on @command{peg-markdown}.")
      (license license:expat))))

(define-public cl-3bmd
  (sbcl-package->cl-source-package sbcl-3bmd))

(define-public ecl-3bmd
  (sbcl-package->ecl-package sbcl-3bmd))

(define-public sbcl-cl-fad
  (package
    (name "sbcl-cl-fad")
    (version "0.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edicl/cl-fad/")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1gc8i82v6gks7g0lnm54r4prk2mklidv2flm5fvbr0a7rsys0vpa"))
       (file-name (string-append "cl-fad" version "-checkout"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-bordeaux-threads))
    (synopsis "Portable pathname library for Common Lisp")
    (description
     "CL-FAD (for \"Files and Directories\") is a thin layer atop Common
Lisp's standard pathname functions.  It is intended to provide some
unification between current CL implementations on Windows, OS X, Linux, and
Unix.  Most of the code was written by Peter Seibel for his book Practical
Common Lisp.")
    (home-page "https://edicl.github.io/cl-fad/")
    (license license:bsd-2)))

(define-public cl-fad
  (sbcl-package->cl-source-package sbcl-cl-fad))

(define-public ecl-cl-fad
  (sbcl-package->ecl-package sbcl-cl-fad))

(define-public sbcl-fn
  (let ((commit "8d8587d03a7b5e26b306fc90018e385d9e5acc2c")
        (revision "1"))
    (package
      (name "sbcl-fn")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cbaggers/fn")
               (commit commit)))
         (file-name (git-file-name "fn" version))
         (sha256
          (base32 "0yyp9z6iwx476whz0n1rpjznjyqqhlylhzwpgg5xx92lxmskl752"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-named-readtables))
      (home-page "https://github.com/cbaggers/fn")
      (synopsis "Macros for lambda brevity")
      (description
       "This is a Common Lisp library providing lambda shorthand macros aiming
to be used in cases where the word @emph{lambda} and the arguments are longer
than the body of the lambda.")
      (license license:public-domain))))

(define-public ecl-fn
  (sbcl-package->ecl-package sbcl-fn))

(define-public cl-fn
  (sbcl-package->cl-source-package sbcl-fn))

(define-public sbcl-nibbles
  ;; No tagged release since 2018.
  (let ((commit "dad25240928d5cf8f7df69c4398244e03570bb35")
        (revision "2"))
    (package
      (name "sbcl-nibbles")
      (version (git-version "0.14" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/nibbles/")
               (commit commit)))
         (sha256
          (base32 "0r6ljlpgjmkf87pmvdwzva8qj15bhznc3ylgcjjqyy4frbx9lygz"))
         (file-name (git-file-name "nibbles" version))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       ;; Tests only.
       (list sbcl-rt))
      (synopsis
       "Common Lisp library for accessing octet-addressed blocks of data")
      (description
       "When dealing with network protocols and file formats, it's common to
have to read or write 16-, 32-, or 64-bit datatypes in signed or unsigned
flavors.  Common Lisp sort of supports this by specifying :element-type for
streams, but that facility is underspecified and there's nothing similar for
read/write from octet vectors.  What most people wind up doing is rolling their
own small facility for their particular needs and calling it a day.

This library attempts to be comprehensive and centralize such
facilities.  Functions to read 16-, 32-, and 64-bit quantities from octet
vectors in signed or unsigned flavors are provided; these functions are also
SETFable.  Since it's sometimes desirable to read/write directly from streams,
functions for doing so are also provided.  On some implementations,
reading/writing IEEE singles/doubles (i.e. single-float and double-float) will
also be supported.")
      (home-page "https://github.com/sharplispers/nibbles")
      (license license:bsd-3))))

(define-public cl-nibbles
  (sbcl-package->cl-source-package sbcl-nibbles))

(define-public ecl-nibbles
  (sbcl-package->ecl-package sbcl-nibbles))

(define-public sbcl-ironclad
  (package
    (name "sbcl-ironclad")
    (version "0.56")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sharplispers/ironclad/")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0dhakily94vswl8a6q9ad0af8nk4pnvfgx7sw9kxl1wdq1pkg3ni"))
       (file-name (git-file-name name version))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     ;; Tests only.
     (list sbcl-rt))
    (inputs
     (list sbcl-bordeaux-threads sbcl-flexi-streams))
    (synopsis "Cryptographic toolkit written in Common Lisp")
    (description
     "Ironclad is a cryptography library written entirely in Common Lisp.
It includes support for several popular ciphers, digests, MACs and public key
cryptography algorithms.  For several implementations that support Gray
streams, support is included for convenient stream wrappers.")
    (home-page "https://github.com/sharplispers/ironclad")
    (license license:bsd-3)))

(define-public cl-ironclad
  (sbcl-package->cl-source-package sbcl-ironclad))

(define-public ecl-ironclad
  (sbcl-package->ecl-package sbcl-ironclad))

(define-public sbcl-named-readtables
  (let ((commit "585a28eee8b1b1999279b48cb7e9731187e14b66")
        (revision "3"))
    (package
      (name "sbcl-named-readtables")
      (version (git-version "0.9" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/melisgl/named-readtables")
               (commit commit)))
         (sha256
          (base32 "072p5djqq9pliw9r20rmpz5r5q5yn6rhbp98vkkp7gfcnp5ppj51"))
         (file-name (git-file-name "named-readtables" version))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/melisgl/named-readtables/")
      (synopsis "Library that creates a namespace for named readtables")
      (description
       "Named readtables is a library that creates a namespace for named
readtables, which is akin to package namespacing in Common Lisp.")
      (license license:bsd-3))))

(define-public cl-named-readtables
  (sbcl-package->cl-source-package sbcl-named-readtables))

(define-public ecl-named-readtables
  (sbcl-package->ecl-package sbcl-named-readtables))

(define-public sbcl-py-configparser
  ;; NOTE: (Sharlatan <2021-01-05 Tue> <19:52:19 UTC+0000>) Project updated last
  ;; time 8y ago, it looks like abandoned. VCS of the project:
  ;; https://svn.common-lisp.net/py-configparser/trunk
  (package
    (name "sbcl-py-configparser")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://common-lisp.net/project/py-configparser/releases/"
             "py-configparser-" version ".tar.gz"))
       (sha256
        (base32 "0i4rqz5cv7d7c2w81x5lwy05s6fbi3zikf4k5kpi3bkx3cabwdxj"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-parse-number))
    (home-page "http://common-lisp.net/project/py-configparser/")
    (synopsis "ConfigParser Python module functionality for Common Lisp")
    (description "The py-configparser package implements the ConfigParser
Python module functionality in Common Lisp.  In short, it implements reading
and writing of .INI-file style configuration files with sections containing
key/value pairs of configuration options.  In line with the functionalities in
the python module, does this package implement basic interpolation of option
values in other options.")
    (license license:expat)))

(define-public cl-py-configparser
  (sbcl-package->cl-source-package sbcl-py-configparser))

(define-public ecl-py-configparser
  (sbcl-package->ecl-package sbcl-py-configparser))

(define-public sbcl-pythonic-string-reader
  (let ((commit "47a70ba1e32362e03dad6ef8e6f36180b560f86a"))
    (package
      (name "sbcl-pythonic-string-reader")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/smithzvk/pythonic-string-reader/")
               (commit commit)))
         (sha256
          (base32 "1b5iryqw8xsh36swckmz8rrngmc39k92si33fgy5pml3n9l5rq3j"))
         (file-name (git-file-name "pythonic-string-reader" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-named-readtables))
      (home-page "https://github.com/smithzvk/pythonic-string-reader")
      (synopsis "Read table modification inspired by Python's three quote strings")
      (description "This piece of code sets up some reader macros that make it
simpler to input string literals which contain backslashes and double quotes
This is very useful for writing complicated docstrings and, as it turns out,
writing code that contains string literals that contain code themselves.")
      (license license:bsd-3))))

(define-public cl-pythonic-string-reader
  (sbcl-package->cl-source-package sbcl-pythonic-string-reader))

(define-public ecl-pythonic-string-reader
  (sbcl-package->ecl-package sbcl-pythonic-string-reader))

(define-public sbcl-slime-swank
  (package
    (name "sbcl-slime-swank")
    (version "2.26.1")
    (source
     (origin
       (file-name (git-file-name "slime-swank" version))
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/slime/slime/")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1a25ixb7q4svqabxnhwkk43v47mbsh13qwm7qlazkd3zkr8j3cli"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     '(#:asd-systems '("swank")))
    (home-page "https://github.com/slime/slime")
    (synopsis "Common Lisp Swank server")
    (description
     "This is only useful if you want to start a Swank server in a Lisp
processes that doesn't run under Emacs.  Lisp processes created by
@command{M-x slime} automatically start the server.")
    (license (list license:gpl2+ license:public-domain))))

(define-public cl-slime-swank
  (sbcl-package->cl-source-package sbcl-slime-swank))

(define-public ecl-slime-swank
  (sbcl-package->ecl-package sbcl-slime-swank))

(define-public sbcl-mgl-pax
  (let ((commit "a7f904784ae59bbeeeb15a14348cda46ed9bdeb3")
        (revision "0"))
    (package
      (name "sbcl-mgl-pax")
      (version (git-version "0.0.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/melisgl/mgl-pax")
               (commit commit)))
         (sha256
          (base32 "119pb3485m6hqsqsaqpaq2x8xh5lrbqapw7zaqyq425n75vd1mc8"))
         (file-name (git-file-name "mgl-pax" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("3bmd" ,sbcl-3bmd)
         ("alexandria" ,sbcl-alexandria)
         ("colorize" ,sbcl-colorize)
         ("md5" ,sbcl-md5)
         ("named-readtables" ,sbcl-named-readtables)
         ("pythonic-string-reader" ,sbcl-pythonic-string-reader)
         ("swank" ,sbcl-slime-swank)))
      (arguments
       `(#:asd-systems '("mgl-pax"
                         "mgl-pax/navigate"
                         "mgl-pax/document"
                         "mgl-pax/transcribe")))
      (synopsis "Exploratory programming environment and documentation generator")
      (description
       "PAX provides an extremely poor man's Explorable Programming
environment.  Narrative primarily lives in so called sections that mix markdown
docstrings with references to functions, variables, etc, all of which should
probably have their own docstrings.

The primary focus is on making code easily explorable by using SLIME's
@command{M-.} (@command{slime-edit-definition}).  See how to enable some
fanciness in Emacs Integration.  Generating documentation from sections and all
the referenced items in Markdown or HTML format is also implemented.

With the simplistic tools provided, one may accomplish similar effects as with
Literate Programming, but documentation is generated from code, not vice versa
and there is no support for chunking yet.  Code is first, code must look
pretty, documentation is code.")
      (home-page "https://melisgl.github.io/mgl-pax/")
      (license license:expat))))

(define-public cl-mgl-pax
  (sbcl-package->cl-source-package sbcl-mgl-pax))

(define-public ecl-mgl-pax
  (sbcl-package->ecl-package sbcl-mgl-pax))

(define-public sbcl-mssql
  (let ((commit "045602a19a32254108f2b75871049293f49731eb")
        (revision "1"))
    (package
      (name "sbcl-mssql")
      (version (git-version "0.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/archimag/cl-mssql")
               (commit commit)))
         (file-name (git-file-name "cl-mssql" version))
         (sha256
          (base32 "09i50adppgc1ybm3ka9vbindhwa2x29f9n3n0jkrryymdhb8zknm"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("freetds" ,freetds)
         ("garbage-pools" ,sbcl-garbage-pools)
         ("iterate" ,sbcl-iterate)
         ("parse-number" ,sbcl-parse-number)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/mssql.lisp"
                 (("libsybdb" all)
                  (string-append (assoc-ref inputs "freetds") "/lib/" all)))
               #t)))))
      (home-page "https://github.com/archimag/cl-mssql")
      (synopsis "Common Lisp library to interact with MS SQL Server databases")
      (description
       "@code{cl-mssql} provides an interface to connect to Microsoft SQL
server.  It uses the @code{libsybdb} foreign library provided by the FreeTDS
project.")
      (license license:llgpl))))

(define-public ecl-mssql
  (sbcl-package->ecl-package sbcl-mssql))

(define-public cl-mssql
  (sbcl-package->cl-source-package sbcl-mssql))

(define-public sbcl-anaphora
  (package
    (name "sbcl-anaphora")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tokenrove/anaphora")
             (commit version)))
       (sha256
        (base32
         "19wfrk3asimznkli0x2rfy637hwpdgqyvwj3vhq9x7vjvyf5vv6x"))
       (file-name (git-file-name "anaphora" version))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-rt))
    (synopsis "The anaphoric macro collection from Hell")
    (description
     "Anaphora is the anaphoric macro collection from Hell: it includes many
new fiends in addition to old friends like @command{aif} and
@command{awhen}.")
    (home-page "https://github.com/tokenrove/anaphora")
    (license license:public-domain)))

(define-public cl-anaphora
  (sbcl-package->cl-source-package sbcl-anaphora))

(define-public ecl-anaphora
  (sbcl-package->ecl-package sbcl-anaphora))

(define-public sbcl-let-plus
  (let ((commit "5f14af61d501ecead02ec6b5a5c810efc0c9fdbb"))
    (package
      (name "sbcl-let-plus")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/let-plus")
               (commit commit)))
         (sha256
          (base32
           "0i050ca2iys9f5mb7dgqgqdxfnc3b0rnjdwv95sqd490vkiwrsaj"))
         (file-name (git-file-name "let-plus" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-anaphora))
      (native-inputs
       (list sbcl-lift))
      (synopsis "Destructuring extension of let*")
      (description
       "This library implements the let+ macro, which is a dectructuring
extension of let*.  It features:

@itemize
@item Clean, consistent syntax and small implementation (less than 300 LOC,
not counting tests)
@item Placeholder macros allow editor hints and syntax highlighting
@item @command{&ign} for ignored values (in forms where that makes sense)
@item Very easy to extend
@end itemize\n")
      (home-page "https://github.com/sharplispers/let-plus")
      (license license:boost1.0))))

(define-public cl-let-plus
  (sbcl-package->cl-source-package sbcl-let-plus))

(define-public ecl-let-plus
  (sbcl-package->ecl-package sbcl-let-plus))

(define-public sbcl-cl-colors
  (let ((commit "827410584553f5c717eec6182343b7605f707f75"))
    (package
      (name "sbcl-cl-colors")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tpapp/cl-colors")
               (commit commit)))
         (sha256
          (base32
           "0l446lday4hybsm9bq3jli97fvv8jb1d33abg79vbylpwjmf3y9a"))
         (file-name (git-file-name "cl-colors" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-let-plus))
      (synopsis "Simple color library for Common Lisp")
      (description
       "This is a very simple color library for Common Lisp, providing

@itemize
@item Types for representing colors in HSV and RGB spaces.
@item Simple conversion functions between the above types (and also
hexadecimal representation for RGB).
@item Some predefined colors (currently X11 color names – of course the
library does not depend on X11).Because color in your terminal is nice.
@end itemize

This library is no longer supported by its author.")
      (home-page "https://github.com/tpapp/cl-colors")
      (license license:boost1.0))))

(define-public cl-colors
  (sbcl-package->cl-source-package sbcl-cl-colors))

(define-public ecl-cl-colors
  (sbcl-package->ecl-package sbcl-cl-colors))

(define-public sbcl-cl-ansi-text
  (let ((commit "8b129d83c7511b54cdd9d4123825a2d06349b25c"))
    (package
      (name "sbcl-cl-ansi-text")
      (version (git-version "2.0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pnathan/cl-ansi-text")
               (commit commit)))
         (sha256
          (base32
           "0nk7ajqfa937w1iy3zy86jjbw8yffm05cqs4wxkgl97v6kmmya14"))
         (file-name (git-file-name "cl-ansi-text" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-cl-colors2))
      (native-inputs
       (list sbcl-fiveam))
      (synopsis "ANSI terminal color implementation for Common Lisp")
      (description
       "@command{cl-ansi-text} provides utilities which enable printing to an
ANSI terminal with colored text.  It provides the macro @command{with-color}
which causes everything printed in the body to be displayed with the provided
color.  It further provides functions which will print the argument with the
named color.")
      (home-page "https://github.com/pnathan/cl-ansi-text")
      (license license:llgpl))))

(define-public cl-ansi-text
  (sbcl-package->cl-source-package sbcl-cl-ansi-text))

(define-public ecl-cl-ansi-text
  (sbcl-package->ecl-package sbcl-cl-ansi-text))

(define-public sbcl-proc-parse
  (let ((commit "ac3636834d561bdc2686c956dbd82494537285fd"))
    (package
      (name "sbcl-proc-parse")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/proc-parse")
               (commit commit)))
         (sha256
          (base32
           "06rnl0h4cx6xv2wj3jczmmcxqn2703inmmvg1s4npbghmijsybfh"))
         (file-name (git-file-name "proc-parse" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("babel" ,sbcl-babel)))
      (native-inputs
       (list sbcl-prove))
      (arguments
       ;; TODO: Tests don't find "proc-parse-test", why?
       `(#:tests? #f))
      (synopsis "Procedural vector parser")
      (description
       "This is a string/octets parser library for Common Lisp with speed and
readability in mind.  Unlike other libraries, the code is not a
pattern-matching-like, but a char-by-char procedural parser.")
      (home-page "https://github.com/fukamachi/proc-parse")
      (license license:bsd-2))))

(define-public cl-proc-parse
  (sbcl-package->cl-source-package sbcl-proc-parse))

(define-public ecl-proc-parse
  (sbcl-package->ecl-package sbcl-proc-parse))

(define-public sbcl-parse-float
  (let ((commit "3074765101e41222b6b624a66aaf1e6416379f9c")
        (revision "2"))
    (package
      (name "sbcl-parse-float")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/soemraws/parse-float")
               (commit commit)))
         (sha256
          (base32 "0jd2spawc3v8vzqf8ky4cngl45jm65fhkrdf20mf6dcbn3mzpkmr"))
         (file-name (git-file-name "proc-parse" version))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; FIXME: https://github.com/soemraws/parse-float/issues/12
       `(#:asd-systems '("parse-float" "parse-float-tests")))
      (native-inputs
       (list sbcl-lisp-unit))
      (inputs
       `(("alexandria" ,sbcl-alexandria)))
      (home-page "https://github.com/soemraws/parse-float")
      (synopsis "Parse a floating point value from a string in Common Lisp")
      (description
       "This package exports the following function to parse floating-point
values from a string in Common Lisp.")
      (license license:public-domain))))

(define-public cl-parse-float
  (sbcl-package->cl-source-package sbcl-parse-float))

(define-public ecl-parse-float
  (sbcl-package->ecl-package sbcl-parse-float))

(define-public sbcl-cl-string-match
  (let ((revision "1")
        (changeset "5048480a61243e6f1b02884012c8f25cdbee6d97"))
    (package
      (name "sbcl-cl-string-match")
      (version (git-version "0" revision changeset))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
               (url "https://github.com/vityok/cl-string-match")
               (changeset changeset)))
         (sha256
          (base32
           "01wn5qx562w43ssy92xlfgv79w7p0nv0wbl76mpmba131n9ziq2y"))
         (file-name (git-file-name "cl-string-match" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("babel" ,sbcl-babel)
         ("iterate" ,sbcl-iterate)
         ("jpl-queues" ,sbcl-jpl-queues)
         ("jpl-util" ,sbcl-jpl-util)
         ("mgl-pax" ,sbcl-mgl-pax)
         ("parse-float" ,sbcl-parse-float)
         ("proc-parse" ,sbcl-proc-parse)
         ("yacc" ,sbcl-cl-yacc)))
      ;; TODO: Tests are not evaluated properly.
      (native-inputs
       ;; For testing:
       (list sbcl-lisp-unit))
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-dependency
             (lambda _
               (substitute* "cl-string-match.asd"
                 ((":mgl-pax")
                  ":mgl-pax/document")))))))
      (synopsis "Set of utilities to manipulate strings in Common Lisp")
      (description
       "@command{cl-strings} is a small, portable, dependency-free set of
utilities that make it even easier to manipulate text in Common Lisp.  It has
100% test coverage and works at least on sbcl, ecl, ccl, abcl and clisp.")
      (home-page "https://github.com/vityok/cl-string-match")
      (license license:bsd-3))))

(define-public cl-string-match
  (sbcl-package->cl-source-package sbcl-cl-string-match))

(define-public ecl-cl-string-match
  (sbcl-package->ecl-package sbcl-cl-string-match))

(define-public sbcl-puri
  (let ((commit "4bbab89d9ccbb26346899d1f496c97604fec567b")
        (revision "2"))
    (package
      (name "sbcl-puri")
      (version (git-version "1.5.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/puri.git")
               (commit commit)))
         (file-name (git-file-name "puri" version))
         (sha256
          (base32 "0gq2rsr0aihs0z20v4zqvmdl4szq53b52rh97pvnmwrlbn4mapmd"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-ptester))
      (home-page "http://puri.kpe.io/")
      (synopsis "Portable URI Library")
      (description
       "This is a portable Universal Resource Identifier library for Common
Lisp programs.  It parses URI according to the RFC 2396 specification.")
      (license license:llgpl))))

(define-public cl-puri
  (sbcl-package->cl-source-package sbcl-puri))

(define-public ecl-puri
  (sbcl-package->ecl-package sbcl-puri))

(define-public sbcl-qmynd
  (let ((commit "7e56daf73f0ed5f49a931c01af75fb874bcf3445")
        (revision "1"))
    (package
      (name "sbcl-qmynd")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/qitab/qmynd")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "06gw5wxcpdclb6a5i5k9lbmdlyqsp182czrm9bm1cpklzbj0ihrl"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-asdf-finalizers
             sbcl-babel
             sbcl-chipz
             sbcl-cl+ssl
             sbcl-flexi-streams
             sbcl-ironclad
             sbcl-salza2
             sbcl-trivial-gray-streams
             sbcl-usocket))
      (home-page "https://github.com/qitab/qmynd")
      (synopsis "QITAB MySQL Native Driver for Common Lisp")
      (description "QMyND, the QITAB MySQL Native Driver, is a MySQL client
library that directly talks to a MySQL server in its native network protocol.

It's a part of QITAB umbrella project.")
      (license license:expat))))

(define-public ecl-qmynd
  (sbcl-package->ecl-package sbcl-qmynd))

(define-public cl-qmynd
  (sbcl-package->cl-source-package sbcl-qmynd))

(define-public sbcl-queues
  (let ((commit "47d4da65e9ea20953b74aeeab7e89a831b66bc94"))
    (package
      (name "sbcl-queues")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/oconnore/queues")
               (commit commit)))
         (file-name (git-file-name "queues" version))
         (sha256
          (base32
           "0wdhfnzi4v6d97pggzj2aw55si94w4327br94jrmyvwf351wqjvv"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("bordeaux-threads" ,sbcl-bordeaux-threads)))
      (arguments
       '(#:asd-systems '("queues"
                         "queues.simple-queue"
                         "queues.simple-cqueue"
                         "queues.priority-queue"
                         "queues.priority-cqueue")))
      (home-page "https://github.com/oconnore/queues")
      (synopsis "Common Lisp queue library")
      (description
       "This is a simple queue library for Common Lisp with features such as
non-consing thread safe queues and fibonacci priority queues.")
      (license license:expat))))

(define-public cl-queues
  (sbcl-package->cl-source-package sbcl-queues))

(define-public ecl-queues
  (sbcl-package->ecl-package sbcl-queues))

(define-public sbcl-glsl-packing
  (let ((commit "03628159468a8e5b7f2a1d5e78b77053e136794a")
        (revision "1"))
    (package
      (name "sbcl-glsl-packing")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/3b/glsl-packing/")
               (commit commit)))
         (file-name (git-file-name "glsl-packing" version))
         (sha256
          (base32 "0k2f1771wd9kdrcasldy1r00k5bdgi9fd07in52zmjggc0i7dd80"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria))
      (home-page "https://github.com/3b/glsl-packing/")
      (synopsis "Common Lisp utilities to calculate OpenGL layouts")
      (description
       "This is a Common Lisp library to calculate std140 or std430 layouts for
a glsl UBO/SSBO.")
      (license license:expat))))

(define-public ecl-glsl-packing
  (sbcl-package->ecl-package sbcl-glsl-packing))

(define-public cl-glsl-packing
  (sbcl-package->cl-source-package sbcl-glsl-packing))

(define-public sbcl-glsl-spec
  (let ((commit "f04476f7da89355ae6856b33283c60ba95c6555d")
        (revision "1"))
    (package
      (name "sbcl-glsl-spec")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cbaggers/glsl-spec")
               (commit commit)))
         (file-name (git-file-name "glsl-spec" version))
         (sha256
          (base32 "01ipspr22fgfj3w8wq2y81lzrjc4vpfiwnr3dqhjlpzzra46am8c"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems '("glsl-spec" "glsl-symbols" "glsl-docs")))
      (home-page "https://github.com/cbaggers/glsl-spec")
      (synopsis "Common Lisp GLSL specification as a datastructure")
      (description
       "This package contains the specification of all functions and variables
from GLSL as data.")
      (license license:unlicense))))

(define-public ecl-glsl-spec
  (sbcl-package->ecl-package sbcl-glsl-spec))

(define-public cl-glsl-spec
  (sbcl-package->cl-source-package sbcl-glsl-spec))

(define-public sbcl-rtg-math
  (let ((commit "29fc5b3d0028a4a11a82355ecc8cca62662c69e0")
        (revision "1"))
    (package
      (name "sbcl-rtg-math")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cbaggers/rtg-math")
               (commit commit)))
         (file-name (git-file-name "rtg-math" version))
         (sha256
          (base32 "0bhxxnv7ldkkb18zdxyz2rj2a3iawzq2kcp7cn5i91iby7n0082x"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-documentation-utils sbcl-glsl-spec))
      (home-page "https://github.com/cbaggers/rtg-math")
      (synopsis "Common Lisp library of game-related math functions")
      (description
       "RTG-MATH provides a selection of the math routines most commonly needed
for making realtime graphics in Lisp.")
      (license license:bsd-2))))

(define-public ecl-rtg-math
  (sbcl-package->ecl-package sbcl-rtg-math))

(define-public cl-rtg-math
  (sbcl-package->cl-source-package sbcl-rtg-math))

(define-public sbcl-varjo
  (let ((commit "9e77f30220053155d2ef8870ceba157f75e538d4")
        (revision "1"))
    (package
      (name "sbcl-varjo")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cbaggers/varjo")
               (commit commit)))
         (file-name (git-file-name "varjo" version))
         (sha256
          (base32 "1p9x1wj576x5d31yvls9r1avkjkyhri7kyxbjfkg9z93a1w18j9z"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-alexandria
             sbcl-cl-ppcre
             sbcl-documentation-utils
             sbcl-fn
             sbcl-glsl-spec
             sbcl-named-readtables
             sbcl-parse-float
             sbcl-vas-string-metrics))
      (home-page "https://github.com/cbaggers/varjo")
      (synopsis "Lisp to GLSL Language Translator")
      (description
       "Varjo is a Lisp to GLSL compiler.  Vari is the dialect of lisp Varjo
compiles.  It aims to be as close to Common Lisp as possible, but naturally it
is statically typed so there are differences.")
      (license license:bsd-2))))

(define-public ecl-varjo
  (sbcl-package->ecl-package sbcl-varjo))

(define-public cl-varjo
  (sbcl-package->cl-source-package sbcl-varjo))

(define-public sbcl-cffi
  (package
    (name "sbcl-cffi")
    (version "0.24.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cffi/cffi")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cffi-bootstrap" version))
       (sha256
        (base32 "17ryim4xilb1rzxydfr7595dnhqkk02lmrbkqrkvi9091shi4cj3"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("babel" ,sbcl-babel)
       ("libffi" ,libffi)
       ("trivial-features" ,sbcl-trivial-features)))
    (native-inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("pkg-config" ,pkg-config)
       ("rt" ,sbcl-rt)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-arm-support
           (lambda _
             ;; This is apparently deprecated since libffi-3.3.
             (substitute* "libffi/libffi-types.lisp"
               (("\\\(\\\(:unix64.*") ")\n"))
             #t))
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libffi/libffi.lisp"
               (("libffi.so.7" all) (string-append
                                     (assoc-ref inputs "libffi")
                                     "/lib/" all)))
             (substitute* "toolchain/c-toolchain.lisp"
               (("\"cc\"") (format #f "~S" (which "gcc"))))))
         (add-after 'build 'install-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "grovel/common.h"
                           (string-append
                            (assoc-ref outputs "out")
                            "/include/grovel")))))
       #:asd-files '("cffi.asd"
                     "cffi-toolchain.asd"
                     "cffi-grovel.asd"
                     "cffi-libffi.asd"
                     "cffi-uffi-compat.asd")
       #:asd-systems '("cffi"
                       "cffi-libffi"
                       "cffi-uffi-compat")))
    (home-page "https://common-lisp.net/project/cffi/")
    (synopsis "Common Foreign Function Interface for Common Lisp")
    (description "The Common Foreign Function Interface (CFFI)
purports to be a portable foreign function interface for Common Lisp.
The CFFI library is composed of a Lisp-implementation-specific backend
in the CFFI-SYS package, and a portable frontend in the CFFI
package.")
    (license license:expat)))

(define-public cl-cffi
  (sbcl-package->cl-source-package sbcl-cffi))

(define-public ecl-cffi
  (sbcl-package->ecl-package sbcl-cffi))

(define-public sbcl-cffi-c-ref
  (let ((commit "8123cbb6034c5f7921a0766107cfb8c4e8efd5ce")
        (revision "0"))
    (package
      (name "sbcl-cffi-c-ref")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/borodust/cffi-c-ref")
               (commit commit)))
         (sha256
          (base32 "1a3pp6xcisabqir3rp1gvvjfdxcvpm8yr35p38nri9azsinmmc7z"))
         (file-name (git-file-name "cffi-c-ref" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-cffi))
      (synopsis "Streamlined access to foreign memory")
      (description
       "This Common Lisp library provides macros to access foreign memory.")
      (home-page "https://github.com/borodust/cffi-c-ref")
      (license license:expat))))

(define-public cl-cffi-c-ref
  (sbcl-package->cl-source-package sbcl-cffi-c-ref))

(define-public ecl-cffi-c-ref
  (sbcl-package->ecl-package sbcl-cffi-c-ref))

(define-public sbcl-cl-sqlite
  (package
    (name "sbcl-cl-sqlite")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmitryvk/cl-sqlite")
             (commit version)))
       (file-name (git-file-name "cl-sqlite" version))
       (sha256
        (base32
         "08iv7b4m0hh7qx2cvq4f510nrgdld0vicnvmqsh9w0fgrcgmyg4k"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("iterate" ,sbcl-iterate)
       ("cffi" ,sbcl-cffi)
       ("sqlite" ,sqlite)))
    (native-inputs
     `(("fiveam" ,sbcl-fiveam)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)))
    (arguments
     `(#:asd-systems '("sqlite")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "sqlite-ffi.lisp"
               (("libsqlite3" all) (string-append
                                    (assoc-ref inputs "sqlite")"/lib/" all))))))))
    (home-page "https://common-lisp.net/project/cl-sqlite/")
    (synopsis "Common Lisp binding for SQLite")
    (description
     "The @command{cl-sqlite} package is an interface to the SQLite embedded
relational database engine.")
    (license license:public-domain)))

(define-public cl-sqlite
  (sbcl-package->cl-source-package sbcl-cl-sqlite))

(define-public ecl-cl-sqlite
  (sbcl-package->ecl-package sbcl-cl-sqlite))

(define-public sbcl-cl-redis
  (let ((commit "7d592417421cf7cd1cffa96043b457af0490df7d")
        (revision "0"))
    (package
      (name "sbcl-cl-redis")
      (version (git-version "2.3.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/vseloved/cl-redis")
               (commit commit)))
         (file-name (git-file-name "cl-redis" version))
         (sha256
          (base32 "0x5ahxb5cx37biyn3cjycshhm1rr9p5cf1a9l5hd1n1xjxm2f8vi"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-before 'check 'start-redis
             (lambda _
               (system "redis-server --port 6379 &"))))))
      (native-inputs
       `(("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("flexi-streams" ,sbcl-flexi-streams)
         ("redis" ,redis)
         ("should-test" ,sbcl-should-test)))
      (inputs
       `(("babel" ,sbcl-babel)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("flexi-streams" ,sbcl-flexi-streams)
         ("rutils" ,sbcl-rutils)
         ("usocket" ,sbcl-usocket)))
      (home-page "https://github.com/vseloved/cl-redis")
      (synopsis "Common Lisp client for Redis")
      (description "This is a Common Lisp wrapper for interacting with the
Redis data structure store.")
      (license license:expat))))

(define-public cl-redis
  (sbcl-package->cl-source-package sbcl-cl-redis))

(define-public ecl-cl-redis
  (let ((pkg (sbcl-package->ecl-package sbcl-cl-redis)))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ;; Tests are failing on ECL with:
         ;;   Test L-COMMANDS: An error occurred during initialization:
         ;;   Protocol not found: "tcp".
         ((#:tests? _ #f) #f))))))

(define-public sbcl-parenscript
  ;; Source archives are overwritten on every release, we use the Git repo instead.
  (let ((commit "7a1ac46353cecd144fc91915ba9f122aafcf4766"))
    (package
      (name "sbcl-parenscript")
      (version (git-version "2.7.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/parenscript/parenscript")
               (commit commit)))
         (file-name (git-file-name "parenscript" version))
         (sha256
          (base32
           "0c22lqarrpbq82dg1sb3y6mp6w2faczp34ymzhnmff88yfq1xzsf"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre sbcl-anaphora sbcl-named-readtables))
      (home-page "https://common-lisp.net/project/parenscript/")
      (synopsis "Translator from a subset of Common Lisp to JavaScript")
      (description
       "Parenscript is a translator from an extended subset of Common Lisp to
JavaScript.  Parenscript code can run almost identically on both the
browser (as JavaScript) and server (as Common Lisp).

Parenscript code is treated the same way as Common Lisp code, making the full
power of Lisp macros available for JavaScript.  This provides a web
development environment that is unmatched in its ability to reduce code
duplication and provide advanced meta-programming facilities to web
developers.

At the same time, Parenscript is different from almost all other \"language
X\" to JavaScript translators in that it imposes almost no overhead:

@itemize
@item No run-time dependencies: Any piece of Parenscript code is runnable
as-is.  There are no JavaScript files to include.
@item Native types: Parenscript works entirely with native JavaScript data
types.  There are no new types introduced, and object prototypes are not
touched.
@item Native calling convention: Any JavaScript code can be called without the
need for bindings.  Likewise, Parenscript can be used to make efficient,
self-contained JavaScript libraries.
@item Readable code: Parenscript generates concise, formatted, idiomatic
JavaScript code.  Identifier names are preserved.  This enables seamless
debugging in tools like Firebug.
@item Efficiency: Parenscript introduces minimal overhead for advanced Common
Lisp features.  The generated code is almost as fast as hand-written
JavaScript.
@end itemize\n")
      (license license:bsd-3))))

(define-public cl-parenscript
  (sbcl-package->cl-source-package sbcl-parenscript))

(define-public ecl-parenscript
  (sbcl-package->ecl-package sbcl-parenscript))

(define-public sbcl-cl-json
  (let ((commit "6dfebb9540bfc3cc33582d0c03c9ec27cb913e79"))
    (package
      (name "sbcl-cl-json")
      (version (git-version "0.5" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hankhero/cl-json")
               (commit commit)))
         (file-name (git-file-name "cl-json" version))
         (sha256
          (base32
           "0fx3m3x3s5ji950yzpazz4s0img3l6b3d6l3jrfjv0lr702496lh"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/hankhero/cl-json")
      (synopsis "JSON encoder and decoder for Common-Lisp")
      (description
       "@command{cl-json} provides an encoder of Lisp objects to JSON format
and a corresponding decoder of JSON data to Lisp objects.  Both the encoder
and the decoder are highly customizable; at the same time, the default
settings ensure a very simple mode of operation, similar to that provided by
@command{yason} or @command{st-json}.")
      (license license:expat))))

(define-public cl-json
  (sbcl-package->cl-source-package sbcl-cl-json))

(define-public ecl-cl-json
  (sbcl-package->ecl-package sbcl-cl-json))

(define-public sbcl-unix-opts
  (let ((commit "0e61f34b2ecf62288437810d4abb31e572048b04")
        (revision "1"))
    (package
      (name "sbcl-unix-opts")
      (version (git-version "0.1.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libre-man/unix-opts")
               (commit commit)))
         (file-name (git-file-name "unix-opts" version))
         (sha256
          (base32 "16mcqpzwrz808p9n3wwl99ckg3hg7yihw08y1i4l7c92aldbkasq"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/hankhero/cl-json")
      (synopsis "Unix-style command line options parser")
      (description
       "This is a minimalistic parser of command line options.  The main
advantage of the library is the ability to concisely define command line
options once and then use this definition for parsing and extraction of
command line arguments, as well as printing description of command line
options (you get --help for free).  This way you don't need to repeat
yourself.  Also, @command{unix-opts} doesn't depend on anything and
precisely controls the behavior of the parser via Common Lisp restarts.")
      (license license:expat))))

(define-public cl-unix-opts
  (sbcl-package->cl-source-package sbcl-unix-opts))

(define-public ecl-unix-opts
  (sbcl-package->ecl-package sbcl-unix-opts))

(define-public sbcl-trivial-garbage
  (package
    (name "sbcl-trivial-garbage")
    (version "0.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trivial-garbage/trivial-garbage")
             (commit (string-append "v" version))))
       (file-name (git-file-name "trivial-garbage" version))
       (sha256
        (base32 "0122jicfg7pca1wxw8zak1n92h5friqy60988ns0ysksj3fphw9n"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-rt))
    (home-page "https://common-lisp.net/project/trivial-garbage/")
    (synopsis "Portable GC-related APIs for Common Lisp")
    (description "@command{trivial-garbage} provides a portable API to
finalizers, weak hash-tables and weak pointers on all major implementations of
the Common Lisp programming language.")
    (license license:public-domain)))

(define-public cl-trivial-garbage
  (sbcl-package->cl-source-package sbcl-trivial-garbage))

(define-public ecl-trivial-garbage
  (sbcl-package->ecl-package sbcl-trivial-garbage))

(define-public sbcl-closer-mop
  (let ((commit "19c9d33f576e10715fd79cc1d4f688dab0f241d6"))
    (package
      (name "sbcl-closer-mop")
      (version (git-version  "1.0.0" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pcostanza/closer-mop")
               (commit commit)))
         (sha256
          (base32 "1w3x087wvlwkd6swfdgbvjfs6kazf0la8ax4pjfzikwjch4snn2c"))
         (file-name (git-file-name "closer-mop" version ))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/pcostanza/closer-mop")
      (synopsis "Rectifies absent or incorrect CLOS MOP features")
      (description "Closer to MOP is a compatibility layer that rectifies many
of the absent or incorrect CLOS MOP features across a broad range of Common
Lisp implementations.")
      (license license:expat))))

(define-public cl-closer-mop
  (sbcl-package->cl-source-package sbcl-closer-mop))

(define-public ecl-closer-mop
  (sbcl-package->ecl-package sbcl-closer-mop))

(define-public sbcl-cl-cffi-gtk
  (let ((commit "e9a46df65995d9a16e6c8dbdc1e09b775eb4a966"))
    (package
      (name "sbcl-cl-cffi-gtk")
      (version (git-version "0.11.2" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Ferada/cl-cffi-gtk/")
               (commit commit)))
         (file-name (git-file-name "cl-cffi-gtk" version))
         (sha256
          (base32
           "04vix0gmqsj91lm975sx7jhlnz5gq1xf9jp873mp7c8frc5dk1jj"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       `(("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cairo" ,cairo)
         ("cffi" ,sbcl-cffi)
         ("closer-mop" ,sbcl-closer-mop)
         ("gdk-pixbuf" ,gdk-pixbuf)
         ("glib" ,glib)
         ("gtk" ,gtk+)
         ("iterate" ,sbcl-iterate)
         ("pango" ,pango)
         ("trivial-features" ,sbcl-trivial-features)
         ("trivial-garbage" ,sbcl-trivial-garbage)))
      (arguments
       `(#:asd-files '("gtk/cl-cffi-gtk.asd"
                       "glib/cl-cffi-gtk-glib.asd"
                       "gobject/cl-cffi-gtk-gobject.asd"
                       "gio/cl-cffi-gtk-gio.asd"
                       "cairo/cl-cffi-gtk-cairo.asd"
                       "pango/cl-cffi-gtk-pango.asd"
                       "gdk-pixbuf/cl-cffi-gtk-gdk-pixbuf.asd"
                       "gdk/cl-cffi-gtk-gdk.asd")
         #:test-asd-file "test/cl-cffi-gtk-test.asd"
         ;; TODO: Tests fail with memory fault.
         ;; See https://github.com/Ferada/cl-cffi-gtk/issues/24.
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "glib/glib.init.lisp"
                 (("libglib|libgthread" all)
                  (string-append (assoc-ref inputs "glib") "/lib/" all)))
               (substitute* "gobject/gobject.init.lisp"
                 (("libgobject" all)
                  (string-append (assoc-ref inputs "glib") "/lib/" all)))
               (substitute* "gio/gio.init.lisp"
                 (("libgio" all)
                  (string-append (assoc-ref inputs "glib") "/lib/" all)))
               (substitute* "cairo/cairo.init.lisp"
                 (("libcairo" all)
                  (string-append (assoc-ref inputs "cairo") "/lib/" all)))
               (substitute* "pango/pango.init.lisp"
                 (("libpango" all)
                  (string-append (assoc-ref inputs "pango") "/lib/" all)))
               (substitute* "gdk-pixbuf/gdk-pixbuf.init.lisp"
                 (("libgdk_pixbuf" all)
                  (string-append (assoc-ref inputs "gdk-pixbuf") "/lib/" all)))
               (substitute* "gdk/gdk.init.lisp"
                 (("libgdk" all)
                  (string-append (assoc-ref inputs "gtk") "/lib/" all)))
               (substitute* "gdk/gdk.package.lisp"
                 (("libgtk" all)
                  (string-append (assoc-ref inputs "gtk") "/lib/" all))))))))
      (home-page "https://github.com/Ferada/cl-cffi-gtk/")
      (synopsis "Common Lisp binding for GTK+3")
      (description
       "@command{cl-cffi-gtk} is a Lisp binding to GTK+ 3 (GIMP Toolkit) which
is a library for creating graphical user interfaces.")
      (license license:lgpl3))))

(define-public cl-cffi-gtk
  (sbcl-package->cl-source-package sbcl-cl-cffi-gtk))

(define-public ecl-cl-cffi-gtk
  (sbcl-package->ecl-package sbcl-cl-cffi-gtk))

(define-public sbcl-cl-webkit
  (package
    (name "sbcl-cl-webkit")
    (version "3.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joachifm/cl-webkit")
             (commit version)))
       (file-name (git-file-name "cl-webkit" version))
       (sha256
        (base32
         "1a16dka15lqzpli0f0qd3afmi14vgdxnfkn9z9d1r4cw9p11s71l"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("cffi" ,sbcl-cffi)
       ("cl-cffi-gtk" ,sbcl-cl-cffi-gtk)
       ("webkitgtk" ,webkitgtk)))
    (native-inputs
     `(;; Tests seem to need Xorg.
       ;; ("xorg-server" ,xorg-server-for-tests)
       ("calispel" ,sbcl-calispel)
       ("fiveam" ,sbcl-fiveam)
       ("float-features" ,sbcl-float-features)))
    (arguments
     `(#:asd-systems '("cl-webkit2")
       #:tests? #f                      ; TODO: Tests hang, why?
       #:phases
       (modify-phases %standard-phases
         ;; The following phase is needed for tests:
         ;; (add-before 'check 'start-xorg-server
         ;;   (lambda* (#:key inputs #:allow-other-keys)
         ;;     ;; The test suite requires a running X server.
         ;;     (system (string-append (assoc-ref inputs "xorg-server")
         ;;                            "/bin/Xvfb :1 &"))
         ;;     (setenv "DISPLAY" ":1")
         ;;     #t))
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "webkit2/webkit2.init.lisp"
               (("libwebkit2gtk" all)
                (string-append
                 (assoc-ref inputs "webkitgtk") "/lib/" all))))))))
    (home-page "https://github.com/joachifm/cl-webkit")
    (synopsis "Binding to WebKitGTK+ for Common Lisp")
    (description
     "@command{cl-webkit} is a binding to WebKitGTK+ for Common Lisp,
currently targeting WebKit version 2.  The WebKitGTK+ library adds web
browsing capabilities to an application, leveraging the full power of the
WebKit browsing engine.")
    (license license:expat)))

(define-public cl-webkit
  (sbcl-package->cl-source-package sbcl-cl-webkit))

(define-public ecl-cl-webkit
  (sbcl-package->ecl-package sbcl-cl-webkit))

(define-public sbcl-lparallel
  (package
    (name "sbcl-lparallel")
    (version "2.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lmj/lparallel/")
             (commit (string-append "lparallel-" version))))
       (file-name (git-file-name "lparallel" version))
       (sha256
        (base32
         "0g0aylrbbrqsz0ahmwhvnk4cmc2931fllbpcfgzsprwnqqd7vwq9"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("trivial-garbage" ,sbcl-trivial-garbage)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-dependency
           ;; lparallel loads a SBCL specific system in its asd file. This is
           ;; not carried over into the fasl which is generated. In order for
           ;; it to be carried over, it needs to be listed as a dependency.
           (lambda _
             (substitute* "lparallel.asd"
               ((":depends-on \\(:alexandria" all)
                (string-append all " #+sbcl :sb-cltl2"))))))))
    (home-page "https://lparallel.org/")
    (synopsis "Parallelism for Common Lisp")
    (description
     "@command{lparallel} is a library for parallel programming in Common
Lisp, featuring:

@itemize
@item a simple model of task submission with receiving queue,
@item constructs for expressing fine-grained parallelism,
@item asynchronous condition handling across thread boundaries,
@item parallel versions of map, reduce, sort, remove, and many others,
@item promises, futures, and delayed evaluation constructs,
@item computation trees for parallelizing interconnected tasks,
@item bounded and unbounded FIFO queues,
@item high and low priority tasks,
@item task killing by category,
@item integrated timeouts.
@end itemize\n")
    (license license:expat)))

(define-public cl-lparallel
  (sbcl-package->cl-source-package sbcl-lparallel))

(define-public ecl-lparallel
  (package
    (inherit (sbcl-package->ecl-package sbcl-lparallel))
    (arguments
     ;; TODO: Find why the tests get stuck forever; disable them for now.
     `(#:tests? #f))))

(define-public sbcl-cl-markup
  (let ((commit "e0eb7debf4bdff98d1f49d0f811321a6a637b390"))
    (package
      (name "sbcl-cl-markup")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/arielnetworks/cl-markup/")
               (commit commit)))
         (file-name (git-file-name "cl-markup" version))
         (sha256
          (base32
           "10l6k45971dl13fkdmva7zc6i453lmq9j4xax2ci6pjzlc6xjhp7"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/arielnetworks/cl-markup/")
      (synopsis "Markup generation library for Common Lisp")
      (description
       "A modern markup generation library for Common Lisp that features:

@itemize
@item Fast (even faster through compiling the code)
@item Safety
@item Support for multiple document types (markup, xml, html, html5, xhtml)
@item Output with doctype
@item Direct output to stream
@end itemize\n")
      (license license:lgpl3+))))

(define-public cl-markup
  (sbcl-package->cl-source-package sbcl-cl-markup))

(define-public ecl-cl-markup
  (sbcl-package->ecl-package sbcl-cl-markup))

;;; The following package is renamed from "markup" to "markup-reader" in order
;;; not to conflict with the "cl-markup" package.
(define-public sbcl-markup-reader
  (let ((commit "d2d4d7b073554f47c24223a9304452966608702e")
        (revision "1"))
    (package
      (name "sbcl-markup-reader")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/moderninterpreters/markup")
               (commit commit)))
         (file-name (git-file-name "markup-reader" version))
         (sha256
          (base32 "0i3v938j8zpzkd6p9j8gadp5zndjcdxhswj1qgsp592v6497rpzj"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:asd-systems '("markup")))
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-str" ,sbcl-cl-str)
         ("named-readtables" ,sbcl-named-readtables)
         ("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
      (home-page "https://github.com/moderninterpreters/markup")
      (synopsis "Reader-macro to read HTML tags inside of Common Lisp code")
      (description
       "Markup allows the use of HTML syntax with in Common Lisp code.
This has the advantage of being able to copy HTML snippets and have them
instantly be functional, less double quotes than a s-expression approach,
and designers will be able to understand the embedded HTML.")
      (license license:asl2.0))))

(define-public ecl-markup-reader
  (sbcl-package->ecl-package sbcl-markup-reader))

(define-public cl-markup-reader
  (sbcl-package->cl-source-package sbcl-markup-reader))

(define-public sbcl-cl-mustache
  (package
    (name "sbcl-cl-mustache")
    (version "0.12.1")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/kanru/cl-mustache")
            (commit (string-append "v" version))))
      (file-name (git-file-name "cl-mustache" version))
      (sha256
       (base32 "149xbb6wxq1napliwm9cv729hwcgfnjli6y8hingfixz7f10lhks"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/kanru/cl-mustache")
    (synopsis "Common Lisp Mustache template renderer")
    (description "This is a Common Lisp implementation for the Mustache
template system.  More details on the standard are available at
@url{https://mustache.github.io}.")
    (license license:expat)))

(define-public cl-mustache
  (sbcl-package->cl-source-package sbcl-cl-mustache))

(define-public ecl-cl-mustache
  (sbcl-package->ecl-package sbcl-cl-mustache))

(define-public sbcl-cl-css
  (let ((commit "8fe654c8f0cf95b300718101cce4feb517f78e2f"))
    (package
      (name "sbcl-cl-css")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/inaimathi/cl-css/")
               (commit commit)))
         (file-name (git-file-name "cl-css" version))
         (sha256
          (base32
           "1lc42zi2sw11fl2589sc19nr5sd2p0wy7wgvgwaggxa5f3ajhsmd"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/inaimathi/cl-css/")
      (synopsis "Non-validating, inline CSS generator for Common Lisp")
      (description
       "This is a dead-simple, non validating, inline CSS generator for Common
Lisp.  Its goals are axiomatic syntax, simple implementation to support
portability, and boilerplate reduction in CSS.")
      (license license:expat))))

(define-public cl-css
  (sbcl-package->cl-source-package sbcl-cl-css))

(define-public ecl-cl-css
  (sbcl-package->ecl-package sbcl-cl-css))

(define-public sbcl-portable-threads
  (let ((commit "aa26bf38338a6b068bf8bfb3375d8d8c3b0a28df"))
    (package
      (name "sbcl-portable-threads")
      (version (git-version "2.3" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/binghe/portable-threads/")
               (commit commit)))
         (file-name (git-file-name "portable-threads" version))
         (sha256
          (base32 "058ksi07vfdmhrf5mdlc833s82m1rcqfja2266520m3r8bzs8bvs"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(;; Tests seem broken.
         #:tests? #f))
      (home-page "https://github.com/binghe/portable-threads")
      (synopsis "Portable threads API for Common Lisp")
      (description
       "Portable Threads (and Scheduled and Periodic Functions) API for Common
Lisp (from GBBopen project).")
      (license license:asl2.0))))

(define-public cl-portable-threads
  (sbcl-package->cl-source-package sbcl-portable-threads))

(define-public ecl-portable-threads
  (sbcl-package->ecl-package sbcl-portable-threads))

(define-public sbcl-usocket
  (package
    (name "sbcl-usocket")
    (version "0.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/usocket/usocket/")
             (commit (string-append "v" version))))
       (file-name (git-file-name "usocket" version))
       (sha256
        (base32
         "0x746wr2324l6bn7skqzgkzcbj5kd0zp2ck0c8rldrw0rzabg826"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-rt))
    (inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("split-sequence" ,sbcl-split-sequence)))
    (arguments
     `(#:tests? #f ; FIXME: Tests need network access?
       #:asd-systems '("usocket"
                       "usocket-server")))
    (home-page "https://common-lisp.net/project/usocket/")
    (synopsis "Universal socket library for Common Lisp")
    (description
     "This library strives to provide a portable TCP/IP and UDP/IP socket
interface for as many Common Lisp implementations as possible, while keeping
the abstraction and portability layer as thin as possible.")
    (license license:expat)))

(define-public cl-usocket
  (sbcl-package->cl-source-package sbcl-usocket))

(define-public ecl-usocket
  (sbcl-package->ecl-package sbcl-usocket))

(define-public sbcl-s-xml
  (package
    (name "sbcl-s-xml")
    (version "3")
    (source
     (origin
       (method url-fetch)
       (uri "https://common-lisp.net/project/s-xml/s-xml.tgz")
       (sha256
        (base32
         "061qcr0dzshsa38s5ma4ay924cwak2nq9gy59dw6v9p0qb58nzjf"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://common-lisp.net/project/s-xml/")
    (synopsis "Simple XML parser implemented in Common Lisp")
    (description
     "S-XML is a simple XML parser implemented in Common Lisp.  This XML
parser implementation has the following features:

@itemize
@item It works (handling many common XML usages).
@item It is very small (the core is about 700 lines of code, including
comments and whitespace).
@item It has a core API that is simple, efficient and pure functional, much
like that from SSAX (see also http://ssax.sourceforge.net).
@item It supports different DOM models: an XSML-based one, an LXML-based one
and a classic xml-element struct based one.
@item It is reasonably time and space efficient (internally avoiding garbage
generatation as much as possible).
@item It does support CDATA.
@item It should support the same character sets as your Common Lisp
implementation.
@item It does support XML name spaces.
@end itemize

This XML parser implementation has the following limitations:

@itemize
@item It does not support any special tags (like processing instructions).
@item It is not validating, even skips DTD's all together.
@end itemize\n")
    (license license:lgpl3+)))

(define-public cl-s-xml
  (sbcl-package->cl-source-package sbcl-s-xml))

(define-public ecl-s-xml
  (sbcl-package->ecl-package sbcl-s-xml))

(define-public sbcl-s-xml-rpc
  (package
    (name "sbcl-s-xml-rpc")
    (version "7")
    (source
     (origin
       (method url-fetch)
       (uri "https://common-lisp.net/project/s-xml-rpc/s-xml-rpc.tgz")
       (sha256
        (base32
         "02z7k163d51v0pzk8mn1xb6h5s6x64gjqkslhwm3a5x26k2gfs11"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-s-xml))
    (home-page "https://common-lisp.net/project/s-xml-rpc/")
    (synopsis "Implementation of XML-RPC in Common Lisp for both client and server")
    (description
     "S-XML-RPC is an implementation of XML-RPC in Common Lisp for both
client and server.")
    (license license:lgpl3+)))

(define-public cl-s-xml-rpc
  (sbcl-package->cl-source-package sbcl-s-xml-rpc))

(define-public ecl-s-xml-rpc
  (sbcl-package->ecl-package sbcl-s-xml-rpc))

(define-public sbcl-trivial-arguments
  (let ((commit "ecd84ed9cf9ef8f1e873d7409e6bd04979372aa7")
        (revision "1"))
    (package
      (name "sbcl-trivial-arguments")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/trivial-arguments")
               (commit commit)))
         (file-name (git-file-name "trivial-arguments" version))
         (sha256
          (base32 "02vaqfavhj8jqxnr68nnzvzshm8jbgcy6m9lvyv4daa6f7ihqf88"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/Shinmera/trivial-arguments")
      (synopsis "Common Lisp library to retrieve a function's lambda-list")
      (description
       "This is a simple library to retrieve the argument list of a function.")
      (license license:zlib))))

(define-public ecl-trivial-arguments
  (sbcl-package->ecl-package sbcl-trivial-arguments))

(define-public cl-trivial-arguments
  (sbcl-package->cl-source-package sbcl-trivial-arguments))

(define-public sbcl-trivial-clipboard
  (let ((commit "8a580cb97196be7cf096548eb1f46794cd22bb39"))
    (package
      (name "sbcl-trivial-clipboard")
      (version (git-version "0.0.0.0" "4" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/snmsts/trivial-clipboard")
               (commit commit)))
         (file-name (git-file-name "trivial-clipboard" version))
         (sha256
          (base32
           "0apkgqrscylw3hhm5x2vs0z3hz6h7zd7dl5y3wr2zl8qjpvpc80k"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list xclip))
      (native-inputs
       (list sbcl-fiveam))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/text.lisp"
                 (("\"xclip\"")
                  (string-append "\"" (assoc-ref inputs "xclip") "/bin/xclip\""))))))))
      (home-page "https://github.com/snmsts/trivial-clipboard")
      (synopsis "Access system clipboard in Common Lisp")
      (description
       "@command{trivial-clipboard} gives access to the system clipboard.")
      (license license:expat))))

(define-public cl-trivial-clipboard
  (sbcl-package->cl-source-package sbcl-trivial-clipboard))

(define-public ecl-trivial-clipboard
  (sbcl-package->ecl-package sbcl-trivial-clipboard))

(define-public sbcl-trivial-backtrace
  (let ((commit "6eb65bde7229413040c81d42ea22f0e4c9c8cfc9")
        (revision "1"))
    (package
     (name "sbcl-trivial-backtrace")
     (version (git-version "1.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gwkkwg/trivial-backtrace")
             (commit commit)))
       (file-name (git-file-name "trivial-backtrace" version))
       (sha256
        (base32 "1mbaqiwj5034iw6jzw30jyhwzp1pvhnz1zcy0lns0z5j2h9ldapw"))))
     (build-system asdf-build-system/sbcl)
     (native-inputs
      (list sbcl-lift))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'check 'delete-test-results
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((test-results (string-append (assoc-ref outputs "out")
                                                 "/share/common-lisp/"
                                                 (%lisp-type)
                                                 "/trivial-backtrace"
                                                 "/test-results")))
                (when (file-exists? test-results)
                  (delete-file-recursively test-results)))
              #t)))))
     (home-page "https://common-lisp.net/project/trivial-backtrace/")
     (synopsis "Portable simple API to work with backtraces in Common Lisp")
     (description
      "One of the many things that didn't quite get into the Common Lisp
standard was how to get a Lisp to output its call stack when something has
gone wrong.  As such, each Lisp has developed its own notion of what to
display, how to display it, and what sort of arguments can be used to
customize it.  @code{trivial-backtrace} is a simple solution to generating a
backtrace portably.")
     (license license:expat))))

(define-public cl-trivial-backtrace
  (sbcl-package->cl-source-package sbcl-trivial-backtrace))

(define-public ecl-trivial-backtrace
  (sbcl-package->ecl-package sbcl-trivial-backtrace))

(define-public sbcl-rfc2388
  (let ((commit "591bcf7e77f2c222c43953a80f8c297751dc0c4e")
        (revision "1"))
    (package
     (name "sbcl-rfc2388")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jdz/rfc2388")
             (commit commit)))
       (file-name (git-file-name "rfc2388" version))
       (sha256
        (base32 "0phh5n3clhl9ji8jaxrajidn22d3f0aq87mlbfkkxlnx2pnw694k"))))
     (build-system asdf-build-system/sbcl)
     (home-page "https://github.com/jdz/rfc2388/")
     (synopsis "Implementation of RFC 2388 in Common Lisp")
     (description
      "This package contains an implementation of RFC 2388, which is used to
process form data posted with HTTP POST method using enctype
\"multipart/form-data\".")
     (license license:bsd-2))))

(define-public cl-rfc2388
  (sbcl-package->cl-source-package sbcl-rfc2388))

(define-public ecl-rfc2388
  (sbcl-package->ecl-package sbcl-rfc2388))

(define-public sbcl-md5
  (package
    (name "sbcl-md5")
    (version "2.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pmai/md5")
              (commit (string-append "release-" version))))
       (file-name (git-file-name "md5" version))
       (sha256
        (base32 "1waqxzm7vlc22n92hv8r27anlvvjkkh9slhrky1ww7mdx4mmxwb8"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/pmai/md5")
    (synopsis
     "Common Lisp implementation of the MD5 Message-Digest Algorithm (RFC 1321)")
    (description
     "This package implements The MD5 Message-Digest Algorithm, as defined in
RFC 1321 by R. Rivest, published April 1992.")
    (license license:public-domain)))

(define-public cl-md5
  (sbcl-package->cl-source-package sbcl-md5))

(define-public ecl-md5
  (package
    (inherit (sbcl-package->ecl-package sbcl-md5))
    (inputs
     (list ecl-flexi-streams))))

(define-public sbcl-cl+ssl
  (let ((commit "09e896b04c112e7eb0f9d443a5801d557fbcd3ea")
        (revision "2"))
    (package
      (name "sbcl-cl+ssl")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cl-plus-ssl/cl-plus-ssl")
               (commit commit)))
         (file-name (git-file-name "cl+ssl" version))
         (sha256
          (base32 "1ynvk8rbd5zvbdrl8mr49jwmg9fh94clzkagkza9jmpj0p1qvynd"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/reload.lisp"
                 (("libssl.so" all)
                  (string-append
                   (assoc-ref inputs "openssl") "/lib/" all))
                 (("libcrypto.so" all)
                  (string-append
                   (assoc-ref inputs "openssl") "/lib/" all))))))))
      (inputs
       (list openssl
             sbcl-cffi
             sbcl-trivial-gray-streams
             sbcl-flexi-streams
             sbcl-bordeaux-threads
             sbcl-trivial-garbage
             sbcl-alexandria
             sbcl-trivial-features
             sbcl-usocket))
      (home-page "https://common-lisp.net/project/cl-plus-ssl/")
      (synopsis "Common Lisp bindings to OpenSSL")
      (description
       "This library is a fork of SSL-CMUCL.  The original SSL-CMUCL source
code was written by Eric Marsden and includes contributions by Jochen Schmidt.
Development into CL+SSL was done by David Lichteblau.")
      (license license:expat))))

(define-public cl-cl+ssl
  (sbcl-package->cl-source-package sbcl-cl+ssl))

(define-public ecl-cl+ssl
  (sbcl-package->ecl-package sbcl-cl+ssl))

(define-public sbcl-kmrcl
  (let ((version "1.111")
        (commit "4a27407aad9deb607ffb8847630cde3d041ea25a")
        (revision "1"))
    (package
      (name "sbcl-kmrcl")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/kmrcl.git/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "06gx04mah5nc8w78s0j8628divbf1s5w7af8w7pvzb2d5mgvrbd2"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-rt))
      (home-page "http://files.kpe.io/kmrcl/")
      (synopsis "General utilities for Common Lisp programs")
      (description
       "KMRCL is a collection of utilities used by a number of Kevin
Rosenberg's Common Lisp packages.")
      (license license:llgpl))))

(define-public cl-kmrcl
  (sbcl-package->cl-source-package sbcl-kmrcl))

(define-public ecl-kmrcl
  (sbcl-package->ecl-package sbcl-kmrcl))

(define-public sbcl-cl-base64
  ;; 3.3.4 tests are broken, upstream fixes them.
  (let ((commit "577683b18fd880b82274d99fc96a18a710e3987a"))
    (package
      (name "sbcl-cl-base64")
      (version (git-version "3.3.4" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/cl-base64.git/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12jj54h0fs6n237cvnp8v6hn0imfksammq22ys6pi0gwz2w47rbj"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs                    ; For tests.
       (list sbcl-ptester sbcl-kmrcl))
      (home-page "http://files.kpe.io/cl-base64/")
      (synopsis
       "Common Lisp package to encode and decode base64 with URI support")
      (description
       "This package provides highly optimized base64 encoding and decoding.
Besides conversion to and from strings, integer conversions are supported.
Encoding with Uniform Resource Identifiers is supported by using a modified
encoding table that uses only URI-compatible characters.")
      (license license:bsd-3))))

(define-public cl-base64
  (sbcl-package->cl-source-package sbcl-cl-base64))

(define-public ecl-cl-base64
  (sbcl-package->ecl-package sbcl-cl-base64))

(define-public sbcl-chunga
  (package
    (name "sbcl-chunga")
    (version "1.1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/edicl/chunga")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jzn3nyb3f22gm983rfk99smqs3mhb9ivjmasvhq9qla5cl9pyhd"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-trivial-gray-streams))
    (home-page "https://edicl.github.io/chunga/")
    (synopsis "Portable chunked streams for Common Lisp")
    (description
     "Chunga implements streams capable of chunked encoding on demand as
defined in RFC 2616.")
    (license license:bsd-2)))

(define-public cl-chunga
  (sbcl-package->cl-source-package sbcl-chunga))

(define-public ecl-chunga
  (sbcl-package->ecl-package sbcl-chunga))

(define-public sbcl-cl-who
  (let ((version "1.1.4")
        (commit "0d3826475133271ee8c590937136c1bc41b8cbe0")
        (revision "2"))
    (package
      (name "sbcl-cl-who")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/edicl/cl-who")
               (commit commit)))
         (file-name (git-file-name "cl-who" version))
         (sha256
          (base32
           "0sc8nji9q1df04lhsiwsjy1a35996bibl31w5hp5sh8q6sa122dy"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-flexi-streams))
      (home-page "https://edicl.github.io/cl-who/")
      (synopsis "Yet another Lisp markup language")
      (description
       "There are plenty of Lisp Markup Languages out there - every Lisp
programmer seems to write at least one during his career - and CL-WHO (where
WHO means \"with-html-output\" for want of a better acronym) is probably just
as good or bad as the next one.")
      (license license:bsd-2))))

(define-public cl-who
  (sbcl-package->cl-source-package sbcl-cl-who))

(define-public ecl-cl-who
  (sbcl-package->ecl-package sbcl-cl-who))

(define-public sbcl-chipz
  (let ((version "0.8")
        (commit "75dfbc660a5a28161c57f115adf74c8a926bfc4d")
        (revision "1"))
    (package
      (name "sbcl-chipz")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/froydnj/chipz")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0plx4rs39zbs4gjk77h4a2q11zpy75fh9v8hnxrvsf8fnakajhwg"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-flexi-streams))
      (home-page "http://method-combination.net/lisp/chipz/")
      (synopsis
       "Common Lisp library for decompressing deflate, zlib, gzip, and bzip2
data")
      (description
       "DEFLATE data, defined in RFC1951, forms the core of popular
compression formats such as zlib (RFC 1950) and gzip (RFC 1952).  As such,
Chipz also provides for decompressing data in those formats as well.  BZIP2 is
the format used by the popular compression tool bzip2.")
      ;; The author describes it as "MIT-like"
      (license license:expat))))

(define-public cl-chipz
  (sbcl-package->cl-source-package sbcl-chipz))

(define-public ecl-chipz
  (sbcl-package->ecl-package sbcl-chipz))

(define-public sbcl-drakma
  (package
    (name "sbcl-drakma")
    (version "2.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edicl/drakma")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cl-drakma" version))
       (sha256
        (base32
         "1wf2zivfvhsh6zvd6wcwfd67bm8s8a1p2fismszc8xb819dqk9yl"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-puri
           sbcl-cl-base64
           sbcl-chunga
           sbcl-flexi-streams
           sbcl-cl-ppcre
           sbcl-chipz
           sbcl-usocket
           sbcl-cl+ssl))
    (native-inputs
     (list sbcl-fiveam))
    (home-page "https://edicl.github.io/drakma/")
    (synopsis "HTTP client written in Common Lisp")
    (description
     "Drakma is a full-featured HTTP client implemented in Common Lisp.  It
knows how to handle HTTP/1.1 chunking, persistent connections, re-usable
sockets, SSL, continuable uploads, file uploads, cookies, and more.")
    (license license:bsd-2)))

(define-public cl-drakma
  (sbcl-package->cl-source-package sbcl-drakma))

(define-public ecl-drakma
  (sbcl-package->ecl-package sbcl-drakma))

(define-public sbcl-hunchentoot
  (package
    (name "sbcl-hunchentoot")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edicl/hunchentoot")
             (commit (string-append "v" version))))
       (file-name (git-file-name "hunchentoot" version))
       (sha256
        (base32 "1z0m45lp6rv59g69l44gj3q3d2bmjlhqzpii0vgkniam21dcimy9"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-cl-who sbcl-drakma))
    (inputs
     (list sbcl-chunga
           sbcl-cl-base64
           sbcl-cl-fad
           sbcl-cl-ppcre
           sbcl-flexi-streams
           sbcl-cl+ssl
           sbcl-md5
           sbcl-rfc2388
           sbcl-trivial-backtrace
           sbcl-usocket))
    (home-page "https://edicl.github.io/hunchentoot/")
    (synopsis "Web server written in Common Lisp")
    (description
     "Hunchentoot is a web server written in Common Lisp and at the same
time a toolkit for building dynamic websites.  As a stand-alone web server,
Hunchentoot is capable of HTTP/1.1 chunking (both directions), persistent
connections (keep-alive), and SSL.")
    (license license:bsd-2)))

(define-public cl-hunchentoot
  (sbcl-package->cl-source-package sbcl-hunchentoot))

(define-public ecl-hunchentoot
  (package
    (inherit (sbcl-package->ecl-package sbcl-hunchentoot))
    (arguments
     ;; Tests fail on ECL with 'Socket error in "socket": EINVAL'.
     '(#:tests? #f))))

(define-public sbcl-trivial-types
  (package
    (name "sbcl-trivial-types")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/m2ym/trivial-types")
             (commit "ee869f2b7504d8aa9a74403641a5b42b16f47d88")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s4cp9bdlbn8447q7w7f1wkgwrbvfzp20mgs307l5pxvdslin341"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/m2ym/trivial-types")
    (synopsis "Trivial type definitions for Common Lisp")
    (description
     "TRIVIAL-TYPES provides missing but important type definitions such as
PROPER-LIST, ASSOCIATION-LIST, PROPERTY-LIST and TUPLE.")
    (license license:llgpl)))

(define-public cl-trivial-types
  (sbcl-package->cl-source-package sbcl-trivial-types))

(define-public ecl-trivial-types
  (sbcl-package->ecl-package sbcl-trivial-types))

(define-public sbcl-cl-annot
  (let ((commit "c99e69c15d935eabc671b483349a406e0da9518d")
        (revision "1"))
    (package
      (name "sbcl-cl-annot")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/m2ym/cl-annot")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1wq1gs9jjd5m6iwrv06c2d7i5dvqsfjcljgbspfbc93cg5xahk4n"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria))
      (home-page "https://github.com/m2ym/cl-annot")
      (synopsis "Python-like Annotation Syntax for Common Lisp")
      (description
       "@code{cl-annot} is an general annotation library for Common Lisp.")
      (license license:llgpl))))

(define-public cl-annot
  (sbcl-package->cl-source-package sbcl-cl-annot))

(define-public ecl-cl-annot
  (sbcl-package->ecl-package sbcl-cl-annot))

(define-public sbcl-cl-syntax
  (package
    (name "sbcl-cl-syntax")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/m2ym/cl-syntax")
             (commit "03f0c329bbd55b8622c37161e6278366525e2ccc")))
       (file-name (git-file-name "cl-syntax" version))
       (sha256
        (base32 "17ran8xp77asagl31xv8w819wafh6whwfc9p6dgx22ca537gyl4y"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("cl-annot" ,sbcl-cl-annot)
       ("cl-interpol" ,sbcl-cl-interpol)
       ("named-readtables" ,sbcl-named-readtables)
       ("trivial-types" ,sbcl-trivial-types)))
    (arguments
     '(#:asd-systems '("cl-syntax"
                       "cl-syntax-annot"
                       "cl-syntax-interpol")))
    (home-page "https://github.com/m2ym/cl-syntax")
    (synopsis "Reader Syntax Coventions for Common Lisp and SLIME")
    (description
     "CL-SYNTAX provides Reader Syntax Coventions for Common Lisp and SLIME.")
    (license license:llgpl)))

(define-public cl-syntax
  (sbcl-package->cl-source-package sbcl-cl-syntax))

(define-public ecl-cl-syntax
  (sbcl-package->ecl-package sbcl-cl-syntax))

(define-public sbcl-cl-utilities
  (let ((commit "dce2d2f6387091ea90357a130fa6d13a6776884b")
        (revision "1"))
    (package
      (name "sbcl-cl-utilities")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method url-fetch)
         (uri
          (string-append
           "https://gitlab.common-lisp.net/cl-utilities/cl-utilities/-/"
           "archive/" commit "/cl-utilities-" commit ".tar.gz"))
         (sha256
          (base32 "1r46v730yf96nk2vb24qmagv9x96xvd08abqwhf02ghgydv1a7z2"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "rotate-byte.lisp"
                 (("in-package :cl-utilities)" all)
                  "in-package :cl-utilities)\n\n#+sbcl\n(require :sb-rotate-byte)")))))))
      (home-page "http://common-lisp.net/project/cl-utilities")
      (synopsis "Collection of semi-standard utilities")
      (description
       "On Cliki.net <http://www.cliki.net/Common%20Lisp%20Utilities>, there
is a collection of Common Lisp Utilities, things that everybody writes since
they're not part of the official standard.  There are some very useful things
there; the only problems are that they aren't implemented as well as you'd
like (some aren't implemented at all) and they aren't conveniently packaged
and maintained.  It takes quite a bit of work to carefully implement utilities
for common use, commented and documented, with error checking placed
everywhere some dumb user might make a mistake.")
      (license license:public-domain))))

(define-public cl-utilities
  (sbcl-package->cl-source-package sbcl-cl-utilities))

(define-public ecl-cl-utilities
  (sbcl-package->ecl-package sbcl-cl-utilities))

(define-public sbcl-map-set
  (let ((commit "7b4b545b68b8")
        (revision "1"))
    (package
      (name "sbcl-map-set")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://bitbucket.org/tarballs_are_good/map-set/get/"
               commit ".tar.gz"))
         (sha256
          (base32 "1sx5j5qdsy5fklspfammwb16kjrhkggdavm922a9q86jm5l0b239"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://bitbucket.org/tarballs_are_good/map-set")
      (synopsis "Set-like data structure")
      (description
       "Implementation of a set-like data structure with constant time
addition, removal, and random selection.")
      (license license:bsd-3))))

(define-public cl-map-set
  (sbcl-package->cl-source-package sbcl-map-set))

(define-public ecl-map-set
  (sbcl-package->ecl-package sbcl-map-set))

(define-public sbcl-quri
  (package
    (name "sbcl-quri")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/quri")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ka5haq3g72hvaz4hdv7y1d6df9ncmx029wwixn4r413gll5yxy7"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     ;; Test system must be loaded before, otherwise tests fail with:
     ;; Component QURI-ASD::QURI-TEST not found, required by #<SYSTEM
     ;; "quri">.
     '(#:asd-systems '("quri-test"
                       "quri")))
    (native-inputs (list sbcl-prove))
    (inputs (list sbcl-babel sbcl-split-sequence sbcl-cl-utilities
                  sbcl-alexandria))
    (home-page "https://github.com/fukamachi/quri")
    (synopsis "Yet another URI library for Common Lisp")
    (description
     "QURI (pronounced \"Q-ree\") is yet another URI library for Common
Lisp. It is intended to be a replacement of PURI.")
    (license license:bsd-3)))

(define-public cl-quri
  (sbcl-package->cl-source-package sbcl-quri))

(define-public ecl-quri
  (sbcl-package->ecl-package sbcl-quri))

(define-public sbcl-myway
  (let ((commit "286230082a11f879c18b93f17ca571c5f676bfb7")
        (revision "1"))
    (package
     (name "sbcl-myway")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/myway")
             (commit commit)))
       (file-name (git-file-name "myway" version))
       (sha256
        (base32 "0briia9bk3lbr0frnx39d1qg6i38dm4j6z9w3yga3d40k6df4a90"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; Tests fail with: Component MYWAY-ASD::MYWAY-TEST not found, required
      ;; by #<SYSTEM "myway">. Why?
      '(#:tests? #f))
     (native-inputs
      (list sbcl-prove))
     (inputs
      (list sbcl-cl-ppcre sbcl-quri sbcl-map-set))
     (home-page "https://github.com/fukamachi/myway")
     (synopsis "Sinatra-compatible URL routing library for Common Lisp")
     (description "My Way is a Sinatra-compatible URL routing library.")
     (license license:llgpl))))

(define-public cl-myway
  (sbcl-package->cl-source-package sbcl-myway))

(define-public ecl-myway
  (sbcl-package->ecl-package sbcl-myway))

(define-public sbcl-xsubseq
  (let ((commit "5ce430b3da5cda3a73b9cf5cee4df2843034422b")
        (revision "1"))
    (package
     (name "sbcl-xsubseq")
     (version (git-version "0.0.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/xsubseq")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xz79q0p2mclf3sqjiwf6izdpb6xrsr350bv4mlmdlm6rg5r99px"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; Tests fail with: Component XSUBSEQ-ASD::XSUBSEQ-TEST not found,
      ;; required by #<SYSTEM "xsubseq">. Why?
      '(#:tests? #f))
     (native-inputs
      (list sbcl-prove))
     (home-page "https://github.com/fukamachi/xsubseq")
     (synopsis "Efficient way to use \"subseq\"s in Common Lisp")
     (description
      "XSubseq provides functions to be able to handle \"subseq\"s more
effieiently.")
     (license license:bsd-2))))

(define-public cl-xsubseq
  (sbcl-package->cl-source-package sbcl-xsubseq))

(define-public ecl-xsubseq
  (sbcl-package->ecl-package sbcl-xsubseq))

(define-public sbcl-smart-buffer
  (let ((commit "09b9a9a0b3abaa37abe9a730f5aac2643dca4e62")
        (revision "1"))
    (package
      (name "sbcl-smart-buffer")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/smart-buffer")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qz1zzxx0wm5ff7gpgsq550a59p0qj594zfmm2rglj97dahj54l7"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Tests fail with: Component SMART-BUFFER-ASD::SMART-BUFFER-TEST not
       ;; found, required by #<SYSTEM "smart-buffer">. Why?
       `(#:tests? #f))
      (native-inputs
       (list sbcl-prove))
      (inputs
       (list sbcl-xsubseq sbcl-flexi-streams))
      (home-page "https://github.com/fukamachi/smart-buffer")
      (synopsis "Smart octets buffer")
      (description
       "Smart-buffer provides an output buffer which changes the destination
depending on content size.")
      (license license:bsd-3))))

(define-public cl-smart-buffer
  (sbcl-package->cl-source-package sbcl-smart-buffer))

(define-public ecl-smart-buffer
  (sbcl-package->ecl-package sbcl-smart-buffer))

(define-public sbcl-fast-http
  (let ((commit "502a37715dcb8544cc8528b78143a942de662c5a")
        (revision "2"))
    (package
      (name "sbcl-fast-http")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/fast-http")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0al2g7g219jjljsf7b23pbilpgacxy5as5gs2nqf76b5qni396mi"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Tests fail with: Component FAST-HTTP-ASD::FAST-HTTP-TEST not found,
       ;; required by #<SYSTEM "fast-http">. Why?
       `(#:tests? #f))
      (native-inputs
       `(("sbcl-prove" ,sbcl-prove)
         ("cl-syntax" ,sbcl-cl-syntax)))
      (inputs
       (list sbcl-alexandria sbcl-proc-parse sbcl-xsubseq
             sbcl-smart-buffer sbcl-cl-utilities))
      (home-page "https://github.com/fukamachi/fast-http")
      (synopsis "HTTP request/response parser for Common Lisp")
      (description
       "@code{fast-http} is a HTTP request/response protocol parser for Common
Lisp.")
      ;; Author specified the MIT license
      (license license:expat))))

(define-public cl-fast-http
  (sbcl-package->cl-source-package sbcl-fast-http))

(define-public ecl-fast-http
  (sbcl-package->ecl-package sbcl-fast-http))

(define-public sbcl-static-vectors
  (package
    (name "sbcl-static-vectors")
    (version "1.8.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sionescu/static-vectors")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "079qa20lhanzsz1qf4iags91n0ziylbjgbcymm5a5qj7yryas4fw"))))
    (native-inputs
     (list sbcl-fiveam))
    (inputs
     (list sbcl-alexandria sbcl-cffi))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/sionescu/static-vectors")
    (synopsis "Allocate SIMPLE-ARRAYs in static memory")
    (description
     "With @code{static-vectors}, you can create vectors allocated in static
memory.")
    (license license:expat)))

(define-public cl-static-vectors
  (sbcl-package->cl-source-package sbcl-static-vectors))

(define-public ecl-static-vectors
  (sbcl-package->ecl-package sbcl-static-vectors))

(define-public sbcl-marshal
  (let ((commit "eff1b15f2b0af2f26f71ad6a4dd5c4beab9299ec")
        (revision "1"))
    (package
     (name "sbcl-marshal")
     (version (git-version "1.3.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wlbr/cl-marshal")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08qs6fhk38xpkkjkpcj92mxx0lgy4ygrbbzrmnivdx281syr0gwh"))))
     (build-system asdf-build-system/sbcl)
     (home-page "https://github.com/wlbr/cl-marshal")
     (synopsis "Simple (de)serialization of Lisp datastructures")
     (description
      "Simple and fast marshalling of Lisp datastructures.  Convert any object
into a string representation, put it on a stream an revive it from there.
Only minimal changes required to make your CLOS objects serializable.")
     (license license:expat))))

(define-public cl-marshal
  (sbcl-package->cl-source-package sbcl-marshal))

(define-public ecl-marshal
  (sbcl-package->ecl-package sbcl-marshal))

(define-public sbcl-fast-io
  (let ((commit "603f4903dd74fb221859da7058ae6ca3853fe64b")
        (revision "2"))
    (package
     (name "sbcl-fast-io")
     (version (git-version "1.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rpav/fast-io")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00agvc0xx4w715i6ach05p995zpcpghn04xc06zyci06q677vw3n"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; Error while trying to load definition for system fast-io-test from
      ;; pathname [...]/fast-io-test.asd: The function CHECKL:DEFINE-TEST-OP
      ;; is undefined.
      '(#:tests? #f
        #:asd-files '("fast-io.asd")))
     (native-inputs
      (list sbcl-fiveam sbcl-checkl))
     (inputs
      (list sbcl-alexandria sbcl-trivial-gray-streams sbcl-static-vectors))
     (home-page "https://github.com/rpav/fast-io")
     (synopsis "Fast octet-vector/stream I/O for Common Lisp")
     (description
      "Fast-io is about improving performance to octet-vectors and octet
streams (though primarily the former, while wrapping the latter).")
     ;; Author specifies this as NewBSD which is an alias
     (license license:bsd-3))))

(define-public cl-fast-io
  (sbcl-package->cl-source-package sbcl-fast-io))

(define-public ecl-fast-io
  (sbcl-package->ecl-package sbcl-fast-io))

(define-public sbcl-jonathan
  (let ((commit "1f448b4f7ac8265e56e1c02b32ce383e65316300")
        (revision "1"))
    (package
     (name "sbcl-jonathan")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Rudolph-Miller/jonathan")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14x4iwz3mbag5jzzzr4sb6ai0m9r4q4kyypbq32jmsk2dx1hi807"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; Tests fail with: Component JONATHAN-ASD::JONATHAN-TEST not found,
      ;; required by #<SYSTEM "jonathan">. Why?
      `(#:tests? #f))
     (native-inputs
      (list sbcl-prove))
     (inputs
      (list sbcl-cl-syntax sbcl-fast-io sbcl-proc-parse sbcl-cl-ppcre))
     (home-page "https://rudolph-miller.github.io/jonathan/overview.html")
     (synopsis "JSON encoder and decoder")
     (description
      "High performance JSON encoder and decoder.  Currently support: SBCL,
CCL.")
     ;; Author specifies the MIT license
     (license license:expat))))

(define-public cl-jonathan
  (sbcl-package->cl-source-package sbcl-jonathan))

(define-public ecl-jonathan
  (sbcl-package->ecl-package sbcl-jonathan))

(define-public sbcl-http-body
  (let ((commit "dd01dc4f5842e3d29728552e5163acce8386eb73")
        (revision "1"))
    (package
     (name "sbcl-http-body")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/http-body")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jd06snjvxcprhapgfq8sx0y5lrldkvhf206ix6d5a23dd6zcmr0"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; Tests fail with: Component HTTP-BODY-ASD::HTTP-BODY-TEST not
      ;; found, required by #<SYSTEM "http-body">. Why?
      `(#:tests? #f))
     (native-inputs
      (list sbcl-prove))
     (inputs
      (list sbcl-fast-http sbcl-jonathan sbcl-quri))
     (home-page "https://github.com/fukamachi/http-body")
     (synopsis "HTTP POST data parser")
     (description
      "HTTP-Body parses HTTP POST data and returns POST parameters.  It
supports application/x-www-form-urlencoded, application/json, and
multipart/form-data.")
     (license license:bsd-2))))

(define-public cl-http-body
  (sbcl-package->cl-source-package sbcl-http-body))

(define-public ecl-http-body
  (sbcl-package->ecl-package sbcl-http-body))

(define-public sbcl-circular-streams
  (let ((commit "e770bade1919c5e8533dd2078c93c3d3bbeb38df")
        (revision "1"))
    (package
     (name "sbcl-circular-streams")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/circular-streams")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wpw6d5cciyqcf92f7mvihak52pd5s47kk4qq6f0r2z2as68p5rs"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; The tests depend on cl-test-more which is now prove. Prove
      ;; tests aren't working for some reason.
      `(#:tests? #f))
     (inputs
      (list sbcl-fast-io sbcl-trivial-gray-streams))
     (home-page "https://github.com/fukamachi/circular-streams")
     (synopsis "Circularly readable streams for Common Lisp")
     (description
      "Circular-Streams allows you to read streams circularly by wrapping real
streams. Once you reach end-of-file of a stream, it's file position will be
reset to 0 and you're able to read it again.")
     (license license:llgpl))))

(define-public cl-circular-streams
  (sbcl-package->cl-source-package sbcl-circular-streams))

(define-public ecl-circular-streams
  (sbcl-package->ecl-package sbcl-circular-streams))

(define-public sbcl-lack
  (let ((commit "abff8efeb0c3a848e6bb0022f2b8b7fa3a1bc88b")
        (revision "1"))
    (package
      (name "sbcl-lack")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/lack")
               (commit commit)))
         (file-name (git-file-name "lack" version))
         (sha256
          (base32 "1avh4ygcj9xcx4m17nj0wnxxaisk26w4ljs2bibzxaln24x7pi85"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-prove))
      (inputs
       `(("circular-streams" ,sbcl-circular-streams)
         ("http-body" ,sbcl-http-body)
         ("ironclad" ,sbcl-ironclad)
         ("local-time" ,sbcl-local-time)
         ("quri" ,sbcl-quri)
         ("trivial-mimes" ,sbcl-trivial-mimes)))
      (arguments
       '(#:asd-systems '("lack"
                         "lack-request"
                         "lack-response"
                         "lack-component"
                         "lack-util"
                         "lack-middleware-backtrace"
                         "lack-middleware-static")
         #:test-asd-file "t-lack.asd"
         ;; XXX: Component :CLACK not found
         #:tests? #f))
      (home-page "https://github.com/fukamachi/lack")
      (synopsis "Lack, the core of Clack")
      (description
       "Lack is a Common Lisp library which allows web applications to be
constructed of modular components.  It was originally a part of Clack, however
it's going to be rewritten as an individual project since Clack v2 with
performance and simplicity in mind.")
      (license license:llgpl))))

(define-public cl-lack
  (sbcl-package->cl-source-package sbcl-lack))

(define-public ecl-lack
  (sbcl-package->ecl-package sbcl-lack))

(define-public sbcl-local-time
  (let ((commit "a177eb911c0e8116e2bfceb79049265a884b701b")
        (revision "2"))
    (package
     (name "sbcl-local-time")
     (version (git-version "1.0.6" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dlowe-net/local-time")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wld28xx20k0ysgg6akic5lg4vkjd0iyhv86m388xfrv8xh87wii"))))
     (build-system asdf-build-system/sbcl)
     (native-inputs
      (list sbcl-hu.dwim.stefil))
     (home-page "https://common-lisp.net/project/local-time/")
     (synopsis "Time manipulation library for Common Lisp")
     (description
      "The LOCAL-TIME library is a Common Lisp library for the manipulation of
dates and times.  It is based almost entirely upon Erik Naggum's paper \"The
Long Painful History of Time\".")
     (license license:expat))))

(define-public cl-local-time
  (sbcl-package->cl-source-package sbcl-local-time))

(define-public ecl-local-time
  (sbcl-package->ecl-package sbcl-local-time))

(define-public sbcl-chronicity
  (package
    (name "sbcl-chronicity")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chaitanyagupta/chronicity")
             (commit (string-append "v" version))))
       (file-name (git-file-name "chronicity" version))
       (sha256
        (base32 "0rzrl9is2v1aqbm0sym0qx3blnpd0bl13dkkmll6mb3983k2mkax"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-lisp-unit))
    (inputs
     (list sbcl-cl-interpol sbcl-cl-ppcre sbcl-local-time))
    (home-page "https://github.com/chaitanyagupta/chronicity")
    (synopsis "Natural language date and time parser for Common Lisp")
    (description
     "CHRONICITY is Common Lisp natural language date and time parser inspired
by Ruby's @code{Chronic}.")
    (license license:bsd-3)))

(define-public ecl-chronicity
  (sbcl-package->ecl-package sbcl-chronicity))

(define-public cl-chronicity
  (sbcl-package->cl-source-package sbcl-chronicity))

(define-public sbcl-trivial-mimes
  (let ((commit "a741fc2f567a4f86b853fd4677d75e62c03e51d9")
        (revision "2"))
    (package
      (name "sbcl-trivial-mimes")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/trivial-mimes")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00kcm17q5plpzdj1qwg83ldhxksilgpcdkf3m9azxcdr968xs9di"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-hu.dwim.stefil))
      (inputs
       (list sbcl-cl-fad))
      (home-page "https://shinmera.github.io/trivial-mimes/")
      (synopsis "Tiny Common Lisp library to detect mime types in files")
      (description
       "This is a teensy library that provides some functions to determine the
mime-type of a file.")
      (license license:zlib))))

(define-public cl-trivial-mimes
  (sbcl-package->cl-source-package sbcl-trivial-mimes))

(define-public ecl-trivial-mimes
  (sbcl-package->ecl-package sbcl-trivial-mimes))

(define-public sbcl-ningle
  (let ((commit "50bd4f09b5a03a7249bd4d78265d6451563b25ad")
        (revision "1"))
    (package
      (name "sbcl-ningle")
      (version (git-version "0.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/ningle")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1bsl8cnxhacb8p92z9n89vhk1ikmij5zavk0m2zvmj7iqm79jzgw"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; TODO: pull in clack-test
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'cleanup-files)
           (delete 'cleanup)
           (add-before 'cleanup 'combine-fasls
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib/sbcl"))
                      (ningle-path (string-append lib "/ningle"))
                      (fasl-files (find-files out "\\.fasl$")))
                 (mkdir-p ningle-path)
                 (let ((fasl-path (lambda (name)
                                    (string-append ningle-path
                                                   "/"
                                                   (basename name)
                                                   "--system.fasl"))))
                   (for-each (lambda (file)
                               (rename-file file
                                            (fasl-path
                                             (basename file ".fasl"))))
                             fasl-files))
                 fasl-files)
               #t)))))
      (native-inputs
       (list sbcl-prove))
      (inputs
       (list sbcl-cl-syntax sbcl-myway sbcl-lack sbcl-alexandria
             sbcl-babel))
      (home-page "https://8arrow.org/ningle/")
      (synopsis "Super micro framework for Common Lisp")
      (description
       "Ningle is a lightweight web application framework for Common Lisp.")
      (license license:llgpl))))

(define-public cl-ningle
  (sbcl-package->cl-source-package sbcl-ningle))

(define-public ecl-ningle
  (sbcl-package->ecl-package sbcl-ningle))

(define-public sbcl-cl-fastcgi
  (let ((commit "de8b49b26de9863996ec18db28af8ab7e8ac4e20")
        (revision "2"))
    (package
      (name "sbcl-cl-fastcgi")
      (version (git-version "0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/KDr2/cl-fastcgi/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0xgmhx766q4nmrvn5z7ag3ikpr9phlh8ypi8b14azshq9lqbq0m7"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("usocket" ,sbcl-usocket)
         ("cffi" ,sbcl-cffi)
         ("fcgi" ,fcgi)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "cl-fastcgi.lisp"
                 (("\"libfcgi.so\"")
                  (string-append
                   "\""
                   (assoc-ref inputs "fcgi") "/lib/libfcgi.so\""))))))))
      (home-page "https://kdr2.com/project/cl-fastcgi.html")
      (synopsis "FastCGI wrapper for Common Lisp")
      (description
       "CL-FastCGI is a generic version of SB-FastCGI, targeting to run on
mostly Common Lisp implementation.")
      (license license:bsd-2))))

(define-public cl-fastcgi
  (sbcl-package->cl-source-package sbcl-cl-fastcgi))

(define-public ecl-cl-fastcgi
  (sbcl-package->ecl-package sbcl-cl-fastcgi))

(define-public sbcl-clack
  (let ((commit "e3e032843bb1220ab96263c411aa7f2feb4746e0")
        (revision "1"))
    (package
      (name "sbcl-clack")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/clack")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ymzs6qyrwhlj6cgqsnpyn6g5cbp7a3s1vgxwna20y2q7y4iacy0"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cl-fastcgi" ,sbcl-cl-fastcgi)
         ("flexi-streams" ,sbcl-flexi-streams)
         ("hunchentoot" ,sbcl-hunchentoot)
         ("lack" ,sbcl-lack)
         ("split-sequence" ,sbcl-split-sequence)
         ("usocket" ,sbcl-usocket)
         ("quri" ,sbcl-quri)))
      (arguments
       '(#:asd-systems '("clack"
                         "clack-handler-fcgi"
                         "clack-socket"
                         "clack-handler-hunchentoot")))
      (home-page "https://github.com/fukamachi/clack")
      (synopsis "Web Application Environment for Common Lisp")
      (description
       "Clack is a web application environment for Common Lisp inspired by
Python's WSGI and Ruby's Rack.")
      (license license:llgpl))))

(define-public cl-clack
  (sbcl-package->cl-source-package sbcl-clack))

(define-public ecl-clack
  (sbcl-package->ecl-package sbcl-clack))

(define-public sbcl-cl-log
  (let ((commit "8f4b766d51e02245c310526cf1e4534ce634f837")
        (revision "1"))
    (package
     (name "sbcl-cl-log")
     (version "1.0.1")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nicklevine/cl-log")
             (commit commit)))
       (sha256
        (base32 "1r3z9swy1b59swvaa5b97is9ysrfmjvjjhhw56p7p5hqg93b92ak"))
       (file-name (git-file-name "cl-log" version))))
     (build-system asdf-build-system/sbcl)
     (synopsis "Common Lisp general purpose logging utility")
     (description "CL-LOG is a general purpose logging utility, loosely modelled
in some respects after Gary King's Log5.  Its features include: logging to
several destinations at once, via \"messengers\", each messenger is tailored to
accept some log messages and reject others, and this tailoring can be changed
on-the-fly, very rapid processing of messages which are rejected by all
messengers, fully independent use of the utility by several different
sub-systems in an application, support for messengers which cl:format text to a
stream, support for messengers which do not invoke cl:format, timestamps in
theory accurate to internal-time-units-per-second.")
     (home-page "https://github.com/nicklevine/cl-log")
     (license license:expat))))

(define-public cl-log
  (sbcl-package->cl-source-package sbcl-cl-log))

(define-public ecl-cl-log
  (sbcl-package->ecl-package sbcl-cl-log))

(define-public sbcl-log4cl
  (let ((commit "8c48d6f41d3a1475d0a91eed0638b9eecc398e35")
        (revision "1"))
    (package
      (name "sbcl-log4cl")
      (version (git-version "1.1.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/log4cl")
               (commit commit)))
         (file-name (git-file-name "log4cl" version))
         (sha256
          (base32 "0166d9aip366pbpdk5gsi2f6xad6q61lssxgbrypa8zslwjn8736"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-stefil))
      (inputs
       (list sbcl-bordeaux-threads))
      (home-page "https://github.com/7max/log4cl")
      (synopsis "Common Lisp logging framework, modeled after Log4J")
      (description
       "This is a Common Lisp logging framework that can log at various levels
and mix text with expressions.")
      (license license:asl2.0))))

(define-public cl-log4cl
  (sbcl-package->cl-source-package sbcl-log4cl))

(define-public ecl-log4cl
  (sbcl-package->ecl-package sbcl-log4cl))

(define-public sbcl-printv
  (let ((commit "646d31978dbbb460fffb160fd65bb2be9a5a434e")
        (revision "1"))
    (package
      (name "sbcl-printv")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/danlentz/printv")
               (commit commit)))
         (file-name (git-file-name "printv" version))
         (sha256
          (base32 "08jvy82abm7qi3wrxh6gvmwg9gy0zzhg4cfqajdwrggbah8mj5a6"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/danlentz/printv")
      (synopsis "Common Lisp tracing and debug-logging macro")
      (description
       "@code{PRINTV} is a \"batteries-included\" tracing and debug-logging
macro for Common Lisp.")
      (license license:asl2.0))))

(define-public ecl-printv
  (sbcl-package->ecl-package sbcl-printv))

(define-public cl-printv
  (sbcl-package->cl-source-package sbcl-printv))

(define-public sbcl-cl-debug
  (let ((commit "b334280806104ee7f7d3aec666bf7e08d2f89b31")
        (revision "1"))
    (package
     (name "sbcl-cl-debug")
      (version (git-version "1.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kmx-io/cl-debug")
             (commit commit)))
       (file-name (git-file-name "cl-debug" version))
       (sha256
        (base32 "0w5vxbjsgr3zfpivdmghmhzxskfdvm1p34c8whwps2xlhypxsa78"))))
     (build-system asdf-build-system/sbcl)
     (home-page "https://github.com/kmx-io/cl-debug")
     (synopsis "Common Lisp cross-package debugging facility")
     (description
      "CL-DEBUG provides a unified way to enable or disable debug-specific code.
Debugging code can be enabled or disabled relative to program features denoted
by either a symbol or a keyword.")
     (license license:isc))))

(define-public ecl-cl-debug
  (sbcl-package->ecl-package sbcl-cl-debug))

(define-public cl-debug
  (sbcl-package->cl-source-package sbcl-cl-debug))

(define-public sbcl-verbose
  (let ((commit "c5b7ecd465be61b35af17ef57564697b88397174")
        (revision "1"))
    (package
      (name "sbcl-verbose")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/verbose/")
               (commit commit)))
         (file-name (git-file-name "verbose" version))
         (sha256
          (base32 "0r51ydj5v7afi2jrlscbhxprv13d9vzg5316g1yzwaxc1kzsdsw6"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-bordeaux-threads sbcl-dissect sbcl-documentation-utils
             sbcl-local-time sbcl-piping))
      (home-page "https://shinmera.github.io/verbose/")
      (synopsis "Logging framework using the piping library")
      (description
       "This is a Common Lisp library providing logging faciltiy similar to
@code{CL-LOG} and @code{LOG4CL}.")
      (license license:zlib))))

(define-public ecl-verbose
  (sbcl-package->ecl-package sbcl-verbose))

(define-public cl-verbose
  (sbcl-package->cl-source-package sbcl-verbose))

(define-public sbcl-find-port
  (let ((commit "00c96a25af93a0f8681d34ec548861f2d7485478")
        (revision "1"))
    (package
      (name "sbcl-find-port")
      (build-system asdf-build-system/sbcl)
      (version "0.1")
      (home-page "https://github.com/eudoxia0/find-port")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0d6dzbb45jh0rx90wgs6v020k2xa87mvzas3mvfzvivjvqqlpryq"))))
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-usocket))
      (synopsis "Find open ports programmatically in Common Lisp")
      (description "This is a small Common Lisp library that finds an open
port within a range.")
      (license license:expat))))

(define-public cl-find-port
  (sbcl-package->cl-source-package sbcl-find-port))

(define-public ecl-find-port
  (sbcl-package->ecl-package sbcl-find-port))

(define-public sbcl-py4cl
  (let ((commit "4c8a2b0814fd311f978964f825ce012290f60136")
        (revision "1"))
    (package
      (name "sbcl-py4cl")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/bendudson/py4cl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "15mk7qdqjkj56gdnbyrdyz6r7m1h26ldvn6ch96pmvg5vmr1m45r"))
         (modules '((guix build utils)))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-clunit))
      (inputs
       (list sbcl-trivial-garbage))
      (propagated-inputs
       ;; This package doesn't do anything without python available
       (list python
             ;; For multi-dimensional array support
             python-numpy))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'replace-*base-directory*-var
             (lambda* (#:key outputs #:allow-other-keys)
               ;; In the ASD, the author makes an attempt to
               ;; programatically determine the location of the
               ;; source-code so lisp can call into "py4cl.py". We can
               ;; hard-code this since we know where this file will
               ;; reside.
               (substitute* "src/callpython.lisp"
                 (("py4cl/config:\\*base-directory\\*")
                  (string-append
                   "\""
                   (assoc-ref outputs "out")
                   "/share/common-lisp/sbcl-source/py4cl/"
                   "\""))))))))
      (synopsis "Call python from Common Lisp")
      (description
       "Py4CL is a bridge between Common Lisp and Python, which enables Common
Lisp to interact with Python code.  It uses streams to communicate with a
separate python process, the approach taken by cl4py.  This is different to
the CFFI approach used by burgled-batteries, but has the same goal.")
      (home-page "https://github.com/bendudson/py4cl")
      ;; MIT License
      (license license:expat))))

(define-public cl-py4cl
  (sbcl-package->cl-source-package sbcl-py4cl))

(define-public ecl-py4cl
  (sbcl-package->ecl-package sbcl-py4cl))

(define-public sbcl-schemeish
  (let ((commit "dff57bafae5d0cffa104c8fdc4146502f32d7f85")
        (revision "1"))
    (package
      (name "sbcl-schemeish")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/chebert/schemeish")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0q9b07spmhg1b576cnnacvkf7zr3mab2rdydfylbn92y9mms9vyj"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-trivial-arguments))
      (synopsis "Scheme style syntax/macros/functions for Common Lisp")
      (description
       "Schemeish implements several useful Scheme constructs for Common Lisp.
These include named-let, define, scheme argument lists, and a shortcut to
FUNCALL with [] instead of ().")
      (home-page "https://github.com/chebert/schemeish")
      ;; MIT License
      (license license:expat))))

(define-public cl-schemeish
  (sbcl-package->cl-source-package sbcl-schemeish))

(define-public ecl-schemeish
  (sbcl-package->ecl-package sbcl-schemeish))

(define-public sbcl-parse-declarations
  (let ((commit "549aebbfb9403a7fe948654126b9c814f443f4f2")
        (revision "1"))
    (package
      (name "sbcl-parse-declarations")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url (string-append
                     "https://gitlab.common-lisp.net/parse-declarations/"
                     "parse-declarations.git"))
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "03g5qks4c59nmxa48pbslxkfh77h8hn8566jddp6m9pl15dzzpxd"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems '("parse-declarations-1.0")))
      (home-page "https://common-lisp.net/project/parse-declarations/")
      (synopsis "Parse, filter, and build declarations")
      (description
       "Parse-Declarations is a Common Lisp library to help writing
macros which establish bindings.  To be semantically correct, such
macros must take user declarations into account, as these may affect
the bindings they establish.  Yet the ANSI standard of Common Lisp does
not provide any operators to work with declarations in a convenient,
high-level way.  This library provides such operators.")
      ;; MIT License
      (license license:expat))))

(define-public cl-parse-declarations
  (sbcl-package->cl-source-package sbcl-parse-declarations))

(define-public ecl-parse-declarations
  (sbcl-package->ecl-package sbcl-parse-declarations))

(define-public sbcl-burgled-batteries3
  (let ((commit "f65f454d13bb6c40e17e9ec62e41eb5069e09760")
        (revision "2"))
    (package
      (name "sbcl-burgled-batteries3")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/snmsts/burgled-batteries3")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1nzn7jawrfajyzwfnzrg2cmn9xxadcqh4szbpg0jggkhdkdzz4wa"))
         (patches
          (search-patches "sbcl-burgled-batteries3-fix-signals.patch"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:tests? #f
         #:modules (((guix build python-build-system) #:select (python-version))
                    ,@%asdf-build-system-modules)
         #:imported-modules ((guix build python-build-system)
                             ,@%asdf-build-system-modules)
         #:phases
         (modify-phases (@ (guix build asdf-build-system) %standard-phases)
           (add-after 'unpack 'set-*cpython-include-dir*-var
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((python (assoc-ref inputs "python")))
                 (setenv "BB_PYTHON3_INCLUDE_DIR"
                         (string-append python "/include/python"
                                        (python-version python)))
                 (setenv "BB_PYTHON3_DYLIB"
                         (string-append python "/lib/libpython3.so"))
                 #t)))
           (add-after 'unpack 'adjust-for-python-3.8
             (lambda _
               ;; This method is no longer part of the public API.
               (substitute* "ffi-interface.lisp"
                 ((".*PyEval_ReInitThreads.*")
                  ""))
               #t)))))
      (native-inputs
       (list sbcl-cl-fad sbcl-lift sbcl-cl-quickcheck))
      (inputs
       `(("python" ,python)
         ("sbcl-cffi" ,sbcl-cffi)
         ("sbcl-alexandria" , sbcl-alexandria)
         ("sbcl-parse-declarations-1.0" ,sbcl-parse-declarations)
         ("sbcl-trivial-garbage" ,sbcl-trivial-garbage)))
      (synopsis "Bridge between Python and Lisp (FFI bindings, etc.)")
      (description
       "This package provides a shim between Python3 (specifically, the
CPython implementation of Python) and Common Lisp.")
      (home-page "https://github.com/snmsts/burgled-batteries3")
      (license license:expat))))

(define-public cl-burgled-batteries3
  (sbcl-package->cl-source-package sbcl-burgled-batteries3))

(define-public ecl-burgled-batteries3
  (sbcl-package->ecl-package sbcl-burgled-batteries3))

(define-public sbcl-metabang-bind
  (let ((commit "c93b7f7e1c18c954c2283efd6a7fdab36746ab5e")
        (revision "1"))
    (package
      (name "sbcl-metabang-bind")
      (version (git-version "0.8.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gwkkwg/metabang-bind")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0hd0kr91795v77akpbcyqiss9p0p7ypa9dznrllincnmgvsxlmf0"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-lift))
      (synopsis "Macro that generalizes @code{multiple-value-bind} etc.")
      (description
       "Bind extends the idea of of let and destructing to provide a uniform
syntax for all your accessor needs.  It combines @code{let},
@code{destructuring-bind}, @code{with-slots}, @code{with-accessors}, structure
editing, property or association-lists, and @code{multiple-value-bind} and a
whole lot more into a single form.")
      (home-page "https://common-lisp.net/project/metabang-bind/")
      ;; MIT License
      (license license:expat))))

(define-public cl-metabang-bind
  (sbcl-package->cl-source-package sbcl-metabang-bind))

(define-public ecl-metabang-bind
  (sbcl-package->ecl-package sbcl-metabang-bind))

(define-public sbcl-fare-utils
  (let ((commit "66e9c6f1499140bc00ccc22febf2aa528cbb5724")
        (revision "1"))
    (package
      (name "sbcl-fare-utils")
      (version (git-version "1.0.0.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url
            "https://gitlab.common-lisp.net/frideau/fare-utils.git")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "01wsr1aap3jdzhn4hrqjbhsjx6qci9dbd3gh4gayv1p49rbg8aqr"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:test-asd-file "test/fare-utils-test.asd"))
      (native-inputs
       (list sbcl-hu.dwim.stefil))
      (synopsis "Collection of utilities and data structures")
      (description
       "fare-utils is a small collection of utilities.  It contains a lot of
basic everyday functions and macros.")
      (home-page "https://gitlab.common-lisp.net/frideau/fare-utils")
      ;; MIT License
      (license license:expat))))

(define-public cl-fare-utils
  (sbcl-package->cl-source-package sbcl-fare-utils))

(define-public ecl-fare-utils
  (sbcl-package->ecl-package sbcl-fare-utils))

(define-public sbcl-fare-mop
  (let ((commit "538aa94590a0354f382eddd9238934763434af30")
        (revision "1"))
    (package
      (name "sbcl-fare-mop")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fare/fare-mop")
               (commit commit)))
         (file-name (git-file-name "fare-mop" version))
         (sha256
          (base32
           "0maxs8392953fhnaa6zwnm2mdbhxjxipp4g4rvypm06ixr6pyv1c"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-closer-mop sbcl-fare-utils))
      (home-page "https://github.com/fare/fare-mop")
      (synopsis "General purpose Common Lisp utilities using the MOP")
      (description
       "FARE-MOP is a small collection of utilities using the MetaObject
Protocol.  It notably contains a SIMPLE-PRINT-OBJECT method, and
a SIMPLE-PRINT-OBJECT-MIXIN mixin that allow you to trivially define
PRINT-OBJECT methods that print the interesting slots in your objects, which is
great for REPL interaction and debugging.")
      (license license:unlicense))))

(define-public ecl-fare-mop
  (sbcl-package->ecl-package sbcl-fare-mop))

(define-public cl-fare-mop
  (sbcl-package->cl-source-package sbcl-fare-mop))

(define-public sbcl-inferior-shell
  (let ((commit "15c2d04a7398db965ea1c3ba2d49efa7c851f2c2")
        (revision "1"))
    (package
      (name "sbcl-inferior-shell")
      (version (git-version "2.0.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fare/inferior-shell")
               (commit commit)))
         (file-name (git-file-name "inferior-shell" version))
         (sha256
          (base32 "02qx37zzk5j4xmwh77k2qa2wvnzvaj6qml5dh2q7b6b1ljvgcj4m"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-hu.dwim.stefil))
      (inputs
       (list sbcl-alexandria sbcl-fare-mop sbcl-fare-quasiquote
             sbcl-fare-utils sbcl-trivia))
      (home-page "https://github.com/fare/inferior-shell")
      (synopsis "Spawn local or remote processes and shell pipes")
      (description
       "This package provides a Common Lisp system helping in scripting, it
uses @code{uiop:run-program} as a backend.")
      (license license:expat))))

(define-public ecl-inferior-shell
  (sbcl-package->ecl-package sbcl-inferior-shell))

(define-public cl-inferior-shell
  (sbcl-package->cl-source-package sbcl-inferior-shell))

(define-public sbcl-trivial-utf-8
  (let ((commit "4d427cfbb1c452436a0efb71c3205c9da67f718f")
        (revision "1"))
    (package
      (name "sbcl-trivial-utf-8")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url (string-append "https://gitlab.common-lisp.net/"
                               "trivial-utf-8/trivial-utf-8.git"))
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1jz27gz8gvqdmvp3k9bxschs6d5b3qgk94qp2bj6nv1d0jc3m1l1"))))
      (arguments
       ;; Guix incorrectly assumes the "8" is part of the version
       ;; number and lobs it off.
       `(#:asd-systems '("trivial-utf-8")))
      (build-system asdf-build-system/sbcl)
      (synopsis "UTF-8 input/output library")
      (description
       "The Babel library solves a similar problem while understanding more
encodings.  Trivial UTF-8 was written before Babel existed, but for new
projects you might be better off going with Babel.  The one plus that Trivial
UTF-8 has is that it doesn't depend on any other libraries.")
      (home-page "https://common-lisp.net/project/trivial-utf-8/")
      (license license:bsd-3))))

(define-public cl-trivial-utf-8
  (sbcl-package->cl-source-package sbcl-trivial-utf-8))

(define-public ecl-trivial-utf-8
  (sbcl-package->ecl-package sbcl-trivial-utf-8))

(define-public sbcl-idna
  (package
    (name "sbcl-idna")
    (build-system asdf-build-system/sbcl)
    (version "0.2.2")
    (home-page "https://github.com/antifuchs/idna")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00nbr3mffxhlq14gg9d16pa6691s4qh35inyw76v906s77khm5a2"))))
    (inputs
     (list sbcl-split-sequence))
    (synopsis "IDNA string encoding and decoding routines for Common Lisp")
    (description "This Common Lisp library provides string encoding and
decoding routines for IDNA, the International Domain Names in Applications.")
    (license license:expat)))

(define-public cl-idna
  (sbcl-package->cl-source-package sbcl-idna))

(define-public ecl-idna
  (sbcl-package->ecl-package sbcl-idna))

(define-public sbcl-swap-bytes
  (package
    (name "sbcl-swap-bytes")
    (build-system asdf-build-system/sbcl)
    (version "1.2")
    (home-page "https://github.com/sionescu/swap-bytes")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hw1v1lw26rifyznpnj1csphha9jgzwpiic16ni3pvs6hcsni9rz"))))
    (inputs
     (list sbcl-trivial-features))
    (native-inputs
     (list sbcl-fiveam))
    (synopsis "Efficient endianness conversion for Common Lisp")
    (description "This Common Lisp library provides optimized byte-swapping
primitives.  The library can change endianness of unsigned integers of length
1/2/4/8.  Very useful in implementing various network protocols and file
formats.")
    (license license:expat)))

(define-public cl-swap-bytes
  (sbcl-package->cl-source-package sbcl-swap-bytes))

(define-public ecl-swap-bytes
  (sbcl-package->ecl-package sbcl-swap-bytes))

(define-public sbcl-iolib
  (package
    (name "sbcl-iolib")
    (version "0.8.4")
    (home-page "https://github.com/sionescu/iolib")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1f43jqqqwp9n7xksqxw91myapsdbc2dxck6nd6flakbnp9haylyq"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cffi" ,sbcl-cffi)
       ("idna" ,sbcl-idna)
       ("libfixposix" ,libfixposix)
       ("split-sequence" ,sbcl-split-sequence)
       ("swap-bytes" ,sbcl-swap-bytes)))
    (arguments
     '(#:asd-files '("iolib.asdf.asd"
                     "iolib.conf.asd"
                     "iolib.common-lisp.asd"
                     "iolib.base.asd"
                     "iolib.asd")
       #:asd-systems '("iolib"
                       "iolib/os")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/syscalls/ffi-functions-unix.lisp"
               (("\\(:default \"libfixposix\"\\)")
                (string-append
                 "(:default \""
                 (assoc-ref inputs "libfixposix") "/lib/libfixposix\")")))
             ;; Socket tests need Internet access, disable them.
             (substitute* "iolib.asd"
               (("\\(:file \"sockets\" :depends-on \\(\"pkgdcl\" \"defsuites\"\\)\\)")
                "")))))))
    (synopsis "Common Lisp I/O library")
    (description "IOlib is to be a better and more modern I/O library than
the standard Common Lisp library.  It contains a socket library, a DNS
resolver, an I/O multiplexer(which supports @code{select(2)}, @code{epoll(4)}
and @code{kqueue(2)}), a pathname library and file-system utilities.")
    (license license:expat)))

(define-public cl-iolib
  (let ((parent (sbcl-package->cl-source-package sbcl-iolib)))
    (package
      (inherit parent)
      (propagated-inputs
       ;; Need header to compile.
       (modify-inputs (package-propagated-inputs parent)
         (prepend libfixposix))))))

(define-public ecl-iolib
  (sbcl-package->ecl-package sbcl-iolib))

(define-public sbcl-ieee-floats
  (let ((commit "566b51a005e81ff618554b9b2f0b795d3b29398d")
        (revision "1"))
    (package
      (name "sbcl-ieee-floats")
      (build-system asdf-build-system/sbcl)
      (version (git-version "20170924" revision commit))
      (home-page "https://github.com/marijnh/ieee-floats/")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1xyj49j9x3lc84cv3dhbf9ja34ywjk1c46dklx425fxw9mkwm83m"))))
      (native-inputs
       (list sbcl-fiveam))
      (synopsis "IEEE 754 binary representation for floats in Common Lisp")
      (description "This is a Common Lisp library that converts
floating point values to IEEE 754 binary representation.")
      (license license:bsd-3))))

(define-public cl-ieee-floats
  (sbcl-package->cl-source-package sbcl-ieee-floats))

(define-public ecl-ieee-floats
  (sbcl-package->ecl-package sbcl-ieee-floats))

(define sbcl-closure-common
  (let ((commit "e3c5f5f454b72b01b89115e581c3c52a7e201e5c")
        (revision "1"))
    (package
      (name "sbcl-closure-common")
      (build-system asdf-build-system/sbcl)
      (version (git-version "20101006" revision commit))
      (home-page "https://common-lisp.net/project/cxml/")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/closure-common")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0k5r2qxn122pxi301ijir3nayi9sg4d7yiy276l36qmzwhp4mg5n"))))
      (inputs
       `(("trivial-gray-streams" ,sbcl-trivial-gray-streams)
         ("babel" ,sbcl-babel)))
      (synopsis "Support Common Lisp library for CXML")
      (description "Closure-common is an internal helper library.  The name
Closure is a reference to the web browser it was originally written for.")
      ;; TODO: License?
      (license #f))))

(define-public sbcl-cxml
  (let ((commit "00b22bf4c4cf11c993d5866fae284f95ab18e6bf")
        (revision "1"))
    (package
      (name "sbcl-cxml")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/cxml")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "13kif7rf3gqdycsk9zq0d7y0g9y81krkl0z87k0p2fkbjfgrph37"))))
      (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-closure-common sbcl-puri sbcl-trivial-gray-streams))
    (synopsis "Common Lisp XML parser")
    (description "CXML implements a namespace-aware, validating XML 1.0
parser as well as the DOM Level 2 Core interfaces.  Two parser interfaces are
offered, one SAX-like, the other similar to StAX.")
    (home-page "https://common-lisp.net/project/cxml/")
    (license license:llgpl))))

(define-public cl-cxml
  (sbcl-package->cl-source-package sbcl-cxml))

(define-public ecl-cxml
  (sbcl-package->ecl-package sbcl-cxml))

(define-public sbcl-cxml-rng
  (let ((commit "bdcfeb92798694b2935a8321e641d8803e814b7b")
        (revision "1"))
    (package
      (name "sbcl-cxml-rng")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://www.lichteblau.com/git/cxml-rng.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rld038hmvm0whaffkszd5ks7mg44z1vfbgddal434df8sgspzql"))))
      (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-cxml sbcl-cl-ppcre sbcl-cl-yacc sbcl-parse-number
           sbcl-cl-base64))
    (synopsis "Relax NG for Closure XML (CXML)")
    (description "An implementation of Relax NG schema validation written in
Common Lisp, including support for compact syntax, DTD Compatibility, and the
XSD type library.")
    (home-page "http://www.lichteblau.com/cxml-rng/")
    (license license:bsd-2))))

(define-public cl-cxml-rng
  (sbcl-package->cl-source-package sbcl-cxml-rng))

;; FIXME: Fails on ECL with
;; In function STRUCTURE-SET, the value of the first argument is
;;   #<empty  0x7fffeb0fd440>
;; which is not of the expected type %TYPED-PATTERN.
;; (define-public ecl-cxml-rng
;;   (sbcl-package->ecl-package sbcl-cxml-rng))

(define-public sbcl-cl-reexport
  (let ((commit "312f3661bbe187b5f28536cd7ec2956e91366c3b")
        (revision "1"))
    (package
      (name "sbcl-cl-reexport")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/takagi/cl-reexport")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1cwpn1m3wrl0fl9plznn7p464db646gnfc8zkyk97dyxski2aq0x"))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)))
      (arguments
       ;; TODO: Tests fail because cl-test-more is missing, but I can't find it online.
       `(#:tests? #f))
      (synopsis "HTTP cookie manager for Common Lisp")
      (description "cl-cookie is a Common Lisp library featuring parsing of
cookie headers, cookie creation, cookie jar creation and more.")
      (license license:llgpl))))

(define-public cl-reexport
  (sbcl-package->cl-source-package sbcl-cl-reexport))

(define-public ecl-cl-reexport
  (sbcl-package->ecl-package sbcl-cl-reexport))

(define-public sbcl-cl-cookie
  (let ((commit "cea55aed8b9ad25fafd13defbcb9fe8f41b29546")
        (revision "1"))
    (package
      (name "sbcl-cl-cookie")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.9.10" revision commit))
      (home-page "https://github.com/fukamachi/cl-cookie")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "090g7z75h98zvc1ldx0vh4jn4086dhjm2w30jcwkq553qmyxwl8h"))))
      (inputs
       `(("proc-parse" ,sbcl-proc-parse)
         ("alexandria" ,sbcl-alexandria)
         ("quri" ,sbcl-quri)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("local-time" ,sbcl-local-time)))
      (native-inputs
       (list sbcl-prove))
      (arguments
       ;; TODO: Tests fail because cl-cookie depends on cl-cookie-test.
       `(#:tests? #f))
      (synopsis "HTTP cookie manager for Common Lisp")
      (description "cl-cookie is a Common Lisp library featuring parsing of
cookie headers, cookie creation, cookie jar creation and more.")
      (license license:bsd-2))))

(define-public cl-cookie
  (sbcl-package->cl-source-package sbcl-cl-cookie))

(define-public ecl-cl-cookie
  (sbcl-package->ecl-package sbcl-cl-cookie))

(define-public sbcl-dexador
  (let ((commit "953090f04c4d1a9ee6632b90133cdc297b68badc")
        (revision "1"))
    (package
      (name "sbcl-dexador")
      (build-system asdf-build-system/sbcl)
      (version "0.9.14" )
      (home-page "https://github.com/fukamachi/dexador")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0w18fz3301rpmwc3kwb810czcd24mbf7r1z8vdyc0v5crjfpw3mn"))))
      (inputs
       `(("trivial-gray-streams" ,sbcl-trivial-gray-streams)
         ("babel" ,sbcl-babel)
         ("usocket" ,sbcl-usocket)
         ("fast-http" ,sbcl-fast-http)
         ("quri" ,sbcl-quri)
         ("fast-io" ,sbcl-fast-io)
         ("chunga" ,sbcl-chunga)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("cl-cookie" ,sbcl-cl-cookie)
         ("trivial-mimes" ,sbcl-trivial-mimes)
         ("chipz" ,sbcl-chipz)
         ("cl-base64" ,sbcl-cl-base64)
         ("cl-reexport" ,sbcl-cl-reexport)
         ("cl+ssl" ,sbcl-cl+ssl)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("alexandria" ,sbcl-alexandria)))
      (native-inputs
       `(("prove" ,sbcl-prove)
         ("lack" ,sbcl-lack)
         ("clack" ,sbcl-clack)
         ("babel" ,sbcl-babel)
         ("alexandria" ,sbcl-alexandria)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("local-time" ,sbcl-local-time)
         ("trivial-features" ,sbcl-trivial-features)))
      (arguments
       ;; TODO: Circular dependency: tests depend on clack-test which depends on dexador.
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-permissions
             (lambda _ (make-file-writable "t/data/test.gz") #t)))))
      (synopsis "Yet another HTTP client for Common Lisp")
      (description "Dexador is yet another HTTP client for Common Lisp with
neat APIs and connection-pooling.  It is meant to supersede Drakma.")
      (license license:expat))))

(define-public cl-dexador
  (package
    (inherit (sbcl-package->cl-source-package sbcl-dexador))
    (arguments
     `(#:phases
       ;; asdf-build-system/source has its own phases and does not inherit
       ;; from asdf-build-system/sbcl phases.
       (modify-phases %standard-phases/source
         ;; Already done in SBCL package.
         (delete 'reset-gzip-timestamps))))))

(define-public ecl-dexador
  (sbcl-package->ecl-package sbcl-dexador))

(define-public sbcl-lisp-namespace
  (let ((commit "28107cafe34e4c1c67490fde60c7f92dc610b2e0")
        (revision "1"))
    (package
      (name "sbcl-lisp-namespace")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/guicho271828/lisp-namespace")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1jw2wykp06z2afb9nm1lgfzll5cjlj36pnknjx614057zkkxq4iy"))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)))
      (native-inputs
       (list sbcl-fiveam))
      (arguments
       `(#:test-asd-file "lisp-namespace.test.asd"
        ;; XXX: Component LISP-NAMESPACE-ASD::LISP-NAMESPACE.TEST not found
         #:tests? #f))
      (synopsis "LISP-N, or extensible namespaces in Common Lisp")
      (description "Common Lisp already has major 2 namespaces, function
namespace and value namespace (or variable namespace), but there are actually
more — e.g., class namespace.
This library offers macros to deal with symbols from any namespace.")
      (license license:llgpl))))

(define-public cl-lisp-namespace
  (sbcl-package->cl-source-package sbcl-lisp-namespace))

(define-public ecl-lisp-namespace
  (sbcl-package->ecl-package sbcl-lisp-namespace))

(define-public sbcl-trivial-cltl2
  (let ((commit "8a3bda30dc25d2f65fcf514d0eb6e6db75252c61")
        (revision "2"))
    (package
      (name "sbcl-trivial-cltl2")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.1.1" revision commit))
      (home-page "https://github.com/Zulu-Inuoe/trivial-cltl2")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "08cnzb9rnczn4pn2zpf0587ny4wjy1mjndy885fz9pw7xrlx37ip"))))
      (synopsis "Simple CLtL2 compatibility layer for Common Lisp")
      (description "This library is a portable compatibility layer around
\"Common Lisp the Language, 2nd
Edition\" (@url{https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node102.html})
and it exports symbols from implementation-specific packages.")
      (license license:llgpl))))

(define-public cl-trivial-cltl2
  (sbcl-package->cl-source-package sbcl-trivial-cltl2))

(define-public ecl-trivial-cltl2
  (sbcl-package->ecl-package sbcl-trivial-cltl2))

(define-public sbcl-introspect-environment
  (let ((commit "fff42f8f8fd0d99db5ad6c5812e53de7d660020b")
        (revision "1"))
    (package
      (name "sbcl-introspect-environment")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/Bike/introspect-environment")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1i305n0wfmpac63ni4i3vixnnkl8daw5ncxy0k3dv92krgx6qzhp"))))
      (native-inputs
       (list sbcl-fiveam))
      (synopsis "Common Lisp environment introspection portability layer")
      (description "This library is a small interface to portable but
nonstandard introspection of Common Lisp environments.  It is intended to
allow a bit more compile-time introspection of environments in Common Lisp.

Quite a bit of information is available at the time a macro or compiler-macro
runs; inlining info, type declarations, that sort of thing.  This information
is all standard - any Common Lisp program can @code{(declare (integer x))} and
such.

This info ought to be accessible through the standard @code{&environment}
parameters, but it is not.  Several implementations keep the information for
their own purposes but do not make it available to user programs, because
there is no standard mechanism to do so.

This library uses implementation-specific hooks to make information available
to users.  This is currently supported on SBCL, CCL, and CMUCL.  Other
implementations have implementations of the functions that do as much as they
can and/or provide reasonable defaults.")
      (license license:wtfpl2))))

(define-public cl-introspect-environment
  (sbcl-package->cl-source-package sbcl-introspect-environment))

(define-public ecl-introspect-environment
  (sbcl-package->ecl-package sbcl-introspect-environment))

(define-public sbcl-type-i
  (let ((commit "d34440ab4ebf5a46a58deccb35950b15670e3667")
        (revision "2"))
    (package
      (name "sbcl-type-i")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/guicho271828/type-i")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "12wsga0pwjkkr176lnjwkmmlm3ccp0n310sjj9h20lk53iyd0z69"))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("introspect-environment" ,sbcl-introspect-environment)
         ("trivia.trivial" ,sbcl-trivia.trivial)))
      (native-inputs
       (list sbcl-fiveam))
      (arguments
       `(#:test-asd-file "type-i.test.asd"))
      (synopsis "Type inference utility on unary predicates for Common Lisp")
      (description "This library tries to provide a way to detect what kind of
type the given predicate is trying to check.  This is different from inferring
the return type of a function.")
      (license license:llgpl))))

(define-public cl-type-i
  (sbcl-package->cl-source-package sbcl-type-i))

(define-public ecl-type-i
  (package
    (inherit (sbcl-package->ecl-package sbcl-type-i))
    (arguments
     ;; The tests get stuck indefinitly
     '(#:tests? #f))))

(define-public sbcl-optima
  (let ((commit "373b245b928c1a5cce91a6cb5bfe5dd77eb36195")
        (revision "1"))
    (package
      (name "sbcl-optima")
      (build-system asdf-build-system/sbcl)
      (version (git-version "1.0" revision commit))
      (home-page "https://github.com/m2ym/optima")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1yw4ymq7ms89342kkvb3aqxgv0w38m9kd8ikdqxxzyybnkjhndal"))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("closer-mop" ,sbcl-closer-mop)))
      (native-inputs
       (list sbcl-eos))
      (arguments
       ;; XXX: Circular dependencies: tests depend on optima.ppcre which depends on optima.
       `(#:tests? #f
         #:test-asd-file "optima.test.asd"))
      (synopsis "Optimized pattern matching library for Common Lisp")
      (description "Optima is a fast pattern matching library which uses
optimizing techniques widely used in the functional programming world.")
      (license license:expat))))

(define-public cl-optima
  (sbcl-package->cl-source-package sbcl-optima))

(define-public ecl-optima
  (sbcl-package->ecl-package sbcl-optima))

(define-public sbcl-fare-quasiquote
  (let ((commit "640d39a0451094071b3e093c97667b3947f43639")
        (revision "1"))
    (package
      (name "sbcl-fare-quasiquote")
      (build-system asdf-build-system/sbcl)
      (version (git-version "1.0.1" revision commit))
      (home-page "https://gitlab.common-lisp.net/frideau/fare-quasiquote")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url (string-append "https://gitlab.common-lisp.net/frideau/"
                                   "fare-quasiquote.git"))
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1g6q11l50kgija9f55lzqpcwvaq0ljiw8v1j265hnyg6nahjwjvg"))))
      (inputs
       `(("fare-utils" ,sbcl-fare-utils)
         ("named-readtables" ,sbcl-named-readtables)
         ("optima" ,sbcl-optima)))
      (arguments
       ;; XXX: Circular dependencies: Tests depend on subsystems,
       ;; which depend on the main systems.
       `(#:tests? #f
         #:asd-systems '("fare-quasiquote"
                         "fare-quasiquote-extras")
         #:phases
         (modify-phases %standard-phases
           ;; XXX: Require 1.0.0 version of fare-utils, and we package some
           ;; commits after 1.0.0.5, but ASDF fails to read the
           ;; "-REVISION-COMMIT" part generated by Guix.
           (add-after 'unpack 'patch-requirement
             (lambda _
               (substitute* "fare-quasiquote.asd"
                 (("\\(:version \"fare-utils\" \"1.0.0\"\\)")
                  "\"fare-utils\""))
               (substitute* "fare-quasiquote-optima.asd"
                 (("\\(:version \"optima\" \"1\\.0\"\\)")
                  "\"optima\""))
               #t)))))
      (synopsis "Pattern-matching friendly implementation of quasiquote")
      (description "The main purpose of this n+2nd reimplementation of
quasiquote is enable matching of quasiquoted patterns, using Optima or
Trivia.")
      (license license:expat))))

(define-public cl-fare-quasiquote
  (sbcl-package->cl-source-package sbcl-fare-quasiquote))

(define-public ecl-fare-quasiquote
  (sbcl-package->ecl-package sbcl-fare-quasiquote))

;;; Split the trivia package in two to work around the circular dependency
;;; between guicho271828/trivia and guicho271828/type-i.
(define-public sbcl-trivia.trivial
  (let ((commit "7286d5d2a4f685f1cac8370816f95276c0851111")
        (revision "3"))
    (package
      (name "sbcl-trivia.trivial")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/guicho271828/trivia")
               (commit commit)))
         (file-name (git-file-name "trivia" version))
         (sha256
          (base32
           "0ln0sj3jry7kzbmxhnin66kpbqan1wp8wwgdbw4k29afbdblkcca"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("closer-mop" ,sbcl-closer-mop)
         ("lisp-namespace" ,sbcl-lisp-namespace)
         ("trivial-cltl2" ,sbcl-trivial-cltl2)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-build
             (lambda _
               (for-each delete-file
                         '("trivia.balland2006.asd"
                           "trivia.ppcre.asd"
                           "trivia.quasiquote.asd"
                           "trivia.cffi.asd"
                           "trivia.asd"
                           "trivia.test.asd"))
               #t)))))
      (synopsis "Pattern matching in Common Lisp")
      (description "Trivia is a pattern matching compiler that is compatible
with Optima, another pattern matching library for Common Lisp.  It is meant to
be faster and more extensible than Optima.")
      (home-page "https://github.com/guicho271828/trivia")
      (license license:llgpl))))

(define-public cl-trivia.trivial
  (sbcl-package->cl-source-package sbcl-trivia.trivial))

(define-public ecl-trivia.trivial
  (sbcl-package->ecl-package sbcl-trivia.trivial))

(define-public sbcl-trivia
  (package
    (inherit sbcl-trivia.trivial)
    (name "sbcl-trivia")
    (native-inputs
     `(("fiveam" ,sbcl-fiveam)
       ("optima" ,sbcl-optima)))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cffi" ,sbcl-cffi)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("fare-quasiquote" ,sbcl-fare-quasiquote)
       ("iterate" ,sbcl-iterate)
       ("trivia.trivial" ,sbcl-trivia.trivial)
       ("type-i" ,sbcl-type-i)))
    (arguments
     '(#:asd-systems '("trivia"
                       "trivia.ppcre"
                       "trivia.quasiquote"
                       "trivia.cffi")
       #:test-asd-file "trivia.test.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build
           (lambda _
             (for-each delete-file
                       '("trivia.level0.asd"
                         "trivia.level1.asd"
                         "trivia.level2.asd"
                         "trivia.trivial.asd"))
             #t)))))))

(define-public cl-trivia
  (sbcl-package->cl-source-package sbcl-trivia))

(define-public ecl-trivia
  (sbcl-package->ecl-package sbcl-trivia))

(define-public sbcl-mk-string-metrics
  (package
    (name "sbcl-mk-string-metrics")
    (version "0.1.2")
    (home-page "https://github.com/cbaggers/mk-string-metrics/")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (sha256
               (base32 "0bg0bv2mfd4k0g3x72x563hvmrx18xavaffr6xk5rh4if5j7kcf6"))
              (file-name (git-file-name name version))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Calculate various string metrics efficiently in Common Lisp")
    (description "This library implements efficient algorithms that calculate
various string metrics in Common Lisp:

@itemize
@item Damerau-Levenshtein distance
@item Hamming distance
@item Jaccard similarity coefficient
@item Jaro distance
@item Jaro-Winkler distance
@item Levenshtein distance
@item Normalized Damerau-Levenshtein distance
@item Normalized Levenshtein distance
@item Overlap coefficient
@end itemize\n")
    (license license:x11)))

(define-public cl-mk-string-metrics
  (sbcl-package->cl-source-package sbcl-mk-string-metrics))

(define-public ecl-mk-string-metrics
  (sbcl-package->ecl-package sbcl-mk-string-metrics))

(define-public sbcl-cl-str
  (package
    (name "sbcl-cl-str")
    (version "0.19")
    (home-page "https://github.com/vindarel/cl-str")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (sha256
               (base32 "1jyza2jhn7w6fl4w87pv0m87z5ia48m6dqw12k0mdh7l3mgjq839"))
              (file-name (git-file-name name version))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("cl-ppcre" ,sbcl-cl-ppcre)
       ("cl-ppcre-unicode" ,sbcl-cl-ppcre-unicode)
       ("cl-change-case" ,sbcl-cl-change-case)))
    (native-inputs
     (list sbcl-prove))
    (arguments
     `(#:asd-systems '("str")
       #:test-asd-file "str.test.asd"))
    (synopsis "Modern, consistent and terse Common Lisp string manipulation library")
    (description "A modern and consistent Common Lisp string manipulation
library that focuses on modernity, simplicity and discoverability:
@code{(str:trim s)} instead of @code{(string-trim '(#\\Space ...) s)}), or
@code{str:concat strings} instead of an unusual format construct; one
discoverable library instead of many; consistency and composability, where
@code{s} is always the last argument, which makes it easier to feed pipes and
arrows.")
    (license license:expat)))

(define-public cl-str
  (sbcl-package->cl-source-package sbcl-cl-str))

(define-public ecl-cl-str
  (sbcl-package->ecl-package sbcl-cl-str))

(define-public sbcl-cl-xmlspam
  (let ((commit "ea06abcca2a73a9779bcfb09081e56665f94e22a"))
    (package
      (name "sbcl-cl-xmlspam")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.0.0" "1" commit))
      (home-page "https://github.com/rogpeppe/cl-xmlspam")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (string-append name "-" version))
         (sha256
          (base32
           "0w4rqvrgdgk3fwfq3kx4r7wwdr2bv3b6n3bdqwsiriw9psqzpz2s"))))
      (inputs
       (list sbcl-cxml sbcl-cl-ppcre))
      (synopsis "Concise, regexp-like pattern matching on streaming XML for Common Lisp")
      (description "CXML does an excellent job at parsing XML elements, but what
do you do when you have a XML file that's larger than you want to fit in
memory, and you want to extract some information from it?  Writing code to deal
with SAX events, or even using Klacks, quickly becomes tedious.
@code{cl-xmlspam} (for XML Stream PAttern Matcher) is designed to make it easy
to write code that mirrors the structure of the XML that it's parsing.  It
also makes it easy to shift paradigms when necessary - the usual Lisp control
constructs can be used interchangeably with pattern matching, and the full
power of CXML is available when necessary.")
      (license license:bsd-3))))

(define-public cl-xmlspam
  (sbcl-package->cl-source-package sbcl-cl-xmlspam))

(define-public ecl-cl-xmlspam
  (sbcl-package->ecl-package sbcl-cl-xmlspam))

(define-public sbcl-dbus
  (let ((commit "24b452df3a45ca5dc95015500f34baad175c981a")
        (revision "1"))
    (package
      (name "sbcl-dbus")
      (version (git-version "20190408" revision commit))
      (home-page "https://github.com/death/dbus")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0fw2q866yddbf23nk9pxphm9gsasx35vjyss82xzvndnjmzlqfl5"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria
             sbcl-trivial-garbage
             sbcl-babel
             sbcl-iolib
             sbcl-ieee-floats
             sbcl-flexi-streams
             sbcl-cl-xmlspam
             sbcl-ironclad))
      (synopsis "D-Bus client library for Common Lisp")
      (description "This is a Common Lisp library that publishes D-Bus
objects as well as send and notify other objects connected to a bus.")
      (license license:bsd-2))))

(define-public cl-dbus
  (sbcl-package->cl-source-package sbcl-dbus))

(define-public ecl-dbus
  (sbcl-package->ecl-package sbcl-dbus))

(define-public sbcl-cl-hooks
  (let ((commit "5b638083f3b4f1221a52631d9c8a0a265565cac7")
        (revision "1"))
    (package
      (name "sbcl-cl-hooks")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.2.1" revision commit))
      (home-page "https://github.com/scymtym/architecture.hooks")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0bg3l0a28lw5gqqjp6p6b5nhwqk46sgkb7184w5qbfngw1hk8x9y"))))
      (inputs
       (list sbcl-alexandria sbcl-let-plus sbcl-trivial-garbage
             sbcl-closer-mop))
      (native-inputs
       (list sbcl-fiveam))
      (synopsis "Hooks extension point mechanism (as in Emacs) for Common Lisp")
      (description "A hook, in the present context, is a certain kind of
extension point in a program that allows interleaving the execution of
arbitrary code with the execution of a the program without introducing any
coupling between the two.  Hooks are used extensively in the extensible editor
Emacs.

In the Common LISP Object System (CLOS), a similar kind of extensibility is
possible using the flexible multi-method dispatch mechanism.  It may even seem
that the concept of hooks does not provide any benefits over the possibilities
of CLOS.  However, there are some differences:

@itemize

@item There can be only one method for each combination of specializers and
qualifiers.  As a result this kind of extension point cannot be used by
multiple extensions independently.
@item Removing code previously attached via a @code{:before}, @code{:after} or
@code{:around} method can be cumbersome.
@item There could be other or even multiple extension points besides @code{:before}
and @code{:after} in a single method.
@item Attaching codes to individual objects using eql specializers can be
cumbersome.
@item Introspection of code attached a particular extension point is
cumbersome since this requires enumerating and inspecting the methods of a
generic function.
@end itemize

This library tries to complement some of these weaknesses of method-based
extension-points via the concept of hooks.")
      (license license:llgpl))))

(define-public cl-hooks
  (sbcl-package->cl-source-package sbcl-cl-hooks))

(define-public ecl-cl-hooks
  (sbcl-package->ecl-package sbcl-cl-hooks))

(define-public sbcl-cl-autowrap
  (let ((revision "2")
        (commit "a5d71ebd7c21b87f449db1e16ab815750d7c0ea4"))
    ;; no taged branches
    (package
      (name "sbcl-cl-autowrap")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rpav/cl-autowrap")
               (commit commit)))
         (file-name (git-file-name "cl-autowrap" version))
         (sha256
          (base32 "0795c817m1c41cz3ywzzg83z4pgkxdg6si553pay9mdgjvmrwmaw"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems '("cl-plus-c" "cl-autowrap")))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("cl-json" ,sbcl-cl-json)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("defpackage-plus" ,sbcl-defpackage-plus)
         ("trivial-features" ,sbcl-trivial-features)))
      (home-page "https://github.com/rpav/cl-autowrap")
      (synopsis "FFI wrapper generator for Common Lisp")
      (description "This is a c2ffi-based wrapper generator for Common Lisp.")
      (license license:bsd-2))))

(define-public cl-autowrap
  (sbcl-package->cl-source-package sbcl-cl-autowrap))

(define-public ecl-cl-autowrap
  (sbcl-package->ecl-package sbcl-cl-autowrap))

(define-public sbcl-s-sysdeps
  ;; No release since 2013.
  (let ((commit "9aa23bbdceb24bcdbe0e7c39fa1901858f823106")
        (revision "2"))
    (package
      (name "sbcl-s-sysdeps")
      (build-system asdf-build-system/sbcl)
      (version (git-version "1" revision commit))
      (home-page "https://github.com/svenvc/s-sysdeps")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1fh8r7kf8s3hvqdg6b71b8p7w3v2kkga9bw8j3qqdxhzr6anpm0b"))))
      (inputs
       (list sbcl-bordeaux-threads sbcl-usocket))
      (synopsis "Common Lisp abstraction layer over platform dependent functionality")
      (description "@code{s-sysdeps} is an abstraction layer over platform
dependent functionality.  This simple package is used as a building block in a
number of other open source projects.

@code{s-sysdeps} abstracts:

@itemize
@item managing processes,
@item implementing a standard TCP/IP server,
@item opening a client TCP/IP socket stream,
@item working with process locks.
@end itemize\n")
      (license license:llgpl))))

(define-public cl-s-sysdeps
  (sbcl-package->cl-source-package sbcl-s-sysdeps))

(define-public ecl-s-sysdeps
  (sbcl-package->ecl-package sbcl-s-sysdeps))

(define-public sbcl-cl-prevalence
  (let ((commit "5a76be036092ed6c18cb695a9e03bce87e21b840")
        (revision "4"))
    (package
      (name "sbcl-cl-prevalence")
      (build-system asdf-build-system/sbcl)
      (version (git-version "5" revision commit))
      (home-page "https://github.com/40ants/cl-prevalence")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "050h6hwv8f16b5v6fzba8zmih92hgaaq27i2x9wv1iib41gbia3r"))))
      (inputs
       (list sbcl-s-sysdeps sbcl-s-xml))
      (native-inputs
       (list sbcl-fiveam))
      (synopsis "Implementation of object prevalence for Common Lisp")
      (description "This Common Lisp library implements object prevalence (see
@url{https://en.wikipedia.org/wiki/System_prevalence}).  It allows
for (de)serializing to and from s-exps as well as XML.  Serialization of arbitrary
classes and cyclic data structures are supported.")
      (license license:llgpl))))

(define-public cl-prevalence
  (sbcl-package->cl-source-package sbcl-cl-prevalence))

(define-public ecl-cl-prevalence
  (sbcl-package->ecl-package sbcl-cl-prevalence))

(define-public sbcl-series
  (let ((commit "da9061b336119d1e5214aff9117171d494d5a58a")
        (revision "1"))
    (package
      (name "sbcl-series")
      (version (git-version "2.2.11" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://git.code.sf.net/p/series/series")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "07hk2lhfx42zk018pxqvn4gs77vd4n4g8m4xxbqaxgca76mifwfw"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Disable the tests, they are apparently buggy and I didn't find
       ;; a simple way to make them run and pass.
       '(#:tests? #f))
      (synopsis "Series data structure for Common Lisp")
      (description
       "This Common Lisp library provides a series data structure much like
a sequence, with similar kinds of operations.  The difference is that in many
situations, operations on series may be composed functionally and yet execute
iteratively, without the need to construct intermediate series values
explicitly.  In this manner, series provide both the clarity of a functional
programming style and the efficiency of an iterative programming style.")
      (home-page "http://series.sourceforge.net/")
      (license license:expat))))

(define-public cl-series
  (sbcl-package->cl-source-package sbcl-series))

(define-public ecl-series
  (sbcl-package->ecl-package sbcl-series))

(define-public sbcl-periods
  (let ((commit "60383dcef88a1ac11f82804ae7a33c361dcd2949")
        (revision "2"))
    (package
      (name "sbcl-periods")
      (version (git-version "0.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jwiegley/periods")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1ym2j4an9ig2hl210jg91gpf7xfnp6mlhkw3n9kkdnwiji3ipqlk"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("local-time" ,sbcl-local-time)
         ("series" ,sbcl-series)))
      (arguments
       '(#:asd-systems '("periods"
                         "periods-series")))
      (synopsis "Common Lisp library for manipulating date/time objects")
      (description
       "Periods is a Common Lisp library providing a set of utilities for
manipulating times, distances between times, and both contiguous and
discontiguous ranges of time.")
      (home-page "https://github.com/jwiegley/periods")
      (license license:bsd-3))))

(define-public cl-periods
  (sbcl-package->cl-source-package sbcl-periods))

(define-public ecl-periods
  (sbcl-package->ecl-package sbcl-periods))

(define-public sbcl-metatilities-base
  (let ((commit "6eaa9e3ff0939a93a92109dd0fcd218de85417d5")
        (revision "1"))
    (package
      (name "sbcl-metatilities-base")
      (version (git-version "0.6.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gwkkwg/metatilities-base")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xpa86pdzlnf4v5g64j3ifaplx71sx2ha8b7vvakswi652679ma0"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-lift))
      (synopsis "Core of the metatilities Common Lisp library")
      (description
       "Metatilities-base is the core of the metatilities Common Lisp library
which implements a set of utilities.")
      (home-page "https://common-lisp.net/project/metatilities-base/")
      (license license:expat))))

(define-public cl-metatilities-base
  (sbcl-package->cl-source-package sbcl-metatilities-base))

(define-public ecl-metatilities-base
  (sbcl-package->ecl-package sbcl-metatilities-base))

(define-public sbcl-cl-containers
  (let ((commit "3d1df53c22403121bffb5d553cf7acb1503850e7")
        (revision "3"))
    (package
      (name "sbcl-cl-containers")
      (version (git-version "0.12.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gwkkwg/cl-containers")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "18s6jfq11n8nv9k4biz32pm1s7y9zl054ry1gmdbcf39nisy377y"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-lift))
      (inputs
       `(("metatilities-base" ,sbcl-metatilities-base)))
      (arguments
       '(#:asd-files '("cl-containers.asd")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'relax-version-checks
             (lambda _
               (substitute* "cl-containers.asd"
                 (("\\(:version \"metatilities-base\" \"0\\.6\\.6\"\\)")
                  "\"metatilities-base\""))
               (substitute* "cl-containers-test.asd"
                 (("\\(:version \"lift\" \"1\\.7\\.0\"\\)")
                  "\"lift\""))
               #t)))))
      (synopsis "Container library for Common Lisp")
      (description
       "Common Lisp ships with a set of powerful built in data structures
including the venerable list, full featured arrays, and hash-tables.
CL-containers enhances and builds on these structures by adding containers
that are not available in native Lisp (for example: binary search trees,
red-black trees, sparse arrays and so on), and by providing a standard
interface so that they are simpler to use and so that changing design
decisions becomes significantly easier.")
      (home-page "https://common-lisp.net/project/cl-containers/")
      (license license:expat))))

(define-public cl-containers
  (sbcl-package->cl-source-package sbcl-cl-containers))

(define-public ecl-cl-containers
  (sbcl-package->ecl-package sbcl-cl-containers))

(define-public sbcl-cambl
  (let ((commit "7016d1a98215f82605d1c158e7a16504ca1f4636")
        (revision "1"))
    (package
      (name "sbcl-cambl")
      (version (git-version "4.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jwiegley/cambl")
               (commit commit)))
         (file-name (git-file-name "cambl" version))
         (sha256
          (base32 "103mry04j2k9vznsxm7wcvccgxkil92cdrv52miwcmxl8daa4jiz"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-xlunit))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-containers" ,sbcl-cl-containers)
         ("local-time" ,sbcl-local-time)
         ("periods" ,sbcl-periods)))
      (arguments
       '(#:asd-files '("fprog.asd"
                       "cambl.asd")))
      (synopsis "Commoditized amounts and balances for Common Lisp")
      (description
       "CAMBL is a Common Lisp library providing a convenient facility for
working with commoditized values.  It does not allow compound units (and so is
not suited for scientific operations) but does work rather nicely for the
purpose of financial calculations.")
      (home-page "https://github.com/jwiegley/cambl")
      (license license:bsd-3))))

(define-public cl-cambl
  (sbcl-package->cl-source-package sbcl-cambl))

(define-public ecl-cambl
  (sbcl-package->ecl-package sbcl-cambl))

(define-public sbcl-cl-ledger
  (let ((commit "08e0be41795e804cd36142e51756ad0b1caa377b")
        (revision "1"))
    (package
      (name "sbcl-cl-ledger")
      (version (git-version "4.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ledger/cl-ledger")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1via0qf6wjcyxnfbmfxjvms0ik9j8rqbifgpmnhrzvkhrq9pv8h1"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cambl" ,sbcl-cambl)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("local-time" ,sbcl-local-time)
         ("periods" ,sbcl-periods)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-system-definition
             (lambda _
               (substitute* "cl-ledger.asd"
                 (("  :build-operation program-op") "")
                 (("  :build-pathname \"cl-ledger\"") "")
                 (("  :entry-point \"ledger::main\"") ""))
               #t)))))
      (synopsis "Common Lisp port of the Ledger accounting system")
      (description
       "CL-Ledger is a Common Lisp port of the Ledger double-entry accounting
system.")
      (home-page "https://github.com/ledger/cl-ledger")
      (license license:bsd-3))))

(define-public cl-ledger
  (sbcl-package->cl-source-package sbcl-cl-ledger))

(define-public ecl-cl-ledger
  (sbcl-package->ecl-package sbcl-cl-ledger))

(define-public sbcl-bst
  (let ((commit "8545aed0d504df2829ad139566feeabe22305388")
        (revision "0"))
    (package
      (name "sbcl-bst")
      (version (git-version "2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/bst")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "18ig7rvxcra69437g0i8sxyv7c5dg26jqnx1rc2f9pxmihdprgk8"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-alexandria sbcl-fiveam))
      (synopsis "Binary search tree for Common Lisp")
      (description
       "BST is a Common Lisp library for working with binary search trees that
can contain any kind of values.")
      (home-page "https://github.com/glv2/bst")
      (license license:gpl3))))

(define-public cl-bst
  (sbcl-package->cl-source-package sbcl-bst))

(define-public ecl-bst
  (sbcl-package->ecl-package sbcl-bst))

(define-public sbcl-cl-octet-streams
  (package
    (name "sbcl-cl-octet-streams")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/glv2/cl-octet-streams")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hffh98bv4w5yrchagzwqrc43d2p473pvw7ka4kyyvhrr52dk2f8"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-fiveam))
    (inputs
     (list sbcl-trivial-gray-streams))
    (synopsis "In-memory octet streams for Common Lisp")
    (description
     "CL-octet-streams is a library implementing in-memory octet
streams for Common Lisp.  It was inspired by the trivial-octet-streams and
cl-plumbing libraries.")
    (home-page "https://github.com/glv2/cl-octet-streams")
    (license license:gpl3+)))

(define-public cl-octet-streams
  (sbcl-package->cl-source-package sbcl-cl-octet-streams))

(define-public ecl-cl-octet-streams
  (sbcl-package->ecl-package sbcl-cl-octet-streams))

(define-public sbcl-lzlib
  (let ((commit "cad10f5becbcfebb44b9d311a257563778803452")
        (revision "2"))
    (package
      (name "sbcl-lzlib")
      (version (git-version "1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/cl-lzlib")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "09lp7li35h4jkls0448fj1sh6pjslr1w7ranbc4szjr8g0c2bdry"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("cl-octet-streams" ,sbcl-cl-octet-streams)
         ("lparallel" ,sbcl-lparallel)
         ("lzlib" ,lzlib)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/lzlib.lisp"
                 (("liblz\\.so")
                  (search-input-file inputs "/lib/liblz.so")))
               #t)))))
      (synopsis "Common Lisp library for lzip (de)compression")
      (description
       "This Common Lisp library provides functions for lzip (LZMA)
compression/decompression using bindings to the lzlib C library.")
      (home-page "https://github.com/glv2/cl-lzlib")
      (license license:gpl3+))))

(define-public cl-lzlib
  (sbcl-package->cl-source-package sbcl-lzlib))

(define-public ecl-lzlib
  (sbcl-package->ecl-package sbcl-lzlib))

(define-public sbcl-chanl
  (let ((commit "56e90a126c78b39bb621a01585e8d3b985238e8c")
        (revision "1"))
    (package
      (name "sbcl-chanl")
      (version (git-version "0.4.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zkat/chanl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0b1cf6c12qx5cy1fw2z42jgh566rp3l8nv5qf0qqc569s7bgmrh4"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-bordeaux-threads))
      (synopsis "Portable channel-based concurrency for Common Lisp")
      (description "Common Lisp library for channel-based concurrency.  In
a nutshell, you create various threads sequentially executing tasks you need
done, and use channel objects to communicate and synchronize the state of these
threads.")
      (home-page "https://github.com/zkat/chanl")
      (license (list license:expat license:bsd-3)))))

(define-public cl-chanl
  (sbcl-package->cl-source-package sbcl-chanl))

(define-public ecl-chanl
  (sbcl-package->ecl-package sbcl-chanl))

(define-public sbcl-cl-store
  (let ((commit "c787337a16ea8cf8a06227f35933a4ec774746b3")
        (revision "1"))
    (package
      (name "sbcl-cl-store")
      (version (git-version "0.8.11" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/skypher/cl-store")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "194srkg8nrym19c6i7zbnkzshc1qhqa82m53qnkirz9fw928bqxr"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-rt))
      (synopsis "Common Lisp library to serialize data")
      (description
       "CL-STORE is a portable serialization package which should give you the
ability to store all Common Lisp data types into streams.")
      (home-page "https://www.common-lisp.net/project/cl-store/")
      (license license:expat))))

(define-public cl-store
  (sbcl-package->cl-source-package sbcl-cl-store))

(define-public ecl-cl-store
  (sbcl-package->ecl-package sbcl-cl-store))

(define-public sbcl-specialization-store
  (let ((commit "8d39a866a6f24986aad3cc52349e9cb2653496f3")
        (revision "1"))
    (package
      (name "sbcl-specialization-store")
      (version (git-version "0.0.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/markcox80/specialization-store")
               (commit commit)))
         (file-name (git-file-name "specialization-store" version))
         (sha256
          (base32 "0r0bgb46q4gy72l78s7djkxq8ibb4bb3yh9brsry5lih7br8lhi0"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-alexandria sbcl-introspect-environment))
      (home-page "https://github.com/markcox80/specialization-store")
      (synopsis "Different type of generic function for Common Lisp")
      (description
       "SPECIALIZATION-STORE system provides a new kind of function, called
a store function, whose behavior depends on the types of objects passed to the
function.")
      (license license:bsd-2))))

(define-public ecl-specialization-store
  (package
    (inherit (sbcl-package->ecl-package sbcl-specialization-store))
    (arguments
     ;; TODO: Find why the tests get stuck forever; disable them for now.
     `(#:tests? #f))))

(define-public cl-specialization-store
  (sbcl-package->cl-source-package sbcl-specialization-store))

(define-public sbcl-cl-gobject-introspection
  (let ((commit "d0136c8d9ade2560123af1fc55bbf70d2e3db539")
        (revision "1"))
    (package
      (name "sbcl-cl-gobject-introspection")
      (version (git-version "0.3" revision commit))
      (home-page "https://github.com/andy128k/cl-gobject-introspection")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0dz0r73pq7yhz2iq2jnkq977awx2zws2qfxdcy33329sys1ii32p"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("iterate" ,sbcl-iterate)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("glib" ,glib)
         ("gobject-introspection" ,gobject-introspection)))
      (native-inputs
       (list sbcl-fiveam))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/init.lisp"
                 (("libgobject-2\\.0\\.so")
                  (search-input-file inputs "/lib/libgobject-2.0.so"))
                 (("libgirepository-1\\.0\\.so")
                  (search-input-file inputs
                                     "/lib/libgirepository-1.0.so"))))))))
      (synopsis "Common Lisp bindings to GObject Introspection")
      (description
       "This library is a bridge between Common Lisp and GObject
Introspection, which enables Common Lisp programs to access the full interface
of C+GObject libraries without the need of writing dedicated bindings.")
      (license (list license:bsd-3
                     ;; Tests are under a different license.
                     license:llgpl)))))

(define-public cl-gobject-introspection
  (sbcl-package->cl-source-package sbcl-cl-gobject-introspection))

(define-public ecl-cl-gobject-introspection
  (sbcl-package->ecl-package sbcl-cl-gobject-introspection))

(define-public sbcl-cl-slug
  (let ((commit "ffb229d10f0d3f7f54e706791725225e200bf749")
        (revision "1"))
    (package
      (name "sbcl-cl-slug")
      (version (git-version "0.4.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/EuAndreh/cl-slug")
               (commit commit)))
         (file-name (git-file-name "cl-slug" version))
         (sha256
          (base32 "1asdq6xllmsvfw5fky9wblqcx9isac9jrrlkfl7vyxcq1wxrnflx"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-files '("cl-slug-test.asd" "cl-slug.asd")
         #:asd-systems '("cl-slug-test" "cl-slug")))
      (native-inputs
       (list sbcl-prove))
      (inputs
       `(("ppcre" ,sbcl-cl-ppcre)))
      (home-page "https://github.com/EuAndreh/cl-slug")
      (synopsis "Multi-language slug formatter")
      (description
       "This is a small Common Lisp library to make slugs, mainly for URIs,
from English and beyond.")
      (license license:llgpl))))

(define-public ecl-cl-slug
  (sbcl-package->ecl-package sbcl-cl-slug))

(define-public cl-slug
  (sbcl-package->cl-source-package sbcl-cl-slug))

(define-public sbcl-string-case
  (let ((commit "718c761e33749e297cd2809c7ba3ade1985c49f7")
        (revision "0"))
    (package
      (name "sbcl-string-case")
      (version (git-version "0.0.2" revision commit))
      (home-page "https://github.com/pkhuong/string-case")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1n5i3yh0h5s636rcnwn7jwqy3rjflikra04lymimhpcshhjsk0md"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Efficient string= case in Common Lisp")
      (description
       "@code{string-case} is a Common Lisp macro that generates specialised decision
trees to dispatch on string equality.")
      (license license:bsd-3))))

(define-public cl-string-case
  (sbcl-package->cl-source-package sbcl-string-case))

(define-public ecl-string-case
  (sbcl-package->ecl-package sbcl-string-case))

(define-public sbcl-garbage-pools
  (let ((commit "9a7cb7f48b04197c0495df3b6d2e8395ad13f790")
        (revision "1"))
    (package
      (name "sbcl-garbage-pools")
      (version (git-version "0.1.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/archimag/garbage-pools")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "04jqwr6j138him6wc4nrwjzm4lvyj5j31xqab02nkf8h9hmsf5v1"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/archimag/garbage-pools")
      (synopsis "Resource management pools for Common Lisp")
      (description "GARBAGE-POOLS is Common Lisp re-implementation of the APR
Pools for resource management.")
      (license license:expat))))

(define-public ecl-garbage-pools
  (sbcl-package->ecl-package sbcl-garbage-pools))

(define-public cl-garbage-pools
  (sbcl-package->cl-source-package sbcl-garbage-pools))

(define-public sbcl-global-vars
  (let ((commit "c749f32c9b606a1457daa47d59630708ac0c266e")
        (revision "0"))
    (package
      (name "sbcl-global-vars")
      (version (git-version "1.0.0" revision commit))
      (home-page "https://github.com/lmj/global-vars")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "06m3xc8l3pgsapl8fvsi9wf6y46zs75cp9zn7zh6dc65v4s5wz3d"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Efficient global variables in Common Lisp")
      (description
       "In Common Lisp, a special variable that is never dynamically bound
typically serves as a stand-in for a global variable.  The @code{global-vars}
library provides true global variables that are implemented by some compilers.
An attempt to rebind a global variable properly results in a compiler error.
That is, a global variable cannot be dynamically bound.

Global variables therefore allow us to communicate an intended usage that
differs from special variables.  Global variables are also more efficient than
special variables, especially in the presence of threads.")
      (license license:expat))))

(define-public cl-global-vars
  (sbcl-package->cl-source-package sbcl-global-vars))

(define-public ecl-global-vars
  (sbcl-package->ecl-package sbcl-global-vars))

(define-public sbcl-trivial-file-size
  (let ((commit "1c1d672a01a446ba0391dbb4ffc40be3b0476f23")
        (revision "0"))
    (package
      (name "sbcl-trivial-file-size")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/ruricolist/trivial-file-size")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "17pp86c9zs4y7i1sh7q9gbfw9iqv6655k7fz8qbj9ly1ypgxp4qs"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (synopsis "Size of a file in bytes in Common Lisp")
      (description
       "The canonical way to determine the size of a file in bytes, using Common Lisp,
is to open the file with an element type of (unsigned-byte 8) and then
calculate the length of the stream.  This is less than ideal.  In most cases
it is better to get the size of the file from its metadata, using a system
call.

This library exports a single function, file-size-in-octets.  It returns the
size of a file in bytes, using system calls when possible.")
      (license license:expat))))

(define-public cl-trivial-file-size
  (sbcl-package->cl-source-package sbcl-trivial-file-size))

(define-public ecl-trivial-file-size
  (sbcl-package->ecl-package sbcl-trivial-file-size))

(define-public sbcl-trivial-macroexpand-all
  (let ((commit "933270ac7107477de1bc92c1fd641fe646a7a8a9")
        (revision "0"))
    (package
      (name "sbcl-trivial-macroexpand-all")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/cbaggers/trivial-macroexpand-all")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "191hnn4b5j4i3crydmlzbm231kj0h7l8zj6mzj69r1npbzkas4bd"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (synopsis "Portable macroexpand-all for Common Lisp")
      (description
       "This library provides a macroexpand-all function that calls the
implementation specific equivalent.")
      (license license:unlicense))))

(define-public cl-trivial-macroexpand-all
  (sbcl-package->cl-source-package sbcl-trivial-macroexpand-all))

(define-public ecl-trivial-macroexpand-all
  (sbcl-package->ecl-package sbcl-trivial-macroexpand-all))

(define-public sbcl-serapeum
  (let ((commit "c29a52ff0c5f6e60b09919c3a0daa8df7599ddb9")
        (revision "6"))
    (package
      (name "sbcl-serapeum")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/ruricolist/serapeum")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0vij9jhji09way1rpd0r5sgjnh5amm3f2ymppnqkw0c6nnk2p0kd"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("trivia" ,sbcl-trivia)
         ("split-sequence" ,sbcl-split-sequence)
         ("string-case" ,sbcl-string-case)
         ("parse-number" ,sbcl-parse-number)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("named-readtables" ,sbcl-named-readtables)
         ("fare-quasiquote" ,sbcl-fare-quasiquote)
         ("parse-declarations-1.0" ,sbcl-parse-declarations)
         ("global-vars" ,sbcl-global-vars)
         ("trivial-file-size" ,sbcl-trivial-file-size)
         ("trivial-macroexpand-all" ,sbcl-trivial-macroexpand-all)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)
         ("local-time" ,sbcl-local-time)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'disable-failing-tests
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "serapeum.asd"
                 ;; Guix does not have Quicklisp, and probably never will.
                 (("\\(:file \"quicklisp\"\\)") ""))
               #t)))))
      (synopsis "Common Lisp utility library beyond Alexandria")
      (description
       "Serapeum is a conservative library of Common Lisp utilities.  It is a
supplement, not a competitor, to Alexandria.")
      (license license:expat))))

(define-public cl-serapeum
  (sbcl-package->cl-source-package sbcl-serapeum))

(define-public ecl-serapeum
  (sbcl-package->ecl-package sbcl-serapeum))

(define-public sbcl-rutils
  (let ((commit "db3c3f4ae897025b5f0cd81042ca147da60ca0c5")
        (revision "0"))
    (package
      (name "sbcl-rutils")
      (version (git-version "5.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/vseloved/rutils")
               (commit commit)))
         (file-name (git-file-name "rutils" version))
         (sha256
          (base32 "1d2whscknh1zga2vdqvfqri8wx0gnml3sfqz62igq0ppap6q07y3"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems '("rutils" "rutilsx")
         ;; Tests disabled because of a circular dependency with should-test.
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-build
             (lambda _
               ;; File faild to load, and we don't use it as tests are
               ;; disabled, so let's delete it.
               (delete-file "rutilsx-test.asd"))))))
      (inputs
       `(("closer-mop" ,sbcl-closer-mop)
         ("named-readtables" ,sbcl-named-readtables)))
      (home-page "https://github.com/vseloved/rutils")
      (synopsis "Radical Utilities for Common Lisp")
      (description "RUTILS is a syntactic utilities package for Common Lisp.")
      (license license:bsd-3))))

(define-public cl-rutils
  (sbcl-package->cl-source-package sbcl-rutils))

(define-public ecl-rutils
  (sbcl-package->ecl-package sbcl-rutils))

(define-public sbcl-arrows
  (let ((commit "df7cf0067e0132d9697ac8b1a4f1b9c88d4f5382")
        (revision "0"))
    (package
      (name "sbcl-arrows")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/Harleqin/arrows.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "042k9vkssrqx9nhp14wdzm942zgdxvp35mba0p2syz98i75im2yy"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-hu.dwim.stefil))
      (synopsis "Clojure-like arrow macros for Common Lisp")
      (description
       "This library implements the @code{->} and @code{->>} macros from
Clojure, as well as several expansions on the idea.")
      (home-page "https://gitlab.com/Harleqin/arrows")
      (license license:public-domain))))

(define-public cl-arrows
  (sbcl-package->cl-source-package sbcl-arrows))

(define-public ecl-arrows
  (sbcl-package->ecl-package sbcl-arrows))

(define-public sbcl-simple-parallel-tasks
  (let ((commit "ce7b60f788d8f68dfb69b24aac54c0e3b63379a6")
        (revision "1"))
    (package
      (name "sbcl-simple-parallel-tasks")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/simple-parallel-tasks")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0gvbpyff4siifp3cp86cpr9ksmakn66fx21f3h0hpn647zl07nj7"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-chanl))
      (synopsis "Common Lisp library to evaluate some forms in parallel")
      (description "This is a simple Common Lisp library to evaluate some
forms in parallel.")
      (home-page "https://github.com/glv2/simple-parallel-tasks")
      (license license:gpl3))))

(define-public cl-simple-parallel-tasks
  (sbcl-package->cl-source-package sbcl-simple-parallel-tasks))

(define-public ecl-simple-parallel-tasks
  (sbcl-package->ecl-package sbcl-simple-parallel-tasks))

(define-public sbcl-cl-heap
  (package
    (name "sbcl-cl-heap")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://common-lisp.net/project/cl-heap/releases/"
                           "cl-heap_" version ".tar.gz"))
       (sha256
        (base32
         "163hb07p2nxz126rpq3cj5dyala24n0by5i5786n2qcr1w0bak4i"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-xlunit))
    (arguments
     `(#:test-asd-file "cl-heap-tests.asd"))
    (synopsis "Heap and priority queue data structures for Common Lisp")
    (description
     "CL-HEAP provides various implementations of heap data structures (a
binary heap and a Fibonacci heap) as well as an efficient priority queue.")
    (home-page "https://common-lisp.net/project/cl-heap/")
    (license license:gpl3+)))

(define-public cl-heap
  (sbcl-package->cl-source-package sbcl-cl-heap))

(define-public ecl-cl-heap
  (sbcl-package->ecl-package sbcl-cl-heap))

(define-public sbcl-curry-compose-reader-macros
  (let ((commit "beaa92dedf392726c042184bfd6149fa8d9e6ac2")
        (revision "0"))
    (package
      (name "sbcl-curry-compose-reader-macros")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/eschulte/curry-compose-reader-macros")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0rv9bl8xrad5wfcg5zs1dazvnpmvqz6297lbn8bywsrcfnlf7h98"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-named-readtables))
      (synopsis "Reader macros for partial application and composition")
      (description
       "This Common Lisp library provides reader macros for concise expression
of function partial application and composition.")
      (home-page "https://eschulte.github.io/curry-compose-reader-macros/")
      (license license:public-domain))))

(define-public cl-curry-compose-reader-macros
  (sbcl-package->cl-source-package sbcl-curry-compose-reader-macros))

(define-public ecl-curry-compose-reader-macros
  (sbcl-package->ecl-package sbcl-curry-compose-reader-macros))

(define-public sbcl-yason
  (package
    (name "sbcl-yason")
    (version "0.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/phmarek/yason")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0479rbjgbj80jpk5bby18inlv1kfp771a82rlcq5psrz65qqa9bj"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-alexandria sbcl-trivial-gray-streams))
    (synopsis "Common Lisp JSON parser/encoder")
    (description
     "YASON is a Common Lisp library for encoding and decoding data in the
JSON interchange format.")
    (home-page "https://github.com/phmarek/yason")
    (license license:bsd-3)))

(define-public cl-yason
  (sbcl-package->cl-source-package sbcl-yason))

(define-public ecl-yason
  (sbcl-package->ecl-package sbcl-yason))

(define-public sbcl-graph
  (let ((commit "78bf9ec930d8eae4f0861b5be76765fb1e45e24f")
        (revision "0"))
    (package
      (name "sbcl-graph")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/eschulte/graph")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1qpij4xh8bqwc2myahpilcbh916v7vg0acz2fij14d3y0jm02h0g"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-stefil))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-heap" ,sbcl-cl-heap)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("curry-compose-reader-macros" ,sbcl-curry-compose-reader-macros)
         ("metabang-bind" ,sbcl-metabang-bind)
         ("named-readtables" ,sbcl-named-readtables)
         ("yason" ,sbcl-yason)))
      (arguments
       '(#:asd-systems '("graph"
                         "graph/dot"
                         "graph/json")))
      (synopsis "Graph data structure and algorithms for Common Lisp")
      (description
       "The GRAPH Common Lisp library provides a data structures to represent
graphs, as well as some graph manipulation and analysis algorithms (shortest
path, maximum flow, minimum spanning tree, etc.).")
      (home-page "https://eschulte.github.io/graph/")
      (license license:gpl3+))))

(define-public cl-graph
  (sbcl-package->cl-source-package sbcl-graph))

(define-public ecl-graph
  (sbcl-package->ecl-package sbcl-graph))

(define-public sbcl-trivial-indent
  (let ((commit "2d016941751647c6cc5bd471751c2cf68861c94a")
        (revision "0"))
    (package
      (name "sbcl-trivial-indent")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/trivial-indent")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1sj90nqz17w4jq0ixz00gb9g5g6d2s7l8r17zdby27gxxh51w266"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Simple Common Lisp library to allow indentation hints for SWANK")
      (description
       "This library allows you to define custom indentation hints for your
macros if the one recognised by SLIME automatically produces unwanted
results.")
      (home-page "https://shinmera.github.io/trivial-indent/")
      (license license:zlib))))

(define-public cl-trivial-indent
  (sbcl-package->cl-source-package sbcl-trivial-indent))

(define-public ecl-trivial-indent
  (sbcl-package->ecl-package sbcl-trivial-indent))

(define-public sbcl-documentation-utils
  (let ((commit "98630dd5f7e36ae057fa09da3523f42ccb5d1f55")
        (revision "0"))
    (package
      (name "sbcl-documentation-utils")
      (version (git-version "1.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/documentation-utils")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "098qhkqskmmrh4wix34mawf7p5c87yql28r51r75yjxj577k5idq"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-trivial-indent))
      (synopsis "Few simple tools to document Common Lisp libraries")
      (description
       "This is a small library to help you with managing the Common Lisp
docstrings for your library.")
      (home-page "https://shinmera.github.io/documentation-utils/")
      (license license:zlib))))

(define-public cl-documentation-utils
  (sbcl-package->cl-source-package sbcl-documentation-utils))

(define-public ecl-documentation-utils
  (sbcl-package->ecl-package sbcl-documentation-utils))

(define-public sbcl-documentation-utils-extensions
  (let ((commit "f67f8a05d583174662a594b79356b201c1d9d750"))
    (package
      (name "sbcl-documentation-utils-extensions")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/sirherrbatka/documentation-utils-extensions/")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0pn45c9rbxlnhn5nvhqz6kyv0nlirwxpg4j27niwdq80yxzsn51f"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-documentation-utils))
      (home-page "https://github.com/sirherrbatka/documentation-utils-extensions")
      (synopsis "Set of extensions for documentation-utils")
      (description
       "Use @code{rich-formatter} to format documentation with sections @code{:syntax},
@code{:arguments}, @code{:examples}, @code{:description}, @code{:returns},
@code{:side-effects}, @code{:thread-safety}, @code{:affected-by},
@code{:see-also} and @code{:notes}.  Gather unformatted input by using
@code{rich-aggregating-formatter} and @code{*DOCUMENTATION*} variable.  Find
gathered documentation with find-documentation function.  Execute code stored
in documentation with @code{execute-documentation}.  See the examples in the
@code{src/documentation.lisp} file.  See the @code{documentation-utils} system
for more information.")
      (license license:expat))))

(define-public cl-documentation-utils-extensions
  (sbcl-package->cl-source-package sbcl-documentation-utils-extensions))

(define-public ecl-documentation-utils-extensions
  (sbcl-package->ecl-package sbcl-documentation-utils-extensions))

(define-public sbcl-form-fiddle
  (let ((commit "e0c23599dbb8cff3e83e012f3d86d0764188ad18")
        (revision "0"))
    (package
      (name "sbcl-form-fiddle")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/form-fiddle")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "041iznc9mpfyrl0sv5893ys9pbb2pvbn9g3clarqi7gsfj483jln"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-documentation-utils))
      (synopsis "Utilities to destructure Common Lisp lambda forms")
      (description
       "Often times we need to destructure a form definition in a Common Lisp
macro.  This library provides a set of simple utilities to help with that.")
      (home-page "https://shinmera.github.io/form-fiddle/")
      (license license:zlib))))

(define-public cl-form-fiddle
  (sbcl-package->cl-source-package sbcl-form-fiddle))

(define-public ecl-form-fiddle
  (sbcl-package->ecl-package sbcl-form-fiddle))

(define-public sbcl-array-utils
  (let ((commit "f90eb9070d0b2205af51126a35033574725e5c56")
        (revision "0"))
    (package
      (name "sbcl-array-utils")
      (version (git-version "1.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/array-utils")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0zhwfbpr53vs1ii4sx75dz2k9yhh1xpwdqqpg8nmfndxkmhpbi3x"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-parachute))
      (inputs
       (list sbcl-documentation-utils))
      (synopsis "Tiny collection of array and vector utilities for Common Lisp")
      (description
       "A miniature toolkit that contains some useful shifting/popping/pushing
functions for arrays and vectors.  Originally from Plump.")
      (home-page "https://shinmera.github.io/array-utils/")
      (license license:zlib))))

(define-public cl-array-utils
  (sbcl-package->cl-source-package sbcl-array-utils))

(define-public ecl-array-utils
  (sbcl-package->ecl-package sbcl-array-utils))

(define-public sbcl-plump
  (let ((commit "3584275f0be9d06c0c51b5c08f89005deafc4ada")
        (revision "2"))
    (package
      (name "sbcl-plump")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/plump")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1w4wz7f6dc2ckdq9wq9r5014bg2nxjifj9yz1zzn41r8h1h5xfcd"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-array-utils sbcl-documentation-utils))
      (synopsis "Lenient XML / XHTML / HTML parser for Common Lisp")
      (description
       "Plump is a parser for HTML/XML-like documents, focusing on being
lenient towards invalid markup.  It can handle things like invalid attributes,
bad closing tag order, unencoded entities, inexistent tag types, self-closing
tags and so on.  It parses documents to a class representation and offers a
small set of DOM functions to manipulate it.  It can be extended to parse to
your own classes.")
      (home-page "https://shinmera.github.io/plump/")
      (license license:zlib))))

(define-public cl-plump
  (sbcl-package->cl-source-package sbcl-plump))

(define-public ecl-plump
  (sbcl-package->ecl-package sbcl-plump))

;;; Split the antik package in two to work around the circular dependency
;;; between antik/antik and antik/gsll.
(define-public sbcl-antik-base
  (let ((commit "e4711a69b3d6bf37b5727af05c3cfd03e8428ba3")
        (revision "1"))
    (package
      (name "sbcl-antik-base")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/antik/antik.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "047ga2c38par2xbgg4qx6hwv06qhf1c1f67as8xvir6s80lip1km"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("drakma" ,sbcl-drakma)
         ("fare-utils" ,sbcl-fare-utils)
         ("iterate" ,sbcl-iterate)
         ("metabang-bind" ,sbcl-metabang-bind)
         ("named-readtables" ,sbcl-named-readtables)
         ("split-sequence" ,sbcl-split-sequence)
         ("static-vectors" ,sbcl-static-vectors)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("trivial-utf-8" ,sbcl-trivial-utf-8)))
      (native-inputs
       (list sbcl-lisp-unit))
      (arguments
       '(#:asd-systems '("antik-base"
                         "foreign-array")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-build
             (lambda _
               (for-each delete-file
                         '("antik.asd"
                           "physical-dimension.asd"
                           "science-data.asd"))
               #t)))))
      (synopsis "Scientific and engineering computation in Common Lisp")
      (description
       "Antik provides a foundation for scientific and engineering
computation in Common Lisp.  It is designed not only to facilitate
numerical computations, but to permit the use of numerical computation
libraries and the interchange of data and procedures, whether
foreign (non-Lisp) or Lisp libraries.  It is named after the
Antikythera mechanism, one of the oldest examples of a scientific
computer known.")
      (home-page "https://common-lisp.net/project/antik/")
      (license license:gpl3))))

(define-public cl-antik-base
  (sbcl-package->cl-source-package sbcl-antik-base))

(define-public ecl-antik-base
  (let ((pkg (sbcl-package->ecl-package sbcl-antik-base)))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'fix-readtable
               (lambda _
                 (substitute* "input-output/readtable.lisp"
                   (("#-ccl")
                    "#-(or ccl ecl)"))
                 #t)))))))))

(define-public sbcl-gsll
  (let ((commit "1a8ada22f9cf5ed7372d352b2317f4ccdb6ab308")
        (revision "1"))
    (package
      (name "sbcl-gsll")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/antik/gsll.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0z5nypfk26hxihb08p085644afawicrgb4xvadh3lmrn46qbjfn4"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-lisp-unit))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("antik-base" ,sbcl-antik-base)
         ("cffi" ,sbcl-cffi)
         ("gsl" ,gsl)
         ("metabang-bind" ,sbcl-metabang-bind)
         ("trivial-features" ,sbcl-trivial-features)
         ("trivial-garbage" ,sbcl-trivial-garbage)))
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-cffi-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "gsll.asd"
                 ((":depends-on \\(#:foreign-array")
                  ":depends-on (#:foreign-array #:cffi-libffi"))
               (substitute* "init/init.lisp"
                 (("libgslcblas.so" all)
                  (string-append
                   (assoc-ref inputs "gsl") "/lib/" all)))
               (substitute* "init/init.lisp"
                 (("libgsl.so" all)
                  (string-append
                   (assoc-ref inputs "gsl") "/lib/" all))))))))
      (synopsis "GNU Scientific Library for Lisp")
      (description
       "The GNU Scientific Library for Lisp (GSLL) allows the use of the
GNU Scientific Library (GSL) from Common Lisp.  This library provides a
full range of common mathematical operations useful to scientific and
engineering applications.  The design of the GSLL interface is such
that access to most of the GSL library is possible in a Lisp-natural
way; the intent is that the user not be hampered by the restrictions
of the C language in which GSL has been written.  GSLL thus provides
interactive use of GSL for getting quick answers, even for someone not
intending to program in Lisp.")
      (home-page "https://common-lisp.net/project/gsll/")
      (license license:gpl3))))

(define-public cl-gsll
  (sbcl-package->cl-source-package sbcl-gsll))

(define-public ecl-gsll
  (sbcl-package->ecl-package sbcl-gsll))

(define-public sbcl-antik
  (package
    (inherit sbcl-antik-base)
    (name "sbcl-antik")
    (inputs
     `(("antik-base" ,sbcl-antik-base)
       ("gsll" ,sbcl-gsll)))
    (arguments
     '(#:asd-systems '("antik"
                       "science-data")
       #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-build
             (lambda _
               (for-each delete-file
                         '("antik-base.asd"
                           "foreign-array.asd"))
               #t)))))))

(define-public cl-antik
  (sbcl-package->cl-source-package sbcl-antik))

(define-public sbcl-cl-interpol
  (let ((commit "70a1137f41dd8889004dbab9536b1adeac2497aa")
        (revision "1"))
    (package
      (name "sbcl-cl-interpol")
      (version (git-version "0.2.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/edicl/cl-interpol")
               (commit commit)))
         (file-name (git-file-name "cl-interpol" version))
         (sha256
          (base32
           "1kr00zf62m7la7rxa2m5w49r9cyzamc106hvjcc8ffmi7a4jw490"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-unicode sbcl-named-readtables))
      (native-inputs
       (list sbcl-flexi-streams))
      (synopsis "String interpolation for Common Lisp")
      (description
       "CL-INTERPOL is a library for Common Lisp which modifies the
reader so that you can have interpolation within strings similar to
Perl or Unix Shell scripts.  It also provides various ways to insert
arbitrary characters into literal strings even if your editor/IDE
doesn't support them.")
      (home-page "https://edicl.github.io/cl-interpol/")
      (license license:bsd-3))))

(define-public cl-interpol
  (sbcl-package->cl-source-package sbcl-cl-interpol))

(define-public ecl-cl-interpol
  (sbcl-package->ecl-package sbcl-cl-interpol))

(define-public sbcl-symbol-munger
  (let ((commit "97598d4c3c53fd5da72ab78908fbd5d8c7a13416")
        (revision "1"))
    (package
      (name "sbcl-symbol-munger")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AccelerationNet/symbol-munger")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0y8jywjy0ldyhp7bxf16fdvdd2qgqnd7nlhlqfpfnzxcqk4xy1km"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("iterate" ,sbcl-iterate)))
      (arguments
       ;; There is a cyclical dependency between symbol-munger and lisp-unit2.
       ;; See https://github.com/AccelerationNet/symbol-munger/issues/4
       '(#:tests? #f))
      (synopsis
       "Capitalization and spacing conversion functions for Common Lisp")
      (description
       "This is a Common Lisp library to change the capitalization and spacing
of a string or a symbol.  It can convert to and from Lisp, english, underscore
and camel-case rules.")
      (home-page "https://github.com/AccelerationNet/symbol-munger")
      ;; The package declares a BSD license, but all of the license
      ;; text is MIT.
      ;; See https://github.com/AccelerationNet/symbol-munger/issues/5
      (license license:expat))))

(define-public cl-symbol-munger
  (sbcl-package->cl-source-package sbcl-symbol-munger))

(define-public ecl-symbol-munger
  (sbcl-package->ecl-package sbcl-symbol-munger))

(define-public sbcl-cl-csv
  (let ((commit "68ecb5d816545677513d7f6308d9e5e8d2265651")
        (revision "2"))
    (package
      (name "sbcl-cl-csv")
      (version (git-version "1.0.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AccelerationNet/cl-csv")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0gcmlbwx5m3kwgk12qi80w08ak8fgdnvyia429fz6gnxmhg0k54x"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; See: https://github.com/AccelerationNet/cl-csv/pull/34
       `(#:tests? #f))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-interpol" ,sbcl-cl-interpol)
         ("iterate" ,sbcl-iterate)))
      (native-inputs
       (list sbcl-lisp-unit2))
      (synopsis "Common lisp library for comma-separated values")
      (description
       "This is a Common Lisp library providing functions to read/write CSV
from/to strings, streams and files.")
      (home-page "https://github.com/AccelerationNet/cl-csv")
      (license license:bsd-3))))

(define-public cl-csv
  (sbcl-package->cl-source-package sbcl-cl-csv))

(define-public ecl-cl-csv
  (sbcl-package->ecl-package sbcl-cl-csv))

(define-public sbcl-external-program
  (let ((commit "5888b8f1fd3953feeeacecbba4384ddda584a749")
        (revision "1"))
    (package
      (name "sbcl-external-program")
      (version (git-version "0.0.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sellout/external-program")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0vww1x3yilb3bjwg6k184vaj4vxyxw4vralhnlm6lk4xac67kc9z"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-trivial-features))
      (native-inputs
       (list sbcl-fiveam))
      (synopsis "Common Lisp library for running external programs")
      (description
       "EXTERNAL-PROGRAM enables running programs outside the Lisp
process.  It is an attempt to make the RUN-PROGRAM functionality in
implementations like SBCL and CCL as portable as possible without
sacrificing much in the way of power.")
      (home-page "https://github.com/sellout/external-program")
      (license license:llgpl))))

(define-public cl-external-program
  (sbcl-package->cl-source-package sbcl-external-program))

(define-public ecl-external-program
  (sbcl-package->ecl-package sbcl-external-program))

(define-public sbcl-cl-ana
  (let ((commit "848185eed1ed65bab3a124870c122f761ce0d87e")
        (revision "2"))
    (package
      (name "sbcl-cl-ana")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ghollisjr/cl-ana")
               (commit commit)))
         (file-name (git-file-name "cl-ana" version))
         (sha256
          (base32 "026agqsxq3pg2k9jmy2wysil2z0yn5rykzzhr8rqxsspdwz51z1y"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list openmpi ;; for hdf-cffi
             pkg-config
             sbcl-cl-fad))
      (inputs
       (list gsl
             hdf5-parallel-openmpi
             libffi
             sbcl-antik
             sbcl-cffi
             sbcl-cl-csv
             sbcl-closer-mop
             sbcl-external-program
             sbcl-gsll
             sbcl-iterate
             sbcl-alexandria
             sbcl-split-sequence))
      (propagated-inputs
       (list gnuplot)) ;; for gnuplot-interface
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "hdf-cffi/src/library.lisp"
                 (("libhdf5.so")
                  (search-input-file inputs "/lib/libhdf5.so")))
               (substitute* "gsl-cffi/gsl-cffi.lisp"
                 (("libgsl.so")
                  (search-input-file inputs "/lib/libgsl.so"))))))))
      (synopsis "Common Lisp data analysis library")
      (description
       "CL-ANA is a data analysis library in Common Lisp providing tabular and
binned data analysis along with nonlinear least squares fitting and
visualization.")
      (home-page "https://github.com/ghollisjr/cl-ana")
      (license license:gpl3))))

(define-public cl-ana
  (sbcl-package->cl-source-package sbcl-cl-ana))

(define-public sbcl-archive
  (let ((commit "631271c091ed02994bec3980cb288a2cf32c7cdc")
        (revision "1"))
    (package
      (name "sbcl-archive")
      (version (git-version "0.9" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sharplispers/archive")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0pvsc9fmybx7rxd0kmzq4shi6hszdpwdc1sfy7jwyfxf8n3hnv4p"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-fad sbcl-trivial-gray-streams))
      (synopsis "Common Lisp library for tar and cpio archives")
      (description
       "This is a Common Lisp library to read and write disk-based file
archives such as those generated by the tar and cpio programs on Unix.")
      (home-page "https://github.com/sharplispers/archive")
      (license license:bsd-3))))

(define-public cl-archive
  (sbcl-package->cl-source-package sbcl-archive))

(define-public ecl-archive
  (sbcl-package->ecl-package sbcl-archive))

(define-public sbcl-misc-extensions
  (let ((commit "101c05112bf2f1e1bbf527396822d2f50ca6327a")
        (revision "1"))
    (package
      (name "sbcl-misc-extensions")
      (version (git-version "3.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/misc-extensions/devel.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0gz5f4p70qzilnxsnf5lih2n9m4wjcw8hlw4w8mpn9jyhyppyyv0"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Collection of small macros and extensions for Common Lisp")
      (description
       "This project is intended as a catchall for small, general-purpose
extensions to Common Lisp.  It contains:

@itemize
@item @code{new-let}, a macro that combines and generalizes @code{let},
@code{let*} and @code{multiple-value-bind},
@item @code{gmap}, an iteration macro that generalizes @code{map}.
@end itemize\n")
      (home-page "https://common-lisp.net/project/misc-extensions/")
      (license license:public-domain))))

(define-public cl-misc-extensions
  (sbcl-package->cl-source-package sbcl-misc-extensions))

(define-public ecl-misc-extensions
  (sbcl-package->ecl-package sbcl-misc-extensions))

(define-public sbcl-mt19937
  (package
    (name "sbcl-mt19937")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://common-lisp.net/project/asdf-packaging/"
                           "mt19937-latest.tar.gz"))
       (sha256
        (base32
         "1iw636b0iw5ygkv02y8i41lh7xj0acglv0hg5agryn0zzi2nf1xv"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Mersenne Twister pseudo-random number generator")
    (description
     "MT19937 is a portable Mersenne Twister pseudo-random number generator
for Common Lisp.")
    (home-page "https://www.cliki.net/mt19937")
    (license license:public-domain)))

(define-public cl-mt19937
  (sbcl-package->cl-source-package sbcl-mt19937))

(define-public ecl-mt19937
  (sbcl-package->ecl-package sbcl-mt19937))

(define-public sbcl-fset
  (let ((commit "6d2f9ded8934d2b42f2571a0ba5bda091037d852")
        (revision "1"))
    (package
      (name "sbcl-fset")
      (version (git-version "1.3.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/slburson/fset")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "127acblwrbqicx47h6sgvknz1cqyfn8p4xkhkn1m7hxh8w5gk1zy"))
         (snippet '(begin
                     ;; Remove obsolete copy of system definition.
                     (delete-file "Code/fset.asd")
                     #t))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-misc-extensions sbcl-mt19937 sbcl-named-readtables))
      (synopsis "Functional set-theoretic collections library")
      (description
       "FSet is a functional set-theoretic collections library for Common Lisp.
Functional means that all update operations return a new collection rather than
modifying an existing one in place.  Set-theoretic means that collections may
be nested arbitrarily with no additional programmer effort; for instance, sets
may contain sets, maps may be keyed by sets, etc.")
      (home-page "https://common-lisp.net/project/fset/Site/index.html")
      (license license:llgpl))))

(define-public cl-fset
  (sbcl-package->cl-source-package sbcl-fset))

(define-public ecl-fset
  (package
    (inherit (sbcl-package->ecl-package sbcl-fset))
    (arguments
     ;; Tests fails on ECL with "The function FSET::MAKE-CHAR is undefined".
     '(#:tests? #f))))

(define-public sbcl-cl-cont
  (let ((commit "fc1fa7e6eb64894fdca13e688e6015fad5290d2a")
        (revision "1"))
    (package
      (name "sbcl-cl-cont")
      (version (git-version "0.3.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/cl-cont/cl-cont.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1zf8zvb0i6jm3hhfks4w74hibm6avgc6f9s1qwgjrn2bcik8lrvz"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-closer-mop))
      (native-inputs
       (list sbcl-rt))
      (synopsis "Delimited continuations for Common Lisp")
      (description
       "This is a library that implements delimited continuations by
transforming Common Lisp code to continuation passing style.")
      (home-page "https://common-lisp.net/project/cl-cont/")
      (license license:llgpl))))

(define-public cl-cont
  (sbcl-package->cl-source-package sbcl-cl-cont))

(define-public ecl-cl-cont
  (sbcl-package->ecl-package sbcl-cl-cont))

(define-public sbcl-cl-coroutine
  (let ((commit "de098f8d5debd8b14ef6864b5bdcbbf5ddbcfd72")
        (revision "1"))
    (package
      (name "sbcl-cl-coroutine")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/takagi/cl-coroutine")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1cqdhdjxffgfs116l1swjlsmcbly0xgcgrckvaajd566idj9yj4l"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-cont" ,sbcl-cl-cont)))
      (native-inputs
       (list sbcl-prove))
      (arguments
       `(;; TODO: Fix the tests. They fail with:
         ;; "Component CL-COROUTINE-ASD::CL-COROUTINE-TEST not found"
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "cl-coroutine-test.asd"
                 (("cl-test-more")
                  "prove"))
               #t)))))
      (synopsis "Coroutine library for Common Lisp")
      (description
       "This is a coroutine library for Common Lisp implemented using the
continuations of the @code{cl-cont} library.")
      (home-page "https://github.com/takagi/cl-coroutine")
      (license license:llgpl))))

(define-public cl-coroutine
  (sbcl-package->cl-source-package sbcl-cl-coroutine))

(define-public ecl-cl-coroutine
  (sbcl-package->ecl-package sbcl-cl-coroutine))

(define-public sbcl-vas-string-metrics
  (let ((commit "f2e4500b180316123fbd549bd51c751ee2d6ba0f")
        (revision "1"))
    (package
      (name "sbcl-vas-string-metrics")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/vsedach/vas-string-metrics")
               (commit commit)))
         (file-name (git-file-name "vas-string-metrics" version))
         (sha256
          (base32 "11fcnd03ybzz37rkg3z0wsb727yqgcd9gn70sccfb34l89ia279k"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:test-asd-file "test.vas-string-metrics.asd"))
      (home-page "https://github.com/vsedach/vas-string-metrics")
      (synopsis "String distance algorithms for Common Lisp")
      (description
       "VAS-STRING-METRICS provides the Jaro, Jaro-Winkler, Soerensen-Dice,
Levenshtein, and normalized Levenshtein string distance/similarity metrics
algorithms.")
      (license license:lgpl3+))))

(define-public ecl-vas-string-metrics
  (sbcl-package->ecl-package sbcl-vas-string-metrics))

(define-public cl-vas-string-metrics
  (sbcl-package->cl-source-package sbcl-vas-string-metrics))

(define-public sbcl-vom
  (let ((commit "1aeafeb5b74c53741b79497e0ef4acf85c92ff24")
        (revision "1"))
    (package
      (name "sbcl-vom")
      (version (git-version "0.1.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orthecreedence/vom")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0536kppj76ax4lrxhv42npkfjsmx45km2g439vf9jmw3apinz9cy"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Tiny logging utility for Common Lisp")
      (description
       "Vom is a logging library for Common Lisp.  It's goal is to be useful
and small.  It does not provide a lot of features as other loggers do, but
has a small codebase that's easy to understand and use.")
      (home-page "https://github.com/orthecreedence/vom")
      (license license:expat))))

(define-public cl-vom
  (sbcl-package->cl-source-package sbcl-vom))

(define-public ecl-vom
  (sbcl-package->ecl-package sbcl-vom))

(define-public sbcl-cl-libuv
  (let ((commit "32100c023c518038d0670a103eaa4d50dd785d29")
        (revision "1"))
    (package
      (name "sbcl-cl-libuv")
      (version (git-version "0.1.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orthecreedence/cl-libuv")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1kwix4si8a8hza34ab2k7whrh7z0yrmx39v2wc3qblv9m244jkh1"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("libuv" ,libuv)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "lib.lisp"
                 (("/usr/lib/libuv.so")
                  (search-input-file inputs "/lib/libuv.so")))))
           (add-after 'fix-paths 'fix-system-definition
             (lambda _
               (substitute* "cl-libuv.asd"
                 (("#:cffi #:alexandria")
                  "#:cffi #:cffi-grovel #:alexandria"))
               #t)))))
      (synopsis "Common Lisp bindings to libuv")
      (description
       "This library provides low-level libuv bindings for Common Lisp.")
      (home-page "https://github.com/orthecreedence/cl-libuv")
      (license license:expat))))

(define-public cl-libuv
  (sbcl-package->cl-source-package sbcl-cl-libuv))

(define-public ecl-cl-libuv
  (sbcl-package->ecl-package sbcl-cl-libuv))

(define-public sbcl-cl-async
  (let ((commit "f6423e44404a44434d803605e0d2e17199158e28")
        (revision "1"))
    (package
      (name "sbcl-cl-async")
      (version (git-version "0.6.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orthecreedence/cl-async")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "11xi9dxb8mjgwzrkj88i0xkgk26z9w9ddxzbv6xsvfc1d4x5cf4x"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("babel" ,sbcl-babel)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cffi" ,sbcl-cffi)
         ("cl-libuv" ,sbcl-cl-libuv)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("fast-io" ,sbcl-fast-io)
         ("openssl" ,openssl)
         ("static-vectors" ,sbcl-static-vectors)
         ("trivial-features" ,sbcl-trivial-features)
         ("trivial-gray-streams" ,sbcl-trivial-gray-streams)
         ("vom" ,sbcl-vom)))
      (arguments
       `(#:asd-systems '("cl-async"
                         "cl-async-repl"
                         "cl-async-ssl")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/ssl/package.lisp"
                 (("libcrypto\\.so")
                  (search-input-file inputs "/lib/libcrypto.so"))
                 (("libssl\\.so")
                  (search-input-file inputs "/lib/libssl.so"))))))))
      (synopsis "Asynchronous operations for Common Lisp")
      (description
       "Cl-async is a library for general purpose, non-blocking programming in
Common Lisp.  It uses the libuv library as backend.")
      (home-page "https://orthecreedence.github.io/cl-async/")
      (license license:expat))))

(define-public cl-async
  (sbcl-package->cl-source-package sbcl-cl-async))

(define-public ecl-cl-async
  (sbcl-package->ecl-package sbcl-cl-async))

(define-public sbcl-blackbird
  (let ((commit "d361f81c1411dec07f6c2dcb11c78f7aea9aaca8")
        (revision "1"))
    (package
      (name "sbcl-blackbird")
      (version (git-version "0.5.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orthecreedence/blackbird")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xfds5yaya64arzr7w1x38karyz11swzbhxx1afldpradj9dh19c"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-vom))
      (native-inputs
       (list sbcl-cl-async sbcl-fiveam))
      (synopsis "Promise implementation for Common Lisp")
      (description
       "This is a standalone promise implementation for Common Lisp.  It is
the successor to the now-deprecated cl-async-future project.")
      (home-page "https://orthecreedence.github.io/blackbird/")
      (license license:expat))))

(define-public cl-blackbird
  (sbcl-package->cl-source-package sbcl-blackbird))

(define-public ecl-blackbird
  (sbcl-package->ecl-package sbcl-blackbird))

(define-public sbcl-cl-async-future
  (let ((commit "ee36c22a69a9516407458d2ed8b475f1fc473959")
        (revision "1"))
    (package
      (name "sbcl-cl-async-future")
      (version (git-version "0.4.4.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orthecreedence/cl-async-future")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0z0sc7qlzzxk99f4l26zp6rai9kv0kj0f599sxai5s44p17zbbvh"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-blackbird))
      (native-inputs
       (list sbcl-cl-async sbcl-eos))
      (synopsis "Futures implementation for Common Lisp")
      (description
       "This is futures implementation for Common Lisp.  It plugs in nicely
to cl-async.")
      (home-page "https://orthecreedence.github.io/cl-async/future")
      (license license:expat))))

(define-public cl-async-future
  (sbcl-package->cl-source-package sbcl-cl-async-future))

(define-public ecl-cl-async-future
  (sbcl-package->ecl-package sbcl-cl-async-future))

(define-public sbcl-green-threads
  (let ((commit "fff5ebecb441a37e5c511773716aafd84a3c5840")
        (revision "1"))
    (package
      (name "sbcl-green-threads")
      (version (git-version "0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/thezerobit/green-threads")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1czw7nr0dwfps76h8hjvglk1wdh53yqbfbvv30whwbgqx33iippz"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-async-future" ,sbcl-cl-async-future)
         ("cl-cont" ,sbcl-cl-cont)))
      (native-inputs
       (list sbcl-prove))
      (arguments
       `(;; TODO: Fix the tests. They fail with:
         ;; "The function BLACKBIRD::PROMISE-VALUES is undefined"
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "green-threads-test.asd"
                 (("cl-test-more")
                  "prove"))
               #t)))))
      (synopsis "Cooperative multitasking library for Common Lisp")
      (description
       "This library allows for cooperative multitasking with help of cl-cont
for continuations.  It tries to mimic the API of bordeaux-threads as much as
possible.")
      (home-page "https://github.com/thezerobit/green-threads")
      (license license:bsd-3))))

(define-public cl-green-threads
  (sbcl-package->cl-source-package sbcl-green-threads))

(define-public ecl-green-threads
  (sbcl-package->ecl-package sbcl-green-threads))

(define-public sbcl-cl-base32
  (let ((commit "8cdee06fab397f7b0a19583b57e7f0c98405be85")
        (revision "1"))
    (package
      (name "sbcl-cl-base32")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hargettp/cl-base32")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17jrng8jb05d64ggyd11hp308c2fl5drvf9g175blgrkkl8l4mf8"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-lisp-unit))
      (synopsis "Common Lisp library for base32 encoding and decoding")
      (description
       "This package provides functions for base32 encoding and decoding as
defined in RFC4648.")
      (home-page "https://github.com/hargettp/cl-base32")
      (license license:expat))))

(define-public cl-base32
  (sbcl-package->cl-source-package sbcl-cl-base32))

(define-public ecl-cl-base32
  (sbcl-package->ecl-package sbcl-cl-base32))

(define-public sbcl-cl-z85
  (let ((commit "85b3951a9cfa2603acb6aee15567684f9a108098")
        (revision "1"))
    (package
      (name "sbcl-cl-z85")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/cl-z85")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0r27pidjaxbm7k1rr90nnajwl5xm2kp65g1fv0fva17lzy45z1mp"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-cl-octet-streams sbcl-fiveam))
      (synopsis "Common Lisp library for Z85 encoding and decoding")
      (description
       "This package provides functions to encode or decode byte vectors or
byte streams using the Z85 format, which is a base-85 encoding used by
ZeroMQ.")
      (home-page "https://github.com/glv2/cl-z85")
      (license license:gpl3+))))

(define-public cl-z85
  (sbcl-package->cl-source-package sbcl-cl-z85))

(define-public ecl-cl-z85
  (sbcl-package->ecl-package sbcl-cl-z85))

(define-public sbcl-ltk
  (package
    (name "sbcl-ltk")
    (version "0.992")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/herth/ltk")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13l2q4mskzilya9xh5wy2xvy30lwn104bd8wrq6ifds56r82iy3x"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list imagemagick tk))
    (arguments
     `(#:asd-systems '("ltk"
                       "ltk-mw"
                       "ltk-remote")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "ltk/ltk.lisp"
               (("#-freebsd \"wish\"")
                (string-append "#-freebsd \""
                               (assoc-ref inputs "tk")
                               "/bin/wish\""))
               (("do-execute \"convert\"")
                (string-append "do-execute \""
                               (assoc-ref inputs "imagemagick")
                               "/bin/convert\"")))
             #t))
         (add-after 'unpack 'fix-build
           (lambda _
             (substitute* "ltk/ltk-remote.lisp"
               (("\\(:export")
                "(:shadow #:raise) (:export"))
             #t)))))
    (synopsis "Common Lisp bindings for the Tk GUI toolkit")
    (description
     "LTK is a Common Lisp binding for the Tk graphics toolkit.  It is written
in pure Common Lisp and does not require any Tk knowledge for its usage.")
    (home-page "http://www.peter-herth.de/ltk/")
    (license license:llgpl)))

(define-public cl-ltk
  (sbcl-package->cl-source-package sbcl-ltk))

(define-public ecl-ltk
  (sbcl-package->ecl-package sbcl-ltk))

(define-public sbcl-cl-lex
  (let ((commit "f2dbbe25ef553005fb402d9a6203180c3fa1093b")
        (revision "1"))
    (package
      (name "sbcl-cl-lex")
      (version (git-version "1.1.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/djr7C4/cl-lex")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1kg50f76bfpfxcv4dfivq1n9a0xlsra2ajb0vd68lxwgbidgyc2y"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre))
      (synopsis "Common Lisp macros for generating lexical analyzers")
      (description
       "This is a Common Lisp library providing a set of macros for generating
lexical analyzers automatically.  The lexers generated using @code{cl-lex} can
be used with @code{cl-yacc}.")
      (home-page "https://github.com/djr7C4/cl-lex")
      (license license:gpl3))))

(define-public cl-lex
  (sbcl-package->cl-source-package sbcl-cl-lex))

(define-public ecl-cl-lex
  (sbcl-package->ecl-package sbcl-cl-lex))

(define-public sbcl-cl-colors2
  (let ((commit "795aedee593b095fecde574bd999b520dd03ed24")
        (revision "1"))
    (package
      (name "sbcl-cl-colors2")
      (version (git-version "0.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://notabug.org/cage/cl-colors2.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0hlyf4h5chkjdp9armla5w4kw5acikk159sym7y8c4jbjp9x47ih"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-clunit2))
      (inputs
       (list sbcl-alexandria sbcl-cl-ppcre))
      (synopsis "Color library for Common Lisp")
      (description
       "This is a very simple color library for Common Lisp, providing:

@itemize
@item Types for representing colors in HSV and RGB spaces.
@item Simple conversion functions between the above types (and also
hexadecimal representation for RGB).
@item Some predefined colors (currently X11 color names -- of course
the library does not depend on X11).
@end itemize\n")
      (home-page "https://notabug.org/cage/cl-colors2")
      (license license:boost1.0))))

(define-public cl-colors2
  (sbcl-package->cl-source-package sbcl-cl-colors2))

(define-public ecl-cl-colors2
  (sbcl-package->ecl-package sbcl-cl-colors2))

(define-public sbcl-cl-jpeg
  (let ((commit "ec557038128df6895fbfb743bfe8faf8ec2534af")
        (revision "1"))
    (package
      (name "sbcl-cl-jpeg")
      (version (git-version "2.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/cl-jpeg")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1bkkiqz8fqldlj1wbmrccjsvxcwj98h6s4b6gslr3cg2wmdv5xmy"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "JPEG image library for Common Lisp")
      (description
       "This is a baseline JPEG codec written in Common Lisp.  It can be used
for reading and writing JPEG image files.")
      (home-page "https://github.com/sharplispers/cl-jpeg")
      (license license:bsd-3))))

(define-public cl-jpeg
  (sbcl-package->cl-source-package sbcl-cl-jpeg))

(define-public ecl-cl-jpeg
  (sbcl-package->ecl-package sbcl-cl-jpeg))

(define-public sbcl-png
  (let ((commit "11b965fe378fd0561abe3616b18ff03af5179648")
        (revision "1"))
    (package
      (name "sbcl-png")
      (version (git-version "0.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ljosa/cl-png")
               (commit commit)))
         (file-name (git-file-name "cl-png" version))
         (sha256
          (base32 "173hqwpd0rwqf95mfx1h9l9c3i8bb0gvnpspzmmz3g5x3440czy4"))
         ;; Patch to fix compiling with SBCL >= 2.1.6.
         (patches (search-patches "sbcl-png-fix-sbcl-compatibility.patch"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-lib-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "libpng.lisp"
                 (("\"libpng\"")
                  (string-append "\""
                                 (assoc-ref inputs "libpng")
                                 "/lib/libpng\""))))))))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("libpng" ,libpng)))
      (home-page "https://github.com/ljosa/cl-png")
      (synopsis "Read and write PNG file format")
      (description
       "This package provides a @code{PNG} Common Lisp system to operate with
Portable Network Graphics file format.")
      (license license:lgpl2.1))))

(define-public ecl-png
  (sbcl-package->ecl-package sbcl-png))

(define-public cl-png
  (sbcl-package->cl-source-package sbcl-png))

(define-public sbcl-cl-svg
  (let ((commit "1e988ebd2d6e2ee7be4744208828ef1b59e5dcdc")
        (revision "1"))
    (package
      (name "sbcl-cl-svg")
      (version (git-version "0.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/wmannis/cl-svg")
               (commit commit)))
         (file-name (git-file-name "cl-svg" version))
         (sha256
          (base32 "11rmzimy6j7ln7q5y1h2kw1225rsfb6fpn89qjcq7h5lc8fay0wz"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/wmannis/cl-svg")
      (synopsis "Write SVG file format")
      (description
       "This package provides the @code{CL-SVG} Common Lisp system to produce
Scalable Vector Graphics files.")
      (license license:expat))))

(define-public ecl-cl-svg
  (sbcl-package->ecl-package sbcl-cl-svg))

(define-public cl-svg
  (sbcl-package->cl-source-package sbcl-cl-svg))

(define-public sbcl-nodgui
  (let ((commit "4a9c2e7714b278fbe97d198c56f54ea87290001d")
        (revision "1"))
    (package
      (name "sbcl-nodgui")
      (version (git-version "0.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://notabug.org/cage/nodgui.git")
               (commit commit)))
         (file-name (git-file-name "nodgui" version))
         (sha256
          (base32 "1vgzzw459h32v2mi41cia6i940jqmvxlc8w3xj3516hbc2mqkaib"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cl-colors2" ,sbcl-cl-colors2)
         ("cl-jpeg" ,sbcl-cl-jpeg)
         ("cl-lex" ,sbcl-cl-lex)
         ("cl-ppcre-unicode" ,sbcl-cl-ppcre-unicode)
         ("cl-unicode" ,sbcl-cl-unicode)
         ("cl-yacc" ,sbcl-cl-yacc)
         ("clunit2" ,sbcl-clunit2)
         ("named-readtables" ,sbcl-named-readtables)
         ("parse-number" ,sbcl-parse-number)
         ("tk" ,tk)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-after 'unpack 'fix-paths
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "src/wish-communication.lisp"
                          (("#-freebsd \"wish\"")
                           (string-append "#-freebsd \""
                                          (assoc-ref inputs "tk")
                                          "/bin/wish\"")))
                        #t)))))
      (synopsis "Common Lisp bindings for the Tk GUI toolkit")
      (description
       "Nodgui (@emph{No Drama GUI}) is a Common Lisp binding for the Tk GUI
toolkit.  It also provides a few additional widgets more than the standard Tk
ones.")
      (home-page "https://www.autistici.org/interzona/nodgui.html")
      (license license:llgpl))))

(define-public cl-nodgui
  (sbcl-package->cl-source-package sbcl-nodgui))

(define-public ecl-nodgui
  (sbcl-package->ecl-package sbcl-nodgui))

(define-public sbcl-salza2
  (package
    (name "sbcl-salza2")
    (version "2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xach/salza2")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p48lxdibnps5rpyh5cmnk0vc77bmmxb32qdzfz93zadr8wwas10"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-chipz sbcl-flexi-streams sbcl-parachute))
    (inputs
     (list sbcl-trivial-gray-streams))
    (synopsis "Common Lisp library for zlib, deflate and gzip compression")
    (description
     "Salza2 is a Common Lisp library for creating compressed data in the zlib,
deflate, or gzip data formats, described in RFC 1950, RFC 1951, and RFC 1952,
respectively.")
    (home-page "https://www.xach.com/lisp/salza2/")
    (license license:bsd-2)))

(define-public cl-salza2
  (sbcl-package->cl-source-package sbcl-salza2))

(define-public ecl-salza2
  (sbcl-package->ecl-package sbcl-salza2))

(define-public sbcl-origin
  (let ((commit "d646134302456408d6d43580bb05299f1695ab8e")
        (revision "1"))
    (package
      (name "sbcl-origin")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.mfiano.net/mfiano/origin")
               (commit commit)))
         (file-name (git-file-name "origin" version))
         (sha256
          (base32 "1n9aszaif3yh8prs5r8v51fbj4r5jd1a048mivd5yij3hplkm82b"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-parachute))
      (inputs
       (list sbcl-golden-utils sbcl-specialization-store))
      (home-page "https://git.mfiano.net/mfiano/origin")
      (synopsis "Common Lisp graphics math library")
      (description
       "This is a native Common Lisp graphics math library with an emphasis on
performance and correctness.")
      (license license:expat))))

(define-public ecl-origin
  (sbcl-package->ecl-package sbcl-origin))

(define-public cl-origin
  (sbcl-package->cl-source-package sbcl-origin))

(define-public sbcl-png-read
  (let ((commit "ec29f38a689972b9f1373f13bbbcd6b05deada88")
        (revision "1"))
    (package
      (name "sbcl-png-read")
      (version (git-version "0.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Ramarren/png-read")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0vyczbcwskrygrf1hgrsnk0jil8skmvf1kiaalw5jps4fjrfdkw0"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-babel sbcl-chipz sbcl-iterate))
      (synopsis "PNG decoder for Common Lisp")
      (description "This is a Common Lisp library for reading PNG images.")
      (home-page "https://github.com/Ramarren/png-read")
      (license license:bsd-3))))

(define-public cl-png-read
  (sbcl-package->cl-source-package sbcl-png-read))

(define-public ecl-png-read
  (sbcl-package->ecl-package sbcl-png-read))

(define-public sbcl-3b-bmfont
  (let ((commit "d1b5bec0de580c2d08ec947a93c56b1400f2a37a")
        (revision "1"))
    (package
      (name "sbcl-3b-bmfont")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/3b/3b-bmfont/")
               (commit commit)))
         (file-name (git-file-name "3b-bmfont" version))
         (sha256
          (base32 "12sgf7m0h6fqzhvkas7vmci6mprj3j3fnz778jlbqbsydln6v2yc"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems
         '("3b-bmfont"
           "3b-bmfont/text"
           "3b-bmfont/common"
           "3b-bmfont/xml"
           "3b-bmfont/json")))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cxml" ,sbcl-cxml)
         ("flexi-streams" ,sbcl-flexi-streams)
         ("jsown" ,sbcl-jsown)
         ("split-sequence" ,sbcl-split-sequence)))
      (home-page "https://github.com/3b/3b-bmfont/")
      (synopsis "Read/write bmfont metadata files")
      (description
       "This is a Common Lisp library which provides functionality to
read/write Bit Map Font (BMF) into text, JSON and XML.")
      (license license:expat))))

(define-public ecl-3b-bmfont
  (sbcl-package->ecl-package sbcl-3b-bmfont))

(define-public cl-3b-bmfont
  (sbcl-package->cl-source-package sbcl-3b-bmfont))

(define-public sbcl-zpng
  (package
    (name "sbcl-zpng")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xach/zpng")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b3ag3jhl3z7kdls3ahdsdxsfhhw5qrizk769984f4wkxhb69rcm"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-salza2))
    (synopsis "PNG encoder for Common Lisp")
    (description "This is a Common Lisp library for creating PNG images.")
    (home-page "https://www.xach.com/lisp/zpng/")
    (license license:bsd-2)))

(define-public cl-zpng
  (sbcl-package->cl-source-package sbcl-zpng))

(define-public ecl-zpng
  (sbcl-package->ecl-package sbcl-zpng))

(define-public sbcl-cl-qrencode
  (package
    (name "sbcl-cl-qrencode")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jnjcc/cl-qrencode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l5k131dchbf6cj8a8xqa731790p01p3qa1kdy2wa9dawy3ymkxr"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-lisp-unit))
    (inputs
     (list sbcl-zpng))
    (synopsis "QR code encoder for Common Lisp")
    (description
     "This Common Lisp library provides function to make QR codes and to save
them as PNG files.")
    (home-page "https://github.com/jnjcc/cl-qrencode")
    (license license:gpl2+)))

(define-public cl-qrencode
  (sbcl-package->cl-source-package sbcl-cl-qrencode))

(define-public ecl-cl-qrencode
  (sbcl-package->ecl-package sbcl-cl-qrencode))

(define-public sbcl-hdf5-cffi
  (let ((commit "5b5c88f191e470e4fe96b462334e3ce0806eed5c")
        (revision "1"))
    (package
      (name "sbcl-hdf5-cffi")
      (version (git-version "1.8.18" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hdfgroup/hdf5-cffi")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0vda3075423xz83qky998lpac5b04dwfv7bwgh9jq8cs5v0zrxjf"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Common Lisp bindings for the HDF5 library")
      (description
       "@code{hdf5-cffi} is a CFFI wrapper for the HDF5 library.")
      (home-page "https://github.com/hdfgroup/hdf5-cffi")
      (license (license:non-copyleft
                (string-append "https://github.com/HDFGroup/hdf5-cffi/raw/"
                               commit
                               "/LICENSE")))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("hdf5" ,hdf5-1.10)))
      (native-inputs
       (list sbcl-fiveam))
      (arguments
       `(#:test-asd-file "hdf5-cffi.test.asd"
         ;; Tests depend on hdf5-cffi.examples.asd in addition to hdf5-cffi.asd,
         ;; I don't know if there is a way to tell asdf-build-system to load
         ;; an additional system first, so tests are disabled.
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/library.lisp"
                 (("libhdf5.so")
                  (string-append
                   (assoc-ref inputs "hdf5")
                   "/lib/libhdf5.so")))))
           (add-after 'unpack 'fix-dependencies
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "hdf5-cffi.asd"
                 ((":depends-on \\(:cffi\\)")
                  ":depends-on (:cffi :cffi-grovel)"))
               (substitute* "hdf5-cffi.test.asd"
                 ((":depends-on \\(:cffi :hdf5-cffi")
                  ":depends-on (:cffi :cffi-grovel :hdf5-cffi"))))))))))

(define-public cl-hdf5-cffi
  (sbcl-package->cl-source-package sbcl-hdf5-cffi))

(define-public ecl-hdf5-cffi
  (sbcl-package->ecl-package sbcl-hdf5-cffi))

(define-public sbcl-cl-randist
  (package
    (name "sbcl-cl-randist")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lvaruzza/cl-randist")
             (commit "f088a54b540a7adefab7c04094a6103f9edda3d0")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0l8hyd6nbxb7f50vyxz3rbbm7kgr1fnadc40jywy4xj5vi5kpj5g"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Random distributions for Common Lisp")
    (description
     "Manual translation from C to Common Lisp of some random number
generation functions from the GSL library.")
    (home-page "https://github.com/lvaruzza/cl-randist")
    (license license:bsd-2)
    (arguments
     `(#:tests? #f))))

(define-public cl-randist
  (sbcl-package->cl-source-package sbcl-cl-randist))

(define-public ecl-cl-randist
  (sbcl-package->ecl-package sbcl-cl-randist))

(define-public sbcl-float-features
  (let ((commit "c1f86aea91cfaa3aa59799162be23ef8a12b199d")
        (revision "2"))
    (package
      (name "sbcl-float-features")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/float-features")
               (commit commit)))
         (file-name (git-file-name "float-features" version))
         (sha256
          (base32 "0vqm9xhn2i4vbjrxnp4hr1l3lydjflhjykdz6cmqg2j48c5kh3g3"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Common Lisp IEEE float portability library")
      (description
       "Portability library for IEEE float features that are not
covered by the Common Lisp standard.")
      (home-page "https://github.com/Shinmera/float-features")
      (license license:zlib)
      (inputs
       `(("documentation-utils" ,sbcl-documentation-utils)))
      (arguments
       `(#:tests? #f)))))

(define-public cl-float-features
  (sbcl-package->cl-source-package sbcl-float-features))

(define-public ecl-float-features
  (sbcl-package->ecl-package sbcl-float-features))

(define-public sbcl-function-cache
  (package
    (name "sbcl-function-cache")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AccelerationNet/function-cache")
             (commit "6a5ada401e57da2c8abf046f582029926e61fce8")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "000vmd3f5rx5hs9nvphfric0gkzaadns31c6mxaslpv0k7pkrmc6"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Function caching / memoization library for Common Lisp")
    (description
     "A common lisp library that provides extensible function result
caching based on arguments (an expanded form of memoization).")
    (home-page "https://github.com/AccelerationNet/function-cache")
    (license
     (license:non-copyleft
      "https://github.com/AccelerationNet/function-cache/blob/master/README.md"))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-interpol" ,sbcl-cl-interpol)
       ("iterate" ,sbcl-iterate)
       ("symbol-munger" ,sbcl-symbol-munger)
       ("closer-mop" ,sbcl-closer-mop)))
    (arguments
     `(#:tests? #f))))

(define-public cl-function-cache
  (sbcl-package->cl-source-package sbcl-function-cache))

(define-public ecl-function-cache
  (sbcl-package->ecl-package sbcl-function-cache))

(define-public sbcl-cache-while
  (let ((commit "38e9ffbdb2c894670c366c1e5802ffcc8cfd43a7")
        (revision "1"))
    (package
      (name "sbcl-cache-while")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/charje/cache-while")
               (commit commit)))
         (file-name (git-file-name "cache-while" version))
         (sha256
          (base32 "1qil68rfn5irmkb0jk1f6g1zy80wgc3skl8cr4rfgh7ywgm5izx3"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/charje/cache-while")
      (synopsis "Temporary / one-time caching macro for Common Lisp")
      (description
       "This is a Common Lisp macro for defining temporary caches that
invalidate based on expressions evaluating to different values.")
      (license license:llgpl))))

(define-public cl-cache-while
  (sbcl-package->cl-source-package sbcl-cache-while))

(define-public ecl-cache-while
  (sbcl-package->ecl-package sbcl-cache-while))

(define-public sbcl-type-r
  (let ((commit "83c89e38f2f7a7b16f1012777ecaf878cfa6a267")
        (revision "1"))
    (package
      (name "sbcl-type-r")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/guicho271828/type-r")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1arsxc2539rg8vbrdirz4xxj1b06mc6g6rqndz7a02g127qvk2sm"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Parser interface for Common Lisp built-in compound types")
      (description
       "Collections of accessor functions and patterns to access
the elements in compound type specifier, e.g. @code{dimensions} in
@code{(array element-type dimensions)}")
      (home-page "https://github.com/guicho271828/type-r")
      (license license:lgpl3+)
      (inputs
       `(("trivia" ,sbcl-trivia)
         ("alexandria" ,sbcl-alexandria)))
      (native-inputs
       (list sbcl-fiveam))
      (arguments
       `(#:test-asd-file "type-r.test.asd")))))

(define-public cl-type-r
  (sbcl-package->cl-source-package sbcl-type-r))

(define-public ecl-type-r
  (sbcl-package->ecl-package sbcl-type-r))

(define-public sbcl-trivialib-type-unify
  (let ((commit "62492ebf04db567dcf435ae84c50b7b8202ecf99")
        (revision "1"))
    (package
      (name "sbcl-trivialib-type-unify")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/guicho271828/trivialib.type-unify")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1bkyfzbwv75p50zp8n1n9rh2r29pw3vgz91gmn2gzzkyq3khj1vh"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Common Lisp type unification")
      (description
       "Unifies a parametrized type specifier against an actual type specifier.
Importantly, it handles complicated array-subtypes and number-related types
correctly.")
      (home-page "https://github.com/guicho271828/trivialib.type-unify")
      (license license:lgpl3+)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("trivia" ,sbcl-trivia)
         ("introspect-environment" ,sbcl-introspect-environment)
         ("type-r" ,sbcl-type-r)))
      (native-inputs
       (list sbcl-fiveam))
      (arguments
       `(#:asd-systems '("trivialib.type-unify")
         #:test-asd-file "trivialib.type-unify.test.asd")))))

(define-public cl-trivialib-type-unify
  (sbcl-package->cl-source-package sbcl-trivialib-type-unify))

(define-public ecl-trivialib-type-unify
  (sbcl-package->ecl-package sbcl-trivialib-type-unify))

(define-public sbcl-cl-unification
  (let ((commit "01079f34d197495880aa49ab727d63774d83035c")
        (revision "1"))
    (package
      (name "sbcl-cl-unification")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/cl-unification/cl-unification")
               (commit commit)))
         (file-name (git-file-name "cl-unification" version))
         (sha256
          (base32 "0nhqamn3qgg38i6aw2pshffdwr2hzslycg8ficmn333gw0h9rf4g"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-ptester))
      (inputs
       (list sbcl-cl-ppcre))
      (home-page "https://common-lisp.net/project/cl-unification/")
      (synopsis "Unification framework for Common Lisp")
      (description
       "This package provides a framework to unify arbitrary
Common Lisp objects while constructing bindings for placeholders
(unification variables) in a template sublanguage.")
      (license license:bsd-0))))

(define-public ecl-cl-unification
  (let ((pkg (sbcl-package->ecl-package sbcl-cl-unification)))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ;; The tests fail on ECL with:
         ;;   "In MAKE-ARRAY: the elements in :INITIAL-CONTENTS do not match
         ;;    the array dimensions."
         ((#:tests? _ #f) #f))))))

(define-public cl-unification
  (sbcl-package->cl-source-package sbcl-cl-unification))

(define-public sbcl-specialized-function
  (let ((commit "5e2b04432bdf728496e6ff7227f210f845af7247")
        (revision "3"))
    (package
      (name "sbcl-specialized-function")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/numcl/specialized-function")
               (commit commit)))
         (file-name (git-file-name "specialized-function" version))
         (sha256
          (base32 "19hfgc83b7as630r1w9r8yl0v6xq3dn01vcrl0bd4pza5hgjn4la"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Julia-like dispatch for Common Lisp")
      (description
       "This library is part of NUMCL.  It provides a macro
@code{SPECIALIZED} that performs a Julia-like dispatch on the arguments,
lazily compiling a type-specific version of the function from the same
code.  The main target of this macro is speed.")
      (home-page "https://github.com/numcl/specialized-function")
      (license license:lgpl3+)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("iterate" ,sbcl-iterate)
         ("lisp-namespace" ,sbcl-lisp-namespace)
         ("trivia" ,sbcl-trivia)
         ("trivial-cltl2" ,sbcl-trivial-cltl2)
         ("type-r" ,sbcl-type-r)))
      (native-inputs
       (list sbcl-fiveam))
      (arguments
       `(#:asd-files '("specialized-function.asd")
         #:test-asd-file "specialized-function.test.asd")))))

(define-public cl-specialized-function
  (sbcl-package->cl-source-package sbcl-specialized-function))

(define-public ecl-specialized-function
  (sbcl-package->ecl-package sbcl-specialized-function))

(define-public sbcl-constantfold
  (let ((commit "0ff1d97a3fbcb89264f6a2af6ce62b73e7b421f4")
        (revision "1"))
    (package
      (name "sbcl-constantfold")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/numcl/constantfold")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "153h0569z6bff1qbad0bdssplwwny75l7ilqwcfqfdvzsxf9jh06"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Support library for numcl")
      (description
       "Support library for numcl.  Registers a function as an
additional form that is considered as a candidate for a constant.")
      (home-page "https://github.com/numcl/constantfold")
      (license license:lgpl3+)
      (inputs
       `(("trivia" ,sbcl-trivia)
         ("alexandria" ,sbcl-alexandria)
         ("iterate" ,sbcl-iterate)
         ("lisp-namespace" ,sbcl-lisp-namespace)))
      (native-inputs
       (list sbcl-fiveam))
      (arguments
       `(#:asd-files '("constantfold.asd")
         #:test-asd-file "constantfold.test.asd")))))

(define-public cl-constantfold
  (sbcl-package->cl-source-package sbcl-constantfold))

(define-public ecl-constantfold
  (sbcl-package->ecl-package sbcl-constantfold))

(define-public sbcl-gtype
  (let ((commit "2442e32485635525af278ebd8fa69a27d5b8cf18")
        (revision "2"))
    (package
      (name "sbcl-gtype")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/numcl/gtype")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0hbkfdw00v7bsa6zbric34p5w6hfwxycccg8wc2faq0cxhsvpv9h"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "C++/Julia-like parametric types in Common Lisp")
      (description
       "Support library for numcl that provides Julia-like runtime parametric
type correctness in Common Lisp.  It is based on CLtL2 extensions.")
      (home-page "https://github.com/numcl/gtype")
      (license license:lgpl3+)
      (inputs
       `(("trivialib.type-unify" ,sbcl-trivialib-type-unify)
         ("trivial-cltl2" ,sbcl-trivial-cltl2)
         ("trivia" ,sbcl-trivia)
         ("alexandria" ,sbcl-alexandria)
         ("iterate" ,sbcl-iterate)
         ("type-r" ,sbcl-type-r)))
      (native-inputs
       (list sbcl-fiveam))
      (arguments
       `(#:asd-files '("gtype.asd")
         #:test-asd-file "gtype.test.asd")))))

(define-public cl-gtype
  (sbcl-package->cl-source-package sbcl-gtype))

(define-public ecl-gtype
  (let ((pkg (sbcl-package->ecl-package sbcl-gtype)))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ;; The tests fail on ECL with a COMPILE-FILE-ERROR for t/package.lisp.
         ((#:tests? _ #f) #f))))))

(define-public sbcl-numcl
  (let ((commit "3dcdb0e24a33943d6c3a188ecbb0c78003bf975c")
        (revision "2"))
    (package
      (name "sbcl-numcl")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/numcl/numcl")
               (commit commit)))
         (file-name (git-file-name "numcl" version))
         (sha256
          (base32 "17m1rx1gfjbbmgjsf33b8s4bygfsj1hb6kvmypkql21qzjvx60nl"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:test-asd-file "numcl.test.asd"
         #:asd-files '("numcl.asd")
         ;; Tests often fail because they require a dynamic-space-size much
         ;; bigger than the default one. Disable them for now.
         #:tests? #f))
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-randist" ,sbcl-cl-randist)
         ("constantfold" ,sbcl-constantfold)
         ("float-features" ,sbcl-float-features)
         ("function-cache" ,sbcl-function-cache)
         ("gtype" ,sbcl-gtype)
         ("iterate" ,sbcl-iterate)
         ("lisp-namespace" ,sbcl-lisp-namespace)
         ("specialized-function" ,sbcl-specialized-function)
         ("trivia" ,sbcl-trivia)
         ("type-r" ,sbcl-type-r)))
      (home-page "https://numcl.github.io/numcl/")
      (synopsis "Numpy clone in Common Lisp")
      (description
       "This package is a Python Numpy clone implemented in pure Common Lisp.")
      (license license:lgpl3+))))

(define-public cl-numcl
  (sbcl-package->cl-source-package sbcl-numcl))

(define-public ecl-numcl
  (sbcl-package->ecl-package sbcl-numcl))

(define-public sbcl-pzmq
  (let ((commit "6f7b2ca02c23ea53510a9b0e0f181d5364ce9d32")
        (revision "2"))
    (package
      (name "sbcl-pzmq")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orivej/pzmq")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "19mdhxhzzghlmff1fic4chg5iz0psglkim09z6dgpijm26biny05"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("fiveam" ,sbcl-fiveam)
         ("let-plus" ,sbcl-let-plus)))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("zeromq" ,zeromq)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-after 'unpack 'fix-paths
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "c-api.lisp"
                          (("\"libzmq")
                           (string-append "\""
                                          (assoc-ref inputs "zeromq")
                                          "/lib/libzmq")))
                        #t)))))
      (synopsis "Common Lisp bindings for the ZeroMQ library")
      (description "This Common Lisp library provides bindings for the ZeroMQ
lightweight messaging kernel.")
      (home-page "https://github.com/orivej/pzmq")
      (license license:unlicense))))

(define-public cl-pzmq
  (sbcl-package->cl-source-package sbcl-pzmq))

(define-public ecl-pzmq
  (sbcl-package->ecl-package sbcl-pzmq))

(define-public sbcl-clss
  (let ((revision "1")
        (commit "2a8e8615ab55870d4ca01928f3ed3bbeb4e75c8d"))
    (package
      (name "sbcl-clss")
      (version (git-version "0.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/clss")
           (commit commit)))
         (sha256
          (base32 "0la4dbcda78x29szanylccrsljqrn9d1mhh569sqkyp44ni5fv91"))
         (file-name (git-file-name name version))))
      (inputs
       (list sbcl-array-utils sbcl-plump))
      (build-system asdf-build-system/sbcl)
      (synopsis "DOM tree searching engine based on CSS selectors")
      (description "CLSS is a DOM traversal engine based on CSS
selectors.  It makes use of the Plump-DOM and is used by lQuery.")
      (home-page "https://github.com/Shinmera/clss")
      (license license:zlib))))

(define-public cl-clss
  (sbcl-package->cl-source-package sbcl-clss))

(define-public ecl-clss
  (sbcl-package->ecl-package sbcl-clss))

(define-public sbcl-lquery
  (let ((revision "1")
        (commit "8048111c6b83956daa632e7a3ffbd8c9c203bd8d"))
    (package
      (name "sbcl-lquery")
      (version (git-version "3.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/lquery")
           (commit commit)))
         (sha256
          (base32 "0520mcpxc2d6fdm8z61arpgd2z38kan7cf06qs373n5r64rakz6w"))
         (file-name (git-file-name name version))))
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-array-utils sbcl-form-fiddle sbcl-plump sbcl-clss))
      (build-system asdf-build-system/sbcl)
      (synopsis "Library to allow jQuery-like HTML/DOM manipulation")
      (description "@code{lQuery} is a DOM manipulation library written in
Common Lisp, inspired by and based on the jQuery syntax and
functions.  It uses Plump and CLSS as DOM and selector engines.  The
main idea behind lQuery is to provide a simple interface for crawling
and modifying HTML sites, as well as to allow for an alternative
approach to templating.")
      (home-page "https://github.com/Shinmera/lquery")
      (license license:zlib))))

(define-public cl-lquery
  (sbcl-package->cl-source-package sbcl-lquery))

(define-public ecl-lquery
  (sbcl-package->ecl-package sbcl-lquery))

(define-public sbcl-cl-mysql
  (let ((commit "ab56c279c1815aec6ca0bfe85164ff7e85cfb6f9")
        (revision "1"))
    (package
      (name "sbcl-cl-mysql")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hackinghat/cl-mysql")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0dg5ynx2ww94d0qfwrdrm7plkn43h64hs4iiq9mj2s1s4ixnp3lr"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-stefil))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("mariadb-lib" ,mariadb "lib")))
      (arguments
       `(#:tests? #f ; TODO: Tests require a running server
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "system.lisp"
                 (("libmysqlclient_r" all)
                  (string-append (assoc-ref inputs "mariadb-lib")
                                 "/lib/"
                                 all)))
               #t)))))
      (synopsis "Common Lisp wrapper for MySQL")
      (description
       "@code{cl-mysql} is a Common Lisp implementation of a MySQL wrapper.")
      (home-page "http://www.hackinghat.com/index.php/cl-mysql")
      (license license:expat))))

(define-public cl-mysql
  (sbcl-package->cl-source-package sbcl-cl-mysql))

(define-public ecl-cl-mysql
  (sbcl-package->ecl-package sbcl-cl-mysql))

(define-public sbcl-postmodern
  (package
    (name "sbcl-postmodern")
    (version "1.32.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/marijnh/Postmodern")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "137jci4hn4vlxf48y39k0di27kc89kvxy3brmn3vl9xq56sy6mhz"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-fiveam))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cl-base64" ,sbcl-cl-base64)
       ("cl-unicode" ,sbcl-cl-unicode)
       ("closer-mop" ,sbcl-closer-mop)
       ("global-vars" ,sbcl-global-vars)
       ("ironclad" ,sbcl-ironclad)
       ("local-time" ,sbcl-local-time)
       ("md5" ,sbcl-md5)
       ("split-sequence" ,sbcl-split-sequence)
       ("uax-15" ,sbcl-uax-15)
       ("usocket" ,sbcl-usocket)))
    (arguments
     ;; TODO: (Sharlatan-20210114T171037+0000) tests still failing but on other
     ;; step, some functionality in `local-time' prevents passing tests.
     ;; Error:
     ;;
     ;; Can't create directory
     ;; /gnu/store
     ;;  /4f47agf1kyiz057ppy6x5p98i7mcbfsv-sbcl-local-time-1.0.6-2.a177eb9
     ;;   /lib/common-lisp/sbcl/local-time/src/integration/
     ;;
     ;; NOTE: (Sharlatan-20210124T191940+0000): When set env HOME to /tmp above
     ;; issue is resolved but it required live test database to connect to now.
     ;; Keep tests switched off.
     `(#:tests? #f
       #:asd-systems '("cl-postgres"
                       "s-sql"
                       "postmodern"
                       "simple-date"
                       "simple-date/postgres-glue")))
    (synopsis "Common Lisp library for interacting with PostgreSQL")
    (description
     "@code{postmodern} is a Common Lisp library for interacting with
PostgreSQL databases.  It provides the following features:

@itemize
@item Efficient communication with the database server without need for
foreign libraries.
@item Support for UTF-8 on Unicode-aware Lisp implementations.
@item A syntax for mixing SQL and Lisp code.
@item Convenient support for prepared statements and stored procedures.
@item A metaclass for simple database-access objects.
@end itemize\n

This package produces 4 systems: postmodern, cl-postgres, s-sql, simple-date

@code{SIMPLE-DATE} is a very basic implementation of date and time objects, used
to support storing and retrieving time-related SQL types.  It is not loaded by
default and you can use local-time (which has support for timezones) instead.

@code{S-SQL} is used to compile s-expressions to strings of SQL code, escaping
any Lisp values inside, and doing as much as possible of the work at compile
time.

@code{CL-POSTGRES} is the low-level library used for interfacing with a PostgreSQL
server over a socket.

@code{POSTMODERN} itself is a wrapper around these packages and provides higher
level functions, a very simple data access object that can be mapped directly to
database tables and some convient utilities.  It then tries to put all these
things together into a convenient programming interface")
    (home-page "https://marijnhaverbeke.nl/postmodern/")
    (license license:zlib)))

(define-public cl-postmodern
  (sbcl-package->cl-source-package sbcl-postmodern))

(define-public ecl-postmodern
  (package
    (inherit (sbcl-package->ecl-package sbcl-postmodern))
    (arguments
     `(#:tests? #f
       #:asd-systems '("cl-postgres"
                       "s-sql"
                       "postmodern"
                       "simple-date"
                       "simple-date/postgres-glue")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build
           (lambda _
             (substitute* "cl-postgres.asd"
               ((":or :sbcl :allegro :ccl :clisp" all)
                (string-append all " :ecl")))
             #t)))))))

(define-public sbcl-db3
  (let ((commit "38e5ad35f025769fb7f8dcdc6e56df3e8efd8e6d")
        (revision "1"))
    (package
      (name "sbcl-db3")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dimitri/cl-db3")
               (commit commit)))
         (file-name (git-file-name "cl-db3" version))
         (sha256
          (base32 "1i7j0mlri6kbklcx1lsm464s8kmyhhij5c4xh4aybrw8m4ixn1s5"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/dimitri/cl-db3")
      (synopsis "Common Lisp library to read dBase III database files")
      (description
       "This is a Common Lisp library for processing data found in dBase III
database files (dbf and db3 files).")
      (license license:public-domain))))

(define-public ecl-db3
  (sbcl-package->ecl-package sbcl-db3))

(define-public cl-db3
  (sbcl-package->cl-source-package sbcl-db3))

(define-public sbcl-dbi
  (let ((commit "7ba050dea8d137c1f85b7e704d4fc945104bf283")
        (revision "1"))
    (package
      (name "sbcl-dbi")
      (version (git-version "0.9.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/cl-dbi")
               (commit commit)))
         (file-name (git-file-name "cl-dbi" version))
         (sha256
          (base32 "0qkpsf8w7ig6chbf4r7j1j7fwa6kpi58ij4hbcxpa4irqdan8s9f"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("alexandria" ,sbcl-alexandria)
         ("rove" ,sbcl-rove)
         ("trivial-types" ,sbcl-trivial-types)))
      (inputs
       `(("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cl-mysql" ,sbcl-cl-mysql)
         ("cl-sqlite" ,sbcl-cl-sqlite)
         ("closer-mop" ,sbcl-closer-mop)
         ("postmodern" ,sbcl-postmodern)
         ("split-sequence" ,sbcl-split-sequence)
         ("trivial-garbage" ,sbcl-trivial-garbage)))
      (arguments
       `(#:asd-systems '("dbi"
                         "dbd-mysql"
                         "dbd-postgres"
                         "dbd-sqlite3")))
      (synopsis "Database independent interface for Common Lisp")
      (description
       "@code{dbi} is a Common Lisp library providing a database independent
interface for MySQL, PostgreSQL and SQLite.")
      (home-page "https://github.com/fukamachi/cl-dbi")
      (license license:llgpl))))

(define-public cl-dbi
  (sbcl-package->cl-source-package sbcl-dbi))

(define-public ecl-dbi
  (sbcl-package->ecl-package sbcl-dbi))

(define-public sbcl-uffi
  (package
    (name "sbcl-uffi")
    (version "2.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "http://git.kpe.io/uffi.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hqszvz0a3wk4s9faa83sc3vjxcb5rxmjclyr17yzwg55z733kry"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #f ; TODO: Fix use of deprecated ASDF functions
       #:asd-files '("uffi.asd")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-permissions
           (lambda _
             (make-file-writable "doc/html.tar.gz")
             #t)))))
    (synopsis "Universal foreign function library for Common Lisp")
    (description
     "UFFI provides a universal foreign function interface (FFI)
 for Common Lisp.")
    (home-page "http://quickdocs.org/uffi/")
    (license license:llgpl)))

(define-public cl-uffi
  (package
    (inherit (sbcl-package->cl-source-package sbcl-uffi))
    (arguments
     `(#:phases
       ;; asdf-build-system/source has its own phases and does not inherit
       ;; from asdf-build-system/sbcl phases.
       (modify-phases %standard-phases/source
         ;; Already done in SBCL package.
         (delete 'reset-gzip-timestamps))))))

(define-public sbcl-clsql
  (package
    (name "sbcl-clsql")
    (version "6.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "http://git.kpe.io/clsql.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v1k3s5bsy3lgd9gk459bzpb1r0kdjda25s29samxw4gsgf1fqvp"))
       (snippet
        '(begin
           ;; Remove precompiled libraries.
           (delete-file "db-mysql/clsql_mysql.dll")
           (delete-file "uffi/clsql_uffi.dll")
           (delete-file "uffi/clsql_uffi.lib")
           #t))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("rt" ,sbcl-rt)))
    (inputs
     `(("cffi" ,sbcl-cffi)
       ("md5" ,sbcl-md5)
       ("mysql" ,mysql)
       ("postgresql" ,postgresql)
       ("postmodern" ,sbcl-postmodern)
       ("sqlite" ,sqlite)
       ("uffi" ,sbcl-uffi)
       ("zlib" ,zlib)))
    (arguments
     `(#:asd-files '("clsql.asd"
                     "clsql-uffi.asd"
                     "clsql-sqlite3.asd"
                     "clsql-postgresql.asd"
                     "clsql-postgresql-socket3.asd"
                     "clsql-mysql.asd")
       #:asd-systems '("clsql"
                       "clsql-sqlite3"
                       "clsql-postgresql"
                       "clsql-postgresql-socket3"
                       "clsql-mysql")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-permissions
           (lambda _
             (make-file-writable "doc/html.tar.gz")
             #t))
         (add-after 'unpack 'fix-build
           (lambda _
             (substitute* "clsql-uffi.asd"
               (("\\(:version uffi \"2.0\"\\)")
                "uffi"))
             (substitute* "db-postgresql/postgresql-api.lisp"
               (("\\(data :cstring\\)")
                "(data :string)"))
             #t))
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "db-sqlite3/sqlite3-loader.lisp"
               (("libsqlite3")
                (string-append (assoc-ref inputs "sqlite")
                               "/lib/libsqlite3")))
             (substitute* "db-postgresql/postgresql-loader.lisp"
               (("libpq")
                (string-append (assoc-ref inputs "postgresql")
                               "/lib/libpq")))
             (let ((lib (string-append "#p\""
                                       (assoc-ref outputs "out")
                                       "/lib/\"")))
               (substitute* "clsql-mysql.asd"
                 (("#p\"/usr/lib/clsql/clsql_mysql\\.so\"")
                  lib))
               (substitute* "db-mysql/mysql-loader.lisp"
                 (("libmysqlclient" all)
                  (string-append (assoc-ref inputs "mysql") "/lib/" all))
                 (("clsql-mysql-system::\\*library-file-dir\\*")
                  lib)))
             #t))
         (add-before 'build 'build-helper-library
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((mysql (assoc-ref inputs "mysql"))
                    (inc-dir (string-append mysql "/include/mysql"))
                    (lib-dir (string-append mysql "/lib"))
                    (shared-lib-dir (string-append (assoc-ref outputs "out")
                                                   "/lib"))
                    (shared-lib (string-append shared-lib-dir
                                               "/clsql_mysql.so")))
               (mkdir-p shared-lib-dir)
               (invoke "gcc" "-fPIC" "-shared"
                       "-I" inc-dir
                       "db-mysql/clsql_mysql.c"
                       "-Wl,-soname=clsql_mysql"
                       "-L" lib-dir "-lmysqlclient" "-lz"
                       "-o" shared-lib)
               #t)))
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "clsql.asd"
               (("clsql-tests :force t")
                "clsql-tests"))
             #t)))))
    (synopsis "Common Lisp SQL Interface library")
    (description
     "@code{clsql} is a Common Lisp interface to SQL RDBMS based on the
Xanalys CommonSQL interface for Lispworks.  It provides low-level database
interfaces as well as a functional and an object oriented interface.")
    (home-page "http://clsql.kpe.io/")
    (license license:llgpl)))

(define-public cl-clsql
  (package
    (inherit (sbcl-package->cl-source-package sbcl-clsql))
    (native-inputs
     `(("rt" ,cl-rt)))
    (inputs
     (list mysql postgresql sqlite zlib))
    (propagated-inputs
     `(("cffi" ,cl-cffi)
       ("md5" ,cl-md5)
       ("postmodern" ,cl-postmodern)
       ("uffi" ,cl-uffi)))
    (arguments
     `(#:phases
       ;; asdf-build-system/source has its own phases and does not inherit
       ;; from asdf-build-system/sbcl phases.
       (modify-phases %standard-phases/source
         (add-after 'unpack 'fix-permissions
           (lambda _
             (make-file-writable "doc/html.tar.gz")
             #t)))))))

(define-public ecl-clsql
  (let ((pkg (sbcl-package->ecl-package sbcl-clsql)))
    (package
      (inherit pkg)
      (inputs
       (alist-delete "uffi" (package-inputs pkg)))
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:asd-files asd-files '())
          `(cons "clsql-cffi.asd" ,asd-files)))))))

(define-public sbcl-sycamore
  (let ((commit "fd2820fec165ad514493426dea209728f64e6d18"))
    (package
      (name "sbcl-sycamore")
      (version "0.0.20120604")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ndantam/sycamore/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00bv1aj89q5vldmq92zp2364jq312zjq2mbd3iyz1s2b4widzhl7"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-cl-ppcre))
      (synopsis "Purely functional data structure library in Common Lisp")
      (description
       "Sycamore is a fast, purely functional data structure library in Common Lisp.
If features:

@itemize
@item Fast, purely functional weight-balanced binary trees.
@item Leaf nodes are simple-vectors, greatly reducing tree height.
@item Interfaces for tree Sets and Maps (dictionaries).
@item Ropes.
@item Purely functional pairing heaps.
@item Purely functional amortized queue.
@end itemize\n")
      (home-page "http://ndantam.github.io/sycamore/")
      (license license:bsd-3))))

(define-public cl-sycamore
  (sbcl-package->cl-source-package sbcl-sycamore))

(define-public ecl-sycamore
  (sbcl-package->ecl-package sbcl-sycamore))

(define-public sbcl-funds
  (let ((commit "39d425818876b898c20780a678803df506df8424")
        (revision "2"))
    (package
      (name "sbcl-funds")
      (version (git-version "1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/charJe/funds")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "13y1jhvnpzrs9daz6f3z67w6h2y21ggb10j3j4vnc5p3m8i7ps4p"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Purely functional data structure library in Common Lisp")
      (description
       "Funds provides portable, purely functional data structures in Common
Lisp.  It includes tree based implementations for Array, Hash, Queue, Stack, and
Heap.")
      (home-page "https://common-lisp.net/project/funds/")
      (license license:asl2.0))))

(define-public cl-funds
  (sbcl-package->cl-source-package sbcl-funds))

(define-public ecl-funds
  (sbcl-package->ecl-package sbcl-funds))

(define-public sbcl-trivial-package-local-nicknames
  (package
    (name "sbcl-trivial-package-local-nicknames")
    (version "0.2")
    (home-page "https://github.com/phoe/trivial-package-local-nicknames")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit "16b7ad4c2b120f50da65154191f468ea5598460e")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18qc27xkjzdcqrilpk3pm7djldwq5rm3ggd5h9cr8hqcd54i2fqg"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Common Lisp compatibility library for package local nicknames")
    (description
     "This library is a portable compatibility layer around package local nicknames (PLN).
This was done so there is a portability library for the PLN API not included
in DEFPACKAGE.")
    (license license:unlicense)))

(define-public cl-trivial-package-local-nicknames
  (sbcl-package->cl-source-package sbcl-trivial-package-local-nicknames))

(define-public ecl-trivial-package-local-nicknames
  (sbcl-package->ecl-package sbcl-trivial-package-local-nicknames))

(define-public sbcl-enchant
  (let ((commit "6af162a7bf10541cbcfcfa6513894900329713fa"))
    (package
      (name "sbcl-enchant")
      (version (git-version "0.0.0" "1" commit))
      (home-page "https://github.com/tlikonen/cl-enchant")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "19yh5ihirzi1d8xqy1cjqipzd6ly3245cfxa5s9xx496rryz0s01"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("enchant" ,enchant)
         ("cffi" ,sbcl-cffi)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "load-enchant.lisp"
                 (("libenchant")
                  (string-append
                   (assoc-ref inputs "enchant") "/lib/libenchant-2"))))))))
      (synopsis "Common Lisp interface for the Enchant spell-checker library")
      (description
       "Enchant is a Common Lisp interface for the Enchant spell-checker
library.  The Enchant library is a generic spell-checker library which uses
other spell-checkers transparently as back-end.  The library supports the
multiple checkers, including Aspell and Hunspell.")
      (license license:public-domain))))

(define-public cl-enchant
  (sbcl-package->cl-source-package sbcl-enchant))

(define-public ecl-enchant
  (sbcl-package->ecl-package sbcl-enchant))

(define-public sbcl-cl-change-case
  (let ((commit "45c70b601125889689e0c1c37d7e727a3a0af022")
        (revision "1"))
    (package
      (name "sbcl-cl-change-case")
      (version (git-version "0.2.0" revision commit))
      (home-page "https://github.com/rudolfochrist/cl-change-case")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name "cl-change-case" version))
         (sha256
          (base32 "0qmk341zzcsbf8sq0w9ix3r080zg4ri6vzxym63lhdjfzwz3y8if"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre sbcl-cl-ppcre-unicode))
      (native-inputs
       (list sbcl-fiveam))
      (synopsis
       "Convert Common Lisp strings between camelCase, PascalCase and more")
      (description
       "@code{cl-change-case} is a library to convert strings between
camelCase, PascalCase, snake_case, param-case, CONSTANT_CASE and more.")
      (license license:llgpl))))

(define-public cl-change-case
  (sbcl-package->cl-source-package sbcl-cl-change-case))

(define-public ecl-cl-change-case
  (sbcl-package->ecl-package sbcl-cl-change-case))

(define-public sbcl-modularize
  (let ((commit "86c5d9a11fbd2df9f0f03ac10b5d71837c8934ba")
        (revision "1"))
    (package
      (name "sbcl-modularize")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/modularize")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zys29rfkb649rkgl3snxhajk8d5yf7ryxkrwy020kwdh7zdsg7d"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:test-asd-file "modularize-test-module.asd"
         #:asd-files '("modularize.asd" "modularize-test-module.asd")
         #:asd-systems '("modularize" "modularize-test-module")))
      (inputs
       `(("documentation-utils" ,sbcl-documentation-utils)
         ("trivial-package-local-nicknames" ,sbcl-trivial-package-local-nicknames)))
      (home-page "https://shinmera.github.io/modularize/")
      (synopsis "Common Lisp modularization framework")
      (description
       "@code{MODULARIZE} is an attempt at providing a common interface to
segregate major application components.  This is achieved by adding special
treatment to packages.  Each module is a package that is specially registered,
which allows it to interact and co-exist with other modules in better ways.  For
instance, by adding module definition options you can introduce mechanisms to
tie modules together in functionality, hook into each other and so on.")
      (license license:zlib))))

(define-public ecl-modularize
  (sbcl-package->ecl-package sbcl-modularize))

(define-public cl-modularize
  (sbcl-package->cl-source-package sbcl-modularize))

(define-public sbcl-modularize-hooks
  (let ((commit "e0348ed3ffd59a9ec31ca4ab28289e748bfbf96a")
        (revision "1"))
    (package
      (name "sbcl-modularize-hooks")
      (version (git-version "1.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/modularize-hooks")
               (commit commit)))
         (file-name (git-file-name "modularize-hooks" version))
         (sha256
          (base32 "12kjvin8hxidwkzfb7inqv5b6g5qzcssnj9wc497v2ixc56fqdz7"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-closer-mop sbcl-lambda-fiddle sbcl-modularize
             sbcl-trivial-arguments))
      (home-page "https://shinmera.github.io/modularize-hooks/")
      (synopsis "Generic hooks and triggers extension for Modularize")
      (description
       "This is a simple extension to @code{MODULARIZE} that allows modules to
define and trigger hooks, which other modules can hook on to.")
      (license license:zlib))))

(define-public ecl-modularize-hooks
  (sbcl-package->ecl-package sbcl-modularize-hooks))

(define-public cl-modularize-hooks
  (sbcl-package->cl-source-package sbcl-modularize-hooks))

(define-public sbcl-modularize-interfaces
  (let ((commit "96353657afb8c7aeba7ef5b51eb04c5ed3bcb6ef")
        (revision "1"))
    (package
      (name "sbcl-modularize-interfaces")
      (version (git-version "0.9.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/modularize-interfaces")
               (commit commit)))
         (file-name (git-file-name "modularize-interfaces" version))
         (sha256
          (base32 "0bjf4wy39cwf75m7vh0r7mmcchs09yz2lrbyap98hnq8blq70fhc"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-lambda-fiddle sbcl-modularize sbcl-trivial-arguments
             sbcl-trivial-indent))
      (home-page "https://shinmera.github.io/modularize-interfaces/")
      (synopsis "Programmatical interfaces extension for Modularize")
      (description
       "This is an extension to @code{MODULARIZE} that allows your application
to define interfaces in-code that serve both as a primary documentation and as
compliance control.")
      (license license:zlib))))

(define-public ecl-modularize-interfaces
  (sbcl-package->ecl-package sbcl-modularize-interfaces))

(define-public cl-modularize-interfaces
  (sbcl-package->cl-source-package sbcl-modularize-interfaces))

(define-public sbcl-moptilities
  (let ((commit "a436f16b357c96b82397ec018ea469574c10dd41"))
    (package
      (name "sbcl-moptilities")
      (version (git-version "0.3.13" "1" commit))
      (home-page "https://github.com/gwkkwg/moptilities/")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1q12bqjbj47lx98yim1kfnnhgfhkl80102fkgp9pdqxg0fp6g5fc"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("closer-mop" ,sbcl-closer-mop)))
      (native-inputs
       (list sbcl-lift))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "lift-standard.config"
                 ((":relative-to lift-test")
                  ":relative-to moptilities-test"))
               #t)))))
      (synopsis "Compatibility layer for Common Lisp MOP implementation differences")
      (description
       "MOP utilities provide a common interface between Lisps and make the
MOP easier to use.")
      (license license:expat))))

(define-public cl-moptilities
  (sbcl-package->cl-source-package sbcl-moptilities))

(define-public sbcl-osicat
  (let ((commit "eab6b8cabd71b59e894b51dc555e171683ec3387"))
    (package
      (name "sbcl-osicat")
      (version (git-version "0.7.0" "2" commit))
      (home-page "http://www.common-lisp.net/project/osicat/")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/osicat/osicat")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "18g137iqf86i247c9cg7d86b8k9cfq3yv272l73fsv734qpnv0g9"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-cffi sbcl-trivial-features))
      (native-inputs
       (list sbcl-rt))
      (synopsis "Operating system interface for Common Lisp")
      (description
       "Osicat is a lightweight operating system interface for Common Lisp on
Unix-platforms.  It is not a POSIX-style API, but rather a simple lispy
accompaniment to the standard ANSI facilities.")
      (license license:expat))))

(define-public cl-osicat
  (sbcl-package->cl-source-package sbcl-osicat))

(define-public ecl-osicat
  (sbcl-package->ecl-package sbcl-osicat))

(define-public sbcl-clx-xembed
  (let ((commit "a5c4b844d31ee68ffa58c933cc1cdddde6990743")
        (revision "1"))
    (package
      (name "sbcl-clx-xembed")
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/laynor/clx-xembed")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/laynor/clx-xembed")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1abx4v36ycmfjdwpjk4hh8058ya8whwia7ds9vd96q2qsrs57f12"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems '("xembed")))
      (inputs
       (list sbcl-clx))
      (synopsis "CL(x) xembed protocol implementation")
      (description "CL(x) xembed protocol implementation")
      ;; MIT License
      (license license:expat))))

(define-public cl-clx-xembed
  (sbcl-package->cl-source-package sbcl-clx-xembed))

(define-public ecl-clx-xembed
  (sbcl-package->ecl-package sbcl-clx-xembed))

(define-public sbcl-quantile-estimator
  (package
    (name "sbcl-quantile-estimator")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deadtrickster/quantile-estimator.cl")
             (commit "84d0ea405d793f5e808c68c4ddaf25417b0ff8e5")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0rlswkf0siaabsvvch3dgxmg45fw5w8pd9b7ri2w7a298aya52z9"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     '(#:asd-files '("quantile-estimator.asd")))
    (inputs
     `(("alexandria" ,sbcl-alexandria)))
    (home-page "https://github.com/deadtrickster/quantile-estimator.cl")
    (synopsis
     "Effective computation of biased quantiles over data streams")
    (description
     "Common Lisp implementation of Graham Cormode and S.
Muthukrishnan's Effective Computation of Biased Quantiles over Data
Streams in ICDE’05.")
    (license license:expat)))

(define-public cl-quantile-estimator
  (sbcl-package->cl-source-package sbcl-quantile-estimator))

(define-public ecl-quantile-estimator
  (sbcl-package->ecl-package sbcl-quantile-estimator))

(define-public sbcl-prometheus
  (package
    (name "sbcl-prometheus")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deadtrickster/prometheus.cl")
             (commit "7352b92296996ff383503e19bdd3bcea30409a15")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fzczls2kfgdx18pja4lqxjrz72i583185d8nq0pb3s331hhzh0z"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cffi" ,sbcl-cffi)
       ("cl-fad" ,sbcl-cl-fad)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("drakma" ,sbcl-drakma)
       ("hunchentoot" ,sbcl-hunchentoot)
       ("local-time" ,sbcl-local-time)
       ("quantile-estimator" ,sbcl-quantile-estimator)
       ("salza2" ,sbcl-salza2)
       ("split-sequence" ,sbcl-split-sequence)
       ("trivial-utf-8" ,sbcl-trivial-utf-8)))
    (arguments
     '(#:asd-files '("prometheus.asd"
                     "prometheus.collectors.sbcl.asd"
                     "prometheus.collectors.process.asd"
                     "prometheus.formats.text.asd"
                     "prometheus.exposers.hunchentoot.asd"
                     "prometheus.pushgateway.asd")
       #:asd-systems '("prometheus"
                       "prometheus.collectors.sbcl"
                       "prometheus.collectors.process"
                       "prometheus.formats.text"
                       "prometheus.exposers.hunchentoot"
                       "prometheus.pushgateway")))
    (home-page "https://github.com/deadtrickster/prometheus.cl")
    (synopsis "Prometheus.io Common Lisp client")
    (description "Prometheus.io Common Lisp client.")
    (license license:expat)))

(define-public cl-prometheus
  (sbcl-package->cl-source-package sbcl-prometheus))

(define-public ecl-prometheus
  (sbcl-package->ecl-package sbcl-prometheus))

(define-public sbcl-uuid
  (let ((commit "e7d6680c3138385c0708f7aaf0c96622eeb140e8"))
    (package
      (name "sbcl-uuid")
      (version (git-version "2012.12.26" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dardoria/uuid")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0jnyp2kibcf5cwi60l6grjrj8wws9chasjvsw7xzwyym2lyid46f"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-ironclad sbcl-trivial-utf-8))
      (home-page "https://github.com/dardoria/uuid")
      (synopsis
       "Common Lisp implementation of UUIDs according to RFC4122")
      (description
       "Common Lisp implementation of UUIDs according to RFC4122.")
      (license license:llgpl))))

(define-public cl-uuid
  (sbcl-package->cl-source-package sbcl-uuid))

(define-public ecl-uuid
  (sbcl-package->ecl-package sbcl-uuid))

(define-public sbcl-dissect
  (let ((commit "cffd38479f0e64e805f167bbdb240b783ecc8d45"))
    (package
      (name "sbcl-dissect")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/dissect")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0rmsjkgjl90gl6ssvgd60hb0d5diyhsiyypvw9hbc0ripvbmk5r5"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre))
      (home-page "https://shinmera.github.io/dissect/")
      (synopsis
       "Introspection library for the call stack and restarts")
      (description
       "Dissect is a small Common Lisp library for introspecting the call stack
and active restarts.")
      (license license:zlib))))

(define-public cl-dissect
  (sbcl-package->cl-source-package sbcl-dissect))

(define-public ecl-dissect
  (sbcl-package->ecl-package sbcl-dissect))

(define-public sbcl-exponential-backoff
  (let ((commit "8d9e8444d8b3184a524c12ce3449f91613ab714f"))
    (package
      (name "sbcl-exponential-backoff")
      (version (git-version "0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/death/exponential-backoff")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1389hm9hxv85s0125ja4js1bvh8ay4dsy9q1gaynjv27ynik6gmv"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/death/exponential-backoff")
      (synopsis "Exponential backoff algorithm in Common Lisp")
      (description
       "An implementation of the exponential backoff algorithm in Common Lisp.
Inspired by the implementation found in Chromium.  Read the header file to
learn about each of the parameters.")
      (license license:expat))))

(define-public cl-exponential-backoff
  (sbcl-package->cl-source-package sbcl-exponential-backoff))

(define-public ecl-exponential-backoff
  (sbcl-package->ecl-package sbcl-exponential-backoff))

(define-public sbcl-sxql
  (let ((commit "acdd183a4c38b4e0699a285f8a711c88f6b4302c"))
    (package
      (name "sbcl-sxql")
      (version (git-version "0.1.0" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/sxql")
               (commit commit)))
         (file-name (git-file-name "sqxl" version))
         (sha256
          (base32 "1i1crdsf2nbyxxpvjgrwmwpjxn6a4drbcmqs4q4shfi8zyap7vng"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria
             sbcl-cl-package-locks
             sbcl-cl-syntax
             sbcl-iterate
             sbcl-optima
             sbcl-split-sequence
             sbcl-trivial-types))
      (native-inputs
       (list sbcl-prove))
      (home-page "https://github.com/fukamachi/sxql")
      (synopsis "SQL generator for Common Lisp")
      (description "SQL generator for Common Lisp.")
      (license license:bsd-3))))

(define-public cl-sxql
  (sbcl-package->cl-source-package sbcl-sxql))

(define-public ecl-sxql
  (sbcl-package->ecl-package sbcl-sxql))

(define-public sbcl-cl-ascii-table
  (let ((commit "d9f5e774a56fad1b416e4dadb8f8a5b0e84094e2")
        (revision "1"))
    (package
      (name "sbcl-cl-ascii-table")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/telephil/cl-ascii-table")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "125fdif9sgl7k0ngjhxv0wjas2q27d075025hvj2rx1b1x948z4s"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Library to make ascii-art tables")
      (description
       "This is a Common Lisp library to present tabular data in ascii-art
tables.")
      (home-page "https://github.com/telephil/cl-ascii-table")
      (license license:expat))))

(define-public cl-ascii-table
  (sbcl-package->cl-source-package sbcl-cl-ascii-table))

(define-public ecl-cl-ascii-table
  (sbcl-package->ecl-package sbcl-cl-ascii-table))

(define-public sbcl-cl-rdkafka
  (package
    (name "sbcl-cl-rdkafka")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SahilKang/cl-rdkafka")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0z2g0k0xy8k1p9g93h8dy9wbygaq7ziwagm4yz93zk67mhc0b84v"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #f ; Attempts to connect to locally running Kafka
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/low-level/librdkafka-bindings.lisp"
               (("librdkafka" all)
                (string-append (assoc-ref inputs "librdkafka") "/lib/"
                               all))))))))
    (inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cffi" ,sbcl-cffi)
       ("librdkafka" ,librdkafka)
       ("lparallel" ,sbcl-lparallel)
       ("trivial-garbage" ,sbcl-trivial-garbage)))
    (home-page "https://github.com/SahilKang/cl-rdkafka")
    (synopsis "Common Lisp client library for Apache Kafka")
    (description "A Common Lisp client library for Apache Kafka.")
    (license license:gpl3)))

(define-public cl-rdkafka
  (sbcl-package->cl-source-package sbcl-cl-rdkafka))

(define-public ecl-cl-rdkafka
  (sbcl-package->ecl-package sbcl-cl-rdkafka))

(define-public sbcl-acclimation
  (let ((commit "4d51150902568fcd59335f4cc4cfa022df6116a5"))
    (package
      (name "sbcl-acclimation")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/robert-strandh/Acclimation")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1aw7rarjl8ai57h0jxnp9hr3dka7qrs55mmbl1p6rhd6xj8mp9wq"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/robert-strandh/Acclimation")
      (synopsis "Internationalization library for Common Lisp")
      (description "This project is meant to provide tools for
internationalizing Common Lisp programs.

One important aspect of internationalization is of course the language used in
error messages, documentation strings, etc.  But with this project we provide
tools for all other aspects of internationalization as well, including dates,
weight, temperature, names of physical quantitites, etc.")
      (license license:bsd-2))))

(define-public cl-acclimation
  (sbcl-package->cl-source-package sbcl-acclimation))

(define-public ecl-acclimation
  (sbcl-package->ecl-package sbcl-acclimation))

(define-public sbcl-clump
  (let ((commit "1ea4dbac1cb86713acff9ae58727dd187d21048a"))
    (package
      (name "sbcl-clump")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/robert-strandh/Clump")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1639msyagsswj85gc0wd90jgh8588j3qg5q70by9s2brf2q6w4lh"))))
      (inputs
       (list sbcl-acclimation))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/robert-strandh/Clump")
      (synopsis "Collection of tree implementations for Common Lisp")
      (description "The purpose of this library is to provide a collection of
implementations of trees.

In contrast to existing libraries such as cl-containers, it does not impose a
particular use for the trees.  Instead, it aims for a stratified design,
allowing client code to choose between different levels of abstraction.

As a consequence of this policy, low-level interfaces are provided where
the concrete representation is exposed, but also high level interfaces
where the trees can be used as search trees or as trees that represent
sequences of objects.")
      (license license:bsd-2))))

(define-public cl-clump
  (sbcl-package->cl-source-package sbcl-clump))

(define-public ecl-clump
  (sbcl-package->ecl-package sbcl-clump))

(define-public sbcl-cluffer
  (let ((commit "4aad29c276a58a593064e79972ee4d77cae0af4a"))
    (package
      (name "sbcl-cluffer")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/robert-strandh/cluffer")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1bcg13g7qb3dr8z50aihdjqa6miz5ivlc9wsj2csgv1km1mak2kj"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-acclimation sbcl-clump))
      (home-page "https://github.com/robert-strandh/cluffer")
      (synopsis "Common Lisp library providing a protocol for text-editor buffers")
      (description "Cluffer is a library for representing the buffer of a text
editor.  As such, it defines a set of CLOS protocols for client code to
interact with the buffer contents in various ways, and it supplies different
implementations of those protocols for different purposes.")
      (license license:bsd-2))))

(define-public cl-cluffer
  (sbcl-package->cl-source-package sbcl-cluffer))

(define-public ecl-cluffer
  (sbcl-package->ecl-package sbcl-cluffer))

(define-public sbcl-cl-libsvm-format
  (let ((commit "3300f84fd8d9f5beafc114f543f9d83417c742fb")
        (revision "0"))
    (package
      (name "sbcl-cl-libsvm-format")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/masatoi/cl-libsvm-format")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0284aj84xszhkhlivaigf9qj855fxad3mzmv3zfr0qzb5k0nzwrg"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-prove))
      (inputs
       (list sbcl-alexandria))
      (synopsis "LibSVM data format reader for Common Lisp")
      (description
       "This Common Lisp library provides a fast reader for data in LibSVM
format.")
      (home-page "https://github.com/masatoi/cl-libsvm-format")
      (license license:expat))))

(define-public cl-libsvm-format
  (sbcl-package->cl-source-package sbcl-cl-libsvm-format))

(define-public ecl-cl-libsvm-format
  (sbcl-package->ecl-package sbcl-cl-libsvm-format))

(define-public sbcl-cl-online-learning
  (let ((commit "87fbef8a340219e853adb3a5bf44a0470da76964")
        (revision "1"))
    (package
      (name "sbcl-cl-online-learning")
      (version (git-version "0.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/masatoi/cl-online-learning")
               (commit commit)))
         (file-name (git-file-name "cl-online-learning" version))
         (sha256
          (base32
           "1lfq04lnxivx59nq5dd02glyqsqzf3vdn4s9b8wnaln5fs8g2ph9"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-prove))
      (inputs
       `(("cl-libsvm-format" ,sbcl-cl-libsvm-format)
         ("cl-store" ,sbcl-cl-store)))
      (arguments
       `(#:test-asd-file "cl-online-learning-test.asd"
         #:asd-systems '("cl-online-learning-test"
                         "cl-online-learning")))
      (home-page "https://github.com/masatoi/cl-online-learning")
      (synopsis "Online Machine Learning for Common Lisp")
      (description
       "This library contains a collection of machine learning algorithms for
online linear classification written in Common Lisp.")
      (license license:expat))))

(define-public cl-online-learning
  (sbcl-package->cl-source-package sbcl-cl-online-learning))

(define-public ecl-cl-online-learning
  (sbcl-package->ecl-package sbcl-cl-online-learning))

(define-public sbcl-cl-mpg123
  (let ((commit "5f042c839d2ea4a2ff2a7b60c839d8633d64161d")
        (revision "1"))
    (package
      (name "sbcl-cl-mpg123")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shirakumo/cl-mpg123")
               (commit commit)))
         (file-name (git-file-name "cl-mpg123" version))
         (sha256
          (base32 "1hl721xaczxck008ax2y3jpkm509ry1sg3lklh2k76764m3ndrjf"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Remove bundled pre-compiled libraries.
             (delete-file-recursively "static")
             #t))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-files '("cl-mpg123.asd" "cl-mpg123-example.asd")
         #:asd-systems '("cl-mpg123" "cl-mpg123-example")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "low-level.lisp"
                 (("libmpg123.so" all)
                  (string-append (assoc-ref inputs "libmpg123")
                                 "/lib/" all))))))))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("cl-out123" ,sbcl-cl-out123)
         ("documentation-utils" ,sbcl-documentation-utils)
         ("libmpg123" ,mpg123)
         ("trivial-features" ,sbcl-trivial-features)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("verbose" ,sbcl-verbose)))
      (home-page "https://shirakumo.github.io/cl-mpg123/")
      (synopsis "Common Lisp bindings to libmpg123")
      (description
       "This is a bindings and wrapper library to @code{libmpg123} allowing for
convenient, extensive, and fast decoding of MPEG1/2/3 (most prominently mp3)
files.")
      (license license:zlib))))

(define-public ecl-cl-mpg123
  (sbcl-package->ecl-package sbcl-cl-mpg123))

(define-public cl-mpg123
  (sbcl-package->cl-source-package sbcl-cl-mpg123))

(define-public sbcl-cl-out123
  (let ((commit "6b58d3f8c2a28ad09059ac4c60fb3c781b9b421b")
        (revision "1"))
    (package
      (name "sbcl-cl-out123")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shirakumo/cl-out123")
               (commit commit)))
         (file-name (git-file-name "cl-out123" version))
         (sha256
          (base32 "0mdwgfax6sq68wvdgjjp78i40ah7wqkpqnvaq8a1c509k7ghdgv1"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Remove bundled pre-compiled libraries.
             (delete-file-recursively "static")
             #t))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "low-level.lisp"
                 (("libout123.so" all)
                  (string-append (assoc-ref inputs "libout123")
                                 "/lib/" all)))))
           ;; NOTE: (Sharlatan-20210129T134529+0000): ECL package `ext' has no
           ;; exported macro `without-interrupts' it's moved to `mp' package
           ;; https://github.com/Shirakumo/cl-out123/issues/2
           ;; https://gitlab.com/embeddable-common-lisp/ecl/-/blob/develop/src/lsp/mp.lsp
           (add-after 'unpack 'fix-ecl-package-name
             (lambda _
               (substitute* "wrapper.lisp"
                 (("ext:without-interrupts.*") "mp:without-interrupts\n"))
               #t)))))
      (inputs
       `(("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cffi" ,sbcl-cffi)
         ("documentation-utils" ,sbcl-documentation-utils)
         ("libout123" ,mpg123)
         ("trivial-features" ,sbcl-trivial-features)
         ("trivial-garbage" ,sbcl-trivial-garbage)))
      (home-page "https://shirakumo.github.io/cl-out123/")
      (synopsis "Common Lisp bindings to libout123")
      (description
       "This is a bindings library to @code{libout123} which allows easy
cross-platform audio playback.")
      (license license:zlib))))

(define-public ecl-cl-out123
  (sbcl-package->ecl-package sbcl-cl-out123))

(define-public cl-out123
  (sbcl-package->cl-source-package sbcl-cl-out123))

(define-public sbcl-cl-random-forest
  (let ((commit "fedb36ce99bb6f4d7e3a7dd6d8b058f331308f91")
        (revision "1"))
    (package
      (name "sbcl-cl-random-forest")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/masatoi/cl-random-forest")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0wqh4dxy5hrvm14jgyfypwhdw35f24rsksid4blz5a6l2z16rlmq"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("prove" ,sbcl-prove)
         ("trivial-garbage" ,sbcl-trivial-garbage)))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-libsvm-format" ,sbcl-cl-libsvm-format)
         ("cl-online-learning" ,sbcl-cl-online-learning)
         ("lparallel" ,sbcl-lparallel)))
      (arguments
       `(#:tests? #f)) ; The tests download data from the Internet
      (synopsis "Random Forest and Global Refinement for Common Lisp")
      (description
       "CL-random-forest is an implementation of Random Forest for multiclass
classification and univariate regression written in Common Lisp.  It also
includes an implementation of Global Refinement of Random Forest.")
      (home-page "https://github.com/masatoi/cl-random-forest")
      (license license:expat))))

(define-public cl-random-forest
  (sbcl-package->cl-source-package sbcl-cl-random-forest))

(define-public ecl-cl-random-forest
  (sbcl-package->ecl-package sbcl-cl-random-forest))

(define-public sbcl-bordeaux-fft
  (let ((commit "4a1f5600cae59bdabcb32de4ee2d7d73a9450d6e")
        (revision "0"))
    (package
      (name "sbcl-bordeaux-fft")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ahefner/bordeaux-fft")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0j584w6kq2k6r8lp2i14f9605rxhp3r15s33xs08iz1pndn6iwqf"))))
      (build-system asdf-build-system/sbcl)
      (home-page "http://vintage-digital.com/hefner/software/bordeaux-fft/")
      (synopsis "Fast Fourier Transform for Common Lisp")
      (description
       "The Bordeaux-FFT library provides a reasonably efficient implementation
of the Fast Fourier Transform and its inverse for complex-valued inputs, in
portable Common Lisp.")
      (license license:gpl2+))))

(define-public cl-bordeaux-fft
  (sbcl-package->cl-source-package sbcl-bordeaux-fft))

(define-public ecl-bordeaux-fft
  (sbcl-package->ecl-package sbcl-bordeaux-fft))

(define-public sbcl-napa-fft3
  (let ((commit "f2d9614c7167da327c9ceebefb04ff6eae2d2236")
        (revision "0"))
    (package
      (name "sbcl-napa-fft3")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pkhuong/Napa-FFT3")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hxjf599xgwm28gbryy7q96j9ys6hfszmv0qxpr5698hxnhknscp"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/pkhuong/Napa-FFT3")
      (synopsis "Fast Fourier Transform routines in Common Lisp")
      (description
       "Napa-FFT3 provides Discrete Fourier Transform (DFT) routines, but also
buildings blocks to express common operations that involve DFTs: filtering,
convolutions, etc.")
      (license license:bsd-3))))

(define-public cl-napa-fft3
  (sbcl-package->cl-source-package sbcl-napa-fft3))

(define-public sbcl-cl-tga
  (let ((commit "4dc2f7b8a259b9360862306640a07a23d4afaacc")
        (revision "0"))
    (package
      (name "sbcl-cl-tga")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fisxoj/cl-tga")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "03k3npmn0xd3fd2m7vwxph82av2xrfb150imqrinlzqmzvz1v1br"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/fisxoj/cl-tga")
      (synopsis "TGA file loader for Common Lisp")
      (description
       "Cl-tga was written to facilitate loading @emph{.tga} files into OpenGL
programs.  It's a very simple library, and, at the moment, only supports
non-RLE encoded forms of the files.")
      (license license:expat))))

(define-public cl-tga
  (sbcl-package->cl-source-package sbcl-cl-tga))

(define-public ecl-cl-tga
  (sbcl-package->ecl-package sbcl-cl-tga))

(define-public sbcl-com.gigamonkeys.binary-data
  (let ((commit "22e908976d7f3e2318b7168909f911b4a00963ee")
        (revision "0"))
    (package
      (name "sbcl-com.gigamonkeys.binary-data")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gigamonkey/monkeylib-binary-data")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "072v417vmcnvmyh8ddq9vmwwrizm7zwz9dpzi14qy9nsw8q649zw"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria))
      (home-page "https://github.com/gigamonkey/monkeylib-binary-data")
      (synopsis "Common Lisp library for reading and writing binary data")
      (description
       "This a Common Lisp library for reading and writing binary data.  It is
based on code from chapter 24 of the book @emph{Practical Common Lisp}.")
      (license license:bsd-3))))

(define-public cl-com.gigamonkeys.binary-data
  (sbcl-package->cl-source-package sbcl-com.gigamonkeys.binary-data))

(define-public ecl-com.gigamonkeys.binary-data
  (sbcl-package->ecl-package sbcl-com.gigamonkeys.binary-data))

(define-public sbcl-deflate
  (package
    (name "sbcl-deflate")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pmai/Deflate")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jpdjnxh6cw2d8hk70r2sxn92is52s9b855irvwkdd777fdciids"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/pmai/Deflate")
    (synopsis "Native deflate decompression for Common Lisp")
    (description
     "This library is an implementation of Deflate (RFC 1951) decompression,
with optional support for ZLIB-style (RFC 1950) and gzip-style (RFC 1952)
wrappers of deflate streams.  It currently does not handle compression.")
    (license license:expat)))

(define-public cl-deflate
  (sbcl-package->cl-source-package sbcl-deflate))

(define-public ecl-deflate
  (sbcl-package->ecl-package sbcl-deflate))

(define-public sbcl-skippy
  (let ((commit "e456210202ca702c792292c5060a264d45e47090")
        (revision "0"))
    (package
      (name "sbcl-skippy")
      (version (git-version "1.3.12" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/xach/skippy")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1sxbn5nh24qpx9w64x8mhp259cxcl1x8p126wk3b91ijjsj7l5vj"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://xach.com/lisp/skippy/")
      (synopsis "Common Lisp library for GIF images")
      (description
       "Skippy is a Common Lisp library to read and write GIF image files.")
      (license license:bsd-2))))

(define-public cl-skippy
  (sbcl-package->cl-source-package sbcl-skippy))

(define-public ecl-skippy
  (sbcl-package->ecl-package sbcl-skippy))

(define-public sbcl-cl-freetype2
  (let ((commit "96058da730b4812df916c1f4ee18c99b3b15a3de")
        (revision "0"))
    (package
      (name "sbcl-cl-freetype2")
      (version (git-version "1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rpav/cl-freetype2")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0f8darhairgxnb5bzqcny7nh7ss3471bdzix5rzcyiwdbr5kymjl"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("freetype" ,freetype)
         ("trivial-garbage" ,sbcl-trivial-garbage)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/ffi/ft2-lib.lisp"
                 (("\"libfreetype\"")
                  (string-append "\"" (assoc-ref inputs "freetype")
                                 "/lib/libfreetype\"")))
               (substitute* "src/ffi/grovel/grovel-freetype2.lisp"
                 (("-I/usr/include/freetype")
                  (string-append "-I" (assoc-ref inputs "freetype")
                                 "/include/freetype")))
               #t)))))
      (home-page "https://github.com/rpav/cl-freetype2")
      (synopsis "Common Lisp bindings for Freetype 2")
      (description
       "This is a general Freetype 2 wrapper for Common Lisp using CFFI.  It's
geared toward both using Freetype directly by providing a simplified API, as
well as providing access to the underlying C structures and functions for use
with other libraries which may also use Freetype.")
      (license license:bsd-3))))

(define-public cl-freetype2
  (sbcl-package->cl-source-package sbcl-cl-freetype2))

(define-public ecl-cl-freetype2
  (sbcl-package->ecl-package sbcl-cl-freetype2))

(define-public sbcl-opticl-core
  (let ((commit "b7cd13d26df6b824b216fbc360dc27bfadf04999")
        (revision "0"))
    (package
      (name "sbcl-opticl-core")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/slyrus/opticl-core")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0458bllabcdjghfrqx6aki49c9qmvfmkk8jl75cfpi7q0i12kh95"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria))
      (home-page "https://github.com/slyrus/opticl-core")
      (synopsis "Core classes and pixel access macros for Opticl")
      (description
       "This Common Lisp library contains the core classes and pixel access
macros for the Opticl image processing library.")
      (license license:bsd-2))))

(define-public cl-opticl-core
  (sbcl-package->cl-source-package sbcl-opticl-core))

(define-public ecl-opticl-core
  (sbcl-package->ecl-package sbcl-opticl-core))

(define-public sbcl-retrospectiff
  (let ((commit "c2a69d77d5010f8cdd9045b3e36a08a73da5d321")
        (revision "0"))
    (package
      (name "sbcl-retrospectiff")
      (version (git-version "0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/slyrus/retrospectiff")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qsn9hpd8j2kp43dk05j8dczz9zppdff5rrclbp45n3ksk9inw8i"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-cl-jpeg
             sbcl-com.gigamonkeys.binary-data
             sbcl-deflate
             sbcl-flexi-streams
             sbcl-ieee-floats
             sbcl-opticl-core))
      (home-page "https://github.com/slyrus/retrospectiff")
      (synopsis "Common Lisp library for TIFF images")
      (description
       "Retrospectiff is a common lisp library for reading and writing images
in the TIFF (Tagged Image File Format) format.")
      (license license:bsd-2))))

(define-public cl-retrospectif
  (sbcl-package->cl-source-package sbcl-retrospectiff))

(define-public ecl-retrospectiff
  (sbcl-package->ecl-package sbcl-retrospectiff))

(define-public sbcl-mmap
  (let ((commit "ba2e98c67e25f0fb8ff838238561120a23903ce7")
        (revision "0"))
    (package
      (name "sbcl-mmap")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/mmap")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qd0xp20i1pcfn12kkapv9pirb6hd4ns7kz4zf1mmjwykpsln96q"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-alexandria sbcl-cffi sbcl-parachute
             sbcl-trivial-features))
      (inputs
       (list sbcl-cffi sbcl-documentation-utils))
      (home-page "https://shinmera.github.io/mmap/")
      (synopsis "File memory mapping for Common Lisp")
      (description
       "This is a utility library providing access to the @emph{mmap} family of
functions in a portable way.  It allows you to directly map a file into the
address space of your process without having to manually read it into memory
sequentially.  Typically this is much more efficient for files that are larger
than a few Kb.")
      (license license:zlib))))

(define-public cl-mmap
  (sbcl-package->cl-source-package sbcl-mmap))

(define-public ecl-mmap
  (sbcl-package->ecl-package sbcl-mmap))

(define-public sbcl-3bz
  (let ((commit "569614c40408f3aefc77ba233e0e4bd66d3850ad")
        (revision "1"))
    (package
      (name "sbcl-3bz")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/3b/3bz")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0kvvlvf50jhhw1s510f3clpr1a68632bq6d698yxcrx722igcrg4"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria
             sbcl-babel
             sbcl-cffi
             sbcl-mmap
             sbcl-nibbles
             sbcl-trivial-features))
      (home-page "https://github.com/3b/3bz")
      (synopsis "Deflate decompression for Common Lisp")
      (description
       "3bz is an implementation of Deflate decompression (RFC 1951) optionally
with zlib (RFC 1950) or gzip (RFC 1952) wrappers, with support for reading from
foreign pointers (for use with mmap and similar, etc), and from CL octet
vectors and streams.")
      (license license:expat))))

(define-public cl-3bz
  (sbcl-package->cl-source-package sbcl-3bz))

(define-public ecl-3bz
  (sbcl-package->ecl-package sbcl-3bz))

(define-public sbcl-zpb-exif
  (package
    (name "sbcl-zpb-exif")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xach/zpb-exif")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15s227jhby55cisz14xafb0p1ws2jmrg2rrbbd00lrb97im84hy6"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://xach.com/lisp/zpb-exif/")
    (synopsis "EXIF information extractor for Common Lisp")
    (description
     "This is a Common Lisp library to extract EXIF information from image
files.")
    (license license:bsd-2)))

(define-public cl-zpb-exif
  (sbcl-package->cl-source-package sbcl-zpb-exif))

(define-public ecl-zpb-exif
  (sbcl-package->ecl-package sbcl-zpb-exif))

(define-public sbcl-pngload
  (let ((commit "91f1d703c65bb6a94d6fee06ddbbbbbc5778b71f")
        (revision "2"))
    (package
      (name "sbcl-pngload")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.mfiano.net/mfiano/pngload.git")
               (commit commit)))
         (file-name (git-file-name "pngload" version))
         (sha256
          (base32 "0s94fdbrbqj12qvgyn2g4lfwvz7qhhzbclrpz5ni7adwxgrmvxl1"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("3bz" ,sbcl-3bz)
         ("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("mmap" ,sbcl-mmap)
         ("parse-float" ,sbcl-parse-float)
         ("static-vectors" ,sbcl-static-vectors)
         ("swap-bytes" ,sbcl-swap-bytes)
         ("zpb-exif" ,sbcl-zpb-exif)))
      (arguments
       ;; Test suite disabled because of a dependency cycle.
       ;; pngload tests depend on opticl which depends on pngload.
       '(#:tests? #f))
      (home-page "https://git.mfiano.net/mfiano/pngload.git")
      (synopsis "PNG image decoder for Common Lisp")
      (description
       "This is a Common Lisp library to load images in the PNG image format,
both from files on disk, or streams in memory.")
      (license license:expat))))

(define-public cl-pngload
  (sbcl-package->cl-source-package sbcl-pngload))

(define-public ecl-pngload
  (sbcl-package->ecl-package sbcl-pngload))

(define-public sbcl-opticl
  (let ((commit "e8684416eca2e78e82a7b436d436ef2ea24c019d")
        (revision "0"))
    (package
      (name "sbcl-opticl")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/slyrus/opticl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "03rirnnhhisjbimlmpi725h1d3x0cfv00r57988am873dyzawmm1"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-jpeg" ,sbcl-cl-jpeg)
         ("cl-tga" ,sbcl-cl-tga)
         ("png-read" ,sbcl-png-read)
         ("pngload" ,sbcl-pngload)
         ("retrospectiff" ,sbcl-retrospectiff)
         ("skippy" ,sbcl-skippy)
         ("zpng" ,sbcl-zpng)))
      (arguments
       '(#:asd-files '("opticl.asd")))
      (home-page "https://github.com/slyrus/opticl")
      (synopsis "Image processing library for Common Lisp")
      (description
       "Opticl is a Common Lisp library for representing, processing, loading,
and saving 2-dimensional pixel-based images.")
      (license license:bsd-2))))

(define-public cl-opticl
  (sbcl-package->cl-source-package sbcl-opticl))

(define-public ecl-opticl
  (sbcl-package->ecl-package sbcl-opticl))

(define-public sbcl-mcclim
  (let ((commit "04cc542dd4b461b9d56406e40681d1a8f080730f")
        (revision "1"))
    (package
      (name "sbcl-mcclim")
      (version (git-version "0.9.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mcclim/mcclim")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1xjly8i62z72hfhlnz5kjd9i8xhrwckc7avyizxvhih67pkjmsx0"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam pkg-config))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("babel" ,sbcl-babel)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cl-freetype2" ,sbcl-cl-freetype2)
         ("cl-pdf" ,sbcl-cl-pdf)
         ("cffi" ,sbcl-cffi)
         ("cl-unicode" ,sbcl-cl-unicode)
         ("cl-vectors" ,sbcl-cl-vectors)
         ("closer-mop" ,sbcl-closer-mop)
         ("clx" ,sbcl-clx)
         ("flexi-streams" ,sbcl-flexi-streams)
         ("flexichain" ,sbcl-flexichain)
         ("font-dejavu" ,font-dejavu)
         ("fontconfig" ,fontconfig)
         ("freetype" ,freetype)
         ("harfbuzz" ,harfbuzz)
         ("log4cl" ,sbcl-log4cl)
         ("opticl" ,sbcl-opticl)
         ("spatial-trees" ,sbcl-spatial-trees)
         ("swank" ,sbcl-slime-swank)
         ("trivial-features" ,sbcl-trivial-features)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("trivial-gray-streams" ,sbcl-trivial-gray-streams)
         ("zpb-ttf" ,sbcl-zpb-ttf)))
      (arguments
       '(#:asd-systems '("mcclim"
                         "clim-examples")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               ;; mcclim-truetype uses DejaVu as default font and
               ;; sets the path at build time.
               (substitute* "Extensions/fonts/fontconfig.lisp"
                 (("/usr/share/fonts/truetype/dejavu/")
                  (string-append (assoc-ref inputs "font-dejavu")
                                 "/share/fonts/truetype/")))
               (substitute* "Extensions/fontconfig/src/functions.lisp"
                 (("libfontconfig\\.so")
                  (search-input-file inputs "/lib/libfontconfig.so")))
               (substitute* "Extensions/harfbuzz/src/functions.lisp"
                 (("libharfbuzz\\.so")
                  (search-input-file inputs "/lib/libharfbuzz.so")))))
           (add-after 'unpack 'fix-build
             (lambda _
               ;; The cffi-grovel system does not get loaded automatically,
               ;; so we load it explicitly.
               (substitute* "Extensions/fontconfig/mcclim-fontconfig.asd"
                 (("\\(asdf:defsystem #:mcclim-fontconfig" all)
                  (string-append "(asdf:load-system :cffi-grovel)\n" all)))
               (substitute* "Extensions/harfbuzz/mcclim-harfbuzz.asd"
                 (("\\(asdf:defsystem #:mcclim-harfbuzz" all)
                  (string-append "(asdf:load-system :cffi-grovel)\n" all)))
               #t)))))
      (home-page "https://common-lisp.net/project/mcclim/")
      (synopsis "Common Lisp GUI toolkit")
      (description
       "McCLIM is an implementation of the @emph{Common Lisp Interface Manager
specification}, a toolkit for writing GUIs in Common Lisp.")
      (license license:lgpl2.1+))))

(define-public cl-mcclim
  (sbcl-package->cl-source-package sbcl-mcclim))

(define-public ecl-mcclim
  (sbcl-package->ecl-package sbcl-mcclim))

(define-public sbcl-cl-inflector
  (let ((commit "f1ab16919ccce3bd82a0042677d9616dde2034fe")
        (revision "1"))
    (package
      (name "sbcl-cl-inflector")
      (version (git-version "0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AccelerationNet/cl-inflector")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1xwwlhik1la4fp984qnx2dqq24v012qv4x0y49sngfpwg7n0ya7y"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-lisp-unit2))
      (inputs
       (list sbcl-alexandria sbcl-cl-ppcre))
      (home-page "https://github.com/AccelerationNet/cl-inflector")
      (synopsis "Library to pluralize/singularize English and Portuguese words")
      (description
       "This is a common lisp library to easily pluralize and singularize
English and Portuguese words.  This is a port of the ruby ActiveSupport
Inflector module.")
      (license license:expat))))

(define-public cl-inflector
  (sbcl-package->cl-source-package sbcl-cl-inflector))

(define-public ecl-cl-inflector
  (sbcl-package->ecl-package sbcl-cl-inflector))

(define-public sbcl-ixf
  (let ((commit "ed26f87e4127e4a9e3aac4ff1e60d1f39cca5183")
        (revision "1"))
    (package
      (name "sbcl-ixf")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dimitri/cl-ixf")
               (commit commit)))
         (file-name (git-file-name "cl-ixf" version))
         (sha256
          (base32 "1wjdnf4vr9z7lcfc49kl43g6l2i23q9n81siy494k17d766cdvqa"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria
             sbcl-babel
             sbcl-cl-ppcre
             sbcl-ieee-floats
             sbcl-local-time
             sbcl-md5
             sbcl-split-sequence))
      (home-page "https://github.com/dimitri/cl-ixf")
      (synopsis "Parse IBM IXF file format")
      (description
       "This is a Common Lisp library to handle the IBM PC version of the IXF
(Integration Exchange Format) file format.")
      (license license:public-domain))))

(define-public ecl-ixf
  (sbcl-package->ecl-package sbcl-ixf))

(define-public cl-ixf
  (sbcl-package->cl-source-package sbcl-ixf))

(define-public sbcl-qbase64
  (package
    (name "sbcl-qbase64")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chaitanyagupta/qbase64")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dir0s70ca3hagxv9x15zq4p4ajgl7jrcgqsza2n2y7iqbxh0dwi"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-metabang-bind sbcl-trivial-gray-streams))
    (native-inputs
     (list sbcl-fiveam))
    (home-page "https://github.com/chaitanyagupta/qbase64")
    (synopsis "Base64 encoder and decoder for Common Lisp")
    (description "@code{qbase64} provides a fast and flexible base64 encoder
and decoder for Common Lisp.")
    (license license:bsd-3)))

(define-public cl-qbase64
  (sbcl-package->cl-source-package sbcl-qbase64))

(define-public ecl-qbase64
  (sbcl-package->ecl-package sbcl-qbase64))

(define-public sbcl-lw-compat
  ;; No release since 2013.
  (let ((commit "aabfe28c6c1a4949f9d7b3cb30319367c9fd1c0d"))
    (package
      (name "sbcl-lw-compat")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pcostanza/lw-compat/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "131rq5k2mlv9bfhmafiv6nfsivl4cxx13d9wr06v5jrqnckh4aav"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/pcostanza/lw-compat/")
      (synopsis "LispWorks utilities ported to other Common Lisp implementations")
      (description "This package contains a few utility functions from the
LispWorks library that are used in software such as ContextL.")
      (license license:expat))))

(define-public cl-lw-compat
  (sbcl-package->cl-source-package sbcl-lw-compat))

(define-public ecl-lw-compat
  (sbcl-package->ecl-package sbcl-lw-compat))

(define-public sbcl-contextl
  ;; No release since 2013.
  (let ((commit "5d18a71a85824f6c25a9f35a21052f967b8b6bb9"))
    (package
      (name "sbcl-contextl")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pcostanza/contextl/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0gk1izx6l6g48nypmnm9r6mzjx0jixqjj2kc6klf8a88rr5xd226"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-closer-mop sbcl-lw-compat))
      (home-page "https://github.com/pcostanza/contextl")
      (synopsis "Context-oriented programming for Common Lisp")
      (description "ContextL is a CLOS extension for Context-Oriented
Programming (COP).

Find overview of ContextL's features in an overview paper:
@url{http://www.p-cos.net/documents/contextl-soa.pdf}.  See also this general
overview article about COP which also contains some ContextL examples:
@url{http://www.jot.fm/issues/issue_2008_03/article4/}.")
      (license license:expat))))

(define-public cl-contextl
  (sbcl-package->cl-source-package sbcl-contextl))

(define-public ecl-contextl
  (sbcl-package->ecl-package sbcl-contextl))

(define-public sbcl-hu.dwim.common-lisp
  (let ((commit "90558195773383142a57a16687d5e7f4adea6418"))
    (package
      (name "sbcl-hu.dwim.common-lisp")
      (version "2021-01-27")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hu-dwim/hu.dwim.common-lisp/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "06zkdw3scnaw0d4nmsgkv7pi7sw00dikdgfgsqmbqfbz2yrsdabk"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-hu.dwim.asdf))
      (home-page "http://dwim.hu/project/hu.dwim.common-lisp")
      (synopsis "Redefine some standard Common Lisp names")
      (description "This library is a redefinition of the standard Common Lisp
package that includes a number of renames and shadows.")
      (license license:public-domain))))

(define-public cl-hu.dwim.common-lisp
  (sbcl-package->cl-source-package sbcl-hu.dwim.common-lisp))

(define-public ecl-hu.dwim.common-lisp
  (sbcl-package->ecl-package sbcl-hu.dwim.common-lisp))

(define-public sbcl-hu.dwim.common
  (package
    (name "sbcl-hu.dwim.common")
    (version "2015-07-09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://beta.quicklisp.org/archive/hu.dwim.common/"
             version "/hu.dwim.common-"
             (string-replace-substring version "-" "")
             "-darcs.tgz"))
       (sha256
        (base32 "12l1rr6w9m99w0b5gc6hv58ainjfhbc588kz6vwshn4gqsxyzbhp"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-hu.dwim.asdf))
    (inputs
     (list sbcl-alexandria
           sbcl-anaphora
           sbcl-closer-mop
           sbcl-hu.dwim.common-lisp
           sbcl-iterate
           sbcl-metabang-bind))
    (home-page "http://dwim.hu/")
    (synopsis "Common Lisp library shared by other hu.dwim systems")
    (description "This package contains a support library for other
hu.dwim systems.")
    (license license:public-domain)))

(define-public cl-hu.dwim.common
  (sbcl-package->cl-source-package sbcl-hu.dwim.common))

(define-public ecl-hu.dwim.common
  (sbcl-package->ecl-package sbcl-hu.dwim.common))

(define-public sbcl-hu.dwim.defclass-star
  (let ((commit "3086878a485074f9b2913c58267a9b764cd632fd"))
    (package
      (name "sbcl-hu.dwim.defclass-star")
      ;; We used to set version from the date when it was a darcs repo, so we
      ;; keep the year so that package gets updated on previous installs.
      (version (git-version "2021" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hu-dwim/hu.dwim.defclass-star")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "19ipds9r71qymfdp4izg0l7zmvinp06adr8rdalhaq7v7mzpg83z"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list ;; These 2 inputs are only needed tests which are disabled, see below.
             ;; ("hu.dwim.common" ,sbcl-hu.dwim.common)
             ;; Need cl- package for the :hu.dwim.stefil+hu.dwim.def+swank system.
             ;; ("hu.dwim.stefil" ,cl-hu.dwim.stefil)
             sbcl-hu.dwim.asdf))
      (arguments
       `(#:test-asd-file "hu.dwim.defclass-star.test.asd"
         ;; Tests require a circular dependency: hu.dwim.stefil -> hu.dwim.def
         ;; -> hu.dwim.util -> hu.dwim.defclass-star.
         #:tests? #f))
      (home-page "https://github.com/hu-dwim/hu.dwim.defclass-star")
      (synopsis "Simplify definitions with defclass* and friends in Common Lisp")
      (description "@code{defclass-star} provides defclass* and defcondition* to
simplify class and condition declarations.  Features include:

@itemize
@item Automatically export all or select slots at compile time.
@item Define the @code{:initarg} and @code{:accessor} automatically.
@item Specify a name transformer for both the @code{:initarg} and
@code{:accessor}, etc.
@item Specify the @code{:initform} as second slot value.
@end itemize

See
@url{https://common-lisp.net/project/defclass-star/configuration.lisp.html}
for an example.")
      (license license:public-domain))))

(define-public cl-hu.dwim.defclass-star
  (sbcl-package->cl-source-package sbcl-hu.dwim.defclass-star))

(define-public ecl-hu.dwim.defclass-star
  (sbcl-package->ecl-package sbcl-hu.dwim.defclass-star))

(define-public sbcl-livesupport
  (let ((commit "71e6e412df9f3759ad8378fabb203913d82e228a")
	(revision "1"))
    (package
      (name "sbcl-livesupport")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cbaggers/livesupport")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1rvnl0mncylbx63608pz5llss7y92j7z3ydambk9mcnjg2mjaapg"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/cbaggers/livesupport")
      (synopsis "Some helpers that make livecoding a little easier")
      (description "This package provides a macro commonly used in livecoding to
enable continuing when errors are raised.  Simply wrap around a chunk of code
and it provides a restart called @code{continue} which ignores the error and
carrys on from the end of the body.")
      (license license:bsd-2))))

(define-public cl-livesupport
  (sbcl-package->cl-source-package sbcl-livesupport))

(define-public ecl-livesupport
  (sbcl-package->ecl-package sbcl-livesupport))

(define-public sbcl-envy
  (let ((commit "956321b2852d58ba71c6fe621f5c2924178e9f88")
	(revision "1"))
    (package
      (name "sbcl-envy")
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/fukamachi/envy")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17iwrfxcdinjbb2h6l09qf40s7xkbhrpmnljlwpjy8l8rll8h3vg"))))
      (build-system asdf-build-system/sbcl)
      ;; (native-inputs ; Only for tests.
      ;;  `(("prove" ,sbcl-prove)
      ;;    ("osicat" ,sbcl-osicat)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "envy-test.asd"
                 (("cl-test-more") "prove"))
               #t)))
         ;; Tests fail with
         ;;   Component ENVY-ASD::ENVY-TEST not found, required by #<SYSTEM "envy">
         ;; like xsubseq.  Why?
         #:tests? #f))
      (synopsis "Common Lisp configuration switcher inspired by Perl's Config::ENV")
      (description "Envy is a configuration manager for various applications.
Envy uses an environment variable to determine a configuration to use.  This
can separate configuration system from an implementation.")
      (license license:bsd-2))))

(define-public cl-envy
  (sbcl-package->cl-source-package sbcl-envy))

(define-public ecl-envy
  (sbcl-package->ecl-package sbcl-envy))

(define-public sbcl-mito
  (let ((commit "2fbfc8aa6f9e3e8029bf09888c74b9af98dad341")
	(revision "2"))
    (package
      (name "sbcl-mito")
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/fukamachi/mito")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name "mito" version))
         (sha256
          (base32 "1a9kivpy9j2grf1c6gdjk7fwcdlvvq67p3m98jyfhiyzj7axjymd"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-prove))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-package-locks" ,sbcl-cl-package-locks)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("cl-reexport" ,sbcl-cl-reexport)
         ("closer-mop" ,sbcl-closer-mop)
         ("dbi" ,sbcl-dbi)
         ("dissect" ,sbcl-dissect)
         ("esrap" ,sbcl-esrap)
         ("local-time" ,sbcl-local-time)
         ("sxql" ,sbcl-sxql)
         ("trivia" ,sbcl-trivia)
         ("uuid" ,sbcl-uuid)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'remove-non-functional-tests
             (lambda _
               (substitute* "mito-test.asd"
                 (("\\(:test-file \"db/mysql\"\\)") "")
                 (("\\(:test-file \"db/postgres\"\\)") "")
                 (("\\(:test-file \"dao\"\\)") "")
                 ;; TODO: migration/sqlite3 should work, re-enable once
                 ;; upstream has fixed it:
                 ;; https://github.com/fukamachi/mito/issues/70
                 (("\\(:test-file \"migration/sqlite3\"\\)") "")
                 (("\\(:test-file \"migration/mysql\"\\)") "")
                 (("\\(:test-file \"migration/postgres\"\\)") "")
                 (("\\(:test-file \"postgres-types\"\\)") "")
                 (("\\(:test-file \"mixin\"\\)") ""))
               #t)))
         ;; TODO: While all enabled tests pass, the phase fails with:
         ;; Component MITO-ASD::MITO-TEST not found, required by #<SYSTEM "mito">
         #:tests? #f))
      (synopsis "ORM for Common Lisp with migrations and relationships support")
      (description "Mito is yet another object relational mapper, and it aims
to be a successor of Integral.

@itemize
@item Support MySQL, PostgreSQL and SQLite3.
@item Add id (serial/uuid primary key), created_at and updated_at by default
like Ruby's ActiveRecord.
@item Migrations.
@item Database schema versioning.
@end itemize\n")
      (license license:llgpl))))

(define-public cl-mito
  (sbcl-package->cl-source-package sbcl-mito))

(define-public ecl-mito
  (sbcl-package->ecl-package sbcl-mito))

(define-public sbcl-kebab
  (let ((commit "e7f77644c4e46131e7b8039d191d35fe6211f31b")
        (revision "1"))
    (package
      (name "sbcl-kebab")
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/pocket7878/kebab")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0j5haabnvj0vz0rx9mwyfsb3qzpga9nickbjw8xs6vypkdzlqv1b"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-ppcre" ,sbcl-cl-ppcre)
         ("alexandria" ,sbcl-alexandria)
         ("cl-interpol" ,sbcl-cl-interpol)
         ("split-sequence" ,sbcl-split-sequence)))
      (native-inputs
       (list sbcl-prove))
      (arguments
       ;; Tests passes but the phase fails with
       ;; Component KEBAB-ASD::KEBAB-TEST not found, required by #<SYSTEM "kebab">.
       `(#:tests? #f))
      (synopsis "Common Lisp case converter")
      (description "This Common Lisp library converts strings, symbols and
keywords between any of the following typographical cases: PascalCase,
camelCase, snake_case, kebab-case (lisp-case).")
      (license license:llgpl))))

(define-public cl-kebab
  (sbcl-package->cl-source-package sbcl-kebab))

(define-public ecl-kebab
  (sbcl-package->ecl-package sbcl-kebab))

(define-public sbcl-datafly
  (let ((commit "adece27fcbc4b5ea39ad1a105048b6b7166e3b0d")
        (revision "1"))
    (package
      (name "sbcl-datafly")
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/fukamachi/datafly")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16b78kzmglp2a4nxlxxl7rpf5zaibsgagn0p3c56fsxvx0c4hszv"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("iterate" ,sbcl-iterate)
         ("optima" ,sbcl-optima)
         ("trivial-types" ,sbcl-trivial-types)
         ("closer-mop" ,sbcl-closer-mop)
         ("cl-syntax" ,sbcl-cl-syntax)
         ("sxql" ,sbcl-sxql)
         ("dbi" ,sbcl-dbi)
         ("babel" ,sbcl-babel)
         ("local-time" ,sbcl-local-time)
         ("function-cache" ,sbcl-function-cache)
         ("jonathan" ,sbcl-jonathan)
         ("kebab" ,sbcl-kebab)
         ("log4cl" ,sbcl-log4cl)))
      (native-inputs
       (list sbcl-prove))
      (arguments
       ;; TODO: Tests fail with
       ;; While evaluating the form starting at line 22, column 0
       ;;   of #P"/tmp/guix-build-sbcl-datafly-0.1-1.adece27.drv-0/source/t/datafly.lisp":
       ;; Unhandled SQLITE:SQLITE-ERROR in thread #<SB-THREAD:THREAD "main thread" RUNNING
       ;; {10009F8083}>:
       ;;   Error when binding parameter 1 to value NIL.
       ;; Code RANGE: column index out of range.
       `(#:tests? #f))
      (synopsis "Lightweight database library for Common Lisp")
      (description "Datafly is a lightweight database library for Common Lisp.")
      (license license:bsd-3))))

(define-public cl-datafly
  (sbcl-package->cl-source-package sbcl-datafly))

(define-public ecl-datafly
  (sbcl-package->ecl-package sbcl-datafly))

(define-public sbcl-do-urlencode
  (let ((commit "199846441dad5dfac5478b8dee4b4e20d107af6a")
        (revision "1"))
    (package
      (name "sbcl-do-urlencode")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/drdo/do-urlencode")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0k2i3d4k9cpci235mwfm0c5a4yqfkijr716bjv7cdlpzx88lazm9"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-babel))
      (synopsis "Percent Encoding (aka URL Encoding) Common Lisp library")
      (description "This library provides trivial percent encoding and
decoding functions for URLs.")
      (license license:isc))))

(define-public cl-do-urlencode
  (sbcl-package->cl-source-package sbcl-do-urlencode))

(define-public ecl-do-urlencode
  (sbcl-package->ecl-package sbcl-do-urlencode))

(define-public sbcl-cl-emb
  (let ((commit "fd8652174d048d4525a81f38cdf42f4fa519f840")
        (revision "1"))
    (package
      (name "sbcl-cl-emb")
      (version (git-version "0.4.3" revision commit))
      (home-page "https://common-lisp.net/project/cl-emb/")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/38a938c2/cl-emb")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1xcm31n7afh5316lwz8iqbjx7kn5lw0l11arg8mhdmkx42aj4gkk"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre))
      (synopsis "Templating system for Common Lisp")
      (description "A mixture of features from eRuby and HTML::Template.  You
could name it \"Yet Another LSP\" (LispServer Pages) but it's a bit more than
that and not limited to a certain server or text format.")
      (license license:llgpl))))

(define-public cl-emb
  (sbcl-package->cl-source-package sbcl-cl-emb))

(define-public ecl-cl-emb
  (sbcl-package->ecl-package sbcl-cl-emb))

(define-public sbcl-cl-project
  (let ((commit "151107014e534fc4666222d57fec2cc8549c8814")
        (revision "1"))
    (package
      (name "sbcl-cl-project")
      (version (git-version "0.3.1" revision commit))
      (home-page "https://github.com/fukamachi/cl-project")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1rmh6s1ncv8s2yrr14ja9wisgg745sq6xibqwb341ikdicxdp26y"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-emb" ,sbcl-cl-emb)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("local-time" ,sbcl-local-time)
         ("prove" ,sbcl-prove)))
      (arguments
       ;; Tests depend on caveman, which in turns depends on cl-project.
       '(#:tests? #f
         #:asd-files '("cl-project.asd")))
      (synopsis "Generate a skeleton for modern Common Lisp projects")
      (description "This library provides a modern project skeleton generator.
In contract with other generators, CL-Project generates one package per file
and encourages unit testing by generating a system for unit testing, so you
can begin writing unit tests as soon as the project is generated.")
      (license license:llgpl))))

(define-public cl-project
  (sbcl-package->cl-source-package sbcl-cl-project))

(define-public ecl-cl-project
  (sbcl-package->ecl-package sbcl-cl-project))

(define-public sbcl-caveman
  (let ((commit "faa5f7e3b364fd7e7096af9a7bb06728b8d80441") ; No release since 2012
        (revision "1"))
    (package
      (name "sbcl-caveman")
      (version (git-version "2.4.0" revision commit))
      (home-page "http://8arrow.org/caveman/")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/caveman/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0kh0gx05pczk8f7r9qdi4zn1p3d0a2prps27k7jpgvc1dxkl8qhq"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("ningle" ,cl-ningle)
         ("lack" ,sbcl-lack)
         ("cl-project" ,sbcl-cl-project)
         ("dbi" ,sbcl-dbi)
         ("cl-syntax" ,sbcl-cl-syntax)
         ("myway" ,sbcl-myway)
         ("quri" ,sbcl-quri)))
      (native-inputs
       `(("usocket" ,sbcl-usocket)
         ("dexador" ,sbcl-dexador)))
      (arguments
       `(#:asd-files '("caveman2.asd")
         #:asd-systems '("caveman2")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'remove-v1
             (lambda _
               (delete-file-recursively "v1")
               (for-each delete-file
                         '("README.v1.markdown" "caveman.asd" "caveman-test.asd")))))
         ;; TODO: Tests fail with:
         ;; writing /gnu/store/...-sbcl-caveman-2.4.0-1.faa5f7e/share/common-lisp/sbcl-source/caveman2/v2/t/tmp/myapp573/tests/myapp573.lisp
         ;; While evaluating the form starting at line 38, column 0
         ;;   of #P"/tmp/guix-build-sbcl-caveman-2.4.0-1.faa5f7e.drv-0/source/v2/t/caveman.lisp":
         ;; Unhandled ASDF/FIND-COMPONENT:MISSING-COMPONENT in thread #<SB-THREAD:THREAD "main thread" RUNNING
         ;;                                                              {10009F8083}>:
         ;;   Component "myapp573" not found
         #:tests? #f))
      (synopsis "Lightweight web application framework in Common Lisp")
      (description "Caveman is intended to be a collection of common parts for
web applications.  Caveman2 has three design goals:

@itemize
@item Be extensible.
@item Be practical.
@item Don't force anything.
@end itemize\n")
      (license license:llgpl))))

(define-public cl-caveman
  (package
    (inherit
     (sbcl-package->cl-source-package sbcl-caveman))
    (propagated-inputs
     `(("ningle" ,cl-ningle)))))

(define-public ecl-caveman
  (sbcl-package->ecl-package sbcl-caveman))

(define-public sbcl-lambda-fiddle
  (let ((commit "d16bba55acf6065b412f64ab8fdff679a4a32b1e") ;; no tagged branch
	(revision "1"))
    (package
      (name "sbcl-lambda-fiddle")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/lambda-fiddle")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zarj1pqjqmk95kdx1axkgpwy2wq3canczk7f9z5hvaw5an6gand"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/Shinmera/lambda-fiddle")
      (synopsis "Collection of utilities to process lambda-lists")
      (description "This collection of utilities is useful in contexts where
you want a macro that uses lambda-lists in some fashion but need more precise
processing.")
      (license license:zlib))))

(define-public cl-lambda-fiddle
  (sbcl-package->cl-source-package sbcl-lambda-fiddle))

(define-public ecl-lambda-fiddle
  (sbcl-package->ecl-package sbcl-lambda-fiddle))

(define-public sbcl-xmls
  (let ((commit "18546f0850b1338e03997ffd1696add1cb1800d1") ;; no tagged branch
	(revision "1"))
    (package
      (name "sbcl-xmls")
      (version (git-version "3.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rpgoldman/xmls")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1lmvfml2ldbb1wkhm25jqqk2bhwsz52hhcgljbnzj1xr8xhc3anp"))))
      (native-inputs
       (list sbcl-fiveam))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/rpgoldman/xmls")
      (synopsis "Non-validating XML parser for Common Lisp")
      (description "Xmls is a self-contained, easily embedded parser that
recognizes a useful subset of the XML spec.  It provides a simple mapping from
XML to Lisp structures or s-expressions and back.")
      (license license:bsd-2))))

(define-public cl-xmls
  (sbcl-package->cl-source-package sbcl-xmls))

(define-public ecl-xmls
  (let ((pkg (sbcl-package->ecl-package sbcl-xmls)))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ;; Upstream doesn't have a test suite adapted for ECL.
         ((#:tests? _ #f) #f))))))

(define-public sbcl-geco
  (let ((commit "db13c9384491092975f46f6a837ccdc04681a93a")
        (revision "1"))
    (package
      (name "sbcl-geco")
      (version (git-version "2.1.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gpwwjr/GECO")
               (commit commit)))
         (file-name (git-file-name "cl-geco" version))
         (sha256
          (base32 "1ncaf9ab7jz59zmga0p97blsjjb1m6db0qih57wipfhqdb5ylz17"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/gpwwjr/GECO")
      (synopsis "Genetic algorithm toolkit for Common Lisp")
      (description
       "GECO (Genetic Evolution through Combination of Objects) is an
extensible, object-oriented framework for prototyping genetic algorithms in
Common Lisp.")
      (license license:lgpl2.0+))))

(define-public cl-geco
  (sbcl-package->cl-source-package sbcl-geco))

(define-public ecl-geco
  (sbcl-package->ecl-package sbcl-geco))

(define-public sbcl-html-entities
  (let ((commit "4af018048e891f41d77e7d680ed3aeb639e1eedb"))
    (package
      (name "sbcl-html-entities")
      (version (git-version "0.02" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/BnMcGn/html-entities/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1b2yl6lf6vis17y4n5s505p7ica96bdafcl6vydy1hg50fy33nfr"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/BnMcGn/html-entities/")
      (synopsis "Encode and decode entities in HTML with Common Lisp")
      (description "Html-entities is a Common Lisp library that lets you
encode and decode entities in HTML.")
      (license license:expat))))

(define-public cl-html-entities
  (sbcl-package->cl-source-package sbcl-html-entities))

(define-public ecl-html-entities
  (sbcl-package->ecl-package sbcl-html-entities))

(define-public sbcl-quicksearch
  (let ((commit "fb02ecf7c876ec580ab18c7d2c8c7814c06af599"))
    (package
      (name "sbcl-quicksearch")
      (version (git-version "0.01.04" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tkych/quicksearch/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16k19zjkhh7r64vjq371k5jwjs7cdfjz83flh561n4h4v1z89fps"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-bordeaux-threads
             sbcl-iterate
             sbcl-alexandria
             sbcl-anaphora
             sbcl-cl-ppcre
             sbcl-drakma
             sbcl-html-entities
             sbcl-yason
             sbcl-flexi-streams
             sbcl-do-urlencode))
      (home-page "https://github.com/tkych/quicksearch/")
      (synopsis "Search Engine Interface for Common Lisp packages")
      (description "Quicksearch is a search-engine-interface for Common Lisp.
The goal of Quicksearch is to find the Common Lisp library quickly.  For
example, if you will find the library about json, just type @code{(qs:?
'json)} at REPL.

The function @code{quicksearch} searches for Common Lisp projects in
Quicklisp, Cliki, GitHub and BitBucket, then outputs results in REPL.  The
function @code{?} is abbreviation wrapper for @code{quicksearch}.")
      (license license:expat))))

(define-public cl-quicksearch
  (sbcl-package->cl-source-package sbcl-quicksearch))

(define-public ecl-quicksearch
  (sbcl-package->ecl-package sbcl-quicksearch))

(define-public sbcl-agutil
  (let ((commit "df188d754d472da9faa1601a48f1f37bb7b34d68"))
    (package
      (name "sbcl-agutil")
      (version (git-version "0.0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alex-gutev/agutil/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1xpnyzksk2xld64b6lw6rw0gn5zxlb77jwna59sd4yl7kxhxlfpf"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-trivia))
      (home-page "https://github.com/alex-gutev/agutil/")
      (synopsis "Collection of Common Lisp utilities")
      (description "A collection of Common Lisp utility functions and macros
mostly not found in other utility packages.")
      (license license:expat))))

(define-public cl-agutil
  (sbcl-package->cl-source-package sbcl-agutil))

(define-public ecl-agutil
  (sbcl-package->ecl-package sbcl-agutil))

(define-public sbcl-custom-hash-table
  (let ((commit "f26983133940f5edf826ebbc8077acc04816ddfa"))
    (package
      (name "sbcl-custom-hash-table")
      (version (git-version "0.3" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/metawilm/cl-custom-hash-table")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1k4mvrpbqqds2fwjxp1bxmrfmr8ch4dkwhnkbw559knbqshvrlj5"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:asd-files '("cl-custom-hash-table.asd")
         #:asd-systems '("cl-custom-hash-table")))
      (home-page "https://github.com/metawilm/cl-custom-hash-table")
      (synopsis "Custom hash tables for Common Lisp")
      (description "This library allows creation of hash tables with arbitrary
@code{test}/@code{hash} functions, in addition to the @code{test} functions
allowed by the standard (@code{EQ}, @code{EQL}, @code{EQUAL} and
@code{EQUALP}), even in implementations that don't support this functionality
directly.")
      (license license:expat))))

(define-public cl-custom-hash-table
  (sbcl-package->cl-source-package sbcl-custom-hash-table))

(define-public ecl-custom-hash-table
  (sbcl-package->ecl-package sbcl-custom-hash-table))

(define-public sbcl-collectors
  (let ((commit "13acef25d8422d1d82e067b1861e513587c166ee"))
    (package
      (name "sbcl-collectors")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AccelerationNet/collectors")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1si68n1j6rpns8jw6ksqjpb937pdl30v7xza8rld7j5vh0jhy2yi"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-closer-mop sbcl-symbol-munger))
      (native-inputs
       (list sbcl-lisp-unit2))
      (home-page "https://github.com/AccelerationNet/collectors/")
      (synopsis "Common lisp library providing collector macros")
      (description "A small collection of common lisp macros to make
collecting values easier.")
      (license license:bsd-3))))

(define-public cl-collectors
  (sbcl-package->cl-source-package sbcl-collectors))

(define-public ecl-collectors
  (sbcl-package->ecl-package sbcl-collectors))

(define-public sbcl-cl-environments
  (package
    (name "sbcl-cl-environments")
    (version "0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alex-gutev/cl-environments")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cl-environments" version))
       (sha256
        (base32 "10jxj043d2dw5vc0i0lz0lsa4qszn8him5is8jdhl4nsyfcazmky"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-alexandria sbcl-anaphora sbcl-collectors sbcl-optima))
    (native-inputs
     (list sbcl-prove))
    (home-page "https://github.com/alex-gutev/cl-environments")
    (synopsis "Implements the Common Lisp standard environment access API")
    (description
     "This library provides a uniform API, as specified in Common Lisp the
Language 2, for accessing information about variable and function bindings
from implementation-defined lexical environment objects.  All major Common
Lisp implementations are supported, even those which don't support the CLTL2
environment access API.")
    (license license:expat)))

(define-public cl-environments
  (sbcl-package->cl-source-package sbcl-cl-environments))

(define-public ecl-cl-environments
  (sbcl-package->ecl-package sbcl-cl-environments))

(define-public sbcl-static-dispatch
  (package
    (name "sbcl-static-dispatch")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alex-gutev/static-dispatch")
             (commit (string-append "v" version))))
       (file-name (git-file-name "static-dispatch" version))
       (sha256
        (base32 "1602vx6ybp0n8mbrrp6q8397fkkyvhrqpahc302pjdb57qildajz"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-fiveam))
    (inputs
     (list sbcl-agutil
           sbcl-alexandria
           sbcl-anaphora
           sbcl-arrows
           sbcl-cl-environments
           sbcl-closer-mop
           sbcl-iterate
           sbcl-optima))
    (home-page "https://github.com/alex-gutev/static-dispatch")
    (synopsis "Static generic function dispatch for Common Lisp")
    (description "Static dispatch is a Common Lisp library, inspired by
@code{inlined-generic-function}, which allows standard Common Lisp generic
function dispatch to be performed statically (at compile time) rather than
dynamically (runtime).  This is similar to what is known as \"overloading\" in
languages such as C++ and Java.

The purpose of static dispatch is to provide an optimization in cases where
the usual dynamic dispatch is too slow, and the dynamic features of generic
functions, such as adding/removing methods at runtime are not required.  An
example of such a case is a generic equality comparison function.  Currently
generic functions are considered far too slow to implement generic arithmetic
and comparison operations when used heavily in numeric code.")
    (license license:expat)))

(define-public cl-static-dispatch
  (sbcl-package->cl-source-package sbcl-static-dispatch))

(define-public ecl-static-dispatch
  (sbcl-package->ecl-package sbcl-static-dispatch))

(define-public sbcl-cl-form-types
  (package
    (name "sbcl-cl-form-types")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alex-gutev/cl-form-types")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "17kdjqmm2ib347b8lqm3k4kca2j53kr0azb6h7m0v5i157ibndsw"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-agutil
           sbcl-anaphora
           sbcl-arrows
           sbcl-cl-environments
           sbcl-introspect-environment
           sbcl-optima))
    (native-inputs
     (list sbcl-fiveam))
    (home-page "https://github.com/alex-gutev/cl-form-types")
    (synopsis "determining the types of Common Lisp forms")
    (description "This library provides functions for determining the value
types of Common Lisp forms, based on type information contained in the
environment.

In order for this library to work the values types of variables and return
types of functions have to be declared.

Macros and symbol-macros are fully expanded and all special forms, except
@code{CATCH}, are supported.")
    (license license:expat)))

(define-public cl-form-types
  (sbcl-package->cl-source-package sbcl-cl-form-types))

(define-public ecl-cl-form-types
  (sbcl-package->ecl-package sbcl-cl-form-types))

(define-public sbcl-generic-cl
  (package
    (name "sbcl-generic-cl")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alex-gutev/generic-cl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1c40vqb49g0adfv17jxgk0ds1n6a2dph30cibq01sicmqdgrrbi8"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("agutil" ,sbcl-agutil)
       ("alexandria" ,sbcl-alexandria)
       ("anaphora" ,sbcl-anaphora)
       ("arrows" ,sbcl-arrows)
       ("cl-custom-hash-table" ,sbcl-custom-hash-table)
       ("cl-form-types" ,sbcl-cl-form-types)
       ("static-dispatch" ,sbcl-static-dispatch)
       ("trivia" ,sbcl-trivia)))
    (native-inputs
     (list sbcl-prove))
    (arguments
     ;; Tests fail because SBCL head size is not high enough.
     ;; https://github.com/alex-gutev/generic-cl/issues/6
     `(#:tests? #f))
    (home-page "https://alex-gutev.github.io/generic-cl/")
    (synopsis "Generic function interface to standard Common Lisp functions")
    (description "@code{generic-cl} provides a generic function wrapper over
various functions in the Common Lisp standard, such as equality predicates and
sequence operations.  The goal of this wrapper is to provide a standard
interface to common operations, such as testing for the equality of two
objects, which is extensible to user-defined types.")
    (license license:expat)))

(define-public cl-generic-cl
  (sbcl-package->cl-source-package sbcl-generic-cl))

(define-public ecl-generic-cl
  (sbcl-package->ecl-package sbcl-generic-cl))

(define-public sbcl-defpackage-plus
  (let ((revision "0")
        (commit "5492e27e0bdb7b75fa5177ea4388519dc7a75f11"))
    (package
      (name "sbcl-defpackage-plus")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rpav/defpackage-plus")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0lzljvf343xb6mlh6lni2i27hpm5qd376522mk6hr2pa20vd6rdq"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria))
      (home-page "https://github.com/rpav/defpackage-plus")
      (synopsis "Extensible @code{DEFPACKAGE} variant with version support")
      (description
       "@code{DEFPACKAGE-PLUS} is an extensible @code{DEFPACKAGE} variant with
predictable cross-platform behavior and some utilities useful for versioning.")
      (license license:bsd-2))))

(define-public cl-defpackage-plus
  (sbcl-package->cl-source-package sbcl-defpackage-plus))

(define-public ecl-defpackage-plus
  (sbcl-package->ecl-package sbcl-defpackage-plus))

(define-public sbcl-deploy
  (let ((commit "9b20e64fe924b9e31832304d87a3a72c383dc6d8")
        (revision "2"))
    (package
      (name "sbcl-deploy")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/deploy")
               (commit commit)))
         (file-name (git-file-name "deploy" version))
         (sha256
          (base32 "07pfkibaridihg8lbq2czwa4iqifqk24n6rx7bfnv7i49p1ppja1"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:test-asd-file "deploy-test.asd"
         #:asd-files '("deploy.asd"
                       "deploy-test.asd")))
      (native-inputs
       `(("cl-mpg123" ,sbcl-cl-mpg123)
         ("cl-out123" ,sbcl-cl-out123)))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("documentation-utils" ,sbcl-documentation-utils)
         ("trivial-features" ,sbcl-trivial-features)))
      (home-page "https://shinmera.github.io/deploy/")
      (synopsis "Deployment tools for standalone Common Lisp application")
      (description
       "This is a system to help you easily and quickly deploy standalone
common lisp applications as binaries.  Specifically it is geared towards
applications with foreign library dependencies that run some kind of GUI.")
      (license license:artistic2.0))))

(define-public cl-deploy
  (sbcl-package->cl-source-package sbcl-deploy))

(define-public ecl-deploy
  (sbcl-package->ecl-package sbcl-deploy))

(define-public sbcl-deeds
  ;; taged branch is outdated
  (let ((revision "1")
        (commit "f5df54eac79b58a34030e0eb8acf3952c788410d"))
    (package
      (name "sbcl-deeds")
      (version (git-version "1.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/deeds")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "062cnb2dwli6pw3zvv46jfxyxdzcbzwsck5pa6nw03qf1j1hyg3k"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-bordeaux-threads sbcl-closer-mop sbcl-form-fiddle
             sbcl-lambda-fiddle))
      (home-page "https://github.com/Shinmera/deeds")
      (synopsis "Extensible Event Delivery System")
      (description
       "@code{deeds} allows for efficient event delivery to multiple handlers
with a complex event filtering system.")
      (license license:zlib))))

(define-public cl-deeds
  (sbcl-package->cl-source-package sbcl-deeds))

(define-public ecl-deeds
  (sbcl-package->ecl-package sbcl-deeds))

(define-public sbcl-make-hash
  ;; no tagged branch
  (let ((revision "1")
        (commit "ae0909cd8e697520a1085fac6f54ac2b448ebd21"))
    (package
      (name "sbcl-make-hash")
      (version (git-version "1.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/genovese/make-hash")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qa4mcmb3pv44py0j129dd8hjx09c2akpnds53b69151mgwv5qz8"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/genovese/make-hash")
      (synopsis "Common Lisp package for flexible hash table creation")
      (description
       "This is a Common Lisp package for hash table creation with flexible,
extensible initializers.")
      (license license:bsd-3))))

(define-public cl-make-hash
  (sbcl-package->cl-source-package sbcl-make-hash))

(define-public ecl-make-hash
  (sbcl-package->ecl-package sbcl-make-hash))

(define-public sbcl-claw-support
  (package
    (name "sbcl-claw-support")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/borodust/claw-support")
             (commit "9a15c8bed04585f45e6a461bcda1b475144dbd0b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1my2ka7h72ipx5n3b465g6kjkasrhsvhqlijwcg6dhlzs5yygl23"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/borodust/claw-support")
    (synopsis "Support routines for claw")
    (description
     "This package provides support routines for the @code{claw} Common Lisp
package.")
    (license license:expat)))

(define-public cl-claw-support
  (sbcl-package->cl-source-package sbcl-claw-support))

(define-public ecl-claw-support
  (sbcl-package->ecl-package sbcl-claw-support))

(define-public sbcl-claw
  (let ((revision "0")
        (commit "3cd4a96fca95eb9e8d5d069426694669f81b2250"))
    (package
      (name "sbcl-claw")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/borodust/claw")
               (commit commit)))
         (file-name (git-file-name "claw" version))
         (sha256
          (base32 "146yv0hc4hmk72562ssj2d41143pp84dcbd1h7f4nx1c7hf2bb0d"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria
             sbcl-cffi
             sbcl-cl-json
             sbcl-cl-ppcre
             sbcl-claw-support
             sbcl-local-time
             sbcl-trivial-features))
      (home-page "https://github.com/borodust/claw")
      (synopsis "Autowrapper for Common Lisp")
      (description
       "This is a Common Lisp autowrapping facility for quickly creating clean
and lean bindings to C libraries.")
      (license license:bsd-2))))

(define-public cl-claw
  (sbcl-package->cl-source-package sbcl-claw))

(define-public ecl-claw
  (sbcl-package->ecl-package sbcl-claw))

(define-public sbcl-claw-utils
  (let ((revision "0")
        (commit "efe25016501973dc369f067a64c7d225802bc56f"))
    (package
      (name "sbcl-claw-utils")
      ;; version is not specified
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/borodust/claw-utils")
               (commit commit)))
         (file-name (git-file-name "claw-utils" version))
         (sha256
          (base32 "01df3kyf2qs3czi332dnz2s35x2j0fq46vgmsw7wjrrvnqc22mk5"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-cffi sbcl-claw))
      (home-page "https://github.com/borodust/claw-utils")
      (synopsis "Utilities for easier autowrapping")
      (description
       "This Common Lisp library contains various handy utilities to help
autowrapping with @code{claw}.")
      (license license:expat))))

(define-public cl-claw-utils
  (sbcl-package->cl-source-package sbcl-claw-utils))

(define-public ecl-claw-utils
  (sbcl-package->ecl-package sbcl-claw-utils))

(define-public sbcl-array-operations
  (let ((commit "75cbc3b1adb2e3ce2109489753d0f290b071e81b")
        (revision "0"))
    (package
      (name "sbcl-array-operations")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/bendudson/array-operations")
               (commit commit)))
         (file-name (git-file-name "array-operations" version))
         (sha256
          (base32 "0ip49hhq32w80qsc7jmspyda5r2rsszvw0mk2r3341cld78sz9ya"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-alexandria sbcl-clunit2))
      (inputs
       (list sbcl-let-plus))
      (synopsis "Simple array operations library for Common Lisp")
      (description
       "This library is a collection of functions and macros for manipulating
Common Lisp arrays and performing numerical calculations with them.")
      (home-page "https://github.com/bendudson/array-operations")
      (license license:expat))))

(define-public cl-array-operations
  (sbcl-package->cl-source-package sbcl-array-operations))

(define-public ecl-array-operations
  (sbcl-package->ecl-package sbcl-array-operations))

(define-public sbcl-clml
  (let ((commit "95505b54c8c7b4b27f500c3be97fa5732f4b51a8")
        (revision "0"))
    (package
      (name "sbcl-clml")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mmaul/clml")
               (commit commit)))
         (file-name (git-file-name "clml" version))
         (sha256
          (base32 "006pii59nmpc61n7p7h8ha5vjg6x0dya327i58z0rnvxs249h345"))
         ;; TODO: Remove this when the patch has been merged upstream.
         (patches (search-patches "sbcl-clml-fix-types.patch"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria
             sbcl-array-operations
             sbcl-cl-fad
             sbcl-cl-ppcre
             sbcl-drakma
             sbcl-introspect-environment
             sbcl-iterate
             sbcl-lparallel
             sbcl-parse-number
             sbcl-split-sequence
             sbcl-trivial-garbage))
      (synopsis "Common Lisp machine learning library")
      (description
       "CLML (Common Lisp Machine Learning) is a high performance and large
scale statistical machine learning package")
      (home-page "https://mmaul.github.io/clml/")
      (license license:llgpl))))

(define-public cl-clml
  (sbcl-package->cl-source-package sbcl-clml))

(define-public sbcl-utm-ups
  (let ((commit "ffcb7b6d5a56fb7d4b2b95b83bbd28ffe6e6961f")
        (revision "0"))
    (package
      (name "sbcl-utm-ups")
      (version (git-version "1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/utm-ups")
               (commit commit)))
         (file-name (git-file-name "utm-ups" version))
         (sha256
          (base32 "1rvyh0srgd81kvbzmq4ysd9y6c0qdwh23naqxc9asw1vh7fq08x1"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (synopsis
       "Convert coordinates between latitude/longitude and UTM or UPS")
      (description
       "This a Common Lisp library to convert geographic coordinates between
latitude/longitude and UTM (Universal Transverse Mercator) or UPS (Universal
Polar Stereographic).")
      (home-page "https://github.com/glv2/utm-ups")
      (license license:gpl3+))))

(define-public cl-utm-ups
  (sbcl-package->cl-source-package sbcl-utm-ups))

(define-public ecl-utm-ups
  (sbcl-package->ecl-package sbcl-utm-ups))

(define-public sbcl-mgrs
  (let ((commit "00455460407b7e5509d1be3da09bf6152956761f")
        (revision "0"))
    (package
      (name "sbcl-mgrs")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/mgrs")
               (commit commit)))
         (file-name (git-file-name "mgrs" version))
         (sha256
          (base32 "0ckvn4hg3wwivzavhfashb6fap4a1q10l8krhbng8bdb54ac10sz"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-utm-ups))
      (synopsis
       "Convert coordinates between latitude/longitude and MGRS")
      (description
       "This a Common Lisp library to convert geographic coordinates between
latitude/longitude and MGRS.")
      (home-page "https://github.com/glv2/mgrs")
      (license license:gpl3+))))

(define-public cl-mgrs
  (sbcl-package->cl-source-package sbcl-mgrs))

(define-public ecl-mgrs
  (sbcl-package->ecl-package sbcl-mgrs))

(define-public sbcl-maidenhead
  (let ((commit "b756d235c27b5d6798867aa240318af1a8f35d6d")
        (revision "0"))
    (package
      (name "sbcl-maidenhead")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/maidenhead")
               (commit commit)))
         (file-name (git-file-name "maidenhead" version))
         (sha256
          (base32 "02p990zprhjvifmsfk8yh3frvz6xyw26ikzxvzglqdixbal36nr3"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (synopsis
       "Convert coordinates between latitude/longitude and Maidenhead")
      (description
       "This a Common Lisp library to convert geographic coordinates between
latitude/longitude and Maidenhead locator system.")
      (home-page "https://github.com/glv2/maidenhead")
      (license license:gpl3+))))

(define-public cl-maidenhead
  (sbcl-package->cl-source-package sbcl-maidenhead))

(define-public ecl-maidenhead
  (sbcl-package->ecl-package sbcl-maidenhead))

(define-public sbcl-olc
  (let ((commit "517e27fa57d9a119b00a29c4b6b31e553deff309")
        (revision "0"))
    (package
      (name "sbcl-olc")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/olc")
               (commit commit)))
         (file-name (git-file-name "olc" version))
         (sha256
          (base32 "1lnfhp6z6kc8l605zp4siyjiw74y1h4bdq3jfizi084v505wxhgr"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (synopsis
       "Convert coordinates between latitude/longitude and Open Location Code")
      (description
       "This a Common Lisp library to convert geographic coordinates between
latitude/longitude and Open Location Code.")
      (home-page "https://github.com/glv2/olc")
      (license license:gpl3+))))

(define-public cl-olc
  (sbcl-package->cl-source-package sbcl-olc))

(define-public ecl-olc
  (sbcl-package->ecl-package sbcl-olc))

(define-public sbcl-regex
  (let ((commit "fbc9a9f313b9edc1788f33d4b23a29151635ae22"))
    (package
      (name "sbcl-regex")
      (version (git-version "1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/michaelw/regex/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0wq5wlafrxv13wg28hg5b10sc48b88swsvznpy2zg7x37m4nmm6a"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/michaelw/regex/")
      (synopsis "Regular expression engine for Common Lisp")
      (description
       "This Common Lisp package provides a regular expression engine.")
      (license license:bsd-2))))

(define-public cl-regex
  (sbcl-package->cl-source-package sbcl-regex))

(define-public ecl-regex
  (sbcl-package->ecl-package sbcl-regex))

(define-public sbcl-clawk
  (let ((commit "3a91634df686417114044a98c063cbe76bfac7b6"))
    (package
      (name "sbcl-clawk")
      (version (git-version "4" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/clawk")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ph3xjqilvinvgr9q3w47zxqyz1sqnq030nlx7kgkkv8j3bnqk7a"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-regex))
      (home-page "https://github.com/sharplispers/clawk")
      (synopsis "Common Lisp AWK")
      (description
       "CLAWK is an AWK implementation embedded into Common Lisp.")
      (license license:bsd-2))))

(define-public cl-clawk
  (sbcl-package->cl-source-package sbcl-clawk))

(define-public ecl-clawk
  (sbcl-package->ecl-package sbcl-clawk))

(define-public sbcl-clamp
  (let ((commit "02b8f3953e5753cc61a719807c82f3795cd28fe1"))
    (package
      (name "sbcl-clamp")
      (version (git-version "0.3" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/arclanguage/Clamp")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0fdr9nqfmmpxm6hvjdxi1jkclya9xlnrw1yc3cn1m4ww3f50p31m"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("iterate" ,sbcl-iterate)
         ("cl-syntax" ,sbcl-cl-syntax)))
      (native-inputs
       `(("cl-unit" ,sbcl-clunit)
         ("check-it" ,sbcl-check-it)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-build
             (lambda _
               (substitute* "clamp.asd"
                 (("\\(:file \"read\"     :depends-on \\(\"aliases\"\\)\\)")
                  "(:file \"read\"     :depends-on (\"aliases\" \"base\"))"))
               #t)))))
      (home-page "https://github.com/arclanguage/Clamp")
      (synopsis "Common Lisp with Arc macros and procedures")
      (description
       "Clamp is an attempt to bring the powerful, but verbose, language of
Common Lisp up to the terseness of Arc.

There are two parts to Clamp.  There is the core of Clamp, which implements
the utilities of Arc that are easily converted from Arc to Common Lisp.  The
other part is the \"experimental\" part.  It contains features of Arc that are
not so easy to copy (ssyntax, argument destructuring, etc.).")
      (license license:artistic2.0))))

(define-public cl-clamp
  (sbcl-package->cl-source-package sbcl-clamp))

(define-public ecl-clamp
  (sbcl-package->ecl-package sbcl-clamp))

(define-public sbcl-trivial-shell
  (let ((commit "e02ec191b34b52deca5d1c4ee99d4fa13b8772e0"))
    (package
      (name "sbcl-trivial-shell")
      (version (git-version "0.2.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gwkkwg/trivial-shell")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "08mpkl5ij5sjfsyn8pq2kvsvpvyvr7ha1r8g1224fa667b8k2q85"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-lift))
      (home-page "http://common-lisp.net/project/trivial-shell/")
      (synopsis "Common Lisp access to the shell")
      (description
       "A simple Common-Lisp interface to the underlying operating system.
It's independent of the implementation and operating system.")
      (license license:expat))))

(define-public cl-trivial-shell
  (sbcl-package->cl-source-package sbcl-trivial-shell))

(define-public ecl-trivial-shell
  (sbcl-package->ecl-package sbcl-trivial-shell))

(define-public sbcl-clesh
  (let ((commit "44e96e04a72e5bc006dc4eb02ce8962348dd4a11"))
    (package
      (name "sbcl-clesh")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Neronus/Clesh")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "012ry02djnqyvvs61wbbqj3saz621w2l9gczrywdxhi5p4ycx318"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-trivial-shell sbcl-named-readtables))
      (home-page "https://github.com/Neronus/Clesh")
      (synopsis "Embed shell code in Common Lisp")
      (description
       "This is a very short and simple program, written in Common Lisp, that
extends Common Lisp to embed shell code in a manner similar to Perl's
backtick.  It has been forked from SHELISP.")
      (license license:bsd-2))))

(define-public cl-clesh
  (sbcl-package->cl-source-package sbcl-clesh))

(define-public ecl-clesh
  (sbcl-package->ecl-package sbcl-clesh))

(define-public sbcl-trivial-channels
  (let ((commit "e2370118d8983ba69c0360a7695f8f2e2fd6a8a6")
        (revision "1"))
    (package
      (name "sbcl-trivial-channels")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rpav/trivial-channels")
               (commit commit)))
         (file-name (git-file-name "trivial-channels" version))
         (sha256
          (base32 "04wnxcgk40x8p0gxnz9arv1a5wasdqrdxa8c4p5v7r2mycfps6jj"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-bordeaux-threads sbcl-trivial-timeout))
      (home-page "https://github.com/rpav/trivial-channels")
      (synopsis "Common Lisp simple thread-safe channels with timeout")
      (description
       "It's very basic implementation of channels and queue for Common Lisp.")
      (license license:bsd-2))))

(define-public ecl-trivial-channels
  (sbcl-package->ecl-package sbcl-trivial-channels))

(define-public cl-trivial-channels
  (sbcl-package->cl-source-package sbcl-trivial-channels))

(define-public sbcl-trivial-download
  (let ((commit "d2472061d86b1cf3d32f388daacd4e32a13af699"))
    (package
      (name "sbcl-trivial-download")
      (version (git-version "0.3" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eudoxia0/trivial-download/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "06f46zr3gp3wlm2kgxna24qd2gpr1v89x9fynh1x5vrw6c6hqjcv"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-drakma))
      (home-page "https://github.com/eudoxia0/trivial-download/")
      (synopsis "Download files from Common Lisp")
      (description
       "@code{trivial-download} allows you to download files from the Internet
from Common Lisp.  It provides a progress bar.")
      (license license:bsd-2))))

(define-public cl-trivial-download
  (sbcl-package->cl-source-package sbcl-trivial-download))

(define-public ecl-trivial-download
  (sbcl-package->ecl-package sbcl-trivial-download))

(define-public sbcl-gtwiwtg
  (package
    (name "sbcl-gtwiwtg")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cbeo/gtwiwtg/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lkraw0dwh4is4x5sp5rjrw6f93m0gr9849abrbi12s25ws7jbw4"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-osicat sbcl-prove))
    (home-page "https://github.com/cbeo/gtwiwtg/")
    (synopsis "Naive generators for Common Lisp")
    (description
     "The GTWIWTG library (Generators The Way I Want Them Generated --
technically not generators, but iterators) is meant to be small, explorable,
and understandable.")
    (license license:gpl3)))

(define-public cl-gtwiwtg
  (sbcl-package->cl-source-package sbcl-gtwiwtg))

(define-public ecl-gtwiwtg
  (sbcl-package->ecl-package sbcl-gtwiwtg))

(define-public sbcl-cl-progress-bar
  (let ((commit "9374170858663c8fe829e9fb5a29bd2cb48d95ae"))
    (package
      (name "sbcl-cl-progress-bar")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sirherrbatka/cl-progress-bar/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ldb4qhmx431n3lsq71ynwb9ybazbfqd55icjbhi06mj52ngndir"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-bordeaux-threads sbcl-documentation-utils-extensions))
      (home-page "https://github.com/sirherrbatka/cl-progress-bar/")
      (synopsis "Progress bars in Common Lisp")
      (description
       "This library provides almost the same code as used inside Quicklisp
for drawning progress bars")
      (license license:expat))))

(define-public cl-progress-bar
  (sbcl-package->cl-source-package sbcl-cl-progress-bar))

(define-public ecl-cl-progress-bar
  (sbcl-package->ecl-package sbcl-cl-progress-bar))

(define-public sbcl-repl-utilities
  (let ((commit "7e300df663177ea4581f4e7e9c601377881dd986"))
    (package
      (name "sbcl-repl-utilities")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/m-n/repl-utilities/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hh56pq5nw3l4b83dzlyss69f06r038byj2cnjwvci4hfjhdfcc3"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/m-n/repl-utilities")
      (synopsis "Ease common tasks at the Common Lisp REPL")
      (description
       "@code{repl-utilities} is a set of utilities which ease life at the
REPL.  It includes three sorts of features: introspective procedures,
miscellaneous utility functions, and, pulling them together, methods to
conveniently keep these symbols and optionally additional symbols available in
whichever package you switch to.")
      (license license:bsd-2))))

(define-public cl-repl-utilities
  (sbcl-package->cl-source-package sbcl-repl-utilities))

(define-public ecl-repl-utilities
  (sbcl-package->ecl-package sbcl-repl-utilities))

(define-public sbcl-supertrace
  (let ((commit "66d22c3ff131ecd1c8048dfced1d62ed6024ecb0"))
    (package
      (name "sbcl-supertrace")
      (version (git-version "0.1.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/supertrace")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0n369n6b7y1m49biccnnr7svymjdsk8sksrkqrn3mj21vgv7s7bg"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-cffi sbcl-rove sbcl-cl-ppcre sbcl-bordeaux-threads))
      (inputs
       (list sbcl-cffi))
      (home-page "https://github.com/fukamachi/supertrace")
      (synopsis "Improved Common Lisp tracing for debugging and profiling")
      (description
       "Supertrace provides a superior Common Lisp @code{trace} functionality
for debugging and profiling real world applications.")
      (license license:bsd-2))))

(define-public cl-supertrace
  (sbcl-package->cl-source-package sbcl-supertrace))

(define-public ecl-supertrace
  (sbcl-package->ecl-package sbcl-supertrace))

(define-public sbcl-trivial-benchmark
  (let ((commit "42d76733dd2e873471c6f1e27d39113293f7dd5c"))
    (package
      (name "sbcl-trivial-benchmark")
      (version (git-version "2.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/trivial-benchmark/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0fbzqbpm2ixz85555krl36kbbbjyn699vdj6k383khi3g9y629fa"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria))
      (home-page "http://shinmera.github.io/trivial-benchmark/")
      (synopsis "Easy to use benchmarking system for Common Lisp")
      (description
       "Trivial-Benchmark runs a block of code many times and outputs some
statistical data for it.  On SBCL this includes the data from @code{time}, for
all other implementations just the @code{real-time} and @code{run-time} data.
However, you can extend the system by adding your own @code{metrics} to it, or
even by adding additional statistical @code{compute}ations.")
      (license license:zlib))))

(define-public cl-trivial-benchmark
  (sbcl-package->cl-source-package sbcl-trivial-benchmark))

(define-public ecl-trivial-benchmark
  (sbcl-package->ecl-package sbcl-trivial-benchmark))

(define-public sbcl-glyphs
  (let ((commit "1ff5714e8c1dca327bc604dfe3e3d1f4b7755373"))
    (package
      (name "sbcl-glyphs")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ahungry/glyphs/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17kai1anbkk5dj5sbrsin2fc019cmcbglb900db60v38myj0y0wf"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre sbcl-parenscript sbcl-named-readtables))
      (home-page "https://github.com/ahungry/glyphs/")
      (synopsis "Reduce Common Lisp verbosity")
      (description
       "This library is a little experiment in reducing verbosity in Common
Lisp, inspired by BODOL (@url{https://github.com/bodil/BODOL}).")
      (license license:gpl3))))

(define-public cl-glyphs
  (sbcl-package->cl-source-package sbcl-glyphs))

(define-public ecl-glyphs
  (sbcl-package->ecl-package sbcl-glyphs))

(define-public sbcl-zs3
  (package
   (name "sbcl-zs3")
   (version "1.3.3")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/xach/zs3")
       (commit (string-append "release-" version))))
     (file-name (git-file-name "zs3" version))
     (sha256
      (base32 "186v95wgsj2hkxdw2jl9x1w4fddjclp7arp0rrd9vf5ly8h8sbf3"))))
   (build-system asdf-build-system/sbcl)
   (inputs
    (list sbcl-drakma
          sbcl-alexandria
          sbcl-cxml
          sbcl-ironclad
          sbcl-puri
          sbcl-cl-base64))
   (synopsis "Work with Amazon S3 and Amazon CloudFront from Common Lisp")
   (description "This is ZS3, a library for working with Amazon's Simple Storage
Service (S3) and CloudFront service from Common Lisp.")
   (home-page "https://github.com/xach/zs3")
   (license license:bsd-2)))

(define-public cl-zs3
  (sbcl-package->cl-source-package sbcl-zs3))

(define-public ecl-zs3
  (sbcl-package->ecl-package sbcl-zs3))

(define-public sbcl-simple-neural-network
  (package
    (name "sbcl-simple-neural-network")
    (version "3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/glv2/simple-neural-network")
             (commit (string-append "v" version))))
       (file-name (git-file-name "simple-neural-network" version))
       (sha256
        (base32 "1jj1c90fr5clwka0jv32hv6xp1bkdlpa6x5jh19an13rhx8ll4zr"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("chipz" ,sbcl-chipz)
       ("fiveam" ,sbcl-fiveam)))
    (inputs
     `(("cl-store" ,sbcl-cl-store)
       ("lparallel" ,sbcl-lparallel)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'check 'remove-test-data
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each delete-file (find-files out "\\.gz$"))))))))
    (synopsis "Simple neural network in Common Lisp")
    (description
     "@code{simple-neural-network} is a Common Lisp library for creating,
training and using basic neural networks.  The networks created by this
library are feedforward neural networks trained using backpropagation.")
    (home-page "https://github.com/glv2/simple-neural-network")
    (license license:gpl3+)))

(define-public cl-simple-neural-network
  (sbcl-package->cl-source-package sbcl-simple-neural-network))

(define-public ecl-simple-neural-network
  (sbcl-package->ecl-package sbcl-simple-neural-network))

(define-public sbcl-zstd
  (let ((commit "d144582c581aaa52bac24d6686af27fa3e781e06")
        (revision "1"))
    (package
      (name "sbcl-zstd")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/cl-zstd")
               (commit commit)))
         (file-name (git-file-name "cl-zstd" version))
         (sha256
          (base32 "1774jy8hzbi6nih3sq6vchk66f7g8w86dwgpbvljyfzcnkcaz6ql"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("cl-octet-streams" ,sbcl-cl-octet-streams)
         ("zstd-lib" ,zstd "lib")))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/libzstd.lisp"
                 (("libzstd\\.so")
                  (search-input-file inputs "/lib/libzstd.so"))))))))
      (synopsis "Common Lisp library for Zstandard (de)compression")
      (description
       "This Common Lisp library provides functions for Zstandard
compression/decompression using bindings to the libzstd C library.")
      (home-page "https://github.com/glv2/cl-zstd")
      (license license:gpl3+))))

(define-public cl-zstd
  (sbcl-package->cl-source-package sbcl-zstd))

(define-public ecl-zstd
  (sbcl-package->ecl-package sbcl-zstd))

(define-public sbcl-agnostic-lizard
  (let ((commit "fe3a73719f05901c8819f8995a3ebae738257952")
        (revision "1"))
    (package
      (name "sbcl-agnostic-lizard")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/mraskin/agnostic-lizard")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ax78y8w4zlp5dcwyhz2nq7j3shi49qn31dkfg8lv2jlg7mkwh2d"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Almost correct portable code walker for Common Lisp")
      (description
       "Agnostic Lizard is a portable implementation of a code walker and in
particular of the macroexpand-all function (and macro) that makes a best
effort to be correct while not expecting much beyond what the Common Lisp
standard requires.

It aims to be implementation-agnostic and to climb the syntax trees.")
      (home-page "https://gitlab.common-lisp.net/mraskin/agnostic-lizard")
      (license license:gpl3+))))

(define-public cl-agnostic-lizard
  (sbcl-package->cl-source-package sbcl-agnostic-lizard))

(define-public ecl-agnostic-lizard
  (sbcl-package->ecl-package sbcl-agnostic-lizard))

(define-public sbcl-dynamic-classes
  (package
    (name "sbcl-dynamic-classes")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gwkkwg/dynamic-classes")
             (commit (string-append "version-" version))))
       (file-name (git-file-name "dynamic-classes" version))
       (sha256
        (base32 "1z3ag6w4ff0v6715xa9zhvwjqnp4i6zrjfmxdz8m115sklbwgm6c"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("metatilities-base" ,sbcl-metatilities-base)))
    (arguments
     ;; NOTE: (Sharlatan-20210106222900+0000) Circular dependencies and failing
     ;; test suites. lift-standard.config contains referances to deprecated
     ;; functionality.
     `(#:tests? #f))
    (home-page "https://common-lisp.net/project/dynamic-classes/")
    (synopsis "Dynamic class definition for Common Lisp")
    (description "Dynamic-Classes helps to ease the prototyping process by
bringing dynamism to class definition.")
    (license license:expat)))

(define-public ecl-dynamic-classes
  (sbcl-package->ecl-package sbcl-dynamic-classes))

(define-public cl-dynamic-classes
  (sbcl-package->cl-source-package sbcl-dynamic-classes))

(define-public sbcl-cl-markdown
  ;; NOTE: (Sharlatan-20210106214629+0000) latest version tag
  ;; "version-0.10.6_version-0.10.6" is failing to build due to missing system
  ;; #:container-dynamic-classes
  (package
    (name "sbcl-cl-markdown")
    (version "0.10.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gwkkwg/cl-markdown")
             (commit (string-append "version-" version))))
       (file-name (git-file-name "cl-markdown" version))
       (sha256
        (base32 "1wdjbdd1zyskxf7zlilcp6fmwkivybj0wjp64vvzb265d5xi7p8p"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("anaphora" ,sbcl-anaphora)
       ("cl-containers" ,sbcl-cl-containers)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("dynamic-classes" ,sbcl-dynamic-classes)
       ("metabang-bind" ,sbcl-metabang-bind)
       ("metatilities-base" ,sbcl-metatilities-base)))
    (arguments
     ;; NOTE: (Sharlatan-20210107213629+0000) Tests depend on too many not
     ;; available systems, which  themself are abandoned.
     `(#:tests? #f))
    (home-page "https://common-lisp.net/project/cl-markdown/")
    (synopsis "Common Lisp rewrite of Markdown")
    (description
     "This is an implementation of a Markdown parser in Common Lisp.")
    (license license:expat)))

(define-public ecl-cl-markdown
  (sbcl-package->ecl-package sbcl-cl-markdown))

(define-public cl-markdown
  (sbcl-package->cl-source-package sbcl-cl-markdown))

(define-public sbcl-magicffi
  (let ((commit "d88f2f280c31f639e4e05be75215d8a8dce6aef2"))
    (package
      (name "sbcl-magicffi")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dochang/magicffi/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0p6ysa92fk34bhxpw7bycbfgw150fv11z9x8jr9xb4lh8cm2hvp6"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-alexandria))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("ppcre" ,sbcl-cl-ppcre)
         ("libmagic" ,file)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((magic (assoc-ref inputs "libmagic")))
                 (substitute* "grovel.lisp"
                   (("/usr/include/magic.h")
                    (string-append magic "/include/magic.h")))
                 (substitute* "api.lisp"
                   ((":default \"libmagic\"" all)
                    (string-append ":default \"" magic "/lib/libmagic\"")))))))))
      (home-page "https://common-lisp.net/project/magicffi/")
      (synopsis "Common Lisp interface to libmagic based on CFFI")
      (description
       "MAGICFFI is a Common Lisp CFFI interface to libmagic(3), the file type
determination library using @emph{magic} numbers.")
      (license license:bsd-2))))

(define-public ecl-magicffi
  (sbcl-package->ecl-package sbcl-magicffi))

(define-public cl-magicffi
  (sbcl-package->cl-source-package sbcl-magicffi))

(define-public sbcl-shlex
  (let ((commit "3dee1cb7c0140fa7660ca7a3b2ac5e75d1218e5c")
        (revision "2"))
    (package
      (name "sbcl-shlex")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ruricolist/cl-shlex")
               (commit commit)))
         (file-name (git-file-name "cl-shlex" version))
         (sha256
          (base32 "16ag48sswgimr1fzr582vhym4s03idpd4lkydw5s58lv80ibpim8"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-serapeum sbcl-cl-ppcre sbcl-cl-unicode))
      (home-page "https://github.com/ruricolist/cl-shlex")
      (synopsis "Common Lisp lexical analyzer for shell-like syntaxes")
      (description
       "This library contains a lexer for syntaxes that use shell-like rules
for quoting and commenting.  It is a port of the @code{shlex} module from Python’s
standard library.")
      (license license:expat))))

(define-public ecl-shlex
  (sbcl-package->ecl-package sbcl-shlex))

(define-public cl-shlex
  (sbcl-package->cl-source-package sbcl-shlex))

(define-public sbcl-cmd
  (let ((commit "b0b79adf1214dbec082f3dd2274a72a0ff58efd7"))
    (package
      (name "sbcl-cmd")
      (version (git-version "0.0.1" "5" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ruricolist/cmd/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0kk29vydmi1fyhpbwy3mrsg3bhvx0478r6r7jcsfkr3ci2h8w8a1"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("coreutils" ,coreutils)
         ("procps" ,procps)
         ("serapeum" ,sbcl-serapeum)
         ("shlex" ,sbcl-shlex)
         ("trivia" ,sbcl-trivia)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref inputs "coreutils") "/bin"))
                     (ps-bin (string-append (assoc-ref inputs "procps") "/bin")))
                 (substitute* "cmd.lisp"
                   (("\\(def \\+env\\+ \"env\"\\)")
                    (format #f "(def +env+ \"~a/env\")" bin))
                   (("\\(def \\+kill\\+ \"kill\"\\)")
                    (format #f "(def +kill+ \"~a/kill\")" bin))
                   (("\\(def \\+ps\\+ \"ps\"\\)")
                    (format #f "(def +ps+ \"~a/ps\")" ps-bin))
                   (("\\(def \\+pwd\\+ \"pwd\"\\)")
                    (format #f "(def +pwd+ \"~a/pwd\")" bin))
                   (("\\(def \\+sh\\+ \"/bin/sh\"\\)")
                    (format #f "(def +sh+ \"~a\")" (which "sh")))
                   (("\\(def \\+tr\\+ \"tr\"\\)")
                    (format #f "(def +tr+ \"~a/tr\")" bin)))))))))
      (home-page "https://github.com/ruricolist/cmd")
      (synopsis "Conveniently run external programs from Common Lisp")
      (description
       "A utility for running external programs, built on UIOP.
Cmd is designed to be natural to use, protect against shell interpolation and
be usable from multi-threaded programs.")
      (license license:expat))))

(define-public ecl-cmd
  (sbcl-package->ecl-package sbcl-cmd))

(define-public cl-cmd
  (sbcl-package->cl-source-package sbcl-cmd))

(define-public sbcl-ppath
  (let ((commit "eb1a8173b4d1d691ea9a7699412123462f58c3ce"))
    (package
      (name "sbcl-ppath")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fourier/ppath/")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "1c46q9lmzqv14z80d3fwdawgn3pn4922x31fyqvsvbcjm4hd16fb"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria
             sbcl-cffi
             sbcl-osicat
             sbcl-cl-ppcre
             sbcl-split-sequence
             sbcl-trivial-features))
      (native-inputs
       (list sbcl-cl-fad sbcl-prove))
      (home-page "https://github.com/fourier/ppath")
      (synopsis "Common Lisp's implementation of the Python's os.path module")
      (description
       "This library is a path strings manipulation library inspired by
Python's @code{os.path}.  All functionality from @code{os.path} is supported on
major operation systems.

The philosophy behind is to use simple strings and \"dumb\" string
manipulation functions to handle paths and filenames.  Where possible the
corresponding OS system functions are called.")
      (license license:bsd-2))))

(define-public ecl-ppath
  (sbcl-package->ecl-package sbcl-ppath))

(define-public cl-ppath
  (sbcl-package->cl-source-package sbcl-ppath))

(define-public sbcl-trivial-escapes
  (let ((commit "1eca78da2078495d09893be58c28b3aa7b8cc4d1"))
    (package
      (name "sbcl-trivial-escapes")
      (version (git-version "1.2.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/williamyaoh/trivial-escapes")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "0v6h8lk17iqv1qkxgqjyzn8gi6v0hvq2vmfbb01md3zjvjqxn6lr"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-named-readtables))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/williamyaoh/trivial-escapes")
      (synopsis "C-style escape directives for Common Lisp")
      (description
       "This Common Lisp library interprets escape characters the same way that
most other programming language do.
It provides four readtables.  The default one lets you write strings like this:
@code{#\"This string has\na newline in it!\"}.")
      (license license:public-domain))))

(define-public ecl-trivial-escapes
  (sbcl-package->ecl-package sbcl-trivial-escapes))

(define-public cl-trivial-escapes
  (sbcl-package->cl-source-package sbcl-trivial-escapes))

(define-public sbcl-cl-indentify
  (let ((commit "eb770f434defa4cd41d84bca822428dfd0dbac53"))
    (package
      (name "sbcl-cl-indentify")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yitzchak/cl-indentify")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "0ha36bhg474vr76vfhr13szc8cfdj1ickg92k1icz791bqaqg67p"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-command-line-arguments
             sbcl-trivial-gray-streams))
      (native-inputs
       (list sbcl-trivial-escapes sbcl-rove))
      (home-page "https://github.com/yitzchak/cl-indentify")
      (synopsis "Code beautifier for Common Lisp")
      (description
       "A library and command line utility to automatically indent Common Lisp
source files.")
      (license license:expat))))

(define-public ecl-cl-indentify
  (sbcl-package->ecl-package sbcl-cl-indentify))

(define-public cl-indentify
  (sbcl-package->cl-source-package sbcl-cl-indentify))

(define-public sbcl-concrete-syntax-tree
  (let ((commit "abd242a59dadc5452aa9dbc1d313c83ec2c11f46"))
    (package
      (name "sbcl-concrete-syntax-tree")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/s-expressionists/Concrete-Syntax-Tree")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "1lyrglc3h1if44gxd9cwv90wa90nrdjvb7fry39b1xn8ywdfa7di"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-acclimation))
      (home-page "https://github.com/s-expressionists/Concrete-Syntax-Tree")
      (synopsis "Parse Common Lisp code into a concrete syntax tree")
      (description
       "This library is intended to solve the problem of source tracking for
Common Lisp code.

By \"source tracking\", it is meant that code elements that have a known
origin in the form of a position in a file or in an editor buffer are
associated with some kind of information about this origin.

Since the exact nature of such origin information depends on the Common Lisp
implementation and the purpose of wanting to track that origin, the library
does not impose a particular structure of this information.  Instead, it
provides utilities for manipulating source code in the form of what is called
concrete syntax trees (CSTs for short) that preserve this information about
the origin.")
      (license license:bsd-2))))

(define-public ecl-concrete-syntax-tree
  (sbcl-package->ecl-package sbcl-concrete-syntax-tree))

(define-public cl-concrete-syntax-tree
  (sbcl-package->cl-source-package sbcl-concrete-syntax-tree))

(define-public sbcl-eclector
  (package
    (name "sbcl-eclector")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/s-expressionists/Eclector")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bwkla0jdp5bg0q1zca5wg22b0nbdmglgax345nrhsf8bdrh47wm"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("acclimation" ,sbcl-acclimation)
       ("alexandria" ,sbcl-alexandria)
       ("closer-mop" ,sbcl-closer-mop)
       ("concrete-syntax-tree" ,sbcl-concrete-syntax-tree)))
    (native-inputs
     (list sbcl-fiveam))
    (arguments
     '(#:asd-systems '("eclector"
                       "eclector-concrete-syntax-tree")))
    (home-page "https://s-expressionists.github.io/Eclector/")
    (synopsis "Highly customizable, portable Common Lisp reader")
    (description
     "Eclector is a portable Common Lisp reader that is highly customizable,
can recover from errors and can return concrete syntax trees.

In contrast to many other reader implementations, eclector can recover from
most errors in the input supplied to it and continue reading.  This capability
is realized as a restart.

It can also produce instances of the concrete syntax tree classes provided by
the concrete syntax tree library.")
    (license license:bsd-2)))

(define-public ecl-eclector
  (sbcl-package->ecl-package sbcl-eclector))

(define-public cl-eclector
  (sbcl-package->cl-source-package sbcl-eclector))

(define-public sbcl-jsown
  (let ((commit "744c4407bef58dfa876d9da0b5c0205d869e7977"))
    (package
      (name "sbcl-jsown")
      (version (git-version "1.0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/madnificent/jsown")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "0gadvmf1d9bq35s61z76psrsnzwwk12svi66jigf491hv48wigw7"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/madnificent/jsown")
      (synopsis "Fast JSON reader / writer library for Common Lisp")
      (description
       "@code{jsown} is a high performance Common Lisp JSON parser.  Its aim
is to allow for the fast parsing of JSON objects in Common Lisp.  Recently,
functions and macros have been added to ease the burden of writing and editing
@code{jsown} objects.

@code{jsown} allows you to parse JSON objects quickly to a modifiable Lisp
list and write them back.  If you only need partial retrieval of objects,
@code{jsown} allows you to select the keys which you would like to see parsed.
@code{jsown} also has a JSON writer and some helper methods to alter the JSON
objects themselves.")
      (license license:expat))))

(define-public ecl-jsown
  (sbcl-package->ecl-package sbcl-jsown))

(define-public cl-jsown
  (sbcl-package->cl-source-package sbcl-jsown))

(define-public sbcl-system-locale
  (let ((commit "4b334bc2fa45651bcaa28ae7d9331095d6bf0a17"))
    (package
      (name "sbcl-system-locale")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/system-locale/")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "00p5c053kmgq4ks6l9mxsqz6g3bjcybvkvj0bh3r90qgpkaawm1p"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-documentation-utils))
      (home-page "https://shinmera.github.io/system-locale/")
      (synopsis "Get the system's locale and language settings in Common Lisp")
      (description
       "This library retrieves locale information configured on the
system.  This is helpful if you want to write applications and libraries that
display messages in the user's native language.")
      (license license:zlib))))

(define-public ecl-system-locale
  (sbcl-package->ecl-package sbcl-system-locale))

(define-public cl-system-locale
  (sbcl-package->cl-source-package sbcl-system-locale))

(define-public sbcl-language-codes
  (let ((commit "e7aa0e37cb97a3d37d6bc7316b479d01bff8f42e"))
    (package
      (name "sbcl-language-codes")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/language-codes")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "0py176ibmsc01n5r0q1bs1ykqf5jwdbh8kx0j1a814l9y51241v0"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-documentation-utils))
      (home-page "https://shinmera.github.io/language-codes/")
      (synopsis "Map ISO language codes to language names in Common Lisp")
      (description
       "This is a small library providing the ISO-639 language code to
language name mapping.")
      (license license:zlib))))

(define-public ecl-language-codes
  (sbcl-package->ecl-package sbcl-language-codes))

(define-public cl-language-codes
  (sbcl-package->cl-source-package sbcl-language-codes))

(define-public sbcl-multilang-documentation
  (let ((commit "59e798a07e949e8957a20927f52aca425d84e4a0"))
    (package
      (name "sbcl-multilang-documentation")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/multilang-documentation")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "13y5jskx8n2b7kimpfarr8v777w3b7zj5swg1b99nj3hk0843ixw"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-documentation-utils sbcl-language-codes
             sbcl-system-locale))
      (home-page "https://shinmera.github.io/multilang-documentation/")
      (synopsis "Add multiple languages support to Common Lisp documentation")
      (description
       "This library provides a drop-in replacement function for
cl:documentation that supports multiple docstrings per-language, allowing you
to write documentation that can be internationalised.")
      (license license:zlib))))

(define-public ecl-multilang-documentation
  (sbcl-package->ecl-package sbcl-multilang-documentation))

(define-public cl-multilang-documentation
  (sbcl-package->cl-source-package sbcl-multilang-documentation))

(define-public sbcl-trivial-do
  (let ((commit "03a1729f1e71bad3ebcf6cf098a0cce52dfa1163"))
    (package
      (name "sbcl-trivial-do")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yitzchak/trivial-do")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "1ffva79nkicc7wc8c2ic5nayis3b2rk5sxzj74yjkymkjgbpcrgd"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/yitzchak/trivial-do")
      (synopsis "Additional dolist style macros for Common Lisp")
      (description
       "Additional dolist style macros for Common Lisp, such as
@code{doalist}, @code{dohash}, @code{dolist*}, @code{doplist}, @code{doseq}
and @code{doseq*}.")
      (license license:zlib))))

(define-public ecl-trivial-do
  (sbcl-package->ecl-package sbcl-trivial-do))

(define-public cl-trivial-do
  (sbcl-package->cl-source-package sbcl-trivial-do))

(define-public sbcl-common-lisp-jupyter
  (let ((commit "ba9f0e746b9200d6fd6db647d7274448119ed01b")
        (revision "3"))
    (package
      (name "sbcl-common-lisp-jupyter")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yitzchak/common-lisp-jupyter")
               (commit commit)))
         (file-name (git-file-name "common-lisp-jupyter" commit))
         (sha256
          (base32 "0si69xfzi769dprwfy7gp1x3bl7lxz6d4n98sa26w9r41wvay5ja"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria
             sbcl-babel
             sbcl-bordeaux-threads
             sbcl-cl-base64
             sbcl-cl-indentify
             sbcl-closer-mop
             sbcl-dissect
             sbcl-eclector
             sbcl-ironclad
             sbcl-iterate
             sbcl-multilang-documentation
             sbcl-puri
             sbcl-pzmq
             sbcl-shasht
             sbcl-static-vectors
             sbcl-trivial-do
             sbcl-trivial-garbage
             sbcl-trivial-gray-streams
             sbcl-trivial-mimes))
      (home-page "https://yitzchak.github.io/common-lisp-jupyter/")
      (synopsis "Common Lisp kernel for Jupyter")
      (description
       "This is a Common Lisp kernel for Jupyter along with a library for
building Jupyter kernels, based on Maxima-Jupyter which was based on
@code{cl-jupyter}.")
      (license license:expat))))

(define-public ecl-common-lisp-jupyter
  (sbcl-package->ecl-package sbcl-common-lisp-jupyter))

(define-public cl-common-lisp-jupyter
  (sbcl-package->cl-source-package sbcl-common-lisp-jupyter))

(define-public sbcl-radiance
  (let ((commit "5ffbe1f157edd17a13194495099efd81e052df85")
        (revision "1"))
    (package
      (name "sbcl-radiance")
      (version (git-version "2.1.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shirakumo/radiance")
               (commit commit)))
         (file-name (git-file-name "radiance" version))
         (sha256
          (base32 "0hbkcnmnlj1cqzbv18zmla2iwbl65kxilz9764hndf8x8as1539c"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:tests? #f  ; TODO: The tests require some configuration.
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'disable-quicklisp
             (lambda _
               ;; Disable the automatic installation of systems by Quicklisp.
               ;; (Maybe there would be a way to package Quicklisp and make it
               ;; install things in the user's directory instead of
               ;; /gnu/store/...).
               (substitute* "interfaces.lisp"
                 (("\\(unless \\(asdf:find-system configured-implementation NIL\\)"
                   all)
                  (string-append "#+quicklisp " all))))))))
      (native-inputs
       `(("alexandria" ,sbcl-alexandria)
         ("dexador" ,sbcl-dexador)
         ("parachute" ,sbcl-parachute)
         ("verbose" ,sbcl-verbose)))
      (inputs
       `(("babel" ,sbcl-babel)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("closer-mop" ,sbcl-closer-mop)
         ("documentation-utils" ,sbcl-documentation-utils)
         ("deploy" ,sbcl-deploy)
         ("form-fiddle" ,sbcl-form-fiddle)
         ("lambda-fiddle" ,sbcl-lambda-fiddle)
         ("local-time" ,sbcl-local-time)
         ("modularize-hooks" ,sbcl-modularize-hooks)
         ("modularize-interfaces" ,sbcl-modularize-interfaces)
         ("puri" ,sbcl-puri)
         ("trivial-indent" ,sbcl-trivial-indent)
         ("trivial-mimes" ,sbcl-trivial-mimes)
         ("ubiquitous-concurrent" ,sbcl-ubiquitous)))
      (home-page "https://shirakumo.github.io/radiance/")
      (synopsis "Common Lisp web application environment")
      (description
       "Radiance is a web application environment, which is sort of like a web
framework, but more general, more flexible.  It should let you write personal
websites and generally deployable applications easily and in such a way that
they can be used on practically any setup without having to undergo special
adaptations.")
      (license license:zlib))))

(define-public ecl-radiance
  (sbcl-package->ecl-package sbcl-radiance))

(define-public cl-radiance
  (sbcl-package->cl-source-package sbcl-radiance))

(define-public sbcl-daemon
  (let ((commit "d5652f4332c3cee21e9bf83b9237129605004597")
        (revision "1"))
    (package
      (name "sbcl-daemon")
      (version (git-version "0.0.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/snmsts/daemon")
               (commit commit)))
         (file-name (git-file-name "daemon" version))
         (sha256
          (base32 "1kdxfnhh9fz34j8qs7pn7mwjz3v33q4v9nh0hqkyzraq5xs2j3f4"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-trivial-features))
      (home-page "https://github.com/snmsts/daemon")
      (synopsis "Daemonize Common Lisp processes")
      (description
       "DAEMON provides the functionality of daemonizing Common Lisp processes
on UNIX like platforms.")
      (license license:expat))))

(define-public ecl-daemon
  (sbcl-package->ecl-package sbcl-daemon))

(define-public cl-daemon
  (sbcl-package->cl-source-package sbcl-daemon))

(define-public sbcl-file-attributes
  (let ((commit "bbde396438f37d676de9775239115410bec4da2d"))
    (package
      (name "sbcl-file-attributes")
      (version (git-version "1.0.0" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/file-attributes/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0n8q818ry2shggjfhp8gjby8v5mla9pg97c5g19pcglpnwim7a74"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cffi sbcl-documentation-utils sbcl-trivial-features))
      (home-page "https://shinmera.github.io/file-attributes/")
      (synopsis "Access to common file attributes in Common Lisp")
      (description
       "This is a small OS portability library to retrieve and set file
attributes not supported by the Common Lisp standard functions.")
      (license license:zlib))))

(define-public ecl-file-attributes
  (sbcl-package->ecl-package sbcl-file-attributes))

(define-public cl-file-attributes
  (sbcl-package->cl-source-package sbcl-file-attributes))

(define-public sbcl-cl-difflib
  (let ((commit "98eb335c693f1881584b83ca7be4a0fe05355c4e")
        (revision "0"))
    (package
      (name "sbcl-cl-difflib")
      (version (git-version "0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/wiseman/cl-difflib")
               (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "08if0abhqg191xcz9s7xv8faqq51nswzp8hw423fkqjzr24pmq48"))))
      (build-system asdf-build-system/sbcl)
      ;; Due to the age of this library tests use some deprecated
      ;; functionality and keep failing.
      (arguments
       '(#:tests? #f
         #:asd-files '("cl-difflib.asd")))
      (home-page "https://github.com/wiseman/cl-difflib")
      (synopsis "Compute differences between pairs of sequences")
      (description
       "A Common Lisp library for computing differences between
sequences based on the Python difflib module.")
      (license license:expat))))

(define-public ecl-cl-difflib
  (sbcl-package->ecl-package sbcl-cl-difflib))

(define-public cl-difflib
  (sbcl-package->cl-source-package sbcl-cl-difflib))

(define-public sbcl-cl-html-diff
  (let ((commit "5a0b39d1c524278d6f60851d7786bb2585614310")
        (revision "0"))
    (package
      (name "sbcl-cl-html-diff")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/wiseman/cl-html-diff")
               (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "1varnijivzd4jpimn1cz8p5ks713zzha5cgl4vmb0xr8ahravwzb"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-difflib))
      (home-page "https://github.com/wiseman/cl-html-diff")
      (synopsis "Generate a human-readable diff of two HTML documents")
      (description
       "A Common Lisp library for generating a human-readable diff of two
HTML documents.")
      (license license:expat))))

(define-public ecl-cl-html-diff
  (sbcl-package->ecl-package sbcl-cl-html-diff))

(define-public cl-html-diff
  (sbcl-package->cl-source-package sbcl-cl-html-diff))

(define-public sbcl-tooter
  (let ((commit "ec97bee3431c55913078e532daae81eb0fd90372")
        (revision "3"))
    (package
      (name "sbcl-tooter")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/tooter")
               (commit commit)))
         (file-name (git-file-name "tooter" version))
         (sha256
          (base32 "02vpjaq38d6laaqmsana9f13c38xzr0xwy05fcfkmzdhh0kllpkv"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre sbcl-documentation-utils sbcl-drakma
             sbcl-yason))
      (synopsis "Common Lisp client library for Mastodon instances")
      (description
       "This is a Common Lisp library implementing the full v1 REST API
protocol for Mastodon.")
      (home-page "https://shinmera.github.io/tooter/")
      (license license:zlib))))

(define-public ecl-tooter
  (sbcl-package->ecl-package sbcl-tooter))

(define-public cl-tooter
  (sbcl-package->cl-source-package sbcl-tooter))

(define-public sbcl-croatoan
  (let ((commit "cf875137a23ed4efbfde63e52691f1b544d55d17")
        (revision "4"))
    (package
      (name "sbcl-croatoan")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/McParen/croatoan")
               (commit commit)))
         (file-name (git-file-name "croatoan" version))
         (sha256
          (base32 "0dvp8irimlnnvz2m4lnmxp19xbam1yfri3d1i9qqml968i08xcwb"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "ncurses/ncurses.lisp"
                 (("libncursesw" all)
                  (string-append (assoc-ref inputs "ncurses")
                                 "/lib/"
                                 all))))))))
      (inputs
       `(("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cffi" ,sbcl-cffi)
         ("ncurses" ,ncurses)
         ("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
      (synopsis "Common Lisp bindings for the ncurses terminal library")
      (description "Croatoan provides high-level Common Lisp CLOS bindings for
the ncurses terminal library.")
      (home-page "https://github.com/McParen/croatoan")
      (license license:expat))))

(define-public ecl-croatoan
  (sbcl-package->ecl-package sbcl-croatoan))

(define-public cl-croatoan
  (sbcl-package->cl-source-package sbcl-croatoan))

(define-public sbcl-cl-spark
  (let ((commit "4e34bcebdcf8e45265986eb43ad4ad03bb41a581")
        (revision "1"))
    (package
      (name "sbcl-cl-spark")
      (version (git-version "0.1.13" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tkych/cl-spark")
               (commit commit)))
         (file-name (git-file-name "cl-spark" version))
         (sha256
          (base32 "0my1fsgi2rjaqkpk934f2bjy63pmnj7faza3fzvnk6k3l66y19nk"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (synopsis "Common Lisp library to make histograms")
      (description "This is a Common Lisp library to make histograms using
UTF-8 block characters.")
      (home-page "https://github.com/tkych/cl-spark")
      (license license:expat))))

(define-public ecl-cl-spark
  (sbcl-package->ecl-package sbcl-cl-spark))

(define-public cl-spark
  (sbcl-package->cl-source-package sbcl-cl-spark))

(define-public sbcl-access
  (let ((commit "1b26db3760018cdc4624f880f0a1e0155d8f6c50")
        (revision "1"))
    (package
      (name "sbcl-access")
      (version (git-version "1.5.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/access")
               (commit commit)))
         (file-name (git-file-name "access" version))
         (sha256
          (base32 "1knd3n4mpzkc97i1znbas32pscd30416isvmx2pjmgvar6k93pl5"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-lisp-unit2))
      (inputs
       (list sbcl-alexandria sbcl-anaphora sbcl-closer-mop
             sbcl-cl-interpol sbcl-iterate))
      (synopsis
       "Common lisp library to unify access to dictionary-like structures")
      (description
       "This is a Common lisp library to unify access to the most common
dictionary-like data structures.")
      (home-page "https://github.com/sharplispers/access")
      (license license:bsd-3))))

(define-public ecl-access
  (sbcl-package->ecl-package sbcl-access))

(define-public cl-access
  (sbcl-package->cl-source-package sbcl-access))

(define-public sbcl-sxql-composer
  (let ((commit "2b2230cb01ae1b68e28921d99e4814046867fb75")
        (revision "1"))
    (package
      (name "sbcl-sxql-composer")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mmontone/sxql-composer")
               (commit commit)))
         (file-name (git-file-name "sxql-composer" version))
         (sha256
          (base32 "1agkrj3ymskzc3c7pxbrj123d1kygjqcls145m0ap3i07q96hh1r"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-sxql))
      (synopsis "Build and compose SXQL queries dynamically")
      (description
       "This is a Common Lisp library to build and compose SXQL queries
dynamically.")
      (home-page "https://github.com/mmontone/sxql-composer")
      (license license:expat))))

(define-public ecl-sxql-composer
  (sbcl-package->ecl-package sbcl-sxql-composer))

(define-public cl-sxql-composer
  (sbcl-package->cl-source-package sbcl-sxql-composer))

(define-public sbcl-cl-i18n
  (let ((commit "4216fe9fc7b54033c0d881dbb835b7af786365ed")
        (revision "1"))
    (package
      (name "sbcl-cl-i18n")
      (version (git-version "0.5.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://notabug.org/cage/cl-i18n")
               (commit commit)))
         (file-name (git-file-name "cl-i18n" version))
         (sha256
          (base32 "1mdhfkk61djj39vgnns5y1cssd12h0m1cfwd21m8xpa2l3rqvmgf"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-babel sbcl-cl-ppcre-unicode))
      (synopsis "Internationalisation framework for Common Lisp")
      (description
       "This is a Gettext-style internationalisation framework for Common
Lisp.")
      (home-page "https://notabug.org/cage/cl-i18n")
      (license license:llgpl))))

(define-public ecl-cl-i18n
  (sbcl-package->ecl-package sbcl-cl-i18n))

(define-public cl-i18n
  (sbcl-package->cl-source-package sbcl-cl-i18n))

(define-public sbcl-crypto-shortcuts
  (let ((commit "7efd22d80e867cd8c9f8f363d4fe7b51ee2dadc0")
        (revision "1"))
    (package
      (name "sbcl-crypto-shortcuts")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/crypto-shortcuts")
               (commit commit)))
         (file-name (git-file-name "crypto-shortcuts" version))
         (sha256
          (base32 "0c0m0ar04jn7qf2v8c4sivamlzki03r13rnxy8b3n27rh9r6hgin"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-base64 sbcl-flexi-streams sbcl-ironclad))
      (synopsis "Collection of common cryptography functions")
      (description
       "This is a collection of common cryptography functions for Common
Lisp.")
      (home-page "https://shinmera.github.io/crypto-shortcuts/")
      (license license:zlib))))

(define-public ecl-crypto-shortcuts
  (sbcl-package->ecl-package sbcl-crypto-shortcuts))

(define-public cl-crypto-shortcuts
  (sbcl-package->cl-source-package sbcl-crypto-shortcuts))

(define-public sbcl-cl-html5-parser
  (let ((commit "74a92eb3a183a0afd089ea33350e816e6b9aeefa")
        (revision "1"))
    (package
      (name "sbcl-cl-html5-parser")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rotatef/cl-html5-parser")
               (commit commit)))
         (file-name (git-file-name "cl-html5-parser" version))
         (sha256
          (base32 "04if61wigylsmn996rbfl8ylsd0d9hzdmg7p2wiglncibjzcl5k9"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-json-streams sbcl-split-sequence sbcl-stefil))
      (inputs
       (list sbcl-cl-ppcre sbcl-flexi-streams sbcl-string-case))
      (synopsis "HTML5 parser for Common Lisp")
      (description "This a Common Lisp library to parse HTML5 documents.")
      (home-page "https://github.com/rotatef/cl-html5-parser")
      (license license:lgpl3+))))

(define-public ecl-cl-html5-parser
  (sbcl-package->ecl-package sbcl-cl-html5-parser))

(define-public cl-html5-parser
  (sbcl-package->cl-source-package sbcl-cl-html5-parser))

(define-public sbcl-percent-encoding
  (let ((commit "c1224e22bc8048fbd3ebbc9329715a0c1b673170")
        (revision "1"))
    (package
      (name "sbcl-percent-encoding")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/llibra/percent-encoding")
               (commit commit)))
         (file-name (git-file-name "percent-encoding" version))
         (sha256
          (base32 "0q1lh3sa6mkjr5gcdkgimkpc29rgf9cjhv90f61h8ridj28grq0h"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-anaphora sbcl-babel))
      (synopsis "RFC 3986 percent-encoding library")
      (description
       "This is a Common Lisp library providing RFC 3986 percent-encoding.")
      (home-page "https://github.com/llibra/percent-encoding")
      (license license:expat))))

(define-public ecl-percent-encoding
  (sbcl-package->ecl-package sbcl-percent-encoding))

(define-public cl-percent-encoding
  (sbcl-package->cl-source-package sbcl-percent-encoding))

(define-public sbcl-cl-mount-info
  (let ((commit "2024f5037a7f63db3e3587dc9972cd7b9318f06b")
        (revision "1"))
    (package
     (name "sbcl-cl-mount-info")
     (version (git-version "0.0.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://notabug.org/cage/cl-mount-info.git")
             (commit commit)))
       (file-name (git-file-name "cl-mount-info" version))
       (sha256
        (base32 "0vza9gj9q42nzb5v8aj22lmn4aqx9vrddsb5a343nbwfz89hbh9x"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      (list sbcl-alexandria sbcl-cffi sbcl-cl-ppcre))
     (home-page "https://notabug.org/cage/cl-mount-info.git")
     (synopsis "Library to get information about mounted filesystems")
     (description
      "CL-MOUNT-INFO is a Common Lisp wrapper around @code{getmntent(3)} and
related C functions to get information about the mounted file system.")
     (license license:lgpl3))))

(define-public ecl-cl-mount-info
  (sbcl-package->ecl-package sbcl-cl-mount-info))

(define-public cl-mount-info
  (sbcl-package->cl-source-package sbcl-cl-mount-info))

(define-public sbcl-cl-diskspace
  (let ((commit "2dce2d0387d58221c452bd76c7b9b7a7de81ef55")
        (revision "1"))
    (package
      (name "sbcl-cl-diskspace")
      (version (git-version "0.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/muyinliu/cl-diskspace")
               (commit commit)))
         (file-name (git-file-name "cl-diskspace" version))
         (sha256
          (base32 "0l19hxqw6b8i5i1jdbr45k1xib9axcwdagsp3y8wkb35g6wwc0s7"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/unix/cl-diskspace-list-all-disks-with-df.lisp"
                 (("grep")
                  (search-input-file inputs "/bin/grep")))
               (substitute* "src/unix/cl-diskspace-list-all-disks-with-df.lisp"
                 (("/bin/df")
                  (which "df")))
               #t)))))
      (inputs
       `(("cl-ppcre" ,sbcl-cl-ppcre)
         ("cffi" ,sbcl-cffi)
         ("grep" ,grep)))
      (home-page "https://github.com/muyinliu/cl-diskspace")
      (synopsis "Disk space information library for Common Lisp")
      (description
       "CL-DISKSPACE is a Common Lisp library to list disks with the command
line tool @code{df} and get disk space information using @code{statvfs}.")
      (license license:isc))))

(define-public ecl-cl-diskspace
  (sbcl-package->ecl-package sbcl-cl-diskspace))

(define-public cl-diskspace
  (sbcl-package->cl-source-package sbcl-cl-diskspace))

(define-public sbcl-cl-cpus
  (package
    (name "sbcl-cl-cpus")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/muyinliu/cl-cpus")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cl-cpus" version))
       (sha256
        (base32 "0sdaff9hpsx7bpkkkqavmxmrrlc2d61gmqjjgn8xchncng4a0rf8"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-cffi))
    (home-page "https://github.com/muyinliu/cl-cpus")
    (synopsis "Common Lisp feature to get number of CPUs")
    (description
     "This package provides a Common Lisp system which has only one function to
return the CPU count of the current system.")
    (license license:isc)))

(define-public ecl-cl-cpus
  (sbcl-package->ecl-package sbcl-cl-cpus))

(define-public cl-cpus
  (sbcl-package->cl-source-package sbcl-cl-cpus))

(define-public sbcl-fof
  (package
    (name "sbcl-fof")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/ambrevar/fof")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j64b7p40h8bq33hqkpgakm3vs1607vyx6n48d7qg3287v1akk6m"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "ffprobe.lisp"
               (("\\(defvar \\*ffprobe-command\\* \"ffprobe\"\\)")
                (format #f "(defvar *ffprobe-command* \"~a/bin/ffprobe\")"
                        (assoc-ref inputs "ffmpeg") )))
             #t)))))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("hu.dwim.defclass-star" ,sbcl-hu.dwim.defclass-star)
       ("local-time" ,sbcl-local-time)
       ("magicffi" ,sbcl-magicffi)
       ("osicat" ,sbcl-osicat)
       ("serapeum" ,sbcl-serapeum)
       ("str" ,sbcl-cl-str)
       ("trivia" ,sbcl-trivia)
       ("trivial-package-local-nicknames" ,sbcl-trivial-package-local-nicknames)
       ;; Non-CL deps:
       ("ffmpeg" ,ffmpeg)))
    (home-page "https://gitlab.com/ambrevar/fof")
    (synopsis "File object finder library for Common Lisp")
    (description
     "This library enable rapid file search, inspection and manipulation
straight from the REPL.
It aims at replacing Unix tools such as @code{find} or @code{du}.
It also offers a replacement to the @code{pathname} Common Lisp API.
Slot writers which commit changes to disk, e.g. permissions, modification
time, etc.")
    (license license:gpl3+)))

(define-public ecl-fof
  (sbcl-package->ecl-package sbcl-fof))

(define-public cl-fof
  (sbcl-package->cl-source-package sbcl-fof))

(define-public sbcl-computable-reals
  (let ((commit "fdc73d75e79d0a4ce6d01c822c950ae2eb137d39"))
    (package
      (name "sbcl-computable-reals")
      (version (git-version "1.1.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stylewarning/computable-reals")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0f12axi53x14l12dgf4a1lfq3p1fx7fh7sjfc0db3lk88ph9qfwl"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/stylewarning/computable-reals")
      (synopsis "Arbitrary-precision, re-computing real-numbers in Common Lisp")
      (description
       "This library provides arbitrary precision (floating point) real
numbers in Common Lisp.")
      (license license:bsd-3))))

(define-public ecl-computable-reals
  (sbcl-package->ecl-package sbcl-computable-reals))

(define-public cl-computable-reals
  (sbcl-package->cl-source-package sbcl-computable-reals))

(define-public sbcl-html-template
  (package
    (name "sbcl-html-template")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edicl/html-template")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wz3czvjsn4x971dsiia9f9nvvcmbkablcl75zsvxndkimc93wxb"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://edicl.github.io/html-template/")
    (synopsis "HTML templates from Common Lisp")
    (description
     "HTML-TEMPLATE is a Common Lisp library which can be used to fill
templates with arbitrary (string) values at runtime.  The result does not have
to be HTML.

It is loosely modeled after the Perl module @code{HTML::Template} and
partially compatible with a its syntax, though both libraries contain some
extensions that the other does not support.

HTML-TEMPLATE translates templates into efficient closures which can be
re-used as often as needed.  It uses a cache mechanism so you can update
templates while your program is running and have the changes take effect
immediately.")
    (license license:bsd-2)))

(define-public ecl-html-template
  (sbcl-package->ecl-package sbcl-html-template))

(define-public cl-html-template
  (sbcl-package->cl-source-package sbcl-html-template))

(define-public sbcl-quickproject
  (package
    (name "sbcl-quickproject")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xach/quickproject")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1szs8p2wr1yr9mjmj3h3557l6wxzzga0iszimb68z0hb1jj3lva6"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("cl-fad" ,sbcl-cl-fad)
       ("html-template" ,sbcl-html-template)))
    (arguments
     '(#:asd-files '("quickproject.asd")))
    (home-page "https://xach.com/lisp/quickproject/")
    (synopsis "Create Common Lisp project skeletons")
    (description
     "Quickproject provides a quick way to make a Common Lisp project.  After
creating a project, it extends the ASDF registry so the project may be
immediately loaded.")
    (license license:expat)))

(define-public ecl-quickproject
  (sbcl-package->ecl-package sbcl-quickproject))

(define-public cl-quickproject
  (sbcl-package->cl-source-package sbcl-quickproject))

(define-public sbcl-bodge-math
  (let ((commit "9159b7faf88d440024c07110dbef2abddb20b9af")
        (revision "1"))
    (package
     (name "sbcl-bodge-math")
     (version (git-version "1.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/borodust/bodge-math")
             (commit commit)))
       (file-name (git-file-name "bodge-math" version))
       (sha256
        (base32 "0r3vnl9lywn4ksy34apcv6j825qp7l1naddawr14v4lwacndb80v"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      (list sbcl-bodge-utilities sbcl-rtg-math))
     (home-page "https://github.com/borodust/bodge-math")
     (synopsis "Common Lisp core math utilities of BODGE library collection")
     (description
      "This Common Lisp package contains the core math utilities of the
@emph{Bodge} library collection.")
     (license license:expat))))

(define-public ecl-bodge-math
  (sbcl-package->ecl-package sbcl-bodge-math))

(define-public cl-bodge-math
  (sbcl-package->cl-source-package sbcl-bodge-math))

(define-public sbcl-bodge-blobs-support
  (let ((commit "c5034ca5f4fc3a44dbadeba215a09afd59a404b0")
        (revision "1"))
    (package
     (name "sbcl-bodge-blobs-support")
     (version (git-version "1.0.0" revision commit))
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/borodust/bodge-blobs-support")
              (commit commit)))
        (file-name (git-file-name "bodge-blobs-support" version))
        (sha256
         (base32 "02nd1x6y1akp1ymv1y4z9ympwbnpd1drwi4f86xbjszxqff6jyj8"))))
     (build-system asdf-build-system/sbcl)
     (native-inputs
      (list sbcl-trivial-features))
     (inputs
      (list sbcl-cffi sbcl-alexandria))
     (home-page "https://github.com/borodust/bodge-blobs-support")
     (synopsis "Common Lisp utilities for blob packages")
     (description
      "This is a Common Lisp library for simplifying packaging and loading of
compiled foreign library collection.")
     (license license:unlicense))))

(define-public cl-bodge-blobs-support
  (sbcl-package->cl-source-package sbcl-bodge-blobs-support))

(define-public ecl-bodge-blobs-support
  (sbcl-package->ecl-package sbcl-bodge-blobs-support))

(define-public sbcl-cl-conspack
  (let ((commit "fc8473bc6f929696b03b43820596b7c976c4678e")
        (revision "1"))
    (package
     (name "sbcl-cl-conspack")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/conspack/cl-conspack")
             (commit commit)))
       (file-name (git-file-name "cl-conspack" version))
       (sha256
        (base32 "0b7qzvsrpvnw12hqhjmz0b02sigj0kdjy55j4k7xzmj8684cs8bx"))))
     (build-system asdf-build-system/sbcl)
     ;; FIXME: (Sharlatan-20210331T220652+0100): Test are disabled because of:
     ;;
     ;; Error while trying to load definition for system cl-conspack-test
     ;; from pathname .../cl-conspack/cl-conspack-test.asd:
     ;; Error opening .../checkl/formalize-tmpGHU3ALSV.fasl": Permission denied
     ;;
     ;; It looks like the issues is in CheckL itself as other packages keep
     ;; failing test where it's in use.
     (arguments
      '(#:tests? #f
        #:asd-files '("cl-conspack.asd")))
     (native-inputs
      (list sbcl-checkl))
     (inputs
      `(("alexandria" ,sbcl-alexandria)
        ("closer-mop" ,sbcl-closer-mop)
        ("fast-io" ,sbcl-fast-io)
        ("ieee-floats" ,sbcl-ieee-floats)
        ("trivial-garbage" ,sbcl-trivial-garbage)
        ("trivial-utf-8" ,sbcl-trivial-utf-8)))
     (home-page "https://github.com/conspack/cl-conspack")
     (synopsis "CONSPACK implementation for Common Lisp")
     (description
      "This package provides a CONSPACK implementation for Common Lisp.")
     (license license:bsd-3))))

(define-public ecl-cl-conspack
  (sbcl-package->ecl-package sbcl-cl-conspack))

(define-public cl-conspack
  (sbcl-package->cl-source-package sbcl-cl-conspack))

(define-public sbcl-cl-opengl
  (let ((commit "e2d83e0977b7e7ac3f3d348d8ccc7ccd04e74d59")
        (revision "1"))
    (package
      (name "sbcl-cl-opengl")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/3b/cl-opengl")
               (commit commit)))
         (file-name (git-file-name "cl-opengl" version))
         (sha256
          (base32 "0mhqmll09f079pnd6mgswz9nvr6h5n27d4q7zpmm2igf1v460id7"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems '("cl-opengl" "cl-glu" "cl-glut")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-lib-path
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "gl/library.lisp"
                 (("libGL.so" all)
                  (search-input-file inputs "/lib/libGL.so")))
               (substitute* "glu/library.lisp"
                 (("libGLU.so" all)
                  (search-input-file inputs "/lib/libGLU.so")))
               (substitute* "glut/library.lisp"
                 (("libglut.so" all)
                  (search-input-file inputs "/lib/libglut.so"))))))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("float-features" ,sbcl-float-features)
         ("freeglut" ,freeglut)
         ("glu" ,glu)
         ("mesa" ,mesa)))
      (home-page "https://github.com/3b/cl-opengl")
      (synopsis "Common Lisp bindings to OpenGL, GLU and GLUT APIs")
      (description
       "This package provides a set of bindings and utilities for accessing the
OpenGL (Mesa), GLU and GLUT (FreeGLUT) APIs using CFFI.")
      (license license:bsd-3))))

(define-public ecl-cl-opengl
  (sbcl-package->ecl-package sbcl-cl-opengl))

(define-public cl-opengl
  (sbcl-package->cl-source-package sbcl-cl-opengl))

(define-public sbcl-shadow
  (let ((commit "b2031adbfba3579b48c9d39ad997e19b79b6852f")
        (revision "1"))
    (package
      (name "sbcl-shadow")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.mfiano.net/mfiano/shadow")
               (commit commit)))
         (file-name (git-file-name "shadow" version))
         (sha256
          (base32 "0w1i734gkdkziin74ql2nhx7jdjxx02ylssaa6qdrvnj4br1124a"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cffi
             sbcl-cl-opengl
             sbcl-glsl-packing
             sbcl-golden-utils
             sbcl-static-vectors
             sbcl-varjo))
      (home-page "https://git.mfiano.net/mfiano/shadow")
      (synopsis "Management system for OpenGL shader programs")
      (description
       "This package provides a Common Lisp library for defining OpenGL shader
programs.  There are also functions for referencing shader programs by name,
querying for basic information about them, modifying uniform variables
throughout the lifecycle of an OpenGL application, and managing certain OpenGL
buffer object types (UBO, SSBO currently).")
      (license license:expat))))

(define-public ecl-shadow
  (sbcl-package->ecl-package sbcl-shadow))

(define-public cl-shadow
  (sbcl-package->cl-source-package sbcl-shadow))

(define-public sbcl-umbra
  (let ((commit "d6ef2f6cbfa26180929061129eaf325bf17f73d8")
        (revision "1"))
    (package
      (name "sbcl-umbra")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.mfiano.net/mfiano/umbra")
               (commit commit)))
         (file-name (git-file-name "umbra" version))
         (sha256
          (base32 "04vyh2j00zdpb8ryxr8g81wjcmqlz9wrn55r3cypcj4qg970r5wi"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-golden-utils sbcl-shadow sbcl-varjo))
      (home-page "https://git.mfiano.net/mfiano/umbra")
      (synopsis "Common Lisp library of reusable GPU shader functions")
      (description
       "This is a Common Lisp library consisting of a collection of useful GPU
shader functions, written with @code{Shadow}.")
      (license license:expat))))

(define-public ecl-umbra
  (sbcl-package->ecl-package sbcl-umbra))

(define-public cl-umbra
  (sbcl-package->cl-source-package sbcl-umbra))

(define-public sbcl-abstract-classes
  (let ((commit "7fa74f1e057f9ba7c1ffecff14f049f979e45267")
        (revision "1"))
    (package
      (name "sbcl-abstract-classes")
      (version (git-version "1.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://bitbucket.org/eeeickythump/cl-abstract-classes")
               (commit commit)))
         (file-name (git-file-name "cl-abstract-classes" version))
         (sha256
          (base32 "06lby4i6xbbgs7kgb0f3fqybvyskyg6djhrf967lnysv7hn3zpg9"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("closer-mop" ,sbcl-closer-mop)))
      (arguments
       `(#:asd-systems '("abstract-classes" "singleton-classes")))
      (home-page "https://bitbucket.org/eeeickythump/cl-abstract-classes")
      (synopsis "Abstract, final, and singleton metaclasses for CLOS")
      (description
       "This package provides Common Lisp extension to the MOP to allow
abstract, final and singleton classes.")
      (license license:public-domain))))

(define-public ecl-abstract-classes
  (sbcl-package->ecl-package sbcl-abstract-classes))

(define-public cl-abstract-classes
  (sbcl-package->cl-source-package sbcl-abstract-classes))

(define-public sbcl-coalton
  (let ((commit "012f6c8db6d73df16f7729090a12a929fb82db17")
        (revision "2"))
    (package
      (name "sbcl-coalton")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/coalton-lang/coalton")
               (commit commit)))
         (file-name (git-file-name "coalton" version))
         (sha256
          (base32 "1j3d12vyyn7y9nz2an4xmaa5si0jbxbwq7y61hq2b7vk376zvw18"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiasco))
      (inputs
       (list sbcl-alexandria
             sbcl-float-features
             sbcl-fset
             sbcl-global-vars
             sbcl-json-streams
             sbcl-serapeum
             sbcl-trivia))
      (home-page "https://coalton-lang.github.io")
      (synopsis "Dialect of ML in Common Lisp")
      (description
       "Coalton is a dialect of ML embedded in Common Lisp.  It emphasizes
practicality and interoperability with Lisp, and is intended to be a DSL that
allows one to gradually make their programs safer.")
      (license license:expat))))

(define-public ecl-coalton
  (sbcl-package->ecl-package sbcl-coalton))

(define-public cl-coalton
  (sbcl-package->cl-source-package sbcl-coalton))

(define-public sbcl-clip
  (let ((commit "7afa68702fbb99c47ed115ea0faccd97a29d9b2e")
        (revision "1"))
    (package
      (name "sbcl-clip")
      (version (git-version "0.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/shinmera/clip")
               (commit commit)))
         (file-name (git-file-name "clip" version))
         (sha256
          (base32 "13kkajkva2shm19fvn4yashsw18l6imv2rmy3hmpcky7g5ay7bv3"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-array-utils sbcl-lquery))
      (home-page "https://shinmera.github.io/clip/")
      (synopsis "Common Lisp HTML templating engine")
      (description
       "Clip is an attempt at a templating library that allows you to write
templates in a way that is both accessible to direct webdesign and
flexible.  The main idea is to incorporate transformation commands into an HTML
file through tags and attributes.  Clip is heavily dependent on Plump and
lQuery.")
      (license license:zlib))))

(define-public ecl-clip
  (sbcl-package->ecl-package sbcl-clip))

(define-public cl-clip
  (sbcl-package->cl-source-package sbcl-clip))

(define-public sbcl-pathname-utils
  (let ((commit "70f517e44e13a38e0c8f296613236376d679fa8f")
        (revision "1"))
    (package
      (name "sbcl-pathname-utils")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/pathname-utils")
               (commit commit)))
         (file-name (git-file-name "pathname-utils" version))
         (sha256
          (base32 "1zm4bf6gajpgmhr7zwf7600zlaf8fs1fcyzabqsh2ma2crkgqdxq"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-parachute))
      (home-page "https://shinmera.github.io/pathname-utils/")
      (synopsis "Collection of utilities to help with pathname operations")
      (description
       "This package provides a Common Lisp system a with collection of common
tests and operations to help handling pathnames.  It does not actually deal in
handling the accessing of files on the underlying system however.")
      (license license:zlib))))

(define-public ecl-pathname-utils
  (sbcl-package->ecl-package sbcl-pathname-utils))

(define-public cl-pathname-utils
  (sbcl-package->cl-source-package sbcl-pathname-utils))

(define-public sbcl-terrable
  (let ((commit "e4fe23ffa08e8d53a8168105b413861da59cc786")
        (revision "1"))
    (package
     (name "sbcl-terrable")
     (version (git-version "1.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Shirakumo/terrable")
             (commit commit)))
       (file-name (git-file-name "terrable" version))
       (sha256
        (base32 "0pnqflgz410zydc1ivwnd8hcl24bgr7x12yjzr7g4lq3ibc8y97b"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      (list sbcl-documentation-utils sbcl-fast-io sbcl-ieee-floats
            sbcl-static-vectors sbcl-trivial-garbage))
     (home-page "https://shirakumo.github.io/terrable/")
     (synopsis "Parser library for Terragen TER terrain files")
     (description
      "This package provides Common Lisp support for reading the Terragen
@code{.TER} format.  The format specification can be found at
@url{https://planetside.co.uk/wiki/index.php?title=Terragen_.TER_Format}")
     (license license:zlib))))

(define-public ecl-terrable
  (sbcl-package->ecl-package sbcl-terrable))

(define-public cl-terrable
  (sbcl-package->cl-source-package sbcl-terrable))

(define-public sbcl-simple-rgb
  (let ((commit "ba9b0689362c28aa6a91c0636796c6c372657293")
        (revision "1"))
    (package
      (name "sbcl-simple-rgb")
      (version (git-version "0.01" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/wmannis/simple-rgb/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ggv0h2n4mvwnggjr1b40gw667gnyykzki2zadaczi38ydzyzlp1"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/wmannis/simple-rgb")
      (synopsis "Manipulate colors in RGB format")
      (description
       "This Common Lisp library focuses on the small set of basic color
manipulations (lightening, compliments, etc.) you might use to generate a
color palette for a GUI or web page.")
      (license license:bsd-2))))

(define-public ecl-simple-rgb
  (sbcl-package->ecl-package sbcl-simple-rgb))

(define-public cl-simple-rgb
  (sbcl-package->cl-source-package sbcl-simple-rgb))

(define-public sbcl-cl-qprint
  (let ((commit "bfe398551cbfb7ca84a9ba59a26a1116ac5c06eb")
        (revision "1"))
    (package
      (name "sbcl-cl-qprint")
      (version (git-version "0.9.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eugeneia/cl-qprint/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "099h0rrdzxnlmn8avi72mg2dl0kccp7w01b2p9nwyy4b8yr32cir"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-flexi-streams))
      (home-page "https://github.com/eugeneia/cl-qprint/")
      (synopsis "Implementation of the quoted-printable encoding")
      (description
       "This Common Lisp library implements the quoted-printable encoding as
described in RFC 2045 (see @url{http://tools.ietf.org/html/rfc2045}).")
      (license license:lgpl2.1))))

(define-public ecl-cl-qprint
  (sbcl-package->ecl-package sbcl-cl-qprint))

(define-public cl-qprint
  (sbcl-package->cl-source-package sbcl-cl-qprint))

(define-public sbcl-cl-mime
  (let ((commit "d30a28e0a40393bd3af7d138daa05319ed2e9d07")
        (revision "1"))
    (package
      (name "sbcl-cl-mime")
      ;; One commit says "update to cl-mime-0.5.3", even though the .asd is at 0.5.1.
      (version (git-version "0.5.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               ;; Maintained fork according to http://www.cliki.net/CL-MIME:
               (url "https://github.com/40ants/cl-mime")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qn8if0fj6vzc897pqqqs0m1y107gmzqngpqhqmwrcsp1ckj5k0v"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre sbcl-cl-base64 sbcl-cl-qprint))
      (native-inputs
       (list sbcl-rove))
      (home-page "https://github.com/eugeneia/cl-qprint/")
      (synopsis "Read and print MIME content in Common Lisp")
      (description
       "This is a Common Lisp library for reading and printing MIME content.
It supports automatic conversion between 7-bit, quoted-printable and base64
encodings.")
      (license license:lgpl2.1))))

(define-public ecl-cl-mime
  (sbcl-package->ecl-package sbcl-cl-mime))

(define-public cl-mime
  (sbcl-package->cl-source-package sbcl-cl-mime))

(define-public sbcl-lispbuilder-sdl
  (let ((commit "589b3c6d552bbec4b520f61388117d6c7b3de5ab"))
    (package
      (name "sbcl-lispbuilder-sdl")
      (version (git-version "0.9.8.2" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lispbuilder/lispbuilder")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zga59fjlhq3mhwbf80qwqwpkjkxqnn2mhxajlb8563vhn3dbafp"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("sdl" ,sdl)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'cd-sdl
             (lambda _
               (chdir "lispbuilder-sdl")
               #t))
           (add-after 'cd-sdl 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "cffi/library.lisp"
                 (("libSDL[^\"]*" all)
                  (string-append (assoc-ref inputs "sdl") "/lib/" all)))
               #t)))))
      (home-page "https://github.com/lispbuilder/lispbuilder/wiki/LispbuilderSDL")
      (synopsis "Common Lisp wrapper for SDL")
      (description
       "This library is an SDL wrapper as part of an umbrella project that
provides cross-platform packages for building large, interactive applications
in Common Lisp.")
      (license license:expat))))

(define-public ecl-lispbuilder-sdl
  (sbcl-package->ecl-package sbcl-lispbuilder-sdl))

(define-public cl-lispbuilder-sdl
  (sbcl-package->cl-source-package sbcl-lispbuilder-sdl))

(define-public sbcl-dufy
  (package
    (name "sbcl-dufy")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/privet-kitty/dufy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15vrp1kayhjb5c1vc9x8wlm8rimk73vpa7yc101cf0gsg1fjazl6"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-alexandria sbcl-cl-ppcre))
    (native-inputs
     (list sbcl-fiveam sbcl-cl-csv sbcl-parse-float sbcl-lispbuilder-sdl
           sbcl-lparallel))
    (home-page "https://github.com/privet-kitty/dufy")
    (synopsis "Color library for Common Lisp")
    (description
     "Dufy is a library for exact color manipulation and conversion in various
color spaces, which supports many color models.")
    (license license:expat)))

(define-public ecl-dufy
  (sbcl-package->ecl-package sbcl-dufy))

(define-public cl-dufy
  (sbcl-package->cl-source-package sbcl-dufy))

(define-public sbcl-bknr-datastore
  (let ((commit "c98d44f47cc88d19ff91ca3eefbd9719a8ace022")
        (revision "1"))
    (package
      (name "sbcl-bknr-datastore")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hanshuebner/bknr-datastore")
               (commit commit)))
         (file-name (git-file-name "bknr-datastore" version))
         (sha256
          (base32 "1vi3w65fnczqvswkm381n6liqfrzjrg40y698qvj7skj28dm5vrm"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems
         '("bknr.datastore"
           "bknr.impex"
           "bknr.indices"
           "bknr.skip-list"
           "bknr.utils"
           "bknr.xml")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'enter-source-directory
             (lambda _
               (chdir "src")
               #t)))))
      (native-inputs
       `(("cl-store" ,sbcl-cl-store)
         ("fiveam" ,sbcl-fiveam)
         ("unit-test" ,sbcl-unit-test)))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("closer-mop" ,sbcl-closer-mop)
         ("cl-interpol" ,sbcl-cl-interpol)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("cxml" ,sbcl-cxml)
         ("flexi-streams" ,sbcl-flexi-streams)
         ("md5" ,sbcl-md5)
         ("trivial-utf-8" ,sbcl-trivial-utf-8)
         ("yason" ,sbcl-yason)))
      (home-page "https://github.com/hanshuebner/bknr-datastore")
      (synopsis "In-memory database for Common Lisp")
      (description
       "BKNR.DATASTORE is an in-memory CLOS based database with transactions
for Common Lisp.")
      (license license:bsd-0))))

;; NOTE: (Sharlatan-20210429T191426+0100):
;; There is no port for ECL in upstream yet
;; (define-public ecl-bknr-datastore
;;   (sbcl-package->ecl-package sbcl-bknr-datastore))

(define-public cl-bknr-datastore
  (sbcl-package->cl-source-package sbcl-bknr-datastore))

(define-public sbcl-authentic
  (let ((commit "4e9194dda227b98f56dda1c2a2480efcc2d1f973")
        (revision "2"))
    (package
      (name "sbcl-authentic")
      (version (git-version "0.1.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/charje/cl-authentic")
               (commit commit)))
         (file-name (git-file-name "cl-authentic" version))
         (sha256
          (base32 "0ncsxrybnx0pjsndv3j8w4lphlpcsld8sxg3c5b46fb3a8nd4ssf"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-clsql sbcl-ironclad))
      (home-page "https://github.com/charje/cl-authentic")
      (synopsis "User/password management for Common Lisp applications")
      (description "Authentic provides a light-weight and extendible
solution to user/password management for Common Lisp applications.  It has
features such as safe password storage in a database, password reset, user
confirmation tokens, and user authentication.")
      (license license:llgpl))))

(define-public ecl-authentic
  (sbcl-package->ecl-package sbcl-authentic))

(define-public cl-authentic
  (sbcl-package->cl-source-package sbcl-authentic))

(define-public sbcl-3d-vectors
  (let ((commit "29bb9684df803590deed344af63dbf7b712aabc0")
        (revision "1"))
    (package
      (name "sbcl-3d-vectors")
      (version (git-version "3.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/3d-vectors")
               (commit commit)))
         (file-name (git-file-name "3d-vectors" version))
         (sha256
          (base32 "0qc7m5brhpwi531rgmlaj1c609by533a1ia5hv8f90ilm8ksmw3l"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-parachute))
      (inputs
       (list sbcl-documentation-utils))
      (home-page "https://shinmera.github.io/3d-vectors/")
      (synopsis "Utility library implementing 2D, 3D, and 4D vectors")
      (description
       "@code{3D-VECTORS} is a library for vector math in 3D space.  It contains
most of the vector operations one would usually expect out of such a library and
offers them both in non-modifying and modifying versions where applicable.")
      (license license:zlib))))

(define-public ecl-3d-vectors
  (sbcl-package->ecl-package sbcl-3d-vectors))

(define-public cl-3d-vectors
  (sbcl-package->cl-source-package sbcl-3d-vectors))

(define-public sbcl-3d-matrices
  (let ((commit "f453b521b8f2ceabb01eac94389119dece8c05f8")
        (revision "1"))
    (package
      (name "sbcl-3d-matrices")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/3d-matrices")
               (commit commit)))
         (file-name (git-file-name "3d-matrices" version))
         (sha256
          (base32 "10q9c96gqzq6k8n89agy0khgimmnsn4s69171m3vhghqa2yv5n6v"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-parachute))
      (inputs
       (list sbcl-3d-vectors sbcl-documentation-utils))
      (home-page "https://shinmera.github.io/3d-matrices/")
      (synopsis "Utility library implementing 2x2, 3x3, 4x4 and NxM matrices")
      (description
       "@code{3D-MATRICES} is a library implementing common matrix operations,
mainly intended as the counterpiece to @code{3d-vectors} and thus being aimed at
operations in 3D space.")
      (license license:zlib))))

(define-public ecl-3d-matrices
  (sbcl-package->ecl-package sbcl-3d-matrices))

(define-public cl-3d-matrices
  (sbcl-package->cl-source-package sbcl-3d-matrices))

(define-public sbcl-messagebox
  (let ((commit "ea3688d9a9954bee7079c0173bc7b3f327021e9f")
        (revision "1"))
    (package
      (name "sbcl-messagebox")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/messagebox")
               (commit commit)))
         (file-name (git-file-name "messagebox" version))
         (sha256
          (base32 "0jkbzlca0wvspgsfj0b0hjwlyyy8jlywsldsbkp79q48fc3aa8jd"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-zenity-path
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "zenity.lisp"
                 (("\"zenity\"")
                  (string-append "\"" (assoc-ref inputs "zenity")
                                 "/bin/zenity\""))))))))
      (inputs
       `(("documentation-utils" ,sbcl-documentation-utils)
         ("trivial-features" ,sbcl-trivial-features)
         ("zenity" ,zenity)))
      (home-page "https://shinmera.github.io/messagebox/")
      (synopsis "Display a native GUI message box")
      (description
       "This is a small library to display a native GUI message box.  This can be
useful to show error messages and other informational pieces should the
application fail and be unable to do so using its standard UI.")
      (license license:zlib))))

(define-public ecl-messagebox
  (sbcl-package->ecl-package sbcl-messagebox))

(define-public cl-messagebox
  (sbcl-package->cl-source-package sbcl-messagebox))

(define-public sbcl-glsl-toolkit
  (let ((commit "d00ba1906e3b5eb08ea346ac300a1e77bb999d04")
        (revision "1"))
    (package
      (name "sbcl-glsl-toolkit")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shirakumo/glsl-toolkit")
               (commit commit)))
         (file-name (git-file-name "glsl-toolkit" version))
         (sha256
          (base32 "0as5796yazchq1qkna3srxlz5v7cf7ffny9cbqi41wsa2s20vbh9"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre sbcl-documentation-utils sbcl-parse-float
             sbcl-trivial-indent))
      (home-page "https://shirakumo.github.io/glsl-toolkit/")
      (synopsis "Parser for OpenGL Shader Language source files")
      (description
       "This package provides Common Lisp system collecting tools written to
wrangle OpenGL Shader Language (GLSL) source files.")
      (license license:zlib))))

(define-public ecl-glsl-toolkit
  (sbcl-package->ecl-package sbcl-glsl-toolkit))

(define-public cl-glsl-toolkit
  (sbcl-package->cl-source-package sbcl-glsl-toolkit))

(define-public sbcl-simple-tasks
  (let ((commit "745d4b54eac9bf5d6909792e63ecd2ef8d303cf2")
        (revision "1"))
    (package
      (name "sbcl-simple-tasks")
      (version (git-version "1.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/simple-tasks")
               (commit commit)))
         (file-name (git-file-name "simple-tasks" version))
         (sha256
          (base32 "1ls1pij7dvb65g4nam7nvik1218jvfk5iplr48vy290fw3lq7v98"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-array-utils sbcl-bordeaux-threads sbcl-dissect))
      (home-page "https://shinmera.github.io/simple-tasks/")
      (synopsis "Simple task scheduling framework")
      (description "This is a task scheduling framework for Common Lisp.")
      (license license:zlib))))

(define-public ecl-simple-tasks
  (sbcl-package->ecl-package sbcl-simple-tasks))

(define-public cl-simple-tasks
  (sbcl-package->cl-source-package sbcl-simple-tasks))

(define-public sbcl-trivial-main-thread
  (let ((commit "25f114973bb69eb63e01d0bbfead31f8e682846a")
        (revision "1"))
    (package
      (name "sbcl-trivial-main-thread")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/trivial-main-thread")
               (commit commit)))
         (file-name (git-file-name "trivial-main-thread" version))
         (sha256
          (base32 "0bw1887i7396lqg75qvmgjfzz4xbiq9w5dp8wxdgrcsm0qwlraw7"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-bordeaux-threads sbcl-simple-tasks sbcl-trivial-features))
      (home-page "https://shinmera.github.io/trivial-main-thread/")
      (synopsis "Compatibility library to run things in the main thread")
      (description
       "This package provides a Common Lisp system which wraps the
BORDEAUX-THREADS system to be able to run things in the main thread of the
implementation, for example drawing calls of GUI applications.")
      (license license:zlib))))

(define-public ecl-trivial-main-thread
  (sbcl-package->ecl-package sbcl-trivial-main-thread))

(define-public cl-trivial-main-thread
  (sbcl-package->cl-source-package sbcl-trivial-main-thread))

(define-public sbcl-cl-package-locks
  (let ((commit "96a358ede7cef416d61d2f699e724fe1d9de602c")
        (revision "1"))
    (package
      (name "sbcl-cl-package-locks")
      (version (git-version "0.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/elliottjohnson/cl-package-locks")
               (commit commit)))
         (file-name (git-file-name "cl-package-locks" version))
         (sha256
          (base32 "0g3gfljnvpgd66ccd2sqawlkwqx4a0wsdrg5180va61w869cgxqq"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/elliottjohnson/cl-package-locks")
      (synopsis "Compatibility layer for dealing with package locks")
      (description
       "This is a Common Lisp library providing a unified way to work with
package locks across supported Common Lisp implementations.")
      (license license:expat))))

(define-public ecl-cl-package-locks
  (sbcl-package->ecl-package sbcl-cl-package-locks))

(define-public cl-package-locks
  (sbcl-package->cl-source-package sbcl-cl-package-locks))

(define-public sbcl-cl-typesetting
  (let ((commit "86eba981fc4254addebecf765578ec350d6e3b75")
        (revision "1"))
    (package
      (name "sbcl-cl-typesetting")
      (version (git-version "0.8.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mbattyani/cl-typesetting")
               (commit commit)))
         (file-name (git-file-name "cl-typesetting" version))
         (sha256
          (base32 "0fcs5mq0gxfczbrg7ay8r4bf5r4g6blvpdbjkhcl8dapcikyn35h"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; NOTE: (Sharlatan-20210515T213014+0100): Circular dependencies with
       ;; cl-typegraph
       `(#:tests? #f))
      (inputs
       `(("cl-pdf" ,sbcl-cl-pdf)
         ("xmls" ,sbcl-xmls)))
      (home-page "https://github.com/mbattyani/cl-typesetting")
      (synopsis "Stand-alone Common Lisp typesetting system")
      (description
       "CL-TYPESETTING is a cross-platform Common Lisp typesetting library for
all kind of typesetting applications.")
      (license license:bsd-2))))

(define-public ecl-cl-typesetting
  (sbcl-package->ecl-package sbcl-cl-typesetting))

(define-public cl-typesetting
  (sbcl-package->cl-source-package sbcl-cl-typesetting))

(define-public sbcl-shasht
  (let ((commit "4055327ef8e2aaa8627892ab256350ff3cb15e3c")
        (revision "1"))
    (package
      (name "sbcl-shasht")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yitzchak/shasht")
               (commit commit)))
         (file-name (git-file-name "shasht" version))
         (sha256
          (base32 "01mh20s5gj0lajq45anxji77ykq1wcg72mn1y9a1k8i7q1ainjlr"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-alexandria sbcl-parachute))
      (inputs
       (list sbcl-trivial-do))
      (home-page "https://yitzchak.github.io/shasht/")
      (synopsis "Common Lisp JSON reading and writing library")
      (description
       "This package provides a Common Lisp library to work with the JSON file
format.")
      (license license:expat))))

(define-public ecl-shasht
  (sbcl-package->ecl-package sbcl-shasht))

(define-public cl-shasht
  (sbcl-package->cl-source-package sbcl-shasht))

(define-public sbcl-cl-speedy-queue
  (let ((commit "0425c7c62ad3b898a5ec58cd1b3e74f7d91eec4b")
        (revision "1"))
    (package
      (name "sbcl-cl-speedy-queue")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zkat/cl-speedy-queue")
               (commit commit)))
         (file-name (git-file-name "cl-speedy-queue" version))
         (sha256
          (base32 "0czhnvxn9lvbjz9h1lb7y18nqrsq3drq5icd3lqdaa07362alriq"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/zkat/cl-speedy-queue")
      (synopsis "Lightweight optimized queue for Common Lisp")
      (description
       "This is a lightweight, non-consing, optimized queue implementation for
Common Lisp.")
      (license license:expat))))

(define-public cl-speedy-queue
  (sbcl-package->cl-source-package sbcl-cl-speedy-queue))

(define-public ecl-cl-speedy-queue
  (sbcl-package->ecl-package sbcl-cl-speedy-queue))

(define-public sbcl-lev
  (let ((commit "7d03c68dad44f1cc4ac2aeeab2d24eb525ad941a")
        (revision "1"))
    (package
      (name "sbcl-lev")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/lev")
               (commit commit)))
         (file-name (git-file-name "lev" version))
         (sha256
          (base32 "14lfnrvfyg2nnvlwck896p6vgarzc6g4kijmvhi2d8wra7gxzifh"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; NOTE: (Sharlatan-20210520T163300+0100): No tests in upstream
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-libev-lib-path
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/lev.lisp"
                 (("libev.so" _)
                  (search-input-file inputs "/lib/libev.so"))))))))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("libev" ,libev)))
      (home-page "https://github.com/fukamachi/lev")
      (synopsis "Common Lisp bindings for libev")
      (description "This package provides Common Lisp bindings for libev.")
      (license license:bsd-2))))

(define-public cl-lev
  (sbcl-package->cl-source-package sbcl-lev))

(define-public ecl-lev
  (sbcl-package->ecl-package sbcl-lev))

(define-public sbcl-woo
  (let ((commit "fba3567be95ed6e782d98a4c1477d3a74b8ad124")
        (revision "1"))
    (package
      (name "sbcl-woo")
      (version (git-version "0.12.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/woo")
               (commit commit)))
         (file-name (git-file-name "woo" version))
         (sha256
          (base32 "06f95x8s8v523gxmrkn9wwgw2pvc3bc66znbgrzhqb30y4aar5v5"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; FIXME: Tests fail because they try to compile clack-test:
       ;;
       ;;   Error opening #P"/gnu/store/...-sbcl-clack-2.0.0-1.e3e0328/
       ;;   lib/common-lisp/sbcl/clack/src/test-tmpGHU3ALSV.fasl":
       ;;
       ;;   Permission denied
       ;;
       ;; clack-test should be compiled when building the sbcl-clack package,
       ;; but it isn't right now because of the circular dependency between
       ;; clack-test and dexador.
       `(#:tests? #f))
      (native-inputs
       `(("clack" ,sbcl-clack)
         ("rove" ,sbcl-rove)))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cffi" ,sbcl-cffi)
         ("cl-speedy-queue" ,sbcl-cl-speedy-queue) ;; Required for ecl build
         ("clack-socket" ,sbcl-clack)
         ("fast-http" ,sbcl-fast-http)
         ("fast-io" ,sbcl-fast-io)
         ("lev" ,sbcl-lev)
         ("quri" ,sbcl-quri)
         ("rove" ,sbcl-rove)
         ("smart-buffer" ,sbcl-smart-buffer)
         ("static-vectors" ,sbcl-static-vectors)
         ("swap-bytes" ,sbcl-swap-bytes)
         ("trivial-utf-8" ,sbcl-trivial-utf-8)
         ("vom" ,sbcl-vom)))
      (home-page "https://github.com/fukamachi/woo")
      (synopsis "Non-blocking HTTP server based on libev")
      (description
       "This package provides the Common Lisp HTTP server @code{WOO}, which
is built on top of the @code{libev} event library.")
      (license license:expat))))

(define-public cl-woo
  (sbcl-package->cl-source-package sbcl-woo))

(define-public ecl-woo
  (sbcl-package->ecl-package sbcl-woo))

(define-public sbcl-json-streams
  (let ((commit "5da012e8133affbf75024e7500feb37394690752")
        (revision "1"))
    (package
      (name "sbcl-json-streams")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rotatef/json-streams")
               (commit commit)))
         (file-name (git-file-name "json-streams" version))
         (sha256
          (base32 "0cia3721im04q73dfkd688d8splgpz03qa4h8s3r39kar4w3xll2"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-cl-quickcheck sbcl-flexi-streams))
      (home-page "https://github.com/rotatef/json-streams")
      (synopsis "Common Lisp library for reading and writing JSON")
      (description
       "This package provides a stream based JSON parser/writer, well suited as
building block for higher level libraries.")
      (license license:gpl3+))))

(define-public cl-json-streams
  (sbcl-package->cl-source-package sbcl-json-streams))

(define-public ecl-json-streams
  (sbcl-package->ecl-package sbcl-json-streams))

(define-public sbcl-arnesi
  (let ((commit "1e7dc4cb2cad8599113c7492c78f4925e839522e")
        (revision "1"))
    (package
      (name "sbcl-arnesi")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AccelerationNet/arnesi")
               (commit commit)))
         (file-name (git-file-name "arnesi" version))
         (sha256
          (base32 "0jgj2xgd1gq6rf8ia43lkmbrbxnp8rgs053br9azfa25ygk3ikbh"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; FIXME: (Sharlatan-20210523T190315+0100): Tests failed on
       ;; special-lisp-var-rebount-in/cc
       ;;
       ;; ; processing (TEST SPECIAL-LISP-VAR-REBOUND-IN/CC ...)
       ;; ; wrote .../sbcl/arnesi/t/call-cc-tmp5GEXGEG5.fasl
       ;; ; compilation finished in 0:00:00.028
       ;; Unhandled SIMPLE-ERROR in thread
       ;; #<SB-THREAD:THREAD "main thread" RUNNING {100B768173}>:
       ;; Sorry, No walker for the special operater DECLARE defined.
       ;;
       ;; Backtrace for: #<SB-THREAD:THREAD "main thread" RUNNING {100B768173}>
       ;; 0: (SB-DEBUG::DEBUGGER-DISABLED-HOOK #<SIMPLE-ERROR "Sorry,
       ;; No walker for the special operater ~S defined."
       ;; {1001FAF9D3}> #<unused argument> :QUIT T)
       ;;
       `(#:tests? #f))
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       `(("cl-ppcre" ,sbcl-cl-ppcre)
         ("collectors" ,sbcl-collectors)
         ("swank" ,sbcl-slime-swank)))
      (home-page "https://github.com/AccelerationNet/arnesi")
      (synopsis "Common Lisp utility suite")
      (description
       "ARNESI is Common Lisp utilities library similar to ALEXANDRIA, ANAPHORA
or GOLDEN-UTILS.")
      (license license:bsd-3))))

(define-public ecl-arnesi
  (sbcl-package->ecl-package sbcl-arnesi))

(define-public cl-arnesi
  (sbcl-package->cl-source-package sbcl-arnesi))

(define-public sbcl-gettext
  (let ((commit "a432020cbad99fc22cbe6bb9aa8a83a35000d7aa")
        (revision "1"))
    (package
      (name "sbcl-gettext")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rotatef/gettext")
               (commit commit)))
         (file-name (git-file-name "gettext" version))
         (sha256
          (base32 "1pzhamgni6k5hi6bbvlb3dm659pcllrrr3vhhn3rpjn238zxg5ar"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-stefil))
      (inputs
       (list sbcl-flexi-streams sbcl-split-sequence sbcl-cl-yacc))
      (home-page "https://github.com/rotatef/gettext")
      (synopsis "Common Lisp implementation of Gettext")
      (description
       "This package provides GNU @code{gettext} completely implemented in
Common Lisp without any C library bindings.")
      (license license:lgpl3+))))

(define-public ecl-gettext
  (sbcl-package->ecl-package sbcl-gettext))

(define-public cl-gettext
  (sbcl-package->cl-source-package sbcl-gettext))

(define-public sbcl-parser-combinators
  (let ((commit "9c7569a4f6af5e60c0d3a51d9c15c16d1714c845")
        (revision "1"))
    (package
      (name "sbcl-parser-combinators")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Ramarren/cl-parser-combinators")
               (commit commit)))
         (file-name (git-file-name "parser-combinators" version))
         (sha256
          (base32 "1k49vha5xm2cklayzpqwg73n4v93xwsbs5in6342pkkiimnidhs8"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; FIXME: (Sharlatan-20210523T184237+0100): Tests require `femlisp', which
       ;; is quite chunky not packaged system yet, enable them when it's packed.
       `(#:tests? #f
         #:test-asd-file "parser-combinators-tests.asd"))
      ;; (native-inputs
      ;;  `(("hu.dwim.stefil" ,sbcl-hu.dwim.stefil)
      ;;    ("infix" ,sbcl-femlisp)))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-containers" ,sbcl-cl-containers)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("iterate" ,sbcl-iterate)))
      (home-page "https://github.com/Ramarren/cl-parser-combinators")
      (synopsis "Parser combinators in Common Lisp")
      (description
       "PARSER-COMBINATORS is a library implementing monadic parser
combinators in Common Lisp, similar in concept to Haskell Parsec system.")
      (license license:bsd-3))))

(define-public ecl-parser-combinators
  (sbcl-package->ecl-package sbcl-parser-combinators))

(define-public cl-parser-combinators
  (sbcl-package->cl-source-package sbcl-parser-combinators))

(define-public sbcl-cl-locale
  (let ((commit "0a36cc0dcf5e0a8c8bf97869cd6199980ca25eec")
        (revision "1"))
    (package
      (name "sbcl-cl-locale")
      (version (git-version "0.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/cl-locale")
               (commit commit)))
         (file-name (git-file-name "cl-locale" version))
         (sha256
          (base32 "1rhannhpsw1yg1fpflam483a3w9qb1izgyvmnmiddv3dn4qsmn9p"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; FIXME: (Sharlatan-20210523T190658+0100): All tests passed ok but
       ;; successfully failed in the end:
       ;;
       ;; Summary:
       ;;   All 1 file passed.
       ;; Unhandled ASDF/FIND-COMPONENT:MISSING-DEPENDENCY
       ;; in thread #<SB-THREAD:THREAD "main thread" RUNNING {100B6C8253}>:
       ;;   Component CL-LOCALE-ASD::CL-LOCALE-TEST not found, required by
       ;;   #<SYSTEM "cl-locale">
       ;;
       `(#:tests? #f))
      (native-inputs
       `(("prove" ,sbcl-prove)
         ("flexi-streams" ,sbcl-flexi-streams)))
      (inputs
       `(("anaphora" ,sbcl-anaphora)
         ("arnesi" ,sbcl-arnesi)
         ("cl-annot" ,sbcl-cl-annot)
         ("cl-syntax" ,sbcl-cl-syntax)))
      (home-page "https://github.com/fukamachi/cl-locale")
      (synopsis "Internalization library for Common Lisp")
      (description
       "This package provides a Common Lisp translation library similar to
CL-I18N and CL-L10N.")
      (license license:llgpl))))

(define-public ecl-cl-locale
  (sbcl-package->ecl-package sbcl-cl-locale))

(define-public cl-locale
  (sbcl-package->cl-source-package sbcl-cl-locale))

(define-public sbcl-cl-slice
  (let ((commit "c531683f287216aebbb0affbe090611fa1b5d697")
        (revision "1"))
    (package
      (name "sbcl-cl-slice")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tpapp/cl-slice")
               (commit commit)))
         (file-name (git-file-name "cl-slice" version))
         (sha256
          (base32 "1ybznf4y5lda6bn163jcvj281qzhm24dfcwhbgxmm5n6f27gdccl"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-clunit))
      (inputs
       (list sbcl-alexandria sbcl-anaphora sbcl-let-plus))
      (home-page "https://github.com/tpapp/cl-slice")
      (synopsis "Array slices for Common Lisp")
      (description
       "This package provides a DSL for array slices in Common Lisp.")
      (license license:expat))))

(define-public ecl-cl-slice
  (sbcl-package->ecl-package sbcl-cl-slice))

(define-public cl-slice
  (sbcl-package->cl-source-package sbcl-cl-slice))

(define-public sbcl-djula
  (let ((commit "5df7af35a21503d468a878fc6029caa527a7d204")
        (revision "1"))
    (package
      (name "sbcl-djula")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mmontone/djula")
               (commit commit)))
         (file-name (git-file-name "djula" version))
         (sha256
          (base32 "1lk2ypm3sacf60h96a7hv9jwjlxkl4k40yzdalmqdg548vrd1jjm"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-access
             sbcl-alexandria
             sbcl-anaphora
             sbcl-babel
             sbcl-cl-locale
             sbcl-cl-ppcre
             sbcl-cl-slice
             sbcl-closer-mop
             sbcl-gettext
             sbcl-iterate
             sbcl-local-time
             sbcl-parser-combinators
             sbcl-split-sequence
             sbcl-trivial-backtrace))
      (home-page "https://mmontone.github.io/djula/")
      (synopsis "Common Lisp port of the Django templating language")
      (description
       "This package provides a Common Lisp templating system based on Python
Django with a syntax similar to Python Jinja2.")
      (license license:expat))))

(define-public ecl-djula
  (sbcl-package->ecl-package sbcl-djula))

(define-public cl-djula
  (sbcl-package->cl-source-package sbcl-djula))

(define-public sbcl-for
  (let ((commit "2e4fcfa0f9c1d2f4559c58cef20ccefa50ba180d")
        (revision "1"))
    (package
      (name "sbcl-for")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/for")
               (commit commit)))
         (file-name (git-file-name "for" version))
         (sha256
          (base32 "1akz9ggh33x2cq3h0f1cd0p632v1mbagv3dzsb0r10bwg9lh3nmv"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-documentation-utils sbcl-form-fiddle sbcl-lambda-fiddle))
      (home-page "https://shinmera.github.io/for/")
      (synopsis "Extensible iteration macro library")
      (description
       "For is a library for an extensible iteration macro.  It allows you to write
concise looping constructs similar to @code{loop} and @code{iterate}.  Unlike
loop however it is extensible and sensible, and unlike iterate it does not
require code-walking and is easier to extend.")
      (license license:zlib))))

(define-public ecl-for
  (sbcl-package->ecl-package sbcl-for))

(define-public cl-for
  (sbcl-package->cl-source-package sbcl-for))

(define-public sbcl-flare
  (let ((commit "4f9f17a4fc5b368c2a1988b9a20288695b8d8c48")
        (revision "1"))
    (package
      (name "sbcl-flare")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/flare")
               (commit commit)))
         (file-name (git-file-name "flare" version))
         (sha256
          (base32 "00nm3sjngbflj2gd5q0xs2m136w4kix6krww23nk64pkkyq2fs86"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-3d-vectors
             sbcl-array-utils
             sbcl-documentation-utils
             sbcl-for
             sbcl-lambda-fiddle
             sbcl-trivial-garbage))
      (home-page "https://shinmera.github.io/flare/")
      (synopsis "Easy particle systems with fine grained control")
      (description
       "FLARE is a library designed to allow quick and precise particle effect
creations.  It does not concern itself with displaying and only with the
management and movement of particles.  As such, it can easily be integrated into
any existing or future application.")
      (license license:zlib))))

(define-public ecl-flare
  (sbcl-package->ecl-package sbcl-flare))

(define-public cl-flare
  (sbcl-package->cl-source-package sbcl-flare))

(define-public sbcl-simple-inferiors
  (let ((commit "deac886354e03f8a9502ce96f12a0459ce3be671"))
    (package
      (name "sbcl-simple-inferiors")
      (version (git-version "1.1.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/simple-inferiors")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "08vsvqv3768bwb2y8mwxbw5wyqzzwqr7rd004r6gafdgf9p9mcx3"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-bordeaux-threads sbcl-documentation-utils))
      (home-page "https://shinmera.github.io/simple-inferiors/")
      (synopsis "Common Lisp library to use inferior processes")
      (description
       "This is a library to allow easy handling of external processes, and
primarily to get their output.  It handles proper copying of the standard and
error outputs of the process simultaneously, both in a sequential and parallel
fashion.  It also features a lazy directory switching mechanism, to avoid
running into parallelism problems when having to change directory.")
      (license license:zlib))))

(define-public ecl-simple-inferiors
  (sbcl-package->ecl-package sbcl-simple-inferiors))

(define-public cl-simple-inferiors
  (sbcl-package->cl-source-package sbcl-simple-inferiors))

(define-public sbcl-legit
  (let ((commit "48d50a20d8dcbd941d119683463b7c8257ba6458"))
    (package
      (name "sbcl-legit")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/legit")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "181aqpj4zkfk1aavj5jw8rym6gw4ma3gd64z2h5fpryabgmwk236"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-simple-inferiors sbcl-lambda-fiddle sbcl-cl-ppcre
             sbcl-documentation-utils))
      (home-page "https://shinmera.github.io/legit/")
      (synopsis "Interface to the git binary")
      (description
       "This is an interface to the @code{git} binary to make controlling it
from within Common Lisp much easier.  It might not ever reach full coverage of
all features given git's immense size, but features will be added as they are
needed.  The low-level command API is fully mapped however.")
      (license license:zlib))))

(define-public ecl-legit
  (sbcl-package->ecl-package sbcl-legit))

(define-public cl-legit
  (sbcl-package->cl-source-package sbcl-legit))

(define-public sbcl-flow
  (let ((commit "6d925af009cdfe033650d7048197a5e6ee937d15")
        (revision "1"))
    (package
      (name "sbcl-flow")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/flow")
               (commit commit)))
         (file-name (git-file-name "flow" version))
         (sha256
          (base32 "0ysw1kwiqlf8kzllhnz8v3q40dmvwf83fzq8bfkbmwy5hfjh3pxp"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; FIXME: (Sharlatan-20210527T203118+0100): FLOW-VISUALIZER requires
       ;; COMMONQT which is not packed yet and required tweaks with QT. Remove
       ;; this when it's ready.
       `(#:asd-files '("flow.asd")))
      (inputs
       `(("closer-mop" ,sbcl-closer-mop)
         ("documentation-utils" ,sbcl-documentation-utils)))
      (home-page "https://shinmera.github.io/flow/")
      (synopsis "Tools for the representation of graphs and flowcharts")
      (description
       "FLOW is a flowchart graph library.  Unlike other graphing libraries,
this one focuses on nodes in a graph having distinct @code{ports} through which
connections to other nodes are formed.  This helps in many concrete scenarios
where it is important to distinguish not only which nodes are connected, but
also how they are connected to each other.

Particularly, a lot of data flow and exchange problems can be reduced to such
a @code{flowchart}.  For example, an audio processing library may present its
pipeline as a flowchart of segments that communicate with each other through
audio sample buffers.  Flow gives a convenient view onto this kind of problem,
and even allows the generic visualisation of graphs in this format.")
      (license license:zlib))))

(define-public ecl-flow
  (sbcl-package->ecl-package sbcl-flow))

(define-public cl-flow
  (sbcl-package->cl-source-package sbcl-flow))

(define-public sbcl-cl-glfw3
  (let ((commit "32c3f34d592d55ee7ce932ed85804c1a9c4158c6")
        (revision "1"))
    (package
      (name "sbcl-cl-glfw3")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AlexCharlton/cl-glfw3")
               (commit commit)))
         (file-name (git-file-name "cl-glfw3" version))
         (sha256
          (base32 "1wzr43nckdx4rlgxzhm1r4kfc264q969mc43y0js9ramh7l8gba5"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-glfw-lib-path
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "glfw-bindings.lisp"
                 (("libglfw.so.3" _)
                  (search-input-file inputs "/lib/libglfw.so.3"))))))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("cl-opengl" ,sbcl-cl-opengl)
         ("glfw" ,glfw)
         ("trivial-main-thread" ,sbcl-trivial-main-thread)))
      (home-page "https://github.com/AlexCharlton/cl-glfw3")
      (synopsis "Common Lisp bindings to GLFW version 3.x")
      (description
       "This package provides a Common Lisp bindings to @code{glfw}, an OpenGL
application development library.")
      (license license:bsd-2))))

(define-public ecl-cl-glfw3
  (sbcl-package->ecl-package sbcl-cl-glfw3))

(define-public cl-glfw3
  (sbcl-package->cl-source-package sbcl-cl-glfw3))

(define-public sbcl-chirp
  (let ((commit "01c79fa41939688216d1f86d0766a687becb0654")
        (revision "1"))
    (package
      (name "sbcl-chirp")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/chirp")
               (commit commit)))
         (file-name (git-file-name "chirp" version))
         (sha256
          (base32 "10xlz1vwdv3jv48kmpndpnrg6672m0r5vsjgm2pksfl8bc05j2m0"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems '("chirp-core" "chirp-dexador" "chirp-drakma" "chirp")))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("babel" ,sbcl-babel)
         ("cl-base64" ,sbcl-cl-base64)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("dexador" ,sbcl-dexador)
         ("drakma" ,sbcl-drakma)
         ("flexi-streams" ,sbcl-flexi-streams)
         ("ironclad" ,sbcl-ironclad)
         ("local-time" ,sbcl-local-time)
         ("split-sequence" ,sbcl-split-sequence)
         ("uuid" ,sbcl-uuid)
         ("yason" ,sbcl-yason)))
      (home-page "https://shinmera.github.io/chirp/")
      (synopsis "Twitter client library for Common Lisp")
      (description
       "This package provides a Common Lisp Twitter client featuring full API
coverage.")
      (license license:zlib))))

(define-public ecl-chirp
  (sbcl-package->ecl-package sbcl-chirp))

(define-public cl-chirp
  (sbcl-package->cl-source-package sbcl-chirp))

(define-public sbcl-cepl
  (let ((commit "d1a10b6c8f4cedc07493bf06aef3a56c7b6f8d5b")
        (revision "1"))
    (package
     (name "sbcl-cepl")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cbaggers/cepl")
             (commit commit)))
       (file-name (git-file-name "cepl" version))
       (sha256
        (base32 "0izbw2advqm3wailj3dpq6zqfrfirwn14pw5qmqh8i71r51xwmm2"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      `(#:asd-files '("cepl.asd" "cepl.build.asd")))
     (inputs
      `(("alexandria" ,sbcl-alexandria)
        ("bordeaux-threads" ,sbcl-bordeaux-threads)
        ("cffi" ,sbcl-cffi)
        ("cl-opengl" ,sbcl-cl-opengl)
        ("cl-ppcre" ,sbcl-cl-ppcre)
        ("documentation-utils" ,sbcl-documentation-utils)
        ("float-features" ,sbcl-float-features)
        ("ieee-floats" ,sbcl-ieee-floats)
        ("split-sequence" ,sbcl-split-sequence)
        ("varjo" ,sbcl-varjo)))
     (propagated-inputs
      (list sbcl-quickproject))
     (home-page "https://github.com/cbaggers/cepl")
     (synopsis "Development playground to work with OpenGL")
     (description
      "CEPL (Code Evaluate Play Loop ) is a lispy and REPL-friendly Common Lisp
library for working with OpenGL.

Its definition of success is making the user feel that GPU programming has
always been part of the languages standard.

The usual approach to using CEPL is to start it at the beginning of your Lisp
session and leave it open for the duration of your work.  You can then treat the
window it creates as just another output for your graphics, analogous to how
@code{*standard-output*} is treated for text.")
     (license license:bsd-2))))

(define-public ecl-cepl
  (sbcl-package->ecl-package sbcl-cepl))

(define-public cl-cepl
  (sbcl-package->cl-source-package sbcl-cepl))

(define-public sbcl-stmx
  ;; No release for years and recent commits contain fixes for revent SBCL versions.
  (let ((commit "a7bb44082cd53ee968965adff03d4351750711a1")
        (revision "1"))
    (package
     (name "sbcl-stmx")
     (version (git-version "2.0.5" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cosmos72/stmx/")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hfmh4vj271jdilir97qs6nqbi5nmn5alyls0w3d3xxqwi6ffqjs"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      (list sbcl-alexandria sbcl-bordeaux-threads sbcl-log4cl
            sbcl-closer-mop sbcl-trivial-garbage))
     (home-page "https://stmx.org/")
     (synopsis "High performance Transactional Memory for Common Lisp")
     (description
      "STMX is a high-performance implementation of composable Transactional
Memory, which is a concurrency control mechanism aimed at making concurrent
programming easier to write and understand.  Instead of traditional lock-based
programming, one programs with atomic memory transactions, which can be
composed together to make larger atomic memory transactions.

A memory transaction gets committed if it returns normally, while it gets
rolled back if it signals an error (and the error is propagated to the
caller).

Finally, memory transactions can safely run in parallel in different threads,
are re-executed from the beginning in case of conflicts or if consistent reads
cannot be guaranteed, and their effects are not visible from other threads
until they commit.

Memory transactions give freedom from deadlocks, are immune to thread-safety
bugs and race conditions, provide automatic roll-back on failure, and aim at
resolving the tension between granularity and concurrency.")
     (license license:llgpl))))

(define-public ecl-stmx
  (sbcl-package->ecl-package sbcl-stmx))

(define-public cl-stmx
  (sbcl-package->cl-source-package sbcl-stmx))

(define-public sbcl-binding-arrows
  ;; Fork of sbcl-arrows that does not have a new tag.
  (let ((commit "d19364ec8850880ed6e42078ccaa2ed9114dc83a")
        (revision "1"))
    (package
     (name "sbcl-binding-arrows")
     (version (git-version "1.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/phoe/binding-arrows")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hqikgzic7kjq2n1d924yldfm30qz67cmsk6gghi9cbmxkwdlwp8"))))
     (build-system asdf-build-system/sbcl)
     (native-inputs
      (list sbcl-hu.dwim.stefil))
     (home-page "https://github.com/phoe/binding-arrows")
     (synopsis "Threading macros based on binding anonymous variables")
     (description
      "This system implements binding threading macros -- a kind of threading
macros with different semantics than classical, Clojure core threading macros
or their extension, swiss-arrows.  Two Common Lisp implementations of those are
@code{arrows} and @code{arrow-macros}.

This system is a fork of @code{arrows} with changes in semantics that make it
impossible to merge back upstream.")
     (license license:expat))))

(define-public ecl-binding-arrows
  (sbcl-package->ecl-package sbcl-binding-arrows))

(define-public cl-binding-arrows
  (sbcl-package->cl-source-package sbcl-binding-arrows))

(define-public sbcl-atomics
  ;; No release in years.
  (let ((commit "9ee0bdebcd2bb9b242671a75460db13fbf45454c")
        (revision "1"))
    (package
      (name "sbcl-atomics")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/atomics")
               (commit commit)))
         (file-name (git-file-name "atomics" version))
         (sha256
          (base32 "0mp5jdqq0aamdhgnvw149cqqi3zg7dkkibp25qi4rafw1fnpd40z"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-documentation-utils))
      (native-inputs
       (list sbcl-parachute))
      (home-page "https://shinmera.github.io/atomics/")
      (synopsis "Common Lisp portability layer for atomic operations")
      (description
       "This is a library for access to atomic operation primitives such as
compare-and-swap.  It aims to be a rather thin layer over what the
implementations offer.")
      (license license:zlib))))

(define-public ecl-atomics
  (sbcl-package->ecl-package sbcl-atomics))

(define-public cl-atomics
  (sbcl-package->cl-source-package sbcl-atomics))

(define-public sbcl-cl-murmurhash
  ;; No release.
  (let ((commit "5433f5e95f1cce63a81259a471150834c6a59364")
        (revision "1"))
    (package
      (name "sbcl-cl-murmurhash")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/ruricolist/cl-murmurhash/")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0251r0mpjm0y3qsm4lm7ncvrkxvgwc53spdm1p2mpayhvkkqqsws"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-babel))
      (native-inputs
       (list sbcl-fiveam))
      (synopsis "32-bit version of Murmurhash3 for Common Lisp")
      (description
       "This Common Lisp package offers an implementation of the 32-bit
variant of MurmurHash3 (@url{https://github.com/aappleby/smhasher}), a fast
non-crytographic hashing algorithm.")
      (license license:expat))))

(define-public ecl-cl-murmurhash
  (sbcl-package->ecl-package sbcl-cl-murmurhash))

(define-public cl-murmurhash
  (sbcl-package->cl-source-package sbcl-cl-murmurhash))

(define-public sbcl-cl-hamt
  ;; No release
  (let ((commit "7a99eaaca1f952029def9ad5a2b80a612a712208"))
    (package
      (name "sbcl-cl-hamt")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/danshapero/cl-hamt/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ycbd73ykfj5j9sdhlzamyv18qbjj6xqf7fhm4fa0nsyr6sr3rf5"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-murmurhash))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/danshapero/cl-hamt")
      (synopsis "Dictionary & set data structure using hash array-mapped tries")
      (description
       "This library provides purely functional dictionaries and sets in
Common Lisp based on the hash array-mapped trie data structure.")
      (license license:bsd-3))))

(define-public ecl-cl-hamt
  (sbcl-package->ecl-package sbcl-cl-hamt))

(define-public cl-hamt
  (sbcl-package->cl-source-package sbcl-cl-hamt))

(define-public sbcl-cl-gserver
  (package
    (name "sbcl-cl-gserver")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdbergmann/cl-gserver")

             ;; The "1.8.0" tag must have been moved from
             ;; c83d92a66102faa514b546029a9bd2078b95dc0f to
             ;; 90832f02f0556463e332cd055c3c1be58f323aea, 22 commits later!
             (commit "c83d92a66102faa514b546029a9bd2078b95dc0f")))
       (file-name (git-file-name "cl-gserver" version))
       (sha256
        (base32 "1bfz7z8v417dvsp1jz76ir3ihcs8g7zis2d56xx1dpzqzjd95g7z"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-alexandria
           sbcl-bordeaux-threads
           sbcl-lparallel
           sbcl-cl-speedy-queue
           sbcl-log4cl
           sbcl-cl-str
           sbcl-blackbird
           sbcl-cl-hamt
           sbcl-binding-arrows
           sbcl-atomics))
    (native-inputs
     (list sbcl-fiveam sbcl-cl-mock))
    (home-page "https://mdbergmann.github.io/cl-gserver/index.html")
    (synopsis "Actor framework for easy access to state and async operations")
    (description
     "@code{cl-gserver} is a 'message passing' library / framework with actors
similar to Erlang or Akka.  It supports creating reactive systems for parallel
computing and event based message handling.")
    (license license:agpl3)))

(define-public ecl-cl-gserver
  (sbcl-package->ecl-package sbcl-cl-gserver))

(define-public cl-gserver
  (sbcl-package->cl-source-package sbcl-cl-gserver))

(define-public sbcl-assoc-utils
  (let ((commit "74af16a3c0f10ad35e406167de02984744fc7854")
        (revision "1"))
    (package
      (name "sbcl-assoc-utils")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/assoc-utils")
               (commit commit)))
         (file-name (git-file-name "cl-assoc-utils" version))
         (sha256
          (base32 "1yac1v7zmdxj0p6rvwrrhyqvy7yjfhmqbchkwqhhr89gpjvvaick"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; All test cases pass, but tests successfully fail at the end:
       ;;
       ;; Summary:
       ;;   All 1 file passed.
       ;; Unhandled ASDF/FIND-COMPONENT:MISSING-DEPENDENCY in thread
       ;; #<SB-THREAD:THREAD "main thread" RUNNING {1001858103}>:
       ;;   Component ASSOC-UTILS-ASD::ASSOC-UTILS-TEST not found, required by
       ;;   #<SYSTEM "assoc-utils">
       `(#:tests? #f))
      (native-inputs
       (list sbcl-prove))
      (home-page "https://github.com/fukamachi/assoc-utils")
      (synopsis "Utilities for manipulating association lists in Common Lisp")
      (description
       "@code{assoc-utils} provides utilities for manipulating association
lists in Common Lisp.")
      (license license:public-domain))))

(define-public cl-assoc-utils
  (sbcl-package->cl-source-package sbcl-assoc-utils))

(define-public ecl-assoc-utils
  (sbcl-package->ecl-package sbcl-assoc-utils))

(define-public sbcl-let-over-lambda
  (let ((commit "481b2e3ab4646186451dfdd2062113203287d520")
        (revision "1"))
    (package
      (name "sbcl-let-over-lambda")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/thephoeron/let-over-lambda")
               (commit commit)))
         (file-name (git-file-name "cl-let-over-lambda" version))
         (sha256
          (base32 "114p781lwi9lrbzg27dnkymz9m4cvm1k430j7qsykwd0b58d8qbk"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; All test cases pass, but tests successfully fail at the end:
       ;;
       ;; Summary:
       ;; All 1 file passed.
       ;; Unhandled ASDF/FIND-COMPONENT:MISSING-DEPENDENCY in thread
       ;; #<SB-THREAD:THREAD "main thread" RUNNING {1001860103}>:
       ;;   Component LET-OVER-LAMBDA-ASD::LET-OVER-LAMBDA-TEST not found,
       ;;   required by #<SYSTEM "let-over-lambda">
       `(#:tests? #f))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("named-readtables" ,sbcl-named-readtables)))
      (native-inputs
       (list sbcl-prove))
      (home-page "https://github.com/thephoeron/let-over-lambda")
      (synopsis "Macros from Let Over Lambda")
      (description
       "This package provides Doug Hoyte's \"Production\" version of macros
from the Let Over Lambda book, including community updates.")
      (license license:bsd-3))))

(define-public cl-let-over-lambda
  (sbcl-package->cl-source-package sbcl-let-over-lambda))

(define-public ecl-let-over-lambda
  (sbcl-package->ecl-package sbcl-let-over-lambda))

(define-public sbcl-flute
  (let ((commit "90ebcd6e82f637f49b6de7d625ccc51ec4c92900")
        (revision "1"))
    (package
      (name "sbcl-flute")
      (version (git-version "0.2-dev" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ailisp/flute")
               (commit commit)))
         (file-name (git-file-name "cl-flute" version))
         (sha256
          (base32 "0q8jhp040cvpppyn820mm6a550yfxyr1lar298x13c42mm807f4f"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-assoc-utils sbcl-let-over-lambda))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/ailisp/flute")
      (synopsis "HTML5 generation library in Common Lisp")
      (description
       "Flute is an easily composable HTML5 generation library in Common
Lisp.")
      (license license:expat))))

(define-public cl-flute
  (sbcl-package->cl-source-package sbcl-flute))

(define-public ecl-flute
  (sbcl-package->ecl-package sbcl-flute))

(define-public sbcl-cl-posix-mqueue
  (let ((commit "8977370c7206d1f62bd1be80f4254af40654b83f")
        (revision "1"))
    (package
      (name "sbcl-cl-posix-mqueue")
      (version (git-version "0.1.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/xFA25E/cl-posix-mqueue")
               (commit commit)))
         (file-name (git-file-name "cl-posix-mqueue" version))
         (sha256
          (base32 "04519rg8vc782z097dzlb8nx0skab2fy2zd0m60r6mz2nw8xdvh6"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:test-asd-file "cl-posix-mqueue-tests.asd"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-librt-path
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/spec.lisp"
                 (("librt.so" all)
                  (string-append (assoc-ref inputs "glibc") "/lib/" all))))))))
      (native-inputs
       `(("cl-ppcre" ,sbcl-cl-ppcre)
         ("rove" ,sbcl-rove)))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("babel" ,sbcl-babel)
         ("cffi" ,sbcl-cffi)
         ("glibc" ,glibc)
         ("local-time" ,sbcl-local-time)))
      (home-page "https://github.com/xFA25E/cl-posix-mqueue")
      (synopsis "Common Lisp binding to POSIX mqueue")
      (description
       "This package provides Common Lisp bindings to POSIX message queue, an
@acronym{IPC, Inter-Process Communication} method that is easy to use and quick
to setup.")
      (license license:gpl3))))

(define-public ecl-cl-posix-mqueue
  (sbcl-package->ecl-package sbcl-cl-posix-mqueue))

(define-public cl-posix-mqueue
  (sbcl-package->cl-source-package sbcl-cl-posix-mqueue))

(define-public sbcl-sdl2
  (let ((commit "bb2aa2a41cf799e3bb1ddf50de41fe389c6db668")
        (revision "1"))
    (package
      (name "sbcl-sdl2")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lispgames/cl-sdl2")
               (commit commit)))
         (file-name (git-file-name "cl-sdl2" version))
         (sha256
          (base32 "1a4904310z2wwq80grnlixmyz30452vgd4lh74y105j2yrr43z97"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/library.lisp"
                 (("libSDL2-2.0.so.0" all)
                  (string-append (assoc-ref inputs "libsdl2") "/lib/" all)))
               #t)))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-autowrap" ,sbcl-cl-autowrap)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("libsdl2" ,sdl2)
         ("trivial-channels" ,sbcl-trivial-channels)
         ("trivial-features" ,sbcl-trivial-features)))
      (home-page "https://github.com/lispgames/cl-sdl2")
      (synopsis "Common Lisp bindings for SDL2 using C2FFI")
      (description
       "This package provides a Common Lisp wrapper system for the SDL 2.0
C Library.")
      (license license:expat))))

(define-public ecl-sdl2
  (sbcl-package->ecl-package sbcl-sdl2))

(define-public cl-sdl2
  (sbcl-package->cl-source-package sbcl-sdl2))

(define-public sbcl-cl-gamepad
  (let ((commit "7e12137927b42db064ffbf9ea34bd4790ad4bb33")
        (revision "1"))
    (package
      (name "sbcl-cl-gamepad")
      (version (git-version "3.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shirakumo/cl-gamepad")
               (commit commit)))
         (file-name (git-file-name "cl-gamepad" version))
         (sha256
          (base32 "1gzx590i7s81qmramnjvfzrrq5yppas8yxqq1jl3yzqhhjwjfvkd"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-evdev-lib-path
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "evdev-cffi.lisp"
                 (("libevdev.so" all)
                  (string-append (assoc-ref inputs "libevdev")
                                 "/lib/" all)))))
           ;; Here we use a custom build phase to work around a compilation bug.
           ;; Using 'asdf:compile-system' fails, but using 'asdf:load-system'
           ;; succeeds (and also compiles the system).
           ;; See https://github.com/Shirakumo/cl-gamepad/issues/8
           (replace 'build
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (source-path (string-append out
                                                  "/share/common-lisp/"
                                                  (%lisp-type)))
                      (translations `((,source-path
                                       :**/ :*.*.*)
                                      (,(string-append out
                                                       "/lib/common-lisp/"
                                                       (%lisp-type))
                                       :**/ :*.*.*))))
                 (setenv "ASDF_OUTPUT_TRANSLATIONS"
                         (format #f "~S" `(:output-translations
                                           ,translations
                                           :inherit-configuration)))
                 (setenv "HOME" (assoc-ref outputs "out"))
                 (with-directory-excursion (string-append source-path
                                                          "/cl-gamepad")
                   (invoke (%lisp-type)
                           "--eval" "(require :asdf)"
                           "--eval" "(asdf:load-asd (truename \"cl-gamepad.asd\"))"
                           "--eval" "(asdf:load-system :cl-gamepad)"
                           "--eval" "(quit)"))))))))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("documentation-utils" ,sbcl-documentation-utils)
         ("libevdev" ,libevdev)
         ("trivial-features" ,sbcl-trivial-features)))
      (home-page "https://shirakumo.github.io/cl-gamepad/")
      (synopsis "Library for access to gamepads and joystick input devices")
      (description
       "This is a library to provide cross-platform access to gamepads,
joysticks, and other such HID devices.")
      (license license:zlib))))

(define-public ecl-cl-gamepad
  (sbcl-package->ecl-package sbcl-cl-gamepad))

(define-public cl-gamepad
  (sbcl-package->cl-source-package sbcl-cl-gamepad))

(define-public sbcl-trial
  (let ((commit "ba178cac3a5528c570c7e8dad66c58cc770db53a")
        (revision "1"))
    (package
      (name "sbcl-trial")
      (version (git-version "1.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shirakumo/trial")
               (commit commit)))
         (file-name (git-file-name "trial" version))
         (sha256
          (base32 "1vpv9nrpq93fz1c5cyi1hazaaz9ijbrf1l7zwp7gammndr5v028r"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-trivial-features))
      (inputs
       (list sbcl-alexandria
             sbcl-3d-matrices
             sbcl-3d-vectors
             sbcl-bordeaux-threads
             sbcl-cl-gamepad
             sbcl-cl-jpeg
             sbcl-cl-opengl
             sbcl-cl-ppcre
             sbcl-cl-tga
             sbcl-closer-mop
             sbcl-deploy
             sbcl-fast-io
             sbcl-flare
             sbcl-float-features
             sbcl-flow
             sbcl-for
             sbcl-form-fiddle
             sbcl-glsl-toolkit
             sbcl-ieee-floats
             sbcl-jsown
             sbcl-lambda-fiddle
             sbcl-lquery
             sbcl-messagebox
             sbcl-mmap
             sbcl-pathname-utils
             sbcl-pngload
             sbcl-retrospectiff
             sbcl-static-vectors
             sbcl-terrable
             sbcl-trivial-garbage
             sbcl-trivial-indent
             sbcl-verbose
             sbcl-zpng))
      (home-page "https://github.com/Shirakumo/trial")
      (synopsis "Common Lisp game engine")
      (description
       "Trial is a game engine written in Common Lisp.  Unlike many other
engines, it is meant to be more of a loose connection of components that can be
fit together as required by any particular game.")
      (license license:zlib))))

(define-public ecl-trial
  (sbcl-package->ecl-package sbcl-trial))

(define-public cl-trial
  (sbcl-package->cl-source-package sbcl-trial))

(define-public sbcl-cl-liballegro
  (let ((commit "49f632ce97fc4f835bf5d450588793234b980a64")
        (revision "1"))
    (package
      (name "sbcl-cl-liballegro")
      (version (git-version "0.2.15" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/resttime/cl-liballegro")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0brbr7i342s0gadlnzd3a61w2b9ihhx60l19ararnc2asvyhmz7x"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-allegro-lib-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((allegro-lib-path (string-append
                                        (assoc-ref inputs "allegro") "/lib/")))
                 (substitute* "src/library.lisp"
                   (("lib \".so\"" all)
                    (string-append "\"" allegro-lib-path "\"" " lib \".so\"")))))))))
      (inputs
       `(("allegro" ,allegro)
         ("cffi" ,sbcl-cffi)
         ("float-features" ,sbcl-float-features)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("trivial-main-thread" ,sbcl-trivial-main-thread)))
      (home-page "https://github.com/resttime/cl-liballegro")
      (synopsis "Allegro 5 game programming library bindings for Common Lisp")
      (description
       "This package provides CFFI bindings and interface to Allegro 5 game
developing library for Common Lisp.")
      (license license:zlib))))

(define-public ecl-cl-liballegro
  (sbcl-package->ecl-package sbcl-cl-liballegro))

(define-public cl-liballegro
  (sbcl-package->cl-source-package sbcl-cl-liballegro))

(define-public sbcl-alloy
  (let ((commit "e86e22c2887836ec31cd97e039f0bca5248d8f1c")
        (revision "1"))
    (package
      (name "sbcl-alloy")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shirakumo/alloy")
               (commit commit)))
         (file-name (git-file-name "alloy" version))
         (sha256
          (base32 "1jsqjr6sf86hcdvnjp4gd10qv0r7kfkr9hmda85irb5lha4q9n7w"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-alexandria sbcl-parachute))
      (inputs
       (list sbcl-array-utils sbcl-closer-mop sbcl-documentation-utils))
      (home-page "https://shirakumo.github.io/alloy/")
      (synopsis
       "Common Lisp user interface protocol and toolkit implementation")
      (description
       "Alloy is a user interface toolkit.  It is defined through a set of
protocols that allow for a clear interface, as well as a standardised way to
integrate Alloy into a target backend.")
      (license license:zlib))))

(define-public ecl-alloy
  (sbcl-package->ecl-package sbcl-alloy))

(define-public cl-alloy
  (sbcl-package->cl-source-package sbcl-alloy))

(define-public sbcl-org-sampler
  (let ((commit "ee135a417750e5b1d810bb9574eb85223cb3038a")
        (revision "1"))
    (package
     (name "sbcl-org-sampler")
     (version (git-version "0.2.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jphmrst/cl-org-sampler")
             (commit commit)))
       (file-name (git-file-name "cl-org-sampler" version))
       (sha256
        (base32 "1dg029in14928qfxvfshyxmdwhzskzhxx3na0zy98ybx69b21qla"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      (list sbcl-iterate))
     (home-page "https://github.com/jphmrst/cl-org-sampler")
     (synopsis "Extracting Common Lisp docstrings as Emacs Org-mode documents")
     (description
      "ORG-SAMPLER allows using Lisp docstrings and reflection to make org-mode
text for inclusion into a larger document.")
     (license license:llgpl))))

(define-public ecl-org-sampler
  (sbcl-package->ecl-package sbcl-org-sampler))

(define-public cl-org-sampler
  (sbcl-package->cl-source-package sbcl-org-sampler))

(define-public sbcl-acl-compat
  ;; There does not seem to be proper releases.
  (let ((commit "cac1d6920998ddcbee8310a873414732e707d8e5"))
    (package
      (name "sbcl-acl-compat")
      (version (git-version "0.1.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://git.code.sf.net/p/portableaserve/git")
               (commit commit)))
         (file-name (git-file-name "acl-compat" version))
         (sha256
          (base32 "0ak6mqp84sjr0a7h5svr16vra4bf4fcx6wpir0n88dc1vjwy5xqa"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'cd-acl-compat
             (lambda _
               (chdir "acl-compat")
               #t)))))
      (inputs
       `(("puri" ,sbcl-puri)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("ironclad" ,sbcl-ironclad)
         ("cl-fad" ,sbcl-cl-fad)))
      (home-page "https://sourceforge.net/projects/portableaserve/")
      (synopsis "AllegroServe, a web server written in Common Lisp")
      (description
       "The server part of AllegroServe can be used either as a standalone web
server or a module loaded into an application to provide a user interface to
the application.  AllegroServe's proxy ability allows it to run on the gateway
machine between some internal network and the Internet.  AllegroServe's client
functions allow Lisp programs to explore the web.")
      (license license:llgpl))))

(define-public cl-acl-compat
  (sbcl-package->cl-source-package sbcl-acl-compat))

(define-public sbcl-aserve
  ;; There does not seem to be proper releases.
  (let ((commit "cac1d6920998ddcbee8310a873414732e707d8e5")
        (revision "2"))
    (package
      (name "sbcl-aserve")
      (version (git-version "1.2.50" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               ;; https://github.com/franzinc/aserve/ seems to be incompatible
               ;; with SBCL, etc.
               (url "git://git.code.sf.net/p/portableaserve/git")
               (commit commit)))
         (file-name (git-file-name "aserve" version))
         (sha256
          (base32 "0ak6mqp84sjr0a7h5svr16vra4bf4fcx6wpir0n88dc1vjwy5xqa"))
         (patches (search-patches
                   ;; Add HTML5 elements to htmlgen.
                   ;; Adapted from https://github.com/franzinc/aserve/ commits:
                   ;; * e47bd763: "rfe12668: add HTML 5 elements to htmlgen"
                   ;; * 7371ce59: "fix bugs in rfe12668 implementation"
                   "sbcl-aserve-add-HTML-5-elements.patch"
                   "sbcl-aserve-fix-rfe12668.patch"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'cd-aserve
             (lambda _
               (chdir "aserve")
               #t))
           (add-after 'cd-aserve 'fix-asd
             (lambda _
               (substitute* "aserve.asd"
                 ((" :force t") ""))
               #t))
           (add-after 'cd-aserve 'fix-tests
             (lambda _
               (substitute* "test/t-aserve.cl"
                 (("\\(asdf:oos 'asdf:load-op :ptester\\)") ""))
               #t)))))
      (inputs
       `(("acl-compat" ,sbcl-acl-compat)))
      (home-page
       "https://franz.com/support/documentation/current/doc/aserve/aserve.html")
      (synopsis "AllegroServe, a web server written in Common Lisp")
      (description
       "The server part of AllegroServe can be used either as a standalone web
server or a module loaded into an application to provide a user interface to
the application.  AllegroServe's proxy ability allows it to run on the gateway
machine between some internal network and the Internet.  AllegroServe's client
functions allow Lisp programs to explore the web.")
      (license license:llgpl))))

(define-public cl-aserve
  (sbcl-package->cl-source-package sbcl-aserve))

(define-public sbcl-yxorp
  (let ((commit "c306898a467995e123a22316c9b79fcac442415b")
        (revision "4"))
    (package
      (name "sbcl-yxorp")
      (version (git-version "0.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/charJe/cl-yxorp")
               (commit commit)))
         (file-name (git-file-name "cl-yxorp" version))
         (sha256
          (base32 "0ll1s9w29yhhgqssgiw58fcapw4n040gkvpz4sxwv3q2v60rbidj"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-chipz
             sbcl-chunga
             sbcl-cl+ssl
             sbcl-binding-arrows
             sbcl-cl-str
             sbcl-usocket
             sbcl-flexi-streams
             sbcl-rutils
             sbcl-salza2
             sbcl-trivial-garbage))
      (home-page "https://github.com/charje/cl-yxorp")
      (synopsis
       "Reverse proxy server written in and configurable in Common Lisp")
      (description
       "This is a reverse proxy server written in and configurable in
Common Lisp.  It supports WebSocket, HTTP, HTTPS, HTTP to HTTPS
redirecting, port and host forwarding configuration using a real programming
language, HTTP header and body manipulation (also using a real programming
language).")
      (license license:agpl3))))

(define-public ecl-yxorp
  ;; Note that due to a bug in ECL this package does not build.
  ;; The bug has already been fixed on the development branch,
  ;; so this package will work work in the version after 21.2.1.
  (sbcl-package->ecl-package sbcl-yxorp))

(define-public cl-yxorp
  (sbcl-package->cl-source-package sbcl-yxorp))

(define-public cl-yxorp-cli
  (package
    (inherit sbcl-yxorp)
    (name "cl-yxorp-cli")
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:strip-binaries? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'set-home
           (lambda _
             (setenv "HOME" "/tmp")))
         (replace 'build
           (lambda _
             (invoke
              "sbcl" "--noinform"
              "--non-interactive"
              "--no-userinit"
              "--eval" "(require :asdf)"
              "--eval" "(pushnew (uiop:getcwd) asdf:*central-registry*)"
              "--load" "build.lisp")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (install-file "cl-yxorp" bin)))))))
    (inputs (cons (list "sbcl" sbcl) (package-inputs sbcl-yxorp)))))

(define-public sbcl-rss
  ;; No release.
  (let ((commit "51d0145e91b86327ae5c36364f9c3048052e7a58"))
    (package
      (name "sbcl-rss")
      (version (git-version "0.9.1.1" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/cl-rss.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0wv3j13fj73gigriw5r9vi920hz05ld7zllsvbxdxvmyfy9k1kly"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-aserve sbcl-kmrcl sbcl-xmls))
      (home-page "https://github.com/nsrahmad/cl-rss")
      (synopsis "Common Lisp RSS processor")
      (description
       "This package provides a Common Lisp library for fetching and parsing
RSS feeds data via HTTP.  Currently, it supports RSS versions 0.90,
0.91, and 0.92 as well as RSS version 2.")
      (license license:bsd-3))))

(define-public cl-rss
  (sbcl-package->cl-source-package sbcl-rss))

(define-public sbcl-trivial-with-current-source-form
  (let ((commit "9e343e043a77a5478c1f77bb626db22335fbbfb8")
        (revision "1"))
    (package
      (name "sbcl-trivial-with-current-source-form")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/scymtym/trivial-with-current-source-form")
               (commit commit)))
         (file-name (git-file-name "trivial-with-current-source-form" version))
         (sha256
          (base32 "15zs7mc422ycp1cvcxmirif1dq15mlmv8vzd6l6nzn4qgmph9wz0"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria))
      (home-page "https://github.com/scymtym/trivial-with-current-source-form")
      (synopsis "Help producing better errors for macro users")
      (description
       "This library allows macro writers to provide better feedback to macro
users when errors are signaled during macroexpansion.  It uses the compiler's
concept of a source-form to report where the error or warning is located.")
      (license license:lgpl3))))

(define-public ecl-trivial-with-current-source-form
  ;; Trivial-with-current-source-form does not give any benefits on ECL.
  ;; This package is so packages dependent on trivial-with-current-source-form
  ;; can be loaded on ECL.
  (sbcl-package->ecl-package sbcl-trivial-with-current-source-form))

(define-public cl-trivial-with-current-source-form
  (sbcl-package->cl-source-package sbcl-trivial-with-current-source-form))

(define-public sbcl-tailrec
  (let ((commit "6f882846d8f5bca9138df26510862e64bb15d92f")
        (revision "2"))
    (package
      (name "sbcl-tailrec")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/charje/tailrec")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1h8m2npdzd2cpnl75pvv4yvvfwxa7kl6qvalc9s0y4yws0kaih3i"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-trivial-macroexpand-all
             sbcl-trivial-with-current-source-form))
      (home-page "https://github.com/charje/tailrec")
      (synopsis "Macro to optimize a Common Lisp function for tail recursion")
      (description "Just wrap your Common Lisp function in this macro call and
it will be optimized for tail recursion.  You will be warned if the function
is not tail recursive.")
      (license license:llgpl))))

(define-public ecl-tailrec
  (sbcl-package->ecl-package sbcl-tailrec))

(define-public cl-tailrec
  (sbcl-package->cl-source-package sbcl-tailrec))

(define-public sbcl-issr-core
  (let ((commit "64e3b07a63a7ca3ad70ba42474f98ac4513580aa")
        (revision "1"))
    (package
      (name "sbcl-issr-core")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/interactive-ssr/core")
               (commit commit)))
         (file-name (git-file-name "issr-core" version))
         (sha256
          (base32 "1bajb09crzadkirdpd6jrpcc55irjd4sxzavygr25l85pafyhniw"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-str sbcl-global-vars sbcl-plump sbcl-tailrec))
      (home-page
       "https://github.com/interactive-ssr/client/blob/master/main.org")
      (synopsis "The core functionality for ISSR server modules")
      (description
       "ISSR core provides functions and variables for ISSR server modules so
that different servers can behave similarly.  The most important features are
Document Object Model differencing to generate instructions to update a DOM,
and DOM cleaning, to ensure that all remote DOMs are the same.")
      (license license:llgpl))))

(define-public ecl-issr-core
  (sbcl-package->ecl-package sbcl-issr-core))

(define-public cl-issr-core
  (sbcl-package->cl-source-package sbcl-issr-core))

(define-public sbcl-portal
  (let ((commit "416589fa04cb239971422a1272acba236c8333be")
        (revision "2"))
    (package
      (name "sbcl-portal")
      (version (git-version "1.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/charJe/portal")
               (commit commit)))
         (file-name (git-file-name "portal" version))
         (sha256
          (base32 "1012jc068qdd8df6mmbn8vmmqlniqm5j2jbyrraw3yz8c13c8280"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria
             sbcl-arrows
             sbcl-cl-base64
             sbcl-cl-str
             sbcl-flexi-streams
             sbcl-global-vars
             sbcl-ironclad
             sbcl-parse-float
             sbcl-usocket))
      (home-page "https://github.com/charJe/portal")
      (synopsis "Portable Websocket Server for Common Lisp")
      (description
       "This is a websocket server for Common Lisp using usockets to be
portable between implementations and operating systems.  It has a programming
interface that allows for multiple websocket apps per server using Common Lisp
keywords for different websocket events.  It has useful restarts and
customizable errors.")
      (license license:llgpl))))

(define-public ecl-portal
  (sbcl-package->ecl-package sbcl-portal))

(define-public cl-portal
  (sbcl-package->cl-source-package sbcl-portal))

(define-public sbcl-hunchenissr
  (let ((commit "7df702f2e110999a2f31c7ebad81bfc39ac06670")
        (revision "1"))
    (package
      (name "sbcl-hunchenissr")
      (version (git-version "1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/interactive-ssr/hunchenissr")
               (commit commit)))
         (file-name (git-file-name "hunchenissr" version))
         (sha256
          (base32 "0826qrvk64pjspdklns29dv3zhzfhd6k42fq030xajv8a7hkcxda"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-base64
             sbcl-cl-str
             sbcl-hunchentoot
             sbcl-issr-core
             sbcl-jonathan
             sbcl-plump
             sbcl-portal))
      (home-page "https://github.com/interactive-ssr/hunchenissr")
      (synopsis "Interactive Server Side Rendering backend for Hunchentoot")
      (description
       "Hunchenissr works together with issr.js for the development of
interactive (changing without page refreshes) websites making use of websocket
and Common Lisp server HTML generation instead of mountains of convoluted
Javascript.")
      (license license:llgpl))))

(define-public ecl-hunchenissr
  (sbcl-package->ecl-package sbcl-hunchenissr))

(define-public cl-hunchenissr
  (sbcl-package->cl-source-package sbcl-hunchenissr))

(define-public sbcl-hunchenissr-routes
  (let ((commit "2e831975dc2a6c030f1b518747cf429be8484b31")
        (revision "1"))
    (package
      (name "sbcl-hunchenissr-routes")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/interactive-ssr/hunchenissr-routes")
               (commit commit)))
         (file-name (git-file-name "hunchenissr-routes" version))
         (sha256
          (base32 "1xyqacihxwk4vnffqlg93czmalscglp6sh3bwy3qwb7hdxv6yxz6"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-cl-ppcre sbcl-cl-unification
             sbcl-hunchenissr))
      (home-page "https://github.com/interactive-ssr/hunchenissr-routes")
      (synopsis "Enable path variables when using Hunchenissr")
      (description
       "This library enables path variables in networking routes when using
Hunchenissr for Common Lisp.  If a part of the path (between two slashes)
starts with a question mark (?), that symbol (without question mark) will be
bound to whatever value was in the same place in the URL (as a string).")
      (license license:llgpl))))

(define-public ecl-hunchenissr-routes
  (sbcl-package->ecl-package sbcl-hunchenissr-routes))

(define-public cl-hunchenissr-routes
  (sbcl-package->cl-source-package sbcl-hunchenissr-routes))

(define-public sbcl-genhash
  (let ((commit "220ae1af8361dbd2005177f2ee11072b6a33934f")
        (revision "1"))
    (package
      (name "sbcl-genhash")
      (version (git-version "1.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pnathan/genhash")
               (commit commit)))
         (file-name (git-file-name "cl-genhash" version))
         (sha256
          (base32 "1jnk1fix1zydhy0kn3cvlp6dy0241x7v8ahq001nlr6v152z1cwk"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/pnathan/genhash")
      (synopsis "Generic hash tables for Common Lisp")
      (description
       "This an implementation of CDR 2: generic hash tables for Common Lisp")
      (license license:public-domain))))

(define-public cl-genhash
  (sbcl-package->cl-source-package sbcl-genhash))

(define-public ecl-genhash
  (sbcl-package->ecl-package sbcl-genhash))

(define-public sbcl-spinneret
  ;; No release since 2019, no tags.
  (let ((commit "52709ab953c46b24cbc2f0e3a50ae362916e730c"))
    (package
      (name "sbcl-spinneret")
      (version (git-version "3.0" "5" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ruricolist/spinneret/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1wzs0hzlwf0vzk4gb66psqz6gqcf3x7yfpi9gghbil97iz6fyc7z"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria
             sbcl-global-vars
             sbcl-parenscript
             sbcl-cl-markdown
             sbcl-cl-ppcre
             sbcl-serapeum
             sbcl-trivial-gray-streams))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/ruricolist/spinneret")
      (synopsis "Common Lisp HTML5 generator")
      (description
       "In the crowded space of Common Lisp HTML generators, Spinneret
occupies the following coordinates:

@itemize

@item Modern.  Targets HTML5.  Does not treat XML and HTML as the same
problem.  Assumes you will be serving your documents as UTF-8.

@item Composable.  Makes it easy to refactor HTML generation into separate
functions and macros.

@item Pretty.  Treats HTML as a document format, not a serialization.  Output
is idiomatic and readable, following the coding style of the HTML5
specification.

@item Aggressive.  If something can be interpreted as HTML, then it will be,
meaning that some Lisp forms can't be mixed with HTML syntax.  In the
trade-off between 90% convenience and 10% correctness Spinneret is on the side
of convenience.

@item Bilingual.  Spinneret (after loading @code{spinneret/ps}) has the same
semantics in Lisp and Parenscript.

@end itemize\n")
      (license license:expat))))

(define-public ecl-spinneret
  (sbcl-package->ecl-package sbcl-spinneret))

(define-public cl-spinneret
  (sbcl-package->cl-source-package sbcl-spinneret))

(define-public sbcl-path-parse
  (let ((commit "86183f3752374435f8933394b4c5d8e75a37a113")
        (revision "1"))
    (package
      (name "sbcl-path-parse")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eudoxia0/path-parse")
               (commit commit)))
         (file-name (git-file-name "cl-path-parse" version))
         (sha256
          (base32 "10mxm6q62cfpv3hw2w8k968ba8a1xglqdkwlkqs4l4nby3b11aaq"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-split-sequence))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/eudoxia0/path-parse")
      (synopsis "Parse the PATH environment variable in Common Lisp")
      (description
       "This package provides a function to parse the @code{PATH} environment
variable portably in Common Lisp.")
      (license license:expat))))

(define-public cl-path-parse
  (sbcl-package->cl-source-package sbcl-path-parse))

(define-public ecl-path-parse
  (sbcl-package->ecl-package sbcl-path-parse))

(define-public sbcl-cl-libxml2
  (let ((commit "8d03110c532c1a3fe15503fdfefe82f60669e4bd"))
    (package
      (name "sbcl-cl-libxml2")
      (version (git-version "0.3.4" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/archimag/cl-libxml2")
               (commit commit)))
         (file-name (git-file-name "cl-libxml2" version))
         (sha256
          (base32 "09049c13cfp5sc6x9lrw762jd7a9qkfq5jgngqgrzn4kn9qscarw"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("flexi-streams" ,sbcl-flexi-streams)
         ("garbage-pools" ,sbcl-garbage-pools)
         ("iterate" ,sbcl-iterate)
         ("metabang-bind" ,sbcl-metabang-bind)
         ("puri" ,sbcl-puri)
         ;; Non-Lisp inputs:
         ("libxml2" ,libxml2)
         ("libxslt" ,libxslt)))
      (native-inputs
       (list sbcl-lift))
      (arguments
       `(#:tests? #f ; FIXME: Tests get stuck indefinitly
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* "tree/xtree.lisp"
                 (("libxml2.so.2")
                  (string-append (assoc-ref inputs "libxml2") "/lib/libxml2.so")))
               (let ((libxslt (assoc-ref inputs "libxslt")))
                 (substitute* "xslt/xslt.lisp"
                   (("libxslt.so.1")
                    (string-append libxslt "/lib/libxslt.so"))
                   (("libexslt.so.0")
                    (string-append libxslt "/lib/libexslt.so"))
                   (("cllibxml2.so")
                    (string-append (assoc-ref outputs "out") "/lib/cllibxml2.so"))))
               #t))
           (add-before 'build 'build-helper-library
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((prefix-dir (string-append (assoc-ref outputs "out"))))
                 (mkdir-p (string-append prefix-dir "/lib"))
                 (invoke "make" "-C" "foreign" "install"
                         "INSOPTS="
                         (string-append "PREFIX=" prefix-dir))
                 #t)))
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* '("cl-libxml2.asd" "cl-libxslt.asd" "xfactory.asd")
                 ((" :force t") ""))
               #t)))))
      (home-page "https://web.archive.org/web/20160121073421/http://cl-libxml2.googlecode.com/svn/doc/index.html")
      (synopsis "High-level wrapper around libxml2 and libxslt libraries")
      (description
       "cl-libxml2 is high-level Common Lisp wrapper around the @code{libxml2}
and @code{libxslt} libraries.

@itemize
@item Interfaces for tree manipulation (like @code{cxml-stp}).
@item Interface for HTML 4.0 non-validating parsers.
@item Specific APIs to process HTML trees, especially serialization.
@item XPath API.
@item XSLT API.
@item Custom URL resolvers.
@item XPath extension functions.
@item XSLT extension elements.
@item Translates @code{libxml2} and @code{libxslt} errors to Lisp conditions.
@item Extends the Common Lisp @code{iterate} library with custom drivers for
child nodes enumeration, etc.
@item The @code{XFACTORY} system provides a simple and compact syntax for XML generation.
@end itemize\n")
      (license license:llgpl))))

(define-public ecl-cl-libxml2
  (sbcl-package->ecl-package sbcl-cl-libxml2))

(define-public cl-libxml2
  (sbcl-package->cl-source-package sbcl-cl-libxml2))

(define-public sbcl-pileup
  (let ((commit "f269473a570a8e55881082545ee63cfe5c7d3e72")
        (revision "1"))
    (package
      (name "sbcl-pileup")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nikodemus/pileup")
               (commit commit)))
         (file-name (git-file-name "cl-pileup" version))
         (sha256
          (base32 "01gvshpxil0ggjgfmgcymbgmpsfaxy6aggm0bywkn40rck3038vb"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-bordeaux-threads))
      (native-inputs
       (list sbcl-hu.dwim.stefil))
      (home-page "https://github.com/nikodemus/pileup")
      (synopsis "Simple thread-safe binary heap implementation for Common Lisp")
      (description
       "@code{Pileup} is a portable, performant, and thread-safe binary heap
for Common Lisp.")
      (license license:expat))))

(define-public cl-pileup
  (sbcl-package->cl-source-package sbcl-pileup))

(define-public ecl-pileup
  (sbcl-package->ecl-package sbcl-pileup))

(define-public sbcl-feeder
  ;; No release.
  (let ((commit "b05f517d7729564575cc809e086c262646a94d34")
        (revision "1"))
    (package
      (name "sbcl-feeder")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/feeder")
               (commit commit)))
         (file-name (git-file-name "feeder" version))
         (sha256
          (base32 "1dpbzhycg50snl3j01c8dh8gdvhfhz0hnfl54xy55a3wbr3m6rp7"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-documentation-utils sbcl-local-time sbcl-plump))
      (home-page "https://shinmera.github.io/feeder/")
      (synopsis "RSS, Atom and general feed parsing and generating")
      (description
       "Feeder is a syndication feed library.  It presents a general protocol
for representation of feed items, as well as a framework to translate these
objects from and to external formats.  It also implements the RSS 2.0 and Atom
formats within this framework.")
      (license license:zlib))))

(define-public ecl-feeder
  (sbcl-package->ecl-package sbcl-feeder))

(define-public cl-feeder
  (sbcl-package->cl-source-package sbcl-feeder))

(define-public sbcl-routes
  (let ((commit "1b79e85aa653e1ec87e21ca745abe51547866fa9")
        (revision "1"))
    (package
      (name "sbcl-routes")
      (version (git-version "0.2.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/archimag/cl-routes")
               (commit commit)))
         (file-name (git-file-name "cl-routes" version))
         (sha256
          (base32 "1zpk3cp2v8hm50ppjl10yxr437vv4552r8hylvizglzrq2ibsbr1"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-tests
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Fix: :FORCE and :FORCE-NOT arguments not allowed in
               ;; a nested call to ASDF/OPERATE:OPERATE unless
               ;; identically to toplevel
               (substitute* "routes.asd"
                 ((" :force t") "")))))))
      (inputs
       `(("iterate" ,sbcl-iterate)
         ("puri" ,sbcl-puri)
         ("split-sequence" ,sbcl-split-sequence)))
      (native-inputs
       (list sbcl-lift))
      (home-page "https://github.com/archimag/cl-routes")
      (synopsis "Rails routes system for Common Lisp")
      (description
       "This is a a Common Lisp re-implementation of the Rails routes system
for mapping URLs.")
      (license license:llgpl))))

(define-public cl-routes
  (sbcl-package->cl-source-package sbcl-routes))

(define-public ecl-routes
  (sbcl-package->ecl-package sbcl-routes))

(define-public sbcl-terminfo
  (let ((commit "b8b2e3ed786bfcf9f1aa4a264cee2e93135080f5")
        (revision "1"))
    (package
      (name "sbcl-terminfo")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/npatrick04/terminfo")
               (commit commit)))
         (file-name (git-file-name "terminfo" version))
         (sha256
          (base32 "1nmin9rr6f75xdhxysba66xa1dh62fh27w9ad1cvmj0062armf6b"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Terminfo database front end in Common Lisp")
      (home-page "https://github.com/npatrick04/terminfo")
      (description
        "This is a terminfo database front end in Common Lisp.  The package
provides a method for determining which capabilities a terminal
(e.g. \"xterm\") has and methods to compile or put commands to a stream.")
      (license license:expat))))

(define-public cl-terminfo
  (sbcl-package->cl-source-package sbcl-terminfo))

(define-public ecl-terminfo
  (sbcl-package->ecl-package sbcl-terminfo))

(define-public sbcl-conium
  (let ((commit "089adfd8759ec7973bb6f67b98d7a246e67aeb05")
        (revision "1"))
    (package
      (name "sbcl-conium")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/conium")
               (commit commit)))
         (file-name (git-file-name "conium" version))
         (sha256
          (base32 "0y31za8xr8734p2pf8mrw1jd1fksh2d4y1p12wwjyn8hxxsvsx1w"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-closer-mop))
      (home-page "https://github.com/sharplispers/conium")
      (synopsis "Portability library for debugger- and compiler-related tasks")
      (description
       "Conium is a portability library for debugger- and compiler-related
tasks in Common Lisp.  It is fork of SWANK-BACKEND.")
      (license license:public-domain))))

(define-public cl-conium
  (sbcl-package->cl-source-package sbcl-conium))

(define-public ecl-conium
  (sbcl-package->ecl-package sbcl-conium))

(define-public sbcl-terminal-size
  (let ((commit "e0b3d56a9dd3366baf2a05d84381da5747a2ef4a")
        (revision "1"))
    (package
      (name "sbcl-terminal-size")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eudoxia0/terminal-size")
               (commit commit)))
         (file-name (git-file-name "cl-terminal-size" version))
         (sha256
          (base32 "1212wbadms9jzrqgarpj3d9xh9w4dab8jhx4k2aryqgf116zs42h"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cffi sbcl-osicat))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/eudoxia0/terminal-size")
      (synopsis "Get the size of the terminal from Common Lisp")
      (description
       "This package provides the @code{terminal-size:size} function to get the
size of the terminal from Common Lisp.")
      (license license:expat))))

(define-public cl-terminal-size
  (sbcl-package->cl-source-package sbcl-terminal-size))

(define-public ecl-terminal-size
  (sbcl-package->ecl-package sbcl-terminal-size))

(define-public sbcl-cl-readline
  (let ((commit "8438c9ebd92ccc95ebab9cc9cbe6c72d44fccc58")
        (revision "1"))
    (package
      (name "sbcl-cl-readline")
      (version (git-version "0.1.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/vindarel/cl-readline")
               (commit commit)))
         (file-name (git-file-name "cl-readline" version))
         (sha256
          (base32 "14iskvqfw71ssaav483vmqw62lrpznysjs800gjjppxs785p1fa0"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("readline" ,readline)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "cl-readline.lisp"
                 (("libreadline.so")
                  (string-append (assoc-ref inputs "readline")
                                 "/lib/libreadline.so"))))))))
      (home-page "https://github.com/vindarel/cl-readline")
      (synopsis "Common Lisp bindings to the GNU Readline library")
      (description
        "The Readline library provides a set of functions for use by
applications that allow users to edit command lines as they are typed in.
Both Emacs and vi editing modes are available.  The Readline library includes
additional functions to maintain a list of previously-entered command lines, to
recall and perhaps reedit those lines, and perform csh-like history expansion on
previous commands.")
      (license license:gpl3+))))

(define-public cl-readline
  (sbcl-package->cl-source-package sbcl-cl-readline))

(define-public ecl-cl-readline
  (sbcl-package->ecl-package sbcl-cl-readline))

(define-public sbcl-generic-comparability
  (let ((commit "53fc2846319a6eb46b36581e203e1f1542a8acff")
        (revision "1"))
    (package
      (name "sbcl-generic-comparability")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pnathan/generic-comparability")
               (commit commit)))
         (file-name (git-file-name "generic-comparability" version))
         (sha256
          (base32 "01ma0cwirxarwwmdwflnh8kmysmr2smh5kyvzhb2074ljxg8yq2p"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/pnathan/generic-comparability")
      (synopsis "Implementation of cdr-8")
      (description
        "GENERIC-COMPARABILITY is an implementation of CDR-8 (Generic Equality
and Comparison for Common Lisp).  CDR-8 provides an interface for the EQUALS
function, which is defined as a general equality predicate, as well as a set of
ordering (COMPARE) functions for comparison.  The semantics are described in
the CDR-8 standard.")
      (license license:llgpl))))

(define-public cl-generic-comparability
  (sbcl-package->cl-source-package sbcl-generic-comparability))

(define-public ecl-generic-comparability
  (sbcl-package->ecl-package sbcl-generic-comparability))

(define-public sbcl-cl-libyaml
  (let ((commit "a7fe9f68bddfd00b7ca467b65b3b41b276336843")
        (revision "1"))
    (package
      (name "sbcl-cl-libyaml")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eudoxia0/cl-libyaml")
               (commit commit)))
         (file-name (git-file-name "cl-libyaml" version))
         (sha256
          (base32
           "06pvmackyhq03rjmihpx6w63m6cy8wx78ll5xpwwvd85bgrqq817"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("libyaml" ,libyaml)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/library.lisp"
                 (("libyaml.so")
                  (string-append (assoc-ref inputs "libyaml")
                                 "/lib/libyaml.so"))))))))
      (home-page "https://github.com/eudoxia0/cl-libyaml")
      (synopsis "Libyaml bindings for Common Lisp")
      (description
        "This is a binding to the libyaml library.  It's not meant as
a full library for YAML, just a bare binding with a couple of utility macros.
For a YAML parser and emitter using this, check out cl-yaml.")
      (license license:expat))))

(define-public cl-libyaml
  (sbcl-package->cl-source-package sbcl-cl-libyaml))

(define-public ecl-cl-libyaml
  (sbcl-package->ecl-package sbcl-cl-libyaml))

(define-public sbcl-cl-yaml
  (let ((commit "c3202be9a753c51f3bc79538a5a498a8865192aa")
        (revision "1"))
    (package
      (name "sbcl-cl-yaml")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eudoxia0/cl-yaml")
               (commit commit)))
         (file-name (git-file-name "cl-yaml" version))
         (sha256
          (base32 "1izjg0v6rf7dh069bbnnr67l30lsqj86wdk7y9ggbgiwh6v9j185"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-cl-libyaml sbcl-cl-ppcre
             sbcl-parse-number))
      (native-inputs
       (list sbcl-cl-fad sbcl-fiveam sbcl-generic-comparability
             sbcl-trivial-benchmark sbcl-yason))
      (home-page "https://github.com/eudoxia0/cl-yaml")
      (synopsis "YAML parser for Common Lisp")
      (description
        "This is a YAML parser and emitter for Common Lisp built on top of
libyaml.")
      (license license:expat))))

(define-public cl-yaml
  (sbcl-package->cl-source-package sbcl-cl-yaml))

(define-public ecl-cl-yaml
  (sbcl-package->ecl-package sbcl-cl-yaml))

(define-public sbcl-linedit
  (let ((commit "0561c97dfca2f5854fcc66558a567a9875ddcb8f")
        (revision "1"))
    (package
      (name "sbcl-linedit")
      (version (git-version "0.17.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/linedit")
               (commit commit)))
         (file-name (git-file-name "cl-linedit" version))
         (sha256
          (base32 "0hhh7xn6q12rviayfihg1ym6x6csa0pdjgb88ykqbrz2rs3pgpz5"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-cffi sbcl-osicat sbcl-terminfo))
      (home-page "https://github.com/sharplispers/linedit")
      (synopsis "Readline-style line-editor for Common Lisp")
      (description
       "Linedit is a readline-style library written in Common Lisp that
provides customizable line-editing for Common Lisp programs.")
      (license license:expat))))

(define-public cl-linedit
  (sbcl-package->cl-source-package sbcl-linedit))

(define-public ecl-linedit
  (sbcl-package->ecl-package sbcl-linedit))

(define-public sbcl-diff
  (let ((commit "9c84befa598d4e07c3d223242b5b3f83cd94f301")
        (revision "1"))
    (package
      (name "sbcl-diff")
      (version (git-version "0.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/froydnj/diff")
               (commit commit)))
         (file-name (git-file-name "cl-diff" version))
         (sha256
          (base32 "1giafck8qfvb688kx5bn9g32rfc12jjywg8vdav36aqbd6lxf5z5"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre sbcl-trivial-gray-streams))
      (home-page "https://github.com/froydnj/diff")
      (synopsis "Common Lisp library for computing differences between files")
      (description
        "DIFF is a package for computing various forms of differences between
blobs of data and then doing neat things with those differences.  Currently diff
knows how to compute three common forms of differences: \"unified\" format
diffs, \"context\" format diffs, and \"vdelta\" format binary diffs.")
      (license license:bsd-3))))

(define-public cl-diff
  (sbcl-package->cl-source-package sbcl-diff))

(define-public ecl-diff
  (sbcl-package->ecl-package sbcl-diff))

(define-public sbcl-montezuma
  (let ((commit "ee2129eece7065760de4ebbaeffaadcb27644738")
        (revision "1"))
    (package
      (name "sbcl-montezuma")
      (version (git-version "0.1.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/montezuma")
               (commit commit)))
         (file-name (git-file-name "cl-montezuma" version))
         (sha256
          (base32 "0svmvsbsirydk3c1spzfvj8qmkzcs9i69anpfvk1843i62wb7x2c"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; The _darcs directory contains a second copy of
           ;; montezuma-indexfiles.asd. Remove the directory to
           ;; prevent build failure caused by .asd files that have
           ;; the same filename.
           (add-after 'unpack 'remove-darcs-directory
             (lambda _
               (delete-file-recursively
                "contrib/montezuma-indexfiles/_darcs")))
           ;; Tests fail with: :FORCE and :FORCE-NOT arguments not
           ;; allowed in a nested call to ASDF/OPERATE:OPERATE unless
           ;; identically to toplevel.
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "montezuma.asd"
                 ((":force t") "")))))))
      (inputs
       `(("babel" ,sbcl-babel)
         ("cl-fad" ,sbcl-cl-fad)
         ("cl-ppcre" ,sbcl-cl-ppcre)))
      (native-inputs
       (list sbcl-trivial-timeout))
      (home-page "https://github.com/sharplispers/montezuma")
      (synopsis "Full-text indexing and search for Common Lisp")
      (description
       "Montezuma is a text search engine library for Lisp based on the Ferret
library for Ruby, which is itself based on the Lucene library for Java.")
      (license (list license:expat       ; montezuma
                     license:gpl3+)))))  ; contrib/montezuma-indexfiles

(define-public cl-montezuma
  (sbcl-package->cl-source-package sbcl-montezuma))

(define-public ecl-montezuma
  (let ((pkg (sbcl-package->ecl-package sbcl-montezuma)))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ;; Tests fail with "Pathname without a physical namestring" error
         ;; on ECL.
         ((#:tests? _ #f) #f))))))

(define-public sbcl-cl-charms
  (let ((commit "64aba59d89f85bc5c9402e445873965338a66a02")
        (revision "1"))
    (package
      (name "sbcl-cl-charms")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/HiTECNOLOGYs/cl-charms")
               (commit commit)))
         (file-name (git-file-name "cl-charms" version))
         (sha256
          (base32 "1jczaypa9dhxr34yyhsxb6lrdnircjx8am4iqkc3shfpjn32q323"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("ncurses" ,ncurses)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/low-level/curses-bindings.lisp"
                 (("libncursesw.so")
                  (string-append (assoc-ref inputs "ncurses")
                                 "/lib/libncursesw.so"))))))))
      (home-page "https://github.com/HiTECNOLOGYs/cl-charms")
      (synopsis "Interface to libcurses in Common Lisp")
      (description
       "@code{cl-charms} is an interface to libcurses in Common Lisp.  It
provides both a raw, low-level interface to libcurses via CFFI, and a more
higher-level lispier interface.")
      (license license:expat))))

(define-public cl-charms
  (sbcl-package->cl-source-package sbcl-cl-charms))

(define-public ecl-cl-charms
  (sbcl-package->ecl-package sbcl-cl-charms))

(define-public sbcl-trivial-open-browser
  (let ((commit "7ab4743dea9d592639f15c565bfa0756e828c427")
        (revision "1"))
    (package
      (name "sbcl-trivial-open-browser")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eudoxia0/trivial-open-browser")
               (commit commit)))
         (file-name (git-file-name "trivial-open-browser" version))
         (sha256
          (base32 "0ixay1piq420i6adx642qhw45l6ik7rvgk52lyz27dvx5f8yqsdb"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/eudoxia0/trivial-open-browser")
      (synopsis "Open a browser window from Common Lisp")
      (description
       "This package provides a library to open a web browser to a URL.")
      (license license:expat))))

(define-public cl-trivial-open-browser
  (sbcl-package->cl-source-package sbcl-trivial-open-browser))

(define-public ecl-trivial-open-browser
  (sbcl-package->ecl-package sbcl-trivial-open-browser))

(define-public sbcl-clinenoise
  (let ((commit "46e21f99d06a55d93eaa382cf652d55d457032ef")
        (revision "1"))
    (package
      (name "sbcl-clinenoise")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jasom/clinenoise")
               (commit commit)))
         (file-name (git-file-name "clinenoise" version))
         (sha256
          (base32 "0ydlirfk4dbpqqjwwph99v5swcrhd8v9g8q24fvs35wn2vm08lh1"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-cffi sbcl-split-sequence))
      (home-page "https://github.com/jasom/clinenoise")
      (synopsis "Port of linenoise to Common Lisp")
      (description
       "This package provides a trivial line-input library for VT-like
terminals.")
      (license license:bsd-2))))

(define-public cl-clinenoise
  (sbcl-package->cl-source-package sbcl-clinenoise))

(define-public ecl-clinenoise
  (sbcl-package->ecl-package sbcl-clinenoise))

(define-public sbcl-trivial-raw-io
  (let ((commit "b1a3c876305baa0dead419841de7b3e433a75867")
        (revision "1"))
    (package
      (name "sbcl-trivial-raw-io")
      (version (git-version "0.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kingcons/trivial-raw-io")
               (commit commit)))
         (file-name (git-file-name "trivial-raw-io" version))
         (sha256
          (base32 "19290zw2b64k78wr62gv30pp7cmqg07q85vfwjknaffjdd73xwi1"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria))
      (home-page "https://github.com/kingcons/trivial-raw-io")
      (synopsis "Trivial portability for raw *nix IO in Common Lisp")
      (description
       "This library exports three symbols: @code{with-raw-io},
@code{read-char}, and @code{read-line}, to provide raw POSIX I/O in Common
Lisp.")
      (license license:bsd-2))))

(define-public cl-trivial-raw-io
  (sbcl-package->cl-source-package sbcl-trivial-raw-io))

(define-public ecl-trivial-raw-io
  (sbcl-package->ecl-package sbcl-trivial-raw-io))

(define-public sbcl-terminal-keypress
  (let ((commit "2ef48c045aa627229764b2aa393a83d392d93d08")
        (revision "1"))
    (package
      (name "sbcl-terminal-keypress")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eudoxia0/terminal-keypress")
               (commit commit)))
         (file-name (git-file-name "cl-terminal-keypress" version))
         (sha256
          (base32 "11c4krpq5x55qkchx6ykcnb455ssb4r3jjywx3c3irfrkj733ybp"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-trivial-raw-io))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/eudoxia0/terminal-keypress")
      (synopsis "Read keyboard events in the terminal from Common Lisp")
      (description
       "This is a library for reading semi-raw user input from terminals.
Semi-raw as in, we can't detect if the user pressed the @code{Control} key
alone, and the function keys are a mystery.  What is supported, however, is:

@itemize
@item Regular characters
@item Control+[key]
@item Alt+[key]
@item Control+Alt+[key]
@end itemize")
      (license license:expat))))

(define-public cl-terminal-keypress
  (sbcl-package->cl-source-package sbcl-terminal-keypress))

(define-public ecl-terminal-keypress
  (sbcl-package->ecl-package sbcl-terminal-keypress))

(define-public sbcl-periodic-table
  (package
    (name "sbcl-periodic-table")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://common-lisp.net/project/chemboy/periodic-table-"
             version ".tar.gz"))
       (sha256
        (base32 "1ircvqm3q93ma4rxbxprb1i9rcax10ld6xmdzdhfnigr27sh5jvg"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://common-lisp.net/project/chemboy/")
    (synopsis "Periodic table for Common Lisp")
    (description
     "This package defines a Common Lisp package, @code{:elements}, with an
@code{ELEMENT} structure and a number of functions to search the periodic
table.")
    (license license:llgpl)))

(define-public cl-periodic-table
  (sbcl-package->cl-source-package sbcl-periodic-table))

(define-public ecl-periodic-table
  (sbcl-package->ecl-package sbcl-periodic-table))

(define-public sbcl-chemical-compounds
  (package
    (name "sbcl-chemical-compounds")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://common-lisp.net/project/chemboy/chemical-compounds-"
             version ".tar.gz"))
       (sha256
        (base32 "12fd8a6ay5qlsq4givzgh9d55mbg4ci2vvmymig6pjl2ms64v0pf"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("periodic-table" ,sbcl-periodic-table)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-files
           (lambda _
             ;; Fix incorrect version number.
             (substitute* "chemical-compounds.asd"
               ((":version \"1.0.1\"")
                (string-append ":version \"" ,version "\"")))
             ;; Remove incorrect declaration of string type.
             (substitute* "parsing.lisp"
               (("\\(declare \\(simple-base-string string\\)")
                "(declare")))))))
    (home-page "https://common-lisp.net/project/chemboy/")
    (synopsis "Chemical formula parser and pretty-printer for Common Lisp")
    (description
     "It can sometimes be useful to be able to parse chemical compounds in a
user-friendly syntax into easy-to-manipulate s-expressions.  You also want to
be able to go in reverse.  You could probably write your own parser — or you
could just install the chemical-compounds package.")
    (license license:llgpl)))

(define-public cl-chemical-compounds
  (sbcl-package->cl-source-package sbcl-chemical-compounds))

(define-public ecl-chemical-compounds
  (sbcl-package->ecl-package sbcl-chemical-compounds))

(define-public sbcl-chemboy
  (package
    (name "sbcl-chemboy")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://common-lisp.net/project/chemboy/chemboy-"
             version ".tar.gz"))
       (sha256
        (base32 "0lr134l16mjcgdj3fm2yff4chlfbihn1sji7q80y7lnr176zgs7d"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("chemical-compounds" ,sbcl-chemical-compounds)
       ("periodic-table" ,sbcl-periodic-table)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-files
           (lambda _
             ;; Fix incorrect version number.
             (substitute* "chemboy.asd"
               ((":version \"0.2\"")
                (string-append ":version \"" ,version "\"")))
             ;; Remove incorrect declaration of string type.
             (substitute* "query-parsing.lisp"
               (("\\(declare \\(simple-base-string string\\)")
                "(declare"))
             ;; Fix incorrect function calls.
             (substitute* "conversions.lisp"
               (("\\(pprint-compound element s\\)")
                "(pprint-compound element :stream s)")
               (("\\(pprint-compound parsed-compound s\\)")
                "(pprint-compound parsed-compound :stream s)")))))))
    (home-page "https://common-lisp.net/project/chemboy/")
    (synopsis "Common Lisp program for doing basic chemistry calculations")
    (description
     "Chemboy is a Common Lisp program for doing basic chemistry calculations.
This package provides the text-based interface for Chemboy.")
    (license license:llgpl)))

(define-public cl-chemboy
  (sbcl-package->cl-source-package sbcl-chemboy))

(define-public ecl-chemboy
  (sbcl-package->ecl-package sbcl-chemboy))

(define-public sbcl-cl-pass
  (let ((commit "e58e97c0c0588dc742c061208afb9bc31e4dbd34")
        (revision "1"))
    (package
      (name "sbcl-cl-pass")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eudoxia0/cl-pass")
               (commit commit)))
         (file-name (git-file-name "cl-pass" version))
         (sha256
          (base32 "05qx4jrkxqbqi72cxgswbpnifbdvp9mh7apc7566v522899bh0hb"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-ironclad sbcl-trivial-utf-8 sbcl-split-sequence))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/eudoxia0/cl-pass")
      (synopsis "Password hashing and verification library")
      (description
       "@code{cl-pass} is a password hashing and verification library.")
      (license license:expat))))

(define-public cl-pass
  (sbcl-package->cl-source-package sbcl-cl-pass))

(define-public ecl-cl-pass
  (sbcl-package->ecl-package sbcl-cl-pass))

(define-public sbcl-which
  (let ((commit "b2333e4fcacab6e5d85eecd28b5ef4944bda1448")
        (revision "1"))
    (package
      (name "sbcl-which")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eudoxia0/which")
               (commit commit)))
         (file-name (git-file-name "cl-which" version))
         (sha256
          (base32 "127pm9h4rm4w9aadw5yvamnfzhk2rr69kchx10rf9k7sk7izqqfk"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-fad sbcl-path-parse))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/eudoxia0/which")
      (synopsis "The which command in Common Lisp")
      (description
       "This package provides an implementation of the @code{which} UNIX
command in Common Lisp.")
      (license license:expat))))

(define-public cl-which
  (sbcl-package->cl-source-package sbcl-which))

(define-public ecl-which
  (sbcl-package->ecl-package sbcl-which))

(define-public sbcl-cl-num-utils
  (let ((commit "97a88cd34540acf52e872a82ebfef3da0a34fa12")
        (revision "1"))
    (package
      (name "sbcl-cl-num-utils")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tpapp/cl-num-utils")
               (commit commit)))
         (file-name (git-file-name "cl-num-utils" version))
         (sha256
          (base32 "15ihsxxs76xnldmqfsbxybckqjwrxwcpphgghiwzr2mnbqjpdqkh"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-anaphora sbcl-alexandria sbcl-array-operations
             sbcl-cl-slice sbcl-let-plus))
      (native-inputs
       (list sbcl-clunit))
      (home-page "https://github.com/tpapp/cl-num-utils")
      (synopsis "Numerical utilities for Common Lisp")
      (description
       "@code{cl-num-utils} implements simple numerical functions for Common
Lisp, including:
@itemize
@item @code{num=}, a comparison operator for floats
@item simple arithmeric functions, like @code{sum} and @code{l2norm}
@item elementwise operations for arrays
@item intervals
@item special matrices and shorthand for their input
@item sample statistics
@item Chebyshev polynomials
@item univariate rootfinding
@end itemize")
      (license license:boost1.0))))

(define-public cl-num-utils
  (sbcl-package->cl-source-package sbcl-cl-num-utils))

(define-public ecl-cl-num-utils
  (sbcl-package->ecl-package sbcl-cl-num-utils))

(define-public sbcl-lla
  (let ((commit "ded805d1e9b1493e17b601116ba9bd8a3de3024f")
        (revision "1"))
    (package
      (name "sbcl-lla")
      (version (git-version "0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tpapp/lla")
               (commit commit)))
         (file-name (git-file-name "cl-lla" version))
         (sha256
          (base32 "0n9vc7dnyjbbsv1n7rd8sylwda5fsdf8f890g4nachanyx0xps9k"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/configuration.lisp"
                 (("\"libblas.so.3gf\"")
                  (string-append "\"" (assoc-ref inputs "lapack")
                                 "/lib/libblas.so\""))
                 (("\"liblapack.so.3gf\"")
                  (string-append "\"" (assoc-ref inputs "lapack")
                                 "/lib/liblapack.so\""))))))))
      (inputs
       `(("anaphora" ,sbcl-anaphora)
         ("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("cl-num-utils" ,sbcl-cl-num-utils)
         ("cl-slice" ,sbcl-cl-slice)
         ("lapack" ,lapack)
         ("let-plus" ,sbcl-let-plus)))
      (native-inputs
       (list sbcl-clunit))
      (home-page "https://github.com/tpapp/lla")
      (synopsis "Linear algebra library for Common Lisp")
      (description
       "LLA is a high-level Common Lisp library built on BLAS and LAPACK, but
providing a much more abstract interface with the purpose of freeing the user
from low-level concerns and reducing the number of bugs in numerical code.")
      (license license:boost1.0))))

(define-public cl-lla
  (sbcl-package->cl-source-package sbcl-lla))

(define-public ecl-lla
  (sbcl-package->ecl-package sbcl-lla))

(define-public sbcl-cl-rmath
  (let ((commit "f6add1edda31547691d08e36ccf6c17305161aca")
        (revision "1"))
    (package
      (name "sbcl-cl-rmath")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tpapp/cl-rmath")
               (commit commit)))
         (file-name (git-file-name "cl-rmath" version))
         (sha256
          (base32 "1ld8vbpy10paymx2hn0mcgd21i7cjhdrayln1jx0kayqxm12mmk4"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "cl-rmath.lisp"
                 (("\\(cffi:define-foreign-library librmath" all)
                  (string-append all "\n"
                                 "  (:unix \""
                                 (assoc-ref inputs "librmath")
                                 "/lib/libRmath.so\")"))))))))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("librmath" ,rmath-standalone)))
      (home-page "https://github.com/tpapp/cl-rmath")
      (synopsis "Common Lisp wrapper for libRmath")
      (description
       "@code{cl-rmath} is a simple, autogenerated foreign interface for the
standalone R API @code{libRmath}.  There has been no effort to provide a
high-level interface for the original library, instead, this library is meant
to serve as a building block for such an interface.")
      (license license:boost1.0))))

(define-public cl-rmath
  (sbcl-package->cl-source-package sbcl-cl-rmath))

(define-public ecl-cl-rmath
  (sbcl-package->ecl-package sbcl-cl-rmath))

(define-public sbcl-cl-random
  (let ((commit "5bb65911037f95a4260bd29a594a09df3849f4ea")
        (revision "1"))
    (package
      (name "sbcl-cl-random")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tpapp/cl-random")
               (commit commit)))
         (file-name (git-file-name "cl-random" version))
         (sha256
          (base32 "0jn80xphyvyp2v72acr6b8a2f6dw06myr5vrjfl14brsvks7wr89"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria
             sbcl-anaphora
             sbcl-array-operations
             sbcl-cl-num-utils
             sbcl-cl-rmath
             sbcl-cl-slice
             sbcl-gsll
             sbcl-let-plus
             sbcl-lla))
      (native-inputs
       (list sbcl-clunit))
      (home-page "https://github.com/tpapp/cl-random")
      (synopsis "Random variates for Common Lisp")
      (description
       "@code{cl-random} is a library for generating random draws from various
commonly used distributions, and for calculating statistical functions, such as
density, distribution and quantiles for these distributions.")
      (license license:expat))))

(define-public cl-random
  (sbcl-package->cl-source-package sbcl-cl-random))

(define-public ecl-cl-random
  (sbcl-package->ecl-package sbcl-cl-random))

(define-public sbcl-mgl-gpr
  (let ((commit "cb6ce51e2f87bf1d589f3703c13eea6e25780afe")
        (revision "1"))
    (package
      (name "sbcl-mgl-gpr")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/melisgl/mgl-gpr")
               (commit commit)))
         (file-name (git-file-name "cl-mgl-gpr" version))
         (sha256
          (base32 "0w51dqixh277k6sl8bqvvp1400y6kd1l5h3d9q2f40l9bpxy8gjx"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-random sbcl-mgl-pax))
      (home-page "https://melisgl.github.io/mgl-gpr/")
      (synopsis "Common Lisp library of evolutionary algorithms")
      (description
       "@code{MGL-GPR} is a library of evolutionary algorithms such as
Genetic Programming (evolving typed expressions from a set of operators and
constants) and Differential Evolution.")
      (license license:expat))))

(define-public cl-mgl-gpr
  (sbcl-package->cl-source-package sbcl-mgl-gpr))

(define-public ecl-mgl-gpr
  (sbcl-package->ecl-package sbcl-mgl-gpr))

(define-public sbcl-cl-tld
  ;; No release.
  (let ((commit "f5014da8d831fa9481d4181d4450f10a52850c75"))
    (package
      (name "sbcl-cl-tld")
      (version (git-version "0.1" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lu4nx/cl-tld")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0sxnn35gzdby1ixil6zbjg72vli9fcspwzsqimqk455310syx9iv"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/lu4nx/cl-tld/")
      (synopsis "Extract the Top Level Domain from domains, in Common Lisp")
      (description
       "This library extracts the TLD (Top Level Domain) from domains.  The
information is taken from @url{https://publicsuffix.org}.")
      (license license:public-domain))))

(define-public cl-tld
  (sbcl-package->cl-source-package sbcl-cl-tld))

(define-public ecl-cl-tld
  (sbcl-package->ecl-package sbcl-cl-tld))

(define-public sbcl-cl-strftime
  ;; No release.
  (let ((commit "21cb57f2595faa26d687893963f24ec41822b63c"))
    (package
      (name "sbcl-cl-strftime")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ruricolist/cl-strftime/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00c8hq7vzgb89ab3q7mrp60x743kiqmsk1g51ynhxlqhph2bnslf"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-local-time sbcl-cl-ppcre sbcl-serapeum))
      (native-inputs
       (list sbcl-fiveam sbcl-cffi))
      (home-page "https://github.com/ruricolist/cl-strftime")
      (synopsis "Common Lisp compiler for the strftime language")
      (description
       "CL-STRFTIME is a Common Lisp compiler for the strftime “language.”")
      (license license:expat))))

(define-public cl-strftime
  (sbcl-package->cl-source-package sbcl-cl-strftime))

(define-public ecl-cl-strftime
  (sbcl-package->ecl-package sbcl-cl-strftime))

(define-public sbcl-exit-hooks
  ;; No release.
  (let ((commit "78050f4f55c138fcea86a9d720928782021b6012"))
    (package
      (name "sbcl-exit-hooks")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ailisp/exit-hooks/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00rk0pr2cy3hy6giblh166b7yrg06d5lanipjcqv508gkfb0vi47"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/ailisp/exit-hooks")
      (synopsis "Call functions automatically when Common Lisp exits")
      (description
       "@code{exit-hooks} provides a portable way to automatically call some
user-defined function when exiting Common Lisp (both @code{quit} from the REPL
or a kill in a shell).  Like @code{atexit} in C and Python or Java’s
@code{Runtime.addShutdownHook()}.  It currently supports SBCL, CCL, ECL, ABCL,
Allegro CL, clisp and CMUCL.  Before exit-hooks, there was no portable way of
doing so and no staightforward way to use an exit hook on ABCL.  It can be used
for tasks like parmenantly save something when exiting Lisp.")
      (license license:bsd-2))))

(define-public cl-exit-hooks
  (sbcl-package->cl-source-package sbcl-exit-hooks))

(define-public ecl-exit-hooks
  (sbcl-package->ecl-package sbcl-exit-hooks))

(define-public sbcl-cl-base58
  (let ((commit "f446835b4104896e0eed6a61d2ceb4ad22f589d8")
        (revision "1"))
    (package
      (name "sbcl-cl-base58")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eudoxia0/cl-base58/")
               (commit commit)))
         (file-name (git-file-name "cl-base58" version))
         (sha256
          (base32 "01wiiyz1jzxx3zhxi2hpq5n8hv28g1mn0adk793vwjzh4v5bi5zz"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:asd-systems '("cl-base58-test" "cl-base58")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "cl-base58-test.asd"
                 (("cl-test-more")
                  "prove"))
               #t)))))
      (native-inputs
       (list sbcl-prove))
      (home-page "https://github.com/eudoxia0/cl-base58")
      (synopsis "Implementation of base58 for Common Lisp")
      (description
       "This library implements the @code{base58} encoding algorithm.  It's
basically @code{base64} but with a smaller alphabet (58, as in the name) that
doesn't include similar looking characters, among other things.  See
@url{https://github.com/bitcoin/bitcoin/blob/master/src/base58.h} for a full
reference.")
      (license license:expat))))

(define-public cl-base58
  (sbcl-package->cl-source-package sbcl-cl-base58))

(define-public ecl-cl-base58
  (sbcl-package->ecl-package sbcl-cl-base58))

(define-public sbcl-bit-smasher
  ;; No release.
  (let ((commit "c2dcb3b5ec0e485484be681fe17c4e81e58790d9"))
    (package
      (name "sbcl-bit-smasher")
      (version (git-version "1.0.2" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/thephoeron/bit-smasher/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0wjmwn06fjpw0rlpaksf3ab727p8fnzj58z7jajl3m0wqd4ii74w"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-base64" ,sbcl-cl-base64)
         ("cl-base58" ,sbcl-cl-base58)))
      ;; Load order matters for tests, both for file reading and evaluation.
      (arguments
       `(#:asd-systems '("bit-smasher-test" "bit-smasher")
         #:asd-files '("bit-smasher.asd" "bit-smasher-test.asd")))
      (home-page "https://github.com/thephoeron/bit-smasher/")
      (synopsis "Handle bit vectors, bit vector arithmetic, and type conversions")
      (description
       "Utility library for handling bit vectors, bit vector arithmetic, and
universal integer type conversions between bit-vectors, byte-vectors, octals,
decimals, and hexadecimal notation.")
      (license license:expat))))

(define-public cl-bit-smasher
  (sbcl-package->cl-source-package sbcl-bit-smasher))

(define-public ecl-bit-smasher
  (sbcl-package->ecl-package sbcl-bit-smasher))

(define-public sbcl-overlord
  ;; No release.
  (let ((commit "a8f37b321a8aae1652fc50b78e74e57c771cc763"))
    (package
      (name "sbcl-overlord")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ruricolist/overlord/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1maqm53yhlhaa3cka8xcc4sq24ifrr4y3y0s5dyyn682xsh14hb4"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("babel" ,sbcl-babel)
         ("bit-smasher" ,sbcl-bit-smasher)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cl-strftime" ,sbcl-cl-strftime)
         ("cmd" ,sbcl-cmd)
         ("drakma" ,sbcl-drakma)
         ("exit-hooks" ,sbcl-exit-hooks)
         ("fset" ,sbcl-fset)
         ("local-time" ,sbcl-local-time)
         ("lparallel" ,sbcl-lparallel)
         ("md5" ,sbcl-md5)
         ("murmurhash" ,sbcl-cl-murmurhash)
         ("named-readtables" ,sbcl-named-readtables)
         ("ppcre" ,sbcl-cl-ppcre)
         ("serapeum" ,sbcl-serapeum)
         ("trivia" ,sbcl-trivia)
         ("trivial-file-size" ,sbcl-trivial-file-size)))
      (propagated-inputs
       `(("quickproject" ,sbcl-quickproject)))
      (native-inputs
       (list sbcl-fiveam))
      (arguments
       `(#:asd-files '("overlord.asd")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'include-overlord/net
             (lambda _
               (substitute* "all.lisp"
                 (("\\(:import-from :overlord/kernel :nproc\\)")
                  (string-append
                   "(:import-from :overlord/kernel :nproc)"
                   "\n"
                   "(:import-from :overlord/net)")))
               #t)))))
      (home-page "https://github.com/ruricolist/overlord")
      (synopsis "Build system in Common Lisp")
      (description
       "Overlord is a build system in Common Lisp.  It is a real build system,
with all the modern features: rules with multiple outputs, parallel builds,
immunity to clock issues, and dynamic dependencies.

But Overlord is more than another build system.  Overlord is a uniform
approach to dependencies inside or outside of a Lisp image.  Overlord is to
Make what Lisp macros are to C macros.

Overlord is designed to be used from the Lisp REPL.  A command line interface
is available in a separate repository.  See
@url{https://github.com/ruricolist/overlord-cli}.")
      (license license:expat))))

(define-public cl-overlord
  (sbcl-package->cl-source-package sbcl-overlord))

;; FIXME: Broken on ECL? https://github.com/ruricolist/overlord/issues/25
;; (define-public ecl-overlord
;;   (sbcl-package->ecl-package sbcl-overlord))

(define-public sbcl-xpath
  ;; No release.
  (let ((commit "d364da693a534e23bd5eb3a85420e9c25e6c75b3"))
    (package
      (name "sbcl-xpath")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/xpath/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1fb03fgnzrvh22lw1jdg04pmyja5fib5n42rzwp5mhr829yvxkvp"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cxml sbcl-parse-number sbcl-cl-ppcre sbcl-cl-yacc))
      (home-page "https://github.com/sharplispers/xpath/")
      (synopsis "Implementation of the XML Path Language (XPath) Version 1.0")
      (description
       "This library is an implementation of the XML Path Language (XPath)
Version 1.0.")
      (license license:bsd-2))))

;; According to
;; https://github.com/sharplispers/xpath/blob/master/doc/index.xml ECL is not
;; supported.
(define-public cl-xpath
  (sbcl-package->cl-source-package sbcl-xpath))

(define-public sbcl-fxml
  ;; No release.
  (let ((commit "a0e73bb48ef03adea94a55986cc27f522074c8e1"))
    (package
      (name "sbcl-fxml")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ruricolist/fxml/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1vxdb1cjjqi986f72bggnw1s4yzv12g4li7vn4y49b6lphshr8lm"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-babel
             sbcl-named-readtables
             sbcl-serapeum
             sbcl-quri
             sbcl-flexi-streams
             sbcl-split-sequence
             sbcl-alexandria
             sbcl-trivial-gray-streams))
      (native-inputs
       (list sbcl-fiveam sbcl-cxml sbcl-cxml-rng sbcl-xpath))
      (home-page "https://github.com/ruricolist/fxml")
      (synopsis "XML parser and serializer in Common Lisp")
      (description
       "FXML is a secure-by-default, error-recovering XML parser and serializer.
It is a fork of CXML.

You should use FXML instead of CXML if:
@itemize
@item You are parsing potentially ill-formed XML.
@item You are parsing potentially malicious XML.
@item You need to use Klacks with namespaces.
@end itemize

FXML’s API is very close to CXML's, and for the most part you can refer to the
CXML documentation for usage.")
      (license license:llgpl))))

(define-public cl-fxml
  (sbcl-package->cl-source-package sbcl-fxml))

(define-public sbcl-vernacular
  ;; No release.
  (let ((commit "79be179e9ada423b3ec41d2a1ea6f6e0266ed21f"))
    (package
      (name "sbcl-vernacular")
      (version (git-version "0.8.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ruricolist/vernacular/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "19vzn28hw4is4sgmvzqin18ds89s0pai21vcm0ky10vmfv6wg745"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; The demo depends on cl-js, which we don't have at this point.
             (delete-file-recursively "demo")
             #t))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("overlord" ,sbcl-overlord)
         ("trivial-macroexpand-all" ,sbcl-trivial-macroexpand-all)
         ("local-time" ,sbcl-local-time)
         ("parse-js" ,sbcl-parse-js)
         ("trivia" ,sbcl-trivia)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("named-readtables" ,sbcl-named-readtables)
         ("alexandria" ,sbcl-alexandria)
         ("serapeum" ,sbcl-serapeum)
         ("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
      (arguments
       ;; Circular dependency: Tests depend on core-lisp
       ;; (http://github.com/ruricolist/core-lisp) which depends on
       ;; Vernacular.
       '(#:tests? #f))
      (home-page "https://github.com/ruricolist/vernacular")
      (synopsis "Module system for languages that compile to Common Lisp")
      (description
       "Vernacular is a build and module system for languages that compile to
Common Lisp.  It allows languages to compile to Lisp while remaining part of
the Common Lisp ecosystem.  Vernacular languages interoperate with Common Lisp
and one another.

Vernacular handles locating files, compiling files into FASLs, tracking
dependencies and rebuilding, and export and import between your new language,
Lisp, and any other language Vernacular supports.

Vernacular builds on Overlord and is inspired by Racket.")
      (license license:expat))))

(define-public cl-vernacular
  (sbcl-package->cl-source-package sbcl-vernacular))

(define-public sbcl-cmn
  (package
    (name "sbcl-cmn")
    (version "2021.11.22")
    (source
     (origin
       (method url-fetch)
       (uri "https://ccrma.stanford.edu/software/cmn/cmn.tar.gz")
       (file-name (string-append "cmn-" version ".tar.gz"))
       (sha256
        (base32 "04j1l57cdyfi2zzxqwmvmf1hl899ffgs3bl4r42ba47zsw45kq14"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://ccrma.stanford.edu/software/cmn/")
    (synopsis "Western music notation package written in Common Lisp")
    (description
     "CMN provides a package of functions to hierarchically describe a musical
score.  When evaluated, the musical score is rendered to an image.")
    (license license:expat)))

(define-public cl-cmn
  (sbcl-package->cl-source-package sbcl-cmn))

(define-public ecl-cmn
  (sbcl-package->ecl-package sbcl-cmn))

(define-public sbcl-core-gp
  (let ((commit "90ec1c4599a19c5a911be1f703f78d5108aee160")
        (revision "1"))
    (package
      (name "sbcl-core-gp")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jorgetavares/core-gp")
               (commit commit)))
         (file-name (git-file-name "cl-core-gp" version))
         (sha256
          (base32 "0nzlb2gwqisa1amlpl4zc5xxph2g3qwhfyaxchci67d31rzws6l3"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/jorgetavares/core-gp")
      (synopsis "Common Lisp library for genetic programming")
      (description
       "@code{core-gp} is a Common Lisp library for genetic programming (GP)
algorithms.  It allows standard GP, strongly-typed GP, grammatical evolution as
well as standard genetic algorithms.")
      (license license:expat))))

(define-public cl-core-gp
  (sbcl-package->cl-source-package sbcl-core-gp))

(define-public ecl-core-gp
  (sbcl-package->ecl-package sbcl-core-gp))

(define-public sbcl-data-sift
  (let ((commit "fd617d8200cdcc1b87ecf45ab59bb38e8b16ef7e")
        (revision "1"))
    (package
      (name "sbcl-data-sift")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/archimag/data-sift")
               (commit commit)))
         (file-name (git-file-name "cl-data-sift" version))
         (sha256
          (base32 "1v7gf0x4ibjzp0c56n9m77hxdgwcm9356zlk5n4l3fx4i0hj6146"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; All test cases test a function that has been removed.
       `(#:tests? #f))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("parse-number" ,sbcl-parse-number)
         ("puri" ,sbcl-puri)))
      (native-inputs
       (list sbcl-lift))
      (home-page "https://github.com/archimag/data-sift")
      (synopsis
       "Common Lisp library for validation and transformation of string data")
      (description
       "@code{DATA-SIFT} is a Common Lisp data validation and transformation
library inspired by @code{cl-data-format-validation} and WTForms validators.")
      (license license:llgpl))))

(define-public cl-data-sift
  (sbcl-package->cl-source-package sbcl-data-sift))

(define-public ecl-data-sift
  (sbcl-package->ecl-package sbcl-data-sift))

(define-public sbcl-restas
  (let ((commit "81bbbab6b36f81f846f78e71232e9d3d15f6d952")
        (revision "1"))
    (package
      (name "sbcl-restas")
      (version (git-version "0.1.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/archimag/restas")
               (commit commit)))
         (file-name (git-file-name "cl-restas" version))
         (sha256
          (base32 "00ng6jik1lwjw3bbxhijy8s0ml24lgm73liwrr01gcsb0r6wrjjn"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "contrib/restas-daemon.lisp"
                 (("/lib64/") "")
                 (("/lib/") "")
                 (("libcap.so")
                  (string-append (assoc-ref inputs "libcap")
                                 "/lib/libcap.so"))))))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cffi" ,sbcl-cffi)
         ("data-sift" ,sbcl-data-sift)
         ("hunchentoot" ,sbcl-hunchentoot)
         ("libcap" ,libcap)
         ("routes" ,sbcl-routes)))
      (home-page "https://github.com/archimag/restas")
      (synopsis "Common Lisp web framework")
      (description "@code{RESTAS} is a Common Lisp web application framework.")
      (license license:llgpl))))

(define-public cl-restas
  (sbcl-package->cl-source-package sbcl-restas))

(define-public ecl-restas
  (sbcl-package->ecl-package sbcl-restas))

(define-public sbcl-zsort
  (let ((commit "f6724a6fff7662a942195cedb0d7f00da59c74ed")
        (revision "1"))
    (package
      (name "sbcl-zsort")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jorgetavares/zsort")
               (commit commit)))
         (file-name (git-file-name "cl-zsort" version))
         (sha256
          (base32 "1vyklyh99712zsll4qi0m4mm8yb1nz04403vl8i57bjv5p5max49"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria))
      (home-page "https://github.com/jorgetavares/zsort")
      (synopsis "Collection of portable sorting algorithms in Common Lisp")
      (description
       "@code{zsort} is a collection of portable sorting algorithms.  Common
Lisp provides the @code{sort} and @code{stable-sort} functions but these can
have different algorithms implemented according to each implementation.  Also,
the standard sorting functions might not be the best for a certain situations.
This library aims to provide developers with more options.")
      (license license:expat))))

(define-public cl-zsort
  (sbcl-package->cl-source-package sbcl-zsort))

(define-public ecl-zsort
  (sbcl-package->ecl-package sbcl-zsort))

(define-public sbcl-cl-https-everywhere
  ;; No release.
  ;; Don't forget to update the https-everywhere input.
  (let ((commit "cbcc73b985a5b1c0ce0d4ec38bc982a0538d4bd8"))
    (package
      (name "sbcl-cl-https-everywhere")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ruricolist/cl-https-everywhere/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1wcvx1icwym1ncd6wl1wxzkyyndrm796caalbklvjd4a2cbl3xxi"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("global-vars" ,sbcl-global-vars)
         ("parenscript" ,sbcl-parenscript)
         ("cl-markdown" ,sbcl-cl-markdown)
         ("cl-tld" ,sbcl-cl-tld)
         ("fxml" ,sbcl-fxml)
         ("overlord" ,sbcl-overlord)
         ("ppcre" ,sbcl-cl-ppcre)
         ("serapeum" ,sbcl-serapeum)
         ("trivial-gray-streams" ,sbcl-trivial-gray-streams)
         ("vernacular" ,sbcl-vernacular)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)
         ("https-everywhere"
          ,(let ((version "2021.7.13"))
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/EFForg/https-everywhere")
                     (commit version)))
               (file-name (git-file-name "https-everywhere" version))
               (sha256
                (base32
                 "1k5gj29imhxf47sv3d8rxyrgr6k65scp2fm040va3nfshayslzia")))))))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'link-https-everywhere-repo
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((https-everywhere (assoc-ref inputs "https-everywhere")))
                 (symlink https-everywhere "https-everywhere"))))
           (add-after 'unpack 'fix-overlord-build
             ;; Upstream bugs?  See
             ;; https://github.com/ruricolist/cl-https-everywhere/issues/1.
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (rulesets.xml (string-append out "/share/common-lisp/" (%lisp-type)
                                                   "/cl-https-everywhere/rulesets.xml")))
                 (substitute* "build.lisp"
                   (("\\(depends-on https-everywhere-version\\)") "")
                   ;; Don't rebuild the rulesets just because the timestamp is epoch.
                   (("\\(vernacular:require-default :cl-https-everywhere/rulesets-file \"rulesets.xml\"\\)")
                    (format #f "(if (uiop:file-exists-p ~s)
      (compile-rulesets ~s)
      (vernacular:require-default :cl-https-everywhere/rulesets-file \"rulesets.xml\"))"
                            rulesets.xml
                            rulesets.xml))
                   (("\\(uiop:parse-unix-namestring \"https-everywhere/src/chrome/content/rules/\\*\\.xml\")")
                    "\"https-everywhere/src/chrome/content/rules/*.xml\"")
                   (("\\(out temp :external-format :utf-8\\)")
                    "(out temp :external-format :utf-8 :if-exists :supersede)")))
               #t)))))
      (home-page "https://github.com/ruricolist/cl-https-everywhere/")
      (synopsis "Use HTTPS Everywhere rules from Lisp")
      (description
       "CL-HTTPS-EVERYWHERE parses HTTPS Everywhere rulesets and makes them
available for use in Lisp programs.")
      (license (list license:expat
                     ;; For the ruleset
                     license:gpl2+)))))

(define-public cl-https-everywhere
  (sbcl-package->cl-source-package sbcl-cl-https-everywhere))

(define-public sbcl-magic-ed
  (let ((commit "30bb27832d4e3e362578e7320934638f9889a8c4")
        (revision "1"))
    (package
      (name "sbcl-magic-ed")
      (version (git-version "0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sanel/magic-ed")
               (commit commit)))
         (file-name (git-file-name "cl-magic-ed" version))
         (sha256
          (base32 "1j6il4lif0dy6hqiz6n91yl8dvii9pk1i9vz0faq5mnr42mr7i5f"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/sanel/magic-ed")
      (synopsis "Editing facility for Common Lisp REPL")
      (description
       "Magic (ed) is a tiny editing facility for Common Lisp, where you can
directly load, edit, manipulate and evaluate file or file content from REPL.
This package also can be a starting point for people who are not accustomed to
Emacs or SLIME and would like to continue using their default terminal/console
editor with Common Lisp.")
      (license license:expat))))

(define-public cl-magic-ed
  (sbcl-package->cl-source-package sbcl-magic-ed))

(define-public ecl-magic-ed
  (sbcl-package->ecl-package sbcl-magic-ed))

(define-public sbcl-maxpc
  (let ((commit "e5e58d053039517d30fd59ab2d128256b87790d5")
        (revision "1"))
    (package
      (name "sbcl-maxpc")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eugeneia/maxpc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "15wrjbr2js6j67c1dd4p2qxj49q9iqv1lhb7cwdcwpn79crr39gf"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://mr.gy/software/maxpc/api.html")
      (synopsis
       "Library for writing parsers and lexers based on combinatory parsing")
      (description
       "@emph{Max’s Parser Combinators} is a simple and pragmatic library for
writing parsers and lexers based on combinatory parsing.  MaxPC is capable of
parsing deterministic, context-free languages, provides powerful tools for
parse tree transformation and error handling, and can operate on
@dfn{sequences} and @dfn{streams}.  It supports unlimited backtracking, but
does not implement @url{http://pdos.csail.mit.edu/~baford/packrat/thesis/,
Packrat Parsing}.  Instead, MaxPC achieves good performance through its
optimized primitives, and explicit separation of matching and capturing input.
In practice, MaxPC parsers perform better on typical computer languages—when
compared to Packrat parsers—at the expense of not producing linear-time
parsers.")
      (license license:agpl3))))

(define-public cl-maxpc
  (sbcl-package->cl-source-package sbcl-maxpc))

(define-public ecl-maxpc
  (sbcl-package->ecl-package sbcl-maxpc))

(define-public sbcl-random-state
  (let ((commit "c270d4f15e0b66ba9680ca8734a5de56959cb118")
        (revision "1"))
    (package
      (name "sbcl-random-state")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/random-state")
               (commit commit)))
         (file-name (git-file-name "cl-random-state" version))
         (sha256
          (base32 "0r3bk6hqpr0qmpza93pknl8wpsd6y0yy9qg7vz751a7gzzww9vj6"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-documentation-utils))
      (home-page "https://shinmera.github.io/random-state/")
      (synopsis "Portable random number generation")
      (description
       "This library is a collection of @dfn{pseudo random number generators}.

While Common Lisp does provide a @code{RANDOM} function, it does not allow the
user to pass an explicit @code{SEED}, nor to portably exchange the random
state between implementations.  This can be a headache in cases like games,
where a controlled seeding process can be very useful.

For both curiosity and convenience, this library offers multiple algorithms to
generate random numbers, as well as a bunch of generally useful methods to
produce desired ranges.")
      (license license:zlib))))

(define-public cl-random-state
  (sbcl-package->cl-source-package sbcl-random-state))

(define-public ecl-random-state
  (sbcl-package->ecl-package sbcl-random-state))

(define-public sbcl-decimals
  (package
    (name "sbcl-decimals")
    (version "2021")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tlikonen/cl-decimals")
             (commit version)))
       (file-name (git-file-name "cl-decimals" version))
       (sha256
        (base32 "0wn5hq1pwd3wpjqqhpjzarcdk1q6416g8y447iaf55j5nbhlmbn6"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/tlikonen/cl-decimals")
    (synopsis "Decimal number parser and formatting package for Common Lisp")
    (description "This Common Lisp package offers functions for parsing and
formatting decimal numbers.  The package's main interface are the functions
@code{parse-decimal-number} and @code{format-decimal-number}.  The former is
for parsing strings for decimal numbers and the latter for pretty-printing
them as strings.")
    (license license:cc0)))

(define-public cl-decimals
  (sbcl-package->cl-source-package sbcl-decimals))

(define-public ecl-decimals
  (sbcl-package->ecl-package sbcl-decimals))

(define-public sbcl-simple-date-time
  (let ((commit "d6992afddedf67a8172a0120a1deac32afcaa2e8")
        (revision "1"))
    (package
      (name "sbcl-simple-date-time")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/quek/simple-date-time")
               (commit commit)))
         (file-name (git-file-name "cl-simple-date-time" version))
         (sha256
          (base32 "06iwf13gcdyqhkzfkcsfdl8iqbdl44cx01c3fjsmhl0v1pp8h2m4"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre))
      (home-page "https://github.com/quek/simple-date-time")
      (synopsis "Date and time library for Common Lisp")
      (description "This package is a simple date and time library.")
      (license license:bsd-4))))

(define-public cl-simple-date-time
  (sbcl-package->cl-source-package sbcl-simple-date-time))

(define-public ecl-simple-date-time
  (sbcl-package->ecl-package sbcl-simple-date-time))

(define-public sbcl-april
  (let ((commit "963e2d8e5575a7d430c1fba7adedd15cb23c4ce8")
        (revision "1"))
    (package
      (name "sbcl-april")
      (version (git-version "0.9.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/phantomics/april")
               (commit commit)))
         (file-name (git-file-name "cl-april" version))
         (sha256
          (base32 "0v27fpss1ayca2r47k0zpqa9a423a86pv8s2mlgc3g5s48lgcmj3"))
         (modules '((guix build utils)))
         (snippet '(begin
                     ;; Remove bundled Apache-relicensed MaxPC.
                     (delete-file-recursively "maxpc-apache")
                     ;; Ensure references are to upstream MaxPC.
                     (substitute* "vex/vex.asd"
                       (("maxpc-apache") "maxpc"))))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria
             sbcl-array-operations
             sbcl-maxpc
             sbcl-cl-ppcre
             sbcl-symbol-munger
             sbcl-prove
             sbcl-parse-number
             sbcl-lparallel
             sbcl-random-state
             sbcl-decimals
             sbcl-simple-date-time
             sbcl-trivia))
      (home-page "https://github.com/phantomics/april")
      (synopsis "Array Programming Re-Imagined in Lisp")
      (description
       "April compiles a subset of the APL programming language into
Common Lisp.  Leveraging Lisp's powerful macros and numeric processing
faculties, it brings APL's expressive potential to bear for Lisp developers.
Replace hundreds of lines of number-crunching code with a single line of
APL.")
      (license license:asl2.0))))

(define-public cl-april
  (sbcl-package->cl-source-package sbcl-april))

(define-public ecl-april
  (sbcl-package->ecl-package sbcl-april))

(define-public sbcl-cl-sxml
  (let ((commit "e5542b1d9bd6cee03ae27547d00660ccfbb60109")
        (revision "1"))
    (package
      (name "sbcl-cl-sxml")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/eadmund/cl-sxml")
               (commit commit)))
         (file-name (git-file-name "cl-sxml" version))
         (sha256
          (base32 "1105s9whidq1lf0lli2wdhcfcs5gwzxa0h1x3izx4mp2p7psvciz"))))
      (build-system asdf-build-system/sbcl)
      (inputs (list sbcl-cxml))
      (native-inputs
       (list sbcl-fiveam
             sbcl-flexi-streams))
      (home-page "https://github.com/eadmund/cl-sxml")
      (synopsis "SXML parsing for Common Lisp")
      (description
       "@code{CL-SXML} implements Oleg Kiselyov’s SXML, an S-expression-based
rendering of the XML Infoset.")
      (license license:gpl3+))))

(define-public cl-sxml
  (sbcl-package->cl-source-package sbcl-cl-sxml))

(define-public ecl-cl-sxml
  (sbcl-package->ecl-package sbcl-cl-sxml))

(define-public sbcl-cl-gopher
  (let ((commit "62cfd180378f56e7e8b57e4302b183810c86e337")
        (revision "2"))
    (package
      (name "sbcl-cl-gopher")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/knusbaum/cl-gopher")
               (commit commit)))
         (file-name (git-file-name "cl-gopher" version))
         (sha256
          (base32 "0szz29d83fk2cxn5j1zlf4v0154qnf9cy1ix5p4jjpsql1a8xiwg"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-bordeaux-threads
             sbcl-drakma
             sbcl-flexi-streams
             sbcl-quri
             sbcl-split-sequence
             sbcl-usocket))
      (home-page "https://github.com/knusbaum/cl-gopher")
      (synopsis "Gopher protocol library in Common Lisp")
      (description
       "@code{cl-gopher} is a Common Lisp library for interacting with the
Gopher protocol.

It is suitable for building both clients and servers, and provides a sample
client.")
      (license license:bsd-2))))

(define-public cl-gopher
  (sbcl-package->cl-source-package sbcl-cl-gopher))

(define-public ecl-cl-gopher
  (sbcl-package->ecl-package sbcl-cl-gopher))

(define-public sbcl-phos
  (let ((commit "6620b82b091cdfed655e1093ef045dbe518d5474"))
    (package
      (name "sbcl-phos")
      (version (git-version "0.0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/omar-polo/phos")
               (commit commit)))
         (file-name (git-file-name "phos" version))
         (sha256
          (base32
           "1zwci86rkbaiix0w2gd5q6nr8v2vv945pkvwp0j240iyzd4hfxr3"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-quri sbcl-cl-ppcre sbcl-trivia sbcl-usocket sbcl-cl+ssl
             ;; For the experimental GUI:
             sbcl-nodgui))
      (native-inputs
       (list sbcl-clunit2))
      (home-page "https://github.com/omar-polo/phos")
      (synopsis "Gemini client library and experimental GUI")
      (description "This package contains a Gemini client library for Common
Lisp.  A subsystem offers an experimental GUI Gemini client.")
      (license license:isc))))

(define-public cl-phos
  (sbcl-package->cl-source-package sbcl-phos))

(define-public ecl-phos
  (sbcl-package->ecl-package sbcl-phos))

(define-public sbcl-nhooks
  (package
    (name "sbcl-nhooks")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atlas-engineer/nhooks")
             (commit version)))
       (file-name (git-file-name "nhooks" version))
       (sha256
        (base32
         "1v3gh9jf4hbpl1hnyzhkrcj3jdh3q0mlp1n6ci2cbn2rsc65baff"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-serapeum))
    (native-inputs
     (list sbcl-fiveam))
    (home-page "https://github.com/atlas-engineer/nhooks")
    (synopsis "Hook facility for Common Lisp")
    (description "This package holds an enhanced implementation of
hooks (extension points).  It works similarly to Emacs hooks with crucial
improvements:

@itemize

@item If the compiler allows it (such as SBCL), type-checking is performed at
compile-time and at run-time when adding handlers to a hook.

@item On failure, multiple restarts are offered, such as disabling the
offending handler or simply continuing to the next function.

@item The hook handler execution order and combination can be customized.

@item Anonymous functions (lambdas) can be added to hooks as handler objects.
When inspecting hooks, readable names are thus exposed instead of lambda
blackboxes.  Handlers are compared through their names (through the mandatory
name slot).  A hook can not contain multiple handlers with the same name.

@item A special provision is taken for “setters”, handlers that are meant to
set a given place to a given values.  Such handler objects can be compared and
identified uniquely.

@end itemize\n")
    (license license:expat)))

(define-public cl-nhooks
  (sbcl-package->cl-source-package sbcl-nhooks))

(define-public ecl-nhooks
  (sbcl-package->ecl-package sbcl-nhooks))
