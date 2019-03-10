;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages perl6)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system rakudo)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls))

(define-public moarvm
  (package
    (name "moarvm")
    (version "2019.03")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://moarvm.org/releases/MoarVM-"
                            version ".tar.gz"))
        (sha256
         (base32
          "017w1zvr6yl0cgjfc1b3ddlc6vjw9q8p7alw1vvsckw95190xc14"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;(delete-file-recursively "3rdparty/dynasm") ; JIT
            (delete-file-recursively "3rdparty/dyncall")
            (delete-file-recursively "3rdparty/freebsd")
            (delete-file-recursively "3rdparty/libatomicops")
            (delete-file-recursively "3rdparty/libuv")
            (delete-file-recursively "3rdparty/libtommath")
            (delete-file-recursively "3rdparty/msinttypes")
            #t))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out        (assoc-ref outputs "out"))
                   (pkg-config (assoc-ref inputs "pkg-config")))
               (setenv "LDFLAGS" (string-append "-Wl,-rpath=" out "/lib"))
               (invoke "perl" "Configure.pl"
                       "--prefix" out
                       "--pkgconfig" (string-append pkg-config "/bin/pkg-config")
                       "--has-libtommath"
                       "--has-libatomic_ops"
                       "--has-libffi"
                       "--has-libuv")))))))
    (home-page "https://moarvm.org/")
    ;; These should be inputs but moar.h can't find them when building rakudo
    (propagated-inputs
     `(("libatomic-ops" ,libatomic-ops)
       ("libtommath" ,libtommath-1.0)
       ("libuv" ,libuv)))
    (inputs
     `(("libffi" ,libffi)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "VM for NQP And Rakudo Perl 6")
    (description
     "Short for \"Metamodel On A Runtime\", MoarVM is a modern virtual machine
built for the Rakudo Perl 6 compiler and the NQP Compiler Toolchain.  Highlights
include:

@itemize
@item Great Unicode support, with strings represented at grapheme level
@item Dynamic analysis of running code to identify hot functions and loops, and
perform a range of optimizations, including type specialization and inlining
@item Support for threads, a range of concurrency control constructs, and
asynchronous sockets, timers, processes, and more
@item Generational, parallel, garbage collection
@item Support for numerous language features, including first class functions,
exceptions, continuations, runtime loading of code, big integers and interfacing
with native libraries.
@end itemize")
    (license license:artistic2.0)))

(define-public nqp
  (package
    (name "nqp")
    (version "2019.03")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://rakudo.perl6.org/downloads/nqp/nqp-"
                            version ".tar.gz"))
        (sha256
         (base32
          "183zhll13fx416s3hkg4bkvib77kyr857h0nydgrl643fpacxp83"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "3rdparty") #t))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda _
             (substitute* '("tools/build/install-jvm-runner.pl.in"
                            "tools/build/gen-js-cross-runner.pl"
                            "tools/build/gen-js-runner.pl"
                            "tools/build/install-js-runner.pl"
                            "tools/build/install-moar-runner.pl"
                            "tools/build/gen-moar-runner.pl"
                            "t/nqp/111-spawnprocasync.t"
                            "t/nqp/113-run-command.t")
               (("/bin/sh") (which "sh")))
             #t))
         (add-after 'unpack 'patch-source-date
           (lambda _
             (substitute* "tools/build/gen-version.pl"
               (("gmtime") "gmtime(0)"))
             #t))
         (add-after 'unpack 'remove-failing-test
           ;; One subtest fails for unknown reasons
           (lambda _
             (delete-file "t/nqp/019-file-ops.t")
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out"))
                   (moar (assoc-ref inputs "moarvm")))
               (invoke "perl" "Configure.pl"
                       "--backends=moar"
                       "--with-moar" (string-append moar "/bin/moar")
                       "--prefix" out)))))))
    (inputs
     `(("moarvm" ,moarvm)))
    (home-page "https://github.com/perl6/nqp")
    (synopsis "Not Quite Perl")
    (description "This is \"Not Quite Perl\" -- a lightweight Perl 6-like
environment for virtual machines.  The key feature of NQP is that it's designed
to be a very small environment (as compared with, say, perl6 or Rakudo) and is
focused on being a high-level way to create compilers and libraries for virtual
machines like MoarVM, the JVM, and others.

Unlike a full-fledged implementation of Perl 6, NQP strives to have as small a
runtime footprint as it can, while still providing a Perl 6 object model and
regular expression engine for the virtual machine.")
    (license license:artistic2.0)))

(define-public rakudo
  (package
    (name "rakudo")
    (version "2019.03.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://rakudo.perl6.org/downloads/rakudo/rakudo-"
                            version ".tar.gz"))
        (sha256
         (base32
          "1nllf69v8xr6v3kkj7pmryg11n5m3ajfkr7j72pvhrgnjy8lv3r1"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source-date
           (lambda _
             (substitute* "tools/build/gen-version.pl"
               (("gmtime") "gmtime(0)"))
             #t))
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda _
             (substitute* '("tools/build/create-js-runner.pl"
                            "tools/build/create-moar-runner.p6"
                            "tools/build/create-jvm-runner.pl"
                            "src/core/Proc.pm6")
               (("/bin/sh") (which "sh")))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (nqp (assoc-ref inputs "nqp")))
               (invoke "perl" "./Configure.pl"
                       "--backend=moar"
                       "--with-nqp" (string-append nqp "/bin/nqp")
                       "--prefix" out))))
         ;; This is the recommended tool for distro maintainers to install perl6
         ;; modules systemwide.  See: https://github.com/ugexe/zef/issues/117
         (add-after 'install 'install-dist-tool
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (dest (string-append out "/share/perl6/tools")))
               (install-file "tools/install-dist.p6" dest)
               (substitute* (string-append dest "/install-dist.p6")
                 (("/usr/bin/env perl6")
                  (string-append out "/bin/perl6"))))
             #t)))))
    (inputs
     `(("moarvm" ,moarvm)
       ("nqp" ,nqp)
       ("openssl" ,openssl)))
    (home-page "https://rakudo.org/")
    (native-search-paths
      (list (search-path-specification
              (variable "PERL6LIB")
              (separator ",")
              (files '("share/perl6/lib"
                       "share/perl6/site/lib"
                       "share/perl6/vendor/lib")))))
    (synopsis "Perl 6 Compiler")
    (description "Rakudo Perl is a compiler that implements the Perl 6
specification and runs on top of several virtual machines.")
    (license license:artistic2.0)))

(define-public perl6-json-class
  (package
    (name "perl6-json-class")
    (version "0.0.12")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jonathanstowe/JSON-Class.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1zyzajc57j3m8q0nr72h9pw4w2nx92rafywlvysgphc5q9sb8np2"))))
    (build-system rakudo-build-system)
    (propagated-inputs
     `(("perl6-json-marshal" ,perl6-json-marshal)
       ("perl6-json-unmarshal" ,perl6-json-unmarshal)))
    (native-inputs
     `(("perl6-json-fast" ,perl6-json-fast)))
    (home-page "https://github.com/jonathanstowe/JSON-Class")
    (synopsis "Provide simple serialisation/deserialisation of objects to/from JSON")
    (description "This is a simple role that provides methods to instantiate a
class from a JSON string that (hopefully,) represents it, and to serialise an
object of the class to a JSON string.  The JSON created from an instance should
round trip to a new instance with the same values for the @quot{public
attributes}.  @quot{Private} attributes (that is ones without accessors,) will
be ignored for both serialisation and de-serialisation.  The exact behaviour
depends on that of @code{JSON::Marshal} and @code{JSON::Unmarshal} respectively.")
    (license license:artistic2.0)))

(define-public perl6-json-fast
  (package
    (name "perl6-json-fast")
    (version "0.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/timo/json_fast.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1fif081gdxdnja14vkj523p9dyzdcdj81lmjv9fvfazvpagb6dg2"))))
    (build-system rakudo-build-system)
    (arguments '(#:with-zef? #f))
    (home-page "https://github.com/timo/json_fast")
    (synopsis "Perl6 json parser")
    (description "A naive imperative json parser in pure perl6 (but with direct
access to @code{nqp::} ops), to evaluate performance against @code{JSON::Tiny}.
It is a drop-in replacement for @code{JSON::Tiny}'s from-json and to-json subs,
but it offers a few extra features.")
    (license license:artistic2.0)))

(define-public perl6-json-marshal
  (package
    (name "perl6-json-marshal")
    (version "0.0.16")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jonathanstowe/JSON-Marshal.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0qy7j83h6gjzyyv74ncd92cd9h45rv8diaz3vldiv3b6fqwz4c6i"))))
    (build-system rakudo-build-system)
    (propagated-inputs
     `(("perl6-json-fast" ,perl6-json-fast)
       ("perl6-json-name" ,perl6-json-name)))
    (native-inputs
     `(("perl6-json-fast" ,perl6-json-fast)))
    (home-page "https://github.com/jonathanstowe/JSON-Marshal")
    (synopsis "Simple serialisation of objects to JSON")
    (description "This library provides a single exported subroutine to create
a JSON representation of an object.  It should round trip back into an object
of the same class using @code{JSON::Unmarshal}.")
    (license license:artistic2.0)))

(define-public perl6-json-name
  (package
    (name "perl6-json-name")
    (version "0.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jonathanstowe/JSON-Name.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "130qwdpbj5qdlsdz05y0rksd79lzbq79scy47n6lnf21b0hz1qjc"))))
    (build-system rakudo-build-system)
    (arguments '(#:with-zef? #f))
    (home-page "https://github.com/jonathanstowe/JSON-Name")
    (synopsis "Provides a trait to store an alternative JSON Name")
    (description "This is released as a dependency of @code{JSON::Marshal} and
@code{JSON::Unmarshal} in order to save duplication, it is intended to store a
separate JSON name for an attribute where the name of the JSON attribute might be
changed, either for aesthetic reasons or the name is not a valid Perl identifier.
It will of course also be needed in classes thar are going to use
@code{JSON::Marshal} or @code{JSON::Unmarshal} for serialisation/de-serialisation.")
    (license license:artistic2.0)))

(define-public perl6-json-unmarshal
  ;; Last commit was May 2017
  (let ((commit "e1b6288c5f3165058f36c0f4e171cdf2dfd640da")
        (revision "1"))
    (package
      (name "perl6-json-unmarshal")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/tadzik/JSON-Unmarshal.git")
                 (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "14azsmzmwdn8k0gqcpvballharcvzylmlyrx2wmv4kpqfnz29fjc"))))
      (build-system rakudo-build-system)
      (propagated-inputs
       `(("perl6-json-fast" ,perl6-json-fast)
         ("perl6-json-name" ,perl6-json-name)))
      (home-page "https://github.com/tadzik/JSON-Unmarshal")
      (synopsis "Make JSON from an Object")
      (description "This library provides a single exported subroutine to
create an object from a JSON representation of an object.")
      (license license:expat))))

(define-public perl6-license-spdx
  (package
    (name "perl6-license-spdx")
    (version "3.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jonathanstowe/License-SPDX")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0dl263c3fbxk001gm5fisrzqz1dx182ipaa0x2qva2gxvl075xm8"))))
    (build-system rakudo-build-system)
    (propagated-inputs
     `(("perl6-json-class" ,perl6-json-class)))
    (home-page "https://github.com/jonathanstowe/License-SPDX")
    (synopsis "Abstraction over the SPDX License List")
    (description "This provides an abstraction over the SPDX License List as
provided in JSON format.  Its primary raison d'être is to help the licence
checking of @code{Test::META} and to allow for the warning about deprecated
licences therein.")
    (license license:artistic2.0)))

(define-public perl6-meta6
  (package
    (name "perl6-meta6")
    (version "0.0.23")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jonathanstowe/META6.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1xnlaamfbdlfb2zidim3bbc4mawsrg6qxhxi6gbld46z1cyry1cw"))))
    (build-system rakudo-build-system)
    (propagated-inputs
     `(("perl6-json-class" ,perl6-json-class)))
    (native-inputs
     `(("perl6-json-fast" ,perl6-json-fast)))
    (home-page "https://github.com/jonathanstowe/META6")
    (synopsis "Do things with Perl 6 [META files]")
    (description "This provides a representation of the Perl 6 META files
specification - the META file data can be read, created, parsed and written in a
manner that is conformant with the specification.

Where they are known about it also makes allowance for @quot{customary} usage in
existing software (such as installers and so forth.)

The intent of this is allow the generation and testing of META files for module
authors, so it can provide meta-information whether the attributes are mandatory
as per the spec and where known the places that @quot{customary} attributes are
used.")
    (license license:artistic2.0)))

(define-public perl6-oo-monitors
  (package
    (name "perl6-oo-monitors")
    (version "1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jnthn/oo-monitors")
               ;; The commit where 1.1 was "tagged"
               (commit "494db3a3852854f30a80c9bd1489a7d5e429e7c5")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1sbw2x54wwjjanghjnc7ipmplaw1srvbrphsdv4ym6cipnbmbj9x"))))
    (build-system rakudo-build-system)
    (arguments '(#:with-zef? #f))
    (home-page "https://github.com/jnthn/oo-monitors")
    (synopsis "Monitors with condition variables for Perl 6")
    (description "A monitor provides per-instance mutual exclusion for objects.
This means that for a given object instance, only one thread can ever be inside
its methods at a time.  This is achieved by a lock being associated with each
object.  The lock is acquired automatically at the entry to each method in the
monitor.  Condition variables are also supported.")
    (license license:artistic2.0)))

(define-public perl6-svg
  ;; Latest commit, basically unchanged since August 2015
  (let ((commit "07190c0602aa276e5319f06aa0012452dbff3582")
        (revision "1"))
    (package
      (name "perl6-svg")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/moritz/svg")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0mkjdhg7ajksdn61n8fqhyzfd7ly9myazsvpsm02a5c2q73hdygg"))))
      (build-system rakudo-build-system)
      (propagated-inputs
       `(("perl6-xml-writer" ,perl6-xml-writer)))
      (home-page "https://github.com/moritz/svg")
      (synopsis "Perl 6 module to generate SVG")
      (description "This is a Perl 6 module that makes it easy to write
@dfn{Scalable Vector Graphic files} (SVG).  Right now it is a shallow wrapper
around @code{XML::Writer}, adding only the xmlns attributes that identifies an
XML file as SVG.")
      (license license:artistic2.0))))

(define-public perl6-svg-plot
  ;; Latest commit
  (let ((commit "062570a78fd38c3c6baba29dfe2fbb8ca014f4de")
        (revision "1"))
    (package
      (name "perl6-svg-plot")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/moritz/svg-plot")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "095ga5hbg92jnmczxvhk1hjz14yr334zyf8cph4w5w5frcza44my"))))
      (build-system rakudo-build-system)
      (propagated-inputs
       `(("perl6-svg" ,perl6-svg)))
      (home-page "https://github.com/moritz/svg-plot")
      (synopsis "Perl 6 charting and plotting library that produces SVG output")
      (description "@code{SVG::Plot} is a simple 2D chart plotter for Perl 6.
It currently supports bars, stacked bars, lines and points (both equally spaced
with optional labels, or xy plots).")
      (license license:artistic2.0))))

(define-public perl6-tap-harness
  (package
    (name "perl6-tap-harness")
    (version "0.0.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/perl6/tap-harness6.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1lig8i0my3fgqvlay9532xslbf3iis2d7wz89gniwvwqffi2kh6r"))))
    (build-system rakudo-build-system)
    (arguments
     '(#:with-zef? #f
       #:with-prove6? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "perl6" "-Ilib" "bin/prove6" "-l" "t"))))))
    (home-page "https://github.com/perl6/tap-harness6/")
    (synopsis "TAP harness for perl6")
    (description "This module provides the @command{prove6} command which runs a
TAP based test suite and prints a report.  The @command{prove6} command is a
minimal wrapper around an instance of this module.")
    (license license:artistic2.0)))

(define-public perl6-terminal-ansicolor
  (package
    (name "perl6-terminal-ansicolor")
    (version "0.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/tadzik/Terminal-ANSIColor.git")
               ;; The commit where 0.5 was "tagged"
               (commit "edded4a7116ce11cbc9fb5a83669c7ba119d0212")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1apm999azkyg5s35gid12wq019aqnvzrkz7qjmipd74mdxgr00x7"))))
    (build-system rakudo-build-system)
    (arguments '(#:with-zef? #f))
    (home-page "https://github.com/tadzik/Terminal-ANSIColor")
    (synopsis "Colorize terminal output")
    (description "This is a @code{Terminal::ANSIColor} module for Perl 6.")
    (license license:expat)))

(define-public perl6-test-meta
  (package
    (name "perl6-test-meta")
    (version "0.0.14")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jonathanstowe/Test-META")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1mzrglb7lbiy5h9dlc7dyhvv9gppxmdmpmrv6nzbd695jzr38bri"))))
    (build-system rakudo-build-system)
    (propagated-inputs
     `(("perl6-meta6" ,perl6-meta6)
       ("perl6-uri" ,perl6-uri)))
    (home-page "https://github.com/jonathanstowe/Test-META")
    (synopsis "Test a distributions META file")
    (description "This provides a simple mechanism for module authors to have
some confidence that they have a working distribution META description file.")
    (license license:artistic2.0)))

(define-public perl6-test-mock
  (package
    (name "perl6-test-mock")
    (version "1.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jnthn/test-mock")
               ;; The commit where 1.5 was "tagged"
               (commit "6eddb42f73f40b9ac29c14badb41ce4a04d876f2")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "07yr3qimc8fl29p23562ayj2j9h53madcnf9sgqvgf2kcprh0zd2"))))
    (build-system rakudo-build-system)
    (propagated-inputs
     `(("perl6-oo-monitors" ,perl6-oo-monitors)))
    (home-page "https://github.com/jnthn/test-mock")
    (synopsis "Module for simply generating and checking mock objects")
    (description "@code{Test::Mock} is a module that works alongside the
standard Test module to help you write tests when you want to verify what
methods are called on an object, while still having calls to undefined methods
die.  You get started just as normal with the test file, but also add a use
statement for @code{Test::Mock}.")
    (license license:artistic2.0)))

(define-public perl6-uri
  (package
    (name "perl6-uri")
    (version "0.1.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/perl6-community-modules/uri.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0h318g75jqn2ckw051g35iqyfxz1mps0jyg5z6pd857y3kacbkpl"))))
    (build-system rakudo-build-system)
    (arguments '(#:with-zef? #f))
    (home-page "https://github.com/perl6-community-modules/uri")
    (synopsis "URI implementation using Perl 6")
    (description "A URI implementation using Perl 6 grammars to implement RFC
3986 BNF.  Currently only implements parsing.  Includes @code{URI::Escape} to
(un?)escape characters that aren't otherwise allowed in a URI with % and a hex
character numbering.")
    (license license:artistic2.0)))

(define-public perl6-xml-writer
  ;; Last commit was May 2017
  (let ((commit "4d30a9d8e06033ca97387971b653817becd5a759")
        (revision "1"))
    (package
      (name "perl6-xml-writer")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/masak/xml-writer")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1kwrf7akp70kyzw1b90khp71a6hpilihwndy2jsjpffcd4hd4m4z"))))
      (build-system rakudo-build-system)
      (arguments '(#:with-zef? #f))
      (home-page "https://github.com/masak/xml-writer")
      (synopsis "Perl 6 module to generate XML")
      (description "@code{XML::Writer} is a module for creating XML in Perl 6.")
      (license license:artistic2.0))))

(define-public perl6-zef
  (package
    (name "perl6-zef")
    (version "0.6.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ugexe/zef.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "07n7g1xw2c4g860rs890gx85vyhdq0ysgwbrnzw6q905jph2bkv7"))))
    (build-system rakudo-build-system)
    (arguments
     '(#:with-zef? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "HOME" "/tmp")
             (invoke "perl6" "-I." "bin/zef" "--debug"
                     "--tap-harness" "test" "."))))))
    (home-page "https://github.com/ugexe/zef")
    (synopsis "Perl6 Module Management")
    (description "Zef is a Perl 6 package (module) manager.  It can be used to
download and install Perl 6 modules in your home directory or as a system-wide
module.")
    (license license:artistic2.0)))
