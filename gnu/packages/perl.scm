;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages perl)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;


(define-public perl
  ;; Yeah, Perl...  It is required early in the bootstrap process by Linux.
  (package
    (name "perl")
    (version "5.16.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.cpan.org/src/5.0/perl-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "15qxzba3a50c9nik5ydgyfp62x7h9vxxn12yd1jgl93hb1wj96km"))
             (patches (list (search-patch "perl-no-sys-dirs.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (alist-replace
        'configure
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let ((out  (assoc-ref outputs "out"))
                (libc (assoc-ref inputs "libc")))
            ;; Use the right path for `pwd'.
            (substitute* "dist/Cwd/Cwd.pm"
              (("/bin/pwd")
               (which "pwd")))

            (zero?
             (system* "./Configure"
                      (string-append "-Dprefix=" out)
                      (string-append "-Dman1dir=" out "/share/man/man1")
                      (string-append "-Dman3dir=" out "/share/man/man3")
                      "-de" "-Dcc=gcc"
                      "-Uinstallusrbinperl"
                      "-Dinstallstyle=lib/perl5"
                      "-Duseshrplib"
                      (string-append "-Dlocincpth=" libc "/include")
                      (string-append "-Dloclibpth=" libc "/lib")))))
        %standard-phases)))
    (native-search-paths (list (search-path-specification
                                (variable "PERL5LIB")
                                (files '("lib/perl5/site_perl")))))
    (synopsis "Implementation of the Perl programming language")
    (description
     "Perl 5 is a highly capable, feature-rich programming language with over
24 years of development.")
    (home-page "http://www.perl.org/")
    (license gpl1+)))                          ; or "Artistic"

(define-public perl-algorithm-diff
  (package
    (name "perl-algorithm-diff")
    (version "1.1903")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TY/TYEMQ/"
                           "Algorithm-Diff-" version ".tar.gz"))
       (sha256
        (base32
         "0l8pk7ziz72d022hsn4xldhhb9f5649j5cgpjdibch0xng24ms1h"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Algorithm-Diff")
    (synopsis "Compute differences between two files or lists")
    (description "This is a module for computing the difference between two
files, two strings, or any other two lists of things.  It uses an intelligent
algorithm similar to (or identical to) the one used by the Unix \"diff\"
program.  It is guaranteed to find the *smallest possible* set of
differences.")
    (license (package-license perl))))

(define-public perl-archive-zip
  (package
    (name "perl-archive-zip")
    (version "1.30")
    (source 
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AD/ADAMK/Archive-Zip-" 
             version ".tar.gz"))
       (sha256
        (base32
         "0633zah5z9njiqnvy3vh42fjymncmil1jdfb7d18w8xpfzzp5d7q"))))
    (build-system perl-build-system)
    (synopsis "Perl API to zip files")
    (description "The Archive::Zip module allows a Perl program to create,
manipulate, read, and write Zip archive files.")
    (home-page "http://search.cpan.org/~phred/Archive-Zip-1.37/lib/Archive/Zip.pm")
    (license (package-license perl))))

(define-public perl-b-hooks-endofscope
  (package
    (name "perl-b-hooks-endofscope")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "B-Hooks-EndOfScope-" version ".tar.gz"))
       (sha256
        (base32
         "1f5d0lbkwf23dfjn60g6fynmjhy5rxdyxcpdfb07srm73qpg2zpi"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-module-runtime" ,perl-module-runtime)
       ("perl-module-implementation" ,perl-module-implementation)
       ("perl-sub-exporter-progressive" ,perl-sub-exporter-progressive)
       ("perl-variable-magic" ,perl-variable-magic)))
    (home-page "http://search.cpan.org/dist/B-Hooks-EndOfScope")
    (synopsis "Execute code after a scope finished compilation")
    (description "This module allows you to execute code when perl finished
compiling the surrounding scope.")
    (license (package-license perl))))

(define-public perl-benchmark-timer
  (package
    (name "perl-benchmark-timer")
    (version "0.7102")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/D/DC/DCOPPIT/"
                                  "Benchmark-Timer-" version ".tar.gz"))
              (sha256
               (base32
                "1gl9ybm9hgia3ld5s11b7bv2p2hmx5rss5hxcfy6rmbzrjcnci01"))))
    (build-system perl-build-system)
    ;; The optional input module Statistics::PointEstimation (from
    ;; Statistics-TTest) lists no license.
    (synopsis "Benchmarking with statistical confidence")
    (description
     "The Benchmark::Timer class allows you to time portions of code
conveniently, as well as benchmark code by allowing timings of repeated
trials.  It is perfect for when you need more precise information about the
running time of portions of your code than the Benchmark module will give you,
but don't want to go all out and profile your code.")
    (home-page (string-append "http://search.cpan.org/~dcoppit/"
                              "Benchmark-Timer-" version))
    (license gpl2)))

(define-public perl-capture-tiny
  (package
    (name "perl-capture-tiny")
    (version "0.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DA/DAGOLDEN/Capture-Tiny-"
             version ".tar.gz"))
       (sha256
        (base32
         "117gmwipql1y5xnw9jil3lhdsrf2wsm9wjdzqj66x971n3fwm573"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Capture-Tiny")
    (synopsis "Capture STDOUT and STDERR from Perl, XS or external programs")
    (description
     "Capture::Tiny provides a simple, portable way to capture almost anything
sent to STDOUT or STDERR, regardless of whether it comes from Perl, from XS
code or from an external program.  Optionally, output can be teed so that it
is captured while being passed through to the original file handles.")
    (license asl2.0)))

(define-public perl-class-data-inheritable
  (package
    (name "perl-class-data-inheritable")
    (version "0.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TM/TMTM/"
                           "Class-Data-Inheritable-" version ".tar.gz"))
       (sha256
        (base32
         "0jpi38wy5xh6p1mg2cbyjjw76vgbccqp46685r27w8hmxb7gwrwr"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Class-Data-Inheritable")
    (synopsis "Inheritable, overridable class data")
    (description "Class::Data::Inheritable is for creating accessor/mutators
to class data.  That is, if you want to store something about your class as a
whole (instead of about a single object).  This data is then inherited by your
subclasses and can be overriden.")
    (license (package-license perl))))

(define-public perl-class-inspector
  (package
    (name "perl-class-inspector")
    (version "1.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AD/ADAMK/"
                           "Class-Inspector-" version ".tar.gz"))
       (sha256
        (base32
         "04iij8dbcgaim7g109frpyf7mh4ydsd8zh53r53chk0zxnivg91w"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Class-Inspector")
    (synopsis "Get information about a class and its structure")
    (description "Class::Inspector allows you to get information about a
loaded class.")
    (license (package-license perl))))

(define-public perl-class-load
  (package
    (name "perl-class-load")
    (version "0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Class-Load-" version ".tar.gz"))
       (sha256
        (base32
         "049i285yj8hwgzj7nncjbs2bhxvpdk88wmx1d0nh0rdmh5hdnlmy"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-package-stash" ,perl-package-stash)
       ("perl-data-optlist" ,perl-data-optlist)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-module-implementation" ,perl-module-implementation)))
    (home-page "http://search.cpan.org/dist/Class-Load")
    (synopsis "Working (require \"Class::Name\") and more")
    (description "\"require EXPR\" only accepts Class/Name.pm style module
names, not Class::Name.  For that, this module provides \"load_class
'Class::Name'\".")
    (license (package-license perl))))

(define-public perl-class-load-xs
  (package
    (name "perl-class-load-xs")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Class-Load-XS-" version ".tar.gz"))
       (sha256
        (base32
         "1aivalms81s3a2cj053ncgnmkpgl7vspna8ajlkqir7rdn8kpv5v"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (inputs `(("perl-class-load" ,perl-class-load)))
    (home-page "http://search.cpan.org/dist/Class-Load-XS")
    (synopsis "XS implementation of parts of Class::Load")
    (description "This module provides an XS implementation for portions of
Class::Load")
    (license artistic2.0)))

(define-public perl-class-tiny
  (package
    (name "perl-class-tiny")
    (version "1.000")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "Class-Tiny-" version ".tar.gz"))
       (sha256
        (base32
         "0jll90byj0nl16hwpf28k54i4n53jidjsj1bnlbx72v0n56qfpb2"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Class-Tiny")
    (synopsis "Minimalist class construction")
    (description "This module offers a minimalist class construction kit.  It
uses no non-core modules for any recent Perl.")
    (license asl2.0)))

(define-public perl-clone
  (package
    (name "perl-clone")
    (version "0.37")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/G/GA/GARU/"
                                  "Clone-" version ".tar.gz"))
              (sha256
               (base32
                "17fdhxpzrq2nwim3zkcrz4m9gjixp0i886yz54ysrshxy3k53wnr"))))
    (build-system perl-build-system)
    (synopsis "Recursively copy Perl datatypes")
    (description
     "This module provides a clone() method which makes recursive copies of
nested hash, array, scalar and reference types, including tied variables and
objects.")
    (home-page (string-append "http://search.cpan.org/~garu/"
                              "Clone-" version))
    (license (package-license perl))))

(define-public perl-config-autoconf
  (package
    (name "perl-config-autoconf")
    (version "0.309")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "Config-AutoConf-" version ".tar.gz"))
       (sha256
        (base32
         "1nqc7calfny12dwfhz7ylsvx55nf69kirdc5dbyvh3sjsqj8yvdq"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)))
    (home-page "http://search.cpan.org/dist/Config-AutoConf")
    (synopsis "Module to implement some AutoConf macros in Perl")
    (description "Config::AutoConf is intended to provide the same
opportunities to Perl developers as GNU Autoconf does for Shell developers.")
    (license (package-license perl))))

(define-public perl-cpan-meta-check
  (package
    (name "perl-cpan-meta-check")
    (version "0.009")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "CPAN-Meta-Check-" version ".tar.gz"))
       (sha256
        (base32
         "0qbk5dwvhd78qgq5x6nim2n0l78pylvlklpbrm56w9yss6pl6bgb"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-test-deep" ,perl-test-deep)))
    (propagated-inputs `(("perl-cpan-meta" ,perl-cpan-meta)))
    (home-page "http://search.cpan.org/dist/CPAN-Meta-Check")
    (synopsis "Verify requirements in a CPAN::Meta object")
    (description "This module verifies if requirements described in a
CPAN::Meta object are present.")
    (license (package-license perl))))

(define-public perl-data-optlist
  (package
    (name "perl-data-optlist")
    (version "0.109")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/R/RJ/RJBS/Data-OptList-"
             version ".tar.gz"))
       (sha256
        (base32
         "1j44rm2spprlq3bc80cxni3dzs3gfjiqv1qc9q7820n1qj0wgmqw"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-sub-install" ,perl-sub-install)
       ("perl-params-util" ,perl-params-util)))
    (home-page "http://search.cpan.org/dist/Data-OptList")
    (synopsis "Parse and validate simple name/value option pairs")
    (description
     "Data::OptList provides a simple syntax for name/value option pairs.")
    (license (package-license perl))))

(define-public perl-data-tumbler
  (package
    (name "perl-data-tumbler")
    (version "0.008")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "Data-Tumbler-" version ".tar.gz"))
       (sha256
        (base32
         "13kww2xj30rkk8w9h50h4blypdb689zgils0zyah587kip0z6509"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-most" ,perl-test-most)))
    (propagated-inputs
     `(("perl-file-homedir" ,perl-file-homedir)))
    (home-page "http://search.cpan.org/dist/Data-Tumbler")
    (synopsis "Dynamic generation of nested combinations of variants")
    (description "Data::Tumbler - Dynamic generation of nested combinations of
variants")
    (license (package-license perl))))

(define-public perl-devel-caller
  (package
    (name "perl-devel-caller")
    (version "2.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Devel-Caller-" version ".tar.gz"))
       (sha256
        (base32
         "1pxpimifzmnjnvf4icclx77myc15ahh0k56sj1djad1855mawwva"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-padwalker" ,perl-padwalker)))
    (home-page "http://search.cpan.org/dist/Devel-Caller")
    (synopsis "Meatier version of caller")
    (description "Devel::Caller provides meatier version of caller.")
    (license (package-license perl))))

(define-public perl-devel-checkbin
  (package
    (name "perl-devel-checkbin")
    (version "0.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOKUHIROM/"
                           "Devel-CheckBin-" version ".tar.gz"))
       (sha256
        (base32
         "0g71sma9jy0fjm619hcrcsb9spg2y03vjxx36y8k1xpa2553sr7m"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Devel-CheckBin")
    (synopsis "Check that a command is available")
    (description "Devel::CheckBin is a perl module that checks whether a
particular command is available.")
    (license (package-license perl))))

(define-public perl-devel-globaldestruction
  (package
    (name "perl-devel-globaldestruction")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Devel-GlobalDestruction-" version ".tar.gz"))
       (sha256
        (base32
         "0qn4iszgylnxjdkb6430f6a3ci7bcx9ih1az6bd5cbij1pf2965j"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-sub-exporter-progressive" ,perl-sub-exporter-progressive)))
    (home-page "http://search.cpan.org/dist/Devel-GlobalDestruction")
    (synopsis "Provides equivalent of ${^GLOBAL_PHASE} eq 'DESTRUCT' for older perls")
    (description "Devel::GlobalDestruction provides a function returning the
equivalent of \"${^GLOBAL_PHASE} eq 'DESTRUCT'\" for older perls.")
    (license (package-license perl))))

(define-public perl-devel-lexalias
  (package
    (name "perl-devel-lexalias")
    (version "0.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Devel-LexAlias-" version ".tar.gz"))
       (sha256
        (base32
         "0wpfpjqlrncslnmxa37494sfdy0901510kj2ds2k6q167vadj2jy"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-devel-caller" ,perl-devel-caller)))
    (home-page "http://search.cpan.org/dist/Devel-LexAlias")
    (synopsis "Alias lexical variables")
    (description "Devel::LexAlias provides the ability to alias a lexical
variable in a subroutines scope to one of your choosing.")
    (license (package-license perl))))

(define-public perl-devel-overloadinfo
  (package
    (name "perl-devel-overloadinfo")
    (version "0.002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "Devel-OverloadInfo-" version ".tar.gz"))
       (sha256
        (base32
         "14gzjlsqhypqp0szqj6152qfn69snzydgk1yk6bji5zimzv86qyy"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-package-stash" ,perl-package-stash)
       ("perl-sub-identify" ,perl-sub-identify)
       ("perl-mro-compat" ,perl-mro-compat)))
    (home-page "http://search.cpan.org/dist/Devel-OverloadInfo")
    (synopsis "Introspect overloaded operators")
    (description "Devel::OverloadInfo returns information about overloaded
operators for a given class (or object), including where in the inheritance
hierarchy the overloads are declared and where the code implementing it is.")
    (license (package-license perl))))

(define-public perl-devel-partialdump
  (package
    (name "perl-devel-partialdump")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Devel-PartialDump-" version ".tar.gz"))
       (sha256
        (base32
         "0nr3qa68x4yp219kd17j1ks9c95qc9agfvz7ddnpn8p78f3kgwfn"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-test-warn" ,perl-test-warn)
       ("perl-test-simple" ,perl-test-simple)))
    (propagated-inputs
     `(("perl-class-tiny" ,perl-class-tiny)
       ("perl-sub-exporter" ,perl-sub-exporter)
       ("perl-namespace-clean" ,perl-namespace-clean)))
    (home-page "http://search.cpan.org/dist/Devel-PartialDump")
    (synopsis "Partial dumping of data structures")
    (description "This module is a data dumper optimized for logging of
arbitrary parameters.")
    (license (package-license perl))))

(define-public perl-devel-stacktrace
  (package
    (name "perl-devel-stacktrace")
    (version "2.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Devel-StackTrace-" version ".tar.gz"))
       (sha256
        (base32
         "1r65iq5i11xh0r0kp3pdycydnd3kxpdmxnp0hq9hx9lr60kygsqx"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Devel-StackTrace")
    (synopsis "Object representing a stack trace")
    (description "The Devel::StackTrace module contains two classes,
Devel::StackTrace and Devel::StackTrace::Frame.  These objects encapsulate the
information that can be retrieved via Perl's caller() function, as well as
providing a simple interface to this data.")
    (license artistic2.0)))

(define-public perl-devel-stacktrace-ashtml
  (package
    (name "perl-devel-stacktrace-ashtml")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Devel-StackTrace-AsHTML-" version ".tar.gz"))
       (sha256
        (base32
         "0yl296y0qfwybwjgqjzd4j2w2bj5a2nz342qqgxchnf5bqynl1c9"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-devel-stacktrace" ,perl-devel-stacktrace)))
    (home-page "http://search.cpan.org/dist/Devel-StackTrace-AsHTML")
    (synopsis "Displays stack trace in HTML")
    (description "Devel::StackTrace::AsHTML adds as_html method to
Devel::StackTrace which displays the stack trace in beautiful HTML, with code
snippet context and function parameters.  If you call it on an instance of
Devel::StackTrace::WithLexicals, you even get to see the lexical variables of
each stack frame.")
    (license (package-license perl))))

(define-public perl-digest-sha1
  (package
    (name "perl-digest-sha1")
    (version "2.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/G/GA/GAAS/"
                                  "Digest-SHA1-" version ".tar.gz"))
              (sha256
               (base32
                "1k23p5pjk42vvzg8xcn4iwdii47i0qm4awdzgbmz08bl331dmhb8"))))
    (build-system perl-build-system)
    (synopsis "Perl implementation of the SHA-1 message digest algorithm")
    (description
     "This package provides 'Digest::SHA1', an implementation of the NIST
SHA-1 message digest algorithm for use by Perl programs.")
    (home-page (string-append "http://search.cpan.org/~gaas/Digest-SHA1-"
                              version "/SHA1.pm"))
    (license (package-license perl))))

(define-public perl-dist-checkconflicts
  (package
    (name "perl-dist-checkconflicts")
    (version "0.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                                  "Dist-CheckConflicts-" version ".tar.gz"))
              (sha256
               (base32
                "1i7dr9jpdiy2nijl2p4q5zg2q2s9ckbj2hs4kmnnckf9hsb4p17a"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-test-fatal" ,perl-test-fatal)))
    (propagated-inputs
     `(("perl-module-runtime" ,perl-module-runtime)))
    (home-page "http://search.cpan.org/dist/Dist-CheckConflicts")
    (synopsis "Declare version conflicts for your dist")
    (description "This module allows you to specify conflicting versions of
modules separately and deal with them after the module is done installing.")
    (license (package-license perl))))

(define-public perl-eval-closure
  (package
    (name "perl-eval-closure")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                           "Eval-Closure-" version ".tar.gz"))
       (sha256
        (base32
         "0ssvlgx3y1y28wrrp0lmmffzqxfrwb2lb3p60b8cjvxsf1c3jbfv"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-devel-lexalias" ,perl-devel-lexalias)))
    (home-page "http://search.cpan.org/dist/Eval-Closure")
    (synopsis "Safely and cleanly create closures via string eval")
    (description "String eval is often used for dynamic code generation.  For
instance, Moose uses it heavily, to generate inlined versions of accessors and
constructors, which speeds code up at runtime by a significant amount.  String
eval is not without its issues however - it's difficult to control the scope
it's used in (which determines which variables are in scope inside the eval),
and it's easy to miss compilation errors, since eval catches them and sticks
them in $@ instead.  This module attempts to solve these problems.  It
provides an eval_closure function, which evals a string in a clean
environment, other than a fixed list of specified variables.  Compilation
errors are rethrown automatically.")
    (license (package-license perl))))

(define-public perl-exception-class
  (package
    (name "perl-exception-class")
    (version "1.39")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Exception-Class-" version ".tar.gz"))
       (sha256
        (base32
         "10r06v6568s33p6h9f9ml0iabc07id86mjkf74gy7ld6d5m7b741"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-devel-stacktrace" ,perl-devel-stacktrace)
       ("perl-class-data-inheritable" ,perl-class-data-inheritable)))
    (home-page "http://search.cpan.org/dist/Exception-Class")
    (synopsis "Allows you to declare real exception classes in Perl")
    (description "Exception::Class allows you to declare exception hierarchies
in your modules in a \"Java-esque\" manner.")
    (license (package-license perl))))

(define-public perl-exporter-lite
  (package
    (name "perl-exporter-lite")
    (version "0.06")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                                  "Exporter-Lite-" version ".tar.gz"))
              (sha256
               (base32
                "0k4gkvid4fr8yvwj0axdx5111mzfw2iipls3qllxr364fqhmclpj"))))
    (build-system perl-build-system)
    (synopsis "Lightweight exporting of functions and variables")
    (description
     "Exporter::Lite is an alternative to Exporter, intended to provide a
lightweight subset of the most commonly-used functionality.  It supports
import(), @EXPORT and @EXPORT_OK and not a whole lot else.")
    (home-page (string-append "http://search.cpan.org/~neilb/"
                              "Exporter-Lite-" version))
    (license (package-license perl))))

(define-public perl-exporter-tiny
  (package
    (name "perl-exporter-tiny")
    (version "0.042")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOBYINK/"
                           "Exporter-Tiny-" version ".tar.gz"))
       (sha256
        (base32
         "0gq2ia8c6n84gdrlc73vab61djs8gs8zf7fqx8cxbg5zxg2j45lg"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Exporter-Tiny")
    (synopsis "Exporter with the features of Sub::Exporter but only core dependencies")
    (description "Exporter::Tiny supports many of Sub::Exporter's
external-facing features including renaming imported functions with the `-as`,
`-prefix` and `-suffix` options; explicit destinations with the `into` option;
and alternative installers with the `installler` option.  But it's written in
only about 40% as many lines of code and with zero non-core dependencies.")
    (license (package-license perl))))

(define-public perl-extutils-installpaths
  (package
    (name "perl-extutils-installpaths")
    (version "0.010")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "ExtUtils-InstallPaths-" version ".tar.gz"))
       (sha256
        (base32
         "0mi1px42in7i442jqncg3gmxd5zn7sw5b2s85h690rz433qvyk6i"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-extutils-config" ,perl-extutils-config)))
    (home-page "http://search.cpan.org/dist/ExtUtils-InstallPaths")
    (synopsis "Build.PL install path logic made easy")
    (description "This module tries to make install path resolution as easy as
possible.")
    (license (package-license perl))))

(define-public perl-extutils-config
  (package
    (name "perl-extutils-config")
    (version "0.008")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "ExtUtils-Config-" version ".tar.gz"))
       (sha256
        (base32
         "130s5zk4krrymbynqxx62g13jynnb7xi7vdpg65cw3b56kv08ldf"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/ExtUtils-Config")
    (synopsis "Wrapper for perl's configuration")
    (description "ExtUtils::Config is an abstraction around the %Config hash.
By itself it is not a particularly interesting module by any measure, however
it ties together a family of modern toolchain modules.")
    (license (package-license perl))))

(define-public perl-extutils-helpers
  (package
    (name "perl-extutils-helpers")
    (version "0.022")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "ExtUtils-Helpers-" version ".tar.gz"))
       (sha256
        (base32
         "15dalfwmpfmifw312i5pwiai8134pxf7b2804shlqhdk1xqczy6k"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/ExtUtils-Helpers")
    (synopsis "Various portability utilities for module builders")
    (description "This module provides various portable helper functions for
module building modules.")
    (license (package-license perl))))

(define-public perl-file-find-rule
  (package
    (name "perl-file-find-rule")
    (version "0.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "File-Find-Rule-" version ".tar.gz"))
       (sha256
        (base32
         "0w73b4jr2fcrd74a1w3b2jryq3mqzc8z5mk7ia9p85xn3qmpa5r4"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-text-glob" ,perl-text-glob)
       ("perl-number-compare" ,perl-number-compare)))
    (home-page "http://search.cpan.org/dist/File-Find-Rule")
    (synopsis "Alternative interface to File::Find")
    (description "File::Find::Rule is a friendlier interface to File::Find.
It allows you to build rules which specify the desired files and
directories.")
    (license (package-license perl))))

(define-public perl-file-find-rule-perl
  (package
    (name "perl-file-find-rule-perl")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AD/ADAMK/"
                           "File-Find-Rule-Perl-" version ".tar.gz"))
       (sha256
        (base32
         "0xi4ppqr6r57l5xlkwxpvkvpb9p7dvz053d76v2m9pwdfxqb5v6j"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-find-rule" ,perl-file-find-rule)
       ("perl-params-util" ,perl-params-util)
       ("perl-parse-cpan-meta" ,perl-parse-cpan-meta)))
    (home-page "http://search.cpan.org/dist/File-Find-Rule-Perl")
    (synopsis "Common rules for searching for Perl things")
    (description "File::Find::Rule::Perl provides methods for finding various
types Perl-related files, or replicating search queries run on a distribution
in various parts of the CPAN ecosystem.")
    (license (package-license perl))))

(define-public perl-file-homedir
  (package
    (name "perl-file-homedir")
    (version "1.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AD/ADAMK/"
                           "File-HomeDir-" version ".tar.gz"))
       (sha256
        (base32
         "0hvihydd0y4gdxafi8dpybk9ll8q35bz5ycibfic0gh92cslzfc5"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-which" ,perl-file-which)
       ("perl-file-temp" ,perl-file-temp)))
    (arguments `(#:tests? #f))          ;Not appropriate for chroot
    (home-page "http://search.cpan.org/dist/File-HomeDir")
    (synopsis "Find your home and other directories on any platform")
    (description "File::HomeDir is a module for locating the directories that
are \"owned\" by a user (typicaly your user) and to solve the various issues
that arise trying to find them consistently across a wide variety of
platforms.")
    (license (package-license perl))))

(define-public perl-file-list
  (package
    (name "perl-file-list")
    (version "0.3.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/D/DO/DOPACKI/File-List-"
                   version ".tar.gz"))
             (sha256
              (base32
               "00m5ax4aq59hdvav6yc4g63vhx3a57006rglyypagvrzfxjvm8s8"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (alist-cons-after
        'unpack 'cd
        (lambda* _
         (chdir "List"))
       %standard-phases)))
    (license (package-license perl))
    (synopsis "Perl extension for crawling directory trees and compiling
lists of files")
    (description
     "The File::List module crawls the directory tree starting at the
provided base directory and can return files (and/or directories if desired)
matching a regular expression.")
    (home-page "http://search.cpan.org/~dopacki/File-List/")))

(define-public perl-file-sharedir
  (package
    (name "perl-file-sharedir")
    (version "1.102")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "File-ShareDir-" version ".tar.gz"))
       (sha256
        (base32
         "04blqn4cms9zjmhlfvwyx6mrglaaq1mmy4xwv7xqf9c8fjwk8wvw"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-file-sharedir-install" ,perl-file-sharedir-install)))
    (propagated-inputs
     `(("perl-class-inspector" ,perl-class-inspector)))
    (home-page "http://search.cpan.org/dist/File-ShareDir")
    (synopsis "Locate per-dist and per-module shared files")
    (description "The intent of File::ShareDir is to provide a companion to
Class::Inspector and File::HomeDir.  Quite often you want or need your Perl
module to have access to a large amount of read-only data that is stored on
the file-system at run-time.  Once the files have been installed to the
correct directory, you can use File::ShareDir to find your files again after
the installation.")
    (license (package-license perl))))

(define-public perl-file-sharedir-install
  (package
    (name "perl-file-sharedir-install")
    (version "0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GW/GWYN/"
                           "File-ShareDir-Install-" version ".tar.gz"))
       (sha256
        (base32
         "1xz60bi7x8755lq24rx7y1jkyk3icssn7s55z665mysdxhfzg2kh"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/File-ShareDir-Install")
    (synopsis "Install shared files")
    (description "File::ShareDir::Install allows you to install read-only data
files from a distribution.  It is a companion module to File::ShareDir, which
allows you to locate these files after installation.")
    (license (package-license perl))))

(define-public perl-file-temp
  (package
    (name "perl-file-temp")
    (version "0.2304")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "File-Temp-" version ".tar.gz"))
       (sha256
        (base32
         "1b11scbw77924awwdf5yw8sk8z0s2hskvpyyxws9yz4gwhim6h8k"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-parent" ,perl-parent)))
    (home-page "http://search.cpan.org/dist/File-Temp")
    (synopsis "Return name and handle of a temporary file safely")
    (description "File::Temp can be used to create and open temporary files in
a safe way.")
    (license (package-license perl))))

(define-public perl-file-which
  (package
    (name "perl-file-which")
    (version "1.09")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AD/ADAMK/"
                                  "File-Which-" version ".tar.gz"))
              (sha256
               (base32
                "1hxjyh9yrv32f3g8vrnr8iylzprajsac14vjm75kf1qnj1jyqbxp"))))
    (build-system perl-build-system)
    (native-inputs `(("test-script" ,perl-test-script)))
    (synopsis "Portable implementation of the `which' utility")
    (description
     "File::Which was created to be able to get the paths to executable
programs on systems under which the `which' program wasn't implemented in the
shell.")
    (home-page (string-append "http://search.cpan.org/~adamk/"
                              "File-Which-" version))
    (license (package-license perl))))

(define-public perl-getopt-tabular
  (package
    (name "perl-getopt-tabular")
    (version "0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/G/GW/GWARD/"
                                  "Getopt-Tabular-" version ".tar.gz"))
              (sha256
               (base32
                "0xskl9lcj07sdfx5dkma5wvhhgf5xlsq0khgh8kk34dm6dv0dpwv"))))
    (build-system perl-build-system)
    (synopsis "Table-driven argument parsing for Perl")
    (description
     "Getopt::Tabular is a Perl 5 module for table-driven argument parsing,
vaguely inspired by John Ousterhout's Tk_ParseArgv.")
    (home-page (string-append "http://search.cpan.org/~gward/"
                              "Getopt-Tabular-" version))
    (license (package-license perl))))

(define-public perl-hash-multivalue
  (package
    (name "perl-hash-multivalue")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Hash-MultiValue-" version ".tar.gz"))
       (sha256
        (base32
         "1jc37kwpa1fl88va8bd1p95h0vjv1gsvmn7pc2pxj62ga6x0wpc0"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Hash-MultiValue")
    (synopsis "Store multiple values per key")
    (description "Hash::MultiValue is an object (and a plain hash reference)
that may contain multiple values per key, inspired by MultiDict of WebOb.")
    (license (package-license perl))))

(define-public perl-inc-latest
  (package
    (name "perl-inc-latest")
    (version "0.500")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "inc-latest-" version ".tar.gz"))
       (sha256
        (base32
         "04f6qf6ll2hkdsr9aglykg3wlgsnf0w4f264nzg4i9y6cgrhbafs"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/inc-latest")
    (synopsis "Use modules in inc/ if newer than installed")
    (description "The inc::latest module helps bootstrap configure-time
dependencies for CPAN distributions.  These dependencies get bundled into the
inc directory within a distribution and are used by Makefile.PL or Build.PL.")
    (license asl2.0)))

(define-public perl-io-tty
  (package
    (name "perl-io-tty")
    (version "1.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/T/TO/TODDR/IO-Tty-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0lgd9xcbi4gf4gw1ka6fj94my3w1f3k1zamb4pfln0qxz45zlxx4"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/~toddr/IO-Tty/")
    (synopsis "Perl interface to pseudo ttys")
    (description
     "This package provides the 'IO::Pty' and 'IO::Tty' Perl interfaces to
pseudo ttys.")
    (license (package-license perl))))

(define-public perl-ipc-run3
  (package
    (name "perl-ipc-run3")
    (version "0.048")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                                  "IPC-Run3-" version ".tar.gz"))
              (sha256
               (base32
                "0r9m8q78bg7yycpixd7738jm40yz71p2q7inm766kzsw3g6c709x"))))
    (build-system perl-build-system)
    (synopsis "Run a subprocess with input/ouput redirection")
    (description
     "The IPC::Run3 module allows you to run a subprocess and redirect stdin,
stdout, and/or stderr to files and perl data structures.  It aims to satisfy
99% of the need for using system, qx, and open3 with a simple, extremely
Perlish API and none of the bloat and rarely used features of IPC::Run.")
    (home-page (string-append "http://search.cpan.org/~rjbs/"
                              "IPC-Run3-" version))
    ;; "You may use this module under the terms of the BSD, Artistic, or GPL
    ;; licenses, any version."
    (license (list bsd-3 gpl3+))))

(define-public perl-list-moreutils
  (package
    (name "perl-list-moreutils")
    (version "0.402")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "List-MoreUtils-" version ".tar.gz"))
       (sha256
        (base32
         "1i0k7kqg1m9nf2xvq9l4lyf38fxvi9952vmmvhcdaf3qa95pxb24"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-config-autoconf" ,perl-config-autoconf)
       ("perl-inc-latest" ,perl-inc-latest)
       ("perl-test-writevariants" ,perl-test-writevariants)))
    (propagated-inputs
     `(("perl-exporter-tiny" ,perl-exporter-tiny)))
    (home-page "http://search.cpan.org/dist/List-MoreUtils")
    (synopsis "Provide the stuff missing in List::Util")
    (description "List::MoreUtils provides some trivial but commonly needed
functionality on lists which is not going to go into List::Util.")
    (license (package-license perl))))

(define-public perl-module-build-tiny
  (package
    (name "perl-module-build-tiny")
    (version "0.039")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "Module-Build-Tiny-" version ".tar.gz"))
       (sha256
        (base32
         "077ijxbvamybph4ymamy1i9q2993xb46vf1npxaybjz0mkv0yn3x"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-extutils-installpaths" ,perl-extutils-installpaths)
       ("perl-extutils-config" ,perl-extutils-config)
       ("perl-extutils-helpers" ,perl-extutils-helpers)
       ("perl-test-harness" ,perl-test-harness)))
    (propagated-inputs
     `(("perl-extutils-installpaths" ,perl-extutils-installpaths)
       ("perl-extutils-config" ,perl-extutils-config)
       ("perl-extutils-helpers" ,perl-extutils-helpers)
       ("perl-test-harness" ,perl-test-harness)))
    (home-page "http://search.cpan.org/dist/Module-Build-Tiny")
    (synopsis "Tiny replacement for Module::Build")
    (description "Many Perl distributions use a Build.PL file instead of a
Makefile.PL file to drive distribution configuration, build, test and
installation.  Traditionally, Build.PL uses Module::Build as the underlying
build system.  This module provides a simple, lightweight, drop-in
replacement.  Whereas Module::Build has over 6,700 lines of code; this module
has less than 120, yet supports the features needed by most distributions.")
    (license (package-license perl))))

(define-public perl-module-implementation
  (package
    (name "perl-module-implementation")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Module-Implementation-" version ".tar.gz"))
       (sha256
        (base32
         "0vfngw4dbryihqhi7g9ks360hyw8wnpy3hpkzyg0q4y2y091lpy1"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-module-runtime" ,perl-module-runtime)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "http://search.cpan.org/dist/Module-Implementation")
    (synopsis "Loads alternate underlying implementations for a module")
    (description "This module abstracts out the process of choosing one of
several underlying implementations for a module.  This can be used to provide
XS and pure Perl implementations of a module, or it could be used to load an
implementation for a given OS or any other case of needing to provide multiple
implementations.")
    (license artistic2.0)))

(define-public perl-module-runtime
  (package
    (name "perl-module-runtime")
    (version "0.014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/Z/ZE/ZEFRAM/"
                           "Module-Runtime-" version ".tar.gz"))
       (sha256
        (base32
         "19326f094jmjs6mgpwkyisid54k67w34br8yfh0gvaaml87gwi2c"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Module-Runtime")
    (synopsis "Perl runtime module handling")
    (description "The functions exported by this module deal with runtime
handling of Perl modules, which are normally handled at compile time.")
    (license (package-license perl))))

(define-public perl-module-runtime-conflicts
  (package
    (name "perl-module-runtime-conflicts")
    (version "0.001")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Module-Runtime-Conflicts-" version ".tar.gz"))
       (sha256
        (base32
         "0pz23ch78lbpn4kdbm04icgsmbr7jvmxwq1p5m4x2pap8qwd0wqg"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-module-runtime" ,perl-module-runtime)
       ("perl-dist-checkconflicts" ,perl-dist-checkconflicts)))
    (home-page "http://search.cpan.org/dist/Module-Runtime-Conflicts")
    (synopsis "Provide information on conflicts for Module::Runtime")
    (description "This module provides conflicts checking for Module::Runtime,
which had a recent release that broke some versions of Moose.  It is called
from Moose::Conflicts and moose-outdated.")
    (license (package-license perl))))

(define-public perl-moose
  (package
    (name "perl-moose")
    (version "2.1403")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                                  "Moose-" version ".tar.gz"))
              (sha256
               (base32
                "16iaazikbnq2jjjac84jrdpfzm4qwqg1nbfgs11jlwn84q4jp1n3"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-cpan-meta-check" ,perl-cpan-meta-check)
       ("perl-dist-checkconflicts" ,perl-dist-checkconflicts)
       ("perl-test-cleannamespaces" ,perl-test-cleannamespaces)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-test-warnings" ,perl-test-warnings)))
    ;; XXX::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ;; # === Other Modules ===
    ;; #
    ;; #     Module                       Want    Have
    ;; #     ---------------------------- ---- -------
    ;; #     Algorithm::C3                 any missing
    ;; #     DBM::Deep                     any missing
    ;; #     DateTime                      any missing
    ;; #     DateTime::Calendar::Mayan     any missing
    ;; #     DateTime::Format::MySQL       any missing
    ;; #     Declare::Constraints::Simple  any missing
    ;; #     Dist::CheckConflicts          any    0.11
    ;; #     HTTP::Headers                 any missing
    ;; #     IO::File                      any    1.16
    ;; #     IO::String                    any missing
    ;; #     Locale::US                    any missing
    ;; #     Module::Refresh               any missing
    ;; #     MooseX::NonMoose              any missing
    ;; #     Params::Coerce                any missing
    ;; #     Regexp::Common                any missing
    ;; #     SUPER                         any missing
    ;; #     Test::Deep                    any missing
    ;; #     Test::DependentModules        any missing
    ;; #     Test::LeakTrace               any missing
    ;; #     Test::Output                  any missing
    ;; #     URI                           any missing
    (propagated-inputs
     `(("perl-class-load" ,perl-class-load)
       ("perl-class-load-xs" ,perl-class-load-xs)
       ("perl-data-optlist" ,perl-data-optlist)
       ("perl-devel-globaldestruction" ,perl-devel-globaldestruction)
       ("perl-devel-overloadinfo" ,perl-devel-overloadinfo)
       ("perl-devel-partialdump" ,perl-devel-partialdump)
       ("perl-devel-stacktrace" ,perl-devel-stacktrace)
       ("perl-dist-checkconflicts" ,perl-dist-checkconflicts)
       ("perl-eval-closure" ,perl-eval-closure)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-module-runtime-conflicts" ,perl-module-runtime-conflicts)
       ("perl-mro-compat" ,perl-mro-compat)
       ("perl-package-deprecationmanager" ,perl-package-deprecationmanager)
       ("perl-package-stash" ,perl-package-stash)
       ("perl-package-stash-xs" ,perl-package-stash-xs)
       ("perl-params-util" ,perl-params-util)
       ("perl-parent" ,perl-parent)
       ("perl-scalar-list-utils" ,perl-scalar-list-utils)
       ("perl-sub-exporter" ,perl-sub-exporter)
       ("perl-sub-name" ,perl-sub-name)
       ("perl-task-weaken" ,perl-task-weaken)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "http://search.cpan.org/dist/Moose")
    (synopsis "Postmodern object system for Perl 5")
    (description
     "Moose is a complete object system for Perl 5.  It provides keywords for
attribute declaration, object construction, inheritance, and maybe more.  With
Moose, you define your class declaratively, without needing to know about
blessed hashrefs, accessor methods, and so on.  You can concentrate on the
logical structure of your classes, focusing on \"what\" rather than \"how\".
A class definition with Moose reads like a list of very concise English
sentences.")
    (license (package-license perl))))

(define-public perl-mro-compat
  (package
    (name "perl-mro-compat")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BO/BOBTFISH/"
                           "MRO-Compat-" version ".tar.gz"))
       (sha256
        (base32
         "1mhma2g83ih9f8nkmg2k9l0x6izhhbb6k5lli4rpllxad4wbk9dv"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/MRO-Compat")
    (synopsis "MRO interface compatibility for Perls < 5.9.5")
    (description "The \"mro\" namespace provides several utilities for dealing
with method resolution order and method caching in general in Perl 5.9.5 and
higher.  This module provides those interfaces for earlier versions of
Perl (back to 5.6.0).")
    (license (package-license perl))))

(define-public perl-namespace-clean
  (package
    (name "perl-namespace-clean")
    (version "0.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RI/RIBASUSHI/"
                           "namespace-clean-" version ".tar.gz"))
       (sha256
        (base32
         "016dds70ql1mp18b07chkxiy4drn976ibnbshqc2hmhrh9xjnsll"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-package-stash" ,perl-package-stash)
       ("perl-b-hooks-endofscope" ,perl-b-hooks-endofscope)))
    (home-page "http://search.cpan.org/dist/namespace-clean")
    (synopsis "Keep imports and functions out of your namespace")
    (description "The namespace::clean pragma will remove all previously
declared or imported symbols at the end of the current package's compile
cycle.  Functions called in the package itself will still be bound by their
name, but they won't show up as methods on your class or instances.")
    (license (package-license perl))))

(define-public perl-number-compare
  (package
    (name "perl-number-compare")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Number-Compare-" version ".tar.gz"))
       (sha256
        (base32
         "09q8i0mxvr7q9vajwlgawsi0hlpc119gnhq4hc933d03x0vkfac3"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Number-Compare")
    (synopsis "Numeric comparisons")
    (description "Number::Compare compiles a simple comparison to an anonymous
subroutine, which you can call with a value to be tested against.")
    (license (package-license perl))))

(define-public perl-package-anon
  (package
    (name "perl-package-anon")
    (version "0.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AU/AUGGY/"
                           "Package-Anon-" version ".tar.gz"))
       (sha256
        (base32
         "1fj1fakkfklf2iwzsl64vfgshya3jgm6vhxiphw12wlac9g2il0m"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-sub-exporter" ,perl-sub-exporter)
       ("perl-params-util" ,perl-params-util)))
    (home-page "http://search.cpan.org/dist/Package-Anon")
    (synopsis "Anonymous packages")
    (description "This module allows for anonymous packages that are
independent of the main namespace and only available through an object
instance, not by name.")
    (license (package-license perl))))

(define-public perl-package-deprecationmanager
  (package
    (name "perl-package-deprecationmanager")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Package-DeprecationManager-" version ".tar.gz"))
       (sha256
        (base32
         "0fkvq3xxwc3l5hg64dr9sj3l12dl59i44cg407qx9sd6r51j3qfi"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-test-output" ,perl-test-output)))
    (propagated-inputs
     `(("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-params-util" ,perl-params-util)
       ("perl-sub-install" ,perl-sub-install)))
    (arguments `(#:tests? #f))          ;XXX: Failing for some reason...
    (home-page "http://search.cpan.org/dist/Package-DeprecationManager")
    (synopsis "Manage deprecation warnings for your distribution")
    (description "This module allows you to manage a set of deprecations for
one or more modules.")
    (license artistic2.0)))

(define-public perl-package-stash
  (package
    (name "perl-package-stash")
    (version "0.37")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                           "Package-Stash-" version ".tar.gz"))
       (sha256
        (base32
         "0b3vg2nbzmz1m5qla4123rmfzmpfmwxkw78fghvwsc4iiww0baq6"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-dist-checkconflicts" ,perl-dist-checkconflicts)
       ("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-package-anon" ,perl-package-anon)))
    (propagated-inputs
     `(("perl-module-implementation" ,perl-module-implementation)
       ("perl-dist-checkconflicts" ,perl-dist-checkconflicts)
       ("perl-package-stash-xs" ,perl-package-stash-xs)))
    (home-page "http://search.cpan.org/dist/Package-Stash")
    (synopsis "Routines for manipulating stashes")
    (description "Manipulating stashes (Perl's symbol tables) is occasionally
necessary, but incredibly messy, and easy to get wrong.  This module hides all
of that behind a simple API.")
    (license (package-license perl))))

(define-public perl-package-stash-xs
  (package
    (name "perl-package-stash-xs")
    (version "0.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                           "Package-Stash-XS-" version ".tar.gz"))
       (sha256
        (base32
         "11nl69n8i56p91pd0ia44ip0vpv2cxwpbfakrv01vvv8az1cbn13"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-fatal" ,perl-test-fatal)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-package-anon" ,perl-package-anon)))
    (home-page "http://search.cpan.org/dist/Package-Stash-XS")
    (synopsis "Faster implementation of the Package::Stash API")
    (description "This is a backend for Package::Stash, which provides the
functionality in a way that's less buggy and much faster.  It will be used by
default if it's installed, and should be preferred in all environments with a
compiler.")
    (license (package-license perl))))

(define-public perl-padwalker
  (package
    (name "perl-padwalker")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RO/ROBIN/"
                           "PadWalker-" version ".tar.gz"))
       (sha256
        (base32
         "058l78rkr6px3rqcv2sdf9sqimdq1nc6py5yb9rrg3wmva7crw84"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/PadWalker")
    (synopsis "Play with other peoples' lexical variables")
    (description "PadWalker is a module which allows you to inspect (and even
change) lexical variables in any subroutine which called you.  It will only
show those variables which are in scope at the point of the call.  PadWalker
is particularly useful for debugging.")
    (license (package-license perl))))

(define-public perl-params-util
  (package
    (name "perl-params-util")
    (version "1.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AD/ADAMK/Params-Util-"
             version ".tar.gz"))
       (sha256
        (base32
         "0v67sx93yhn7xa0nh9mnbf8mixf54czk6wzrjsp6dzzr5hzyrw9h"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Params-Util")
    (synopsis "Simple, compact and correct param-checking functions")
    (description
     "Params::Util provides a basic set of importable functions that makes
checking parameters easier.")
    (license (package-license perl))))

(define-public perl-parent
  (package
    (name "perl-parent")
    (version "0.228")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CO/CORION/"
                           "parent-" version ".tar.gz"))
       (sha256
        (base32
         "0w0i02y4z8465z050kml57mvhv7c5gl8w8ivplhr3cms0zbaq87b"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/parent")
    (synopsis "Establish an ISA relationship with base classes at compile time")
    (description "Allows you to both load one or more modules, while setting
up inheritance from those modules at the same time.")
    (license (package-license perl))))

(define-public perl-posix-strftime-compiler
  (package
    (name "perl-posix-strftime-compiler")
    (version "0.41")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KA/KAZEBURO/"
                           "POSIX-strftime-Compiler-" version ".tar.gz"))
       (sha256
        (base32
         "0f9p3hx0vqx8zg5v24pz0s4zc8ln100c7c91ks681wq02phqj2v7"))))
    (build-system perl-build-system)
    (arguments `(#:tests? #f))          ;TODO: Timezone test failures
    (home-page "http://search.cpan.org/dist/POSIX-strftime-Compiler")
    (synopsis "GNU C library compatible strftime for loggers and servers")
    (description "POSIX::strftime::Compiler provides GNU C library compatible
strftime(3).  But this module is not affected by the system locale.  This
feature is useful when you want to write loggers, servers, and portable
applications.")
    (license (package-license perl))))

(define-public perl-probe-perl
  (package
    (name "perl-probe-perl")
    (version "0.03")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/K/KW/KWILLIAMS/"
                                  "Probe-Perl-" version ".tar.gz"))
              (sha256
               (base32
                "0c9wiaz0mqqknafr4jdr0g2gdzxnn539182z0icqaqvp5qgd5r6r"))))
    (build-system perl-build-system)
    (synopsis "Information about the currently running perl")
    (description
     "Probe::Perl provides methods for obtaining information about the
currently running perl interpreter.  It originally began life as code in the
Module::Build project, but has been externalized here for general use.")
    (home-page (string-append "http://search.cpan.org/~kwilliams/"
                              "Probe-Perl-" version))
    (license (package-license perl))))

(define-public perl-regexp-common
  (package
    (name "perl-regexp-common")
    (version "2013031301")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AB/ABIGAIL/"
                                  "Regexp-Common-" version ".tar.gz"))
              (sha256
               (base32
                "112wybsm0vr8yfannx6sdfvgp5vza28gjgr3pgn69ak4sac836kj"))))
    (build-system perl-build-system)
    (synopsis "Provide commonly requested regular expressions")
    (description
     "This module exports a single hash (`%RE') that stores or generates
commonly needed regular expressions.  Patterns currently provided include:
balanced parentheses and brackets, delimited text (with escapes), integers and
floating-point numbers in any base (up to 36), comments in 44 languages,
offensive language, lists of any pattern, IPv4 addresses, URIs, and Zip
codes.")
    (home-page (string-append "http://search.cpan.org/~abigail/"
                              "Regexp-Common-" version))
    ;; Quad-licensed: Perl Artistic, Perl Artistic 2.0, X11, and BSD.
    (license (list (package-license perl) x11 bsd-3))))

(define-public perl-sub-exporter
  (package
    (name "perl-sub-exporter")
    (version "0.987")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/R/RJ/RJBS/Sub-Exporter-"
             version ".tar.gz"))
       (sha256
        (base32
         "1ml3n1ck4ln9qjm2mcgkczj1jb5n1fkscz9c4x23v4db0glb4g2l"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-data-optlist" ,perl-data-optlist)
       ("perl-params-util" ,perl-params-util)))
    (home-page "http://search.cpan.org/dist/Sub-Exporter")
    (synopsis "Sophisticated exporter for custom-built routines")
    (description
     "Sub::Exporter provides a sophisticated alternative to Exporter.pm for
custom-built routines.")
    (license (package-license perl))))

(define-public perl-sub-exporter-progressive
  (package
    (name "perl-sub-exporter-progressive")
    (version "0.001011")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FR/FREW/"
                           "Sub-Exporter-Progressive-" version ".tar.gz"))
       (sha256
        (base32
         "01kwzbqwdhvadpphnczid03nlyj0h4cxaq3m3v2401bckkkcc606"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-sub-exporter" ,perl-sub-exporter)))
    (home-page "http://search.cpan.org/dist/Sub-Exporter-Progressive")
    (synopsis "Only use Sub::Exporter if you need it")
    (description "Sub::Exporter is an incredibly powerful module, but with
that power comes great responsibility, as well as some runtime penalties.
This module is a \"Sub::Exporter\" wrapper that will let your users just use
Exporter if all they are doing is picking exports, but use \"Sub::Exporter\"
if your users try to use \"Sub::Exporter\"'s more advanced features, like
renaming exports, if they try to use them.")
    (license (package-license perl))))

(define-public perl-sub-identify
  (package
    (name "perl-sub-identify")
    (version "0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RG/RGARCIA/"
                           "Sub-Identify-" version ".tar.gz"))
       (sha256
        (base32
         "087fjcg6w576w47i1slj6mjfd3gl1b0airgddmn3prn0nff6nn2m"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Sub-Identify")
    (synopsis "Retrieve names of code references")
    (description "Sub::Identify allows you to retrieve the real name of code
references.")
    (license (package-license perl))))

(define-public perl-sub-install
  (package
    (name "perl-sub-install")
    (version "0.928")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/R/RJ/RJBS/Sub-Install-"
             version ".tar.gz"))
       (sha256
        (base32
         "03zgk1yh128gciyx3q77zxzxg9kf8yy2gm46gdxqi24mcykngrb1"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Sub-Install")
    (synopsis "Install subroutines into packages easily")
    (description
     "Sub::Install makes it easy to install subroutines into packages without
the unsightly mess of C<no strict> or typeglobs lying about where just anyone
can see them.")
    (license (package-license perl))))

(define-public perl-sub-name
  (package
    (name "perl-sub-name")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Sub-Name-" version ".tar.gz"))
       (sha256
        (base32
         "1sdlc8pv7vyyc48gzh70hbwzn0hzwl3zbcy2dkmfw8vjzgya5i06"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-devel-checkbin" ,perl-devel-checkbin)))
    (home-page "http://search.cpan.org/dist/Sub-Name")
    (synopsis "(Re)name a sub")
    (description "Assigns a new name to referenced sub.  If package
specification is omitted in the name, then the current package is used.  The
return value is the sub.")
    (license (package-license perl))))

(define-public perl-sub-uplevel
  (package
    (name "perl-sub-uplevel")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "Sub-Uplevel-" version ".tar.gz"))
       (sha256
        (base32
         "1yzxqsim8vpavzqm2wfksh8dpmy6qbr9s3hdqqicp38br3lzd4qg"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Sub-Uplevel")
    (synopsis "Apparently run a function in a higher stack frame")
    (description "Like Tcl's uplevel() function, but not quite so dangerous.
The idea is just to fool caller().  All the really naughty bits of Tcl's
uplevel() are avoided.")
    (license (package-license perl))))

(define-public perl-sys-cpu
  (package
    (name "perl-sys-cpu")
    (version "0.61")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MZ/MZSANFORD/"
                                  "Sys-CPU-" version ".tar.gz"))
              (sha256
               (base32
                "1r6976bs86j7zp51m5vh42xlyah951jgdlkimv202413kjvqc2i5"))))
    (build-system perl-build-system)
    (synopsis "Perl extension for getting CPU information")
    (description
     "In responce to a post on perlmonks.org, a module for counting the number
of CPU's on a system.  Support has now also been added for type of CPU and
clock speed.")
    (home-page (string-append "http://search.cpan.org/~mzsanford/"
                              "Sys-CPU-" version))
    (license (package-license perl))))

(define-public perl-task-weaken
  (package
    (name "perl-task-weaken")
    (version "1.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AD/ADAMK/"
                           "Task-Weaken-" version ".tar.gz"))
       (sha256
        (base32
         "1i7kd9v8fjsqyhr4rx4a1jv7n5vfjjm1v4agb24pizh0b72p3qk7"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Task-Weaken")
    (synopsis "Ensure that a platform has weaken support")
    (description "One recurring problem in modules that use Scalar::Util's
weaken function is that it is not present in the pure-perl variant.  If
Scalar::Util is not available at all, it will issue a normal dependency on the
module.  However, if Scalar::Util is relatively new ( it is >= 1.19 ) and the
module does not have weaken, the install will bail out altogether with a long
error encouraging the user to seek support.")
    (license (package-license perl))))

(define-public perl-test-cleannamespaces
  (package
    (name "perl-test-cleannamespaces")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Test-CleanNamespaces-" version ".tar.gz"))
       (sha256
        (base32
         "1ynrds515gcq954z34zm03rgcx0dskiaz7qj0k7k5gmrjj1kfycp"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-requires" ,perl-test-requires)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-warnings" ,perl-test-warnings)
       ("perl-test-tester" ,perl-test-tester)))
    (propagated-inputs
     `(("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-package-stash" ,perl-package-stash)
       ("perl-sub-identify" ,perl-sub-identify)
       ("perl-sub-exporter" ,perl-sub-exporter)
       ("perl-file-find-rule" ,perl-file-find-rule)
       ("perl-file-find-rule-perl" ,perl-file-find-rule-perl)))
    (home-page "http://search.cpan.org/dist/Test-CleanNamespaces")
    (synopsis "Check for uncleaned imports")
    (description "This module lets you check your module's namespaces for
imported functions you might have forgotten to remove with
namespace::autoclean or namespace::clean and are therefore available to be
called as methods, which usually isn't want you want.")
    (license (package-license perl))))

(define-public perl-test-deep
  (package
    (name "perl-test-deep")
    (version "0.114")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                                  "Test-Deep-" version ".tar.gz"))
              (sha256
               (base32
                "09yr47vw7vj27sdik312x08938higcij8ybyq8k67mlccx8cpqf0"))))
    (build-system perl-build-system)
    (inputs `(("perl-test-tester" ,perl-test-tester)
              ("perl-test-nowarnings" ,perl-test-nowarnings)))
    (synopsis "Flexible deep comparison for the Test::Builder framework")
    (description
     "Test::Deep compares two structures by going through each level, ensuring
that the values match, that arrays and hashes have the same elements and that
references are blessed into the correct class. It also handles circular data
structures without getting caught in an infinite loop.")
    (home-page (string-append "http://search.cpan.org/~rjbs/"
                              "Test-Deep-" version))
    (license gpl1+)))  ; or "Artistic License"

(define-public perl-test-differences
  (package
    (name "perl-test-differences")
    (version "0.63")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DC/DCANTRELL/"
                           "Test-Differences-" version ".tar.gz"))
       (sha256
        (base32
         "0rhs4q6qn64ji06ns7lwl6iiiw3mggvd9xk9nkiqvx1jihbplrbw"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-text-diff" ,perl-text-diff)
       ("perl-capture-tiny" ,perl-capture-tiny)))
    (home-page "http://search.cpan.org/dist/Test-Differences")
    (synopsis "Test strings and data structures and show differences")
    (description "This module exports three test functions and four diff-style
functions")
    ;; See LICENSE section of Test/Differences.pm, which reads "... GNU public
    ;; license, any version, ..."
    (license gpl3+)))

(define-public perl-test-directory
  (package
    (name "perl-test-directory")
    (version "0.041")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SA/SANBEG/"
                           "Test-Directory-" version ".tar.gz"))
       (sha256
        (base32
         "1ncql08cizhicbxwd753b4czns8nlcnlw0zfjcfrbdd41x4j6hqr"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-test-exception" ,perl-test-exception)))
    (home-page "http://search.cpan.org/dist/Test-Directory")
    (synopsis "Perl extension for maintaining test directories")
    (description "Testing code can involve making sure that files are created
and deleted as expected.  Doing this manually can be error prone, as it's easy
to forget a file, or miss that some unexpected file was added.  This module
simplifies maintaining test directories by tracking their status as they are
modified or tested with this API, making it simple to test both individual
files, as well as to verify that there are no missing or unknown files.")
    (license (package-license perl))))

(define-public perl-test-exception
  (package
    (name "perl-test-exception")
    (version "0.36")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/EX/EXODIST/"
                           "Test-Exception-" version ".tar.gz"))
       (sha256
        (base32
         "1zpwimspbq11wjrli481qk17aabzxab15cnnryflx45nzn3za2xk"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-sub-uplevel" ,perl-sub-uplevel)))
    (home-page "http://search.cpan.org/dist/Test-Exception")
    (synopsis "Test exception based code")
    (description "This module provides a few convenience methods for testing
exception based code.  It is built with Test::Builder and plays happily with
Test::More and friends.")
    (license (package-license perl))))

(define-public perl-test-fatal
  (package
    (name "perl-test-fatal")
    (version "0.014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Test-Fatal-" version ".tar.gz"))
       (sha256
        (base32
         "1c6bs68mss4q7cyapkv2c0jn66i21050p0faxf3s3417gdffzp5w"))))
    (build-system perl-build-system)
    (propagated-inputs `(("perl-try-tiny" ,perl-try-tiny)))
    (home-page "http://search.cpan.org/dist/Test-Fatal")
    (synopsis "Simple helpers for testing code with exceptions")
    (description "Test::Fatal is an alternative to the popular
Test::Exception.  It does much less, but should allow greater flexibility in
testing exception-throwing code with about the same amount of typing.")
    (license (package-license perl))))

(define-public perl-test-harness
  (package
    (name "perl-test-harness")
    (version "3.35")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "Test-Harness-" version ".tar.gz"))
       (sha256
        (base32
         "06l29y1bpizb9vd9g49lgi0wzj1xy4rsk42ahdj3fpgqnvb9wp05"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases (alist-cons-before
                 'check 'patch-test
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; This test looks for "#!/usr/bin/perl" in some source.
                   ;; Patch what the test looks for.
                   (substitute* "t/source.t"
                     (("#!/usr/bin/perl")
                      (string-append "#!" (assoc-ref inputs "perl")
                                     "/bin/perl"))))
                 %standard-phases)))
    (home-page "http://search.cpan.org/dist/Test-Harness")
    (synopsis "Run Perl standard test scripts with statistics")
    (description "Simple test harness which allows tests to be run and results
automatically aggregated and output to STDOUT.")
    (license (package-license perl))))

(define-public perl-test-mocktime
  (package
    (name "perl-test-mocktime")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DD/DDICK/"
                           "Test-MockTime-" version ".tar.gz"))
       (sha256
        (base32
         "0yrqmjg33akannwz2f99rfm7dvvxpzsdj23lsvlvfi4qslrlqfvw"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Test-MockTime")
    (synopsis "Replaces actual time with simulated time")
    (description "This module was created to enable test suites to test code
at specific points in time.  Specifically it overrides localtime, gmtime and
time at compile time and then relies on the user supplying a mock time via
set_relative_time, set_absolute_time or set_fixed_time to alter future calls
to gmtime,time or localtime.")
    (license (package-license perl))))

(define-public perl-test-most
  (package
    (name "perl-test-most")
    (version "0.34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OV/OVID/"
                           "Test-Most-" version ".tar.gz"))
       (sha256
        (base32
         "0i72aih3pakm8gh73wx1n4dwq8lbx6dvxhla46gsapszws6hr0n2"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-test-differences" ,perl-test-differences)
       ("perl-test-warn" ,perl-test-warn)
       ("perl-exception-class" ,perl-exception-class)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-exception" ,perl-test-exception)))
    (home-page "http://search.cpan.org/dist/Test-Most")
    (synopsis "Most commonly needed test functions and features")
    (description "This module provides the most commonly used testing
functions, along with automatically turning on strict and warning and gives a
bit more fine-grained control over test suites.")
    (license (package-license perl))))

(define-public perl-test-nowarnings
  (package
    (name "perl-test-nowarnings")
    (version "1.04")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AD/ADAMK/"
                                  "Test-NoWarnings-" version ".tar.gz"))
              (sha256
               (base32
                "0v385ch0hzz9naqwdw2az3zdqi15gka76pmiwlgsy6diiijmg2k3"))))
    (build-system perl-build-system)
    (inputs `(("perl-test-tester" ,perl-test-tester)))
    (synopsis "Ensure no warnings are produced while testing")
    (description
     "This modules causes any warnings during testing to be captured and
stored.  It automatically adds an extra test that will run when your script
ends to check that there were no warnings.  If there were any warings, the
test will fail and output diagnostics of where, when and what the warning was,
including a stack trace of what was going on when it occurred.")
    (home-page (string-append "http://search.cpan.org/~adamk/"
                              "Test-NoWarnings-" version))
    (license lgpl2.1)))

(define-public perl-test-output
  (package
    (name "perl-test-output")
    (version "1.03")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/"
                                  "Test-Output-" version ".tar.gz"))
              (sha256
               (base32
                "12991jnzj4cbw9whhprmqvnzd1ayii84g2mh8vxbjngwqrjsy41i"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-test-tester" ,perl-test-tester)
       ("perl-sub-exporter" ,perl-sub-exporter)))
    (synopsis "Utilities to test STDOUT and STDERR messages")
    (description
     "Test::Output provides a simple interface for testing output sent to
STDOUT or STDERR.  A number of different utilities are included to try and be
as flexible as possible to the tester.")
    (home-page (string-append "http://search.cpan.org/~bdfoy/"
                              "Test-Output-" version))
    (license (package-license perl))))

(define-public perl-test-requires
  (package
    (name "perl-test-requires")
    (version "0.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOKUHIROM/"
                           "Test-Requires-" version ".tar.gz"))
       (sha256
        (base32
         "08c29m0dn34384mmmpqqlbb899zpbkkc01c2lsp31mch1frv9cg7"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Test-Requires")
    (synopsis "Checks to see if the module can be loaded")
    (description "Test::Requires checks to see if the module can be loaded.
If this fails, then rather than failing tests this skips all tests.")
    (license (package-license perl))))

(define-public perl-test-script
  (package
    (name "perl-test-script")
    (version "1.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AD/ADAMK/"
                                  "Test-Script-" version ".tar.gz"))
              (sha256
               (base32
                "15pb4zzsnm33msc1syhig2bk05xqc0pckmfyahdwbd177bj5w7p2"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("probe-perl" ,perl-probe-perl)
       ("ipc-run3"   ,perl-ipc-run3)))
    (synopsis "Basic cross-platform tests for scripts")
    (description
     "The intent of the Test::Script module is to provide a series of basic
tests for 80% of the testing you will need to do for scripts in the script (or
bin as is also commonly used) paths of your Perl distribution.")
    (home-page (string-append "http://search.cpan.org/~adamk/"
                              "Test-Script-" version))
    (license (package-license perl))))

(define-public perl-test-simple
  (package
    (name "perl-test-simple")
    (version "1.001014")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/E/EX/EXODIST/"
                                  "Test-Simple-" version ".tar.gz"))
              (sha256
               (base32
                "0szi95shwwdvc4nqykzgx05g2m1001mjhvqqhjg5wypbi771992m"))))
    (build-system perl-build-system)
    (synopsis "Basic utilities for writing tests")
    (description
     "Test::Simple contains basic utilities for writing tests.")
    (home-page (string-append "http://search.cpan.org/~exodist/"
                              "Test-Simple-" version))
    (license (package-license perl))))

(define-public perl-test-tester
  (package
    (name "perl-test-tester")
    (version "0.109")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/F/FD/FDALY/"
                                  "Test-Tester-" version ".tar.gz"))
              (sha256
               (base32
                "0m9n28z09kq455r5nydj1bnr85lvmbfpcbjdkjfbpmfb5xgciiyk"))))
    (build-system perl-build-system)
    (synopsis "Simplify running Test::Builder tests")
    (description
     "Test::Tester allows testing of test modules based on Test::Builder with
a minimum of effort.")
    (home-page (string-append "http://search.cpan.org/~fdaly/"
                              "Test-Tester-" version))
    ;; "Under the same license as Perl itself"
    (license (package-license perl))))

(define-public perl-test-warn
  (package
    (name "perl-test-warn")
    (version "0.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CH/CHORNY/"
                           "Test-Warn-" version ".tar.gz"))
       (sha256
        (base32
         "0haf2ii7br5z0psmkvlvmx2z2q9qz1c70gx0969r378qjidmb5w1"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-sub-uplevel" ,perl-sub-uplevel)))
    (home-page "http://search.cpan.org/dist/Test-Warn")
    (synopsis "Perl extension to test methods for warnings")
    (description "This module provides a few convenience methods for testing
warning based code.")
    (license (package-license perl))))

(define-public perl-test-warnings
  (package
    (name "perl-test-warnings")
    (version "0.020")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Test-Warnings-" version ".tar.gz"))
       (sha256
        (base32
         "1x262kybrdnbiiw53m1axp4zyh4lsfb9mm2shmpm8lwf7sp30isi"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Test-Warnings")
    (synopsis "Test for warnings and the lack of them")
    (description "This module is intended to be used as a drop-in replacement
for Test::NoWarnings.  It also adds an extra test, but runs this test before
done_testing calculates the test count, rather than after.  It does this by
hooking into done_testing as well as via an END block.  You can declare a
plan, or not, and things will still Just Work.")
    (license (package-license perl))))

(define-public perl-test-writevariants
  (package
    (name "perl-test-writevariants")
    (version "0.010")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "Test-WriteVariants-" version ".tar.gz"))
       (sha256
        (base32
         "0kklp05fj98yiq8znyfx9lx1vmjay2ypfb868qdwv3kf93m5zjwr"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-most" ,perl-test-most)
       ("perl-test-directory" ,perl-test-directory)))
    (propagated-inputs
     `(("perl-data-tumbler" ,perl-data-tumbler)
       ("perl-file-homedir" ,perl-file-homedir)))
    (home-page "http://search.cpan.org/dist/Test-WriteVariants")
    (synopsis "Dynamic generation of tests")
    (description "The Test::WriteVariants module provides for the dynamic
generation of tests in nested combinations of contexts.")
    (license (package-license perl))))  ;See LICENSE

(define-public perl-text-diff
  (package
    (name "perl-text-diff")
    (version "1.41")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OV/OVID/"
                           "Text-Diff-" version ".tar.gz"))
       (sha256
        (base32
         "1ynjsa4sr1yvyh65sdfvahaafglibz70j8b6rj01cg1iisj50zx6"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-algorithm-diff" ,perl-algorithm-diff)))
    (home-page "http://search.cpan.org/dist/Text-Diff")
    (synopsis "Perform diffs on files and record sets")
    (description "Text::Diff provides a basic set of services akin to the GNU
diff utility.  It is not anywhere near as feature complete as GNU diff, but it
is better integrated with Perl and available on all platforms.  It is often
faster than shelling out to a system's diff executable for small files, and
generally slower on larger files.")
    (license (package-license perl))))

(define-public perl-text-glob
  (package
    (name "perl-text-glob")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Text-Glob-" version ".tar.gz"))
       (sha256
        (base32
         "0lr76wrsj8wcxrq4wi8z1640w4dmdbkznp06q744rg3g0bd238d5"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Text-Glob")
    (synopsis "Match globbing patterns against text")
    (description "Text::Glob implements glob(3) style matching that can be
used to match against text, rather than fetching names from a filesystem.  If
you want to do full file globbing use the File::Glob module instead.")
    (license (package-license perl))))

(define-public perl-time-local
  (package
    (name "perl-time-local")
    (version "1.2300")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Time-Local-" version ".tar.gz"))
       (sha256
        (base32
         "0jgvd6v93hlrcmy56yxbm4yrhzi8yvrq8c3xffpgh28af01wmb5j"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Time-Local")
    (synopsis "Efficiently compute time from local and GMT time")
    (description "This module provides functions that are the inverse of
built-in perl functions localtime() and gmtime().  They accept a date as a
six-element array, and return the corresponding time(2) value in seconds since
the system epoch")
    (license (package-license perl))))

(define-public perl-timedate
  (package
    (name "perl-timedate")
    (version "2.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GB/GBARR/"
                           "TimeDate-" version ".tar.gz"))
       (sha256
        (base32
         "11lf54akr9nbivqkjrhvkmfdgkbhw85sq0q4mak56n6bf542bgbm"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/TimeDate")
    (synopsis "Date parsing/formating subroutines")
    (description "This module provides routines for parsing date string into
time values and formating dates into ASCII strings.")
    (license (package-license perl))))

(define-public perl-time-mock
  (package
    (name "perl-time-mock")
    (version "v0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/EW/EWILHELM/"
                           "Time-Mock-" version ".tar.gz"))
       (sha256
        (base32
         "0bwqyg8z98m8cjw1qcm4wg502n225k33j2fp8ywxkgfjdd1zgllv"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-timedate" ,perl-timedate))) ;For Date::Parse
    (home-page "http://search.cpan.org/dist/Time-Mock")
    (synopsis "Shift and scale time")
    (description "This module allows you to speed up your sleep(), alarm(),
and time() calls.")
    (license (package-license perl))))

(define-public perl-try-tiny
  (package
    (name "perl-try-tiny")
    (version "0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                           "Try-Tiny-" version ".tar.gz"))
       (sha256
        (base32
         "068vdbpacfawc3lkfs0b82xxl27h3l0gj14iada3vlwk8rps9yv0"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Try-Tiny")
    (synopsis "Minimal try/catch with proper preservation of $@")
    (description "This module provides bare bones try/catch/finally statements
that are designed to minimize common mistakes with eval blocks, and nothing
else.")
    (license x11)))

(define-public perl-variable-magic
  (package
    (name "perl-variable-magic")
    (version "0.55")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/V/VP/VPIT/"
                           "Variable-Magic-" version ".tar.gz"))
       (sha256
        (base32
         "0xzh2vy45ph80bp09j5fcjy8ydgn8yaxsa0fj831q6p1spvyniwg"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Variable-Magic")
    (synopsis "Associate user-defined magic to variables from Perl")
    (description "Magic is Perl's way of enhancing variables.  This mechanism
lets the user add extra data to any variable and hook syntactical
operations (such as access, assignment or destruction) that can be applied to
it.  With this module, you can add your own magic to any variable without
having to write a single line of XS.")
    (license (package-license perl))))


;;; Some packaged modules need versions of core modules that are newer than
;;; those in our perl 5.16.1.

(define-public perl-cpan-meta
  (package
    (name "perl-cpan-meta")
    (version "2.143240")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "CPAN-Meta-" version ".tar.gz"))
       (sha256
        (base32
         "1d80bxphpp5dq7fx5ipxszn7j8q9d85w6fnapdrbym21k1vsmlf6"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-cpan-meta-requirements" ,perl-cpan-meta-requirements)
       ("perl-cpan-meta-yaml" ,perl-cpan-meta-yaml)
       ("perl-parse-cpan-meta" ,perl-parse-cpan-meta)))
    (home-page "http://search.cpan.org/dist/CPAN-Meta")
    (synopsis "Distribution metadata for a CPAN dist")
    (description "Software distributions released to the CPAN include a
META.json or, for older distributions, META.yml, which describes the
distribution, its contents, and the requirements for building and installing
the distribution.  The data structure stored in the META.json file is
described in CPAN::Meta::Spec.  CPAN::Meta provides a simple class to
represent this distribution metadata (or distmeta), along with some helpful
methods for interrogating that data.")
    (license (package-license perl))))

(define-public perl-cpan-meta-requirements
  (package
    (name "perl-cpan-meta-requirements")
    (version "2.131")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "CPAN-Meta-Requirements-" version ".tar.gz"))
       (sha256
        (base32
         "12p5s7w3cwcrbpcrxzanvpr0syswhwlqzbaki6m044c45jix2fss"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/CPAN-Meta-Requirements")
    (synopsis "Set of version requirements for a CPAN dist")
    (description "A CPAN::Meta::Requirements object models a set of version
constraints like those specified in the META.yml or META.json files in CPAN
distributions, and as defined by CPAN::Meta::Spec.  It can be built up by
adding more and more constraints, and will reduce them to the simplest
representation.")
    (license (package-license perl))))

(define-public perl-cpan-meta-yaml
  (package
    (name "perl-cpan-meta-yaml")
    (version "0.012")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "CPAN-Meta-YAML-" version ".tar.gz"))
       (sha256
        (base32
         "0a0d62w8d81kkas4j1h48znk0f0vrpibl31gvz9r8hm77dbqqwkw"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f))                    ;Tests require Test::More >= 0.99
    (home-page "http://search.cpan.org/dist/CPAN-Meta-YAML")
    (synopsis "Read and write a subset of YAML for CPAN Meta files")
    (description "This module implements a subset of the YAML specification
for use in reading and writing CPAN metadata files like META.yml and
MYMETA.yml.")
    (license (package-license perl))))

(define-public perl-module-build
  (package
    (name "perl-module-build")
    (version "0.4211")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "Module-Build-" version ".tar.gz"))
       (sha256
        (base32
         "1c5hfhajr963w4mdjivsc7yz4vf4pz1rrfch5a93fbac1x2mr58h"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-cpan-meta" ,perl-cpan-meta)))
    (home-page "http://search.cpan.org/dist/Module-Build")
    (synopsis "Build and install Perl modules")
    (description "\"Module::Build\" is a system for building, testing, and
installing Perl modules.  It is meant to be an alternative to
\"ExtUtils::MakeMaker\".  Developers may alter the behavior of the module
through subclassing in a much more straightforward way than with
\"MakeMaker\".  It also does not require a \"make\" on your system - most of
the \"Module::Build\" code is pure-perl and written in a cross-platform way.")
    (license (package-license perl))))

(define-public perl-parse-cpan-meta
  (package
    (name "perl-parse-cpan-meta")
    (version "1.4414")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "Parse-CPAN-Meta-" version ".tar.gz"))
       (sha256
        (base32
         "06ya2rg599qanqb1fxiyrd489mvmdgzbw4ph23hwjwpv9lahhxnd"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-cpan-meta-yaml" ,perl-cpan-meta-yaml)))
    (home-page "http://search.cpan.org/dist/Parse-CPAN-Meta")
    (synopsis "Parse META.yml and META.json CPAN metadata files")
    (description "Parse::CPAN::Meta is a parser for META.json and META.yml
files, using JSON::PP and/or CPAN::Meta::YAML.")
    (license (package-license perl))))

(define-public perl-scalar-list-utils
  (package
    (name "perl-scalar-list-utils")
    (version "1.41")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PEVANS/"
                           "Scalar-List-Utils-" version ".tar.gz"))
       (sha256
        (base32
         "04l1q4hps9n8b1hk9kpgpc1cryim7pl9sfdyb7fz5nq4gmz307j7"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Scalar-List-Utils")
    (synopsis "Common Scalar and List utility subroutines")
    (description "This package contains a selection of subroutines that people
have expressed would be nice to have in the perl core, but the usage would not
really be high enough to warrant the use of a keyword, and the size so small
such that being individual extensions would be wasteful.")
    (license (package-license perl))))

;;; END: Core module overrides
