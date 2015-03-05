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

;;; END: Core module overrides
