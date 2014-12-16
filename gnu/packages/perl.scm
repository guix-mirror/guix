;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages perl)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl))

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
                                (directories '("lib/perl5/site_perl")))))
    (synopsis "Implementation of the Perl programming language")
    (description
     "Perl 5 is a highly capable, feature-rich programming language with over
24 years of development.")
    (home-page "http://www.perl.org/")
    (license gpl1+)))                          ; or "Artistic"

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
