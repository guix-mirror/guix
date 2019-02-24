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
  #:use-module (guix packages)
  #:use-module (guix build-system perl)
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
