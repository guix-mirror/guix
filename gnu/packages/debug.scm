;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages debug)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages indent)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pretty-print))

(define-public delta
  (package
    (name "delta")
    (version "2006.08.03")
    (source
     (origin
      (method url-fetch)
      (uri (list
            (string-append "http://ftp.de.debian.org/debian/pool/main/d/delta/"
                           "delta_" version ".orig.tar.gz")
            ;; This uri seems to send guix download into an infinite loop
            (string-append "http://delta.tigris.org/files/documents/3103/"
                           "33566/delta-" version ".tar.gz")))
      (sha256
       (base32
        "184wh35pf2ddx97319s6sgkzpz48xxkbwzcjpycv009bm53lh61q"))))
    (build-system gnu-build-system)
    (inputs                             ;Installed programs are perl scripts
     `(("perl" ,perl)))
    (arguments
     `(#:phases
       (alist-replace
        'install
        (lambda* (#:key outputs #:allow-other-keys)
          ;; Makefile contains no install target
          (let* ((out (assoc-ref outputs "out"))
                 (bin (string-append out "/bin"))
                 (doc (string-append out "/share/doc/delta-" ,version)))
            (begin
              (mkdir-p bin)
              (mkdir-p doc)
              (for-each (lambda (h)
                          (copy-file h (string-append doc "/" (basename h))))
                        `("License.txt" ,@(find-files "www" ".*\\.html")))
              (for-each (lambda (b)
                          (copy-file b (string-append bin "/" b)))
                        `("delta" "multidelta" "topformflat")))))
        (alist-delete 'configure %standard-phases))))
    (home-page "http://delta.tigris.org/")
    (synopsis "Heuristical file minimizer")
    (description
     "Delta assists you in minimizing \"interesting\" files subject to a test
of their interestingness.  A common such situation is when attempting to
isolate a small failure-inducing substring of a large input that causes your
program to exhibit a bug.")
    ;; See License.txt, which is a bsd-3 license, despite the project's
    ;; home-page pointing to a bsd-2 license.
    (license bsd-3)))

(define-public c-reduce
  (package
    (name "c-reduce")
    (version "2.2.1")
    (source
     (origin
      (method url-fetch)
      (uri (list
            (string-append "http://embed.cs.utah.edu/creduce/"
                           "creduce-" version ".tar.gz")))
      (sha256
       (base32
        "0wh0fkyg2l41d2wkndrgdiai9g2qiav7jik7cys21vmgzq01pyy2"))
      (modules '((guix build utils)))
      (snippet
       '(substitute* "clang_delta/TransformationManager.cpp"
          (("llvm/Config/config.h") "llvm/Config/llvm-config.h")))))
    (build-system gnu-build-system)
    (inputs
     `(("astyle"          ,astyle)
       ("delta"           ,delta)
       ("llvm"            ,llvm)
       ("clang"           ,clang)
       ("flex"            ,flex)
       ("indent"          ,indent)
       ("perl"            ,perl)
       ("benchmark-timer" ,perl-benchmark-timer)
       ("exporter-lite"   ,perl-exporter-lite)
       ("file-which"      ,perl-file-which)
       ("getopt-tabular"  ,perl-getopt-tabular)
       ("regex-common"    ,perl-regexp-common)
       ("sys-cpu"         ,perl-sys-cpu)))
    (arguments
     `(#:phases (alist-cons-after
                 'install 'set-load-paths
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   ;; Tell creduce where to find the perl modules it needs.
                   (let* ((out (assoc-ref outputs "out"))
                          (prog (string-append out "/bin/creduce")))
                     (wrap-program
                      prog
                      `("PERL5LIB" ":" prefix
                        ,(map (lambda (p)
                                (string-append (assoc-ref inputs p)
                                               "/lib/perl5/site_perl/"
                                               ,(package-version perl)))
                              '("benchmark-timer" "exporter-lite"
                                "file-which"      "getopt-tabular"
                                "regex-common"    "sys-cpu"))))))
                 %standard-phases)))
    (home-page "http://embed.cs.utah.edu/creduce")
    (synopsis "Reducer for interesting code")
    (description
     "C-Reduce is a tool that takes a large C or C++ program that has a
property of interest (such as triggering a compiler bug) and automatically
produces a much smaller C/C++ program that has the same property.  It is
intended for use by people who discover and report bugs in compilers and other
tools that process C/C++ code.")
    (license ncsa)))
