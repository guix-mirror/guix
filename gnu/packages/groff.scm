;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Prafulla Giri <pratheblackdiamond@gmail.com>
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

(define-module (gnu packages groff)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages web))

(define-public groff
  (package
   (name "groff")
   (version "1.22.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/groff/groff-" version
                                ".tar.gz"))
            (sha256 (base32
                     "14q2mldnr1vx0l9lqp9v2f6iww24gj28iyh4j2211hyynx67p3p7"))))
   (build-system gnu-build-system)
   (outputs '("out"
              "doc"))                    ;12MiB of PS, PDF, HTML, and examples

   ;; Note: groff's HTML backend uses executables from netpbm when they are in
   ;; $PATH.  In practice, not having them doesn't prevent it from install its
   ;; own HTML doc, nor does it change its capabilities, so we removed netpbm
   ;; from 'inputs'.

   (inputs `(("ghostscript" ,ghostscript)))

   ;; When cross-compiling, this package depends upon a native install of
   ;; itself.
   (native-inputs `(,@(if (%current-target-system)
                          `(("self" ,this-package))
                          '())
                    ("bison" ,bison)
                    ("perl" ,perl)
                    ("psutils" ,psutils)
                    ("texinfo" ,texinfo)))
   (arguments
    `(#:parallel-build? #f   ; parallel build fails
      ,@(if (%current-target-system)
            `(#:make-flags
              ;; In groff-minimal package, that inherits from this package,
              ;; we'll need to locate "groff" instead of "self".
              (let ((groff (or (assoc-ref %build-host-inputs "groff")
                               (assoc-ref %build-host-inputs "self"))))
                (list
                 (string-append "GROFF_BIN_PATH=" groff)
                 (string-append "GROFFBIN=" groff "/bin/groff"))))
            '())
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'disable-relocatability
          (lambda _
            ;; Groff contains a Rube Goldberg-esque relocator for the file
            ;; "charset.alias".  It tries to find the current executable
            ;; using realpath, a do-it-yourself search in $PATH and so on.
            ;; Furthermore, the routine that does the search is buggy
            ;; in that it doesn't handle error cases when they arise.
            ;; This causes preconv to segfault when trying to look up
            ;; the file "charset.alias" in the NULL location.
            ;; The "charset.alias" parser is a copy of gnulib's, and a
            ;; non-broken version of gnulib's "charset.alias" parser is
            ;; part of glibc's libcharset.
            ;; However, groff unconditionally uses their own
            ;; "charset.alias" parser, but then DOES NOT INSTALL the
            ;; file "charset.alias" when glibc is too new.
            ;; In Guix, our file "charset.alias" only contains an obscure
            ;; alias for ASCII and nothing else.  So just disable relocation
            ;; and make the entire "charset.alias" lookup fail.
            ;; See <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=30785> for
            ;; details.
            (substitute* "Makefile.in"
              (("-DENABLE_RELOCATABLE=1") ""))
            #t))
        (add-after 'unpack 'setenv
          (lambda _
            (setenv "GS_GENERATE_UUIDS" "0")
            #t))
        (add-after 'unpack 'fix-docdir
          (lambda _         ;see https://savannah.gnu.org/bugs/index.php?55461
            (substitute* "Makefile.in"
              (("^docdir =.*") "docdir = @docdir@\n"))
            #t)))))
   (synopsis "Typesetting from plain text mixed with formatting commands")
   (description
    "Groff is a typesetting package that reads plain text and produces
formatted output based on formatting commands contained within the text.  It
is usually the formatter of \"man\" documentation pages.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/groff/")))

(define-public groff-minimal
  ;; Minimialist groff for use by man-db.  Its closure size is less than half
  ;; that of the full-blown groff.
  (package/inherit groff
    (name "groff-minimal")
    (synopsis "Minimalist variant of Groff for use by man-db")
    (outputs '("out"))

    ;; Omit the DVI, PS, PDF, and HTML backends.
    (inputs '())
    (native-inputs `(("bison" ,bison)
                     ("perl" ,perl)
                     ("groff" ,groff)))

    (arguments
     `(#:disallowed-references (,perl)

       #:configure-flags '("--with-doc=no")

       ,@(substitute-keyword-arguments (package-arguments groff)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'install 'remove-non-essential-programs
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Keep only the programs that man-db needs at run time,
                   ;; and make sure we don't pull in Perl.
                   (let ((out  (assoc-ref outputs "out"))
                         (kept '("eqn" "neqn" "pic" "tbl" "refer" "preconv"
                                 "nroff" "groff" "troff" "grotty")))
                     (for-each (lambda (file)
                                 (unless (member (basename file) kept)
                                   (delete-file file)))
                               (find-files (string-append out "/bin")))

                     ;; Remove a bunch of unneeded Perl scripts.
                     (for-each delete-file (find-files out "\\.pl$"))
                     (for-each delete-file
                               (find-files out "BuildFoundries"))

                     ;; Remove ~3 MiB from share/groff/X.Y/font/devBACKEND
                     ;; corresponding to the unused backends.
                     (for-each delete-file-recursively
                               (find-files out "^dev(dvi|ps|pdf|html|lj4)$"
                                           #:directories? #t))
                     #t))))))))))

;; There are no releases, so we take the latest commit.
(define-public roffit
  (let ((commit "b59e6c855ebea03daf76e996b5c0f8343f11be3d")
        (revision "1"))
    (package
      (name "roffit")
      (version (string-append "0.12-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/bagder/roffit")
                      (commit commit)))
                (file-name (string-append "roffit-" commit "-checkout"))
                (sha256
                 (base32
                  "0z4cs92yqh22sykfgbjlyxfaifdvsd47cf1yhr0f2rgcc6l0fj1r"))))
      (build-system gnu-build-system)
      (arguments
       `(#:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "roffit" (string-append out "/bin"))
                 (install-file "roffit.1" (string-append out "/share/man/man1"))
                 #t)))
           (add-after 'install 'wrap-program
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (wrap-program (string-append out "/bin/roffit")
                   `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB"))))
                 #t))))))
      (native-inputs `(("html-tree" ,perl-html-tree))) ; for test
      (inputs
       `(("perl" ,perl)))
      (home-page "https://daniel.haxx.se/projects/roffit/")
      (synopsis "Convert nroff files to HTML")
      (description
       "Roffit is a program that reads an nroff file and outputs an HTML file.
It is typically used to display man pages on a web site.")
      (license expat))))

(define-public ronn-ng
  (package
    (name "ronn-ng")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ronn-ng" version))
       (sha256
        (base32
         "1slxfg57cabmh98fw507z4ka6lwq1pvbrqwppflxw6700pi8ykfh"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'fix-gemspec-mustache
           (lambda _
             (substitute* "ronn-ng.gemspec"
               (("(<mustache>.freeze.*~>).*(\".*$)" all start end)
                (string-append start " 1.0" end)))
             #t))
         (add-after 'wrap 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref %outputs "out") "/bin/ronn")))
               (wrap-program prog
                 `("PATH" ":" suffix ,(map
                                       (lambda (exp_inpt)
                                         (string-append
                                          (assoc-ref %build-inputs exp_inpt)
                                          "/bin"))
                                       '("ruby-kramdown"
                                         "ruby-mustache"
                                         "ruby-nokogiri")))))
             #t)))))
    (inputs
     `(("ruby-kramdown" ,ruby-kramdown)
       ("ruby-mustache" ,ruby-mustache)
       ("ruby-nokogiri" ,ruby-nokogiri)))
    (synopsis
     "Build manuals in HTML and Unix man page format from Markdown")
    (description
     "Ronn-NG is an updated fork of ronn.  It builds manuals in HTML and Unix
man page format from Markdown.")
    (home-page "https://github.com/apjanke/ronn-ng")
    (license expat)))
