;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2021 Chris Marusich <cmmarusich@gmail.com>
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

(define-module (gnu packages commencement)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages c)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages mes)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages xml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix memoization)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:export (make-gcc-toolchain))

;;; Commentary:
;;;
;;; This is the commencement, this is where things start.  Before the
;;; commencement, of course, there's the 'bootstrap' module, which provides us
;;; with the initial binaries.  This module uses those bootstrap binaries to
;;; actually build up the whole tool chain that make up the implicit inputs of
;;; 'gnu-build-system'.
;;;
;;; To avoid circular dependencies, this module should not be imported
;;; directly from anywhere.
;;;
;;; Below, we frequently use "inherit" to create modified packages.  The
;;; reason why we use "inherit" instead of "package/inherit" is because we do
;;; not want these commencement packages to inherit grafts.  By definition,
;;; these packages are not depended on at run time by any of the packages we
;;; use.  Thus it does not make sense to inherit grafts.  Furthermore, those
;;; grafts would often lead to extra overhead for users who would end up
;;; downloading those "-boot0" packages just to build package replacements
;;; that are in fact not going to be used.
;;;
;;; Code:

(define bootar
  (package
    (name "bootar")
    (version "1a")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                          "mirror://gnu/guix/mirror/bootar-" version ".ses")
                         (string-append
                          "https://files.ngyro.com/bootar/bootar-"
                          version ".ses")))
              (sha256
               (base32
                "0mvp6vgx0q316fvy3z2lddlc5xgq5np3bm1fypgvj6dnayibg9np"))))
    (build-system gnu-build-system)
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       #:imported-modules ((guix build gnu-bootstrap)
                           ,@%gnu-build-system-modules)
       #:phases
       (begin
         (use-modules (guix build gnu-bootstrap))
         (modify-phases %standard-phases
           (replace 'unpack
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((source (assoc-ref inputs "source"))
                      (guile-dir (assoc-ref inputs "guile"))
                      (guile (string-append guile-dir "/bin/guile")))
                 (invoke guile "--no-auto-compile" source)
                 (chdir "bootar")
                 #t)))
           (replace 'configure (bootstrap-configure ,version "." "scripts"))
           (replace 'build (bootstrap-build "."))
           (replace 'install (bootstrap-install "." "scripts"))))))
    (inputs `(("guile" ,%bootstrap-guile)))
    (home-page "https://git.ngyro.com/bootar")
    (synopsis "Tar decompression and extraction in Guile Scheme")
    (description "Bootar is a simple Tar extractor written in Guile
Scheme.  It supports running 'tar xvf' on uncompressed tarballs or
tarballs that are compressed with BZip2, GZip, or XZ.  It also provides
standalone scripts for 'bzip2', 'gzip', and 'xz' that each support
decompression to standard output.

What makes this special is that Bootar is distributed as a
self-extracting Scheme (SES) program.  That is, a little script that
outputs the source code of Bootar.  This makes it possible to go from
pure Scheme to Tar and decompression in one easy step.")
    (license license:gpl3+)))

(define gash-boot
  (package
    (inherit gash)
    (name "gash-boot")
    (source (origin
              (inherit (package-source gash))
              (snippet #f)))            ;discard snippet for Guile 3.0 support
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       #:imported-modules ((guix build gnu-bootstrap)
                           ,@%gnu-build-system-modules)
       #:phases
       (begin
         (use-modules (guix build gnu-bootstrap))
         (modify-phases %standard-phases
           (replace 'configure
             (bootstrap-configure ,(package-version gash) "gash" "scripts"))
           (replace 'build (bootstrap-build "gash"))
           (replace 'install (bootstrap-install "gash" "scripts"))
           (add-after 'install 'install-symlinks
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (symlink (string-append out "/bin/gash")
                          (string-append out "/bin/sh"))
                 (symlink (string-append out "/bin/gash")
                          (string-append out "/bin/bash"))
                 #t)))))))
    (inputs `(("guile" ,%bootstrap-guile)))
    (native-inputs `(("bootar" ,bootar)))))

(define gash-utils-boot
  (package
    (inherit gash-utils)
    (name "gash-utils-boot")
    (source (origin
              (inherit (package-source gash-utils))
              (patches '())
              (snippet #f)))            ;discard snippet for Guile 3.0 support
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       #:imported-modules ((guix build gnu-bootstrap)
                           ,@%gnu-build-system-modules)
       #:phases
       (begin
         (use-modules (guix build gnu-bootstrap))
         (modify-phases %standard-phases
           (add-after 'unpack 'set-load-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((gash (assoc-ref inputs "gash")))
                 (add-to-load-path (string-append gash "/share/guile/site/"
                                                  (effective-version))))
               #t))
           (add-before 'configure 'pre-configure
             (lambda _
               (format #t "Creating gash/commands/testb.scm~%")
               (copy-file "gash/commands/test.scm"
                          "gash/commands/testb.scm")
               (substitute* "gash/commands/testb.scm"
                 (("gash commands test") "gash commands testb")
                 (("apply test [(]cdr") "apply test/bracket (cdr"))
               (for-each (lambda (script)
                           (let ((target (string-append "scripts/"
                                                        script ".in")))
                             (format #t "Creating scripts/~a~%" target)
                             (copy-file "scripts/template.in" target)
                             (substitute* target
                               (("@UTILITY@") script))))
                         '("awk" "basename" "cat" "chmod" "cmp" "command"
                           "compress" "cp" "cut" "diff" "dirname" "expr"
                           "false" "find" "grep" "head" "ln" "ls" "mkdir"
                           "mv" "printf" "pwd" "reboot" "rm" "rmdir"
                           "sed" "sleep" "sort" "tar" "test" "touch" "tr"
                           "true" "uname" "uniq" "wc" "which"))
               (format #t "Creating scripts/[.in~%")
               (copy-file "scripts/template.in" "scripts/[.in")
               (substitute* "scripts/[.in"
                 (("@UTILITY@") "testb"))
               (delete-file "scripts/template.in")
               #t))
           (replace 'configure
             (bootstrap-configure ,(package-version gash-utils)
                                  "gash" "scripts"))
           (replace 'build (bootstrap-build "gash"))
           (replace 'install (bootstrap-install "gash" "scripts"))
           ;; XXX: The scripts should add Gash to their load paths and
           ;; this phase should not exist.
           (add-after 'install 'copy-gash
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (moddir (string-append out "/share/guile/site/"
                                             (effective-version)))
                      (godir (string-append out "/lib/guile/"
                                            (effective-version)
                                            "/site-ccache"))
                      (gash (assoc-ref inputs "gash"))
                      (gash-moddir (string-append gash "/share/guile/site/"
                                                  (effective-version)))
                      (gash-godir (string-append gash "/lib/guile/"
                                                 (effective-version)
                                                 "/site-ccache")))
                 (copy-file (string-append gash-moddir "/gash/compat.scm")
                            (string-append moddir "/gash/compat.scm"))
                 (copy-recursively (string-append gash-moddir "/gash/compat")
                                   (string-append moddir "/gash/compat"))
                 (copy-file (string-append gash-godir "/gash/compat.go")
                            (string-append godir "/gash/compat.go"))
                 (copy-recursively (string-append gash-godir "/gash/compat")
                                   (string-append godir "/gash/compat"))
                 #t)))))))
    (inputs `(("gash" ,gash-boot)
              ("guile" ,%bootstrap-guile)))
    (native-inputs `(("bootar" ,bootar)))))

(define (%boot-gash-inputs)
  `(("bash" , gash-boot)                ; gnu-build-system wants "bash"
    ("coreutils" , gash-utils-boot)
    ("bootar" ,bootar)
    ("guile" ,%bootstrap-guile)))

(define %bootstrap-mes-rewired
  (package
    (inherit mes)
    (name "bootstrap-mes-rewired")
    (version "0.19")
    (source #f)
    (native-inputs `(("mes" ,%bootstrap-mes)
                     ("gash" ,gash-boot)))
    (inputs '())
    (propagated-inputs '())
    (outputs '("out"))
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))

       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let* ((mes (assoc-ref %build-inputs "mes"))
                          (gash (assoc-ref %build-inputs "gash"))
                          (mes-bin (string-append mes "/bin"))
                          (guile (string-append mes-bin "/mes"))
                          (mes-module (string-append mes "/share/mes/module"))
                          (out (assoc-ref %outputs "out"))
                          (bin (string-append out "/bin"))
                          (mescc (string-append bin "/mescc"))
                          (module (string-append out "/share/mes/module")))
                     (define (rewire file)
                       (substitute* file
                         ((mes) out)
                         (("/gnu/store[^ ]+mes-minimal-[^/)}\"]*") out)
                         (("/gnu/store[^ ]+guile-[^/]*/bin/guile") guile)
                         (("/gnu/store[^ ]+bash-[^/)}\"]*") gash)))

                     (mkdir-p bin)
                     (for-each (lambda (file) (install-file file bin))
                               (find-files mes-bin))
                     (mkdir-p module)
                     (copy-recursively (string-append mes-module "/mes")
                                       (string-append module "/mes"))
                     (copy-recursively (string-append mes-module "/srfi")
                                       (string-append module "/srfi"))
                     (for-each rewire
                               ;; Cannot easily rewire "mes" because it
                               ;; contains NUL characters; would require
                               ;; remove-store-references alike trick
                               (filter (negate (cut string-suffix? "/mes" <>))
                                       (find-files bin)))
                     (rewire (string-append module "/mes/boot-0.scm"))

                     (delete-file mescc)
                     (with-output-to-file mescc
                       (lambda _
                         (display (string-append
                                   "\
#! " gash "/bin/sh
LANG=C
LC_ALL=C
export LANG LC_ALL

MES_PREFIX=${MES_REWIRED_PREFIX-" out "/share/mes}
MES=" bin "/mes
export MES MES_PREFIX

MES_ARENA=${MES_REWIRED_ARENA-10000000}
MES_MAX_ARENA=${MES_REWIRED_ARENA-10000000}
MES_STACK=${MES_REWIRED_STACK-1000000}
export MES_ARENA MES_MAX_ARENA MES_STACK

$MES -e '(mescc)' module/mescc.scm -- \"$@\"
"))))
                     (chmod mescc #o555)

                     (with-directory-excursion module
                       (chmod "mes/base.mes" #o644)
                       (copy-file "mes/base.mes" "mes/base.mes.orig")
                       (let ((base.mes (open-file "mes/base.mes" "a")))
                         (display "
;; A fixed map, from Mes 0.21, required to bootstrap Mes 0.21
(define (map f h . t)
  (if (or (null? h)
          (and (pair? t) (null? (car t)))
          (and (pair? t) (pair? (cdr t)) (null? (cadr t)))) '()
      (if (null? t) (cons (f (car h)) (map f (cdr h)))
          (if (null? (cdr t))
              (cons (f (car h) (caar t)) (map f (cdr h) (cdar t)))
              (if (null? (cddr t))
                  (cons (f (car h) (caar t) (caadr t)) (map f (cdr h) (cdar t) (cdadr t)))
                  (error 'unsupported (cons* 'map-4: f h t))b )))))
" base.mes)
                         (close base.mes))

                       (chmod "mes/guile.mes" #o644)
                       (copy-file "mes/guile.mes" "mes/guile.mes.orig")
                       (let ((guile.mes (open-file "mes/guile.mes" "a")))
                         (display "
;; After booting guile.scm; use Mes 0.21; especially: MesCC 0.21
(let* ((self (car (command-line)))
       (prefix (dirname (dirname self))))
  (set! %moduledir (string-append prefix \"/mes/module/\"))
  (setenv \"%numbered_arch\" \"true\"))

" guile.mes)
                         (close guile.mes)))
                     #t))))))

(define mes-boot
  (package
    (inherit mes)
    (name "mes-boot")
    (version "0.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/mes/"
                                  "mes-" version ".tar.gz"))
              (sha256
               (base32
                "0p1jsrrmcbc0zrvbvnjbb6iyxr0in71km293q8qj6gnar6bw09av"))))
    (inputs '())
    (propagated-inputs '())
    (native-inputs
     `(("nyacc-source" ,(origin (inherit (package-source nyacc-0.99))
                                (snippet #f)))
       ("mes" ,%bootstrap-mes-rewired)
       ("mescc-tools" ,%bootstrap-mescc-tools)
       ,@(%boot-gash-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       #:strip-binaries? #f    ; binutil's strip b0rkes MesCC/M1/hex2 binaries
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-seeds
           (lambda _
             (let ((nyacc-source (assoc-ref %build-inputs "nyacc-source")))
               (with-directory-excursion ".."
                 (invoke "tar" "-xvf" nyacc-source)))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref %outputs "out"))
                   (gash (assoc-ref %build-inputs "bash"))
                   (mes (assoc-ref %build-inputs "mes"))
                   (dir (with-directory-excursion ".." (getcwd))))
               (setenv "AR" (string-append "gash " (getcwd) "/scripts/mesar"))
               (setenv "BASH" (string-append gash "/bin/bash"))
               (setenv "CC" (string-append mes "/bin/mescc"))
               (setenv "GUILE_LOAD_PATH"
                       (string-append
                        mes "/share/mes/module"
                        ":" dir "/nyacc-0.99.0/module"))
               (invoke "gash" "configure.sh"
                       (string-append "--prefix=" out)
                       (string-append "--host=i686-linux-gnu")))))
         (replace 'build
           (lambda _
             (invoke "sh" "bootstrap.sh")))
         (delete 'check)
         (replace 'install
           (lambda _
             (substitute* "install.sh"  ; show some progress
               ((" -xf") " -xvf")
               (("^( *)((cp|mkdir|tar) [^']*[^\\])\n" all space cmd)
                (string-append space "echo '" cmd "'\n"
                               space cmd "\n")))
             (invoke "sh" "install.sh")
             ;; Keep ASCII output, for friendlier comparison and bisection
             (let* ((out (assoc-ref %outputs "out"))
                    (cache (string-append out "/lib/cache")))
               (define (objects-in-dir dir)
                 (find-files dir
                             (lambda (name stat)
                               (and (equal? (dirname name) dir)
                                    (or (string-suffix? ".o" name)
                                        (string-suffix? ".s" name))))))
               (for-each (lambda (x) (install-file x cache))
                         (append (objects-in-dir ".")
                                 (objects-in-dir "mescc-lib"))))
             #t)))))
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))
           (search-path-specification
            (variable "MES_PREFIX")
            (separator #f)
            (files '("")))))))


(define tcc-boot0
  ;; Pristine tcc cannot be built by MesCC, we are keeping a delta of 11
  ;; patches.  In a very early and rough form they were presented to the
  ;; TinyCC developers, who at the time showed no interest in supporting the
  ;; bootstrappable effort; we will try again later.  These patches have been
  ;; ported to 0.9.27, alas the resulting tcc is buggy.  Once MesCC is more
  ;; mature, this package should use the 0.9.27 sources (or later).
  (package
    (inherit tcc)
    (name "tcc-boot0")
    (version "0.9.26-1103-g6e62e0e")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://lilypond.org/janneke/mes/20191117/"
                    "/tcc-" version ".tar.gz"))
              (sha256
               (base32
                "1qbybw7mxbgkv3sazvz1v7c8byq998vk8f1h25ik8w3d2l63lxng"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs
     `(("mes" ,mes-boot)
       ("nyacc-source" ,(origin (inherit (package-source nyacc-0.99))
                                (snippet #f)))
       ("mescc-tools" ,%bootstrap-mescc-tools)
       ,@(%boot-gash-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:validate-runpath? #f           ; no dynamic executables
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-seeds
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((nyacc-source (assoc-ref %build-inputs "nyacc-source")))
               (with-directory-excursion ".."
                 (invoke "tar" "-xvf" nyacc-source)))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref %outputs "out"))
                    (dir (with-directory-excursion ".." (getcwd)))
                    (interpreter "/lib/mes-loader"))

               (setenv "prefix" out)
               (setenv "GUILE_LOAD_PATH"
                       (string-append dir "/nyacc-0.99.0/module"))

               (substitute* "conftest.c"
                 (("volatile") ""))

               (invoke "sh" "configure"
                       "--cc=mescc"
                       (string-append "--prefix=" out)
                       (string-append "--elfinterp=" interpreter)
                       "--crtprefix=."
                       "--tccdir=."))))
         (replace 'build
           (lambda _
             (substitute* "bootstrap.sh" ; Show some progress
               (("^( *)((cp|ls|mkdir|rm|[.]/tcc|[.]/[$][{PROGRAM_PREFIX[}]tcc) [^\"]*[^\\])\n" all space cmd)
                (string-append space "echo \"" cmd "\"\n"
                               space cmd "\n")))
             (invoke "sh" "bootstrap.sh")))
         (replace 'check
           (lambda _
             ;; fail fast tests
             (system* "./tcc" "--help") ; --help exits 1
             ;; (invoke "sh" "test.sh" "mes/scaffold/tests/30-strlen")
             ;; (invoke "sh" "-x" "test.sh" "mes/scaffold/tinycc/00_assignment")
             ;; TODO: add sensible check target (without depending on make)
             ;; (invoke "sh" "check.sh")
             #t))
         (replace 'install
           (lambda _
             (substitute* "install.sh"  ; Show some progress
               (("^( *)((cp|ls|mkdir|rm|tar|./[$][{PROGRAM_PREFIX[}]tcc) [^\"]*[^\\])\n" all space cmd)
                (string-append space "echo \"" cmd "\"\n"
                               space cmd "\n")))

             (invoke "sh" "install.sh"))))))
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))))))

(define gzip-mesboot
  ;; The initial gzip.  We keep this scripted gzip build before building make
  ;; to soften the dependency on Gash Core Utils gzip.
  (package
    (inherit gzip)
    (version "1.2.4")
    (name "gzip-mesboot")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gzip/gzip-" version ".tar"))
              (sha256
               (base32
                "1rhgk2vvmdvnn6vygf0dja92ryyng00knl0kz5srb77k2kryjb2d"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("tcc" ,tcc-boot0)
                     ,@(%boot-gash-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'scripted-patch
           (lambda _
             (substitute* "util.c"
               (("^char [*]strlwr" all) (string-append all "_tcc_cannot_handle_dupe")))
             #t))
         (replace 'build
           (lambda _
             (let ((files '("bits" "crypt" "deflate" "getopt" "gzip"
                            "inflate" "lzw" "trees" "unlzh" "unlzw"
                            "unpack" "unzip" "util" "zip")))
               (define (compile x)
                 (invoke "tcc" "-c" "-D NO_UTIME=1" "-D HAVE_UNISTD_H=1"
                         (string-append x ".c")))
               (for-each compile files)
               (apply invoke
                      (cons* "tcc" "-o" "gzip"
                             (map (lambda (x) (string-append x ".o")) files)))
               (link "gzip" "gunzip"))))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "gzip" bin)
               (install-file "gunzip" bin))))
         (replace 'check
           (lambda _
             (invoke "./gzip" "--version")))
         ;; no gzip yet
         (delete 'compress-documentation))))))

(define gnu-make-mesboot0
  ;; The initial make
  (package
    (inherit gnu-make)
    (name "make-mesboot0")
    (version "3.80")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/make/make-" version ".tar.gz"))
              (sha256
               (base32
                "1pb7fb7fqf9wz9najm85qdma1xhxzf1rhj5gwrlzdsz2zm0hpcv4"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("tcc" ,tcc-boot0)
                     ,@(%boot-gash-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:configure-flags '("CC=tcc"
                           "CPP=tcc -E"
                           "LD=tcc"
                           "--build=i686-unknown-linux-gnu"
                           "--host=i686-unknown-linux-gnu"
                           "--disable-nls")
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda _
             (substitute* "build.sh.in"
               (("@LIBOBJS@") "getloadavg.o")
               (("@REMOTE@") "stub"))
             #t))
         (add-after 'configure 'configure-fixup
           (lambda _
             (substitute* "make.h"
               (("^extern long int lseek.*" all) (string-append "// " all)))
             #t))
         (replace 'build
           (lambda _
             (invoke "sh" "./build.sh")))
         (replace 'check                ; proper check needs awk
           (lambda _
             (invoke "./make" "--version")))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "make" bin)))))))))

(define (%boot-tcc0-inputs)
  `(("make" ,gnu-make-mesboot0)
    ("tcc" ,tcc-boot0)
    ,@(%boot-gash-inputs)))

(define bzip2-mesboot
  ;; The initial bzip2
  (package
    (inherit bzip2)
    (name "bzip2-mesboot")
    (version (package-version bzip2))
    (source (bootstrap-origin (package-source bzip2)))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-tcc0-inputs))
    (outputs '("out"))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       #:tests? #f            ; check is naive, also checks non-built PROGRAMS
       #:strip-binaries? #f   ; no strip yet
       #:make-flags (list "CC=tcc -I ." "AR=tcc -ar" "bzip2"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda _
             (substitute* "Makefile"
               (("\tln " all)
                (string-append "\t#" all)))
             (substitute* "bzip2.c"
               (("struct utimbuf uTimBuf;" all)
                (string-append "// " all))
               (("uTimBuf[.]" all)
                (string-append "// " all))
               (("retVal = utime [(] dstName, &uTimBuf [)];" all)
                (string-append "retVal = 0; // " all)))
             #t))
         (replace 'configure
           (lambda _
             (with-output-to-file "utime.h"
               (lambda _ (display "
#define fchown(filedes, owner, group) 0
#define fchmod(filedes, mode) 0
")))
             #t))
         (replace 'check
           (lambda _
             (invoke "./bzip2" "--help")))
         ;; FIXME: no compressing gzip yet
         (delete 'compress-documentation))))))

(define bash-mesboot0
  ;; The initial Bash
  (package
    (inherit static-bash)
    (name "bash-mesboot0")
    (version "2.05b")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/bash/bash-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1r1z2qdw3rz668nxrzwa14vk2zcn00hw7mpjn384picck49d80xs"))))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-tcc0-inputs))
    (outputs '("out"))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       #:strip-binaries? #f             ; no strip yet
       #:configure-flags
       (list "--build=i686-unknown-linux-gnu"
             "--host=i686-unknown-linux-gnu"

             "--without-bash-malloc"
             "--disable-readline"
             "--disable-history"
             "--disable-help-builtin"
             "--disable-progcomp"
             "--disable-net-redirections"
             "--disable-nls"

             ;; Pretend 'dlopen' is missing so we don't build loadable
             ;; modules and related code.
             "ac_cv_func_dlopen=no")
       #:make-flags '("bash")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'setenv
           (lambda _
             (let* ((gash (assoc-ref %build-inputs "bash"))
                    (shell (string-append gash "/bin/gash")))
               (setenv "CONFIG_SHELL" shell)
               (setenv "SHELL" shell)
               (setenv "CC" "tcc")
               (setenv "LD" "tcc")
               (setenv "AR" "tcc -ar")
               (setenv "CFLAGS" "-D _POSIX_VERSION=1")
               #t)))
         (add-after 'unpack 'scripted-patch
           (lambda _
             (substitute* "Makefile.in"
               (("mksyntax\\.c\n") "mksyntax.c -lgetopt\n")
               (("buildversion[.]o\n") "buildversion.o -lgetopt\n")
               ;; No size in Gash
               (("\tsize ") "#\tsize"))
             (substitute* "lib/sh/oslib.c"
               (("int name, namelen;") "char *name; int namelen;"))
             (substitute* "lib/sh/snprintf.c"
               (("^#if (defined [(]HAVE_LOCALE_H[)])" all define) (string-append "#if 0 //" define)))
             (substitute* "configure"
               ((" egrep") " grep"))
             #t))
         (replace 'configure
           (lambda* (#:key configure-flags #:allow-other-keys)
             (let ((configure-flags (filter (lambda (x)
                                              (and (not (string-prefix? "CONFIG_SHELL=" x))
                                                   (not (string-prefix? "SHELL=" x))))
                                            configure-flags)))
               (format (current-error-port)
                       "running ./configure ~a\n" (string-join configure-flags)))
             (apply invoke (cons "./configure" configure-flags))))
         (add-after 'configure 'configure-fixups
           (lambda _
             (substitute* "config.h"
               (("#define GETCWD_BROKEN 1") "#undef GETCWD_BROKEN"))
             (let ((config.h (open-file "config.h" "a")))
               (display (string-append
                         ;; XXX TODO: remove nested ,(string-append ...) and
                         ;; store file name on next rebuild cycle
                         ,(string-append "
// tcc: error: undefined symbol 'enable_hostname_completion'
#define enable_hostname_completion(on_or_off) 0

// /gnu/store/" "cq0cmv35s9dhilx14zaghlc08gpc0hwr-tcc-boot0-0.9.26-6.c004e9a/lib/libc.a: error: 'sigprocmask' defined twice
#define HAVE_POSIX_SIGNALS 1
#define endpwent(x) 0
"))
                        config.h)
               (close config.h))
             #t))
         (replace 'check
           (lambda _
             (invoke "./bash" "--version")))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (copy-file "bash" (string-append bin "/bash"))
               (copy-file "bash" (string-append bin "/sh"))
               #t))))))))

(define tcc-boot
  ;; The final tcc.
  (package
    (inherit tcc-boot0)
    (name "tcc-boot")
    (version "0.9.27")
    (source (origin
              (inherit (package-source tcc))
              ;; `patches' needs XZ
              ;; (patches (search-patches "tcc-boot-0.9.27.patch"))
              ))
    (build-system gnu-build-system)
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(;;("boot-patch" ,(search-patch "tcc-boot-0.9.27.patch"))
                     ("bzip2" ,bzip2-mesboot)
                     ,@(%boot-tcc0-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:validate-runpath? #f           ; no dynamic executables
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         ;; tar xvf ..bz2 gives
         ;; bzip2: PANIC -- internal consistency error
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (copy-file source "tarball.tar.bz2")
             (invoke "bzip2" "-d" "tarball.tar.bz2")
             (invoke "tar" "xvf" "tarball.tar")
             (chdir (string-append "tcc-" ,version))
             #t))
         ;; no patch yet
         ;; (add-after 'unpack 'apply-boot-patch
         ;;   (lambda* (#:key inputs #:allow-other-keys)
         ;;     (let ((patch-file (assoc-ref inputs "boot-patch")))
         ;;       (invoke "patch" "-p1" "-i" patch-file))))
         (add-after 'unpack 'scripted-patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libtcc.c"
               (("s->alacarte_link = 1;" all)
                (string-append all "
    s->static_link = 1;")))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref %outputs "out"))
                    (tcc (assoc-ref %build-inputs "tcc"))
                    (libc (assoc-ref %build-inputs "libc"))
                    (interpreter "/mes/loader"))
               (invoke "sh" "configure"
                       (string-append "--cc=tcc")
                       (string-append "--cpu=i386")
                       (string-append "--prefix=" out)
                       (string-append "--elfinterp=" interpreter)
                       (string-append "--crtprefix=" tcc "/lib")
                       (string-append "--sysincludepaths=" tcc "/include")
                       (string-append "--libpaths=" tcc "/lib")))))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref %outputs "out"))
                    (tcc (assoc-ref %build-inputs "tcc"))
                    (libc (assoc-ref %build-inputs "libc"))
                    (interpreter "/mes/loader"))
               (invoke
                "tcc"
                "-vvv"
                "-D" "BOOTSTRAP=1"
                "-D" "ONE_SOURCE=1"
                "-D" "TCC_TARGET_I386=1"
                "-D" "CONFIG_TCC_STATIC=1"
                "-D" "CONFIG_USE_LIBGCC=1"
                "-D" (string-append "CONFIG_TCCDIR=\"" out "/lib/tcc\"")
                "-D" (string-append "CONFIG_TCC_CRTPREFIX=\"" out "/lib:{B}/lib:.\"")
                "-D" (string-append "CONFIG_TCC_CRTPREFIX=\"" out "/lib:{B}/lib:.\"")
                "-D" (string-append "CONFIG_TCC_ELFINTERP=\"" interpreter "\"")
                "-D" (string-append "CONFIG_TCC_LIBPATHS=\"" tcc "/lib:{B}/lib:.\"")
                "-D" (string-append "CONFIG_TCC_SYSINCLUDEPATHS=\""
                                    tcc "/include" ":/include:{B}/include\"")
                "-D" (string-append "TCC_LIBGCC=\"" tcc "/lib/libc.a\"")
                "-o" "tcc"
                "tcc.c"))))
         (replace 'check
           (lambda _
             ;; FIXME: add sensible check target (without depending on make)
             ;; ./check.sh ?
             (= 1 (status:exit-val (system* "./tcc" "--help")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref %outputs "out"))
                   (tcc (assoc-ref %build-inputs "tcc")))
               (and
                (mkdir-p (string-append out "/bin"))
                (copy-file "tcc" (string-append out "/bin/tcc"))
                (mkdir-p (string-append out "/lib/tcc"))
                (copy-recursively (string-append tcc "/include")
                                  (string-append out "/include"))
                (copy-recursively (string-append tcc "/lib")
                                  (string-append out "/lib"))
                (invoke "tcc" "-D" "TCC_TARGET_I386=1" "-c" "-o" "libtcc1.o" "lib/libtcc1.c")
                (invoke "tcc" "-ar" "rc" "libtcc1.a" "libtcc1.o")
                (copy-file "libtcc1.a" (string-append out "/lib/libtcc1.a"))
                (delete-file (string-append out "/lib/tcc/libtcc1.a"))
                (copy-file "libtcc1.a" (string-append out "/lib/tcc/libtcc1.a"))
                #t)))))))))

(define diffutils-mesboot
  ;; The initial diffutils.
  (package
    (inherit diffutils)
    (name "diffutils-mesboot")
    (version "2.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/diffutils/diffutils-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1mirn5i825bn5w7rh6mgn0r8aj9xqanav95dwcl1b8sn82f4iwnm"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-tcc0-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       #:tests? #f            ; check is naive, also checks non-built PROGRAMS
       #:strip-binaries? #f   ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'remove-diff3-sdiff
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile.in"
               (("PROGRAMS = .*" all) "PROGRAMS = cmp diff"))))
         (replace 'configure           ; needs classic invocation of configure
           (lambda* (#:key configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref %outputs "out"))
                    (bash (assoc-ref %build-inputs "bash"))
                    (shell (string-append bash "/bin/bash")))
               (setenv "CONFIG_SHELL" shell)
               (setenv "CC" "tcc")
               (setenv "LD" "tcc")
               (format (current-error-port)
                       "running ./configure ~a\n" (string-join configure-flags))
               (apply invoke (cons "./configure" configure-flags)))))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (install-file "cmp" bin)
               (install-file "diff" bin)
               #t))))))))

(define patch-mesboot
  ;; The initial patch.
  (package
    (inherit patch)
    (name "patch-mesboot")
    (version "2.5.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/patch/patch-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "12nv7jx3gxfp50y11nxzlnmqqrpicjggw6pcsq0wyavkkm3cddgc"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-tcc0-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       #:tests? #f            ; check is naive, also checks non-built PROGRAMS
       #:strip-binaries? #f   ; no strip yet
       #:configure-flags '("AR=tcc -ar" "CC=tcc" "LD-tcc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda _
             ;; avoid another segfault
             (substitute* "pch.c"
               (("while [(]p_end >= 0[)]" all)
                "p_end = -1;\nwhile (0)"))
             #t))
         ;; FIXME: no compressing gzip yet
         (delete 'compress-documentation))))))

(define sed-mesboot0
  ;; The initial sed.
  (package
    (inherit sed)
    (name "sed-mesboot0")
    (version "1.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/sed/sed-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1hyv7i82jd0q18xcql51ylc8jwadp3gb3irgcqlis3v61p35jsv2"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-tcc0-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       #:configure-flags '("CC=tcc")
       #:make-flags '("CC=tcc" "extra_objs=" "DEFS=-D HAVE_BCOPY")
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bash (assoc-ref %build-inputs "bash"))
                    (shell (string-append bash "/bin/bash")))
               (substitute* "configure"
                 (("/bin/sh") shell))
               #t)))
         (replace 'check
           (lambda _
             (invoke "./sed" "--version")))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "sed" bin)
               #t))))))))

(define (%boot-tcc-inputs)
  `(("bash" ,bash-mesboot0)
    ("bzip2" ,bzip2-mesboot)
    ("diffutils" ,diffutils-mesboot)
    ("gzip" ,gzip-mesboot)
    ("patch" ,patch-mesboot)
    ("sed" ,sed-mesboot0)
    ("tcc" ,tcc-boot)
    ,@(alist-delete "tcc" (%boot-tcc0-inputs))))

(define binutils-mesboot0
  ;; The initial Binutils
  (package
    (inherit binutils)
    (name "binutils-mesboot0")
    (version "2.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/binutils/binutils-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1w8xp7k44bkijr974x9918i4p1sw4g2fcd5mxvspkjpg38m214ds"))))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-tcc-inputs))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:tests? #f                      ; runtest: command not found
       #:parallel-build? #f
       #:strip-binaries? #f             ; no strip yet
       #:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         `("--disable-nls"
           "--disable-shared"
           "--disable-werror"
           "--build=i386-unknown-linux"
           "--host=i386-unknown-linux"
           "--target=i386-unknown-linux"
           "--with-sysroot=/"
           ,(string-append "--prefix=" out)))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'setenv
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bash (assoc-ref %build-inputs "bash"))
                    (shell (string-append bash "/bin/bash")))
               (setenv "CONFIG_SHELL" shell)
               (setenv "SHELL" shell)
               (setenv "AR" "tcc -ar")
               (setenv "RANLIB" "true")
               (setenv "CC" "tcc -D __GLIBC_MINOR__=6")
               #t)))
         (add-after 'unpack 'scripted-patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "bfd/configure"
               (("^sed -e '/SRC-POTFILES.*" all)
                "echo -e 'all:\\n\\ttrue\\n\\ninstall:\\n\\ttrue\\n' > po/Makefile\n"))
             #t))
         (replace 'configure           ; needs classic invocation of configure
           (lambda* (#:key configure-flags #:allow-other-keys)
             (format (current-error-port)
                     "running ./configure ~a\n" (string-join configure-flags))
             (apply system* "./configure" configure-flags)
             (substitute* "config.status"
               (("[.]//dev/null") "/dev/null"))
             (invoke "sh" "./config.status"))))))))

(define gcc-core-mesboot0
  ;; Gcc-2.95.3 is the most recent GCC that is supported by what the Mes C
  ;; Library v0.16 offers.  Gcc-3.x (and 4.x) place higher demands on a C
  ;; library, such as dir.h/struct DIR/readdir, locales, signals...  Also,
  ;; with gcc-2.95.3, binutils (2.14.0, 2.20.1a) and glibc-2.2.5 we found a
  ;; GNU toolchain triplet "that works".
  (package
    (inherit gcc)
    (name "gcc-core-mesboot0")
    (version "2.95.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-2.95.3/gcc-core-"
                                  version
                                  ".tar.gz"))
              ;; `patches' needs XZ
              ;; (patches (search-patches "gcc-boot-2.95.3.patch"))
              (sha256
               (base32
                "1xvfy4pqhrd5v2cv8lzf63iqg92k09g6z9n2ah6ndd4h17k1x0an"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("boot-patch" ,(search-patch "gcc-boot-2.95.3.patch"))
                     ("binutils" ,binutils-mesboot0)
                     ,@(%boot-tcc-inputs)))
    (outputs '("out"))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:tests? #f
       #:parallel-build? #f
       #:strip-binaries? #f
       #:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         `("--enable-static"
           "--disable-shared"
           "--disable-werror"
           "--build=i686-unknown-linux-gnu"
           "--host=i686-unknown-linux-gnu"
           ,(string-append "--prefix=" out)))
       #:make-flags
       `("CC=tcc -static -D __GLIBC_MINOR__=6"
         "OLDCC=tcc -static -D __GLIBC_MINOR__=6"
         "CC_FOR_BUILD=tcc -static -D __GLIBC_MINOR__=6"
         "AR=ar"
         "RANLIB=ranlib"
         ,(string-append "LIBGCC2_INCLUDES=-I "
                         (assoc-ref %build-inputs "tcc")
                         "/include")
         "LANGUAGES=c"
         ,(string-append "BOOT_LDFLAGS="
                         " -B" (assoc-ref %build-inputs "tcc")
                         "/lib/"))
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'apply-boot-patch
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((patch-file (assoc-ref inputs "boot-patch")))
               (system* "patch" "--force" "-p1" "-i" patch-file)
               #t)))
         (add-before 'configure 'setenv
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (assoc-ref %build-inputs "bash"))
                    (shell (string-append bash "/bin/bash"))
                    (tcc (assoc-ref %build-inputs "tcc"))
                    (cppflags " -D __GLIBC_MINOR__=6"))
               (setenv "CONFIG_SHELL" shell)
               (setenv "CPPFLAGS" cppflags)
               (setenv "CC" (string-append "tcc" cppflags))
               (setenv "CC_FOR_BUILD" (string-append "tcc" cppflags))
               (setenv "CPP" (string-append "tcc -E" cppflags))
               (with-output-to-file "config.cache"
                 (lambda _
                   (display "
ac_cv_c_float_format='IEEE (little-endian)'
"))))))
         ;; gcc-2.95.3
         (replace 'configure           ; needs classic invocation of configure
           (lambda* (#:key configure-flags  #:allow-other-keys)
             (format (current-error-port)
                     "running ./configure ~a\n" (string-join configure-flags))
             (apply invoke "./configure" configure-flags)))
         (add-after 'configure 'remove-info
           (lambda _
             ;; no info at this stage
             (delete-file-recursively "texinfo")
             (invoke "touch" "gcc/cpp.info" "gcc/gcc.info")))
         (add-after 'install 'install2
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((tcc (assoc-ref %build-inputs "tcc"))
                    (tcc-lib (string-append tcc "/lib/x86-mes-gcc"))
                    (out (assoc-ref outputs "out"))
                    (gcc-dir (string-append
                              out "/lib/gcc-lib/i686-unknown-linux-gnu/2.95.3")))
               (mkdir-p "tmp")
               (with-directory-excursion "tmp"
                 (invoke "ar" "x" (string-append "../gcc/libgcc2.a"))
                 (invoke "ar" "x" (string-append tcc "/lib/libtcc1.a"))
                 (apply invoke "ar" "r" (string-append gcc-dir "/libgcc.a")
                        (find-files "." "\\.o")))
               (copy-file "gcc/libgcc2.a" (string-append out "/lib/libgcc2.a"))
               (copy-file (string-append tcc "/lib/libtcc1.a")
                          (string-append out "/lib/libtcc1.a"))
               (invoke "ar" "x" (string-append tcc "/lib/libtcc1.a"))
               (invoke "ar" "x" (string-append tcc "/lib/libc.a"))
               (invoke "ar" "r" (string-append gcc-dir "/libc.a")
                       "libc.o" "libtcc1.o")
               #t))))))
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include"

                     ;; Needed to get things like GCC's <stddef.h>.
                     "lib/gcc-lib/i686-unknown-linux-gnu/2.95.3/include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))))))

(define (%boot-mesboot-core-inputs)
  `(("binutils" ,binutils-mesboot0)
    ("gawk" ,gawk-mesboot0)
    ("gcc" ,gcc-core-mesboot0)
    ,@(alist-delete "tcc" (%boot-tcc-inputs))))

(define mesboot-headers
  (package
    (inherit mes-boot)
    (name "mesboot-headers")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("kernel-headers" ,%bootstrap-linux-libre-headers)
                     ,@(%boot-tcc-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:tests? #f
       #:strip-binaries? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (include (string-append out "/include"))
                    (headers (assoc-ref %build-inputs "kernel-headers")))
               (mkdir-p include)
               (copy-recursively "include" out)
               (copy-recursively headers out)
               #t))))))))

(define gawk-mesboot0
  ;; The initial Gawk.
  (package
    (inherit gawk)
    (name "gawk-mesboot0")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gawk/gawk-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "087s7vpc8zawn3l7bwv9f44bf59rc398hvaiid63klw6fkbvabr3"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-tcc-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:configure-flags '("--build=i686-unknown-linux-gnu"
                           "--host=i686-unknown-linux-gnu"
                           "--disable-nls")
       #:make-flags '("gawk")
       #:parallel-build? #f
       #:parallel-tests? #f
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda _
             (substitute* "Makefile.in"
               (("date ") "echo today ")
               ((" autoheader") "true")
               ((" -lm ") " "))
             (substitute* "test/Makefile.in"
               (("^bigtest:.*") "bigtest: basic\n")
               (("( |\t)(childin|convfmt|fflush|longwrds|math|negexp)" all sep) sep))))
         (add-before 'configure 'setenv
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bash (assoc-ref %build-inputs "bash"))
                    (shell (string-append bash "/bin/bash")))
               (setenv "CONFIG_SHELL" shell)
               (setenv "SHELL" shell)
               (setenv "CC" "tcc")
               (setenv "CPP" "tcc -E")
               (setenv "LD" "tcc")
               (setenv "ac_cv_func_getpgrp_void" "yes")
               (setenv "ac_cv_func_tzset" "yes"))
             #t))
         (replace 'configure           ; needs classic invocation of configure
           (lambda* (#:key configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref %outputs "out"))
                    (configure-flags
                     `(,@configure-flags
                       ,(string-append "--prefix=" out))))
               (format (current-error-port) "running ./configure ~a\n" (string-join configure-flags))
               (system* "touch" "configure") ; aclocal.m4 is newer than configure
               (apply invoke (cons "./configure" configure-flags)))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "gawk" bin)
               (symlink "gawk" (string-append bin "/awk"))
               #t))))))))

(define glibc-mesboot0
  ;; GNU C Library 2.2.5 is the most recent glibc that we managed to build
  ;; using gcc-2.95.3.  Newer versions (2.3.x, 2.6, 2.1x) seem to need a newer
  ;; gcc.
  (package
    (inherit glibc)
    (name "glibc-mesboot0")
    (version "2.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/glibc/glibc-"
                                  version
                                  ".tar.gz"))
              ;; Patch needs XZ
              ;; (patches (search-patches "glibc-boot-2.2.5.patch"))
              (sha256
               (base32
                "1vl48i16gx6h68whjyhgnn1s57vqq32f9ygfa2fls7pdkbsqvp2q"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("boot-patch" ,(search-patch "glibc-boot-2.2.5.patch"))
                     ("system-patch" ,(search-patch "glibc-bootstrap-system-2.2.5.patch"))
                     ("headers" ,mesboot-headers)
                     ,@(%boot-mesboot-core-inputs)))
    (outputs '("out"))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:tests? #f
       #:strip-binaries? #f
       #:validate-runpath? #f   ; no dynamic executables
       #:parallel-build? #f     ; gcc-2.95.3 ICEs on massively parallel builds
       #:make-flags (list (string-append
                           "SHELL="
                           (assoc-ref %build-inputs "bash")
                           "/bin/sh"))
       #:configure-flags
       (let ((out (assoc-ref %outputs "out"))
             (headers (assoc-ref %build-inputs "headers")))
         `("--disable-shared"
           "--enable-static"
           "--disable-sanity-checks"
           "--build=i686-unknown-linux-gnu"
           "--host=i686-unknown-linux-gnu"
           ,(string-append "--with-headers=" headers "/include")
           "--enable-static-nss"
           "--without-__thread"
           "--without-cvs"
           "--without-gd"
           "--without-tls"
           ,(string-append "--prefix=" out)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'apply-boot-patch
           (lambda* (#:key inputs #:allow-other-keys)
             (and (let ((patch (assoc-ref inputs "boot-patch")))
                    (invoke "patch" "--force" "-p1" "-i" patch))
                  (let ((patch (assoc-ref inputs "system-patch")))
                    (invoke "patch" "--force" "-p1" "-i" patch)))))
         (add-before 'configure 'setenv
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (assoc-ref %build-inputs "bash"))
                    (shell (string-append bash "/bin/bash"))
                    (gcc (assoc-ref %build-inputs "gcc"))
                    (headers (assoc-ref %build-inputs "headers"))
                    (cppflags (string-append
                               ;;" -D __STDC__=1"
                               " -D MES_BOOTSTRAP=1"
                               " -D BOOTSTRAP_GLIBC=1"))
                    (cflags (string-append " -L " (getcwd))))
               (setenv "CONFIG_SHELL" shell)
               (setenv "SHELL" shell)
               (setenv "CPP" (string-append gcc "/bin/gcc -E " cppflags))
               (setenv "CC" (string-append gcc "/bin/gcc " cppflags cflags))
               #t)))
         (replace 'configure           ; needs classic invocation of configure
           (lambda* (#:key configure-flags #:allow-other-keys)
             (format (current-error-port)
                     "running ./configure ~a\n" (string-join configure-flags))
             (apply invoke "./configure" configure-flags)))
                  (add-after 'configure 'fixup-configure
                    (lambda _
                      (let* ((out (assoc-ref %outputs "out"))
                             (bash (assoc-ref %build-inputs "bash"))
                             (shell (string-append bash "/bin/bash")))
                        (substitute* "config.make"
                          (("INSTALL = scripts/") "INSTALL = $(..)./scripts/"))
                        (substitute* "config.make"
                          (("INSTALL = scripts/") "INSTALL = $(..)./scripts/")
                          (("BASH = ") (string-append
                                        "SHELL = " shell "
         BASH = ")))
                        #t))))))))

(define gcc-mesboot0
  (package
    (inherit gcc-core-mesboot0)
    (name "gcc-mesboot0")
    (native-inputs `(("boot-patch" ,(search-patch "gcc-boot-2.95.3.patch"))
                     ;; Packages are given in an order that's relevant for
                     ;; #include_next purposes.
                     ("libc" ,glibc-mesboot0)
                     ("kernel-headers" ,%bootstrap-linux-libre-headers)
                     ,@(%boot-mesboot-core-inputs)))
    (arguments
     (substitute-keyword-arguments (package-arguments gcc-core-mesboot0)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'setenv
             (lambda _
               (setenv "CONFIG_SHELL" (which "sh"))
               (with-output-to-file "config.cache"
                 (lambda _
                   (display "
ac_cv_c_float_format='IEEE (little-endian)'
")))
               #t))
           (replace 'install2
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (gcc-dir (string-append
                                out "/lib/gcc-lib/i686-unknown-linux-gnu/2.95.3")))
                 (and
                  (mkdir-p "tmp")
                  (zero? (system (string-append "set -x; cd tmp && ar x ../gcc/libgcc2.a")))
                  (zero? (system (string-append "set -x; cd tmp && ar r " gcc-dir "/libgcc.a *.o")))
                  (copy-file "gcc/libgcc2.a" (string-append out "/lib/libgcc2.a"))))))))
       ((#:configure-flags configure-flags)
        `(let ((out (assoc-ref %outputs "out")))
           `("--disable-shared"
             "--disable-werror"
             "--build=i686-unknown-linux-gnu"
             "--host=i686-unknown-linux-gnu"
             ,(string-append "--prefix=" out))))
       ((#:make-flags make-flags)
        `(let ((gcc (assoc-ref %build-inputs "gcc")))
           `("RANLIB=true"
             ,(string-append "LIBGCC2_INCLUDES=-I " gcc "/include")
             "LANGUAGES=c")))))))

(define (%boot-mesboot0-inputs)
  `(("gcc" ,gcc-mesboot0)
    ("kernel-headers" ,%bootstrap-linux-libre-headers)
    ("libc" ,glibc-mesboot0)
    ,@(alist-delete "gcc" (%boot-mesboot-core-inputs))))

(define tar-mesboot
  ;; Initial tar with support for xz compression.
  (package
    (inherit tar)
    (name "tar-mesboot")
    (version  "1.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/tar/tar-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "19nvix64y95n5v6rr5g9g3fn08zz85cb5anzd7csfv4a4sz9lw4y"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-mesboot0-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       #:tests? #f            ; check is naive, also checks non-built PROGRAMS
       #:strip-binaries? #f   ; no strip yet
       #:configure-flags '("--build=i686-unknown-linux-gnu"
                           "--host=i686-unknown-linux-gnu"
                           "--disable-nls")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref %outputs "out"))
                    (bash (assoc-ref %build-inputs "bash"))
                    (shell (string-append bash "/bin/bash")))
               (setenv "CONFIG_SHELL" shell)
               (setenv "SHELL" shell)
               (setenv "LIBS" "-lc -lnss_files -lnss_dns -lresolv")
               (setenv "gl_cv_func_rename_dest_works" "yes")
               (format (current-error-port)
                       "running ./configure ~a\n" (string-join configure-flags))
               (apply invoke (cons "./configure" configure-flags)))))
         (add-after 'unpack 'scripted-patch
           (lambda _
             (let* ((bash (assoc-ref %build-inputs "bash"))
                    (shell (string-append bash "/bin/bash")))
               (substitute* "configure"
                 ((" /bin/sh") shell)))
             (substitute* "Makefile.in"
               (("^SUBDIRS = doc") "SUBDIRS ="))
             #t))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "src/tar" bin)
               #t))))))))

(define grep-mesboot
  ;; The initial grep.
  (package
    (inherit grep)
    (name "grep-mesboot")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/grep/grep-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1w862l80lgc5mxvpiy4cfwk761d6xxavn0m3xd2l7xs2kmzvp6lq"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-mesboot0-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-configure
           (lambda _
             (let* ((bash (assoc-ref %build-inputs "bash"))
                    (shell (string-append bash "/bin/bash")))
               (substitute* "configure"
                 ((" [|][|] ./config.status") " || sh ./config.status")))))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "grep" bin)
               (symlink "grep" (string-append bin "/egrep"))
               (symlink "grep" (string-append bin "/fgrep"))
               #t))))))))

(define binutils-mesboot1
  (package
    (inherit binutils-mesboot0)
    (name "binutils-mesboot1")
    (native-inputs (%boot-mesboot0-inputs))
    (arguments
     (substitute-keyword-arguments (package-arguments binutils-mesboot0)
       ((#:configure-flags configure-flags)
        '(let ((out (assoc-ref %outputs "out")))
           `("--disable-nls"
             "--disable-shared"
             "--disable-werror"
             "--build=i686-unknown-linux-gnu"
             "--host=i686-unknown-linux-gnu"
             "--with-sysroot=/"
             ,(string-append "--prefix=" out))))
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'setenv
             (lambda _
               (let* ((out (assoc-ref %outputs "out"))
                      (bash (assoc-ref %build-inputs "bash"))
                      (shell (string-append bash "/bin/bash")))
                 (setenv "CONFIG_SHELL" shell)
                 #t)))))))))

(define coreutils-mesboot0
  (package
    (inherit coreutils)
    (name "coreutils-mesboot0")
    ;; The latest .gz release of Coreutils is 8.13; which does not build with gcc-2.95.3:
    ;; randperm.c: In function `sparse_swap':
    ;; randperm.c:117: invalid lvalue in unary `&'
    (version "5.0")                     ; 2003-04
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/coreutils/coreutils-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "10wq6k66i8adr4k08p0xmg87ff4ypiazvwzlmi7myib27xgffz62"))))
    (native-inputs (%boot-mesboot0-inputs))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f                      ; WARNING: `perl' is needed, ...
       #:parallel-build? #f
       #:strip-binaries? #f   ; strip: unrecognized option `--only-keep-debug'
       #:guile ,%bootstrap-guile
       #:configure-flags
       '("--disable-doc"
         "LIBS=-lc -lnss_files -lnss_dns -lresolv"
         "ac_cv_func_gethostbyname=no"
         "gl_cv_func_rename_dest_works=yes")))))

(define gnu-make-mesboot
  (package
    (inherit gnu-make)
    (name "make-mesboot")
    (version "3.82")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/make/make-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1rs2f9hmvy3q6zkl15jnlmnpgffm0bhw5ax0h5c7q604wqrip69x"))))
    (native-inputs (%boot-mesboot0-inputs))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (arguments
     `(#:implicit-inputs? #f
       #:parallel-build? #f
       #:guile ,%bootstrap-guile
       #:configure-flags '("LIBS=-lc -lnss_files -lnss_dns -lresolv")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./make" "--version")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "make" bin)
               #t))))))))

(define gawk-mesboot
  (package
    (inherit gawk)
    (name "gawk-mesboot")
    (version "3.1.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gawk/gawk-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "03d5y7jabq7p2s7ys9alay9446mm7i5g2wvy8nlicardgb6b6ii1"))))
    (native-inputs `(,@(%boot-mesboot0-inputs)
                     ("mesboot-headers" ,mesboot-headers)))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (arguments
     `(#:implicit-inputs? #f
       #:parallel-build? #f
       #:guile ,%bootstrap-guile
       #:configure-flags '("ac_cv_func_connect=no")
       #:make-flags '("gawk")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./gawk" "--version")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "gawk" bin)
               (symlink "gawk" (string-append bin "/awk"))
               #t))))))))

(define sed-mesboot
  (package
    (inherit sed)
    (name "sed-mesboot")
    (version "4.0.6")                   ; 2003-04
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/sed/sed-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0861ij94cqc4vaaki6r2wlapwcmhpx4ggp4r70f46mb21a8fkvf1"))))
    (native-inputs (%boot-mesboot0-inputs))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (arguments
     `(#:implicit-inputs? #f
       #:parallel-build? #f
       #:guile ,%bootstrap-guile
       #:tests? #f                      ; 8to7 fails
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack '/bin/sh
           (lambda _
             (let* ((bash (assoc-ref %build-inputs "bash"))
                    (shell (string-append bash "/bin/bash")))
               (substitute* "testsuite/Makefile.tests"
                 (("^SHELL = /bin/sh")
                  (string-append "SHELL = " shell)))
               #t))))))))

(define bash-mesboot
  (package
    (inherit bash-mesboot0)
    (version "4.4")
    (name "bash-mesboot")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/bash/bash-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1jyz6snd63xjn6skk7za6psgidsd53k05cr3lksqybi0q6936syq"))))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-mesboot0-inputs))
    (outputs '("out"))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       #:configure-flags
       '("--build=i686-unknown-linux-gnu"
         "--host=i686-unknown-linux-gnu"

         "--without-bash-malloc"
         "--disable-readline"
         "--disable-history"
         "--disable-help-builtin"
         "--disable-progcomp"
         "--disable-net-redirections"
         "--disable-nls"

         ;; Pretend 'dlopen' is missing so we don't build loadable
         ;; modules and related code.
         "ac_cv_func_dlopen=no")
       #:make-flags '("bash")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda _
             (substitute* "shell.c"
               ((";;") ";"))
             #t))
         (add-before 'configure 'setenv
           (lambda _
             (setenv "AWK" "gawk")
             (setenv "LIBS" "-lc -lnss_files -lnss_dns -lresolv")
             (setenv "gl_cv_func_rename_dest_works" "yes")
             #t))
         (add-after 'configure 'configure-fixups
           (lambda _
             (let ((config.h (open-file "config.h" "a")))
               (display (string-append "
#define enable_hostname_completion(on_or_off) 0
")
                        config.h)
               (close config.h))
             #t))
         (replace 'check
           (lambda _
             (invoke "./bash" "--version")))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (copy-file "bash" (string-append bin "/bash"))
               (copy-file "bash" (string-append bin "/sh"))
               #t))))))))

(define (%boot-mesboot1-inputs)
  `(("bash" ,bash-mesboot)
    ("binutils" ,binutils-mesboot1)
    ("coreutils" ,coreutils-mesboot0)
    ("gawk" ,gawk-mesboot)
    ("grep" ,grep-mesboot)
    ("make" ,gnu-make-mesboot)
    ("sed" ,sed-mesboot)
    ("tar" ,tar-mesboot)
    ,@(fold alist-delete (%boot-mesboot0-inputs)
            '("bash" "binutils" "bootar" "coreutils" "gash"
              "gawk" "grep" "guile" "make" "sed" "tar"))))

(define gmp-boot
  (package
    (inherit gmp)
    (version "4.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gmp/gmp-" version
                                  ".tar.gz"))
              (sha256 (base32
                       "15rwq54fi3s11izas6g985y9jklm3xprfsmym3v1g6xr84bavqvv"))))))

(define mpfr-boot
  (package
    (inherit mpfr)
    (version "2.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/mpfr/mpfr-" version
                                  ".tar.gz"))
              (sha256 (base32
                       "0dxn4904dra50xa22hi047lj8kkpr41d6vb9sd4grca880c7wv94"))))))

(define mpc-boot
  (package
    (inherit mpc)
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/mpc/mpc-" version ".tar.gz"))
              (sha256
               (base32
                "1hzci2zrrd7v3g1jk35qindq05hbl0bhjcyyisq9z209xb3fqzb1"))))))

(define gcc-core-mesboot1
  ;; GCC 4.6.4 is the latest modular distribution.  This package is not
  ;; stricly needed, but very helpful for development because it builds
  ;; relatively fast.  If this configures and builds then gcc-mesboot1 also
  ;; builds.
  (package
    (inherit gcc-mesboot0)
    (name "gcc-core-mesboot1")
    (version "4.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-core-" version ".tar.gz"))
              (sha256
               (base32
                "173kdb188qg79pcz073cj9967rs2vzanyjdjyxy9v0xb0p5sad75"))
              ;; Patch needs XZ
              ;; (patches (search-patches "gcc-boot-4.6.4.patch"))
              ))
    (inputs `(("gmp-source" ,(package-source gmp-boot))
              ("mpfr-source" ,(package-source mpfr-boot))
              ("mpc-source" ,(package-source mpc-boot))))
    (native-inputs `(("boot-patch" ,(search-patch "gcc-boot-4.6.4.patch"))
                     ,@(%boot-mesboot1-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:tests? #f
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:parallel-build? #f             ; for debugging
       #:make-flags
       (let* ((libc (assoc-ref %build-inputs "libc"))
              (ldflags (string-append
                        "-B" libc "/lib "
                        "-Wl,-dynamic-linker "
                        "-Wl," libc
                        ,(glibc-dynamic-linker "i686-linux"))))
         (list (string-append "LDFLAGS=" ldflags)
               (string-append "LDFLAGS_FOR_TARGET=" ldflags)))
       #:configure-flags
       (let ((out (assoc-ref %outputs "out"))
             (glibc (assoc-ref %build-inputs "libc")))
         (list (string-append "--prefix=" out)
               "--build=i686-unknown-linux-gnu"
               "--host=i686-unknown-linux-gnu"
               (string-append "--with-native-system-header-dir=" glibc "/include")
               (string-append "--with-build-sysroot=" glibc "/include")
               "--disable-bootstrap"
               "--disable-decimal-float"
               "--disable-libatomic"
               "--disable-libcilkrts"
               "--disable-libgomp"
               "--disable-libitm"
               "--disable-libmudflap"
               "--disable-libquadmath"
               "--disable-libsanitizer"
               "--disable-libssp"
               "--disable-libvtv"
               "--disable-lto"
               "--disable-lto-plugin"
               "--disable-multilib"
               "--disable-plugin"
               "--disable-threads"
               "--enable-languages=c"
               "--enable-static"
               "--disable-shared"
               "--enable-threads=single"
               "--disable-libstdcxx-pch"
               "--disable-build-with-cxx"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'apply-boot-patch
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((patch-file (assoc-ref inputs "boot-patch")))
               (format (current-error-port) "patch file=~s\n" patch-file)
               (system* "patch" "--force" "-p1" "-i" patch-file))
             #t))
         ;; c&p from commencement.scm:gcc-boot0
         (add-after 'unpack 'unpack-gmp&co
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gmp  (assoc-ref %build-inputs "gmp-source"))
                   (mpfr (assoc-ref %build-inputs "mpfr-source"))
                   (mpc  (assoc-ref %build-inputs "mpc-source")))

               ;; To reduce the set of pre-built bootstrap inputs, build
               ;; GMP & co. from GCC.
               (for-each (lambda (source)
                           (or (invoke "tar" "xvf" source)
                               (error "failed to unpack tarball"
                                      source)))
                         (list gmp mpfr mpc))

               ;; Create symlinks like `gmp' -> `gmp-x.y.z'.
               ,@(map (lambda (lib)
                        ;; Drop trailing letters, as gmp-6.0.0a unpacks
                        ;; into gmp-6.0.0.
                        `(symlink ,(string-trim-right
                                    (package-full-name lib "-")
                                    char-set:letter)
                                  ,(package-name lib)))
                      (list gmp-boot mpfr-boot mpc-boot))
               #t)))
         (add-before 'configure 'setenv
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (binutils (assoc-ref %build-inputs "binutils"))
                    (bash (assoc-ref %build-inputs "bash"))
                    (gcc (assoc-ref %build-inputs "gcc"))
                    (glibc (assoc-ref %build-inputs "libc"))
                    (kernel-headers (assoc-ref %build-inputs "kernel-headers")))
               (setenv "CONFIG_SHELL" (string-append bash "/bin/sh"))
               (setenv "C_INCLUDE_PATH" (string-append
                                         gcc "/lib/gcc-lib/i686-unknown-linux-gnu/2.95.3/include"
                                         ":" kernel-headers "/include"
                                         ":" glibc "/include"
                                         ":" (getcwd) "/mpfr/src"))
               (setenv "LIBRARY_PATH" (string-append glibc "/lib"
                                                     ":" gcc "/lib"))
               (format (current-error-port) "C_INCLUDE_PATH=~a\n" (getenv "C_INCLUDE_PATH"))
               (format (current-error-port) "LIBRARY_PATH=~a\n" (getenv "LIBRARY_PATH"))
               #t))))))))

(define gcc-mesboot1
  (package
    (inherit gcc-core-mesboot1)
    (name "gcc-mesboot1")
    (version "4.6.4")
    (native-inputs
     `(("gcc-g++"
        ,(origin
           (method url-fetch)
           (uri (string-append "mirror://gnu/gcc/gcc-"
                               version "/gcc-g++-" version ".tar.gz"))
           (sha256
            (base32
             "1fqqk5zkmdg4vmqzdmip9i42q6b82i3f6yc0n86n9021cr7ms2k9"))))
       ,@(package-native-inputs gcc-core-mesboot1)))
    (arguments
     (substitute-keyword-arguments (package-arguments gcc-core-mesboot1)
       ((#:configure-flags configure-flags)
        `(let ((out (assoc-ref %outputs "out")))
           `("--enable-languages=c,c++"
             ,@(filter
                (negate (lambda (x) (string-prefix? "--enable-languages=" x)))
                ,configure-flags))))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'unpack 'unpack-g++
             (lambda _
               (let ((source-g++ (assoc-ref %build-inputs "gcc-g++")))
                 (invoke "tar" "xvf" source-g++))
               #t))
           (replace 'setenv
             (lambda _
               (setenv "CONFIG_SHELL" (which "sh"))

               ;; Allow MPFR headers to be found.
               (setenv "C_INCLUDE_PATH"
                       (string-append (getcwd) "/mpfr/src:"
                                      (getenv "C_INCLUDE_PATH")))

               ;; Set the C++ search path so that C headers can be found as
               ;; libstdc++ is being compiled.
               (setenv "CPLUS_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))
               #t))))))))

(define (%boot-mesboot2-inputs)
  `(("gcc" ,gcc-mesboot1)
    ,@(alist-delete "gcc" (%boot-mesboot1-inputs))))

(define xz-mesboot
  ;; Finally, we can build xz.
  (package
    (inherit xz)
    (name "xz-mesboot")
    (version "5.0.0")
    (source (bootstrap-origin
             (origin
               (method url-fetch)
               (uri (list (string-append "http://tukaani.org/xz/xz-" version
                                         ".tar.gz")
                          (string-append "http://multiprecision.org/guix/xz-"
                                         version ".tar.gz")))
               (sha256
                (base32
                 "0kf40ggbs1vaaj5s9k4csycahzqcf65n20pa6lngqhm6j0cj3agb")))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (outputs '("out"))
    (propagated-inputs '())
    (native-inputs (%boot-mesboot2-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       #:configure-flags
       `("--disable-assembler"
         "--disable-shared"
         "--enable-small"
         "--disable-threads"
         "--disable-xzdec"
         "--disable-lzmadec"
         "--disable-lzmainfo"
         "--disable-lzma-links"
         "--disable-scripts"
         "--disable-doc"
         "--disable-nls"
         "--disable-symbol-versions"
         ;; configure disqualifies BASH, CPP, GCC and GREP
         ;; all of which seem fine for the build
         "ac_cv_prog_cc_c99=-std=gnu9x"
         "ac_cv_path_GREP=grep"
         "gl_cv_posix_shell=bash"
         "ac_cv_have_decl_optreset=no"
         "CPPFLAGS=-D__GNUC__=1")))))

(define hello-mesboot
  ;; Check for Scheme-only bootstrap.
  (package
    (inherit hello)
    (name "hello-mesboot")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-mesboot2-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       ;; checking for grep that handles long lines and -e...
       ;; configure: error: no acceptable grep could be found
       #:configure-flags '("ac_cv_path_GREP=grep")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./hello"))))))))

(define binutils-mesboot
  (package
    (inherit binutils)
    (name "binutils-mesboot")
    (version "2.20.1a")
    (source (bootstrap-origin
             (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/binutils/binutils-"
                                   version ".tar.bz2"))
               (patches (search-patches "binutils-boot-2.20.1a.patch"))
               (sha256
                (base32
                 "0r7dr0brfpchh5ic0z9r4yxqn4ybzmlh25sbp30cacqk8nb7rlvi")))))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("xz" ,xz-mesboot)
                     ,@(%boot-mesboot2-inputs)))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:tests? #f                     ; runtest: command not found
       #:parallel-build? #f
       #:strip-binaries? #f            ; no strip yet
       #:configure-flags
       `("CC=gcc"
         "CXX=false"
         "RANLIB=true"
         "--disable-doc"
         "--disable-nls"
         "--disable-shared"
         "--disable-werror"
         "--build=i686-unknown-linux-gnu"
         "--host=i686-unknown-linux-gnu"
         "--with-sysroot=/"
         ;; checking for grep that handles long lines and -e
         "ac_cv_path_GREP=grep")
       ;; FIXME: ac_cv_path_GREP=grep doesn't seem to be forwarded to
       ;; cascading configure's?
       #:make-flags '("ac_cv_path_GREP=grep")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda _
             ;; sed-mesboot0 cannot build these
             (copy-file "binutils/Makefile.in" "binutils/Makefile.in.orig")
             (substitute* "binutils/Makefile.in"
               ;; binutils/binutils uses an amazingly complex install
               ;; command, using FOR, SED, READ, IF, ECHO, SED, SED, AWK,
               ;; READ, and then LIBTOOL (to do something like
               ;; `mkdir $DESTDIR$bindir; cp readline $DESTDIR$bindir ...')

               ;; Some tool [debugme!] cannot handle two escaped newlines
               ;; (bash?), and the install stops after $(am__EXEEXT_11)
               ;; ("objcopy"), so $(am__EXEEXT_13) ("readelf") and others do
               ;; not get installed.  Remove the stray newline:
               (("^\t@BUILD_NLMCONV@ @BUILD_SRCONV@ @BUILD_DLLTOOL@ @BUILD_WINDRES@ .*") ""))
             (substitute* "opcodes/Makefile.in"
               (("^SUBDIRS = [.] po") "SUBDIRS = ."))
             (substitute* "binutils/Makefile.in"
               (("^SUBDIRS = doc po") "SUBDIRS ="))
             (substitute* "gas/Makefile.in"
               (("^SUBDIRS = doc po") "SUBDIRS ="))
             (substitute* "gprof/Makefile.in"
               (("^SUBDIRS = po") "SUBDIRS ="))
             (substitute* "ld/Makefile.in"
               (("^SUBDIRS = po") "SUBDIRS ="))
             #t)))))))

(define (%boot-mesboot3-inputs)
  `(("binutils" ,binutils-mesboot)
    ("xz" ,xz-mesboot)
    ,@(alist-delete "binutils" (%boot-mesboot2-inputs))))

(define glibc-headers-mesboot
  (package
    (inherit glibc-mesboot0)
    (name "glibc-headers-mesboot")
    (version "2.16.0")
    (source (bootstrap-origin
             (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/glibc/glibc-"
                                   version
                                   ".tar.gz"))
               (patches (search-patches "glibc-boot-2.16.0.patch"
                                        "glibc-bootstrap-system-2.16.0.patch"))
               (sha256
                (base32
                 "0vlz4x6cgz7h54qq4528q526qlhnsjzbsvgc4iizn76cb0bfanx7")))))
    (native-inputs `(("headers" ,mesboot-headers)
                     ,@(%boot-mesboot3-inputs)))
    (arguments
     (substitute-keyword-arguments (package-arguments glibc-mesboot0)
       ((#:configure-flags configure-flags)
        `(let ((out (assoc-ref %outputs "out"))
               (headers (assoc-ref %build-inputs "headers")))
           (list
            (string-append "--prefix=" out)
            "--disable-obsolete-rpc"
            "--host=i686-unknown-linux-gnu"
            (string-append "--with-headers=" headers "/include")
            "--enable-static-nss"
            "--with-pthread"
            "--without-cvs"
            "--without-gd"
            "--enable-add-ons=nptl"
            ;; avoid: configure: error: confusing output from nm -u
            "libc_cv_predef_stack_protector=no")))
       ((#:make-flags make-flags)
        '(list "install-bootstrap-headers=yes" "install-headers"))
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'apply-boot-patch)
           (delete 'fixup-configure)
           (delete 'set-path)
           (replace 'unpack
             (lambda* (#:key source #:allow-other-keys)
               (invoke "tar" "xvf" source)
               (chdir (string-append "glibc-" ,version))
               #t))
           (replace 'setenv
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((headers  (assoc-ref inputs "headers"))
                      (libc     (assoc-ref inputs "libc"))
                      (gcc      (assoc-ref inputs "gcc"))
                      (cppflags (string-append
                                 " -I " (getcwd) "/nptl/sysdeps/pthread/bits"
                                 " -D BOOTSTRAP_GLIBC=1"))
                      (cflags (string-append " -L " (getcwd)
                                             " -L " libc "/lib")))
                 (setenv "libc_cv_friendly_stddef" "yes")
                 (setenv "CONFIG_SHELL" (which "sh"))
                 (setenv "SHELL" (which "sh"))

                 (setenv "CPP" (string-append gcc "/bin/gcc -E " cppflags))
                 (setenv "CC" (string-append gcc "/bin/gcc " cppflags cflags))
                 (setenv "LD" "gcc")

                 ;; avoid -fstack-protector
                 (setenv "libc_cv_ssp" "false")
                 (substitute* "configure"
                   (("/bin/pwd") "pwd"))
                 #t)))
           (replace 'install
             (lambda* (#:key outputs make-flags #:allow-other-keys)
               (let ((kernel-headers (assoc-ref %build-inputs "kernel-headers"))
                     (out (assoc-ref outputs "out")))
                 (and (apply invoke "make" make-flags)
                      (copy-recursively kernel-headers out)
                      #t))))
           (replace 'configure
             (lambda* (#:key configure-flags #:allow-other-keys)
               (format (current-error-port) "running ../configure ~a\n" (string-join configure-flags))
               (mkdir-p "build")
               (chdir "build")
               (apply invoke "../configure" configure-flags)))
           (add-after 'configure 'remove-sunrpc
             (lambda _
               (let* ((out (assoc-ref %outputs "out"))
                      (bash (assoc-ref %build-inputs "bash"))
                      (shell (string-append bash "/bin/bash")))

                 (let ((Makefile (open-file "Makefile" "a")))
                   (display (string-append "

SHELL := " shell "
")
                            Makefile)
                   (close Makefile))
                 (substitute* "../Makefile"
                   (("^SHELL := /bin/sh") (string-append "SHELL := " shell)))
                 (substitute* "../Makeconfig"
                   (("^SHELL := /bin/sh") (string-append "SHELL := " shell)))
                 (substitute* "../elf/Makefile"
                   (("^SHELL := /bin/sh") (string-append "SHELL := " shell)))
                 (invoke "make" (string-append (getcwd) "/sysd-sorted" ))
                 (substitute* "sysd-sorted"
                   ((" sunrpc") " ")
                   ((" nis") " "))
                 #t)))))))))

(define glibc-mesboot
  (package
    (inherit glibc-headers-mesboot)
    (name "glibc-mesboot")
    (native-inputs `(("headers" ,glibc-headers-mesboot)
                     ,@(%boot-mesboot3-inputs)))
    (arguments
     `(#:validate-runpath? #f ; fails when using --enable-shared
       ,@(substitute-keyword-arguments (package-arguments glibc-headers-mesboot)
           ((#:make-flags make-flags)
            `(let ((bash (assoc-ref %build-inputs "bash")))
               (list (string-append "SHELL=" bash "/bin/sh"))))
           ((#:phases phases)
            `(modify-phases ,phases
               (replace 'install
                 (lambda* (#:key outputs make-flags #:allow-other-keys)
                   (let* ((kernel-headers (assoc-ref %build-inputs "kernel-headers"))
                          (out (assoc-ref outputs "out"))
                          (install-flags (cons "install" make-flags)))
                     (and (apply invoke "make" install-flags)
                          (copy-recursively kernel-headers out)
                          #t)))))))))))

(define (%boot-mesboot4-inputs)
  `(("libc" ,glibc-mesboot)
    ,@(alist-delete "libc" (%boot-mesboot3-inputs))))

(define gcc-mesboot1-wrapper
  ;; We need this so gcc-mesboot1 can be used to create shared binaries that
  ;; have the correct interpreter, otherwise configuring gcc-mesboot using
  ;; --enable-shared will fail.
  (package
    (inherit gcc-mesboot1)
    (name "gcc-mesboot1-wrapper")
    (source #f)
    (inputs '())
    (native-inputs `(("bash" ,bash-mesboot)
                     ("coreutils" ,coreutils-mesboot0)
                     ("libc" ,glibc-mesboot)
                     ("gcc" ,gcc-mesboot1)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:phases
       (modify-phases %standard-phases
         (delete 'unpack)
         (delete 'configure)
         (delete 'install)
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (assoc-ref %build-inputs "bash"))
                    (libc (assoc-ref %build-inputs "libc"))
                    (gcc (assoc-ref %build-inputs "gcc"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (for-each
                (lambda (program)
                  (let ((wrapper (string-append bin "/" program)))
                    (with-output-to-file wrapper
                      (lambda _
                        (display (string-append "#! " bash "/bin/bash
exec " gcc "/bin/" program
" -Wl,--dynamic-linker"
;; also for x86_64-linux, we are still on i686-linux
" -Wl," libc ,(glibc-dynamic-linker "i686-linux")
" -Wl,--rpath"
" -Wl," libc "/lib"
" \"$@\"
"))
                        (chmod wrapper #o555)))))
                '("cpp"
                  "gcc"
                  "g++"
                  "i686-unknown-linux-gnu-cpp"
                  "i686-unknown-linux-gnu-gcc"
                  "i686-unknown-linux-gnu-g++"))
               #t)))
         (replace 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (program (string-append bin "/gcc")))
               (invoke program "--help")))))))))

(define gcc-mesboot
  (package
    (inherit gcc-mesboot1)
    (name "gcc-mesboot")
    (version (package-version gcc-4.9))
    (source (bootstrap-origin (package-source gcc-4.9)))
    (native-inputs `(("gcc-wrapper" ,gcc-mesboot1-wrapper)
                     ("gcc" ,gcc-mesboot1)
                     ("headers" ,glibc-headers-mesboot)
                     ,@(%boot-mesboot4-inputs)))
    (arguments
     `(#:validate-runpath? #f
       ,@(substitute-keyword-arguments (package-arguments gcc-mesboot1)
           ((#:configure-flags configure-flags)
            `(let ((out (assoc-ref %outputs "out"))
                   (glibc (assoc-ref %build-inputs "libc")))
               (list (string-append "--prefix=" out)
                     "--build=i686-unknown-linux-gnu"
                     "--host=i686-unknown-linux-gnu"

                     "--with-host-libstdcxx=-lsupc++"

                     (string-append "--with-native-system-header-dir=" glibc "/include")
                     (string-append "--with-build-sysroot=" glibc "/include")

                     "--disable-bootstrap"
                     "--disable-decimal-float"
                     "--disable-libatomic"
                     "--disable-libcilkrts"
                     "--disable-libgomp"
                     "--disable-libitm"
                     "--disable-libmudflap"
                     "--disable-libquadmath"
                     "--disable-libsanitizer"
                     "--disable-libssp"
                     "--disable-libvtv"
                     "--disable-lto"
                     "--disable-lto-plugin"
                     "--disable-multilib"
                     "--disable-plugin"
                     "--disable-threads"
                     "--enable-languages=c,c++"

                     "--enable-static"
                     "--enable-shared"
                     "--enable-threads=single"

                     ;; No pre-compiled libstdc++ headers, to save space.
                     "--disable-libstdcxx-pch"

                     ;; for libcpp ...
                     "--disable-build-with-cxx")))
           ((#:phases phases)
            `(modify-phases ,phases
               (delete 'apply-boot-patch)
               (delete 'unpack-g++)     ; sadly, gcc-4.9.4 does not provide
                                        ; modular core/language downloads
               (replace 'setenv
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (binutils (assoc-ref %build-inputs "binutils"))
                          (bash (assoc-ref %build-inputs "bash"))
                          (gcc (assoc-ref %build-inputs "gcc"))
                          (glibc (assoc-ref %build-inputs "libc"))
                          (kernel-headers (assoc-ref %build-inputs "kernel-headers")))
                     (setenv "CONFIG_SHELL" (string-append bash "/bin/sh"))
                     (setenv "C_INCLUDE_PATH" (string-append
                                               gcc "/lib/gcc-lib/i686-unknown-linux-gnu/4.6.4/include"
                                               ":" kernel-headers "/include"
                                               ":" glibc "/include"
                                               ":" (getcwd) "/mpfr/src"))
                     (setenv "CPLUS_INCLUDE_PATH" (string-append
                                                   gcc "/lib/gcc-lib/i686-unknown-linux-gnu/4.6.4/include"
                                                   ":" kernel-headers "/include"
                                                   ":" glibc "/include"
                                                   ":" (getcwd) "/mpfr/src"))
                     (setenv "LIBRARY_PATH" (string-append glibc "/lib"
                                                           ":" gcc "/lib"))
                     (format (current-error-port) "C_INCLUDE_PATH=~a\n" (getenv "C_INCLUDE_PATH"))
                     (format (current-error-port) "CPLUS_INCLUDE_PATH=~a\n" (getenv "CPLUS_INCLUDE_PATH"))
                     (format (current-error-port) "LIBRARY_PATH=~a\n" (getenv "LIBRARY_PATH"))
                     #t))))))))))

(define gcc-mesboot-wrapper
  ;; We need this so gcc-mesboot can be used to create shared binaries that
  ;; have the correct interpreter and runpath to libc.
  (package
    (inherit gcc-mesboot1-wrapper)
    (name "gcc-mesboot-wrapper")
    (version (package-version gcc-mesboot))
    (source #f)
    (inputs '())
    (native-inputs `(("bash" ,bash-mesboot)
                     ("coreutils" ,coreutils-mesboot0)
                     ("libc" ,glibc-mesboot)
                     ("gcc" ,gcc-mesboot)))))

(define (%boot-mesboot5-inputs)
  `(("gcc-wrapper" ,gcc-mesboot-wrapper)
    ("gcc" ,gcc-mesboot)
    ,@(fold alist-delete (%boot-mesboot4-inputs) '("gcc" "gcc-wrapper"))))

(define coreutils-mesboot
  (package
    (inherit coreutils)
    (name "coreutils-mesboot")
    (source (bootstrap-origin (package-source coreutils)))
    (native-inputs (%boot-mesboot5-inputs))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:tests? #f))))

(define (%boot-mesboot6-inputs)
  `(("coreutils" ,coreutils-mesboot)
    ,@(fold alist-delete (%boot-mesboot5-inputs)
            '("coreutils" "kernel-headers"))))

(define (%bootstrap-inputs+toolchain)
  ;; The traditional bootstrap-inputs.  For the i686-linux, x86_64-linux
  ;; Scheme-only bootstrap the actual reduced set with bootstrapped toolchain.
  (match (%current-system)
    ((or "i686-linux" "x86_64-linux")
     (%boot-mesboot6-inputs))
    (_
     (%bootstrap-inputs))))

(define gnu-make-boot0
  (package
    (inherit gnu-make)
    (source (bootstrap-origin (package-source gnu-make)))
    (name "make-boot0")
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       #:tests? #f                                ; cannot run "make check"
       ,@(substitute-keyword-arguments (package-arguments gnu-make)
           ((#:configure-flags flags ''())
            ;; The generated config.status has some problems due to the
            ;; bootstrap environment.  Disable dependency tracking to work
            ;; around it.
            `(cons "--disable-dependency-tracking" ,flags))
           ((#:phases phases)
            `(modify-phases ,phases
               (replace 'build
                 (lambda _
                   (invoke "./build.sh")))
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin")))
                     (install-file "make" bin)
                     #t))))))))
    (native-inputs '())                           ; no need for 'pkg-config'
    (inputs (%bootstrap-inputs+toolchain))))

(define bzip2-boot0
  (package
    (inherit bzip2)
    (name "bzip2-boot0")
    (native-inputs `())
    (inputs
     `(("diffutils" ,diffutils-boot0)
       ("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       ,@(package-arguments bzip2)))))

(define coreutils-boot0
  (package
    (inherit coreutils)
    (source (bootstrap-origin (package-source coreutils)))
    (name "coreutils-boot0")
    (native-inputs `())
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:tests? #f
       #:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       ,@(package-arguments coreutils)))))

(define diffutils-boot0
  (package
    (inherit diffutils)
    (name "diffutils-boot0")
    (native-inputs `())
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:tests? #f                            ; the test suite needs diffutils
       #:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       ,@(match (%current-system)
           ((or "arm-linux" "aarch64-linux")
            (substitute-keyword-arguments (package-arguments diffutils)
              ((#:configure-flags flags ''())
               ;; The generated config.status has some problems due to the
               ;; bootstrap environment.  Disable dependency tracking to work
               ;; around it.
               `(cons "--disable-dependency-tracking" ,flags))))
           (_ '()))))))

(define findutils-boot0
  (package
    (inherit findutils)
    (name "findutils-boot0")
    (source (bootstrap-origin (package-source findutils)))
    (inputs
     `(("make" ,gnu-make-boot0)
       ("diffutils" ,diffutils-boot0) ; for tests
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile

       ;; The build system assumes we have done a mistake when time_t is 32-bit
       ;; on a 64-bit system.  Ignore that for our bootstrap toolchain.
       ,@(substitute-keyword-arguments (package-arguments findutils)
           ((#:configure-flags flags ''())
            `(append
              ,(if (target-64bit?)
                   ''("TIME_T_32_BIT_OK=yes")
                   ''())
              ,(match (%current-system)
                 ((or "arm-linux" "aarch64-linux")
                  ''("--disable-dependency-tracking"))
                 (_ ''()))
              ,flags)))))))

(define file
  (package
    (inherit (@ (gnu packages file) file))
    (arguments
     `(#:configure-flags
       `("--disable-bzlib"
         ,,@(match (%current-system)
              ((or "arm-linux" "aarch64-linux")
               '("--disable-dependency-tracking"))
              (_ '())))))))

(define file-boot0
  (package
    (inherit file)
    (source (bootstrap-origin (package-source file)))
    (name "file-boot0")
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:tests? #f                      ; merge test fails
       #:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:configure-flags '("--disable-bzlib")
       #:strip-binaries? #f
       #:validate-runpath? #f
       ,@(package-arguments file)))))

(define gawk-boot0
  (package
    (inherit patch)
    (source (bootstrap-origin (package-source gawk)))
    (name "gawk-boot0")
    (native-inputs '())
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:tests? #f
       #:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:strip-binaries? #f
       #:validate-runpath? #f))))

(define patch-boot0
  (package
    (inherit patch)
    (source (bootstrap-origin (package-source patch)))
    (name "patch-boot0")
    (native-inputs '())
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:tests? #f                      ; merge test fails
       #:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:strip-binaries? #f
       #:validate-runpath? #f))))

(define sed-boot0
  (package
    (inherit sed)
    (name "sed-boot0")
    (source (bootstrap-origin (package-source sed)))
    (native-inputs '())
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       ,@(package-arguments sed)))))

(define tar-boot0
  (package
    (inherit tar)
    (name "tar-boot0")
    (source (bootstrap-origin (package-source tar)))
    (native-inputs '())
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       ,@(package-arguments tar)))))

(define (%boot0-inputs)
  `(,@(match (%current-system)
        ((or "i686-linux" "x86_64-linux")
         `(("bzip2" ,bzip2-boot0)
           ("coreutils" ,coreutils-boot0)
           ("gawk" ,gawk-boot0)
           ("patch" ,patch-boot0)
           ("sed" ,sed-boot0)
           ("tar" ,tar-boot0)))
        (_ '()))
    ("make" ,gnu-make-boot0)
    ("diffutils" ,diffutils-boot0)
    ("findutils" ,findutils-boot0)
    ("file" ,file-boot0)
    ,@(%bootstrap-inputs+toolchain)))

(define* (boot-triplet #:optional (system (%current-system)))
  ;; Return the triplet used to create the cross toolchain needed in the
  ;; first bootstrapping stage.
  (nix-system->gnu-triplet system "guix"))

;; Following Linux From Scratch, build a cross-toolchain in stage 0.  That
;; toolchain actually targets the same OS and arch, but it has the advantage
;; of being independent of the libc and tools in
;; (%BOOTSTRAP-INPUTS+TOOLCHAIN), since GCC-BOOT0 (below) is built without any
;; reference to the target libc.

(define binutils-boot0
  (package
    (inherit binutils)
    (source (bootstrap-origin (package-source binutils)))
    (name "binutils-cross-boot0")
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f

       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw))                    ; for 'scandir'
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'add-symlinks
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; The cross-gcc invokes 'as', 'ld', etc, without the
                      ;; triplet prefix, so add symlinks.
                      (let ((out (assoc-ref outputs "out"))
                            (triplet-prefix (string-append ,(boot-triplet) "-")))
                        (define (has-triplet-prefix? name)
                          (string-prefix? triplet-prefix name))
                        (define (remove-triplet-prefix name)
                          (substring name (string-length triplet-prefix)))
                        (with-directory-excursion (string-append out "/bin")
                          (for-each (lambda (name)
                                      (symlink name (remove-triplet-prefix name)))
                                    (scandir "." has-triplet-prefix?)))
                        #t))))

       ,@(substitute-keyword-arguments (package-arguments binutils)
           ((#:configure-flags cf)
            `(cons ,(string-append "--target=" (boot-triplet))
                   ,cf)))))
    (inputs (%boot0-inputs))))

(define libstdc++-boot0
  ;; GCC's libcc1 is always built as a shared library (the top-level
  ;; 'Makefile.def' forcefully adds --enable-shared) and thus needs to refer
  ;; to libstdc++.so.  We cannot build libstdc++-5.3 because it relies on
  ;; C++14 features missing in some of our bootstrap compilers.
  (let ((lib (make-libstdc++ gcc-4.9)))
    (package
      (inherit lib)
      (source (bootstrap-origin (package-source lib)))
      (name "libstdc++-boot0")
      (arguments
       `(#:guile ,%bootstrap-guile
         #:implicit-inputs? #f

         ;; XXX: libstdc++.so NEEDs ld.so for some reason.
         #:validate-runpath? #f

         ,@(match (%current-system)
             ((or "i686-linux" "x86_64-linux")
              (substitute-keyword-arguments (package-arguments lib)
                ((#:phases phases)
                 `(modify-phases ,phases
                    (add-after 'unpack 'workaround-wrapper-bug
                      ;; XXX: The crude gcc-cross-wrapper causes "g++ -v" to
                      ;; fail, which in turn confuses the configure script.
                      (lambda _
                        (substitute* "libstdc++-v3/configure"
                          (("g\\+\\+ -v") "true"))
                        #t))))))
             (_ (package-arguments lib)))))
      (inputs (%boot0-inputs))
      (native-inputs '()))))

(define gcc-boot0
  (package
    (inherit gcc)
    (name "gcc-cross-boot0")
    (source (bootstrap-origin (package-source gcc)))
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 regex)
                  (srfi srfi-1)
                  (srfi srfi-26))
       ,@(substitute-keyword-arguments (package-arguments gcc)
           ((#:configure-flags flags)
            `(append (list ,(string-append "--target=" (boot-triplet))

                           ;; No libc yet.
                           "--without-headers"

                           ;; Disable features not needed at this stage.
                           "--disable-shared"
                           "--enable-languages=c,c++"

                           ;; boot-triplet inserts "guix" in the triplet.
                           ,@(if (equal? "powerpc64le-guix-linux-gnu" (boot-triplet))
                                 ;; On POWER9 (little endian) glibc needs the
                                 ;; 128-bit long double type.
                                 '("--with-long-double-128")
                                 '())

                           ;; libstdc++ cannot be built at this stage
                           ;; ("Link tests are not allowed after
                           ;; GCC_NO_EXECUTABLES.").
                           "--disable-libstdc++-v3"

                           "--disable-threads"
                           "--disable-libmudflap"
                           "--disable-libatomic"
                           "--disable-libsanitizer"
                           "--disable-libitm"
                           "--disable-libgomp"
                           "--disable-libmpx"
                           "--disable-libcilkrts"
                           "--disable-libvtv"
                           "--disable-libssp"
                           "--disable-libquadmath"
                           "--disable-decimal-float")
                     (remove (cut string-match
                               "--(with-system-zlib|enable-languages.*)" <>)
                             ,flags)))
           ((#:make-flags flags)
            `(let* ((libc        (assoc-ref %build-inputs "libc"))
                    (libc-native (or (assoc-ref %build-inputs "libc-native")
                                     libc)))
               `(,(string-append "LDFLAGS="
                                 "-Wl,-rpath=" libc-native "/lib "
                                 "-Wl,-dynamic-linker "
                                 "-Wl," libc-native ,(glibc-dynamic-linker
                                                      (match (%current-system)
                                                        ("x86_64-linux" "i686-linux")
                                                        (_ (%current-system))))))))
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'unpack 'unpack-gmp&co
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((gmp  (assoc-ref %build-inputs "gmp-source"))
                         (mpfr (assoc-ref %build-inputs "mpfr-source"))
                         (mpc  (assoc-ref %build-inputs "mpc-source")))

                     ;; To reduce the set of pre-built bootstrap inputs, build
                     ;; GMP & co. from GCC.
                     (for-each (lambda (source)
                                 (invoke "tar" "xvf" source))
                               (list gmp mpfr mpc))

                     ;; Create symlinks like `gmp' -> `gmp-x.y.z'.
                     ,@(map (lambda (lib)
                              ;; Drop trailing letters, as gmp-6.0.0a unpacks
                              ;; into gmp-6.0.0.
                              `(symlink ,(string-trim-right
                                          (package-full-name lib "-")
                                          char-set:letter)
                                        ,(package-name lib)))
                            (list gmp-6.0 mpfr mpc))
                     #t)))
               ,(match (%current-system)
                  ((or "i686-linux" "x86_64-linux")
                   '(add-before 'configure 'fix-libcc1
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; libcc1.so NEEDs libgcc_s.so, so provide one here
                        ;; to placate the 'validate-runpath' phase.
                        (substitute* "libcc1/Makefile.in"
                          (("la_LDFLAGS =")
                           (string-append "la_LDFLAGS = -Wl,-rpath="
                                          (assoc-ref inputs "gcc") "/lib")))
                        ;; XXX: "g++ -v" is broken (see also libstdc++ above).
                        (substitute* "libcc1/configure"
                          (("g\\+\\+ -v") "true"))
                        #t)))
                  (_ '(add-before 'configure 'return-true
                        (lambda _ #t))))
               (add-after 'install 'symlink-libgcc_eh
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "lib")))
                     ;; Glibc wants to link against libgcc_eh, so provide
                     ;; it.
                     (with-directory-excursion
                         (string-append out "/lib/gcc/"
                                        ,(boot-triplet)
                                        "/" ,(package-version gcc))
                       (symlink "libgcc.a" "libgcc_eh.a"))
                     #t))))))))

    (inputs `(("gmp-source" ,(bootstrap-origin (package-source gmp-6.0)))
              ("mpfr-source" ,(bootstrap-origin (package-source mpfr)))
              ("mpc-source" ,(bootstrap-origin (package-source mpc)))
              ("binutils-cross" ,binutils-boot0)

              ;; The libstdc++ that libcc1 links against.
              ("libstdc++" ,libstdc++-boot0)

              ;; Call it differently so that the builder can check whether
              ;; the "libc" input is #f.
              ("libc-native" ,@(assoc-ref (%boot0-inputs) "libc"))
              ,@(alist-delete "libc" (%boot0-inputs))))

    ;; No need for the native-inputs to build the documentation at this stage.
    (native-inputs `())))

(define perl-boot0
  (package
    (inherit perl)
    (name "perl-boot0")
    (source (bootstrap-origin (package-source perl)))
    (inputs (%boot0-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:validate-runpath? #f

       ;; At the very least, this must not depend on GCC & co.
       #:disallowed-references ,(list %bootstrap-binutils)

       ,@(substitute-keyword-arguments (package-arguments perl)
           ((#:phases phases)
            `(modify-phases ,phases
               ;; Pthread support is missing in the bootstrap compiler
               ;; (broken spec file), so disable it.
               (add-before 'configure 'disable-pthreads
                 (lambda _
                   (substitute* "Configure"
                     (("^libswanted=(.*)pthread" _ before)
                      (string-append "libswanted=" before)))
                   #t))))
           ;; Do not configure with '-Dusethreads' since pthread
           ;; support is missing.
           ((#:configure-flags configure-flags)
            `(delete "-Dusethreads" ,configure-flags)))))))

(define m4-boot0
  (package
    (inherit m4)
    (name "m4-boot0")
    (source (bootstrap-origin (package-source m4)))
    (inputs (%boot0-inputs))
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       ,@(package-arguments m4)))))

(define bison-boot0
  ;; This Bison is needed to build MiG so we need it early in the process.
  ;; Recent versions of Linux-Libre headers also depend on this.
  (package
    (inherit bison)
    (name "bison-boot0")
    (propagated-inputs `(("m4" ,m4-boot0)))
    (native-inputs `(("perl" ,perl-boot0)))
    (inputs (%boot0-inputs))                      ;remove Flex...
    (arguments
     `(#:tests? #f                                ;... and thus disable tests
       #:implicit-inputs? #f
       #:guile ,%bootstrap-guile

       ;; Zero timestamps in liby.a; this must be done
       ;; explicitly here because the bootstrap Binutils don't
       ;; do that (default is "cru".)
       #:make-flags `("ARFLAGS=crD"
                      ,,(match (%current-system)
                          ;; ranlib: '-D': No such file
                          ((or "i686-linux" "x86_64-linux")
                           "RANLIB=ranlib")
                          (_
                           "RANLIB=ranlib -D"))
                      "V=1")

       ,@(package-arguments bison)))))

(define flex-boot0
  ;; This Flex is needed to build MiG as well as Linux-Libre headers.
  (package
    (inherit flex)
    (native-inputs `(("bison" ,bison-boot0)))
    (propagated-inputs `(("m4" ,m4-boot0)))
    (inputs (%boot0-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:tests? #f))))

(define rsync-boot0
  (package
    (inherit rsync)
    (native-inputs `(("perl" ,perl-boot0)))
    (inputs (%boot0-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile))))

(define-syntax define/system-dependent
  (lambda (s)
    "Bind IDENTIFIER to EXP, where the value of EXP is known to depend on
'%current-system'.  The definition ensures that (1) EXP is \"thunked\" so that
it sees the right value of '%current-system', and (2) that its result is
memoized as a function of '%current-system'."
    (syntax-case s ()
      ((_ identifier exp)
       (with-syntax ((memoized (datum->syntax #'identifier
                                              (symbol-append
                                               (syntax->datum #'identifier)
                                               '/memoized))))
         #'(begin
             (define memoized
               (mlambda (system) exp))
             (define-syntax identifier
               (identifier-syntax (memoized (%current-system))))))))))

(define/system-dependent linux-libre-headers-boot0
  ;; Note: this is wrapped in a thunk to nicely handle circular dependencies
  ;; between (gnu packages linux) and this module.  Additionally, memoize
  ;; the result to play well with further memoization and code that relies
  ;; on pointer identity; see <https://bugs.gnu.org/30155>.
  (package
    (inherit linux-libre-headers)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       ,@(package-arguments linux-libre-headers)))
    (native-inputs
     `(("perl" ,perl-boot0)

       ;; Flex and Bison are required since version 4.16.
       ("flex" ,flex-boot0)
       ("bison" ,bison-boot0)

       ;; Rsync is required since version 5.3.
       ("rsync" ,rsync-boot0)
       ,@(%boot0-inputs)))))

(define with-boot0
  (package-with-explicit-inputs %boot0-inputs
                                %bootstrap-guile))

(define gnumach-headers-boot0
  (with-boot0
   (package-with-bootstrap-guile
    (package
      (inherit gnumach-headers)
      (version "1.8-116-g28b53508")
      (source (bootstrap-origin
               (origin
                 (method url-fetch)
                 (uri (string-append "https://lilypond.org/janneke/hurd/"
                                     "gnumach-" version ".tar.gz"))
                 (sha256
                  (base32
                   "006i0zgwy81vxarpfm12vip4q6i5mgmi5mmy5ldvxp5hx9h3l0zg")))))
      (native-inputs '())))))

(define mig-boot0
  (let* ((mig (package
                 (inherit (package-with-bootstrap-guile mig))
                 (native-inputs `(("bison" ,bison-boot0)
                                  ("flex" ,flex-boot0)))
                 (inputs `(("flex" ,flex-boot0)))
                 (arguments
                  `(#:configure-flags
                    `(,(string-append "LDFLAGS=-Wl,-rpath="
                                      (assoc-ref %build-inputs "flex") "/lib/")))))))
    (with-boot0 mig)))

(define hurd-version-boot0 "0.9-229-ga1efcee8")
(define hurd-source-boot0
  (let ((version hurd-version-boot0))
    (bootstrap-origin
     (origin
       (method url-fetch)
       (uri (string-append "https://lilypond.org/janneke/hurd/"
                           "hurd-v" version ".tar.gz"))
       (sha256
        (base32
         "0bq2q2jisxcy0kgcm6rz0z2fddwxxm7azsama7li28a2m08kdpzy"))))))

(define hurd-headers-boot0
  (let ((hurd-headers (package (inherit hurd-headers)
                               (version hurd-version-boot0)
                               (source hurd-source-boot0)
                               (native-inputs `(("mig" ,mig-boot0)))
                               (inputs '()))))
    (with-boot0 (package-with-bootstrap-guile hurd-headers))))

(define hurd-minimal-boot0
  (let ((hurd-minimal (package (inherit hurd-minimal)
                               (version hurd-version-boot0)
                               (source hurd-source-boot0)
                               (native-inputs `(("mig" ,mig-boot0)))
                               (inputs '()))))
    (with-boot0 (package-with-bootstrap-guile hurd-minimal))))

(define/system-dependent hurd-core-headers-boot0
  ;; Return the Hurd and Mach headers as well as initial Hurd libraries for
  ;; the bootstrap environment.
  (package (inherit (package-with-bootstrap-guile hurd-core-headers))
           (arguments `(#:guile ,%bootstrap-guile
                        ,@(package-arguments hurd-core-headers)))
           (inputs
            `(("gnumach-headers" ,gnumach-headers-boot0)
              ("hurd-headers" ,hurd-headers-boot0)
              ("hurd-minimal" ,hurd-minimal-boot0)
              ,@(%boot0-inputs)))))

(define* (kernel-headers-boot0 #:optional (system (%current-system)))
  (match system
    ("i586-gnu" hurd-core-headers-boot0)
    (_ linux-libre-headers-boot0)))

(define texinfo-boot0
  ;; Texinfo used to build libc's manual.
  ;; We build without ncurses because it fails to build at this stage, and
  ;; because we don't need the stand-alone Info reader.
  ;; Also, use (%BOOT0-INPUTS) to avoid building Perl once more.
  (package
    (inherit texinfo)
    (native-inputs '())
    (inputs `(,@(%boot0-inputs)
              ("perl" ,perl-boot0)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile

       ;; Some of Texinfo 6.1's tests would fail with "Couldn't set UTF-8
       ;; character type in locale" but we don't have a UTF-8 locale at this
       ;; stage, so skip them.
       #:tests? #f))))

(define expat-sans-tests
  (package
    (inherit expat)
    (inputs (%boot0-inputs))
    (arguments
     ;; XXX: Linking 'runtestscpp' fails with things like:
     ;;
     ;;   ld: Dwarf Error: found dwarf version '3789', this reader only handles version 2 and 3 information.
     ;;
     ;; Skip tests altogether.
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile

       ,@(substitute-keyword-arguments (package-arguments expat)
           ((#:configure-flags flags ''())
            ;; Since we're not passing the right -Wl,-rpath flags, build the
            ;; static library to avoid RUNPATH validation failure.
            `(cons "--disable-shared" ,flags))
           ((#:tests? _ #f) #f))))))

(define python-boot0
  (package
    (inherit python-minimal)
    ;; We cannot use Python 3.7 and later here, because they require
    ;; pthreads, which is missing on non-x86 platforms at this stage.
    ;; Python 3.6 technically supports being built without threading
    ;; support, but requires additional patches.
    (version "3.5.9")
    (source (bootstrap-origin
             (origin
               (method url-fetch)
               (uri (string-append "https://www.python.org/ftp/python/"
                                   version "/Python-" version ".tar.xz"))
               (sha256
                (base32
                 "0jdh9pvx6m6lfz2liwvvhn7vks7qrysqgwn517fkpxb77b33fjn2"))
               (modules '((guix build utils)))
               (snippet
                '(begin
                   ;; Delete the bundled copy of libexpat.
                   (delete-file-recursively "Modules/expat")
                   (substitute* "Modules/Setup.dist"
                     ;; Link Expat instead of embedding the bundled one.
                     (("^#pyexpat.*") "pyexpat pyexpat.c -lexpat\n"))
                   #t)))))
    (inputs
     `(,@(%boot0-inputs)
       ("expat" ,expat-sans-tests)))              ;remove OpenSSL, zlib, etc.
    (native-inputs '())                           ;and pkg-config
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile

       ,@(substitute-keyword-arguments (package-arguments python-minimal)
           ;; Disable features that cannot be built at this stage.
           ((#:configure-flags _ ''())
            `(list "--without-ensurepip"
                   "--without-threads"))
           ;; Clear #:make-flags, such that changes to the regular
           ;; Python package won't interfere with this one.
           ((#:make-flags _ ''()) ''())
           ((#:phases phases)
            ;; Remove the 'apply-alignment-patch' phase if present to avoid
            ;; rebuilding this package.  TODO: for the next rebuild cycle,
            ;; consider inlining all the arguments instead of inheriting to
            ;; make it easier to patch Python without risking a full rebuild.
            ;; Or better yet, change to 'python-on-guile'.
            `(modify-phases ,@(list (match phases
                                      (('modify-phases original-phases
                                         changes ...
                                         ('add-after unpack apply-alignment-patch _))
                                       `(modify-phases ,original-phases ,@changes))
                                      (_ phases)))
               (add-before 'configure 'disable-modules
                 (lambda _
                   (substitute* "setup.py"
                     ;; Disable ctypes, since it requires libffi.
                     (("extensions\\.append\\(ctypes\\)") "")
                     ;; Prevent the 'ossaudiodev' extension from being
                     ;; built, since it requires Linux headers.
                     (("'linux', ") ""))
                   #t))
               (delete 'set-TZDIR)
               ,@(if (hurd-system?)
                     `((add-before 'build 'fix-regen
                         (lambda* (#:key inputs #:allow-other-keys)
                           (let ((libc (assoc-ref inputs "libc")))
                             (substitute* "Lib/plat-generic/regen"
                               (("/usr/include/") (string-append libc "/include/")))
                             #t))))
                     '())))
           ((#:tests? _ #f) #f))))))

(define/system-dependent ld-wrapper-boot0
  ;; The first 'ld' wrapper, defined with 'define/system-dependent' because
  ;; its calls '%boot0-inputs', whose result depends on (%current-system)
  ;;
  ;; We need this so binaries on Hurd will have libmachuser and libhurduser
  ;; in their RUNPATH, otherwise validate-runpath will fail.
  (make-ld-wrapper "ld-wrapper-boot0"
                   #:target boot-triplet
                   #:binutils binutils-boot0
                   #:guile %bootstrap-guile
                   #:bash (car (assoc-ref (%boot0-inputs) "bash"))
                   #:guile-for-build %bootstrap-guile))

(define (%boot1-inputs)
  ;; 2nd stage inputs.
  `(("gcc" ,gcc-boot0)
    ("ld-wrapper-cross" ,ld-wrapper-boot0)
    ("binutils-cross" ,binutils-boot0)
    ,@(alist-delete "binutils" (%boot0-inputs))))

(define glibc-final-with-bootstrap-bash
  ;; The final libc, "cross-built".  If everything went well, the resulting
  ;; store path has no dependencies.  Actually, the really-final libc is
  ;; built just below; the only difference is that this one uses the
  ;; bootstrap Bash.
  (package
    (inherit glibc)
    (name "glibc-intermediate")
    (source (bootstrap-origin (package-source glibc)))
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f

       ,@(substitute-keyword-arguments (package-arguments glibc)
           ((#:configure-flags flags)
            `(append (list ,(string-append "--host=" (boot-triplet))
                           ,(string-append "--build="
                                           (nix-system->gnu-triplet))
                           ,(if (hurd-system?) "--disable-werror"
                                ""))
                     ,flags))
           ((#:phases phases)
            `(modify-phases ,phases
               (add-before 'configure 'pre-configure
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Don't clobber CPATH with the bootstrap libc.
                   (setenv "NATIVE_CPATH" (getenv "CPATH"))
                   (unsetenv "CPATH")

                   ;; Tell 'libpthread' where to find 'libihash' on Hurd systems.
                   ,@(if (hurd-system?)
                       '((substitute* '("sysdeps/mach/Makefile"
                                        "sysdeps/mach/hurd/Makefile")
                           (("LDLIBS-pthread.so =.*")
                            (string-append "LDLIBS-pthread.so = "
                                           (assoc-ref %build-inputs "kernel-headers")
                                           "/lib/libihash.a\n"))))
                       '())

                   ;; 'rpcgen' needs native libc headers to be built.
                   (substitute* "sunrpc/Makefile"
                     (("sunrpc-CPPFLAGS =.*" all)
                      (string-append "CPATH = $(NATIVE_CPATH)\n"
                                     "export CPATH\n"
                                     all "\n")))
                   #t)))))))
    (propagated-inputs `(("kernel-headers" ,(kernel-headers-boot0))))
    (native-inputs
     `(("bison" ,bison-boot0)
       ("texinfo" ,texinfo-boot0)
       ("perl" ,perl-boot0)
       ("python" ,python-boot0)
       ,@(if (target-powerpc?)
           `(("powerpc64le-patch" ,@(search-patches
                                      "glibc-ldd-powerpc.patch")))
           '())))
    (inputs
     `( ;; The boot inputs.  That includes the bootstrap libc.  We don't want
       ;; it in $CPATH, hence the 'pre-configure' phase above.
       ,@(%boot1-inputs)

       ;; A native MiG is needed to build Glibc on Hurd.
       ,@(if (hurd-system?)
             `(("mig" ,mig-boot0))
             '())

       ;; A native GCC is needed to build `cross-rpcgen'.
       ("native-gcc" ,@(assoc-ref (%boot0-inputs) "gcc"))

       ;; Here, we use the bootstrap Bash, which is not satisfactory
       ;; because we don't want to depend on bootstrap tools.
       ("static-bash" ,@(assoc-ref (%boot0-inputs) "bash"))))))

(define (cross-gcc-wrapper gcc binutils glibc bash)
  "Return a wrapper for the pseudo-cross toolchain GCC/BINUTILS/GLIBC
that makes it available under the native tool names."
  (package (inherit gcc)
    (name (string-append (package-name gcc) "-wrapped"))
    (source #f)
    (build-system trivial-build-system)
    (outputs '("out"))
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))

                   (let* ((binutils (assoc-ref %build-inputs "binutils"))
                          (gcc      (assoc-ref %build-inputs "gcc"))
                          (libc     (assoc-ref %build-inputs "libc"))
                          (bash     (assoc-ref %build-inputs "bash"))
                          (out      (assoc-ref %outputs "out"))
                          (bindir   (string-append out "/bin"))
                          (triplet  ,(boot-triplet)))
                     (define (wrap-program program)
                       ;; GCC-BOOT0 is a libc-less cross-compiler, so it
                       ;; needs to be told where to find the crt files and
                       ;; the dynamic linker.
                       (call-with-output-file program
                         (lambda (p)
                           (format p "#!~a/bin/bash
exec ~a/bin/~a-~a -B~a/lib -Wl,-dynamic-linker -Wl,~a/~a \"$@\"~%"
                                   bash
                                   gcc triplet program
                                   libc libc
                                   ,(glibc-dynamic-linker))))

                       (chmod program #o555))

                     (mkdir-p bindir)
                     (with-directory-excursion bindir
                       (for-each (lambda (tool)
                                   (symlink (string-append binutils "/bin/"
                                                           triplet "-" tool)
                                            tool))
                                 '("ar" "ranlib"))
                       (for-each wrap-program '("gcc" "g++")))

                     #t))))
    (native-inputs
     `(("binutils" ,binutils)
       ("gcc" ,gcc)
       ("libc" ,glibc)
       ("bash" ,bash)))
    (inputs '())))

(define/system-dependent gcc-boot0-intermediate-wrapped
  ;; Make the cross-tools GCC-BOOT0 and BINUTILS-BOOT0 available under the
  ;; non-cross names.
  (cross-gcc-wrapper gcc-boot0 binutils-boot0
                     glibc-final-with-bootstrap-bash
                     (car (assoc-ref (%boot1-inputs) "bash"))))

(define static-bash-for-glibc
  ;; A statically-linked Bash to be used by GLIBC-FINAL in system(3) & co.
  (package
    (inherit static-bash)
    (source (bootstrap-origin (package-source static-bash)))
    (inputs `(("gcc" ,gcc-boot0-intermediate-wrapped)
              ("libc" ,glibc-final-with-bootstrap-bash)
              ("libc:static" ,glibc-final-with-bootstrap-bash "static")
              ,@(fold alist-delete (%boot1-inputs)
                      '("gcc" "libc"))))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile

       ,@(substitute-keyword-arguments (package-arguments static-bash)
           ((#:configure-flags flags '())
            ;; Add a '-L' flag so that the pseudo-cross-ld of
            ;; BINUTILS-BOOT0 can find libc.a.
            `(append ,flags
                     (list (string-append "LDFLAGS=-static -L"
                                          (assoc-ref %build-inputs
                                                     "libc:static")
                                          "/lib")))))))))

(define gettext-boot0
  ;; A minimal gettext used during bootstrap.
  (package
    (inherit gettext-minimal)
    (name "gettext-boot0")
    ;; Newer versions of GNU gettext depends on libxml2 and ncurses.  To
    ;; simplify the dependency chain, we stick to this version here.
    (version "0.19.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gettext/gettext-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hsw28f9q9xaggjlsdp2qmbp2rbd1mp0njzan2ld9kiqwkq2m57z"))))
    (inputs (%boot1-inputs))                      ;zero dependencies
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  ;; Build only the tools.
                  (add-after 'unpack 'chdir
                    (lambda _
                      (chdir "gettext-tools")
                      #t))

                  ;; Some test programs require pthreads, which we don't have.
                  (add-before 'configure 'no-test-programs
                    (lambda _
                      (substitute* "tests/Makefile.in"
                        (("^PROGRAMS =.*$")
                         "PROGRAMS =\n"))
                      #t)))))))

(define glibc-final
  ;; The final glibc, which embeds the statically-linked Bash built above.
  ;; Use 'package/inherit' so we get the 'replacement' of 'glibc', if any.
  (package/inherit
   glibc
   (name "glibc")
   (source (bootstrap-origin (package-source glibc)))
   (inputs `(("static-bash" ,static-bash-for-glibc)
             ,@(alist-delete
                "static-bash"
                (package-inputs glibc-final-with-bootstrap-bash))))

   ;; This time we need 'msgfmt' to install all the libc.mo files.
   (native-inputs `(,@(package-native-inputs glibc-final-with-bootstrap-bash)
                     ,@(if (target-powerpc?)
                         `(("powerpc64le-patch" ,@(search-patches
                                                    "glibc-ldd-powerpc.patch")))
                         '())
                    ("gettext" ,gettext-boot0)))

   (propagated-inputs
    (package-propagated-inputs glibc-final-with-bootstrap-bash))

   ;; The final libc only refers to itself, but the 'debug' output contains
   ;; references to GCC-BOOT0 and to the Linux headers.  XXX: Would be great
   ;; if 'allowed-references' were per-output.
   (arguments
    `(#:allowed-references
      ((,gcc-boot0 "lib")
       ,(kernel-headers-boot0)
       ,static-bash-for-glibc
       ,@(if (hurd-system?)
             `(,gnumach-headers-boot0
               ,hurd-headers-boot0)
             '())
       ,@(package-outputs glibc-final-with-bootstrap-bash))
      ,@(package-arguments glibc-final-with-bootstrap-bash)))))

(define/system-dependent gcc-boot0-wrapped
  ;; Make the cross-tools GCC-BOOT0 and BINUTILS-BOOT0 available under the
  ;; non-cross names.
  (cross-gcc-wrapper gcc-boot0 binutils-boot0 glibc-final
                     (car (assoc-ref (%boot1-inputs) "bash"))))

(define (%boot2-inputs)
  ;; 3rd stage inputs.
  `(("libc" ,glibc-final)
    ("libc:static" ,glibc-final "static")
    ("gcc" ,gcc-boot0-wrapped)
    ,@(fold alist-delete (%boot1-inputs) '("libc" "gcc" "linux-libre-headers"))))

(define binutils-final
  (package
    (inherit binutils)
    (source (bootstrap-origin (package-source binutils)))
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       #:allowed-references
       ,@(match (%current-system)
         ((? target-powerpc?)
          `(("out" ,glibc-final ,static-bash-for-glibc)))
         (_
          `(("out" ,glibc-final))))
       ,@(package-arguments binutils)))
    (inputs
     (match (%current-system)
       ((? target-powerpc?)
        `(("bash" ,static-bash-for-glibc)
          ,@(%boot2-inputs)))
       (_ (%boot2-inputs))))))

(define libstdc++
  ;; Intermediate libstdc++ that will allow us to build the final GCC
  ;; (remember that GCC-BOOT0 cannot build libstdc++.)
  (let ((lib (make-libstdc++ gcc)))
    (package
      (inherit lib)
      (source (bootstrap-origin (package-source lib)))
      (arguments
       `(#:guile ,%bootstrap-guile
         #:implicit-inputs? #f
         #:allowed-references ("out")

         ;; XXX: libstdc++.so NEEDs ld.so for some reason.
         #:validate-runpath? #f

         ;; All of the package arguments from 'make-libstdc++
         ;; except for the configure-flags.
         ,@(package-arguments lib)
         #:configure-flags `("--disable-shared"
                             "--disable-libstdcxx-dual-abi"
                             "--disable-libstdcxx-threads"
                             "--disable-libstdcxx-pch"
                             ,(string-append "--with-gxx-include-dir="
                                             (assoc-ref %outputs "out")
                                             "/include"))))
      (outputs '("out"))
      (inputs (%boot2-inputs))
      (synopsis "GNU C++ standard library (intermediate)"))))

(define zlib-final
  ;; Zlib used by GCC-FINAL.
  (package
    (inherit zlib)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       #:allowed-references ("out" ,glibc-final)
       ,@(package-arguments zlib)))
    (inputs (%boot2-inputs))))

(define/system-dependent ld-wrapper-boot3
  ;; A linker wrapper that uses the bootstrap Guile.
  (make-ld-wrapper "ld-wrapper-boot3"
                   #:binutils binutils-final
                   #:guile %bootstrap-guile
                   #:bash (car (assoc-ref (%boot2-inputs) "bash"))
                   #:guile-for-build %bootstrap-guile))

(define gcc-final
  ;; The final GCC.
  (package (inherit gcc-boot0)
    (name "gcc")

    ;; XXX: Currently #:allowed-references applies to all the outputs but the
    ;; "debug" output contains disallowed references, notably
    ;; linux-libre-headers.  Disable the debugging output to work around that.
    (outputs (delete "debug" (package-outputs gcc-boot0)))

    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f

       #:allowed-references ("out" "lib" ,zlib-final
                             ,glibc-final ,static-bash-for-glibc)

       ;; Things like libasan.so and libstdc++.so NEED ld.so for some
       ;; reason, but it is not in their RUNPATH.  This is a false
       ;; positive, so turn it off.
       #:validate-runpath? #f

       ,@(substitute-keyword-arguments (package-arguments gcc)
           ((#:make-flags flags)
            ;; Since $LIBRARY_PATH is not honored, add the relevant flags.
            `(let ((zlib (assoc-ref %build-inputs "zlib")))
               (map (lambda (flag)
                      (if (string-prefix? "LDFLAGS=" flag)
                          (string-append flag " -L"
                                         (assoc-ref %build-inputs "libstdc++")
                                         "/lib -L" zlib "/lib -Wl,-rpath="
                                         zlib "/lib")
                          flag))
                    ,flags)))
           ;; Build again GMP & co. within GCC's build process, because it's hard
           ;; to do outside (because GCC-BOOT0 is a cross-compiler, and thus
           ;; doesn't honor $LIBRARY_PATH, which breaks `gnu-build-system'.)
           ((#:phases phases)
            `(modify-phases ,phases
                (add-after 'unpack 'unpack-gmp&co
                  (lambda* (#:key inputs #:allow-other-keys)
                    (let ((gmp  (assoc-ref %build-inputs "gmp-source"))
                          (mpfr (assoc-ref %build-inputs "mpfr-source"))
                          (mpc  (assoc-ref %build-inputs "mpc-source")))

                      ;; To reduce the set of pre-built bootstrap inputs, build
                      ;; GMP & co. from GCC.
                      (for-each (lambda (source)
                                  (invoke "tar" "xvf" source))
                                (list gmp mpfr mpc))

                      ;; Create symlinks like `gmp' -> `gmp-x.y.z'.
                      ,@(map (lambda (lib)
                               ;; Drop trailing letters, as gmp-6.0.0a unpacks
                               ;; into gmp-6.0.0.
                               `(symlink ,(string-trim-right
                                           (package-full-name lib "-")
                                           char-set:letter)
                                         ,(package-name lib)))
                             (list gmp-6.0 mpfr mpc))
                      #t))))))))

    ;; This time we want Texinfo, so we get the manual.  Add
    ;; STATIC-BASH-FOR-GLIBC so that it's used in the final shebangs of
    ;; scripts such as 'mkheaders' and 'fixinc.sh' (XXX: who cares about these
    ;; scripts?).
    (native-inputs `(("texinfo" ,texinfo-boot0)
                     ("perl" ,perl-boot0) ;for manpages
                     ("static-bash" ,static-bash-for-glibc)
                     ,@(package-native-inputs gcc-boot0)))

    (inputs `(("gmp-source" ,(bootstrap-origin (package-source gmp-6.0)))
              ("mpfr-source" ,(package-source mpfr))
              ("mpc-source" ,(package-source mpc))
              ("ld-wrapper" ,ld-wrapper-boot3)
              ("binutils" ,binutils-final)
              ("libstdc++" ,libstdc++)
              ("zlib" ,zlib-final)
              ,@(%boot2-inputs)))))

(define (%boot3-inputs)
  ;; 4th stage inputs.
  `(("gcc" ,gcc-final)
    ("ld-wrapper" ,ld-wrapper-boot3)
    ,@(alist-delete "gcc" (%boot2-inputs))))

(define bash-final
  ;; Link with `-static-libgcc' to make sure we don't retain a reference
  ;; to the bootstrap GCC.  Use "bash-minimal" to avoid an extra dependency
  ;; on Readline and ncurses.
  (let ((bash (static-libgcc-package bash-minimal)))
    (package
      (inherit bash)
      (source (bootstrap-origin (package-source bash)))
      (inputs (%boot3-inputs))
      (arguments
       `(#:implicit-inputs? #f
         #:guile ,%bootstrap-guile

         #:disallowed-references ,(assoc-ref (%boot3-inputs) "coreutils&co")

         ,@(package-arguments bash))))))

(define (%boot4-inputs)
  ;; Now use the final Bash.
  `(("bash" ,bash-final)
    ,@(alist-delete "bash" (%boot3-inputs))))

(define with-boot4
  (package-with-explicit-inputs %boot4-inputs %bootstrap-guile))

(define-public guile-final
  ;; This package must be public because other modules refer to it.  However,
  ;; mark it as hidden so that 'fold-packages' ignores it.
  (with-boot4 (hidden-package
               (package-with-bootstrap-guile guile-3.0/fixed))))

(define glibc-utf8-locales-final
  ;; Now that we have GUILE-FINAL, build the UTF-8 locales.  They are needed
  ;; by the build processes afterwards so their 'scm_to_locale_string' works
  ;; with the full range of Unicode codepoints (remember
  ;; 'scm_to_locale_string' is called every time a string is passed to a C
  ;; function.)
  (package
    (inherit glibc-utf8-locales)
    (native-inputs
     `(("glibc" ,glibc-final)
       ("gzip" ,(with-boot4 gzip))))))

(define-public ld-wrapper
  ;; The final 'ld' wrapper, which uses the final Guile and Binutils.
  (make-ld-wrapper "ld-wrapper"
                   #:binutils binutils-final
                   #:guile guile-final
                   #:bash bash-final))

(define (%boot5-inputs)
  ;; Now with UTF-8 locales.  Remember that the bootstrap binaries were built
  ;; with an older libc, which cannot load the new locale format.  See
  ;; <https://lists.gnu.org/archive/html/guix-devel/2015-08/msg00737.html>.
  `(("locales" ,glibc-utf8-locales-final)
    ,@(%boot4-inputs)))

(define with-boot5
  (package-with-explicit-inputs %boot5-inputs))

(define gnu-make-final
  ;; The final GNU Make, which uses the final Guile.
  (let ((pkg-config (package
                      (inherit %pkg-config)       ;the native pkg-config
                      (inputs `(("guile" ,guile-final)
                                ,@(%boot5-inputs)))
                      (arguments
                       `(#:implicit-inputs? #f
                         ,@(package-arguments pkg-config))))))
    (package
      (inherit (package-with-bootstrap-guile gnu-make))
      (inputs `(("guile" ,guile-final)
                ,@(%boot5-inputs)))
      (native-inputs `(("pkg-config" ,pkg-config)))
      (arguments
       `(#:implicit-inputs? #f
         ,@(package-arguments gnu-make))))))


(define coreutils-final
  ;; The final Coreutils.  Treat them specially because some packages, such as
  ;; Findutils, keep a reference to the Coreutils they were built with.
  (with-boot5 (package-with-bootstrap-guile
               (package
                 (inherit coreutils-minimal)
                 (name "coreutils")))
              ;; Use the final Guile, linked against the
              ;; final libc with working iconv, so that
              ;; 'substitute*' works well when touching
              ;; test files in Gettext.
              ))

(define grep-final
  ;; The final grep.  Gzip holds a reference to it (via zgrep), so it must be
  ;; built before gzip.
  (let ((grep (with-boot5 (package-with-bootstrap-guile grep))))
    (package/inherit grep
                     (inputs (alist-delete "pcre" (package-inputs grep)))
                     (native-inputs `(("perl" ,perl-boot0))))))

(define (%boot6-inputs)
  ;; Now use the final Coreutils.
  `(("coreutils" ,coreutils-final)
    ("grep" ,grep-final)
    ,@(%boot5-inputs)))

(define with-boot6
  (package-with-explicit-inputs %boot6-inputs))

(define sed-final
  ;; The final sed.
  (let ((sed (with-boot6 (package-with-bootstrap-guile sed))))
    (package/inherit sed (native-inputs `(("perl" ,perl-boot0))))))

(define-public %final-inputs
  ;; Final derivations used as implicit inputs by 'gnu-build-system'.  We
  ;; still use 'package-with-bootstrap-guile' so that the bootstrap tools are
  ;; used for origins that have patches, thereby avoiding circular
  ;; dependencies.
  (let ((finalize (compose with-boot6
                           package-with-bootstrap-guile)))
    `(,@(map (match-lambda
               ((name package)
                (list name (finalize package))))
             `(("tar" ,tar)
               ("gzip" ,gzip)
               ("bzip2" ,bzip2)
               ("xz" ,xz)
               ("file" ,file)
               ("diffutils" ,diffutils)
               ("patch" ,patch)
               ("findutils" ,findutils)
               ("gawk" ,gawk)))
      ("sed" ,sed-final)
      ("grep" ,grep-final)
      ("coreutils" ,coreutils-final)
      ("make" ,gnu-make-final)
      ("bash" ,bash-final)
      ("ld-wrapper" ,ld-wrapper)
      ("binutils" ,binutils-final)
      ("gcc" ,gcc-final)
      ("libc" ,glibc-final)
      ("libc:static" ,glibc-final "static")
      ("locales" ,glibc-utf8-locales-final))))

(define-public canonical-package
  (let ((name->package (fold (lambda (input result)
                               (match input
                                 ((_ package . outputs)
                                  (vhash-cons (package-full-name package)
                                              package result))))
                             vlist-null
                             `(("guile" ,guile-final)
                               ,@%final-inputs))))
    (lambda (package)
      "Return the 'canonical' variant of PACKAGE---i.e., if PACKAGE is one of
the implicit inputs of 'gnu-build-system', return that one, otherwise return
PACKAGE.

The goal is to avoid duplication in cases like GUILE-FINAL vs. GUILE-2.2,
COREUTILS-FINAL vs. COREUTILS, etc."
      ;; XXX: This doesn't handle dependencies of the final inputs, such as
      ;; libunistring, GMP, etc.
      (match (vhash-assoc (package-full-name package) name->package)
        ((_ . canon)
         ;; In general we want CANON, except if we're cross-compiling: CANON
         ;; uses explicit inputs, so it is "anchored" in the bootstrapped
         ;; process, with dependencies on things that cannot be
         ;; cross-compiled.
         (if (%current-target-system)
             package
             canon))
        (_ package)))))


;;;
;;; GCC toolchain.
;;;

;; Using the following procedure, a gcc toolchain targeting glibc-2.27 can be
;; instantiated like this:
;;
;; (define-public gcc-glibc-2.27-toolchain
;;   (make-gcc-toolchain gcc glibc-2.27))

(define* (make-gcc-toolchain gcc
                            #:optional
                            (libc #f))
  "Return a complete toolchain for GCC. If LIBC is specified, target that libc."
  (let ((gcc (if libc (make-gcc-libc gcc libc) gcc))
        (libc (if libc libc glibc-final)))
    (package
      (name (string-append (package-name gcc) "-toolchain"))
      (version (package-version gcc))
      (source #f)
      (build-system trivial-build-system)
      (arguments
       '(#:modules ((guix build union))
         #:builder (begin
                     (use-modules (ice-9 match)
                                  (srfi srfi-1)
                                  (srfi srfi-26)
                                  (guix build union))

                     (let ((out (assoc-ref %outputs "out")))
                       (union-build out
                                    (filter-map (match-lambda
                                                  (("libc-debug" . _) #f)
                                                  (("libc-static" . _) #f)
                                                  ((_ . directory) directory))
                                                %build-inputs))

                       (union-build (assoc-ref %outputs "debug")
                                    (list (assoc-ref %build-inputs
                                                     "libc-debug")))
                       (union-build (assoc-ref %outputs "static")
                                    (list (assoc-ref %build-inputs
                                                     "libc-static")))
                       #t))))

      (native-search-paths
       (append (package-native-search-paths gcc)
               (package-native-search-paths libc))) ;GUIX_LOCPATH
      (search-paths
       (append (package-search-paths gcc)
               (package-search-paths libc)))

      (license (package-license gcc))
      (synopsis "Complete GCC tool chain for C/C++ development")
      (description
       "This package provides a complete GCC tool chain for C/C++ development to
be installed in user profiles.  This includes GCC, as well as libc (headers and
binaries, plus debugging symbols in the @code{debug} output), and Binutils.  GCC
is the GNU Compiler Collection.")
      (home-page "https://gcc.gnu.org/")
      (outputs '("out" "debug" "static"))

      ;; The main raison d'être of this "meta-package" is (1) to conveniently
      ;; install everything that we need, and (2) to make sure ld-wrapper comes
      ;; before Binutils' ld in the user's profile.
      (inputs `(("gcc" ,gcc)
                ("ld-wrapper" ,(car (assoc-ref %final-inputs "ld-wrapper")))
                ("binutils" ,binutils-final)
                ("libc" ,libc)
                ("libc-debug" ,libc "debug")
                ("libc-static" ,libc "static"))))))

(define-public gcc-toolchain
  (make-gcc-toolchain gcc-final))

(define-public gcc-toolchain-4.8
  (make-gcc-toolchain gcc-4.8))

(define-public gcc-toolchain-4.9
  (make-gcc-toolchain gcc-4.9))

(define-public gcc-toolchain-5
  (make-gcc-toolchain gcc-5))

(define-public gcc-toolchain-6
  (make-gcc-toolchain gcc-6))

(define-public gcc-toolchain-7
  gcc-toolchain)

(define-public gcc-toolchain-8
  (make-gcc-toolchain gcc-8))

(define-public gcc-toolchain-9
  (make-gcc-toolchain gcc-9))

(define-public gcc-toolchain-10
  (make-gcc-toolchain gcc-10))

(define-public gcc-toolchain-aka-gcc
  ;; It's natural for users to try "guix install gcc".  This package
  ;; automatically "redirects" them to 'gcc-toolchain'.
  (deprecated-package "gcc" gcc-toolchain-10))


(define-public gdc-toolchain-10
  (package (inherit (make-gcc-toolchain gdc-10))
    (synopsis "Complete GCC tool chain for D lang development")
    (description "This package provides a complete GCC tool chain for
D lang development to be installed in user profiles.  This includes
gdc, as well as libc (headers and binaries, plus debugging symbols
in the @code{debug} output), and binutils.")))

;; Provide the Fortran toolchain package only for the version of gfortran that
;; is used by Guix internally to build Fortran libraries, because combining
;; code compiled with different versions can cause problems.

(define-public gfortran-toolchain
  (package (inherit (make-gcc-toolchain gfortran))
    (synopsis "Complete GCC tool chain for Fortran development")
    (description "This package provides a complete GCC tool chain for
Fortran development to be installed in user profiles.  This includes
gfortran, as well as libc (headers and binaries, plus debugging symbols
in the @code{debug} output), and binutils.")))


;;; commencement.scm ends here
