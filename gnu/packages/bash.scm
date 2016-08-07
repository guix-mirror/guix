;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
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

(define-module (gnu packages bash)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:autoload   (guix gnupg) (gnupg-verify*)
  #:autoload   (guix hash) (port-sha256)
  #:autoload   (guix base32) (bytevector->nix-base32-string)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format))

(define (patch-url seqno)
  "Return the URL of Bash patch number SEQNO."
  (format #f "mirror://gnu/bash/bash-4.3-patches/bash43-~3,'0d" seqno))

(define (bash-patch seqno sha256)
  "Return the origin of Bash patch SEQNO, with expected hash SHA256"
  (origin
    (method url-fetch)
    (uri (patch-url seqno))
    (sha256 sha256)))

(define-syntax-rule (patch-series (seqno hash) ...)
  (list (bash-patch seqno (base32 hash))
        ...))

(define %patch-series-4.3
  ;; This is the current patches series for 4.3, generated using
  ;; 'download-patches' below.
  (patch-series
   (1 "0hip2n2s5hws8p4nfcz37379zn6cak83ljsm64z52rw6ckrdzczc")
   (2 "0ashj5d1g3zbyr7zf0r72s5wnk96cz1xj919y3jajadbc9qcvrzf")
   (3 "0z88q4daq7dmw93iqd9c5i5d1sndklih3nrh0v75746da2n6w3h0")
   (4 "0f0kh9j5k4ym6knshscx31przm50x5cc7ifkwqk0swh6clna982y")
   (5 "1ym3b8b7lgmdp3dklp8qaqhyq965wd5392namq8mz7rb0d231j0s")
   (6 "04q20igq49py49ynb0f83f6f52cdkyqwd9bpic6akr0m5pkqwr50")
   (7 "18zkz23d9myshrwfcwcdjk7qmkqp8az5n91ni9jaixlwqlhy64qi")
   (8 "0pprcwvh7ngdli0x95pc1cpssg4qg7layi9xrv2jq6c7965ajhcr")
   (9 "19a0pf0alp30d1bjj0zf3zq2f5n0s6y91w7brm9jyswl51kns8n0")
   (10 "1dzhr5ammyijisz48cqi5vaw26hfr5vh9smnqxq4qc9p06f7j1ff")
   (11 "0fvzdzzi142a8rf3v965r6gbpn0k7fv2gif1yq8a4160vcn40qvw")
   (12 "04lcgfcyz7p3zagb4hkia3hkpd7lii9m8ycy9qqwzyrm1c1pj4ry")
   (13 "0y9cqi378z6flapkd5k5lfl4lq3ivzg4njj3i3wmw7xb6r9wma5z")
   (14 "04xcb0k9fxxq4vashgzb98567xzdnm4655nlm4jvfvjv6si6ykas")
   (15 "13ay6lldy1p00xj41nfjpq8lai3vw2qwca79gx6s80z04j53wa8k")
   (16 "0wq7bvx3pfw90pnfb86yg5nr9jgjsvm2nq5rrkqxf6zn977hpmlj")
   (17 "103p7sibihv6cshqj12k546zsbz0dnd5cv5vlx1719avddfc4rqj")
   (18 "0n1x3812y1brb9xbabaj3fvr4cpvm2225iwckmqk2fcpkq5b9a3s")
   (19 "08rd1p7zpzgbpmmmnj2im8wj2pcwmbbx51psr9vdc5c049si9ad7")
   (20 "163c6g05qpag2plx5q795pmw3f3m904jy7z93xj2i08pgzc8cpna")
   (21 "1a90cl3h10dh8k9f2ddrsjmw5ywaw2d5x78xb4fd2sryi039yhs1")
   (22 "120s0s4qcqd0q12j1iv0hkpf9fp3w5jnqw646kv66n66jnxlfkgx")
   (23 "1m00sfi88p2akgiyrg4hw0gvz3s1586pkzjdr3dm73vs773m1hls")
   (24 "0v0gjqzjsqjfgj5x17fq7g649k94jn8zq92qsxkhc2d6l215hl1v")
   (25 "0lcj96i659q35f1jcmwwbnw3p7w7vvlxjxqi989vn6d6qksqcl8y") ;CVE-2014-6271
   (26 "0k919ir0inwn4wai2vdzpbwqq5h54fnrlkmgccxjg91v3ch15k1f") ;CVE-2014-7169
   (27 "1gnsfvq6bhb3srlbh0cannj2hackdsipcg7z0ds7zlk1hp96mdqy")
   (28 "17a65c4fn4c5rgsiw9gqqnzhznh3gwnd2xzzv2dppyi48znxpc78") ;CVE-2014-7186
   (29 "14k27p28r5l2fz3r03kd0x72vvsq8bja8c6hjz5kxikbzsbs7i2c") ;CVE-2014-6277
   (30 "0nrqb0m7s89qsrbfaffpilc5gcf82bx9yvgzld4hr79p5y54yhw5") ;CVE-2014-6278
   (31 "07d62bl3z7qa8v6kgk47vzzazw563mlk9zhrsr4xsbqgvmcrylnd")
   (32 "0jjgapfq4qhmndfrw8c3q3lva8xjdhlbd9cc631v41b0kb95g4w8")
   (33 "05ma5rlxiadnfh925p4y7s0vvk917kmsdb1mfdx05gizl63pfapv")
   (34 "12gq9whkq3naa3iy7c7x5pfpvrg7d0kwqld8609zxphhy424ysgi")
   (35 "1qy1jflmbazjykq766gwabkaiswnx7pwa66whqiny0w02zjqa39p")
   (36 "0z6jbyy70lfdm6d3x0sbazbqdxb3xnpn9bmz7madpvrnbd284pxc")
   (37 "04sqr8zkl6s5fccfvb775ppn3ldij5imria9swc39aq0fkfp1w9k")
   (38 "0rv3g14mpgv8br267bf7rmgqlgwnc4v6g3g8y0sjba571i8amgmd")
   (39 "1v3l3vkc3g2b6fjycqwlakr8xhiw6bmw6q0zd6bi0m0m4bnxr55b")
   (40 "0sypv66vsldmc95gwvf7ylz1k7y37vnvdsjg8ajjr6b2j9mkkfw4")
   (41 "06ic2gdpbi1afik3wqf9d4vh95if4bz8bmhcgr555621dsb35i2f")
   (42 "06a90k0p6bqc4wk2dsmapna69124an76xvlnlj3xm497vci968dc")))

(define (download-patches store count)
  "Download COUNT Bash patches into store.  Return a list of
number/base32-hash tuples, directly usable in the 'patch-series' form."
  (unfold (cut > <> count)
          (lambda (number)
            (let* ((patch  (download-to-store store (patch-url number)))
                   (sig    (download-to-store store
                                              (string-append (patch-url number)
                                                             ".sig"))))
              (unless (gnupg-verify* sig patch)
                (error "failed to verify signature" patch))

              (list number
                    (bytevector->nix-base32-string
                     (call-with-input-file patch port-sha256)))))
          1+
          1))

(define-public bash
  (let* ((cppflags (string-join '("-DSYS_BASHRC='\"/etc/bashrc\"'"
                                  "-DSYS_BASH_LOGOUT='\"/etc/bash_logout\"'"
                                  "-DDEFAULT_PATH_VALUE='\"/no-such-path\"'"
                                  "-DSTANDARD_UTILS_PATH='\"/no-such-path\"'"
                                  "-DNON_INTERACTIVE_LOGIN_SHELLS"
                                  "-DSSH_SOURCE_BASHRC")
                                " "))
         (configure-flags
          ``("--with-installed-readline"
             ,,(string-append "CPPFLAGS=" cppflags)
             ,(string-append
               "LDFLAGS=-Wl,-rpath -Wl,"
               (assoc-ref %build-inputs "readline")
               "/lib"
               " -Wl,-rpath -Wl,"
               (assoc-ref %build-inputs "ncurses")
               "/lib")))
         (post-install-phase
          '(lambda* (#:key outputs #:allow-other-keys)
             ;; Add a `bash' -> `sh' link.
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion (string-append out "/bin")
                 (symlink "bash" "sh")))))
         (install-headers-phase
          '(lambda* (#:key outputs #:allow-other-keys)
             ;; Install Bash headers so that packages that provide extensions
             ;; can use them.  We install them in include/bash; that's what
             ;; Debian does and what Bash extensions like recutils or
             ;; guile-bash expect.
             (let ((include (string-append (assoc-ref outputs "include")
                                            "/include/bash"))
                   (includes "^\\./include/[^/]+\\.h$")
                   (headers "^\\./(builtins/|lib/glob/|lib/tilde/|)[^/]+\\.h$"))
               (mkdir-p include)
               (for-each (lambda (file)
                           (when (string-match includes file)
                             (install-file file include))
                           (when (string-match headers file)
                             (install-file file
                                           (string-append include "/"
                                                          (dirname file)))))
                         (find-files "." "\\.h$"))
               (delete-file (string-append include "/" "y.tab.h"))
               #t)))
         (version "4.3"))
    (package
     (name "bash")
     (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/bash/bash-" version ".tar.gz"))
              (sha256
               (base32
                "1m14s1f61mf6bijfibcjm9y6pkyvz6gibyl8p4hxq90fisi8gimg"))
              (patch-flags '("-p0"))
              (patches %patch-series-4.3)

              ;; The patches above modify 'parse.y', so force a rebuild of the
              ;; parser.
              (snippet '(for-each delete-file
                                  '("y.tab.c" "y.tab.h" "parser-built")))))
     (version (string-append version "."
                             (number->string (length %patch-series-4.3))))
     (build-system gnu-build-system)

     (outputs '("out"
                "doc"                         ;1.7 MiB of HTML and extra files
                "include"))                   ;headers used by extensions
     (native-inputs `(("bison" ,bison)))      ;to rebuild the parser
     (inputs `(("readline" ,readline)
               ("ncurses" ,ncurses)))             ;TODO: add texinfo
     (arguments
      `(;; When cross-compiling, `configure' incorrectly guesses that job
        ;; control is missing.
        #:configure-flags ,(if (%current-target-system)
                               `(cons* "bash_cv_job_control_missing=no"
                                       ,configure-flags)
                               configure-flags)

        ;; Bash is reportedly not parallel-safe.  See, for instance,
        ;; <http://patches.openembedded.org/patch/32745/> and
        ;; <http://git.buildroot.net/buildroot/commit/?h=79e2d802a>.
        #:parallel-build? #f
        #:parallel-tests? #f

        ;; XXX: The tests have a lot of hard-coded paths, so disable them
        ;; for now.
        #:tests? #f

        #:modules ((ice-9 regex)
                   (guix build utils)
                   (guix build gnu-build-system))

        #:phases (modify-phases %standard-phases
                   (add-after 'install 'post-install ,post-install-phase)
                   (add-after 'install 'install-headers
                     ,install-headers-phase))))
     (synopsis "The GNU Bourne-Again SHell")
     (description
      "Bash is the shell, or command-line interpreter, of the GNU system.  It
is compatible with the Bourne Shell, but it also integrates useful features
from the Korn Shell and the C Shell and new improvements of its own.  It
allows command-line editing, unlimited command history, shell functions and
aliases, and job control while still allowing most sh scripts to be run
without modification.")
     (license gpl3+)
     (home-page "http://www.gnu.org/software/bash/"))))

(define-public bash-minimal
  ;; A stripped-down Bash for non-interactive use.
  (package (inherit bash)
    (name "bash-minimal")
    (inputs '())                                ; no readline, no curses
    (arguments
     (let ((args `(#:modules ((guix build gnu-build-system)
                              (guix build utils)
                              (srfi srfi-1)
                              (srfi srfi-26))
                   ,@(package-arguments bash))))
       (substitute-keyword-arguments args
         ((#:configure-flags flags)
          `(list "--without-bash-malloc"
                 "--disable-readline"
                 "--disable-history"
                 "--disable-help-builtin"
                 "--disable-progcomp"
                 "--disable-net-redirections"
                 "--disable-nls"

                 ,@(if (%current-target-system)
                       '("bash_cv_job_control_missing=no")
                       '()))))))))

(define-public static-bash
  ;; Statically-linked Bash that contains nothing but the 'bash' binary and
  ;; 'sh' symlink, without any reference.
  (let ((bash (static-package bash-minimal)))
    (package
      (inherit bash)
      (name "bash-static")
      (arguments
       (substitute-keyword-arguments
           `(#:allowed-references ("out") ,@(package-arguments bash))
         ((#:phases phases)
          `(alist-cons-after
            'strip 'remove-everything-but-the-binary
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (remove-store-references (string-append bin "/bash"))
                (delete-file (string-append bin "/bashbug"))
                (delete-file-recursively (string-append out "/share"))
                #t))
            ,phases)))))))

(define-public bash-completion
  (package
    (name "bash-completion")
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/scop/" name "/releases/download/"
                    version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1sg82nmsr00ig37skln2qvmi8mgbxgdvycm3ygzs8gbz66pq3q5j"))
              (patches
               (search-patches "bash-completion-directories.patch"))))
    (build-system gnu-build-system)
    (native-inputs `(("util-linux" ,util-linux)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after
                   'install 'remove-redundant-completions
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     ;; Util-linux comes with a bunch of completion files for
                     ;; its own commands which are more sophisticated and
                     ;; up-to-date than those of bash-completion.  Remove those
                     ;; from bash-completion.
                     (let* ((out         (assoc-ref outputs "out"))
                            (util-linux  (assoc-ref inputs "util-linux"))
                            (completions (string-append out
                                                        "/share/bash-completion"
                                                        "/completions"))
                            (already     (find-files
                                          (string-append
                                           util-linux
                                           "/etc/bash_completion.d"))))
                       (with-directory-excursion completions
                         (for-each (lambda (file)
                                     (when (file-exists? file)
                                       (delete-file file)))
                                   (map basename already)))
                       #t))))))
    (synopsis "Bash completions for common commands")
    (description
     "This package provides extensions that allow Bash to provide adapted
completion for many common commands.")
    (home-page "http://bash-completion.alioth.debian.org/")
    (license gpl2+)))

(define-public bash-tap
  (package
    (name "bash-tap")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/illusori/bash-tap/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qs1qi38bl3ns4mpagcawv618dsk2q1lgrbddgvs0wl3ia12cyz5"))))
    ;; There is no compilation process to use this package, however, the bash
    ;; scripts installed by this package start with "#!/bin/bash".  To fix
    ;; these lines, we use the patch-shebangs of the GNU build system.  The
    ;; project does not use a Makefile.
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There is no test suite.
       #:phases
       (modify-phases %standard-phases
         ;; Because there are no configure scripts or Makefile, we can
         ;; remove these phases.
         (delete 'configure)
         (delete 'build)
         ;; The installation involves manually copying the files to a location.
         ;; To make them easily accessible by setting PATH, we add the scripts
         ;; to the "bin" folder.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (install-file "bash-tap" bin)
               (install-file "bash-tap-bootstrap" bin)
               (install-file "bash-tap-mock" bin)))))))
    (home-page "http://www.illusori.co.uk/projects/bash-tap/")
    (synopsis "Bash port of a Test::More/Test::Builder-style TAP-compliant
test library")
    (description "Bash TAP is a TAP-compliant Test::More-style testing library
for Bash shell scripts and functions.  Along with the Test::More-style testing
helpers it provides helper functions for mocking commands and in-process output
capturing.")
    (license expat)))
