;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2018, 2019 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2019 Carl Dong <contact@carldong.me>
;;; Copyright © 2019 Léo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
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

(define-module (gnu packages bootstrap)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix store)
                #:select (%store-monad interned-file text-file store-lift))
  #:use-module ((guix derivations)
                #:select (raw-derivation derivation-input derivation->output-path))
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:select (elf-file?))
  #:use-module ((guix gexp) #:select (lower-object))
  #:use-module (guix monads)
  #:use-module (guix memoization)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (bootstrap-origin
            package-with-bootstrap-guile
            glibc-dynamic-linker

            bootstrap-executable
            bootstrap-guile-origin

            %bootstrap-guile
            %bootstrap-coreutils&co
            %bootstrap-linux-libre-headers
            %bootstrap-binutils
            %bootstrap-gcc
            %bootstrap-glibc
            %bootstrap-inputs
            %bootstrap-mescc-tools
            %bootstrap-mes

            %bootstrap-inputs-for-tests))

;;; Commentary:
;;;
;;; Pre-built packages that are used to bootstrap the
;;; distribution--i.e., to build all the core packages from scratch.
;;;
;;; Code:



;;;
;;; The bootstrap executables: 'bash', 'mkdir', 'tar', 'xz'.  They allow us to
;;; extract the very first tarball.
;;;

(define %bootstrap-executables
  ;; List of bootstrap executables and their recursive hashes (as per 'guix
  ;; hash -r'), taking their executable bit into account.
  `(("aarch64-linux"
     ("bash"
      ,(base32 "13aqhqb8nydlwq1ah9974q0iadx1pb95v13wzzyf7vgv6nasrwzr"))
     ("mkdir"
      ,(base32 "1pxhdp7ldwavmm71xbh9wc197cb2nr66acjn26yjx3732cixh9ws"))
     ("tar"
      ,(base32 "1j51gv08sfg277yxj73xd564wjq3f8xwd6s9rbcg8v9gms47m4cx"))
     ("xz"
      ,(base32 "1d779rwsrasphg5g3r37qppcqy3p7ay1jb1y83w7x4i3qsc7zjy2")))
    ("powerpc-linux"
     ("bash"
      ,(base32 "0hwlw5lcyjzadprf5fm0cv4zb6jw667g9amnmhq0lbixasy7j72j"))
     ("mkdir"
      ,(base32 "12lfwh5p8pp06250wgi9mdvjv1jdfpd5xpmvfc0616aj0xqh09hp"))
     ("tar"
      ,(base32 "00sbmwl8qh6alxv9mw4hvj1j4yipwmw5mrw6qad8bi2pr7ya5386"))
     ("xz"
      ,(base32 "0hi47y6zh5zz137i59l5ibw92x6g54zn7ris1b1ym9rvavsasg7b")))
    ("armhf-linux"
     ("bash"
      ,(base32 "0s6f1s26g4dsrrkl39zblvwpxmbzi6n9mgqf6vxsqz42gik6bgyn"))
     ("mkdir"
      ,(base32 "1r5rcp35niyxfkrdf00y2ba8ifrq9bi76cr63lwjf2l655j1i5p7"))
     ("tar"
      ,(base32 "0dksx5im3fv8ximz7368bsax9f26nn47ds74298flm5lnvpv9xly"))
     ("xz"
      ,(base32 "1cqqavghjfr0iwxqf61lrssv27wfigysgq2rs4rm1gkmn04yn1k3")))
    ("i686-linux"
     ("bash"
      ,(base32 "0rjaxyzjdllfkf1abczvgaf3cdcc7mmahyvdbkjmjzhgz92pv23g"))
     ("mkdir"
      ,(base32 "133ybmfpkmsnysrzbngwvbysqnsmfi8is8zifs7i7n6n600h4s1w"))
     ("tar"
      ,(base32 "07830bx29ad5i0l1ykj0g0b1jayjdblf01sr3ww9wbnwdbzinqms"))
     ("xz"
      ,(base32 "0i9kxdi17bm5gxfi2xzm0y73p3ii0cqxli1sbljm6rh2fjgyn90k")))
    ("i586-gnu"
     ("bash"
      ,(base32 "1as8649aqaibahhhrvkj10ci8shpi4hq5n7gnik8rhhy0dc1jarg"))
     ("mkdir"
      ,(base32 "1snqgpfrl00hfn82lm29jqylzjsfb9jd6ha74dp12phwb8fpbmb9"))
     ("tar"
      ,(base32 "0nq2c1zb3wv5bf7kd83sziaashydazrn7xgq6kijlk0zj2syzc2m"))
     ("xz"
      ,(base32 "033rhpk6zrpxpd6ffjyg5y2zwq9x9cnq0zljb7k8jlncbalsayq5")))
    ("mips64el-linux"
     ("bash"
      ,(base32 "1aw046dhda240k9pb9iaj5aqkm23gkvxa9j82n4k7fk87nbrixw6"))
     ("mkdir"
      ,(base32 "0c9j6qgyw84zxbry3ypifzll13gy8ax71w40kdk1h11jbgla3f5k"))
     ("tar"
      ,(base32 "06gmqdjq3rl8lr47b9fyx4ifnm5x56ymc8lyryp1ax1j2s4y5jb4"))
     ("xz"
      ,(base32 "09j1d69qr0hhhx4k4ih8wp00dfc9y4rp01hfg3vc15yxd0jxabh5")))
    ("powerpc64le-linux"
     ("bash"
      ,(base32 "1kiw7n6mkdy2x9in97646nb7aiayxr090ws1hbrlazah3fjqi6nj"))
     ("mkdir"
      ,(base32 "04dpvi231zcl40ig048vqqnyvmnkw1byrm1q1qqvs1f0g16yhrrk"))
     ("tar"
      ,(base32 "150c8948cz8r208g6qgn2dn4f4zs5kpgbpbg6bwag6yw42rapw2l"))
     ("xz"
      ,(base32 "0v5738idy9pqzcbrjdpxi5c6qs5m78zrpsydmrpx5cfcfzbkxzjh")))
    ("riscv64-linux"
     ("bash"
      ,(base32 "0almlf73k6hbm495kzf4bw1rzsg5qddn7z2rf5l3d1xcapac2hj3"))
     ("mkdir"
      ,(base32 "0rg1amdcqfkplcy1608jignl8jq0wqzfkp430mwik3f62959gya6"))
     ("tar"
      ,(base32 "17d3x27qhiwk7h6ns0xrvbrq0frxz89mjjh2cdwx2rraq5x6wffm"))
     ("xz"
      ,(base32 "0nxn75xf386vdq3igmgm8gnyk4h4x0cm8jv71vlb2jvwxh0cyw1q")))))

(define %bootstrap-executable-base-urls
  ;; This is where the bootstrap executables come from.
  '("https://git.savannah.gnu.org/cgit/guix.git/plain/gnu/packages/bootstrap/"
    "https://alpha.gnu.org/gnu/guix/bootstrap/"
    "http://flashner.co.il/guix/bootstrap/"
    "http://lilypond.org/janneke/guix/"))

(define (bootstrap-executable-file-name system program)
  "Return the FILE-NAME part of url where PROGRAM can be found for SYSTEM."
  (match system
    ("powerpc64le-linux" (string-append system "/20210106/" program))
    ("i586-gnu" (string-append system "/20200326/" program))
    ("powerpc-linux" (string-append system "/20200923/bin/" program))
    ("riscv64-linux" (string-append system "/20210725/bin/" program))
    (_ (string-append system "/" program
                      "?id=44f07d1dc6806e97c4e9ee3e6be883cc59dc666e"))))

(define bootstrap-executable
  (mlambda (program system)
    "Return an origin for PROGRAM, a statically-linked bootstrap executable
built for SYSTEM."
    (let ((system (if (string=? system "x86_64-linux")
                      "i686-linux"
                      system)))
      (match (assoc-ref (assoc-ref %bootstrap-executables system)
                        program)
        (#f
         (raise (condition
                 (&message
                  (message
                   (format #f (G_ "could not find bootstrap binary '~a' \
for system '~a'")
                           program system))))))
        ((bv)
         (origin
           (method url-fetch/executable)
           (uri (map (cute string-append <>
                           (bootstrap-executable-file-name system program))
                     %bootstrap-executable-base-urls))
           (file-name program)
           (hash (content-hash bv sha256))))))))


;;;
;;; Helper procedures.
;;;

(define bootstrap-origin
  (mlambdaq (source)
    "Return a variant of SOURCE, an <origin> instance, whose method uses
%BOOTSTRAP-GUILE to do its job."
    (define (boot fetch)
      (lambda* (url hash-algo hash
                    #:optional name #:key system)
        (fetch url hash-algo hash name
               #:guile %bootstrap-guile
               #:system system)))

    (define %bootstrap-patch-inputs
      ;; Packages used when an <origin> has a non-empty 'patches' field.
      `(("tar"   ,%bootstrap-coreutils&co)
        ("xz"    ,%bootstrap-coreutils&co)
        ("bzip2" ,%bootstrap-coreutils&co)
        ("gzip"  ,%bootstrap-coreutils&co)
        ("patch" ,%bootstrap-coreutils&co)))

    (let ((orig-method (origin-method source)))
      (if (or (not (null? (origin-patches source)))
              (origin-snippet source))
          (origin (inherit source)
                  (method (if (eq? orig-method url-fetch)
                              (boot url-fetch)
                              orig-method))
                  (patch-guile %bootstrap-guile)
                  (patch-inputs %bootstrap-patch-inputs)

                  ;; Patches can be origins as well, so process them.
                  (patches (map (match-lambda
                                  ((? origin? patch)
                                   (bootstrap-origin patch))
                                  (patch patch))
                                (origin-patches source))))
          source))))

(define* (package-from-tarball name source program-to-test description
                               #:key snippet)
  "Return a package that correspond to the extraction of SOURCE.
PROGRAM-TO-TEST is #f or a string: the program to run after extraction of
SOURCE to check whether everything is alright.  If SNIPPET is provided, it is
evaluated after extracting SOURCE.  SNIPPET should return true if successful,
or false to signal an error."
  (package
    (name name)
    (version "0")
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (let ((out     (assoc-ref %outputs "out"))
              (tar     (assoc-ref %build-inputs "tar"))
              (xz      (assoc-ref %build-inputs "xz"))
              (tarball (assoc-ref %build-inputs "tarball")))

          (mkdir out)
          (copy-file tarball "binaries.tar.xz")
          (invoke xz "-d" "binaries.tar.xz")
          (let ((builddir (getcwd)))
            (with-directory-excursion out
              (invoke tar "xvf"
                      (string-append builddir "/binaries.tar"))
              ,@(if snippet (list snippet) '())
              (or (not ,program-to-test)
                  (invoke (string-append "bin/" ,program-to-test)
                          "--version"))))))))
    (inputs
     `(("tar" ,(bootstrap-executable "tar" (%current-system)))
       ("xz"  ,(bootstrap-executable "xz" (%current-system)))
       ("tarball" ,(bootstrap-origin (source (%current-system))))))
    (source #f)
    (synopsis description)
    (description description)
    (home-page #f)
    (license gpl3+)))

(define package-with-bootstrap-guile
  (mlambdaq (p)
    "Return a variant of P such that all its origins are fetched with
%BOOTSTRAP-GUILE."
    (define rewritten-input
      (match-lambda
        ((name (? origin? o))
         `(,name ,(bootstrap-origin o)))
        ((name (? package? p) sub-drvs ...)
         `(,name ,(package-with-bootstrap-guile p) ,@sub-drvs))
        (x x)))

    (package (inherit p)
             (source (match (package-source p)
                       ((? origin? o) (bootstrap-origin o))
                       (s s)))
             (inputs (map rewritten-input
                          (package-inputs p)))
             (native-inputs (map rewritten-input
                                 (package-native-inputs p)))
             (propagated-inputs (map rewritten-input
                                     (package-propagated-inputs p)))
             (replacement (and=> (package-replacement p)
                                 package-with-bootstrap-guile)))))

(define* (glibc-dynamic-linker
          #:optional (system (or (and=> (%current-target-system)
                                        gnu-triplet->nix-system)
                                 (%current-system))))
  "Return the name of Glibc's dynamic linker for SYSTEM."
  ;; See the 'SYSDEP_KNOWN_INTERPRETER_NAMES' cpp macro in libc.
  (cond ((string=? system "x86_64-linux") "/lib/ld-linux-x86-64.so.2")
        ((string=? system "i686-linux") "/lib/ld-linux.so.2")
        ((string=? system "armhf-linux") "/lib/ld-linux-armhf.so.3")
        ((string=? system "mips64el-linux") "/lib/ld.so.1")
        ((string=? system "i586-gnu") "/lib/ld.so.1")
        ((string=? system "i686-gnu") "/lib/ld.so.1")
        ((string=? system "aarch64-linux") "/lib/ld-linux-aarch64.so.1")
        ((string=? system "powerpc-linux") "/lib/ld.so.1")
        ((string=? system "powerpc64-linux") "/lib/ld64.so.1")
        ((string=? system "powerpc64le-linux") "/lib/ld64.so.2")
        ((string=? system "alpha-linux") "/lib/ld-linux.so.2")
        ((string=? system "s390x-linux") "/lib/ld64.so.1")
        ((string=? system "riscv64-linux") "/lib/ld-linux-riscv64-lp64d.so.1")

        ;; XXX: This one is used bare-bones, without a libc, so add a case
        ;; here just so we can keep going.
        ((string=? system "arm-elf") "no-ld.so")
        ((string=? system "arm-eabi") "no-ld.so")
        ((string=? system "xtensa-elf") "no-ld.so")
        ((string=? system "avr") "no-ld.so")
        ((string=? system "propeller-elf") "no-ld.so")
        ((string=? system "i686-mingw") "no-ld.so")
        ((string=? system "x86_64-mingw") "no-ld.so")
        ((string=? system "vc4-elf") "no-ld.so")

        (else (error "dynamic linker name not known for this system"
                     system))))


;;;
;;; Bootstrap packages.
;;;

(define %bootstrap-base-urls
  ;; This is where the initial binaries come from.
  '("https://ftp.gnu.org/gnu/guix/bootstrap"
    "https://alpha.gnu.org/gnu/guix/bootstrap"
    "http://ftp.gnu.org/gnu/guix/bootstrap"
    "http://alpha.gnu.org/gnu/guix/bootstrap"
    "ftp://alpha.gnu.org/gnu/guix/bootstrap"
    "http://www.fdn.fr/~lcourtes/software/guix/packages"
    "http://flashner.co.il/guix/bootstrap"
    "http://lilypond.org/janneke/guix/"))

(define (bootstrap-guile-url-path system)
  "Return the URI for FILE."
  (string-append "/" system
                 (match system
                   ("aarch64-linux"
                    "/20170217/guile-2.0.14.tar.xz")
                   ("powerpc-linux"
                    "/20200923/guile-2.0.14.tar.xz")
                   ("armhf-linux"
                    "/20150101/guile-2.0.11.tar.xz")
                   ("i586-gnu"
                    "/20200326/guile-static-stripped-2.0.14-i586-pc-gnu.tar.xz")
                   ("powerpc64le-linux"
                    "/20210106/guile-static-stripped-2.0.14-powerpc64le-linux-gnu.tar.xz")
                   ("riscv64-linux"
                    "/20210725/guile-3.0.2.tar.xz")
                   (_
                    "/20131110/guile-2.0.9.tar.xz"))))

(define (bootstrap-guile-hash system)
  "Return the SHA256 hash of the Guile bootstrap tarball for SYSTEM."
  (match system
    ("x86_64-linux"
     (base32 "1w2p5zyrglzzniqgvyn1b55vprfzhgk8vzbzkkbdgl5248si0yq3"))
    ("i686-linux"
     (base32 "0im800m30abgh7msh331pcbjvb4n02smz5cfzf1srv0kpx3csmxp"))
    ("mips64el-linux"
     (base32 "0fzp93lvi0hn54acc0fpvhc7bvl0yc853k62l958cihk03q80ilr"))
    ("armhf-linux"
     (base32 "1mi3brl7l58aww34rawhvja84xc7l1b4hmwdmc36fp9q9mfx0lg5"))
    ("powerpc64le-linux"
     (base32 "1rnyfz5q38jyvxddj617443bnnzql4vw0mxzqpj8wz48wx4bhbq0"))
    ("aarch64-linux"
     (base32 "1giy2aprjmn5fp9c4s9r125fljw4wv6ixy5739i5bffw4jgr0f9r"))
    ("i586-gnu"
     (base32 "0wgqpsmvg25rnqn49ap7kwd2qxccd8dr4lllzp7i3rjvgav27vac"))
    ("powerpc-linux"
     (base32 "1by2p7s27fbyjzfkcw8h65h4kkqh7d23kv4sgg5jppjn2qx7swq4"))
    ("riscv64-linux"
     (base32 "12pqmhsbbp7hh9r1bjdl14l3a4q06plpz6dcks9dysb4czay8p9f"))))

(define (bootstrap-guile-origin system)
  "Return an <origin> object for the Guile tarball of SYSTEM."
  (origin
    (method url-fetch)
    (uri (map (cute string-append <> (bootstrap-guile-url-path system))
              %bootstrap-base-urls))
    (sha256 (bootstrap-guile-hash system))))

(define (download-bootstrap-guile system)
  "Return a derivation that downloads the bootstrap Guile tarball for SYSTEM."
  (let* ((path (bootstrap-guile-url-path system))
         (base (basename path))
         (urls (map (cut string-append <> path) %bootstrap-base-urls)))
    (url-fetch urls 'sha256 (bootstrap-guile-hash system)
               #:system system)))

(define* (raw-build name inputs
                    #:key outputs system search-paths
                    #:allow-other-keys)
  (define (->store file)
    (lower-object (bootstrap-executable file system)
                  system))

  (define (make-guile-wrapper bash guile-real)
    ;; The following code, run by the bootstrap guile after it is unpacked,
    ;; creates a wrapper for itself to set its load path.  This replaces the
    ;; previous non-portable method based on reading the /proc/self/exe
    ;; symlink.
    '(begin
       (use-modules (ice-9 match))
       (match (command-line)
         ((_ out bash)
          (let ((bin-dir    (string-append out "/bin"))
                (guile      (string-append out "/bin/guile"))
                (guile-real (string-append out "/bin/.guile-real"))
                ;; We must avoid using a bare dollar sign in this code,
                ;; because it would be interpreted by the shell.
                (dollar     (string (integer->char 36))))
            (chmod bin-dir #o755)
            (rename-file guile guile-real)
            (call-with-output-file guile
              (lambda (p)
                (format p "\
#!~a
export GUILE_SYSTEM_PATH=~a/share/guile/2.0
export GUILE_SYSTEM_COMPILED_PATH=~a/lib/guile/2.0/ccache
exec -a \"~a0\" ~a \"~a@\"\n"
                        bash out out dollar guile-real dollar)))
            (chmod guile   #o555)
            (chmod bin-dir #o555))))))

  (mlet* %store-monad ((tar   (->store "tar"))
                       (xz    (->store "xz"))
                       (mkdir (->store "mkdir"))
                       (bash  (->store "bash"))
                       (guile (download-bootstrap-guile system))
                       (wrapper -> (make-guile-wrapper bash guile))
                       (builder
                        (text-file "build-bootstrap-guile.sh"
                                   (format #f "
echo \"unpacking bootstrap Guile to '$out'...\"
~a $out
cd $out
~a -dc < $GUILE_TARBALL | ~a xv

# Use the bootstrap guile to create its own wrapper to set the load path.
GUILE_SYSTEM_PATH=$out/share/guile/2.0 \
GUILE_SYSTEM_COMPILED_PATH=$out/lib/guile/2.0/ccache \
$out/bin/guile -c ~s $out ~a

# Sanity check.
$out/bin/guile --version~%"
                                           (derivation->output-path mkdir)
                                           (derivation->output-path xz)
                                           (derivation->output-path tar)
                                           (object->string wrapper)
                                           (derivation->output-path bash)))))
    (raw-derivation name
                    (derivation->output-path bash) `(,builder)
                    #:system system
                    #:inputs (map derivation-input
                                  (list bash mkdir tar xz guile))
                    #:sources (list builder)
                    #:env-vars `(("GUILE_TARBALL"
                                  . ,(derivation->output-path guile))))))

(define* (raw-build-guile3 name inputs
                    #:key outputs system search-paths
                    #:allow-other-keys)
  (define (->store file)
    (lower-object (bootstrap-executable file system)
                  system))

  (define (make-guile-wrapper bash guile-real)
    ;; The following code, run by the bootstrap guile after it is unpacked,
    ;; creates a wrapper for itself to set its load path.  This replaces the
    ;; previous non-portable method based on reading the /proc/self/exe
    ;; symlink.
    '(begin
       (use-modules (ice-9 match))
       (match (command-line)
         ((_ out bash)
          (let ((bin-dir    (string-append out "/bin"))
                (guile      (string-append out "/bin/guile"))
                (guile-real (string-append out "/bin/.guile-real"))
                ;; We must avoid using a bare dollar sign in this code,
                ;; because it would be interpreted by the shell.
                (dollar     (string (integer->char 36))))
            (chmod bin-dir #o755)
            (rename-file guile guile-real)
            (call-with-output-file guile
              (lambda (p)
                (format p "\
#!~a
export GUILE_SYSTEM_PATH=~a/share/guile/3.0
export GUILE_SYSTEM_COMPILED_PATH=~a/lib/guile/3.0/ccache
exec -a \"~a0\" ~a \"~a@\"\n"
                        bash out out dollar guile-real dollar)))
            (chmod guile   #o555)
            (chmod bin-dir #o555))))))

  (mlet* %store-monad ((tar   (->store "tar"))
                       (xz    (->store "xz"))
                       (mkdir (->store "mkdir"))
                       (bash  (->store "bash"))
                       (guile (download-bootstrap-guile system))
                       (wrapper -> (make-guile-wrapper bash guile))
                       (builder
                        (text-file "build-bootstrap-guile.sh"
                                   (format #f "
echo \"unpacking bootstrap Guile to '$out'...\"
~a $out
cd $out
~a -dc < $GUILE_TARBALL | ~a xv

# Use the bootstrap guile to create its own wrapper to set the load path.
GUILE_SYSTEM_PATH=$out/share/guile/3.0 \
GUILE_SYSTEM_COMPILED_PATH=$out/lib/guile/3.0/ccache \
$out/bin/guile -c ~s $out ~a

# Sanity check.
$out/bin/guile --version~%"
                                           (derivation->output-path mkdir)
                                           (derivation->output-path xz)
                                           (derivation->output-path tar)
                                           (object->string wrapper)
                                           (derivation->output-path bash)))))
    (raw-derivation name
                    (derivation->output-path bash) `(,builder)
                    #:system system
                    #:inputs (map derivation-input
                                  (list bash mkdir tar xz guile))
                    #:sources (list builder)
                    #:env-vars `(("GUILE_TARBALL"
                                  . ,(derivation->output-path guile))))))

(define* (make-raw-bag name
                       #:key source inputs native-inputs outputs
                       system target)
  (bag
    (name name)
    (system system)
    (build-inputs inputs)
    (build (cond ((target-riscv64?)
                  raw-build-guile3)
                 (else raw-build)))))

(define %bootstrap-guile
  ;; The Guile used to run the build scripts of the initial derivations.
  ;; It is just unpacked from a tarball containing a pre-built binary.
  ;; This is typically built using %GUILE-BOOTSTRAP-TARBALL below.
  ;;
  ;; XXX: Would need libc's `libnss_files2.so' for proper `getaddrinfo'
  ;; support (for /etc/services).
  (let ((raw (build-system
               (name 'raw)
               (description "Raw build system with direct store access")
               (lower make-raw-bag))))
   (package
     (name "guile-bootstrap")
     (version "2.0")
     (source #f)
     (build-system raw)
     (synopsis "Bootstrap Guile")
     (description "Pre-built Guile for bootstrapping purposes.")
     (home-page #f)
     (license lgpl3+))))

(define %bootstrap-coreutils&co
  (package-from-tarball "bootstrap-binaries"
                        (lambda (system)
                          (origin
                           (method url-fetch)
                           (uri (map (cut string-append <> "/" system
                                          (match system
                                            ("armhf-linux"
                                             "/20150101/static-binaries.tar.xz")
                                            ("aarch64-linux"
                                             "/20170217/static-binaries.tar.xz")
                                            ("powerpc64le-linux"
                                             "/20210106/static-binaries-0-powerpc64le-linux-gnu.tar.xz")
                                            ("i586-gnu"
                                             "/20200326/static-binaries-0-i586-pc-gnu.tar.xz")
                                            ("powerpc-linux"
                                             "/20200923/static-binaries.tar.xz")
                                            ("riscv64-linux"
                                             "/20210725/static-binaries.tar.xz")
                                            (_
                                             "/20131110/static-binaries.tar.xz")))
                                     %bootstrap-base-urls))
                           (sha256
                            (match system
                              ("x86_64-linux"
                               (base32
                                "0c533p9dhczzcsa1117gmfq3pc8w362g4mx84ik36srpr7cx2bg4"))
                              ("i686-linux"
                               (base32
                                "0s5b3jb315n13m1k8095l0a5hfrsz8g0fv1b6riyc5hnxqyphlak"))
                              ("armhf-linux"
                               (base32
                                "0gf0fn2kbpxkjixkmx5f4z6hv6qpmgixl69zgg74dbsfdfj8jdv5"))
                              ("aarch64-linux"
                               (base32
                                "18dfiq6c6xhsdpbidigw6480wh0vdgsxqq3xindq4lpdgqlccpfh"))
                              ("powerpc64le-linux"
                               (base32
                                "0afs2j9z2d1hjq42myz4iwjh0aqgzf59inifw87x6b6p1z9wv92v"))
                              ("i586-gnu"
                               (base32
                                "17kllqnf3fg79gzy9ansgi801c46yh9c23h4d923plvb0nfm1cfn"))
                              ("powerpc-linux"
                               (base32
                                "0kspxy0yczan2vlih6aa9hailr2inz000fqa0gn5x9d1fxxa5y8m"))
                              ("riscv64-linux"
                               (base32
                                "0x0xjlpmyh6rkr51p00gp6pscgl6zjida1rsg8vk3rinyi6rrbkg"))
                              ("mips64el-linux"
                               (base32
                                "072y4wyfsj1bs80r6vbybbafy8ya4vfy7qj25dklwk97m6g71753"))))))
                        "fgrep"                    ; the program to test
                        "Bootstrap binaries of Coreutils, Awk, etc."
                        #:snippet
                        '(let ((path (list (string-append (getcwd) "/bin"))))
                           (chmod "bin" #o755)
                           (patch-shebang "bin/egrep" path)
                           (patch-shebang "bin/fgrep" path)
                           ;; Starting with grep@2.25 'egrep' and 'fgrep' are shell files
                           ;; that call 'grep'.  If the bootstrap 'egrep' and 'fgrep'
                           ;; are not binaries then patch them to execute 'grep' via its
                           ;; absolute file name instead of searching for it in $PATH.
                           (if (not (elf-file? "bin/egrep"))
                             (substitute* '("bin/egrep" "bin/fgrep")
                               (("^exec grep") (string-append (getcwd) "/bin/grep"))))
                           (chmod "bin" #o555))))

(define-public %bootstrap-linux-libre-headers
  (package-from-tarball
   "linux-libre-headers-bootstrap"
   (lambda (system)
     (origin
       (method url-fetch)
       (uri (map (cute string-append <>
                       "/i686-linux/20190815/"
                       "linux-libre-headers-stripped-4.14.67-i686-linux.tar.xz")
                 %bootstrap-base-urls))
       (sha256
        (base32
         "0sm2z9x4wk45bh6qfs94p0w1d6hsy6dqx9sw38qsqbvxwa1qzk8s"))))
   #f                                   ; no program to test
   "Bootstrap linux-libre-headers"))

(define %bootstrap-binutils
  (package-from-tarball "binutils-bootstrap"
                        (lambda (system)
                          (origin
                           (method url-fetch)
                           (uri (map (cut string-append <> "/" system
                                          (match system
                                            ("armhf-linux"
                                             "/20150101/binutils-2.25.tar.xz")
                                            ("aarch64-linux"
                                             "/20170217/binutils-2.27.tar.xz")
                                            ("powerpc64le-linux"
                                             "/20210106/binutils-static-stripped-2.34-powerpc64le-linux-gnu.tar.xz")
                                            ("i586-gnu"
                                             "/20200326/binutils-static-stripped-2.34-i586-pc-gnu.tar.xz")
                                            ("powerpc-linux"
                                             "/20200923/binutils-2.35.1.tar.xz")
                                            ("riscv64-linux"
                                             "/20210725/binutils-2.34.tar.xz")
                                            (_
                                             "/20131110/binutils-2.23.2.tar.xz")))
                                     %bootstrap-base-urls))
                           (sha256
                            (match system
                              ("x86_64-linux"
                               (base32
                                "1j5yivz7zkjqfsfmxzrrrffwyayjqyfxgpi89df0w4qziqs2dg20"))
                              ("i686-linux"
                               (base32
                                "14jgwf9gscd7l2pnz610b1zia06dvcm2qyzvni31b8zpgmcai2v9"))
                              ("armhf-linux"
                               (base32
                                "1v7dj6bzn6m36f20gw31l99xaabq4xrhrx3gwqkhhig0mdlmr69q"))
                              ("aarch64-linux"
                               (base32
                                "111s7ilfiby033rczc71797xrmaa3qlv179wdvsaq132pd51xv3n"))
                              ("powerpc64le-linux"
                               (base32
                                "1klxy945c61134mzhqzz2gbk8w0n8jq7arwkrvz78d22ff2q0cwz"))
                              ("riscv64-linux"
                               (base32
                                "0n9qf4vbilfmh1lknhw000waakj4q6s50pnjazr5137skm976z5m"))
                              ("i586-gnu"
                               (base32
                                "11kykv1kmqc5wln57rs4klaqa13hm952smkc57qcsyss21kfjprs"))
                              ("powerpc-linux"
                               (base32
                                "0asbg1c4avkrvh057mx0942xwddd136jni382zqsxzn79ls42yq8"))
                              ("mips64el-linux"
                               (base32
                                "1x8kkhcxmfyzg1ddpz2pxs6fbdl6412r7x0nzbmi5n7mj8zw2gy7"))))))
                        "ld"                      ; the program to test
                        "Bootstrap binaries of the GNU Binutils"))

(define %bootstrap-glibc
  ;; The initial libc.
  (package
    (name "glibc-bootstrap")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (let ((out     (assoc-ref %outputs "out"))
              (tar     (assoc-ref %build-inputs "tar"))
              (xz      (assoc-ref %build-inputs "xz"))
              (tarball (assoc-ref %build-inputs "tarball")))

          (mkdir out)
          (copy-file tarball "binaries.tar.xz")
          (invoke xz "-d" "binaries.tar.xz")
          (let ((builddir (getcwd)))
            (with-directory-excursion out
              (invoke tar "xvf"
                      (string-append builddir
                                     "/binaries.tar"))
              (chmod "lib" #o755)

              ;; Patch libc.so so it refers to the right path.
              (substitute* "lib/libc.so"
                (("/[^ ]+/lib/(libc|ld)" _ prefix)
                 (string-append out "/lib/" prefix)))

              #t))))))
    (inputs
     `(("tar" ,(bootstrap-executable "tar" (%current-system)))
       ("xz"  ,(bootstrap-executable "xz" (%current-system)))
       ("tarball" ,(bootstrap-origin
                    (origin
                     (method url-fetch)
                     (uri (map (cut string-append <> "/" (%current-system)
                                    (match (%current-system)
                                      ("armhf-linux"
                                       "/20150101/glibc-2.20.tar.xz")
                                      ("aarch64-linux"
                                       "/20170217/glibc-2.25.tar.xz")
                                      ("powerpc64le-linux"
                                       "/20210106/glibc-stripped-2.31-powerpc64le-linux-gnu.tar.xz")
                                      ("i586-gnu"
                                       "/20200326/glibc-stripped-2.31-i586-pc-gnu.tar.xz")
                                      ("powerpc-linux"
                                       "/20200923/glibc-2.32.tar.xz")
                                      ("riscv64-linux"
                                       "/20210725/glibc-2.31.tar.xz")
                                      (_
                                       "/20131110/glibc-2.18.tar.xz")))
                               %bootstrap-base-urls))
                     (sha256
                      (match (%current-system)
                        ("x86_64-linux"
                         (base32
                          "0jlqrgavvnplj1b083s20jj9iddr4lzfvwybw5xrcis9spbfzk7v"))
                        ("i686-linux"
                         (base32
                          "1hgrccw1zqdc7lvgivwa54d9l3zsim5pqm0dykxg0z522h6gr05w"))
                        ("armhf-linux"
                         (base32
                          "18cmgvpllqfpn6khsmivqib7ys8ymnq0hdzi3qp24prik0ykz8gn"))
                        ("aarch64-linux"
                         (base32
                          "07nx3x8598i2924rjnlrncg6rm61c9bmcczbbcpbx0fb742nvv5c"))
                        ("powerpc64le-linux"
                         (base32
                          "1a1df6z8gkaq09md3jy94lixnh20599p58p0s856p10xwjaqr1iz"))
                        ("riscv64-linux"
                         (base32
                          "0d9x80vm7ca1pd2whcmpm1h14zxpb58kqajlxlwffzm04xfsjnxm"))
                        ("i586-gnu"
                         (base32
                          "14ddm10lpbas8bankmn5bcrlqvz1v5dnn1qjzxb19r57vd2w5952"))
                        ("powerpc-linux"
                         (base32
                          "0smmssyjrlk5cvx49586smmk81gkwff0i6r91n4rir4jm6ba25sb"))
                        ("mips64el-linux"
                         (base32
                          "0k97a3whzx3apsi9n2cbsrr79ad6lh00klxph9hw4fqyp1abkdsg")))))))))
    (synopsis "Bootstrap binaries and headers of the GNU C Library")
    (description synopsis)
    (home-page #f)
    (license lgpl2.1+)))

(define %bootstrap-gcc
  ;; The initial GCC.  Uses binaries from a tarball typically built by
  ;; %GCC-BOOTSTRAP-TARBALL.
  (package
    (name "gcc-bootstrap")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 popen))

         (let ((out     (assoc-ref %outputs "out"))
               (tar     (assoc-ref %build-inputs "tar"))
               (xz      (assoc-ref %build-inputs "xz"))
               (bash    (assoc-ref %build-inputs "bash"))
               (libc    (assoc-ref %build-inputs "libc"))
               (tarball (assoc-ref %build-inputs "tarball")))

           (mkdir out)
           (copy-file tarball "binaries.tar.xz")
           (invoke xz "-d" "binaries.tar.xz")
           (let ((builddir (getcwd))
                 (bindir   (string-append out "/bin")))
             (with-directory-excursion out
               (invoke tar "xvf"
                       (string-append builddir "/binaries.tar")))

             (with-directory-excursion bindir
               (chmod "." #o755)
               (rename-file "gcc" ".gcc-wrapped")
               (call-with-output-file "gcc"
                 (lambda (p)
                   (format p "#!~a
exec ~a/bin/.gcc-wrapped -B~a/lib \
     -Wl,-rpath -Wl,~a/lib \
     -Wl,-dynamic-linker -Wl,~a/~a \"$@\"~%"
                           bash
                           out libc libc libc
                           ,(glibc-dynamic-linker))))

               (chmod "gcc" #o555)
               #t))))))
    (inputs
     `(("tar" ,(bootstrap-executable "tar" (%current-system)))
       ("xz"  ,(bootstrap-executable "xz" (%current-system)))
       ("bash" ,(bootstrap-executable "bash" (%current-system)))
       ("libc" ,%bootstrap-glibc)
       ("tarball" ,(bootstrap-origin
                    (origin
                      (method url-fetch)
                      (uri (map (cut string-append <> "/" (%current-system)
                                     (match (%current-system)
                                       ("armhf-linux"
                                        "/20150101/gcc-4.8.4.tar.xz")
                                       ("aarch64-linux"
                                        "/20170217/gcc-5.4.0.tar.xz")
                                       ("powerpc64le-linux"
                                        "/20210106/gcc-stripped-5.5.0-powerpc64le-linux-gnu.tar.xz")
                                       ("i586-gnu"
                                        "/20200326/gcc-stripped-5.5.0-i586-pc-gnu.tar.xz")
                                       ("powerpc-linux"
                                        "/20200923/gcc-5.5.0.tar.xz")
                                       ("riscv64-linux"
                                        "/20210725/gcc-7.5.0.tar.xz")
                                       (_
                                        "/20131110/gcc-4.8.2.tar.xz")))
                                %bootstrap-base-urls))
                      (sha256
                       (match (%current-system)
                         ("x86_64-linux"
                          (base32
                           "17ga4m6195n4fnbzdkmik834znkhs53nkypp6557pl1ps7dgqbls"))
                         ("i686-linux"
                          (base32
                           "150c1arrf2k8vfy6dpxh59vcgs4p1bgiz2av5m19dynpks7rjnyw"))
                         ("armhf-linux"
                          (base32
                           "0ghz825yzp43fxw53kd6afm8nkz16f7dxi9xi40bfwc8x3nbbr8v"))
                         ("aarch64-linux"
                          (base32
                           "1ar3vdzyqbfm0z36kmvazvfswxhcihlacl2dzdjgiq25cqnq9ih1"))
                         ("powerpc64le-linux"
                          (base32
                           "151kjsai25vz2s667bgzpisx8f281fpl3n9pxz2yrp9jlnadz3m1"))
                         ("riscv64-linux"
                          (base32
                           "1k4mbnb54wj2q37fgshf5dfixixqnhn002vhzvi9pnb57xb9v14d"))
                         ("i586-gnu"
                          (base32
                           "1j2zc58wzil71a34h7c70sd68dmqvcscrw3rmn2whq79vd70zvv5"))
                         ("powerpc-linux"
                          (base32
                           "1p7df3yixhm87dw5sccc6yn1i9db1r9hnmsg87wq5xi4rfmirq7w"))
                         ("mips64el-linux"
                          (base32
                           "1m5miqkyng45l745n0sfafdpjkqv9225xf44jqkygwsipj2cv9ks")))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "CPLUS_INCLUDE_PATH")
            (files '("include/c++" "include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib" "lib64")))))
    (synopsis "Bootstrap binaries of the GNU Compiler Collection")
    (description synopsis)
    (home-page #f)
    (license gpl3+)))

(define %bootstrap-mescc-tools
  ;; The initial MesCC tools.  Uses binaries from a tarball typically built by
  ;; %MESCC-TOOLS-BOOTSTRAP-TARBALL.
  (package
    (name "bootstrap-mescc-tools")
    (version "0.5.2")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 popen))
         (let ((out     (assoc-ref %outputs "out"))
               (tar     (assoc-ref %build-inputs "tar"))
               (xz      (assoc-ref %build-inputs "xz"))
               (tarball (assoc-ref %build-inputs "tarball")))

           (mkdir out)
           (copy-file tarball "binaries.tar.xz")
           (invoke xz "-d" "binaries.tar.xz")
           (let ((builddir (getcwd))
                 (bindir   (string-append out "/bin")))
             (with-directory-excursion out
               (invoke tar "xvf"
                       (string-append builddir "/binaries.tar"))))))))
    (inputs
     `(("tar" ,(bootstrap-executable "tar" (%current-system)))
       ("xz"  ,(bootstrap-executable "xz" (%current-system)))
       ("tarball"
        ,(bootstrap-origin
          (origin
            (method url-fetch)
            (uri (map
                  (cute string-append <>
                        "/i686-linux/20190815/"
                        "mescc-tools-static-stripped-0.5.2-i686-linux.tar.xz")
                  %bootstrap-base-urls))
            (sha256
             (base32
              "0c3kklgghzh4q2dbpl6asb74cimp7hp6jscdwqwmzxbapgcl6582")))))))
    (synopsis "Bootstrap binaries of MesCC Tools")
    (description synopsis)
    (home-page #f)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license gpl3+)))

(define %bootstrap-mes
  ;; The initial Mes.  Uses binaries from a tarball typically built by
  ;; %MES-BOOTSTRAP-TARBALL.
  (package
    (name "bootstrap-mes")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 popen))
         (let ((out     (assoc-ref %outputs "out"))
               (tar     (assoc-ref %build-inputs "tar"))
               (xz      (assoc-ref %build-inputs "xz"))
               (tarball (assoc-ref %build-inputs "tarball")))

           (mkdir out)
           (copy-file tarball "binaries.tar.xz")
           (invoke xz "-d" "binaries.tar.xz")
           (let ((builddir (getcwd))
                 (bindir   (string-append out "/bin")))
             (with-directory-excursion out
               (invoke tar "xvf"
                       (string-append builddir "/binaries.tar"))))))))
    (inputs
     `(("tar" ,(bootstrap-executable "tar" (%current-system)))
       ("xz"  ,(bootstrap-executable "xz" (%current-system)))
       ("tarball"
        ,(bootstrap-origin
          (origin
            (method url-fetch)
            (uri (map
                  (cute string-append <>
                        "/i686-linux/20190815/"
                        "mes-minimal-stripped-0.19-i686-linux.tar.xz")
                  %bootstrap-base-urls))
            (sha256
             (base32
              "1q4xjpx6nbn44kxnilpgl12bhpmwy2bblzwszc2ci7xkf400jcpv")))))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (synopsis "Bootstrap binaries of Mes")
    (description synopsis)
    (home-page #f)
    (license gpl3+)))

(define (%bootstrap-inputs)
  ;; The initial, pre-built inputs.  From now on, we can start building our
  ;; own packages.
  (match (%current-system)
    ((or "i686-linux" "x86_64-linux")
     `(("linux-libre-headers" ,%bootstrap-linux-libre-headers)
       ("bootstrap-mescc-tools" ,%bootstrap-mescc-tools)
       ("mes" ,%bootstrap-mes)))
    (_
     `(("libc" ,%bootstrap-glibc)
       ("gcc" ,%bootstrap-gcc)
       ("binutils" ,%bootstrap-binutils)
       ("coreutils&co" ,%bootstrap-coreutils&co)
       ;; In gnu-build-system.scm, we rely on the availability of Bash.
       ("bash" ,%bootstrap-coreutils&co)))))

(define %bootstrap-inputs-for-tests
  ;; These are bootstrap inputs that are cheap to produce (no compilation
  ;; needed) and that are meant to be used for testing.  (These are those we
  ;; used before the Mes-based reduced bootstrap.)
  `(("libc" ,%bootstrap-glibc)
    ("gcc" ,%bootstrap-gcc)
    ("binutils" ,%bootstrap-binutils)
    ("coreutils&co" ,%bootstrap-coreutils&co)
    ("bash" ,%bootstrap-coreutils&co)))

;;; bootstrap.scm ends here
