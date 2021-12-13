;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
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

(define-module (gnu packages sml)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public polyml
  (package
    (name "polyml")
    (version "5.8.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/polyml/polyml")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y3i919kzylvhwfsi6adnc0ah0xahl6ncna0g5bcjyhxsq2416rn"))))
    (build-system gnu-build-system)
    (inputs
     (list gmp lesstif libffi libx11 libxt))
    (arguments
     '(#:configure-flags
       (list "--with-system-libffi=yes"
             "--with-x=yes"
             "--with-threads=yes"
             "--with-gmp=yes")
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-compiler
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (define flags
               (if parallel-build?
                   (cons (format #f "-j~d" (parallel-job-count))
                         make-flags)
                   make-flags))
             (apply system* "make" (append flags (list "compiler"))))))))
    (home-page "https://www.polyml.org/")
    (synopsis "Standard ML implementation")
    (description "Poly/ML is a Standard ML implementation.  It is fully
compatible with the ML97 standard.  It includes a thread library, a foreign
function interface, and a symbolic debugger.")
    ;; Some source files specify 'or any later version'; some don't
    (license
     (list license:lgpl2.1
           license:lgpl2.1+))))

(define (smlnj-file version filename hash)
  (origin
    (method url-fetch)
    (uri (string-append "http://smlnj.cs.uchicago.edu/dist/working/"
                        version "/" filename))
    (sha256 (base32 hash))))

(define-public smlnj
  (package
    (name "smlnj")
    (version "110.99.2")
    (source #f)  ; Sources are passed as native-inputs.
    (supported-systems '("x86_64-linux" "i686-linux"))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each
               (lambda (file)
                 (invoke "tar" "xvf" (assoc-ref inputs file)))
               (list (if (string=? "i686-linux" ,(%current-system))
                       "boot.x86-unix"
                       "boot.amd64-unix")
                     "config"
                     "cm"
                     "compiler"
                     "runtime"
                     "system"
                     "MLRISC"
                     "smlnj-lib"
                     "old-basis"
                     "ckit"
                     "nlffi"
                     "cml"
                     "eXene"
                     "ml-lpt"
                     "ml-lex"
                     "ml-yacc"
                     "ml-burg"
                     "pgraph"
                     "trace-debug-profile"
                     "heap2asm"
                     "smlnj-c"
                     "doc"
                     "asdl"))
             ;; Same directory structure as what the config/unpack script
             ;; would produce.
             (mkdir "base")
             (rename-file "runtime" "base/runtime")
             (rename-file "compiler" "base/compiler")
             (rename-file "cm" "base/cm")
             (rename-file "old-basis" "base/old-basis")
             (rename-file "system" "base/system")
             #t))
         (delete 'configure)
         (replace 'patch-source-shebangs
           (lambda _
             ;; Fix paths to /bin/sh.
             (substitute* (list "config/install.sh"
                                (if (string=? "i686-linux" ,(%current-system))
                                  "base/runtime/objs/mk.x86-linux"
                                  "base/runtime/objs/mk.amd64-linux")
                                "asdl/configure"
                                "asdl/src/asdlgen/Makefile.in")
               (("^SHELL[[:space:]]*=[[:space:]]*/bin/sh")
                (string-append "SHELL=" (which "sh"))))
             (substitute* "asdl/configure"
               (("^SHELL=\\$\\{CONFIG_SHELL-/bin/sh\\}")
                (string-append "SHELL=" (which "sh"))))
             (substitute* (list "asdl/src/gen/fragments/mkfrags_sh.in"
                                "asdl/src/gen/fragments/mkmk_sh.in")
               (("^#!/bin/sh")
                (string-append "#!" (which "sh"))))
             #t))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "config/chk-global-names.sh"
               (("^CC=.*")
                (string-append "CC=" ,(cc-for-target))))

             ;; /bin and /usr/bin do not exist in the build environment.
             (substitute* "config/_arch-n-opsys"
               (("^export PATH") "")
               (("^PATH=\"/bin:/usr/bin\"") "")
               (("uname") (which "uname")))
             (substitute* "base/runtime/config/gen-posix-names.sh"
               (("^PATH=/bin:/usr/bin") ""))

             ;; The build process uses an SML Basis Library function
             ;; `OS.Process.system`, which uses "/bin/sh" (this is hardcoded).
             ;; However, /bin/sh does not exist in the Guix build environment.
             ;; Solution: binary patch — replace "/bin/sh" with "/tmp/sh".
             (symlink (which "sh") "/tmp/sh")
             (invoke "sed" "-i" "s,/bin/sh,/tmp/sh,"
                     (if (string=? "i686-linux" ,(%current-system))
                       "sml.boot.x86-unix/SMLNJ-BASIS/.cm/x86-unix/basis-common.cm"
                       "sml.boot.amd64-unix/SMLNJ-BASIS/.cm/amd64-unix/basis-common.cm"))

             ;; Build.
             (invoke "./config/install.sh" "-default"
                     (if (string=? "i686-linux" ,(%current-system))
                       "32"
                       "64"))

             ;; Undo the binary patch.
             (for-each
               (lambda (file)
                 (invoke "sed" "-i" "s,/tmp/sh,/bin/sh," file))
               (if (string=? "i686-linux" ,(%current-system))
                 '("bin/.heap/sml.x86-linux"
                   "lib/SMLNJ-BASIS/.cm/x86-unix/basis-common.cm")
                 '("bin/.heap/sml.amd64-linux"
                   "lib/SMLNJ-BASIS/.cm/amd64-unix/basis-common.cm")))

             ;; Set SMLNJ_HOME in the bin/ files, so that `sml` is able to find
             ;; the SML/NJ Library.
             (let ((out (assoc-ref outputs "out")))
               (for-each
                 (lambda (file)
                   (invoke "sed" "-i"
                           (string-append "2iSMLNJ_HOME=${SMLNJ_HOME:-" out "}")
                           file))
                 '("bin/.link-sml"
                   "bin/.run-sml"
                   "bin/ml-build"
                   "bin/ml-makedepend")))))
        (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin/"))
                    (out-lib (string-append out "/lib/"))
                    (out-man (string-append out "/share/man/")))
               (copy-recursively "bin" out-bin)
               (copy-recursively "lib" out-lib)
               (copy-recursively "doc/man" out-man))
             #t))
        (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((doc (string-append (assoc-ref outputs "doc")
                                       "/share/doc/smlnj")))
               (mkdir-p doc)
               (copy-recursively "doc" doc))
             #t)))))
    (native-inputs
     `(,(if (string=? "i686-linux" (%current-system))
          `("boot.x86-unix"
            ,(smlnj-file version
                         "boot.x86-unix.tgz"
                         "117dq1g387vcy1105dlpw770gzrg423jng89ynraiy6kiaalvbcq"))
          `("boot.amd64-unix"
            ,(smlnj-file version
                         "boot.amd64-unix.tgz"
                         "0cbaz8sxp30y2a4blm0pzk9aa1g9jj65d6d7kydvn0w7m22rjjff")))
       ("config"
        ,(smlnj-file version
                     "config.tgz"
                     "1sq60frd66kv807bahsyxda355qq67zvvb4sr1d72cv6capv5nsg"))
       ("cm"
        ,(smlnj-file version
                     "cm.tgz"
                     "087yy9k9vyyvhb24rni6js7s8iqbs8vjm9kjd9s311swjd66qhjc"))
       ("compiler"
        ,(smlnj-file version
                     "compiler.tgz"
                     "149vrmiba0dqggr15axqqzzfmd0kx7kchlr49gii6hw5dal6vqnn"))
       ("runtime"
        ,(smlnj-file version
                     "runtime.tgz"
                     "0c3q7qr2i3r91sx9p3z1ar7gvyk6qsm5gcbpbyj6l67qzn192yc3"))
       ("system"
        ,(smlnj-file version
                     "system.tgz"
                     "0aflnqh9zi9f8vs9pjlxysxplrwl98aiaxx4n41sba4m1r30n0za"))
       ("MLRISC"
        ,(smlnj-file version
                     "MLRISC.tgz"
                     "1kkga7r7qnw09s8yhqbhrq8gxf6c8x0fiwsbwkr8ls6xwv48sp74"))
       ("smlnj-lib"
        ,(smlnj-file version
                     "smlnj-lib.tgz"
                     "14fvqgn580k3ylvfhbkwv4cw87ipggq548r7jzp8fzfn2h6wdl01"))
       ("old-basis"
        ,(smlnj-file version
                     "old-basis.tgz"
                     "11j7a0sxyd1kwxjvxnarab5vc3x43gl90m07wxm37fd4jbfd1fn1"))
       ("ckit"
        ,(smlnj-file version
                     "ckit.tgz"
                     "1fbfjb2fhr6zkcz5jhqh3888zska6vffndyqwvk6rpbcl7an8niq"))
       ("nlffi"
        ,(smlnj-file version
                     "nlffi.tgz"
                     "0p5z77x295xfh71481kbd5pwis52kv03vxpad4pzkpk0l6smcgmj"))
       ("cml"
        ,(smlnj-file version
                     "cml.tgz"
                     "13xchaamwanxhwklsgkn1rmkr044h9qsj0rbr9c7pm903yivdwcn"))
       ("eXene"
        ,(smlnj-file version
                     "eXene.tgz"
                     "0p4snql0a1a952h98ma9ybmp7z1q305mz859b0mxhsg3jdrzl9wb"))
       ("ml-lpt"
        ,(smlnj-file version
                     "ml-lpt.tgz"
                     "0m00vglg95apdpzsy6qv88izj5ai4ibylxni01an75xpnxyy5qbg"))
       ("ml-lex"
        ,(smlnj-file version
                     "ml-lex.tgz"
                     "1pmi5qwjcf1h5nfi7d4vvm7cf90g6dlk2mqikj0y9c464ia1l2jc"))
       ("ml-yacc"
        ,(smlnj-file version
                     "ml-yacc.tgz"
                     "1zla2m1rn8r8k85ps9r2mw38xkh276j7aqv9f69v55102hchx13p"))
       ("ml-burg"
        ,(smlnj-file version
                     "ml-burg.tgz"
                     "14cqasasa273x09phykzjgk1wl6vrkdcwrdi39hnacp443cilz7x"))
       ("pgraph"
        ,(smlnj-file version
                     "pgraph.tgz"
                     "183fv61xlac5kpxn5m4iqgdvc2xb1chlxy5ip4i25x589bh4b5k9"))
       ("trace-debug-profile"
        ,(smlnj-file version
                     "trace-debug-profile.tgz"
                     "1k0w581kr43mpjzm7778xgx1rpz45aq1h80jdr6jls5vz3k8ia18"))
       ("heap2asm"
        ,(smlnj-file version
                     "heap2asm.tgz"
                     "0p9s42acngxh0401wm6fqs3im3rzzw9sh448x38zhdi47h8h1m9n"))
       ("smlnj-c"
        ,(smlnj-file version
                     "smlnj-c.tgz"
                     "054b1nhg5yk2jj01p11k08qzq8zc9jzg4mbgkcmcqaq7axp1rnxm"))
       ("doc"
        ,(smlnj-file version
                     "doc.tgz"
                     "0s35zrxdj76wzdz7c1i8ij00n6lfll4vjnypsy2j17q1maw7fq8j"))
       ("asdl"
        ,(smlnj-file version
                     "asdl.tgz"
                     "0mad2df5pmkdsb69gflxma6m6i3gla6hdmjjnkzk76pagpr8zb0m"))))
    (home-page "http://www.smlnj.org")
    (synopsis "Standard ML of New Jersey interactive compiler")
    (description
      "SML/NJ is an implementation of the Standard ML programming language.
Standard ML has many features, including type safety, polymorphism, algebraic
data types with pattern matching, higher-order functions, and a sophisticated
module system.  It is especially well-suited for writing compilers and other
language processors.")
    (license (license:fsf-free
               "https://www.smlnj.org/license.html"
               "https://www.gnu.org/licenses/license-list#StandardMLofNJ"))))
