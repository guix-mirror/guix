;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2020, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages pep)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mail) ; for libetpan
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sequoia)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public yml2
  (package
    (name "yml2")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.pep.foundation/fdik/yml2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fm1x1fv4lwcpbn59s55idzf7x173n59xpz8rlrxalmi6gvsjijr"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-lxml))
    (home-page "https://fdik.org/yml/")
    (synopsis "Use a Domain Specific Language for XML without defining
a grammar")
    (description "The YML compiler is a small Python script.  It
provides the command line front end yml2c.  As default, it compiles
your script and outputs to stdout, that usually is the terminal.  Your
shell provides options to redirect the output into a pipe or a file.")
    (license license:gpl2)))

(define fdik-libetpan
  ;; pEp Engine requires libetpan with a set of patches that have not been
  ;; upstreamed yet.
  (let ((commit "0b80c39dd1504462ba3a39dc53db7c960c3a63f3") ; 2020-11-27
        (checksum "0gv3ivaziadybjlf6rfpv1j5z5418243v5cvl4swlxd2njsh7gjk")
        (revision "6"))
   (package
    (inherit libetpan)
    (name "fdik-libetpan")
    (version (string-append "1.6-" revision "." (string-take commit 8)))
    (source
     (origin
       (inherit (package-source libetpan))
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.pep.foundation/pEp.foundation/libetpan")
             (commit commit)))
       (file-name (string-append name "-" version))
       (sha256 (base32 checksum)))))))

(define-public pep-engine
  (package
    (name "pep-engine")
    (version "2.1.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.pep.foundation/pEp.foundation/pEpEngine")
             (commit (string-append "Release_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00q96y9j985qfa382acsz02i0zf6ayq2gmg8z70jzl04isg1h3cn"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-build? #f
       #:make-flags '("NDEBUG=1") ; release build
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; pEpEngine does not use autotools and configure,
           ;; but a local.conf. We need to tweak the values there.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (yml2 (assoc-ref inputs "yml2")))
               (with-output-to-file "local.conf"
                 (lambda ()
                   (format #t "
PREFIX=~a
PER_MACHINE_DIRECTORY=${PREFIX}/share/pEp
SYSTEM_DB=~a/share/pEp/system.db
ASN1C=~a
YML2_PATH=~a
OPENPGP=SEQUOIA
"
                           out out (which "asn1c")
                           (string-append yml2 "/bin"))))
               #t)))
         (delete 'check)
         (add-after 'install 'install-db
           (lambda _
             (invoke "make" "-C" "db" "install"))))))
    (native-inputs
     (list asn1c ; >= 0.9.27
           pkg-config yml2))
    (inputs
     `(("libetpan" ,fdik-libetpan)
       ("libiconv" ,libiconv)
       ("nettle" ,nettle)
       ("openssl" ,openssl)
       ("sequoia" ,sequoia)
       ("sqlite3" ,sqlite)
       ("util-linux" ,util-linux "lib"))) ;; uuid.h
    (home-page "https://pep.foundation/")
    (synopsis "Library for automatic key management and encryption of
messages")
    (description "The p≡p engine is the core part of p≡p (pretty Easy
privacy).")
    (license ;; code: GPL 3, docs: CC-BY-SA
     (list license:gpl3 license:cc-by-sa3.0))))

(define-public libpepadapter
  (package
    (name "libpepadapter")
    (version "2.1.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.pep.foundation/pEp.foundation/libpEpAdapter")
             (commit (string-append "Release_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09ljj3x09y99wc47n063hpn62zi8cdvdha82rnaypvirrlga6a5w"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:tests? #f ;; building the tests fails
       #:make-flags '("NDEBUG=1") ; release build
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; libpEpAdapter does not use autotools and configure,
           ;; but a local.conf. We need to tweak the values there.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (engine (assoc-ref inputs "pep-engine")))
               (with-output-to-file "local.conf"
                 (lambda _
                   (format #t "
PREFIX=~a
ENGINE_LIB_PATH=~a/lib
ENGINE_INC_PATH=~a/include
" out engine engine))))
             #t)))))
    (inputs
     (list pep-engine))
    (home-page "https://pep.foundation/")
    (synopsis "Library for building p≡p adapters")
    (description "This C++ library provides common structures used in p≡p
(pretty Easy privacy) adapters.")
    (license license:bsd-3)))

(define-public python-pep-adapter
  (package
    (name "python-pep-adapter")
    (version "2.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.pep.foundation/pEp.foundation/pEpPythonAdapter")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ssz21p1k7xx3snmd4g3ggzq565rlzdmp90l2mph6yfp1y65p39s"))))
    (build-system python-build-system)
    (arguments
     `(;; Adding configure-flags does not work, running `build_ext`
       ;; with these flags, neither does adding the options to
       ;; `setup.cfg`: Either `build` or `install` fails (since
       ;; flags are given or missing), or "command 'BuildExtCommand'
       ;; has no such option 'pep_engine"
       ;; '(#:configure-flags
       ;;          (list (string-append "--with-pEp-engine="
       ;;                           (assoc-ref inputs "pEpEngine"))
       ;;                (string-append "--with-pEp-libadapter="
       ;;                           (assoc-ref inputs "libpEpAdapter"))
       ;;                ;;(string-append "--boost="
       ;;                           (assoc-ref inputs "boost")) not supported
       ;;                (string-append "--with-asn1c-share="
       ;;                           (assoc-ref inputs "asn1c") "/share"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-setup.py
           (lambda _
             (substitute* "setup.py"
               (("^(\\s+SYS_INCLUDES = )\\['/usr.*" _ a)
                (string-append a "os.getenv('CPATH', '').split(os.pathsep)\n"))
               (("^(\\s+SYS_LIB_PREFIXES = )\\['/usr.*" _ a)
                (string-append a "os.getenv('LIBRARY_PATH', '').split(os.pathsep)\n"))
               (("^(\\s+SYS_SHARES = )\\['/usr.*" _ a)
                (string-append a "['" (assoc-ref %build-inputs "asn1c") "/share']\n")))
             #t))
         (add-before 'build 'remove-wheel-requirement
           ;; we dont't build a wheel
           (lambda _
             (substitute* "setup.cfg"
               ((" wheel *>= [0-9.]*") ""))
             (substitute* "pyproject.toml"
               (("\"wheel *>=.*\"") ""))
             #t)))))
    (native-inputs
     (list python-pytest python-pytest-forked))
    (inputs
     (list boost libpepadapter pep-engine python-setuptools-scm
           `(,util-linux "lib"))) ;; uuid.h
    (home-page "https://pep.foundation/")
    (synopsis "Python adapter for p≡p (pretty Easy Privacy)")
    (description "The p≡p Python adapter is an adaptor interface to the p≡p
(pretty Easy privacy) engine.")
    (license license:gpl3)))

(define-public java-pep-adapter
  (package
    (name "java-pep-adapter")
    (version "2.1.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.pep.foundation/pEp.foundation/pEpJNIAdapter")
             (commit (string-append "Release_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sw3a5ggxcrkghvpp0a6h2lz461x55ldgfw5y4pw7c3gk5wynvjk"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:test-target "test"
       #:make-flags '("NDEBUG=1" "all" "doc") ; release build
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pin-shared-lib-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/java/foundation/pEp/jniadapter/AbstractEngine.java"
               (("System.loadLibrary\\(\"pEpJNI\"\\);")
                (string-append "System.load(\""
                               (assoc-ref outputs "out")
                               "/lib/libpEpJNI.so" "\");")))
             #t))
         (replace 'configure
           ;; pEpJNIAdapter does not use autotools and configure,
           ;; but a local.conf. We need to tweak the values there.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (engine (assoc-ref inputs "pep-engine"))
                   (libadapter (assoc-ref inputs "libpepadapter"))
                   (openjdk  (assoc-ref inputs "openjdk")))
               (with-output-to-file "local.conf"
                 (lambda _
                   (format #t "
PREFIX=~a
ENGINE_LIB_PATH=~a/lib
ENGINE_INC_PATH=~a/include
AD_LIB_PATH=~a/lib
AD_INC_PATH=~a/include
YML2_PROC=~a
JAVA_HOME=~a
"
                           out engine engine libadapter libadapter
                           (which "yml2proc") openjdk)))
               (substitute* "src/Makefile"  ;; suppress some warnings
                 (("^\\s+OLD_JAVA=") "    xxx_OLD_JAVA="))
               #t)))
         (add-before 'build 'build-codegen
           ;; run codegen first to allow parallel build of other parts
           (lambda _
             (invoke "make" "-C" "src" "create-dirs" "codegen")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libout (string-append out "/lib/"))
                    (javaout (string-append out "/share/java/")))
               (install-file "dist/libpEpJNI.a" libout)
               (install-file "dist/libpEpJNI.so" libout)
               (install-file "dist/pEp.jar" javaout)
               #t)))
         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "doc"))
                    (docout (string-append out "/share/doc/pEp-JNI-adapter"))
                    (cxxout (string-append docout "/cxx"))
                    (javaout (string-append docout "/java")))
               (mkdir-p cxxout)
               (mkdir-p javaout)
               (copy-recursively "doc/doxygen/cxx/html" cxxout)
               (copy-recursively "doc/doxygen/java/html" javaout)
               #t))))))
    (native-inputs
     (list doxygen graphviz
           `(,openjdk9 "jdk") which yml2))
    (inputs
     (list libpepadapter pep-engine
           `(,util-linux "lib"))) ;; uuid.h
    (home-page "https://pep.foundation/")
    (synopsis "Java adapter for p≡p (pretty Easy Privacy)")
    (description "The p≡p JNI adapter is a Java adapter interface to the p≡p
(pretty Easy privacy) engine.")
    (license license:gpl3)))
