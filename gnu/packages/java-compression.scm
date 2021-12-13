;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages java-compression)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python-compression))

(define-public java-snappy
  (package
    (name "java-snappy")
    (version "1.1.7.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xerial/snappy-java")
             (commit version)))
       (sha256
        (base32 "0894zyasrmbi268d1ky9db16wrnc6x8b9ilq0b5paaxi2pwgjlrp"))
       (file-name (git-file-name name version))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "snappy.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-before 'build 'remove-binaries
           (lambda _
             (delete-file "lib/org/xerial/snappy/OSInfo.class")
             (delete-file-recursively "src/main/resources/org/xerial/snappy/native")
             #t))
         (add-before 'build 'build-jni
           (lambda _
             ;; Rebuild one of the binaries we removed earlier
             (invoke "javac" "src/main/java/org/xerial/snappy/OSInfo.java"
                      "-d" "lib")
             ;; Link to the dynamic bitshuffle and snappy, not the static ones
             (substitute* "Makefile.common"
               (("-shared")
                "-shared -lbitshuffle -lsnappy"))
             (substitute* "Makefile"
               ;; Don't try to use git, don't download bitshuffle source
               ;; and don't build it.
               (("\\$\\(SNAPPY_GIT_UNPACKED\\) ")
                "")
               ((": \\$\\(SNAPPY_GIT_UNPACKED\\)")
                ":")
               (("\\$\\(BITSHUFFLE_UNPACKED\\) ")
                "")
               ((": \\$\\(SNAPPY_SOURCE_CONFIGURED\\)") ":")
               ;; What we actually want to build
               (("SNAPPY_OBJ:=.*")
                "SNAPPY_OBJ:=$(addprefix $(SNAPPY_OUT)/, \
                 SnappyNative.o BitShuffleNative.o)\n")
               ;; Since we removed the directory structure in "native" during
               ;; the previous phase, we need to recreate it.
               (("NAME\\): \\$\\(SNAPPY_OBJ\\)")
                "NAME): $(SNAPPY_OBJ)\n\t@mkdir -p $(@D)"))
             ;; Finally we can run the Makefile to build the dynamic library.
             ;; Use the -nocmake target to avoid a dependency on cmake,
             ;; which in turn requires the "git_unpacked" directory.
             (invoke "make" "native-nocmake")))
         ;; Once we have built the shared library, we need to place it in the
         ;; "build" directory so it can be added to the jar file.
         (add-after 'build-jni 'copy-jni
           (lambda _
             (copy-recursively "src/main/resources/org/xerial/snappy/native"
                               "build/classes/org/xerial/snappy/native")
             #t))
         (add-before 'build 'set-test-memory-size
           (lambda _
             (substitute* "build.xml"
               (("<junit printsummary=") "<junit maxmemory=\"2G\" printsummary="))
             #t))
         (add-before 'check 'fix-failing
           (lambda _
             (with-directory-excursion "src/test/java/org/xerial/snappy"
               ;; This package assumes maven build, which puts results in "target".
               ;; We put them in "build" instead, so fix that.
               (substitute* "SnappyLoaderTest.java"
                 (("target/classes") "build/classes"))
               ;; This requires Hadoop, which is not in Guix yet.
               (delete-file "SnappyHadoopCompatibleOutputStreamTest.java"))
             #t)))))
    (inputs
     `(("osgi-framework" ,java-osgi-framework)))
    (propagated-inputs
     `(("bitshuffle" ,bitshuffle-for-snappy)
       ("snappy" ,snappy)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("xerial-core" ,java-xerial-core)
       ("classworlds" ,java-plexus-classworlds)
       ("commons-lang" ,java-commons-lang)
       ("commons-io" ,java-commons-io)
       ("perl" ,perl)))
    (home-page "https://github.com/xerial/snappy-java")
    (synopsis "Compression/decompression algorithm in Java")
    (description "Snappy-java is a Java port of snappy, a fast C++
compressor/decompressor.")
    (license license:asl2.0)))

(define-public java-snappy-1
  (package
    (inherit java-snappy)
    (name "java-snappy")
    (version "1.0.3-rc3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://github.com/xerial/snappy-java"))
             (commit (string-append "snappy-java-" version))))
       (sha256
        (base32 "0gbg3xmhniyh5p6w5zqj16fr15fa8j4raswd8pj00l4ixf5qa6m4"))
       (file-name (git-file-name name version))))
    (arguments
     `(#:jar-name "snappy.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-binaries
           (lambda _
             (delete-file "lib/org/xerial/snappy/OSInfo.class")
             (delete-file-recursively "src/main/resources/org/xerial/snappy/native")
             #t))
         (add-before 'build 'build-jni
           (lambda _
             ;; Rebuild one of the binaries we removed earlier
             (invoke "javac" "src/main/java/org/xerial/snappy/OSInfo.java"
                      "-d" "lib")
             ;; Link to the dynamic snappy, not the static ones
             (substitute* "Makefile.common"
               (("-shared") "-shared -lsnappy"))
             (substitute* "Makefile"
               ;; Don't download the sources here.
               (("\\$\\(SNAPPY_UNPACKED\\) ") "")
               ((": \\$\\(SNAPPY_UNPACKED\\) ") ":")
               ;; What we actually want to build
               (("SNAPPY_OBJ:=.*")
                "SNAPPY_OBJ:=$(addprefix $(SNAPPY_OUT)/, SnappyNative.o)\n")
               ;; Since we removed the directory structure in "native" during
               ;; the previous phase, we need to recreate it.
               (("NAME\\): \\$\\(SNAPPY_OBJ\\)")
                "NAME): $(SNAPPY_OBJ)\n\t@mkdir -p $(@D)"))
             ;; Finally we can run the Makefile to build the dynamic library.
             (invoke "make" "native")))
         ;; Once we have built the shared library, we need to place it in the
         ;; "build" directory so it can be added to the jar file.
         (add-after 'build-jni 'copy-jni
           (lambda _
             (copy-recursively "src/main/resources/org/xerial/snappy/native"
                               "build/classes/org/xerial/snappy/native")
             #t))
         (add-before 'check 'fix-tests
           (lambda _
             (mkdir-p "src/test/resources/org/xerial/snappy/")
             (copy-recursively "src/test/java/org/xerial/snappy/testdata"
                               "src/test/resources/org/xerial/snappy/testdata")
             (install-file "src/test/java/org/xerial/snappy/alice29.txt"
                           "src/test/resources/org/xerial/snappy/")
             #t)))))))

(define-public java-iq80-snappy
  (package
    (name "java-iq80-snappy")
    (version "0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dain/snappy")
             (commit (string-append "snappy-" version))))
       (sha256
        (base32 "1mswh207065rdzbxk6rxaqlxhbg1ngxa0vjc20knsn31kqbq1bcz"))
       (file-name (git-file-name name version))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "iq80-snappy.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (define (test class)
               (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                   ":build/classes"
                                                   ":build/test-classes")
                       "-Dtest.resources.dir=src/test/resources"
                       "org.testng.TestNG" "-testclass"
                       class))
             (invoke "ant" "compile-tests")
             (test "org.iq80.snappy.SnappyFramedStreamTest")
             (test "org.iq80.snappy.SnappyStreamTest")
             #t))
         (add-before 'build 'remove-hadoop-dependency
           (lambda _
             ;; We don't have hadoop
             (delete-file "src/main/java/org/iq80/snappy/HadoopSnappyCodec.java")
             (delete-file "src/test/java/org/iq80/snappy/TestHadoopSnappyCodec.java")
             #t))
         (replace 'install (install-from-pom "pom.xml")))))
    (home-page "https://github.com/dain/snappy")
    (native-inputs
     (list java-guava java-snappy java-testng))
    (synopsis "Java port of the Snappy (de)compressor")
    (description
     "Iq80-snappy is a port of the Snappy compressor and decompressor rewritten
in pure Java.  This compression code produces a byte-for-byte exact copy of the
output created by the original C++ code, and is extremely fast.")
    (license license:asl2.0)))

(define-public java-jbzip2
  (package
    (name "java-jbzip2")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://storage.googleapis.com/"
                                  "google-code-archive-source/v2/"
                                  "code.google.com/jbzip2/"
                                  "source-archive.zip"))
              (file-name (string-append name "-" version ".zip"))
              (sha256
               (base32
                "0ncmhlqmrfmj96nqf6p77b9ws35lcfsvpfxzwxi2asissc83z1l3"))))
    (build-system ant-build-system)
    (native-inputs
     (list unzip java-junit))
    (arguments
     `(#:tests? #f                      ; no tests
       #:jar-name "jbzip2.jar"
       #:source-dir "tags/release-0.9.1/src"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-encoding-problems
           (lambda _
             ;; Some of the files we're patching are
             ;; ISO-8859-1-encoded, so choose it as the default
             ;; encoding so the byte encoding is preserved.
             (with-fluids ((%default-port-encoding #f))
               (substitute* "tags/release-0.9.1/src/org/itadaki/bzip2/HuffmanAllocator.java"
                 (("Milidi.") "Milidiu")))
             #t)))))
    (home-page "https://code.google.com/archive/p/jbzip2/")
    (synopsis "Java bzip2 compression/decompression library")
    (description "Jbzip2 is a Java bzip2 compression/decompression library.
It can be used as a replacement for the Apache @code{CBZip2InputStream} /
@code{CBZip2OutputStream} classes.")
    (license license:expat)))

(define-public java-xz
  (package
    (name "java-xz")
    (version "1.9")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://tukaani.org/xz/xz-java-" version ".zip"))
              (sha256
               (base32
                "007d9f83277qn70swz9inqhyf0qxq6ygajpq5rqg0xgsyh1sdndi"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:jdk ,openjdk9
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'generate-pom
           (lambda _
             (copy-file "maven/pom_template.xml" "pom.xml")
             (substitute* "pom.xml"
               (("@VERSION@") ,version)
               (("@TITLE@") "XZ data compression")
               (("@HOMEPAGE@") "http://tukaani.org/xz/java.html"))
             #t))
        (add-before 'install 'rename-jar
          (lambda _
            (rename-file "build/jar/xz.jar"
                         (string-append "build/jar/xz-" ,version ".jar"))
            #t))
        (replace 'install
          (install-from-pom "pom.xml")))))
    (native-inputs
     (list unzip))
    (home-page "https://tukaani.org")
    (synopsis "XZ in Java")
    (description "Tukaani-xz is an implementation of xz compression/decompression
algorithms in Java.")
    (license license:public-domain)))
