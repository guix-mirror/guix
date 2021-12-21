;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018-2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages maven)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maven-parent-pom)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 match))

(define-public maven-resolver-api
  (package
    (name "maven-resolver-api")
    (version "1.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/apache/maven-resolver")
                    (commit (string-append "maven-resolver-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hbbbxj14qyq8pccyab96pjqq90jnjmid1pml9kx55c5smfpjn37"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-resolver-api.jar"
       #:source-dir "maven-resolver-api/src/main/java"
       #:test-dir "maven-resolver-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "maven-resolver-api/pom.xml")))))
    (native-inputs
     `(("java-asm-8" ,java-asm-8)
       ("java-cglib" ,java-cglib)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-objenesis" ,java-objenesis)))
    (propagated-inputs
     (list maven-resolver-parent-pom))
    (home-page "https://github.com/apache/maven-resolver")
    (synopsis "Maven repository system API")
    (description "This package contains the API for the maven repository system.")
    (license license:asl2.0)))

(define maven-resolver-parent-pom
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'fix-pom
           (lambda _
             (substitute* "pom.xml"
               (("<classifier>no_aop</classifier>") ""))
             #t))
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs '())))

(define-public maven-resolver-spi
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-spi")
    (arguments
     `(#:jar-name "maven-resolver-spi.jar"
       #:source-dir "maven-resolver-spi/src/main/java"
       #:test-dir "maven-resolver-spi/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "maven-resolver-spi/pom.xml")))))
    (propagated-inputs
     (list maven-resolver-api))
    (synopsis "Maven repository system SPI")
    (description "This package contains the service provider interface (SPI)
for repository system implementations and repository connectors.")))

(define-public maven-resolver-test-util
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-test-util")
    (arguments
     `(#:jar-name "maven-resolver-test-util.jar"
       #:source-dir "maven-resolver-test-util/src/main/java"
       #:test-dir "maven-resolver-test-util/src/test"
       #:jdk ,icedtea-8))
    (inputs
     (list maven-resolver-api maven-resolver-spi))
    (synopsis "Utility classes for testing the maven repository system")
    (description "This package contains a collection of utility classes to
ease testing of the repository system.")))

(define-public maven-resolver-util
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-util")
    (arguments
     `(#:jar-name "maven-resolver-util.jar"
       #:source-dir "maven-resolver-util/src/main/java"
       #:test-dir "maven-resolver-util/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "maven-resolver-util/pom.xml")))))
    (propagated-inputs
     (list maven-resolver-api))
    (native-inputs
     (list java-junit java-hamcrest-core maven-resolver-test-util))
    (synopsis "Utility classes for the maven repository system")
    (description "This package contains a collection of utility classes to
ease usage of the repository system.")))

(define-public maven-resolver-connector-basic
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-connector-basic")
    (arguments
     `(#:jar-name "maven-resolver-connector-basic.jar"
       #:source-dir "maven-resolver-connector-basic/src/main/java"
       #:test-dir "maven-resolver-connector-basic/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory\n")))
             #t))
         (replace 'install
           (install-from-pom "maven-resolver-connector-basic/pom.xml")))))
    (propagated-inputs
     (list maven-resolver-api maven-resolver-spi maven-resolver-util
           java-slf4j-api))
    (native-inputs
     (list java-javax-inject java-junit maven-resolver-test-util))
    (synopsis "Maven repository connector implementation")
    (description "This package contains a repository connector implementation
for repositories using URI-based layouts.")))

(define-public maven-resolver-impl
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-impl")
    (arguments
     `(#:jar-name "maven-resolver-impl.jar"
       #:source-dir "maven-resolver-impl/src/main/java"
       #:test-dir "maven-resolver-impl/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display
                   (string-append
                     ;; Build this list by looking for files containing "@Named"
                     "org.eclipse.aether.internal.impl.DefaultArtifactResolver\n"
                     "org.eclipse.aether.internal.impl.collect.DefaultDependencyCollector\n"
                     "org.eclipse.aether.internal.impl.DefaultChecksumPolicyProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultDeployer\n"
                     "org.eclipse.aether.internal.impl.DefaultFileProcessor\n"
                     "org.eclipse.aether.internal.impl.DefaultInstaller\n"
                     "org.eclipse.aether.internal.impl.DefaultLocalRepositoryProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultMetadataResolver\n"
                     "org.eclipse.aether.internal.impl.DefaultOfflineController\n"
                     "org.eclipse.aether.internal.impl.DefaultRemoteRepositoryManager\n"
                     "org.eclipse.aether.internal.impl.DefaultRepositoryConnectorProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultRepositoryEventDispatcher\n"
                     "org.eclipse.aether.internal.impl.DefaultRepositoryLayoutProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultRepositorySystem\n"
                     "org.eclipse.aether.internal.impl.DefaultSyncContextFactory\n"
                     "org.eclipse.aether.internal.impl.DefaultTransporterProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultUpdateCheckManager\n"
                     "org.eclipse.aether.internal.impl.DefaultUpdatePolicyAnalyzer\n"
                     "org.eclipse.aether.internal.impl.EnhancedLocalRepositoryManagerFactory\n"
                     "org.eclipse.aether.internal.impl.LoggerFactoryProvider\n"
                     "org.eclipse.aether.internal.impl.Maven2RepositoryLayoutFactory\n"
                     "org.eclipse.aether.internal.impl.SimpleLocalRepositoryManagerFactory\n"
                     "org.eclipse.aether.internal.impl.slf4j.Slf4jLoggerFactory"))))
             #t))
         (replace 'install
           (install-from-pom "maven-resolver-impl/pom.xml")))))
    (propagated-inputs
     (list maven-resolver-api
           maven-resolver-spi
           maven-resolver-util
           java-commons-lang3
           java-eclipse-sisu-inject
           java-javax-inject
           java-guice
           java-slf4j-api
           maven-resolver-parent-pom))
    (native-inputs
     (list java-junit maven-resolver-test-util))))

(define-public maven-resolver-transport-wagon
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-transport-wagon")
    (arguments
     `(#:jar-name "maven-resolver-transport-wagon.jar"
       #:source-dir "maven-resolver-transport-wagon/src/main/java"
       #:test-dir "maven-resolver-transport-wagon/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.eclipse.aether.transport.wagon.WagonTransporterFactory\n")))
             #t))
         (add-before 'build 'generate-components.xml
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (with-output-to-file "build/classes/META-INF/plexus/components.xml"
               (lambda _
                 (display
                   (string-append
                     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<component-set>
  <components>
    <component>
      <role>org.eclipse.aether.transport.wagon.WagonConfigurator</role>
      <role-hint>plexus</role-hint>
      <implementation>org.eclipse.aether.internal.transport.wagon.PlexusWagonConfigurator</implementation>
      <description />
      <isolated-realm>false</isolated-realm>
      <requirements>
        <requirement>
          <role>org.codehaus.plexus.PlexusContainer</role>
          <role-hint />
          <field-name>container</field-name>
        </requirement>
      </requirements>
    </component>
    <component>
      <role>org.eclipse.aether.transport.wagon.WagonProvider</role>
      <role-hint>plexus</role-hint>
      <implementation>org.eclipse.aether.internal.transport.wagon.PlexusWagonProvider</implementation>
      <description />
      <isolated-realm>false</isolated-realm>
      <requirements>
        <requirement>
          <role>org.codehaus.plexus.PlexusContainer</role>
          <role-hint />
          <field-name>container</field-name>
        </requirement>
      </requirements>
    </component>
  </components>
</component-set>\n"))))
             #t)))))
    (inputs
     `(("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-util" ,maven-resolver-util)
       ("java-javax-inject" ,java-javax-inject)
       ("mavne-wagon-provider-api" ,maven-wagon-provider-api)
       ("java-plexus-component-annotation" ,java-plexus-component-annotations-1.7)
       ("java-plexus-classworld" ,java-plexus-classworlds)
       ("java-plexus-plexus-util" ,java-plexus-utils)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)))
    (native-inputs
     (list java-junit
           java-hamcrest-core
           maven-resolver-test-util
           java-guava
           java-cglib
           java-aopalliance
           java-guice))
    (synopsis "Transport implementation for Maven")
    (description "This package contains a transport implementation based on
Maven Wagon, for use in Maven.")))

(define-public maven-resolver-transport-file
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-transport-file")
    (arguments
     `(#:jar-name "maven-resolver-transport-file.jar"
       #:source-dir "maven-resolver-transport-file/src/main/java"
       #:test-dir "maven-resolver-transport-file/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.eclipse.aether.transport.file.FileTransporterFactory\n"))))))))
    (inputs
     (list java-eclipse-sisu-inject
           java-eclipse-sisu-plexus
           java-javax-inject
           java-plexus-classworlds
           java-plexus-component-annotations
           java-plexus-utils
           java-slf4j-api
           maven-resolver-api
           maven-resolver-spi
           maven-resolver-util
           maven-wagon-provider-api))
    (native-inputs
     (list java-asm
           java-aopalliance
           java-cglib
           java-guava
           java-guice
           java-hamcrest-core
           java-junit
           maven-resolver-test-util))
    (synopsis "Transport implementation for Maven")
    (description "This package contains a transport implementation based on
files, for use in Maven.")))

(define-public maven-resolver-transport-http
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-transport-http")
    (arguments
     `(#:jar-name "maven-resolver-transport-http.jar"
       #:source-dir "maven-resolver-transport-http/src/main/java"
       #:test-dir "maven-resolver-transport-http/src/test"
       #:jdk ,icedtea-8
       ;; Tests all fail because
       ;; org.eclipse.aether.transport.http.SslSocketFactory is not available.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.eclipse.aether.transport.http.HttpTransporterFactory\n"))))))))
    (inputs
     (list java-eclipse-sisu-inject
           java-eclipse-sisu-plexus
           java-javax-inject
           java-plexus-classworlds
           java-plexus-component-annotations
           java-plexus-utils
           java-slf4j-api
           maven-resolver-api
           maven-resolver-spi
           maven-resolver-util
           maven-wagon-provider-api))
    (propagated-inputs
     (list java-httpcomponents-httpclient
           java-httpcomponents-httpcore))
    (native-inputs
     (list java-aopalliance
           java-asm
           java-cglib
           java-eclipse-aether-api
           java-eclipse-jetty-http
           java-eclipse-jetty-io
           java-eclipse-jetty-server
           java-eclipse-jetty-servlet
           java-eclipse-jetty-util
           java-guava
           java-guice
           java-hamcrest-core
           java-javaee-servletapi
           java-junit
           maven-resolver-test-util))
    (synopsis "Transport implementation for Maven")
    (description "This package contains a transport implementation based on
HTTP, for use in Maven.")))

;; aether is the parent project that was forked into maven-resolver.  It used
;; to be used with older versions of Maven, and is still required for some
;; plugins and their dependencies.  This version is required for the plugins,
;; even though there are newer versions of this project.
(define-public java-sonatype-aether-api
  (package
    (name "java-sonatype-aether-api")
    (version "1.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sonatype/sonatype-aether")
                     (commit (string-append "aether-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wn9fv91n40bvlwbzy0dmh0xqibxl2mpzpnbibhqss3c0zlr1ccq"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "aether-api.jar"
       #:source-dir "aether-api/src/main/java"
       #:test-dir "aether-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-parent (install-pom-file "pom.xml"))
         (replace 'install (install-from-pom "aether-api/pom.xml")))))
    (propagated-inputs
     (list java-sonatype-forge-parent-pom-6))
    (native-inputs (list java-junit))
    (home-page "https://github.com/sonatype/sonatype-aether")
    (synopsis "Maven repository system API")
    (description "This package contains the API for the maven repository system.")
    (license license:asl2.0)))

(define-public java-sonatype-aether-spi
  (package
    (inherit java-sonatype-aether-api)
    (name "java-sonatype-aether-spi")
    (arguments
     `(#:jar-name "aether-spi.jar"
       #:source-dir "aether-spi/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-spi/pom.xml")))))
    (propagated-inputs
     (list java-sonatype-aether-api))
    (synopsis "Maven repository system SPI")
    (description "This package contains the service provider interface (SPI)
for repository system implementations and repository connectors.")))

(define-public java-sonatype-aether-test-util
  (package
    (inherit java-sonatype-aether-api)
    (name "java-sonatype-aether-test-util")
    (arguments
     `(#:jar-name "java-sonatype-aether-test-util.jar"
       #:source-dir "aether-test-util/src/main/java"
       #:test-dir "aether-test-util/src/test"))
    (inputs
     (list java-sonatype-aether-api java-sonatype-aether-spi))
    (synopsis "Utility classes for testing the maven repository system")
    (description "This package contains a collection of utility classes to
ease testing of the repository system.")))

(define-public java-sonatype-aether-util
  (package
    (inherit java-sonatype-aether-api)
    (name "java-sonatype-aether-util")
    (arguments
     `(#:jar-name "aether-util.jar"
       #:source-dir "aether-util/src/main/java"
       #:test-dir "aether-util/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-util/pom.xml")))))
    (propagated-inputs
     (list java-sonatype-aether-api))
    (native-inputs
     (list java-junit java-sonatype-aether-test-util))
    (synopsis "Utility classes for the maven repository system")
    (description "This package contains a collection of utility classes to
ease usage of the repository system.")))

(define-public java-sonatype-aether-impl
  (package
    (inherit java-sonatype-aether-api)
    (name "java-sonatype-aether-impl")
    (arguments
     `(#:jar-name "aether-impl.jar"
       #:source-dir "aether-impl/src/main/java"
       #:test-dir "aether-impl/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'fix-pom
           (lambda _
             (substitute* "aether-impl/pom.xml"
               (("org.sonatype.sisu") "org.codehaus.plexus")
               (("sisu-inject-plexus") "plexus-container-default"))
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (replace 'install (install-from-pom "aether-impl/pom.xml")))))
    (propagated-inputs
     (list java-sonatype-aether-api
           java-sonatype-aether-spi
           java-sonatype-aether-util
           java-plexus-component-annotations
           java-plexus-container-default
           java-slf4j-api))
    (native-inputs
     (list java-junit java-plexus-component-metadata
           java-sonatype-aether-test-util))))

;; This slightly newer version is also required by some plugins
(define-public java-sonatype-aether-api-1.13
  (package
    (name "java-sonatype-aether-api")
    (version "1.13.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sonatype/sonatype-aether")
                     (commit (string-append "aether-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yl34dqhm6ykb7h63gkssyrdxv3dsa3n5b8d8cvy8rh4qsm6p2yb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "aether-api.jar"
       #:source-dir "aether-api/src/main/java"
       #:test-dir "aether-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-parent (install-pom-file "pom.xml"))
         (replace 'install (install-from-pom "aether-api/pom.xml")))))
    (propagated-inputs
     `(("java-sonatype-forge-parent-pom" ,java-sonatype-forge-parent-pom-10)))
    (native-inputs `(("java-junit" ,java-junit)))
    (home-page "https://github.com/sonatype/sonatype-aether")
    (synopsis "Maven repository system API")
    (description "This package contains the API for the maven repository system.")
    (license license:asl2.0)))

(define-public java-sonatype-aether-spi-1.13
  (package
    (inherit java-sonatype-aether-api-1.13)
    (name "java-sonatype-aether-spi")
    (arguments
     `(#:jar-name "aether-spi.jar"
       #:source-dir "aether-spi/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-spi/pom.xml")))))
    (propagated-inputs
     `(("java-sonatype-aether-api" ,java-sonatype-aether-api-1.13)))
    (synopsis "Maven repository system SPI")
    (description "This package contains the service provider interface (SPI)
for repository system implementations and repository connectors.")))

(define-public java-sonatype-aether-test-util-1.13
  (package
    (inherit java-sonatype-aether-api-1.13)
    (name "java-sonatype-aether-test-util")
    (arguments
     `(#:jar-name "java-sonatype-aether-test-util.jar"
       #:source-dir "aether-test-util/src/main/java"
       #:test-dir "aether-test-util/src/test"))
    (inputs
     `(("java-sonatype-aether-api" ,java-sonatype-aether-api-1.13)
       ("java-sonatype-aether-spi" ,java-sonatype-aether-spi-1.13)))
    (synopsis "Utility classes for testing the maven repository system")
    (description "This package contains a collection of utility classes to
ease testing of the repository system.")))

(define-public java-sonatype-aether-util-1.13
  (package
    (inherit java-sonatype-aether-api-1.13)
    (name "java-sonatype-aether-util")
    (arguments
     `(#:jar-name "aether-util.jar"
       #:source-dir "aether-util/src/main/java"
       #:test-dir "aether-util/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-util/pom.xml")))))
    (propagated-inputs
     `(("java-sonatype-aether-api" ,java-sonatype-aether-api-1.13)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-sonatype-aether-test-util" ,java-sonatype-aether-test-util-1.13)))
    (synopsis "Utility classes for the maven repository system")
    (description "This package contains a collection of utility classes to
ease usage of the repository system.")))

;; Again, this old version is required by some maven plugins
(define-public java-eclipse-aether-api
  (package
    (name "java-eclipse-aether-api")
    (version "1.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eclipse/aether-core")
                     (commit "aether-1.0.2.v20150114")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14d336nn0kh5ddf23j37va3hd8gaai19llrpxhf4bcc7g7sgdqxs"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "aether-api.jar"
       #:source-dir "aether-api/src/main/java"
       #:test-dir "aether-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-parent (install-pom-file "pom.xml"))
         (replace 'install (install-from-pom "aether-api/pom.xml")))))
    (native-inputs (list java-junit))
    (home-page "https://github.com/sonatype/sonatype-aether")
    (synopsis "Maven repository system API")
    (description "This package contains the API for the maven repository system.")
    (license license:asl2.0)))

(define-public java-eclipse-aether-spi
  (package
    (inherit java-eclipse-aether-api)
    (name "java-eclipse-aether-spi")
    (arguments
     `(#:jar-name "aether-spi.jar"
       #:source-dir "aether-spi/src/main/java"
       #:test-dir "aether-spi/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-spi/pom.xml")))))
    (propagated-inputs
     (list java-eclipse-aether-api))
    (synopsis "Maven repository system SPI")
    (description "This package contains the service provider interface (SPI)
for repository system implementations and repository connectors.")))

(define-public java-eclipse-aether-test-util
  (package
    (inherit java-eclipse-aether-api)
    (name "java-eclipse-aether-test-util")
    (arguments
     `(#:jar-name "aether-test-util.jar"
       #:source-dir "aether-test-util/src/main/java"
       #:test-dir "aether-test-util/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-util/pom.xml")))))
    (propagated-inputs
     (list java-eclipse-aether-api java-eclipse-aether-spi))
    (synopsis "Utility classes for testing the maven repository system")
    (description "This package contains a collection of utility classes to
ease testing of the repository system.")))

(define-public java-eclipse-aether-util
  (package
    (inherit java-eclipse-aether-api)
    (name "java-eclipse-aether-util")
    (arguments
     `(#:jar-name "aether-util.jar"
       #:source-dir "aether-util/src/main/java"
       #:test-dir "aether-util/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-util/pom.xml")))))
    (propagated-inputs
     (list java-eclipse-aether-api))
    (native-inputs
     (list java-eclipse-aether-test-util java-junit))
    (synopsis "Utility classes for the maven repository system")
    (description "This package contains a collection of utility classes to
ease usage of the repository system.")))

(define-public java-eclipse-aether-impl
  (package
    (inherit java-eclipse-aether-api)
    (name "java-eclipse-aether-impl")
    (arguments
     `(#:jar-name "aether-impl.jar"
       #:source-dir "aether-impl/src/main/java"
       #:test-dir "aether-impl/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-impl/pom.xml")))))
    (propagated-inputs
     (list java-eclipse-aether-api
           java-eclipse-aether-spi
           java-eclipse-aether-util
           java-javax-inject
           java-eclipse-sisu-inject
           java-guice
           java-slf4j-api))
    (native-inputs
     (list java-eclipse-aether-test-util java-junit))))

(define-public maven-shared-utils
  (package
    (name "maven-shared-utils")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-shared-utils-" version "-source-release.zip"))
              (sha256
               (base32
                "1kzmj68wwdcznb36hm6kfz57wbavw7g1rp236pz10znkjljn6rf6"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-shared-utils.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-/bin/sh-invocation
           (lambda _
             (substitute* (find-files "src" ".*.java$")
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'check 'remove-cyclic-dep
           (lambda _
             (delete-file
               "src/test/java/org/apache/maven/shared/utils/introspection/ReflectionValueExtractorTest.java")
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     `(("java-jansi" ,java-jansi)
       ("java-commons-io" ,java-commons-io)
       ("java-jsr305" ,java-jsr305)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("maven-parent-pom-30" ,maven-parent-pom-30)))
    (native-inputs
     (list unzip java-junit java-hamcrest-core java-commons-lang3))
    (home-page "https://maven.apache.org/shared/maven-shared-utils/")
    (synopsis "Plexus-util replacement for maven")
    (description "This project aims to be a functional replacement for
plexus-utils in Maven.  It is not a 100% API compatible replacement but a
replacement with improvements.")
    (license license:asl2.0)))

(define-public maven-plugin-annotations
  (package
    (name "maven-plugin-annotations")
    (version "3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/"
                                  "plugin-tools/maven-plugin-tools-" version
                                  "-source-release.zip"))
              (sha256 (base32 "1ryqhs62j5pas93brhf5dsnvp99hxbvssf681yj5rk3r9h24hqm2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-plugin-annotations.jar"
       #:source-dir "maven-plugin-annotations/src/main/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "maven-plugin-annotations/pom.xml")))))
    (propagated-inputs
     (list maven-artifact maven-plugin-tools-parent-pom))
    (native-inputs
     (list unzip))
    (home-page "https://maven.apache.org/plugin-tools/maven-plugin-annotations/")
    (synopsis "Java 5 annotations to use in Mojos")
    (description "This package contains Java 5 annotations for use in Mojos.")
    (license license:asl2.0)))

(define maven-plugin-tools-parent-pom
  (package
    (inherit maven-plugin-annotations)
    (name "maven-plugin-tools-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs '())))

(define-public maven-wagon-provider-api
  (package
    (name "maven-wagon-provider-api")
    (version "3.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/wagon/"
                                  "wagon-" version "-source-release.zip"))
              (sha256
               (base32
                "1rnviw0yr4g5902fb8pkd1gyvci4bz7hndjvhkqmnkj7ay0y6mf0"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-wagon-provider-api.jar"
       #:source-dir "wagon-provider-api/src/main/java"
       #:test-dir "wagon-provider-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "wagon-provider-api/pom.xml")))))
    (propagated-inputs
     `(("java-plexus-utils" ,java-plexus-utils)
       ("maven-wagon-parent-pom" ,maven-wagon-parent-pom)))
    (native-inputs
     (list unzip java-junit java-easymock))
    (home-page "https://maven.apache.org/wagon")
    (synopsis "Transport abstraction for Maven")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.")
    (license license:asl2.0)))

(define maven-wagon-parent-pom
  (package
    (inherit maven-wagon-provider-api)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     `(("maven-parent-pom-33" ,maven-parent-pom-33)))
    (native-inputs
     `(("unzip" ,unzip)))))

(define-public maven-wagon-provider-test
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-provider-test")
    (arguments
     `(#:jar-name "maven-wagon-provider-test.jar"
       #:source-dir "wagon-provider-test/src/main/java"
       #:tests? #f; no tests
       #:jdk ,icedtea-8))
    (inputs
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-eclipse-jetty-http-9.2" ,java-eclipse-jetty-http-9.2)
       ("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ("java-eclipse-jetty-security-9.2" ,java-eclipse-jetty-security-9.2)
       ("java-eclipse-jetty-server-9.2" ,java-eclipse-jetty-server-9.2)
       ("java-eclipse-jetty-servlet-9.2" ,java-eclipse-jetty-servlet-9.2)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-javaee-servletapi" ,java-javaee-servletapi)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)))
    (synopsis "Test classes from maven-wagon")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.  This package contains common test
classes used in multiple maven-wagon components.")))

(define-public maven-wagon-file
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-file")
    (arguments
     `(#:jar-name "maven-wagon-file.jar"
       #:source-dir "wagon-providers/wagon-file/src/main/java"
       #:test-dir "wagon-providers/wagon-file/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-paths
           (lambda _
             ;; Tests assume they are run by maven, which copies test resources
             ;; to target.  Our ant-build-system does the same, but with the
             ;; build directory.
             (substitute* "wagon-providers/wagon-file/src/test/java/org/apache/maven/wagon/providers/file/FileWagonTest.java"
               (("target") "build"))
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "wagon-providers/wagon-file/src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t)))))
    (inputs
     (list java-plexus-utils maven-wagon-provider-api))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata-1.7)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations-1.7)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-javax-inject" ,java-javax-inject)
       ("java-cglib" ,java-cglib)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ,@(package-native-inputs maven-wagon-provider-api)))
    (synopsis "Wagon provider that gets and puts artifacts using the file system")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.  It uses providers, that are tools to
manage artifacts and deployment.  This package contains the file provider which
gets and puts artifacts using the file system.")))

(define-public maven-wagon-tck-http
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-tck-http")
    (arguments
     `(#:jar-name "maven-wagon-tck-http.jar"
       #:source-dir "wagon-tcks/wagon-tck-http/src/main/java"
       #:tests? #f; no tests
       #:jdk ,icedtea-8))
    (inputs
     `(("java-plexus-util" ,java-plexus-utils)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)
       ("java-javaee-servletapi" ,java-javaee-servletapi)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-commons-codec" ,java-commons-codec)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ("java-eclipse-jetty-webapp-9.2" ,java-eclipse-jetty-webapp-9.2)
       ("java-eclipse-jetty-security-9.2" ,java-eclipse-jetty-security-9.2)
       ("java-eclipse-jetty-server-9.2" ,java-eclipse-jetty-server-9.2)
       ("java-eclipse-jetty-servlet-9.2" ,java-eclipse-jetty-servlet-9.2)))
    (synopsis "Wagon HTTP Test Compatibility Kit")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.  This package contains the HTTP
Test Compatibility Kit.")))

(define-public maven-wagon-http-shared
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-http-shared")
    (arguments
     `(#:jar-name "maven-wagon-http-shared.jar"
       #:source-dir "wagon-providers/wagon-http-shared/src/main/java"
       #:test-dir "wagon-providers/wagon-http-shared/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "wagon-providers/wagon-http-shared/src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t)))))
    (inputs
     (list java-plexus-utils
           java-httpcomponents-httpclient
           java-httpcomponents-httpcore
           java-commons-io
           java-jsoup
           maven-wagon-provider-api))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata-1.7)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations-1.7)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-javax-inject" ,java-javax-inject)
       ("java-cglib" ,java-cglib)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ,@(package-native-inputs maven-wagon-provider-api)))
    (synopsis "Shared Library for wagon providers supporting HTTP")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.  It uses providers, that are tools to
manage artifacts and deployment.  This package contains a shared library for
wagon providers supporting HTTP.")))

(define-public maven-wagon-http
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-http")
    (arguments
     `(#:jar-name "maven-wagon-http.jar"
       #:source-dir "wagon-providers/wagon-http/src/main/java"
       #:test-dir "wagon-providers/wagon-http/src/test"
       #:test-exclude (list
                        "**/Abstract*.java"
                        ;; FIXME: javax.net.ssl.SSLHandshakeException:
                        ;; sun.security.validator.ValidatorException:
                        ;; PKIX path building failed:
                        ;; sun.security.provider.certpath.SunCertPathBuilderException:
                        ;; unable to find valid certification path to requested target
                        "**/HttpsWagonPreemptiveTest.java"
                        "**/HttpsWagonTest.java"
                        ;; Timeout
                        "**/HugeFileDownloadTest.java"
                        ;; Injection errors
                        "**/TckTest.java")
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (install-file "wagon-providers/wagon-http/src/main/resources/META-INF/plexus/components.xml"
                           "build/classes/META-INF/plexus")
             #t))
         (add-before 'check 'fix-resource-path
           (lambda _
             (substitute* '("wagon-providers/wagon-http/src/test/java/org/apache/maven/wagon/providers/http/HttpsWagonPreemptiveTest.java"
                            "wagon-providers/wagon-http/src/test/java/org/apache/maven/wagon/providers/http/HttpsWagonTest.java")
               (("src/test") "wagon-providers/wagon-http/src/test"))
             #t)))))
    (inputs
     (list java-plexus-utils
           java-httpcomponents-httpclient
           java-httpcomponents-httpcore
           maven-wagon-http-shared
           maven-wagon-tck-http
           maven-wagon-provider-api))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata-1.7)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations-1.7)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-inject" ,java-javax-inject)
       ("java-cglib" ,java-cglib)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-javaee-servletapi" ,java-javaee-servletapi)
       ("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ("java-eclipse-jetty-io-9.2" ,java-eclipse-jetty-io-9.2)
       ("java-eclipse-jetty-http-9.2" ,java-eclipse-jetty-http-9.2)
       ("java-eclipse-jetty-server-9.2" ,java-eclipse-jetty-server-9.2)
       ("java-eclipse-jetty-servlet-9.2" ,java-eclipse-jetty-servlet-9.2)
       ("java-eclipse-jetty-security-9.2" ,java-eclipse-jetty-security-9.2)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-codec" ,java-commons-codec)
       ("java-commons-io" ,java-commons-io)
       ("java-jsoup" ,java-jsoup)
       ("java-slf4j-simple" ,java-slf4j-simple)
       ,@(package-native-inputs maven-wagon-provider-api)))
    (synopsis "Wagon provider that gets and puts artifacts through HTTP(S)")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.  It uses providers, that are tools to
manage artifacts and deployment.  This package contains a Wagon provider that
gets and puts artifacts through HTTP(S) using Apache HttpClient-4.x.")))

(define maven-pom
  (package
    (name "maven-pom")
    (version "3.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/"
                                  "maven-3/" version "/source/"
                                  "apache-maven-" version "-src.tar.gz"))
              (sha256 (base32 "16xbhkhhp05gskgbhrf1ia8riivvkhpk822n9xgnad61f9hzp2r9"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "." "\\.jar$"))
                  #t))
              (patches
                (search-patches "maven-generate-component-xml.patch"
                                "maven-generate-javax-inject-named.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'fix-dependencies
           (lambda _
             (substitute* "pom.xml"
               (("classWorldsVersion>.*")
                (string-append
                  "classWorldsVersion>"
                  ,(package-version java-plexus-classworlds)
                  "</classWorldsVersion>\n"))
               (("commonsCliVersion>.*")
                (string-append
                  "commonsCliVersion>"
                  ,(package-version java-commons-cli)
                  "</commonsCliVersion>\n"))
               (("commonsLangVersion>.*")
                (string-append
                  "commonsLangVersion>"
                  ,(package-version java-commons-lang3)
                  "</commonsLangVersion>\n"))
               (("plexusUtilsVersion>.*")
                (string-append
                  "plexusUtilsVersion>"
                  ,(package-version java-plexus-utils)
                  "</plexusUtilsVersion>\n"))
               (("plexusInterpolationVersion>.*")
                (string-append
                  "plexusInterpolationVersion>"
                  ,(package-version java-plexus-interpolation)
                  "</plexusInterpolationVersion>\n"))
               (("guiceVersion>.*")
                (string-append
                  "guiceVersion>"
                  ,(package-version java-guice)
                  "</guiceVersion>\n"))
               (("sisuInjectVersion>.*")
                (string-append
                  "sisuInjectVersion>"
                  ,(package-version java-eclipse-sisu-inject)
                  "</sisuInjectVersion>\n"))
               (("securityDispatcherVersion>.*")
                (string-append
                  "securityDispatcherVersion>"
                  ,(package-version java-plexus-sec-dispatcher)
                  "</securityDispatcherVersion>\n"))
               (("cipherVersion>.*")
                (string-append
                  "cipherVersion>"
                  ,(package-version java-plexus-cipher)
                  "</cipherVersion>\n"))
               (("slf4jVersion>.*")
                (string-append
                  "slf4jVersion>"
                  ,(package-version java-slf4j-api)
                  "</slf4jVersion>\n"))
               (("<classifier>no_aop</classifier>") ""))
             #t))
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     `(("maven-parent-pom-34" ,maven-parent-pom-34)))
    (home-page "https://maven.apache.org/")
    (synopsis "Build system")
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains the Maven pom file, used by all maven components.")
    (license license:asl2.0)))

(define-public maven-artifact
  (package
    (inherit maven-pom)
    (name "maven-artifact")
    (arguments
     `(#:jar-name "maven-artifact.jar"
       #:source-dir "maven-artifact/src/main/java"
       #:test-dir "maven-artifact/src/test"
       #:main-class "org.apache.maven.artifact.versioning.ComparableVersion"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "maven-artifact/pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils java-commons-lang3 maven-pom))
    (native-inputs
     (list java-junit))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains the Maven Artifact classes, providing the
@code{Artifact} interface, with its @code{DefaultArtifact} implementation.  The
jar file is executable and provides a little tool to display how Maven parses
and compares versions:")))

(define-public maven-model
  (package
    (inherit maven-artifact)
    (name "maven-model")
    (arguments
     `(#:jar-name "maven-model.jar"
       #:source-dir "maven-model/src/main/java"
       #:test-dir "maven-model/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-model/src/main/java" version
                       "false" "true"))
             (let ((file "maven-model/src/main/mdo/maven.mdo"))
               (modello-single-mode file "4.0.0" "java")
               (modello-single-mode file "4.0.0" "xpp3-reader")
               (modello-single-mode file "4.0.0" "xpp3-extended-reader")
               (modello-single-mode file "4.0.0" "xpp3-writer")
               (modello-single-mode file "4.0.0" "xpp3-extended-writer"))
             #t))
         (replace 'install (install-from-pom "maven-model/pom.xml")))))
    (propagated-inputs
     (list java-commons-lang3 java-plexus-utils maven-pom))
    (native-inputs
     (list java-modello-core
           ;; for modello:
           java-eclipse-sisu-plexus
           java-plexus-component-annotations
           java-guice
           java-cglib
           java-asm
           java-eclipse-sisu-inject
           java-javax-inject
           java-plexus-classworlds
           java-guava
           java-geronimo-xbean-reflect
           java-plexus-build-api
           ;; modello plugins:
           java-modello-plugins-java
           java-modello-plugins-xml
           java-modello-plugins-xpp3
           ;; for tests
           java-junit))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains the model for Maven @dfn{POM} (Project Object Model),
so really just plain Java objects.")))

(define-public maven-builder-support
  (package
    (inherit maven-artifact)
    (name "maven-builder-support")
    (arguments
     `(#:jar-name "maven-builder-support.jar"
       #:source-dir "maven-builder-support/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-builder-support/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-paths
           (lambda _
             (with-directory-excursion "maven-builder-support/src/test/java"
               (substitute*
                 '("org/apache/maven/building/FileSourceTest.java"
                   "org/apache/maven/building/UrlSourceTest.java")
                 (("target/test-classes") "maven-builder-support/src/test/resources")))
             #t))
         (replace 'install (install-from-pom "maven-builder-support/pom.xml")))))
    (propagated-inputs
     (list maven-pom))
    (native-inputs
     (list java-junit))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains a support library for descriptor builders (model,
setting, toolchains)")))

(define-public maven-settings
  (package
    (inherit maven-artifact)
    (name "maven-settings")
    (arguments
     `(#:jar-name "maven-settings.jar"
       #:source-dir "maven-settings/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-settings/src/main/java" version
                       "false" "true"))
             (let ((file "maven-settings/src/main/mdo/settings.mdo"))
               (modello-single-mode file "1.2.0" "java")
               (modello-single-mode file "1.2.0" "xpp3-reader")
               (modello-single-mode file "1.2.0" "xpp3-writer"))
             #t))
         (replace 'install (install-from-pom "maven-settings/pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils maven-pom))
    (native-inputs
     (list java-modello-core
           ;; for modello:
           ;("container" ,java-plexus-container-default)
           java-eclipse-sisu-plexus
           java-plexus-component-annotations
           java-guice
           java-cglib
           java-eclipse-sisu-inject
           java-javax-inject
           java-plexus-classworlds
           java-plexus-utils
           java-guava
           java-geronimo-xbean-reflect
           java-plexus-build-api
           ;; modello plugins:
           java-modello-plugins-java
           java-modello-plugins-xml
           java-modello-plugins-xpp3))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains strictly the model for Maven settings, that is
simply plain java objects.")))

(define-public maven-settings-builder
  (package
    (inherit maven-artifact)
    (name "maven-settings-builder")
    (arguments
     `(#:jar-name "maven-settings-builder.jar"
       #:source-dir "maven-settings-builder/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-settings-builder/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu-named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (chmod "sisu.sh" #o755)
             (invoke "./sisu.sh" "maven-settings-builder/src/main/java"
                     "build/classes/META-INF/sisu/javax.inject.Named")))
         (replace 'install (install-from-pom "maven-settings-builder/pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils
           java-plexus-interpolation
           java-plexus-sec-dispatcher
           maven-builder-support
           maven-settings
           maven-pom))
    (native-inputs
     (list java-junit java-javax-inject java-plexus-component-annotations))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains the effective model builder, with profile activation,
inheritance, interpolation, @dots{}")))

(define-public maven-model-builder
  (package
    (inherit maven-artifact)
    (name "maven-model-builder")
    (arguments
     `(#:jar-name "maven-model-builder.jar"
       #:source-dir "maven-model-builder/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-model-builder/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "maven-model-builder/src/main/resources"
                               "build/classes")
             #t))
         (add-before 'build 'generate-sisu-named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (chmod "sisu.sh" #o755)
             (invoke "./sisu.sh" "maven-model-builder/src/main/java"
                     "build/classes/META-INF/sisu/javax.inject.Named")))
         (add-before 'check 'fix-paths
           (lambda _
             (substitute* (find-files "maven-model-builder/src/test/java" ".*.java")
               (("src/test") "maven-model-builder/src/test"))
             #t))
         (replace 'install
           (install-from-pom "maven-model-builder/pom.xml")))))
    (propagated-inputs
     (list java-plexus-interpolation
           java-plexus-utils
           maven-artifact
           maven-builder-support
           maven-model
           maven-pom))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-guava" ,java-guava)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-powermock-reflect" ,java-powermock-reflect)
       ("java-objenesis" ,java-objenesis)
       ("guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("javax-inject" ,java-javax-inject)
       ("java-xmlunit" ,java-xmlunit)
       ("java-xmlunit-matchers" ,java-xmlunit-matchers)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("classworlds" ,java-plexus-classworlds)))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains the effective model builder, with profile activation,
inheritance, interpolation, @dots{}")))

(define-public maven-repository-metadata
  (package
    (inherit maven-artifact)
    (name "maven-repository-metadata")
    (arguments
     `(#:jar-name "maven-repository-metadata.jar"
       #:source-dir "maven-repository-metadata/src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-repository-metadata/src/main/java" version
                       "false" "true"))
             (let ((file "maven-repository-metadata/src/main/mdo/metadata.mdo"))
               (modello-single-mode file "1.1.0" "java")
               (modello-single-mode file "1.1.0" "xpp3-reader")
               (modello-single-mode file "1.1.0" "xpp3-writer"))
             #t))
         (replace 'install
           (install-from-pom "maven-repository-metadata/pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils maven-pom))
    (native-inputs
     `(("modello" ,java-modello-core)
       ;; for modello:
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-plexus-build-api" ,java-plexus-build-api)
       ;; modello plugins:
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains strictly the model for Maven Repository Metadata,
so really just plain objects.")))

(define-public maven-resolver-provider
  (package
    (inherit maven-artifact)
    (name "maven-resolver-provider")
    (arguments
     `(#:jar-name "maven-resolver-provider.jar"
       #:source-dir "maven-resolver-provider/src/main/java"
       #:test-dir "maven-resolver-provider/src/test"
       #:jdk ,icedtea-8
       #:tests? #f; dependency loop on maven-core (@Component RepositorySystem)
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu-named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (chmod "./sisu.sh" #o755)
             (invoke "./sisu.sh" "maven-resolver-provider/src/main/java"
                     "build/classes/META-INF/sisu/javax.inject.Named")
             #t))
         (replace 'install
           (install-from-pom "maven-resolver-provider/pom.xml")))))
    (propagated-inputs
     (list maven-model
           maven-model-builder
           maven-resolver-spi
           maven-resolver-api
           maven-resolver-impl
           maven-resolver-util
           maven-builder-support
           maven-repository-metadata
           java-plexus-utils
           java-plexus-component-annotations
           java-guice
           java-javax-inject))))

(define-public maven-plugin-api
  (package
    (inherit maven-artifact)
    (name "maven-plugin-api")
    (arguments
     `(#:jar-name "maven-plugin-api.jar"
       #:source-dir "maven-plugin-api/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-plugin-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-plugin-api/src/main/java" version
                       "false" "true"))
             (let ((file "maven-plugin-api/src/main/mdo/lifecycle.mdo"))
               (modello-single-mode file "1.0.0" "java")
               (modello-single-mode file "1.0.0" "xpp3-reader")
               (modello-single-mode file "1.0.0" "xpp3-writer"))
             #t))
         (replace 'install
           (install-from-pom "maven-plugin-api/pom.xml")))))
    (propagated-inputs
     `(("maven-artifact" ,maven-artifact)
       ("maven-model" ,maven-model)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("javax-inject" ,java-javax-inject)
       ("utils" ,java-plexus-utils)))
    (native-inputs
     `(("modello" ,java-modello-core)
       ;; for modello:
       ("classworlds" ,java-plexus-classworlds)
       ("guava" ,java-guava)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("build-api" ,java-plexus-build-api)
       ;; modello plugins:
       ("java" ,java-modello-plugins-java)
       ("xml" ,java-modello-plugins-xml)
       ("xpp3" ,java-modello-plugins-xpp3)
       ;; for tests
       ("java-junit" ,java-junit)))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains strictly the API for plugins -- composed of goals
implemented by Mojos -- development.

A plugin is described in a @file{META-INF/maven/plugin.xml} plugin descriptor,
generally generated from plugin sources using maven-plugin-plugin.")))

(define-public maven-core-bootstrap
  (hidden-package
    (package
      (inherit maven-artifact)
      (name "maven-core")
      (arguments
       `(#:jar-name "maven-core.jar"
         #:source-dir "src/main/java"
         #:jdk ,icedtea-8
         ;; Tests need maven-compat, which requires maven-core
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'chdir
             (lambda _
               ;; Required for generating components.xml in maven-core
               (chdir "maven-core")
               #t))
           (add-before 'build 'copy-resources
             (lambda _
               (mkdir-p "build/classes/")
               (copy-recursively "src/main/resources" "build/classes")
               #t))
           (add-after 'copy-resources 'fill-properties
             (lambda _
               ;; This file controls the output of some mvn subcommands, such as
               ;; mvn -version.
               (substitute* "build/classes/org/apache/maven/messages/build.properties"
                 (("\\$\\{buildNumber\\}") "guix_build")
                 (("\\$\\{timestamp\\}") "0")
                 (("\\$\\{project.version\\}") ,(package-version maven-artifact))
                 (("\\$\\{distributionId\\}") "apache-maven")
                 (("\\$\\{distributionShortName\\}") "Maven")
                 (("\\$\\{distributionName\\}") "Apache Maven"))
               #t))
           (add-before 'build 'generate-sisu-named
             (lambda _
               (mkdir-p "build/classes/META-INF/sisu")
               (chmod "../sisu.sh" #o755)
               (invoke "../sisu.sh" "src/main/java"
                       "build/classes/META-INF/sisu/javax.inject.Named")
               #t))
           (add-before 'build 'generate-models
             (lambda* (#:key inputs #:allow-other-keys)
               (define (modello-single-mode file version mode)
                 (invoke "java" "org.codehaus.modello.ModelloCli"
                         file mode "src/main/java" version
                         "false" "true"))
               (let ((file "src/main/mdo/toolchains.mdo"))
                 (modello-single-mode file "1.1.0" "java")
                 (modello-single-mode file "1.1.0" "xpp3-reader")
                 (modello-single-mode file "1.1.0" "xpp3-writer"))
               #t))
           (add-before 'install 'fix-pom
             (lambda _
               (substitute* "pom.xml"
                 (("<classifier>no_aop</classifier>") ""))
               #t))
           (replace 'install
             (install-from-pom "pom.xml")))))
      (propagated-inputs
       `(("maven-artifact" ,maven-artifact)
         ("maven-resolver-provider" ,maven-resolver-provider)
         ("maven-builder-support" ,maven-builder-support)
         ("maven-model" ,maven-model)
         ("maven-model-builder" ,maven-model-builder)
         ("maven-settings" ,maven-settings)
         ("maven-settings-builder" ,maven-settings-builder)
         ("maven-plugin-api" ,maven-plugin-api)
         ("maven-repository-metadata" ,maven-repository-metadata)
         ("maven-shared-utils" ,maven-shared-utils)
         ("java-plexus-component-annotations" ,java-plexus-component-annotations-1.7)
         ("java-plexus-utils" ,java-plexus-utils)
         ("java-commons-lang3" ,java-commons-lang3)
         ("java-guava" ,java-guava)
         ("java-guice" ,java-guice)
         ("maven-resolver-api" ,maven-resolver-api)
         ("maven-resolver-spi" ,maven-resolver-spi)
         ("maven-resolver-util" ,maven-resolver-util)
         ("maven-resolver-impl" ,maven-resolver-impl)
         ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
         ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
         ("java-javax-inject" ,java-javax-inject)
         ("java-plexus-classworld" ,java-plexus-classworlds)))
      (native-inputs
       `(("java-modello-core" ,java-modello-core)
         ("java-cglib" ,java-cglib)
         ("java-plexus-classworlds" ,java-plexus-classworlds)
         ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
         ("java-plexus-build-api" ,java-plexus-build-api)
         ("java-modello-plugins-java" ,java-modello-plugins-java)
         ("java-modello-plugins-xml" ,java-modello-plugins-xml)
         ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
         ;; tests
         ("java-junit" ,java-junit)
         ("java-mockito-1" ,java-mockito-1)
         ("java-commons-jxpath" ,java-commons-jxpath)))
      (description "Apache Maven is a software project management and comprehension
tool.  This package contains the maven core classes managing the whole build
process."))))

(define-public maven-core
  (package
    (inherit maven-core-bootstrap)
    (arguments
      (substitute-keyword-arguments (package-arguments maven-core-bootstrap)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'modify-metainf
              (lambda _
                (substitute* "build.xml"
                  (("message=\"\"")
                   (string-append "message=\"Implementation-Version: "
                                  (package-version maven) "\n\"")))
                #t))
            (add-before 'build 'add-maven-files
              (lambda _
                (mkdir-p "build/classes/META-INF/maven/org.apache.maven/maven-core")
                (copy-file "pom.xml"
                           "build/classes/META-INF/maven/org.apache.maven/maven-core/pom.xml")
                (with-output-to-file "build/classes/META-INF/maven/org.apache.maven/maven-core/pom.properties"
                  (lambda _
                    (format #t "version=~a~%
groupId=org.apache.maven~%
artifactId=maven-core" ,(package-version maven-core-bootstrap))))
                #t))
            (add-after 'build 'generate-metadata
              (lambda _
                (define (components file)
                  (let ((sxml (with-input-from-file file
                                (lambda _ (xml->sxml (current-input-port)
                                                     #:trim-whitespace? #t)))))
                    ;; Select the list of <component>s inside the <component-set>
                    ;; and <components>.
                    ((@ (ice-9 match) match) sxml
                     (('*TOP*
                       ('*PI* foo ...)
                       ('component-set
                        ('components x ...))) x))))
                (use-modules (sxml simple))
                (delete-file "build/classes/META-INF/plexus/components.xml")
                (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                        "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                        "--source" "build/classes/META-INF/plexus"
                        "--output" "build/classes/META-INF/plexus/components.t.xml"
                        "--classes" "build/classes"
                        "--descriptors" "build/classes")
                ;; Now we merge all other components from hand-written xml
                (let ((generated-xml (components "build/classes/META-INF/plexus/components.t.xml"))
                      (components-xml (components "src/main/resources/META-INF/plexus/components.xml"))
                      (default-bindings-xml (components "src/main/resources/META-INF/plexus/default-bindings.xml"))
                      (artifact-handlers-xml (components "src/main/resources/META-INF/plexus/artifact-handlers.xml")))
                  (with-output-to-file "build/classes/META-INF/plexus/components.xml"
                    (lambda _
                      (display "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
                      (sxml->xml
                        `(component-set
                           (components
                             ,@(append generated-xml components-xml
                                       default-bindings-xml
                                       artifact-handlers-xml)))))))
                #t))
            (add-after 'generate-metadata 'fix-plugin-versions
              (lambda _
                ;; This file controls the default plugins used by Maven.  Ensure
                ;; we use the versions we have packaged by default
                (substitute* '("build/classes/META-INF/plexus/default-bindings.xml"
                               "build/classes/META-INF/plexus/components.xml")
                  (("maven-install-plugin:[0-9.]+")
                   (string-append "maven-install-plugin:"
                                  ,(package-version maven-install-plugin)))
                  (("maven-resources-plugin:[0-9.]+")
                   (string-append "maven-resources-plugin:"
                                  ,(package-version maven-resources-plugin)))
                  (("maven-compiler-plugin:[0-9.]+")
                   (string-append "maven-compiler-plugin:"
                                  ,(package-version maven-compiler-plugin)))
                  (("maven-surefire-plugin:[0-9.]+")
                   (string-append "maven-surefire-plugin:"
                                  ,(package-version maven-surefire-plugin)))
                  (("maven-jar-plugin:[0-9.]+")
                   (string-append "maven-jar-plugin:"
                                  ,(package-version maven-jar-plugin))))))
            (add-after 'fix-plugin-versions 'rebuild
              (lambda _
                (invoke "ant" "jar")
                #t))))))
    (native-inputs
     `(("java-plexus-component-metadata" ,java-plexus-component-metadata-1.7)
       ("java-commons-cli" ,java-commons-cli)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-jdom2" ,java-jdom2)
       ("java-qdox" ,java-qdox)
       ("maven-core-boot" ,maven-core-bootstrap)
       ,@(package-native-inputs maven-core-bootstrap)))))

(define-public maven-embedder
  (package
    (inherit maven-artifact)
    (name "maven-embedder")
    (arguments
     `(#:jar-name "maven-embedder.jar"
       #:source-dir "maven-embedder/src/main/java"
       #:test-dir "maven-embedder/src/test"
       #:test-exclude (list "**/MavenCliTest.java")
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu-named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (chmod "sisu.sh" #o755)
             (invoke "./sisu.sh" "maven-embedder/src/main/java"
                     "build/classes/META-INF/sisu/javax.inject.Named")
             #t))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-embedder/src/main/java" version
                       "false" "true"))
             (let ((file "maven-embedder/src/main/mdo/core-extensions.mdo"))
               (modello-single-mode file "1.0.0" "java")
               (modello-single-mode file "1.0.0" "xpp3-reader")
               (modello-single-mode file "1.0.0" "xpp3-writer"))
             #t))
         (add-before 'check 'fix-test-paths
           (lambda _
             (substitute* "maven-embedder/src/test/java/org/apache/maven/cli/CLIManagerDocumentationTest.java"
               (("target/test-classes") "build/test-classes"))
             #t))
         (add-before 'check 'fix-test-compilation
           (lambda _
             ;; Tests are in the java/ subdir. Other subdirectories contain
             ;; additional test plugins, with duplicate classes, so we can't
             ;; compile them. Also, they are meant to be built with maven, to
             ;; test its build process.
             (substitute* "build.xml"
               (("srcdir=\"maven-embedder/src/test\"")
                "srcdir=\"maven-embedder/src/test/java\""))
             #t))
         (add-before 'check 'disable-failing-test
           (lambda _
             (delete-file "maven-embedder/src/test/java/org/apache/maven/cli/event/ExecutionEventLoggerTest.java")))
         (add-before 'install 'fix-pom
           (lambda _
             (substitute* "maven-embedder/pom.xml"
               (("jsr250-api") "javax.annotation-api"))))
         (replace 'install
           (install-from-pom "maven-embedder/pom.xml")))))
    (propagated-inputs
     `(("maven-core" ,maven-core)
       ("maven-artifact" ,maven-artifact)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-model" ,maven-model)
       ("maven-model-builder" ,maven-model-builder)
       ("maven-settings" ,maven-settings)
       ("maven-settings-builder" ,maven-settings-builder)
       ("maven-shared-utils" ,maven-shared-utils)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-util" ,java-plexus-utils)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-cipher" ,java-plexus-cipher)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("maven-resolevr-util" ,maven-resolver-util)
       ("maven-resolevr-api" ,maven-resolver-api)
       ("java-logback-core" ,java-logback-core)
       ("java-logback-classic" ,java-logback-classic)
       ("java-commons-cli" ,java-commons-cli)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-javax-inject" ,java-javax-inject)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-slf4j-simple" ,java-slf4j-simple)
       ("java-jsr250" ,java-jsr250)))
    (native-inputs
     `(("java-asm-8" ,java-asm-8)
       ("java-modello-core" ,java-modello-core)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-plexus-build-api" ,java-plexus-build-api)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-cglib" ,java-cglib)
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
       ;; tests
       ("java-junit" ,java-junit)
       ("java-objenesis" ,java-objenesis)
       ("java-mockito-1" ,java-mockito-1)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains a Maven embeddable component, with CLI and
logging support.")))

(define-public maven-compat
  (package
    (inherit maven-artifact)
    (name "maven-compat")
    (arguments
     `(#:jar-name "maven-compat.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         ;; Tests assume we're in this directory
         (add-before 'configure 'chdir
           (lambda _
             (chdir "maven-compat")
             #t))
         (add-before 'build 'recreate-removed-jar
           (lambda _
             (with-output-to-file "src/test/repository-system/maven-core-2.1.0.jar"
               (const #t))
             (with-directory-excursion "src/test/resources"
               (with-output-to-file "artifact-install/artifact-1.0.jar"
                 (lambda _
                   (format #t "dummy~%")))
               (for-each
                 (lambda (file)
                   (with-output-to-file file
                     (lambda _
                       (format #t "foo~%"))))
                 '("local-repo/maven-test/jars/maven-test-a-1.0.jar"
                   "local-repo/maven-test/jars/maven-test-c-1.0.jar"
                   "local-repo/maven-test/jars/maven-test-d-1.0.jar"
                   "inheritance-repo/t04/maven-test/jars/t04-a-1.0.jar"
                   "inheritance-repo/t04/maven-test/jars/t04-b-1.0.jar"
                   "inheritance-repo/t04/maven-test/jars/t04-b-2.0.jar"
                   "inheritance-repo/t04/maven-test/jars/t04-c-1.0.jar"
                   "inheritance-repo/t04/maven-test/jars/t04-c-2.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-a-1.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-a-2.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-b-1.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-b-1.1.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-b-2.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-c-1.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-d-1.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-d-1.1.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-d-1.2.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-a-1.0.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-b-1.0.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-b-1.1.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-c-1.0.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-d-1.0.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-d-1.1.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-d-1.2.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-a-1.0.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-b-1.0.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-b-1.1.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-c-1.0.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-d-1.0.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-d-1.1.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-d-1.2.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-a-1.0.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-b-1.0.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-b-1.1.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-c-1.0.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-d-1.0.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-d-1.1.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-d-1.2.jar"
                   "inheritance-repo/t09/maven-test/jars/t09-a-1.0.jar"
                   "inheritance-repo/t09/maven-test/jars/t09-b-1.0.jar"
                   "inheritance-repo/t09/maven-test/jars/t09-c-1.0.jar"
                   "inheritance-repo/t09/maven-test/jars/t09-d-1.0.jar"
                   "inheritance-repo/t10/maven-test/jars/t10-a-1.0.jar"
                   "inheritance-repo/t10/maven-test/jars/t10-b-1.0.jar"
                   "inheritance-repo/t10/maven-test/jars/t10-c-1.0.jar"))
               (with-directory-excursion "local-repo/snapshot-test/jars"
                 (for-each
                   (lambda (file)
                     (with-output-to-file file
                       (lambda _
                         ;; No end-of-line
                         (format #t "local"))))
                   '("maven-snapshot-e-1.0-SNAPSHOT.jar"
                     "maven-snapshot-b-1.0-SNAPSHOT.jar"
                     "maven-snapshot-a-1.0-SNAPSHOT.jar"))))
             (for-each
               (lambda (letter)
                 (with-directory-excursion
                   (string-append "src/test/remote-repo/org/apache/maven/its/"
                                  letter "/0.1")
                   (let ((dir (string-append "META-INF/maven/org.apache.maven.its/"
                                             letter)))
                     (mkdir-p dir)
                     (copy-file (string-append letter "-0.1.pom")
                                (string-append dir "/pom.xml"))
                     (with-output-to-file (string-append dir "/pom.properties")
                       (lambda _
                         (format #t "version=0.1~%")
                         (format #t "groupId=org.apache.maven.its")
                         (format #t (string-append "artifactId=" letter))))
                     (with-output-to-file "META-INF/MANIFEST.MF"
                       (lambda _
                         (format #t "Manifest-Version: 1.0~%"))))
                     (invoke "jar" "cmf" "META-INF/MANIFEST.MF"
                             (string-append letter "-0.1.jar") "META-INF")))
               '("a" "b"))
             #t))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "src/main/java" version
                       "false" "true"))
             (let ((file "src/main/mdo/profiles.mdo"))
               (modello-single-mode file "1.0.0" "java")
               (modello-single-mode file "1.0.0" "xpp3-reader")
               (modello-single-mode file "1.0.0" "xpp3-writer"))
             (let ((file "src/main/mdo/paramdoc.mdo"))
               (modello-single-mode file "1.0.0" "java")
               (modello-single-mode file "1.0.0" "xpp3-reader")
               (modello-single-mode file "1.0.0" "xpp3-writer"))
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-before 'check 'build-tests
          (lambda _
            (invoke "ant" "compile-tests")
            #t))
         (add-after 'build-tests 'generate-test-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                 ":build/classes"
                                                 ":build/test-classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/test/java"
                     "--output" "build/test-classes/META-INF/plexus/components.xml"
                     "--classes" "build/test-classes"
                     "--descriptors" "build/test-classes/META-INF")
             #t))
         (add-before 'check 'disable-failing-test
           (lambda _
             (delete-file "src/test/java/org/apache/maven/profiles/manager/DefaultProfileManagerTest.java")))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-artifact
           maven-repository-metadata
           maven-builder-support
           maven-model
           maven-model-builder
           maven-settings
           maven-settings-builder
           maven-core
           maven-wagon-provider-api
           maven-wagon-file
           maven-resolver-api
           maven-resolver-util
           maven-resolver-spi
           java-plexus-interpolation))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations-1.7)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-plexus-build-api" ,java-plexus-build-api)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-exclispe-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-guice" ,java-guice)
       ("java-guava" ,java-guava)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
       ;; metadata
       ("java-plexus-component-metadata" ,java-plexus-component-metadata-1.7)
       ("java-commons-cli" ,java-commons-cli)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-jdom2" ,java-jdom2)
       ("maven-plugin-api" ,maven-plugin-api)
       ("java-qdox" ,java-qdox)
       ;; tests
       ("java-plexus-cipher" ,java-plexus-cipher)
       ("java-plexus-sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("java-jsr250" ,java-jsr250)
       ("java-cdi-api" ,java-cdi-api)
       ("java-junit" ,java-junit)
       ("maven-resolver-impl" ,maven-resolver-impl)
       ("maven-resolver-connector-basic" ,maven-resolver-connector-basic)
       ("maven-resolver-transport-wagon" ,maven-resolver-transport-wagon)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-aop" ,java-aopalliance)
       ("maven-resolver-provider" ,maven-resolver-provider)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-slf4j-simple" ,java-slf4j-simple)
       ,@(package-inputs java-slf4j-api)))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains Maven2 classes maintained as compatibility
layer for plugins that need to keep Maven2 compatibility.")))

(define-public maven
  (package
    (inherit maven-artifact)
    (name "maven")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Recreate the configuration for the loader
             (with-output-to-file "apache-maven/src/bin/m2.conf"
               (lambda _
                 (format #t "main is org.apache.maven.cli.MavenCli from plexus.core~%")
                 (format #t "~%")
                 (format #t "set maven.conf default ${maven.home}/conf~%")
                 (format #t "~%")
                 (format #t "[plexus.core]~%")
                 (format #t "load       ${maven.conf}/logging~%")
                 (format #t "optionally ${maven.home}/lib/ext/*.jar~%")
                 ;; Reference every jar so plexus-classworlds can find them.
                 (for-each
                   (lambda (dependency)
                     (for-each
                       (lambda (file)
                         (format #t "load       ~a~%" file))
                       (find-files (assoc-ref inputs dependency) ".*.jar$")))
                   '("maven-artifact" "maven-embedder" "maven-core" "maven-compat"
                     "maven-builder-support" "maven-model" "maven-model-builder"
                     "maven-settings" "maven-settings-builder" "maven-plugin-api"
                     "maven-repository-metadata" "maven-shared-utils" "maven-resolver-api"
                     "maven-resolver-spi" "maven-resolver-util" "maven-resolver-impl"
                     "maven-resolver-connector-basic" "maven-resolver-provider"
                     "maven-resolver-transport-wagon" "maven-wagon-provider-api"
                     "maven-wagon-file" "maven-wagon-http" "java-commons-logging-minimal"
                     "java-httpcomponents-httpclient" "java-httpcomponents-httpcore"
                     "maven-wagon-http-shared" "maven-wagon-tck-http"
                     "java-eclipse-sisu-plexus" "java-guice" "java-aopalliance"
                     "java-cglib" "java-asm" "java-eclipse-sisu-inject"
                     "java-javax-inject" "java-plexus-component-annotations"
                     "java-plexus-utils" "java-plexus-interpolation"
                     "java-plexus-sec-dispatcher" "java-plexus-cipher" "java-guava"
                     "java-jansi" "java-jsr250" "java-cdi-api" "java-commons-cli"
                     "java-commons-io" "java-commons-lang3" "java-slf4j-api"
                     "java-slf4j-simple"))))
             (substitute* "apache-maven/src/bin/mvn"
               (("cygwin=false;")
                (string-append
                  "CLASSPATH="
                  (car (find-files
                         (assoc-ref inputs "java-plexus-classworlds")
                         ".*.jar"))
                  "\ncygwin=false;"))
               (("-classpath.*") "-classpath ${CLASSPATH} \\\n"))
             #t))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/"))
                   (conf (string-append (assoc-ref outputs "out") "/conf/")))
               (mkdir-p (string-append (assoc-ref outputs "out") "/lib"))
               (for-each (lambda (file)
                           (install-file (string-append "apache-maven/src/bin/" file)
                                         bin)
                           (chmod (string-append bin file) #o755))
                '("mvn" "mvnDebug" "mvnyjp"))
               (install-file "apache-maven/src/bin/m2.conf" bin)
               (copy-recursively "apache-maven/src/conf" conf))
             #t)))))
    (inputs
     (list java-plexus-classworlds
           maven-artifact
           maven-embedder
           maven-core
           maven-compat
           maven-builder-support
           maven-model
           maven-model-builder
           maven-settings
           maven-settings-builder
           maven-plugin-api
           maven-repository-metadata
           maven-shared-utils
           maven-resolver-api
           maven-resolver-spi
           maven-resolver-util
           maven-resolver-impl
           maven-resolver-connector-basic
           maven-resolver-provider
           maven-resolver-transport-wagon
           maven-wagon-provider-api
           maven-wagon-file
           maven-wagon-http
           java-commons-logging-minimal
           java-httpcomponents-httpclient
           java-httpcomponents-httpcore
           maven-wagon-http-shared
           maven-wagon-tck-http
           java-eclipse-sisu-plexus
           java-guice
           java-aopalliance
           java-cglib
           java-asm-8
           java-eclipse-sisu-inject
           java-javax-inject
           java-plexus-component-annotations
           java-plexus-utils
           java-plexus-interpolation
           java-plexus-sec-dispatcher
           java-plexus-cipher
           java-guava
           java-jansi
           java-jsr250
           java-cdi-api
           java-commons-cli
           java-commons-io
           java-commons-lang3
           java-slf4j-api
           ;; TODO: replace with maven-slf4j-provider
           java-slf4j-simple))
    (propagated-inputs
     (list coreutils which))
    (description "Apache Maven is a software project management and comprehension
tool.  Based on the concept of a project object model: builds, dependency
management, documentation creation, site publication, and distribution
publication are all controlled from the @file{pom.xml} declarative file.  Maven
can be extended by plugins to utilise a number of other development tools for
reporting or the build process.")))

;; Many plugins require maven 3.0 as a dependency.
(define maven-3.0-pom
  (package
    (inherit maven-pom)
    (version "3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/apache/maven")
                     (commit (string-append "maven-" version))))
              (file-name (git-file-name "maven" version))
              (sha256
               (base32
                "06jdwxx9w24shhv3kca80rlrikynn7kdqcrwg59lv2b7adpllwnh"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "." "\\.jar$"))
                  (for-each (lambda (file) (chmod file #o644))
                            (find-files "." "."))
                  #t))
              (patches
                (search-patches "maven-generate-component-xml.patch"
                                "maven-generate-javax-inject-named.patch"))))
    (propagated-inputs
     `(("maven-parent-pom-15" ,maven-parent-pom-15)))))

(define-public maven-3.0-artifact
  (package
    (inherit maven-artifact)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (propagated-inputs
      (map
        (lambda (input)
          (if (equal? (car input) "maven-pom")
              `("maven-pom" ,maven-3.0-pom)
              input))
        (package-propagated-inputs maven-artifact)))))

(define-public maven-3.0-model
  (package
    (inherit maven-model)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (propagated-inputs
      (map
        (lambda (input)
          (if (equal? (car input) "maven-pom")
              `("maven-pom" ,maven-3.0-pom)
              input))
        (package-propagated-inputs maven-artifact)))
    (arguments
     `(#:jar-name "maven-model.jar"
       #:source-dir "maven-model/src/main/java"
       #:test-dir "maven-model/src/test"
       #:modules
       ((guix build ant-build-system)
        (guix build java-utils)
        (guix build syscalls)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'use-newer-model
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The model has almost not changed, but the newer version is
             ;; needed to prevent an error in the newer modello we have
             (let ((source (assoc-ref inputs "maven-source"))
                   (dir (mkdtemp! "maven-source-XXXXXXXX")))
               (with-directory-excursion dir
                 (invoke "tar" "xf" source)
                 (copy-file (car (find-files "." "maven.mdo"))
                            "../maven-model/src/main/mdo/maven.mdo")))
             #t))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-model/src/main/java" version
                       "false" "true" "UTF-8"))
             (let ((file "maven-model/src/main/mdo/maven.mdo"))
               (modello-single-mode file "4.0.0" "java")
               (modello-single-mode file "4.0.0" "xpp3-reader")
               (modello-single-mode file "4.0.0" "xpp3-writer")
               (modello-single-mode file "4.0.0" "xpp3-extended-reader"))
             #t))
         (replace 'install
           (install-from-pom "maven-model/pom.xml")))))
    (inputs
      `(("maven-source" ,(package-source maven-pom))
        ,@(package-inputs maven-model)))))

(define-public maven-3.0-settings
  (package
    (inherit maven-settings)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (propagated-inputs
      (map
        (lambda (input)
          (if (equal? (car input) "maven-pom")
              `("maven-pom" ,maven-3.0-pom)
              input))
        (package-propagated-inputs maven-settings)))))

(define-public maven-3.0-settings-builder
  (package
    (inherit maven-settings-builder)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (arguments
      (substitute-keyword-arguments (package-arguments maven-settings-builder)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'generate-components.xml
              (lambda _
                (mkdir-p "build/classes/META-INF/plexus")
                (chmod "components.sh" #o755)
                (invoke "./components.sh" "maven-settings-builder/src/main/java"
                        "build/classes/META-INF/plexus/components.xml")))))))
    (propagated-inputs
     `(("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ,@(filter
           (lambda (a) a)
           (map
             (lambda (input)
               (match (car input)
                 ("maven-pom" `("maven-pom" ,maven-3.0-pom))
                 ("maven-settings" `("maven-settings" ,maven-3.0-settings))
                 ("maven-builder-support" #f)
                 ("java-plexus-sec-dispatcher"
                  `("java-plexus-sec-dispatcher" ,java-plexus-sec-dispatcher-1.4))
                 (_ input)))
             (package-propagated-inputs maven-settings-builder)))))))

(define-public maven-3.0-model-builder
  (package
    (inherit maven-model-builder)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (arguments
      (substitute-keyword-arguments (package-arguments maven-model-builder)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'generate-components.xml
              (lambda _
                (mkdir-p "build/classes/META-INF/plexus")
                (chmod "components.sh" #o755)
                (invoke "./components.sh" "maven-model-builder/src/main/java"
                        "build/classes/META-INF/plexus/components.xml")))
            (add-before 'check 'remove-failing-test
              (lambda _
                (delete-file "maven-model-builder/src/test/java/org/apache/maven/model/interpolation/StringSearchModelInterpolatorTest.java")))))))
    (propagated-inputs
     `(("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ,@(filter
           (lambda (a) a)
           (map
             (lambda (input)
               (match (car input)
                 ("maven-pom" `("maven-pom" ,maven-3.0-pom))
                 ("maven-model" `("maven-model" ,maven-3.0-model))
                 ("maven-artifact" `("maven-artifact" ,maven-3.0-artifact))
                 ("maven-builder-support" #f)
                 (_ input)))
             (package-propagated-inputs maven-model-builder)))))))

(define-public maven-3.0-plugin-api
  (package
    (inherit maven-plugin-api)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (arguments
      (substitute-keyword-arguments (package-arguments maven-plugin-api)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'install 'fix-pom
              (lambda _
                (substitute* "maven-plugin-api/pom.xml"
                  (("org.sonatype.sisu") "org.codehaus.plexus")
                  (("sisu-inject-plexus") "plexus-container-default"))
                #t))))))
    (propagated-inputs
      (map
        (lambda (input)
          (match (car input)
            ("maven-pom" `("maven-pom" ,maven-3.0-pom))
            ("maven-artifact" `("maven-artifact" ,maven-3.0-artifact))
            ("maven-model" `("maven-model" ,maven-3.0-model))
            (_ input)))
        (package-propagated-inputs maven-model-builder)))
    (native-inputs
     (modify-inputs (package-native-inputs maven-plugin-api)
       (prepend java-plexus-container-default)))))

(define-public maven-3.0-repository-metadata
  (package
    (inherit maven-repository-metadata)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (propagated-inputs
      (map
        (lambda (input)
          (if (equal? (car input) "maven-pom")
              `("maven-pom" ,maven-3.0-pom)
              input))
        (package-propagated-inputs maven-repository-metadata)))))

(define-public maven-3.0-aether-provider
  (package
    (inherit maven-3.0-pom)
    (name "maven-aether-provider")
    (arguments
     `(#:jar-name "maven-aether-provider.jar"
       #:source-dir "maven-aether-provider/src/main/java"
       #:tests? #f; no tests in 3.0
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (replace 'install
           (install-from-pom "maven-aether-provider/pom.xml")))))
    (propagated-inputs
     `(("maven-model" ,maven-3.0-model)
       ("maven-model-builder" ,maven-3.0-model-builder)
       ("maven-repository-metadata" ,maven-3.0-repository-metadata)
       ("java-sonatype-aether-api" ,java-sonatype-aether-api)
       ("java-sonatype-aether-spi" ,java-sonatype-aether-spi)
       ("java-sonatype-aether-impl" ,java-sonatype-aether-impl)
       ("java-plexus-component-annotation" ,java-plexus-component-annotations)
       ("java-plexus-utils" ,java-plexus-utils)
       ("maven-pom" ,maven-3.0-pom)))
    (native-inputs
     (list java-plexus-component-metadata))))

(define-public maven-3.0-core
  (package
    (inherit maven-core)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (arguments
     `(#:jar-name "maven-core.jar"
       #:source-dir "src/main/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Required for generating components.xml in maven-core
             (chdir "maven-core")
             #t))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "src/main/java" version
                       "false" "true" "UTF-8"))
             (let ((file "src/main/mdo/toolchains.mdo"))
               (modello-single-mode file "1.0.0" "java")
               (modello-single-mode file "1.0.0" "xpp3-reader")
               (modello-single-mode file "1.0.0" "xpp3-writer"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/")
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (define (components file)
               (let ((sxml (with-input-from-file file
                             (lambda _ (xml->sxml (current-input-port)
                                                  #:trim-whitespace? #t)))))
                 ;; Select the list of <component>s inside the <component-set>
                 ;; and <components>.
                 ((@ (ice-9 match) match) sxml
                  (('*TOP*
                    ('*PI* foo ...)
                    ('component-set
                     ('components x ...))) x))))
             (use-modules (sxml simple))
             (delete-file "build/classes/META-INF/plexus/components.xml")
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "build/classes/META-INF/plexus"
                     "--output" "build/classes/META-INF/plexus/components.t.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes")
             ;; Now we merge all other components from hand-written xml
             (let ((generated-xml (components "build/classes/META-INF/plexus/components.t.xml"))
                   (components-xml (components "src/main/resources/META-INF/plexus/components.xml"))
                   (artifact-handlers-xml (components "src/main/resources/META-INF/plexus/artifact-handlers.xml")))
               (with-output-to-file "build/classes/META-INF/plexus/components.xml"
                 (lambda _
                   (display "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
                   (sxml->xml
                     `(component-set
                        (components
                          ,@(append generated-xml components-xml
                                    artifact-handlers-xml)))))))
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (add-before 'install 'fix-pom
           (lambda _
             (substitute* "pom.xml"
               (("org.sonatype.sisu") "org.codehaus.plexus")
               (("sisu-inject-plexus") "plexus-container-default"))
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-3.0-model
           maven-3.0-settings
           maven-3.0-settings-builder
           maven-3.0-repository-metadata
           maven-3.0-artifact
           maven-3.0-model-builder
           maven-3.0-aether-provider
           java-sonatype-aether-impl
           java-sonatype-aether-api
           java-sonatype-aether-util
           java-plexus-interpolation
           java-plexus-utils
           java-plexus-classworlds
           java-plexus-component-annotations
           java-plexus-container-default
           java-plexus-sec-dispatcher-1.4
           maven-3.0-pom))))

(define-public maven-3.0-compat
  (package
    (inherit maven-compat)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (arguments
     `(#:tests? #f ;require an old version of java-easymock
       ,@(substitute-keyword-arguments (package-arguments maven-compat)
          ((#:phases phases)
           `(modify-phases ,phases
              (add-before 'install 'fix-pom
                (lambda _
                  (substitute* "pom.xml"
                    (("org.sonatype.sisu") "org.codehaus.plexus")
                    (("sisu-inject-plexus") "plexus-container-default"))
                  #t))
              (delete 'build-tests))))))
    (propagated-inputs
     (list maven-3.0-model
           maven-3.0-model-builder
           maven-3.0-settings
           maven-3.0-settings-builder
           maven-3.0-artifact
           maven-3.0-core
           maven-3.0-aether-provider
           maven-3.0-repository-metadata
           java-sonatype-aether-api
           java-sonatype-aether-util
           java-sonatype-aether-impl
           java-plexus-utils
           java-plexus-interpolation
           java-eclipse-sisu-plexus
           java-plexus-component-annotations
           java-plexus-container-default
           maven-wagon-provider-api
           maven-3.0-pom))))

(define-public maven-shared-utils-3.0
  (package
    (inherit maven-shared-utils)
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/shared/"
                                  "maven-shared-utils-" version "-source-release.zip"))
              (sha256
               (base32
                "0qm8y85kip2hyhnhlkqgj0rhmf83z07s7l7gzsfl5dzl3kvp8nal"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs maven-shared-utils)
       (prepend maven-3.0-core maven-components-parent-pom-21)))))

(define-public maven-shared-utils-3.1
  (package
    (inherit maven-shared-utils)
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/shared/"
                                  "maven-shared-utils-" version "-source-release.zip"))
              (sha256
               (base32
                "0vfaas4g09ch0agrd1dcxcmhdd3w971ssvfr9mx9gi2lp5nv8w66"))))))

(define-public maven-shared-io
  (package
    (name "maven-shared-io")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-shared-io-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "0hsyll8gg581802xhs4achdz8fpmfz7y02abx9s4mb8bc6yfh229"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-shared-io.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes/")
             (copy-recursively "src/test/resources" "build/test-classes/")
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-3.0-artifact
           maven-3.0-compat
           maven-3.0-plugin-api
           maven-shared-utils
           maven-wagon-provider-api
           java-plexus-utils
           maven-components-parent-pom-22))
    (native-inputs
     (list unzip java-junit java-easymock))
    (home-page "https://maven.apache.org/shared/maven-dependency-tree")
    (synopsis "Tree-based API for resolution of Maven project dependencies")
    (description "This package provides a tree-based API for resolution of
Maven project dependencies.")
    (license license:asl2.0)))

(define-public maven-file-management
  (package
    (name "maven-file-management")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "file-management-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "0wisz6sm67axrwvx8a75mb9s03h7kzkzfw8j3aaa4sx4k9ph58da"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-file-management.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes/")
             #t))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java"
                       "org.codehaus.modello.ModelloCli"
                       file mode "src/main/java" version
                       "false" "true"))
             (let ((file "src/main/mdo/fileset.mdo"))
               (modello-single-mode file "1.1.0" "java")
               (modello-single-mode file "1.1.0" "xpp3-reader")
               (modello-single-mode file "1.1.0" "xpp3-writer"))
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-3.0-plugin-api maven-shared-io maven-shared-utils
           java-plexus-utils maven-components-parent-pom-22))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ;; modello plugins:
       ("java-modellop-plugins-java" ,java-modello-plugins-java)
       ("java-modellop-plugins-xpp3" ,java-modello-plugins-xpp3)
       ("unzip" ,unzip)))
    (home-page "https://maven.apache.org/shared/maven-dependency-tree")
    (synopsis "Tree-based API for resolution of Maven project dependencies")
    (description "This package provides a tree-based API for resolution of
Maven project dependencies.")
    (license license:asl2.0)))

(define-public maven-archiver
  (package
    (name "maven-archiver")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-archiver-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "1204xkqj259brpk3yscprml0lbfbyn1vn5nrgqjk44z5vx127lbw"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-archiver.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     `(("java-commons-io" ,java-commons-io)
       ("maven-artifact" ,maven-3.0-artifact)
       ("maven-core" ,maven-3.0-core)
       ("maven-model" ,maven-3.0-model)
       ("maven-shared-utils" ,maven-shared-utils)
       ("java-plexus-archiver" ,java-plexus-archiver)
       ("java-plexus-interpolation" ,java-plexus-interpolation)
       ("java-plexus-utils" ,java-plexus-utils)
       ("maen-parent-pom" ,maven-parent-pom-33)))
    (native-inputs
     (list java-junit java-assertj unzip))
    (home-page "https://maven.apache.org/shared/maven-dependency-tree")
    (synopsis "Tree-based API for resolution of Maven project dependencies")
    (description "This package provides a tree-based API for resolution of
Maven project dependencies.")
    (license license:asl2.0)))

(define-public maven-dependency-tree
  (package
    (name "maven-dependency-tree")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-dependency-tree-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "1vhcd3lmbyy8q61c37sqgbllqj4ypkxm344l6pb05mkchlyk5dy5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-dependency-tree.jar"
       #:source-dir "src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-3.0-core java-plexus-component-annotations
           maven-parent-pom-34))
    (inputs
     (list java-sonatype-aether-api-1.13 java-sonatype-aether-util-1.13
           java-eclipse-aether-api java-eclipse-aether-util))
    (native-inputs
     (list unzip java-junit))
    (home-page "https://maven.apache.org/shared/maven-dependency-tree")
    (synopsis "Tree-based API for resolution of Maven project dependencies")
    (description "This package provides a tree-based API for resolution of
Maven project dependencies.")
    (license license:asl2.0)))

(define-public maven-common-artifact-filters
  (package
    (name "maven-common-artifact-filters")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-common-artifact-filters-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "1mr92s4zz6gf028wiskjg8rd1znxzdnmskg42ac55ifg9v1p1884"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-common-artifact-filters.jar"
       #:source-dir "src/main/java"
       #:tests? #f; require maven-plugin-testing-harness, which requires maven 3.2.
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-aether
           (lambda _
             (substitute* "pom.xml"
               (("eclipse.aether") "sonatype.aether"))
             (substitute* "src/main/java/org/apache/maven/shared/artifact/filter/collection/ArtifactTransitivityFilter.java"
               (("eclipse") "sonatype"))))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-3.0-artifact
           maven-3.0-model
           maven-3.0-core
           maven-3.0-plugin-api
           maven-shared-utils
           maven-parent-pom-33
           java-eclipse-sisu-plexus
           java-sonatype-aether-api
           java-sonatype-aether-util))
    (inputs
     (list maven-resolver-api maven-resolver-util))
    (native-inputs
     (list unzip))
   (home-page "https://maven.apache.org/shared/maven-dependency-tree")
    (synopsis "Tree-based API for resolution of Maven project dependencies")
    (description "This package provides a tree-based API for resolution of
Maven project dependencies.")
    (license license:asl2.0)))

(define-public maven-common-artifact-filters-3.1.0
  (package
    (inherit maven-common-artifact-filters)
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-common-artifact-filters-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "1cl1qk4r0gp62bjzfm7lml9raz1my2kd4yf0ci0lnfsn0h5qivnb"))))
    (arguments
      (substitute-keyword-arguments (package-arguments maven-common-artifact-filters)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'fix-aether)
           (add-before 'build 'remove-sisu
             (lambda _
               (substitute* "pom.xml"
                 (("sisu-inject-plexus") "maven-plugin-api")
                 (("org.sonatype.sisu") "org.apache.maven"))))))))))

(define-public maven-enforcer-api
  (package
    (name "maven-enforcer-api")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/enforcer/"
                                  "enforcer-" version "-source-release.zip"))
              (sha256
               (base32
                "1479yp58jv788xc1jc2sbdxpajlbvwlk60639vd2h4s8r6x7naqh"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-enforcer-api.jar"
       #:source-dir "enforcer-api/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "enforcer-api/pom.xml")))))
    (propagated-inputs
     (list maven-plugin-api java-plexus-container-default java-jsr305
           maven-enforcer-parent-pom))
    (native-inputs
     (list unzip))
    (home-page "https://maven.apache.org/shared/maven-dependency-tree")
    (synopsis "Tree-based API for resolution of Maven project dependencies")
    (description "This package provides a tree-based API for resolution of
Maven project dependencies.")
    (license license:asl2.0)))

(define maven-enforcer-parent-pom
  (package
    (inherit maven-enforcer-api)
    (name "maven-enforcer-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'fix-pom-versions
           (lambda _
             (substitute* "pom.xml"
               (("<maven.version>.*</maven.version>")
                ,(string-append "<maven.version>" (package-version maven)
                                "</maven.version>"))
               (("2.11.0") ,(package-version java-commons-io))
               (("3.12.0") ,(package-version java-commons-lang3))
               (("1.6.1") ,(package-version maven-resolver-util))
               (("1.15") ,(package-version java-commons-codec)))))
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     `(("maven-parent-pom" ,maven-parent-pom-30)))))

(define-public maven-enforcer-rules
  (package
    (inherit maven-enforcer-api)
    (name "maven-enforcer-rules")
    (arguments
     `(#:tests? #f; requires maven-plugin-testing-harness
       #:jar-name "maven-enforcer-rules.jar"
       #:source-dir "enforcer-rules/src/main/java"
       #:test-dir "enforcer-rules/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "enforcer-rules/pom.xml")))))
    (propagated-inputs
     (list maven-artifact
           maven-plugin-api
           maven-core
           maven-common-artifact-filters
           java-commons-codec
           java-commons-lang3
           maven-enforcer-api
           maven-resolver-util
           java-bsh
           maven-dependency-tree
           maven-3.0-compat
           maven-enforcer-parent-pom))))

(define-public maven-enforcer-plugin
  (package
    (inherit maven-enforcer-api)
    (name "maven-enforcer-plugin")
    (arguments
     `(#:tests? #f
       #:jar-name "maven-enforcer-plugin.jar"
       #:source-dir "maven-enforcer-plugin/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "maven-enforcer-plugin/pom.xml"
             "enforcer"
             "maven-enforcer-plugin/src/main/java/org/apache/maven/plugins/enforcer/"
             (list
               (list "DisplayInfoMojo.java")
               (list "EnforceMojo.java"))))
         (replace 'install
           (install-from-pom "maven-enforcer-plugin/pom.xml")))))
    (propagated-inputs
     (list maven-artifact
           maven-plugin-api
           maven-core
           java-plexus-utils
           maven-enforcer-api
           maven-enforcer-rules
           maven-plugin-annotations
           maven-enforcer-parent-pom))))

(define-public maven-artifact-transfer
  (package
    (name "maven-artifact-transfer")
    (version "0.13.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-artifact-transfer-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "0xl7lkksljacrhmvwf924zb6h0h5zw9494jaz9cz4hll0lrhlpz6"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; require mockito 2
       #:jar-name "maven-artifact-transfer.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-commons-codec
           maven-3.0-artifact
           maven-3.0-core
           maven-common-artifact-filters-3.1.0
           java-plexus-component-annotations
           java-plexus-utils
           java-slf4j-api
           java-plexus-classworlds
           java-sonatype-aether-api
           java-eclipse-aether-api
           java-eclipse-aether-util
           java-eclipse-aether-impl))
    (native-inputs
     (list unzip java-plexus-component-metadata))
    (home-page "https://maven.apache.org/shared/maven-artifact-transfer")
    (synopsis "API to install, deploy and resolve artifacts in Maven")
    (description "This package contains an API to install, deploy and resolve
artifacts in Maven 3.")
    (license license:asl2.0)))

(define-public maven-install-plugin
  (package
    (name "maven-install-plugin")
    (version "3.0.0-M1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/plugins/"
                                  "maven-install-plugin-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "1l9iydxririrair0i5sk2iypn9wspzbb666lc0ddg20yyr8w39dm"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; require maven-plugin-testing-harness
       #:jar-name "maven-install-plugin.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-pom
           (lambda _
             (substitute* "pom.xml"
               (("maven-project") "maven-core")
               (("maven-artifact-manager") "maven-artifact")
               (("2.0.6") "3.0"))
             #t))
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "pom.xml"
             "install"
             "src/main/java/org/apache/maven/plugins/install"
             (list
               (list "AbstractInstallMojo.java" "InstallFileMojo.java")
               (list "AbstractInstallMojo.java" "InstallMojo.java"))))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     `(("maven-artifact" ,maven-artifact)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-compat" ,maven-compat)
       ("maven-artifact-transfer" ,maven-artifact-transfer)
       ("maven-plugins-pom-23" ,maven-plugins-pom-23)
       ("java-plexus-digest" ,java-plexus-digest)))
    (inputs
     (list maven-plugin-annotations java-slf4j-api))
    (native-inputs
     (list unzip))
    (home-page "https://maven.apache.org/plugin/maven-install-plugin")
    (synopsis "Maven's install plugin")
    (description "The Install Plugin is used during the install phase to add
artifact(s) to the local repository.  The Install Plugin uses the information
in the POM (groupId, artifactId, version) to determine the proper location for
the artifact within the local repository.

The local repository is the local cache where all artifacts needed for the
build are stored.  By default, it is located within the user's home directory
(@file{~/.m2/repository}) but the location can be configured in
@file{~/.m2/settings.xml} using the @code{<localRepository>} element.")
    (license license:asl2.0)))

(define-public maven-filtering
  (package
    (name "maven-filtering")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/"
                                  "shared/maven-filtering-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "09wrdhchnszd2l6h4z30ra0bv1a19qyjgac9z8zf1pn0m4nw05yz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-filtering.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       ;; this test comes from plexus-build-api, not this package
       #:test-exclude (list "**/IncrementalResourceFilteringTest.java"
                            "**/Abstract*.java")
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (add-before 'check 'decompress-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((build-api-source (assoc-ref inputs "java-plexus-build-api-origin"))
                    (classes (string-append build-api-source "/src/test/java")))
               (copy-recursively classes "src/test/"))
             #t))
         (add-before 'check 'fix-directory
           (lambda _
             (substitute* (find-files "src/test" ".*.java$")
               (("target/test-classes/") "build/test-classes/"))))
         (add-before 'check 'copy-test-resources
           (lambda _
             (copy-recursively "src/test/resources" "build/test-classes/")
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-3.0-core
           maven-shared-utils
           java-plexus-utils-3.2.1
           java-plexus-interpolation
           java-plexus-build-api
           maven-parent-pom-30))
    (inputs
     (list java-jsr305))
    (native-inputs
     `(("unzip" ,unzip)
       ("java-assertj" ,java-assertj)
       ("java-junit" ,java-junit)
       ("java-mockito" ,java-mockito-1)
       ("java-objenesis" ,java-objenesis)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-plexus-build-api-origin" ,(package-source java-plexus-build-api))))
    (home-page "https://maven.apache.org/shared/maven-filtering")
    (synopsis "Shared component for all plugins that needs to filter resources")
    (description "This component provides an API to filter resources in Maven
projects.")
    (license license:asl2.0)))

(define-public maven-resources-plugin
  (package
    (name "maven-resources-plugin")
    (version "3.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/apache/maven-resources-plugin")
                     (commit (string-append  "maven-resources-plugin-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "090k5j2y7ak54czfjjg3v7pdmdlgd96fbs91d1fd3vslm9zzndg8"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-resources-plugin.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; test depends on maven-plugin-test-harness
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "pom.xml" "resources"
             "src/main/java/org/apache/maven/plugins/resources"
             (list
               (list "ResourcesMojo.java" "CopyResourcesMojo.java")
               (list "ResourcesMojo.java")
               (list "ResourcesMojo.java" "TestResourcesMojo.java"))))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-plugin-api
           maven-core
           java-plexus-utils
           maven-filtering
           java-plexus-interpolation
           maven-parent-pom-31))
    (inputs
     (list maven-plugin-annotations java-commons-io))
    (native-inputs
     (list java-plexus-component-metadata))
    (home-page "https://maven.apache.org/plugins/maven-resources-plugin")
    (synopsis "Maven plugin to collect and install resources")
    (description "The Resources Plugin handles the copying of project resources
to the output directory.  There are two different kinds of resources: main
resources and test resources.  The difference is that the main resources are
the resources associated to the main source code while the test resources are
associated to the test source code.

Thus, this allows the separation of resources for the main source code and its
unit tests.")
    (license license:asl2.0)))

(define-public maven-shared-incremental
  (package
    (name "maven-shared-incremental")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/shared/"
                                  "maven-shared-incremental-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "03n4nfswyg9ahkz2zx4skcr3ghs01zh95g9js51hc75mfqx9b976"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "shared-incremental.java"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'change-parent
           (lambda _
             (substitute* "pom.xml"
               (("19") "30"))
             #t))
         (add-before 'build 'fix-pom
           (lambda _
             (substitute* "pom.xml"
               (("plexus-component-api") "plexus-component-annotations"))
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-plugin-api maven-core maven-shared-utils
           java-plexus-component-annotations maven-parent-pom-30))
    (native-inputs
     (list unzip java-plexus-component-metadata))
    (home-page "https://maven.apache.org/shared/maven-shared-incremental")
    (synopsis "Maven Incremental Build support utilities")
    (description "This package contains various utility classes and plexus
components for supporting incremental build functionality in maven plugins.")
    (license license:asl2.0)))

(define-public maven-compiler-plugin
  (package
    (name "maven-compiler-plugin")
    (version "3.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/apache/maven-compiler-plugin")
                     (commit (string-append "maven-compiler-plugin-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jkbq02vykd09ws8k9bzqxv6fjrpmir8gcxydbmj05kkhl242bma"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-compiler-plugin.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; test depends on maven-plugin-test-harness
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "pom.xml"
             "compiler"
             "src/main/java/org/apache/maven/plugin/compiler"
             (list
               (list "AbstractCompilerMojo.java" "CompilerMojo.java")
               (list "AbstractCompilerMojo.java" "TestCompilerMojo.java"))))
         (add-after 'generate-plugin.xml 'fix-plugin.xml
           (lambda _
             (substitute* "build/classes/META-INF/maven/plugin.xml"
               ;; These are defined in AbstractCompilerMojo.java, but not
               ;; parsed correctly in the previous phase
               (("DEFAULT_TARGET") "1.6")
               (("DEFAULT_SOURCE") "1.6"))
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-plugin-api
           maven-artifact
           maven-core
           maven-shared-utils
           maven-shared-incremental
           java-plexus-java
           java-plexus-compiler-api
           java-plexus-compiler-manager
           java-plexus-compiler-javac
           maven-parent-pom-33))
    (inputs
     (list maven-plugin-annotations java-commons-io))
    (home-page "https://maven.apache.org/plugins/maven-compiler-plugin")
    (synopsis "Compiler plugin for Maven")
    (description "The Compiler Plugin is used to compile the sources of your
project.  Since 3.0, the default compiler is @code{javax.tools.JavaCompiler}
(if you are using java 1.6) and is used to compile Java sources.  If you want
to force the plugin using javac, you must configure the plugin option
@code{forceJavacCompilerUse}.

Also note that at present the default source setting is 1.6 and the default
target setting is 1.6, independently of the JDK you run Maven with.  You are
highly encouraged to change these defaults by setting source and target as
described in Setting the -source and -target of the Java Compiler.

Other compilers than javac can be used and work has already started on
AspectJ, .NET, and C#.")
    (license license:asl2.0)))

(define-public java-surefire-logger-api
  (package
    (name "java-surefire-logger-api")
    (version "3.0.0-M4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/surefire/"
                                  "surefire-" version "-source-release.zip"))
              (sha256
               (base32
                "1s6d4pzk3bjm9l38mj9sfgbgmk145rppdj1dmqwc4d5105mr9q9w"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-surefire-logger-api.jar"
       #:source-dir "surefire-logger-api/src/main/java"
       #:tests? #f; require mockito 2
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "surefire-logger-api/pom.xml")))))
    (propagated-inputs
     (list java-surefire-parent-pom))
    (native-inputs
     (list unzip))
    (home-page "https://maven.apache.org/surefire/surefire-logger-api")
    (synopsis "Interfaces and Utilities related only to internal SureFire Logger API")
    (description "This package contains interfaces and utilities that are
internal to the SureFire Logger API.  It is designed to have no dependency.")
    (license license:asl2.0)))

(define-public java-surefire-parent-pom
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'fix-pom-dependency-versions
           (lambda _
             (substitute* "pom.xml"
               (("1.11") ,(package-version java-commons-compress))
               (("1.13") ,(package-version java-commons-codec)))
             (substitute* "pom.xml"
               (("commonsLang3Version>.*")
                (string-append
                  "commonsLang3Version>"
                  ,(package-version java-commons-lang3)
                  "</commonsLang3Version>\n"))
               (("commonsCompress>.*")
                (string-append
                  "commonsCompress>"
                  ,(package-version java-commons-compress)
                  "</commonsCompress>\n"))
               (("commonsIoVersion>.*")
                (string-append
                  "commonsIoVersion>"
                  ,(package-version java-commons-io)
                  "</commonsIoVersion>\n"))
               (("0.11.0") ,(package-version maven-artifact-transfer))
               (("1.0.3") ,(package-version java-plexus-java)))
             #t))
         (add-after 'install 'install-providers
           (install-pom-file "surefire-providers/pom.xml"))
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     (list maven-parent-pom-33))))

(define-public java-surefire-api
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-api")
    (arguments
     `(#:tests? #f
       #:jar-name "java-surefire-api.jar"
       #:source-dir "surefire-api/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "surefire-api/src/main/resources" "build/classes")
             #t))
         (add-before 'build 'prepare-shade
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             (with-directory-excursion "build/classes"
               (for-each
                 (lambda (input)
                   (for-each
                     (lambda (jar-file)
                       (invoke "jar" "xf" jar-file)
                       (delete-file-recursively "META-INF"))
                     (find-files (assoc-ref inputs input) ".*.jar$")))
                 '("maven-shared-utils" "java-commons-codec")))
             #t))
         (add-after 'build 'shade
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jarjar
                   (car (find-files (assoc-ref inputs "java-jarjar") ".*.jar$")))
                   (injar "java-surefire-api.jar")
                   (outjar "java-surefire-api-shaded.jar"))
               (with-directory-excursion "build/jar"
                 (with-output-to-file "rules"
                   (lambda _
                     (format #t (string-append
                                  "rule "
                                  "org.apache.maven.shared.utils.** "
                                  "org.apache.maven.surefire.shade.api."
                                  "org.apache.maven.shared.utils.@1~%"))
                     (format #t (string-append
                                  "rule "
                                  "org.apache.commons.codec.** "
                                  "org.apache.maven.surefire.shade.api."
                                  "org.apache.commons.codec.@1~%"))))
                 (invoke "java" "-jar" jarjar "process" "rules" injar outjar)
                 (delete-file injar)
                 (rename-file outjar injar)))
             #t))
         (replace 'install
           (install-from-pom "surefire-api/pom.xml")))))
    (propagated-inputs
     (list java-surefire-logger-api java-commons-codec
           java-surefire-parent-pom maven-shared-utils-3.1))
    (inputs
     (list java-jsr305))
    (native-inputs
     (list unzip java-jarjar))
    (synopsis "Maven SureFire API")
    (description "This package contains the API to use Maven SureFire.")))

(define-public java-surefire-booter
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-booter")
    (arguments
     `(#:tests? #f; require mockito 2
       #:jar-name "java-surefire-booter.jar"
       #:source-dir "surefire-booter/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-/bin/sh
           (lambda _
             (substitute* "surefire-booter/src/main/java/org/apache/maven/surefire/booter/PpidChecker.java"
               (("/bin/sh") (which "sh")))
             #t))
         (replace 'install
           (install-from-pom "surefire-booter/pom.xml")))))
    (propagated-inputs
     (list java-surefire-api java-commons-lang3 java-commons-io
           java-surefire-parent-pom))
    (inputs
     (list java-jsr305))
    (synopsis "API and Facilities used by forked tests running in JVM sub-process")
    (description "SureFire runs tests inside a forked JVM subprocess.  This
package contains an API and facilities used inside that forked JVM.")))

(define-public java-surefire-extensions-api
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-extensions-api")
    (arguments
     `(#:tests? #f; requires mockito 2
       #:jar-name "java-surefire-extensions-api.jar"
       #:source-dir "surefire-extensions-api/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "surefire-extensions-api/pom.xml")))))
    (propagated-inputs
     (list java-surefire-api java-surefire-parent-pom))
    (inputs
     (list java-plexus-component-annotations))
    (synopsis "Extension API for Maven SureFire")
    (description "Surefire is a test framework project.  This is the aggregator
POM in Apache Maven Surefire project.")))

(define-public java-surefire-common-java5
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-common-java5")
    (arguments
     `(#:jar-name "java-surefire-common-java5.jar"
       #:source-dir "surefire-providers/common-java5/src/main/java"
       #:test-dir "surefire-providers/common-java5/src/test"
       #:test-exclude (list
                        ;; Abstract class
                        "**/PojoStackTraceWriterTest.java"
                        ;; Fails
                        "**/SmartStackTraceParserTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare-shade
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             (with-directory-excursion "build/classes"
               (for-each
                 (lambda (jar-file)
                   (invoke "jar" "xf" jar-file)
                   (delete-file-recursively "META-INF"))
                 (find-files (assoc-ref inputs "maven-shared-utils") ".*.jar$")))
             #t))
         (add-after 'build 'shade
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jarjar
                   (car (find-files (assoc-ref inputs "java-jarjar") ".*.jar$")))
                   (injar "java-surefire-common-java5.jar")
                   (outjar "java-surefire-common-java5-shaded.jar"))
               (with-directory-excursion "build/jar"
                 (with-output-to-file "rules"
                   (lambda _
                     (format #t (string-append
                                  "rule "
                                  "org.apache.maven.shared.utils.** "
                                  "org.apache.maven.surefire.shade.common."
                                  "org.apache.maven.shared.utils.@1~%"))))
                 (invoke "java" "-jar" jarjar "process" "rules" injar outjar)
                 (delete-file injar)
                 (rename-file outjar injar)))
             #t))
         (replace 'install
           (install-from-pom "surefire-providers/common-java5/pom.xml")))))
    (propagated-inputs
     (list maven-shared-utils-3.1 java-surefire-api
           java-surefire-parent-pom))
    (native-inputs
     (list unzip java-jarjar java-junit java-fest-assert))
    (synopsis "Common java5 facilities for Maven SureFire")
    (description "This package contains shared Java 5 code for all providers.")))

(define-public java-surefire-common-junit3
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-common-junit3")
    (arguments
     `(#:jar-name "java-surefire-common-junit3.jar"
       #:source-dir "surefire-providers/common-junit3/src/main/java"
       #:test-dir "surefire-providers/common-junit3/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "surefire-providers/common-junit3/pom.xml")))))
    (propagated-inputs
     (list java-junit java-surefire-api java-surefire-parent-pom))
    (native-inputs
     (list unzip java-junit java-fest-assert))
    (synopsis "Shared JUnit3 provider code for Maven SureFire")
    (description "This package contains shared code for all JUnit providers.")))

(define-public java-surefire-common-junit4
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-common-junit4")
    (arguments
     `(#:jar-name "java-surefire-common-junit4.jar"
       #:source-dir "surefire-providers/common-junit4/src/main/java"
       #:tests? #f; tests require junit 4.0
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "surefire-providers/common-junit4/pom.xml")))))
    (propagated-inputs
     (list java-junit
           java-surefire-api
           java-surefire-common-java5
           java-surefire-common-junit3
           maven-shared-utils-3.1
           java-surefire-parent-pom))
    (synopsis "Shared JUnit4 provider code for Maven SureFire")
    (description "This package contains shared code for all JUnit providers,
starting from JUnit 4.")))

(define-public java-surefire-junit4
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-junit4")
    (arguments
     `(;#:tests? #f
       #:jar-name "java-surefire-junit4.jar"
       #:source-dir "surefire-providers/surefire-junit4/src/main/java"
       #:test-dir "surefire-providers/surefire-junit4/src/test"
       #:modules ((guix build ant-build-system)
                  (guix build utils)
                  (guix build java-utils)
                  (sxml simple))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'regenerate-own-pom
           ;; Surefire struggles resolving artifacts because of this pom
           ;; file, resulting in a NullPointerException when collecting
           ;; Artifacts (and a "Failure detected." message from
           ;; DefaultArtifactResolver).  Replace the pom file with a much
           ;; simpler one.  Everything is shaded anyway (as used to be the
           ;; case in 2.22), so there will not be missing dependencies.
           (generate-pom.xml
             "surefire-providers/surefire-junit4/pom.xml"
             "org.apache.maven.surefire" "surefire-junit4"
             ,(package-version java-surefire-common-java5)
             #:name "Surefire JUnit4"))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "surefire-providers/surefire-junit4/src/main/resources"
                               "build/classes")
             #t))
         (add-before 'build 'prepare-shade
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             (with-directory-excursion "build/classes"
               (for-each
                 (lambda (input)
                   (for-each
                     (lambda (jar-file)
                       (invoke "jar" "xf" jar-file)
                       (delete-file-recursively "META-INF"))
                     (find-files (assoc-ref inputs input) ".*.jar$")))
                 '("maven-shared-utils" "java-surefire-common-java5"
                   "java-surefire-common-junit3" "java-surefire-common-junit4"
                   "java-surefire-api")))
             #t))
         (add-after 'build 'shade
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jarjar
                   (car (find-files (assoc-ref inputs "java-jarjar") ".*.jar$")))
                   (injar "java-surefire-junit4.jar")
                   (outjar "java-surefire-junit4-shaded.jar"))
               (with-directory-excursion "build/jar"
                 (with-output-to-file "rules"
                   (lambda _
                     (format #t (string-append
                                  "rule "
                                  "org.apache.maven.shared.utils.** "
                                  "org.apache.maven.surefire.shade."
                                  "org.apache.maven.shared.utils.@1~%"))))
                 (invoke "java" "-jar" jarjar "process" "rules" injar outjar)
                 (delete-file injar)
                 (rename-file outjar injar)))
             #t))
         (replace 'install
           (install-from-pom "surefire-providers/surefire-junit4/pom.xml")))))
    (propagated-inputs
     (list java-junit java-surefire-parent-pom))
    (inputs
     (list java-surefire-common-junit4 java-surefire-common-junit3
           java-surefire-common-java5 java-surefire-api))
    (native-inputs
     (list java-jarjar unzip java-junit java-hamcrest-all
           java-fest-assert))
    (synopsis "SureFire JUnit 4.0+ runner")
    (description "This package contains the runner for tests run on a forked
JVM, using JUnit 4.0 or later.")))

(define-public maven-surefire-common
  (package
    (inherit java-surefire-logger-api)
    (name "maven-surefire-common")
    (arguments
     `(#:tests? #f; require mockito 2
       #:jar-name "maven-surefire-common.jar"
       #:source-dir "maven-surefire-common/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare-shade
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             (with-directory-excursion "build/classes"
               (for-each
                 (lambda (input)
                   (for-each
                     (lambda (jar-file)
                       (invoke "jar" "xf" jar-file)
                       (delete-file-recursively "META-INF"))
                     (find-files (assoc-ref inputs input) ".*.jar$")))
                 '("maven-shared-utils" "java-commons-io" "java-commons-lang3"
                   "java-commons-compress" "maven-common-artifact-filters")))
             #t))
         (add-after 'build 'shade
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jarjar
                   (car (find-files (assoc-ref inputs "java-jarjar") ".*.jar$")))
                   (injar "maven-surefire-common.jar")
                   (outjar "maven-surefire-common-shaded.jar"))
               (with-directory-excursion "build/jar"
                 (with-output-to-file "rules"
                   (lambda _
                     (format #t (string-append
                                  "rule "
                                  "org.apache.maven.shared.utils.** "
                                  "org.apache.maven.surefire.shade.common."
                                  "org.apache.maven.shared.utils.@1~%"))
                     (format #t (string-append
                                  "rule "
                                  "org.apache.commons.io.** "
                                  "org.apache.maven.surefire.shade.common."
                                  "org.apache.commons.io.@1~%"))
                     (format #t (string-append
                                  "rule "
                                  "org.apache.commons.lang3.** "
                                  "org.apache.maven.surefire.shade.common."
                                  "org.apache.commons.lang3.@1~%"))
                     (format #t (string-append
                                  "rule "
                                  "org.apache.commons.compress.** "
                                  "org.apache.maven.surefire.shade.common."
                                  "org.apache.commons.compress.@1~%"))))
                 (invoke "java" "-jar" jarjar "process" "rules" injar outjar)
                 (delete-file injar)
                 (rename-file outjar injar)))
             #t))
         (add-before 'install 'fix-pom
           (lambda _
             (substitute* "maven-surefire-common/pom.xml"
               (("maven-toolchain") "maven-core"))
             #t))
         (replace 'install
           (install-from-pom "maven-surefire-common/pom.xml")))))
    (propagated-inputs
     (list java-surefire-api
           java-surefire-extensions-api
           java-surefire-booter
           maven-core
           maven-plugin-annotations
           maven-common-artifact-filters
           maven-artifact-transfer
           java-plexus-java
           java-jansi
           java-commons-io
           java-commons-lang3
           java-commons-compress
           maven-shared-utils-3.1
           java-surefire-parent-pom))
    (inputs
     (list java-jsr305))
    (native-inputs
     (list unzip java-jarjar))
    (synopsis "API used in Surefire and Failsafe MOJO")
    (description "This package contains an API used in SureFire and Failsafe
MOJO.")))

(define-public maven-surefire-plugin
  (package
    (inherit java-surefire-logger-api)
    (name "maven-surefire-plugin")
    (arguments
     `(#:jar-name "maven-surefire-plugin.jar"
       #:source-dir "maven-surefire-plugin/src/main/java"
       #:tests? #f; test depends on maven-plugin-test-harness
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "maven-surefire-plugin/pom.xml"
             "surefire"
             "."
             (list
               (list
                 "maven-surefire-common/src/main/java/org/apache/maven/plugin/surefire/AbstractSurefireMojo.java"
                 "maven-surefire-plugin/src/main/java/org/apache/maven/plugin/surefire/SurefirePlugin.java"))))
         (replace 'install
           (install-from-pom "maven-surefire-plugin/pom.xml")))))
    (propagated-inputs
     (list maven-surefire-common maven-core))
    (native-inputs
     (list maven-plugin-annotations unzip))
    (synopsis "SureFire Maven plugin that runs tests")
    (description "The Surefire Plugin is used during the test phase of the
build lifecycle to execute the unit tests of an application.  It generates
reports in two different file formats, plain text and xml.")))

(define-public maven-jar-plugin
  (package
    (name "maven-jar-plugin")
    (version "3.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/apache/maven-jar-plugin")
                     (commit (string-append "maven-jar-plugin-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04y2rlmcabmc55ljqlkgbb5xx94a59cz1dvrnpfj1vzz024pqkyg"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-jar-plugin.jar"
       #:source-dir "src/main/java"
       #:tests? #f; test depends on maven-plugin-test-harness
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "pom.xml"
             "jar"
             "src/main/java/org/apache/maven/plugins/jar"
             (list
               (list "AbstractJarMojo.java" "JarMojo.java")
               (list "AbstractJarMojo.java" "TestJarMojo.java"))))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-archiver
           maven-3.0-artifact
           maven-3.0-core
           maven-3.0-plugin-api
           maven-file-management
           maven-shared-utils
           java-plexus-archiver
           java-plexus-utils))
    (inputs
     (list maven-plugin-annotations))
    (home-page "https://maven.apache.org/plugins/maven-jar-plugin")
    (synopsis "Jar builder plugin for Maven")
    (description "This plugin provides the capability to build jars.  If you
would like to sign jars please use the Maven Jarsigner Plugin instead.")
    (license license:asl2.0)))
