;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Tobias Geerinckx-Rite <me@tobias.gr>
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
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public java-plexus-component-metadata
  (package
    (inherit java-plexus-container-default)
    (name "java-plexus-component-metadata")
    (arguments
     `(#:jar-name "plexus-component-metadata.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "plexus-component-metadata")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources"
                               "build/classes/")
             #t)))))
    (inputs
     `(("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexu-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-core-bootstrap" ,maven-core-bootstrap)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-asm" ,java-asm)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-guava" ,java-guava)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)))
    (synopsis "Inversion-of-control container for Maven")
    (description "The Plexus project provides a full software stack for creating
and executing software projects.  Based on the Plexus container, the
applications can utilise component-oriented programming to build modular,
reusable components that can easily be assembled and reused.  This package
provides the Maven plugin generating the component metadata.")))

(define-public maven-resolver-api
  (package
    (name "maven-resolver-api")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/apache/maven-resolver.git")
                    (commit (string-append "maven-resolver-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x1gll8nkfl6zgnab78fxxvvhg42b2grxgdh1wp2h4qxsjkxg93d"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-resolver-api.jar"
       #:source-dir "maven-resolver-api/src/main/java"
       #:test-dir "maven-resolver-api/src/test"))
    (native-inputs
     `(("java-asm" ,java-asm)
       ("java-cglib" ,java-cglib)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-objenesis" ,java-objenesis)))
    (home-page "https://github.com/apache/maven-resolver")
    (synopsis "Maven repository system API")
    (description "This package contains the API for the maven repository system.")
    (license license:asl2.0)))

(define-public maven-resolver-spi
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-spi")
    (arguments
     `(#:jar-name "maven-resolver-spi.jar"
       #:source-dir "maven-resolver-spi/src/main/java"
       #:test-dir "maven-resolver-spi/src/test"
       #:jdk ,icedtea-8))
    (inputs
     `(("maven-resolver-api" ,maven-resolver-api)))
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
     `(("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)))
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
       #:jdk ,icedtea-8))
    (inputs
     `(("maven-resolver-api" ,maven-resolver-api)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("maven-resolver-test-util" ,maven-resolver-test-util)))
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
             #t)))))
    (inputs
     `(("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-util" ,maven-resolver-util)
       ("java-javax-inject" ,java-javax-inject)
       ("java-slf4j-api" ,java-slf4j-api)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("maven-resolver-test-util" ,maven-resolver-test-util)))
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
       #:jdk ,icedtea-8
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
             #t)))))
    (inputs
     `(("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-util" ,maven-resolver-util)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-guice" ,java-guice)
       ("java-guava" ,java-guava)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("jajva-aopalliance" ,java-aopalliance)
       ("java-slf4j-api" ,java-slf4j-api)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("maven-resolver-test-util" ,maven-resolver-test-util)))))

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
       ("java-plexus-component-annotation" ,java-plexus-component-annotations)
       ("java-plexus-classworld" ,java-plexus-classworlds)
       ("java-plexus-plexus-util" ,java-plexus-utils)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("maven-resolver-test-util" ,maven-resolver-test-util)
       ("java-guava" ,java-guava)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-aopalliance" ,java-aopalliance)
       ("java-guice" ,java-guice)))
    (synopsis "Transport implementation for Maven")
    (description "This package contains a transport implementation based on
Maven Wagon, for use in Maven.")))

(define-public maven-shared-utils
  (package
    (name "maven-shared-utils")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/shared/"
                                  "maven-shared-utils-" version "-source-release.zip"))
              (sha256
               (base32
                "1kzmj68wwdcznb36hm6kfz57wbavw7g1rp236pz10znkjljn6rf6"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-shared-utils.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-cyclic-dep
           (lambda _
             (delete-file
               "src/test/java/org/apache/maven/shared/utils/introspection/ReflectionValueExtractorTest.java")
             #t)))))
    (inputs
     `(("java-jansi" ,java-jansi)
       ("java-commons-io" ,java-commons-io)
       ("java-jsr305" ,java-jsr305)
       ("java-plexus-container-default" ,java-plexus-container-default)))
    (native-inputs
     `(("unzip" ,unzip)
       ("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-commons-lang3" ,java-commons-lang3)))
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
              (uri (string-append "https://archive.apache.org/dist/maven/"
                                  "plugin-tools/maven-plugin-tools-" version
                                  "-source-release.zip"))
              (sha256 (base32 "1ryqhs62j5pas93brhf5dsnvp99hxbvssf681yj5rk3r9h24hqm2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-plugin-annotations.jar"
       #:source-dir "maven-plugin-annotations/src/main/java"
       #:tests? #f))
    (inputs
     `(("maven-artifact" ,maven-artifact)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://maven.apache.org/plugin-tools/maven-plugin-annotations/")
    (synopsis "Java 5 annotations to use in Mojos")
    (description "This package contains Java 5 annotations for use in Mojos.")
    (license license:asl2.0)))

(define-public maven-wagon-provider-api
  (package
    (name "maven-wagon-provider-api")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/wagon/"
                                  "wagon-" version "-source-release.zip"))
              (sha256 (base32 "0r07j6xdzdnrvqnv8ida7dx1m05pznh5qgmcfcfpyvg9nxbj3l1n"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-wagon-provider-api.jar"
       #:source-dir "wagon-provider-api/src/main/java"
       #:test-dir "wagon-provider-api/src/test"))
    (inputs
     `(("java-plexus-utils" ,java-plexus-utils)))
    (native-inputs
     `(("unzip" ,unzip)
       ("java-junit" ,java-junit)
       ("java-easymock" ,java-easymock)))
    (home-page "https://maven.apache.org/wagon")
    (synopsis "Transport abstraction for Maven")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.")
    (license license:asl2.0)))

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
     `(("java-plexus-utils" ,java-plexus-utils)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
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
       ("java-asm" ,java-asm)
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
       ("java-tomcat" ,java-tomcat)
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
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-httpcomponents-httpclient" ,java-httpcomponents-httpclient)
       ("java-httpcomponents-httpcore" ,java-httpcomponents-httpcore)
       ("java-commons-io" ,java-commons-io)
       ("java-jsoup" ,java-jsoup)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
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
       ("java-asm" ,java-asm)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ,@(package-native-inputs maven-wagon-provider-api)))
    (synopsis "Shared Library for wagon providers supporting HTTP.")
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
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-httpcomponents-httpclient" ,java-httpcomponents-httpclient)
       ("java-httpcomponents-httpcore" ,java-httpcomponents-httpcore)
       ("maven-wagon-http-shared" ,maven-wagon-http-shared)
       ("maven-wagon-tck-http" ,maven-wagon-tck-http)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
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
       ("java-asm" ,java-asm)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-tomcat" ,java-tomcat)
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

(define-public maven-artifact
  (package
    (name "maven-artifact")
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/"
                                  "maven-3/" version "/source/"
                                  "apache-maven-" version "-src.tar.gz"))
              (sha256 (base32 "17jrqfqwn569jgnv8m4pqc27csssb0rf6mznpq61l5bnbd6hl75k"))
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
     `(#:jar-name "maven-artifact.jar"
       #:source-dir "maven-artifact/src/main/java"
       #:test-dir "maven-artifact/src/test"
       #:main-class "org.apache.maven.artifact.versioning.ComparableVersion"))
    (inputs
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-commons-lang3" ,java-commons-lang3)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://maven.apache.org/")
    (synopsis "Build system")
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains the Maven Artifact classes, providing the
@code{Artifact} interface, with its @code{DefaultArtifact} implementation.  The
jar file is executable and provides a little tool to display how Maven parses
and compares versions:")
    (license license:asl2.0)))

(define-public maven-model
  (package
    (inherit maven-artifact)
    (name "maven-model")
    (arguments
     `(#:jar-name "maven-model.jar"
       #:source-dir "maven-model/src/main/java"
       #:test-dir "maven-model/src/test"
       #:jdk ,icedtea-8
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
               (modello-single-mode file "4.0.0" "xpp3-writer")
               (modello-single-mode file "4.0.0" "xpp3-extended-reader"))
             #t)))))
    (inputs
     `(("java-commons-lang3" ,java-commons-lang3)
       ("java-plexus-utils" ,java-plexus-utils)))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ;; for modello:
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ;; modello plugins:
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
       ;; for tests
       ("java-junit" ,java-junit)))
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
             #t)))))
    (inputs
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-commons-lang3" ,java-commons-lang3)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
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
       #:jdk ,icedtea-8
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
               (modello-single-mode file "1.1.0" "java")
               (modello-single-mode file "1.1.0" "xpp3-reader")
               (modello-single-mode file "1.1.0" "xpp3-writer"))
             #t)))))
    (inputs '())
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ;; for modello:
       ;("container" ,java-plexus-container-default)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-guava" ,java-guava)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ;; modello plugins:
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)))
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
         (add-before 'build 'generate-components.xml
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (chmod "components.sh" #o755)
             (invoke "./components.sh" "maven-settings-builder/src/main/java"
                     "build/classes/META-INF/plexus/components.xml")
             #t)))))
    (inputs
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-interpolation" ,java-plexus-interpolation)
       ("java-plexus-sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-settings" ,maven-settings)
       ("java-commons-lang3" ,java-commons-lang3)))
    (native-inputs
     `(("java-junit" ,java-junit)))
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
         (add-before 'build 'generate-components.xml
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (chmod "components.sh" #o755)
             (invoke "./components.sh" "maven-model-builder/src/main/java"
                     "build/classes/META-INF/plexus/components.xml")
             #t))
         (add-before 'check 'fix-paths
           (lambda _
             (substitute* (find-files "maven-model-builder/src/test/java" ".*.java")
               (("src/test") "maven-model-builder/src/test"))
             #t)))))
    (inputs
     `(("model" ,maven-model)
       ("artifact" ,maven-artifact)
       ("support" ,maven-builder-support)
       ("annotations" ,java-plexus-component-annotations)
       ("utils" ,java-plexus-utils)
       ("interpolation" ,java-plexus-interpolation)
       ("lang3" ,java-commons-lang3)
       ("guava" ,java-guava)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
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
             #t)))))
    (inputs '())
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
       ("java-sisu-build-api" ,java-sisu-build-api)
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
             #t)))))
    (inputs
     `(("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-impl" ,maven-resolver-impl)
       ("maven-resolver-util" ,maven-resolver-util)
       ("maven-model" ,maven-model)
       ("maven-model-builder" ,maven-model-builder)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-repository-metadata" ,maven-repository-metadata)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-guice" ,java-guice)
       ("java-guava" ,java-guava)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)))))

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
             #t)))))
    (inputs
     `(("maven-artifact" ,maven-artifact)
       ("maven-model" ,maven-model)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("javax-inject" ,java-javax-inject)
       ("utils" ,java-plexus-utils)))
    (native-inputs
     `(("modello" ,java-modello-core)
       ;; for modello:
       ("classworlds" ,java-plexus-classworlds)
       ("guava" ,java-guava)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("build-api" ,java-sisu-build-api)
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

(define maven-core-bootstrap
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
             #t)))))
    (inputs
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
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
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
       ("java-asm" ,java-asm)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
       ;; tests
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-commons-jxpath" ,java-commons-jxpath)))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains the maven core classes managing the whole build
process.")))

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
                  (("message=\"\"") "message=\"Implementation-Version: 3.5.4\n\""))
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
            (add-after 'generate-metadata 'rebuild
              (lambda _
                (invoke "ant" "jar")
                #t))))))
    (native-inputs
     `(("java-plexus-component-metadata" ,java-plexus-component-metadata)
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
             #t)))))
    (inputs
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
       ("java-slf4j-simple" ,java-slf4j-simple)))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
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
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t)))))
    (inputs
     `(("maven-artifact" ,maven-artifact)
       ("maven-repository-metadata" ,maven-repository-metadata)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-model" ,maven-model)
       ("maven-model-builder" ,maven-model-builder)
       ("maven-settings" ,maven-settings)
       ("maven-settings-builder" ,maven-settings-builder)
       ("maven-core" ,maven-core)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)
       ("maven-wagon-file" ,maven-wagon-file)
       ("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-util" ,maven-resolver-util)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("java-plexus-interpolation" ,java-plexus-interpolation)))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-sisu-build-api" ,java-sisu-build-api)
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
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
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
                     (format #t "load       ~a/share/java/*.jar~%"
                             (assoc-ref inputs dependency)))
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
     `(("java-plexus-classworlds" ,java-plexus-classworlds)
       ("maven-artifact" ,maven-artifact)
       ("maven-embedder" ,maven-embedder)
       ("maven-core" ,maven-core)
       ("maven-compat" ,maven-compat)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-model" ,maven-model)
       ("maven-model-builder" ,maven-model-builder)
       ("maven-settings" ,maven-settings)
       ("maven-settings-builder" ,maven-settings-builder)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-repository-metadata" ,maven-repository-metadata)
       ("maven-shared-utils" ,maven-shared-utils)
       ("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-util" ,maven-resolver-util)
       ("maven-resolver-impl" ,maven-resolver-impl)
       ("maven-resolver-connector-basic" ,maven-resolver-connector-basic)
       ("maven-resolver-provider" ,maven-resolver-provider)
       ("maven-resolver-transport-wagon" ,maven-resolver-transport-wagon)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)
       ("maven-wagon-file" ,maven-wagon-file)
       ("maven-wagon-http" ,maven-wagon-http)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-httpcomponents-httpclient" ,java-httpcomponents-httpclient)
       ("java-httpcomponents-httpcore" ,java-httpcomponents-httpcore)
       ("maven-wagon-http-shared" ,maven-wagon-http-shared)
       ("maven-wagon-tck-http" ,maven-wagon-tck-http)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-guice" ,java-guice)
       ("java-aopalliance" ,java-aopalliance)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-interpolation" ,java-plexus-interpolation)
       ("java-plexus-sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("java-plexus-cipher" ,java-plexus-cipher)
       ("java-guava" ,java-guava)
       ("java-jansi" ,java-jansi)
       ("java-jsr250" ,java-jsr250)
       ("java-cdi-api" ,java-cdi-api)
       ("java-commons-cli" ,java-commons-cli)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-slf4j-api" ,java-slf4j-api)
       ;; TODO: replace with maven-slf4j-provider
       ("java-slf4j-simple" ,java-slf4j-simple)))
    (propagated-inputs
     `(("coreutils" ,coreutils)
       ("which" ,which)))
    (description "Apache Maven is a software project management and comprehension
tool.  Based on the concept of a project object model: builds, dependency
management, documentation creation, site publication, and distribution
publication are all controlled from the @file{pom.xml} declarative file.  Maven
can be extended by plugins to utilise a number of other development tools for
reporting or the build process.")))
