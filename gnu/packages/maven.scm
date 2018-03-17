;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
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
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/maven-resolver/"
                                  "archive/maven-resolver-" version ".tar.gz"))
              (sha256
               (base32
                "0rpvdg3qr1j88gw0ankf0wnwfyq6238mdlm7s39vf5jrcvhdgwcl"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-resolver-api.jar"
       #:source-dir "maven-resolver-api/src/main/java"
       #:test-dir "maven-resolver-api/src/test"))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
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
                 (display "org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory\n"))))))))
    (inputs
     `(("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-util" ,maven-resolver-util)
       ("java-javax-inject" ,java-javax-inject)))
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
                     "org.eclipse.aether.internal.impl.DefaultTransporterProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultUpdatePolicyAnalyzer\n"
                     "org.eclipse.aether.internal.impl.slf4j.Slf4jLoggerFactory\n"
                     "org.eclipse.aether.internal.impl.DefaultRepositorySystem\n"
                     "org.eclipse.aether.internal.impl.LoggerFactoryProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultFileProcessor\n"
                     "org.eclipse.aether.internal.impl.DefaultLocalRepositoryProvider\n"
                     "org.eclipse.aether.internal.impl.SimpleLocalRepositoryManagerFactory\n"
                     "org.eclipse.aether.internal.impl.DefaultDeployer\n"
                     "org.eclipse.aether.internal.impl.DefaultMetadataResolver\n"
                     "org.eclipse.aether.internal.impl.DefaultInstaller\n"
                     "org.eclipse.aether.internal.impl.Maven2RepositoryLayoutFactory\n"
                     "org.eclipse.aether.internal.impl.DefaultSyncContextFactory\n"
                     "org.eclipse.aether.internal.impl.DefaultOfflineController\n"
                     "org.eclipse.aether.internal.impl.EnhancedLocalRepositoryManagerFactory\n"
                     "org.eclipse.aether.internal.impl.DefaultRepositoryLayoutProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultRemoteRepositoryManager\n"
                     "org.eclipse.aether.internal.impl.DefaultRepositoryEventDispatcher\n"
                     "org.eclipse.aether.internal.impl.DefaultRepositoryConnectorProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultUpdateCheckManager\n"
                     "org.eclipse.aether.internal.impl.DefaultChecksumPolicyProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultDependencyCollector\n"))))
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
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/wagon/"
                                  "wagon-" version "-source-release.zip"))
              (sha256 (base32 "1qb0q4m7vmf290xp3fnfdi3pwl3hkskia5g3z2v82q1ch3y2knqv"))))
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

(define-public maven-artifact
  (package
    (name "maven-artifact")
    (version "3.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/"
                                  "maven-3/" version "/source/"
                                  "apache-maven-" version "-src.tar.gz"))
              (sha256 (base32 "06by23fz207lkvsndq883irfcf4p77jzkgf7n2q7hzyw1hs4h5s7"))
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
       ("xmlunit" ,java-xmlunit)
       ("xmlunit" ,java-xmlunit-legacy)
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
                  (("message=\"\"") "message=\"Implementation-Version: 3.5.3\n\""))
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
