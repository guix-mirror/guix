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
  #:use-module (gnu packages java))

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
