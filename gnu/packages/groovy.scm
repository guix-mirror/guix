;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Tobias Geerinck-Rice <me@tobias.gr>
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

(define-module (gnu packages groovy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages java)
  #:use-module (gnu packages xml))

(define java-groovy-bootstrap
  (package
    (name "java-groovy-bootstrap")
    (version "3.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/groovy")
             (commit (string-append
                      "GROOVY_"
                      (string-map (lambda (x) (if (eq? x #\.) #\_ x))
                                  version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00556qxjmcn3a3xhfy6n0zw3d69mnw72vzm2rb6n4ihzkk7579nm"))
       (patches
        (search-patches "groovy-add-exceptionutilsgenerator.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "groovy.jar"
       #:source-dir "src/main/java:src/main/antlr2:subprojects/parser-antlr4/src/main/java:src/antlr"
       #:test-dir "src/test"
       #:tests? #f
       #:jdk ,openjdk9
       #:main-class "groovy.ui.GroovyMain"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-parsers
           (lambda _
             (with-directory-excursion "src/main/antlr2/org/codehaus/groovy/antlr/java"
               (invoke "antlr" "java.g"))
             (with-directory-excursion "src/main/antlr2/org/codehaus/groovy/antlr"
               (mkdir "parser")
               (with-directory-excursion "parser"
                 (invoke "antlr" "../groovy.g")))
             (invoke "antlr4" "-lib" "src/antlr"
                     "-package" "org.apache.groovy.parser.antlr4"
                     "-visitor" "-no-listener" "src/antlr/GroovyLexer.g4")
             (invoke "antlr4" "-lib" "src/antlr"
                     "-package" "org.apache.groovy.parser.antlr4"
                     "-visitor" "-no-listener" "src/antlr/GroovyParser.g4")
             #t))
         (add-before 'build 'generate-exception-utils
           (lambda _
             (invoke "javac" "-cp" (getenv "CLASSPATH")
                     "-source" "1.8" "-target" "1.8"
                     "config/ant/src/org/codehaus/groovy/ExceptionUtilsGenerator.java")
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                 ":config/ant/src")
                     "org.codehaus.groovy.ExceptionUtilsGenerator"
                     "build/classes/org/codehaus/groovy/runtime/ExceptionUtils.class")
             #t))
         (add-before 'build 'set-source-level
           (lambda _
             (substitute* "build.xml"
               (("<javac") "<javac source=\"1.8\" target=\"1.8\""))
             #t)))))
    (native-inputs
     `(("antlr2" ,antlr2)
       ("antlr4" ,java-tunnelvisionlabs-antlr4)
       ("java-jsr305" ,java-jsr305)
       ("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-jmock-1" ,java-jmock-1)
       ("java-xmlunit-legacy" ,java-xmlunit-legacy)))
    (inputs
     `(("java-antlr4-runtime" ,java-tunnelvisionlabs-antlr4-runtime)
       ("java-antlr4-runtime-annotations"
        ,java-tunnelvisionlabs-antlr4-runtime-annotations)
       ("java-asm" ,java-asm-8)
       ("java-asm-util" ,java-asm-util-8)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-commons-cli" ,java-commons-cli)
       ("java-jansi" ,java-jansi-1)
       ("java-jline-2" ,java-jline-2)
       ("java-picocli" ,java-picocli)
       ("java-xstream" ,java-xstream)))
    (home-page "http://groovy-lang.org/")
    (synopsis "Groovy's java bootstrap")
    (description "This package contains the java bootstrap that is used to build
groovy submodules.")
    (license license:asl2.0)))

(define groovy-bootstrap
  (package
    (inherit java-groovy-bootstrap)
    (name "groovy-bootstrap")
    (arguments
     `(#:jar-name "groovy.jar"
       #:jdk ,openjdk9
       ;Requires groovy-xml and logback-classic which are circular dependencies
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-parser
           (lambda _
             (with-directory-excursion "src/main/antlr2/org/codehaus/groovy/antlr/java"
               (invoke "antlr" "java.g"))
             (with-directory-excursion "src/main/antlr2/org/codehaus/groovy/antlr"
               (mkdir "parser")
               (with-directory-excursion "parser"
                 (invoke "antlr" "../groovy.g")))
             (invoke "antlr4" "-lib" "src/antlr"
                     "-package" "org.apache.groovy.parser.antlr4"
                     "-visitor" "-no-listener" "src/antlr/GroovyLexer.g4")
             (invoke "antlr4" "-lib" "src/antlr"
                     "-package" "org.apache.groovy.parser.antlr4"
                     "-visitor" "-no-listener" "src/antlr/GroovyParser.g4")
             #t))
         (add-before 'build 'generate-exception-utils
           (lambda _
             (invoke "javac" "-cp" (getenv "CLASSPATH")
                     "config/ant/src/org/codehaus/groovy/ExceptionUtilsGenerator.java")
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                 ":config/ant/src")
                     "org.codehaus.groovy.ExceptionUtilsGenerator"
                     "target/classes/org/codehaus/groovy/runtime/ExceptionUtils.class")
             #t))
         (add-before 'build 'generate-dgminfo
           (lambda _
             (mkdir-p "target/classes/org/codehaus/groovy/runtime")
             (mkdir-p "target/classes/META-INF")
             (invoke "javac" "-cp" (getenv "CLASSPATH")
                     "src/main/java/org/codehaus/groovy/tools/DgmConverter.java")
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                 ":src/main/java")
                     "org.codehaus.groovy.tools.DgmConverter")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (with-directory-excursion "src/main/java"
               (for-each (lambda (file)
                           (mkdir-p (string-append "../../../target/classes/"
                                                   (dirname file)))
                           (copy-file file
                                      (string-append "../../../target/classes/"
                                                     file)))
                  (find-files "." ".*.(txt|properties|xml|html)")))
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                           "org.codehaus.groovy.tools.FileSystemCompiler"
                           "-cp" (getenv "CLASSPATH")
                           "-d" "target/classes"
                           "-j"; joint compilation
                           (find-files "src/main"
                                       ".*\\.(groovy|java)$"))
             (invoke "jar" "-cf" "build/jar/groovy.jar"
                     "-C" "target/classes" ".")
             #t)))))
    (inputs
     `(("java-apache-ivy" ,java-apache-ivy)
       ,@(package-inputs java-groovy-bootstrap)))
    (native-inputs
     `(("java-groovy-bootstrap" ,java-groovy-bootstrap)
       ,@(package-native-inputs java-groovy-bootstrap)))
    (synopsis "Groovy compiler")
    (description "This package contains the first version of the Groovy compiler.
Although already usable, it doesn't contain the groovy library yet.  This package
is used to build the groovy submodules written in groovy.")))

(define groovy-tests-bootstrap
  (package
    (inherit groovy-bootstrap)
    (name "groovy-tests-bootstrap")
    (arguments
     `(#:jar-name "groovy-tests-bootstrap.jar"
       #:jdk ,openjdk9
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-cp" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    "-j"; joint compilation
                    (append
                      (find-files "src/test" "TestSupport.java")
                      (find-files "src/test" "HeadlessTestSupport.java")
                      (find-files "src/test" "AstAssert.groovy")
                      (find-files "src/test" "XmlAssert.java")))
             (invoke "jar" "-cf" "build/jar/groovy-tests-bootstrap.jar"
                     "-C" "build/classes" ".")
             #t)))))
    (inputs
     `(("groovy-test" ,groovy-test)
       ("groovy-parser-antlr4" ,groovy-parser-antlr4)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ,@(package-native-inputs java-groovy-bootstrap)))
    (synopsis "Groovy test classes")
    (description "This package contains three classes required for testing
other groovy submodules.")))

(define (groovy-subproject name)
  (package
    (inherit groovy-bootstrap)
    (name name)
    (arguments
     `(#:jar-name ,(string-append name ".jar")
       #:test-dir ,(string-append name "/src/test")
       #:test-include (list "**/*Test.java" "**/*.groovy")
       #:jdk ,openjdk9
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-cp" (getenv "CLASSPATH")
                    "-d" "build/classes" "-j"
                    (append
                      (find-files ,(string-append "subprojects/" name "/src/main/java")
                        ".*\\.(groovy|java)$")
                      (find-files ,(string-append "subprojects/" name "/src/main/groovy")
                        ".*\\.(groovy|java)$")))
             (invoke "jar" "-cf" ,(string-append "build/jar/" name ".jar")
                     "-C" "build/classes" ".")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (let ((resource-dir ,(string-append "subprojects/" name
                                                 "/src/main/resources")))
               (when (file-exists? resource-dir)
                 (copy-recursively resource-dir "build/classes")))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (mkdir-p "build/test-classes")
               (substitute* "build.xml"
                 (("depends=\"compile-tests\"") "depends=\"\"")
                 (("}/java") "}/"))
               (apply invoke "java" "-cp"
                      (string-append (getenv "CLASSPATH") ":build/classes")
                      "org.codehaus.groovy.tools.FileSystemCompiler" "-cp"
                      (string-append (getenv "CLASSPATH") ":build/classes")
                      "-d" "build/test-classes" "-j"
                      (append
                        (find-files ,(string-append "subprojects/" name "/src/test/java")
                                    ".*\\.(groovy|java)$")
                        (find-files ,(string-append "subprojects/" name "/src/test/groovy")
                                    ".*\\.(groovy|java)$")))
               (invoke "ant" "check"))
             #t)))))))

(define groovy-parser-antlr4
  (let ((base (groovy-subproject "parser-antlr4")))
    (package
      (inherit base)
      (name "groovy-parser-antlr4")
      (arguments
       `(#:tests? #f
         ,@(substitute-keyword-arguments (package-arguments base)
            ((#:phases phases)
             `(modify-phases ,phases
                (add-before 'build 'generate-parser
                  (lambda _
                    (invoke "antlr4" "-lib" "src/antlr"
                            "-package" "org.apache.groovy.parser.antlr4"
                            "-visitor" "-no-listener" "src/antlr/GroovyLexer.g4")
                    (invoke "antlr4" "-lib" "src/antlr"
                            "-package" "org.apache.groovy.parser.antlr4"
                            "-visitor" "-no-listener" "src/antlr/GroovyParser.g4")
                    (for-each
                      (lambda (file)
                        (install-file file
                                      "subprojects/parser-antlr4/src/main/java/org/apache/groovy/parser/antlr4"))
                      (find-files "src/antlr" ".*.java$"))
                    #t)))))))
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ,@(package-native-inputs java-groovy-bootstrap)))
      (synopsis "Groovy antlr4 parser submodule")
      (description "This package contains the new parser Parrot for Groovy, which
is based on Antlr4.  The new parser can parse Groovy source code and construct
the related AST, which is almost identical to the one generated by the old
parser.  Currently all features of Groovy are available."))))

(define groovy-test
  (let ((base (groovy-subproject "groovy-test")))
    (package
      (inherit base)
      (arguments
        `(;#:tests? #f
          ,@(package-arguments base)))
      (synopsis "Groovy test submodule")
      (description "This package contains the test submodules used to test
other groovy submodules."))))

(define groovy-xml
  (let ((base (groovy-subproject "groovy-xml")))
    (package
      (inherit base)
      (native-inputs
       `(("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs base)))
      (inputs
       `(("groovy-parser-antlr4" ,groovy-parser-antlr4)
         ,@(package-inputs base)))
      (synopsis "Groovy XML")
      (description "This package contains XML-related utilities for groovy."))))

(define groovy-templates
  (let ((base (groovy-subproject "groovy-templates")))
    (package
      (inherit base)
      (arguments
       `(#:tests? #f;Requires spock-framework which is a circular dependency
         ,@(substitute-keyword-arguments (package-arguments base)
             ((#:phases phases)
              `(modify-phases ,phases
                 ;; These annotations are used for QA, but do not affect build output.
                 ;; They require findbugs, which we don't have yet.
                 (add-before 'build 'remove-annotation
                   (lambda _
                     (substitute* '("subprojects/groovy-templates/src/main/groovy/groovy/text/StreamingTemplateEngine.java"
                                    "subprojects/groovy-templates/src/main/groovy/groovy/text/TemplateEngine.java")
                       (("import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;") "")
                       (("@SuppressFBWarnings.*") ""))
                     #t)))))))
      (inputs
       `(("groovy-xml" ,groovy-xml)
         ,@(package-inputs base)))
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs base)))
      (synopsis "Groovy template engine")
      (description "This package contains a template framework which is
well-suited to applications where the text to be generated follows the form of
a static template."))))

(define groovy-groovydoc
  (let ((base (groovy-subproject "groovy-groovydoc")))
    (package
      (inherit base)
      (arguments
       `(#:tests? #f; Requires groovy-ant which is a circular dependency
         ,@(package-arguments base)))
      (inputs
       `(("groovy-templates" ,groovy-templates)
         ("groovy-parser-antlr4" ,groovy-parser-antlr4)
         ("java-javaparser" ,java-javaparser)
         ,@(package-inputs groovy-bootstrap)))
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs java-groovy-bootstrap)))
      (synopsis "Groovy documentation generation")
      (description "This package contains the groovy documentation generator,
similar to javadoc."))))

(define groovy-ant
  (let ((base (groovy-subproject "groovy-ant")))
    (package
      (inherit base)
      (arguments
       `(;#:tests? #f;Requires spock-framework which is a circular dependency
         #:ant ,ant/java8; ant is actually a dependency of this package, and we need 1.10
         ,@(substitute-keyword-arguments (package-arguments base)
             ((#:phases phases)
              `(modify-phases ,phases
                 ;; These annotations are used for QA, but do not affect build output.
                 ;; They require findbugs, which we don't have yet.
                 (add-before 'build 'remove-annotation
                   (lambda _
                     (substitute* (find-files "subprojects/groovy-ant"
                                              ".*.java$")
                       (("import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;") "")
                       (("@SuppressFBWarnings.*") ""))
                     #t)))))))
      (inputs
       `(("groovy-groovydoc" ,groovy-groovydoc)
         ("java-asm-tree" ,java-asm-tree-8)
         ("java-asm-analysis" ,java-asm-analysis-8)
         ,@(package-inputs base)))
      (native-inputs
       `(("ant-junit" ,ant-junit)
         ("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-xml" ,groovy-xml)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs base)))
      (synopsis "Groovy ant tasks")
      (description "This package contains groovy-related ant tasks definitions."))))

(define groovy-astbuilder
  (let ((base (groovy-subproject "groovy-astbuilder")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'copy-resources)
             (add-after 'build 'copy-resources
               (lambda _
                 (copy-recursively
                   "subprojects/groovy-astbuilder/src/main/resources"
                   "build/classes")
                 (substitute* "build.xml"
                   (("depends=\"compile,") "depends=\""))
                 (invoke "ant" "jar")
                 #t))))))
      (inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ,@(package-inputs base)))
      (native-inputs
       `(("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs base)))
      (synopsis "Transformation to capture ASTBuilder from code statements")
      (description "This package contains an AST transformation for use with
ASTBuilder when building \"from string\" Groovy statements."))))

(define groovy-bsf
  (let ((base (groovy-subproject "groovy-bsf")))
    (package
      (inherit base)
      (arguments
       `(#:test-exclude (list
;; exception from Groovy: org.codehaus.groovy.runtime.InvokerInvocationException:
;; groovy.lang.MissingMethodException: No signature of method:
;; java.util.ArrayList.each() is applicable for argument types:
;; (groovy.script.MapFromList$_doit_closure1) values:
;; [groovy.script.MapFromList$_doit_closure1@17e554d5]
                        "**/BSFTest.java")
         ,@(package-arguments base)))
      (inputs
       `(("java-commons-bsf" ,java-commons-bsf)
         ,@(package-inputs base)))
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ("java-commons-logging-minimal" ,java-commons-logging-minimal)
         ,@(package-native-inputs base)))
      (synopsis "Groovy BSF engine")
      (description "This package defines the BSF engine for using Groovy inside
any @dfn{Bean Scripting Framework} (BSF) application."))))

(define groovy-cli-commons
  (let ((base (groovy-subproject "groovy-cli-commons")))
    (package
      (inherit base)
      (inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ,@(package-inputs base)))
      (native-inputs
       `(("groovy-test" ,groovy-test)
         ,@(package-native-inputs base)))
      (synopsis "Groovy CLI common classes")
      (description "This package defines common classes for dealing with
command-line arguments in Groovy."))))

(define groovy-cli-picocli
  (let ((base (groovy-subproject "groovy-cli-picocli")))
    (package
      (inherit base)
      (inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("java-picocli" ,java-picocli)
         ,@(package-inputs base)))
      (native-inputs
       `(("groovy-test" ,groovy-test)
         ,@(package-native-inputs base)))
      (synopsis "Groovy CLI classes that use picocli")
      (description "This package defines classes for dealing with command-line
arguments in Groovy using the picocli library."))))

(define groovy-swing
  (let ((base (groovy-subproject "groovy-swing")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'check 'fix-test
               (lambda _
                 (substitute*
                   "subprojects/groovy-swing/src/test/groovy/groovy/swing/GroovySwingTestCase.groovy"
                   (("HeadlessTestSupport.headless") "isHeadless()"))
                 #t))))))
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ("java-commons-logging-minimal" ,java-commons-logging-minimal)
         ,@(package-native-inputs base)))
      (synopsis "Groovy graphical library")
      (description "This package contains the groovy bindings to Java Swing, a
library used to build graphical interfaces."))))

(define groovy-console
  (let ((base (groovy-subproject "groovy-console")))
    (package
      (inherit base)
      (arguments
         (substitute-keyword-arguments (package-arguments base)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-before 'check 'build-swing
                 (lambda _
                   (substitute*
                     "subprojects/groovy-swing/src/test/groovy/groovy/swing/GroovySwingTestCase.groovy"
                     (("HeadlessTestSupport.headless") "isHeadless()"))
                   (mkdir-p "build/test-classes")
                   (apply invoke "java" "-cp"
                          (string-append (getenv "CLASSPATH") ":build/classes")
                          "org.codehaus.groovy.tools.FileSystemCompiler" "-cp"
                          (string-append (getenv "CLASSPATH") ":build/classes")
                          "-d" "build/test-classes" "-j"
                          (append
                            (find-files "subprojects/groovy-swing/src/test/java"
                                        ".*\\.(groovy|java)$")
                            (find-files "subprojects/groovy-swing/src/test/groovy"
                                        ".*\\.(groovy|java)$")))
                   #t))))))
      (inputs
       `(("groovy-swing" ,groovy-swing)
         ("groovy-templates" ,groovy-templates)
         ,@(package-inputs base)))
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ("java-commons-logging-minimal" ,java-commons-logging-minimal)
         ,@(package-native-inputs base)))
      (synopsis "Groovy graphical interface")
      (description "This package contains a graphical interface to run groovy."))))

(define groovy-datetime
  (let ((base (groovy-subproject "groovy-datetime")))
    (package
      (inherit base)
      (native-inputs
       `(("groovy-test" ,groovy-test)
         ,@(package-native-inputs base)))
      (synopsis "Date/Time API for Groovy")
      (description "This package defines new Groovy methods which appear on
normal JDK Date/Time API (@code{java.time}) classes inside the Groovy
environment."))))

(define groovy-dateutil
  (let ((base (groovy-subproject "groovy-dateutil")))
    (package
      (inherit base)
      (native-inputs
       `(("groovy-test" ,groovy-test)
         ,@(package-native-inputs base)))
      (synopsis "Date and Calendar API for Groovy")
      (description "This package defines new groovy methods which appear on
normal JDK Date and Calendar classes inside the Groovy environment."))))

(define groovy-docgenerator
  (let ((base (groovy-subproject "groovy-docgenerator")))
    (package
      (inherit base)
      (arguments
       `(#:tests? #f; No tests
         ,@(package-arguments base)))
      (inputs
       `(("groovy-templates" ,groovy-templates)
         ("groovy-swing" ,groovy-swing)
         ("java-qdox-1.12" ,java-qdox-1.12)
         ,@(package-inputs base)))
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs base)))
      (synopsis "Groovy documentation generation")
      (description "This package contains a command line tool to generate
documentation for groovy applications."))))

(define groovy-groovysh
  (let ((base (groovy-subproject "groovy-groovysh")))
    (package
      (inherit base)
      (inputs
       `(("groovy-xml" ,groovy-xml)
         ("groovy-console" ,groovy-console)
         ,@(package-inputs base)))
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs base)))
      (synopsis "Groovy REPL")
      (description "This package contains the Groovy REPL."))))

(define groovy-jmx
  (let ((base (groovy-subproject "groovy-jmx")))
    (package
      (inherit base)
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs base)))
      (synopsis "Groovy JMX extension")
      (description "This package contains the JMX extension of Groovy, for
management and monitoring of JVM-based solutions."))))

(define groovy-json
  (let ((base (groovy-subproject "groovy-json")))
    (package
      (inherit base)
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs base)))
      (synopsis "Groovy JSON")
      (description "This package contains JSON-related utilities for groovy."))))

(define groovy-jsr223
  (let ((base (groovy-subproject "groovy-jsr223")))
    (package
      (inherit base)
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs base)))
      (synopsis "Groovy's own JSR223 implementation")
      (description "This package contains Groovy's own JSR223 implementation.  This
module is used for interaction between Groovy and Java code."))))

(define groovy-nio
  (let ((base (groovy-subproject "groovy-nio")))
    (package
      (inherit base)
      (arguments
       `(#:tests? #f; Require spock-framework
         ,@(package-arguments base)))
      (synopsis "Groovy input-output library")
      (description "This package implements an input/output library that extends
the functionality of the common library of Java."))))

(define groovy-servlet
  (let ((base (groovy-subproject "groovy-servlet")))
    (package
      (inherit base)
      (inputs
       `(("groovy-templates" ,groovy-templates)
         ("groovy-xml" ,groovy-xml)
         ,@(package-inputs base)))
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-json" ,groovy-json)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs base)))
      (synopsis "Groovy's servlet implementation")
      (description "This package contains a library to create groovlets, Groovy's
version of Java servlets."))))

(define groovy-sql
  (let ((base (groovy-subproject "groovy-sql")))
    (package
      (inherit base)
      (arguments
       `(#:tests? #f;TODO: Requires hsqldb
         ,@(package-arguments base)))
      (synopsis "Groovy SQL library")
      (description "This package contains a facade over Java's normal JDBC APIs
providing greatly simplified resource management and result set handling."))))

(define groovy-testng
  (let ((base (groovy-subproject "groovy-testng")))
    (package
      (inherit base)
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs base)))
      (synopsis "Groovy testing framework")
      (description "This package contains integration code for running TestNG
tests in Groovy."))))

(define groovy-macro
  (let ((base (groovy-subproject "groovy-macro")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'copy-resources)
             (add-after 'build 'copy-resources
               (lambda _
                 (copy-recursively "subprojects/groovy-macro/src/main/resources"
                                   "build/classes")
                 (substitute* "build.xml"
                   (("depends=\"compile,") "depends=\""))
                 (invoke "ant" "jar")
                 #t))))))
      (inputs
       `(("groovy-templates" ,groovy-templates)
         ("groovy-xml" ,groovy-xml)
         ,@(package-inputs base)))
      (native-inputs
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ("groovy-json" ,groovy-json)
         ("groovy-test" ,groovy-test)
         ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
         ,@(package-native-inputs base)))
      (synopsis "Groovy macro processor")
      (description "This package contains a high-level library to create macro
and modify groovy's @dfn{Abstract Syntax Tree} (AST)."))))

(define groovy-yaml
  (let ((base (groovy-subproject "groovy-yaml")))
    (package
      (inherit base)
      (inputs
       `(("groovy-json" ,groovy-json)
         ("java-fasterxml-jackson-annotations" ,java-fasterxml-jackson-annotations)
         ("java-fasterxml-jackson-core" ,java-fasterxml-jackson-core)
         ("java-fasterxml-jackson-databind" ,java-fasterxml-jackson-databind)
         ("java-fasterxml-jackson-dataformat-yaml" ,java-fasterxml-jackson-dataformat-yaml)
         ,@(package-inputs base)))
      (native-inputs
       `(("groovy-test" ,groovy-test)
         ,@(package-native-inputs base)))
      (synopsis "Groovy YAML")
      (description "This package contains YAML-related utilities for groovy."))))

(define-public groovy
  (package
    (inherit groovy-bootstrap)
    (name "groovy")
    (arguments
     `(#:tests? #f; No tests
       #:jdk ,openjdk9
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin"))
                    (out-lib (string-append out "/lib")))
               (with-directory-excursion "src/bin"
                 (substitute* "startGroovy"
                   (("\"\\\\\"") "\"")
                   (("\\\\\"\"") "\"")
                   (("\\\\\\$") "$")
                   (("@GROOVYJAR@") "groovy.jar")
                   (("MAX_FD=\"maximum\"")
                    (string-append
                      "MAX_FD=\"maximum\"\nJAVA_HOME="
                      (assoc-ref inputs "jdk"))))
                 ;; Groovy uses class loading. It's not enough to put the class
                 ;; in the loader's classpath, as it causes breakages:
                 ;; the compiler would give this error:
                 ;; "Prohibited package name: java.lang"
                 ;; So we symlink dependencies in this package's output. The
                 ;; starter class (in groovy-bootstrap) is where the class loader
                 ;; will look for dependencies, so we put it there too.
                 (mkdir-p out-lib)
                 (for-each
                   (lambda (input)
                     (for-each
                       (lambda (jar)
                         (symlink jar (string-append out-lib "/" (basename jar))))
                       (find-files (assoc-ref inputs input) ".*.jar")))
                   '("groovy-bootstrap" "groovy-ant" "groovy-astbuilder"
                     "groovy-bsf" "groovy-cli-commons" "groovy-cli-picocli"
                     "groovy-console" "groovy-datetime" "groovy-dateutil"
                     "groovy-docgenerator" "groovy-groovydoc" "groovy-groovysh"
                     "groovy-jmx" "groovy-json" "groovy-jsr223" "groovy-nio"
                     "groovy-parser-antlr4" "groovy-servlet" "groovy-sql"
                     "groovy-swing" "groovy-templates" "groovy-testng"
                     "groovy-xml" "groovy-yaml" "java-commons-cli"
                     "java-tunnelvisionlabs-antlr4-runtime" "java-asm"
                     "java-asm-analysis" "java-asm-tree" "java-asm-util"
                     "java-classpathx-servletapi" "java-xstream" "java-picocli"
                     "java-jansi" "java-jline-2"))
                 ;; antlr.jar is present twice in antlr2.  Symlink doesn't like
                 ;; it, so we symlink it here.
                 (symlink (search-input-file inputs "/lib/antlr.jar")
                          (string-append out-lib "/antlr.jar"))
                 (for-each
                   (lambda (tool)
                     (install-file tool out-bin)
                     (chmod (string-append out-bin "/" tool) #o755))
                   '("grape" "groovy" "groovyc" "groovyConsole" "groovydoc"
                     "groovysh" "java2groovy" "startGroovy")))
               (install-file "src/conf/groovy-starter.conf"
                             (string-append out "/conf"))
               #t))))))
    (inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-ant" ,groovy-ant)
       ("groovy-astbuilder" ,groovy-astbuilder)
       ("groovy-bsf" ,groovy-bsf)
       ("groovy-cli-commons" ,groovy-cli-commons)
       ("groovy-cli-picocli" ,groovy-cli-picocli)
       ("groovy-console" ,groovy-console)
       ("groovy-datetime" ,groovy-datetime)
       ("groovy-dateutil" ,groovy-dateutil)
       ("groovy-docgenerator" ,groovy-docgenerator)
       ("groovy-groovydoc" ,groovy-groovydoc)
       ("groovy-groovysh" ,groovy-groovysh)
       ("groovy-jmx" ,groovy-jmx)
       ("groovy-json" ,groovy-json)
       ("groovy-jsr223" ,groovy-jsr223)
       ("groovy-nio" ,groovy-nio)
       ("groovy-parser-antlr4" ,groovy-parser-antlr4)
       ("groovy-servlet" ,groovy-servlet)
       ("groovy-sql" ,groovy-sql)
       ("groovy-swing" ,groovy-swing)
       ("groovy-templates" ,groovy-templates)
       ("groovy-testng" ,groovy-testng)
       ("groovy-xml" ,groovy-xml)
       ("groovy-yaml" ,groovy-yaml)
       ("java-tunnelvisionlabs-antlr4-runtime"
        ,java-tunnelvisionlabs-antlr4-runtime)
       ("java-commons-cli" ,java-commons-cli)
       ("java-asm" ,java-asm-8)
       ("java-asm-analysis" ,java-asm-analysis-8)
       ("java-asm-tree" ,java-asm-tree-8)
       ("java-asm-util" ,java-asm-util-8)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-picocli" ,java-picocli)
       ("java-jansi" ,java-jansi)
       ("java-jline-2" ,java-jline-2)
       ("java-xstream" ,java-xstream)
       ("antlr2" ,antlr2)))
    (synopsis "Programming language for the JVM")
    (description "Apache Groovy is a powerful, optionally typed and dynamic
language, with static-typing and static compilation capabilities, for the Java
platform.  It integrates smoothly with any Java program, and immediately
delivers to your application powerful features, including scripting
capabilities, Domain-Specific Language authoring, runtime and compile-time
meta-programming and functional programming.")))
