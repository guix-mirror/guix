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

(define-module (gnu packages groovy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages java)
  #:use-module (gnu packages xml))

(define java-groovy-bootstrap
  (package
    (name "java-groovy-bootstrap")
    (version "2.4.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/groovy/archive/GROOVY_"
                                  (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                                  ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19f3yd2z6jmz1xhwi5kkg1wmgbqkfs7qvd3rzb43xr3nffz8cisv"))
              (patches
                (search-patches
                  "groovy-add-exceptionutilsgenerator.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "groovy.jar"
       #:source-dir "src/main:subprojects/groovy-test/src/main/java"
       #:test-dir "src/test"
       #:tests? #f
       #:jdk ,icedtea-8
       #:main-class "groovy.ui.GroovyMain"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-java8
           ;; Fix "Reference to plus is ambiguous"
           (lambda _
             (substitute* "src/main/org/codehaus/groovy/runtime/DefaultGroovyMethods.java"
               (("toList\\(left\\)")
                "(List<T>)toList(left)"))
             #t))
         (add-before 'build 'generate-parsers
           (lambda _
             (with-directory-excursion "src/main/org/codehaus/groovy/antlr/java"
               (invoke "antlr" "java.g"))
             (with-directory-excursion "src/main/org/codehaus/groovy/antlr"
               (mkdir "parser")
               (with-directory-excursion "parser"
                 (invoke "antlr" "../groovy.g")))
             #t))
         (add-before 'build 'generate-exception-utils
           (lambda _
             (invoke "javac" "-cp" (getenv "CLASSPATH")
                     "config/ant/src/org/codehaus/groovy/ExceptionUtilsGenerator.java")
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                 ":config/ant/src")
                     "org.codehaus.groovy.ExceptionUtilsGenerator"
                     "build/classes/org/codehaus/groovy/runtime/ExceptionUtils.class")
             #t)))))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("antlr2" ,antlr2)
       ("java-jmock-1" ,java-jmock-1)
       ("java-xmlunit-legacy" ,java-xmlunit-legacy)))
    (inputs
     `(("java-commons-cli" ,java-commons-cli)
       ("java-asm" ,java-asm)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-xstream" ,java-xstream)
       ("java-jansi" ,java-jansi)
       ("java-jline-2" ,java-jline-2)))
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
       #:jdk ,icedtea-8
       ;Requires groovy-xml and logback-classic which are circular dependencies
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-java8
           ;; Fix "Reference to plus is ambiguous"
           (lambda _
             (substitute* "src/main/org/codehaus/groovy/runtime/DefaultGroovyMethods.java"
               (("toList\\(left\\)")
                "(List<T>)toList(left)"))
             #t))
         (add-before 'build 'generate-parser
           (lambda _
             (with-directory-excursion "src/main/org/codehaus/groovy/antlr/java"
               (invoke "antlr" "java.g"))
             (with-directory-excursion "src/main/org/codehaus/groovy/antlr"
               (mkdir "parser")
               (with-directory-excursion "parser"
                 (invoke "antlr" "../groovy.g")))
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
                     "src/main/org/codehaus/groovy/tools/DgmConverter.java")
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                 ":src/main")
                     "org.codehaus.groovy.tools.DgmConverter")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (with-directory-excursion "src/main"
               (for-each (lambda (file)
                           (mkdir-p (string-append "../../target/classes/"
                                                   (dirname file)))
                           (copy-file file
                                      (string-append "../../target/classes/"
                                                     file)))
                  (find-files "." ".*.(txt|properties|xml|html)")))
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                           "org.codehaus.groovy.tools.FileSystemCompiler"
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
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/classes"
                    "-j"; joint compilation
                    (append
                      (find-files "src/test" "TestSupport.java")
                      (find-files "src/test" "HeadlessTestSupport.java")
                      (find-files "src/test" "XmlAssert.java")))
             (invoke "jar" "-cf" "build/jar/groovy-tests-bootstrap.jar"
                     "-C" "build/classes" ".")
             #t)))))
    (inputs
     `(("groovy-test" ,groovy-test)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ,@(package-native-inputs java-groovy-bootstrap)))
    (synopsis "Groovy test classes")
    (description "This package contains three classes required for testing
other groovy submodules.")))

(define groovy-test
  (package
    (inherit groovy-bootstrap)
    (name "groovy-test")
    (arguments
     `(#:jar-name "groovy-test.jar"
       #:jdk ,icedtea-8
       #:test-dir "subprojects/groovy-test/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/classes" "-j"; joint compilation
                    (find-files "subprojects/groovy-test/src/main"
                                ".*\\.(groovy|java)$"))
             (invoke "jar" "-cf" "build/jar/groovy-test.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (apply invoke "java" "-cp"
                    (string-append (getenv "CLASSPATH") ":build/classes")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/test-classes" "-j"
                    (append (find-files "subprojects/groovy-test/src/test"
                                        ".*\\.(groovy|java)$")))
             (invoke "ant" "check")
             #t)))))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ,@(package-native-inputs java-groovy-bootstrap)))
    (synopsis "Groovy test submodule")
    (description "This package contains the test submodules used to test
other groovy submodules.")))

(define groovy-xml
  (package
    (inherit groovy-bootstrap)
    (name "groovy-xml")
    (arguments
     `(#:jar-name "groovy-xml.jar"
       #:jdk ,icedtea-8
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-xml")
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/classes" "-j"; joint compilation
                    (find-files "src/main" ".*\\.(groovy|java)$"))
             (invoke "jar" "-cf" "build/jar/groovy-xml.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (apply invoke "java" "-cp"
                    (string-append (getenv "CLASSPATH") ":build/classes")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/test-classes" "-j"
                    (append (find-files "src/test" ".*\\.(groovy|java)$")))
             (invoke "ant" "check")
             #t)))))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs java-groovy-bootstrap)))
    (synopsis "Groovy XML")
    (description "This package contains XML-related utilities for groovy.")))

(define groovy-templates
  (package
    (inherit groovy-bootstrap)
    (name "groovy-templates")
    (arguments
     `(#:jar-name "groovy-templates.jar"
       #:jdk ,icedtea-8
       #:test-dir "subprojects/groovy-templates/src/test"
       #:tests? #f;Requires spock-framework which is a circular dependency
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/classes" "-j"; joint compilation
                    (find-files "subprojects/groovy-templates/src/main"
                                ".*\\.(groovy|java)$"))
             (invoke "jar" "-cf" "build/jar/groovy-templates.jar"
                     "-C" "build/classes" ".")
             #t)))))
    (inputs
     `(("groovy-xml" ,groovy-xml)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs java-groovy-bootstrap)))
    (synopsis "Groovy template engine")
    (description "This package contains a template framework which is
well-suited to applications where the text to be generated follows the form of
a static template.")))

(define groovy-groovydoc
  (package
    (inherit groovy-bootstrap)
    (name "groovy-groovydoc")
    (arguments
     `(#:jar-name "groovy-groovydoc.jar"
       #:jdk ,icedtea-8
       #:test-dir "subprojects/groovy-groovydoc/src/test"
       #:tests? #f; Requires groovy-ant which is a circular dependency
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "subprojects/groovy-groovydoc/src/main/resources"
                               "build/classes")
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/classes"
                    "-j"; joint compilation
                    (find-files "subprojects/groovy-groovydoc/src/main"
                                ".*\\.(groovy|java)$"))
             (invoke "jar" "-cf" "build/jar/groovy-groovydoc.jar"
                     "-C" "build/classes" ".")
             #t)))))
    (inputs
     `(("groovy-templates" ,groovy-templates)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs java-groovy-bootstrap)))
    (synopsis "Groovy documentation generation")
    (description "This package contains the groovy documentation generator,
similar to javadoc.")))

(define groovy-ant
  (package
    (inherit groovy-bootstrap)
    (name "groovy-ant")
    (arguments
     `(#:jar-name "groovy-ant.jar"
       #:jdk ,icedtea-8
       #:test-dir "src/test"
       ;; FIXME: Excluding all tests because they fail
       #:test-exclude (list
                        "**/GroovyTest.java"
                        "**/GroovycTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-ant")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/classes" "-j"; joint compilation
                    (find-files "src/main" ".*\\.(groovy|java)$"))
             (invoke "jar" "-cf" "build/jar/groovy-ant.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (apply invoke "java" "-cp"
                    (string-append (getenv "CLASSPATH") ":build/classes")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/test-classes" "-j"
                    (find-files "src/test" ".*\\.(groovy|java)$"))
             (invoke "ant" "check")
             #t)))))
    (inputs
     `(("groovy-groovydoc" ,groovy-groovydoc)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-xml" ,groovy-xml)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs java-groovy-bootstrap)))
    (synopsis "Groovy ant tasks")
    (description "This package contains groovy-related ant tasks definitions.")))

(define groovy-bsf
  (package
    (inherit groovy-bootstrap)
    (name "groovy-bsf")
    (arguments
     `(#:jar-name "groovy-bsf.jar"
       #:jdk ,icedtea-8
       #:test-dir "src/test"
       #:test-exclude (list
;; exception from Groovy: org.codehaus.groovy.runtime.InvokerInvocationException:
;; groovy.lang.MissingMethodException: No signature of method:
;; java.util.ArrayList.each() is applicable for argument types:
;; (groovy.script.MapFromList$_doit_closure1) values:
;; [groovy.script.MapFromList$_doit_closure1@17e554d5]
                        "**/BSFTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-bsf")
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/classes" "-j"; joint compilation
                    (find-files "src/main" ".*\\.(groovy|java)$"))
             (invoke "jar" "-cf" "build/jar/groovy-bsf.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\""))
             (apply invoke "java" "-cp"
                    (string-append (getenv "CLASSPATH") ":build/classes")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/test-classes" "-j"
                    (find-files "src/test" ".*\\.(groovy|java)$"))
             (invoke "ant" "check")
             #t)))))
    (inputs
     `(("java-commons-bsf" ,java-commons-bsf)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ,@(package-native-inputs java-groovy-bootstrap)))
    (synopsis "Groovy BSF engine")
    (description "This package defines the BSF engine for using Groovy inside
any @dfn{Bean Scripting Framework} (BSF) application.")))

(define groovy-swing
  (package
    (inherit groovy-bootstrap)
    (name "groovy-swing")
    (arguments
     `(#:jar-name "groovy-swing.jar"
       #:jdk ,icedtea-8
       ;; FIXME: tests are not run
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-swing")
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/classes" "-j"; joint compilation
                    (find-files "src/main" ".*\\.(groovy|java)$"))
             (invoke "jar" "-cf" "build/jar/groovy-swing.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "src/test/groovy/groovy/util/GroovySwingTestCase.groovy"
               (("HeadlessTestSupport.headless") "isHeadless()"))
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (apply invoke "java" "-cp"
                    (string-append (getenv "CLASSPATH") ":build/classes")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/test-classes" "-j"
                    (find-files "src/test" ".*\\.(groovy|java)$"))
             (invoke "ant" "check")
             #t)))))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ,@(package-native-inputs java-groovy-bootstrap)))
    (synopsis "Groovy graphical library")
    (description "This package contains the groovy bindings to Java Swing, a
library used to build graphical interfaces.")))

(define groovy-console
  (package
    (inherit groovy-bootstrap)
    (name "groovy-console")
    (arguments
     `(#:jar-name "groovy-console.jar"
       #:jdk ,icedtea-8
       ;; FIXME: tests are not run
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-console")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "java" "-cp" (getenv "CLASSPATH")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/classes" "-j"; joint compilation
                    (find-files "src/main" ".*\\.(groovy|java)$"))
             (invoke "jar" "-cf" "build/jar/groovy-console.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (substitute*
               "../groovy-swing/src/test/groovy/groovy/util/GroovySwingTestCase.groovy"
               (("HeadlessTestSupport.headless") "isHeadless()"))
             (apply invoke "java" "-cp"
                    (string-append (getenv "CLASSPATH") ":build/classes")
                    "org.codehaus.groovy.tools.FileSystemCompiler"
                    "-d" "build/test-classes" "-j"
                    (append
                      (find-files "../groovy-swing/src/test" ".*\\.(groovy|java)$")
                      (find-files "src/test" ".*\\.(groovy|java)$")))
             (invoke "ant" "check")
             #t)))))
    (inputs
     `(("groovy-swing" ,groovy-swing)
       ("groovy-templates" ,groovy-templates)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ,@(package-native-inputs java-groovy-bootstrap)))
    (synopsis "Groovy graphical interface")
    (description "This package contains a graphical interface to run groovy.")))
