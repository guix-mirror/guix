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
