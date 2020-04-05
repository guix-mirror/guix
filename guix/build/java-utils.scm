;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
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

(define-module (guix build java-utils)
  #:use-module (guix build utils)
  #:use-module (guix build syscalls)
  #:use-module (guix build maven pom)
  #:use-module (guix build maven plugin)
  #:use-module (ice-9 match)
  #:use-module (sxml simple)
  #:export (ant-build-javadoc
            generate-plugin.xml
            install-jars
            install-javadoc
            install-pom-file
            install-from-pom))

(define* (ant-build-javadoc #:key (target "javadoc") (make-flags '())
                            #:allow-other-keys)
  (apply invoke `("ant" ,target ,@make-flags)))

(define* (install-jars jar-directory)
  "Install jar files from JAR-DIRECTORY to the default target directory.  This
is used in case the build.xml does not include an install target."
  (lambda* (#:key outputs #:allow-other-keys)
    (let ((share (string-append (assoc-ref outputs "out")
                                "/share/java")))
      (for-each (lambda (f) (install-file f share))
                (find-files jar-directory "\\.jar$"))
      #t)))

(define* (install-javadoc apidoc-directory)
  "Install the APIDOC-DIRECTORY to the target directory.  This is used to
install javadocs when this is not done by the install target."
  (lambda* (#:key outputs #:allow-other-keys)
    (let* ((out (assoc-ref outputs "out"))
           (name-version (strip-store-file-name out))
           (docs (string-append (or (assoc-ref outputs "doc") out)
                                "/share/doc/" name-version "/")))
      (mkdir-p docs)
      (copy-recursively apidoc-directory docs)
      #t)))

(define* (install-pom-file pom-file)
  "Install a @file{.pom} file to a maven repository structure in @file{lib/m2}
that respects the file's artifact ID and group ID.  This requires the parent
pom, if any, to be present in the inputs so some of this information can be
fetched."
  (lambda* (#:key inputs outputs #:allow-other-keys)
    (let* ((out (assoc-ref outputs "out"))
           (java-inputs (append (map cdr inputs) (map cdr outputs)))
           (pom-content (get-pom pom-file))
           (version (pom-version pom-content java-inputs))
           (artifact (pom-artifactid pom-content))
           (group (group->dir (pom-groupid pom-content java-inputs)))
           (repository (string-append out "/lib/m2/" group "/" artifact "/"
                                      version "/"))
           (pom-name (string-append repository artifact "-" version ".pom")))
      (mkdir-p (dirname pom-name))
      (copy-file pom-file pom-name))
    #t))

(define (install-jar-file-with-pom jar pom-file inputs)
  "Unpack the jar archive, add the pom file, and repack it.  This is necessary
to ensure that maven can find dependencies."
  (format #t "adding ~a to ~a\n" pom-file jar)
  (let* ((dir (mkdtemp! "jar-contents.XXXXXX"))
         (manifest (string-append dir "/META-INF/MANIFEST.MF"))
         (pom (get-pom pom-file))
         (artifact (pom-artifactid pom))
         (group (pom-groupid pom inputs))
         (version (pom-version pom inputs))
         (pom-dir (string-append "META-INF/maven/" group "/" artifact)))
    (mkdir-p (string-append dir "/" pom-dir))
    (copy-file pom-file (string-append dir "/" pom-dir "/pom.xml"))
    (with-directory-excursion dir
      (with-output-to-file (string-append pom-dir "/pom.properties")
        (lambda _
          (format #t "version=~a~%" version)
          (format #t "groupId=~a~%" group)
          (format #t "artifactId=~a~%" artifact)))
      (invoke "jar" "uf" jar (string-append pom-dir "/pom.xml")
              (string-append pom-dir "/pom.properties")))
    #t))

(define* (install-from-pom pom-file)
  "Install a jar archive and its @var{pom-file} to a maven repository structure
in @file{lib/m2}.  This requires the parent pom file, if any, to be present in
the inputs of the package being built.  This phase looks either for a properly
named jar file (@file{artifactID-version.jar}) or the single jar in the build
directory.  If there are more than one jar, and none is named appropriately,
the phase fails."
  (lambda* (#:key inputs outputs jar-name #:allow-other-keys)
    (let* ((out (assoc-ref outputs "out"))
           (java-inputs (append (map cdr inputs) (map cdr outputs)))
           (pom-content (get-pom pom-file))
           (version (pom-version pom-content java-inputs))
           (artifact (pom-artifactid pom-content))
           (group (group->dir (pom-groupid pom-content java-inputs)))
           (repository (string-append out "/lib/m2/" group "/" artifact "/"
                                      version "/"))
           ;; We try to find the file that was built.  If it was built from our
           ;; generated ant.xml file, it is name jar-name, otherwise it should
           ;; have the expected name for maven.
           (jars (find-files "." (or jar-name (string-append artifact "-"
                                                             version ".jar"))))
           ;; Otherwise, we try to find any jar file.
           (jars (if (null? jars)
                     (find-files "." ".*.jar")
                     jars))
           (jar-name (string-append repository artifact "-" version ".jar"))
           (pom-name (string-append repository artifact "-" version ".pom")))
      ;; Ensure we can override the file
      (chmod pom-file #o644)
      (fix-pom-dependencies pom-file java-inputs)
      (mkdir-p (dirname jar-name))
      (copy-file pom-file pom-name)
      ;; If there are too many jar files, we don't know which one to install, so
      ;; fail.
      (if (= (length jars) 1)
          (begin
            (copy-file (car jars) jar-name)
            (install-jar-file-with-pom jar-name pom-file java-inputs))
          (throw 'no-jars jars)))
    #t))

(define (sxml-indent sxml)
  "Adds some indentation to @var{sxml}, an sxml value, to make reviewing easier
after the value is written to an xml file."
  (define (sxml-indent-aux sxml lvl)
    (match sxml
      ((? string? str) str)
      ((tag ('@ attr ...) content ...)
       (cond
         ((null? content) sxml)
         ((string? (car content)) sxml)
         (else
           `(,tag (@ ,@attr) ,(sxml-indent-content content (+ lvl 1))))))
      ((tag content ...)
       (cond
         ((null? content) sxml)
         ((string? (car content)) sxml)
         (else `(,tag ,(sxml-indent-content content (+ lvl 1))))))
      (_ sxml)))
  (define (sxml-indent-content sxml lvl)
    (map
      (lambda (sxml)
        (list "\n" (string-join (make-list (* 2 lvl) " ") "")
              (sxml-indent-aux sxml lvl)))
      sxml))
  (sxml-indent-aux sxml 0))

(define* (generate-plugin.xml pom-file goal-prefix directory source-groups
                              #:key
                              (plugin.xml "build/classes/META-INF/maven/plugin.xml"))
  "Generates the @file{plugin.xml} file that is required by Maven so it can
recognize the package as a plugin, and find the entry points in the plugin."
  (lambda* (#:key inputs outputs #:allow-other-keys)
    (let* ((pom-content (get-pom pom-file))
           (java-inputs (append (map cdr inputs) (map cdr outputs)))
           (name (pom-name pom-content))
           (description (pom-description pom-content))
           (dependencies (pom-dependencies pom-content))
           (version (pom-version pom-content java-inputs))
           (artifact (pom-artifactid pom-content))
           (groupid (pom-groupid pom-content java-inputs))
           (mojos
            `(mojos
               ,@(with-directory-excursion directory
                   (map
                     (lambda (group)
                       (apply generate-mojo-from-files maven-convert-type group))
                     source-groups)))))
      (mkdir-p (dirname plugin.xml))
      (with-output-to-file plugin.xml
        (lambda _
          (sxml->xml
            (sxml-indent
              `(plugin
                 (name ,name)
                 (description ,description)
                 (groupId ,groupid)
                 (artifactId ,artifact)
                 (version ,version)
                 (goalPrefix ,goal-prefix)
                 (isolatedRealm "false")
                 (inheritedByDefault "true")
                 ,mojos
                 (dependencies
                  ,@dependencies)))))))))
