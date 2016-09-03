;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix build ant-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (sxml simple)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            ant-build))

;; Commentary:
;;
;; Builder-side code of the standard build procedure for Java packages using
;; Ant.
;;
;; Code:

(define (default-build.xml jar-name prefix)
  "Create a simple build.xml with standard targets for Ant."
  (call-with-output-file "build.xml"
    (lambda (port)
      (sxml->xml
       `(project (@ (basedir "."))
                 (property (@ (name "classes.dir")
                              (value "${basedir}/build/classes")))
                 (property (@ (name "jar.dir")
                              (value "${basedir}/build/jar")))
                 (property (@ (name "dist.dir")
                              (value ,prefix)))

                 ;; respect the CLASSPATH environment variable
                 (property (@ (name "build.sysclasspath")
                              (value "first")))
                 (property (@ (environment "env")))
                 (path (@ (id "classpath"))
                       (pathelement (@ (location "${env.CLASSPATH}"))))

                 (target (@ (name "compile"))
                         (mkdir (@ (dir "${classes.dir}")))
                         (javac (@ (includeantruntime "false")
                                   (srcdir "src")
                                   (destdir "${classes.dir}")
                                   (classpath (@ (refid "classpath"))))))

                 (target (@ (name "jar")
                            (depends "compile"))
                         (mkdir (@ (dir "${jar.dir}")))
                         (exec (@ (executable "jar"))
                               (arg (@ (line ,(string-append "-cf ${jar.dir}/" jar-name
                                                             " -C ${classes.dir} ."))))))

                 (target (@ (name "install"))
                         (copy (@ (todir "${dist.dir}"))
                               (fileset (@ (dir "${jar.dir}"))
                                        (include (@ (name "**/*.jar")))))))
       port)))
  (utime "build.xml" 0 0)
  #t)

(define (generate-classpath inputs)
  "Return a colon-separated string of full paths to jar files found among the
INPUTS."
  (string-join
   (apply append (map (match-lambda
                        ((_ . dir)
                         (find-files dir "\\.jar$")))
                      inputs)) ":"))

(define* (unpack #:key source #:allow-other-keys)
  "Unpack the jar archive SOURCE.  When SOURCE is not a jar archive fall back
to the default GNU unpack strategy."
  (if (string-suffix? ".jar" source)
      (begin
        (mkdir "src")
        (with-directory-excursion "src"
          (zero? (system* "jar" "-xf" source))))
      ;; Use GNU unpack strategy for things that aren't jar archives.
      ((assq-ref gnu:%standard-phases 'unpack) #:source source)))

(define* (configure #:key inputs outputs (jar-name #f)
                    #:allow-other-keys)
  (when jar-name
    (default-build.xml jar-name
                       (string-append (assoc-ref outputs "out")
                                      "/share/java")))
  (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
  (setenv "CLASSPATH" (generate-classpath inputs)))

(define* (build #:key (make-flags '()) (build-target "jar")
                #:allow-other-keys)
  (zero? (apply system* `("ant" ,build-target ,@make-flags))))

(define* (strip-jar-timestamps #:key outputs
                               #:allow-other-keys)
  "Unpack all jar archives, reset the timestamp of all contained files, and
repack them.  This is necessary to ensure that archives are reproducible."
  (define (repack-archive jar)
    (format #t "repacking ~a\n" jar)
    (let* ((dir (mkdtemp! "jar-contents.XXXXXX"))
           (manifest (string-append dir "/META-INF/MANIFEST.MF")))
      (and (with-directory-excursion dir
             (zero? (system* "jar" "xf" jar)))
           (delete-file jar)
           ;; XXX: copied from (gnu build install)
           (for-each (lambda (file)
                       (let ((s (lstat file)))
                         (unless (eq? (stat:type s) 'symlink)
                           (utime file 0 0 0 0))))
                     (find-files dir #:directories? #t))

           ;; The jar tool will always set the timestamp on the manifest file
           ;; and the containing directory to the current time, even when we
           ;; reuse an existing manifest file.  To avoid this we use "zip"
           ;; instead of "jar".  It is important that the manifest appears
           ;; first.
           (with-directory-excursion dir
             (let* ((files (find-files "." ".*" #:directories? #t))
                    (command (if (file-exists? manifest)
                                 `("zip" "-X" ,jar ,manifest ,@files)
                                 `("zip" "-X" ,jar ,@files))))
               (unless (zero? (apply system* command))
                 (error "'zip' failed"))))
           (utime jar 0 0)
           #t)))

  (every (match-lambda
           ((output . directory)
            (every repack-archive (find-files directory "\\.jar$"))))
         outputs))

(define* (check #:key target (make-flags '()) (tests? (not target))
                (test-target "check")
                #:allow-other-keys)
  (if tests?
      (zero? (apply system* `("ant" ,test-target ,@make-flags)))
      (begin
        (format #t "test suite not run~%")
        #t)))

(define* (install #:key (make-flags '()) #:allow-other-keys)
  (zero? (apply system* `("ant" "install" ,@make-flags))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (replace 'unpack unpack)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'install 'strip-jar-timestamps strip-jar-timestamps)))

(define* (ant-build #:key inputs (phases %standard-phases)
                    #:allow-other-keys #:rest args)
  "Build the given Java package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; ant-build-system.scm ends here
