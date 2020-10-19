;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2020 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (guix build lisp-utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix build utils)
  #:export (%lisp
            %lisp-type
            %source-install-prefix
            lisp-eval-program
            compile-systems
            test-system
            replace-escaped-macros
            generate-executable-wrapper-system
            generate-executable-entry-point
            generate-executable-for-system
            wrap-output-translations
            prepend-to-source-registry
            build-program
            build-image
            make-asdf-configuration
            valid-char-set
            normalize-string
            library-output))

;;; Commentary:
;;;
;;; Tools to evaluate lisp programs within a lisp session, generate wrapper
;;; systems for executables. Compile, test, and produce images for systems and
;;; programs, and link them with their dependencies.
;;;
;;; Code:

(define %lisp
  ;; File name of the Lisp compiler.
  (make-parameter "lisp"))

(define %lisp-type
  ;; String representing the class of implementation being used.
  (make-parameter "lisp"))

;; The common parent for Lisp source files, as will as the symbolic
;; link farm for system definition (.asd) files.
(define %source-install-prefix "/share/common-lisp")

(define (library-output outputs)
  "If a `lib' output exists, build things there. Otherwise use `out'."
  (or (assoc-ref outputs "lib") (assoc-ref outputs "out")))

;; See nix/libstore/store-api.cc#checkStoreName.
(define valid-char-set
  (string->char-set
   "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+-._?="))

(define (normalize-string str)
  "Replace invalid characters in STR with a hyphen."
  (string-join (string-tokenize str valid-char-set) "-"))

(define (wrap-output-translations translations)
  `(:output-translations
    ,@translations
    :inherit-configuration))

(define (lisp-eval-program program)
  "Evaluate PROGRAM with a given LISP implementation."
  (define invocation (lisp-invocation program))
  (format #t "Invoking ~a: ~{~s ~}~%" (%lisp-type) invocation)
  (apply invoke invocation))

(define (spread-statements program argument-name)
  "Return a list with the statements from PROGRAM spread between
ARGUMENT-NAME, a string representing the argument a lisp implementation uses
to accept statements to be evaluated before starting."
  (append-map (lambda (statement)
                (list argument-name (format #f "~S" statement)))
              program))

(define (lisp-invocation program)
  "Return a list of arguments for system* determining how to invoke LISP
with PROGRAM."
  (match (%lisp-type)
    ("sbcl" `(,(%lisp) "--non-interactive"
              ,@(spread-statements program "--eval")))
    ("ecl" `(,(%lisp)
             ,@(spread-statements program "--eval")
             "--eval" "(quit)"))
    (_ (error "The LISP provided is not supported at this time."))))

(define (compile-systems systems asd-files)
  "Use a lisp implementation to compile the SYSTEMS using asdf.
Load ASD-FILES first."
  (lisp-eval-program
   `((require :asdf)
     ,@(map (lambda (asd-file)
              `(asdf:load-asd (truename ,asd-file)))
            asd-files)
     ,@(map (lambda (system)
              `(asdf:compile-system ,system))
            systems))))

(define (test-system system asd-files test-asd-file)
  "Use a lisp implementation to test SYSTEM using asdf.  Load ASD-FILES first.
Also load TEST-ASD-FILE if necessary."
  (lisp-eval-program
   `((require :asdf)
     ,@(map (lambda (asd-file)
              `(asdf:load-asd (truename ,asd-file)))
            asd-files)
     ,@(if test-asd-file
           `((asdf:load-asd (truename ,test-asd-file)))
           ;; Try some likely files.
           (map (lambda (file)
                  `(when (uiop:file-exists-p ,file)
                     (asdf:load-asd (truename ,file))))
                (list
                 (string-append system "-tests.asd")
                 (string-append system "-test.asd")
                 "tests.asd"
                 "test.asd")))
     (asdf:test-system ,system))))

(define (string->lisp-keyword . strings)
  "Return a lisp keyword for the concatenation of STRINGS."
  (string->symbol (apply string-append ":" strings)))

(define* (generate-executable-for-system type system #:key compress?)
  "Use LISP to generate an executable, whose TYPE can be 'asdf:image-op or
'asdf:program-op.  The latter will always be standalone.  Depends on having
created a \"SYSTEM-exec\" system which contains the entry program."
  (lisp-eval-program
   `((require :asdf)
     ;; Only SBCL supports compression as of 2019-09-02.
     ,(if (and compress? (string=? (%lisp-type) "sbcl"))
          '(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
                      (uiop:dump-image (asdf:output-file o c)
                                       :executable t
                                       :compression t))
          '())
     (asdf:load-asd (truename ,(string-append system "-exec.asd")))
     (asdf:operate ',type ,(string-append system "-exec")))))

(define (generate-executable-wrapper-system system dependencies)
  "Generates a system which can be used by asdf to produce an image or program
inside the current directory.  The image or program will contain
DEPENDENCIES."
  (with-output-to-file (string-append system "-exec.asd")
    (lambda _
      (format #t "~y~%"
              `(defsystem ,(string->lisp-keyword system "-exec")
                 :entry-point ,(string-append system "-exec:main")
                 :depends-on (:uiop
                              ,@(map string->lisp-keyword
                                     dependencies))
                 :components ((:file ,(string-append system "-exec"))))))))

(define (generate-executable-entry-point system entry-program)
  "Generates an entry point program from the list of lisp statements
ENTRY-PROGRAM for SYSTEM within the current directory."
  (with-output-to-file (string-append system "-exec.lisp")
    (lambda _
      (let ((system (string->lisp-keyword system "-exec")))
        (format #t "~{~y~%~%~}"
                `((defpackage ,system
                    (:use :cl)
                    (:export :main))

                  (in-package ,system)

                  (defun main ()
                    (let ((arguments uiop:*command-line-arguments*))
                      (declare (ignorable arguments))
                      ,@entry-program))))))))

(define (make-asdf-configuration name conf-dir deps-conf-dir source-dir lib-dir)
  (let ((registry-dir (string-append
                       conf-dir "/source-registry.conf.d"))
        (translations-dir (string-append
                           conf-dir "/asdf-output-translations.conf.d"))
        (deps-registry-dir (string-append
                            deps-conf-dir "/source-registry.conf.d"))
        (deps-translations-dir (string-append
                                deps-conf-dir
                                "/asdf-output-translations.conf.d")))
    (mkdir-p registry-dir)
    (when (directory-exists? deps-registry-dir)
      (copy-recursively deps-registry-dir registry-dir))
    (with-output-to-file (string-append registry-dir "/50-" name ".conf")
      (lambda _
        (format #t "~y~%" `(:tree ,source-dir))))

    (mkdir-p translations-dir)
    (when (directory-exists? deps-translations-dir)
      (copy-recursively deps-translations-dir translations-dir))
    (with-output-to-file (string-append translations-dir "/50-" name ".conf")
      (lambda _
        (format #t "~y~%" `((,source-dir :**/ :*.*.*)
                            (,lib-dir :**/ :*.*.*)))))))

(define (replace-escaped-macros string)
  "Replace simple lisp forms that the guile writer escapes, for example by
replacing #{#p}# with #p.  Should only be used to replace truly simple forms
which are not nested."
  (regexp-substitute/global #f "(#\\{)(\\S*)(\\}#)" string
                            'pre 2 'post))

(define (prepend-to-source-registry path)
  (setenv "CL_SOURCE_REGISTRY"
          (string-append path ":" (or (getenv "CL_SOURCE_REGISTRY") ""))))

(define* (build-program program outputs #:key
                        (dependency-prefixes (list (library-output outputs)))
                        (dependencies (list (basename program)))
                        entry-program
                        compress?
                        #:allow-other-keys)
  "Generate an executable program containing all DEPENDENCIES, and which will
execute ENTRY-PROGRAM.  The result is placed in PROGRAM.  When executed, it
will run ENTRY-PROGRAM, a list of Common Lisp expressions in which `arguments'
has been bound to the command-line arguments which were passed.  Link in any
asd files from DEPENDENCY-PREFIXES to ensure references to those libraries are
retained."
  (setenv "XDG_CONFIG_DIRS" (string-append (library-output outputs) "/etc"))
  (generate-executable program
                       #:dependencies dependencies
                       #:dependency-prefixes dependency-prefixes
                       #:entry-program entry-program
                       #:compress? compress?
                       #:type 'asdf:program-op)
  (let* ((name (basename program))
         (bin-directory (dirname program)))
    (with-directory-excursion bin-directory
      (rename-file (string-append name "-exec")
                   name)))
  #t)

(define* (build-image image outputs #:key
                      (dependency-prefixes (list (library-output outputs)))
                      (dependencies (list (basename image)))
                      #:allow-other-keys)
  "Generate an image, possibly standalone, which contains all DEPENDENCIES,
placing the result in IMAGE.image.  Link in any asd files from
DEPENDENCY-PREFIXES to ensure references to those libraries are retained."
  (setenv "XDG_CONFIG_DIRS" (string-append (library-output outputs) "/etc"))
  (generate-executable image
                       #:dependencies dependencies
                       #:dependency-prefixes dependency-prefixes
                       #:entry-program '(nil)
                       #:type 'asdf:image-op)
  (let* ((name (basename image))
         (bin-directory (dirname image)))
    (with-directory-excursion bin-directory
      (rename-file (string-append name "-exec--all-systems.image")
                   (string-append name ".image"))))
  #t)

(define* (generate-executable out-file #:key
                              dependencies
                              dependency-prefixes
                              entry-program
                              type
                              compress?
                              #:allow-other-keys)
  "Generate an executable by using asdf operation TYPE, containing within the
image all DEPENDENCIES, and running ENTRY-PROGRAM in the case of an
executable.  Link in any asd files from DEPENDENCY-PREFIXES to ensure
references to those libraries are retained."
  (let* ((bin-directory (dirname out-file))
         (name (basename out-file)))
    (mkdir-p bin-directory)
    (with-directory-excursion bin-directory
      (generate-executable-wrapper-system name dependencies)
      (generate-executable-entry-point name entry-program)
      (setenv "ASDF_OUTPUT_TRANSLATIONS"
              (replace-escaped-macros
               (format
                #f "~S"
                (wrap-output-translations
                 `(((,bin-directory :**/ :*.*.*)
                    (,bin-directory :**/ :*.*.*)))))))
      (generate-executable-for-system type name #:compress? compress?))

    (let* ((after-store-prefix-index
            (string-index out-file #\/
                          (1+ (string-length (%store-directory)))))
           (output (string-take out-file after-store-prefix-index))
           (hidden-asd-links (string-append output "/.asd-files")))

      (mkdir-p hidden-asd-links)
      (for-each
       (lambda (path)
         (for-each
          (lambda (asd-file)
            (symlink asd-file
                     (string-append hidden-asd-links
                                    "/" (basename asd-file))))
          (find-files (string-append path %source-install-prefix "/"
                                     (%lisp-type))
                      "\\.asd$")))
       dependency-prefixes))

    (delete-file (string-append bin-directory "/" name "-exec.asd"))
    (delete-file (string-append bin-directory "/" name "-exec.lisp"))
    (delete-file (string-append bin-directory "/" name "-exec.fasl"))))
