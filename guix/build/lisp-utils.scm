;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
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
            %source-install-prefix
            lisp-eval-program
            compile-system
            test-system
            replace-escaped-macros
            generate-executable-wrapper-system
            generate-executable-entry-point
            generate-executable-for-system
            bundle-install-prefix
            bundle-asd-file
            remove-lisp-from-name
            wrap-output-translations
            prepend-to-source-registry
            build-program
            build-image
            make-asd-file))

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

;; The common parent for Lisp source files, as will as the symbolic
;; link farm for system definition (.asd) files.
(define %source-install-prefix "/share/common-lisp")

(define (bundle-install-prefix lisp)
  (string-append %source-install-prefix "/" lisp "-bundle-systems"))

(define (remove-lisp-from-name name lisp)
  (string-drop name (1+ (string-length lisp))))

(define (inputs->asd-file-map inputs lisp)
  "Produce a hash table of the form (system . asd-file), where system is the
name of an ASD system, and asd-file is the full path to its definition."
  (alist->hash-table
   (filter-map
    (match-lambda
      ((_ . path)
       (let ((prefix (string-append path (bundle-install-prefix lisp))))
         (and (directory-exists? prefix)
              (match (find-files prefix "\\.asd$")
                ((asd-file)
                 (cons
                  (string-drop-right (basename asd-file) 4) ; drop ".asd"
                  asd-file))
                (_ #f))))))
    inputs)))

(define (wrap-output-translations translations)
  `(:output-translations
    ,@translations
    :inherit-configuration))

(define (lisp-eval-program lisp program)
  "Evaluate PROGRAM with a given LISP implementation."
  (unless (zero? (apply system*
                        (lisp-invoke lisp (format #f "~S" program))))
    (error "lisp-eval-program failed!" lisp program)))

(define (lisp-invoke lisp program)
  "Return a list of arguments for system* determining how to invoke LISP
with PROGRAM."
  (match lisp
    ("sbcl" `(,(%lisp) "--non-interactive" "--eval" ,program))
    ("ecl" `(,(%lisp) "-eval" ,program "-eval" "(quit)"))
    (_ (error "The LISP provided is not supported at this time."))))

(define (asdf-load-all systems)
  (map (lambda (system)
         `(funcall
           (find-symbol
            (symbol-name :load-system)
            (symbol-name :asdf))
           ,system))
       systems))

(define (compile-system system lisp asd-file)
  "Use a lisp implementation to compile SYSTEM using asdf.  Load ASD-FILE
first if SYSTEM is defined there."
  (lisp-eval-program lisp
                     `(progn
                       (require :asdf)
                       (in-package :asdf)
                       ,@(if asd-file
                             `((load ,asd-file))
                             '())
                       (in-package :cl-user)
                       (funcall (find-symbol
                                 (symbol-name :operate)
                                 (symbol-name :asdf))
                                (find-symbol
                                 (symbol-name :compile-bundle-op)
                                 (symbol-name :asdf))
                                ,system))))

(define (system-dependencies lisp system asd-file)
  "Return the dependencies of SYSTEM, as reported by
asdf:system-depends-on.  First load the system's ASD-FILE, if necessary."
  (define deps-file ".deps.sexp")
  (define program
    `(progn
      (require :asdf)
      ,@(if asd-file
            `((let ((*package* (find-package :asdf)))
                (load ,asd-file)))
            '())
      (with-open-file
       (stream ,deps-file :direction :output)
       (format stream
               "~s~%"
               (funcall
                (find-symbol
                 (symbol-name :system-depends-on)
                 (symbol-name :asdf))

                (funcall
                 (find-symbol
                  (symbol-name :find-system)
                  (symbol-name :asdf))

                 ,system))))))

  (dynamic-wind
    (lambda _
      (lisp-eval-program lisp program))
    (lambda _
      (call-with-input-file deps-file read))
    (lambda _
      (when (file-exists? deps-file)
        (delete-file deps-file)))))

(define (compiled-system system lisp)
  (match lisp
    ("sbcl" (string-append system "--system"))
    (_ system)))

(define* (generate-system-definition lisp system
                                     #:key version dependencies)
  `(asdf:defsystem
    ,system
    :class asdf/bundle:prebuilt-system
    :version ,version
    :depends-on ,dependencies
    :components ((:compiled-file ,(compiled-system system lisp)))
    ,@(if (string=? "ecl" lisp)
          `(:lib ,(string-append system ".a"))
          '())))

(define (test-system system lisp asd-file)
  "Use a lisp implementation to test SYSTEM using asdf.  Load ASD-FILE first
if SYSTEM is defined there."
  (lisp-eval-program lisp
                     `(progn
                       (require :asdf)
                       (in-package :asdf)
                       ,@(if asd-file
                             `((load ,asd-file))
                             '())
                       (in-package :cl-user)
                       (funcall (find-symbol
                                 (symbol-name :test-system)
                                 (symbol-name :asdf))
                                ,system))))

(define (string->lisp-keyword . strings)
  "Return a lisp keyword for the concatenation of STRINGS."
  (string->symbol (apply string-append ":" strings)))

(define (generate-executable-for-system type system lisp)
  "Use LISP to generate an executable, whose TYPE can be \"image\" or
\"program\".  The latter will always be standalone.  Depends on having created
a \"SYSTEM-exec\" system which contains the entry program."
  (lisp-eval-program
   lisp
   `(progn
     (require :asdf)
     (funcall (find-symbol
               (symbol-name :operate)
               (symbol-name :asdf))
              (find-symbol
               (symbol-name ,(string->lisp-keyword type "-op"))
               (symbol-name :asdf))
              ,(string-append system "-exec")))))

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

(define (generate-dependency-links lisp registry system)
  "Creates a program which populates asdf's source registry from REGISTRY, an
alist of dependency names to corresponding asd files.  This allows the system
to locate its dependent systems."
  `(progn
    (asdf/source-registry:ensure-source-registry)
    ,@(map (match-lambda
             ((name . asd-file)
              `(setf
                (gethash ,name
                         asdf/source-registry:*source-registry*)
                ,(string->symbol "#p")
                ,asd-file)))
           registry)))

(define* (make-asd-file asd-file
                        #:key lisp system version inputs
                        (system-asd-file #f))
  "Create an ASD-FILE for SYSTEM@VERSION, appending a program to allow the
system to find its dependencies, as described by GENERATE-DEPENDENCY-LINKS."
  (define dependencies
    (parameterize ((%lisp (string-append (assoc-ref inputs lisp) "/bin/" lisp)))
      (system-dependencies lisp system system-asd-file)))

  (define lisp-input-map
    (inputs->asd-file-map inputs lisp))

  (define registry
    (filter-map hash-get-handle
                (make-list (if (eq? 'NIL dependencies)
                               0
                               (length dependencies))
                           lisp-input-map)
                (if (eq? 'NIL dependencies)
                    '()
                    dependencies)))

  (call-with-output-file asd-file
    (lambda (port)
      (display
       (replace-escaped-macros
        (format #f "~y~%~y~%"
                (generate-system-definition lisp system
                                            #:version version
                                            #:dependencies dependencies)
                (generate-dependency-links lisp registry system)))
       port))))

(define (bundle-asd-file output-path original-asd-file lisp)
  "Find the symlinked bundle file for ORIGINAL-ASD-FILE by looking in
OUTPUT-PATH/share/common-lisp/LISP-bundle-systems/<system>.asd.  Returns two
values: the asd file itself and the directory in which it resides."
  (let ((bundle-asd-path (string-append output-path
                                        (bundle-install-prefix lisp))))
    (values (string-append bundle-asd-path "/" (basename original-asd-file))
            bundle-asd-path)))

(define (replace-escaped-macros string)
  "Replace simple lisp forms that the guile writer escapes, for example by
replacing #{#p}# with #p.  Should only be used to replace truly simple forms
which are not nested."
  (regexp-substitute/global #f "(#\\{)(\\S*)(\\}#)" string
                            'pre 2 'post))

(define (prepend-to-source-registry path)
  (setenv "CL_SOURCE_REGISTRY"
          (string-append path ":" (or (getenv "CL_SOURCE_REGISTRY") ""))))

(define* (build-program lisp program #:key inputs
                        (dependencies (list (basename program)))
                        entry-program
                        #:allow-other-keys)
  "Generate an executable program containing all DEPENDENCIES, and which will
execute ENTRY-PROGRAM.  The result is placed in PROGRAM.  When executed, it
will run ENTRY-PROGRAM, a list of Common Lisp expressions in which `arguments'
has been bound to the command-line arguments which were passed."
  (generate-executable lisp program
                       #:inputs inputs
                       #:dependencies dependencies
                       #:entry-program entry-program
                       #:type "program")
  (let* ((name (basename program))
         (bin-directory (dirname program)))
    (with-directory-excursion bin-directory
      (rename-file (string-append name "-exec")
                   name)))
  #t)

(define* (build-image lisp image #:key inputs
                      (dependencies (list (basename image)))
                      #:allow-other-keys)
  "Generate an image, possibly standalone, which contains all DEPENDENCIES,
placing the result in IMAGE.image."
  (generate-executable lisp image
                       #:inputs inputs
                       #:dependencies dependencies
                       #:entry-program '(nil)
                       #:type "image")
  (let* ((name (basename image))
         (bin-directory (dirname image)))
    (with-directory-excursion bin-directory
      (rename-file (string-append name "-exec--all-systems.image")
                   (string-append name ".image"))))
  #t)

(define* (generate-executable lisp out-file #:key inputs
                              dependencies
                              entry-program
                              type
                              #:allow-other-keys)
  "Generate an executable by using asdf's TYPE-op, containing whithin the
image all DEPENDENCIES, and running ENTRY-PROGRAM in the case of an
executable."
  (let* ((bin-directory (dirname out-file))
         (name (basename out-file)))
    (mkdir-p bin-directory)
    (with-directory-excursion bin-directory
      (generate-executable-wrapper-system name dependencies)
      (generate-executable-entry-point name entry-program))

    (prepend-to-source-registry
     (string-append bin-directory "/"))

    (setenv "ASDF_OUTPUT_TRANSLATIONS"
            (replace-escaped-macros
             (format
              #f "~S"
              (wrap-output-translations
               `(((,bin-directory :**/ :*.*.*)
                  (,bin-directory :**/ :*.*.*)))))))

    (parameterize ((%lisp (string-append
                           (assoc-ref inputs lisp) "/bin/" lisp)))
      (generate-executable-for-system type name lisp))

    (delete-file (string-append bin-directory "/" name "-exec.asd"))
    (delete-file (string-append bin-directory "/" name "-exec.lisp"))))
