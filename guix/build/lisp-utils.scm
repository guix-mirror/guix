;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
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
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix build utils)
  #:export (%lisp
            %install-prefix
            lisp-eval-program
            compile-system
            test-system
            replace-escaped-macros
            generate-executable-wrapper-system
            generate-executable-entry-point
            generate-executable-for-system
            patch-asd-file
            bundle-install-prefix
            lisp-dependencies
            bundle-asd-file
            remove-lisp-from-name
            wrap-output-translations
            prepend-to-source-registry
            build-program
            build-image))

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

(define %install-prefix "/share/common-lisp")

(define (bundle-install-prefix lisp)
  (string-append %install-prefix "/" lisp "-bundle-systems"))

(define (remove-lisp-from-name name lisp)
  (string-drop name (1+ (string-length lisp))))

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
    ("ecl" `(,(%lisp) "-eval" ,program "-eval" "(quit)"))))

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
                                ,system)
                       (funcall (find-symbol
                                 (symbol-name :operate)
                                 (symbol-name :asdf))
                                (find-symbol
                                 (symbol-name :deliver-asd-op)
                                 (symbol-name :asdf))
                                ,system))))

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

(define (wrap-perform-method lisp registry dependencies file-name)
  "Creates a wrapper method which allows the system to locate its dependent
systems from REGISTRY, an alist of the same form as %outputs, which contains
lisp systems which the systems is dependent on.  All DEPENDENCIES which the
system depends on will the be loaded before this system."
  (let* ((system (string-drop-right (basename file-name) 4))
         (system-symbol (string->lisp-keyword system)))

    `(defmethod asdf:perform :before
       (op (c (eql (asdf:find-system ,system-symbol))))
       (asdf/source-registry:ensure-source-registry)
       ,@(map (match-lambda
                ((name . path)
                 (let ((asd-file (string-append path
                                                (bundle-install-prefix lisp)
                                                "/" name ".asd")))
                   `(setf
                     (gethash ,name
                              asdf/source-registry:*source-registry*)
                     ,(string->symbol "#p")
                     ,(bundle-asd-file path asd-file lisp)))))
              registry)
       ,@(map (lambda (system)
                `(asdf:load-system ,(string->lisp-keyword system)))
              dependencies))))

(define (patch-asd-file asd-file registry lisp dependencies)
  "Patches ASD-FILE with a perform method as described in WRAP-PERFORM-METHOD."
  (chmod asd-file #o644)
  (let ((port (open-file asd-file "a")))
    (dynamic-wind
      (lambda _ #t)
      (lambda _
        (display
         (replace-escaped-macros
          (format #f "~%~y~%"
                  (wrap-perform-method lisp registry
                                       dependencies asd-file)))
         port))
      (lambda _ (close-port port))))
  (chmod asd-file #o444))

(define (lisp-dependencies lisp inputs)
  "Determine which inputs are lisp system dependencies, by using the convention
that a lisp system dependency will resemble \"system-LISP\"."
  (filter-map (match-lambda
                ((name . value)
                 (and (string-prefix? lisp name)
                      (string<> lisp name)
                      `(,(remove-lisp-from-name name lisp)
                        . ,value))))
              inputs))

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
