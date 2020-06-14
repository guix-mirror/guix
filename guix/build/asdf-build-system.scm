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

(define-module (guix build asdf-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (guix build lisp-utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:export (%standard-phases
            %standard-phases/source
            asdf-build
            asdf-build/source))

;; Commentary:
;;
;; System for building ASDF packages; creating executable programs and images
;; from them.
;;
;; Code:

(define %object-prefix "/lib")

(define (%lisp-source-install-prefix)
  (string-append %source-install-prefix "/" (%lisp-type) "-source"))

(define %system-install-prefix
  (string-append %source-install-prefix "/systems"))

(define (lisp-source-directory output name)
  (string-append output (%lisp-source-install-prefix) "/" name))

(define (source-directory output name)
  (string-append output %source-install-prefix "/source/" name))

(define (library-directory output)
  (string-append output %object-prefix
                 "/" (%lisp-type)))

(define (output-translation source-path
                            object-output)
  "Return a translation for the system's source path
to it's binary output."
  `((,source-path
     :**/ :*.*.*)
    (,(library-directory object-output)
     :**/ :*.*.*)))

(define (source-asd-file output name asd-file)
  (string-append (lisp-source-directory output name) "/" asd-file))

(define (copy-files-to-output out name)
  "Copy all files from the current directory to OUT.  Create an extra link to
any system-defining files in the source to a convenient location.  This is
done before any compiling so that the compiled source locations will be
valid."
  (let ((source (getcwd))
        (target (source-directory out name))
        (system-path (string-append out %system-install-prefix)))
    ;; SBCL keeps the modification time of the source file in the compiled
    ;; file, and the source files might just have been patched by a custom
    ;; phase. Therefore we reset the modification time of all the source
    ;; files before compiling.
    (for-each (lambda (file)
                (let ((s (lstat file)))
                  (unless (or (eq? (stat:type s) 'symlink)
                              (not (access? file W_OK)))
                    (utime file 0 0 0 0))))
              (find-files source #:directories? #t))
    (copy-recursively source target #:keep-mtime? #t)
    (mkdir-p system-path)
    (for-each
     (lambda (file)
       (symlink file
                (string-append system-path "/" (basename file))))
     (find-files target "\\.asd$"))
    #t))

(define* (install #:key inputs outputs #:allow-other-keys)
  "Copy and symlink all the source files.
The source files are taken from the corresponding compile package (e.g. SBCL)
if it's present in the native-inputs."
  (define output (assoc-ref outputs "out"))
  (define package-name
    (package-name->name+version
     (strip-store-file-name output)))
  (define (no-prefix pkgname)
    (if (string-index pkgname #\-)
        (string-drop pkgname (1+ (string-index pkgname #\-)))
        pkgname))
  (define parent
    (match (assoc package-name inputs
                  (lambda (key alist-car)
                    (let* ((alt-key (no-prefix key))
                           (alist-car (no-prefix alist-car)))
                      (or (string=? alist-car key)
                          (string=? alist-car alt-key)))))
      (#f #f)
      (p (cdr p))))
  (define parent-name
    (and parent
         (package-name->name+version (strip-store-file-name parent))))
  (define parent-source
    (and parent
         (string-append parent "/share/common-lisp/"
                        (string-take parent-name
                                     (string-index parent-name #\-))
                        "-source")))

  (define (first-subdirectory directory) ; From gnu-build-system.
    "Return the file name of the first sub-directory of DIRECTORY."
    (match (scandir directory
                    (lambda (file)
                      (and (not (member file '("." "..")))
                           (file-is-directory? (string-append directory "/"
                                                              file)))))
      ((first . _) first)))
  (define source-directory
    (if (and parent-source
             (file-exists? parent-source))
        (string-append parent-source "/" (first-subdirectory parent-source))
        "."))

  (with-directory-excursion source-directory
    (copy-files-to-output output package-name)))

(define* (copy-source #:key outputs asd-system-name #:allow-other-keys)
  "Copy the source to the library output."
  (let* ((out (library-output outputs))
         (install-path (string-append out %source-install-prefix)))
    (copy-files-to-output out asd-system-name)
    ;; Hide the files from asdf
    (with-directory-excursion install-path
      (rename-file "source" (string-append (%lisp-type) "-source"))
      (delete-file-recursively "systems")))
  #t)

(define* (build #:key outputs inputs asd-file asd-system-name
                #:allow-other-keys)
  "Compile the system."
  (let* ((out (library-output outputs))
         (source-path (lisp-source-directory out asd-system-name))
         (translations (wrap-output-translations
                        `(,(output-translation source-path
                                               out))))
         (asd-file (source-asd-file out asd-system-name asd-file)))

    (setenv "ASDF_OUTPUT_TRANSLATIONS"
            (replace-escaped-macros (format #f "~S" translations)))

    (setenv "HOME" out) ; ecl's asdf sometimes wants to create $HOME/.cache

    (compile-system asd-system-name asd-file)

    ;; As above, ecl will sometimes create this even though it doesn't use it

    (let ((cache-directory (string-append out "/.cache")))
      (when (directory-exists? cache-directory)
        (delete-file-recursively cache-directory))))
  #t)

(define* (check #:key tests? outputs inputs asd-file asd-system-name
                test-asd-file
                #:allow-other-keys)
  "Test the system."
  (let* ((out (library-output outputs))
         (asd-file (source-asd-file out asd-system-name asd-file))
         (test-asd-file
          (and=> test-asd-file
                 (cut source-asd-file out asd-system-name <>))))
    (if tests?
        (test-system asd-system-name asd-file test-asd-file)
        (format #t "test suite not run~%")))
  #t)

(define* (create-asd-file #:key outputs
                          inputs
                          asd-file
                          asd-system-name
                          #:allow-other-keys)
  "Create a system definition file for the built system."
  (let*-values (((out) (library-output outputs))
                ((_ version) (package-name->name+version
                              (strip-store-file-name out)))
                ((new-asd-file) (string-append
                                 (library-directory out)
                                 "/" (normalize-string asd-system-name)
                                 ".asd")))

    (make-asd-file new-asd-file
                   #:system asd-system-name
                   #:version version
                   #:inputs inputs
                   #:system-asd-file asd-file))
  #t)

(define* (symlink-asd-files #:key outputs #:allow-other-keys)
  "Create an extra reference to the system in a convenient location."
  (let* ((out (library-output outputs)))
    (for-each
     (lambda (asd-file)
       (receive (new-asd-file asd-file-directory)
           (bundle-asd-file out asd-file)
         (mkdir-p asd-file-directory)
         (symlink asd-file new-asd-file)
         ;; Update the source registry for future phases which might want to
         ;; use the newly compiled system.
         (prepend-to-source-registry
          (string-append asd-file-directory "/"))))

     (find-files (string-append out %object-prefix) "\\.asd$")))
  #t)

(define* (cleanup-files #:key outputs
                        #:allow-other-keys)
  "Remove any compiled files which are not a part of the final bundle."
  (let ((out (library-output outputs)))
    (match (%lisp-type)
      ("sbcl"
       (for-each
        (lambda (file)
          (unless (string-suffix? "--system.fasl" file)
            (delete-file file)))
        (find-files out "\\.fasl$")))
      ("ecl"
       (for-each delete-file
                 (append (find-files out "\\.fas$")
                         (find-files out "\\.o$")))))

    (with-directory-excursion (library-directory out)
      (for-each
       (lambda (file)
         (rename-file file
                      (string-append "./" (basename file))))
       (find-files "."))
      (for-each delete-file-recursively
                (scandir "."
                         (lambda (file)
                           (and
                            (directory-exists? file)
                            (string<> "." file)
                            (string<> ".." file)))))))
  #t)

(define* (strip #:rest args)
  ;; stripping sbcl binaries removes their entry program and extra systems
  (or (string=? (%lisp-type) "sbcl")
      (apply (assoc-ref gnu:%standard-phases 'strip) args)))

(define %standard-phases/source
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'check)
    (delete 'build)
    (replace 'install install)))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'install)
    (replace 'build build)
    (add-before 'build 'copy-source copy-source)
    (replace 'check check)
    (replace 'strip strip)
    (add-after 'check 'create-asd-file create-asd-file)
    (add-after 'create-asd-file 'cleanup cleanup-files)
    (add-after 'cleanup 'create-symlinks symlink-asd-files)))

(define* (asdf-build #:key inputs
                     (phases %standard-phases)
                     #:allow-other-keys
                     #:rest args)
  (apply gnu:gnu-build
         #:inputs inputs
         #:phases phases
         args))

(define* (asdf-build/source #:key inputs
                            (phases %standard-phases/source)
                            #:allow-other-keys
                            #:rest args)
  (apply gnu:gnu-build
         #:inputs inputs
         #:phases phases
         args))

;;; asdf-build-system.scm ends here
