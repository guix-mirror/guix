;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
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
  #:use-module (guix build union)
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

(define %object-prefix "/lib/common-lisp")

(define (%lisp-source-install-prefix)
  (string-append %source-install-prefix "/" (%lisp-type)))

(define %system-install-prefix
  (string-append %source-install-prefix "/systems"))

(define (main-system-name output)
  ;; FIXME: Find a more reliable way to get the main system name.
  (let* ((full-name (strip-store-file-name output))
         (lisp-prefix (string-append (%lisp-type) "-"))
         (package-name (if (string-prefix? lisp-prefix full-name)
                           (string-drop full-name (string-length lisp-prefix))
                           full-name)))
    (package-name->name+version package-name)))

(define (lisp-source-directory output name)
  (string-append output (%lisp-source-install-prefix) "/" name))

(define (source-directory output name)
  (string-append output %source-install-prefix "/source/" name))

(define (library-directory output)
  (string-append output %object-prefix
                 "/" (%lisp-type)))

(define (output-translation source-path
                            object-output)
  "Return a translation for the system's source path to its binary output."
  `((,source-path
     :**/ :*.*.*)
    (,(library-directory object-output)
     :**/ :*.*.*)))

(define (source-asd-file output name asd-file)
  (string-append (lisp-source-directory output name) "/" asd-file))

(define (find-asd-files output name asd-files)
  (if (null? asd-files)
      (find-files (lisp-source-directory output name) "\\.asd$")
      (map (lambda (asd-file)
             (source-asd-file output name asd-file))
           asd-files)))

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
    (let ((index (string-index pkgname #\-)))
      (if index
          (string-drop pkgname (1+ index))
          pkgname)))
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
                        (let ((index (string-index parent-name #\-)))
                          (if index
                              (string-take parent-name index)
                              parent-name)))))

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

(define* (copy-source #:key outputs asd-systems #:allow-other-keys)
  "Copy the source to the library output."
  (let* ((out (library-output outputs))
         (install-path (string-append out %source-install-prefix))
         (system-name (main-system-name out)))
    (copy-files-to-output out system-name)
    ;; Hide the files from asdf
    (with-directory-excursion install-path
      (rename-file "source" (%lisp-type))
      (delete-file-recursively "systems")))
  #t)

(define* (configure #:key inputs #:allow-other-keys)
  ;; Create a directory having the configuration files for
  ;; all the dependencies in 'etc/common-lisp/'.
  (let ((out (string-append (getcwd) "/.cl-union")))
    (match inputs
      (((name . directories) ...)
       (union-build out (filter directory-exists? directories)
                    #:create-all-directories? #t
                    #:log-port (%make-void-port "w"))))
    (setenv "CL_UNION" out)
    (setenv "XDG_CONFIG_DIRS" (string-append out "/etc")))
  #t)

(define* (build #:key outputs inputs asd-files asd-systems
                #:allow-other-keys)
  "Compile the system."
  (let* ((out (library-output outputs))
         (system-name (main-system-name out))
         (source-path (string-append out (%lisp-source-install-prefix)))
         (translations (wrap-output-translations
                        `(,(output-translation source-path
                                               out))))
         (asd-files (find-asd-files out system-name asd-files)))
    (setenv "ASDF_OUTPUT_TRANSLATIONS"
            (replace-escaped-macros (format #f "~S" translations)))
    (setenv "HOME" out) ; ecl's asdf sometimes wants to create $HOME/.cache
    (compile-systems asd-systems asd-files))
  #t)

(define* (check #:key tests? outputs inputs asd-files asd-systems
                test-asd-file
                #:allow-other-keys)
  "Test the system."
  (let* ((out (library-output outputs))
         (system-name (main-system-name out))
         (asd-files (find-asd-files out system-name asd-files))
         (test-asd-file
          (and=> test-asd-file
                 (cut source-asd-file out system-name <>))))
    (if tests?
        (test-system (first asd-systems) asd-files test-asd-file)
        (format #t "test suite not run~%")))
  #t)

(define* (create-asdf-configuration #:key inputs outputs #:allow-other-keys)
  "Create the ASDF configuration files for the built systems."
  (let* ((system-name (main-system-name (assoc-ref outputs "out")))
         (out (library-output outputs))
         (conf-dir (string-append out "/etc/common-lisp"))
         (deps-conf-dir (string-append (getenv "CL_UNION") "/etc/common-lisp"))
         (source-dir (lisp-source-directory out system-name))
         (lib-dir (string-append (library-directory out) "/" system-name)))
    (make-asdf-configuration system-name conf-dir deps-conf-dir
                             source-dir lib-dir)
    #t))

(define* (cleanup-files #:key outputs
                        #:allow-other-keys)
  "Remove any compiled files which are not a part of the final bundle."
  (let* ((out (library-output outputs))
         (cache-directory (string-append out "/.cache")))
    ;; Remove the cache directory in case the lisp implementation wrote
    ;; something in there when compiling or testing a system.
    (when (directory-exists? cache-directory)
      (delete-file-recursively cache-directory)))
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
    (replace 'configure configure)
    (add-before 'configure 'copy-source copy-source)
    (replace 'build build)
    (replace 'check check)
    (add-after 'check 'create-asdf-configuration create-asdf-configuration)
    (add-after 'create-asdf-configuration 'cleanup cleanup-files)
    (delete 'install)
    (replace 'strip strip)))

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
