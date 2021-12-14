;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2018, 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix build emacs-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module ((guix build utils) #:hide (delete))
  #:use-module (guix build emacs-utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            %default-include
            %default-exclude
            emacs-build
            elpa-directory))

;; Commentary:
;;
;; Builder-side code of the build procedure for ELPA Emacs packages.
;;
;; Code:

;;; The location in which Emacs looks for packages.  Emacs Lisp code that is
;;; installed there directly will be found when that directory is added to
;;; EMACSLOADPATH.  To avoid clashes between packages (particularly considering
;;; auxiliary files), we install them one directory level below, however.
;;; This indirection is handled by ‘expand-load-path’ during build and a
;;; profile hook otherwise.
(define %install-dir "/share/emacs/site-lisp")

;; These are the default inclusion/exclusion regexps for the install phase.
(define %default-include '("^[^/]*\\.el$" "^[^/]*\\.info$" "^doc/.*\\.info$"))
(define %default-exclude '("^\\.dir-locals\\.el$" "^[^/]*tests?\\.el$"))

(define gnu:unpack (assoc-ref gnu:%standard-phases 'unpack))

(define (store-file->elisp-source-file file)
  "Convert FILE, a store file name for an Emacs Lisp source file, into a file
name that has been stripped of the hash and version number."
  (let ((suffix ".el"))
    (let-values (((name version)
                  (package-name->name+version
                   (basename
                    (strip-store-file-name file) suffix))))
      (string-append name suffix))))

(define* (unpack #:key source #:allow-other-keys)
  "Unpack SOURCE into the build directory.  SOURCE may be a compressed
archive, a directory, or an Emacs Lisp file."
  (if (string-suffix? ".el" source)
      (begin
        (mkdir "source")
        (chdir "source")
        (copy-file source (store-file->elisp-source-file source))
        #t)
      (gnu:unpack #:source source)))

(define* (expand-load-path #:key (prepend-source? #t) #:allow-other-keys)
  "Expand EMACSLOADPATH, so that inputs, whose code resides in subdirectories,
are properly found.
If @var{prepend-source?} is @code{#t} (the default), also add the current
directory to EMACSLOADPATH in front of any other directories."
  (let* ((source-directory (getcwd))
         (emacs-load-path (string-split (getenv "EMACSLOADPATH") #\:))
         (emacs-load-path*
          (map
           (lambda (dir)
             (match (scandir dir (negate (cute member <> '("." ".."))))
               ((sub) (string-append dir "/" sub))
               (_ dir)))
           emacs-load-path))
         (emacs-load-path-value (string-append
                                 (string-join
                                  (if prepend-source?
                                      (cons source-directory emacs-load-path*)
                                      emacs-load-path*)
                                  ":")
                                 ":")))
    (setenv "EMACSLOADPATH" emacs-load-path-value)
    (when prepend-source?
      (format #t "source directory ~s prepended to the `EMACSLOADPATH' \
environment variable\n" source-directory))
    (let ((diff (lset-difference string=? emacs-load-path* emacs-load-path)))
      (unless (null? diff)
        (format #t "expanded load paths for ~{~a~^, ~}\n"
                (map basename diff))))))

(define* (build #:key outputs inputs #:allow-other-keys)
  "Compile .el files."
  (let* ((emacs (search-input-file inputs "/bin/emacs"))
         (out (assoc-ref outputs "out")))
    (setenv "SHELL" "sh")
    (parameterize ((%emacs emacs))
      (emacs-byte-compile-directory (elpa-directory out)))))

(define* (patch-el-files #:key outputs #:allow-other-keys)
  "Substitute the absolute \"/bin/\" directory with the right location in the
store in '.el' files."

  (let* ((out (assoc-ref outputs "out"))
         (elpa-name-ver (store-directory->elpa-name-version out))
         (el-dir (string-append out %install-dir "/" elpa-name-ver))
         (el-files (find-files (getcwd) "\\.el$")))
    (define (substitute-program-names)
      (substitute* el-files
        (("\"/bin/([^.]\\S*)\"" _ cmd-name)
         (let ((cmd (which cmd-name)))
           (unless cmd
             (error "patch-el-files: unable to locate " cmd-name))
           (string-append "\"" cmd "\"")))))

    (with-directory-excursion el-dir
      ;; Some old '.el' files (e.g., tex-buf.el in AUCTeX) are still
      ;; ISO-8859-1-encoded.
      (unless (false-if-exception (substitute-program-names))
        (with-fluids ((%default-port-encoding "ISO-8859-1"))
          (substitute-program-names))))
    #t))

(define* (check #:key tests? (test-command '("make" "check"))
                (parallel-tests? #t) #:allow-other-keys)
  "Run the tests by invoking TEST-COMMAND.

When TEST-COMMAND uses make and PARALLEL-TESTS is #t, the tests are run in
parallel. PARALLEL-TESTS? is ignored when using a non-make TEST-COMMAND."
  (match-let (((test-program . args) test-command))
    (let ((using-make? (string=? test-program "make")))
      (if tests?
          (apply invoke test-program
                 `(,@args
                   ,@(if (and using-make? parallel-tests?)
                         `("-j" ,(number->string (parallel-job-count)))
                         '())))
          (begin
            (format #t "test suite not run~%")
            #t)))))

(define* (install #:key outputs
                  (include %default-include)
                  (exclude %default-exclude)
                  #:allow-other-keys)
  "Install the package contents."

  (define source (getcwd))

  (define* (install-file? file stat #:key verbose?)
    (let* ((stripped-file (string-trim
                           (string-drop file (string-length source)) #\/)))
      (define (match-stripped-file action regex)
        (let ((result (string-match regex stripped-file)))
          (when (and result verbose?)
                (format #t "info: ~A ~A as it matches \"~A\"\n"
                        stripped-file action regex))
          result))

      (when verbose?
            (format #t "info: considering installing ~A\n" stripped-file))

      (and (any (cut match-stripped-file "included" <>) include)
           (not (any (cut match-stripped-file "excluded" <>) exclude)))))

  (let* ((out (assoc-ref outputs "out"))
         (el-dir (elpa-directory out))
         (files-to-install (find-files source install-file?)))
    (cond
     ((not (null? files-to-install))
      (for-each
       (lambda (file)
         (let* ((stripped-file (string-drop file (string-length source)))
                (target-file (string-append el-dir stripped-file)))
           (format #t "`~a' -> `~a'~%" file target-file)
           (install-file file (dirname target-file))))
       files-to-install)
      #t)
     (else
      (format #t "error: No files found to install.\n")
      (find-files source (lambda (file stat)
                           (install-file? file stat #:verbose? #t)))
      #f))))

(define* (move-doc #:key outputs #:allow-other-keys)
  "Move info files from the ELPA package directory to the info directory."
  (let* ((out (assoc-ref outputs "out"))
         (site-lisp (string-append out %install-dir))
         (info-dir (string-append out "/share/info/"))
         (info-files (find-files site-lisp "\\.info$")))
    (unless (null? info-files)
      (mkdir-p info-dir)
      (with-directory-excursion site-lisp
        (when (file-exists? "dir") (delete-file "dir"))
        (for-each (lambda (f)
                    (copy-file f (string-append info-dir "/" (basename f)))
                    (delete-file f))
                  info-files)))
    #t))

(define* (make-autoloads #:key outputs inputs #:allow-other-keys)
  "Generate the autoloads file."
  (let* ((emacs (search-input-file inputs "/bin/emacs"))
         (out (assoc-ref outputs "out"))
         (elpa-name-ver (store-directory->elpa-name-version out))
         (elpa-name (package-name->name+version elpa-name-ver))
         (el-dir (elpa-directory out)))
    (parameterize ((%emacs emacs))
      (emacs-generate-autoloads elpa-name el-dir))))

(define* (enable-autoloads-compilation #:key outputs #:allow-other-keys)
  "Remove the NO-BYTE-COMPILATION local variable embedded in the generated
autoload files."
  (let* ((out (assoc-ref outputs "out"))
         (autoloads (find-files out "-autoloads.el$")))
    (substitute* autoloads
      ((";; no-byte-compile.*") ""))
    #t))

(define* (validate-compiled-autoloads #:key outputs #:allow-other-keys)
  "Verify whether the byte compiled autoloads load fine."
  (let* ((out (assoc-ref outputs "out"))
         (autoloads (find-files out "-autoloads.elc$")))
    (emacs-batch-eval (format #f "(mapc #'load '~s)" autoloads))))

(define (emacs-package? name)
  "Check if NAME correspond to the name of an Emacs package."
  (string-prefix? "emacs-" name))

(define (package-name-version->elpa-name-version name-ver)
  "Convert the Guix package NAME-VER to the corresponding ELPA name-version
format.  Essentially drop the prefix used in Guix."
  (if (emacs-package? name-ver)  ; checks for "emacs-" prefix
      (string-drop name-ver (string-length "emacs-"))
      name-ver))

(define (store-directory->elpa-name-version store-dir)
  "Given a store directory STORE-DIR return the part of the basename after the
second hyphen.  This corresponds to 'name-version' as used in ELPA packages."
  ((compose package-name-version->elpa-name-version
            strip-store-file-name)
   store-dir))

(define (elpa-directory store-dir)
  "Given the store directory STORE-DIR return the absolute install directory
for libraries following the ELPA convention."
  (string-append store-dir %install-dir "/"
                 (store-directory->elpa-name-version store-dir)))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (replace 'unpack unpack)
    (add-after 'unpack 'expand-load-path expand-load-path)
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'install 'make-autoloads make-autoloads)
    (add-after 'make-autoloads 'enable-autoloads-compilation
      enable-autoloads-compilation)
    (add-after 'enable-autoloads-compilation 'patch-el-files patch-el-files)
    ;; The .el files are byte compiled directly in the store.
    (add-after 'patch-el-files 'build build)
    (add-after 'build 'validate-compiled-autoloads validate-compiled-autoloads)
    (add-after 'validate-compiled-autoloads 'move-doc move-doc)))

(define* (emacs-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Emacs package, applying all of PHASES in order."
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))

;;; emacs-build-system.scm ends here
